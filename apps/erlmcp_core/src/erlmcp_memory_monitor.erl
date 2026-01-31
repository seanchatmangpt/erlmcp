%%%-------------------------------------------------------------------
%%% @doc Memory Monitor - Binary Garbage Collection and Heap Protection
%%%
%%% This module implements proactive memory monitoring to prevent heap
%%% exhaustion. It provides:
%%%
%%% - Periodic memory pressure monitoring
%%% - Binary garbage collection triggers
%%% - Heap size monitoring with configurable thresholds
%%% - Process memory limit enforcement
%%% - Automatic GC under memory pressure
%%%
%%% Configuration via sys.config:
%%% ```erlang
%%% {erlmcp_core, [
%%%     {memory_monitoring, #{
%%%         enabled => true,
%%%         check_interval => 30000,        % 30 seconds
%%%         memory_threshold => 0.80,       % 80% of system limit
%%%         binary_threshold => 50000000,   % 50MB binaries
%%%         auto_gc => true                 % Auto GC on pressure
%%%     }}
%%% ]}
%%% ```
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_memory_monitor).

-behaviour(gen_server).


%% API
-export([
    start_link/0,
    stop/0,
    get_memory_stats/0,
    get_gc_stats/0,
    check_memory_pressure/0,
    check_binary_memory/0,
    force_gc/0,
    force_binary_gc/0
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("kernel/include/logger.hrl").

%% Constants
-define(DEFAULT_CHECK_INTERVAL, 30000).      % 30 seconds
-define(DEFAULT_MEMORY_THRESHOLD, 0.80).     % 80% of system limit
-define(DEFAULT_BINARY_THRESHOLD, 50000000). % 50MB binaries
-define(DEFAULT_AUTO_GC, true).              % Auto GC enabled

%% Types
-type memory_stats() :: #{
    total => non_neg_integer(),
    processes => non_neg_integer(),
    system => non_neg_integer(),
    atom => non_neg_integer(),
    binary => non_neg_integer(),
    code => non_neg_integer(),
    ets => non_neg_integer(),
    used_percent => float(),
    threshold_exceeded => boolean()
}.

-type gc_stats() :: #{
    gc_count => non_neg_integer(),
    gc_words_reclaimed => non_neg_integer(),
    last_gc_time => integer() | undefined,
    binary_gc_count => non_neg_integer(),
    last_binary_gc_time => integer() | undefined
}.

-type memory_pressure() :: low | medium | high | critical.

%% Server state
-record(state, {
    check_interval :: pos_integer(),
    memory_threshold :: float(),
    binary_threshold :: non_neg_integer(),
    auto_gc :: boolean(),
    check_timer :: reference() | undefined,
    gc_count = 0 :: non_neg_integer(),
    binary_gc_count = 0 :: non_neg_integer(),
    last_gc_time :: integer() | undefined,
    last_binary_gc_time :: integer() | undefined,
    stats = #{} :: map()
}).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Start the memory monitor
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Stop the memory monitor
-spec stop() -> ok.
stop() ->
    gen_server:stop(?MODULE).

%% @doc Get current memory statistics
-spec get_memory_stats() -> memory_stats().
get_memory_stats() ->
    gen_server:call(?MODULE, get_memory_stats).

%% @doc Get garbage collection statistics
-spec get_gc_stats() -> gc_stats().
get_gc_stats() ->
    gen_server:call(?MODULE, get_gc_stats).

%% @doc Check current memory pressure level
-spec check_memory_pressure() -> memory_pressure().
check_memory_pressure() ->
    gen_server:call(?MODULE, check_memory_pressure).

%% @doc Check binary memory usage
-spec check_binary_memory() -> {ok, non_neg_integer()} | {warning, non_neg_integer()}.
check_binary_memory() ->
    gen_server:call(?MODULE, check_binary_memory).

%% @doc Force garbage collection across all processes
-spec force_gc() -> {ok, non_neg_integer()}.
force_gc() ->
    gen_server:call(?MODULE, force_gc).

%% @doc Force binary garbage collection
-spec force_binary_gc() -> {ok, non_neg_integer()}.
force_binary_gc() ->
    gen_server:call(?MODULE, force_binary_gc).

%%====================================================================
%% gen_server Callbacks
%%====================================================================

-spec init([]) -> {ok, #state{}}.
init([]) ->
    %% Load configuration
    CheckInterval = load_config(check_interval, ?DEFAULT_CHECK_INTERVAL),
    MemoryThreshold = load_config(memory_threshold, ?DEFAULT_MEMORY_THRESHOLD),
    BinaryThreshold = load_config(binary_threshold, ?DEFAULT_BINARY_THRESHOLD),
    AutoGC = load_config(auto_gc, ?DEFAULT_AUTO_GC),

    %% Start periodic memory check timer
    CheckTimer = erlang:send_after(CheckInterval, self(), check_memory),

    logger:info("Memory monitor started: check_interval=~pms, memory_threshold=~p, binary_threshold=~p bytes, auto_gc=~p",
               [CheckInterval, MemoryThreshold, BinaryThreshold, AutoGC]),

    {ok, #state{
        check_interval = CheckInterval,
        memory_threshold = MemoryThreshold,
        binary_threshold = BinaryThreshold,
        auto_gc = AutoGC,
        check_timer = CheckTimer
    }}.

handle_call(get_memory_stats, _From, State) ->
    Stats = get_memory_stats_internal(State),
    {reply, Stats, State};

handle_call(get_gc_stats, _From, State) ->
    Stats = #{
        gc_count => State#state.gc_count,
        gc_words_reclaimed => 0,  % Not tracked in this implementation
        last_gc_time => State#state.last_gc_time,
        binary_gc_count => State#state.binary_gc_count,
        last_binary_gc_time => State#state.last_binary_gc_time
    },
    {reply, Stats, State};

handle_call(check_memory_pressure, _From, State) ->
    Pressure = calculate_memory_pressure(State),
    {reply, Pressure, State};

handle_call(check_binary_memory, _From, State) ->
    BinaryMem = erlang:memory(binary),
    Result = case BinaryMem > State#state.binary_threshold of
        true -> {warning, BinaryMem};
        false -> {ok, BinaryMem}
    end,
    {reply, Result, State};

handle_call(force_gc, _From, State) ->
    Collected = do_force_gc(),
    Now = erlang:monotonic_time(millisecond),
    NewState = State#state{
        gc_count = State#state.gc_count + 1,
        last_gc_time = Now
    },
    logger:info("Forced GC across ~p processes", [Collected]),
    {reply, {ok, Collected}, NewState};

handle_call(force_binary_gc, _From, State) ->
    Collected = do_force_binary_gc(),
    Now = erlang:monotonic_time(millisecond),
    NewState = State#state{
        binary_gc_count = State#state.binary_gc_count + 1,
        last_binary_gc_time = Now
    },
    logger:info("Forced binary GC across ~p processes", [Collected]),
    {reply, {ok, Collected}, NewState};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(check_memory, State) ->
    %% Perform periodic memory check
    NewState = do_memory_check(State),

    %% Reschedule check timer
    NewTimer = erlang:send_after(State#state.check_interval, self(), check_memory),

    {noreply, NewState#state{check_timer = NewTimer}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    logger:info("Memory monitor terminated"),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private Get memory statistics with threshold check
-spec get_memory_stats_internal(#state{}) -> memory_stats().
get_memory_stats_internal(State) ->
    Total = erlang:memory(total),
    Processes = erlang:memory(processes),
    System = erlang:memory(system),
    Atom = erlang:memory(atom),
    Binary = erlang:memory(binary),
    Code = erlang:memory(code),
    Ets = erlang:memory(ets),

    %% Get system limit from memory guard
    SystemLimit = try
        erlmcp_memory_guard:get_system_limit()
    catch
        _:_ -> 16 * 1024 * 1024 * 1024  % 16GB fallback
    end,

    UsedPercent = (Total / SystemLimit) * 100,
    ThresholdExceeded = UsedPercent > (State#state.memory_threshold * 100),

    #{
        total => Total,
        processes => Processes,
        system => System,
        atom => Atom,
        binary => Binary,
        code => Code,
        ets => Ets,
        used_percent => UsedPercent,
        threshold_exceeded => ThresholdExceeded
    }.

%% @private Calculate memory pressure level
-spec calculate_memory_pressure(#state{}) -> memory_pressure().
calculate_memory_pressure(State) ->
    Stats = get_memory_stats_internal(State),
    UsedPercent = maps:get(used_percent, Stats, 0.0),
    Threshold = State#state.memory_threshold * 100,

    if
        UsedPercent >= Threshold + 10 -> critical;
        UsedPercent >= Threshold -> high;
        UsedPercent >= Threshold - 10 -> medium;
        true -> low
    end.

%% @private Perform periodic memory check
-spec do_memory_check(#state{}) -> #state{}.
do_memory_check(State) ->
    Stats = get_memory_stats_internal(State),
    Pressure = calculate_memory_pressure(State),
    BinaryMem = maps:get(binary, Stats, 0),

    %% Check if action needed
    case {Pressure, BinaryMem > State#state.binary_threshold, State#state.auto_gc} of
        {critical, _, true} ->
            logger:warning("Critical memory pressure detected - forcing GC"),
            do_force_gc(),
            Now = erlang:monotonic_time(millisecond),
            State#state{
                gc_count = State#state.gc_count + 1,
                last_gc_time = Now
            };
        {high, true, true} ->
            logger:warning("High memory pressure and binary threshold exceeded - forcing binary GC"),
            do_force_binary_gc(),
            Now = erlang:monotonic_time(millisecond),
            State#state{
                binary_gc_count = State#state.binary_gc_count + 1,
                last_binary_gc_time = Now
            };
        {high, _, true} ->
            logger:info("High memory pressure detected - forcing GC"),
            do_force_gc(),
            Now = erlang:monotonic_time(millisecond),
            State#state{
                gc_count = State#state.gc_count + 1,
                last_gc_time = Now
            };
        _ ->
            %% Log if memory usage is elevated but below action threshold
            case Pressure of
                medium ->
                    logger:debug("Medium memory pressure: ~.2f%", [maps:get(used_percent, Stats)]);
                _ ->
                    ok
            end,
            State
    end.

%% @private Force garbage collection across all processes
-spec do_force_gc() -> non_neg_integer().
do_force_gc() ->
    Processes = erlang:processes(),
    lists:foldl(fun(Pid, Count) ->
        try
            erlang:garbage_collect(Pid),
            Count + 1
        catch
            _:_ -> Count
        end
    end, 0, Processes).

%% @private Force binary garbage collection
-spec do_force_binary_gc() -> non_neg_integer().
do_force_binary_gc() ->
    Processes = erlang:processes(),
    lists:foldl(fun(Pid, Count) ->
        try
            %% Check binary memory first
            case erlang:process_info(Pid, binary) of
                {binary, Binaries} when length(Binaries) > 0 ->
                    erlang:garbage_collect(Pid),
                    Count + 1;
                _ ->
                    Count
            end
        catch
            _:_ -> Count
        end
    end, 0, Processes).

%% @private Load configuration value
-spec load_config(atom(), term()) -> term().
load_config(Key, Default) ->
    case application:get_env(erlmcp_core, memory_monitoring) of
        {ok, Config} when is_map(Config) ->
            maps:get(Key, Config, Default);
        {ok, Config} when is_list(Config) ->
            proplists:get_value(Key, Config, Default);
        _ ->
            Default
    end.
