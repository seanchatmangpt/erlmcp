%%%-------------------------------------------------------------------
%%% @doc erlmcp_memory_manager - Memory Management for Validation Framework
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_memory_manager).
-behaviour(gen_server).

-include_lib("kernel/include/logger.hrl").

%% API
-export([
    start_link/0, start_link/1, stop/1,
    start_memory_monitor/0, get_memory_usage/0,
    cache_spec/1, cache_spec/2, get_cached_spec/1,
    purge_cache/0, purge_cache/1,
    optimize_memory_usage/1, force_garbage_collection/0,
    set_memory_limit/1, get_memory_limit/0,
    check_memory_pressure/0, get_stats/0, reset_stats/0
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(DEFAULT_MAX_CACHE_SIZE, 100).
-define(DEFAULT_SPEC_MEMORY_LIMIT, 100 * 1024 * 1024).
-define(MEMORY_CHECK_INTERVAL, 5000).
-define(GC_INTERVAL, 60000).

-record(state, {
    spec_cache = #{},
    max_cache_size :: pos_integer(),
    max_spec_memory :: pos_integer(),
    memory_monitor_ref :: reference() | undefined,
    gc_timer_ref :: reference() | undefined,
    memory_limit :: pos_integer(),
    stats = #{cache_hits => 0, cache_misses => 0, cache_evictions => 0, gc_runs => 0}
}).

%%====================================================================
%% API
%%====================================================================

start_link() -> start_link(#{}).
start_link(Config) -> gen_server:start_link({local, ?MODULE}, ?MODULE, Config, []).

stop(Pid) -> gen_server:stop(Pid).

start_memory_monitor() -> gen_server:call(?MODULE, start_memory_monitor).

get_memory_usage() -> gen_server:call(?MODULE, get_memory_usage).

cache_spec(Spec) -> 
    SpecId = generate_spec_id(Spec),
    gen_server:call(?MODULE, {cache_spec, SpecId, Spec}).

cache_spec(SpecId, Spec) -> 
    gen_server:call(?MODULE, {cache_spec, SpecId, Spec}).

get_cached_spec(SpecId) -> 
    gen_server:call(?MODULE, {get_cached_spec, SpecId}).

purge_cache() -> gen_server:call(?MODULE, purge_cache).
purge_cache(SpecId) -> gen_server:call(?MODULE, {purge_cache, SpecId}).

optimize_memory_usage(Pressure) -> 
    gen_server:call(?MODULE, {optimize_memory, Pressure}).

force_garbage_collection() -> gen_server:call(?MODULE, force_garbage_collection).

set_memory_limit(Limit) -> gen_server:call(?MODULE, {set_memory_limit, Limit}).
get_memory_limit() -> gen_server:call(?MODULE, get_memory_limit).

check_memory_pressure() -> gen_server:call(?MODULE, check_memory_pressure).

get_stats() -> gen_server:call(?MODULE, get_stats).
reset_stats() -> gen_server:call(?MODULE, reset_stats).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init(Config) ->
    MaxCacheSize = maps:get(max_cache_size, Config, ?DEFAULT_MAX_CACHE_SIZE),
    MaxSpecMemory = maps:get(max_spec_memory, Config, ?DEFAULT_SPEC_MEMORY_LIMIT),
    MemoryLimit = maps:get(memory_limit, Config, erlang:memory(total) * 2),
    MonitorRef = erlang:send_after(?MEMORY_CHECK_INTERVAL, self(), memory_check),
    GCTimerRef = erlang:send_after(?GC_INTERVAL, self(), gc_cycle),
    State = #state{
        max_cache_size = MaxCacheSize,
        max_spec_memory = MaxSpecMemory,
        memory_monitor_ref = MonitorRef,
        gc_timer_ref = GCTimerRef,
        memory_limit = MemoryLimit
    },
    {ok, State}.

handle_call(start_memory_monitor, _From, State) ->
    MonitorPid = spawn_link(fun() -> memory_monitor_loop(self()) end),
    {reply, {ok, MonitorPid}, State};

handle_call(get_memory_usage, _From, State) ->
    Stats = calculate_memory_stats(State),
    {reply, {ok, Stats}, State};

handle_call({cache_spec, SpecId, Spec}, _From, State) ->
    SpecSize = calculate_term_size(Spec),
    MaxSpecMemory = State#state.max_spec_memory,
    case SpecSize > MaxSpecMemory of
        true ->
            {reply, {error, {spec_too_large, SpecSize, MaxSpecMemory}}, State};
        false ->
            CacheSize = maps:size(State#state.spec_cache),
            State2 = case CacheSize >= State#state.max_cache_size of
                true -> evict_lru_entry(State);
                false -> State
            end,
            Now = erlang:monotonic_time(millisecond),
            CacheEntry = #{
                spec => Spec,
                size_bytes => SpecSize,
                cached_at => Now,
                last_accessed => Now,
                access_count => 0
            },
            NewCache = maps:put(SpecId, CacheEntry, State2#state.spec_cache),
            {reply, ok, State2#state{spec_cache = NewCache}}
    end;

handle_call({get_cached_spec, SpecId}, _From, State) ->
    case maps:get(SpecId, State#state.spec_cache, undefined) of
        undefined ->
            NewStats = maps:update_with(cache_misses, fun(V) -> V + 1 end, State#state.stats),
            {reply, {error, not_found}, State#state{stats = NewStats}};
        CacheEntry ->
            Now = erlang:monotonic_time(millisecond),
            UpdatedEntry = CacheEntry#{
                last_accessed => Now,
                access_count => maps:get(access_count, CacheEntry, 0) + 1
            },
            NewCache = maps:put(SpecId, UpdatedEntry, State#state.spec_cache),
            NewStats = maps:update_with(cache_hits, fun(V) -> V + 1 end, State#state.stats),
            {reply, {ok, CacheEntry#{spec => CacheEntry#{spec => CacheEntry}}}, 
             State#state{spec_cache = NewCache, stats = NewStats}}
    end;

handle_call(purge_cache, _From, State) ->
    {reply, ok, State#state{spec_cache = #{}}};

handle_call({purge_cache, SpecId}, _From, State) ->
    NewCache = maps:remove(SpecId, State#state.spec_cache),
    {reply, ok, State#state{spec_cache = NewCache}};

handle_call({optimize_memory, Pressure}, _From, State) ->
    NewState = apply_optimization(Pressure, State),
    {reply, ok, NewState};

handle_call(force_garbage_collection, _From, State) ->
    Processes = erlang:processes(),
    lists:foreach(fun(P) -> catch erlang:garbage_collect(P) end, Processes),
    NewStats = maps:update_with(gc_runs, fun(V) -> V + 1 end, State#state.stats),
    {reply, {ok, length(Processes)}, State#state{stats = NewStats}};

handle_call({set_memory_limit, Limit}, _From, State) ->
    {reply, ok, State#state{memory_limit = Limit}};

handle_call(get_memory_limit, _From, State) ->
    {reply, {ok, State#state.memory_limit}, State};

handle_call(check_memory_pressure, _From, State) ->
    Stats = calculate_memory_stats(State),
    UsedPercent = maps:get(used_percent, Stats, 0.0),
    Pressure = determine_pressure_level(UsedPercent),
    {reply, Pressure, State};

handle_call(get_stats, _From, State) ->
    {reply, {ok, State#state.stats}, State};

handle_call(reset_stats, _From, State) ->
    ResetStats = #{cache_hits => 0, cache_misses => 0, cache_evictions => 0, gc_runs => 0},
    {reply, ok, State#state{stats = ResetStats}};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(memory_check, State) ->
    NewState = monitor_memory_pressure(State),
    TimerRef = erlang:send_after(?MEMORY_CHECK_INTERVAL, self(), memory_check),
    {noreply, NewState#state{memory_monitor_ref = TimerRef}};

handle_info(gc_cycle, State) ->
    NewState = perform_garbage_collection(State),
    TimerRef = erlang:send_after(?GC_INTERVAL, self(), gc_cycle),
    {noreply, NewState#state{gc_timer_ref = TimerRef}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    case State#state.memory_monitor_ref of
        undefined -> ok;
        MemRef -> erlang:cancel_timer(MemRef)
    end,
    case State#state.gc_timer_ref of
        undefined -> ok;
        GcRef -> erlang:cancel_timer(GcRef)
    end,
    ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

generate_spec_id(Spec) ->
    Hash = erlang:phash2(Spec),
    binary_to_atom(<<"spec_", (integer_to_binary(Hash))/binary>>, utf8).

calculate_term_size(Term) ->
    try
        %% Try external_size first (available in all OTP versions)
        erlang:external_size(Term)
    catch
        _:_ ->
            %% Fallback to binary encoding size
            byte_size(term_to_binary(Term))
    end.

calculate_memory_stats(State) ->
    SystemMem = erlang:memory(),
    Total = proplists:get_value(total, SystemMem, 0),
    CacheMemory = lists:foldl(
        fun({_SpecId, Entry}, Acc) -> Acc + maps:get(size_bytes, Entry, 0) end,
        0, maps:to_list(State#state.spec_cache)
    ),
    UsedPercent = case State#state.memory_limit of
        0 -> 0.0;
        Limit -> (Total / Limit) * 100
    end,
    #{
        total_memory => Total,
        used_memory => Total,
        cache_memory => CacheMemory,
        process_memory => proplists:get_value(processes, SystemMem, 0),
        system_memory => proplists:get_value(system, SystemMem, 0),
        used_percent => UsedPercent,
        cache_entries => maps:size(State#state.spec_cache)
    }.

determine_pressure_level(UsedPercent) when UsedPercent >= 95.0 -> critical;
determine_pressure_level(UsedPercent) when UsedPercent >= 85.0 -> high;
determine_pressure_level(UsedPercent) when UsedPercent >= 70.0 -> medium;
determine_pressure_level(_UsedPercent) -> low.

monitor_memory_pressure(State) ->
    Stats = calculate_memory_stats(State),
    UsedPercent = maps:get(used_percent, Stats, 0.0),
    Pressure = determine_pressure_level(UsedPercent),
    case Pressure of
        critical -> apply_optimization(aggressive, State);
        high -> apply_optimization(balanced, State);
        medium -> apply_optimization(conservative, State);
        low -> State
    end.

apply_optimization(Level, State) when Level =:= aggressive orelse Level =:= critical ->
    purge_cache_entries(State, 0.75);
apply_optimization(Level, State) when Level =:= balanced orelse Level =:= high ->
    purge_cache_entries(State, 0.50);
apply_optimization(Level, State) when Level =:= conservative orelse Level =:= medium ->
    purge_cache_entries(State, 0.25);
apply_optimization(_Level, State) -> State.

purge_cache_entries(State, Percentage) ->
    CacheList = lists:sort(
        fun(_SpecIdA, EntryA, _SpecIdB, EntryB) ->
            maps:get(last_accessed, EntryA) < maps:get(last_accessed, EntryB)
        end, maps:to_list(State#state.spec_cache)
    ),
    EntriesToPurge = max(1, round(length(CacheList) * Percentage)),
    ToPurge = lists:sublist(CacheList, EntriesToPurge),
    NewCache = lists:foldl(
        fun({SpecId, _Entry}, Acc) -> maps:remove(SpecId, Acc) end,
        State#state.spec_cache, ToPurge
    ),
    NewStats = maps:update_with(
        cache_evictions,
        fun(V) -> V + EntriesToPurge end,
        State#state.stats
    ),
    State#state{spec_cache = NewCache, stats = NewStats}.

evict_lru_entry(State) ->
    CacheList = maps:to_list(State#state.spec_cache),
    case CacheList of
        [] -> State;
        _ ->
            {SpecId, _Entry} = lists:foldl(
                fun({_Id, Entry}, {AccId, AccEntry}) ->
                    case maps:get(last_accessed, Entry) < maps:get(last_accessed, AccEntry) of
                        true -> {_Id, Entry};
                        false -> {AccId, AccEntry}
                    end
                end, hd(CacheList), tl(CacheList)
            ),
            NewCache = maps:remove(SpecId, State#state.spec_cache),
            NewStats = maps:update_with(cache_evictions, fun(V) -> V + 1 end, State#state.stats),
            State#state{spec_cache = NewCache, stats = NewStats}
    end.

perform_garbage_collection(State) ->
    ValidationProcesses = find_validation_processes(),
    lists:foreach(fun(Pid) -> catch erlang:garbage_collect(Pid) end, ValidationProcesses),
    NewStats = maps:update_with(gc_runs, fun(V) -> V + 1 end, State#state.stats),
    State#state{stats = NewStats}.

find_validation_processes() ->
    AllProcesses = erlang:processes(),
    lists:filtermap(fun(Pid) ->
        case process_info(Pid, registered_name) of
            {registered_name, Name} when is_atom(Name) ->
                case lists:suffix("validator", atom_to_list(Name)) of
                    true -> {true, Pid};
                    false -> false
                end;
            _ -> false
        end
    end, AllProcesses).

memory_monitor_loop(Parent) ->
    receive
        stop -> ok;
        {check_memory, ReplyTo} ->
            ReplyTo ! {memory_stats, erlmcp_memory_manager:get_memory_usage()},
            memory_monitor_loop(Parent);
        _ -> memory_monitor_loop(Parent)
    after 10000 ->
        case erlmcp_memory_manager:check_memory_pressure() of
            critical -> Parent ! {memory_pressure, critical};
            high -> Parent ! {memory_pressure, high};
            _ -> ok
        end,
        memory_monitor_loop(Parent)
    end.
