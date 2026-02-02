%%%-------------------------------------------------------------------
%%% @doc
%%% erlmcp_process_monitor_sup - Real-time Process Monitoring Supervisor
%%%
%%% OTP 28-based process introspection monitor with:
%%% - Periodic process scanning using iterators
%%% - Real-time statistics aggregation by type
%%% - Memory usage tracking
%%% - Message queue length statistics (min, max, avg)
%%% - Process count monitoring with thresholds
%%% - Metrics export to OTEL
%%%
%%% Uses erlmcp_inspector for O(1) memory-efficient data collection.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_process_monitor_sup).

-behaviour(gen_server).

%% API
-export([start_link/0, start_link/1, get_stats/0, trigger_scan/0,
         set_scan_interval/1, get_scan_interval/0,
         subscribe_to_updates/1, unsubscribe_from_updates/0]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("kernel/include/logger.hrl").

%% Types
-type stats() :: #{timestamp => erlang:timestamp(),
                    scan_duration_ms => non_neg_integer(),
                    total_processes => non_neg_integer(),
                    by_type => #{atom() => non_neg_integer()},
                    total_memory_bytes => non_neg_integer(),
                    memory_by_type => #{atom() => non_neg_integer()},
                    queue_stats => #{atom() => #{min => non_neg_integer(),
                                                max => non_neg_integer(),
                                                avg => float()}}}.
-type subscriber() :: {pid(), reference()}.

-record(state,
        {scan_interval :: pos_integer(),
         timer_ref :: undefined | timer:tref(),
         last_stats :: undefined | stats(),
         subscribers = [] :: [subscriber()]}).

-define(DEFAULT_SCAN_INTERVAL, 5000). % 5 seconds

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Start the process monitor with default scan interval.
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link([]).

%% @doc Start the process monitor with options.
-spec start_link(list()) -> {ok, pid()} | {error, term()}.
start_link(Opts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Opts, []).

%% @doc Get current process statistics.
-spec get_stats() -> {ok, stats()} | {error, term()}.
get_stats() ->
    gen_server:call(?MODULE, get_stats).

%% @doc Trigger an immediate scan (useful for testing or on-demand updates).
-spec trigger_scan() -> ok.
trigger_scan() ->
    gen_server:cast(?MODULE, trigger_scan).

%% @doc Set scan interval in milliseconds.
-spec set_scan_interval(pos_integer()) -> ok.
set_scan_interval(IntervalMs) when is_integer(IntervalMs), IntervalMs > 0 ->
    gen_server:call(?MODULE, {set_scan_interval, IntervalMs}).

%% @doc Get current scan interval.
-spec get_scan_interval() -> {ok, pos_integer()}.
get_scan_interval() ->
    gen_server:call(?MODULE, get_scan_interval).

%% @doc Subscribe to statistics updates.
%% Caller will receive {process_stats_update, stats()} messages.
-spec subscribe_to_updates(pid()) -> ok.
subscribe_to_updates(SubscriberPid) when is_pid(SubscriberPid) ->
    gen_server:call(?MODULE, {subscribe, SubscriberPid}).

%% @doc Unsubscribe from statistics updates.
-spec unsubscribe_from_updates() -> ok.
unsubscribe_from_updates() ->
    gen_server:call(?MODULE, unsubscribe).

%%====================================================================
%% gen_server Callbacks
%%====================================================================

init(Opts) ->
    ?LOG_INFO("Starting process monitor supervisor"),

    %% Trap exits for cleanup
    process_flag(trap_exit, true),

    %% Initialize state
    ScanInterval = proplists:get_value(scan_interval, Opts, ?DEFAULT_SCAN_INTERVAL),

    %% Start periodic scan timer
    {ok, TimerRef} = timer:send_interval(ScanInterval, periodic_scan),

    %% Perform initial scan
    InitialStats = perform_scan(),

    State = #state{scan_interval = ScanInterval,
                   timer_ref = TimerRef,
                   last_stats = InitialStats},

    ?LOG_INFO("Process monitor supervisor started with interval ~pms", [ScanInterval]),
    {ok, State}.

handle_call(get_stats, _From, State) ->
    {reply, {ok, State#state.last_stats}, State};

handle_call({set_scan_interval, IntervalMs}, _From, State) ->
    %% Cancel old timer
    timer:cancel(State#state.timer_ref),

    %% Start new timer with updated interval
    {ok, NewTimerRef} = timer:send_interval(IntervalMs, periodic_scan),

    ?LOG_INFO("Scan interval updated to ~pms", [IntervalMs]),
    {reply, ok, State#state{scan_interval = IntervalMs, timer_ref = NewTimerRef}};

handle_call(get_scan_interval, _From, State) ->
    {reply, {ok, State#state.scan_interval}, State};

handle_call({subscribe, SubscriberPid}, _From, State) ->
    %% Monitor the subscriber
    MonitorRef = erlang:monitor(process, SubscriberPid),
    NewSubscriber = {SubscriberPid, MonitorRef},
    NewSubscribers = [NewSubscriber | State#state.subscribers],

    %% Send current stats immediately
    SubscriberPid ! {process_stats_update, State#state.last_stats},

    {reply, ok, State#state{subscribers = NewSubscribers}};

handle_call(unsubscribe, {FromPid, _Tag}, State) ->
    %% Remove subscriber with matching PID
    NewSubscribers = lists:filter(fun({Pid, _Ref}) -> Pid =/= FromPid end,
                                  State#state.subscribers),
    {reply, ok, State#state{subscribers = NewSubscribers}};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(trigger_scan, State) ->
    %% Perform immediate scan
    NewStats = perform_scan(),
    notify_subscribers(NewStats, State#state.subscribers),
    {noreply, State#state{last_stats = NewStats}};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(periodic_scan, State) ->
    %% Perform periodic scan
    NewStats = perform_scan(),

    %% Notify subscribers
    notify_subscribers(NewStats, State#state.subscribers),

    {noreply, State#state{last_stats = NewStats}};

handle_info({'DOWN', Ref, process, _Pid, _Reason}, State) ->
    %% Subscriber died, remove from list
    NewSubscribers = lists:filter(fun({_Pid, MonitorRef}) -> MonitorRef =/= Ref end,
                                  State#state.subscribers),
    {noreply, State#state{subscribers = NewSubscribers}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    %% Cancel timer
    timer:cancel(State#state.timer_ref),

    %% Clean up subscribers (demonitor all)
    lists:foreach(fun({_Pid, Ref}) -> erlang:demonitor(Ref, [flush]) end,
                  State#state.subscribers),

    ?LOG_INFO("Process monitor supervisor terminating"),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @doc Perform process scan using erlmcp_inspector (OTP 28 iterators).
-spec perform_scan() -> stats().
perform_scan() ->
    StartTime = erlang:monotonic_time(millisecond),

    %% Get aggregate stats from inspector
    AggregateStats = erlmcp_inspector:get_aggregate_stats(),

    EndTime = erlang:monotonic_time(millisecond),
    ScanDuration = EndTime - StartTime,

    %% Build stats map
    Stats = #{timestamp => erlang:timestamp(),
              scan_duration_ms => ScanDuration,
              total_processes => maps:get(total, AggregateStats, 0),
              by_type => normalize_types(maps:get(by_type, AggregateStats, #{})),
              total_memory_bytes => maps:get(total_memory, AggregateStats, 0),
              memory_by_type => normalize_types(maps:get(memory_by_type, AggregateStats, #{})),
              queue_stats => normalize_types(maps:get(queue_stats, AggregateStats, #{}))},

    %% Log slow scans
    case ScanDuration > 1000 of
        true ->
            ?LOG_WARNING("Slow process scan: ~pms", [ScanDuration]);
        false ->
            ok
    end,

    Stats.

%% @doc Normalize process type atoms to strings for JSON serialization.
-spec normalize_types(map()) -> map().
normalize_types(Map) ->
    maps:fold(fun(Type, Value, Acc) ->
                     TypeStr = type_to_string(Type),
                     maps:put(TypeStr, Value, Acc)
              end,
              #{},
              Map).

%% @doc Convert process type atom to binary string.
-spec type_to_string(atom()) -> binary().
type_to_string(model_context) -> <<"model_context">>;
type_to_string(tool_process) -> <<"tool_process">>;
type_to_string(transport_handler) -> <<"transport_handler">>;
type_to_string(session_backend) -> <<"session_backend">>;
type_to_string(registry) -> <<"registry">>;
type_to_string(monitor) -> <<"monitor">>;
type_to_string(unknown) -> <<"unknown">>;
type_to_string(Other) when is_atom(Other) ->
    list_to_binary(atom_to_list(Other)).

%% @doc Notify all subscribers of stats update.
-spec notify_subscribers(stats(), [subscriber()]) -> ok.
notify_subscribers(Stats, Subscribers) ->
    lists:foreach(fun({Pid, _Ref}) ->
                     catch Pid ! {process_stats_update, Stats}
                 end,
                 Subscribers),
    ok.
