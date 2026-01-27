-module(erlmcp_simple_metrics).

%% Simple ETS-based metrics for erlmcp
%% Tracks: request_count, error_count, latency_ms
%% NO external dependencies, just ETS and standard Erlang

-export([
    start/0,
    stop/0,
    increment/2,
    record_latency/2,
    get_stats/0,
    reset/0,
    request/0,
    error/0,
    success/0
]).

-define(METRICS_TABLE, erlmcp_simple_metrics).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Start the metrics system
-spec start() -> ok | {error, already_exists}.
start() ->
    case ets:info(?METRICS_TABLE) of
        undefined ->
            ?METRICS_TABLE = ets:new(?METRICS_TABLE, [named_table, public, set, {write_concurrency, true}]),
            init_counters(),
            ok;
        _ ->
            {error, already_exists}
    end.

%% @doc Stop the metrics system
-spec stop() -> ok.
stop() ->
    case ets:info(?METRICS_TABLE) of
        undefined -> ok;
        _ -> 
            ets:delete(?METRICS_TABLE),
            ok
    end.

%% @doc Increment a counter metric
-spec increment(atom(), pos_integer()) -> ok.
increment(Metric, Count) when is_atom(Metric), is_integer(Count), Count > 0 ->
    ensure_table_exists(),
    Key = {counter, Metric},
    try
        ets:update_counter(?METRICS_TABLE, Key, Count)
    catch
        error:badarg ->
            ets:insert(?METRICS_TABLE, {Key, Count})
    end,
    ok.

%% @doc Record a latency measurement in milliseconds
-spec record_latency(atom(), number()) -> ok.
record_latency(Operation, Latency) when is_atom(Operation), is_number(Latency) ->
    ensure_table_exists(),
    Timestamp = erlang:system_time(millisecond),
    
    % Store individual latency measurement
    LatencyKey = {latency, Operation, Timestamp},
    ets:insert(?METRICS_TABLE, {LatencyKey, Latency}),
    
    % Update aggregate stats
    CountKey = {latency_count, Operation},
    SumKey = {latency_sum, Operation},
    MinKey = {latency_min, Operation},
    MaxKey = {latency_max, Operation},
    
    try
        % Increment count
        ets:update_counter(?METRICS_TABLE, CountKey, 1),
        % Add to sum
        ets:update_counter(?METRICS_TABLE, SumKey, round(Latency)),
        % Update min
        case ets:lookup(?METRICS_TABLE, MinKey) of
            [{MinKey, CurrentMin}] when Latency < CurrentMin ->
                ets:insert(?METRICS_TABLE, {MinKey, Latency});
            [] ->
                ets:insert(?METRICS_TABLE, {MinKey, Latency});
            _ -> ok
        end,
        % Update max
        case ets:lookup(?METRICS_TABLE, MaxKey) of
            [{MaxKey, CurrentMax}] when Latency > CurrentMax ->
                ets:insert(?METRICS_TABLE, {MaxKey, Latency});
            [] ->
                ets:insert(?METRICS_TABLE, {MaxKey, Latency});
            _ -> ok
        end
    catch
        error:badarg ->
            % Initialize if not exists
            ets:insert(?METRICS_TABLE, {CountKey, 1}),
            ets:insert(?METRICS_TABLE, {SumKey, round(Latency)}),
            ets:insert(?METRICS_TABLE, {MinKey, Latency}),
            ets:insert(?METRICS_TABLE, {MaxKey, Latency})
    end,
    
    % Cleanup old latency measurements (keep last 1000)
    cleanup_old_latencies(Operation),
    ok.

%% @doc Get all current statistics
-spec get_stats() -> map().
get_stats() ->
    ensure_table_exists(),
    
    % Get counters
    Counters = get_counters(),
    
    % Get latency stats
    LatencyStats = get_latency_stats(),
    
    % Get system info
    SystemInfo = #{
        uptime_ms => get_uptime(),
        memory_total => erlang:memory(total),
        process_count => erlang:system_info(process_count)
    },
    
    #{
        counters => Counters,
        latencies => LatencyStats,
        system => SystemInfo,
        timestamp => erlang:system_time(millisecond)
    }.

%% @doc Reset all metrics
-spec reset() -> ok.
reset() ->
    case ets:info(?METRICS_TABLE) of
        undefined -> ok;
        _ ->
            ets:delete_all_objects(?METRICS_TABLE),
            init_counters(),
            ok
    end.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private
ensure_table_exists() ->
    case ets:info(?METRICS_TABLE) of
        undefined -> start();
        _ -> ok
    end.

%% @private
init_counters() ->
    % Initialize basic counters
    ets:insert(?METRICS_TABLE, [
        {{counter, request_count}, 0},
        {{counter, error_count}, 0},
        {{counter, success_count}, 0},
        {start_time, erlang:system_time(millisecond)}
    ]).

%% @private
get_counters() ->
    Pattern = {{counter, '$1'}, '$2'},
    Counters = ets:match(?METRICS_TABLE, Pattern),
    maps:from_list([{Counter, Value} || [Counter, Value] <- Counters]).

%% @private
get_latency_stats() ->
    % Get all operations that have latency data
    Pattern = {{latency_count, '$1'}, '$2'},
    Operations = ets:match(?METRICS_TABLE, Pattern),
    
    maps:from_list([
        {Operation, get_operation_latency_stats(Operation)} 
        || [Operation, _] <- Operations
    ]).

%% @private
get_operation_latency_stats(Operation) ->
    CountKey = {latency_count, Operation},
    SumKey = {latency_sum, Operation},
    MinKey = {latency_min, Operation},
    MaxKey = {latency_max, Operation},
    
    Count = case ets:lookup(?METRICS_TABLE, CountKey) of
        [{CountKey, C}] -> C;
        [] -> 0
    end,
    
    Sum = case ets:lookup(?METRICS_TABLE, SumKey) of
        [{SumKey, S}] -> S;
        [] -> 0
    end,
    
    Min = case ets:lookup(?METRICS_TABLE, MinKey) of
        [{MinKey, MinValue}] -> MinValue;
        [] -> 0
    end,
    
    Max = case ets:lookup(?METRICS_TABLE, MaxKey) of
        [{MaxKey, MaxValue}] -> MaxValue;
        [] -> 0
    end,
    
    Avg = case Count of
        0 -> 0;
        _ -> Sum / Count
    end,
    
    #{
        count => Count,
        sum_ms => Sum,
        min_ms => Min,
        max_ms => Max,
        avg_ms => Avg
    }.

%% @private
cleanup_old_latencies(Operation) ->
    % Get all latency measurements for this operation
    Pattern = {{latency, Operation, '$1'}, '$2'},
    Measurements = ets:match(?METRICS_TABLE, Pattern),
    
    % Sort by timestamp (newest first)
    Sorted = lists:sort(fun([T1, _], [T2, _]) -> T1 >= T2 end, Measurements),
    
    % Keep only the newest 1000, delete the rest
    case length(Sorted) > 1000 of
        true ->
            ToDelete = lists:nthtail(1000, Sorted),
            [ets:delete(?METRICS_TABLE, {latency, Operation, Timestamp}) 
             || [Timestamp, _] <- ToDelete];
        false ->
            ok
    end.

%% @private
get_uptime() ->
    case ets:lookup(?METRICS_TABLE, start_time) of
        [{start_time, StartTime}] ->
            erlang:system_time(millisecond) - StartTime;
        [] -> 0
    end.

%%====================================================================
%% Convenience Functions
%%====================================================================

%% Common metric patterns for erlmcp

%% @doc Increment request counter
-spec request() -> ok.
request() ->
    increment(request_count, 1).

%% @doc Increment error counter  
-spec error() -> ok.
error() ->
    increment(error_count, 1).

%% @doc Increment success counter
-spec success() -> ok.
success() ->
    increment(success_count, 1).

