-module(erlmcp_routing_metrics).

%% Metrics collection and monitoring for the enhanced routing system
%% Implements latency tracking, queue depth monitoring, and performance analytics

-export([
    start_link/0,
    init/0,
    reset/0,
    
    % Counter operations
    increment_counter/1,
    increment_counter/2,
    get_counter/1,
    set_counter/2,
    
    % Latency tracking
    record_latency/2,
    get_latency_stats/1,
    get_latency_histogram/1,
    get_latency_percentiles/1,
    
    % Queue depth monitoring
    update_queue_depth/2,
    get_queue_depth/1,
    get_max_queue_depth/1,
    
    % Error tracking
    record_error/2,
    get_error_rate/1,
    get_error_count/1,
    
    % Performance analytics
    get_throughput/1,
    get_system_metrics/0,
    export_metrics/1,
    
    % Cleanup and maintenance
    cleanup_old_metrics/0,
    compact_metrics/0
]).

-include("erlmcp.hrl").

%% ETS tables for metrics storage
-define(COUNTERS_TABLE, erlmcp_routing_counters).
-define(LATENCIES_TABLE, erlmcp_routing_latencies).
-define(QUEUE_DEPTHS_TABLE, erlmcp_routing_queues).
-define(ERRORS_TABLE, erlmcp_routing_errors).
-define(TIMESTAMPS_TABLE, erlmcp_routing_timestamps).

%% Configuration constants
-define(MAX_LATENCY_SAMPLES, 1000).
-define(MAX_ERROR_SAMPLES, 500).
-define(CLEANUP_INTERVAL, 300000). % 5 minutes
-define(METRIC_RETENTION_TIME, 3600000). % 1 hour

%%====================================================================
%% API Functions
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    case init() of
        ok -> 
            % Start cleanup timer
            erlang:send_after(?CLEANUP_INTERVAL, self(), cleanup_metrics),
            {ok, self()};
        Error -> 
            Error
    end.

-spec init() -> ok | {error, term()}.
init() ->
    try
        % Create ETS tables for metrics storage
        create_metrics_tables(),
        logger:info("Routing metrics system initialized"),
        ok
    catch
        Class:Reason:Stacktrace ->
            logger:error("Failed to initialize routing metrics: ~p:~p~n~p", 
                        [Class, Reason, Stacktrace]),
            {error, {Class, Reason}}
    end.

-spec reset() -> ok.
reset() ->
    lists:foreach(fun(Table) ->
        case ets:info(Table, name) of
            undefined -> ok;
            _ -> ets:delete_all_objects(Table)
        end
    end, [?COUNTERS_TABLE, ?LATENCIES_TABLE, ?QUEUE_DEPTHS_TABLE, 
          ?ERRORS_TABLE, ?TIMESTAMPS_TABLE]),
    logger:info("Routing metrics reset"),
    ok.

%%====================================================================
%% Counter Operations
%%====================================================================

-spec increment_counter(atom()) -> non_neg_integer().
increment_counter(Counter) ->
    increment_counter(Counter, 1).

-spec increment_counter(atom(), non_neg_integer()) -> non_neg_integer().
increment_counter(Counter, Increment) ->
    try
        NewValue = case ets:lookup(?COUNTERS_TABLE, Counter) of
            [{Counter, CurrentValue}] -> CurrentValue + Increment;
            [] -> Increment
        end,
        ets:insert(?COUNTERS_TABLE, {Counter, NewValue}),
        NewValue
    catch
        error:badarg ->
            % Table doesn't exist, create it
            create_metrics_tables(),
            increment_counter(Counter, Increment)
    end.

-spec get_counter(atom()) -> non_neg_integer().
get_counter(Counter) ->
    try
        case ets:lookup(?COUNTERS_TABLE, Counter) of
            [{Counter, Value}] -> Value;
            [] -> 0
        end
    catch
        error:badarg -> 0
    end.

-spec set_counter(atom(), non_neg_integer()) -> ok.
set_counter(Counter, Value) ->
    try
        ets:insert(?COUNTERS_TABLE, {Counter, Value}),
        ok
    catch
        error:badarg ->
            create_metrics_tables(),
            set_counter(Counter, Value)
    end.

%%====================================================================
%% Latency Tracking
%%====================================================================

-spec record_latency(atom(), non_neg_integer()) -> ok.
record_latency(Identifier, LatencyMicros) ->
    try
        Timestamp = erlang:system_time(microsecond),
        
        % Get existing latencies
        Latencies = case ets:lookup(?LATENCIES_TABLE, Identifier) of
            [{Identifier, ExistingLatencies}] -> ExistingLatencies;
            [] -> []
        end,
        
        % Add new latency (keep only last N samples)
        NewLatencies = [{LatencyMicros, Timestamp} | 
                       lists:sublist(Latencies, ?MAX_LATENCY_SAMPLES - 1)],
        
        ets:insert(?LATENCIES_TABLE, {Identifier, NewLatencies}),
        ok
    catch
        error:badarg ->
            create_metrics_tables(),
            record_latency(Identifier, LatencyMicros)
    end.

-spec get_latency_stats(atom()) -> #{
    count => non_neg_integer(),
    avg => float(),
    min => non_neg_integer(),
    max => non_neg_integer(),
    std_dev => float()
}.
get_latency_stats(Identifier) ->
    try
        case ets:lookup(?LATENCIES_TABLE, Identifier) of
            [{Identifier, Latencies}] ->
                calculate_latency_stats(Latencies);
            [] ->
                #{count => 0, avg => 0.0, min => 0, max => 0, std_dev => 0.0}
        end
    catch
        error:badarg ->
            #{count => 0, avg => 0.0, min => 0, max => 0, std_dev => 0.0}
    end.

-spec get_latency_histogram(atom()) -> #{
    buckets => #{non_neg_integer() => non_neg_integer()},
    total_count => non_neg_integer()
}.
get_latency_histogram(Identifier) ->
    try
        case ets:lookup(?LATENCIES_TABLE, Identifier) of
            [{Identifier, Latencies}] ->
                create_latency_histogram([L || {L, _} <- Latencies]);
            [] ->
                #{buckets => #{}, total_count => 0}
        end
    catch
        error:badarg ->
            #{buckets => #{}, total_count => 0}
    end.

-spec get_latency_percentiles(atom()) -> #{
    p50 => float(),
    p95 => float(),
    p99 => float(),
    p999 => float()
}.
get_latency_percentiles(Identifier) ->
    try
        case ets:lookup(?LATENCIES_TABLE, Identifier) of
            [{Identifier, Latencies}] ->
                LatencyValues = [L || {L, _} <- Latencies],
                calculate_percentiles(lists:sort(LatencyValues));
            [] ->
                #{p50 => 0.0, p95 => 0.0, p99 => 0.0, p999 => 0.0}
        end
    catch
        error:badarg ->
            #{p50 => 0.0, p95 => 0.0, p99 => 0.0, p999 => 0.0}
    end.

%%====================================================================
%% Queue Depth Monitoring
%%====================================================================

-spec update_queue_depth(atom(), non_neg_integer()) -> ok.
update_queue_depth(Identifier, Depth) ->
    try
        Timestamp = erlang:system_time(microsecond),
        
        % Store current depth
        ets:insert(?QUEUE_DEPTHS_TABLE, {Identifier, Depth, Timestamp}),
        
        % Update max depth if necessary
        MaxKey = {Identifier, max_depth},
        case ets:lookup(?QUEUE_DEPTHS_TABLE, MaxKey) of
            [{MaxKey, CurrentMax, _}] when Depth > CurrentMax ->
                ets:insert(?QUEUE_DEPTHS_TABLE, {MaxKey, Depth, Timestamp});
            [] ->
                ets:insert(?QUEUE_DEPTHS_TABLE, {MaxKey, Depth, Timestamp});
            _ ->
                ok
        end,
        
        ok
    catch
        error:badarg ->
            create_metrics_tables(),
            update_queue_depth(Identifier, Depth)
    end.

-spec get_queue_depth(atom()) -> non_neg_integer().
get_queue_depth(Identifier) ->
    try
        case ets:lookup(?QUEUE_DEPTHS_TABLE, Identifier) of
            [{Identifier, Depth, _}] -> Depth;
            [] -> 0
        end
    catch
        error:badarg -> 0
    end.

-spec get_max_queue_depth(atom()) -> non_neg_integer().
get_max_queue_depth(Identifier) ->
    try
        MaxKey = {Identifier, max_depth},
        case ets:lookup(?QUEUE_DEPTHS_TABLE, MaxKey) of
            [{MaxKey, MaxDepth, _}] -> MaxDepth;
            [] -> 0
        end
    catch
        error:badarg -> 0
    end.

%%====================================================================
%% Error Tracking
%%====================================================================

-spec record_error(atom(), term()) -> ok.
record_error(Identifier, ErrorReason) ->
    try
        Timestamp = erlang:system_time(microsecond),
        
        % Get existing errors
        Errors = case ets:lookup(?ERRORS_TABLE, Identifier) of
            [{Identifier, ExistingErrors}] -> ExistingErrors;
            [] -> []
        end,
        
        % Add new error (keep only last N samples)
        NewErrors = [{ErrorReason, Timestamp} | 
                    lists:sublist(Errors, ?MAX_ERROR_SAMPLES - 1)],
        
        ets:insert(?ERRORS_TABLE, {Identifier, NewErrors}),
        
        % Increment error counter
        ErrorCounterKey = list_to_atom(atom_to_list(Identifier) ++ "_errors"),
        increment_counter(ErrorCounterKey),
        
        ok
    catch
        error:badarg ->
            create_metrics_tables(),
            record_error(Identifier, ErrorReason)
    end.

-spec get_error_rate(atom()) -> float().
get_error_rate(Identifier) ->
    try
        MessageCounterKey = list_to_atom(atom_to_list(Identifier) ++ "_messages"),
        ErrorCounterKey = list_to_atom(atom_to_list(Identifier) ++ "_errors"),
        
        TotalMessages = get_counter(MessageCounterKey),
        TotalErrors = get_counter(ErrorCounterKey),
        
        case TotalMessages of
            0 -> 0.0;
            _ -> TotalErrors / TotalMessages
        end
    catch
        error:badarg -> 0.0
    end.

-spec get_error_count(atom()) -> non_neg_integer().
get_error_count(Identifier) ->
    try
        case ets:lookup(?ERRORS_TABLE, Identifier) of
            [{Identifier, Errors}] -> length(Errors);
            [] -> 0
        end
    catch
        error:badarg -> 0
    end.

%%====================================================================
%% Performance Analytics
%%====================================================================

-spec get_throughput(atom()) -> float().
get_throughput(Identifier) ->
    try
        MessageCounterKey = list_to_atom(atom_to_list(Identifier) ++ "_messages"),
        
        % Get message count
        Messages = get_counter(MessageCounterKey),
        
        % Get time window (look for first timestamp)
        case ets:first(?TIMESTAMPS_TABLE) of
            '$end_of_table' ->
                0.0;
            FirstKey ->
                case ets:lookup(?TIMESTAMPS_TABLE, FirstKey) of
                    [{FirstKey, StartTime}] ->
                        Now = erlang:system_time(microsecond),
                        DurationSeconds = (Now - StartTime) / 1000000,
                        case DurationSeconds > 0 of
                            true -> Messages / DurationSeconds;
                            false -> 0.0
                        end;
                    [] ->
                        0.0
                end
        end
    catch
        error:badarg -> 0.0
    end.

-spec get_system_metrics() -> #{
    counters => map(),
    latencies => map(),
    queue_depths => map(),
    errors => map(),
    system_info => map()
}.
get_system_metrics() ->
    try
        #{
            counters => get_all_counters(),
            latencies => get_all_latency_stats(),
            queue_depths => get_all_queue_depths(),
            errors => get_all_error_stats(),
            system_info => #{
                memory_usage => erlang:memory(),
                process_count => erlang:system_info(process_count),
                uptime => erlang:statistics(wall_clock)
            }
        }
    catch
        error:badarg ->
            #{counters => #{}, latencies => #{}, queue_depths => #{}, 
              errors => #{}, system_info => #{}}
    end.

-spec export_metrics(json | csv | erlang) -> binary() | string() | term().
export_metrics(Format) ->
    Metrics = get_system_metrics(),
    case Format of
        json ->
            % Simple JSON encoding (you might want to use a proper JSON library)
            encode_json(Metrics);
        csv ->
            encode_csv(Metrics);
        erlang ->
            Metrics
    end.

%%====================================================================
%% Cleanup and Maintenance
%%====================================================================

-spec cleanup_old_metrics() -> ok.
cleanup_old_metrics() ->
    try
        Now = erlang:system_time(microsecond),
        CutoffTime = Now - ?METRIC_RETENTION_TIME * 1000,
        
        % Clean up old latency samples
        cleanup_latency_table(CutoffTime),
        
        % Clean up old error samples
        cleanup_error_table(CutoffTime),
        
        % Clean up old queue depth samples
        cleanup_queue_depth_table(CutoffTime),
        
        logger:debug("Old routing metrics cleaned up"),
        ok
    catch
        error:badarg -> ok
    end.

-spec compact_metrics() -> ok.
compact_metrics() ->
    try
        % Compact latency data (keep only statistical summaries for old data)
        compact_latency_table(),
        
        % Compact error data
        compact_error_table(),
        
        logger:debug("Routing metrics compacted"),
        ok
    catch
        error:badarg -> ok
    end.

%%====================================================================
%% Internal Functions
%%====================================================================

-spec create_metrics_tables() -> ok.
create_metrics_tables() ->
    Tables = [
        {?COUNTERS_TABLE, [named_table, public, set, {write_concurrency, true}]},
        {?LATENCIES_TABLE, [named_table, public, set, {write_concurrency, true}]},
        {?QUEUE_DEPTHS_TABLE, [named_table, public, set, {write_concurrency, true}]},
        {?ERRORS_TABLE, [named_table, public, set, {write_concurrency, true}]},
        {?TIMESTAMPS_TABLE, [named_table, public, ordered_set, {write_concurrency, true}]}
    ],
    
    lists:foreach(fun({TableName, Options}) ->
        case ets:info(TableName, name) of
            undefined ->
                ets:new(TableName, Options);
            _ ->
                ok % Table already exists
        end
    end, Tables),
    
    % Initialize system start timestamp
    StartTime = erlang:system_time(microsecond),
    ets:insert(?TIMESTAMPS_TABLE, {system_start, StartTime}),
    
    ok.

-spec calculate_latency_stats([{non_neg_integer(), non_neg_integer()}]) -> map().
calculate_latency_stats([]) ->
    #{count => 0, avg => 0.0, min => 0, max => 0, std_dev => 0.0};
calculate_latency_stats(Latencies) ->
    Values = [L || {L, _} <- Latencies],
    Count = length(Values),
    Sum = lists:sum(Values),
    Avg = Sum / Count,
    Min = lists:min(Values),
    Max = lists:max(Values),
    
    % Calculate standard deviation
    Variance = lists:sum([math:pow(V - Avg, 2) || V <- Values]) / Count,
    StdDev = math:sqrt(Variance),
    
    #{
        count => Count,
        avg => Avg,
        min => Min,
        max => Max,
        std_dev => StdDev
    }.

-spec create_latency_histogram([non_neg_integer()]) -> map().
create_latency_histogram([]) ->
    #{buckets => #{}, total_count => 0};
create_latency_histogram(Latencies) ->
    % Define histogram buckets (in microseconds)
    Buckets = [100, 500, 1000, 5000, 10000, 50000, 100000, 500000, 1000000],
    
    BucketCounts = lists:foldl(fun(Latency, Acc) ->
        Bucket = find_bucket(Latency, Buckets),
        maps:update_with(Bucket, fun(Count) -> Count + 1 end, 1, Acc)
    end, #{}, Latencies),
    
    #{
        buckets => BucketCounts,
        total_count => length(Latencies)
    }.

-spec find_bucket(non_neg_integer(), [non_neg_integer()]) -> non_neg_integer().
find_bucket(Value, [Bucket | Rest]) when Value =< Bucket ->
    Bucket;
find_bucket(Value, [_ | Rest]) ->
    find_bucket(Value, Rest);
find_bucket(Value, []) ->
    % Value is larger than all buckets, use "infinity" bucket
    infinity.

-spec calculate_percentiles([non_neg_integer()]) -> map().
calculate_percentiles([]) ->
    #{p50 => 0.0, p95 => 0.0, p99 => 0.0, p999 => 0.0};
calculate_percentiles(SortedValues) ->
    Length = length(SortedValues),
    
    P50Index = max(1, trunc(Length * 0.5)),
    P95Index = max(1, trunc(Length * 0.95)),
    P99Index = max(1, trunc(Length * 0.99)),
    P999Index = max(1, trunc(Length * 0.999)),
    
    #{
        p50 => lists:nth(P50Index, SortedValues) / 1.0,
        p95 => lists:nth(P95Index, SortedValues) / 1.0,
        p99 => lists:nth(P99Index, SortedValues) / 1.0,
        p999 => lists:nth(P999Index, SortedValues) / 1.0
    }.

-spec get_all_counters() -> map().
get_all_counters() ->
    try
        ets:foldl(fun({Key, Value}, Acc) ->
            maps:put(Key, Value, Acc)
        end, #{}, ?COUNTERS_TABLE)
    catch
        error:badarg -> #{}
    end.

-spec get_all_latency_stats() -> map().
get_all_latency_stats() ->
    try
        ets:foldl(fun({Key, Latencies}, Acc) ->
            Stats = calculate_latency_stats(Latencies),
            maps:put(Key, Stats, Acc)
        end, #{}, ?LATENCIES_TABLE)
    catch
        error:badarg -> #{}
    end.

-spec get_all_queue_depths() -> map().
get_all_queue_depths() ->
    try
        ets:foldl(fun
            ({{Key, max_depth}, MaxDepth, _}, Acc) ->
                Current = maps:get(Key, Acc, #{current => 0, max => 0}),
                maps:put(Key, Current#{max => MaxDepth}, Acc);
            ({Key, Depth, _}, Acc) when is_atom(Key) ->
                Current = maps:get(Key, Acc, #{current => 0, max => 0}),
                maps:put(Key, Current#{current => Depth}, Acc);
            (_, Acc) ->
                Acc
        end, #{}, ?QUEUE_DEPTHS_TABLE)
    catch
        error:badarg -> #{}
    end.

-spec get_all_error_stats() -> map().
get_all_error_stats() ->
    try
        ets:foldl(fun({Key, Errors}, Acc) ->
            ErrorCount = length(Errors),
            RecentErrors = [E || {E, T} <- Errors, 
                               erlang:system_time(microsecond) - T < 300000000], % 5 minutes
            maps:put(Key, #{
                total_count => ErrorCount,
                recent_count => length(RecentErrors)
            }, Acc)
        end, #{}, ?ERRORS_TABLE)
    catch
        error:badarg -> #{}
    end.

-spec cleanup_latency_table(non_neg_integer()) -> ok.
cleanup_latency_table(CutoffTime) ->
    try
        ets:foldl(fun({Key, Latencies}, _) ->
            FilteredLatencies = [{L, T} || {L, T} <- Latencies, T >= CutoffTime],
            case FilteredLatencies of
                [] -> ets:delete(?LATENCIES_TABLE, Key);
                _ -> ets:insert(?LATENCIES_TABLE, {Key, FilteredLatencies})
            end
        end, ok, ?LATENCIES_TABLE)
    catch
        error:badarg -> ok
    end.

-spec cleanup_error_table(non_neg_integer()) -> ok.
cleanup_error_table(CutoffTime) ->
    try
        ets:foldl(fun({Key, Errors}, _) ->
            FilteredErrors = [{E, T} || {E, T} <- Errors, T >= CutoffTime],
            case FilteredErrors of
                [] -> ets:delete(?ERRORS_TABLE, Key);
                _ -> ets:insert(?ERRORS_TABLE, {Key, FilteredErrors})
            end
        end, ok, ?ERRORS_TABLE)
    catch
        error:badarg -> ok
    end.

-spec cleanup_queue_depth_table(non_neg_integer()) -> ok.
cleanup_queue_depth_table(CutoffTime) ->
    try
        ets:foldl(fun
            ({{_, max_depth}, _, T}, _) when T < CutoffTime ->
                ok; % Keep max depth entries
            ({_, _, T}, _) when T < CutoffTime ->
                ok; % Could delete old current depth entries
            (_, _) ->
                ok
        end, ok, ?QUEUE_DEPTHS_TABLE)
    catch
        error:badarg -> ok
    end.

-spec compact_latency_table() -> ok.
compact_latency_table() ->
    % Keep only statistical summaries for identifiers with many samples
    try
        ets:foldl(fun({Key, Latencies}, _) ->
            case length(Latencies) > ?MAX_LATENCY_SAMPLES * 2 of
                true ->
                    % Keep only recent half of samples
                    CompactedLatencies = lists:sublist(Latencies, ?MAX_LATENCY_SAMPLES),
                    ets:insert(?LATENCIES_TABLE, {Key, CompactedLatencies});
                false ->
                    ok
            end
        end, ok, ?LATENCIES_TABLE)
    catch
        error:badarg -> ok
    end.

-spec compact_error_table() -> ok.
compact_error_table() ->
    try
        ets:foldl(fun({Key, Errors}, _) ->
            case length(Errors) > ?MAX_ERROR_SAMPLES * 2 of
                true ->
                    CompactedErrors = lists:sublist(Errors, ?MAX_ERROR_SAMPLES),
                    ets:insert(?ERRORS_TABLE, {Key, CompactedErrors});
                false ->
                    ok
            end
        end, ok, ?ERRORS_TABLE)
    catch
        error:badarg -> ok
    end.

% Simple JSON encoding (for basic metrics export)
-spec encode_json(term()) -> binary().
encode_json(Term) ->
    % Use jsx for proper JSON encoding
    jsx:encode(Term).

% Simple CSV encoding
-spec encode_csv(map()) -> string().
encode_csv(Metrics) ->
    % Basic CSV format for metrics
    "metric_type,identifier,value,timestamp\n" ++
    encode_csv_counters(maps:get(counters, Metrics, #{})) ++
    encode_csv_latencies(maps:get(latencies, Metrics, #{})).

-spec encode_csv_counters(map()) -> string().
encode_csv_counters(Counters) ->
    maps:fold(fun(Key, Value, Acc) ->
        Line = io_lib:format("counter,~p,~p,~p~n", 
                           [Key, Value, erlang:system_time(microsecond)]),
        Acc ++ lists:flatten(Line)
    end, "", Counters).

-spec encode_csv_latencies(map()) -> string().
encode_csv_latencies(Latencies) ->
    maps:fold(fun(Key, Stats, Acc) ->
        Avg = maps:get(avg, Stats, 0.0),
        Line = io_lib:format("latency,~p,~p,~p~n", 
                           [Key, Avg, erlang:system_time(microsecond)]),
        Acc ++ lists:flatten(Line)
    end, "", Latencies).