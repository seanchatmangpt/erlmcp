%%%-------------------------------------------------------------------
%%% @doc Performance Validator - Joe Armstrong's Philosophy
%%%
%%% "PERFORMANCE IS REAL MEASUREMENTS, NOT GUESSES."
%%% - Actually run the code thousands of times
%%% - Measure it
%%% - Report the TRUTH
%%%
%%% Features:
%%% - Benchmark operations with REAL load
%%% - Stress test to ACTUAL failure point
%%% - Detect memory leaks (1M operations)
%%% - Compare against targets with real data
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_performance_validator).

-behaviour(gen_server).

%% Client API
-export([start_link/0, start_link/1]).
-export([benchmark/2, validate_performance/2]).
-export([stress_test/2, detect_memory_leak/2]).
-export([run_all_benchmarks/0, run_all_benchmarks/1]).
-export([get_benchmark_report/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("erlmcp.hrl").

%%%===================================================================
%%% Constants & Macros
%%%===================================================================

-define(SERVER, ?MODULE).
-define(DEFAULT_WARMUP_ITERATIONS, 100).
-define(MEMORY_LEAK_ITERATIONS, 1000000).  % 1M operations
-define(MEMORY_LEAK_THRESHOLD_MB, 10).    % 10MB threshold
-define(STRESS_TEST_MAX_ITERATIONS, 10000000).  % 10M max

%%%===================================================================
%%% Types
%%%===================================================================

-type benchmark_name() :: binary().
-type operation() :: fun(()).
-type benchmark_result() :: #{
    iterations => integer(),
    duration_us => integer(),
    throughput_per_sec => float(),
    latency_p50 => integer(),
    latency_p95 => integer(),
    latency_p99 => integer(),
    latency_p999 => integer(),
    min_latency_us => integer(),
    max_latency_us => integer()
}.
-type validation_result() :: #{
    benchmark_name => binary(),
    passed => boolean(),
    target_ms => number(),
    actual_p50_ms => number(),
    actual_p95_ms => number(),
    actual_p99_ms => number(),
    throughput_per_sec => float(),
    metrics => benchmark_result()
}.
-type stress_result() :: #{
    benchmark_name => binary(),
    breaking_point_iterations => integer(),
    breaking_point_reason => binary(),
    max_achieved_throughput => float(),
    total_duration_us => integer()
}.
-type memory_leak_result() :: #{
    benchmark_name => binary(),
    has_leak => boolean(),
    memory_before_mb => float(),
    memory_after_mb => float(),
    memory_diff_mb => float(),
    iterations => integer()
}.

-record(state, {
    benchmark_results = #{} :: #{benchmark_name() => benchmark_result()},
    stress_results = #{} :: #{benchmark_name() => stress_result()},
    memory_leak_results = #{} :: #{benchmark_name() => memory_leak_result()}
}).

%%%===================================================================
%%% Performance Targets (from MCP spec)
%%%===================================================================

%% Performance targets for various operations
-define(TARGET_SUBSCRIPTION_NOTIFICATION_P95_MS, 100).
-define(TARGET_SESSION_OPERATION_P95_MS, 5).
-define(TARGET_SECRET_FETCH_CACHED_P95_MS, 50).
-define(TARGET_SECRET_FETCH_UNCACHED_P95_MS, 500).
-define(TARGET_TOOL_CALL_P95_MS, 100).
-define(TARGET_RESOURCE_READ_P95_MS, 50).

%%%===================================================================
%%% Client API
%%%===================================================================

%% @doc Start the performance validator.
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link(#{}).

-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Options) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Options, []).

%% @doc Benchmark an operation with REAL measurements.
%% Operation is a zero-arity fun to measure.
%% Config is a map with:
%%   - iterations: Number of iterations (default 10000)
%%   - warmup: Number of warmup iterations (default 100)
%%   - name: Benchmark name for reporting (optional)
-spec benchmark(operation(), map()) -> {ok, benchmark_result()}.
benchmark(Operation, Config) when is_function(Operation, 0), is_map(Config) ->
    Iterations = maps:get(iterations, Config, 10000),
    Warmup = maps:get(warmup, Config, ?DEFAULT_WARMUP_ITERATIONS),
    
    %% Warmup - Joe said: "WARMUP IS REAL"
    do_warmup(Operation, Warmup),
    
    %% ACTUAL BENCHMARK - Measure each iteration
    {Latencies, TotalDurationUs} = measure_latencies(Operation, Iterations),
    
    %% Calculate REAL metrics
    Result = compute_metrics(Latencies, TotalDurationUs, Iterations),
    {ok, Result}.

%% @doc Validate performance against targets.
%% Returns validation result with pass/fail for each target.
-spec validate_performance(operation(), map()) -> {ok, validation_result()}.
validate_performance(Operation, Config) when is_function(Operation, 0), is_map(Config) ->
    Name = maps:get(name, Config, <<"unnamed_benchmark">>),
    TargetP95Ms = maps:get(target_p95_ms, Config, ?TARGET_TOOL_CALL_P95_MS),
    
    %% Run benchmark
    {ok, Metrics} = benchmark(Operation, Config),
    
    %% Check against targets
    P50Ms = maps:get(latency_p50, Metrics) / 1000.0,
    P95Ms = maps:get(latency_p95, Metrics) / 1000.0,
    P99Ms = maps:get(latency_p99, Metrics) / 1000.0,
    Throughput = maps:get(throughput_per_sec, Metrics),
    
    Passed = P95Ms =< TargetP95Ms,
    
    Result = #{
        benchmark_name => Name,
        passed => Passed,
        target_ms => TargetP95Ms,
        actual_p50_ms => P50Ms,
        actual_p95_ms => P95Ms,
        actual_p99_ms => P99Ms,
        throughput_per_sec => Throughput,
        metrics => Metrics
    },
    {ok, Result}.

%% @doc Stress test to find ACTUAL breaking point.
%% Ramp up load until it FAILS. Report WHAT failed.
-spec stress_test(operation(), map()) -> {ok, stress_result()}.
stress_test(Operation, Config) when is_function(Operation, 0), is_map(Config) ->
    Name = maps:get(name, Config, <<"unnamed_stress_test">>),
    StartIterations = maps:get(start_iterations, Config, 1000),
    Multiplier = maps:get(multiplier, Config, 10),
    MaxIterations = maps:get(max_iterations, Config, ?STRESS_TEST_MAX_ITERATIONS),
    
    %% Warmup first
    do_warmup(Operation, 100),
    
    %% Find breaking point
    Result = find_breaking_point(Operation, Name, StartIterations, Multiplier, MaxIterations),
    {ok, Result}.

%% @doc Detect memory leaks by running 1M operations.
-spec detect_memory_leak(operation(), map()) -> {ok, memory_leak_result()}.
detect_memory_leak(Operation, Config) when is_function(Operation, 0), is_map(Config) ->
    Name = maps:get(name, Config, <<"unnamed_memory_test">>),
    Iterations = maps:get(iterations, Config, ?MEMORY_LEAK_ITERATIONS),
    
    %% Force garbage collection before measurement
    garbage_collect(),
    MemoryBefore = get_memory_mb(),
    
    %% Run many iterations
    {TimeUs, _} = timer:tc(fun() ->
        lists:foreach(fun(_) -> Operation() end, lists:seq(1, Iterations))
    end),
    
    %% Force GC and measure again
    garbage_collect(),
    MemoryAfter = get_memory_mb(),
    
    MemoryDiff = MemoryAfter - MemoryBefore,
    HasLeak = MemoryDiff > ?MEMORY_LEAK_THRESHOLD_MB,
    
    Result = #{
        benchmark_name => Name,
        has_leak => HasLeak,
        memory_before_mb => MemoryBefore,
        memory_after_mb => MemoryAfter,
        memory_diff_mb => MemoryDiff,
        iterations => Iterations
    },
    {ok, Result}.

%% @doc Run all standard benchmarks.
-spec run_all_benchmarks() -> {ok, [validation_result()]}.
run_all_benchmarks() ->
    run_all_benchmarks(#{}).

%% @doc Run all benchmarks with custom config.
-spec run_all_benchmarks(map()) -> {ok, [validation_result()]}.
run_all_benchmarks(Config) ->
    Results = [
        benchmark_registry_lookup(Config),
        benchmark_json_encode_small(Config),
        benchmark_json_encode_medium(Config),
        benchmark_json_decode_small(Config),
        benchmark_json_decode_medium(Config),
        benchmark_session_create(Config),
        benchmark_tool_call(Config),
        benchmark_resource_read(Config),
        benchmark_progress_update(Config),
        benchmark_subscription_notify(Config)
    ],
    {ok, [R || {ok, R} <- Results]}.

%% @doc Get detailed benchmark report.
-spec get_benchmark_report(binary()) -> {ok, map()} | {error, not_found}.
get_benchmark_report(Name) ->
    gen_server:call(?SERVER, {get_report, Name}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init(_Options) ->
    logger:info("Performance validator started"),
    {ok, #state{}}.

handle_call({get_report, Name}, _From, State) ->
    Result = case maps:get(Name, State#state.benchmark_results, undefined) of
        undefined -> {error, not_found};
        Metrics -> {ok, #{metrics => Metrics}}
    end,
    {reply, Result, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal Functions - Benchmark Implementations
%%%===================================================================

%% @private Warmup operation - Joe Armstrong: "WARMUP IS REAL"
do_warmup(_Operation, 0) -> ok;
do_warmup(Operation, Warmup) when Warmup > 0 ->
    Operation(),
    do_warmup(Operation, Warmup - 1).

%% @private Measure latencies for each iteration
measure_latencies(Operation, Iterations) ->
    %% Measure each individual operation
    {Times, Latencies} = timer:tc(fun() ->
        measure_each(Operation, Iterations, [])
    end),
    {lists:reverse(Latencies), Times}.

measure_each(_Operation, 0, Acc) ->
    Acc;
measure_each(Operation, Count, Acc) ->
    {TimeUs, _} = timer:tc(Operation),
    measure_each(Operation, Count - 1, [TimeUs | Acc]).

%% @private Compute metrics from raw latencies
compute_metrics(Latencies, TotalDurationUs, Iterations) ->
    Sorted = lists:sort(Latencies),
    Len = length(Sorted),
    
    MinLatency = hd(Sorted),
    MaxLatency = lists:last(Sorted),
    
    %% Percentiles
    P50 = percentile(Sorted, Len, 50),
    P95 = percentile(Sorted, Len, 95),
    P99 = percentile(Sorted, Len, 99),
    P999 = percentile(Sorted, Len, 99.9),
    
    %% Throughput (operations per second)
    Throughput = (Iterations * 1000000) / max(1, TotalDurationUs),
    
    #{
        iterations => Iterations,
        duration_us => TotalDurationUs,
        throughput_per_sec => Throughput,
        latency_p50 => P50,
        latency_p95 => P95,
        latency_p99 => P99,
        latency_p999 => P999,
        min_latency_us => MinLatency,
        max_latency_us => MaxLatency
    }.

%% @private Calculate percentile from sorted list
percentile(Sorted, Len, P) ->
    Index = max(1, min(Len, round((P / 100) * Len))),
    lists:nth(Index, Sorted).

%% @private Find breaking point in stress test
find_breaking_point(Operation, Name, Iterations, Multiplier, MaxIterations) ->
    case Iterations > MaxIterations of
        true ->
            #{
                benchmark_name => Name,
                breaking_point_iterations => MaxIterations,
                breaking_point_reason => <<"Max iterations reached without failure">>,
                max_achieved_throughput => 0.0,
                total_duration_us => 0
            };
        false ->
            case try_run_stress(Operation, Iterations) of
                {ok, DurationUs} ->
                    %% Success - ramp up
                    Throughput = (Iterations * 1000000) / DurationUs,
                    logger:info("Stress test passed at ~p iterations: ~.2f ops/sec", 
                               [Iterations, Throughput]),
                    find_breaking_point(Operation, Name, Iterations * Multiplier, 
                                       Multiplier, MaxIterations);
                {error, Reason} ->
                    %% Found breaking point
                    #{
                        benchmark_name => Name,
                        breaking_point_iterations => Iterations,
                        breaking_point_reason => format_failure_reason(Reason),
                        max_achieved_throughput => 0.0,
                        total_duration_us => 0
                    }
            end
    end.

%% @private Try to run stress test iteration
try_run_stress(Operation, Iterations) ->
    try
        {DurationUs, _} = timer:tc(fun() ->
            lists:foreach(fun(_) -> Operation() end, lists:seq(1, Iterations))
        end),
        {ok, DurationUs}
    catch
        Type:Reason:Stack ->
            logger:warning("Stress test failed at ~p iterations: ~p:~p~n~p", 
                          [Iterations, Type, Reason, Stack]),
            {error, {Type, Reason}}
    end.

%% @private Format failure reason
format_failure_reason({Type, Reason}) ->
    BinType = atom_to_binary(Type, utf8),
    BinReason = io_lib:format("~p", [Reason]),
    <<BinType/binary, ": ", BinReason/binary>>.

%% @private Get current memory usage in MB
get_memory_mb() ->
    Memory = erlang:memory(total),
    Memory / 1024 / 1024.

%%%===================================================================
%%% Benchmark Implementations
%%%===================================================================

%% @private Benchmark: Registry lookup
benchmark_registry_lookup(Config) ->
    {ok, RegistryPid} = erlmcp_registry:start_link(),
    
    %% Register a test process
    TestPid = self(),
    erlmcp_registry:register_name(<<"test_process">>, TestPid),
    
    Operation = fun() ->
        erlmcp_registry:lookup_name(<<"test_process">>)
    end,
    
    Result = validate_performance(Operation, Config#{
        name => <<"registry_lookup">>,
        target_p95_ms => ?TARGET_SESSION_OPERATION_P95_MS
    }),
    
    gen_server:stop(RegistryPid),
    Result.

%% @private Benchmark: JSON encode small message (10 fields)
benchmark_json_encode_small(Config) ->
    Message = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 1,
        <<"method">> => <<"tools/call">>,
        <<"params">> => #{
            <<"name">> => <<"test_tool">>,
            <<"arguments">> => #{}
        }
    },
    
    Operation = fun() ->
        erlmcp_json_rpc:encode_request(Message)
    end,
    
    validate_performance(Operation, Config#{
        name => <<"json_encode_small">>,
        target_p95_ms => ?TARGET_SESSION_OPERATION_P95_MS
    }).

%% @private Benchmark: JSON encode medium message (50 fields)
benchmark_json_encode_medium(Config) ->
    Arguments = lists:foldl(fun(I, Acc) ->
        Key = <<"arg", (integer_to_binary(I))/binary>>,
        maps:put(Key, <<"value", (integer_to_binary(I))/binary>>, Acc)
    end, #{}, lists:seq(1, 50)),
    
    Message = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 1,
        <<"method">> => <<"tools/call">>,
        <<"params">> => #{
            <<"name">> => <<"medium_tool">>,
            <<"arguments">> => Arguments
        }
    },
    
    Operation = fun() ->
        erlmcp_json_rpc:encode_request(Message)
    end,
    
    validate_performance(Operation, Config#{
        name => <<"json_encode_medium">>,
        target_p95_ms => ?TARGET_SESSION_OPERATION_P95_MS * 2
    }).

%% @private Benchmark: JSON decode small message
benchmark_json_decode_small(Config) ->
    JSON = <<"{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"tools/call\",\"params\":{\"name\":\"test_tool\",\"arguments\":{}}}">>,
    
    Operation = fun() ->
        erlmcp_json_rpc:decode_request(JSON)
    end,
    
    validate_performance(Operation, Config#{
        name => <<"json_decode_small">>,
        target_p95_ms => ?TARGET_SESSION_OPERATION_P95_MS
    }).

%% @private Benchmark: JSON decode medium message
benchmark_json_decode_medium(Config) ->
    %% Build medium JSON
    Arguments = lists:foldl(fun(I, Acc) ->
        Acc ++ [<<"\"arg", (integer_to_binary(I))/binary, "\":\"value", (integer_to_binary(I))/binary, "\">>]
    end, [], lists:seq(1, 50)),
    
    ArgsStr = binary:list_to_bin(lists:join(<<",">>, Arguments)),
    JSON = <<"{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"tools/call\",\"params\":{\"name\":\"medium_tool\",\"arguments\":{", ArgsStr/binary, "}}}">>,
    
    Operation = fun() ->
        erlmcp_json_rpc:decode_request(JSON)
    end,
    
    validate_performance(Operation, Config#{
        name => <<"json_decode_medium">>,
        target_p95_ms => ?TARGET_SESSION_OPERATION_P95_MS * 2
    }).

%% @private Benchmark: Session create
benchmark_session_create(Config) ->
    {ok, SessionPid} = erlmcp_session:start_link(<<"test_session">>),
    
    Operation = fun() ->
        erlmcp_session:set(SessionPid, test_key, test_value)
    end,
    
    Result = validate_performance(Operation, Config#{
        name => <<"session_operation">>,
        target_p95_ms => ?TARGET_SESSION_OPERATION_P95_MS
    }),
    
    gen_server:stop(SessionPid),
    Result.

%% @private Benchmark: Tool call
benchmark_tool_call(Config) ->
    {ok, ServerPid} = erlmcp_server:start_link(test_server, #{}),
    
    %% Add a simple tool
    Handler = fun(_Args) -> #{result => ok} end,
    erlmcp_server:add_tool(ServerPid, <<"test_tool">>, <<"Test tool">>, Handler),
    
    Operation = fun() ->
        erlmcp_server:call_tool(ServerPid, <<"test_tool">>, #{})
    end,
    
    Result = validate_performance(Operation, Config#{
        name => <<"tool_call">>,
        target_p95_ms => ?TARGET_TOOL_CALL_P95_MS
    }),
    
    gen_server:stop(ServerPid),
    Result.

%% @private Benchmark: Resource read
benchmark_resource_read(Config) ->
    {ok, ServerPid} = erlmcp_server:start_link(test_server, #{}),
    
    %% Add a simple resource
    Handler = fun(_Uri) -> <<"test content">> end,
    erlmcp_server:add_resource(ServerPid, <<"test://resource">>, <<"Test Resource">>, Handler),
    
    Operation = fun() ->
        erlmcp_server:read_resource(ServerPid, <<"test://resource">>)
    end,
    
    Result = validate_performance(Operation, Config#{
        name => <<"resource_read">>,
        target_p95_ms => ?TARGET_RESOURCE_READ_P95_MS
    }),
    
    gen_server:stop(ServerPid),
    Result.

%% @private Benchmark: Progress update
benchmark_progress_update(Config) ->
    {ok, ProgressPid} = erlmcp_progress:start_link(),
    Token = erlmcp_progress:generate_token(),
    erlmcp_progress:create(self(), <<"Test progress">>),
    
    Operation = fun() ->
        erlmcp_progress:update(Token, #{progress => 0.5, total => 1.0})
    end,
    
    Result = validate_performance(Operation, Config#{
        name => <<"progress_update">>,
        target_p95_ms => ?TARGET_SESSION_OPERATION_P95_MS
    }),
    
    gen_server:stop(ProgressPid),
    Result.

%% @private Benchmark: Subscription notification
benchmark_subscription_notify(Config) ->
    {ok, ServerPid} = erlmcp_server:start_link(test_server, #{}),
    
    %% Add a resource and subscribe
    Handler = fun(_Uri) -> <<"test content">> end,
    erlmcp_server:add_resource(ServerPid, <<"test://resource">>, <<"Test Resource">>, Handler),
    {ok, _SubscriptionId} = erlmcp_server:subscribe_resource(ServerPid, <<"test://resource">>),
    
    Operation = fun() ->
        erlmcp_server:notify_resource_updated(ServerPid, <<"test://resource">>, #{})
    end,
    
    Result = validate_performance(Operation, Config#{
        name => <<"subscription_notification">>,
        target_p95_ms => ?TARGET_SUBSCRIPTION_NOTIFICATION_P95_MS
    }),
    
    gen_server:stop(ServerPid),
    Result.
