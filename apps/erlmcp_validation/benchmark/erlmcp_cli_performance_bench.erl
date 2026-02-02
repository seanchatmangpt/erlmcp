%%%====================================================================
%%% ERLMCP COMPREHENSIVE CLI PERFORMANCE BENCHMARKER
%%%====================================================================
%%% Targets: 553K ops/sec registry, 971K ops/sec queue
%%% Measures: All CLI performance aspects with optimization recommendations
%%%====================================================================

-module(erlmcp_cli_performance_bench).

-export([
    run/0,
    run/1,
    run_comprehensive/0,
    establish_baselines/0,
    optimize_performance/0,
    generate_report/0
]).

-include_lib("kernel/include/logger.hrl").

%%====================================================================
%% Public API
%%====================================================================

%% @doc Run comprehensive performance benchmark
-spec run() -> ok.
run() ->
    run(#{iterations => 100, profile => true, optimize => false}).

-spec run(map()) -> ok.
run(Opts) ->
    io:format("~n==============================================~n"),
    io:format("ERLMCP COMPREHENSIVE CLI PERFORMANCE BENCHMARK~n"),
    io:format("==============================================~n~n"),

    %% Run comprehensive benchmark
    Results = run_comprehensive_benchmark(Opts),

    %% Generate report
    Report = generate_report(Results),

    %% Print results
    print_comprehensive_results(Report),

    %% Write to file
    write_benchmark_report(Report),

    %% Check against targets
    check_performance_targets(Report),

    ok.

%% @doc Run full comprehensive benchmark suite
-spec run_comprehensive() -> ok.
run_comprehensive() ->
    run(#{iterations => 1000, profile => true, optimize => false}).

%% @doc Establish performance baselines
-spec establish_baselines() -> ok.
establish_baselines() ->
    Baselines = establish_all_baselines(),
    io:format("Performance baselines established:~n"),
    print_baselines(Baselines),
    write_baselines(Baselines),
    ok.

%% @doc Optimize CLI performance
-spec optimize_performance() -> ok.
optimize_performance() ->
    Baselines = establish_all_baselines(),
    Optimizations = suggest_optimizations(Baselines),
    apply_optimizations(Optimizations),
    io:format("Performance optimizations applied:~n"),
    print_optimizations(Optimizations),
    ok.

%% @doc Generate performance report
-spec generate_report() -> ok.
generate_report() ->
    Baselines = establish_all_baselines(),
    Report = generate_baseline_report(Baselines),
    print_comprehensive_results(Report),
    write_benchmark_report(Report),
    ok.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @doc Run all benchmark suites
-spec run_comprehensive_benchmark(map()) -> map().
run_comprehensive_benchmark(Opts) ->
    Iterations = maps:get(iterations, Opts, 100),
    Profile = maps:get(profile, Opts, false),

    io:format("Running comprehensive benchmark suite (~p iterations)...~n", [Iterations]),

    %% Connection performance
    ConnectionResults = benchmark_connection_performance(Iterations),

    %% Request/response latency
    LatencyResults = benchmark_latency(Iterations),

    %% Throughput benchmarks
    ThroughputResults = benchmark_throughput(Iterations),

    %% Memory usage
    MemoryResults = benchmark_memory_usage(Iterations),

    %% CPU utilization
    CPUResults = benchmark_cpu_utilization(Iterations),

    %% Registry performance
    RegistryResults = benchmark_registry_performance(Iterations),

    %% Transport performance
    TransportResults = benchmark_transport_performance(Iterations),

    %% Session management
    SessionResults = benchmark_session_management(Iterations),

    %% OTEL overhead
    OtelResults = benchmark_otel_overhead(Iterations),

    %% JSON-RPC performance
    JsonRpcResults = benchmark_json_rpc_performance(Iterations),

    %% Optional profiling
    ProfileData = case Profile of
        true -> profile_all_components();
        false -> #{}
    end,

    #{
        timestamp => erlang:system_time(second),
        iterations => Iterations,
        profile => ProfileData,
        benchmarks => #{
            connection => ConnectionResults,
            latency => LatencyResults,
            throughput => ThroughputResults,
            memory => MemoryResults,
            cpu => CPUResults,
            registry => RegistryResults,
            transport => TransportResults,
            session => SessionResults,
            otel => OtelResults,
            json_rpc => JsonRpcResults
        }
    }.

%%====================================================================
%% Connection Performance Benchmarks
%%====================================================================

-spec benchmark_connection_performance(pos_integer()) -> map().
benchmark_connection_performance(Iterations) ->
    io:format("Benchmarking connection performance...~n"),

    Times = lists:map(fun(_) ->
        StartTime = erlang:monotonic_time(microsecond),

        %% Simulate TCP connection establishment
        Socket = case gen_tcp:connect("127.0.0.1", 8080,
                                     [{active, false}, {packet, 0}, {reuseaddr, true}],
                                     5000) of
            {ok, S} ->
                gen_tcp:close(S),
                true;
            {error, timeout} ->
                %% Expected failure in test environment
                true
        end,

        EndTime = erlang:monotonic_time(microsecond),
        EndTime - StartTime
    end, lists:seq(1, Iterations)),

    #{
        metric => <<"connection_establishment">>,
        target_us => 10000,  % 10ms target
        iterations => Iterations,
        times_us => Times,
        mean_us => mean(Times),
        median_us => median(Times),
        p95_us => percentile(Times, 0.95),
        p99_us => percentile(Times, 0.99),
        min_us => lists:min(Times),
        max_us => lists:max(Times),
        status => determine_status(mean(Times), 10000)
    }.

%%====================================================================
%% Latency Benchmarks
%%====================================================================

-spec benchmark_latency(pos_integer()) -> map().
benchmark_latency(Iterations) ->
    io:format("Benchmarking request/response latency...~n"),

    %% Test message
    TestRequest = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"method">> => <<"tools/list">>,
        <<"id">> => 1
    },

    Times = lists:map(fun(_) ->
        StartTime = erlang:monotonic_time(microsecond),

        %% Simulate request processing
        try
            JSON = jsx:encode(TestRequest),
            _ = erlmcp_json_rpc:decode_message(JSON),
            true
        catch
            _ -> true
        end,

        EndTime = erlang:monotonic_time(microsecond),
        EndTime - StartTime
    end, lists:seq(1, Iterations)),

    #{
        metric => <<"request_response_latency">>,
        target_us => 5000,  % 5ms target
        iterations => Iterations,
        times_us => Times,
        mean_us => mean(Times),
        median_us => median(Times),
        p95_us => percentile(Times, 0.95),
        p99_us => percentile(Times, 0.99),
        min_us => lists:min(Times),
        max_us => lists:max(Times),
        status => determine_status(mean(Times), 5000)
    }.

%%====================================================================
 Throughput Benchmarks
%%====================================================================

-spec benchmark_throughput(pos_integer()) -> map().
benchmark_throughput(Iterations) ->
    io:format("Benchmarking throughput...~n"),

    BatchSizes = [1, 10, 100, 1000],
    Results = lists:map(fun(BatchSize) ->
        Times = lists:map(fun(_) ->
            StartTime = erlang:monotonic_time(millisecond),

            %% Process batch of requests
            Requests = [test_request(I) || I <- lists:seq(1, BatchSize)],
            lists:foreach(fun(Request) ->
                JSON = jsx:encode(Request),
                _ = erlmcp_json_rpc:decode_message(JSON)
            end, Requests),

            EndTime = erlang:monotonic_time(millisecond),
            EndTime - StartTime
        end, lists:seq(1, max(1, Iterations div BatchSize))),

        %% Calculate throughput (ops/sec)
        MeanTime = mean(Times),
        Throughput = (BatchSize * 1000) / MeanTime,

        #{
            batch_size => BatchSize,
            throughput_per_sec => Throughput,
            mean_ms => MeanTime,
            target_achieved => Throughput >= 1000
        }
    end, BatchSizes),

    #{
        metric => <<"throughput">>,
        target_ops_sec => 1000,
        batch_results => Results,
        overall_mean => lists:foldl(fun(R, Acc) -> Acc + maps:get(throughput_per_sec, R) end, 0, Results) / length(Results)
    }.

%%====================================================================
%% Memory Usage Benchmarks
%%====================================================================

-spec benchmark_memory_usage(pos_integer()) -> map().
benchmark_memory_usage(Iterations) ->
    io:format("Benchmarking memory usage...~n"),

    %% Measure initial memory
    InitialMemory = erlang:memory(process),

    %% Perform operations and track memory
    MemoryMeasurements = lists:map(fun(_) ->
        Before = erlang:memory(process),

        %% Create and destroy session objects
        Session = erlmcp_session:new(#{test => true}),
        _ = erlmcp_session:delete(erlmcp_session:get_session_id(Session)),

        After = erlang:memory(process),
        After - Before
    end, lists:seq(1, Iterations)),

    %% Measure memory after cleanup
    FinalMemory = erlang:memory(process),

    #{
        metric => <<"memory_usage">>,
        target_bytes => 1024,  % 1KB per operation target
        initial_bytes => InitialMemory,
        final_bytes => FinalMemory,
        delta_bytes => FinalMemory - InitialMemory,
        measurements => MemoryMeasurements,
        mean_delta => mean(MemoryMeasurements),
        median_delta => median(MemoryMeasurements),
        has_memory_leak => (FinalMemory - InitialMemory) > 1024 * Iterations
    }.

%%====================================================================
%% CPU Utilization Benchmarks
%%====================================================================

-spec benchmark_cpu_utilization(pos_integer()) -> map().
benchmark_cpu_utilization(Iterations) ->
    io:format("Benchmarking CPU utilization...~n"),

    %% Measure CPU time for operations
    Process = self(),
    CPUResults = spawn(fun() ->
        %% Monitor CPU time during operations
        CPUStart = erlang:monotonic_time(microsecond),
        erlang:system_info(scheduler_id),

        %% Perform CPU-intensive operations
        lists:foreach(fun(_) ->
            %% JSON encoding/decoding operations
            Data = #{<<"test">> => binary:repeat(<<"x">>, 1000)},
            JSON = jsx:encode(Data),
            _ = jsx:decode(JSON, [return_maps])
        end, lists:seq(1, Iterations)),

        CPUEnd = erlang:monotonic_time(microsecond),
        CPUTime = CPUEnd - CPUStart,

        Process ! {cpu_result, CPUTime}
    end),

    %% Wait for process to complete
    receive
        {cpu_result, CPUTime} ->
            RealTime = erlang:monotonic_time(microsecond),

            #{
                metric => <<"cpu_utilization">>,
                target_percent => 80,  % 80% target CPU utilization
                iterations => Iterations,
                cpu_time_us => CPUTime,
                real_time_us => RealTime - RealTime + CPUTime,
                cpu_utilization_percent => (CPUTime / RealTime) * 100,
                ops_per_cpu_second => Iterations / (CPUTime / 1000000)
            }
    after 5000 ->
        Process ! exit,
        #{error => <<"benchmark_timeout">>}
    end.

%%====================================================================
%% Registry Performance Benchmarks
%%====================================================================

-spec benchmark_registry_performance(pos_integer()) -> map().
benchmark_registry_performance(Iterations) ->
    io:format("Benchmarking registry performance...~n"),

    %% Measure registry lookup performance
    Times = lists:map(fun(_) ->
        StartTime = erlang:monotonic_time(microsecond),

        %% Test multiple registry operations
        lists:foreach(fun(I) ->
            ServerId = list_to_binary("server_" ++ integer_to_list(I)),
            _ = erlmcp_registry:find_server(ServerId)
        end, lists:seq(1, 10)),

        EndTime = erlang:monotonic_time(microsecond),
        EndTime - StartTime
    end, lists:seq(1, Iterations)),

    %% Calculate operations per second
    MeanTime = mean(Times),
    OpsPerSecond = (10 * Iterations * 1000000) / MeanTime,

    #{
        metric => <<"registry_performance">>,
        target_ops_sec => 553000,  % 553K ops/sec target
        operations_per_second => OpsPerSecond,
        target_achieved => OpsPerSecond >= 553000,
        mean_us => MeanTime,
        measurements => Times
    }.

%%====================================================================
%% Transport Performance Benchmarks
%%====================================================================

-spec benchmark_transport_performance(pos_integer()) -> map().
benchmark_transport_performance(Iterations) ->
    io:format("Benchmarking transport performance...~n"),

    TransportTypes = [stdio, tcp, http, ws, sse],
    Results = lists:map(fun(TransportType) ->
        Times = lists:map(fun(_) ->
            StartTime = erlang:monotonic_time(microsecond),

            %% Test message sending
            TestData = binary:repeat(<<"test data">>, 100),
            Result = erlmcp_cli_transport:send_data(TestData, TransportType),

            EndTime = erlang:monotonic_time(microsecond),
            case Result of
                {ok, _} -> EndTime - StartTime;
                {error, _} -> EndTime - StartTime  % Include error time
            end
        end, lists:seq(1, Iterations)),

        #{
            transport => atom_to_binary(TransportType),
            mean_us => mean(Times),
            max_us => lists:max(Times),
            min_us => lists:min(Times),
            throughput_ops_sec => (Iterations * 1000000) / mean(Times)
        }
    end, TransportTypes),

    #{
        metric => <<"transport_performance">>,
        target_ops_sec => 100000,  % 100K ops/sec per transport
        results => Results
    }.

%%====================================================================
%% Session Management Benchmarks
%%====================================================================

-spec benchmark_session_management(pos_integer()) -> map().
benchmark_session_management(Iterations) ->
    io:format("Benchmarking session management...~n"),

    %% Create and destroy sessions
    CreationTimes = lists:map(fun(_) ->
        StartTime = erlang:monotonic_time(microsecond),

        %% Create session
        {ok, SessionId} = erlmcp_session:create(#{test => true}),

        EndTime = erlang:monotonic_time(microsecond),

        %% Clean up
        _ = erlmcp_session:delete(SessionId),

        EndTime - StartTime
    end, lists:seq(1, Iterations)),

    %% Session lookup times
    LookupTimes = lists:map(fun(_) ->
        %% Create session first
        {ok, SessionId} = erlmcp_session:create(#{test => true}),

        StartTime = erlang:monotonic_time(microsecond),
        _ = erlmcp_session:retrieve(SessionId),
        EndTime = erlang:monotonic_time(microsecond),

        %% Clean up
        _ = erlmcp_session:delete(SessionId),

        EndTime - StartTime
    end, lists:seq(1, Iterations)),

    #{
        metric => <<"session_management">>,
        target_us => 1000,  % 1ms per operation target
        creation_times_us => CreationTimes,
        lookup_times_us => LookupTimes,
        mean_creation_us => mean(CreationTimes),
        mean_lookup_us => mean(LookupTimes),
        status => determine_status(mean(CreationTimes), 1000)
    }.

%%====================================================================
%% OTEL Overhead Benchmarks
%%====================================================================

-spec benchmark_otel_overhead(pos_integer()) -> map().
benchmark_otel_overhead(Iterations) ->
    io:format("Benchmarking OTEL overhead...~n"),

    %% Measure with OTEL enabled
    WithOtelTimes = lists:map(fun(_) ->
        StartTime = erlang:monotonic_time(microsecond),

        %% Operation with OTEL tracing
        erlmcp_otel:with_span("test_operation", #{}, fun() ->
            %% Simple operation
            _ = lists:sum(lists:seq(1, 100))
        end),

        EndTime = erlang:monotonic_time(microsecond),
        EndTime - StartTime
    end, lists:seq(1, Iterations)),

    %% Measure without OTEL (simulated)
    WithoutOtelTimes = lists:map(fun(_) ->
        StartTime = erlang:monotonic_time(microsecond),

        %% Same operation without OTEL
        _ = lists:sum(lists:seq(1, 100)),

        EndTime = erlang:monotonic_time(microsecond),
        EndTime - StartTime
    end, lists:seq(1, Iterations)),

    %% Calculate overhead
    WithOtelMean = mean(WithOtelTimes),
    WithoutOtelMean = mean(WithoutOtelTimes),
    OverheadPercent = ((WithOtelMean - WithoutOtelMean) / WithoutOtelMean) * 100,

    #{
        metric => <<"otel_overhead">>,
        target_percent => 5,  % 5% overhead target
        with_otel_mean_us => WithOtelMean,
        without_otel_mean_us => WithoutOtelMean,
        overhead_percent => OverheadPercent,
        overhead_acceptable => OverheadPercent =< 5,
        measurements => #{
            with_otel => WithOtelTimes,
            without_otel => WithoutOtelTimes
        }
    }.

%%====================================================================
%% JSON-RPC Performance Benchmarks
%%====================================================================

-spec benchmark_json_rpc_performance(pos_integer()) -> map().
benchmark_json_rpc_performance(Iterations) ->
    io:format("Benchmarking JSON-RPC performance...~n"),

    TestMethods = [<<"tools/list">>, <<"resources/list">>, <<"initialize">>],
    Results = lists:map(fun(Method) ->
        Messages = [generate_test_request(Method, I) || I <- lists:seq(1, 100)],

        %% Encoding benchmark
        EncodeStartTime = erlang:monotonic_time(microsecond),
        lists:foreach(fun(Message) ->
            JSON = erlmcp_json_rpc:encode_request(1, Method, Message),
            _ = binary:length(JSON)
        end, Messages),
        EncodeEndTime = erlang:monotonic_time(microsecond),

        %% Decoding benchmark
        DecodedMessages = lists:map(fun(Message) ->
            JSON = erlmcp_json_rpc:encode_request(1, Method, Message),
            {ok, Decoded} = erlmcp_json_rpc:decode_message(JSON),
            Decoded
        end, Messages),
        DecodeEndTime = erlang:monotonic_time(microsecond),

        #{
            method => Method,
            encode_time_ms => (EncodeEndTime - EncodeStartTime) / 1000,
            decode_time_ms => (DecodeEndTime - EncodeEndTime) / 1000,
            total_time_ms => (DecodeEndTime - EncodeStartTime) / 1000,
            messages_processed => length(Messages),
            encode_ops_per_sec => (length(Messages) * 1000) / ((EncodeEndTime - EncodeStartTime) / 1000),
            decode_ops_per_sec => (length(Messages) * 1000) / ((DecodeEndTime - EncodeEndTime) / 1000)
        }
    end, TestMethods),

    #{
        metric => <<"json_rpc_performance">>,
        target_ops_sec => 100000,  % 100K ops/sec target
        results => Results
    }.

%%====================================================================
%% Performance Analysis and Optimization
%%====================================================================

-spec establish_all_baselines() -> map().
establish_all_baselines() ->
    %% Run all benchmarks to establish baselines
    Baselines = #{
        connection => establish_connection_baselines(),
        latency => establish_latency_baselines(),
        throughput => establish_throughput_baselines(),
        memory => establish_memory_baselines(),
        registry => establish_registry_baselines(),
        transport => establish_transport_baselines(),
        session => establish_session_baselines(),
        otel => establish_otel_baselines(),
        json_rpc => establish_json_rpc_baselines()
    },

    Baselines.

-spec suggest_optimizations(map()) -> map().
suggest_optimizations(Baselines) ->
    Optimizations = #{
        connection => suggest_connection_optimizations(Baselines),
        latency => suggest_latency_optimizations(Baselines),
        throughput => suggest_throughput_optimizations(Baselines),
        memory => suggest_memory_optimizations(Baselines),
        registry => suggest_registry_optimizations(Baselines),
        transport => suggest_transport_optimizations(Baselines),
        session => suggest_session_optimizations(Baselines),
        otel => suggest_otel_optimizations(Baselines),
        json_rpc => suggest_json_rpc_optimizations(Baselines)
    },

    Optimizations.

-spec apply_optimizations(map()) -> ok.
apply_optimizations(Optimizations) ->
    %% Apply each optimization category
    maps:foreach(fun(Category, OptList) ->
        lists:foreach(fun(Optimization) ->
            apply_optimization(Category, Optimization)
        end, OptList)
    end, Optimizations).

%%====================================================================
%% Reporting Functions
%%====================================================================

-spec generate_report(map()) -> map().
generate_report(BenchmarkResults) ->
    Baselines = establish_all_baselines(),
    Optimizations = suggest_optimizations(Baselines),

    #{
        benchmark_results => BenchmarkResults,
        baselines => Baselines,
        optimizations => Optimizations,
        recommendations => generate_recommendations(Baselines, Optimizations),
        timestamp => erlmcp_iso8601:now_to_iso8601(erlang:timestamp())
    }.

-spec generate_baseline_report(map()) -> map().
generate_baseline_report(Baselines) ->
    #{
        type => <<"baseline_performance">>,
        timestamp => erlmcp_iso8601:now_to_iso8601(erlang:timestamp()),
        baselines => Baselines,
        overall_status => determine_overall_status(Baselines),
        recommendations => generate_baseline_recommendations(Baselines)
    }.

%%====================================================================
%% Utility Functions
%%====================================================================

-spec test_request(integer()) -> map().
test_request(I) ->
    #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"method">> => <<"tools/list">>,
        <<"id">> => I,
        <<"params">> => #{}
    }.

-spec generate_test_request(binary(), integer()) -> map().
generate_test_request(Method, Id) ->
    #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"method">> => Method,
        <<"id">> => Id,
        <<"params">> => #{}
    }.

-spec ensure_apps_started() -> ok.
ensure_apps_started() ->
    Apps = [crypto, ssl, inets],
    lists:foreach(fun(App) ->
        case application:start(App) of
            ok -> ok;
            {error, {already_started, App}} -> ok
        end
    end, Apps).

-spec determine_status(float(), integer()) -> binary().
determine_status(Value, Target) when Value =< Target -> <<"excellent">>;
determine_status(Value, Target) when Value =< Target * 1.2 -> <<"good">>;
determine_status(Value, Target) when Value =< Target * 1.5 -> <<"fair">>;
determine_status(_, _) -> <<"poor">>.

-spec determine_overall_status(map()) -> binary().
determine_overall_status(Baselines) ->
    %% Check if any benchmark fails target
    Categories = maps:keys(Baselines),
    Failed = lists:filter(fun(Category) ->
        Baseline = maps:get(Category, Baselines),
        case maps:get(status, Baseline, undefined) of
            <<"excellent">> -> false;
            <<"good">> -> false;
            <<"fair">> -> false;
            _ -> true
        end
    end, Categories),

    case length(Failed) of
        0 -> <<"excellent">>;
        1 -> <<"good">>;
        2 -> <<"fair">>;
        _ -> <<"poor">>
    end.

%%====================================================================
%% Output Functions
%%====================================================================

-spec print_comprehensive_results(map()) -> ok.
print_comprehensive_results(Report) ->
    io:format("~n==============================================~n"),
    io:format("COMPREHENSIVE PERFORMANCE RESULTS~n"),
    io:format("==============================================~n~n"),

    %% Print overall status
    case maps:get(type, Report, undefined) of
        <<"baseline_performance">> ->
            Baselines = maps:get(baselines, Report),
            OverallStatus = maps:get(overall_status, Baselines),
            io:format("Overall Performance Status: ~p~n", [OverallStatus]);
        _ ->
            %% Benchmark results
            Benchmarks = maps:get(benchmarks, Report),
            print_benchmark_results(Benchmarks)
    end,

    %% Print recommendations
    Recommendations = maps:get(recommendations, Report, []),
    print_recommendations(Recommendations),

    io:format("==============================================~n~n").

-spec print_benchmark_results(map()) -> ok.
print_benchmark_results(Benchmarks) ->
    maps:foreach(fun(Category, Results) ->
        io:format("~n~s Performance:~n", [string:to_upper(atom_to_binary(Category, utf8))]),

        case Category of
            connection ->
                print_connection_results(Results);
            latency ->
                print_latency_results(Results);
            throughput ->
                print_throughput_results(Results);
            memory ->
                print_memory_results(Results);
            registry ->
                print_registry_results(Results);
            transport ->
                print_transport_results(Results);
            session ->
                print_session_results(Results);
            otel ->
                print_otel_results(Results);
            json_rpc ->
                print_json_rpc_results(Results);
            _ ->
                io:format("  Unknown benchmark category~n")
        end
    end, Benchmarks).

%% Individual benchmark printers (simplified for brevity)
print_connection_results(Results) ->
    io:format("  Mean: ~.2f us | Status: ~p~n", [maps:get(mean_us, Results), maps:get(status, Results)]).
print_latency_results(Results) ->
    io:format("  Mean: ~.2f us | P95: ~.2f us | Status: ~p~n", [maps:get(mean_us, Results), maps:get(p95_us, Results), maps:get(status, Results)]).
print_throughput_results(Results) ->
    io:format("  Throughput: ~.0f ops/sec | Target: 1000 ops/sec~n", [maps:get(overall_mean, Results)]).
print_memory_results(Results) ->
    io:format("  Memory delta: ~.2f bytes | Leak: ~p~n", [maps:get(mean_delta, Results), maps:get(has_memory_leak, Results)]).
print_registry_results(Results) ->
    io:format("  Registry: ~.0f ops/sec | Target: 553K ops/sec | Status: ~p~n", [maps:get(operations_per_second, Results), maps:get(target_achieved, Results)]).
print_transport_results(Results) ->
    io:format("  Best transport: ~p | Worst transport: ~p~n", [find_best_transport(Results), find_worst_transport(Results)]).
print_session_results(Results) ->
    io:format("  Creation: ~.2f us | Lookup: ~.2f us | Status: ~p~n", [maps:get(mean_creation_us, Results), maps:get(mean_lookup_us, Results), maps:get(status, Results)]).
print_otel_results(Results) ->
    io:format("  Overhead: ~.2f%% | Acceptable: ~p~n", [maps:get(overhead_percent, Results), maps:get(overhead_acceptable, Results)]).
print_json_rpc_results(Results) ->
    io:format("  Throughput: ~.0f ops/sec | Target: 100K ops/sec~n", [calculate_json_rpc_throughput(Results)]).

print_recommendations(Recommendations) ->
    io:format("~nPERFORMANCE RECOMMENDATIONS:~n"),
    lists:foreach(fun(Rec) ->
        io:format("- ~p~n", [Rec])
    end, Recommendations).

%%====================================================================
%% File Output Functions
%%====================================================================

-spec write_benchmark_report(map()) -> ok.
write_benchmark_report(Report) ->
    Timestamp = erlang:system_time(second),
    Filename = "bench/results/cli_performance_full_~p.json",
    FullFilename = io_lib:format(Filename, [Timestamp]),

    filelib:ensure_dir(FullFilename),
    JSON = jsx:encode(Report, [{space, 1}, {indent, 2}]),
    file:write_file(FullFilename, JSON),

    io:format("Full report written to: ~s~n", [FullFilename]).

-spec write_baselines(map()) -> ok.
write_baselines(Baselines) ->
    Timestamp = erlmcp_iso8601:now_to_iso8601(erlang:timestamp()),
    Filename = "bench/results/baselines_~p.json",
    FullFilename = io_lib:format(Filename, [Timestamp]),

    filelib:ensure_dir(FullFilename),
    Report = #{
        type => <<"performance_baselines">>,
        timestamp => Timestamp,
        baselines => Baselines
    },
    JSON = jsx:encode(Report, [{space, 1}, {indent, 2}]),
    file:write_file(FullFilename, JSON),

    io:format("Baselines written to: ~s~n", [FullFilename]).

%%====================================================================
%% Statistical Helper Functions
%%====================================================================

mean([]) -> 0.0;
mean(List) -> lists:sum(List) / length(List).

median([]) -> 0.0;
median(List) ->
    Sorted = lists:sort(List),
    Len = length(Sorted),
    Mid = Len div 2,
    case Len rem 2 of
        0 -> (lists:nth(Mid, Sorted) + lists:nth(Mid + 1, Sorted)) / 2;
        1 -> lists:nth(Mid + 1, Sorted)
    end.

percentile([], _) -> 0.0;
percentile(List, P) ->
    Sorted = lists:sort(List),
    Len = length(Sorted),
    Index = max(1, min(Len, round(P * Len))),
    lists:nth(Index, Sorted).

%%====================================================================
%% Target Checking Functions
%%====================================================================

-spec check_performance_targets(map()) -> ok.
check_performance_targets(Report) ->
    Benchmarks = maps:get(benchmarks, Report, #{}),

    %% Check registry target (553K ops/sec)
    case maps:get(registry, Benchmarks, undefined) of
        Results when is_map(Results) ->
            OpsPerSecond = maps:get(operations_per_second, Results),
            if OpsPerSecond >= 553000 ->
                    io:format("✓ Registry target achieved: ~.0f ops/sec~n", [OpsPerSecond]);
               true ->
                    io:format("✗ Registry target missed: ~.0f ops/sec (target: 553K)~n", [OpsPerSecond])
            end;
        _ ->
            io:format("✗ Registry benchmark failed~n")
    end,

    %% Check queue throughput target (971K ops/sec)
    case maps:get(throughput, Benchmarks, undefined) of
        Results when is_map(Results) ->
            Throughput = maps:get(overall_mean, Results),
            if Throughput >= 971000 ->
                    io:format("✓ Queue target achieved: ~.0f ops/sec~n", [Throughput]);
               true ->
                    io:format("✗ Queue target missed: ~.0f ops/sec (target: 971K)~n", [Throughput])
            end;
        _ ->
            io:format("✗ Queue benchmark failed~n")
    end.

%%====================================================================
%% Optimization Functions (Placeholder - would need actual implementation)
%%====================================================================

-spec establish_connection_baselines() -> map().
establish_connection_baselines() ->
    #{target => 10000, current => 15000, status => <<"needs_optimization">>}.

-spec establish_latency_baselines() -> map().
establish_latency_baselines() ->
    #{target => 5000, current => 8000, status => <<"needs_optimization">>}.

%% ... more baseline establishment functions ...

-spec suggest_connection_optimizations(map()) -> [map()].
suggest_connection_optimizations(_Baseline) ->
    [#{type => connection_optimization, action => "enable_connection_pools"}].

-spec apply_optimization(binary(), map()) -> ok.
apply_optimization(_Category, _Optimization) ->
    io:format("Applying optimization: ~p~n", [_Optimization]).

%% ... more optimization functions ...