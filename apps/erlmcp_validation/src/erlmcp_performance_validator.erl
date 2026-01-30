%%%-------------------------------------------------------------------
%%% @doc erlmcp_performance_validator - Performance Validation Module
%%%
%%% Comprehensive performance validation for erlmcp implementations.
%%% Measures actual metrics using real processes and validates against
%%% performance targets.
%%%
%%% == Performance Targets ==
%%% - P50 Latency: < 5ms
%%% - P95 Latency: < 20ms
%%% - P99 Latency: < 50ms
%%% - Throughput: > 1000 req/s
%%% - Memory Per Connection: < 100KB
%%% - Connection Setup: < 100ms
%%% - Concurrent Connections: 10K support
%%%
%%% == Usage ==
%%%
%%% === Run Full Validation ===
%%% ```erlang
%%% %% Validate STDIO transport
%%% {ok, Report} = erlmcp_performance_validator:run(stdio).
%%%
%%% %% Validate TCP transport
%%% {ok, Report} = erlmcp_performance_validator:run(tcp).
%%% '''
%%%
%%% === Individual Measurements ===
%%% ```erlang
%%% %% Measure latency
%%% {ok, Latency} = erlmcp_performance_validator:measure_latency(stdio, 100).
%%%
%%% %% Measure throughput
%%% {ok, Throughput} = erlmcp_performance_validator:measure_throughput(stdio, 1000).
%%%
%%% %% Measure memory
%%% {ok, Memory} = erlmcp_performance_validator:measure_memory(stdio).
%%%
%%% %% Measure connection setup time
%%% {ok, SetupTime} = erlmcp_performance_validator:measure_connection_setup(stdio).
%%%
%%% %% Test concurrent connections
%%% {ok, Result} = erlmcp_performance_validator:test_concurrent_connections(stdio, 1000).
%%% '''
%%%
%%% === Generate Report ===
%%% ```erlang
%%% {ok, Report} = erlmcp_performance_validator:generate_report().
%%% '''
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_performance_validator).

%% API - Main validation
-export([
    run/1,
    run/2,
    generate_report/0
]).

%% API - Individual measurements
-export([
    measure_latency/2,
    measure_throughput/2,
    measure_memory/1,
    measure_connection_setup/1,
    test_concurrent_connections/2
]).

%% API - Validation helpers
-export([
    validate_latency/1,
    validate_throughput/1,
    validate_memory/1,
    validate_connection_setup/1,
    validate_concurrent_connections/1,
    calculate_percentiles/1,
    format_report/1
]).

-include_lib("kernel/include/logger.hrl").

%%====================================================================
%% Types
%%====================================================================

-type transport_type() :: stdio | tcp | http | websocket.
-type latency_result() :: #{
    p50_us => number(),
    p95_us => number(),
    p99_us => number(),
    samples => pos_integer()
}.
-type throughput_result() :: #{
    requests_per_second => number(),
    total_requests => pos_integer(),
    duration_s => number()
}.
-type memory_result() :: #{
    bytes_per_connection => number(),
    kb_per_connection => number(),
    total_connections => pos_integer()
}.
-type connection_setup_result() :: #{
    avg_setup_time_us => number(),
    max_setup_time_us => number(),
    samples => pos_integer()
}.
-type concurrent_result() :: #{
    success_count => pos_integer(),
    failure_count => non_neg_integer(),
    success_rate => number(),
    total_connections => pos_integer()
}.
-type validation_result() :: #{
    passed => boolean(),
    target => number(),
    actual => number(),
    status => pass | fail | warning
}.
-type performance_report() :: #{
    transport => transport_type(),
    timestamp => integer(),
    latency => validation_result() | map(),
    throughput => validation_result() | map(),
    memory => validation_result() | map(),
    connection_setup => validation_result() | map(),
    concurrent_connections => validation_result() | map(),
    overall_passed => boolean(),
    details => map()
}.

%% Performance targets (from spec)
-define(TARGET_P50_LATENCY_US, 5000).      % 5ms
-define(TARGET_P95_LATENCY_US, 20000).     % 20ms
-define(TARGET_P99_LATENCY_US, 50000).     % 50ms
-define(TARGET_THROUGHPUT, 1000).          % 1000 req/s
-define(TARGET_MEMORY_PER_CONN_BYTES, 102400). % 100KB
-define(TARGET_CONN_SETUP_US, 100000).     % 100ms
-define(TARGET_CONCURRENT_CONNS, 10000).   % 10K connections

%% Default test parameters
-define(DEFAULT_LATENCY_SAMPLES, 100).
-define(DEFAULT_THROUGHPUT_REQUESTS, 1000).
-define(DEFAULT_MEMORY_CONNECTIONS, 10).
-define(DEFAULT_SETUP_SAMPLES, 50).

%%====================================================================
%% API - Main Validation
%%====================================================================

%% @doc Run full performance validation for a transport
-spec run(transport_type()) -> {ok, performance_report()} | {error, term()}.
run(Transport) ->
    run(Transport, #{}).

%% @doc Run performance validation with custom options
-spec run(transport_type(), map()) -> {ok, performance_report()} | {error, term()}.
run(Transport, Options) when is_atom(Transport), is_map(Options) ->
    ?LOG_INFO("Starting performance validation for transport: ~p", [Transport]),
    
    try
        %% Capture start time
        StartTime = erlang:system_time(millisecond),
        
        %% Run all measurements
        LatencySamples = maps:get(latency_samples, Options, ?DEFAULT_LATENCY_SAMPLES),
        ThroughputReqs = maps:get(throughput_requests, Options, ?DEFAULT_THROUGHPUT_REQUESTS),
        MemoryConns = maps:get(memory_connections, Options, ?DEFAULT_MEMORY_CONNECTIONS),
        SetupSamples = maps:get(setup_samples, Options, ?DEFAULT_SETUP_SAMPLES),
        ConcurrentConns = maps:get(concurrent_connections, Options, 1000),
        
        %% Measure latency
        {ok, LatencyResult} = measure_latency(Transport, LatencySamples),
        LatencyValidation = validate_latency(LatencyResult),
        
        %% Measure throughput
        {ok, ThroughputResult} = measure_throughput(Transport, ThroughputReqs),
        ThroughputValidation = validate_throughput(ThroughputResult),
        
        %% Measure memory
        {ok, MemoryResult} = measure_memory(Transport),
        MemoryValidation = validate_memory(MemoryResult),
        
        %% Measure connection setup
        {ok, SetupResult} = measure_connection_setup(Transport),
        SetupValidation = validate_connection_setup(SetupResult),
        
        %% Test concurrent connections
        {ok, ConcurrentResult} = test_concurrent_connections(Transport, ConcurrentConns),
        ConcurrentValidation = validate_concurrent_connections(ConcurrentResult),
        
        %% Calculate overall pass/fail
        OverallPassed = 
            maps:get(passed, LatencyValidation) andalso
            maps:get(passed, ThroughputValidation) andalso
            maps:get(passed, MemoryValidation) andalso
            maps:get(passed, SetupValidation) andalso
            maps:get(passed, ConcurrentValidation),
        
        EndTime = erlang:system_time(millisecond),
        
        %% Build report
        Report = #{
            transport => Transport,
            timestamp => EndTime,
            duration_ms => EndTime - StartTime,
            latency => maps:merge(LatencyResult, LatencyValidation),
            throughput => maps:merge(ThroughputResult, ThroughputValidation),
            memory => maps:merge(MemoryResult, MemoryValidation),
            connection_setup => maps:merge(SetupResult, SetupValidation),
            concurrent_connections => maps:merge(ConcurrentResult, ConcurrentValidation),
            overall_passed => OverallPassed,
            details => #{
                latency_samples => LatencySamples,
                throughput_requests => ThroughputReqs,
                memory_connections => MemoryConns,
                setup_samples => SetupSamples,
                concurrent_tested => ConcurrentConns
            }
        },
        
        ?LOG_INFO("Performance validation complete: passed=~p", [OverallPassed]),
        
        %% Cache latest report
        application:set_env(erlmcp_validation, latest_performance_report, Report),
        
        {ok, Report}
    catch
        Class:Reason:Stacktrace ->
            ?LOG_ERROR("Performance validation failed: ~p:~p~n~p", 
                      [Class, Reason, Stacktrace]),
            {error, {validation_failed, {Class, Reason}}}
    end.

%% @doc Generate performance validation report
-spec generate_report() -> {ok, binary()} | {error, term()}.
generate_report() ->
    case application:get_env(erlmcp_validation, latest_performance_report) of
        undefined ->
            {error, no_report_available};
        {ok, Report} ->
            Formatted = format_report(Report),
            {ok, Formatted}
    end.

%%====================================================================
%% API - Individual Measurements
%%====================================================================

%% @doc Measure request latency (P50, P95, P99)
-spec measure_latency(transport_type(), pos_integer()) -> 
    {ok, latency_result()} | {error, term()}.
measure_latency(Transport, Samples) when is_integer(Samples), Samples > 0 ->
    ?LOG_INFO("Measuring latency for ~p with ~p samples", [Transport, Samples]),
    
    try
        %% Start test server
        {ok, ServerPid} = start_test_server(Transport),
        
        %% Start test client
        {ok, ClientPid} = erlmcp_test_client:start_test_client(Transport, #{
            owner => self(),
            test_mode => true
        }),
        
        %% Measure round-trip time for each request
        Latencies = lists:map(fun(_) ->
            Request = #{
                <<"method">> => <<"ping">>,
                <<"params">> => #{}
            },
            
            Start = erlang:monotonic_time(microsecond),
            
            case erlmcp_test_client:send_request(ClientPid, Request) of
                {ok, _Response} ->
                    End = erlang:monotonic_time(microsecond),
                    End - Start;
                {error, _Reason} ->
                    %% Record timeout as max latency
                    1000000 % 1 second
            end
        end, lists:seq(1, Samples)),
        
        %% Cleanup
        erlmcp_test_client:stop_test_server(ClientPid),
        stop_test_server(Transport, ServerPid),
        
        %% Calculate percentiles
        SortedLatencies = lists:sort(Latencies),
        Percentiles = calculate_percentiles(SortedLatencies),
        
        Result = #{
            p50_us => maps:get(p50, Percentiles),
            p95_us => maps:get(p95, Percentiles),
            p99_us => maps:get(p99, Percentiles),
            samples => Samples,
            transport => Transport
        },
        
        ?LOG_INFO("Latency measurement complete: p50=~pµs, p95=~pµs, p99=~pµs",
                 [maps:get(p50, Percentiles), maps:get(p95, Percentiles), 
                  maps:get(p99, Percentiles)]),
        
        {ok, Result}
    catch
        Class:Reason:Stacktrace ->
            ?LOG_ERROR("Latency measurement failed: ~p:~p", [Class, Reason]),
            {error, {measurement_failed, {Class, Reason}}}
    end.

%% @doc Measure throughput (requests per second)
-spec measure_throughput(transport_type(), pos_integer()) -> 
    {ok, throughput_result()} | {error, term()}.
measure_throughput(Transport, TotalRequests) when is_integer(TotalRequests), TotalRequests > 0 ->
    ?LOG_INFO("Measuring throughput for ~p with ~p requests", [Transport, TotalRequests]),
    
    try
        %% Start test server
        {ok, ServerPid} = start_test_server(Transport),
        
        %% Start test client
        {ok, ClientPid} = erlmcp_test_client:start_test_client(Transport, #{
            owner => self(),
            test_mode => true
        }),
        
        %% Measure sustained throughput
        StartTime = erlang:monotonic_time(microsecond),
        
        %% Send requests as fast as possible
        SendFun = fun() ->
            Request = #{<<"method">> => <<"ping">>, <<"params">> => #{}},
            lists:foreach(fun(_) ->
                erlmcp_test_client:send_request(ClientPid, Request)
            end, lists:seq(1, TotalRequests div 10)) % Divide across workers
        end,
        
        %% Spawn concurrent workers
        Workers = [spawn_link(SendFun) || _ <- lists:seq(1, 10)],
        
        %% Wait for all workers
        lists:foreach(fun(W) ->
            Ref = monitor(process, W),
            receive {'DOWN', Ref, process, W, _} -> ok end
        end, Workers),
        
        EndTime = erlang:monotonic_time(microsecond),
        
        %% Cleanup
        erlmcp_test_client:stop_test_server(ClientPid),
        stop_test_server(Transport, ServerPid),
        
        %% Calculate throughput
        DurationS = (EndTime - StartTime) / 1_000_000,
        Throughput = TotalRequests / DurationS,
        
        Result = #{
            requests_per_second => Throughput,
            total_requests => TotalRequests,
            duration_s => DurationS,
            transport => Transport
        },
        
        ?LOG_INFO("Throughput measurement complete: ~.2f req/s", [Throughput]),
        
        {ok, Result}
    catch
        Class:Reason:Stacktrace ->
            ?LOG_ERROR("Throughput measurement failed: ~p:~p", [Class, Reason]),
            {error, {measurement_failed, {Class, Reason}}}
    end.

%% @doc Measure memory per connection
-spec measure_memory(transport_type()) -> 
    {ok, memory_result()} | {error, term()}.
measure_memory(Transport) ->
    NumConnections = ?DEFAULT_MEMORY_CONNECTIONS,
    ?LOG_INFO("Measuring memory for ~p with ~p connections", [Transport, NumConnections]),
    
    try
        %% Measure baseline memory
        BeforeMemory = erlang:memory(total),
        BeforeProcessCount = erlang:system_info(process_count),
        
        %% Start multiple connections
        {ok, ServerPid} = start_test_server(Transport),
        
        ClientPids = lists:map(fun(_) ->
            {ok, Pid} = erlmcp_test_client:start_test_client(Transport, #{
                owner => self(),
                test_mode => true
            }),
            Pid
        end, lists:seq(1, NumConnections)),
        
        %% Allow processes to stabilize
        timer:sleep(100),
        
        %% Force garbage collection
        lists:foreach(fun(Pid) ->
            erlang:garbage_collect(Pid)
        end, [ServerPid | ClientPids]),
        erlang:garbage_collect(self()),
        
        %% Measure memory after connections
        AfterMemory = erlang:memory(total),
        AfterProcessCount = erlang:system_info(process_count),
        
        %% Calculate per-connection memory
        MemoryDelta = AfterMemory - BeforeMemory,
        ProcessDelta = AfterProcessCount - BeforeProcessCount,
        BytesPerConnection = MemoryDelta / ProcessDelta,
        
        %% Cleanup
        lists:foreach(fun(Pid) ->
            erlmcp_test_client:stop_test_server(Pid)
        end, ClientPids),
        stop_test_server(Transport, ServerPid),
        
        Result = #{
            bytes_per_connection => BytesPerConnection,
            kb_per_connection => BytesPerConnection / 1024,
            total_connections => NumConnections,
            memory_delta_bytes => MemoryDelta,
            transport => Transport
        },
        
        ?LOG_INFO("Memory measurement complete: ~.2f KB per connection", 
                 [BytesPerConnection / 1024]),
        
        {ok, Result}
    catch
        Class:Reason:Stacktrace ->
            ?LOG_ERROR("Memory measurement failed: ~p:~p", [Class, Reason]),
            {error, {measurement_failed, {Class, Reason}}}
    end.

%% @doc Measure connection setup time
-spec measure_connection_setup(transport_type()) -> 
    {ok, connection_setup_result()} | {error, term()}.
measure_connection_setup(Transport) ->
    Samples = ?DEFAULT_SETUP_SAMPLES,
    ?LOG_INFO("Measuring connection setup for ~p with ~p samples", [Transport, Samples]),
    
    try
        %% Start test server once
        {ok, ServerPid} = start_test_server(Transport),
        
        %% Measure connection setup time multiple times
        SetupTimes = lists:map(fun(_) ->
            Start = erlang:monotonic_time(microsecond),
            
            case erlmcp_test_client:start_test_client(Transport, #{
                owner => self(),
                test_mode => true
            }) of
                {ok, ClientPid} ->
                    erlmcp_test_client:stop_test_server(ClientPid),
                    End = erlang:monotonic_time(microsecond),
                    End - Start;
                {error, _Reason} ->
                    1000000 % 1 second timeout
            end
        end, lists:seq(1, Samples)),
        
        %% Cleanup server
        stop_test_server(Transport, ServerPid),
        
        %% Calculate statistics
        AvgSetup = lists:sum(SetupTimes) / length(SetupTimes),
        MaxSetup = lists:max(SetupTimes),
        
        Result = #{
            avg_setup_time_us => AvgSetup,
            max_setup_time_us => MaxSetup,
            samples => Samples,
            transport => Transport
        },
        
        ?LOG_INFO("Connection setup measurement complete: avg=~pµs, max=~pµs",
                 [AvgSetup, MaxSetup]),
        
        {ok, Result}
    catch
        Class:Reason:Stacktrace ->
            ?LOG_ERROR("Connection setup measurement failed: ~p:~p", [Class, Reason]),
            {error, {measurement_failed, {Class, Reason}}}
    end.

%% @doc Test with N concurrent connections
-spec test_concurrent_connections(transport_type(), pos_integer()) -> 
    {ok, concurrent_result()} | {error, term()}.
test_concurrent_connections(Transport, NumConnections) ->
    ?LOG_INFO("Testing ~p concurrent connections for ~p", [NumConnections, Transport]),
    
    try
        %% Start test server
        {ok, ServerPid} = start_test_server(Transport),
        
        %% Start all connections concurrently
        StartTime = erlang:monotonic_time(millisecond),
        
        Clients = lists:map(fun(_) ->
            spawn(fun() ->
                case erlmcp_test_client:start_test_client(Transport, #{
                    owner => self(),
                    test_mode => true
                }) of
                    {ok, Pid} ->
                        self() ! {result, {ok, Pid}};
                    {error, Reason} ->
                        self() ! {result, {error, Reason}}
                end
            end)
        end, lists:seq(1, NumConnections)),
        
        %% Wait for all clients to connect
        Results = lists:map(fun(ClientRef) ->
            Ref = monitor(process, ClientRef),
            receive
                {'DOWN', Ref, process, ClientRef, _} ->
                    receive
                        {result, Result} -> Result
                    after 1000 ->
                        {error, timeout}
                    end
            after 5000 ->
                {error, timeout}
            end
        end, Clients),
        
        EndTime = erlang:monotonic_time(millisecond),
        
        %% Count successes and failures
        Successes = [R || {ok, _} = R <- Results],
        Failures = [R || {error, _} = R <- Results],
        
        SuccessCount = length(Successes),
        FailureCount = length(Failures),
        SuccessRate = (SuccessCount / NumConnections) * 100,
        
        %% Cleanup successful clients
        lists:foreach(fun({ok, Pid}) ->
            catch erlmcp_test_client:stop_test_server(Pid)
        end, Successes),
        stop_test_server(Transport, ServerPid),
        
        Result = #{
            success_count => SuccessCount,
            failure_count => FailureCount,
            success_rate => SuccessRate,
            total_connections => NumConnections,
            duration_ms => EndTime - StartTime,
            transport => Transport
        },
        
        ?LOG_INFO("Concurrent connection test complete: ~p/~p successful (~.1f%)",
                 [SuccessCount, NumConnections, SuccessRate]),
        
        {ok, Result}
    catch
        Class:Reason:Stacktrace ->
            ?LOG_ERROR("Concurrent connection test failed: ~p:~p", [Class, Reason]),
            {error, {measurement_failed, {Class, Reason}}}
    end.

%%====================================================================
%% API - Validation Helpers
%%====================================================================

%% @doc Validate latency against targets
-spec validate_latency(latency_result()) -> validation_result().
validate_latency(#{p50_us := P50, p95_us := P95, p99_us := P99}) ->
    P50Pass = P50 =< ?TARGET_P50_LATENCY_US,
    P95Pass = P95 =< ?TARGET_P95_LATENCY_US,
    P99Pass = P99 =< ?TARGET_P99_LATENCY_US,
    
    Passed = P50Pass andalso P95Pass andalso P99Pass,
    
    Status = case Passed of
        true -> pass;
        false when P99Pass -> warning;
        false -> fail
    end,
    
    #{
        passed => Passed,
        p50 => #{
            target => ?TARGET_P50_LATENCY_US,
            actual => P50,
            status => bool_to_status(P50Pass)
        },
        p95 => #{
            target => ?TARGET_P95_LATENCY_US,
            actual => P95,
            status => bool_to_status(P95Pass)
        },
        p99 => #{
            target => ?TARGET_P99_LATENCY_US,
            actual => P99,
            status => bool_to_status(P99Pass)
        },
        status => Status
    }.

%% @doc Validate throughput against targets
-spec validate_throughput(throughput_result()) -> validation_result().
validate_throughput(#{requests_per_second := RPS}) ->
    Passed = RPS >= ?TARGET_THROUGHPUT,
    
    #{
        passed => Passed,
        target => ?TARGET_THROUGHPUT,
        actual => RPS,
        status => bool_to_status(Passed)
    }.

%% @doc Validate memory against targets
-spec validate_memory(memory_result()) -> validation_result().
validate_memory(#{bytes_per_connection := Bytes}) ->
    Passed = Bytes =< ?TARGET_MEMORY_PER_CONN_BYTES,
    
    #{
        passed => Passed,
        target => ?TARGET_MEMORY_PER_CONN_BYTES,
        actual => Bytes,
        status => bool_to_status(Passed)
    }.

%% @doc Validate connection setup against targets
-spec validate_connection_setup(connection_setup_result()) -> validation_result().
validate_connection_setup(#{avg_setup_time_us := Avg}) ->
    Passed = Avg =< ?TARGET_CONN_SETUP_US,
    
    #{
        passed => Passed,
        target => ?TARGET_CONN_SETUP_US,
        actual => Avg,
        status => bool_to_status(Passed)
    }.

%% @doc Validate concurrent connections against targets
-spec validate_concurrent_connections(concurrent_result()) -> validation_result().
validate_concurrent_connections(#{success_count := Success, total_connections := Total}) ->
    SuccessRate = (Success / Total) * 100,
    %% Require 99% success rate at target concurrency
    TargetRate = 99.0,
    RatePassed = SuccessRate >= TargetRate,
    %% Also check if we achieved the target number
    CountPassed = Success >= ?TARGET_CONCURRENT_CONNS,
    
    Passed = RatePassed andalso CountPassed,
    
    #{
        passed => Passed,
        target => ?TARGET_CONCURRENT_CONNS,
        actual => Success,
        success_rate => SuccessRate,
        target_success_rate => TargetRate,
        status => bool_to_status(Passed)
    }.

%% @doc Calculate percentiles from a list of numbers
-spec calculate_percentiles([number()]) -> #{p50 => number(), p95 => number(), p99 => number()}.
calculate_percentiles([]) ->
    #{p50 => 0, p95 => 0, p99 => 0};
calculate_percentiles(Values) when is_list(Values) ->
    Sorted = lists:sort(Values),
    N = length(Sorted),
    
    #{
        p50 => percentile(Sorted, N, 50),
        p95 => percentile(Sorted, N, 95),
        p99 => percentile(Sorted, N, 99)
    }.

%% @doc Format performance report as readable text
-spec format_report(performance_report()) -> binary().
format_report(Report) ->
    Transport = maps:get(transport, Report),
    OverallPassed = maps:get(overall_passed, Report),
    
    Status = case OverallPassed of
        true -> "PASSED";
        false -> "FAILED"
    end,
    
    Text = [
        io_lib:format("============================================================================~n", []),
        io_lib:format("                    PERFORMANCE VALIDATION REPORT~n", []),
        io_lib:format("============================================================================~n~n", []),
        
        io_lib:format("Transport: ~p~n", [Transport]),
        io_lib:format("Status: ~s~n", [Status]),
        io_lib:format("Timestamp: ~p~n", [maps:get(timestamp, Report)]),
        io_lib:format("Duration: ~p ms~n~n", [maps:get(duration_ms, Report)]),
        
        format_section("LATENCY", maps:get(latency, Report)),
        format_section("THROUGHPUT", maps:get(throughput, Report)),
        format_section("MEMORY", maps:get(memory, Report)),
        format_section("CONNECTION SETUP", maps:get(connection_setup, Report)),
        format_section("CONCURRENT CONNECTIONS", maps:get(concurrent_connections, Report)),
        
        io_lib:format("~nOverall Result: ~s~n", [Status]),
        io_lib:format("============================================================================~n", [])
    ],
    
    iolist_to_binary(Text).

%%====================================================================
%% Internal Functions
%%====================================================================

%% @doc Calculate percentile from sorted list
-spec percentile([number()], pos_integer(), pos_integer()) -> number().
percentile(Sorted, N, P) ->
    Index = ceil(N * P / 100),
    ClampedIndex = max(1, min(Index, N)),
    lists:nth(ClampedIndex, Sorted).

%% @doc Convert boolean to pass/fail status
bool_to_status(true) -> pass;
bool_to_status(false) -> fail.

%% @doc Format a report section
format_section(Title, Data) ->
    Passed = maps:get(passed, Data, true),
    Status = case Passed of
        true -> "PASS";
        false -> "FAIL"
    end,
    
    Section = [
        io_lib:format("--- ~s (~s) ---~n", [Title, Status])
    ],
    
    Section2 = case Title of
        "LATENCY" ->
            P50 = maps:get(p50_us, Data, 0),
            P95 = maps:get(p95_us, Data, 0),
            P99 = maps:get(p99_us, Data, 0),
            [
                io_lib:format("  P50: ~.2f ms~n", [P50 / 1000]),
                io_lib:format("  P95: ~.2f ms~n", [P95 / 1000]),
                io_lib:format("  P99: ~.2f ms~n", [P99 / 1000])
            ];
        "THROUGHPUT" ->
            RPS = maps:get(requests_per_second, Data, 0),
            io_lib:format("  Throughput: ~.2f req/s~n", [RPS]);
        "MEMORY" ->
            KB = maps:get(kb_per_connection, Data, 0),
            io_lib:format("  Per Connection: ~.2f KB~n", [KB]);
        "CONNECTION SETUP" ->
            Avg = maps:get(avg_setup_time_us, Data, 0),
            io_lib:format("  Average Setup: ~.2f ms~n", [Avg / 1000]);
        "CONCURRENT CONNECTIONS" ->
            Success = maps:get(success_count, Data, 0),
            Rate = maps:get(success_rate, Data, 0.0),
            io_lib:format("  Successful: ~p (~.1f%)~n", [Success, Rate])
    end,
    
    [Section, Section2, "\n"].

%% @doc Start a test server for the transport
-spec start_test_server(transport_type()) -> {ok, pid()} | {error, term()}.
start_test_server(stdio) ->
    %% For STDIO, use the existing test server
    {ok, self()};
start_test_server(tcp) ->
    %% Start a TCP echo server for testing
    case erlmcp_transport_tcp:start_server(#{
        port => 0, % OS assigns port
        transport_id => perf_test_tcp
    }) of
        {ok, Pid} -> {ok, Pid};
        Error -> Error
    end;
start_test_server(Transport) ->
    {error, {unsupported_transport, Transport}}.

%% @doc Stop test server
-spec stop_test_server(transport_type(), pid()) -> ok.
stop_test_server(stdio, _Pid) ->
    ok;
stop_test_server(tcp, Pid) when is_pid(Pid) ->
    catch gen_server:stop(Pid),
    ok;
stop_test_server(_Transport, _Pid) ->
    ok.
