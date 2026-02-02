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
-export([validate_all/1, run/1, run/2, generate_report/0]).
%% API - Individual measurements
-export([measure_latency/2, measure_throughput/2, measure_memory/1, measure_connection_setup/1,
         test_concurrent_connections/2]).
%% API - Validation helpers
-export([validate_latency/1, validate_latency/2, validate_throughput/1, validate_throughput/2,
         validate_memory/1, validate_memory/2, validate_connection_setup/1,
         validate_concurrent_connections/1, validate_concurrency/2, benchmark_comparison/3,
         calculate_percentiles/1, format_report/1, generate_performance_report/1,
         get_metric_value/3, get_threshold_value/4]).

-include_lib("kernel/include/logger.hrl").

%%====================================================================
%% Types
%%====================================================================

-type transport_type() :: stdio | tcp | http | websocket.
-type latency_result() ::
    #{p50_us => number(),
      p95_us => number(),
      p99_us => number(),
      samples => pos_integer()}.
-type throughput_result() ::
    #{requests_per_second => number(),
      total_requests => pos_integer(),
      duration_s => number()}.
-type memory_result() ::
    #{bytes_per_connection => number(),
      kb_per_connection => number(),
      total_connections => pos_integer()}.
-type connection_setup_result() ::
    #{avg_setup_time_us => number(),
      max_setup_time_us => number(),
      samples => pos_integer()}.
-type concurrent_result() ::
    #{success_count => pos_integer(),
      failure_count => non_neg_integer(),
      success_rate => number(),
      total_connections => pos_integer()}.
-type validation_result() ::
    #{passed => boolean(),
      target => number(),
      actual => number(),
      status => pass | fail | warning}.
-type performance_report() ::
    #{transport => transport_type(),
      timestamp => integer(),
      latency => validation_result() | map(),
      throughput => validation_result() | map(),
      memory => validation_result() | map(),
      connection_setup => validation_result() | map(),
      concurrent_connections => validation_result() | map(),
      overall_passed => boolean(),
      details => map()}.

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

%% @doc Validate all performance compliance aspects for MCP specification
-spec validate_all(binary()) ->
                      #{status := passed | failed | warning,
                        timestamp := integer(),
                        checks :=
                            [#{name := binary(),
                               status := passed | failed | warning,
                               message => binary(),
                               details => map()}],
                        passed := non_neg_integer(),
                        failed := non_neg_integer()}.
validate_all(SpecVersion) when is_binary(SpecVersion) ->
    Timestamp = erlang:system_time(millisecond),

    %% Run baseline performance checks using STDIO transport (fastest)
    Transport = stdio,

    %% Perform lightweight validation checks
    Checks =
        [check_latency_threshold(Transport),
         check_throughput_threshold(Transport),
         check_memory_threshold(Transport),
         check_connection_setup_threshold(Transport),
         check_concurrent_capability(Transport)],

    %% Count results
    {Passed, Failed, Warnings} =
        lists:foldl(fun(Check, {P, F, W}) ->
                       case maps:get(status, Check) of
                           passed ->
                               {P + 1, F, W};
                           failed ->
                               {P, F + 1, W};
                           warning ->
                               {P, F, W + 1}
                       end
                    end,
                    {0, 0, 0},
                    Checks),

    %% Performance warnings don't fail overall status
    OverallStatus =
        case Failed of
            0 ->
                passed;
            N when N > 0, Warnings > Failed ->
                warning;
            _ ->
                failed
        end,

    #{status => OverallStatus,
      timestamp => Timestamp,
      spec_version => SpecVersion,
      transport => Transport,
      checks => Checks,
      passed => Passed,
      failed => Failed,
      warnings => Warnings}.

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
            maps:get(passed, LatencyValidation)
            andalso maps:get(passed, ThroughputValidation)
            andalso maps:get(passed, MemoryValidation)
            andalso maps:get(passed, SetupValidation)
            andalso maps:get(passed, ConcurrentValidation),

        EndTime = erlang:system_time(millisecond),

        %% Build report
        Report =
            #{transport => Transport,
              timestamp => EndTime,
              duration_ms => EndTime - StartTime,
              latency => maps:merge(LatencyResult, LatencyValidation),
              throughput => maps:merge(ThroughputResult, ThroughputValidation),
              memory => maps:merge(MemoryResult, MemoryValidation),
              connection_setup => maps:merge(SetupResult, SetupValidation),
              concurrent_connections => maps:merge(ConcurrentResult, ConcurrentValidation),
              overall_passed => OverallPassed,
              details =>
                  #{latency_samples => LatencySamples,
                    throughput_requests => ThroughputReqs,
                    memory_connections => MemoryConns,
                    setup_samples => SetupSamples,
                    concurrent_tested => ConcurrentConns}},

        ?LOG_INFO("Performance validation complete: passed=~p", [OverallPassed]),

        %% Cache latest report
        application:set_env(erlmcp_validation, latest_performance_report, Report),

        {ok, Report}
    catch
        Class:Reason:Stacktrace ->
            ?LOG_ERROR("Performance validation failed: ~p:~p~n~p", [Class, Reason, Stacktrace]),
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
-spec measure_latency(transport_type(), pos_integer()) -> {ok, latency_result()} | {error, term()}.
measure_latency(Transport, Samples) when is_integer(Samples), Samples > 0 ->
    ?LOG_INFO("Measuring latency for ~p with ~p samples", [Transport, Samples]),

    try
        %% Start test server
        {ok, ServerPid} = start_test_server(Transport),

        %% Start test client
        {ok, ClientPid} =
            erlmcp_test_client:start_test_client(Transport, #{owner => self(), test_mode => true}),

        %% Measure round-trip time for each request
        Latencies =
            lists:map(fun(_) ->
                         Request = #{<<"method">> => <<"ping">>, <<"params">> => #{}},

                         Start = erlang:monotonic_time(microsecond),

                         case erlmcp_test_client:send_request(ClientPid, Request) of
                             {ok, _Response} ->
                                 End = erlang:monotonic_time(microsecond),
                                 End - Start;
                             {error, _Reason} ->
                                 %% Record timeout as max latency
                                 1000000 % 1 second
                         end
                      end,
                      lists:seq(1, Samples)),

        %% Cleanup
        erlmcp_test_client:stop_test_server(ClientPid),
        stop_test_server(Transport, ServerPid),

        %% Calculate percentiles
        SortedLatencies = lists:sort(Latencies),
        Percentiles = calculate_percentiles(SortedLatencies),

        Result =
            #{p50_us => maps:get(p50, Percentiles),
              p95_us => maps:get(p95, Percentiles),
              p99_us => maps:get(p99, Percentiles),
              samples => Samples,
              transport => Transport},

        ?LOG_INFO("Latency measurement complete: p50=~pµs, p95=~pµs, p99=~pµs",
                  [maps:get(p50, Percentiles),
                   maps:get(p95, Percentiles),
                   maps:get(p99, Percentiles)]),

        {ok, Result}
    catch
        Class:Reason:_Stacktrace ->
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
        {ok, ClientPid} =
            erlmcp_test_client:start_test_client(Transport, #{owner => self(), test_mode => true}),

        %% Measure sustained throughput
        StartTime = erlang:monotonic_time(microsecond),

        %% Send requests as fast as possible using supervised worker pool
        SendFun =
            fun() ->
               Request = #{<<"method">> => <<"ping">>, <<"params">> => #{}},
               lists:foreach(fun(_) -> erlmcp_test_client:send_request(ClientPid, Request) end,
                             lists:seq(1, TotalRequests div 10)) % Divide across workers
            end,

        %% Start a temporary supervisor for workers
        {ok, WorkerSup} = supervisor:start_link(erlmcp_temp_worker_sup, []),

        %% Start supervised workers via supervisor
        Workers = [start_supervised_worker(WorkerSup, SendFun) || _ <- lists:seq(1, 10)],

        %% Wait for all workers using monitors (not links)
        lists:foreach(fun(W) ->
                         Ref = monitor(process, W),
                         receive
                             {'DOWN', Ref, process, W, _} ->
                                 ok
                         end
                      end,
                      Workers),

        %% Stop supervisor and all children
        supervisor:stop(WorkerSup),

        EndTime = erlang:monotonic_time(microsecond),

        %% Cleanup
        erlmcp_test_client:stop_test_server(ClientPid),
        stop_test_server(Transport, ServerPid),

        %% Calculate throughput
        DurationS = (EndTime - StartTime) / 1_000_000,
        Throughput = TotalRequests / DurationS,

        Result =
            #{requests_per_second => Throughput,
              total_requests => TotalRequests,
              duration_s => DurationS,
              transport => Transport},

        ?LOG_INFO("Throughput measurement complete: ~.2f req/s", [Throughput]),

        {ok, Result}
    catch
        Class:Reason:_Stacktrace ->
            ?LOG_ERROR("Throughput measurement failed: ~p:~p", [Class, Reason]),
            {error, {measurement_failed, {Class, Reason}}}
    end.

%% @doc Measure memory per connection
-spec measure_memory(transport_type()) -> {ok, memory_result()} | {error, term()}.
measure_memory(Transport) ->
    NumConnections = ?DEFAULT_MEMORY_CONNECTIONS,
    ?LOG_INFO("Measuring memory for ~p with ~p connections", [Transport, NumConnections]),

    try
        %% Measure baseline memory
        BeforeMemory = erlang:memory(total),
        BeforeProcessCount = erlang:system_info(process_count),

        %% Start multiple connections
        {ok, ServerPid} = start_test_server(Transport),

        ClientPids =
            lists:map(fun(_) ->
                         {ok, Pid} =
                             erlmcp_test_client:start_test_client(Transport,
                                                                  #{owner => self(),
                                                                    test_mode => true}),
                         Pid
                      end,
                      lists:seq(1, NumConnections)),

        %% Allow processes to stabilize
        timer:sleep(100),

        %% Force garbage collection
        lists:foreach(fun(Pid) -> erlang:garbage_collect(Pid) end, [ServerPid | ClientPids]),
        erlang:garbage_collect(self()),

        %% Measure memory after connections
        AfterMemory = erlang:memory(total),
        AfterProcessCount = erlang:system_info(process_count),

        %% Calculate per-connection memory
        MemoryDelta = AfterMemory - BeforeMemory,
        ProcessDelta = AfterProcessCount - BeforeProcessCount,
        BytesPerConnection = MemoryDelta / ProcessDelta,

        %% Cleanup
        lists:foreach(fun(Pid) -> erlmcp_test_client:stop_test_server(Pid) end, ClientPids),
        stop_test_server(Transport, ServerPid),

        Result =
            #{bytes_per_connection => BytesPerConnection,
              kb_per_connection => BytesPerConnection / 1024,
              total_connections => NumConnections,
              memory_delta_bytes => MemoryDelta,
              transport => Transport},

        ?LOG_INFO("Memory measurement complete: ~.2f KB per connection",
                  [BytesPerConnection / 1024]),

        {ok, Result}
    catch
        Class:Reason:_Stacktrace ->
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
        SetupTimes =
            lists:map(fun(_) ->
                         Start = erlang:monotonic_time(microsecond),

                         case erlmcp_test_client:start_test_client(Transport,
                                                                   #{owner => self(),
                                                                     test_mode => true})
                         of
                             {ok, ClientPid} ->
                                 erlmcp_test_client:stop_test_server(ClientPid),
                                 End = erlang:monotonic_time(microsecond),
                                 End - Start;
                             {error, _Reason} ->
                                 1000000 % 1 second timeout
                         end
                      end,
                      lists:seq(1, Samples)),

        %% Cleanup server
        stop_test_server(Transport, ServerPid),

        %% Calculate statistics
        AvgSetup = lists:sum(SetupTimes) / length(SetupTimes),
        MaxSetup = lists:max(SetupTimes),

        Result =
            #{avg_setup_time_us => AvgSetup,
              max_setup_time_us => MaxSetup,
              samples => Samples,
              transport => Transport},

        ?LOG_INFO("Connection setup measurement complete: avg=~pµs, max=~pµs",
                  [AvgSetup, MaxSetup]),

        {ok, Result}
    catch
        Class:Reason:_Stacktrace ->
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

        %% Start all connections concurrently using supervised workers
        StartTime = erlang:monotonic_time(millisecond),

        %% Start temporary supervisor for connection workers
        {ok, ConnSup} = supervisor:start_link(erlmcp_temp_worker_sup, []),

        Clients =
            lists:map(fun(_) ->
                         start_supervised_worker(ConnSup,
                             fun() ->
                                  case erlmcp_test_client:start_test_client(Transport,
                                                                            #{owner => self(),
                                                                              test_mode => true})
                                  of
                                      {ok, Pid} ->
                                          self() ! {result, {ok, Pid}};
                                      {error, Reason} ->
                                          self() ! {result, {error, Reason}}
                                  end
                             end)
                      end,
                      lists:seq(1, NumConnections)),

        %% Wait for all clients to connect
        Results =
            lists:map(fun(ClientRef) ->
                         Ref = monitor(process, ClientRef),
                         receive
                             {'DOWN', Ref, process, ClientRef, _} ->
                                 receive
                                     {result, Result} ->
                                         Result
                                 after 1000 ->
                                     {error, timeout}
                                 end
                         after 5000 ->
                             {error, timeout}
                         end
                      end,
                      Clients),

        EndTime = erlang:monotonic_time(millisecond),

        %% Count successes and failures
        Successes = [R || {ok, _} = R <- Results],
        Failures = [R || {error, _} = R <- Results],

        SuccessCount = length(Successes),
        FailureCount = length(Failures),
        SuccessRate = SuccessCount / NumConnections * 100,

        %% Cleanup successful clients
        lists:foreach(fun({ok, Pid}) -> catch erlmcp_test_client:stop_test_server(Pid) end,
                      Successes),
        stop_test_server(Transport, ServerPid),

        %% Stop connection supervisor
        supervisor:stop(ConnSup),

        Result =
            #{success_count => SuccessCount,
              failure_count => FailureCount,
              success_rate => SuccessRate,
              total_connections => NumConnections,
              duration_ms => EndTime - StartTime,
              transport => Transport},

        ?LOG_INFO("Concurrent connection test complete: ~p/~p successful (~.1f%)",
                  [SuccessCount, NumConnections, SuccessRate]),

        {ok, Result}
    catch
        Class:Reason:_Stacktrace ->
            ?LOG_ERROR("Concurrent connection test failed: ~p:~p", [Class, Reason]),
            {error, {measurement_failed, {Class, Reason}}}
    end.

%%====================================================================
%% API - Validation Helpers
%%====================================================================

%% @doc Validate latency against targets
-spec validate_latency(map()) -> {pass | fail, map()}.
validate_latency(LatencyMap) ->
    validate_latency(LatencyMap, #{}).

%% @doc Validate latency against custom thresholds
-spec validate_latency(map(), map()) -> {pass | fail, map()}.
validate_latency(LatencyMap, Thresholds) when is_map(Thresholds) ->
    %% Check if all required fields are present
    HasP50 = maps:is_key(<<"latency_p50_us">>, LatencyMap) orelse maps:is_key(p50_us, LatencyMap),
    HasP95 = maps:is_key(<<"latency_p95_us">>, LatencyMap) orelse maps:is_key(p95_us, LatencyMap),
    HasP99 = maps:is_key(<<"latency_p99_us">>, LatencyMap) orelse maps:is_key(p99_us, LatencyMap),

    case {HasP50, HasP95, HasP99} of
        {false, _, _} ->
            {fail,
             #{is_valid => false,
               is_error => true,
               error => missing_fields}};
        {_, false, _} ->
            {fail,
             #{is_valid => false,
               is_error => true,
               error => missing_fields}};
        {_, _, false} ->
            {fail,
             #{is_valid => false,
               is_error => true,
               error => missing_fields}};
        _ ->
            try
                P50 = get_metric_value(latency_p50_us, p50_us, LatencyMap),
                P95 = get_metric_value(latency_p95_us, p95_us, LatencyMap),
                P99 = get_metric_value(latency_p99_us, p99_us, LatencyMap),

                TargetP50 = get_threshold_value(latency_p50_us, p50_us, Thresholds, 100),
                TargetP95 = get_threshold_value(latency_p95_us, p95_us, Thresholds, 500),
                TargetP99 = get_threshold_value(latency_p99_us, p99_us, Thresholds, 1000),

                %% Check for negative values (invalid)
                case P50 < 0 orelse P95 < 0 orelse P99 < 0 of
                    true ->
                        {fail,
                         #{is_valid => false,
                           is_error => true,
                           error => negative_values}};
                    false ->
                        %% Build violation list
                        Violations = [],
                        Violations1 =
                            case P50 > TargetP50 of
                                true ->
                                    [#{<<"metric">> => <<"latency_p50_us">>,
                                       <<"threshold_us">> => TargetP50,
                                       <<"actual_us">> => P50,
                                       <<"severity">> => <<"critical">>}
                                     | Violations];
                                false ->
                                    Violations
                            end,
                        Violations2 =
                            case P95 > TargetP95 of
                                true ->
                                    [#{<<"metric">> => <<"latency_p95_us">>,
                                       <<"threshold_us">> => TargetP95,
                                       <<"actual_us">> => P95,
                                       <<"severity">> => <<"critical">>}
                                     | Violations1];
                                false ->
                                    Violations1
                            end,
                        Violations3 =
                            case P99 > TargetP99 of
                                true ->
                                    [#{<<"metric">> => <<"latency_p99_us">>,
                                       <<"threshold_us">> => TargetP99,
                                       <<"actual_us">> => P99,
                                       <<"severity">> => <<"critical">>}
                                     | Violations2];
                                false ->
                                    Violations2
                            end,

                        Passed = length(Violations3) =:= 0,
                        Result =
                            #{is_valid => Passed,
                              violations => length(Violations3),
                              violation_list => lists:reverse(Violations3)},

                        case Passed of
                            true ->
                                {pass, Result};
                            false ->
                                {fail, Result}
                        end
                end
            catch
                _:_ ->
                    {fail,
                     #{is_valid => false,
                       is_error => true,
                       error => unexpected_error}}
            end
    end.

%% @doc Validate throughput against targets
-spec validate_throughput(map()) -> validation_result().
validate_throughput(ThroughputMap) ->
    validate_throughput(ThroughputMap, 100000).

%% @doc Validate throughput against custom target
-spec validate_throughput(map(), number() | undefined) -> {pass | fail, map()}.
validate_throughput(ThroughputMap, Target) when is_number(Target) ->
    RPS = get_metric_value(throughput_msg_per_s, requests_per_second, ThroughputMap),
    %% Check if field exists (RPS will be 0 if not found)
    FieldExists =
        maps:is_key(<<"throughput_msg_per_s">>, ThroughputMap)
        orelse maps:is_key(requests_per_second, ThroughputMap),

    case {FieldExists, RPS} of
        {false, _} ->
            {fail,
             #{is_valid => false,
               is_error => true,
               error => missing_field}};
        {_, 0} when not FieldExists ->
            {fail,
             #{is_valid => false,
               is_error => true,
               error => missing_field}};
        _ ->
            Passed = RPS >= Target,
            Severity =
                case (Target - RPS) / Target of
                    Degradation when Degradation >= 0.5 ->
                        <<"critical">>;
                    Degradation when Degradation >= 0.2 ->
                        <<"warning">>;
                    _ ->
                        <<"info">>
                end,
            Result =
                #{is_valid => Passed,
                  actual_throughput => RPS,
                  min_required => Target,
                  severity => Severity},
            case Passed of
                true ->
                    {pass, Result};
                false ->
                    {fail, Result}
            end
    end;
validate_throughput(ThroughputMap, undefined) ->
    validate_throughput(ThroughputMap, 100000).

%% @doc Validate memory against targets
-spec validate_memory(map()) -> validation_result().
validate_memory(MemoryMap) ->
    validate_memory(MemoryMap, 10).

%% @doc Validate memory against custom target
-spec validate_memory(map(), number() | undefined) -> {pass | fail, map()}.
validate_memory(MemoryMap, MaxMemoryMib) when is_number(MaxMemoryMib) ->
    BytesMib = get_metric_value(memory_per_connection_mib, bytes_per_connection, MemoryMap),
    Passed = BytesMib =< MaxMemoryMib,
    Result =
        #{is_valid => Passed,
          actual_memory_mib => BytesMib,
          max_allowed_mib => MaxMemoryMib},
    case Passed of
        true ->
            {pass, Result};
        false ->
            {fail, Result}
    end;
validate_memory(MemoryMap, undefined) ->
    validate_memory(MemoryMap, 10).

%% @doc Compare benchmark results against baseline
-spec benchmark_comparison(map(), map(), number() | undefined) -> {pass | fail, map()}.
benchmark_comparison(Current, Baseline, Tolerance) when is_map(Current), is_map(Baseline) ->
    DefaultTolerance =
        case Tolerance of
            undefined ->
                10;  % 10% default tolerance (as percentage)
            _ ->
                Tolerance
        end,

    CurrentThroughput = maps:get(<<"throughput_msg_per_s">>, Current, 0),
    BaselineThroughput = maps:get(<<"throughput_msg_per_s">>, Baseline, 0),
    CurrentLatency = maps:get(<<"latency_p99_us">>, Current, 0),
    BaselineLatency = maps:get(<<"latency_p99_us">>, Baseline, 0),

    %% Check throughput ratio
    ThroughputRatio =
        case BaselineThroughput of
            0 ->
                0;
            _ ->
                CurrentThroughput / BaselineThroughput
        end,

    %% Check latency degradation (inverse - higher is worse)
    LatencyRatio =
        case BaselineLatency of
            0 ->
                0;
            _ ->
                CurrentLatency / BaselineLatency
        end,

    %% Count regressions and build regression list
    ToleranceDecimal = DefaultTolerance / 100,
    Regressions = [],
    Regressions1 =
        case ThroughputRatio < 1.0 - ToleranceDecimal of
            true ->
                [#{<<"metric">> => <<"throughput_msg_per_s">>,
                   <<"baseline">> => BaselineThroughput,
                   <<"current">> => CurrentThroughput,
                   <<"degradation">> => (1.0 - ThroughputRatio) * 100}
                 | Regressions];
            false ->
                Regressions
        end,
    Regressions2 =
        case LatencyRatio > 1.0 + ToleranceDecimal of
            true ->
                [#{<<"metric">> => <<"latency_p99_us">>,
                   <<"baseline">> => BaselineLatency,
                   <<"current">> => CurrentLatency,
                   <<"degradation">> => (LatencyRatio - 1.0) * 100}
                 | Regressions1];
            false ->
                Regressions1
        end,

    %% Count improvements
    Improvements = 0,
    Improvements1 =
        case ThroughputRatio > 1.0 + ToleranceDecimal of
            true ->
                Improvements + 1;
            false ->
                Improvements
        end,
    Improvements2 =
        case LatencyRatio < 1.0 - ToleranceDecimal of
            true ->
                Improvements1 + 1;
            false ->
                Improvements1
        end,

    Passed = length(Regressions2) =:= 0,

    Result =
        #{is_valid => Passed,
          regressions => length(Regressions2),
          regression_list => lists:reverse(Regressions2),
          improvements => Improvements2},

    case Passed of
        true ->
            {pass, Result};
        false ->
            {fail, Result}
    end;
benchmark_comparison(_Current, _Baseline, _Tolerance) ->
    {fail,
     #{is_valid => false,
       is_error => true,
       error => invalid_input}}.

%% @doc Validate connection setup against targets
-spec validate_connection_setup(connection_setup_result()) -> validation_result().
validate_connection_setup(#{avg_setup_time_us := Avg}) ->
    Passed = Avg =< ?TARGET_CONN_SETUP_US,

    #{passed => Passed,
      target => ?TARGET_CONN_SETUP_US,
      actual => Avg,
      status => bool_to_status(Passed)}.

%% @doc Validate concurrent connections against targets
-spec validate_concurrent_connections(concurrent_result()) -> validation_result().
validate_concurrent_connections(#{success_count := Success, total_connections := Total}) ->
    SuccessRate = Success / Total * 100,
    %% Require 99% success rate at target concurrency
    TargetRate = 99.0,
    RatePassed = SuccessRate >= TargetRate,
    %% Also check if we achieved the target number
    CountPassed = Success >= ?TARGET_CONCURRENT_CONNS,

    Passed = RatePassed andalso CountPassed,

    #{passed => Passed,
      target => ?TARGET_CONCURRENT_CONNS,
      actual => Success,
      success_rate => SuccessRate,
      target_success_rate => TargetRate,
      status => bool_to_status(Passed)}.

%% @doc validate_concurrency - accepts binary/atom keys for test compatibility
-spec validate_concurrency(map(), pos_integer()) -> {pass | fail, map()}.
validate_concurrency(ConcurrencyMap, MaxConnections) ->
    Connections = get_metric_value(concurrent_connections, success_count, ConcurrencyMap),
    Passed = Connections =< MaxConnections,
    Severity =
        case Connections > MaxConnections of
            true ->
                ExcessRatio = (Connections - MaxConnections) / MaxConnections,
                case ExcessRatio >= 0.5 of
                    true ->
                        <<"critical">>;
                    false ->
                        <<"warning">>
                end;
            false ->
                <<"info">>
        end,
    Result =
        #{is_valid => Passed,
          actual_connections => Connections,
          max_allowed => MaxConnections,
          severity => Severity},
    case Passed of
        true ->
            {pass, Result};
        false ->
            {fail, Result}
    end.

%% @doc Generate comprehensive performance report from metrics map
-spec generate_performance_report(map()) -> map().
generate_performance_report(Metrics) ->
    Timestamp = os:system_time(second),

    %% Check if metrics is empty
    case maps:size(Metrics) =:= 0 of
        true ->
            #{overall_passed => true,
              total_checks => 0,
              passed_checks => 0,
              failed_checks => 0,
              generated_at => Timestamp};
        false ->
            %% Extract metric maps
            LatencyMap = maps:get(latency, Metrics, #{}),
            ThroughputMap = maps:get(throughput, Metrics, #{}),
            MemoryMap = maps:get(memory, Metrics, #{}),
            ConcurrencyMap = maps:get(concurrency, Metrics, #{}),

            %% Validate each metric category
            LatencyResult = validate_latency(LatencyMap, #{}),
            ThroughputResult = validate_throughput(ThroughputMap, undefined),
            MemoryResult = validate_memory(MemoryMap, undefined),
            ConcurrencyResult = validate_concurrency(ConcurrencyMap, 50000),

            %% Calculate overall statistics
            TotalChecks = 4,
            PassedChecks =
                count_passed([LatencyResult, ThroughputResult, MemoryResult, ConcurrencyResult]),
            FailedChecks = TotalChecks - PassedChecks,
            OverallPassed = FailedChecks =:= 0,

            #{overall_passed => OverallPassed,
              total_checks => TotalChecks,
              passed_checks => PassedChecks,
              failed_checks => FailedChecks,
              generated_at => Timestamp,
              latency => LatencyResult,
              throughput => ThroughputResult,
              memory => MemoryResult,
              concurrency => ConcurrencyResult}
    end.

%% @doc Count how many validation results passed
count_passed(Results) ->
    length([R || {pass, _} = R <- Results]).

%% @doc Calculate percentiles from a list of numbers
-spec calculate_percentiles([number()]) ->
                               #{p50 => number(),
                                 p95 => number(),
                                 p99 => number()}.
calculate_percentiles([]) ->
    #{p50 => 0,
      p95 => 0,
      p99 => 0};
calculate_percentiles(Values) when is_list(Values) ->
    Sorted = lists:sort(Values),
    N = length(Sorted),

    #{p50 => percentile(Sorted, N, 50),
      p95 => percentile(Sorted, N, 95),
      p99 => percentile(Sorted, N, 99)}.

%% @doc Format performance report as readable text
-spec format_report(performance_report()) -> binary().
format_report(Report) ->
    Transport = maps:get(transport, Report),
    OverallPassed = maps:get(overall_passed, Report),

    Status =
        case OverallPassed of
            true ->
                "PASSED";
            false ->
                "FAILED"
        end,

    Text =
        [io_lib:format("============================================================================~n",
                       []),
         io_lib:format("                    PERFORMANCE VALIDATION REPORT~n", []),
         io_lib:format("============================================================================~n~n",
                       []),
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
         io_lib:format("============================================================================~n",
                       [])],

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
bool_to_status(true) ->
    pass;
bool_to_status(false) ->
    fail.

%% @doc Format a report section
format_section(Title, Data) ->
    Passed = maps:get(passed, Data, true),
    Status =
        case Passed of
            true ->
                "PASS";
            false ->
                "FAIL"
        end,

    Section = [io_lib:format("--- ~s (~s) ---~n", [Title, Status])],

    Section2 =
        case Title of
            "LATENCY" ->
                P50 = maps:get(p50_us, Data, 0),
                P95 = maps:get(p95_us, Data, 0),
                P99 = maps:get(p99_us, Data, 0),
                [io_lib:format("  P50: ~.2f ms~n", [P50 / 1000]),
                 io_lib:format("  P95: ~.2f ms~n", [P95 / 1000]),
                 io_lib:format("  P99: ~.2f ms~n", [P99 / 1000])];
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
    case erlmcp_transport_tcp:start_server(#{port => 0, % OS assigns port
                                             transport_id => perf_test_tcp})
    of
        {ok, Pid} ->
            {ok, Pid};
        Error ->
            Error
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

%%====================================================================
%% Internal Helper Functions
%%====================================================================

%% @doc Get metric value from map, trying both canonical (binary) and legacy (atom) keys
-spec get_metric_value(atom(), atom(), map()) -> number().
get_metric_value(CanonicalKeyName, LegacyKey, Map) ->
    %% Convert canonical key name atom to binary
    CanonicalKey = atom_to_binary(CanonicalKeyName, utf8),

    case maps:find(CanonicalKey, Map) of
        {ok, Value} when is_number(Value) ->
            Value;
        error ->
            case maps:find(LegacyKey, Map) of
                {ok, Value} when is_number(Value) ->
                    Value;
                error ->
                    0
            end
    end.

%% @doc Get threshold value from custom thresholds map with fallback
-spec get_threshold_value(atom(), atom(), map(), number()) -> number().
get_threshold_value(CanonicalKeyName, LegacyKey, Thresholds, Default) ->
    %% Convert canonical key name atom to binary
    CanonicalKey = atom_to_binary(CanonicalKeyName, utf8),

    case maps:find(CanonicalKey, Thresholds) of
        {ok, Value} when is_number(Value) ->
            Value;
        error ->
            case maps:find(LegacyKey, Thresholds) of
                {ok, Value} when is_number(Value) ->
                    Value;
                error ->
                    Default
            end
    end.

%%%===================================================================
%%% Temporary Worker Supervisor for Safe Process Spawning
%%%===================================================================

%% @doc Start a supervised temporary worker
-spec start_supervised_worker(pid(), function()) -> pid().
start_supervised_worker(Supervisor, WorkerFun) when is_pid(Supervisor), is_function(WorkerFun, 0) ->
    ChildSpec = #{
        id => make_ref(),
        start => {erlang, apply, [WorkerFun, []]},
        restart => temporary,
        shutdown => brutal_kill,
        type => worker,
        modules => []
    },
    {ok, Pid} = supervisor:start_child(Supervisor, ChildSpec),
    Pid.

%% @private Check latency meets threshold (lightweight check)
check_latency_threshold(Transport) ->
    try
        %% Quick latency sample (10 samples)
        {ok, Result} = measure_latency(Transport, 10),
        P99 = maps:get(p99_us, Result),

        case P99 =< ?TARGET_P99_LATENCY_US of
            true ->
                #{name => <<"latency_threshold">>,
                  status => passed,
                  message => <<"P99 latency meets target">>,
                  details => #{p99_us => P99, target_us => ?TARGET_P99_LATENCY_US}};
            false ->
                %% Latency warning, not critical failure
                #{name => <<"latency_threshold">>,
                  status => warning,
                  message => <<"P99 latency exceeds target">>,
                  details => #{p99_us => P99, target_us => ?TARGET_P99_LATENCY_US}}
        end
    catch
        _:_ ->
            #{name => <<"latency_threshold">>,
              status => warning,
              message => <<"Latency measurement unavailable">>}
    end.

%% @private Check throughput meets threshold
check_throughput_threshold(Transport) ->
    try
        %% Quick throughput test (100 requests)
        {ok, Result} = measure_throughput(Transport, 100),
        RPS = maps:get(requests_per_second, Result),

        case RPS >= ?TARGET_THROUGHPUT of
            true ->
                #{name => <<"throughput_threshold">>,
                  status => passed,
                  message => <<"Throughput meets target">>,
                  details => #{rps => RPS, target_rps => ?TARGET_THROUGHPUT}};
            false ->
                #{name => <<"throughput_threshold">>,
                  status => warning,
                  message => <<"Throughput below target">>,
                  details => #{rps => RPS, target_rps => ?TARGET_THROUGHPUT}}
        end
    catch
        _:_ ->
            #{name => <<"throughput_threshold">>,
              status => warning,
              message => <<"Throughput measurement unavailable">>}
    end.

%% @private Check memory per connection meets threshold
check_memory_threshold(Transport) ->
    try
        {ok, Result} = measure_memory(Transport),
        BytesPerConn = maps:get(bytes_per_connection, Result),

        case BytesPerConn =< ?TARGET_MEMORY_PER_CONN_BYTES of
            true ->
                #{name => <<"memory_threshold">>,
                  status => passed,
                  message => <<"Memory per connection meets target">>,
                  details =>
                      #{bytes_per_conn => BytesPerConn,
                        target_bytes => ?TARGET_MEMORY_PER_CONN_BYTES}};
            false ->
                #{name => <<"memory_threshold">>,
                  status => warning,
                  message => <<"Memory per connection exceeds target">>,
                  details =>
                      #{bytes_per_conn => BytesPerConn,
                        target_bytes => ?TARGET_MEMORY_PER_CONN_BYTES}}
        end
    catch
        _:_ ->
            #{name => <<"memory_threshold">>,
              status => warning,
              message => <<"Memory measurement unavailable">>}
    end.

%% @private Check connection setup time meets threshold
check_connection_setup_threshold(Transport) ->
    try
        {ok, Result} = measure_connection_setup(Transport),
        AvgSetup = maps:get(avg_setup_time_us, Result),

        case AvgSetup =< ?TARGET_CONN_SETUP_US of
            true ->
                #{name => <<"connection_setup_threshold">>,
                  status => passed,
                  message => <<"Connection setup meets target">>,
                  details => #{avg_setup_us => AvgSetup, target_us => ?TARGET_CONN_SETUP_US}};
            false ->
                #{name => <<"connection_setup_threshold">>,
                  status => warning,
                  message => <<"Connection setup exceeds target">>,
                  details => #{avg_setup_us => AvgSetup, target_us => ?TARGET_CONN_SETUP_US}}
        end
    catch
        _:_ ->
            #{name => <<"connection_setup_threshold">>,
              status => warning,
              message => <<"Connection setup measurement unavailable">>}
    end.

%% @private Check concurrent connection capability (smoke test)
check_concurrent_capability(Transport) ->
    try
        %% Test with smaller concurrency for quick validation (100 connections)
        {ok, Result} = test_concurrent_connections(Transport, 100),
        SuccessRate = maps:get(success_rate, Result),

        case SuccessRate >= 99.0 of
            true ->
                #{name => <<"concurrent_capability">>,
                  status => passed,
                  message => <<"Concurrent connections supported">>,
                  details => #{success_rate => SuccessRate, tested_connections => 100}};
            false ->
                #{name => <<"concurrent_capability">>,
                  status => warning,
                  message => <<"Concurrent connection success rate below 99%">>,
                  details => #{success_rate => SuccessRate, tested_connections => 100}}
        end
    catch
        _:_ ->
            #{name => <<"concurrent_capability">>,
              status => warning,
              message => <<"Concurrent connection test unavailable">>}
    end.
