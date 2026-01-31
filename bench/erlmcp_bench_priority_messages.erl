%%%====================================================================
%%% ERLMCP PRIORITY MESSAGES BENCHMARK - OTP 28 FEATURE
%%%====================================================================
%%% Module: erlmcp_bench_priority_messages
%%% Purpose: Measure health check latency with priority messages (OTP 28)
%%% Target: <1ms p99 latency for health checks under load
%%% Workloads: 100, 1000, 10000 concurrent low-priority requests
%%% Measures: Health check interrupt latency under load
%%%====================================================================

-module(erlmcp_bench_priority_messages).

-export([
    run/0,
    run/1,
    run_all/0,
    workloads/0
]).

-include_lib("kernel/include/logger.hrl").

%%====================================================================
%% Workload Definitions
%%====================================================================

-spec workloads() -> [map()].
workloads() ->
    [
        #{id => <<"priority_msg_100">>, background_load => 100, health_checks => 50},
        #{id => <<"priority_msg_1000">>, background_load => 1000, health_checks => 50},
        #{id => <<"priority_msg_10000">>, background_load => 10000, health_checks => 50}
    ].

%%====================================================================
%% Main Entry Points
%%====================================================================

-spec run() -> ok.
run() ->
    run_all().

-spec run(binary()) -> ok | {error, term()}.
run(WorkloadId) when is_binary(WorkloadId) ->
    Workloads = workloads(),
    case lists:filter(fun(#{id := Id}) -> Id =:= WorkloadId end, Workloads) of
        [] ->
            io:format("ERROR: Unknown workload: ~s~n", [WorkloadId]),
            {error, {unknown_workload, WorkloadId}};
        [Workload] ->
            run_workload(Workload)
    end;
run(WorkloadId) when is_list(WorkloadId) ->
    run(list_to_binary(WorkloadId)).

-spec run_all() -> ok.
run_all() ->
    io:format("~n==============================================~n"),
    io:format("ERLMCP PRIORITY MESSAGES BENCHMARK~n"),
    io:format("OTP 28 Priority Message Feature~n"),
    io:format("==============================================~n~n"),

    OtpVersion = erlang:system_info(otp_release),
    io:format("OTP Version: ~s~n", [OtpVersion]),
    io:format("Priority messages available: ~p~n~n", [has_priority_messages()]),

    lists:foreach(fun(Workload) ->
        run_workload(Workload)
    end, workloads()),

    io:format("~n==============================================~n"),
    io:format("Priority messages benchmarks complete.~n"),
    io:format("Results in bench/results/~n"),
    io:format("==============================================~n~n"),
    ok.

%%====================================================================
%% Workload Execution
%%====================================================================

-spec run_workload(map()) -> ok | {error, term()}.
run_workload(#{id := WorkloadId, background_load := BgLoad, health_checks := HealthChecks} = _Workload) ->
    io:format("~n--- Workload: ~s ---~n", [WorkloadId]),
    io:format("Background load: ~p low-priority messages~n", [BgLoad]),
    io:format("Health checks: ~p high-priority checks~n", [HealthChecks]),

    %% Capture environment
    Env = capture_environment(),

    %% Start test server
    io:format("Starting test server...~n"),
    {ok, ServerPid} = start_test_server(),

    %% Benchmark without priority messages (baseline)
    io:format("~nBenchmarking WITHOUT priority messages (baseline)...~n"),
    NormalResults = benchmark_without_priority(ServerPid, BgLoad, HealthChecks),

    %% Benchmark with priority messages (OTP 28)
    PriorityResults = case has_priority_messages() of
        true ->
            io:format("Benchmarking WITH priority messages (OTP 28)...~n"),
            benchmark_with_priority(ServerPid, BgLoad, HealthChecks);
        false ->
            io:format("Priority messages not available (OTP < 28)~n"),
            undefined
    end,

    %% Cleanup
    stop_test_server(ServerPid),

    %% Build report
    Report = build_report(WorkloadId, Env, BgLoad, HealthChecks, NormalResults, PriorityResults),

    %% Validate and write
    case validate_report(Report) of
        ok ->
            Timestamp = erlang:system_time(second),
            Filename = io_lib:format("bench/results/priority_msg_~s_~p.json", [WorkloadId, Timestamp]),
            write_report(Filename, Report),
            io:format("✓ Report written: ~s~n", [Filename]),
            
            %% Display comparison
            display_comparison(BgLoad, NormalResults, PriorityResults),
            ok;
        {error, ValidationError} ->
            io:format("✗ Validation failed: ~p~n", [ValidationError]),
            {error, {validation_failed, ValidationError}}
    end.

%%====================================================================
%% Test Server
%%====================================================================

%% Start a test server that processes messages
-spec start_test_server() -> {ok, pid()}.
start_test_server() ->
    ServerPid = spawn_link(fun() -> test_server_loop(0) end),
    {ok, ServerPid}.

%% Test server loop
-spec test_server_loop(non_neg_integer()) -> ok.
test_server_loop(Count) ->
    receive
        {low_priority, From, Ref} ->
            %% Simulate slow processing
            timer:sleep(1),
            From ! {reply, Ref, ok},
            test_server_loop(Count + 1);
            
        {health_check, From, Ref} ->
            %% Fast health check - should be prioritized
            From ! {health_reply, Ref, ok},
            test_server_loop(Count + 1);
            
        stop ->
            ok
    end.

-spec stop_test_server(pid()) -> ok.
stop_test_server(ServerPid) ->
    catch ServerPid ! stop,
    ok.

%%====================================================================
%% Benchmark: Without Priority Messages (Baseline)
%%====================================================================

-spec benchmark_without_priority(pid(), pos_integer(), pos_integer()) -> map().
benchmark_without_priority(ServerPid, BgLoad, HealthChecks) ->
    %% Flood with background traffic
    BackgroundPids = spawn_background_traffic(ServerPid, BgLoad),
    
    %% Wait for queue to build up
    timer:sleep(100),
    
    %% Measure health check latency
    HealthLatencies = lists:map(fun(_) ->
        Ref = make_ref(),
        Start = erlang:monotonic_time(microsecond),
        
        %% Send health check (normal message)
        ServerPid ! {health_check, self(), Ref},
        
        %% Wait for reply
        receive
            {health_reply, Ref, ok} ->
                End = erlang:monotonic_time(microsecond),
                End - Start
        after 5000 ->
            5000000  % Timeout in microseconds
        end
    end, lists:seq(1, HealthChecks)),
    
    %% Cleanup background traffic
    cleanup_background_traffic(BackgroundPids),
    
    #{
        method => without_priority,
        background_load => BgLoad,
        health_checks => HealthChecks,
        latency_metrics => calculate_metrics(HealthLatencies),
        description => <<"Health checks in normal message queue (FIFO)">>
    }.

%%====================================================================
%% Benchmark: With Priority Messages (OTP 28)
%%====================================================================

-spec benchmark_with_priority(pid(), pos_integer(), pos_integer()) -> map().
benchmark_with_priority(ServerPid, BgLoad, HealthChecks) ->
    case has_priority_messages() of
        false ->
            #{method => with_priority, available => false};
        true ->
            %% Flood with background traffic
            BackgroundPids = spawn_background_traffic(ServerPid, BgLoad),
            
            %% Wait for queue to build up
            timer:sleep(100),
            
            %% Measure health check latency with priority
            HealthLatencies = lists:map(fun(_) ->
                Ref = make_ref(),
                Start = erlang:monotonic_time(microsecond),
                
                %% Send health check as HIGH PRIORITY message (OTP 28)
                erlang:send(ServerPid, {health_check, self(), Ref}, [nosuspend, noconnect, {priority, high}]),
                
                %% Wait for reply
                receive
                    {health_reply, Ref, ok} ->
                        End = erlang:monotonic_time(microsecond),
                        End - Start
                after 5000 ->
                    5000000  % Timeout in microseconds
                end
            end, lists:seq(1, HealthChecks)),
            
            %% Cleanup background traffic
            cleanup_background_traffic(BackgroundPids),
            
            #{
                method => with_priority,
                background_load => BgLoad,
                health_checks => HealthChecks,
                latency_metrics => calculate_metrics(HealthLatencies),
                description => <<"Health checks with priority messages (skip queue)">>
            }
    end.

%%====================================================================
%% Background Traffic Generation
%%====================================================================

-spec spawn_background_traffic(pid(), pos_integer()) -> [pid()].
spawn_background_traffic(ServerPid, NumMessages) ->
    %% Spawn workers that send continuous low-priority traffic
    NumWorkers = min(100, NumMessages div 10),
    MsgsPerWorker = NumMessages div NumWorkers,
    
    lists:map(fun(_) ->
        spawn_link(fun() ->
            background_worker_loop(ServerPid, MsgsPerWorker)
        end)
    end, lists:seq(1, NumWorkers)).

-spec background_worker_loop(pid(), pos_integer()) -> ok.
background_worker_loop(ServerPid, MessagesRemaining) when MessagesRemaining > 0 ->
    Ref = make_ref(),
    ServerPid ! {low_priority, self(), Ref},
    
    receive
        {reply, Ref, ok} -> ok;
        stop -> exit(normal)
    after 1000 ->
        ok
    end,
    
    background_worker_loop(ServerPid, MessagesRemaining - 1);
background_worker_loop(_ServerPid, 0) ->
    receive
        stop -> ok
    end.

-spec cleanup_background_traffic([pid()]) -> ok.
cleanup_background_traffic(Pids) ->
    lists:foreach(fun(Pid) ->
        catch Pid ! stop
    end, Pids),
    timer:sleep(100),
    ok.

%%====================================================================
%% Metrics Calculation
%%====================================================================

-spec calculate_metrics([non_neg_integer()]) -> map().
calculate_metrics([]) ->
    #{p50_us => 0.0, p95_us => 0.0, p99_us => 0.0, avg_us => 0.0, min_us => 0.0, max_us => 0.0};
calculate_metrics(Latencies) ->
    Sorted = lists:sort(Latencies),
    Len = length(Sorted),
    
    P50 = percentile(Sorted, 0.50),
    P95 = percentile(Sorted, 0.95),
    P99 = percentile(Sorted, 0.99),
    Avg = lists:sum(Sorted) / Len,
    Min = lists:min(Sorted),
    Max = lists:max(Sorted),
    
    #{
        p50_us => round_float(P50, 1),
        p95_us => round_float(P95, 1),
        p99_us => round_float(P99, 1),
        avg_us => round_float(Avg, 1),
        min_us => round_float(Min, 1),
        max_us => round_float(Max, 1),
        p50_ms => round_float(P50 / 1000, 3),
        p95_ms => round_float(P95 / 1000, 3),
        p99_ms => round_float(P99 / 1000, 3)
    }.

-spec percentile([number()], float()) -> float().
percentile(SortedList, Percentile) ->
    Len = length(SortedList),
    Index = max(1, min(Len, round(Len * Percentile))),
    lists:nth(Index, SortedList).

%%====================================================================
%% Report Building
%%====================================================================

-spec build_report(binary(), map(), pos_integer(), pos_integer(), map(), map() | undefined) -> map().
build_report(WorkloadId, Env, BgLoad, HealthChecks, NormalResults, PriorityResults) ->
    BaseReport = #{
        workload_id => WorkloadId,
        benchmark => <<"priority_messages">>,
        timestamp => erlang:system_time(second),
        environment => Env,
        background_load => BgLoad,
        health_checks => HealthChecks,
        precision => <<"microsecond">>,
        scope => <<"per_node">>,
        target_p99_ms => 1.0,  % Target: <1ms p99
        normal_results => NormalResults
    },
    
    case PriorityResults of
        undefined ->
            BaseReport;
        _ ->
            %% Calculate latency improvement
            NormalMetrics = maps:get(latency_metrics, NormalResults),
            PriorityMetrics = maps:get(latency_metrics, PriorityResults),
            
            NormalP99 = maps:get(p99_ms, NormalMetrics),
            PriorityP99 = maps:get(p99_ms, PriorityMetrics),
            
            Improvement = if
                NormalP99 > 0 -> ((NormalP99 - PriorityP99) / NormalP99) * 100;
                true -> 0.0
            end,
            
            TargetMet = PriorityP99 < 1.0,
            
            BaseReport#{
                priority_results => PriorityResults,
                latency_improvement_percent => round_float(Improvement, 1),
                target_met => TargetMet
            }
    end.

%%====================================================================
%% Display & Output
%%====================================================================

-spec display_comparison(pos_integer(), map(), map() | undefined) -> ok.
display_comparison(_BgLoad, NormalResults, undefined) ->
    io:format("~n--- Results (without priority messages) ---~n"),
    display_method_results(normal, NormalResults),
    ok;
display_comparison(BgLoad, NormalResults, PriorityResults) ->
    io:format("~n--- Performance Comparison (background load: ~p) ---~n~n", [BgLoad]),
    
    io:format("Without priority messages:~n"),
    display_method_results(normal, NormalResults),
    
    io:format("~nWith priority messages (OTP 28):~n"),
    display_method_results(priority, PriorityResults),
    
    %% Calculate improvements
    NormalMetrics = maps:get(latency_metrics, NormalResults),
    PriorityMetrics = maps:get(latency_metrics, PriorityResults),
    
    NormalP99Ms = maps:get(p99_ms, NormalMetrics),
    PriorityP99Ms = maps:get(p99_ms, PriorityMetrics),
    
    ImprovementPercent = if
        NormalP99Ms > 0 -> ((NormalP99Ms - PriorityP99Ms) / NormalP99Ms) * 100;
        true -> 0.0
    end,
    
    LatencyReduction = NormalP99Ms - PriorityP99Ms,
    
    io:format("~n--- Latency Improvement ---~n"),
    io:format("Normal p99:   ~.3f ms~n", [NormalP99Ms]),
    io:format("Priority p99: ~.3f ms~n", [PriorityP99Ms]),
    io:format("Reduction:    ~.3f ms (~.1f% improvement)~n", [LatencyReduction, ImprovementPercent]),
    io:format("Target:       <1.0 ms p99~n"),
    
    TargetMet = PriorityP99Ms < 1.0,
    Status = if
        TargetMet andalso ImprovementPercent > 80 -> "✓ EXCELLENT - Target met with major improvement";
        TargetMet -> "✓ GOOD - Target met";
        PriorityP99Ms < 5.0 -> "⚠ CLOSE - Near target";
        true -> "✗ BELOW TARGET"
    end,
    io:format("~nStatus: ~s~n", [Status]),
    
    ok.

-spec display_method_results(atom(), map()) -> ok.
display_method_results(_Method, Results) ->
    Metrics = maps:get(latency_metrics, Results),
    
    io:format("  Health check latency:~n"),
    io:format("    p50: ~.3f ms (~.1f us)~n", [maps:get(p50_ms, Metrics), maps:get(p50_us, Metrics)]),
    io:format("    p95: ~.3f ms (~.1f us)~n", [maps:get(p95_ms, Metrics), maps:get(p95_us, Metrics)]),
    io:format("    p99: ~.3f ms (~.1f us)~n", [maps:get(p99_ms, Metrics), maps:get(p99_us, Metrics)]),
    ok.

%%====================================================================
%% Helpers
%%====================================================================

-spec has_priority_messages() -> boolean().
has_priority_messages() ->
    %% Check if priority option for erlang:send/3 is available (OTP 28+)
    %% This is a heuristic - actual implementation may vary
    OtpRelease = list_to_integer(erlang:system_info(otp_release)),
    OtpRelease >= 28.

-spec capture_environment() -> map().
capture_environment() ->
    {ok, Hostname} = inet:gethostname(),
    OtpRelease = erlang:system_info(otp_release),
    
    #{
        hostname => list_to_binary(Hostname),
        erlang_version => list_to_binary("OTP-" ++ OtpRelease),
        priority_messages_available => has_priority_messages()
    }.

-spec validate_report(map()) -> ok | {error, term()}.
validate_report(Report) ->
    RequiredFields = [workload_id, benchmark, timestamp, environment, normal_results],
    case lists:all(fun(F) -> maps:is_key(F, Report) end, RequiredFields) of
        true -> ok;
        false -> {error, missing_required_fields}
    end.

-spec write_report(string(), map()) -> ok | {error, term()}.
write_report(Filename, Report) ->
    filelib:ensure_dir(Filename),
    Json = jsx:encode(Report, [{space, 2}, {indent, 2}]),
    file:write_file(Filename, Json).

-spec round_float(number(), integer()) -> float().
round_float(Value, DecimalPlaces) ->
    Multiplier = math:pow(10, DecimalPlaces),
    round(Value * Multiplier) / Multiplier.
