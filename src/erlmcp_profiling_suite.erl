%%%-------------------------------------------------------------------
%%% @doc
%%% Profiling Suite - Unified Interface for All Profiling Tools
%%%
%%% Coordinates CPU, memory, and latency profilers along with
%%% bottleneck detection to provide comprehensive performance analysis.
%%%
%%% Usage:
%%%   erlmcp_profiling_suite:start_full_profiling(),
%%%   %% ... run workload ...
%%%   Report = erlmcp_profiling_suite:stop_and_generate_report()
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_profiling_suite).

-export([
    start_full_profiling/0,
    start_full_profiling/1,
    stop_and_generate_report/0,
    get_comprehensive_analysis/0,
    get_executive_summary/0,
    generate_profiling_report_file/1,
    benchmark_profiling_overhead/0,
    stress_test_profilers/1,
    analyze_100k_scenario/1
]).

-include_lib("kernel/include/logger.hrl").

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Start all profiling tools
-spec start_full_profiling() -> ok.
start_full_profiling() ->
    start_full_profiling(#{}).

-spec start_full_profiling(map()) -> ok.
start_full_profiling(_Options) ->
    logger:info("Starting comprehensive profiling suite"),

    %% Start all profilers
    erlmcp_cpu_profiler:start_profiling(),
    erlmcp_latency_profiler:start_profiling(),
    erlmcp_memory_profiler:start_profiling(),
    erlmcp_bottleneck_detector:start_detection(),

    logger:info("All profilers started successfully"),
    ok.

%% @doc Stop profiling and generate comprehensive report
-spec stop_and_generate_report() -> map().
stop_and_generate_report() ->
    logger:info("Stopping profiling and generating report"),

    %% Stop all profilers
    CpuReport = erlmcp_cpu_profiler:stop_profiling(),
    MemoryReport = erlmcp_memory_profiler:stop_profiling(),
    LatencyReport = erlmcp_latency_profiler:stop_profiling(),
    BottleneckReport = erlmcp_bottleneck_detector:stop_detection(),

    %% Combine into comprehensive report
    ComprehensiveReport = #{
        timestamp => erlang:system_time(millisecond),
        cpu_profiling => maps:get(cpu_report, CpuReport, CpuReport),
        memory_profiling => maps:get(memory_report, MemoryReport, MemoryReport),
        latency_profiling => maps:get(latency_report, LatencyReport, LatencyReport),
        bottleneck_detection => maps:get(bottleneck_report, BottleneckReport, BottleneckReport),
        summary => generate_summary(CpuReport, MemoryReport, LatencyReport, BottleneckReport)
    },

    logger:info("Profiling report generated: ~p", [ComprehensiveReport]),
    ComprehensiveReport.

%% @doc Get comprehensive analysis of current profiling state
-spec get_comprehensive_analysis() -> map().
get_comprehensive_analysis() ->
    CpuAnalysis = erlmcp_cpu_profiler:analyze_cpu_usage(),
    LatencyAnalysis = erlmcp_latency_profiler:analyze_latency_distribution(),
    MemoryStats = erlmcp_memory_profiler:get_profiling_data(),
    BottleneckReport = erlmcp_bottleneck_detector:get_bottleneck_report(),

    #{
        cpu => CpuAnalysis,
        latency => LatencyAnalysis,
        memory => MemoryStats,
        bottlenecks => BottleneckReport,
        analysis_time => erlang:system_time(millisecond)
    }.

%% @doc Get executive summary of profiling results
-spec get_executive_summary() -> map().
get_executive_summary() ->
    %% Get all data
    CpuStats = erlmcp_cpu_profiler:get_cpu_stats(),
    LatencyStats = erlmcp_latency_profiler:get_latency_stats(),
    LatencyPercentiles = erlmcp_latency_profiler:get_percentiles(),
    BottleneckReport = erlmcp_bottleneck_detector:get_bottleneck_report(),

    %% Extract key metrics
    CpuOverhead = erlmcp_cpu_profiler:get_cpu_overhead(),
    TopFunctions = erlmcp_cpu_profiler:get_top_functions(3),
    SlowOps = erlmcp_latency_profiler:get_slow_operations(100000),

    #{
        summary => "Comprehensive Performance Analysis",
        cpu => #{
            functions_tracked => maps:get(total_functions, CpuStats, 0),
            total_calls => maps:get(total_calls, CpuStats, 0),
            profiler_overhead_percent => CpuOverhead,
            top_3_functions => TopFunctions
        },
        latency => #{
            total_samples => maps:get(total_samples, LatencyStats, 0),
            avg_latency_us => maps:get(avg_latency_us, LatencyStats, 0),
            p50_us => maps:get(p50, LatencyPercentiles, 0),
            p95_us => maps:get(p95, LatencyPercentiles, 0),
            p99_us => maps:get(p99, LatencyPercentiles, 0),
            slow_operations => length(SlowOps)
        },
        bottlenecks => #{
            total_alerts => maps:get(total_alerts, BottleneckReport, 0),
            critical_alerts => maps:get(critical_alerts, BottleneckReport, 0),
            cpu_incidents => maps:get(cpu_incidents, BottleneckReport, 0),
            latency_incidents => maps:get(latency_incidents, BottleneckReport, 0),
            memory_incidents => maps:get(memory_incidents, BottleneckReport, 0)
        },
        recommendations => erlmcp_bottleneck_detector:get_recommendations()
    }.

%% @doc Generate profiling report file
-spec generate_profiling_report_file(string()) -> {ok, string()} | {error, term()}.
generate_profiling_report_file(Filename) ->
    Report = get_executive_summary(),
    ReportText = format_report(Report),

    case file:write_file(Filename, ReportText) of
        ok ->
            logger:info("Report written to ~s", [Filename]),
            {ok, Filename};
        Error ->
            logger:error("Failed to write report: ~p", [Error]),
            Error
    end.

%% @doc Benchmark the overhead of profiling tools
-spec benchmark_profiling_overhead() -> map().
benchmark_profiling_overhead() ->
    logger:info("Benchmarking profiling overhead"),

    %% Baseline (no profiling)
    {CpuBefore1, _} = erlang:statistics(runtime),
    WallBefore1 = erlang:system_time(microsecond),

    lists:foreach(
        fun(_) -> crypto:hash(sha256, <<"test">>) end,
        lists:seq(1, 10000)
    ),

    {CpuAfter1, _} = erlang:statistics(runtime),
    WallAfter1 = erlang:system_time(microsecond),
    BaselineCpu = CpuAfter1 - CpuBefore1,
    BaselineWall = WallAfter1 - WallBefore1,

    %% With profiling
    erlmcp_cpu_profiler:start_profiling(),
    erlmcp_latency_profiler:start_profiling(),

    {CpuBefore2, _} = erlang:statistics(runtime),
    WallBefore2 = erlang:system_time(microsecond),

    lists:foreach(
        fun(I) ->
            erlmcp_cpu_profiler:measure_function_call({test, bench, I rem 10}, 10),
            erlmcp_latency_profiler:measure_operation(bench_op, 100),
            crypto:hash(sha256, <<"test">>)
        end,
        lists:seq(1, 10000)
    ),

    {CpuAfter2, _} = erlang:statistics(runtime),
    WallAfter2 = erlang:system_time(microsecond),
    ProfiledCpu = CpuAfter2 - CpuBefore2,
    ProfiledWall = WallAfter2 - WallBefore2,

    erlmcp_cpu_profiler:stop_profiling(),
    erlmcp_latency_profiler:stop_profiling(),

    OverheadCpu = case BaselineCpu of
        0 -> 0.0;
        _ -> ((ProfiledCpu - BaselineCpu) / BaselineCpu) * 100.0
    end,

    OverheadWall = case BaselineWall of
        0 -> 0.0;
        _ -> ((ProfiledWall - BaselineWall) / BaselineWall) * 100.0
    end,

    Report = #{
        baseline_cpu_ms => BaselineCpu,
        baseline_wall_us => BaselineWall,
        profiled_cpu_ms => ProfiledCpu,
        profiled_wall_us => ProfiledWall,
        overhead_cpu_percent => OverheadCpu,
        overhead_wall_percent => OverheadWall,
        assessment => assess_overhead(OverheadCpu)
    },

    logger:info("Profiling overhead benchmark: ~p", [Report]),
    Report.

%% @doc Stress test profilers at 100K scale
-spec stress_test_profilers(pos_integer()) -> map().
stress_test_profilers(ConcurrentOps) ->
    logger:info("Stress testing profilers at ~B concurrent operations", [ConcurrentOps]),

    start_full_profiling(),
    StartTime = erlang:system_time(microsecond),

    %% Create workers
    NumWorkers = max(10, ConcurrentOps div 10000),
    OpsPerWorker = ConcurrentOps div NumWorkers,

    logger:info("Creating ~B workers with ~B ops each", [NumWorkers, OpsPerWorker]),

    Pids = [spawn_link(fun() ->
        run_stress_worker(OpsPerWorker)
    end) || _ <- lists:seq(1, NumWorkers)],

    %% Monitor workers
    lists:foreach(fun(Pid) -> monitor(process, Pid) end, Pids),

    %% Wait for completion
    wait_stress_workers(NumWorkers),

    ElapsedUs = erlang:system_time(microsecond) - StartTime,
    ThroughputOpsPerSec = (ConcurrentOps * 1000000) / ElapsedUs,

    Report = stop_and_generate_report(),

    ResultReport = Report#{
        stress_test => #{
            concurrent_operations => ConcurrentOps,
            workers_used => NumWorkers,
            elapsed_us => ElapsedUs,
            throughput_ops_per_sec => ThroughputOpsPerSec
        }
    },

    logger:info("Stress test complete: ~.0f ops/sec", [ThroughputOpsPerSec]),
    ResultReport.

%% @doc Analyze 100K concurrent scenario
-spec analyze_100k_scenario(map()) -> map().
analyze_100k_scenario(Options) ->
    logger:info("Analyzing 100K concurrent scenario"),

    TargetOps = maps:get(target_ops, Options, 100000),
    DurationSeconds = maps:get(duration_seconds, Options, 60),

    StressReport = stress_test_profilers(TargetOps),
    ExecutiveSummary = get_executive_summary(),

    AnalysisReport = StressReport#{
        analysis => ExecutiveSummary,
        target_ops => TargetOps,
        target_duration_seconds => DurationSeconds,
        analysis_time => erlang:system_time(millisecond)
    },

    logger:info("100K scenario analysis complete"),
    AnalysisReport.

%%====================================================================
%% Internal Functions
%%====================================================================

-spec generate_summary(map(), map(), map(), map()) -> map().
generate_summary(CpuReport, MemoryReport, LatencyReport, BottleneckReport) ->
    #{
        cpu_functions => maps:get(functions_tracked, CpuReport, 0),
        cpu_overhead_percent => erlmcp_cpu_profiler:get_cpu_overhead(),
        memory_tracked_mb => maps:get(total_memory_delta, MemoryReport, 0) / (1024 * 1024),
        latency_samples => maps:get(total_samples, LatencyReport, 0),
        bottleneck_alerts => maps:get(total_alerts, BottleneckReport, 0)
    }.

-spec run_stress_worker(pos_integer()) -> ok.
run_stress_worker(NumOps) ->
    lists:foreach(
        fun(I) ->
            %% Simulate varied operations
            Latency = case I rem 100 of
                0 -> 150000;    %% 150ms - slow
                N when N < 10 -> 50000;  %% 50ms - moderate
                _ -> 5000       %% 5ms - fast
            end,

            erlmcp_latency_profiler:measure_operation(stress_op, Latency),
            erlmcp_cpu_profiler:measure_function_call({stress, worker, I rem 20}, Latency div 20),

            %% Simulate CPU work
            crypto:hash(sha256, <<I:64>>)
        end,
        lists:seq(1, NumOps)
    ).

-spec wait_stress_workers(integer()) -> ok.
wait_stress_workers(0) ->
    ok;
wait_stress_workers(Remaining) ->
    receive
        {'DOWN', _, process, _, _} ->
            wait_stress_workers(Remaining - 1)
    after 120000 ->
        logger:warning("Timeout waiting for stress workers")
    end.

-spec assess_overhead(float()) -> atom().
assess_overhead(Overhead) when Overhead < 5.0 ->
    excellent;
assess_overhead(Overhead) when Overhead < 10.0 ->
    good;
assess_overhead(Overhead) when Overhead < 20.0 ->
    acceptable;
assess_overhead(_) ->
    high.

-spec format_report(map()) -> binary().
format_report(Report) ->
    Lines = [
        "=== COMPREHENSIVE PROFILING REPORT ===\n\n",
        "CPU Profiling:\n",
        format_cpu_section(maps:get(cpu, Report, #{})),
        "\nLatency Profiling:\n",
        format_latency_section(maps:get(latency, Report, #{})),
        "\nBottleneck Detection:\n",
        format_bottleneck_section(maps:get(bottlenecks, Report, #{})),
        "\nRecommendations:\n",
        format_recommendations(maps:get(recommendations, Report, []))
    ],
    unicode:characters_to_binary(lists:flatten(Lines)).

-spec format_cpu_section(map()) -> string().
format_cpu_section(CPU) ->
    io_lib:format(
        "  Functions Tracked: ~B\n  Total Calls: ~B\n  Profiler Overhead: ~.2f%\n  Top 3 Functions:\n~s",
        [
            maps:get(functions_tracked, CPU, 0),
            maps:get(total_calls, CPU, 0),
            maps:get(profiler_overhead_percent, CPU, 0),
            format_functions(maps:get(top_3_functions, CPU, []))
        ]
    ).

-spec format_latency_section(map()) -> string().
format_latency_section(Latency) ->
    io_lib:format(
        "  Total Samples: ~B\n  Average: ~.0f us\n  p50: ~B us\n  p95: ~B us\n  p99: ~B us\n  Slow Operations: ~B\n",
        [
            maps:get(total_samples, Latency, 0),
            maps:get(avg_latency_us, Latency, 0),
            maps:get(p50_us, Latency, 0),
            maps:get(p95_us, Latency, 0),
            maps:get(p99_us, Latency, 0),
            maps:get(slow_operations, Latency, 0)
        ]
    ).

-spec format_bottleneck_section(map()) -> string().
format_bottleneck_section(Bottlenecks) ->
    io_lib:format(
        "  Total Alerts: ~B\n  Critical: ~B\n  CPU Incidents: ~B\n  Latency Incidents: ~B\n  Memory Incidents: ~B\n",
        [
            maps:get(total_alerts, Bottlenecks, 0),
            maps:get(critical_alerts, Bottlenecks, 0),
            maps:get(cpu_incidents, Bottlenecks, 0),
            maps:get(latency_incidents, Bottlenecks, 0),
            maps:get(memory_incidents, Bottlenecks, 0)
        ]
    ).

-spec format_functions([map()]) -> string().
format_functions([]) ->
    "    (None)\n";
format_functions(Funcs) ->
    lists:flatten([
        io_lib:format("    ~p: ~B calls, ~.1f%\n",
                      [maps:get(mfa, F, unknown),
                       maps:get(call_count, F, 0),
                       maps:get(cpu_percentage, F, 0)])
        || F <- Funcs
    ]).

-spec format_recommendations([string()]) -> string().
format_recommendations([]) ->
    "  (No recommendations)\n";
format_recommendations(Recs) ->
    lists:flatten([
        io_lib:format("  - ~s\n", [R])
        || R <- lists:usort(Recs)
    ]).
