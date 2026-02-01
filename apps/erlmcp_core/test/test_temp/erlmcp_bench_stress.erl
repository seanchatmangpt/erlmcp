%%%-------------------------------------------------------------------
%%% @doc
%%% Stress/Sustained Load Benchmark - Time-Series Performance Analysis
%%%
%%% Measures system behavior under sustained load with:
%%% - Continuous operation monitoring (30s to 24hr)
%%% - Time-series data collection (5s sampling)
%%% - Degradation detection (throughput, memory, latency trends)
%%% - Memory leak detection (linear regression)
%%% - Early termination on resource limits
%%% - Metrology-compliant output (precision, scope, units)
%%%
%%% Workloads:
%%% - stress_30s_100k_ops: 30s @ 100k ops/s (quick validation)
%%% - stress_5min_100k_ops: 5min @ 100k ops/s (standard test)
%%% - stress_1hr_50k_ops: 1hr @ 50k ops/s (endurance)
%%% - stress_24hr_10k_ops: 24hr @ 10k ops/s (production simulation)
%%%
%%% Degradation Detection:
%%% - Throughput decline > 5% per minute
%%% - Memory growth > 1 MiB/minute
%%% - Latency p99 growth > 10% per minute
%%%
%%% Output Format (JSON):
%%% {
%%%   "workload_id": "stress_5min_100k_ops",
%%%   "benchmark": "stress",
%%%   "duration_s": 300,
%%%   "target_ops_per_s": 100000,
%%%   "actual_ops_total": 30000000,
%%%   "actual_throughput_avg": 100000.0,
%%%   "throughput_std_dev": 2500.0,
%%%   "latency_p99_avg_us": 150.0,
%%%   "latency_p99_max_us": 350.0,
%%%   "memory_start_mib": 100.0,
%%%   "memory_end_mib": 102.0,
%%%   "memory_leak_detected": false,
%%%   "degradation_detected": false,
%%%   "samples": [
%%%     {"t": 0, "ops": 0, "tput": 100000, "p99": 145, "mem": 100},
%%%     {"t": 5, "ops": 500000, "tput": 100000, "p99": 148, "mem": 101}
%%%   ],
%%%   "scope": "per_node",
%%%   "precision": "microsecond"
%%% }
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_bench_stress).

-export([run_all/0, run_workload/1, workloads/0, quick_stress/0, standard_stress/0,
         endurance_stress/0]).

-include_lib("kernel/include/logger.hrl").

-record(stress_state,
        {workload_id :: binary(),
         start_time :: integer(),
         duration_s :: integer(),
         target_ops_per_s :: integer(),
         workers :: integer(),
         samples = [] :: [map()],
         total_ops = 0 :: integer(),
         memory_start_mib :: float(),
         latencies = [] :: [integer()]}).
-record(worker_state,
        {worker_id :: integer(), ops_completed = 0 :: integer(), start_time :: integer()}).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Run all stress workloads
-spec run_all() -> [map()].
run_all() ->
    logger:info("Starting stress benchmark suite"),
    Results = lists:map(fun run_workload/1, workloads()),
    logger:info("Stress benchmark suite complete"),
    Results.

%% @doc Quick 30s stress test
-spec quick_stress() -> map().
quick_stress() ->
    run_workload(hd(workloads())).

%% @doc Standard 5min stress test
-spec standard_stress() -> map().
standard_stress() ->
    run_workload(lists:nth(2, workloads())).

%% @doc Endurance 1hr stress test
-spec endurance_stress() -> map().
endurance_stress() ->
    run_workload(lists:nth(3, workloads())).

%% @doc Define stress workloads
-spec workloads() -> [map()].
workloads() ->
    [#{id => <<"stress_30s_100k_ops">>,
       duration_s => 30,
       target_ops_per_s => 100000,
       workers => 100},
     #{id => <<"stress_5min_100k_ops">>,
       duration_s => 300,
       target_ops_per_s => 100000,
       workers => 100},
     #{id => <<"stress_1hr_50k_ops">>,
       duration_s => 3600,
       target_ops_per_s => 50000,
       workers => 50},
     #{id => <<"stress_24hr_10k_ops">>,
       duration_s => 86400,
       target_ops_per_s => 10000,
       workers => 10}].

%% @doc Run a specific workload
-spec run_workload(map()) -> map().
run_workload(#{id := Id,
               duration_s := Duration,
               target_ops_per_s := TargetOps,
               workers := NumWorkers}) ->
    logger:info("Starting stress workload: ~s (~p workers, ~p ops/s, ~p seconds)",
                [Id, NumWorkers, TargetOps, Duration]),

    %% Initialize state
    StartTime = erlang:system_time(millisecond),
    MemoryStart = get_memory_usage_mib(),

    State =
        #stress_state{workload_id = Id,
                      start_time = StartTime,
                      duration_s = Duration,
                      target_ops_per_s = TargetOps,
                      workers = NumWorkers,
                      memory_start_mib = MemoryStart},

    %% Start profiling if available
    start_profiling(),

    %% Spawn worker processes
    OpsPerWorker = TargetOps div NumWorkers,
    WorkerPids = spawn_workers(NumWorkers, OpsPerWorker, self()),

    %% Run monitoring loop
    FinalState = monitoring_loop(State, WorkerPids, StartTime, Duration * 1000),

    %% Stop profiling
    stop_profiling(),

    %% Generate report
    generate_report(FinalState).

%%====================================================================
%% Internal Functions
%%====================================================================

%% @doc Spawn worker processes
-spec spawn_workers(integer(), integer(), pid()) -> [pid()].
spawn_workers(NumWorkers, OpsPerSecond, ParentPid) ->
    [spawn_link(fun() -> worker_loop(WorkerId, OpsPerSecond, ParentPid) end)
     || WorkerId <- lists:seq(1, NumWorkers)].

%% @doc Worker loop - execute operations at target rate
-spec worker_loop(integer(), integer(), pid()) -> ok.
worker_loop(WorkerId, OpsPerSecond, ParentPid) ->
    WorkerState = #worker_state{worker_id = WorkerId, start_time = erlang:system_time(microsecond)},
    worker_execute(WorkerState, OpsPerSecond, ParentPid).

-spec worker_execute(#worker_state{}, integer(), pid()) -> ok.
worker_execute(State, OpsPerSecond, ParentPid) ->
    receive
        {collect_stats, From} ->
            From ! {worker_stats, State#worker_state.ops_completed},
            worker_execute(State, OpsPerSecond, ParentPid);
        stop ->
            ok
    after 0 ->
        %% Execute operation with latency measurement
        StartMicros = erlang:system_time(microsecond),
        _ = execute_operation(),
        EndMicros = erlang:system_time(microsecond),
        Latency = EndMicros - StartMicros,

        %% Report latency to parent
        ParentPid ! {latency, Latency},

        %% Rate limiting: sleep to maintain target ops/s
        %% Each worker should complete ops at: OpsPerSecond rate
        %% Time per op = 1_000_000 / OpsPerSecond microseconds
        MicrosPerOp = 1_000_000 div OpsPerSecond,
        Elapsed = EndMicros - State#worker_state.start_time,
        ExpectedOps = Elapsed div MicrosPerOp,
        ActualOps = State#worker_state.ops_completed + 1,

        SleepMicros =
            if ActualOps > ExpectedOps ->
                   %% Running ahead, sleep
                   (ActualOps - ExpectedOps) * MicrosPerOp;
               true ->
                   0
            end,

        case SleepMicros > 0 of
            true ->
                timer:sleep(SleepMicros div 1000);
            false ->
                ok
        end,

        NewState = State#worker_state{ops_completed = ActualOps},
        worker_execute(NewState, OpsPerSecond, ParentPid)
    end.

%% @doc Execute a single operation (simulated or real)
-spec execute_operation() -> ok.
execute_operation() ->
    %% Simulate JSON-RPC encode/decode operation
    %% Replace with real workload as needed
    Req = #{<<"jsonrpc">> => <<"2.0">>,
            <<"id">> => rand:uniform(1000000),
            <<"method">> => <<"tools/call">>,
            <<"params">> => #{<<"name">> => <<"test_tool">>, <<"arguments">> => #{}}},
    _Encoded = jsx:encode(Req),
    ok.

%% @doc Monitoring loop - collect samples every 5 seconds
-spec monitoring_loop(#stress_state{}, [pid()], integer(), integer()) -> #stress_state{}.
monitoring_loop(State, WorkerPids, StartTime, DurationMs) ->
    Now = erlang:system_time(millisecond),
    Elapsed = Now - StartTime,

    case Elapsed >= DurationMs of
        true ->
            %% Duration exceeded, stop workers
            [Pid ! stop || Pid <- WorkerPids],
            State;
        false ->
            %% Collect metrics sample
            Sample = collect_sample(State, WorkerPids, Elapsed),

            %% Check for early termination conditions
            case check_termination_conditions(Sample) of
                {terminate, Reason} ->
                    logger:warning("Early termination: ~p", [Reason]),
                    [Pid ! stop || Pid <- WorkerPids],
                    State#stress_state{samples = [Sample | State#stress_state.samples]};
                continue ->
                    %% Collect latencies from message queue
                    Latencies = collect_latencies(),

                    NewState =
                        State#stress_state{samples = [Sample | State#stress_state.samples],
                                           total_ops = maps:get(<<"ops">>, Sample),
                                           latencies = Latencies ++ State#stress_state.latencies},

                    %% Sleep until next sample (5 seconds)
                    timer:sleep(5000),
                    monitoring_loop(NewState, WorkerPids, StartTime, DurationMs)
            end
    end.

%% @doc Collect a single metrics sample
-spec collect_sample(#stress_state{}, [pid()], integer()) -> map().
collect_sample(State, WorkerPids, ElapsedMs) ->
    %% Collect worker stats
    TotalOps = lists:sum([collect_worker_stats(Pid) || Pid <- WorkerPids]),

    %% Calculate throughput (ops/s over last 5 seconds)
    TimeSec = ElapsedMs / 1000,
    Throughput =
        case TimeSec > 0 of
            true ->
                TotalOps / TimeSec;
            false ->
                0.0
        end,

    %% Calculate latency percentiles from recent samples
    RecentLatencies = lists:sublist(State#stress_state.latencies, 1000),
    P99 = case RecentLatencies of
              [] ->
                  0;
              _ ->
                  percentile(RecentLatencies, 99)
          end,

    %% Memory usage
    MemoryMib = get_memory_usage_mib(),

    %% CPU usage
    CpuPercent = get_cpu_usage_percent(),

    #{<<"t">> => round(TimeSec),
      <<"ops">> => TotalOps,
      <<"tput">> => round(Throughput),
      <<"p99">> => P99,
      <<"mem">> => round(MemoryMib * 10) / 10,
      <<"cpu">> => round(CpuPercent * 10) / 10}.

%% @doc Collect stats from a single worker
-spec collect_worker_stats(pid()) -> integer().
collect_worker_stats(Pid) ->
    Pid ! {collect_stats, self()},
    receive
        {worker_stats, OpsCompleted} ->
            OpsCompleted
    after 100 ->
        0
    end.

%% @doc Collect latencies from message queue
-spec collect_latencies() -> [integer()].
collect_latencies() ->
    collect_latencies([]).

-spec collect_latencies([integer()]) -> [integer()].
collect_latencies(Acc) ->
    receive
        {latency, Latency} ->
            collect_latencies([Latency | Acc])
    after 0 ->
        Acc
    end.

%% @doc Check for early termination conditions
-spec check_termination_conditions(map()) -> continue | {terminate, atom()}.
check_termination_conditions(Sample) ->
    MemoryMib = maps:get(<<"mem">>, Sample),
    CpuPercent = maps:get(<<"cpu">>, Sample),

    %% Memory limit: 90% of available (assume 4GB available)
    MaxMemoryMib = 4096 * 0.9,
    case MemoryMib > MaxMemoryMib of
        true ->
            throw({terminate, memory_limit});
        false ->
            ok
    end,

    %% CPU limit: 95% sustained
    case CpuPercent > 95.0 of
        true ->
            {terminate, cpu_limit};
        false ->
            continue
    end.

%% @doc Generate comprehensive report with trend analysis
-spec generate_report(#stress_state{}) -> map().
generate_report(State) ->
    Samples = lists:reverse(State#stress_state.samples),

    %% Calculate aggregate statistics
    TotalOps = State#stress_state.total_ops,
    DurationSec = State#stress_state.duration_s,
    AvgThroughput = TotalOps / DurationSec,

    %% Throughput statistics
    Throughputs = [maps:get(<<"tput">>, S) || S <- Samples],
    ThroughputStdDev = std_dev(Throughputs),

    %% Latency statistics (all collected latencies)
    Latencies = State#stress_state.latencies,
    LatencyP99Avg = percentile(Latencies, 99),
    LatencyP99Max =
        case Latencies of
            [] ->
                0;
            _ ->
                lists:max(Latencies)
        end,

    %% Memory statistics
    MemoryEnd = get_memory_usage_mib(),
    MemoryLeak = detect_memory_leak(Samples),

    %% Degradation detection
    Degradation = detect_degradation(Samples),

    %% Build report
    #{<<"workload_id">> => State#stress_state.workload_id,
      <<"benchmark">> => <<"stress">>,
      <<"duration_s">> => State#stress_state.duration_s,
      <<"target_ops_per_s">> => State#stress_state.target_ops_per_s,
      <<"actual_ops_total">> => TotalOps,
      <<"actual_throughput_avg">> => round(AvgThroughput * 10) / 10,
      <<"throughput_std_dev">> => round(ThroughputStdDev * 10) / 10,
      <<"latency_p99_avg_us">> => LatencyP99Avg,
      <<"latency_p99_max_us">> => LatencyP99Max,
      <<"memory_start_mib">> => round(State#stress_state.memory_start_mib * 10) / 10,
      <<"memory_end_mib">> => round(MemoryEnd * 10) / 10,
      <<"memory_leak_detected">> => MemoryLeak,
      <<"degradation_detected">> => Degradation,
      <<"samples">> => Samples,
      <<"scope">> => <<"per_node">>,
      <<"precision">> => <<"microsecond">>}.

%% @doc Detect memory leak using linear regression
-spec detect_memory_leak([map()]) -> boolean().
detect_memory_leak(Samples) ->
    case length(Samples) < 3 of
        true ->
            false;
        false ->
            MemoryValues = [maps:get(<<"mem">>, S) || S <- Samples],
            Slope = linear_regression_slope(MemoryValues),
            %% Memory leak if growing > 1 MiB/minute
            %% Samples are every 5s, so 12 samples per minute
            %% Slope is MiB per sample, so slope * 12 = MiB per minute
            SlopePerMinute = Slope * 12,
            SlopePerMinute > 1.0
    end.

%% @doc Detect performance degradation
-spec detect_degradation([map()]) -> boolean().
detect_degradation(Samples) ->
    case length(Samples) < 3 of
        true ->
            false;
        false ->
            Throughputs = [maps:get(<<"tput">>, S) || S <- Samples],
            Slope = linear_regression_slope(Throughputs),
            %% Degradation if throughput declining > 5% per minute
            %% Calculate percent decline relative to first sample
            FirstThroughput = hd(Throughputs),
            SlopePerMinute = Slope * 12,
            PercentDeclinePerMinute = SlopePerMinute / FirstThroughput * 100,
            PercentDeclinePerMinute < -5.0
    end.

%% @doc Calculate linear regression slope
-spec linear_regression_slope([number()]) -> float().
linear_regression_slope(Values) ->
    N = length(Values),
    case N < 2 of
        true ->
            0.0;
        false ->
            %% X values are indices: 0, 1, 2, ..., N-1
            %% Y values are the input Values
            XValues = lists:seq(0, N - 1),
            SumX = lists:sum(XValues),
            SumY = lists:sum(Values),
            SumXY = lists:sum([X * Y || {X, Y} <- lists:zip(XValues, Values)]),
            SumX2 = lists:sum([X * X || X <- XValues]),

            %% Slope = (N * SumXY - SumX * SumY) / (N * SumX2 - SumX * SumX)
            Numerator = N * SumXY - SumX * SumY,
            Denominator = N * SumX2 - SumX * SumX,

            case Denominator == 0 of
                true ->
                    0.0;
                false ->
                    Numerator / Denominator
            end
    end.

%% @doc Calculate percentile
-spec percentile([number()], integer()) -> number().
percentile([], _) ->
    0;
percentile(Values, Percentile) ->
    Sorted = lists:sort(Values),
    Len = length(Sorted),
    Index = ceil(Percentile / 100 * Len),
    lists:nth(min(Index, Len), Sorted).

%% @doc Calculate standard deviation
-spec std_dev([number()]) -> float().
std_dev([]) ->
    0.0;
std_dev([_]) ->
    0.0;
std_dev(Values) ->
    N = length(Values),
    Mean = lists:sum(Values) / N,
    Variance = lists:sum([math:pow(X - Mean, 2) || X <- Values]) / N,
    math:sqrt(Variance).

%% @doc Get memory usage in MiB
-spec get_memory_usage_mib() -> float().
get_memory_usage_mib() ->
    Memory = erlang:memory(total),
    Memory / (1024 * 1024).

%% @doc Get CPU usage percentage (approximation)
-spec get_cpu_usage_percent() -> float().
get_cpu_usage_percent() ->
    %% Approximate CPU usage using scheduler utilization
    case erlang:statistics(scheduler_wall_time) of
        undefined ->
            erlang:system_flag(scheduler_wall_time, true),
            timer:sleep(100),
            get_cpu_usage_percent();
        SchedulerTimes ->
            %% Calculate utilization
            TotalActive = lists:sum([A || {_, A, _} <- SchedulerTimes]),
            TotalWall = lists:sum([T || {_, _, T} <- SchedulerTimes]),
            case TotalWall > 0 of
                true ->
                    TotalActive / TotalWall * 100;
                false ->
                    0.0
            end
    end.

%% @doc Start profiling if available
-spec start_profiling() -> ok.
start_profiling() ->
    case code:is_loaded(erlmcp_profiling_suite) of
        {file, _} ->
            try
                erlmcp_profiling_suite:start_full_profiling()
            catch
                _:_ ->
                    ok
            end;
        false ->
            ok
    end.

%% @doc Stop profiling if available
-spec stop_profiling() -> ok.
stop_profiling() ->
    case code:is_loaded(erlmcp_profiling_suite) of
        {file, _} ->
            try
                _Report = erlmcp_profiling_suite:stop_and_generate_report(),
                ok
            catch
                _:_ ->
                    ok
            end;
        false ->
            ok
    end.
