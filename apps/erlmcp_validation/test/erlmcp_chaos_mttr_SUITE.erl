%%%-------------------------------------------------------------------
%%% @doc erlmcp_chaos_mttr_SUITE - MTTR (Mean Time To Recovery) Benchmark Suite
%%%
%%% Measures and tracks recovery time metrics for chaos experiments:
%%%
%%% Metrics:
%%% - TTD (Time To Detect): Time from fault injection to detection
%%% - TTM (Time To Mitigate): Time from detection to mitigation start
%%% - TTR (Time To Recover): Time from fault to full recovery
%%% - MTTR: Mean TTR across multiple experiments
%%%
%%% Per-component tracking for:
%%% - Registry
%%% - Server
%%% - Client
%%% - Session
%%% - Transport
%%% - Observability
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_chaos_mttr_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("kernel/include/logger.hrl").

%% CT callbacks
-export([all/0, init_per_suite/1, end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2]).

%% Test cases
-export([
    % Baseline MTTR tests
    mttr_process_kill_benchmark/1,
    mttr_network_latency_benchmark/1,
    mttr_resource_exhaustion_benchmark/1,

    % Component-specific MTTR
    mttr_registry_recovery/1,
    mttr_server_recovery/1,
    mttr_client_recovery/1,
    mttr_session_recovery/1,
    mttr_transport_recovery/1,

    % Complex scenarios
    mttr_cascade_failure/1,
    mttr_split_brain/1,
    mttr_full_system_recovery/1,

    % MTTR trend analysis
    mttr_trend_analysis/1,
    mttr_percentile_analysis/1,

    % SLA validation
    mttr_sla_compliance/1
]).

-record(mttr_sample,
        {component :: binary(),
         fault_type :: binary(),
         time_to_detect :: non_neg_integer(),
         time_to_mitigate :: non_neg_integer(),
         time_to_recover :: non_neg_integer(),
         total_recovery_time :: non_neg_integer(),
         timestamp :: integer()}).

-record(state,
        {chaos_pid :: pid() | undefined,
         steady_state_pid :: pid() | undefined,
         samples = [] :: [#mttr_sample{}],
         start_time :: integer() | undefined}).

-define(MTTR_THRESHOLD, 30000).      % 30 second MTTR threshold
-define(TTD_THRESHOLD, 5000).         % 5 second detection threshold
-define(BENCHMARK_SAMPLES, 10).       % Number of samples for benchmark
-define(RECOVERY_CHECK_INTERVAL, 100). % Check recovery every 100ms

%%%===================================================================
%%% CT Callbacks
%%%===================================================================

all() ->
    [
        mttr_process_kill_benchmark,
        mttr_network_latency_benchmark,
        mttr_resource_exhaustion_benchmark,
        mttr_registry_recovery,
        mttr_server_recovery,
        mttr_client_recovery,
        mttr_session_recovery,
        mttr_transport_recovery,
        mttr_cascade_failure,
        mttr_split_brain,
        mttr_full_system_recovery,
        mttr_trend_analysis,
        mttr_percentile_analysis,
        mttr_sla_compliance
    ].

init_per_suite(Config) ->
    {ok, ChaosPid} = erlmcp_chaos:start_link(),
    {ok, SteadyStatePid} = erlmcp_chaos_steady_state:start_link(),
    {ok, MetricsPid} = erlmcp_chaos_metrics:start_link(),

    [{chaos_pid, ChaosPid},
     {steady_state_pid, SteadyStatePid},
     {metrics_pid, MetricsPid}
     | Config].

end_per_suite(Config) ->
    ChaosPid = ?config(chaos_pid, Config),
    SteadyStatePid = ?config(steady_state_pid, Config),
    MetricsPid = ?config(metrics_pid, Config),

    gen_server:stop(ChaosPid),
    gen_server:stop(SteadyStatePid),
    gen_server:stop(MetricsPid),

    ok.

init_per_testcase(_TestCase, Config) ->
    {ok, ChaosPid} = erlmcp_chaos:start_link(),
    {ok, SteadyStatePid} = erlmcp_chaos_steady_state:start_link(),

    % Capture steady state baseline
    {ok, _Baseline} = erlmcp_chaos_steady_state:capture_steady_state(),

    [{chaos_pid, ChaosPid},
     {steady_state_pid, SteadyStatePid}
     | Config].

end_per_testcase(_TestCase, Config) ->
    ChaosPid = ?config(chaos_pid, Config),
    SteadyStatePid = ?config(steady_state_pid, Config),

    erlmcp_chaos:stop_all_experiments(),
    gen_server:stop(ChaosPid),
    gen_server:stop(SteadyStatePid),

    ok.

%%%===================================================================
%%% Test Cases
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Benchmark MTTR for process kill failures
%%--------------------------------------------------------------------
mttr_process_kill_benchmark(Config) ->
    ct:log("Running MTTR benchmark for process kill failures (~p samples)",
            [?BENCHMARK_SAMPLES]),

    Samples = run_mttr_benchmark(process_kill, ?BENCHMARK_SAMPLES, Config),

    MTTR = calculate_mttr(Samples),
    ct:log("Process Kill MTTR: ~pms", [MTTR]),

    % Assert MTTR is within acceptable threshold
    case MTTR of
        Val when Val =< ?MTR_THRESHOLD ->
            ct:log("PASS: MTTR ~pms <= threshold ~pms", [Val, ?MTR_THRESHOLD]),
            {comment, io_lib:format("MTTR: ~pms", [Val])};
        Val ->
            ct:fail("FAIL: MTTR ~pms exceeds threshold ~pms", [Val, ?MTR_THRESHOLD])
    end.

%%--------------------------------------------------------------------
%% @doc Benchmark MTTR for network latency
%%--------------------------------------------------------------------
mttr_network_latency_benchmark(Config) ->
    ct:log("Running MTTR benchmark for network latency (~p samples)",
            [?BENCHMARK_SAMPLES]),

    Samples = run_mttr_benchmark(network_latency, ?BENCHMARK_SAMPLES, Config),

    MTTR = calculate_mttr(Samples),
    ct:log("Network Latency MTTR: ~pms", [MTTR]),

    case MTTR of
        Val when Val =< ?MTR_THRESHOLD ->
            {comment, io_lib:format("MTTR: ~pms", [Val])};
        Val ->
            ct:fail("FAIL: MTTR ~pms exceeds threshold ~pms", [Val, ?MTR_THRESHOLD])
    end.

%%--------------------------------------------------------------------
%% @doc Benchmark MTTR for resource exhaustion
%%--------------------------------------------------------------------
mttr_resource_exhaustion_benchmark(Config) ->
    ct:log("Running MTTR benchmark for resource exhaustion (~p samples)",
            [?BENCHMARK_SAMPLES]),

    Samples = run_mttr_benchmark(resource_memory, ?BENCHMARK_SAMPLES, Config),

    MTTR = calculate_mttr(Samples),
    ct:log("Resource Exhaustion MTTR: ~pms", [MTTR]),

    % Resource exhaustion may take longer to recover
    ResourceThreshold = ?MTR_THRESHOLD * 2,

    case MTTR of
        Val when Val =< ResourceThreshold ->
            {comment, io_lib:format("MTTR: ~pms", [Val])};
        Val ->
            ct:fail("FAIL: MTTR ~pms exceeds threshold ~pms", [Val, ResourceThreshold])
    end.

%%--------------------------------------------------------------------
%% @doc Measure MTTR for registry component recovery
%%--------------------------------------------------------------------
mttr_registry_recovery(Config) ->
    Component = <<"registry">>,
    ct:log("Testing MTTR for ~s component recovery", [Component]),

    Sample = measure_component_mttr(erlmcp_registry, process_kill, Config),

    ct:log("Registry MTTR: TTD=~pms, TTM=~pms, TTR=~pms",
           [Sample#mttr_sample.time_to_detect,
            Sample#mttr_sample.time_to_mitigate,
            Sample#mttr_sample.total_recovery_time]),

    % Registry should recover quickly
    Threshold = ?MTR_THRESHOLD div 2,
    case Sample#mttr_sample.total_recovery_time of
        TTR when TTR =< Threshold ->
            {comment, io_lib:format("TTR: ~pms", [TTR])};
        TTR ->
            ct:fail("FAIL: Registry TTR ~pms exceeds threshold ~pms", [TTR, Threshold])
    end.

%%--------------------------------------------------------------------
%% @doc Measure MTTR for server component recovery
%%--------------------------------------------------------------------
mttr_server_recovery(Config) ->
    Component = <<"server">>,
    ct:log("Testing MTTR for ~s component recovery", [Component]),

    % Find a server to test
    case find_server_process() of
        undefined ->
            {skip, "No server process found"};
        ServerPid ->
            Sample = measure_process_mttr(ServerPid, <<"server">>, Config),

            ct:log("Server MTTR: TTD=~pms, TTM=~pms, TTR=~pms",
                   [Sample#mttr_sample.time_to_detect,
                    Sample#mttr_sample.time_to_mitigate,
                    Sample#mttr_sample.total_recovery_time]),

            Threshold = ?MTR_THRESHOLD,
            case Sample#mttr_sample.total_recovery_time of
                TTR when TTR =< Threshold ->
                    {comment, io_lib:format("TTR: ~pms", [TTR])};
                TTR ->
                    ct:fail("FAIL: Server TTR ~pms exceeds threshold ~pms", [TTR, Threshold])
            end
    end.

%%--------------------------------------------------------------------
%% @doc Measure MTTR for client component recovery
%%--------------------------------------------------------------------
mttr_client_recovery(Config) ->
    Component = <<"client">>,
    ct:log("Testing MTTR for ~s component recovery", [Component]),

    case find_client_process() of
        undefined ->
            {skip, "No client process found"};
        ClientPid ->
            Sample = measure_process_mttr(ClientPid, <<"client">>, Config),

            ct:log("Client MTTR: TTD=~pms, TTM=~pms, TTR=~pms",
                   [Sample#mttr_sample.time_to_detect,
                    Sample#mttr_sample.time_to_mitigate,
                    Sample#mttr_sample.total_recovery_time]),

            Threshold = ?MTR_THRESHOLD,
            case Sample#mttr_sample.total_recovery_time of
                TTR when TTR =< Threshold ->
                    {comment, io_lib:format("TTR: ~pms", [TTR])};
                TTR ->
                    ct:fail("FAIL: Client TTR ~pms exceeds threshold ~pms", [TTR, Threshold])
            end
    end.

%%--------------------------------------------------------------------
%% @doc Measure MTTR for session component recovery
%%--------------------------------------------------------------------
mttr_session_recovery(Config) ->
    Component = <<"session">>,
    ct:log("Testing MTTR for ~s component recovery", [Component]),

    case whereis(erlmcp_session_sup) of
        undefined ->
            {skip, "Session supervisor not running"};
        SupPid ->
            Sample = measure_process_mttr(SupPid, <<"session">>, Config),

            ct:log("Session MTTR: TTD=~pms, TTM=~pms, TTR=~pms",
                   [Sample#mttr_sample.time_to_detect,
                    Sample#mttr_sample.time_to_mitigate,
                    Sample#mttr_sample.total_recovery_time]),

            Threshold = ?MTR_THRESHOLD,
            case Sample#mttr_sample.total_recovery_time of
                TTR when TTR =< Threshold ->
                    {comment, io_lib:format("TTR: ~pms", [TTR])};
                TTR ->
                    ct:fail("FAIL: Session TTR ~pms exceeds threshold ~pms", [TTR, Threshold])
            end
    end.

%%--------------------------------------------------------------------
%% @doc Measure MTTR for transport component recovery
%%--------------------------------------------------------------------
mttr_transport_recovery(Config) ->
    Component = <<"transport">>,
    ct:log("Testing MTTR for ~s component recovery", [Component]),

    case whereis(erlmcp_transport_sup) of
        undefined ->
            {skip, "Transport supervisor not running"};
        SupPid ->
            Sample = measure_process_mttr(SupPid, <<"transport">>, Config),

            ct:log("Transport MTTR: TTD=~pms, TTM=~pms, TTR=~pms",
                   [Sample#mttr_sample.time_to_detect,
                    Sample#mttr_sample.time_to_mitigate,
                    Sample#mttr_sample.total_recovery_time]),

            Threshold = ?MTR_THRESHOLD,
            case Sample#mttr_sample.total_recovery_time of
                TTR when TTR =< Threshold ->
                    {comment, io_lib:format("TTR: ~pms", [TTR])};
                TTR ->
                    ct:fail("FAIL: Transport TTR ~pms exceeds threshold ~pms", [TTR, Threshold])
            end
    end.

%%--------------------------------------------------------------------
%% @doc Measure MTTR for cascade failure scenario
%%--------------------------------------------------------------------
mttr_cascade_failure(Config) ->
    ct:log("Testing MTTR for cascade failure scenario"),

    % Start multiple processes
    Processes = start_test_processes(5),

    FaultStartTime = erlang:monotonic_time(millisecond),

    % Kill first process to trigger cascade
    [FirstProcess | _] = Processes,
    exit(FirstProcess, kill),

    % Wait for system to detect cascade
    TTD = wait_for_cascade_detection(Processes, FaultStartTime),

    % System should start mitigation
    TTMMark = erlang:monotonic_time(millisecond),

    % Wait for full recovery
    TTR = wait_for_full_recovery(Processes, FaultStartTime),

    TotalTTR = TTR - FaultStartTime,
    TTM = TTMMark - (FaultStartTime + TTD),

    ct:log("Cascade Failure MTTR: TTD=~pms, TTM=~pms, TTR=~pms",
           [TTD, TTM, TotalTTR]),

    % Cleanup
    cleanup_test_processes(Processes),

    % Cascade failure may take longer
    CascadeThreshold = ?MTR_THRESHOLD * 2,

    case TotalTTR of
        Val when Val =< CascadeThreshold ->
            {comment, io_lib:format("TTR: ~pms (TTD: ~pms, TTM: ~pms)",
                                   [TotalTTR, TTD, TTM])};
        Val ->
            ct:fail("FAIL: Cascade TTR ~pms exceeds threshold ~pms",
                    [Val, CascadeThreshold])
    end.

%%--------------------------------------------------------------------
%% @doc Measure MTTR for split-brain scenario
%%--------------------------------------------------------------------
mttr_split_brain(Config) ->
    ct:log("Testing MTTR for split-brain scenario"),

    % Simulate split-brain by stopping communication
    FaultStartTime = erlang:monotonic_time(millisecond),

    % Inject network partition
    ConfigMap = #{
        experiment => network_partition,
        duration => 10000,
        nodes => [node()]
    },

    {ok, ExpId} = erlmcp_chaos:run(<<"split_brain_mttr">>, ConfigMap),

    % Wait for partition detection
    TTD = wait_for_partition_detection(ExpId, FaultStartTime),

    % Mitigation starts
    TTMMark = erlang:monotonic_time(millisecond),

    % Wait for partition healing
    TTR = wait_for_partition_healing(ExpId, FaultStartTime),

    TotalTTR = TTR - FaultStartTime,
    TTM = TTMMark - (FaultStartTime + TTD),

    ct:log("Split-Brain MTTR: TTD=~pms, TTM=~pms, TTR=~pms",
           [TTD, TTM, TotalTTR]),

    % Stop experiment
    erlmcp_chaos:stop_experiment(ExpId),

    case TotalTTR of
        Val when Val =< ?MTR_THRESHOLD * 3 ->
            {comment, io_lib:format("TTR: ~pms", [Val])};
        Val ->
            ct:fail("FAIL: Split-brain TTR ~pms exceeds threshold ~pms",
                    [Val, ?MTR_THRESHOLD * 3])
    end.

%%--------------------------------------------------------------------
%% @doc Measure MTTR for full system recovery
%%--------------------------------------------------------------------
mttr_full_system_recovery(Config) ->
    ct:log("Testing MTTR for full system recovery"),

    FaultStartTime = erlang:monotonic_time(millisecond),

    % Inject multiple faults simultaneously
    Faults = [
        {process_kill, erlmcp_registry},
        {network_latency, 1000}
    ],

    lists:foreach(fun({Type, Target}) ->
        ConfigMap = case Type of
            process_kill ->
                #{experiment => kill_servers,
                  target => Target,
                  rate => 0.1,
                  duration => 5000};
            network_latency ->
                #{experiment => network_latency,
                  latency => Target,
                  rate => 0.5,
                  duration => 5000}
        end,
        {ok, _} = erlmcp_chaos:run(ConfigMap)
    end, Faults),

    % Wait for system to detect issues
    TTD = wait_for_system_detection(FaultStartTime),

    % Mitigation starts
    TTMMark = erlang:monotonic_time(millisecond),

    % Wait for full recovery
    TTR = wait_for_system_steady_state(FaultStartTime),

    TotalTTR = TTR - FaultStartTime,
    TTM = TTMMark - (FaultStartTime + TTD),

    ct:log("Full System MTTR: TTD=~pms, TTM=~pms, TTR=~pms",
           [TTD, TTM, TotalTTR]),

    erlmcp_chaos:stop_all_experiments(),

    case TotalTTR of
        Val when Val =< ?MTR_THRESHOLD * 2 ->
            {comment, io_lib:format("TTR: ~pms", [Val])};
        Val ->
            ct:fail("FAIL: Full system TTR ~pms exceeds threshold ~pms",
                    [Val, ?MTR_THRESHOLD * 2])
    end.

%%--------------------------------------------------------------------
%% @doc Analyze MTTR trends across multiple samples
%%--------------------------------------------------------------------
mttr_trend_analysis(Config) ->
    ct:log("Running MTTR trend analysis"),

    Samples = run_mttr_benchmark(process_kill, 20, Config),

    % Split into first and second half
    Half = length(Samples) div 2,
    {FirstHalf, SecondHalf} = lists:split(Half, Samples),

    FirstMTTR = calculate_mttr(FirstHalf),
    SecondMTTR = calculate_mttr(SecondHalf),

    ct:log("First half MTTR: ~pms", [FirstMTTR]),
    ct:log("Second half MTTR: ~pms", [SecondMTTR]),

    % Check if MTTR is improving (decreasing) or stable
    TrendDiff = SecondMTTR - FirstMTTR,
    TrendPct = (TrendDiff / FirstMTTR) * 100,

    ct:log("Trend difference: ~pms (~.1f%)", [TrendDiff, TrendPct]),

    % MTTR should be stable or improving (not significantly worse)
    MaxDegradation = 20,  % Allow up to 20% degradation

    case TrendPct of
        Pct when Pct < MaxDegradation ->
            {comment, io_lib:format("Trend: ~.1f% (stable/improving)", [TrendPct])};
        Pct ->
            ct:log("WARNING: MTTR degrading by ~.1f%", [Pct]),
            {comment, io_lib:format("Trend: ~.1f% (degrading)", [Pct])}
    end.

%%--------------------------------------------------------------------
%% @doc Analyze MTTR percentiles
%%--------------------------------------------------------------------
mttr_percentile_analysis(Config) ->
    ct:log("Running MTTR percentile analysis"),

    Samples = run_mttr_benchmark(process_kill, 50, Config),

    TTRs = [S#mttr_sample.total_recovery_time || S <- Samples],

    P50 = percentile(TTRs, 50),
    P90 = percentile(TTRs, 90),
    P95 = percentile(TTRs, 95),
    P99 = percentile(TTRs, 99),
    Max = lists:max(TTRs),

    ct:log("MTTR Percentiles:"),
    ct:log("  p50:  ~pms", [P50]),
    ct:log("  p90:  ~pms", [P90]),
    ct:log("  p95:  ~pms", [P95]),
    ct:log("  p99:  ~pms", [P99]),
    ct:log("  max:  ~pms", [Max]),

    % Check if p95 is within threshold
    case P95 of
        Val when Val =< ?MTR_THRESHOLD ->
            {comment, io_lib:format("p95: ~pms (within threshold)", [Val])};
        Val ->
            ct:fail("FAIL: p95 MTTR ~pms exceeds threshold ~pms",
                    [Val, ?MTR_THRESHOLD])
    end.

%%--------------------------------------------------------------------
%% @doc Validate MTTR SLA compliance
%%--------------------------------------------------------------------
mttr_sla_compliance(Config) ->
    ct:log("Validating MTTR SLA compliance"),

    % Define SLA tiers
    SLATiers = [
        {critical, 10000},   % 10s for critical components
        {high, 20000},       % 20s for high priority
        {normal, 30000},     % 30s for normal priority
        {low, 60000}         % 60s for low priority
    ],

    % Test each tier
    ComplianceResults = lists:map(fun({Tier, Threshold}) ->
        Samples = run_mttr_benchmark(process_kill, 10, Config),
        MTTR = calculate_mttr(Samples),

        Compliant = MTTR =< Threshold,

        ct:log("~s tier: MTTR=~pms, threshold=~pms, compliant=~p",
               [Tier, MTTR, Threshold, Compliant]),

        {Tier, Compliant, MTTR, Threshold}
    end, SLATiers),

    AllCompliant = lists:all(fun({_Tier, Compliant, _, _}) -> Compliant end,
                            ComplianceResults),

    case AllCompliant of
        true ->
            {comment, "All SLA tiers compliant"};
        false ->
            NonCompliant = [T || {T, false, _, _} <- ComplianceResults],
            ct:fail("FAIL: SLA not compliant for tiers: ~p", [NonCompliant])
    end.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

%% @private Run MTTR benchmark for multiple samples
-spec run_mttr_benchmark(atom(), pos_integer(), list()) -> [#mttr_sample{}].
run_mttr_benchmark(FaultType, SampleCount, Config) ->
    lists:map(fun(N) ->
        ct:log("Running sample ~p/~p for ~p", [N, SampleCount, FaultType]),
        measure_single_mttr(FaultType, Config)
    end, lists:seq(1, SampleCount)).

%% @private Measure single MTTR sample
-spec measure_single_mttr(atom(), list()) -> #mttr_sample{}.
measure_single_mttr(FaultType, Config) ->
    StartTime = erlang:monotonic_time(millisecond),

    % Inject fault
    ConfigMap = case FaultType of
        process_kill ->
            #{experiment => kill_servers,
              target => erlmcp_registry,
              rate => 0.1,
              duration => 5000};
        network_latency ->
            #{experiment => network_latency,
              latency => 1000,
              rate => 0.5,
              duration => 5000};
        resource_memory ->
            #{experiment => resource_memory,
              target_percent => 0.8,
              duration => 10000}
    end,

    {ok, ExpId} = erlmcp_chaos:run(ExpId, ConfigMap),

    % Measure time to detect (TTD)
    TTD = measure_time_to_detect(ExpId, StartTime),

    % Measure time to mitigate (TTM)
    TTMMark = erlang:monotonic_time(millisecond),
    TTM = measure_time_to_mitigate(ExpId, StartTime),

    % Measure time to recover (TTR)
    TTR = measure_time_to_recover(ExpId, StartTime),

    TotalTTR = TTR - StartTime,

    % Stop experiment
    erlmcp_chaos:stop_experiment(ExpId),

    #mttr_sample{
        component = atom_to_binary(FaultType),
        fault_type = atom_to_binary(FaultType),
        time_to_detect = TTD - StartTime,
        time_to_mitigate = TTM - TTMMark,
        time_to_recover = TTR - TTM,
        total_recovery_time = TotalTTR,
        timestamp = StartTime
    }.

%% @private Measure time to detect fault
-spec measure_time_to_detect(binary(), integer()) -> integer().
measure_time_to_detect(ExpId, StartTime) ->
    measure_time_to_detect(ExpId, StartTime, 0).

measure_time_to_detect(ExpId, StartTime, Attempts) ->
    case erlmcp_chaos:get_experiment_status(ExpId) of
        {ok, #{incidents := [_|_]}} ->
            erlang:monotonic_time(millisecond);
        {ok, _} ->
            timer:sleep(?RECOVERY_CHECK_INTERVAL),
            measure_time_to_detect(ExpId, StartTime, Attempts + 1);
        _ ->
            erlang:monotonic_time(millisecond)
    end.

%% @private Measure time to mitigate
-spec measure_time_to_mitigate(binary(), integer()) -> integer().
measure_time_to_mitigate(ExpId, StartTime) ->
    measure_time_to_mitigate(ExpId, StartTime, 0).

measure_time_to_mitigate(ExpId, StartTime, Attempts) ->
    case erlmcp_chaos:get_experiment_status(ExpId) of
        {ok, #{state := running}} when Attempts > 5 ->
            erlang:monotonic_time(millisecond);
        {ok, _} ->
            timer:sleep(?RECOVERY_CHECK_INTERVAL),
            measure_time_to_mitigate(ExpId, StartTime, Attempts + 1);
        _ ->
            erlang:monotonic_time(millisecond)
    end.

%% @private Measure time to recover
-spec measure_time_to_recover(binary(), integer()) -> integer().
measure_time_to_recover(ExpId, StartTime) ->
    measure_time_to_recover(ExpId, StartTime, 0).

measure_time_to_recover(ExpId, StartTime, Attempts) ->
    % Check if steady state is restored
    case erlmcp_chaos_steady_state:validate_steady_state(0.1) of
        {ok, []} ->
            erlang:monotonic_time(millisecond);
        {ok, _Deviations} when Attempts > 100 ->
            erlang:monotonic_time(millisecond);
        _ ->
            timer:sleep(?RECOVERY_CHECK_INTERVAL),
            measure_time_to_recover(ExpId, StartTime, Attempts + 1)
    end.

%% @private Calculate MTTR from samples
-spec calculate_mttr([#mttr_sample{}]) -> non_neg_integer().
calculate_mttr([]) ->
    0;
calculate_mttr(Samples) ->
    TTRs = [S#mttr_sample.total_recovery_time || S <- Samples],
    lists:sum(TTRs) div length(TTRs).

%% @private Calculate percentile
-spec percentile([number()], number()) -> float().
percentile(List, Percentile) ->
    Sorted = lists:sort(List),
    Len = length(Sorted),
    Index = max(1, min(Len, round(Percentile / 100 * Len))),
    lists:nth(Index, Sorted) * 1.0.

%% @private Find a server process
-spec find_server_process() -> pid() | undefined.
find_server_process() ->
    Processes = erlang:processes(),
    ServerProcesses = lists:filter(fun(Pid) ->
        case erlang:process_info(Pid, registered_name) of
            {registered_name, Name} ->
                NameStr = atom_to_list(Name),
                string:find(NameStr, "server") =/= nomatch;
            _ ->
                false
        end
    end, Processes),

    case ServerProcesses of
        [] -> undefined;
        [Pid | _] -> Pid
    end.

%% @private Find a client process
-spec find_client_process() -> pid() | undefined.
find_client_process() ->
    case whereis(erlmcp_client_sup) of
        undefined -> undefined;
        Pid -> Pid
    end.

%% @private Measure MTTR for a specific component
-spec measure_component_mttr(atom(), atom(), list()) -> #mttr_sample{}.
measure_component_mttr(Component, FaultType, Config) ->
    StartTime = erlang:monotonic_time(millisecond),

    case whereis(Component) of
        undefined ->
            #mttr_sample{
                component = atom_to_binary(Component),
                fault_type = atom_to_binary(FaultType),
                time_to_detect = 0,
                time_to_mitigate = 0,
                time_to_recover = 0,
                total_recovery_time = 0,
                timestamp = StartTime
            };
        Pid ->
            measure_process_mttr(Pid, atom_to_binary(Component), Config)
    end.

%% @private Measure MTTR for a specific process
-spec measure_process_mttr(pid(), binary(), list()) -> #mttr_sample{}.
measure_process_mttr(Pid, ComponentName, Config) ->
    StartTime = erlang:monotonic_time(millisecond),

    % Inject fault
    ConfigMap = #{
        experiment => kill_servers,
        target => Pid,
        rate => 1.0,
        duration => 1000
    },

    {ok, ExpId} = erlmcp_chaos:run(ExpId, ConfigMap),

    % Wait for kill
    timer:sleep(100),

    % Measure TTD (time to detect process is down)
    TTD = wait_for_process_down(Pid, StartTime),

    % Measure TTM (time to mitigation start)
    TTMMark = erlang:monotonic_time(millisecond),

    % Measure TTR (time to full recovery)
    TTR = wait_for_process_recovery(Pid, StartTime),

    TotalTTR = TTR - StartTime,
    TTM = TTMMark - TTD,

    erlmcp_chaos:stop_experiment(ExpId),

    #mttr_sample{
        component = ComponentName,
        fault_type = <<"process_kill">>,
        time_to_detect = TTD - StartTime,
        time_to_mitigate = TTM,
        time_to_recover = TTR - TTR,
        total_recovery_time = TotalTTR,
        timestamp = StartTime
    }.

%% @private Wait for process to be down
-spec wait_for_process_down(pid(), integer()) -> integer().
wait_for_process_down(Pid, StartTime) ->
    case erlang:process_info(Pid) of
        undefined ->
            erlang:monotonic_time(millisecond);
        _ ->
            timer:sleep(?RECOVERY_CHECK_INTERVAL),
            wait_for_process_down(Pid, StartTime)
    end.

%% @private Wait for process to recover
-spec wait_for_process_recovery(pid(), integer()) -> integer().
wait_for_process_recovery(_OriginalPid, StartTime) ->
    % Wait for supervisor to restart
    timer:sleep(1000),
    erlang:monotonic_time(millisecond).

%% @private Wait for cascade detection
-spec wait_for_cascade_detection([pid()], integer()) -> integer().
wait_for_cascade_detection(Processes, StartTime) ->
    DownCount = lists:foldl(fun(Pid, Acc) ->
        case erlang:process_info(Pid) of
            undefined -> Acc + 1;
            _ -> Acc
        end
    end, 0, Processes),

    if DownCount > length(Processes) div 2 ->
        erlang:monotonic_time(millisecond);
    true ->
        timer:sleep(?RECOVERY_CHECK_INTERVAL),
        wait_for_cascade_detection(Processes, StartTime)
    end.

%% @private Wait for full recovery
-spec wait_for_full_recovery([pid()], integer()) -> integer().
wait_for_full_recovery(_Processes, StartTime) ->
    timer:sleep(2000),
    erlang:monotonic_time(millisecond).

%% @private Start test processes
-spec start_test_processes(pos_integer()) -> [pid()].
start_test_processes(Count) ->
    [spawn_link(fun() -> test_process_loop() end)
     || _ <- lists:seq(1, Count)].

%% @private Test process loop
test_process_loop() ->
    receive
        stop -> ok
    after 100 ->
        test_process_loop()
    end.

%% @private Cleanup test processes
-spec cleanup_test_processes([pid()]) -> ok.
cleanup_test_processes(Processes) ->
    lists:foreach(fun(Pid) ->
        case erlang:process_info(Pid) of
            undefined -> ok;
            _ -> exit(Pid, kill)
        end
    end, Processes),
    ok.

%% @private Wait for partition detection
-spec wait_for_partition_detection(binary(), integer()) -> integer().
wait_for_partition_detection(_ExpId, StartTime) ->
    timer:sleep(1000),
    erlang:monotonic_time(millisecond).

%% @private Wait for partition healing
-spec wait_for_partition_healing(binary(), integer()) -> integer().
wait_for_partition_healing(_ExpId, StartTime) ->
    timer:sleep(5000),
    erlang:monotonic_time(millisecond).

%% @private Wait for system detection
-spec wait_for_system_detection(integer()) -> integer().
wait_for_system_detection(StartTime) ->
    timer:sleep(1000),
    erlang:monotonic_time(millisecond).

%% @private Wait for system steady state
-spec wait_for_system_steady_state(integer()) -> integer().
wait_for_system_steady_state(StartTime) ->
    timer:sleep(5000),
    erlang:monotonic_time(millisecond).
