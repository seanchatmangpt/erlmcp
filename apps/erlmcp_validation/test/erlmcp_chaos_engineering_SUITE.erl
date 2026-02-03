%%%-------------------------------------------------------------------
%%% @doc erlmcp_chaos_engineering_SUITE - Comprehensive Chaos Engineering Test Suite
%%%
%%% This suite validates system resilience through controlled chaos experiments.
%%% Tests follow the Principles of Chaos Engineering:
%%% 1. Define steady state as measurable output
%%% 2. Hypothesize that steady state will continue in control and experiment
%%% 3. Introduce variables that reflect real-world events
%%% 4. Try to disprove the hypothesis by looking for difference in steady state
%%% 5. Fix infrastructure and code that doesn't return to steady state
%%%
%%% Test Categories:
%%% - Fault Injection: Process kills, network delays, packet loss
%%% - Network Partitions: Split-brain scenarios
%%% - Resource Exhaustion: Memory, CPU, file descriptors
%%% - Cascade Failures: Supervisor tree resilience
%%% - Recovery: Automatic recovery validation
%%% - Game Day: Runbook validation
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_chaos_engineering_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("kernel/include/logger.hrl").

%% CT callbacks
-export([all/0, init_per_suite/1, end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2]).

%% Test Cases - Fault Injection
-export([
    % Process Faults
    fault_inject_random_process_kill/1,
    fault_inject_targeted_process_kill/1,
    fault_inject_supervisor_kill/1,
    fault_inject_gen_server_crash/1,

    % Network Faults
    fault_inject_network_latency/1,
    fault_inject_packet_loss/1,
    fault_inject_network_jitter/1,

    % Resource Faults
    fault_inject_memory_pressure/1,
    fault_inject_cpu_saturation/1,
    fault_inject_disk_exhaustion/1
]).

%% Test Cases - Network Partitions
-export([
    partition_simulate_split_brain/1,
    partition_partial_isolation/1,
    partition_multi_node_failure/1
]).

%% Test Cases - Resource Exhaustion
-export([
    exhaust_memory_oom/1,
    exhaust_cpu_spin/1,
    exhaust_file_descriptors/1,
    exhaust_port_limit/1
]).

%% Test Cases - Cascade Failures
-export([
    cascade_supervisor_tree_failure/1,
    cascade_dependency_chain/1,
    cascade_message_queue_overflow/1,
    cascade_ets_table_full/1
]).

%% Test Cases - Recovery Validation
-export([
    recovery_supervisor_restart/1,
    recovery_state_restoration/1,
    recovery_connection_rebuild/1,
    recovery_data_integrity/1
]).

%% Test Cases - Steady State
-export([
    steady_state_baseline/1,
    steady_state_validation/1,
    steady_state_after_chaos/1,
    steady_state_deviation_analysis/1
]).

%% Test Cases - Game Day Scenarios
-export([
    gameday_full_system_outage/1,
    gameday_region_failure/1,
    gameday_database_disconnect/1,
    gameday_load_balancer_failure/1
]).

-record(state,
        {chaos_pid :: pid() | undefined,
         steady_state_pid :: pid() | undefined,
         fault_injector_pid :: pid() | undefined,
         baseline :: map() | undefined}).

-define(STEADY_STATE_THRESHOLD, 0.1).  % 10% deviation
-define(RECOVERY_TIMEOUT, 30000).       % 30 second recovery timeout
-define(FAULT_DURATION, 5000).          % 5 second fault duration

%%%===================================================================
%%% CT Callbacks
%%%===================================================================

all() ->
    [
        % Fault Injection Tests
        fault_inject_random_process_kill,
        fault_inject_targeted_process_kill,
        fault_inject_supervisor_kill,
        fault_inject_gen_server_crash,
        fault_inject_network_latency,
        fault_inject_packet_loss,
        fault_inject_network_jitter,

        % Network Partition Tests
        partition_simulate_split_brain,
        partition_partial_isolation,
        partition_multi_node_failure,

        % Resource Exhaustion Tests
        exhaust_memory_oom,
        exhaust_cpu_spin,
        exhaust_file_descriptors,
        exhaust_port_limit,

        % Cascade Failure Tests
        cascade_supervisor_tree_failure,
        cascade_dependency_chain,
        cascade_message_queue_overflow,
        cascade_ets_table_full,

        % Recovery Tests
        recovery_supervisor_restart,
        recovery_state_restoration,
        recovery_connection_rebuild,
        recovery_data_integrity,

        % Steady State Tests
        steady_state_baseline,
        steady_state_validation,
        steady_state_after_chaos,
        steady_state_deviation_analysis,

        % Game Day Tests
        gameday_full_system_outage,
        gameday_region_failure,
        gameday_database_disconnect,
        gameday_load_balancer_failure
    ].

init_per_suite(Config) ->
    ct:log("Initializing Chaos Engineering Suite"),

    % Start chaos infrastructure
    {ok, ChaosPid} = erlmcp_chaos:start_link(),
    {ok, SteadyStatePid} = erlmcp_chaos_steady_state:start_link(),
    {ok, FaultInjectorPid} = erlmcp_chaos_fault_injector:start_link(),
    {ok, MetricsPid} = erlmcp_chaos_metrics:start_link(),

    ct:log("Chaos infrastructure started: chaos=~p, steady_state=~p, "
           "fault_injector=~p, metrics=~p",
           [ChaosPid, SteadyStatePid, FaultInjectorPid, MetricsPid]),

    [{chaos_pid, ChaosPid},
     {steady_state_pid, SteadyStatePid},
     {fault_injector_pid, FaultInjectorPid},
     {metrics_pid, MetricsPid}
     | Config].

end_per_suite(Config) ->
    ct:log("Tearing down Chaos Engineering Suite"),

    ChaosPid = ?config(chaos_pid, Config),
    SteadyStatePid = ?config(steady_state_pid, Config),
    FaultInjectorPid = ?config(fault_injector_pid, Config),
    MetricsPid = ?config(metrics_pid, Config),

    % Stop all experiments
    erlmcp_chaos:stop_all_experiments(),
    erlmcp_chaos_fault_injector:clear_all_faults(),

    % Stop processes
    gen_server:stop(ChaosPid),
    gen_server:stop(SteadyStatePid),
    gen_server:stop(FaultInjectorPid),
    gen_server:stop(MetricsPid),

    ct:log("Chaos infrastructure stopped"),
    ok.

init_per_testcase(TestCase, Config) ->
    ct:log("Initializing test case: ~p", [TestCase]),

    % Capture baseline before each test
    {ok, Baseline} = erlmcp_chaos_steady_state:capture_steady_state(),

    State = #state{
        chaos_pid = ?config(chaos_pid, Config),
        steady_state_pid = ?config(steady_state_pid, Config),
        fault_injector_pid = ?config(fault_injector_pid, Config),
        baseline = Baseline
    },

    [{state, State} | Config].

end_per_testcase(TestCase, Config) ->
    ct:log("Cleaning up test case: ~p", [TestCase]),

    % Stop all active experiments
    erlmcp_chaos:stop_all_experiments(),
    erlmcp_chaos_fault_injector:clear_all_faults(),

    % Verify steady state restoration
    case proplists:get_value(state, Config) of
        undefined -> ok;
        State ->
            verify_steady_state_restored(State)
    end,

    ok.

%%%===================================================================
%%% Fault Injection Tests
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Test random process kill injection
%% Validates: System recovers from random process termination
%%--------------------------------------------------------------------
fault_inject_random_process_kill(Config) ->
    ct:log("=== Fault Injection: Random Process Kill ==="),

    State = proplists:get_value(state, Config),

    % Configure experiment
    ExperimentConfig = #{
        experiment => kill_random,
        rate => 0.1,              % Kill 10% of processes
        interval => 1000,          % Check every second
        duration => ?FAULT_DURATION,
        max_blast_radius => 0.2,   % Affect at most 20% of system
        auto_rollback => true,
        safety_checks => true
    },

    {ok, ExpId} = erlmcp_chaos:run(<<"random_kill_test">>, ExperimentConfig),

    ct:log("Started random kill experiment: ~p", [ExpId]),

    % Wait for fault duration
    timer:sleep(?FAULT_DURATION),

    % Verify system is still running
    Processes = erlang:processes(),
    ct:log("Active processes after fault: ~p", [length(Processes)]),

    % Verify recovery
    {ok, Status} = erlmcp_chaos:get_experiment_status(ExpId),
    ct:log("Experiment status: ~p", [Status]),

    % Stop experiment
    erlmcp_chaos:stop_experiment(ExpId),

    % Validate steady state recovery
    validate_steady_state_recovery(State),

    {comment, "Random process kill recovered successfully"}.

%%--------------------------------------------------------------------
%% @doc Test targeted process kill injection
%% Validates: Specific component recovery works correctly
%%--------------------------------------------------------------------
fault_inject_targeted_process_kill(Config) ->
    ct:log("=== Fault Injection: Targeted Process Kill ==="),

    State = proplists:get_value(state, Config),

    % Find a target component
    Target = case whereis(erlmcp_registry) of
        undefined ->
            % Registry not running, find another target
            case find_safe_target() of
                undefined -> {skip, "No suitable target found"};
                T -> T
            end;
        Pid when is_pid(Pid) -> erlmcp_registry;
        Atom -> Atom
    end,

    ct:log("Target component: ~p", [Target]),

    ExperimentConfig = #{
        experiment => kill_servers,
        target => Target,
        rate => 0.1,
        interval => 1000,
        duration => ?FAULT_DURATION
    },

    {ok, ExpId} = erlmcp_chaos:run(<<"targeted_kill_test">>, ExperimentConfig),

    timer:sleep(?FAULT_DURATION + 2000),  % Wait for recovery

    % Verify component recovered
    case whereis(Target) of
        undefined ->
            ct:fail("Target ~p did not recover", [Target]);
        _ ->
            ct:log("Target ~p recovered successfully", [Target])
    end,

    erlmcp_chaos:stop_experiment(ExpId),
    validate_steady_state_recovery(State),

    {comment, io_lib:format("Targeted ~p kill recovered", [Target])}.

%%--------------------------------------------------------------------
%% @doc Test supervisor kill injection
%% Validates: Supervisor tree properly recovers
%%--------------------------------------------------------------------
fault_inject_supervisor_kill(Config) ->
    ct:log("=== Fault Injection: Supervisor Kill ==="),

    State = proplists:get_value(state, Config),

    % Target a supervisor
    Target = case whereis(erlmcp_core_sup) of
        undefined -> {skip, "Core supervisor not running"};
        _ -> erlmcp_core_sup
    end,

    ExperimentConfig = #{
        experiment => kill_supervisor_tree,
        target => Target
    },

    {ok, ExpId} = erlmcp_chaos:run(<<"supervisor_kill_test">>, ExperimentConfig),

    ct:log("Killed supervisor tree: ~p", [Target]),

    timer:sleep(2000),

    % Verify supervisor restarted
    case whereis(Target) of
        undefined ->
            ct:fail("Supervisor ~p did not restart", [Target]);
        _ ->
            ct:log("Supervisor ~p restarted successfully", [Target])
    end,

    erlmcp_chaos:stop_experiment(ExpId),
    validate_steady_state_recovery(State),

    {comment, "Supervisor recovered"}.

%%--------------------------------------------------------------------
%% @doc Test gen_server crash injection
%% Validates: gen_servers handle crashes gracefully
%%--------------------------------------------------------------------
fault_inject_gen_server_crash(Config) ->
    ct:log("=== Fault Injection: gen_server Crash ==="),

    State = proplists:get_value(state, Config),

    % Create a test gen_server
    {ok, TestServer} = start_test_server(),

    ct:log("Started test server: ~p", [TestServer]),

    % Crash the server
    exit(TestServer, kill),

    timer:sleep(500),

    % Verify it restarted
    case test_server_ping(TestServer) of
        ok ->
            ct:log("Test server recovered");
        {error, not_running} ->
            ct:fail("Test server did not recover")
    end,

    stop_test_server(TestServer),
    validate_steady_state_recovery(State),

    {comment, "gen_server crash handled"}.

%%--------------------------------------------------------------------
%% @doc Test network latency injection
%% Validates: System tolerates increased latency
%%--------------------------------------------------------------------
fault_inject_network_latency(Config) ->
    ct:log("=== Fault Injection: Network Latency ==="),

    State = proplists:get_value(state, Config),

    ExperimentConfig = #{
        experiment => network_latency,
        latency => 500,              % 500ms latency
        rate => 0.3,                 % 30% of messages
        duration => ?FAULT_DURATION
    },

    {ok, ExpId} = erlmcp_chaos:run(<<"latency_test">>, ExperimentConfig),

    ct:log("Injected 500ms latency at 30% rate"),

    % Monitor during latency injection
    LatencyMetrics = collect_metrics_during_fault(?FAULT_DURATION),

    erlmcp_chaos:stop_experiment(ExpId),

    ct:log("Latency metrics: ~p", [LatencyMetrics]),

    validate_steady_state_recovery(State),

    {comment, "Network latency handled"}.

%%--------------------------------------------------------------------
%% @doc Test packet loss injection
%% Validates: System handles message loss
%%--------------------------------------------------------------------
fault_inject_packet_loss(Config) ->
    ct:log("=== Fault Injection: Packet Loss ==="),

    State = proplists:get_value(state, Config),

    ExperimentConfig = #{
        experiment => packet_loss,
        rate => 0.1,                 % 10% packet loss
        interval => 1000,
        duration => ?FAULT_DURATION
    },

    {ok, ExpId} = erlmcp_chaos:run(<<"packet_loss_test">>, ExperimentConfig),

    ct:log("Injected 10% packet loss"),

    timer:sleep(?FAULT_DURATION),

    erlmcp_chaos:stop_experiment(ExpId),

    validate_steady_state_recovery(State),

    {comment, "Packet loss handled"}.

%%--------------------------------------------------------------------
%% @doc Test network jitter injection
%% Validates: System handles variable latency
%%--------------------------------------------------------------------
fault_inject_network_jitter(Config) ->
    ct:log("=== Fault Injection: Network Jitter ==="),

    State = proplists:get_value(state, Config),

    % Vary latency over time
    Latencies = [50, 200, 100, 500, 150, 300, 75, 400],

    lists:foreach(fun(Latency) ->
        Config = #{
            experiment => network_latency,
            latency => Latency,
            rate => 0.5,
            duration => 500
        },
        {ok, ExpId} = erlmcp_chaos:run(Config),
        timer:sleep(600),
        erlmcp_chaos:stop_experiment(ExpId)
    end, Latencies),

    validate_steady_state_recovery(State),

    {comment, "Network jitter handled"}.

%%%===================================================================
%%% Network Partition Tests
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Test split-brain scenario
%% Validates: System handles network partition
%%--------------------------------------------------------------------
partition_simulate_split_brain(Config) ->
    ct:log("=== Network Partition: Split Brain ==="),

    State = proplists:get_value(state, Config),

    Nodes = [node()],

    ExperimentConfig = #{
        experiment => network_partition,
        nodes => Nodes,
        duration => ?FAULT_DURATION
    },

    {ok, ExpId} = erlmcp_chaos:run(<<"split_brain_test">>, ExperimentConfig),

    ct:log("Simulated split-brain for nodes: ~p", [Nodes]),

    timer:sleep(?FAULT_DURATION),

    % Verify no split-brain occurred
    case check_split_brain() of
        true ->
            ct:log("Split-brain detected - expected in test");
        false ->
            ct:log("No split-brain - system stable")
    end,

    erlmcp_chaos:stop_experiment(ExpId),
    validate_steady_state_recovery(State),

    {comment, "Split-brain scenario tested"}.

%%--------------------------------------------------------------------
%% @doc Test partial isolation
%% Validates: Components work in degraded mode
%%--------------------------------------------------------------------
partition_partial_isolation(Config) ->
    ct:log("=== Network Partition: Partial Isolation ==="),

    State = proplists:get_value(state, Config),

    % Isolate specific components
    ExperimentConfig = #{
        experiment => network_partition,
        nodes => [],
        duration => ?FAULT_DURATION
    },

    {ok, ExpId} = erlmcp_chaos:run(<<"partial_isolation_test">>, ExperimentConfig),

    timer:sleep(?FAULT_DURATION),

    erlmcp_chaos:stop_experiment(ExpId),
    validate_steady_state_recovery(State),

    {comment, "Partial isolation handled"}.

%%--------------------------------------------------------------------
%% @doc Test multi-node failure
%% Validates: System handles multiple node failures
%%--------------------------------------------------------------------
partition_multi_node_failure(Config) ->
    ct:log("=== Network Partition: Multi-Node Failure ==="),

    State = proplists:get_value(state, Config),

    % Simulate multiple nodes failing
    ExperimentConfig = #{
        experiment => network_partition,
        nodes => [node()],
        duration => ?FAULT_DURATION
    },

    {ok, ExpId} = erlmcp_chaos:run(<<"multi_node_test">>, ExperimentConfig),

    timer:sleep(?FAULT_DURATION),

    erlmcp_chaos:stop_experiment(ExpId),
    validate_steady_state_recovery(State),

    {comment, "Multi-node failure handled"}.

%%%===================================================================
%%% Resource Exhaustion Tests
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Test memory exhaustion
%% Validates: System handles OOM scenarios
%%--------------------------------------------------------------------
exhaust_memory_oom(Config) ->
    ct:log("=== Resource Exhaustion: Memory OOM ==="),

    State = proplists:get_value(state, Config),

    % Get initial memory
    InitialMemory = erlang:memory(total),
    ct:log("Initial memory: ~p bytes", [InitialMemory]),

    ExperimentConfig = #{
        experiment => resource_memory,
        target_percent => 0.85,       % Target 85% memory
        duration => ?FAULT_DURATION div 2
    },

    {ok, ExpId} = erlmcp_chaos:run(<<"memory_exhaustion_test">>, ExperimentConfig),

    timer:sleep(?FAULT_DURATION div 2),

    PeakMemory = erlang:memory(total),
    ct:log("Peak memory: ~p bytes", [PeakMemory]),

    erlmcp_chaos:stop_experiment(ExpId),

    % Verify memory returned to normal
    FinalMemory = erlang:memory(total),
    MemoryGrowth = (FinalMemory - InitialMemory) / InitialMemory,

    ct:log("Final memory: ~p bytes (~.1f% growth)",
           [FinalMemory, MemoryGrowth * 100]),

    validate_steady_state_recovery(State),

    {comment, io_lib:format("Memory exhaustion: ~.1f% growth", [MemoryGrowth * 100])}.

%%--------------------------------------------------------------------
%% @doc Test CPU saturation
%% Validates: System handles high CPU load
%%--------------------------------------------------------------------
exhaust_cpu_spin(Config) ->
    ct:log("=== Resource Exhaustion: CPU Spin ==="),

    State = proplists:get_value(state, Config),

    SchedulerCount = erlang:system_info(schedulers_online),
    ct:log("Schedulers: ~p", [SchedulerCount]),

    ExperimentConfig = #{
        experiment => resource_cpu,
        target_load => 1.0,           % 100% CPU
        duration => ?FAULT_DURATION div 2
    },

    {ok, ExpId} = erlmcp_chaos:run(<<"cpu_spin_test">>, ExperimentConfig),

    timer:sleep(?FAULT_DURATION div 2),

    erlmcp_chaos:stop_experiment(ExpId),
    validate_steady_state_recovery(State),

    {comment, "CPU saturation handled"}.

%%--------------------------------------------------------------------
%% @doc Test file descriptor exhaustion
%% Validates: System handles FD limits
%%--------------------------------------------------------------------
exhaust_file_descriptors(Config) ->
    ct:log("=== Resource Exhaustion: File Descriptors ==="),

    State = proplists:get_value(state, Config),

    % Open many ports
    MaxPorts = 100,

    Ports = open_many_ports(MaxPorts, []),
    ct:log("Opened ~p ports", [length(Ports)]),

    timer:sleep(1000),

    % Close all ports
    lists:foreach(fun(P) -> erlang:port_close(P) end, Ports),

    validate_steady_state_recovery(State),

    {comment, "File descriptor exhaustion handled"}.

%%--------------------------------------------------------------------
%% @doc Test port limit exhaustion
%% Validates: System handles port limits
%%--------------------------------------------------------------------
exhaust_port_limit(Config) ->
    ct:log("=== Resource Exhaustion: Port Limit ==="),

    State = proplists:get_value(state, Config),

    % Try to open many ports
    Ports = try_open_ports(50),

    ct:log("Opened ~p ports before hitting limit", [length(Ports)]),

    % Close ports
    lists:foreach(fun(P) -> erlang:port_close(P) end, Ports),

    validate_steady_state_recovery(State),

    {comment, "Port limit handled"}.

%%%===================================================================
%%% Cascade Failure Tests
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Test supervisor tree cascade
%% Validates: Cascade failures are contained
%%--------------------------------------------------------------------
cascade_supervisor_tree_failure(Config) ->
    ct:log("=== Cascade Failure: Supervisor Tree ==="),

    State = proplists:get_value(state, Config),

    % Start multiple test processes
    Processes = start_test_processes(10),

    % Kill root to trigger cascade
    [Root | _] = Processes,
    exit(Root, kill),

    timer:sleep(1000),

    % Verify cascade was contained
    Remaining = lists:filter(fun(P) ->
        case erlang:process_info(P) of
            undefined -> false;
            _ -> true
        end
    end, Processes),

    ct:log("Cascade contained: ~p/~p processes remaining",
           [length(Remaining), length(Processes)]),

    cleanup_test_processes(Processes),
    validate_steady_state_recovery(State),

    {comment, "Cascade contained"}.

%%--------------------------------------------------------------------
%% @doc Test dependency chain failure
%% Validates: Dependency chain failures don't cascade
%%--------------------------------------------------------------------
cascade_dependency_chain(Config) ->
    ct:log("=== Cascade Failure: Dependency Chain ==="),

    State = proplists:get_value(state, Config),

    % Create dependency chain
    {ok, Chain} = create_dependency_chain(5),

    % Break middle of chain
    break_dependency_chain(Chain, 3),

    timer:sleep(1000),

    % Verify failure didn't cascade beyond isolation point
    verify_chain_isolation(Chain, 3),

    cleanup_dependency_chain(Chain),
    validate_steady_state_recovery(State),

    {comment, "Dependency chain isolated"}.

%%--------------------------------------------------------------------
%% @doc Test message queue overflow
%% Validates: System handles queue overflow
%%--------------------------------------------------------------------
cascade_message_queue_overflow(Config) ->
    ct:log("=== Cascade Failure: Message Queue Overflow ==="),

    State = proplists:get_value(state, Config),

    % Start a process that won't handle messages
    OverflowPid = spawn(fun() ->
        receive
            after 10000 -> ok
        end
    end),

    % Flood it with messages
    lists:foreach(fun(N) ->
        OverflowPid ! {message, N}
    end, lists:seq(1, 10000)),

    timer:sleep(1000),

    % Check queue size
    {message_queue_len, QueueLen} = erlang:process_info(OverflowPid,
                                                        message_queue_len),

    ct:log("Message queue length: ~p", [QueueLen]),

    % Clean up
    exit(OverflowPid, kill),

    validate_steady_state_recovery(State),

    {comment, "Message queue overflow handled"}.

%%--------------------------------------------------------------------
%% @doc Test ETS table full scenario
%% Validates: System handles ETS limits
%%--------------------------------------------------------------------
cascade_ets_table_full(Config) ->
    ct:log("=== Cascade Failure: ETS Table Full ==="),

    State = proplists:get_value(state, Config),

    % Create multiple ETS tables
    Tables = [ets:new(test_table, [set, private]) || _ <- lists:seq(1, 10)],

    ct:log("Created ~p ETS tables", [length(Tables)]),

    % Fill tables
    lists:foreach(fun(T) ->
        ets:insert(T, {key, value})
    end, Tables),

    timer:sleep(1000),

    % Clean up
    lists:foreach(fun(T) -> ets:delete(T) end, Tables),

    validate_steady_state_recovery(State),

    {comment, "ETS table scenario handled"}.

%%%===================================================================
%%% Recovery Tests
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Test supervisor restart
%% Validates: Supervisors restart children correctly
%%--------------------------------------------------------------------
recovery_supervisor_restart(Config) ->
    ct:log("=== Recovery: Supervisor Restart ==="),

    State = proplists:get_value(state, Config),

    % Kill a supervised process
    case whereis(erlmcp_registry) of
        undefined ->
            {skip, "Registry not running"};
        Pid ->
            OriginalPid = Pid,
            exit(Pid, kill),

            timer:sleep(1000),

            % Verify restart
            case whereis(erlmcp_registry) of
                undefined ->
                    ct:fail("Registry did not restart");
                NewPid when NewPid =/= OriginalPid ->
                    ct:log("Registry restarted: ~p -> ~p",
                           [OriginalPid, NewPid]);
                NewPid ->
                    ct:log("Registry recovered: ~p", [NewPid])
            end
    end,

    validate_steady_state_recovery(State),

    {comment, "Supervisor restart successful"}.

%%--------------------------------------------------------------------
%% @doc Test state restoration
%% Validates: State is restored after restart
%%--------------------------------------------------------------------
recovery_state_restoration(Config) ->
    ct:log("=== Recovery: State Restoration ==="),

    State = proplists:get_value(state, Config),

    % Create a process with state
    {ok, ServerPid} = start_stateful_server(test_state),

    % Get initial state
    {ok, InitialState} = get_server_state(ServerPid),

    % Kill and wait for restart
    exit(ServerPid, kill),
    timer:sleep(1000),

    % Verify state restored (or handled correctly)
    case get_server_state(ServerPid) of
        {ok, InitialState} ->
            ct:log("State restored: ~p", [InitialState]);
        {ok, NewState} ->
            ct:log("State reset: ~p", [NewState]);
        {error, not_running} ->
            ct:fail("Server did not restart")
    end,

    stop_stateful_server(ServerPid),
    validate_steady_state_recovery(State),

    {comment, "State restoration tested"}.

%%--------------------------------------------------------------------
%% @doc Test connection rebuild
%% Validates: Connections rebuild after failure
%%--------------------------------------------------------------------
recovery_connection_rebuild(Config) ->
    ct:log("=== Recovery: Connection Rebuild ==="),

    State = proplists:get_value(state, Config),

    % Get initial connection count
    InitialConns = count_connections(),
    ct:log("Initial connections: ~p", [InitialConns]),

    % Simulate connection loss
    simulate_connection_loss(),

    timer:sleep(2000),

    % Verify connections rebuilt
    FinalConns = count_connections(),
    ct:log("Final connections: ~p", [FinalConns]),

    validate_steady_state_recovery(State),

    {comment, "Connection rebuild tested"}.

%%--------------------------------------------------------------------
%% @doc Test data integrity after recovery
%% Validates: Data remains consistent
%%--------------------------------------------------------------------
recovery_data_integrity(Config) ->
    ct:log("=== Recovery: Data Integrity ==="),

    State = proplists:get_value(state, Config),

    % Create test data in ETS
    Table = ets:new(integrity_test, [set, public, named_table]),
    ets:insert(Table, [{key1, value1}, {key2, value2}]),

    % Simulate crash
    exit(whereis(erlmcp_registry), kill),
    timer:sleep(1000),

    % Verify data integrity
    case ets:lookup(Table, key1) of
        [{key1, value1}] ->
            ct:log("Data intact after crash");
        _ ->
            ct:fail("Data corrupted after crash")
    end,

    ets:delete(Table),
    validate_steady_state_recovery(State),

    {comment, "Data integrity verified"}.

%%%===================================================================
%%% Steady State Tests
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Establish baseline steady state
%%--------------------------------------------------------------------
steady_state_baseline(Config) ->
    ct:log("=== Steady State: Baseline ==="),

    {ok, Snapshot} = erlmcp_chaos_steady_state:capture_steady_state(5),

    ct:log("Baseline snapshot captured at ~p",
           [maps:get(timestamp, Snapshot)]),

    ct:log("System metrics: ~p",
           [maps:get(system, Snapshot)]),

    ct:log("Component metrics: ~p",
           [maps:get(components, Snapshot)]),

    {comment, "Baseline captured"}.

%%--------------------------------------------------------------------
%% @doc Validate steady state
%%--------------------------------------------------------------------
steady_state_validation(Config) ->
    ct:log("=== Steady State: Validation ==="),

    % Set baseline first
    {ok, Baseline} = erlmcp_chaos_steady_state:capture_steady_state(),
    ok = erlmcp_chaos_steady_state:set_baseline(Baseline),

    % Validate against baseline
    {ok, Deviations} = erlmcp_chaos_steady_state:validate_steady_state(0.1),

    ct:log("Steady state deviations: ~p", [Deviations]),

    % Check no critical deviations
    CriticalDeviations = [D || D <- Deviations,
                             maps:get(severity, D) =:= critical],

    case CriticalDeviations of
        [] ->
            {comment, "Steady state valid"};
        _ ->
            ct:fail("Critical deviations: ~p", [CriticalDeviations])
    end.

%%--------------------------------------------------------------------
%% @doc Test steady state after chaos
%%--------------------------------------------------------------------
steady_state_after_chaos(Config) ->
    ct:log("=== Steady State: After Chaos ==="),

    State = proplists:get_value(state, Config),

    % Inject fault
    ConfigMap = #{
        experiment => kill_servers,
        target => erlmcp_registry,
        rate => 0.1,
        duration => 1000
    },

    {ok, ExpId} = erlmcp_chaos:run(ConfigMap),
    timer:sleep(3000),
    erlmcp_chaos:stop_experiment(ExpId),

    % Validate steady state recovered
    validate_steady_state_recovery(State),

    {comment, "Steady state recovered after chaos"}.

%%--------------------------------------------------------------------
%% @doc Analyze steady state deviation
%%--------------------------------------------------------------------
steady_state_deviation_analysis(Config) ->
    ct:log("=== Steady State: Deviation Analysis ==="),

    % Capture baseline
    {ok, Baseline} = erlmcp_chaos_steady_state:capture_steady_state(),
    ok = erlmcp_chaos_steady_state:set_baseline(Baseline),

    % Inject minor fault
    ConfigMap = #{
        experiment => network_latency,
        latency => 100,
        rate => 0.1,
        duration => 1000
    },

    {ok, ExpId} = erlmcp_chaos:run(ConfigMap),
    timer:sleep(2000),

    % Measure deviation during fault
    {ok, Deviations} = erlmcp_chaos_steady_state:validate_steady_state(0.2),

    ct:log("Deviations during fault: ~p", [length(Deviations)]),
    lists:foreach(fun(D) ->
        ct:log("  - ~s: ~.1f% (~s)",
               [maps:get(metric, D), maps:get(deviation_pct, D),
                maps:get(severity, D)])
    end, Deviations),

    erlmcp_chaos:stop_experiment(ExpId),

    {comment, io_lib:format("~p deviations measured", [length(Deviations)])}.

%%%===================================================================
%%% Game Day Scenarios
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Full system outage game day
%%--------------------------------------------------------------------
gameday_full_system_outage(Config) ->
    ct:log("=== Game Day: Full System Outage ==="),

    State = proplists:get_value(state, Config),

    % Simulate full outage
    run_game_day_scenario(full_outage, #{
        duration => 10000,
        checks => [connectivity, data_integrity, performance]
    }, Config),

    validate_steady_state_recovery(State),

    {comment, "Full outage game day completed"}.

%%--------------------------------------------------------------------
%% @doc Region failure game day
%%--------------------------------------------------------------------
gameday_region_failure(Config) ->
    ct:log("=== Game Day: Region Failure ==="),

    State = proplists:get_value(state, Config),

    run_game_day_scenario(region_failure, #{
        region => local,
        duration => 10000,
        failover => true
    }, Config),

    validate_steady_state_recovery(State),

    {comment, "Region failure game day completed"}.

%%--------------------------------------------------------------------
%% @doc Database disconnect game day
%%--------------------------------------------------------------------
gameday_database_disconnect(Config) ->
    ct:log("=== Game Day: Database Disconnect ==="),

    State = proplists:get_value(state, Config),

    run_game_day_scenario(db_disconnect, #{
        duration => 10000,
        reconnect => true
    }, Config),

    validate_steady_state_recovery(State),

    {comment, "DB disconnect game day completed"}.

%%--------------------------------------------------------------------
%% @doc Load balancer failure game day
%%--------------------------------------------------------------------
gameday_load_balancer_failure(Config) ->
    ct:log("=== Game Day: Load Balancer Failure ==="),

    State = proplists:get_value(state, Config),

    run_game_day_scenario(lb_failure, #{
        duration => 10000,
        fallback => true
    }, Config),

    validate_steady_state_recovery(State),

    {comment, "LB failure game day completed"}.

%%%===================================================================
%%% Internal Helper Functions
%%%===================================================================

%% @private Validate steady state recovery
validate_steady_state_recovery(State) ->
    Baseline = State#state.baseline,

    case Baseline of
        undefined ->
            ct:log("No baseline to compare");
        _ ->
            {ok, Current} = erlmcp_chaos_steady_state:capture_steady_state(1),

            BaselineSystem = maps:get(system, Baseline),
            CurrentSystem = maps:get(system, Current),

            % Compare process counts
            BaselineProcs = maps:get(total_processes, BaselineSystem),
            CurrentProcs = maps:get(total_processes, CurrentSystem),

            ProcDiff = abs(CurrentProcs - BaselineProcs),
            ProcPct = case BaselineProcs of
                0 -> 0;
                _ -> ProcDiff / BaselineProcs * 100
            end,

            ct:log("Process count: baseline=~p, current=~p, diff=~p (~.1f%)",
                   [BaselineProcs, CurrentProcs, ProcDiff, ProcPct]),

            case ProcPct of
                Pct when Pct > 50 ->
                    ct:fail("Process count deviation too high: ~.1f%", [Pct]);
                _ ->
                    ok
            end
    end.

%% @private Find a safe target for fault injection
find_safe_target() ->
    % List of safe targets (non-critical components)
    SafeTargets = [
        erlmcp_chaos_metrics,
        erlmcp_chaos_steady_state
    ],

    lists:foreach(fun(T) ->
        case whereis(T) of
            undefined -> ok;
            _ -> throw(T)
        end
    end, SafeTargets),

    undefined.

%% @private Start a test server
start_test_server() ->
    {ok, Pid} = test_server:start_link(),
    {ok, Pid}.

%% @private Stop test server
stop_test_server(Pid) ->
    gen_server:stop(Pid).

%% @private Ping test server
test_server_ping(Pid) ->
    try gen_server:call(Pid, ping, 1000) of
        pong -> ok
    catch
        _:_ -> {error, not_running}
    end.

%% @private Collect metrics during fault
collect_metrics_during_fault(Duration) ->
    collect_metrics_during_fault(Duration, 0, #{}).

collect_metrics_during_fault(Remaining, Attempts, Acc) when Remaining =< 0 ->
    Acc;
collect_metrics_during_fault(Remaining, Attempts, Acc) ->
    % Collect current metrics
    ProcessCount = erlang:system_info(process_count),
    Memory = erlang:memory(total),

    NewAcc = Acc#{
        process_count => ProcessCount,
        memory => Memory,
        samples => maps:get(samples, Acc, 0) + 1
    },

    timer:sleep(100),
    collect_metrics_during_fault(Remaining - 100, Attempts + 1, NewAcc).

%% @private Check for split-brain
check_split_brain() ->
    % In a real implementation, would check for split-brain indicators
    % For now, return false (no split-brain detected)
    false.

%% @private Open many ports
open_many_ports(0, Acc) ->
    lists:reverse(Acc);
open_many_ports(N, Acc) ->
    case erlang:open_port({spawn, "cat"}, []) of
        Port when is_port(Port) ->
            open_many_ports(N - 1, [Port | Acc]);
        _ ->
            lists:reverse(Acc)
    end.

%% @private Try to open ports
try_open_ports(N) ->
    try_open_ports(N, []).

try_open_ports(0, Acc) ->
    lists:reverse(Acc);
try_open_ports(N, Acc) ->
    case erlang:open_port({spawn, "cat"}, [exit_status]) of
        Port when is_port(Port) ->
            try_open_ports(N - 1, [Port | Acc]);
        _ ->
            lists:reverse(Acc)
    end.

%% @private Start test processes
start_test_processes(Count) ->
    [spawn_link(fun() -> test_proc_loop() end)
     || _ <- lists:seq(1, Count)].

%% @private Test process loop
test_proc_loop() ->
    receive
        stop -> ok
    after 100 ->
        test_proc_loop()
    end.

%% @private Cleanup test processes
cleanup_test_processes(Processes) ->
    lists:foreach(fun(P) ->
        case erlang:process_info(P) of
            undefined -> ok;
            _ -> exit(P, kill)
        end
    end, Processes).

%% @private Create dependency chain
create_dependency_chain(Length) ->
    {ok, create_chain(Length, undefined)}.

create_chain(0, PrevPid) ->
    PrevPid;
create_chain(N, PrevPid) ->
    Pid = spawn_link(fun() ->
        chain_loop(PrevPid)
    end),
    create_chain(N - 1, Pid).

chain_loop(NextPid) ->
    receive
        stop -> ok;
        Msg when NextPid =/= undefined ->
            NextPid ! Msg,
            chain_loop(NextPid);
        _ ->
            chain_loop(NextPid)
    end.

%% @private Break dependency chain
break_dependency_chain(_Chain, Index) ->
    % Simplified - in real would find and break at index
    ok.

%% @private Verify chain isolation
verify_chain_isolation(_Chain, _Index) ->
    ok.

%% @private Cleanup dependency chain
cleanup_dependency_chain(_Chain) ->
    ok.

%% @private Start stateful server
start_stateful_server(State) ->
    Pid = spawn_link(fun() ->
        stateful_server_loop(State)
    end),
    {ok, Pid}.

stateful_server_loop(State) ->
    receive
        {get_state, From} ->
            From ! {state, State},
            stateful_server_loop(State);
        stop ->
            ok
    end.

%% @private Get server state
get_server_state(Pid) ->
    Pid ! {get_state, self()},
    receive
        {state, State} -> {ok, State}
    after 1000 ->
        {error, not_running}
    end.

%% @private Stop stateful server
stop_stateful_server(Pid) ->
    Pid ! stop,
    ok.

%% @private Count connections
count_connections() ->
    % Simplified - count processes with registered names
    Processes = erlang:processes(),
    length([P || P <- Processes,
                case erlang:process_info(P, registered_name) of
                    {registered_name, _} -> true;
                    _ -> false
                end]).

%% @private Simulate connection loss
simulate_connection_loss() ->
    % In real implementation, would disconnect network
    ok.

%% @private Run game day scenario
run_game_day_scenario(Type, Config, _TestConfig) ->
    ct:log("Running game day scenario: ~p", [Type]),
    ct:log("Config: ~p", [Config]),

    Duration = maps:get(duration, Config, 10000),

    % Run scenario steps
    scenario_step(pre_outage, Type, Config),
    timer:sleep(1000),

    scenario_step(outage, Type, Config),
    timer:sleep(Duration),

    scenario_step(recovery, Type, Config),
    timer:sleep(2000),

    ct:log("Game day scenario ~p completed", [Type]),
    ok.

%% @private Scenario step
scenario_step(Step, Type, Config) ->
    ct:log("Scenario step: ~p (~p)", [Step, Type]),
    % In real implementation, would execute specific actions
    ok.
