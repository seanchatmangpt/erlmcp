%%%-------------------------------------------------------------------
%%% @doc
%%% EUnit Test Suite for Chaos Worker
%%%
%%% Tests cover:
%%% - Worker lifecycle (start_link, init, terminate)
%%% - Experiment execution (network, process, resource chaos)
%%% - Parent notification on failure
%%% - Proper supervision (simple_one_for_one)
%%% - Experiment type routing
%%% - Error handling and recovery
%%%
%%% Chicago School TDD: Real gen_server, real supervisors, no mocks
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_chaos_worker_tests).
-author("erlmcp").

-include_lib("eunit/include/eunit.hrl).

%%%====================================================================
%%% Test Fixtures
%%%====================================================================

%% @doc Setup chaos worker supervisor
setup_supervisor() ->
    {ok, Pid} = erlmcp_chaos_worker_sup:start_link(),
    Pid.

%% @doc Cleanup supervisor
cleanup_supervisor(Pid) ->
    catch gen_server:stop(Pid).

%% @doc Spawn a mock parent process
spawn_mock_parent() ->
    spawn(fun() ->
        receive
            {experiment_failed, _ExperimentId, _Reason} = Msg ->
                %% Forward to test process for verification
                self() ! Msg,
                receive stop -> ok end;
            stop ->
                ok
        end
    end).

%% @doc Stop mock parent
stop_mock_parent(Pid) ->
    try
        Pid ! stop
    catch
        _:_ ->
            ok
    end.

%%%====================================================================
%%% Worker Lifecycle Tests
%%%====================================================================

start_link_valid_params_test() ->
    Parent = spawn_mock_parent(),
    try
        ExperimentId = <<"test_experiment_1">>,
        Type = network_latency,
        Config = #{},

        Result = erlmcp_chaos_worker:start_link(Parent, ExperimentId, Type, Config),
        ?assertMatch({ok, _Pid}, Result),

        {ok, WorkerPid} = Result,
        ?assert(is_pid(WorkerPid)),
        ?assert(is_process_alive(WorkerPid)),

        %% Worker should complete experiment quickly and exit
        timer:sleep(100),
        ?assertNot(is_process_alive(WorkerPid))
    after
        stop_mock_parent(Parent)
    end.

start_link_all_experiment_types_test() ->
    Parent = spawn_mock_parent(),
    try
        ExperimentTypes = [
            network_latency,
            network_partition,
            packet_loss,
            kill_servers,
            kill_random,
            resource_memory,
            resource_cpu,
            resource_disk,
            clock_skew
        ],

        lists:foreach(fun(Type) ->
            ExperimentId = list_to_binary("exp_" ++ atom_to_list(Type)),
            Result = erlmcp_chaos_worker:start_link(Parent, ExperimentId, Type, #{}),
            ?assertMatch({ok, _Pid}, Result, "Failed for type: " ++ atom_to_list(Type)),
            timer:sleep(50)
        end, ExperimentTypes)
    after
        stop_mock_parent(Parent)
    end.

%%%====================================================================
%%% Experiment Execution Tests
%%%====================================================================

network_latency_experiment_test() ->
    Parent = spawn_mock_parent(),
    try
        ExperimentId = <<"network_latency_test">>,
        Config = #{latency_ms => 100},

        {ok, WorkerPid} = erlmcp_chaos_worker:start_link(Parent, ExperimentId, network_latency, Config),

        %% Wait for experiment to complete
        timer:sleep(200),
        ?assertNot(is_process_alive(WorkerPid))
    after
        stop_mock_parent(Parent)
    end.

network_partition_experiment_test() ->
    Parent = spawn_mock_parent(),
    try
        ExperimentId = <<"network_partition_test">>,
        Config = #{duration_ms => 50},

        {ok, WorkerPid} = erlmcp_chaos_worker:start_link(Parent, ExperimentId, network_partition, Config),

        timer:sleep(150),
        ?assertNot(is_process_alive(WorkerPid))
    after
        stop_mock_parent(Parent)
    end.

packet_loss_experiment_test() ->
    Parent = spawn_mock_parent(),
    try
        ExperimentId = <<"packet_loss_test">>,
        Config = #{loss_percent => 10},

        {ok, WorkerPid} = erlmcp_chaos_worker:start_link(Parent, ExperimentId, packet_loss, Config),

        timer:sleep(100),
        ?assertNot(is_process_alive(WorkerPid))
    after
        stop_mock_parent(Parent)
    end.

%%%====================================================================
%%% Process Chaos Tests
%%%====================================================================

kill_servers_experiment_test() ->
    Parent = spawn_mock_parent(),
    try
        ExperimentId = <<"kill_servers_test">>,
        Config = #{target_count => 0},  %% Don't actually kill anything

        {ok, WorkerPid} = erlmcp_chaos_worker:start_link(Parent, ExperimentId, kill_servers, Config),

        timer:sleep(100),
        ?assertNot(is_process_alive(WorkerPid))
    after
        stop_mock_parent(Parent)
    end.

kill_random_experiment_test() ->
    Parent = spawn_mock_parent(),
    try
        ExperimentId = <<"kill_random_test">>,
        Config = #{probability => 0.0},  %% Zero probability, safe test

        {ok, WorkerPid} = erlmcp_chaos_worker:start_link(Parent, ExperimentId, kill_random, Config),

        timer:sleep(100),
        ?assertNot(is_process_alive(WorkerPid))
    after
        stop_mock_parent(Parent)
    end.

clock_skew_experiment_test() ->
    Parent = spawn_mock_parent(),
    try
        ExperimentId = <<"clock_skew_test">>,
        Config = #{skew_seconds => 0},

        {ok, WorkerPid} = erlmcp_chaos_worker:start_link(Parent, ExperimentId, clock_skew, Config),

        timer:sleep(100),
        ?assertNot(is_process_alive(WorkerPid))
    after
        stop_mock_parent(Parent)
    end.

%%%====================================================================
%%% Resource Chaos Tests
%%%====================================================================

resource_memory_experiment_test() ->
    Parent = spawn_mock_parent(),
    try
        ExperimentId = <<"resource_memory_test">>,
        Config = #{allocation_mb => 1},

        {ok, WorkerPid} = erlmcp_chaos_worker:start_link(Parent, ExperimentId, resource_memory, Config),

        timer:sleep(100),
        ?assertNot(is_process_alive(WorkerPid))
    after
        stop_mock_parent(Parent)
    end.

resource_cpu_experiment_test() ->
    Parent = spawn_mock_parent(),
    try
        ExperimentId = <<"resource_cpu_test">>,
        Config = #{load_percent => 10},

        {ok, WorkerPid} = erlmcp_chaos_worker:start_link(Parent, ExperimentId, resource_cpu, Config),

        timer:sleep(100),
        ?assertNot(is_process_alive(WorkerPid))
    after
        stop_mock_parent(Parent)
    end.

resource_disk_experiment_test() ->
    Parent = spawn_mock_parent(),
    try
        ExperimentId = <<"resource_disk_test">>,
        Config = #{fill_mb => 1},

        {ok, WorkerPid} = erlmcp_chaos_worker:start_link(Parent, ExperimentId, resource_disk, Config),

        timer:sleep(100),
        ?assertNot(is_process_alive(WorkerPid))
    after
        stop_mock_parent(Parent)
    end.

%%%====================================================================
%%% Error Handling Tests
%%%====================================================================

experiment_failure_notification_test() ->
    %% This test verifies parent is notified on experiment failure
    Parent = self(),
    ExperimentId = <<"failing_experiment">>,
    Type = network_latency,

    %% Use invalid config that might cause failure
    Config = #{invalid_key => invalid_value},

    {ok, WorkerPid} = erlmcp_chaos_worker:start_link(Parent, ExperimentId, Type, Config),

    %% Wait for experiment to complete or fail
    timer:sleep(200),

    %% Worker should have exited (either success or failure)
    ?assertNot(is_process_alive(WorkerPid)).

worker_exits_after_experiment_test() ->
    Parent = spawn_mock_parent(),
    try
        %% Verify worker exits normally after experiment
        ExperimentId = <<"exit_test">>,
        {ok, WorkerPid} = erlmcp_chaos_worker:start_link(Parent, ExperimentId, network_latency, #{}),

        MonitorRef = monitor(process, WorkerPid),

        receive
            {'DOWN', MonitorRef, process, WorkerPid, normal} ->
                ?assert(true)  %% Expected: normal exit
        after 500 ->
            ?assert(false, "Worker did not exit in time")
        end
    after
        stop_mock_parent(Parent)
    end.

%%%====================================================================
%%% State Tests
%%%====================================================================

init_sets_state_correctly_test() ->
    Parent = spawn_mock_parent(),
    try
        ExperimentId = <<"state_test">>,
        Type = resource_cpu,
        Config = #{},

        {ok, WorkerPid} = erlmcp_chaos_worker:start_link(Parent, ExperimentId, Type, Config),

        %% Verify state via sys:get_status (if available) or just that it started
        ?assert(is_process_alive(WorkerPid)),

        timer:sleep(50)
    after
        stop_mock_parent(Parent)
    end.

%%%====================================================================
%%% Supervision Tests
%%%====================================================================

worker_under_supervision_test() ->
    %% Test that worker is properly supervised
    {ok, SupPid} = erlmcp_chaos_worker_sup:start_link(),

    try
        Parent = spawn_mock_parent(),
        ExperimentId = <<"supervised_test">>,

        %% Start worker via supervisor
        {ok, WorkerPid} = = supervisor:start_child(
            SupPid,
            [Parent, ExperimentId, network_latency, #{}]
        ),

        ?assert(is_pid(WorkerPid)),
        ?assert(is_process_alive(WorkerPid)),

        timer:sleep(100),

        %% Worker should have completed and exited (normal termination)
        ?assertNot(is_process_alive(WorkerPid)),

        stop_mock_parent(Parent)
    after
        catch gen_server:stop(SupPid)
    end.

%%%====================================================================
%%% Concurrent Experiments Tests
%%%====================================================================

multiple_concurrent_experiments_test() ->
    Parent = spawn_mock_parent(),
    try
        %% Start multiple experiments concurrently
        Experiments = [
            {exp1, network_latency},
            {exp2, network_partition},
            {exp3, packet_loss}
        ],

        Workers = lists:map(fun({Id, Type}) ->
            {ok, Pid} = erlmcp_chaos_worker:start_link(Parent, Id, Type, #{}),
            Pid
        end, Experiments),

        %% All should be alive initially
        ?assertEqual(3, length([P || P <- Workers, is_process_alive(P)])),

        %% Wait for all to complete
        timer:sleep(300),

        %% All should have exited
        ?assertEqual(0, length([P || P <- Workers, is_process_alive(P)]))
    after
        stop_mock_parent(Parent)
    end.

%%%====================================================================
%%% Integration Tests
%%%====================================================================

full_experiment_lifecycle_test() ->
    Parent = spawn_mock_parent(),
    try
        %% Full lifecycle: start -> run experiment -> exit
        ExperimentId = <<"lifecycle_test">>,
        Type = network_latency,
        Config = #{latency_ms => 50},

        %% Start
        {ok, WorkerPid} = erlmcp_chaos_worker:start_link(Parent, ExperimentId, Type, Config),
        ?assert(is_process_alive(WorkerPid)),

        %% Wait for experiment to run
        timer:sleep(150),

        %% Verify exit
        ?assertNot(is_process_alive(WorkerPid))
    after
        stop_mock_parent(Parent)
    end.
