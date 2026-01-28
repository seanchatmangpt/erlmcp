%%%-------------------------------------------------------------------
%%% @doc
%%% Application Supervisor Test Suite
%%% 
%%% Comprehensive tests for the OTP supervision tree structure focusing on:
%%% - One-for-all restart strategy validation
%%% - Registry failure triggers full restart
%%% - Component initialization order
%%% - Graceful shutdown sequence
%%% - Memory leak detection
%%% - Recovery time metrics
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_sup_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("erlmcp.hrl").

%%--------------------------------------------------------------------
%% CT Callbacks
%%--------------------------------------------------------------------

suite() ->
    [{timetrap, {seconds, 30}}].

all() ->
    [
        % Basic supervisor functionality
        {group, initialization},
        {group, restart_strategy},
        {group, failure_handling},
        {group, shutdown_sequence},
        {group, performance_monitoring},
        {group, integration_tests}
    ].

groups() ->
    [
        {initialization, [parallel], [
            test_supervisor_start_link,
            test_child_specs_order,
            test_registry_startup_first,
            test_component_dependencies
        ]},
        {restart_strategy, [sequence], [
            test_one_for_all_strategy,
            test_registry_failure_triggers_restart,
            test_supervisor_failure_cascading,
            test_restart_intensity_limits
        ]},
        {failure_handling, [parallel], [
            test_registry_crash_recovery,
            test_server_sup_crash_handling,
            test_transport_sup_crash_handling,
            test_component_isolation_failures
        ]},
        {shutdown_sequence, [sequence], [
            test_graceful_shutdown,
            test_shutdown_ordering,
            test_forced_shutdown_timeout,
            test_resource_cleanup
        ]},
        {performance_monitoring, [parallel], [
            test_startup_time_metrics,
            test_recovery_time_measurement,
            test_memory_leak_detection,
            test_concurrent_operations
        ]},
        {integration_tests, [sequence], [
            test_full_tree_startup,
            test_cascading_failure_prevention,
            test_supervisor_tree_stability,
            test_runtime_configuration_changes
        ]}
    ].

%%--------------------------------------------------------------------
%% Test Case Setup/Teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    % Start observer for memory monitoring
    observer:start(),
    application:ensure_all_started(erlmcp),
    [{test_start_time, erlang:system_time(millisecond)} | Config].

end_per_suite(Config) ->
    observer:stop(),
    application:stop(erlmcp),
    Config.

init_per_group(Group, Config) ->
    ct:pal("Starting test group: ~p", [Group]),
    [{group, Group} | Config].

end_per_group(Group, Config) ->
    ct:pal("Completed test group: ~p", [Group]),
    Config.

init_per_testcase(TestCase, Config) ->
    ct:pal("Starting test case: ~p", [TestCase]),
    % Ensure clean slate
    case whereis(erlmcp_sup) of
        undefined -> ok;
        Pid when is_pid(Pid) -> 
            supervisor:terminate_child(erlmcp_sup, erlmcp_sup),
            timer:sleep(100)
    end,
    [{test_case, TestCase}, {start_memory, get_memory_usage()} | Config].

end_per_testcase(TestCase, Config) ->
    StartMemory = proplists:get_value(start_memory, Config, 0),
    EndMemory = get_memory_usage(),
    MemoryDiff = EndMemory - StartMemory,
    ct:pal("Test case ~p completed. Memory diff: ~p bytes", [TestCase, MemoryDiff]),
    % Cleanup any remaining processes
    cleanup_test_processes(),
    Config.

%%--------------------------------------------------------------------
%% Initialization Tests
%%--------------------------------------------------------------------

test_supervisor_start_link(Config) ->
    StartTime = erlang:system_time(microsecond),
    
    % Test normal startup
    {ok, SupervisorPid} = erlmcp_sup:start_link(),
    
    EndTime = erlang:system_time(microsecond),
    StartupTime = EndTime - StartTime,
    
    % Verify supervisor is running
    ?assert(is_pid(SupervisorPid)),
    ?assert(is_process_alive(SupervisorPid)),
    
    % Verify it's registered
    ?assertEqual(SupervisorPid, whereis(erlmcp_sup)),
    
    % Check startup time (should be under 100ms for production readiness)
    ct:pal("Supervisor startup time: ~p microseconds", [StartupTime]),
    ?assert(StartupTime < 100000), % 100ms
    
    % Verify supervisor behavior
    SupFlags = supervisor:count_children(erlmcp_sup),
    ?assertMatch([{specs, 3}, {active, 3}, {supervisors, 2}, {workers, 1}], 
                 lists:sort(SupFlags)),
    
    ok = supervisor:terminate_child(erlmcp_sup, erlmcp_sup).

test_child_specs_order(Config) ->
    {ok, SupervisorPid} = erlmcp_sup:start_link(),
    
    % Get children in start order
    Children = supervisor:which_children(erlmcp_sup),
    ChildIds = [Id || {Id, _Pid, _Type, _Modules} <- Children],
    
    % Verify correct initialization order:
    % 1. Registry (core infrastructure)
    % 2. Server supervisor 
    % 3. Transport supervisor
    ExpectedOrder = [erlmcp_registry, erlmcp_server_sup, erlmcp_transport_sup],
    ?assertEqual(ExpectedOrder, ChildIds),
    
    % Verify all children are running
    lists:foreach(fun({Id, Pid, Type, _Modules}) ->
        ?assert(is_pid(Pid)),
        ?assert(is_process_alive(Pid)),
        case Id of
            erlmcp_registry -> ?assertEqual(worker, Type);
            _ -> ?assertEqual(supervisor, Type)
        end
    end, Children),
    
    ok = supervisor:terminate_child(erlmcp_sup, erlmcp_sup).

test_registry_startup_first(Config) ->
    % Test that registry starts before other components
    meck:new(erlmcp_server_sup, [passthrough]),
    meck:new(erlmcp_transport_sup, [passthrough]),
    
    % Track start order
    Parent = self(),
    meck:expect(erlmcp_registry, start_link, fun() ->
        Parent ! {started, erlmcp_registry, erlang:system_time(microsecond)},
        meck:passthrough([])
    end),
    meck:expect(erlmcp_server_sup, start_link, fun() ->
        Parent ! {started, erlmcp_server_sup, erlang:system_time(microsecond)},
        meck:passthrough([])
    end),
    meck:expect(erlmcp_transport_sup, start_link, fun() ->
        Parent ! {started, erlmcp_transport_sup, erlang:system_time(microsecond)},
        meck:passthrough([])
    end),
    
    {ok, _SupervisorPid} = erlmcp_sup:start_link(),
    
    % Collect start times
    StartTimes = collect_start_times(3, []),
    
    % Verify registry started first
    [{Component1, Time1}, {Component2, Time2}, {Component3, Time3}] = 
        lists:sort(fun({_, T1}, {_, T2}) -> T1 =< T2 end, StartTimes),
    
    ?assertEqual(erlmcp_registry, Component1),
    ?assert(Time2 > Time1),
    ?assert(Time3 > Time2),
    
    meck:unload([erlmcp_registry, erlmcp_server_sup, erlmcp_transport_sup]),
    ok = supervisor:terminate_child(erlmcp_sup, erlmcp_sup).

test_component_dependencies(Config) ->
    {ok, SupervisorPid} = erlmcp_sup:start_link(),
    
    % Verify registry is accessible to other components
    ?assertMatch({ok, _}, erlmcp_registry:list_servers()),
    ?assertMatch({ok, _}, erlmcp_registry:list_transports()),
    
    % Test server supervisor can use registry
    {ok, ServerPid} = erlmcp_sup:start_server(test_server, #{capabilities => []}),
    ?assert(is_pid(ServerPid)),
    
    % Verify server is registered
    ?assertMatch({ok, {ServerPid, _}}, erlmcp_registry:find_server(test_server)),
    
    % Test transport supervisor can use registry
    {ok, TransportPid} = erlmcp_sup:start_transport(test_transport, stdio, #{}),
    ?assert(is_pid(TransportPid)),
    
    % Verify transport is registered
    ?assertMatch({ok, {TransportPid, _}}, erlmcp_registry:find_transport(test_transport)),
    
    % Cleanup
    ok = erlmcp_sup:stop_server(test_server),
    ok = erlmcp_sup:stop_transport(test_transport),
    ok = supervisor:terminate_child(erlmcp_sup, erlmcp_sup).

%%--------------------------------------------------------------------
%% Restart Strategy Tests
%%--------------------------------------------------------------------

test_one_for_all_strategy(Config) ->
    {ok, SupervisorPid} = erlmcp_sup:start_link(),
    
    % Get initial child PIDs
    InitialChildren = supervisor:which_children(erlmcp_sup),
    InitialPids = [Pid || {_Id, Pid, _Type, _Modules} <- InitialChildren, is_pid(Pid)],
    
    % Create some registered servers/transports
    {ok, ServerPid} = erlmcp_sup:start_server(test_server, #{capabilities => []}),
    {ok, TransportPid} = erlmcp_sup:start_transport(test_transport, stdio, #{}),
    
    % Force kill the registry (should trigger one_for_all restart)
    RegistryPid = whereis(erlmcp_registry),
    ?assert(is_pid(RegistryPid)),
    exit(RegistryPid, kill),
    
    % Wait for restart to complete
    timer:sleep(1000),
    
    % Verify supervisor is still alive
    ?assert(is_process_alive(SupervisorPid)),
    
    % Verify all children restarted (new PIDs)
    NewChildren = supervisor:which_children(erlmcp_sup),
    NewPids = [Pid || {_Id, Pid, _Type, _Modules} <- NewChildren, is_pid(Pid)],
    
    ?assertEqual(length(InitialPids), length(NewPids)),
    % All PIDs should be different after restart
    ?assertEqual([], lists:intersect(InitialPids, NewPids)),
    
    % Verify registry is working again
    ?assertMatch({ok, _}, erlmcp_registry:list_servers()),
    
    ok = supervisor:terminate_child(erlmcp_sup, erlmcp_sup).

test_registry_failure_triggers_restart(Config) ->
    {ok, SupervisorPid} = erlmcp_sup:start_link(),
    
    % Monitor all child processes
    Children = supervisor:which_children(erlmcp_sup),
    ChildPids = [Pid || {_Id, Pid, _Type, _Modules} <- Children, is_pid(Pid)],
    
    MonitorRefs = [monitor(process, Pid) || Pid <- ChildPids],
    
    % Kill registry with different reasons
    RegistryPid = whereis(erlmcp_registry),
    exit(RegistryPid, simulated_failure),
    
    % Collect DOWN messages - all children should restart
    collect_down_messages(length(MonitorRefs), []),
    
    % Wait for restart
    timer:sleep(500),
    
    % Verify all components restarted and working
    ?assert(is_process_alive(SupervisorPid)),
    ?assertMatch({ok, _}, erlmcp_registry:list_servers()),
    
    NewChildren = supervisor:which_children(erlmcp_sup),
    ?assertEqual(length(Children), length(NewChildren)),
    
    ok = supervisor:terminate_child(erlmcp_sup, erlmcp_sup).

test_supervisor_failure_cascading(Config) ->
    {ok, SupervisorPid} = erlmcp_sup:start_link(),
    
    % Kill server supervisor - should trigger full restart due to one_for_all
    ServerSupPid = whereis(erlmcp_server_sup),
    exit(ServerSupPid, kill),
    
    timer:sleep(1000),
    
    % Verify main supervisor still alive
    ?assert(is_process_alive(SupervisorPid)),
    
    % Verify all components restarted
    ?assert(is_process_alive(whereis(erlmcp_registry))),
    ?assert(is_process_alive(whereis(erlmcp_server_sup))),
    ?assert(is_process_alive(whereis(erlmcp_transport_sup))),
    
    % Different from initial PIDs (proving restart occurred)
    ?assertNotEqual(ServerSupPid, whereis(erlmcp_server_sup)),
    
    ok = supervisor:terminate_child(erlmcp_sup, erlmcp_sup).

test_restart_intensity_limits(Config) ->
    {ok, SupervisorPid} = erlmcp_sup:start_link(),
    
    % Test rapid successive failures (intensity = 3, period = 60s)
    RegistryPid1 = whereis(erlmcp_registry),
    exit(RegistryPid1, kill),
    timer:sleep(200),
    
    RegistryPid2 = whereis(erlmcp_registry),
    exit(RegistryPid2, kill),
    timer:sleep(200),
    
    RegistryPid3 = whereis(erlmcp_registry),
    exit(RegistryPid3, kill),
    timer:sleep(200),
    
    % 4th failure within period should kill supervisor
    RegistryPid4 = whereis(erlmcp_registry),
    exit(RegistryPid4, kill),
    
    % Wait for supervisor termination
    timer:sleep(1000),
    
    % Supervisor should be dead
    ?assertNot(is_process_alive(SupervisorPid)).

%%--------------------------------------------------------------------
%% Failure Handling Tests
%%--------------------------------------------------------------------

test_registry_crash_recovery(Config) ->
    {ok, _SupervisorPid} = erlmcp_sup:start_link(),
    
    % Create some state in registry
    {ok, ServerPid} = erlmcp_sup:start_server(test_server, #{capabilities => []}),
    {ok, TransportPid} = erlmcp_sup:start_transport(test_transport, stdio, #{}),
    
    % Verify state exists
    ?assertMatch({ok, {ServerPid, _}}, erlmcp_registry:find_server(test_server)),
    ?assertMatch({ok, {TransportPid, _}}, erlmcp_registry:find_transport(test_transport)),
    
    % Crash registry
    RegistryPid = whereis(erlmcp_registry),
    exit(RegistryPid, simulated_crash),
    
    % Wait for restart
    timer:sleep(1000),
    
    % Registry should be clean (state lost due to one_for_all restart)
    ?assertMatch({error, not_found}, erlmcp_registry:find_server(test_server)),
    ?assertMatch({error, not_found}, erlmcp_registry:find_transport(test_transport)),
    
    % But registry should be functional
    ?assertMatch({ok, []}, erlmcp_registry:list_servers()),
    ?assertMatch({ok, []}, erlmcp_registry:list_transports()),
    
    ok = supervisor:terminate_child(erlmcp_sup, erlmcp_sup).

test_server_sup_crash_handling(Config) ->
    {ok, _SupervisorPid} = erlmcp_sup:start_link(),
    
    % Server supervisor crash triggers full restart
    ServerSupPid = whereis(erlmcp_server_sup),
    RegistryPid = whereis(erlmcp_registry),
    TransportSupPid = whereis(erlmcp_transport_sup),
    
    exit(ServerSupPid, kill),
    timer:sleep(1000),
    
    % All should be restarted
    ?assertNotEqual(ServerSupPid, whereis(erlmcp_server_sup)),
    ?assertNotEqual(RegistryPid, whereis(erlmcp_registry)),
    ?assertNotEqual(TransportSupPid, whereis(erlmcp_transport_sup)),
    
    % All should be alive
    ?assert(is_process_alive(whereis(erlmcp_server_sup))),
    ?assert(is_process_alive(whereis(erlmcp_registry))),
    ?assert(is_process_alive(whereis(erlmcp_transport_sup))),
    
    ok = supervisor:terminate_child(erlmcp_sup, erlmcp_sup).

test_transport_sup_crash_handling(Config) ->
    {ok, _SupervisorPid} = erlmcp_sup:start_link(),
    
    % Transport supervisor crash triggers full restart
    TransportSupPid = whereis(erlmcp_transport_sup),
    RegistryPid = whereis(erlmcp_registry),
    ServerSupPid = whereis(erlmcp_server_sup),
    
    exit(TransportSupPid, kill),
    timer:sleep(1000),
    
    % All should be restarted due to one_for_all
    ?assertNotEqual(TransportSupPid, whereis(erlmcp_transport_sup)),
    ?assertNotEqual(RegistryPid, whereis(erlmcp_registry)),
    ?assertNotEqual(ServerSupPid, whereis(erlmcp_server_sup)),
    
    ok = supervisor:terminate_child(erlmcp_sup, erlmcp_sup).

test_component_isolation_failures(Config) ->
    {ok, _SupervisorPid} = erlmcp_sup:start_link(),
    
    % Test that random process failures don't affect supervision tree
    TestPid = spawn(fun() -> timer:sleep(infinity) end),
    exit(TestPid, kill),
    
    timer:sleep(100),
    
    % All supervisors should still be alive
    ?assert(is_process_alive(whereis(erlmcp_sup))),
    ?assert(is_process_alive(whereis(erlmcp_registry))),
    ?assert(is_process_alive(whereis(erlmcp_server_sup))),
    ?assert(is_process_alive(whereis(erlmcp_transport_sup))),
    
    ok = supervisor:terminate_child(erlmcp_sup, erlmcp_sup).

%%--------------------------------------------------------------------
%% Shutdown Sequence Tests
%%--------------------------------------------------------------------

test_graceful_shutdown(Config) ->
    {ok, SupervisorPid} = erlmcp_sup:start_link(),
    
    % Create some active components
    {ok, _ServerPid} = erlmcp_sup:start_server(test_server, #{capabilities => []}),
    {ok, _TransportPid} = erlmcp_sup:start_transport(test_transport, stdio, #{}),
    
    % Monitor children
    Children = supervisor:which_children(erlmcp_sup),
    ChildPids = [Pid || {_Id, Pid, _Type, _Modules} <- Children, is_pid(Pid)],
    MonitorRefs = [monitor(process, Pid) || Pid <- ChildPids],
    
    % Graceful shutdown
    StartTime = erlang:system_time(microsecond),
    ok = supervisor:terminate_child(erlmcp_sup, erlmcp_sup),
    EndTime = erlang:system_time(microsecond),
    
    ShutdownTime = EndTime - StartTime,
    ct:pal("Graceful shutdown time: ~p microseconds", [ShutdownTime]),
    
    % Should complete within reasonable time (5 seconds timeout + overhead)
    ?assert(ShutdownTime < 6000000), % 6 seconds
    
    % Verify all children terminated
    lists:foreach(fun(_Ref) ->
        receive
            {'DOWN', _Ref, process, _Pid, _Reason} -> ok
        after 1000 ->
            ?assert(false, "Child did not terminate within timeout")
        end
    end, MonitorRefs).

test_shutdown_ordering(Config) ->
    {ok, SupervisorPid} = erlmcp_sup:start_link(),
    
    % Track termination order
    Parent = self(),
    meck:new([erlmcp_registry, erlmcp_server_sup, erlmcp_transport_sup], [passthrough]),
    
    meck:expect(erlmcp_registry, terminate, fun(Reason, State) ->
        Parent ! {terminated, erlmcp_registry, erlang:system_time(microsecond)},
        meck:passthrough([Reason, State])
    end),
    
    meck:expect(erlmcp_server_sup, terminate, fun(Reason, State) ->
        Parent ! {terminated, erlmcp_server_sup, erlang:system_time(microsecond)},
        meck:passthrough([Reason, State])
    end),
    
    meck:expect(erlmcp_transport_sup, terminate, fun(Reason, State) ->
        Parent ! {terminated, erlmcp_transport_sup, erlang:system_time(microsecond)},
        meck:passthrough([Reason, State])
    end),
    
    % Shutdown
    ok = supervisor:terminate_child(erlmcp_sup, erlmcp_sup),
    
    % Collect termination times
    TermTimes = collect_termination_times(3, []),
    
    % Verify shutdown order (should be reverse of startup order)
    [{Component1, Time1}, {Component2, Time2}, {Component3, Time3}] = 
        lists:sort(fun({_, T1}, {_, T2}) -> T1 =< T2 end, TermTimes),
    
    % Transport sup should terminate first, then server sup, then registry
    ExpectedOrder = [erlmcp_transport_sup, erlmcp_server_sup, erlmcp_registry],
    ActualOrder = [Component1, Component2, Component3],
    ?assertEqual(ExpectedOrder, ActualOrder),
    
    meck:unload([erlmcp_registry, erlmcp_server_sup, erlmcp_transport_sup]).

test_forced_shutdown_timeout(Config) ->
    {ok, SupervisorPid} = erlmcp_sup:start_link(),
    
    % Mock a component that refuses to shutdown
    meck:new(erlmcp_registry, [passthrough]),
    meck:expect(erlmcp_registry, terminate, fun(_Reason, State) ->
        timer:sleep(10000), % Longer than shutdown timeout
        meck:passthrough([_Reason, State])
    end),
    
    StartTime = erlang:system_time(microsecond),
    ok = supervisor:terminate_child(erlmcp_sup, erlmcp_sup),
    EndTime = erlang:system_time(microsecond),
    
    ShutdownTime = EndTime - StartTime,
    
    % Should force kill after timeout (5000ms + overhead)
    ?assert(ShutdownTime < 7000000), % 7 seconds max
    ?assert(ShutdownTime > 4000000), % At least 4 seconds (near timeout)
    
    meck:unload(erlmcp_registry).

test_resource_cleanup(Config) ->
    InitialMemory = get_memory_usage(),
    
    {ok, SupervisorPid} = erlmcp_sup:start_link(),
    
    % Create many resources
    ServerIds = [list_to_atom("server_" ++ integer_to_list(N)) || N <- lists:seq(1, 10)],
    TransportIds = [list_to_atom("transport_" ++ integer_to_list(N)) || N <- lists:seq(1, 10)],
    
    lists:foreach(fun(ServerId) ->
        {ok, _} = erlmcp_sup:start_server(ServerId, #{capabilities => []})
    end, ServerIds),
    
    lists:foreach(fun(TransportId) ->
        {ok, _} = erlmcp_sup:start_transport(TransportId, stdio, #{})
    end, TransportIds),
    
    MemoryAfterCreate = get_memory_usage(),
    
    % Shutdown everything
    ok = supervisor:terminate_child(erlmcp_sup, erlmcp_sup),
    
    % Force garbage collection
    erlang:garbage_collect(),
    timer:sleep(1000),
    
    FinalMemory = get_memory_usage(),
    
    % Memory should return close to initial levels
    MemoryGrowth = FinalMemory - InitialMemory,
    ct:pal("Memory usage - Initial: ~p, After create: ~p, Final: ~p, Growth: ~p", 
           [InitialMemory, MemoryAfterCreate, FinalMemory, MemoryGrowth]),
    
    % Allow some growth but not excessive (< 10% of created resources memory)
    CreatedMemory = MemoryAfterCreate - InitialMemory,
    ?assert(MemoryGrowth < (CreatedMemory * 0.1)).

%%--------------------------------------------------------------------
%% Performance Monitoring Tests
%%--------------------------------------------------------------------

test_startup_time_metrics(Config) ->
    StartTimes = lists:map(fun(_) ->
        StartTime = erlang:system_time(microsecond),
        {ok, SupervisorPid} = erlmcp_sup:start_link(),
        EndTime = erlang:system_time(microsecond),
        ok = supervisor:terminate_child(erlmcp_sup, erlmcp_sup),
        EndTime - StartTime
    end, lists:seq(1, 5)),
    
    AvgStartTime = lists:sum(StartTimes) / length(StartTimes),
    MaxStartTime = lists:max(StartTimes),
    MinStartTime = lists:min(StartTimes),
    
    ct:pal("Startup times - Avg: ~p μs, Min: ~p μs, Max: ~p μs", 
           [AvgStartTime, MinStartTime, MaxStartTime]),
    
    % Performance requirements
    ?assert(AvgStartTime < 50000), % 50ms average
    ?assert(MaxStartTime < 100000), % 100ms max
    ?assert(MinStartTime > 0).

test_recovery_time_measurement(Config) ->
    {ok, _SupervisorPid} = erlmcp_sup:start_link(),
    
    % Create initial state
    {ok, _ServerPid} = erlmcp_sup:start_server(test_server, #{capabilities => []}),
    
    RecoveryTimes = lists:map(fun(_) ->
        % Crash registry and measure recovery
        RegistryPid = whereis(erlmcp_registry),
        StartTime = erlang:system_time(microsecond),
        exit(RegistryPid, kill),
        
        % Wait for recovery
        wait_for_process_restart(erlmcp_registry, RegistryPid),
        EndTime = erlang:system_time(microsecond),
        
        EndTime - StartTime
    end, lists:seq(1, 3)),
    
    AvgRecoveryTime = lists:sum(RecoveryTimes) / length(RecoveryTimes),
    MaxRecoveryTime = lists:max(RecoveryTimes),
    
    ct:pal("Recovery times - Avg: ~p μs, Max: ~p μs", [AvgRecoveryTime, MaxRecoveryTime]),
    
    % Recovery should be fast
    ?assert(AvgRecoveryTime < 500000), % 500ms average
    ?assert(MaxRecoveryTime < 1000000), % 1s max
    
    ok = supervisor:terminate_child(erlmcp_sup, erlmcp_sup).

test_memory_leak_detection(Config) ->
    InitialMemory = get_memory_usage(),
    
    {ok, _SupervisorPid} = erlmcp_sup:start_link(),
    BaseMemory = get_memory_usage(),
    
    % Perform repeated operations that could leak memory
    lists:foreach(fun(N) ->
        ServerId = list_to_atom("temp_server_" ++ integer_to_list(N)),
        {ok, _} = erlmcp_sup:start_server(ServerId, #{capabilities => []}),
        ok = erlmcp_sup:stop_server(ServerId),
        
        % Force GC every 10 iterations
        case N rem 10 of
            0 -> erlang:garbage_collect();
            _ -> ok
        end
    end, lists:seq(1, 100)),
    
    % Final cleanup and measurement
    erlang:garbage_collect(),
    timer:sleep(1000),
    FinalMemory = get_memory_usage(),
    
    MemoryGrowth = FinalMemory - BaseMemory,
    ct:pal("Memory growth after 100 operations: ~p bytes", [MemoryGrowth]),
    
    % Should not grow significantly (< 1MB)
    ?assert(MemoryGrowth < 1048576),
    
    ok = supervisor:terminate_child(erlmcp_sup, erlmcp_sup).

test_concurrent_operations(Config) ->
    {ok, _SupervisorPid} = erlmcp_sup:start_link(),
    
    % Test concurrent supervisor operations
    Parent = self(),
    NumWorkers = 10,
    
    Workers = lists:map(fun(N) ->
        spawn_link(fun() ->
            try
                ServerId = list_to_atom("concurrent_server_" ++ integer_to_list(N)),
                TransportId = list_to_atom("concurrent_transport_" ++ integer_to_list(N)),
                
                {ok, ServerPid} = erlmcp_sup:start_server(ServerId, #{capabilities => []}),
                {ok, TransportPid} = erlmcp_sup:start_transport(TransportId, stdio, #{}),
                
                % Verify registration
                ?assertMatch({ok, {ServerPid, _}}, erlmcp_registry:find_server(ServerId)),
                ?assertMatch({ok, {TransportPid, _}}, erlmcp_registry:find_transport(TransportId)),
                
                % Cleanup
                ok = erlmcp_sup:stop_server(ServerId),
                ok = erlmcp_sup:stop_transport(TransportId),
                
                Parent ! {success, N}
            catch
                Class:Reason:Stacktrace ->
                    Parent ! {error, N, Class, Reason, Stacktrace}
            end
        end)
    end, lists:seq(1, NumWorkers)),
    
    % Collect results
    Results = lists:map(fun(N) ->
        receive
            {success, N} -> success;
            {error, N, Class, Reason, Stacktrace} -> 
                ct:pal("Worker ~p failed: ~p:~p~n~p", [N, Class, Reason, Stacktrace]),
                error
        after 10000 ->
            timeout
        end
    end, lists:seq(1, NumWorkers)),
    
    % All operations should succeed
    ?assertEqual(NumWorkers, length([R || R <- Results, R =:= success])),
    
    % Wait for workers to finish
    lists:foreach(fun(Worker) ->
        monitor(process, Worker),
        receive
            {'DOWN', _, process, Worker, _} -> ok
        after 5000 -> ok
        end
    end, Workers),
    
    ok = supervisor:terminate_child(erlmcp_sup, erlmcp_sup).

%%--------------------------------------------------------------------
%% Integration Tests
%%--------------------------------------------------------------------

test_full_tree_startup(Config) ->
    StartTime = erlang:system_time(microsecond),
    
    % Full application startup
    {ok, _SupervisorPid} = erlmcp_sup:start_link(),
    
    % Create complete setup
    {ok, ServerPid1} = erlmcp_sup:start_server(server1, #{capabilities => []}),
    {ok, ServerPid2} = erlmcp_sup:start_server(server2, #{capabilities => []}),
    {ok, TransportPid1} = erlmcp_sup:start_transport(transport1, stdio, #{}),
    {ok, TransportPid2} = erlmcp_sup:start_transport(transport2, tcp, #{port => 8080}),
    
    EndTime = erlang:system_time(microsecond),
    FullStartupTime = EndTime - StartTime,
    
    ct:pal("Full tree startup time: ~p microseconds", [FullStartupTime]),
    
    % Verify everything is running
    ?assert(is_process_alive(ServerPid1)),
    ?assert(is_process_alive(ServerPid2)),
    ?assert(is_process_alive(TransportPid1)),
    ?assert(is_process_alive(TransportPid2)),
    
    % Verify registry state
    {ok, Servers} = erlmcp_registry:list_servers(),
    {ok, Transports} = erlmcp_registry:list_transports(),
    
    ?assertEqual(2, length(Servers)),
    ?assertEqual(2, length(Transports)),
    
    % Performance requirement for full startup
    ?assert(FullStartupTime < 200000), % 200ms
    
    ok = supervisor:terminate_child(erlmcp_sup, erlmcp_sup).

test_cascading_failure_prevention(Config) ->
    {ok, _SupervisorPid} = erlmcp_sup:start_link(),
    
    % Create a complex setup
    ServerIds = [server1, server2, server3],
    TransportIds = [transport1, transport2, transport3],
    
    lists:foreach(fun(ServerId) ->
        {ok, _} = erlmcp_sup:start_server(ServerId, #{capabilities => []})
    end, ServerIds),
    
    lists:foreach(fun(TransportId) ->
        {ok, _} = erlmcp_sup:start_transport(TransportId, stdio, #{})
    end, TransportIds),
    
    % Bind transports to servers
    ok = erlmcp_registry:bind_transport_to_server(transport1, server1),
    ok = erlmcp_registry:bind_transport_to_server(transport2, server2),
    
    % Simulate failure of one component
    {ok, {ServerPid, _}} = erlmcp_registry:find_server(server1),
    exit(ServerPid, simulated_failure),
    
    % Wait for restart
    timer:sleep(1000),
    
    % Due to one_for_all strategy, everything should restart but work
    ?assertMatch({ok, _}, erlmcp_registry:list_servers()),
    ?assertMatch({ok, _}, erlmcp_registry:list_transports()),
    
    % Verify functionality after restart
    {ok, _NewServerPid} = erlmcp_sup:start_server(new_server, #{capabilities => []}),
    
    ok = supervisor:terminate_child(erlmcp_sup, erlmcp_sup).

test_supervisor_tree_stability(Config) ->
    {ok, SupervisorPid} = erlmcp_sup:start_link(),
    
    % Run stability test for extended period
    TestDuration = 5000, % 5 seconds
    StartTime = erlang:system_time(millisecond),
    
    % Spawn processes that create/destroy resources
    StressPids = lists:map(fun(N) ->
        spawn_link(fun() -> 
            stress_test_worker(N, StartTime, TestDuration)
        end)
    end, lists:seq(1, 5)),
    
    % Monitor supervisor stability
    SupervisorRef = monitor(process, SupervisorPid),
    
    % Wait for test duration
    timer:sleep(TestDuration + 1000),
    
    % Verify supervisor still alive
    ?assert(is_process_alive(SupervisorPid)),
    
    % Check no DOWN message received
    receive
        {'DOWN', SupervisorRef, process, SupervisorPid, Reason} ->
            ?assert(false, lists:flatten(io_lib:format("Supervisor died: ~p", [Reason])))
    after 0 ->
        ok
    end,
    
    % Cleanup stress test workers
    lists:foreach(fun(Pid) -> exit(Pid, shutdown) end, StressPids),
    
    % Verify components still functional
    ?assertMatch({ok, _}, erlmcp_registry:list_servers()),
    ?assertMatch({ok, _}, erlmcp_registry:list_transports()),
    
    ok = supervisor:terminate_child(erlmcp_sup, erlmcp_sup).

test_runtime_configuration_changes(Config) ->
    {ok, _SupervisorPid} = erlmcp_sup:start_link(),
    
    % Test dynamic configuration changes
    {ok, ServerPid} = erlmcp_sup:start_server(config_server, #{
        capabilities => [],
        config => #{setting1 => value1}
    }),
    
    % Verify initial config
    {ok, {ServerPid, InitialConfig}} = erlmcp_registry:find_server(config_server),
    ?assertMatch(#{config := #{setting1 := value1}}, InitialConfig),
    
    % Simulate configuration update (through restart)
    ok = erlmcp_sup:stop_server(config_server),
    {ok, NewServerPid} = erlmcp_sup:start_server(config_server, #{
        capabilities => [],
        config => #{setting1 => value2, setting2 => value3}
    }),
    
    % Verify updated config
    {ok, {NewServerPid, UpdatedConfig}} = erlmcp_registry:find_server(config_server),
    ?assertMatch(#{config := #{setting1 := value2, setting2 := value3}}, UpdatedConfig),
    ?assertNotEqual(ServerPid, NewServerPid),
    
    ok = supervisor:terminate_child(erlmcp_sup, erlmcp_sup).

%%--------------------------------------------------------------------
%% Helper Functions
%%--------------------------------------------------------------------

get_memory_usage() ->
    erlang:memory(total).

cleanup_test_processes() ->
    % Kill any remaining test processes
    TestProcesses = [P || P <- processes(), 
                          case process_info(P, registered_name) of
                              {registered_name, Name} -> 
                                  lists:prefix("test_", atom_to_list(Name));
                              _ -> false
                          end],
    lists:foreach(fun(P) -> exit(P, kill) end, TestProcesses),
    timer:sleep(100).

collect_start_times(0, Acc) ->
    Acc;
collect_start_times(N, Acc) ->
    receive
        {started, Component, Time} ->
            collect_start_times(N-1, [{Component, Time} | Acc])
    after 5000 ->
        ?assert(false, "Timeout collecting start times")
    end.

collect_termination_times(0, Acc) ->
    Acc;
collect_termination_times(N, Acc) ->
    receive
        {terminated, Component, Time} ->
            collect_termination_times(N-1, [{Component, Time} | Acc])
    after 5000 ->
        ?assert(false, "Timeout collecting termination times")
    end.

collect_down_messages(0, Acc) ->
    Acc;
collect_down_messages(N, Acc) ->
    receive
        {'DOWN', Ref, process, Pid, Reason} ->
            collect_down_messages(N-1, [{Ref, Pid, Reason} | Acc])
    after 5000 ->
        ?assert(false, "Timeout collecting DOWN messages")
    end.

wait_for_process_restart(ProcessName, OldPid) ->
    wait_for_process_restart(ProcessName, OldPid, 50).

wait_for_process_restart(_ProcessName, _OldPid, 0) ->
    ?assert(false, "Process did not restart within timeout");
wait_for_process_restart(ProcessName, OldPid, Retries) ->
    case whereis(ProcessName) of
        undefined ->
            timer:sleep(100),
            wait_for_process_restart(ProcessName, OldPid, Retries - 1);
        NewPid when NewPid =/= OldPid, is_pid(NewPid) ->
            ok;
        OldPid ->
            timer:sleep(100),
            wait_for_process_restart(ProcessName, OldPid, Retries - 1)
    end.

stress_test_worker(WorkerId, StartTime, Duration) ->
    case erlang:system_time(millisecond) - StartTime of
        Elapsed when Elapsed > Duration ->
            ok;
        _ ->
            try
                % Create and destroy resources rapidly
                ServerId = list_to_atom("stress_server_" ++ integer_to_list(WorkerId) 
                                       ++ "_" ++ integer_to_list(rand:uniform(1000))),
                {ok, _ServerPid} = erlmcp_sup:start_server(ServerId, #{capabilities => []}),
                timer:sleep(rand:uniform(100)),
                ok = erlmcp_sup:stop_server(ServerId),
                stress_test_worker(WorkerId, StartTime, Duration)
            catch
                _:_ ->
                    % Ignore errors in stress test
                    timer:sleep(rand:uniform(50)),
                    stress_test_worker(WorkerId, StartTime, Duration)
            end
    end.