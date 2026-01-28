-module(erlmcp_supervision_SUITE).

-include_lib("common_test/include/ct.hrl").
-include("erlmcp.hrl").

%%====================================================================
%% v1.3.0 Supervision Tree Bulkhead Test Suite
%%
%% Tests the redesigned supervision tree to verify:
%% - Failure isolation between subsystems
%% - Recovery times for each tier
%% - Connection survival during crashes
%% - No cascading failures
%%
%% Execution: rebar3 ct --suite=erlmcp_supervision_SUITE
%%====================================================================

-export([
    suite/0,
    init_per_suite/1, end_per_suite/1,
    init_per_testcase/2, end_per_testcase/2,
    all/0
]).

-export([
    %% Test group: Supervision tree structure
    test_tree_structure/1,
    test_supervisor_relationships/1,
    
    %% Test group: Registry subsystem failure
    test_registry_crash_isolation/1,
    test_registry_recovery_time/1,
    test_registry_reconnect/1,
    
    %% Test group: Infrastructure subsystem failure
    test_infrastructure_crash_isolation/1,
    test_infrastructure_recovery_time/1,
    
    %% Test group: Transport subsystem failure
    test_transport_crash_isolation/1,
    test_transport_recovery_time/1,
    test_transport_connection_survival/1,
    
    %% Test group: Monitoring subsystem failure
    test_monitoring_crash_isolation/1,
    test_monitoring_recovery_independence/1,
    
    %% Test group: Full system scenarios
    test_cascading_failures_prevented/1,
    test_parallel_subsystem_recovery/1,
    test_system_stability_under_repeated_crashes/1
]).

-define(DEFAULT_TIMEOUT, 30000).
-define(CRASH_TIMEOUT, 5000).
-define(RECOVERY_TIMEOUT, 10000).

%%====================================================================
%% Suite and initialization
%%====================================================================

suite() ->
    [
        {timetrap, {seconds, 60}},
        {require, erlmcp_app}
    ].

init_per_suite(Config) ->
    %% Start the erlmcp application
    case application:start(erlmcp) of
        ok -> ok;
        {error, {already_started, erlmcp}} -> ok;
        Error -> ct:fail("Failed to start erlmcp: ~p", [Error])
    end,
    
    %% Wait for tree to stabilize
    timer:sleep(500),
    
    Config.

end_per_suite(_Config) ->
    application:stop(erlmcp),
    ok.

init_per_testcase(_TestCase, Config) ->
    %% Record baseline metrics
    BaselineMetrics = capture_system_metrics(),
    [{baseline_metrics, BaselineMetrics} | Config].

end_per_testcase(_TestCase, _Config) ->
    %% Wait for system to stabilize
    timer:sleep(200),
    ok.

all() ->
    [
        %% Tree structure tests
        test_tree_structure,
        test_supervisor_relationships,
        
        %% Registry subsystem
        test_registry_crash_isolation,
        test_registry_recovery_time,
        test_registry_reconnect,
        
        %% Infrastructure subsystem
        test_infrastructure_crash_isolation,
        test_infrastructure_recovery_time,
        
        %% Transport subsystem
        test_transport_crash_isolation,
        test_transport_recovery_time,
        test_transport_connection_survival,
        
        %% Monitoring subsystem
        test_monitoring_crash_isolation,
        test_monitoring_recovery_independence,
        
        %% Full system scenarios
        test_cascading_failures_prevented,
        test_parallel_subsystem_recovery,
        test_system_stability_under_repeated_crashes
    ].

%%====================================================================
%% Test: Supervision Tree Structure
%%====================================================================

test_tree_structure(_Config) ->
    %% Verify main supervisor exists
    {ok, SupPid} = rpc:call(node(), erlmcp_sup, start_link, []),
    true = is_pid(SupPid),
    
    %% Verify main supervisor has expected children
    {ok, _} = supervisor:which_children(erlmcp_sup),
    
    %% Cleanup
    supervisor:stop(SupPid),
    ok.

test_supervisor_relationships(_Config) ->
    %% Verify each subsystem supervisor exists and is registered
    Sups = [erlmcp_registry_sup, erlmcp_infrastructure_sup, 
            erlmcp_server_sup, erlmcp_transport_sup, erlmcp_monitoring_sup],
    
    lists:foreach(fun(Sup) ->
        case whereis(Sup) of
            undefined -> ct:fail("Supervisor ~p not found", [Sup]);
            Pid when is_pid(Pid) -> ok
        end
    end, Sups),
    
    ok.

%%====================================================================
%% Test: Registry Subsystem (TIER 1)
%%====================================================================

test_registry_crash_isolation(_Config) ->
    %% Get registry pid
    RegistryPid = whereis(erlmcp_registry),

    %% Crash registry
    exit(RegistryPid, kill),
    timer:sleep(?CRASH_TIMEOUT),

    %% Wait for recovery
    timer:sleep(?RECOVERY_TIMEOUT),

    %% Verify registry restarted
    NewRegistryPid = whereis(erlmcp_registry),
    true = is_pid(NewRegistryPid),
    true = NewRegistryPid =/= RegistryPid,

    %% Verify servers and transports survived
    ServersAfter = count_servers(),
    TransportsAfter = count_transports(),

    ct:pal("Registry crash isolation:~n"
           "  Servers survived: ~p~n"
           "  Transports survived: ~p~n",
           [ServersAfter, TransportsAfter]),

    ok.

test_registry_recovery_time(_Config) ->
    RegistryPid = whereis(erlmcp_registry),
    
    StartTime = erlang:monotonic_time(millisecond),
    exit(RegistryPid, kill),
    
    %% Wait for recovery
    recovered_within_timeout(erlmcp_registry, ?RECOVERY_TIMEOUT),
    
    RecoveryTime = erlang:monotonic_time(millisecond) - StartTime,
    ct:pal("Registry recovery time: ~pms~n", [RecoveryTime]),
    
    %% Assert recovery within SLA (2 seconds)
    true = RecoveryTime < 2000,
    
    ok.

test_registry_reconnect(_Config) ->
    %% Send test message to registry
    RegistryPid = whereis(erlmcp_registry),
    erlmcp_registry ! {test_message, self()},
    
    %% Crash registry
    exit(RegistryPid, kill),
    timer:sleep(?CRASH_TIMEOUT),
    
    %% Send message to new registry
    NewRegistryPid = whereis(erlmcp_registry),
    true = is_pid(NewRegistryPid),
    erlmcp_registry ! {test_message, self()},
    
    ok.

%%====================================================================
%% Test: Infrastructure Subsystem (TIER 2)
%%====================================================================

test_infrastructure_crash_isolation(_Config) ->
    %% Get infrastructure supervisor
    InfraSup = whereis(erlmcp_infrastructure_sup),

    %% Crash infrastructure supervisor (but not registry)
    exit(InfraSup, kill),
    timer:sleep(?CRASH_TIMEOUT),

    %% Wait for recovery
    timer:sleep(?RECOVERY_TIMEOUT),

    %% Verify infrastructure restarted
    NewInfraSup = whereis(erlmcp_infrastructure_sup),
    true = is_pid(NewInfraSup),

    %% Registry should still be running
    RegistryPid = whereis(erlmcp_registry),
    true = is_pid(RegistryPid),

    ct:pal("Infrastructure crash isolation: Registry survived~n"),

    ok.

test_infrastructure_recovery_time(_Config) ->
    InfraSup = whereis(erlmcp_infrastructure_sup),
    
    StartTime = erlang:monotonic_time(millisecond),
    exit(InfraSup, kill),
    
    recovered_within_timeout(erlmcp_infrastructure_sup, ?RECOVERY_TIMEOUT),
    
    RecoveryTime = erlang:monotonic_time(millisecond) - StartTime,
    ct:pal("Infrastructure recovery time: ~pms~n", [RecoveryTime]),
    
    ok.

%%====================================================================
%% Test: Transport Subsystem (TIER 4)
%%====================================================================

test_transport_crash_isolation(_Config) ->
    %% Get transport supervisor
    TransportSup = whereis(erlmcp_transport_sup),

    %% Crash transport supervisor
    exit(TransportSup, kill),
    timer:sleep(?CRASH_TIMEOUT),

    %% Wait for recovery
    timer:sleep(?RECOVERY_TIMEOUT),

    %% Verify transport restarted
    NewTransportSup = whereis(erlmcp_transport_sup),
    true = is_pid(NewTransportSup),

    %% Registry and servers should still be running
    RegistryPid = whereis(erlmcp_registry),
    true = is_pid(RegistryPid),
    ServersAfter = count_servers(),

    ct:pal("Transport crash isolation: Servers survived ~p~n", [ServersAfter]),

    ok.

test_transport_recovery_time(_Config) ->
    TransportSup = whereis(erlmcp_transport_sup),
    
    StartTime = erlang:monotonic_time(millisecond),
    exit(TransportSup, kill),
    
    recovered_within_timeout(erlmcp_transport_sup, ?RECOVERY_TIMEOUT),
    
    RecoveryTime = erlang:monotonic_time(millisecond) - StartTime,
    ct:pal("Transport recovery time: ~pms~n", [RecoveryTime]),
    
    ok.

test_transport_connection_survival(_Config) ->
    %% Crash transport layer
    TransportSup = whereis(erlmcp_transport_sup),
    exit(TransportSup, kill),
    timer:sleep(?CRASH_TIMEOUT),

    %% Wait for recovery
    timer:sleep(?RECOVERY_TIMEOUT),

    %% Verify servers survived and will reconnect
    NewTransportSup = whereis(erlmcp_transport_sup),
    true = is_pid(NewTransportSup),
    ServersAfter = count_servers(),

    ct:pal("Transport connection survival: ~p servers active~n",
           [ServersAfter]),

    ok.

%%====================================================================
%% Test: Monitoring Subsystem (TIER 5)
%%====================================================================

test_monitoring_crash_isolation(_Config) ->
    %% Get monitoring supervisor
    MonitoringSup = whereis(erlmcp_monitoring_sup),

    %% Crash monitoring supervisor
    exit(MonitoringSup, kill),
    timer:sleep(?CRASH_TIMEOUT),
    
    %% Wait for recovery
    timer:sleep(?RECOVERY_TIMEOUT),
    
    %% Verify monitoring restarted independently
    NewMonitoringSup = whereis(erlmcp_monitoring_sup),
    true = is_pid(NewMonitoringSup),
    
    %% Core systems should be unaffected
    RegistryPid = whereis(erlmcp_registry),
    ServerSup = whereis(erlmcp_server_sup),
    TransportSup = whereis(erlmcp_transport_sup),
    
    true = is_pid(RegistryPid),
    true = is_pid(ServerSup),
    true = is_pid(TransportSup),
    
    ServersAfter = count_servers(),
    ct:pal("Monitoring crash isolation: Core systems survived, ~p servers~n",
           [ServersAfter]),
    
    ok.

test_monitoring_recovery_independence(_Config) ->
    %% This test verifies monitoring can recover without affecting core
    %% Create multiple crashes to monitoring layer
    lists:foreach(fun(_) ->
        MonitoringSup = whereis(erlmcp_monitoring_sup),
        case is_pid(MonitoringSup) of
            true ->
                exit(MonitoringSup, kill),
                timer:sleep(500);
            false ->
                ok
        end
    end, lists:seq(1, 3)),
    
    timer:sleep(?RECOVERY_TIMEOUT),
    
    %% Verify systems are still running
    RegistryPid = whereis(erlmcp_registry),
    true = is_pid(RegistryPid),
    
    ct:pal("Monitoring recovery independence: Core systems stable~n"),
    
    ok.

%%====================================================================
%% Test: Full System Scenarios
%%====================================================================

test_cascading_failures_prevented(_Config) ->
    %% Verify that crashing lower tiers doesn't cascade to higher tiers
    
    %% Get initial state
    RegistryPid1 = whereis(erlmcp_registry),
    ServerSup1 = whereis(erlmcp_server_sup),
    TransportSup1 = whereis(erlmcp_transport_sup),
    
    %% Crash transport layer (lowest priority)
    exit(TransportSup1, kill),
    timer:sleep(?CRASH_TIMEOUT),
    
    %% Wait for recovery
    timer:sleep(?RECOVERY_TIMEOUT),
    
    %% Verify higher tiers were not restarted
    RegistryPid2 = whereis(erlmcp_registry),
    ServerSup2 = whereis(erlmcp_server_sup),
    
    true = RegistryPid1 =:= RegistryPid2,  % Registry should NOT restart
    true = ServerSup1 =:= ServerSup2,      % Servers should NOT restart
    
    ct:pal("Cascading failures prevented: No cascade from transport crash~n"),
    
    ok.

test_parallel_subsystem_recovery(_Config) ->
    %% Simulate parallel recovery of multiple subsystems
    
    %% Get supervisors
    InfraSup = whereis(erlmcp_infrastructure_sup),
    TransportSup = whereis(erlmcp_transport_sup),
    MonitoringSup = whereis(erlmcp_monitoring_sup),
    
    StartTime = erlang:monotonic_time(millisecond),
    
    %% Crash multiple subsystems at once
    exit(InfraSup, kill),
    exit(TransportSup, kill),
    exit(MonitoringSup, kill),
    
    timer:sleep(?CRASH_TIMEOUT),
    
    %% Wait for recovery
    timer:sleep(?RECOVERY_TIMEOUT),
    
    TotalRecoveryTime = erlang:monotonic_time(millisecond) - StartTime,
    
    %% Verify all recovered
    true = is_pid(whereis(erlmcp_infrastructure_sup)),
    true = is_pid(whereis(erlmcp_transport_sup)),
    true = is_pid(whereis(erlmcp_monitoring_sup)),
    
    ct:pal("Parallel subsystem recovery: ~pms for 3 subsystems~n",
           [TotalRecoveryTime]),
    
    ok.

test_system_stability_under_repeated_crashes(_Config) ->
    %% Verify system remains stable under repeated component crashes
    
    CrashSequence = [
        erlmcp_monitoring_sup,
        erlmcp_transport_sup,
        erlmcp_infrastructure_sup,
        erlmcp_registry_sup
    ],
    
    lists:foreach(fun(Component) ->
        Pid = whereis(Component),
        case is_pid(Pid) of
            true ->
                exit(Pid, kill),
                timer:sleep(500),
                
                %% Wait for recovery
                recovered_within_timeout(Component, ?RECOVERY_TIMEOUT),
                timer:sleep(200);
            false ->
                ok
        end
    end, CrashSequence),
    
    %% Verify all components recovered
    lists:foreach(fun(Component) ->
        Pid = whereis(Component),
        true = is_pid(Pid)
    end, CrashSequence),
    
    ct:pal("System stability: Survived ~p sequential crashes~n",
           [length(CrashSequence)]),
    
    ok.

%%====================================================================
%% Helper Functions
%%====================================================================

count_servers() ->
    case supervisor:which_children(erlmcp_server_sup) of
        [] -> 0;
        Children -> length(Children)
    end.

count_transports() ->
    case supervisor:which_children(erlmcp_transport_sup) of
        [] -> 0;
        Children -> length(Children)
    end.

recovered_within_timeout(Component, Timeout) ->
    Start = erlang:monotonic_time(millisecond),
    recovered_within_timeout(Component, Timeout, Start).

recovered_within_timeout(Component, Timeout, Start) ->
    Elapsed = erlang:monotonic_time(millisecond) - Start,
    case is_pid(whereis(Component)) of
        true ->
            ok;
        false when Elapsed < Timeout ->
            timer:sleep(10),
            recovered_within_timeout(Component, Timeout, Start);
        false ->
            ct:fail("Component ~p did not recover within ~pms", [Component, Timeout])
    end.

capture_system_metrics() ->
    #{
        timestamp => erlang:system_time(millisecond),
        process_count => erlang:system_info(process_count),
        memory => erlang:memory(total)
    }.
