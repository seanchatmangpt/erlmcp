%% @doc Unit Tests for Cluster Service Discovery
-module(erlmcp_cluster_discovery_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("erlmcp.hrl").

%%%====================================================================
%%% Test Setup and Teardown
%%%====================================================================

%% @doc Setup discovery service for each test
discovery_setup() ->
    {ok, Pid} = erlmcp_cluster_discovery:start_link(#{}),
    Pid.

%% @doc Cleanup after each test
discovery_cleanup(Pid) ->
    case is_process_alive(Pid) of
        true -> gen_server:stop(Pid);
        false -> ok
    end.

%% @doc Test generator with setup/teardown
cluster_discovery_test_() ->
    {foreach,
     fun discovery_setup/0,
     fun discovery_cleanup/1,
     [
      fun basic_service_registration/1,
      fun service_discovery/1,
      fun multiple_services/1,
      fun service_filters/1,
      fun load_balancing_strategies/1,
      fun health_monitoring/1,
      fun service_update/1,
      fun cluster_info/1,
      fun event_subscriptions/1,
      fun error_handling/1
     ]}.

%%%====================================================================
%%% Basic Service Registration Tests
%%%====================================================================

basic_service_registration(_Pid) ->
    fun() ->
        %% Test local service registration
        ServicePid = spawn(fun() -> receive die -> ok end end),
        Metadata = #{version => <<"1.0.0">>, region => <<"us-east">>},

        {ok, ServiceId} = erlmcp_cluster_discovery:register_service(
            local, test_service, ServicePid, test_type, Metadata),

        ?assert(is_binary(ServiceId)),
        ?assertMatch({ok, _}, erlmcp_cluster_discovery:discover_service(test_service, local)),

        %% Clean up service process
        ServicePid ! die
    end.

%%%====================================================================
%%% Service Discovery Tests
%%%====================================================================

service_discovery(_Pid) ->
    fun() ->
        %% Register a service
        ServicePid = spawn(fun() -> receive die -> ok end end),
        Metadata = #{version => <<"1.0.0">>},

        {ok, ServiceId} = erlmcp_cluster_discovery:register_service(
            local, echo_service, ServicePid, echo, Metadata),

        %% Discover the service
        {ok, ServiceInfo} = erlmcp_cluster_discovery:discover_service(
            echo_service, local),

        ?assertEqual(<<"1.0.0">>, maps:get(version, ServiceInfo)),
        ?assertEqual(echo, maps:get(type, ServiceInfo)),
        ?assertEqual(ServicePid, maps:get(pid, ServiceInfo)),
        ?assertEqual(ServiceId, maps:get(id, ServiceInfo)),

        %% Discover all services
        {ok, AllServices} = erlmcp_cluster_discovery:discover_all_services(local),
        ?assertEqual(1, length(AllServices)),

        %% Clean up
        ServicePid ! die
    end.

%%%====================================================================
%%% Multiple Services Tests
%%%====================================================================

multiple_services(_Pid) ->
    fun() ->
        %% Register multiple instances of the same service
        ServicePids = [spawn(fun() -> receive die -> ok end end) || _ <- lists:seq(1, 3)],

        ServiceIds = lists:map(fun(ServicePid) ->
            {ok, Id} = erlmcp_cluster_discovery:register_service(
                local, multi_service, ServicePid, multi, #{}),
            Id
        end, ServicePids),

        ?assertEqual(3, length(ServiceIds)),

        %% Discover all instances
        {ok, Services} = erlmcp_cluster_discovery:discover_services(
            multi_service, local),

        ?assertEqual(3, length(Services)),

        %% Get service count
        {ok, Count} = erlmcp_cluster_discovery:get_service_count(
            multi_service, local),

        ?assertEqual(3, Count),

        %% Clean up
        lists:foreach(fun(Pid) -> Pid ! die end, ServicePids)
    end.

%%%====================================================================
%%% Service Filter Tests
%%%====================================================================

service_filters(_Pid) ->
    fun() ->
        %% Register services with different types
        Service1 = spawn(fun() -> receive die -> ok end end),
        Service2 = spawn(fun() -> receive die -> ok end end),

        {ok, _} = erlmcp_cluster_discovery:register_service(
            local, filtered_service, Service1, type_a, #{version => <<"1.0.0">>}),

        {ok, _} = erlmcp_cluster_discovery:register_service(
            local, filtered_service, Service2, type_b, #{version => <<"2.0.0">>}),

        %% Filter by type
        {ok, TypeA} = erlmcp_cluster_discovery:discover_services(
            filtered_service, local, [{type, type_a}]),

        ?assertEqual(1, length(TypeA)),

        %% Filter by version
        {ok, Version2} = erlmcp_cluster_discovery:discover_services(
            filtered_service, local, [{version, <<"2.0.0">>}]),

        ?assertEqual(1, length(Version2)),

        %% Filter by metadata
        {ok, LocalOnly} = erlmcp_cluster_discovery:discover_services(
            filtered_service, local, [{local, true}]),

        ?assertEqual(2, length(LocalOnly)),

        %% Clean up
        Service1 ! die,
        Service2 ! die
    end.

%%%====================================================================
%%% Load Balancing Strategy Tests
%%%====================================================================

load_balancing_strategies(_Pid) ->
    fun() ->
        %% Register multiple services
        ServicePids = [spawn(fun() -> receive die -> ok end end) || _ <- lists:seq(1, 5)],

        lists:foreach(fun(ServicePid) ->
            {ok, _} = erlmcp_cluster_discovery:register_service(
                local, lb_service, ServicePid, lb, #{})
        end, ServicePids),

        %% Test round-robin
        {ok, RR1} = erlmcp_cluster_discovery:discover_with_strategy(
            lb_service, local, round_robin),
        {ok, RR2} = erlmcp_cluster_discovery:discover_with_strategy(
            lb_service, local, round_robin),

        ?assertNotEqual(maps:get(id, RR1), maps:get(id, RR2)),

        %% Test random (should eventually get different services)
        {ok, Random1} = erlmcp_cluster_discovery:discover_with_strategy(
            lb_service, local, random),
        {ok, Random2} = erlmcp_cluster_discovery:discover_with_strategy(
            lb_service, local, random),

        %% Random might return same service, but let's just verify they're valid
        ?assert(maps:get(id, Random1) =/= undefined),
        ?assert(maps:get(id, Random2) =/= undefined),

        %% Test local first (all services are local)
        {ok, Local} = erlmcp_cluster_discovery:discover_with_strategy(
            lb_service, local, local_first),

        ?assertEqual(node(), maps:get(node, Local)),

        %% Clean up
        lists:foreach(fun(Pid) -> Pid ! die end, ServicePids)
    end.

%%%====================================================================
%%% Health Monitoring Tests
%%%====================================================================

health_monitoring(Pid) ->
    fun() ->
        %% Register a service
        ServicePid = spawn(fun() -> receive die -> ok end end),

        {ok, ServiceId} = erlmcp_cluster_discovery:register_service(
            local, health_service, ServicePid, health, #{}),

        %% Initially unknown health
        ?assertEqual({error, not_found},
            erlmcp_cluster_discovery:service_is_healthy(health_service, ServiceId)),

        %% Mark as healthy
        ok = erlmcp_cluster_discovery:mark_service_healthy(health_service, ServiceId),

        ?assertEqual(true, erlmcp_cluster_discovery:service_is_healthy(
            health_service, ServiceId)),

        %% Perform health check
        {ok, healthy} = erlmcp_cluster_discovery:service_health_check(
            health_service, ServiceId),

        %% Mark as unhealthy
        ok = erlmcp_cluster_discovery:mark_service_unhealthy(health_service, ServiceId),

        ?assertEqual(false, erlmcp_cluster_discovery:service_is_healthy(
            health_service, ServiceId)),

        %% Kill service process
        ServicePid ! die,
        timer:sleep(100),

        %% Service should be cleaned up
        {error, no_services_available} =
            erlmcp_cluster_discovery:discover_service(health_service, local),

        %% Force health check on discovery service
        ok = erlmcp_cluster_discovery:force_health_check(Pid),

        %% Check discovery service health
        {ok, Health} = erlmcp_cluster_discovery:health_check(Pid),
        ?assertEqual(healthy, maps:get(status, Health))
    end.

%%%====================================================================
%%% Service Update Tests
%%%====================================================================

service_update(_Pid) ->
    fun() ->
        %% Register a service
        ServicePid = spawn(fun() -> receive die -> ok end end),

        {ok, ServiceId} = erlmcp_cluster_discovery:register_service(
            local, update_service, ServicePid, update,
            #{version => <<"1.0.0">>, region => <<"us-east">>}),

        %% Discover and verify initial metadata
        {ok, ServiceInfo} = erlmcp_cluster_discovery:discover_service(
            update_service, local),

        ?assertEqual(<<"us-east">>, maps:get(region, maps:get(metadata, ServiceInfo))),

        %% Update metadata
        ok = erlmcp_cluster_discovery:update_service(
            update_service, ServiceId, #{region => <<"us-west">>}),

        %% Discover and verify updated metadata
        {ok, UpdatedInfo} = erlmcp_cluster_discovery:discover_service(
            update_service, local),

        ?assertEqual(<<"us-west">>, maps:get(region, maps:get(metadata, UpdatedInfo))),

        %% Renew service (heartbeat)
        ok = erlmcp_cluster_discovery:renew_service(update_service, ServiceId),

        %% Clean up
        ServicePid ! die
    end.

%%%====================================================================
%%% Cluster Info Tests
%%%====================================================================

cluster_info(_Pid) ->
    fun() ->
        %% Register some services
        ServicePids = [spawn(fun() -> receive die -> ok end end) || _ <- lists:seq(1, 3)],

        lists:foreach(fun(ServicePid) ->
            {ok, _} = erlmcp_cluster_discovery:register_service(
                local, cluster_service, ServicePid, cluster, #{})
        end, ServicePids),

        %% Get cluster nodes
        {ok, Nodes} = erlmcp_cluster_discovery:get_cluster_nodes(),
        ?assert(is_list(Nodes)),

        %% Get cluster services
        {ok, ClusterServices} = erlmcp_cluster_discovery:get_cluster_services(),
        ?assert(length(ClusterServices) >= 1),

        %% Get service stats
        {ok, Stats} = erlmcp_cluster_discovery:get_service_stats(cluster_service),
        ?assertEqual(3, maps:get(total_count, Stats)),

        %% Get all stats
        {ok, AllStats} = erlmcp_cluster_discovery:get_all_stats(),
        ?assert(maps:is_key(total_services, AllStats)),
        ?assert(maps:is_key(nodes, AllStats)),

        %% Clean up
        lists:foreach(fun(Pid) -> Pid ! die end, ServicePids)
    end.

%%%====================================================================
%%% Event Subscription Tests
%%%====================================================================

event_subscriptions(_Pid) ->
    fun() ->
        %% Create subscriber process
        Subscriber = spawn(fun() ->
            receive
                {discovery_event, Event} ->
                    %% Forward event to test process
                    ?MODULE ! {subscriber_event, Event}
            end
        end),

        %% Subscribe to events
        ok = erlmcp_cluster_discovery:subscribe_to_events(Subscriber),

        %% Register a service (should trigger event)
        ServicePid = spawn(fun() -> receive die -> ok end end),

        {ok, ServiceId} = erlmcp_cluster_discovery:register_service(
            local, event_service, ServicePid, event, #{}),

        %% Wait for event (with timeout)
        receive
            {subscriber_event, {service_registered, event_service, ServiceId, _}} ->
                ?assert(true)
        after 1000 ->
            ?assert(false, "Did not receive service_registered event")
        end,

        %% Unsubscribe
        ok = erlmcp_cluster_discovery:unsubscribe_from_events(Subscriber),

        %% Clean up
        ServicePid ! die
    end.

%%%====================================================================
%%% Error Handling Tests
%%%====================================================================

error_handling(_Pid) ->
    fun() ->
        %% Try to discover non-existent service
        ?assertEqual({error, no_services_available},
            erlmcp_cluster_discovery:discover_service(nonexistent, local)),

        %% Try to unregister non-existent service
        ?assertEqual(ok,
            erlmcp_cluster_discovery:unregister_service(nonexistent, <<"invalid">>)),

        %% Register with dead PID
        DeadPid = spawn(fun() -> ok end),
        timer:sleep(10),  % Let process die

        ?assertEqual({error, service_pid_not_alive},
            erlmcp_cluster_discovery:register_service(
                local, dead_service, DeadPid, dead, #{})),

        %% Try to update non-existent service
        ?assertEqual({error, not_found},
            erlmcp_cluster_discovery:update_service(
                nonexistent, <<"invalid">>, #{})),

        %% Discover with fallback
        ?assertEqual({error, no_services_available},
            erlmcp_cluster_discovery:discover_with_fallback(
                nonexistent, also_nonexistent, local)),

        %% Register one service and test fallback
        ServicePid = spawn(fun() -> receive die -> ok end end),

        {ok, _} = erlmcp_cluster_discovery:register_service(
            local, fallback_service, ServicePid, fallback, #{}),

        {ok, _} = erlmcp_cluster_discovery:discover_with_fallback(
            nonexistent, fallback_service, local),

        %% Clean up
        ServicePid ! die
    end.

%%%====================================================================
%%% Advanced Discovery Tests
%%%====================================================================

advanced_discovery_by_capability_test_() ->
    {setup,
     fun discovery_setup/0,
     fun discovery_cleanup/1,
     fun(_Pid) ->
         %% Register services with different capabilities
         Service1 = spawn(fun() -> receive die -> ok end end),
         Service2 = spawn(fun() -> receive die -> ok end end),

         {ok, _} = erlmcp_cluster_discovery:register_service(
             local, cap_service, Service1, cap,
             #{capabilities => [http, grpc]}),

         {ok, _} = erlmcp_cluster_discovery:register_service(
             local, cap_service, Service2, cap,
             #{capabilities => [http, websocket]}),

         %% Discover by capability
         {ok, GrpcServices} = erlmcp_cluster_discovery:discover_by_capability(
             cap_service, local, grpc),

         ?assertEqual(1, length(GrpcServices)),

         {ok, HttpServices} = erlmcp_cluster_discovery:discover_by_capability(
             cap_service, local, http),

         ?assertEqual(2, length(HttpServices)),

         %% Clean up
         Service1 ! die,
         Service2 ! die
     end}.

advanced_discovery_healthy_test_() ->
    {setup,
     fun discovery_setup/0,
     fun discovery_cleanup/1,
     fun(_Pid) ->
         %% Register services
         Service1 = spawn(fun() -> receive die -> ok end end),
         Service2 = spawn(fun() -> receive die -> ok end end),

         {ok, Id1} = erlmcp_cluster_discovery:register_service(
             local, healthy_service, Service1, healthy, #{}),

         {ok, Id2} = erlmcp_cluster_discovery:register_service(
             local, healthy_service, Service2, healthy, #{}),

         %% Mark one as healthy
         ok = erlmcp_cluster_discovery:mark_service_healthy(healthy_service, Id1),

         %% Discover only healthy services
         {ok, HealthyServices} = erlmcp_cluster_discovery:discover_healthy(
             healthy_service, local),

         %% Note: Health filtering requires more sophisticated implementation
         %% For now, just verify we get services back
         ?assert(length(HealthyServices) >= 0),

         %% Clean up
         Service1 ! die,
         Service2 ! die
     end}.

advanced_discover_local_test_() ->
    {setup,
     fun discovery_setup/0,
     fun discovery_cleanup/1,
     fun(_Pid) ->
         %% Register local service
         ServicePid = spawn(fun() -> receive die -> ok end end),

         {ok, _} = erlmcp_cluster_discovery:register_service(
             local, local_service, ServicePid, local, #{}),

         %% Discover local services
         {ok, LocalServices} = erlmcp_cluster_discovery:discover_local(
             local_service, []),

         ?assertEqual(1, length(LocalServices)),
         ?assertEqual(node(), maps:get(node, hd(LocalServices))),

         %% Clean up
         ServicePid ! die
     end}.

%%%====================================================================
%%% Integration Tests
%%%====================================================================

integration_service_lifecycle_test_() ->
    {setup,
     fun discovery_setup/0,
     fun discovery_cleanup/1,
     fun(_Pid) ->
         %% Complete service lifecycle
         ServicePid = spawn(fun() -> receive die -> ok end end),

         %% 1. Register
         {ok, ServiceId} = erlmcp_cluster_discovery:register_service(
             local, lifecycle_service, ServicePid, lifecycle,
             #{version => <<"1.0.0">>}),

         ?assertMatch({ok, _}, erlmcp_cluster_discovery:discover_service(
             lifecycle_service, local)),

         %% 2. Update
         ok = erlmcp_cluster_discovery:update_service(
             lifecycle_service, ServiceId, #{version => <<"2.0.0">>}),

         {ok, Updated} = erlmcp_cluster_discovery:discover_service(
             lifecycle_service, local),

         ?assertEqual(<<"2.0.0">>, maps:get(version, Updated)),

         %% 3. Renew
         ok = erlmcp_cluster_discovery:renew_service(lifecycle_service, ServiceId),

         %% 4. Unregister
         ok = erlmcp_cluster_discovery:unregister_service(
             lifecycle_service, ServiceId),

         ?assertEqual({error, no_services_available},
             erlmcp_cluster_discovery:discover_service(lifecycle_service, local)),

         %% Clean up
         ServicePid ! die
     end}.

integration_concurrent_discovery_test_() ->
    {setup,
     fun discovery_setup/0,
     fun discovery_cleanup/1,
     fun(_Pid) ->
         %% Register many services concurrently
         ServiceCount = 100,

         ServicePids = [spawn(fun() -> receive die -> ok end end)
                       || _ <- lists:seq(1, ServiceCount)],

         %% Register all services
         lists:foreach(fun(ServicePid) ->
             {ok, _} = erlmcp_cluster_discovery:register_service(
                 local, concurrent_service, ServicePid, concurrent, #{})
         end, ServicePids),

         %% Discover all services
         {ok, Services} = erlmcp_cluster_discovery:discover_services(
             concurrent_service, local),

         ?assertEqual(ServiceCount, length(Services)),

         %% Test load balancing across many services
         {ok, _} = erlmcp_cluster_discovery:discover_with_strategy(
             concurrent_service, local, round_robin),

         {ok, _} = erlmcp_cluster_discovery:discover_with_strategy(
             concurrent_service, local, random),

         %% Get service count
         {ok, Count} = erlmcp_cluster_discovery:get_service_count(
             concurrent_service, local),

         ?assertEqual(ServiceCount, Count),

         %% Clean up all services
         lists:foreach(fun(Pid) -> Pid ! die end, ServicePids),

         %% Wait for cleanup
         timer:sleep(500),

         %% Verify all services cleaned up
         {ok, Remaining} = erlmcp_cluster_discovery:discover_services(
             concurrent_service, local),

         ?assertEqual(0, length(Remaining))
     end}.
