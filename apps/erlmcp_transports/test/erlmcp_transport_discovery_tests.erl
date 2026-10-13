-module(erlmcp_transport_discovery_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Fixture helpers
%%%===================================================================

%% Each test that needs a running gen_server uses start/stop helpers.
%% We stop by pid (not registered name) so a failed test cannot leave
%% a zombie that breaks the next one.

start_server() ->
    %% Stop any leftover from a previous run, ignoring errors.
    catch gen_server:stop(erlmcp_transport_discovery),
    {ok, Pid} = erlmcp_transport_discovery:start_link(),
    Pid.

stop_server(Pid) ->
    catch gen_server:stop(Pid),
    ok.

%%%===================================================================
%%% Service Discovery Tests
%%%===================================================================

discovery_start_test() ->
    Pid = start_server(),
    ?assert(is_pid(Pid)),
    ?assert(is_process_alive(Pid)),
    stop_server(Pid).

discovery_register_mdns_test() ->
    Pid = start_server(),

    ServiceName = <<"test-service">>,
    ServiceInfo = #{
        name => ServiceName,
        type => <<"_erlmcp._tcp">>,
        port => 8080,
        host => "localhost",
        ttl => 120
    },

    Result = erlmcp_transport_discovery:register_mdns(ServiceName, ServiceInfo),
    ?assertEqual(ok, Result),

    %% Cleanup
    ok = erlmcp_transport_discovery:unregister_mdns(ServiceName),
    stop_server(Pid).

discovery_register_consul_test() ->
    Pid = start_server(),

    ServiceName = <<"consul-service">>,
    ServiceInfo = #{
        name => ServiceName,
        port => 8080,
        host => "localhost"
    },

    %% Consul is not available in dev/test; expect ok or network error.
    Result = erlmcp_transport_discovery:register_consul(ServiceName, ServiceInfo),
    ?assert(Result =:= ok orelse element(1, Result) =:= error),

    %% Best-effort deregister (ignore failure if not registered).
    catch erlmcp_transport_discovery:unregister_consul(ServiceName),
    stop_server(Pid).

discovery_register_k8s_test() ->
    Pid = start_server(),

    ServiceName = <<"k8s-service">>,
    ServiceInfo = #{
        name => ServiceName,
        port => 8080,
        host => "localhost",
        namespace => "default"
    },

    %% K8s API not available in dev/test; accept ok or error.
    Result = erlmcp_transport_discovery:register_k8s(ServiceName, ServiceInfo),
    ?assert(Result =:= ok orelse element(1, Result) =:= error),

    catch erlmcp_transport_discovery:unregister_k8s(ServiceName),
    stop_server(Pid).

discovery_get_endpoints_test() ->
    Pid = start_server(),

    ServiceName = <<"endpoint-service">>,
    ServiceInfo = #{
        name => ServiceName,
        port => 9999,
        host => "example.com"
    },

    ok = erlmcp_transport_discovery:register_service(ServiceName, ServiceInfo),

    {ok, Endpoints} = erlmcp_transport_discovery:get_service_endpoints(ServiceName),
    ?assert(length(Endpoints) >= 1),

    %% Cleanup
    ok = erlmcp_transport_discovery:unregister_service(ServiceName),
    stop_server(Pid).

discovery_stats_test() ->
    Pid = start_server(),

    {ok, Stats} = erlmcp_transport_discovery:get_discovery_stats(),
    ?assert(is_map(Stats)),
    ?assert(maps:is_key(registered_services, Stats)),
    ?assert(maps:is_key(active_watchers, Stats)),

    stop_server(Pid).

%%%===================================================================
%%% mDNS Discovery Tests
%%%===================================================================

mdns_discover_test() ->
    Pid = start_server(),

    ServiceName = <<"discover-mdns">>,
    Result = erlmcp_transport_discovery:discover_services(ServiceName, mdns),
    ?assertMatch({ok, _}, Result),

    stop_server(Pid).

%%%===================================================================
%%% Consul Discovery Tests
%%%===================================================================

consul_discover_test() ->
    Pid = start_server(),

    ServiceName = <<"discover-consul">>,
    Result = erlmcp_transport_discovery:discover_services(ServiceName, consul),
    %% Consul not available in dev; accept ok (empty list) or error.
    ?assert(element(1, Result) =:= ok orelse element(1, Result) =:= error),

    stop_server(Pid).

%%%===================================================================
%%% Kubernetes Discovery Tests
%%%===================================================================

k8s_discover_test() ->
    Pid = start_server(),

    ServiceName = <<"discover-k8s">>,
    Result = erlmcp_transport_discovery:discover_services(ServiceName, k8s),
    %% K8s not available in dev; accept ok (empty list) or error.
    ?assert(element(1, Result) =:= ok orelse element(1, Result) =:= error),

    stop_server(Pid).
