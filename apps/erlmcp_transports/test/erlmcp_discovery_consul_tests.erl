%%%-------------------------------------------------------------------
%%% @doc
%%% Consul Service Discovery Tests
%%%
%%% Tests for Consul HTTP API integration.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_discovery_consul_tests).
-author("erlmcp").

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Unit Tests
%%====================================================================

%% @doc Test Consul service discovery with single instance
consul_single_service_test() ->
    ServiceJson = [
        #{
            <<"ServiceID">> => <<"erlmcp-1">>,
            <<"ServiceAddress">> => <<"10.0.0.1">>,
            <<"ServicePort">> => 9000,
            <<"ServiceTags">> => [<<"erlmcp:tcp">>],
            <<"Address">> => <<"10.0.0.1">>
        }
    ],

    {ok, Transports} = erlmcp_transport_discovery:parse_consul_services(ServiceJson),

    ?assertEqual(1, maps:size(Transports)),
    ?assert(maps:is_key(erlmcp_1, Transports)),

    Config = maps:get(erlmcp_1, Transports),
    ?assertEqual(consul, maps:get(discovered_via, Config)),
    ?assertEqual(tcp, maps:get(type, Config)),
    ?assertEqual("10.0.0.1", maps:get(host, Config)),
    ?assertEqual(9000, maps:get(port, Config)).

%% @doc Test Consul service discovery with multiple instances
consul_multiple_services_test() ->
    ServiceJson = [
        #{
            <<"ServiceID">> => <<"erlmcp-1">>,
            <<"ServiceAddress">> => <<"10.0.0.1">>,
            <<"ServicePort">> => 9000,
            <<"ServiceTags">> => [<<"erlmcp:tcp">>],
            <<"Address">> => <<"10.0.0.1">>
        },
        #{
            <<"ServiceID">> => <<"erlmcp-2">>,
            <<"ServiceAddress">> => <<"10.0.0.2">>,
            <<"ServicePort">> => 9001,
            <<"ServiceTags">> => [<<"erlmcp:http">>],
            <<"Address">> => <<"10.0.0.2">>
        }
    ],

    {ok, Transports} = erlmcp_transport_discovery:parse_consul_services(ServiceJson),

    ?assertEqual(2, maps:size(Transports)),
    ?assert(maps:is_key(erlmcp_1, Transports)),
    ?assert(maps:is_key(erlmcp_2, Transports)),

    Config1 = maps:get(erlmcp_1, Transports),
    ?assertEqual(tcp, maps:get(type, Config1)),

    Config2 = maps:get(erlmcp_2, Transports),
    ?assertEqual(http, maps:get(type, Config2)).

%% @doc Test transport type extraction from tags
consul_transport_type_test() ->
    TcpService = #{
        <<"ServiceID">> => <<"tcp-service">>,
        <<"ServiceAddress">> => <<"10.0.0.1">>,
        <<"ServicePort">> => 9000,
        <<"ServiceTags">> => [<<"erlmcp:tcp">>],
        <<"Address">> => <<"10.0.0.1">>
    },

    {ok, tcp_service, TcpConfig} = erlmcp_transport_discovery:parse_consul_service(TcpService),
    ?assertEqual(tcp, maps:get(type, TcpConfig)),

    HttpService = TcpService#{<<"ServiceID">> => <<"http-service">>, <<"ServiceTags">> => [<<"erlmcp:http">>]},
    {ok, http_service, HttpConfig} = erlmcp_transport_discovery:parse_consul_service(HttpService),
    ?assertEqual(http, maps:get(type, HttpConfig)),

    WsService = TcpService#{<<"ServiceID">> => <<"ws-service">>, <<"ServiceTags">> => [<<"erlmcp:ws">>]},
    {ok, ws_service, WsConfig} = erlmcp_transport_discovery:parse_consul_service(WsService),
    ?assertEqual(ws, maps:get(type, WsConfig)),

    DefaultService = TcpService#{<<"ServiceID">> => <<"default-service">>, <<"ServiceTags">> => []},
    {ok, default_service, DefaultConfig} = erlmcp_transport_discovery:parse_consul_service(DefaultService),
    ?assertEqual(tcp, maps:get(type, DefaultConfig)).

%% @doc Test service with empty ServiceAddress
consul_empty_service_address_test() ->
    ServiceJson = [
        #{
            <<"ServiceID">> => <<"erlmcp-node">>,
            <<"ServiceAddress">> => <<"">>,
            <<"ServicePort">> => 9000,
            <<"ServiceTags">> => [<<"erlmcp:tcp">>],
            <<"Address">> => <<"192.168.1.100">>
        }
    ],

    {ok, Transports} = erlmcp_transport_discovery:parse_consul_services(ServiceJson),
    Config = maps:get(erlmcp_node, Transports),
    ?assertEqual("192.168.1.100", maps:get(host, Config)).

%% @doc Test service with missing ServiceAddress
consul_missing_service_address_test() ->
    ServiceJson = [
        #{
            <<"ServiceID">> => <<"erlmcp-node">>,
            <<"ServicePort">> => 9000,
            <<"ServiceTags">> => [<<"erlmcp:tcp">>],
            <<"Address">> => <<"192.168.1.100">>
        }
    ],

    {ok, Transports} = erlmcp_transport_discovery:parse_consul_services(ServiceJson),
    Config = maps:get(erlmcp_node, Transports),
    ?assertEqual("192.168.1.100", maps:get(host, Config)).

%% @doc Test error handling for missing required fields
consul_missing_field_test() ->
    MissingId = #{
        <<"ServicePort">> => 9000,
        <<"ServiceTags">> => []
    },

    ?assertMatch({error, {missing_field, <<"ServiceID">>}},
                 erlmcp_transport_discovery:parse_consul_service(MissingId)),

    MissingPort = #{
        <<"ServiceID">> => <<"test">>,
        <<"ServiceTags">> => []
    },

    ?assertMatch({error, {missing_field, <<"ServicePort">>}},
                 erlmcp_transport_discovery:parse_consul_service(MissingPort)).

%% @doc Test empty service list
consul_empty_services_test() ->
    {ok, Transports} = erlmcp_transport_discovery:parse_consul_services([]),
    ?assertEqual(0, maps:size(Transports)).

%% @doc Test Consul headers without token
consul_headers_no_token_test() ->
    Headers = erlmcp_transport_discovery:build_consul_headers(undefined),
    ?assertEqual(1, length(Headers)),
    ?assertMatch({<<"accept">>, <<"application/json">>}, lists:keyfind(<<"accept">>, 1, Headers)).

%% @doc Test Consul headers with token
consul_headers_with_token_test() ->
    Token = <<"my-secret-token">>,
    Headers = erlmcp_transport_discovery:build_consul_headers(Token),
    ?assertEqual(2, length(Headers)),
    ?assertMatch({<<"x-consul-token">>, Token}, lists:keyfind(<<"x-consul-token">>, 1, Headers)).

%% @doc Test gun application startup
ensure_gun_started_test() ->
    application:stop(gun),
    ?assertEqual(ok, erlmcp_transport_discovery:ensure_gun_started()),
    ?assertEqual(ok, erlmcp_transport_discovery:ensure_gun_started()).
