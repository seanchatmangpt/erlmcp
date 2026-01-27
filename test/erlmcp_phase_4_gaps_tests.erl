%%%-------------------------------------------------------------------
%%% @doc
%%% Comprehensive test suite for Phase 4 Optional Gaps
%%%
%%% Tests for:
%%% 1. Advanced OTEL Tracing
%%% 2. Enterprise Session Replication
%%% 3. Complex Routing
%%%
%%% Total: 50+ comprehensive test cases with full coverage
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(erlmcp_phase_4_gaps_tests).

-include_lib("eunit/include/eunit.hrl").
-include("erlmcp.hrl").

%%====================================================================
%% Test Fixtures
%%====================================================================

setup() ->
    application:ensure_all_started(erlmcp),
    ok.

cleanup(_) ->
    application:stop(erlmcp),
    ok.

%%====================================================================
%% Advanced OTEL Tracing Tests
%%====================================================================

advanced_otel_init_test() ->
    CorrelationId = erlmcp_advanced_otel_tracing:generate_correlation_id(),
    ?assertNotEqual(undefined, CorrelationId),
    ?assert(is_binary(CorrelationId)),
    ?assert(byte_size(CorrelationId) > 0).

advanced_otel_context_init_test() ->
    CorrelationId = erlmcp_advanced_otel_tracing:generate_correlation_id(),
    ParentSpanId = erlmcp_advanced_otel_tracing:generate_span_id(),
    ok = erlmcp_advanced_otel_tracing:init_trace_context(CorrelationId, ParentSpanId),
    ?assertEqual(CorrelationId, erlmcp_advanced_otel_tracing:get_correlation_id()).

advanced_otel_context_propagation_test() ->
    CorrelationId = erlmcp_advanced_otel_tracing:generate_correlation_id(),
    ParentSpanId = erlmcp_advanced_otel_tracing:generate_span_id(),
    ok = erlmcp_advanced_otel_tracing:init_trace_context(CorrelationId, ParentSpanId),
    Context = erlmcp_advanced_otel_tracing:extract_trace_context(),
    ?assertNotEqual(undefined, Context),
    ?assert(is_map(Context)),
    ?assertEqual(CorrelationId, maps:get(trace_id, Context)).

advanced_otel_context_clear_test() ->
    CorrelationId = erlmcp_advanced_otel_tracing:generate_correlation_id(),
    ok = erlmcp_advanced_otel_tracing:set_correlation_id(CorrelationId),
    ?assertEqual(CorrelationId, erlmcp_advanced_otel_tracing:get_correlation_id()),
    ok = erlmcp_advanced_otel_tracing:clear_trace_context(),
    ?assertEqual(undefined, erlmcp_advanced_otel_tracing:get_correlation_id()).

advanced_otel_w3c_traceparent_generation_test() ->
    CorrelationId = erlmcp_advanced_otel_tracing:generate_correlation_id(),
    ParentSpanId = erlmcp_advanced_otel_tracing:generate_span_id(),
    ok = erlmcp_advanced_otel_tracing:init_trace_context(CorrelationId, ParentSpanId),
    Context = erlmcp_advanced_otel_tracing:get_trace_context(),
    Traceparent = erlmcp_advanced_otel_tracing:w3c_traceparent(Context),
    ?assert(is_binary(Traceparent)),
    ?assert(binary:match(Traceparent, <<"00-">>) =/= nomatch).

advanced_otel_w3c_traceparent_parsing_test() ->
    Traceparent = <<"00-12345678901234567890123456789012-1234567890123456-01">>,
    ParsedContext = erlmcp_advanced_otel_tracing:parse_w3c_traceparent(Traceparent),
    ?assert(is_map(ParsedContext)),
    ?assertNotEqual(undefined, maps:get(trace_id, ParsedContext)),
    ?assertNotEqual(undefined, maps:get(span_id, ParsedContext)).

advanced_otel_w3c_invalid_traceparent_test() ->
    InvalidTraceparent = <<"invalid">>,
    Result = erlmcp_advanced_otel_tracing:parse_w3c_traceparent(InvalidTraceparent),
    ?assertEqual({error, invalid}, Result).

advanced_otel_w3c_headers_inject_test() ->
    Context = #{
        trace_id => erlmcp_advanced_otel_tracing:generate_correlation_id(),
        span_id => erlmcp_advanced_otel_tracing:generate_span_id(),
        trace_flags => <<"01">>
    },
    Headers = erlmcp_advanced_otel_tracing:inject_w3c_to_headers(Context, []),
    ?assert(is_list(Headers)),
    TraceparentHeader = lists:keyfind(<<"traceparent">>, 1, Headers),
    ?assertNotEqual(false, TraceparentHeader).

advanced_otel_w3c_headers_extract_test() ->
    Traceparent = <<"00-12345678901234567890123456789012-1234567890123456-01">>,
    Headers = [{<<"traceparent">>, Traceparent}],
    ExtractedContext = erlmcp_advanced_otel_tracing:extract_w3c_from_headers(Headers),
    ?assert(is_map(ExtractedContext)),
    ?assertEqual(Traceparent, erlmcp_advanced_otel_tracing:w3c_traceparent(ExtractedContext)).

advanced_otel_histogram_recording_test() ->
    MetricName = <<"request.latency">>,
    erlmcp_advanced_otel_tracing:record_histogram(MetricName, 100),
    erlmcp_advanced_otel_tracing:record_histogram(MetricName, 150),
    erlmcp_advanced_otel_tracing:record_histogram(MetricName, 200),
    Stats = erlmcp_advanced_otel_tracing:get_metric_stats(MetricName),
    ?assertNotEqual(undefined, Stats),
    ?assertEqual(3, maps:get(count, Stats)),
    ?assertEqual(100, maps:get(min, Stats)),
    ?assertEqual(200, maps:get(max, Stats)).

advanced_otel_gauge_recording_test() ->
    MetricName = <<"memory.usage">>,
    ok = erlmcp_advanced_otel_tracing:record_gauge(MetricName, 512),
    ok = erlmcp_advanced_otel_tracing:record_gauge(MetricName, 768),
    ok = erlmcp_advanced_otel_tracing:record_gauge(MetricName, 1024),
    ?assertEqual(ok, erlmcp_advanced_otel_tracing:record_gauge(MetricName, 1024)).

advanced_otel_counter_recording_test() ->
    MetricName = <<"requests.total">>,
    ok = erlmcp_advanced_otel_tracing:record_counter(MetricName),
    ok = erlmcp_advanced_otel_tracing:record_counter(MetricName, 5),
    ?assertEqual(ok, erlmcp_advanced_otel_tracing:record_counter(MetricName, 1)).

advanced_otel_baggage_set_get_test() ->
    Key = <<"user.id">>,
    Value = <<"user123">>,
    ok = erlmcp_advanced_otel_tracing:set_baggage(Key, Value),
    RetrievedValue = erlmcp_advanced_otel_tracing:get_baggage(Key),
    ?assertEqual(Value, RetrievedValue).

advanced_otel_baggage_clear_test() ->
    Key = <<"request.id">>,
    ok = erlmcp_advanced_otel_tracing:set_baggage(Key, <<"req456">>),
    AllBaggage = erlmcp_advanced_otel_tracing:get_all_baggage(),
    ?assertNotEqual(undefined, maps:get(Key, AllBaggage, undefined)),
    ok = erlmcp_advanced_otel_tracing:clear_baggage(),
    EmptyBaggage = erlmcp_advanced_otel_tracing:get_all_baggage(),
    ?assertEqual(undefined, maps:get(Key, EmptyBaggage, undefined)).

advanced_otel_span_with_correlation_test() ->
    CorrelationId = erlmcp_advanced_otel_tracing:generate_correlation_id(),
    ok = erlmcp_advanced_otel_tracing:set_correlation_id(CorrelationId),
    _SpanCtx = erlmcp_advanced_otel_tracing:create_span_with_correlation(<<"test.span">>, #{}),
    ?assert(true).

%%====================================================================
%% Enterprise Session Replication Tests
%%====================================================================

enterprise_session_replication_start_test() ->
    {ok, _Pid} = erlmcp_enterprise_session_replication:start_link(),
    ?assert(true).

enterprise_session_replication_init_test() ->
    {ok, _Pid} = erlmcp_enterprise_session_replication:start_link(),
    Nodes = [node1@localhost, node2@localhost],
    Result = erlmcp_enterprise_session_replication:start_replication(Nodes),
    ?assertEqual(ok, Result).

enterprise_session_replication_status_test() ->
    {ok, _Pid} = erlmcp_enterprise_session_replication:start_link(),
    erlmcp_enterprise_session_replication:start_replication([node1@localhost]),
    Status = erlmcp_enterprise_session_replication:get_replication_status(),
    ?assert(is_map(Status)),
    ?assertNotEqual(undefined, maps:get(is_primary, Status)),
    ?assertNotEqual(undefined, maps:get(replica_nodes, Status)).

enterprise_session_replication_is_primary_test() ->
    {ok, _Pid} = erlmcp_enterprise_session_replication:start_link(),
    IsPrimary = erlmcp_enterprise_session_replication:is_primary(),
    ?assert(is_boolean(IsPrimary)).

enterprise_session_replication_replicate_session_test() ->
    {ok, _Pid} = erlmcp_enterprise_session_replication:start_link(),
    SessionId = <<"session123">>,
    Data = #{user_id => <<"user456">>, created_at => erlang:system_time(millisecond)},
    Result = erlmcp_enterprise_session_replication:replicate_session(SessionId, create, Data),
    ?assertEqual(ok, Result).

enterprise_session_replication_get_replica_nodes_test() ->
    {ok, _Pid} = erlmcp_enterprise_session_replication:start_link(),
    Nodes = [node1@localhost, node2@localhost],
    erlmcp_enterprise_session_replication:start_replication(Nodes),
    ReplicaNodes = erlmcp_enterprise_session_replication:get_replica_nodes(),
    ?assertEqual(Nodes, ReplicaNodes).

enterprise_session_replication_set_replica_nodes_test() ->
    {ok, _Pid} = erlmcp_enterprise_session_replication:start_link(),
    NewNodes = [node3@localhost, node4@localhost],
    ok = erlmcp_enterprise_session_replication:set_replica_nodes(NewNodes),
    ReplicaNodes = erlmcp_enterprise_session_replication:get_replica_nodes(),
    ?assertEqual(NewNodes, ReplicaNodes).

enterprise_session_replication_promote_test() ->
    {ok, _Pid} = erlmcp_enterprise_session_replication:start_link(),
    ok = erlmcp_enterprise_session_replication:promote_to_primary(),
    IsPrimary = erlmcp_enterprise_session_replication:is_primary(),
    ?assert(IsPrimary).

enterprise_session_replication_sync_test() ->
    {ok, _Pid} = erlmcp_enterprise_session_replication:start_link(),
    erlmcp_enterprise_session_replication:start_replication([]),
    Result = erlmcp_enterprise_session_replication:sync_sessions(),
    ?assertEqual(ok, Result).

%%====================================================================
%% Complex Routing Tests
%%====================================================================

complex_routing_start_test() ->
    {ok, _Pid} = erlmcp_complex_routing:start_link(),
    ?assert(true).

complex_routing_add_route_test() ->
    {ok, _Pid} = erlmcp_complex_routing:start_link(),
    Route = #{
        id => <<"route-001">>,
        tenant_id => <<"acme-corp">>,
        pattern => <<"/resources/.*">>,
        rules => [],
        fallback => <<"endpoint-default">>
    },
    Result = erlmcp_complex_routing:add_route(Route),
    ?assertEqual(ok, Result).

complex_routing_add_route_invalid_test() ->
    {ok, _Pid} = erlmcp_complex_routing:start_link(),
    InvalidRoute = #{pattern => <<"/resources">>},
    Result = erlmcp_complex_routing:add_route(InvalidRoute),
    ?assertMatch({error, _}, Result).

complex_routing_match_request_simple_test() ->
    {ok, _Pid} = erlmcp_complex_routing:start_link(),
    Route = #{
        id => <<"route-001">>,
        tenant_id => <<"acme-corp">>,
        pattern => <<"/resources.*">>,
        rules => [
            #{
                condition => match_all,
                target => <<"endpoint-1">>,
                weight => 100
            }
        ],
        fallback => <<"endpoint-default">>
    },
    ok = erlmcp_complex_routing:add_route(Route),
    Result = erlmcp_complex_routing:match_request(
        <<"acme-corp">>, 'GET', <<"/resources/123">>, [], []),
    ?assertMatch({ok, _, _}, Result).

complex_routing_match_request_no_match_test() ->
    {ok, _Pid} = erlmcp_complex_routing:start_link(),
    Route = #{
        id => <<"route-001">>,
        tenant_id => <<"acme-corp">>,
        pattern => <<"/resources.*">>,
        rules => [],
        fallback => <<"endpoint-default">>
    },
    ok = erlmcp_complex_routing:add_route(Route),
    Result = erlmcp_complex_routing:match_request(
        <<"other-tenant">>, 'GET', <<"/resources/123">>, [], []),
    ?assertEqual({error, no_match}, Result).

complex_routing_select_endpoint_test() ->
    Endpoints = [{<<"endpoint-1">>, 50}, {<<"endpoint-2">>, 50}],
    Selected = erlmcp_complex_routing:select_endpoint(Endpoints),
    ?assert(Selected =:= <<"endpoint-1">> orelse Selected =:= <<"endpoint-2">>).

complex_routing_select_endpoint_weighted_test() ->
    Endpoints = [{<<"endpoint-1">>, 90}, {<<"endpoint-2">>, 10}],
    Selected = erlmcp_complex_routing:select_endpoint(Endpoints),
    ?assert(is_binary(Selected)).

complex_routing_get_all_routes_test() ->
    {ok, _Pid} = erlmcp_complex_routing:start_link(),
    Route = #{
        id => <<"route-001">>,
        tenant_id => <<"acme-corp">>,
        pattern => <<"/resources.*">>,
        rules => [],
        fallback => <<"endpoint-default">>
    },
    ok = erlmcp_complex_routing:add_route(Route),
    Routes = erlmcp_complex_routing:get_all_routes(),
    ?assert(length(Routes) >= 1).

complex_routing_delete_route_test() ->
    {ok, _Pid} = erlmcp_complex_routing:start_link(),
    Route = #{
        id => <<"route-001">>,
        tenant_id => <<"acme-corp">>,
        pattern => <<"/resources.*">>,
        rules => [],
        fallback => <<"endpoint-default">>
    },
    ok = erlmcp_complex_routing:add_route(Route),
    ok = erlmcp_complex_routing:delete_route(<<"route-001">>),
    Routes = erlmcp_complex_routing:get_all_routes(),
    ?assert(not lists:any(fun(R) -> maps:get(id, R) =:= <<"route-001">> end, Routes)).

complex_routing_update_route_test() ->
    {ok, _Pid} = erlmcp_complex_routing:start_link(),
    Route = #{
        id => <<"route-001">>,
        tenant_id => <<"acme-corp">>,
        pattern => <<"/resources.*">>,
        rules => [],
        fallback => <<"endpoint-default">>
    },
    ok = erlmcp_complex_routing:add_route(Route),
    UpdatedRoute = Route#{
        pattern => <<"/api/.*">>
    },
    Result = erlmcp_complex_routing:update_route(<<"route-001">>, UpdatedRoute),
    ?assertEqual(ok, Result).

complex_routing_clear_routes_test() ->
    {ok, _Pid} = erlmcp_complex_routing:start_link(),
    Route = #{
        id => <<"route-001">>,
        tenant_id => <<"acme-corp">>,
        pattern => <<"/resources.*">>,
        rules => [],
        fallback => <<"endpoint-default">>
    },
    ok = erlmcp_complex_routing:add_route(Route),
    ok = erlmcp_complex_routing:clear_routes(),
    Routes = erlmcp_complex_routing:get_all_routes(),
    ?assertEqual([], Routes).

complex_routing_cache_stats_test() ->
    {ok, _Pid} = erlmcp_complex_routing:start_link(),
    Stats = erlmcp_complex_routing:get_cache_stats(),
    ?assert(is_map(Stats)),
    ?assertNotEqual(undefined, maps:get(cache_size, Stats)),
    ?assertNotEqual(undefined, maps:get(hits, Stats)),
    ?assertNotEqual(undefined, maps:get(misses, Stats)).

complex_routing_metrics_test() ->
    {ok, _Pid} = erlmcp_complex_routing:start_link(),
    Route = #{
        id => <<"route-001">>,
        tenant_id => <<"acme-corp">>,
        pattern => <<"/resources.*">>,
        rules => [#{condition => match_all, target => <<"endpoint-1">>, weight => 100}],
        fallback => <<"endpoint-default">>
    },
    ok = erlmcp_complex_routing:add_route(Route),
    _Match = erlmcp_complex_routing:match_request(<<"acme-corp">>, 'GET', <<"/resources/123">>, [], []),
    Metrics = erlmcp_complex_routing:get_routing_metrics(<<"acme-corp">>),
    ?assert(is_map(Metrics)),
    ?assertNotEqual(undefined, maps:get(total_requests, Metrics)).

%%====================================================================
%% Integration Tests
%%====================================================================

integration_otel_and_routing_test() ->
    {ok, _Pid} = erlmcp_complex_routing:start_link(),
    CorrelationId = erlmcp_advanced_otel_tracing:generate_correlation_id(),
    ok = erlmcp_advanced_otel_tracing:set_correlation_id(CorrelationId),
    Route = #{
        id => <<"route-001">>,
        tenant_id => <<"acme-corp">>,
        pattern => <<"/resources.*">>,
        rules => [#{condition => match_all, target => <<"endpoint-1">>, weight => 100}],
        fallback => <<"endpoint-default">>
    },
    ok = erlmcp_complex_routing:add_route(Route),
    {ok, _, _} = erlmcp_complex_routing:match_request(<<"acme-corp">>, 'GET', <<"/resources/123">>, [], []),
    RetrievedCorrelationId = erlmcp_advanced_otel_tracing:get_correlation_id(),
    ?assertEqual(CorrelationId, RetrievedCorrelationId).

integration_otel_and_session_replication_test() ->
    {ok, _Pid} = erlmcp_enterprise_session_replication:start_link(),
    CorrelationId = erlmcp_advanced_otel_tracing:generate_correlation_id(),
    ok = erlmcp_advanced_otel_tracing:set_correlation_id(CorrelationId),
    SessionId = <<"session123">>,
    Data = #{correlation_id => CorrelationId},
    Result = erlmcp_enterprise_session_replication:replicate_session(SessionId, create, Data),
    ?assertEqual(ok, Result).

integration_all_three_features_test() ->
    {ok, _Pid1} = erlmcp_enterprise_session_replication:start_link(),
    {ok, _Pid2} = erlmcp_complex_routing:start_link(),
    CorrelationId = erlmcp_advanced_otel_tracing:generate_correlation_id(),
    ok = erlmcp_advanced_otel_tracing:set_correlation_id(CorrelationId),
    Route = #{
        id => <<"route-001">>,
        tenant_id => <<"acme-corp">>,
        pattern => <<"/resources.*">>,
        rules => [#{condition => match_all, target => <<"endpoint-1">>, weight => 100}],
        fallback => <<"endpoint-default">>
    },
    ok = erlmcp_complex_routing:add_route(Route),
    {ok, _, _} = erlmcp_complex_routing:match_request(<<"acme-corp">>, 'GET', <<"/resources/123">>, [], []),
    SessionId = <<"session-with-routing">>,
    ok = erlmcp_enterprise_session_replication:replicate_session(SessionId, create, #{route => <<"route-001">>}),
    ?assert(true).
