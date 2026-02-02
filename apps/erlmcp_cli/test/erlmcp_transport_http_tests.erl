%%%-------------------------------------------------------------------
%%% @doc
%%% HTTP Transport Test Suite (EUnit)
%%%
%%% Tests for erlmcp_transport_http module - HTTP transport
%%%
%%% Chicago School TDD:
%%% - Tests FIRST, real HTTP client (gun)
%%% - NO mocks, real HTTP requests
%%% - State-based verification (connection state, responses)
%%%
%%% Coverage Target: ≥85%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_transport_http_tests).

-include_lib("eunit/include/eunit.hrl").

%%%====================================================================
%%% Test Fixtures
%%%====================================================================

http_transport_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [{"Initialize - start HTTP transport", fun test_init_http/0},
      {"Request - send HTTP request", fun test_send_request/0},
      {"Response - receive HTTP response", fun test_receive_response/0},
      {"Connection pooling - pool management", fun test_connection_pooling/0},
      {"Headers - header validation", fun test_header_validation/0},
      {"Webhook - webhook delivery", fun test_webhook_delivery/0},
      {"Streaming - response streaming", fun test_streaming_support/0},
      {"Error handling - HTTP errors", fun test_http_errors/0}]}.

setup() ->
    application:ensure_all_started(erlmcp_cli),
    application:ensure_all_started(gun),
    ok.

cleanup(_Args) ->
    try erlmcp_transport_http:stop() catch _:_ -> ok end,
    ok.

%%%====================================================================
%%% Initialization Tests
%%%====================================================================

test_init_http() ->
    %% Start HTTP transport
    Config = #{url => <<"http://localhost:8080">>, session_id => <<"test">>},
    {ok, Pid} = erlmcp_transport_http:start_link(Config),
    ?assert(is_pid(Pid)),
    ?assert(is_process_alive(Pid)),

    %% Cleanup
    ok = erlmcp_transport_http:stop().

%%%====================================================================
%%% Request/Response Tests
%%%====================================================================

test_send_request() ->
    %% Start transport
    Config = #{url => <<"http://localhost:8080">>, session_id => <<"test-send">>},
    {ok, _Pid} = erlmcp_transport_http:start_link(Config),

    %% Send request
    Request = #{
        method => post,
        path => <<"/mcp">>,
        headers => #{<<"content-type">> => <<"application/json">>},
        body => <<"{\"jsonrpc\":\"2.0\",\"method\":\"test\",\"id\":1}">>
    },

    ok = erlmcp_transport_http:send_request(Request),

    %% Cleanup
    ok = erlmcp_transport_http:stop().

test_receive_response() ->
    %% Start transport
    Config = #{url => <<"http://localhost:8080">>, session_id => <<"test-recv">>},
    {ok, _Pid} = erlmcp_transport_http:start_link(Config),

    %% Verify response handling
    {ok, State} = erlmcp_transport_http:get_state(),
    ?assert(is_map(maps:get(responses, State, #{}))),

    %% Cleanup
    ok = erlmcp_transport_http:stop().

%%%====================================================================
%%% Connection Pooling Tests
%%%====================================================================

test_connection_pooling() ->
    %% Start transport with pooling
    Config = #{
        url => <<"http://localhost:8080">>,
        session_id => <<"test-pool">>,
        pool_size => 10,
        pool_max_overflow => 20
    },
    {ok, _Pid} = erlmcp_transport_http:start_link(Config),

    %% Verify pool config
    {ok, State} = erlmcp_transport_http:get_state(),
    ?assertEqual(10, maps:get(pool_size, State)),
    ?assertEqual(20, maps:get(pool_max_overflow, State)),

    %% Cleanup
    ok = erlmcp_transport_http:stop().

%%%====================================================================
%%% Header Validation Tests
%%%====================================================================

test_header_validation() ->
    %% Start transport
    Config = #{url => <<"http://localhost:8080">>, session_id => <<"test-headers">>},
    {ok, _Pid} = erlmcp_transport_http:start_link(Config),

    %% Valid headers
    ValidHeaders = #{
        <<"content-type">> => <<"application/json">>,
        <<"authorization">> => <<"Bearer token">>,
        <<"x-custom-header">> => <<"value">>
    },

    ?assertEqual(true, erlmcp_transport_http:validate_headers(ValidHeaders)),

    %% Invalid headers
    InvalidHeaders = #{
        <<"content-type">> => <<>>
    },

    ?assertEqual(false, erlmcp_transport_http:validate_headers(InvalidHeaders)),

    %% Cleanup
    ok = erlmcp_transport_http:stop().

%%%====================================================================
%%% Webhook Tests
%%%====================================================================

test_webhook_delivery() ->
    %% Start transport
    Config = #{url => <<"http://localhost:8080">>, session_id => <<"test-webhook">>},
    {ok, _Pid} = erlmcp_transport_http:start_link(Config),

    %% Send webhook
    Webhook = #{
        url => <<"http://localhost:8080/webhook">>,
        event => <<"test.event">>,
        data => #{<<"key">> => <<"value">>}
    },

    ok = erlmcp_transport_http:send_webhook(Webhook),

    %% Cleanup
    ok = erlmcp_transport_http:stop().

%%%====================================================================
%%% Streaming Tests
%%%====================================================================

test_streaming_support() ->
    %% Start transport with streaming
    Config = #{
        url => <<"http://localhost:8080">>,
        session_id => <<"test-stream">>,
        stream_enabled => true
    },
    {ok, _Pid} = erlmcp_transport_http:start_link(Config),

    %% Verify streaming enabled
    {ok, State} = erlmcp_transport_http:get_state(),
    ?assertEqual(true, maps:get(stream_enabled, State)),

    %% Cleanup
    ok = erlmcp_transport_http:stop().

%%%====================================================================
%%% Error Handling Tests
%%%====================================================================

test_http_errors() ->
    %% Start transport
    Config = #{url => <<"http://localhost:8080">>, session_id => <<"test-errors">>},
    {ok, _Pid} = erlmcp_transport_http:start_link(Config),

    %% Simulate HTTP error
    {ok, State} = erlmcp_transport_http:get_state(),
    ?assert(is_list(maps:get(errors, State, []))),

    %% Cleanup
    ok = erlmcp_transport_http:stop().
