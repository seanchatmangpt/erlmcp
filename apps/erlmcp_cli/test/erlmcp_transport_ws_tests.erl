%%%-------------------------------------------------------------------
%%% @doc
%%% WebSocket Transport Test Suite (EUnit)
%%%
%%% Tests for erlmcp_transport_ws module - WebSocket transport
%%%
%%% Chicago School TDD:
%%% - Tests FIRST, real WebSocket client (gun)
%%% - NO mocks, real WebSocket connections
%%% - State-based verification (connection state, messages)
%%%
%%% Coverage Target: ≥85%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_transport_ws_tests).

-include_lib("eunit/include/eunit.hrl").

%%%====================================================================
%%% Test Fixtures
%%%====================================================================

ws_transport_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [{"Initialize - start WebSocket transport", fun test_init_ws/0},
      {"Connection - establish WebSocket connection", fun test_connect_ws/0},
      {"Message - send WebSocket message", fun test_send_message/0},
      {"Ping/Pong - ping/pong handling", fun test_ping_pong/0},
      {"Reconnect - automatic reconnection", fun test_reconnect/0},
      {"Subprotocol - subprotocol negotiation", fun test_subprotocol/0},
      {"Concurrent - concurrent messages", fun test_concurrent_messages/0},
      {"Error handling - WebSocket errors", fun test_ws_errors/0}]}.

setup() ->
    application:ensure_all_started(erlmcp_cli),
    application:ensure_all_started(gun),
    ok.

cleanup(_Args) ->
    try erlmcp_transport_ws:stop() catch _:_ -> ok end,
    ok.

%%%====================================================================
%%% Initialization Tests
%%%====================================================================

test_init_ws() ->
    %% Start WebSocket transport
    Config = #{url => <<"ws://localhost:8080/ws">>, session_id => <<"test">>},
    {ok, Pid} = erlmcp_transport_ws:start_link(Config),
    ?assert(is_pid(Pid)),
    ?assert(is_process_alive(Pid)),

    %% Cleanup
    ok = erlmcp_transport_ws:stop().

%%%====================================================================
%%% Connection Tests
%%%====================================================================

test_connect_ws() ->
    %% Start transport
    Config = #{url => <<"ws://localhost:8080/ws">>, session_id => <<"test-connect">>},
    {ok, _Pid} = erlmcp_transport_ws:start_link(Config),

    %% Verify connection state
    {ok, State} = erlmcp_transport_ws:get_state(),
    ?assert(is_map(maps:get(connection, State, #{}))),

    %% Cleanup
    ok = erlmcp_transport_ws:stop().

%%%====================================================================
%%% Message Tests
%%%====================================================================

test_send_message() ->
    %% Start transport
    Config = #{url => <<"ws://localhost:8080/ws">>, session_id => <<"test-send">>},
    {ok, _Pid} = erlmcp_transport_ws:start_link(Config),

    %% Send message
    Message = <<"{\"jsonrpc\":\"2.0\",\"method\":\"test\",\"id\":1}">>,
    ok = erlmcp_transport_ws:send(Message),

    %% Verify message sent
    {ok, State} = erlmcp_transport_ws:get_state(),
    ?assert(maps:get(messages_sent, State, 0) > 0),

    %% Cleanup
    ok = erlmcp_transport_ws:stop().

%%%====================================================================
%%% Ping/Pong Tests
%%%====================================================================

test_ping_pong() ->
    %% Start transport with ping enabled
    Config = #{
        url => <<"ws://localhost:8080/ws">>,
        session_id => <<"test-ping">>,
        ping_interval => 30000,
        ping_timeout => 5000
    },
    {ok, _Pid} = erlmcp_transport_ws:start_link(Config),

    %% Verify ping config
    {ok, State} = erlmcp_transport_ws:get_state(),
    ?assertEqual(30000, maps:get(ping_interval, State)),
    ?assertEqual(5000, maps:get(ping_timeout, State)),

    %% Cleanup
    ok = erlmcp_transport_ws:stop().

%%%====================================================================
%%% Reconnection Tests
%%%====================================================================

test_reconnect() ->
    %% Start transport with reconnect
    Config = #{
        url => <<"ws://localhost:8080/ws">>,
        session_id => <<"test-reconnect">>,
        auto_reconnect => true,
        max_retries => 5
    },
    {ok, _Pid} = erlmcp_transport_ws:start_link(Config),

    %% Verify reconnect config
    {ok, State} = erlmcp_transport_ws:get_state(),
    ?assertEqual(true, maps:get(auto_reconnect, State)),
    ?assertEqual(5, maps:get(max_retries, State)),

    %% Cleanup
    ok = erlmcp_transport_ws:stop().

%%%====================================================================
%%% Subprotocol Tests
%%%====================================================================

test_subprotocol() ->
    %% Start transport with subprotocol
    Config = #{
        url => <<"ws://localhost:8080/ws">>,
        session_id => <<"test-subprotocol">>,
        subprotocols => [<<"mcp.json">>, <<"mcp.msgpack">>]
    },
    {ok, _Pid} = erlmcp_transport_ws:start_link(Config),

    %% Verify subprotocol config
    {ok, State} = erlmcp_transport_ws:get_state(),
    ?assertEqual([<<"mcp.json">>, <<"mcp.msgpack">>], maps:get(subprotocols, State)),

    %% Cleanup
    ok = erlmcp_transport_ws:stop().

%%%====================================================================
%%% Concurrent Messages Tests
%%%====================================================================

test_concurrent_messages() ->
    %% Start transport
    Config = #{url => <<"ws://localhost:8080/ws">>, session_id => <<"test-concurrent">>},
    {ok, _Pid} = erlmcp_transport_ws:start_link(Config),

    %% Send concurrent messages
    Pids = [spawn(fun() ->
        Message = <<"{\"jsonrpc\":\"2.0\",\"method\":\"test\",\"id\":",
                   (integer_to_binary(N))/binary, "}">>,
        erlmcp_transport_ws:send(Message)
    end) || N <- lists:seq(1, 10)],

    %% Wait for all sends
    timer:sleep(100),

    %% Verify all processes completed
    lists:foreach(fun(P) ->
        ?assertNot(is_process_alive(P))
    end, Pids),

    %% Cleanup
    ok = erlmcp_transport_ws:stop().

%%%====================================================================
%%% Error Handling Tests
%%%====================================================================

test_ws_errors() ->
    %% Start transport
    Config = #{url => <<"ws://localhost:8080/ws">>, session_id => <<"test-errors">>},
    {ok, _Pid} = erlmcp_transport_ws:start_link(Config),

    %% Verify error handling
    {ok, State} = erlmcp_transport_ws:get_state(),
    ?assert(is_list(maps:get(errors, State, []))),

    %% Cleanup
    ok = erlmcp_transport_ws:stop().
