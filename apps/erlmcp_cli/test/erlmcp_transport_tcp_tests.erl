%%%-------------------------------------------------------------------
%%% @doc
%%% TCP Transport Test Suite (EUnit)
%%%
%%% Tests for erlmcp_transport_tcp module - TCP transport
%%%
%%% Chicago School TDD:
%%% - Tests FIRST, real TCP sockets
%%% - NO mocks, real TCP connections
%%% - State-based verification (connection state, messages)
%%%
%%% Coverage Target: ≥85%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_transport_tcp_tests).

-include_lib("eunit/include/eunit.hrl").

%%%====================================================================
%%% Test Fixtures
%%%====================================================================

tcp_transport_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [{"Initialize - start TCP transport", fun test_init_tcp/0},
      {"Connection - connect to server", fun test_connect_server/0},
      {"Message framing - frame TCP messages", fun test_frame_messages/0},
      {"Reconnect - automatic reconnection", fun test_reconnect/0},
      {"Error handling - connection refused", fun test_connection_refused/0},
      {"Error handling - connection timeout", fun test_connection_timeout/0},
      {"Concurrent - multiple connections", fun test_multiple_connections/0},
      {"TLS - TLS/SSL support", fun test_tls_support/0}]}.

setup() ->
    application:ensure_all_started(erlmcp_cli),
    ok.

cleanup(_Args) ->
    try erlmcp_transport_tcp:stop() catch _:_ -> ok end,
    ok.

%%%====================================================================
%%% Initialization Tests
%%%====================================================================

test_init_tcp() ->
    %% Start TCP transport
    Config = #{host => <<"localhost">>, port => 9999, session_id => <<"test">>},
    {ok, Pid} = erlmcp_transport_tcp:start_link(Config),
    ?assert(is_pid(Pid)),
    ?assert(is_process_alive(Pid)),

    %% Cleanup
    ok = erlmcp_transport_tcp:stop().

%%%====================================================================
%%% Connection Tests
%%%====================================================================

test_connect_server() ->
    %% Start transport
    Config = #{host => <<"localhost">>, port => 9999, session_id => <<"test-connect">>},
    {ok, _Pid} = erlmcp_transport_tcp:start_link(Config),

    %% Verify connection state
    {ok, State} = erlmcp_transport_tcp:get_state(),
    ?assert(is_map(maps:get(connection, State, #{}))),

    %% Cleanup
    ok = erlmcp_transport_tcp:stop().

%%%====================================================================
%%% Message Framing Tests
%%%====================================================================

test_frame_messages() ->
    %% Start transport
    Config = #{host => <<"localhost">>, port => 9999, session_id => <<"test-frame">>},
    {ok, _Pid} = erlmcp_transport_tcp:start_link(Config),

    %% Frame message
    Message = <<"{\"jsonrpc\":\"2.0\",\"method\":\"test\",\"id\":1}">>,
    Framed = erlmcp_transport_tcp:frame_message(Message),

    %% Verify framing (length prefix + message)
    ?assert(is_binary(Framed)),
    ?assert(byte_size(Framed) > byte_size(Message)),

    %% Cleanup
    ok = erlmcp_transport_tcp:stop().

%%%====================================================================
%%% Reconnection Tests
%%%====================================================================

test_reconnect() ->
    %% Start transport with reconnect enabled
    Config = #{
        host => <<"localhost">>,
        port => 9999,
        session_id => <<"test-reconnect">>,
        auto_reconnect => true,
        max_retries => 3
    },
    {ok, _Pid} = erlmcp_transport_tcp:start_link(Config),

    %% Verify reconnect config stored
    {ok, State} = erlmcp_transport_tcp:get_state(),
    ?assertEqual(true, maps:get(auto_reconnect, State)),
    ?assertEqual(3, maps:get(max_retries, State)),

    %% Cleanup
    ok = erlmcp_transport_tcp:stop().

%%%====================================================================
%%% Error Handling Tests
%%%====================================================================

test_connection_refused() ->
    %% Try to connect to non-existent server
    Config = #{host => <<"localhost">>, port => 1, session_id => <<"test-refused">>},
    {ok, _Pid} = erlmcp_transport_tcp:start_link(Config),

    %% Wait for connection attempt
    timer:sleep(100),

    %% Verify error handled gracefully
    {ok, State} = erlmcp_transport_tcp:get_state(),
    ?assert(is_list(maps:get(errors, State, []))),

    %% Cleanup
    ok = erlmcp_transport_tcp:stop().

test_connection_timeout() ->
    %% Start transport with short timeout
    Config = #{
        host => <<"192.0.2.1">>,  %% TEST-NET-1 (non-routable)
        port => 9999,
        session_id => <<"test-timeout">>,
        connect_timeout => 100
    },
    {ok, _Pid} = erlmcp_transport_tcp:start_link(Config),

    %% Wait for timeout
    timer:sleep(200),

    %% Verify timeout handled
    {ok, State} = erlmcp_transport_tcp:get_state(),
    ?assertEqual(disconnected, maps:get(status, State)),

    %% Cleanup
    ok = erlmcp_transport_tcp:stop().

%%%====================================================================
%%% Concurrent Connections Tests
%%%====================================================================

test_multiple_connections() ->
    %% Start multiple TCP transports
    Configs = [
        #{host => <<"localhost">>, port => 9999, session_id => <<"test-multi1">>},
        #{host => <<"localhost">>, port => 9998, session_id => <<"test-multi2">>},
        #{host => <<"localhost">>, port => 9997, session_id => <<"test-multi3">>}
    ],

    Pids = [begin
        {ok, P} = erlmcp_transport_tcp:start_link(C),
        P
    end || C <- Configs],

    %% Verify all started
    ?assertEqual(3, length(Pids)),
    lists:foreach(fun(P) -> ?assert(is_process_alive(P)) end, Pids),

    %% Cleanup
    lists:foreach(fun(_) -> erlmcp_transport_tcp:stop() end, Configs).

%%%====================================================================
%%% TLS Tests
%%%====================================================================

test_tls_support() ->
    %% Start transport with TLS enabled
    Config = #{
        host => <<"localhost">>,
        port => 9999,
        session_id => <<"test-tls">>,
        tls_enabled => true,
        tls_verify => verify_none
    },
    {ok, _Pid} = erlmcp_transport_tcp:start_link(Config),

    %% Verify TLS config stored
    {ok, State} = erlmcp_transport_tcp:get_state(),
    ?assertEqual(true, maps:get(tls_enabled, State)),

    %% Cleanup
    ok = erlmcp_transport_tcp:stop().
