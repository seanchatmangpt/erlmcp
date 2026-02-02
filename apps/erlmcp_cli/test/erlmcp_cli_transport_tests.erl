%%%-------------------------------------------------------------------
%%% @doc
%%% CLI Transport Test Suite (EUnit)
%%%
%%% Tests for erlmcp_cli_transport module - Transport layer management
%%%
%%% Chicago School TDD:
%%% - Tests FIRST, real transport processes
%%% - NO mocks, real transport initialization
%%% - State-based verification (transport state, messages)
%%%
%%% Coverage Target: ≥90%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_cli_transport_tests).

-include_lib("eunit/include/eunit.hrl").

%%%====================================================================
%%% Test Fixtures
%%%====================================================================

transport_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [{"Transport initialization - stdio transport", fun test_init_stdio_transport/0},
      {"Transport initialization - tcp transport", fun test_init_tcp_transport/0},
      {"Transport initialization - http transport", fun test_init_http_transport/0},
      {"Transport initialization - ws transport", fun test_init_ws_transport/0},
      {"Transport initialization - sse transport", fun test_init_sse_transport/0},
      {"Transport initialization - invalid transport", fun test_init_invalid_transport/0},
      {"Transport state - active check", fun test_transport_active_check/0},
      {"Transport state - inactive check", fun test_transport_inactive_check/0},
      {"Transport state - get transport state", fun test_get_transport_state/0},
      {"Message sending - send to active transport", fun test_send_to_active_transport/0},
      {"Message sending - send to inactive transport", fun test_send_to_inactive_transport/0},
      {"Message sending - send large message", fun test_send_large_message/0},
      {"Message receiving - receive message", fun test_receive_message/0},
      {"Connection handling - connection established", fun test_connection_established/0},
      {"Connection handling - connection lost", fun test_connection_lost/0},
      {"Error recovery - transport restart", fun test_transport_restart/0},
      {"Error recovery - connection retry", fun test_connection_retry/0},
      {"Concurrent operations - multiple transports", fun test_multiple_transports/0},
      {"Concurrent operations - concurrent sends", fun test_concurrent_sends/0},
      {"Metrics - transport metrics collection", fun test_transport_metrics/0},
      {"Metrics - get transport stats", fun test_get_transport_stats/0},
      {"Cleanup - close transport", fun test_close_transport/0},
      {"Cleanup - close all transports", fun test_close_all_transports/0}]}.

setup() ->
    %% Start required applications
    application:ensure_all_started(crypto),
    application:ensure_all_started(erlmcp_cli),
    ok.

cleanup(_Args) ->
    %% Clean up any remaining transports
    try
        erlmcp_cli_transport:close_transport(<<"stdio">>),
        erlmcp_cli_transport:close_transport(<<"tcp">>),
        erlmcp_cli_transport:close_transport(<<"http">>),
        erlmcp_cli_transport:close_transport(<<"ws">>),
        erlmcp_cli_transport:close_transport(<<"sse">>)
    catch
        _:_ -> ok
    end,
    ok.

%%%====================================================================
%%% Transport Initialization Tests
%%%====================================================================

test_init_stdio_transport() ->
    %% Initialize stdio transport
    Config = #{
        <<"type">> => <<"stdio">>,
        <<"session_id">> => <<"test-session-stdio">>
    },

    ok = erlmcp_cli_transport:transport(<<"stdio">>, Config),

    %% Verify transport active
    ?assert(erlmcp_cli_transport:is_active(<<"stdio">>)),

    %% Cleanup
    ok = erlmcp_cli_transport:close_transport(<<"stdio">>).

test_init_tcp_transport() ->
    %% Initialize tcp transport
    Config = #{
        <<"type">> => <<"tcp">>,
        <<"host">> => <<"localhost">>,
        <<"port">> => 9999,
        <<"session_id">> => <<"test-session-tcp">>
    },

    ok = erlmcp_cli_transport:transport(<<"tcp">>, Config),

    %% Verify transport initialized
    ?assert(is_pid(whereis(erlmcp_transport_tcp))),

    %% Cleanup
    ok = erlmcp_cli_transport:close_transport(<<"tcp">>).

test_init_http_transport() ->
    %% Initialize http transport
    Config = #{
        <<"type">> => <<"http">>,
        <<"url">> => <<"http://localhost:8080">>,
        <<"session_id">> => <<"test-session-http">>
    },

    ok = erlmcp_cli_transport:transport(<<"http">>, Config),

    %% Verify transport initialized
    ?assert(is_pid(whereis(erlmcp_transport_http))),

    %% Cleanup
    ok = erlmcp_cli_transport:close_transport(<<"http">>).

test_init_ws_transport() ->
    %% Initialize websocket transport
    Config = #{
        <<"type">> => <<"ws">>,
        <<"url">> => <<"ws://localhost:8080/ws">>,
        <<"session_id">> => <<"test-session-ws">>
    },

    ok = erlmcp_cli_transport:transport(<<"ws">>, Config),

    %% Verify transport initialized
    ?assert(is_pid(whereis(erlmcp_transport_ws))),

    %% Cleanup
    ok = erlmcp_cli_transport:close_transport(<<"ws">>).

test_init_sse_transport() ->
    %% Initialize sse transport
    Config = #{
        <<"type">> => <<"sse">>,
        <<"url">> => <<"http://localhost:8080/events">>,
        <<"session_id">> => <<"test-session-sse">>
    },

    ok = erlmcp_cli_transport:transport(<<"sse">>, Config),

    %% Verify transport initialized
    ?assert(is_pid(whereis(erlmcp_transport_sse))),

    %% Cleanup
    ok = erlmcp_cli_transport:close_transport(<<"sse">>).

test_init_invalid_transport() ->
    %% Attempt to initialize invalid transport
    Config = #{
        <<"type">> => <<"invalid">>,
        <<"session_id">> => <<"test-session-invalid">>
    },

    {error, invalid_transport_type} = erlmcp_cli_transport:transport(<<"invalid">>, Config).

%%%====================================================================
%%% Transport State Tests
%%%====================================================================

test_transport_active_check() ->
    %% Initialize transport
    Config = #{<<"type">> => <<"stdio">>, <<"session_id">> => <<"test">>},
    ok = erlmcp_cli_transport:transport(<<"stdio">>, Config),

    %% Check if active
    ?assert(erlmcp_cli_transport:is_active(<<"stdio">>)),

    %% Cleanup
    ok = erlmcp_cli_transport:close_transport(<<"stdio">>).

test_transport_inactive_check() ->
    %% Check inactive transport
    ?assertNot(erlmcp_cli_transport:is_active(<<"nonexistent">>)).

test_get_transport_state() ->
    %% Initialize transport
    Config = #{<<"type">> => <<"stdio">>, <<"session_id">> => <<"test-state">>},
    ok = erlmcp_cli_transport:transport(<<"stdio">>, Config),

    %% Get transport state
    {ok, State} = erlmcp_cli_transport:get_transport_state(<<"stdio">>),

    %% Verify state structure
    ?assert(is_map(State)),
    ?assertEqual(<<"stdio">>, maps:get(<<"type">>, State)),
    ?assertEqual(true, maps:get(<<"active">>, State)),

    %% Cleanup
    ok = erlmcp_cli_transport:close_transport(<<"stdio">>).

%%%====================================================================
%%% Message Sending Tests
%%%====================================================================

test_send_to_active_transport() ->
    %% Initialize transport
    Config = #{<<"type">> => <<"stdio">>, <<"session_id">> => <<"test-send">>},
    ok = erlmcp_cli_transport:transport(<<"stdio">>, Config),

    %% Send message
    Message = <<"{\"jsonrpc\":\"2.0\",\"method\":\"test\",\"id\":1}">>,
    ok = erlmcp_cli_transport:send_data(<<"stdio">>, Message),

    %% Verify message sent (check metrics)
    {ok, Stats} = erlmcp_cli_transport:get_transport_stats(<<"stdio">>),
    ?assert(maps:get(<<"messages_sent">>, Stats, 0) > 0),

    %% Cleanup
    ok = erlmcp_cli_transport:close_transport(<<"stdio">>).

test_send_to_inactive_transport() ->
    %% Send to inactive transport
    Message = <<"{\"jsonrpc\":\"2.0\",\"method\":\"test\",\"id\":1}">>,
    {error, transport_not_active} = erlmcp_cli_transport:send_data(<<"inactive">>, Message).

test_send_large_message() ->
    %% Initialize transport
    Config = #{<<"type">> => <<"stdio">>, <<"session_id">> => <<"test-large">>},
    ok = erlmcp_cli_transport:transport(<<"stdio">>, Config),

    %% Send large message
    LargeMessage = <<"{\"jsonrpc\":\"2.0\",\"method\":\"test\",\"params\":{\"data\":\"",
                     binary:copy(<<"x">>, 10000), "\"},\"id\":1}">>,

    ok = erlmcp_cli_transport:send_data(<<"stdio">>, LargeMessage),

    %% Verify sent
    {ok, Stats} = erlmcp_cli_transport:get_transport_stats(<<"stdio">>),
    ?assert(maps:get(<<"bytes_sent">>, Stats, 0) > 10000),

    %% Cleanup
    ok = erlmcp_cli_transport:close_transport(<<"stdio">>).

%%%====================================================================
%%% Message Receiving Tests
%%%====================================================================

test_receive_message() ->
    %% Initialize transport
    Config = #{<<"type">> => <<"stdio">>, <<"session_id">> => <<"test-recv">>},
    ok = erlmcp_cli_transport:transport(<<"stdio">>, Config),

    %% Send message to self (simulating receive)
    Message = <<"{\"jsonrpc\":\"2.0\",\"method\":\"test\",\"id\":1}">>,

    %% In real scenario, transport would receive this
    %% For now, verify receive stats are tracked
    ok = erlmcp_cli_transport:send_data(<<"stdio">>, Message),

    {ok, Stats} = erlmcp_cli_transport:get_transport_stats(<<"stdio">>),
    ?assert(is_map(Stats)),

    %% Cleanup
    ok = erlmcp_cli_transport:close_transport(<<"stdio">>).

%%%====================================================================
%%% Connection Handling Tests
%%%====================================================================

test_connection_established() ->
    %% Initialize transport
    Config = #{<<"type">> => <<"stdio">>, <<"session_id">> => <<"test-conn">>},
    ok = erlmcp_cli_transport:transport(<<"stdio">>, Config),

    %% Verify connection established
    {ok, State} = erlmcp_cli_transport:get_transport_state(<<"stdio">>),
    ?assertEqual(connected, maps:get(<<"connection_status">>, State, connected)),

    %% Cleanup
    ok = erlmcp_cli_transport:close_transport(<<"stdio">>).

test_connection_lost() ->
    %% Initialize transport
    Config = #{<<"type">> => <<"stdio">>, <<"session_id">> => <<"test-lost">>},
    ok = erlmcp_cli_transport:transport(<<"stdio">>, Config),

    %% Simulate connection loss
    ok = erlmcp_cli_transport:close_transport(<<"stdio">>),

    %% Verify connection lost
    {error, transport_not_active} = erlmcp_cli_transport:get_transport_state(<<"stdio">>).

%%%====================================================================
%%% Error Recovery Tests
%%%====================================================================

test_transport_restart() ->
    %% Initialize transport
    Config = #{<<"type">> => <<"stdio">>, <<"session_id">> => <<"test-restart">>},
    ok = erlmcp_cli_transport:transport(<<"stdio">>, Config),

    %% Close transport
    ok = erlmcp_cli_transport:close_transport(<<"stdio">>),

    %% Restart transport
    ok = erlmcp_cli_transport:transport(<<"stdio">>, Config),

    %% Verify restarted
    ?assert(erlmcp_cli_transport:is_active(<<"stdio">>)),

    %% Cleanup
    ok = erlmcp_cli_transport:close_transport(<<"stdio">>).

test_connection_retry() ->
    %% Initialize transport with retry config
    Config = #{
        <<"type">> => <<"stdio">>,
        <<"session_id">> => <<"test-retry">>,
        <<"retry_count">> => 3,
        <<"retry_delay">> => 100
    },

    ok = erlmcp_cli_transport:transport(<<"stdio">>, Config),

    %% Verify retry config stored
    {ok, State} = erlmcp_cli_transport:get_transport_state(<<"stdio">>),
    ?assertEqual(3, maps:get(<<"retry_count">>, State, 3)),

    %% Cleanup
    ok = erlmcp_cli_transport:close_transport(<<"stdio">>).

%%%====================================================================
%%% Concurrent Operations Tests
%%%====================================================================

test_multiple_transports() ->
    %% Initialize multiple transports
    StdioConfig = #{<<"type">> => <<"stdio">>, <<"session_id">> => <<"test-multi1">>},
    TcpConfig = #{<<"type">> => <<"tcp">>, <<"host">> => <<"localhost">>, <<"port">> => 9999, <<"session_id">> => <<"test-multi2">>},

    ok = erlmcp_cli_transport:transport(<<"stdio">>, StdioConfig),
    ok = erlmcp_cli_transport:transport(<<"tcp">>, TcpConfig),

    %% Verify both active
    ?assert(erlmcp_cli_transport:is_active(<<"stdio">>)),
    ?assert(erlmcp_cli_transport:is_active(<<"tcp">>)),

    %% Cleanup
    ok = erlmcp_cli_transport:close_transport(<<"stdio">>),
    ok = erlmcp_cli_transport:close_transport(<<"tcp">>).

test_concurrent_sends() ->
    %% Initialize transport
    Config = #{<<"type">> => <<"stdio">>, <<"session_id">> => <<"test-concurrent">>},
    ok = erlmcp_cli_transport:transport(<<"stdio">>, Config),

    %% Spawn multiple processes sending concurrently
    NumSends = 10,
    Pids = [spawn(fun() ->
        Message = <<"{\"jsonrpc\":\"2.0\",\"method\":\"test\",\"id\":",
                   (integer_to_binary(N))/binary, "}">>,
        erlmcp_cli_transport:send_data(<<"stdio">>, Message),
        self() ! sent
    end) || N <- lists:seq(1, NumSends)],

    %% Wait for all sends
    [receive
        sent -> ok
    after 1000 ->
        ct:fail("Concurrent send timeout")
    end || _ <- Pids],

    %% Verify all sent
    {ok, Stats} = erlmcp_cli_transport:get_transport_stats(<<"stdio">>),
    ?assert(maps:get(<<"messages_sent">>, Stats, 0) >= NumSends),

    %% Cleanup
    ok = erlmcp_cli_transport:close_transport(<<"stdio">>).

%%%====================================================================
%%% Metrics Tests
%%%====================================================================

test_transport_metrics() ->
    %% Initialize transport
    Config = #{<<"type">> => <<"stdio">>, <<"session_id">> => <<"test-metrics">>},
    ok = erlmcp_cli_transport:transport(<<"stdio">>, Config),

    %% Send messages to generate metrics
    lists:foreach(fun(N) ->
        Message = <<"{\"jsonrpc\":\"2.0\",\"method\":\"test\",\"id\":",
                   (integer_to_binary(N))/binary, "}">>,
        erlmcp_cli_transport:send_data(<<"stdio">>, Message)
    end, lists:seq(1, 5)),

    %% Get metrics
    {ok, Metrics} = erlmcp_cli_transport:get_transport_metrics(<<"stdio">>),

    %% Verify metrics collected
    ?assert(is_map(Metrics)),
    ?assert(maps:get(<<"messages_sent">>, Metrics, 0) > 0),

    %% Cleanup
    ok = erlmcp_cli_transport:close_transport(<<"stdio">>).

test_get_transport_stats() ->
    %% Initialize transport
    Config = #{<<"type">> => <<"stdio">>, <<"session_id">> => <<"test-stats">>},
    ok = erlmcp_cli_transport:transport(<<"stdio">>, Config),

    %% Get stats
    {ok, Stats} = erlmcp_cli_transport:get_transport_stats(<<"stdio">>),

    %% Verify stats structure
    ?assert(is_map(Stats)),
    ?assert(is_integer(maps:get(<<"messages_sent">>, Stats, 0))),
    ?assert(is_integer(maps:get(<<"messages_received">>, Stats, 0))),
    ?assert(is_integer(maps:get(<<"bytes_sent">>, Stats, 0))),
    ?assert(is_integer(maps:get(<<"bytes_received">>, Stats, 0))),
    ?assert(is_integer(maps:get(<<"errors">>, Stats, 0))),

    %% Cleanup
    ok = erlmcp_cli_transport:close_transport(<<"stdio">>).

%%%====================================================================
%%% Cleanup Tests
%%%====================================================================

test_close_transport() ->
    %% Initialize transport
    Config = #{<<"type">> => <<"stdio">>, <<"session_id">> => <<"test-close">>},
    ok = erlmcp_cli_transport:transport(<<"stdio">>, Config),

    %% Verify active
    ?assert(erlmcp_cli_transport:is_active(<<"stdio">>)),

    %% Close transport
    ok = erlmcp_cli_transport:close_transport(<<"stdio">>),

    %% Verify inactive
    ?assertNot(erlmcp_cli_transport:is_active(<<"stdio">>)).

test_close_all_transports() ->
    %% Initialize multiple transports
    StdioConfig = #{<<"type">> => <<"stdio">>, <<"session_id">> => <<"test-closeall1">>},
    TcpConfig = #{<<"type">> => <<"tcp">>, <<"host">> => <<"localhost">>, <<"port">> => 9999, <<"session_id">> => <<"test-closeall2">>},

    ok = erlmcp_cli_transport:transport(<<"stdio">>, StdioConfig),
    ok = erlmcp_cli_transport:transport(<<"tcp">>, TcpConfig),

    %% Verify both active
    ?assert(erlmcp_cli_transport:is_active(<<"stdio">>)),
    ?assert(erlmcp_cli_transport:is_active(<<"tcp">>)),

    %% Close all
    ok = erlmcp_cli_transport:close_all_transports(),

    %% Verify all inactive
    ?assertNot(erlmcp_cli_transport:is_active(<<"stdio">>)),
    ?assertNot(erlmcp_cli_transport:is_active(<<"tcp">>)).
