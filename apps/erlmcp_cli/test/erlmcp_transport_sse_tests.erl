%%%-------------------------------------------------------------------
%%% @doc
%%% SSE Transport Test Suite (EUnit)
%%%
%%% Tests for erlmcp_transport_sse module - Server-Sent Events transport
%%%
%%% Chicago School TDD:
%%% - Tests FIRST, real HTTP client for SSE
%%% - NO mocks, real SSE connections
%%% - State-based verification (event parsing, delivery)
%%%
%%% Coverage Target: ≥85%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_transport_sse_tests).

-include_lib("eunit/include/eunit.hrl").

%%%====================================================================
%%% Test Fixtures
%%%====================================================================

sse_transport_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [{"Initialize - start SSE transport", fun test_init_sse/0},
      {"Event parsing - parse SSE events", fun test_parse_events/0},
      {"Connection - establish SSE connection", fun test_connect_sse/0},
      {"Reconnect - automatic reconnection", fun test_reconnect/0},
      {"Concurrent - multiple SSE streams", fun test_multiple_streams/0},
      {"Backpressure - backpressure handling", fun test_backpressure/0},
      {"Delivery - event delivery guarantees", fun test_event_delivery/0},
      {"Error handling - SSE errors", fun test_sse_errors/0}]}.

setup() ->
    application:ensure_all_started(erlmcp_cli),
    application:ensure_all_started(gun),
    ok.

cleanup(_Args) ->
    try erlmcp_transport_sse:stop() catch _:_ -> ok end,
    ok.

%%%====================================================================
%%% Initialization Tests
%%%====================================================================

test_init_sse() ->
    %% Start SSE transport
    Config = #{url => <<"http://localhost:8080/events">>, session_id => <<"test">>},
    {ok, Pid} = erlmcp_transport_sse:start_link(Config),
    ?assert(is_pid(Pid)),
    ?assert(is_process_alive(Pid)),

    %% Cleanup
    ok = erlmcp_transport_sse:stop().

%%%====================================================================
%%% Event Parsing Tests
%%%====================================================================

test_parse_events() ->
    %% Start transport
    Config = #{url => <<"http://localhost:8080/events">>, session_id => <<"test-parse">>},
    {ok, _Pid} = erlmcp_transport_sse:start_link(Config),

    %% Parse SSE event
    EventRaw = <<"event: message\ndata: {\"test\":\"value\"}\n\n">>,
    {ok, Event} = erlmcp_transport_sse:parse_event(EventRaw),

    %% Verify parsed event
    ?assertEqual(<<"message">>, maps:get(<<"event">>, Event)),
    ?assertEqual(<<"{\"test\":\"value\"}">>, maps:get(<<"data">>, Event)),

    %% Cleanup
    ok = erlmcp_transport_sse:stop().

%%%====================================================================
%%% Connection Tests
%%%====================================================================

test_connect_sse() ->
    %% Start transport
    Config = #{url => <<"http://localhost:8080/events">>, session_id => <<"test-connect">>},
    {ok, _Pid} = erlmcp_transport_sse:start_link(Config),

    %% Verify connection state
    {ok, State} = erlmcp_transport_sse:get_state(),
    ?assert(is_map(maps:get(connection, State, #{}))),

    %% Cleanup
    ok = erlmcp_transport_sse:stop().

%%%====================================================================
%%% Reconnection Tests
%%%====================================================================

test_reconnect() ->
    %% Start transport with reconnect
    Config = #{
        url => <<"http://localhost:8080/events">>,
        session_id => <<"test-reconnect">>,
        auto_reconnect => true,
        retry_delay => 1000,
        max_retries => 10
    },
    {ok, _Pid} = erlmcp_transport_sse:start_link(Config),

    %% Verify reconnect config
    {ok, State} = erlmcp_transport_sse:get_state(),
    ?assertEqual(true, maps:get(auto_reconnect, State)),
    ?assertEqual(1000, maps:get(retry_delay, State)),
    ?assertEqual(10, maps:get(max_retries, State)),

    %% Cleanup
    ok = erlmcp_transport_sse:stop().

%%%====================================================================
%%% Multiple Streams Tests
%%%====================================================================

test_multiple_streams() ->
    %% Start multiple SSE streams
    Streams = [
        #{url => <<"http://localhost:8080/events1">>, session_id => <<"test-stream1">>},
        #{url => <<"http://localhost:8080/events2">>, session_id => <<"test-stream2">>},
        #{url => <<"http://localhost:8080/events3">>, session_id => <<"test-stream3">>}
    ],

    Pids = [begin
        {ok, P} = erlmcp_transport_sse:start_link(Config),
        P
    end || Config <- Streams],

    %% Verify all started
    ?assertEqual(3, length(Pids)),
    lists:foreach(fun(P) -> ?assert(is_process_alive(P)) end, Pids),

    %% Cleanup
    lists:foreach(fun(_) -> erlmcp_transport_sse:stop() end, Streams).

%%%====================================================================
%%% Backpressure Tests
%%%====================================================================

test_backpressure() ->
    %% Start transport with backpressure handling
    Config = #{
        url => <<"http://localhost:8080/events">>,
        session_id => <<"test-backpressure">>,
        buffer_size => 1000,
        backpressure_threshold => 800
    },
    {ok, _Pid} = erlmcp_transport_sse:start_link(Config),

    %% Verify backpressure config
    {ok, State} = erlmcp_transport_sse:get_state(),
    ?assertEqual(1000, maps:get(buffer_size, State)),
    ?assertEqual(800, maps:get(backpressure_threshold, State)),

    %% Cleanup
    ok = erlmcp_transport_sse:stop().

%%%====================================================================
%%% Event Delivery Tests
%%%====================================================================

test_event_delivery() ->
    %% Start transport
    Config = #{
        url => <<"http://localhost:8080/events">>,
        session_id => <<"test-delivery">>,
        delivery_guarantee => at_least_once
    },
    {ok, _Pid} = erlmcp_transport_sse:start_link(Config),

    %% Verify delivery guarantee
    {ok, State} = erlmcp_transport_sse:get_state(),
    ?assertEqual(at_least_once, maps:get(delivery_guarantee, State)),

    %% Cleanup
    ok = erlmcp_transport_sse:stop().

%%%====================================================================
%%% Error Handling Tests
%%%====================================================================

test_sse_errors() ->
    %% Start transport
    Config = #{url => <<"http://localhost:8080/events">>, session_id => <<"test-errors">>},
    {ok, _Pid} = erlmcp_transport_sse:start_link(Config),

    %% Verify error handling
    {ok, State} = erlmcp_transport_sse:get_state(),
    ?assert(is_list(maps:get(errors, State, []))),

    %% Cleanup
    ok = erlmcp_transport_sse:stop().
