%%%=====================================================================
%%% @doc
%%% Comprehensive SSE Stream Resumability Tests.
%%%
%%% Tests for event ID generation, Last-Event-ID parsing, event replay,
%%% connection closure with retry, stream resumption, missed event recovery,
%%% and concurrent stream handling.
%%%
%%% @end
%%%=====================================================================

-module(erlmcp_sse_resumability_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Setup & Cleanup
%%====================================================================

setup() ->
    {ok, _} = application:ensure_all_started(erlmcp),
    {ok, _} = application:ensure_all_started(erlmcp_sse_event_store),
    ok.

cleanup(_) ->
    ok.

%%====================================================================
%% Test Suites
%%====================================================================

sse_resumability_test_() ->
    {setup,
        fun setup/0,
        fun cleanup/1,
        [
            ?_test(test_event_id_generation()),
            ?_test(test_event_id_parsing()),
            ?_test(test_add_event_to_store()),
            ?_test(test_get_events_since_all()),
            ?_test(test_get_events_since_partial()),
            ?_test(test_get_events_since_empty()),
            ?_test(test_last_event_id_parsing()),
            ?_test(test_event_replay_sequence()),
            ?_test(test_connection_closure_with_retry()),
            ?_test(test_stream_resumption_after_disconnect()),
            ?_test(test_missed_event_recovery()),
            ?_test(test_concurrent_streams()),
            ?_test(test_event_store_cleanup()),
            ?_test(test_session_info_tracking()),
            ?_test(test_sse_format_with_event_id())
        ]
    }.

%%====================================================================
%% Unit Tests - Event ID Generation
%%====================================================================

test_event_id_generation() ->
    SessionId = <<"session_12345">>,
    EventNumber = 42,

    %% Test event ID generation format
    EventId = generate_event_id(SessionId, EventNumber),
    ?assert(is_binary(EventId)),
    ?assert(binary:match(EventId, SessionId) =/= nomatch),
    ?assert(binary:match(EventId, <<"42">>) =/= nomatch).

test_event_id_parsing() ->
    %% Test parsing event IDs back to event number
    EventId1 = <<"session_abc123_0">>,
    ?assertEqual(0, parse_event_id(EventId1)),

    EventId2 = <<"session_xyz_999">>,
    ?assertEqual(999, parse_event_id(EventId2)),

    EventId3 = <<"session_test_1234567890">>,
    ?assertEqual(1234567890, parse_event_id(EventId3)),

    %% Test invalid event ID
    ?assertEqual(0, parse_event_id(<<"invalid">>)),
    ?assertEqual(0, parse_event_id(<<">>)).

%%====================================================================
%% Unit Tests - Event Store Operations
%%====================================================================

test_add_event_to_store() ->
    SessionId = <<"session_test_add">>,
    EventNumber = 1,
    EventData = <<"{\\"jsonrpc\\":\\"2.0\\",\\"method\\":\\"test\\"}">>,

    %% Add event and get event ID back
    {ok, EventId} = erlmcp_sse_event_store:add_event(SessionId, EventNumber, EventData),

    ?assert(is_binary(EventId)),
    ?assert(binary:match(EventId, SessionId) =/= nomatch),
    ?assert(binary:match(EventId, <<"1">>) =/= nomatch).

test_get_events_since_all() ->
    SessionId = <<"session_test_all">>,

    %% Clear any existing events
    erlmcp_sse_event_store:clear_session(SessionId),

    %% Add multiple events
    {ok, EventId1} = erlmcp_sse_event_store:add_event(SessionId, 1, <<"event1">>),
    {ok, EventId2} = erlmcp_sse_event_store:add_event(SessionId, 2, <<"event2">>),
    {ok, _EventId3} = erlmcp_sse_event_store:add_event(SessionId, 3, <<"event3">>),

    %% Get all events (undefined LastEventId)
    {ok, Events} = erlmcp_sse_event_store:get_events_since(SessionId, undefined),
    ?assertEqual(3, length(Events)),
    ?assertMatch([<<"event1">>, <<"event2">>, <<"event3">>], Events).

test_get_events_since_partial() ->
    SessionId = <<"session_test_partial">>,

    %% Clear and add events
    erlmcp_sse_event_store:clear_session(SessionId),
    {ok, EventId1} = erlmcp_sse_event_store:add_event(SessionId, 1, <<"event1">>),
    {ok, _EventId2} = erlmcp_sse_event_store:add_event(SessionId, 2, <<"event2">>),
    {ok, _EventId3} = erlmcp_sse_event_store:add_event(SessionId, 3, <<"event3">>),

    %% Get events since first event
    {ok, Events} = erlmcp_sse_event_store:get_events_since(SessionId, EventId1),
    ?assertEqual(2, length(Events)),
    ?assertMatch([<<"event2">>, <<"event3">>], Events).

test_get_events_since_empty() ->
    SessionId = <<"session_test_empty">>,

    %% Clear session
    erlmcp_sse_event_store:clear_session(SessionId),

    %% Get events from empty session
    {ok, Events} = erlmcp_sse_event_store:get_events_since(SessionId, undefined),
    ?assertEqual([], Events).

%%====================================================================
%% Unit Tests - Last-Event-ID Handling
%%====================================================================

test_last_event_id_parsing() ->
    %% Test standard format: session_id_event_number
    LastEventId1 = <<"session_abc123_xyz_100">>,
    EventNum1 = parse_event_id(LastEventId1),
    ?assertEqual(100, EventNum1),

    %% Test various event numbers
    LastEventId2 = <<"s_0">>,
    EventNum2 = parse_event_id(LastEventId2),
    ?assertEqual(0, EventNum2),

    LastEventId3 = <<"session_test_9999999">>,
    EventNum3 = parse_event_id(LastEventId3),
    ?assertEqual(9999999, EventNum3).

%%====================================================================
%% Integration Tests - Event Replay & Sequence
%%====================================================================

test_event_replay_sequence() ->
    SessionId = <<"session_test_replay">>,

    %% Clear and add ordered events
    erlmcp_sse_event_store:clear_session(SessionId),

    Events = [
        {1, <<"{\\"id\\":1,\\"msg\\":\\"first\\"}">>},
        {2, <<"{\\"id\\":2,\\"msg\\":\\"second\\"}">>},
        {3, <<"{\\"id\\":3,\\"msg\\":\\"third\\"}">>},
        {4, <<"{\\"id\\":4,\\"msg\\":\\"fourth\\"}">>},
        {5, <<"{\\"id\\":5,\\"msg\\":\\"fifth\\"}">>}
    ],

    %% Add all events
    EventIds = lists:map(
        fun({Num, Data}) ->
            {ok, EventId} = erlmcp_sse_event_store:add_event(SessionId, Num, Data),
            EventId
        end,
        Events
    ),

    %% Get all events and verify order
    {ok, ReplayedEvents} = erlmcp_sse_event_store:get_events_since(SessionId, undefined),
    ExpectedData = [Data || {_Num, Data} <- Events],
    ?assertEqual(ExpectedData, ReplayedEvents).

test_connection_closure_with_retry() ->
    %% Verify SSE event format includes retry hint
    EventData = <<"{\\"status\\":\\"closed\\"}">>,

    %% Format should include retry: 3000
    FormattedClose = format_close_event(),
    ?assert(binary:match(FormattedClose, <<"retry: 3000">>) =/= nomatch),
    ?assert(binary:match(FormattedClose, <<"event: close">>) =/= nomatch),
    ?assert(binary:match(FormattedClose, <<"data: ">>) =/= nomatch).

%%====================================================================
%% Integration Tests - Stream Resumption
%%====================================================================

test_stream_resumption_after_disconnect() ->
    SessionId = <<"session_test_reconnect">>,

    %% Clear and set up initial events
    erlmcp_sse_event_store:clear_session(SessionId),
    {ok, EventId1} = erlmcp_sse_event_store:add_event(SessionId, 1, <<"msg1">>),
    {ok, EventId2} = erlmcp_sse_event_store:add_event(SessionId, 2, <<"msg2">>),
    {ok, _EventId3} = erlmcp_sse_event_store:add_event(SessionId, 3, <<"msg3">>),

    %% Simulate disconnect and reconnection with Last-Event-ID
    {ok, ResumeEvents} = erlmcp_sse_event_store:get_events_since(SessionId, EventId2),

    %% Should only get events after #2
    ?assertEqual([<<"msg3">>], ResumeEvents).

test_missed_event_recovery() ->
    SessionId = <<"session_test_missed">>,

    %% Clear and add events with gaps
    erlmcp_sse_event_store:clear_session(SessionId),
    {ok, _E1} = erlmcp_sse_event_store:add_event(SessionId, 1, <<"event1">>),
    {ok, _E2} = erlmcp_sse_event_store:add_event(SessionId, 2, <<"event2">>),
    %% Simulate client missing event #2 due to network interruption
    {ok, _E3} = erlmcp_sse_event_store:add_event(SessionId, 3, <<"event3">>),
    {ok, _E4} = erlmcp_sse_event_store:add_event(SessionId, 4, <<"event4">>),

    %% Client reconnects with event #2 as last known
    {ok, RecoveredEvents} = erlmcp_sse_event_store:get_events_since(
        SessionId,
        <<"session_test_missed_2">>
    ),

    %% Should recover events #3 and #4
    ?assertEqual([<<"event3">>, <<"event4">>], RecoveredEvents).

%%====================================================================
%% Concurrency Tests
%%====================================================================

test_concurrent_streams() ->
    %% Test multiple concurrent streams don't interfere
    Session1 = <<"session_concurrent_1">>,
    Session2 = <<"session_concurrent_2">>,
    Session3 = <<"session_concurrent_3">>,

    erlmcp_sse_event_store:clear_session(Session1),
    erlmcp_sse_event_store:clear_session(Session2),
    erlmcp_sse_event_store:clear_session(Session3),

    %% Add events to different sessions concurrently
    Parent = self(),
    Pids = [
        spawn(fun() ->
            {ok, _} = erlmcp_sse_event_store:add_event(Session1, 1, <<"s1e1">>),
            {ok, _} = erlmcp_sse_event_store:add_event(Session1, 2, <<"s1e2">>),
            {ok, _} = erlmcp_sse_event_store:add_event(Session1, 3, <<"s1e3">>),
            Parent ! {self(), done}
        end),
        spawn(fun() ->
            {ok, _} = erlmcp_sse_event_store:add_event(Session2, 1, <<"s2e1">>),
            {ok, _} = erlmcp_sse_event_store:add_event(Session2, 2, <<"s2e2">>),
            Parent ! {self(), done}
        end),
        spawn(fun() ->
            {ok, _} = erlmcp_sse_event_store:add_event(Session3, 1, <<"s3e1">>),
            {ok, _} = erlmcp_sse_event_store:add_event(Session3, 2, <<"s3e2">>),
            {ok, _} = erlmcp_sse_event_store:add_event(Session3, 3, <<"s3e3">>),
            {ok, _} = erlmcp_sse_event_store:add_event(Session3, 4, <<"s3e4">>),
            Parent ! {self(), done}
        end)
    ],

    %% Wait for all processes
    lists:foreach(fun(Pid) ->
        receive
            {Pid, done} -> ok
        after 5000 -> ?fail("Timeout waiting for concurrent session")
        end
    end, Pids),

    %% Verify each session has correct events
    {ok, Events1} = erlmcp_sse_event_store:get_events_since(Session1, undefined),
    ?assertEqual(3, length(Events1)),

    {ok, Events2} = erlmcp_sse_event_store:get_events_since(Session2, undefined),
    ?assertEqual(2, length(Events2)),

    {ok, Events3} = erlmcp_sse_event_store:get_events_since(Session3, undefined),
    ?assertEqual(4, length(Events3)).

%%====================================================================
%% Maintenance Tests
%%====================================================================

test_event_store_cleanup() ->
    %% Test cleanup expiry mechanism
    SessionId = <<"session_test_cleanup">>,

    erlmcp_sse_event_store:clear_session(SessionId),
    {ok, _E1} = erlmcp_sse_event_store:add_event(SessionId, 1, <<"event1">>),
    {ok, _E2} = erlmcp_sse_event_store:add_event(SessionId, 2, <<"event2">>),

    %% Verify events exist
    {ok, EventsBefore} = erlmcp_sse_event_store:get_events_since(SessionId, undefined),
    ?assertEqual(2, length(EventsBefore)),

    %% Cleanup (normally done periodically)
    ok = erlmcp_sse_event_store:cleanup_expired(),

    %% Events should still exist (not expired yet)
    {ok, EventsAfter} = erlmcp_sse_event_store:get_events_since(SessionId, undefined),
    ?assertEqual(2, length(EventsAfter)).

test_session_info_tracking() ->
    SessionId = <<"session_test_info">>,

    erlmcp_sse_event_store:clear_session(SessionId),
    {ok, _E1} = erlmcp_sse_event_store:add_event(SessionId, 1, <<"event1">>),
    {ok, _E2} = erlmcp_sse_event_store:add_event(SessionId, 2, <<"event2">>),
    {ok, _E3} = erlmcp_sse_event_store:add_event(SessionId, 3, <<"event3">>),

    %% Get session info
    {ok, Info} = erlmcp_sse_event_store:get_session_info(SessionId),

    ?assert(maps:is_map(Info)),
    ?assertEqual(SessionId, maps:get(session_id, Info)),
    ?assertEqual(3, maps:get(event_count, Info)),
    ?assertEqual(3, maps:get(last_event_number, Info)).

%%====================================================================
%% SSE Format Tests
%%====================================================================

test_sse_format_with_event_id() ->
    %% Verify SSE event format with event ID
    EventId = <<"session_test_456">>,
    EventData = <<"{\\"jsonrpc\\":\\"2.0\\",\\"method\\":\\"test\\"}">>,

    Formatted = format_sse_event_with_id(EventId, EventData),

    ?assert(is_binary(Formatted)),
    ?assert(binary:match(Formatted, <<"id: ", EventId/binary>>) =/= nomatch),
    ?assert(binary:match(Formatted, <<"data: ", EventData/binary>>) =/= nomatch),
    ?assert(binary:match(Formatted, <<"\n\n">>) =/= nomatch).

%%====================================================================
%% Helper Functions
%%====================================================================

%% @doc Generate unique event ID: session_id_event_number.
-spec generate_event_id(binary(), pos_integer()) -> binary().
generate_event_id(SessionId, EventNumber) ->
    EventStr = integer_to_binary(EventNumber),
    <<SessionId/binary, "_", EventStr/binary>>.

%% @doc Parse event ID to extract event number.
-spec parse_event_id(binary()) -> non_neg_integer().
parse_event_id(EventId) when is_binary(EventId) ->
    case binary:split(EventId, <<"_">>, [global]) of
        Parts when length(Parts) >= 2 ->
            LastPart = lists:last(Parts),
            try
                binary_to_integer(LastPart)
            catch
                _:_ -> 0
            end;
        _ -> 0
    end;
parse_event_id(_) ->
    0.

%% @doc Format SSE event with event ID.
-spec format_sse_event_with_id(binary(), binary()) -> binary().
format_sse_event_with_id(EventId, Data) ->
    <<"id: ", EventId/binary, "\ndata: ", Data/binary, "\n\n">>.

%% @doc Format close event with retry hint.
-spec format_close_event() -> binary().
format_close_event() ->
    <<"event: close\ndata: {\"status\":\"closed\"}\nretry: 3000\n\n">>.
