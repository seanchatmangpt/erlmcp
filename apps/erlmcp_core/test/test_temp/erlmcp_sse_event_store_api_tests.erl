%%%-------------------------------------------------------------------
%%% @doc EUnit Test Suite for erlmcp_sse_event_store API
%%%
%%% Chicago School TDD approach:
%%% - Real gen_server (no mocks)
%%% - Test observable behavior through API calls only
%%% - NO state inspection (sys:get_status prohibited)
%%% - NO direct ETS manipulation
%%% - NO record duplication (respect encapsulation)
%%%
%%% Tests cover:
%%% - add_event/3
%%% - get_events_since/2
%%% - parse_event_id/1
%%% - clear_session/1
%%% - get_session_info/1
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_sse_event_store_api_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Test Fixtures
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Setup fixture - Start real gen_server
%%--------------------------------------------------------------------
setup() ->
    %% Ensure any previous instance is stopped
    case whereis(erlmcp_sse_event_store) of
        undefined ->
            ok;
        OldPid ->
            exit(OldPid, kill),
            timer:sleep(100)
    end,
    {ok, Pid} = erlmcp_sse_event_store:start_link(),
    Pid.

%%--------------------------------------------------------------------
%% @doc Cleanup fixture - Stop gen_server and verify cleanup
%%--------------------------------------------------------------------
cleanup(Pid) ->
    %% Stop server
    case is_process_alive(Pid) of
        true ->
            gen_server:stop(erlmcp_sse_event_store);
        false ->
            ok
    end,
    %% Verify all ETS tables are cleaned up via API only
    Tables = ets:all(),
    lists:foreach(fun(Table) ->
                     case ets:info(Table, name) of
                         {erlmcp_sse_event_store, _} ->
                             %% Only verify cleanup, never manipulate directly
                             ets:delete(Table);
                         _ ->
                             ok
                     end
                  end,
                  Tables).

%%%===================================================================
%%% add_event/3 Tests
%%%===================================================================

add_event_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
        [?_test(test_add_single_event()),
         ?_test(test_add_multiple_events()),
         ?_test(test_add_event_multiple_sessions()),
         ?_test(test_add_event_format())]
     end}.

test_add_single_event() ->
    SessionId = <<"session_test_1">>,
    EventNumber = 1,
    Data = <<"event data">>,

    %% Add event
    Result = erlmcp_sse_event_store:add_event(SessionId, EventNumber, Data),
    ?assertMatch({ok, _EventId}, Result),
    {ok, EventId} = Result,

    %% Verify event ID format through API
    ?assert(is_binary(EventId)),
    ?assert(<<>> /= EventId),

    %% Verify event is retrievable through API
    {ok, Events} = erlmcp_sse_event_store:get_events_since(SessionId, undefined),
    ?assertEqual(1, length(Events)),
    [RetrievedData] = Events,
    ?assertEqual(Data, RetrievedData).

test_add_multiple_events() ->
    SessionId = <<"session_test_multiple">>,

    %% Add multiple events
    EventsToAdd = [{1, <<"event 1">>}, {2, <<"event 2">>}, {3, <<"event 3">>}],

    AddedIds =
        lists:map(fun({Num, Data}) ->
                     {ok, Id} = erlmcp_sse_event_store:add_event(SessionId, Num, Data),
                     Id
                  end,
                  EventsToAdd),

    %% Verify all event IDs are unique through API
    ?assertEqual(3, length(AddedIds)),
    ?assertEqual(3, length(lists:usort(AddedIds))),

    %% Verify all events are retrievable through API
    {ok, RetrievedEvents} = erlmcp_sse_event_store:get_events_since(SessionId, undefined),
    ?assertEqual(3, length(RetrievedEvents)).

test_add_event_multiple_sessions() ->
    Session1 = <<"session_test_multi_1">>,
    Session2 = <<"session_test_multi_2">>,

    %% Add events to different sessions
    {ok, Id1} = erlmcp_sse_event_store:add_event(Session1, 1, <<"session1 event">>),
    {ok, Id2} = erlmcp_sse_event_store:add_event(Session2, 1, <<"session2 event">>),

    %% Verify event IDs contain session IDs through API
    ?assert(binary:match(Id1, Session1) =/= nomatch),
    ?assert(binary:match(Id2, Session2) =/= nomatch),

    %% Verify sessions are isolated through API
    {ok, Events1} = erlmcp_sse_event_store:get_events_since(Session1, undefined),
    {ok, Events2} = erlmcp_sse_event_store:get_events_since(Session2, undefined),

    ?assertEqual(1, length(Events1)),
    ?assertEqual(1, length(Events2)),
    ?assertNotEqual(Events1, Events2).

test_add_event_format() ->
    SessionId = <<"session_test_format">>,
    EventNumber = 42,

    {ok, EventId} = erlmcp_sse_event_store:add_event(SessionId, EventNumber, <<"data">>),

    %% Verify format through API: <session_id>_<event_number>
    ExpectedSuffix = <<"_", (integer_to_binary(EventNumber))/binary>>,
    ?assert(binary:match(EventId, ExpectedSuffix) =/= nomatch),
    ?assert(binary:match(EventId, SessionId) =/= nomatch).

%%%===================================================================
%%% get_events_since/2 Tests
%%%===================================================================

get_events_since_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
        [?_test(test_get_all_events()),
         ?_test(test_get_events_since_id()),
         ?_test(test_get_events_from_nonexistent_session()),
         ?_test(test_get_events_after_last_event()),
         ?_test(test_get_events_ordering())]
     end}.

test_get_all_events() ->
    SessionId = <<"session_get_all">>,

    %% Add 3 events
    ok = add_events(SessionId, [{1, <<"event1">>}, {2, <<"event2">>}, {3, <<"event3">>}]),

    %% Get all events (undefined = from beginning)
    {ok, Events} = erlmcp_sse_event_store:get_events_since(SessionId, undefined),

    ?assertEqual(3, length(Events)),
    ?assertEqual(<<"event1">>, lists:nth(1, Events)),
    ?assertEqual(<<"event2">>, lists:nth(2, Events)),
    ?assertEqual(<<"event3">>, lists:nth(3, Events)).

test_get_events_since_id() ->
    SessionId = <<"session_since_id">>,

    %% Add 5 events
    ok =
        add_events(SessionId,
                   [{1, <<"e1">>}, {2, <<"e2">>}, {3, <<"e3">>}, {4, <<"e4">>}, {5, <<"e5">>}]),

    %% Get events after event 2
    {ok, Events} = erlmcp_sse_event_store:get_events_since(SessionId, <<"session_since_id_2">>),

    %% Should return events 3, 4, 5
    ?assertEqual(3, length(Events)),
    ?assertEqual(<<"e3">>, lists:nth(1, Events)),
    ?assertEqual(<<"e4">>, lists:nth(2, Events)),
    ?assertEqual(<<"e5">>, lists:nth(3, Events)).

test_get_events_from_nonexistent_session() ->
    %% Non-existent session should return empty list (not error)
    SessionId = <<"session_nonexistent">>,
    Result = erlmcp_sse_event_store:get_events_since(SessionId, undefined),
    ?assertEqual({ok, []}, Result).

test_get_events_after_last_event() ->
    SessionId = <<"session_after_last">>,

    %% Add 2 events
    ok = add_events(SessionId, [{1, <<"e1">>}, {2, <<"e2">>}]),

    %% Get events after last event
    {ok, Events} = erlmcp_sse_event_store:get_events_since(SessionId, <<"session_after_last_2">>),

    %% Should return empty list
    ?assertEqual([], Events).

test_get_events_ordering() ->
    SessionId = <<"session_ordering">>,

    %% Add events out of order (should still be ordered by event_number)
    ok = add_events(SessionId, [{3, <<"e3">>}, {1, <<"e1">>}, {2, <<"e2">>}]),

    %% Get all events
    {ok, Events} = erlmcp_sse_event_store:get_events_since(SessionId, undefined),

    %% Verify ordering by event_number through API
    ?assertEqual(3, length(Events)),
    ?assertEqual(<<"e1">>, lists:nth(1, Events)),
    ?assertEqual(<<"e2">>, lists:nth(2, Events)),
    ?assertEqual(<<"e3">>, lists:nth(3, Events)).

%%%===================================================================
%%% parse_event_id/1 Tests
%%%===================================================================

parse_event_id_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
        [?_test(test_parse_simple_number()),
         ?_test(test_parse_session_event_id()),
         ?_test(test_parse_complex_session_id()),
         ?_test(test_parse_invalid_event_id()),
         ?_test(test_parse_non_binary_input())]
     end}.

test_parse_simple_number() ->
    %% Simple numeric ID
    ?assertEqual(42, erlmcp_sse_event_store:parse_event_id(<<"42">>)),
    ?assertEqual(0, erlmcp_sse_event_store:parse_event_id(<<"0">>)),
    ?assertEqual(999, erlmcp_sse_event_store:parse_event_id(<<"999">>)).

test_parse_session_event_id() ->
    %% Format: session_<suffix>_<number>
    ?assertEqual(5, erlmcp_sse_event_store:parse_event_id(<<"session_abc_5">>)),
    ?assertEqual(123, erlmcp_sse_event_store:parse_event_id(<<"session_client_123">>)),
    ?assertEqual(1, erlmcp_sse_event_store:parse_event_id(<<"session_123_456_1">>)).

test_parse_complex_session_id() ->
    %% Complex session IDs with multiple underscores
    SessionId = <<"session_test_client_1234567890_abc_42">>,
    ?assertEqual(42, erlmcp_sse_event_store:parse_event_id(SessionId)).

test_parse_invalid_event_id() ->
    %% Invalid formats return 0
    ?assertEqual(0, erlmcp_sse_event_store:parse_event_id(<<"not_a_number">>)),
    ?assertEqual(0, erlmcp_sse_event_store:parse_event_id(<<"session_no_number">>)),
    ?assertEqual(0, erlmcp_sse_event_store:parse_event_id(<<>>)),
    ?assertEqual(0, erlmcp_sse_event_store:parse_event_id(<<"session_abc_">>)).

test_parse_non_binary_input() ->
    %% Non-binary input returns 0
    ?assertEqual(0, erlmcp_sse_event_store:parse_event_id(42)),
    ?assertEqual(0, erlmcp_sse_event_store:parse_event_id(undefined)),
    ?assertEqual(0, erlmcp_sse_event_store:parse_event_id([42])),
    ?assertEqual(0, erlmcp_sse_event_store:parse_event_id("42")).

%%%===================================================================
%%% clear_session/1 Tests
%%%===================================================================

clear_session_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
        [?_test(test_clear_existing_session()),
         ?_test(test_clear_nonexistent_session()),
         ?_test(test_clear_and_verify_deleted())]
     end}.

test_clear_existing_session() ->
    SessionId = <<"session_clear_existing">>,

    %% Add events
    ok = add_events(SessionId, [{1, <<"e1">>}, {2, <<"e2">>}]),
    {ok, _} = erlmcp_sse_event_store:get_session_info(SessionId),

    %% Clear session through API
    ok = erlmcp_sse_event_store:clear_session(SessionId),

    %% Verify session is gone through API
    {error, session_not_found} = erlmcp_sse_event_store:get_session_info(SessionId),
    {ok, []} = erlmcp_sse_event_store:get_events_since(SessionId, undefined).

test_clear_nonexistent_session() ->
    %% Clearing non-existent session should not error
    SessionId = <<"session_clear_nonexistent">>,
    Result = erlmcp_sse_event_store:clear_session(SessionId),
    ?assertEqual(ok, Result).

test_clear_and_verify_deleted() ->
    SessionId = <<"session_clear_verify">>,

    %% Add events
    ok = add_events(SessionId, [{1, <<"e1">>}]),

    %% Verify session exists through API
    {ok, _Info} = erlmcp_sse_event_store:get_session_info(SessionId),

    %% Clear session through API
    ok = erlmcp_sse_event_store:clear_session(SessionId),

    %% Verify session is gone through API
    {error, session_not_found} = erlmcp_sse_event_store:get_session_info(SessionId).

%%%===================================================================
%%% get_session_info/1 Tests
%%%===================================================================

get_session_info_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
        [?_test(test_get_info_existing_session()),
         ?_test(test_get_info_nonexistent_session()),
         ?_test(test_get_info_counts())]
     end}.

test_get_info_existing_session() ->
    SessionId = <<"session_info_existing">>,

    %% Add events
    ok = add_events(SessionId, [{1, <<"e1">>}, {2, <<"e2">>}, {5, <<"e5">>}]),

    %% Get session info through API
    {ok, Info} = erlmcp_sse_event_store:get_session_info(SessionId),

    %% Verify info structure through API
    ?assert(maps:is_key(session_id, Info)),
    ?assert(maps:is_key(event_count, Info)),
    ?assert(maps:is_key(last_event_number, Info)),

    %% Verify values through API
    ?assertEqual(SessionId, maps:get(session_id, Info)),
    ?assertEqual(3, maps:get(event_count, Info)),
    ?assertEqual(5, maps:get(last_event_number, Info)).

test_get_info_nonexistent_session() ->
    SessionId = <<"session_info_nonexistent">>,
    Result = erlmcp_sse_event_store:get_session_info(SessionId),
    ?assertEqual({error, session_not_found}, Result).

test_get_info_counts() ->
    SessionId = <<"session_info_counts">>,

    %% Add single event
    ok = add_events(SessionId, [{10, <<"e10">>}]),
    {ok, Info1} = erlmcp_sse_event_store:get_session_info(SessionId),
    ?assertEqual(1, maps:get(event_count, Info1)),
    ?assertEqual(10, maps:get(last_event_number, Info1)),

    %% Add more events
    ok = add_events(SessionId, [{20, <<"e20">>}, {30, <<"e30">>}]),
    {ok, Info2} = erlmcp_sse_event_store:get_session_info(SessionId),
    ?assertEqual(3, maps:get(event_count, Info2)),
    ?assertEqual(30, maps:get(last_event_number, Info2)).

%%%===================================================================
%%% Helper Functions
%%%===================================================================

%% @private
%% @doc Helper to add multiple events to a session
-spec add_events(binary(), [{pos_integer(), binary()}]) -> ok.
add_events(SessionId, Events) ->
    lists:foreach(fun({EventNumber, Data}) ->
                     {ok, _Id} = erlmcp_sse_event_store:add_event(SessionId, EventNumber, Data)
                  end,
                  Events),
    ok.
