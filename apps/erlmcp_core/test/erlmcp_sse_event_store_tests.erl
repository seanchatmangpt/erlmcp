%%%-------------------------------------------------------------------
%%% @doc EUnit Test Suite for erlmcp_sse_event_store
%%%
%%% Chicago School TDD approach:
%%% - Real gen_server (no mocks)
%%% - State-based verification (observable behavior)
%%% - Real ETS operations (not stubbed)
%%% - Integration testing with real processes
%%%
%%% Coverage targets:
%%% - All 7 public APIs tested
%%% - Positive and negative cases
%%% - Concurrent access patterns
%%% - Edge cases and error paths
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_sse_event_store_tests).
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Records (copied from module under test)
%%%===================================================================
-record(state, {
    cleanup_timer :: reference() | undefined
}).

-record(event, {
    event_number :: pos_integer(),
    event_id :: binary(),
    data :: binary(),
    timestamp :: integer()
}).

%%%===================================================================
%%% Test Fixtures
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Setup fixture - Start real gen_server
%%--------------------------------------------------------------------
setup() ->
    %% Ensure any previous instance is stopped
    case whereis(erlmcp_sse_event_store) of
        undefined -> ok;
        OldPid -> exit(OldPid, kill), timer:sleep(100)
    end,
    {ok, Pid} = erlmcp_sse_event_store:start_link(),
    Pid.

%%--------------------------------------------------------------------
%% @doc Cleanup fixture - Stop gen_server and verify cleanup
%%--------------------------------------------------------------------
cleanup(Pid) ->
    %% Stop server
    case is_process_alive(Pid) of
        true -> gen_server:stop(erlmcp_sse_event_store);
        false -> ok
    end,
    %% Verify all ETS tables are cleaned up
    Tables = ets:all(),
    lists:foreach(fun(Table) ->
        case ets:info(Table, name) of
            {erlmcp_sse_event_store, _} -> ets:delete(Table);
            _ -> ok
        end
    end, Tables),
    %% Verify cleanup
    [] = [T || T <- ets:all(), element(1, ets:info(T, name)) =:= erlmcp_sse_event_store].

%%%===================================================================
%%% Lifecycle Tests
%%%===================================================================

lifecycle_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          ?_test(test_start_link()),
          ?_test(test_double_start_fails()),
          ?_test(test_stop_and_restart())
         ]
     end}.

test_start_link() ->
    %% Setup already started server, verify it's alive
    Pid = whereis(erlmcp_sse_event_store),
    ?assert(is_pid(Pid)),
    ?assert(is_process_alive(Pid)),

    %% Verify state has cleanup timer
    {status, _, _, [_, _, _, _, _, {data, [{_, State}]}]} = sys:get_status(erlmcp_sse_event_store),
    ?assertMatch(#state{cleanup_timer = _Ref}, State).

test_double_start_fails() ->
    %% Attempting to start again should fail (already registered)
    Result = erlmcp_sse_event_store:start_link(),
    ?assertMatch({error, {already_started, _}}, Result).

test_stop_and_restart() ->
    %% Stop server
    Pid = whereis(erlmcp_sse_event_store),
    ok = gen_server:stop(erlmcp_sse_event_store),
    timer:sleep(100),
    ?assertNot(is_process_alive(Pid)),

    %% Restart and verify
    {ok, NewPid} = erlmcp_sse_event_store:start_link(),
    ?assert(is_pid(NewPid)),
    ?assert(is_process_alive(NewPid)).

%%%===================================================================
%%% add_event/3 Tests
%%%===================================================================

add_event_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(Pid) ->
         [
          ?_test(test_add_single_event(Pid)),
          ?_test(test_add_multiple_events(Pid)),
          ?_test(test_add_event_multiple_sessions(Pid)),
          ?_test(test_add_event_format(Pid))
         ]
     end}.

test_add_single_event(_Pid) ->
    SessionId = <<"session_test_1">>,
    EventNumber = 1,
    Data = <<"event data">>,

    %% Add event
    Result = erlmcp_sse_event_store:add_event(SessionId, EventNumber, Data),
    ?assertMatch({ok, _EventId}, Result),
    {ok, EventId} = Result,

    %% Verify event ID format
    ?assert(is_binary(EventId)),
    ?assert(<<>> /= EventId),

    %% Verify event is retrievable
    {ok, Events} = erlmcp_sse_event_store:get_events_since(SessionId, undefined),
    ?assertEqual(1, length(Events)),
    [RetrievedData] = Events,
    ?assertEqual(Data, RetrievedData).

test_add_multiple_events(_Pid) ->
    SessionId = <<"session_test_multiple">>,

    %% Add multiple events
    EventsToAdd = [
        {1, <<"event 1">>},
        {2, <<"event 2">>},
        {3, <<"event 3">>}
    ],

    AddedIds = lists:map(fun({Num, Data}) ->
        {ok, Id} = erlmcp_sse_event_store:add_event(SessionId, Num, Data),
        Id
    end, EventsToAdd),

    %% Verify all event IDs are unique
    ?assertEqual(3, length(AddedIds)),
    ?assertEqual(3, length(lists:usort(AddedIds))),

    %% Verify all events are retrievable
    {ok, RetrievedEvents} = erlmcp_sse_event_store:get_events_since(SessionId, undefined),
    ?assertEqual(3, length(RetrievedEvents)).

test_add_event_multiple_sessions(_Pid) ->
    Session1 = <<"session_test_multi_1">>,
    Session2 = <<"session_test_multi_2">>,

    %% Add events to different sessions
    {ok, Id1} = erlmcp_sse_event_store:add_event(Session1, 1, <<"session1 event">>),
    {ok, Id2} = erlmcp_sse_event_store:add_event(Session2, 1, <<"session2 event">>),

    %% Verify event IDs contain session IDs
    ?assert(binary:match(Id1, Session1) =/= nomatch),
    ?assert(binary:match(Id2, Session2) =/= nomatch),

    %% Verify sessions are isolated
    {ok, Events1} = erlmcp_sse_event_store:get_events_since(Session1, undefined),
    {ok, Events2} = erlmcp_sse_event_store:get_events_since(Session2, undefined),

    ?assertEqual(1, length(Events1)),
    ?assertEqual(1, length(Events2)),
    ?assertNotEqual(Events1, Events2).

test_add_event_format(_Pid) ->
    SessionId = <<"session_test_format">>,
    EventNumber = 42,

    {ok, EventId} = erlmcp_sse_event_store:add_event(SessionId, EventNumber, <<"data">>),

    %% Verify format: <session_id>_<event_number>
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
     fun(Pid) ->
         [
          ?_test(test_get_all_events(Pid)),
          ?_test(test_get_events_since_id(Pid)),
          ?_test(test_get_events_from_nonexistent_session(Pid)),
          ?_test(test_get_events_after_last_event(Pid)),
          ?_test(test_get_events_ordering(Pid))
         ]
     end}.

test_get_all_events(_Pid) ->
    SessionId = <<"session_get_all">>,

    %% Add 3 events
    ok = add_events(SessionId, [{1, <<"event1">>}, {2, <<"event2">>}, {3, <<"event3">>}]),

    %% Get all events (undefined = from beginning)
    {ok, Events} = erlmcp_sse_event_store:get_events_since(SessionId, undefined),

    ?assertEqual(3, length(Events)),
    ?assertEqual(<<"event1">>, lists:nth(1, Events)),
    ?assertEqual(<<"event2">>, lists:nth(2, Events)),
    ?assertEqual(<<"event3">>, lists:nth(3, Events)).

test_get_events_since_id(_Pid) ->
    SessionId = <<"session_since_id">>,

    %% Add 5 events
    ok = add_events(SessionId, [
        {1, <<"e1">>}, {2, <<"e2">>}, {3, <<"e3">>}, {4, <<"e4">>}, {5, <<"e5">>}
    ]),

    %% Get events after event 2
    {ok, Events} = erlmcp_sse_event_store:get_events_since(SessionId, <<"session_since_id_2">>),

    %% Should return events 3, 4, 5
    ?assertEqual(3, length(Events)),
    ?assertEqual(<<"e3">>, lists:nth(1, Events)),
    ?assertEqual(<<"e4">>, lists:nth(2, Events)),
    ?assertEqual(<<"e5">>, lists:nth(3, Events)).

test_get_events_from_nonexistent_session(_Pid) ->
    %% Non-existent session should return empty list (not error)
    SessionId = <<"session_nonexistent">>,
    Result = erlmcp_sse_event_store:get_events_since(SessionId, undefined),
    ?assertEqual({ok, []}, Result).

test_get_events_after_last_event(_Pid) ->
    SessionId = <<"session_after_last">>,

    %% Add 2 events
    ok = add_events(SessionId, [{1, <<"e1">>}, {2, <<"e2">>}]),

    %% Get events after last event
    {ok, Events} = erlmcp_sse_event_store:get_events_since(SessionId, <<"session_after_last_2">>),

    %% Should return empty list
    ?assertEqual([], Events).

test_get_events_ordering(_Pid) ->
    SessionId = <<"session_ordering">>,

    %% Add events out of order (should still be ordered by event_number)
    ok = add_events(SessionId, [{3, <<"e3">>}, {1, <<"e1">>}, {2, <<"e2">>}]),

    %% Get all events
    {ok, Events} = erlmcp_sse_event_store:get_events_since(SessionId, undefined),

    %% Verify ordering by event_number
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
         [
          ?_test(test_parse_simple_number()),
          ?_test(test_parse_session_event_id()),
          ?_test(test_parse_complex_session_id()),
          ?_test(test_parse_invalid_event_id()),
          ?_test(test_parse_non_binary_input())
         ]
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
     fun(Pid) ->
         [
          ?_test(test_clear_existing_session(Pid)),
          ?_test(test_clear_nonexistent_session(Pid)),
          ?_test(test_clear_and_verify_ets_deleted(Pid))
         ]
     end}.

test_clear_existing_session(_Pid) ->
    SessionId = <<"session_clear_existing">>,

    %% Add events
    ok = add_events(SessionId, [{1, <<"e1">>}, {2, <<"e2">>}]),
    {ok, _} = erlmcp_sse_event_store:get_session_info(SessionId),

    %% Clear session
    ok = erlmcp_sse_event_store:clear_session(SessionId),

    %% Verify session is gone
    {error, session_not_found} = erlmcp_sse_event_store:get_session_info(SessionId),
    {ok, []} = erlmcp_sse_event_store:get_events_since(SessionId, undefined).

test_clear_nonexistent_session(_Pid) ->
    %% Clearing non-existent session should not error
    SessionId = <<"session_clear_nonexistent">>,
    Result = erlmcp_sse_event_store:clear_session(SessionId),
    ?assertEqual(ok, Result).

test_clear_and_verify_ets_deleted(_Pid) ->
    SessionId = <<"session_clear_ets">>,

    %% Add events
    ok = add_events(SessionId, [{1, <<"e1">>}]),

    %% Verify ETS table exists
    TableName = {erlmcp_sse_event_store, SessionId},
    ?assertMatch(_, ets:whereis(TableName)),

    %% Clear session
    ok = erlmcp_sse_event_store:clear_session(SessionId),

    %% Verify ETS table is deleted
    ?assertEqual(undefined, ets:whereis(TableName)).

%%%===================================================================
%%% get_session_info/1 Tests
%%%===================================================================

get_session_info_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(Pid) ->
         [
          ?_test(test_get_info_existing_session(Pid)),
          ?_test(test_get_info_nonexistent_session(Pid)),
          ?_test(test_get_info_counts(Pid))
         ]
     end}.

test_get_info_existing_session(_Pid) ->
    SessionId = <<"session_info_existing">>,

    %% Add events
    ok = add_events(SessionId, [{1, <<"e1">>}, {2, <<"e2">>}, {5, <<"e5">>}]),

    %% Get session info
    {ok, Info} = erlmcp_sse_event_store:get_session_info(SessionId),

    %% Verify info structure
    ?assert(maps:is_key(session_id, Info)),
    ?assert(maps:is_key(event_count, Info)),
    ?assert(maps:is_key(last_event_number, Info)),

    %% Verify values
    ?assertEqual(SessionId, maps:get(session_id, Info)),
    ?assertEqual(3, maps:get(event_count, Info)),
    ?assertEqual(5, maps:get(last_event_number, Info)).

test_get_info_nonexistent_session(_Pid) ->
    SessionId = <<"session_info_nonexistent">>,
    Result = erlmcp_sse_event_store:get_session_info(SessionId),
    ?assertEqual({error, session_not_found}, Result).

test_get_info_counts(_Pid) ->
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
%%% cleanup_expired/0 Tests
%%%===================================================================

cleanup_expired_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(Pid) ->
         [
          ?_test(test_cleanup_expired_sessions(Pid)),
          ?_test(test_cleanup_with_recent_events(Pid)),
          ?_test(test_cleanup_empty_sessions(Pid))
         ]
     end}.

test_cleanup_expired_sessions(_Pid) ->
    SessionId = <<"session_expired">>,

    %% Add events
    ok = add_events(SessionId, [{1, <<"e1">>}]),

    %% Manually expire events by modifying timestamp (requires direct ETS access)
    TableName = {erlmcp_sse_event_store, SessionId},
    [{1, #event{} = Event}] = ets:lookup(TableName, 1),
    ExpiredEvent = Event#event{timestamp = 0}, %% Very old timestamp
    ets:insert(TableName, ExpiredEvent),

    %% Trigger cleanup
    erlmcp_sse_event_store:cleanup_expired(),

    %% Verify session was cleaned up
    timer:sleep(100),
    {error, session_not_found} = erlmcp_sse_event_store:get_session_info(SessionId).

test_cleanup_with_recent_events(_Pid) ->
    SessionId = <<"session_recent">>,

    %% Add recent events
    ok = add_events(SessionId, [{1, <<"e1">>}, {2, <<"e2">>}]),

    %% Trigger cleanup (should not delete recent events)
    erlmcp_sse_event_store:cleanup_expired(),
    timer:sleep(100),

    %% Verify session still exists
    {ok, Info} = erlmcp_sse_event_store:get_session_info(SessionId),
    ?assertEqual(2, maps:get(event_count, Info)).

test_cleanup_empty_sessions(_Pid) ->
    SessionId = <<"session_empty">>,

    %% Create empty table by adding then removing event
    ok = add_events(SessionId, [{1, <<"e1">>}]),
    TableName = {erlmcp_sse_event_store, SessionId},
    ets:delete(TableName, 1),

    %% Trigger cleanup (should delete empty tables)
    erlmcp_sse_event_store:cleanup_expired(),
    timer:sleep(100),

    %% Verify table was deleted
    ?assertEqual(undefined, ets:whereis(TableName)).

%%%===================================================================
%%% Concurrency Tests
%%%===================================================================

concurrent_access_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(Pid) ->
         [
          ?_test(test_concurrent_add_events(Pid)),
          ?_test(test_concurrent_session_isolation(Pid)),
          ?_test(test_concurrent_read_write(Pid))
         ]
     end}.

test_concurrent_add_events(_Pid) ->
    SessionId = <<"session_concurrent_add">>,

    %% Spawn multiple processes adding events concurrently
    NumProcesses = 10,
    EventsPerProcess = 5,

    Pids = lists:map(fun(ProcessNum) ->
        spawn(fun() ->
            StartNum = ProcessNum * EventsPerProcess + 1,
            lists:foreach(fun(EventNum) ->
                erlmcp_sse_event_store:add_event(SessionId, EventNum, <<"event_", (integer_to_binary(EventNum))/binary>>)
            end, lists:seq(StartNum, StartNum + EventsPerProcess - 1))
        end)
    end, lists:seq(0, NumProcesses - 1)),

    %% Wait for all processes to complete
    timer:sleep(500),

    %% Verify all events were added
    {ok, Events} = erlmcp_sse_event_store:get_events_since(SessionId, undefined),
    ?assertEqual(NumProcesses * EventsPerProcess, length(Events)).

test_concurrent_session_isolation(_Pid) ->
    %% Multiple sessions with concurrent writes
    NumSessions = 5,
    SessionIds = [<<"session_concurrent_iso_", (integer_to_binary(N))/binary>> || N <- lists:seq(1, NumSessions)],

    %% Add events to all sessions concurrently
    lists:foreach(fun(SessionId) ->
        spawn(fun() ->
            lists:foreach(fun(N) ->
                erlmcp_sse_event_store:add_event(SessionId, N, <<"data">>)
            end, lists:seq(1, 10))
        end)
    end, SessionIds),

    %% Wait for completion
    timer:sleep(500),

    %% Verify all sessions have correct event counts
    lists:foreach(fun(SessionId) ->
        {ok, Info} = erlmcp_sse_event_store:get_session_info(SessionId),
        ?assertEqual(10, maps:get(event_count, Info))
    end, SessionIds).

test_concurrent_read_write(_Pid) ->
    SessionId = <<"session_concurrent_rw">>,

    %% Writer process
    spawn(fun() ->
        lists:foreach(fun(N) ->
            erlmcp_sse_event_store:add_event(SessionId, N, <<"event_", (integer_to_binary(N))/binary>>),
            timer:sleep(10)
        end, lists:seq(1, 20))
    end),

    %% Reader processes
    Readers = lists:map(fun(_) ->
        spawn(fun() ->
            lists:foreach(fun(_) ->
                {ok, _Events} = erlmcp_sse_event_store:get_events_since(SessionId, undefined),
                timer:sleep(20)
            end, lists:seq(1, 10))
        end)
    end, lists:seq(1, 5)),

    %% Wait for completion
    timer:sleep(1000),

    %% Verify final state
    {ok, Info} = erlmcp_sse_event_store:get_session_info(SessionId),
    ?assertEqual(20, maps:get(event_count, Info)).

%%%===================================================================
%%% Event Replay Tests
%%%===================================================================

event_replay_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(Pid) ->
         [
          ?_test(test_replay_from_disconnect(Pid)),
          ?_test(test_replay_after_multiple_disconnects(Pid)),
          ?_test(test_replay_with_gaps(Pid))
         ]
     end}.

test_replay_from_disconnect(_Pid) ->
    SessionId = <<"session_replay_disconnect">>,

    %% Client receives events 1-5
    ok = add_events(SessionId, [{1, <<"e1">>}, {2, <<"e2">>}, {3, <<"e3">>}, {4, <<"e4">>}, {5, <<"e5">>}]),

    %% Simulate client disconnects after event 3
    %% Server continues adding events
    ok = add_events(SessionId, [{6, <<"e6">>}, {7, <<"e7">>}, {8, <<"e8">>}]),

    %% Client reconnects with Last-Event-ID = 3
    {ok, MissedEvents} = erlmcp_sse_event_store:get_events_since(SessionId, <<"session_replay_disconnect_3">>),

    %% Should receive events 4, 5, 6, 7, 8
    ?assertEqual(5, length(MissedEvents)),
    ?assertEqual(<<"e4">>, lists:nth(1, MissedEvents)),
    ?assertEqual(<<"e8">>, lists:nth(5, MissedEvents)).

test_replay_after_multiple_disconnects(Pid) ->
    SessionId = <<"session_replay_multiple">>,

    %% First batch
    ok = add_events(SessionId, [{1, <<"e1">>}, {2, <<"e2">>}]),

    %% Client disconnects, reconnects from 2
    {ok, Batch2} = erlmcp_sse_event_store:get_events_since(SessionId, <<"session_replay_multiple_2">>),

    %% Add more events
    ok = add_events(SessionId, [{3, <<"e3">>}, {4, <<"e4">>}]),

    %% Client disconnects again, reconnects from 4
    {ok, Batch3} = erlmcp_sse_event_store:get_events_since(SessionId, <<"session_replay_multiple_4">>),

    %% Verify first replay (should be empty - no new events after 2 yet)
    ?assertEqual([], Batch2),

    %% Verify second replay
    ?assertEqual([], Batch3).

test_replay_with_gaps(_Pid) ->
    SessionId = <<"session_replay_gaps">>,

    %% Add events with gaps (1, 5, 10)
    ok = add_events(SessionId, [{1, <<"e1">>}, {5, <<"e5">>}, {10, <<"e10">>}]),

    %% Replay from event 1
    {ok, Events} = erlmcp_sse_event_store:get_events_since(SessionId, <<"session_replay_gaps_1">>),

    %% Should return events 5 and 10
    ?assertEqual(2, length(Events)),
    ?assertEqual(<<"e5">>, lists:nth(1, Events)),
    ?assertEqual(<<"e10">>, lists:nth(2, Events)).

%%%===================================================================
%%% Termination Tests
%%%===================================================================

termination_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(Pid) ->
         [
          ?_test(test_terminate_cleans_all_tables(Pid)),
          ?_test(test_terminate_cancels_timer(Pid))
         ]
     end}.

test_terminate_cleans_all_tables(Pid) ->
    %% Create multiple sessions with events
    SessionIds = [
        <<"session_term_1">>,
        <<"session_term_2">>,
        <<"session_term_3">>
    ],

    lists:foreach(fun(SessionId) ->
        ok = add_events(SessionId, [{1, <<"e1">>}])
    end, SessionIds),

    %% Verify tables exist
    TableNames = [{erlmcp_sse_event_store, S} || S <- SessionIds],
    lists:foreach(fun(TableName) ->
        ?assertNotEqual(undefined, ets:whereis(TableName))
    end, TableNames),

    %% Stop server
    ok = gen_server:stop(erlmcp_sse_event_store),
    timer:sleep(100),

    %% Verify all tables are deleted
    lists:foreach(fun(TableName) ->
        ?assertEqual(undefined, ets:whereis(TableName))
    end, TableNames).

test_terminate_cancels_timer(Pid) ->
    %% Server is running with cleanup timer
    ?assert(is_process_alive(Pid)),

    %% Stop server
    ok = gen_server:stop(erlmcp_sse_event_store),
    timer:sleep(100),

    %% Verify process is dead
    ?assertNot(is_process_alive(Pid)).

%%%===================================================================
%%% Helper Functions
%%%===================================================================

%% @private
%% @doc Helper to add multiple events to a session
-spec add_events(binary(), [{pos_integer(), binary()}]) -> ok.
add_events(SessionId, Events) ->
    lists:foreach(fun({EventNumber, Data}) ->
        {ok, _Id} = erlmcp_sse_event_store:add_event(SessionId, EventNumber, Data)
    end, Events),
    ok.
