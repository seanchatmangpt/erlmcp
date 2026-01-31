%%%-------------------------------------------------------------------
%%% @doc EUnit Test Suite for erlmcp_sse_event_store Replay
%%%
%%% Chicago School TDD approach:
%%% - Real gen_server (no mocks)
%%% - Test observable behavior through API calls only
%%% - NO state inspection (sys:get_status prohibited)
%%% - NO record duplication (respect encapsulation)
%%%
%%% Tests cover:
%%% - Event replay after disconnect
%%% - Multiple disconnect/reconnect cycles
%%% - Replay with gaps in event numbers
%%% - Last-Event-ID functionality
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_sse_event_store_replay_tests).
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
    end, Tables).

%%%===================================================================
%%% Event Replay Tests
%%%===================================================================

event_replay_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [
          ?_test(test_replay_from_disconnect()),
          ?_test(test_replay_after_multiple_disconnects()),
          ?_test(test_replay_with_gaps())
         ]
     end}.

test_replay_from_disconnect() ->
    SessionId = <<"session_replay_disconnect">>,

    %% Client receives events 1-5 through API
    ok = add_events(SessionId, [
        {1, <<"e1">>},
        {2, <<"e2">>},
        {3, <<"e3">>},
        {4, <<"e4">>},
        {5, <<"e5">>}
    ]),

    %% Simulate client disconnects after event 3
    %% Server continues adding events
    ok = add_events(SessionId, [
        {6, <<"e6">>},
        {7, <<"e7">>},
        {8, <<"e8">>}
    ]),

    %% Client reconnects with Last-Event-ID = 3
    {ok, MissedEvents} = erlmcp_sse_event_store:get_events_since(
        SessionId,
        <<"session_replay_disconnect_3">>
    ),

    %% Should receive events 4, 5, 6, 7, 8 through API
    ?assertEqual(5, length(MissedEvents)),
    ?assertEqual(<<"e4">>, lists:nth(1, MissedEvents)),
    ?assertEqual(<<"e8">>, lists:nth(5, MissedEvents)).

test_replay_after_multiple_disconnects() ->
    SessionId = <<"session_replay_multiple">>,

    %% First batch
    ok = add_events(SessionId, [{1, <<"e1">>}, {2, <<"e2">>}]),

    %% Client disconnects, reconnects from 2
    {ok, Batch2} = erlmcp_sse_event_store:get_events_since(
        SessionId,
        <<"session_replay_multiple_2">>
    ),

    %% Add more events
    ok = add_events(SessionId, [{3, <<"e3">>}, {4, <<"e4">>}]),

    %% Client disconnects again, reconnects from 4
    {ok, Batch3} = erlmcp_sse_event_store:get_events_since(
        SessionId,
        <<"session_replay_multiple_4">>
    ),

    %% Verify first replay through API
    ?assertEqual([], Batch2),

    %% Verify second replay through API
    ?assertEqual([], Batch3).

test_replay_with_gaps() ->
    SessionId = <<"session_replay_gaps">>,

    %% Add events with gaps (1, 5, 10) through API
    ok = add_events(SessionId, [
        {1, <<"e1">>},
        {5, <<"e5">>},
        {10, <<"e10">>}
    ]),

    %% Replay from event 1
    {ok, Events} = erlmcp_sse_event_store:get_events_since(
        SessionId,
        <<"session_replay_gaps_1">>
    ),

    %% Should return events 5 and 10 through API
    ?assertEqual(2, length(Events)),
    ?assertEqual(<<"e5">>, lists:nth(1, Events)),
    ?assertEqual(<<"e10">>, lists:nth(2, Events)).

%%%===================================================================
%%% Last-Event-ID Tests
%%%===================================================================

last_event_id_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [
          ?_test(test_replay_from_beginning()),
          ?_test(test_replay_from_middle()),
          ?_test(test_replay_from_end()),
          ?_test(test_replay_beyond_end())
         ]
     end}.

test_replay_from_beginning() ->
    SessionId = <<"session_last_id_begin">>,

    %% Add events through API
    ok = add_events(SessionId, [
        {1, <<"first">>},
        {2, <<"second">>},
        {3, <<"third">>}
    ]),

    %% Replay from beginning (undefined Last-Event-ID)
    {ok, Events} = erlmcp_sse_event_store:get_events_since(SessionId, undefined),

    %% Should return all events through API
    ?assertEqual(3, length(Events)),
    ?assertEqual(<<"first">>, lists:nth(1, Events)),
    ?assertEqual(<<"third">>, lists:nth(3, Events)).

test_replay_from_middle() ->
    SessionId = <<"session_last_id_middle">>,

    %% Add events through API
    ok = add_events(SessionId, [
        {1, <<"e1">>},
        {2, <<"e2">>},
        {3, <<"e3">>},
        {4, <<"e4">>},
        {5, <<"e5">>}
    ]),

    %% Replay from event 3
    {ok, Events} = erlmcp_sse_event_store:get_events_since(
        SessionId,
        <<"session_last_id_middle_3">>
    ),

    %% Should return events 4 and 5 through API
    ?assertEqual(2, length(Events)),
    ?assertEqual(<<"e4">>, lists:nth(1, Events)),
    ?assertEqual(<<"e5">>, lists:nth(2, Events)).

test_replay_from_end() ->
    SessionId = <<"session_last_id_end">>,

    %% Add events through API
    ok = add_events(SessionId, [
        {1, <<"e1">>},
        {2, <<"e2">>},
        {3, <<"e3">>}
    ]),

    %% Replay from last event
    {ok, Events} = erlmcp_sse_event_store:get_events_since(
        SessionId,
        <<"session_last_id_end_3">>
    ),

    %% Should return empty list through API
    ?assertEqual([], Events).

test_replay_beyond_end() ->
    SessionId = <<"session_last_id_beyond">>,

    %% Add events through API
    ok = add_events(SessionId, [{1, <<"e1">>}]),

    %% Replay from non-existent event
    {ok, Events} = erlmcp_sse_event_store:get_events_since(
        SessionId,
        <<"session_last_id_beyond_999">>
    ),

    %% Should return empty list through API
    ?assertEqual([], Events).

%%%===================================================================
%%% Replay Scenarios Tests
%%%===================================================================

replay_scenarios_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [
          ?_test(test_client_reconnect_pattern()),
          ?_test(test_missed_events_recovery()),
          ?_test(test_incremental_replay())
         ]
     end}.

test_client_reconnect_pattern() ->
    SessionId = <<"session_reconnect_pattern">>,

    %% Simulate client connection pattern through API
    %% 1. Initial connection - receive events 1-3
    ok = add_events(SessionId, [
        {1, <<"initial1">>},
        {2, <<"initial2">>},
        {3, <<"initial3">>}
    ]),

    %% 2. Client disconnects, server adds events 4-6
    ok = add_events(SessionId, [
        {4, <<"missed1">>},
        {5, <<"missed2">>},
        {6, <<"missed3">>}
    ]),

    %% 3. Client reconnects with Last-Event-ID = 3
    {ok, MissedEvents} = erlmcp_sse_event_store:get_events_since(
        SessionId,
        <<"session_reconnect_pattern_3">>
    ),

    %% Should receive missed events 4-6 through API
    ?assertEqual(3, length(MissedEvents)),
    ?assertEqual(<<"missed1">>, lists:nth(1, MissedEvents)),
    ?assertEqual(<<"missed3">>, lists:nth(3, MissedEvents)).

test_missed_events_recovery() ->
    SessionId = <<"session_missed_recovery">>,

    %% Client receives events 1-10 through API
    ok = add_events(SessionId, lists:map(fun(N) -> {N, <<"e", (integer_to_binary(N))/binary>>} end, lists:seq(1, 10))),

    %% Client disconnects at event 10
    %% Server continues: events 11-20
    ok = add_events(SessionId, lists:map(fun(N) -> {N, <<"e", (integer_to_binary(N))/binary>>} end, lists:seq(11, 20))),

    %% Client reconnects with Last-Event-ID = 10
    {ok, RecoveredEvents} = erlmcp_sse_event_store:get_events_since(
        SessionId,
        <<"session_missed_recovery_10">>
    ),

    %% Should recover events 11-20 through API
    ?assertEqual(10, length(RecoveredEvents)),
    ?assertEqual(<<"e11">>, lists:nth(1, RecoveredEvents)),
    ?assertEqual(<<"e20">>, lists:nth(10, RecoveredEvents)).

test_incremental_replay() ->
    SessionId = <<"session_incremental_replay">>,

    %% Add large event set through API
    ok = add_events(SessionId, lists:map(fun(N) -> {N, <<"event_", (integer_to_binary(N))/binary>>} end, lists:seq(1, 100))),

    %% Client reconnects multiple times incrementally
    {ok, Batch1} = erlmcp_sse_event_store:get_events_since(
        SessionId,
        undefined
    ),
    ?assertEqual(100, length(Batch1)),

    %% Simulate client processes first 25, reconnects
    {ok, Batch2} = erlmcp_sse_event_store:get_events_since(
        SessionId,
        <<"session_incremental_replay_25">>
    ),
    ?assertEqual(75, length(Batch2)),

    %% Client processes next 25, reconnects again
    {ok, Batch3} = erlmcp_sse_event_store:get_events_since(
        SessionId,
        <<"session_incremental_replay_50">>
    ),
    ?assertEqual(50, length(Batch3)).

%%%===================================================================
%%% Edge Cases Tests
%%%===================================================================

edge_cases_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [
          ?_test(test_replay_empty_session()),
          ?_test(test_replay_single_event()),
          ?_test(test_replay_with_duplicates())
         ]
     end}.

test_replay_empty_session() ->
    SessionId = <<"session_replay_empty">>,

    %% Replay from empty session through API
    {ok, Events} = erlmcp_sse_event_store:get_events_since(SessionId, undefined),

    %% Should return empty list through API
    ?assertEqual([], Events).

test_replay_single_event() ->
    SessionId = <<"session_replay_single">>,

    %% Add single event through API
    ok = add_events(SessionId, [{1, <<"only_event">>}]),

    %% Replay from beginning
    {ok, Events} = erlmcp_sse_event_store:get_events_since(SessionId, undefined),

    %% Should return single event through API
    ?assertEqual(1, length(Events)),
    ?assertEqual(<<"only_event">>, lists:nth(1, Events)).

test_replay_with_duplicates() ->
    SessionId = <<"session_replay_dups">>,

    %% Add same event number twice (last write wins through API)
    {ok, _Id1} = erlmcp_sse_event_store:add_event(SessionId, 1, <<"first">>),
    {ok, _Id2} = erlmcp_sse_event_store:add_event(SessionId, 1, <<"second">>),

    %% Replay should return latest value
    {ok, Events} = erlmcp_sse_event_store:get_events_since(SessionId, undefined),

    %% Should return single event with latest value through API
    ?assertEqual(1, length(Events)),
    ?assertEqual(<<"second">>, lists:nth(1, Events)).

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
