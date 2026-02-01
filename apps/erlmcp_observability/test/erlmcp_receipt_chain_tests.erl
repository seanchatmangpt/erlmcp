%%%-------------------------------------------------------------------
%%% @doc Unit tests for erlmcp_receipt_chain following Chicago School TDD
%%%
%%% Chicago School TDD Principles:
%%% - Test observable behavior through API calls
%%% - Use REAL ETS tables (no mocks)
%%% - Verify state through public API only
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_receipt_chain_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Setup
%%====================================================================

receipt_chain_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
         fun test_add_event/0,
         fun test_get_event_by_id/0,
         fun test_get_events_by_type/0,
         fun test_get_all_events/0,
         fun test_restore_state/0
     ]}.

setup() ->
    % Each test gets a clean start
    ok.

cleanup(_) ->
    % Clean up ETS table if it exists
    case ets:info(erlmcp_receipt_chain_table) of
        undefined -> ok;
        _ -> ets:delete(erlmcp_receipt_chain_table)
    end,
    ok.

%%====================================================================
%% Test Cases
%%====================================================================

test_add_event() ->
    Event = #{
        type => auth_success,
        user_id => <<"user123">>,
        timestamp => erlang:system_time(millisecond)
    },

    ok = erlmcp_receipt_chain:add_event(Event),

    % Verify event was added
    AllEvents = erlmcp_receipt_chain:get_all_events(),
    ?assertEqual(1, length(AllEvents)).

test_get_event_by_id() ->
    Event1 = #{type => login, user => <<"alice">>},
    ok = erlmcp_receipt_chain:add_event(Event1),

    timer:sleep(1),  % Ensure different IDs

    Event2 = #{type => logout, user => <<"bob">>},
    ok = erlmcp_receipt_chain:add_event(Event2),

    % Get all events to find IDs
    AllEvents = erlmcp_receipt_chain:get_all_events(),
    ?assertEqual(2, length(AllEvents)),

    % Verify each event can be retrieved by ID
    [E1, E2] = AllEvents,
    EventId1 = maps:get(id, E1),
    EventId2 = maps:get(id, E2),

    {ok, Retrieved1} = erlmcp_receipt_chain:get_event_by_id(EventId1),
    ?assertEqual(maps:get(type, E1), maps:get(type, Retrieved1)),

    {ok, Retrieved2} = erlmcp_receipt_chain:get_event_by_id(EventId2),
    ?assertEqual(maps:get(type, E2), maps:get(type, Retrieved2)).

test_get_events_by_type() ->
    % Add events of different types
    ok = erlmcp_receipt_chain:add_event(#{type => auth_success, user => <<"user1">>}),
    timer:sleep(1),
    ok = erlmcp_receipt_chain:add_event(#{type => auth_failure, user => <<"user2">>}),
    timer:sleep(1),
    ok = erlmcp_receipt_chain:add_event(#{type => auth_success, user => <<"user3">>}),

    % Get events by type
    {ok, AuthSuccesses} = erlmcp_receipt_chain:get_events_by_type(auth_success),
    ?assertEqual(2, length(AuthSuccesses)),

    {ok, AuthFailures} = erlmcp_receipt_chain:get_events_by_type(auth_failure),
    ?assertEqual(1, length(AuthFailures)),

    % Non-existent type returns empty list
    {ok, NoEvents} = erlmcp_receipt_chain:get_events_by_type(nonexistent),
    ?assertEqual(0, length(NoEvents)).

test_get_all_events() ->
    % Empty initially
    ?assertEqual([], erlmcp_receipt_chain:get_all_events()),

    % Add events
    lists:foreach(fun(N) ->
        ok = erlmcp_receipt_chain:add_event(#{
            type => test_event,
            sequence => N
        }),
        timer:sleep(1)
    end, lists:seq(1, 5)),

    % Get all events
    AllEvents = erlmcp_receipt_chain:get_all_events(),
    ?assertEqual(5, length(AllEvents)).

test_restore_state() ->
    % Create initial state
    ok = erlmcp_receipt_chain:add_event(#{type => event1}),
    timer:sleep(1),
    ok = erlmcp_receipt_chain:add_event(#{type => event2}),

    InitialEvents = erlmcp_receipt_chain:get_all_events(),
    ?assertEqual(2, length(InitialEvents)),

    % Simulate state snapshot
    StateSnapshot = maps:from_list([
        {maps:get(id, E), E} || E <- InitialEvents
    ]),

    % Clear and restore
    cleanup(ok),
    setup(),

    ok = erlmcp_receipt_chain:restore_state(StateSnapshot),

    % Verify restored state
    RestoredEvents = erlmcp_receipt_chain:get_all_events(),
    ?assertEqual(2, length(RestoredEvents)).

%%====================================================================
%% Edge Cases
%%====================================================================

nonexistent_event_test() ->
    ?assertEqual({error, not_found}, erlmcp_receipt_chain:get_event_by_id(99999999999)).

high_volume_test() ->
    % Add many events
    lists:foreach(fun(N) ->
        erlmcp_receipt_chain:add_event(#{type => bulk_test, n => N})
    end, lists:seq(1, 100)),

    AllEvents = erlmcp_receipt_chain:get_all_events(),
    ?assertEqual(100, length(AllEvents)).

concurrent_add_test() ->
    % Spawn multiple processes adding events
    Pids = [spawn(fun() ->
        lists:foreach(fun(N) ->
            erlmcp_receipt_chain:add_event(#{
                type => concurrent_test,
                worker => self(),
                n => N
            })
        end, lists:seq(1, 10))
    end) || _ <- lists:seq(1, 5)],

    % Wait for all
    lists:foreach(fun(Pid) ->
        Ref = monitor(process, Pid),
        receive
            {'DOWN', Ref, process, Pid, _} -> ok
        after 5000 -> timeout
        end
    end, Pids),

    AllEvents = erlmcp_receipt_chain:get_all_events(),
    ?assertEqual(50, length(AllEvents)).
