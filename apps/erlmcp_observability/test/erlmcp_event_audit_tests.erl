%%%-------------------------------------------------------------------
%%% @doc Unit tests for erlmcp_event_audit following Chicago School TDD
%%%
%%% Chicago School TDD Principles:
%%% - Test observable behavior through gen_event API
%%% - Use real event manager (no mocks)
%%% - Verify state through handler calls
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_event_audit_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Setup
%%====================================================================

event_audit_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
         fun test_init/0,
         fun test_handle_tool_executed/0,
         fun test_handle_resource_updated/0,
         fun test_handle_connection_state/0,
         fun test_handle_error/0,
         fun test_handle_request_received/0,
         fun test_handle_session_created/0,
         fun test_get_stats/0,
         fun test_set_enabled/0,
         fun test_set_log_all/0
     ]}.

setup() ->
    % Start event manager
    {ok, Manager} = gen_event:start_link(),
    Manager.

cleanup(Manager) ->
    gen_event:stop(Manager).

%%====================================================================
%% Test Cases
%%====================================================================

test_init() ->
    {ok, State} = erlmcp_event_audit:init(#{}),
    ?assertMatch({state, _, _, _, _}, State).

test_handle_tool_executed() ->
    {ok, State} = erlmcp_event_audit:init(#{enabled => true}),

    Event = {tool_executed, <<"echo_tool">>, 15, {ok, <<"result">>}},
    {ok, NewState} = erlmcp_event_audit:handle_event(Event, State),

    % Verify event count increased
    ?assert(element(4, NewState) =:= element(4, State) + 1).

test_handle_resource_updated() ->
    {ok, State} = erlmcp_event_audit:init(#{enabled => true}),

    Event = {resource_updated, <<"resource://test">>, #{version => 1}},
    {ok, NewState} = erlmcp_event_audit:handle_event(Event, State),

    ?assert(element(4, NewState) =:= element(4, State) + 1).

test_handle_connection_state() ->
    {ok, State} = erlmcp_event_audit:init(#{enabled => true}),

    Event = {connection_state, connected, #{transport => stdio}},
    {ok, NewState} = erlmcp_event_audit:handle_event(Event, State),

    ?assert(element(4, NewState) =:= element(4, State) + 1).

test_handle_error() ->
    {ok, State} = erlmcp_event_audit:init(#{enabled => true}),

    Event = {error, protocol_error, <<"Invalid JSON">>},
    {ok, NewState} = erlmcp_event_audit:handle_event(Event, State),

    ?assert(element(4, NewState) =:= element(4, State) + 1).

test_handle_request_received() ->
    {ok, State} = erlmcp_event_audit:init(#{enabled => true, log_all_events => true}),

    Event = {request_received, <<"tools/call">>, <<"req-001">>},
    {ok, NewState} = erlmcp_event_audit:handle_event(Event, State),

    % Should increment counter when log_all_events is true
    ?assert(element(4, NewState) =:= element(4, State) + 1).

test_handle_session_created() ->
    {ok, State} = erlmcp_event_audit:init(#{enabled => true}),

    Event = {session_created, <<"session-123">>, #{user => <<"alice">>}},
    {ok, NewState} = erlmcp_event_audit:handle_event(Event, State),

    ?assert(element(4, NewState) =:= element(4, State) + 1).

test_get_stats() ->
    {ok, State} = erlmcp_event_audit:init(#{enabled => true}),

    {ok, Stats, _NewState} = erlmcp_event_audit:handle_call(get_stats, State),

    ?assert(is_map(Stats)),
    ?assert(maps:is_key(enabled, Stats)),
    ?assert(maps:is_key(event_count, Stats)),
    ?assert(maps:is_key(uptime_ms, Stats)).

test_set_enabled() ->
    {ok, State} = erlmcp_event_audit:init(#{enabled => true}),

    {ok, ok, NewState} = erlmcp_event_audit:handle_call({set_enabled, false}, State),

    % Verify behavior changed: events should not be logged when disabled
    Event = {tool_executed, <<"tool">>, 10, ok},
    {ok, StateAfterEvent} = erlmcp_event_audit:handle_event(Event, NewState),

    % Event count should not increase (observable behavior)
    ?assertEqual(element(4, NewState), element(4, StateAfterEvent)).

test_set_log_all() ->
    {ok, State} = erlmcp_event_audit:init(#{log_all_events => false, enabled => true}),

    {ok, ok, NewState} = erlmcp_event_audit:handle_call({set_log_all, true}, State),

    % Verify behavior changed: request_received events should now be logged
    Event = {request_received, <<"method">>, <<"id">>},
    {ok, StateAfterEvent} = erlmcp_event_audit:handle_event(Event, NewState),

    % Event count should increase when log_all is true (observable behavior)
    ?assertEqual(element(4, NewState) + 1, element(4, StateAfterEvent)).

%%====================================================================
%% Edge Cases
%%====================================================================

disabled_handler_test() ->
    {ok, State} = erlmcp_event_audit:init(#{enabled => false}),

    Event = {tool_executed, <<"tool">>, 10, ok},
    {ok, NewState} = erlmcp_event_audit:handle_event(Event, State),

    % Event count should not increase when disabled
    ?assertEqual(element(4, State), element(4, NewState)).

unknown_event_test() ->
    {ok, State} = erlmcp_event_audit:init(#{enabled => true}),

    UnknownEvent = {unknown_event_type, some_data},
    {ok, NewState} = erlmcp_event_audit:handle_event(UnknownEvent, State),

    % Should not crash, event count unchanged
    ?assertEqual(element(4, State), element(4, NewState)).

request_received_without_log_all_test() ->
    {ok, State} = erlmcp_event_audit:init(#{enabled => true, log_all_events => false}),

    Event = {request_received, <<"method">>, <<"id">>},
    {ok, NewState} = erlmcp_event_audit:handle_event(Event, State),

    % Should not increment when log_all_events is false
    ?assertEqual(element(4, State), element(4, NewState)).

multiple_events_test() ->
    {ok, State0} = erlmcp_event_audit:init(#{enabled => true}),

    Events = [
        {tool_executed, <<"tool1">>, 10, ok},
        {resource_updated, <<"res">>, #{}},
        {error, test_error, <<>>},
        {connection_state, connected, #{}},
        {session_created, <<"sess">>, #{}}
    ],

    FinalState = lists:foldl(fun(Event, StateAcc) ->
        {ok, NewState} = erlmcp_event_audit:handle_event(Event, StateAcc),
        NewState
    end, State0, Events),

    % Should have processed 5 events
    ?assertEqual(5, element(4, FinalState)).
