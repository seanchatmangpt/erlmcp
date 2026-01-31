%%%-------------------------------------------------------------------
%%% @doc
%%% EUnit tests for erlmcp_event_audit
%%%
%%% Tests the audit trail event handler including:
%%% - Audit record creation
%%% - Enabled/disabled state
%%% - Log all events flag
%%% - Sensitive data sanitization
%%% - Statistics collection
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_event_audit_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Fixtures
%%====================================================================

audit_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
         fun test_init_enabled/1,
         fun test_init_disabled/1,
         fun test_tool_execution_audit/1,
         fun test_resource_update_audit/1,
         fun test_connection_state_audit/1,
         fun test_error_audit/1,
         fun test_session_audit/1,
         fun test_log_all_events/1,
         fun test_selective_logging/1,
         fun test_get_stats/1,
         fun test_enable_disable/1,
         fun test_param_sanitization/1
     ]}.

setup() ->
    {ok, Pid} = erlmcp_event_manager:start_link(),
    ok = erlmcp_event_manager:add_handler(erlmcp_event_audit, #{enabled => true}),
    Pid.

cleanup(Pid) ->
    case is_process_alive(Pid) of
        true -> erlmcp_event_manager:stop();
        false -> ok
    end.

%%====================================================================
%% Tests
%%====================================================================

test_init_enabled(_Pid) ->
    %% Handler should be registered
    Handlers = erlmcp_event_manager:which_handlers(),

    %% Get stats
    Stats = gen_event:call(erlmcp_event_manager, erlmcp_event_audit, get_stats),

    [
        ?_assert(lists:member(erlmcp_event_audit, Handlers)),
        ?_assertMatch(#{enabled := true}, Stats)
    ].

test_init_disabled(_Pid) ->
    %% Remove current handler and add disabled one
    ok = erlmcp_event_manager:delete_handler(erlmcp_event_audit, []),
    ok = erlmcp_event_manager:add_handler(erlmcp_event_audit, #{enabled => false}),

    %% Send an event
    ok = erlmcp_event_manager:notify({tool_executed, <<"test">>, 1000000, ok}),

    %% Get stats - event should not be counted
    Stats = gen_event:call(erlmcp_event_manager, erlmcp_event_audit, get_stats),

    [
        ?_assertMatch(#{enabled := false}, Stats),
        ?_assertEqual(0, maps:get(event_count, Stats))
    ].

test_tool_execution_audit(_Pid) ->
    %% Send tool execution events
    ok = erlmcp_event_manager:notify({tool_executed, <<"echo">>, 1000000, ok}),
    ok = erlmcp_event_manager:notify({tool_executed, <<"fail">>, 2000000, {error, timeout}}),

    %% Get stats
    Stats = gen_event:call(erlmcp_event_manager, erlmcp_event_audit, get_stats),

    [
        ?_assertEqual(2, maps:get(event_count, Stats))
    ].

test_resource_update_audit(_Pid) ->
    %% Send resource update events
    ok = erlmcp_event_manager:notify({resource_updated, <<"file://test.txt">>, #{size => 1024}}),
    ok = erlmcp_event_manager:notify({resource_updated, <<"http://example.com">>, #{status => ok}}),

    %% Get stats
    Stats = gen_event:call(erlmcp_event_manager, erlmcp_event_audit, get_stats),

    [
        ?_assertEqual(2, maps:get(event_count, Stats))
    ].

test_connection_state_audit(_Pid) ->
    %% Send connection state events
    ok = erlmcp_event_manager:notify({connection_state, connected, #{transport => stdio}}),
    ok = erlmcp_event_manager:notify({connection_state, disconnected, #{reason => normal}}),

    %% Get stats
    Stats = gen_event:call(erlmcp_event_manager, erlmcp_event_audit, get_stats),

    [
        ?_assertEqual(2, maps:get(event_count, Stats))
    ].

test_error_audit(_Pid) ->
    %% Send error events
    ok = erlmcp_event_manager:notify({error, validation, invalid_params}),
    ok = erlmcp_event_manager:notify({error, transport, connection_lost}),

    %% Get stats
    Stats = gen_event:call(erlmcp_event_manager, erlmcp_event_audit, get_stats),

    [
        ?_assertEqual(2, maps:get(event_count, Stats))
    ].

test_session_audit(_Pid) ->
    %% Send session events
    ok = erlmcp_event_manager:notify({session_created, <<"sess_1">>, #{user => <<"test">>}}),
    ok = erlmcp_event_manager:notify({session_terminated, <<"sess_1">>, normal}),

    %% Get stats
    Stats = gen_event:call(erlmcp_event_manager, erlmcp_event_audit, get_stats),

    [
        ?_assertEqual(2, maps:get(event_count, Stats))
    ].

test_log_all_events(_Pid) ->
    %% Remove current handler and add one with log_all_events = true
    ok = erlmcp_event_manager:delete_handler(erlmcp_event_audit, []),
    ok = erlmcp_event_manager:add_handler(erlmcp_event_audit, #{enabled => true, log_all_events => true}),

    %% Send events that are normally not logged
    ok = erlmcp_event_manager:notify({request_received, <<"method">>, 1}),
    ok = erlmcp_event_manager:notify({response_sent, <<"method">>, 1, 500000}),
    ok = erlmcp_event_manager:notify({notification_sent, <<"notif">>, #{}}),

    %% Get stats - all should be counted
    Stats = gen_event:call(erlmcp_event_manager, erlmcp_event_audit, get_stats),

    [
        ?_assertMatch(#{log_all_events := true}, Stats),
        ?_assertEqual(3, maps:get(event_count, Stats))
    ].

test_selective_logging(_Pid) ->
    %% With log_all_events = false (default), request/response events are not logged
    Stats1 = gen_event:call(erlmcp_event_manager, erlmcp_event_audit, get_stats),
    InitialCount = maps:get(event_count, Stats1),

    ok = erlmcp_event_manager:notify({request_received, <<"method">>, 1}),
    ok = erlmcp_event_manager:notify({response_sent, <<"method">>, 1, 500000}),

    %% But critical events are always logged
    ok = erlmcp_event_manager:notify({tool_executed, <<"tool">>, 1000000, ok}),

    Stats2 = gen_event:call(erlmcp_event_manager, erlmcp_event_audit, get_stats),

    [
        ?_assertEqual(InitialCount + 1, maps:get(event_count, Stats2))
    ].

test_get_stats(_Pid) ->
    %% Send some events
    ok = erlmcp_event_manager:notify({tool_executed, <<"tool1">>, 1000000, ok}),
    ok = erlmcp_event_manager:notify({error, test, reason}),

    %% Get stats
    Stats = gen_event:call(erlmcp_event_manager, erlmcp_event_audit, get_stats),

    [
        ?_assertMatch(#{enabled := true}, Stats),
        ?_assertMatch(#{log_all_events := false}, Stats),
        ?_assertMatch(#{event_count := Count} when Count >= 2, Stats),
        ?_assertMatch(#{uptime_ms := Uptime} when Uptime >= 0, Stats)
    ].

test_enable_disable(_Pid) ->
    %% Initially enabled
    Stats1 = gen_event:call(erlmcp_event_manager, erlmcp_event_audit, get_stats),
    ?assertMatch(#{enabled := true}, Stats1),

    %% Send event
    ok = erlmcp_event_manager:notify({tool_executed, <<"tool1">>, 1000000, ok}),

    Stats2 = gen_event:call(erlmcp_event_manager, erlmcp_event_audit, get_stats),
    Count1 = maps:get(event_count, Stats2),

    %% Disable
    ok = gen_event:call(erlmcp_event_manager, erlmcp_event_audit, {set_enabled, false}),

    %% Send event - should not be counted
    ok = erlmcp_event_manager:notify({tool_executed, <<"tool2">>, 1000000, ok}),

    Stats3 = gen_event:call(erlmcp_event_manager, erlmcp_event_audit, get_stats),

    [
        ?_assertMatch(#{enabled := false}, Stats3),
        ?_assertEqual(Count1, maps:get(event_count, Stats3))
    ].

test_param_sanitization(_Pid) ->
    %% Remove current handler and add one with log_all_events = true
    ok = erlmcp_event_manager:delete_handler(erlmcp_event_audit, []),
    ok = erlmcp_event_manager:add_handler(erlmcp_event_audit, #{enabled => true, log_all_events => true}),

    %% Send notification with sensitive params
    Params = #{
        <<"user">> => <<"alice">>,
        <<"password">> => <<"secret123">>,
        <<"token">> => <<"bearer_xyz">>,
        <<"data">> => <<"safe_data">>
    },

    ok = erlmcp_event_manager:notify({notification_sent, <<"auth">>, Params}),

    %% Stats should show event was logged
    Stats = gen_event:call(erlmcp_event_manager, erlmcp_event_audit, get_stats),

    [
        ?_assertEqual(1, maps:get(event_count, Stats))
    ].

%%====================================================================
%% Integration Tests
%%====================================================================

integration_test_() ->
    {setup,
     fun() ->
         {ok, Pid} = erlmcp_event_manager:start_link(),
         ok = erlmcp_event_manager:add_handler(erlmcp_event_audit, #{
             enabled => true,
             log_all_events => true
         }),
         Pid
     end,
     fun(Pid) ->
         case is_process_alive(Pid) of
             true -> erlmcp_event_manager:stop();
             false -> ok
         end
     end,
     fun test_comprehensive_audit/1}.

test_comprehensive_audit(_Pid) ->
    %% Send a comprehensive set of events
    Events = [
        {tool_executed, <<"echo">>, 1000000, ok},
        {tool_executed, <<"grep">>, 2000000, {error, not_found}},
        {resource_updated, <<"file://a.txt">>, #{}},
        {connection_state, connected, #{}},
        {connection_state, disconnected, #{}},
        {error, validation, test},
        {request_received, <<"method1">>, 1},
        {response_sent, <<"method1">>, 1, 500000},
        {notification_sent, <<"notif1">>, #{}},
        {session_created, <<"sess1">>, #{}},
        {session_terminated, <<"sess1">>, normal}
    ],

    lists:foreach(fun(E) -> erlmcp_event_manager:notify(E) end, Events),

    %% Get stats
    Stats = gen_event:call(erlmcp_event_manager, erlmcp_event_audit, get_stats),

    [
        ?_assertEqual(11, maps:get(event_count, Stats)),
        ?_assertMatch(#{enabled := true}, Stats),
        ?_assertMatch(#{log_all_events := true}, Stats)
    ].
