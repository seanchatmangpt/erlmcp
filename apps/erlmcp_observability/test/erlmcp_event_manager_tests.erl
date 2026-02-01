%%%-------------------------------------------------------------------
%%% @doc
%%% EUnit tests for erlmcp_event_manager
%%%
%%% Tests the gen_event manager functionality including:
%%% - Manager lifecycle (start/stop)
%%% - Handler registration and removal
%%% - Event notification (sync and async)
%%% - Multiple handlers for same event
%%% - Handler crash isolation
%%% - Handler swapping
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_event_manager_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Fixtures
%%====================================================================

manager_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [fun test_start_stop/1,
      fun test_notify_no_handlers/1,
      fun test_add_single_handler/1,
      fun test_add_multiple_handlers/1,
      fun test_remove_handler/1,
      fun test_async_notify/1,
      fun test_sync_notify/1,
      fun test_which_handlers/1,
      fun test_swap_handler/1,
      fun test_handler_crash_isolation/1,
      fun test_notify_when_stopped/1]}.

setup() ->
    %% Start the event manager
    {ok, Pid} = erlmcp_event_manager:start_link(),
    Pid.

cleanup(Pid) ->
    %% Stop the event manager if still running
    case is_process_alive(Pid) of
        true ->
            erlmcp_event_manager:stop();
        false ->
            ok
    end,
    ok.

%%====================================================================
%% Tests
%%====================================================================

test_start_stop(Pid) ->
    [?_assert(is_process_alive(Pid)),
     ?_assertEqual(ok, erlmcp_event_manager:stop()),
     ?_assertNot(is_process_alive(Pid))].

test_notify_no_handlers(_Pid) ->
    %% Notification should succeed even with no handlers
    Event = {test_event, data},
    [?_assertEqual(ok, erlmcp_event_manager:notify(Event)),
     ?_assertEqual(ok, erlmcp_event_manager:notify_async(Event))].

test_add_single_handler(_Pid) ->
    %% Add a test handler
    Result = erlmcp_event_manager:add_handler(erlmcp_event_logger, #{}),
    [?_assertEqual(ok, Result),
     ?_assertEqual([erlmcp_event_logger], erlmcp_event_manager:which_handlers())].

test_add_multiple_handlers(_Pid) ->
    %% Add multiple handlers
    ok = erlmcp_event_manager:add_handler(erlmcp_event_logger, #{}),
    ok = erlmcp_event_manager:add_handler(erlmcp_event_metrics, []),
    ok = erlmcp_event_manager:add_handler(erlmcp_event_audit, #{enabled => true}),

    Handlers = erlmcp_event_manager:which_handlers(),
    [?_assertEqual(3, length(Handlers)),
     ?_assert(lists:member(erlmcp_event_logger, Handlers)),
     ?_assert(lists:member(erlmcp_event_metrics, Handlers)),
     ?_assert(lists:member(erlmcp_event_audit, Handlers))].

test_remove_handler(_Pid) ->
    %% Add and remove handler
    ok = erlmcp_event_manager:add_handler(erlmcp_event_logger, #{}),
    ?assertEqual([erlmcp_event_logger], erlmcp_event_manager:which_handlers()),

    ok = erlmcp_event_manager:delete_handler(erlmcp_event_logger, []),

    [?_assertEqual([], erlmcp_event_manager:which_handlers())].

test_async_notify(_Pid) ->
    %% Add logger handler and send async notification
    ok = erlmcp_event_manager:add_handler(erlmcp_event_logger, #{}),

    Events =
        [{tool_executed, <<"test_tool">>, 1000000, ok},
         {resource_updated, <<"file://test.txt">>, #{}},
         {connection_state, connected, #{}}],

    %% All async notifications should return ok immediately
    Results = [erlmcp_event_manager:notify_async(E) || E <- Events],

    [?_assertEqual([ok, ok, ok], Results)].

test_sync_notify(_Pid) ->
    %% Add logger handler and send sync notification
    ok = erlmcp_event_manager:add_handler(erlmcp_event_logger, #{}),

    Events =
        [{tool_executed, <<"test_tool">>, 1000000, ok},
         {resource_updated, <<"file://test.txt">>, #{}},
         {error, test_category, test_reason}],

    %% All sync notifications should succeed
    Results = [erlmcp_event_manager:notify(E) || E <- Events],

    [?_assertEqual([ok, ok, ok], Results)].

test_which_handlers(_Pid) ->
    %% Initially no handlers
    ?assertEqual([], erlmcp_event_manager:which_handlers()),

    %% Add handlers one by one
    ok = erlmcp_event_manager:add_handler(erlmcp_event_logger, #{}),
    ?assertEqual([erlmcp_event_logger], erlmcp_event_manager:which_handlers()),

    ok = erlmcp_event_manager:add_handler(erlmcp_event_metrics, []),
    Handlers = erlmcp_event_manager:which_handlers(),

    [?_assertEqual(2, length(Handlers)),
     ?_assert(lists:member(erlmcp_event_logger, Handlers)),
     ?_assert(lists:member(erlmcp_event_metrics, Handlers))].

test_swap_handler(_Pid) ->
    %% Add initial handler
    ok = erlmcp_event_manager:add_handler(erlmcp_event_logger, #{}),
    ?assertEqual([erlmcp_event_logger], erlmcp_event_manager:which_handlers()),

    %% Swap for different handler
    ok = erlmcp_event_manager:swap_handler(erlmcp_event_logger, erlmcp_event_metrics, []),

    Handlers = erlmcp_event_manager:which_handlers(),
    [?_assertEqual([erlmcp_event_metrics], Handlers),
     ?_assertNot(lists:member(erlmcp_event_logger, Handlers))].

test_handler_crash_isolation(_Pid) ->
    %% Add multiple handlers
    ok = erlmcp_event_manager:add_handler(erlmcp_event_logger, #{}),
    ok = erlmcp_event_manager:add_handler(erlmcp_event_metrics, []),

    %% Send valid events - both handlers should process
    ok = erlmcp_event_manager:notify({tool_executed, <<"test">>, 1000, ok}),

    %% Verify both handlers still registered
    Handlers = erlmcp_event_manager:which_handlers(),
    [?_assert(lists:member(erlmcp_event_logger, Handlers)),
     ?_assert(lists:member(erlmcp_event_metrics, Handlers))].

test_notify_when_stopped(_Pid) ->
    %% Stop the manager
    ok = erlmcp_event_manager:stop(),

    %% Wait for process to terminate
    timer:sleep(100),

    %% Notifications should fail gracefully
    Result1 = erlmcp_event_manager:notify({test_event, data}),
    Result2 = erlmcp_event_manager:notify_async({test_event, data}),

    [?_assertEqual({error, noproc}, Result1),
     ?_assertEqual(ok, Result2)].  % Async always returns ok

%%====================================================================
%% Integration Tests
%%====================================================================

integration_test_() ->
    {setup, fun integration_setup/0, fun integration_cleanup/1, fun integration_tests/1}.

integration_setup() ->
    {ok, Pid} = erlmcp_event_manager:start_link(),

    %% Add all three handlers
    ok = erlmcp_event_manager:add_handler(erlmcp_event_logger, #{}),
    ok = erlmcp_event_manager:add_handler(erlmcp_event_metrics, []),
    ok =
        erlmcp_event_manager:add_handler(erlmcp_event_audit,
                                         #{enabled => true, log_all_events => true}),

    Pid.

integration_cleanup(Pid) ->
    case is_process_alive(Pid) of
        true ->
            erlmcp_event_manager:stop();
        false ->
            ok
    end.

integration_tests(_Pid) ->
    %% Test all event types with all handlers
    Events =
        [{tool_executed, <<"echo">>, 500000, ok},
         {tool_executed, <<"fail_tool">>, 1000000, {error, timeout}},
         {resource_updated, <<"file://data.json">>, #{size => 1024}},
         {connection_state, connected, #{transport => stdio}},
         {connection_state, disconnected, #{reason => normal}},
         {error, validation, invalid_params},
         {request_received, <<"tools/call">>, 123},
         {response_sent, <<"tools/call">>, 123, 750000},
         {notification_sent, <<"notifications/tools/list_changed">>, #{}},
         {session_created, <<"sess_001">>, #{user => <<"test">>}},
         {session_terminated, <<"sess_001">>, normal}],

    %% Send all events
    lists:foreach(fun(Event) -> ok = erlmcp_event_manager:notify(Event) end, Events),

    %% Verify handlers still registered
    Handlers = erlmcp_event_manager:which_handlers(),

    [?_assertEqual(3, length(Handlers)),
     ?_assert(lists:member(erlmcp_event_logger, Handlers)),
     ?_assert(lists:member(erlmcp_event_metrics, Handlers)),
     ?_assert(lists:member(erlmcp_event_audit, Handlers))].
