%%%-------------------------------------------------------------------
%%% @doc
%%% EUnit tests for erlmcp_event_metrics
%%%
%%% Tests the metrics collection event handler including:
%%% - Metrics collection for all event types
%%% - Counter increments
%%% - Telemetry integration
%%% - Statistics retrieval
%%% - Metrics reset
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_event_metrics_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Fixtures
%%====================================================================

metrics_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [fun test_init/1,
      fun test_tool_execution_metrics/1,
      fun test_resource_update_metrics/1,
      fun test_connection_metrics/1,
      fun test_error_metrics/1,
      fun test_request_response_metrics/1,
      fun test_session_metrics/1,
      fun test_get_metrics/1,
      fun test_reset_metrics/1,
      fun test_multiple_tool_executions/1]}.

setup() ->
    {ok, Pid} = erlmcp_event_manager:start_link(),
    ok = erlmcp_event_manager:add_handler(erlmcp_event_metrics, []),
    Pid.

cleanup(Pid) ->
    case is_process_alive(Pid) of
        true ->
            erlmcp_event_manager:stop();
        false ->
            ok
    end.

%%====================================================================
%% Tests
%%====================================================================

test_init(_Pid) ->
    %% Handler should be registered
    Handlers = erlmcp_event_manager:which_handlers(),
    [?_assert(lists:member(erlmcp_event_metrics, Handlers))].

test_tool_execution_metrics(_Pid) ->
    %% Send tool execution events
    ok = erlmcp_event_manager:notify({tool_executed, <<"echo">>, 1000000, ok}),
    ok = erlmcp_event_manager:notify({tool_executed, <<"echo">>, 1500000, ok}),
    ok = erlmcp_event_manager:notify({tool_executed, <<"grep">>, 2000000, {error, not_found}}),

    %% Get metrics
    Metrics = gen_event:call(erlmcp_event_manager, erlmcp_event_metrics, get_metrics),

    ToolExecs = maps:get(tool_executions, Metrics),

    [?_assertEqual(2, maps:get(<<"echo">>, ToolExecs, 0)),
     ?_assertEqual(1, maps:get(<<"grep">>, ToolExecs, 0))].

test_resource_update_metrics(_Pid) ->
    %% Send resource update events
    ok = erlmcp_event_manager:notify({resource_updated, <<"file://test.txt">>, #{}}),
    ok = erlmcp_event_manager:notify({resource_updated, <<"file://test.txt">>, #{}}),
    ok = erlmcp_event_manager:notify({resource_updated, <<"http://example.com">>, #{}}),

    %% Get metrics
    Metrics = gen_event:call(erlmcp_event_manager, erlmcp_event_metrics, get_metrics),

    ResUpdates = maps:get(resource_updates, Metrics),

    [?_assertEqual(2, maps:get(<<"file://test.txt">>, ResUpdates, 0)),
     ?_assertEqual(1, maps:get(<<"http://example.com">>, ResUpdates, 0))].

test_connection_metrics(_Pid) ->
    %% Send connection state events
    ok = erlmcp_event_manager:notify({connection_state, connected, #{}}),
    ok = erlmcp_event_manager:notify({connection_state, disconnected, #{}}),
    ok = erlmcp_event_manager:notify({connection_state, connected, #{}}),

    %% Get metrics
    Metrics = gen_event:call(erlmcp_event_manager, erlmcp_event_metrics, get_metrics),

    ConnectionEvents = maps:get(connection_events, Metrics),

    [?_assertEqual(3, ConnectionEvents)].

test_error_metrics(_Pid) ->
    %% Send error events
    ok = erlmcp_event_manager:notify({error, validation, invalid_params}),
    ok = erlmcp_event_manager:notify({error, validation, missing_field}),
    ok = erlmcp_event_manager:notify({error, transport, connection_lost}),

    %% Get metrics
    Metrics = gen_event:call(erlmcp_event_manager, erlmcp_event_metrics, get_metrics),

    ErrorCounts = maps:get(error_counts, Metrics),

    [?_assertEqual(2, maps:get(validation, ErrorCounts, 0)),
     ?_assertEqual(1, maps:get(transport, ErrorCounts, 0))].

test_request_response_metrics(_Pid) ->
    %% Send request/response events
    ok = erlmcp_event_manager:notify({request_received, <<"tools/call">>, 1}),
    ok = erlmcp_event_manager:notify({request_received, <<"resources/read">>, 2}),
    ok = erlmcp_event_manager:notify({response_sent, <<"tools/call">>, 1, 500000}),

    %% Get metrics
    Metrics = gen_event:call(erlmcp_event_manager, erlmcp_event_metrics, get_metrics),

    [?_assertEqual(2, maps:get(request_count, Metrics)),
     ?_assertEqual(1, maps:get(response_count, Metrics))].

test_session_metrics(_Pid) ->
    %% Send session events
    ok = erlmcp_event_manager:notify({session_created, <<"sess_1">>, #{}}),
    ok = erlmcp_event_manager:notify({session_created, <<"sess_2">>, #{}}),
    ok = erlmcp_event_manager:notify({session_terminated, <<"sess_1">>, normal}),

    %% Get metrics
    Metrics = gen_event:call(erlmcp_event_manager, erlmcp_event_metrics, get_metrics),

    [?_assertEqual(2, maps:get(session_count, Metrics))].

test_get_metrics( _Pid ) -> ok = erlmcp_event_manager : notify( { tool_executed , << "tool1" >> , 1000000 , ok } ) , ok = erlmcp_event_manager : notify( { resource_updated , << "uri1" >> , #{ } } ) , ok = erlmcp_event_manager : notify( { error , test , reason } ) , Metrics = gen_event : call( erlmcp_event_manager , erlmcp_event_metrics , get_metrics ) , [ ?_assertMatch( #{ tool_executions := _ } , Metrics ) , ?_assertMatch( #{ resource_updates := _ } , Metrics ) , ?_assertMatch( #{ error_counts := _ } , Metrics ) , ?_assertMatch( #{ connection_events := _ } , Metrics ) , ?_assertMatch( #{ request_count := _ } , Metrics ) , ?_assertMatch( #{ response_count := _ } , Metrics ) , ?_assertMatch( #{ notification_count := _ } , Metrics ) , ?_assertMatch( #{ session_count := _ } , Metrics ) , ?_assertMatch( #{ uptime_ms := Uptime } when Uptime >= 0 , Metrics ) ] .

    %% Send various events
    %% Get metrics

test_reset_metrics(_Pid) ->
    %% Send some events
    ok = erlmcp_event_manager:notify({tool_executed, <<"tool1">>, 1000000, ok}),
    ok = erlmcp_event_manager:notify({resource_updated, <<"uri1">>, #{}}),

    %% Get metrics before reset
    MetricsBefore = gen_event:call(erlmcp_event_manager, erlmcp_event_metrics, get_metrics),
    ?assertMatch(#{tool_executions := #{<<"tool1">> := 1}}, MetricsBefore),

    %% Reset metrics
    ok = gen_event:call(erlmcp_event_manager, erlmcp_event_metrics, reset_metrics),

    %% Get metrics after reset
    MetricsAfter = gen_event:call(erlmcp_event_manager, erlmcp_event_metrics, get_metrics),

    [?_assertEqual(#{}, maps:get(tool_executions, MetricsAfter)),
     ?_assertEqual(#{}, maps:get(resource_updates, MetricsAfter)),
     ?_assertEqual(#{}, maps:get(error_counts, MetricsAfter)),
     ?_assertEqual(0, maps:get(connection_events, MetricsAfter))].

test_multiple_tool_executions(_Pid) ->
    %% Execute same tool multiple times
    ToolName = <<"multi_exec_tool">>,

    lists:foreach(fun(N) ->
                     ok = erlmcp_event_manager:notify({tool_executed, ToolName, N * 1000000, ok})
                  end,
                  lists:seq(1, 10)),

    %% Get metrics
    Metrics = gen_event:call(erlmcp_event_manager, erlmcp_event_metrics, get_metrics),
    ToolExecs = maps:get(tool_executions, Metrics),

    [?_assertEqual(10, maps:get(ToolName, ToolExecs, 0))].

%%====================================================================
%% Integration Tests
%%====================================================================

integration_test_() ->
    {setup,
     fun() ->
        {ok, Pid} = erlmcp_event_manager:start_link(),
        ok = erlmcp_event_manager:add_handler(erlmcp_event_metrics, []),
        Pid
     end,
     fun(Pid) ->
        case is_process_alive(Pid) of
            true ->
                erlmcp_event_manager:stop();
            false ->
                ok
        end
     end,
     fun test_comprehensive_metrics/1}.

test_comprehensive_metrics(_Pid) ->
    %% Send a comprehensive set of events
    Events =
        [{tool_executed, <<"echo">>, 1000000, ok},
         {tool_executed, <<"echo">>, 1100000, ok},
         {tool_executed, <<"grep">>, 2000000, {error, not_found}},
         {resource_updated, <<"file://a.txt">>, #{}},
         {resource_updated, <<"file://b.txt">>, #{}},
         {resource_updated, <<"file://a.txt">>, #{}},
         {connection_state, connected, #{}},
         {connection_state, disconnected, #{}},
         {error, validation, test1},
         {error, validation, test2},
         {error, transport, test3},
         {request_received, <<"method1">>, 1},
         {request_received, <<"method2">>, 2},
         {response_sent, <<"method1">>, 1, 500000},
         {notification_sent, <<"notif1">>, #{}},
         {notification_sent, <<"notif2">>, #{}},
         {session_created, <<"sess1">>, #{}},
         {session_terminated, <<"sess1">>, normal}],

    lists:foreach(fun(E) -> erlmcp_event_manager:notify(E) end, Events),

    %% Get comprehensive metrics
    Metrics = gen_event:call(erlmcp_event_manager, erlmcp_event_metrics, get_metrics),

    [?_assertEqual(2, maps:get(<<"echo">>, maps:get(tool_executions, Metrics))),
     ?_assertEqual(1, maps:get(<<"grep">>, maps:get(tool_executions, Metrics))),
     ?_assertEqual(2, maps:get(<<"file://a.txt">>, maps:get(resource_updates, Metrics))),
     ?_assertEqual(1, maps:get(<<"file://b.txt">>, maps:get(resource_updates, Metrics))),
     ?_assertEqual(2, maps:get(connection_events, Metrics)),
     ?_assertEqual(2, maps:get(validation, maps:get(error_counts, Metrics))),
     ?_assertEqual(1, maps:get(transport, maps:get(error_counts, Metrics))),
     ?_assertEqual(2, maps:get(request_count, Metrics)),
     ?_assertEqual(1, maps:get(response_count, Metrics)),
     ?_assertEqual(2, maps:get(notification_count, Metrics)),
     ?_assertEqual(1, maps:get(session_count, Metrics))].
