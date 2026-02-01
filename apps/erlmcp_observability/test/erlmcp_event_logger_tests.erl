%%%-------------------------------------------------------------------
%%% @doc
%%% EUnit tests for erlmcp_event_logger
%%%
%%% Tests the logging event handler functionality including:
%%% - Event logging at appropriate levels
%%% - Statistics collection
%%% - Handler lifecycle
%%% - All event types
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_event_logger_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Fixtures
%%====================================================================

logger_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [fun test_init/1,
      fun test_tool_execution_logging/1,
      fun test_resource_update_logging/1,
      fun test_connection_state_logging/1,
      fun test_error_logging/1,
      fun test_request_response_logging/1,
      fun test_session_logging/1,
      fun test_get_stats/1,
      fun test_unknown_event/1]}.

setup() ->
    {ok, Pid} = erlmcp_event_manager:start_link(),
    ok = erlmcp_event_manager:add_handler(erlmcp_event_logger, #{}),
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
    [?_assert(lists:member(erlmcp_event_logger, Handlers))].

test_tool_execution_logging(_Pid) ->
    %% Test successful tool execution
    ok = erlmcp_event_manager:notify({tool_executed, <<"test_tool">>, 1000000, ok}),

    %% Test failed tool execution
    ok = erlmcp_event_manager:notify({tool_executed, <<"fail_tool">>, 2000000, {error, timeout}}),

    %% Test tool execution with result
    ok = erlmcp_event_manager:notify({tool_executed, <<"echo">>, 500000, {ok, <<"hello">>}}),

    [?_assertEqual(ok, erlmcp_event_manager:notify({tool_executed, <<"another">>, 750000, ok}))].

test_resource_update_logging(_Pid) ->
    %% Test resource updates with different metadata
    Events =
        [{resource_updated, <<"file://test.txt">>, #{size => 1024}},
         {resource_updated, <<"http://example.com/data">>, #{status => updated}},
         {resource_updated, <<"memory://cache">>, #{}}],

    Results = [erlmcp_event_manager:notify(E) || E <- Events],

    [?_assertEqual([ok, ok, ok], Results)].

test_connection_state_logging(_Pid) ->
    %% Test connection state changes
    ok = erlmcp_event_manager:notify({connection_state, connected, #{transport => stdio}}),
    ok = erlmcp_event_manager:notify({connection_state, disconnected, #{reason => normal}}),
    ok =
        erlmcp_event_manager:notify({connection_state,
                                     connected,
                                     #{transport => tcp, port => 8080}}),

    [?_assertEqual(ok,
                   erlmcp_event_manager:notify({connection_state,
                                                disconnected,
                                                #{reason => error}}))].

test_error_logging(_Pid) ->
    %% Test different error categories
    Errors =
        [{error, validation, invalid_params},
         {error, transport, connection_lost},
         {error, protocol, unsupported_version},
         {error, internal, unexpected_error}],

    Results = [erlmcp_event_manager:notify(E) || E <- Errors],

    [?_assertEqual([ok, ok, ok, ok], Results)].

test_request_response_logging(_Pid) ->
    %% Test request/response logging
    RequestId = 123,

    ok = erlmcp_event_manager:notify({request_received, <<"tools/call">>, RequestId}),
    ok = erlmcp_event_manager:notify({response_sent, <<"tools/call">>, RequestId, 500000}),

    %% Test notification
    ok =
        erlmcp_event_manager:notify({notification_sent,
                                     <<"notifications/tools/list_changed">>,
                                     #{}}),

    [?_assertEqual(ok, erlmcp_event_manager:notify({request_received, <<"resources/read">>, 124}))].

test_session_logging(_Pid) ->
    %% Test session lifecycle events
    SessionId = <<"sess_001">>,

    ok = erlmcp_event_manager:notify({session_created, SessionId, #{user => <<"test">>}}),
    ok = erlmcp_event_manager:notify({session_terminated, SessionId, normal}),

    [?_assertEqual(ok, erlmcp_event_manager:notify({session_created, <<"sess_002">>, #{}}))].

test_get_stats( _Pid ) -> Events = [ { tool_executed , << "tool1" >> , 1000000 , ok } , { tool_executed , << "tool2" >> , 2000000 , ok } , { resource_updated , << "uri" >> , #{ } } , { error , test , reason } ] , lists : foreach( fun ( E ) -> erlmcp_event_manager : notify( E ) end , Events ) , Stats = gen_event : call( erlmcp_event_manager , erlmcp_event_logger , get_stats ) , [ ?_assertMatch( #{ event_count := Count } when Count >= 4 , Stats ) , ?_assertMatch( #{ uptime_ms := _ } , Stats ) , ?_assertMatch( #{ log_level := info } , Stats ) ] .

    %% Send several events
    %% Get stats via gen_event:call

test_unknown_event(_Pid) ->
    %% Unknown event should be logged but not crash
    Result = erlmcp_event_manager:notify({unknown_event_type, some, data}),

    [?_assertEqual(ok, Result),
     ?_assert(lists:member(erlmcp_event_logger, erlmcp_event_manager:which_handlers()))].

%%====================================================================
%% Custom Log Level Tests
%%====================================================================

custom_log_level_test_() ->
    {setup,
     fun() ->
        {ok, Pid} = erlmcp_event_manager:start_link(),
        ok = erlmcp_event_manager:add_handler(erlmcp_event_logger, #{log_level => debug}),
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
     fun(_) ->
        Stats = gen_event:call(erlmcp_event_manager, erlmcp_event_logger, get_stats),
        [?_assertMatch(#{log_level := debug}, Stats)]
     end}.
