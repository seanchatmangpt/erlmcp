%%%-------------------------------------------------------------------
%%% @doc Unit tests for erlmcp_progress module
%%% Tests progress token tracking, notifications, and lifecycle.
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_progress_tests).
-include_lib("eunit/include/eunit.hrl").
-include("erlmcp.hrl").

%%%===================================================================
%%% Test Setup and Teardown
%%%===================================================================

%% @doc Start progress server for each test group
progress_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      fun create_progress_token/1,
      fun update_progress_with_increment/1,
      fun update_progress_with_current/1,
      fun update_progress_with_total/1,
      fun update_progress_with_message/1,
      fun complete_progress/1,
      fun cancel_progress/1,
      fun get_progress_state/1,
      fun encode_progress_notification/1,
      fun track_tool_call/1,
      fun cleanup_completed/1,
      fun multiple_progress_streams/1,
      fun progress_percentage_calculation/1,
      fun progress_notification_format/1,
      fun undefined_client_pid/1
     ]
    }.

setup() ->
    {ok, Pid} = erlmcp_progress:start_link(),
    Pid.

cleanup(_Pid) ->
    gen_server:stop(erlmcp_progress),
    ok.

%%%===================================================================
%%% Test Cases
%%%===================================================================

%% @doc Test creating a progress token
create_progress_token(_Pid) ->
    fun() ->
        ClientPid = self(),
        Token = erlmcp_progress:create(ClientPid, <<"Starting operation">>),
        ?assert(is_reference(Token)),
        receive
            {mcp_notification, Notification} ->
                ?assertMatch(#{
                    <<"jsonrpc">> := <<"2.0">>,
                    <<"method">> := <<"notifications/progress">>
                }, Notification),
                Params = maps:get(<<"params">>, Notification),
                ?assertMatch(Token, maps:get(<<"progressToken">>, Params)),
                ?assertEqual(0, maps:get(<<"progress">>, Params)),
                ?assertEqual(undefined, maps:get(<<"total">>, Params)),
                ?assertEqual(<<"Starting operation">>, maps:get(<<"message">>, Params))
        after 1000 ->
            ?assert(false, "No notification received")
        end
    end.

%% @doc Test updating progress with increment
update_progress_with_increment(_Pid) ->
    fun() ->
        ClientPid = self(),
        Token = erlmcp_progress:create(ClientPid, <<"Initializing">>),
        flush_notifications(),
        erlmcp_progress:update(Token, #{increment => 25}),
        receive
            {mcp_notification, Notification} ->
                Params = maps:get(<<"params">>, Notification),
                ?assertEqual(25, maps_get_current(Params))
        after 1000 ->
            ?assert(false, "No notification received after increment")
        end
    end.

%% @doc Test updating progress with absolute current value
update_progress_with_current(_Pid) ->
    fun() ->
        ClientPid = self(),
        Token = erlmcp_progress:create(ClientPid, <<"Starting">>),
        flush_notifications(),
        erlmcp_progress:update(Token, #{current => 50}),
        receive
            {mcp_notification, Notification} ->
                Params = maps:get(<<"params">>, Notification),
                ?assertEqual(50, maps_get_current(Params))
        after 1000 ->
            ?assert(false, "No notification received after current update")
        end
    end.

%% @doc Test updating progress with total (for percentage)
update_progress_with_total(_Pid) ->
    fun() ->
        ClientPid = self(),
        Token = erlmcp_progress:create(ClientPid, <<"Starting">>),
        flush_notifications(),
        erlmcp_progress:update(Token, #{current => 25, total => 100}),
        receive
            {mcp_notification, Notification} ->
                Params = maps:get(<<"params">>, Notification),
                ?assertEqual(25, maps_get_current(Params)),
                ?assertEqual(100, maps_get(<<"total">>, Params)),
                ?assertEqual(25, maps_get(<<"progress">>, Params))
        after 1000 ->
            ?assert(false, "No notification received after total update")
        end
    end.

%% @doc Test updating progress message
update_progress_with_message(_Pid) ->
    fun() ->
        ClientPid = self(),
        Token = erlmcp_progress:create(ClientPid, <<"Starting">>),
        flush_notifications(),
        erlmcp_progress:update(Token, #{message => <<"Processing data">>}),
        receive
            {mcp_notification, Notification} ->
                Params = maps:get(<<"params">>, Notification),
                ?assertEqual(<<"Processing data">>, maps_get(<<"message">>, Params))
        after 1000 ->
            ?assert(false, "No notification received after message update")
        end
    end.

%% @doc Test completing progress
complete_progress(_Pid) ->
    fun() ->
        ClientPid = self(),
        Token = erlmcp_progress:create(ClientPid, <<"Starting">>),
        flush_notifications(),
        erlmcp_progress:complete(Token),
        receive
            {mcp_notification, Notification} ->
                Params = maps:get(<<"params">>, Notification),
                ?assertEqual(100, maps_get(<<"progress">>, Params)),
                ?assertEqual(100, maps_get(<<"total">>, Params)),
                ?assertMatch(<<"Starting - Complete">>, maps_get(<<"message">>, Params))
        after 1000 ->
            ?assert(false, "No completion notification received")
        end,
        % Token should be removed
        ?assertEqual({error, not_found}, erlmcp_progress:get_progress(Token))
    end.

%% @doc Test canceling progress
cancel_progress(_Pid) ->
    fun() ->
        ClientPid = self(),
        Token = erlmcp_progress:create(ClientPid, <<"Starting">>),
        flush_notifications(),
        erlmcp_progress:cancel(Token),
        % Token should be removed
        ?assertEqual({error, not_found}, erlmcp_progress:get_progress(Token)),
        % No completion notification should be sent
        receive
            {mcp_notification, _} ->
                ?assert(false, "Unexpected notification after cancel")
        after 100 ->
            ok
        end
    end.

%% @doc Test getting progress state
get_progress_state(_Pid) ->
    fun() ->
        ClientPid = self(),
        Token = erlmcp_progress:create(ClientPid, <<"Starting">>),
        flush_notifications(),
        erlmcp_progress:update(Token, #{current => 50, total => 100}),
        {ok, ProgressMap} = erlmcp_progress:get_progress(Token),
        ?assertMatch(Token, maps:get(token, ProgressMap)),
        ?assertEqual(50, maps:get(current, ProgressMap)),
        ?assertEqual(100, maps:get(total, ProgressMap)),
        ?assertEqual(50, maps:get(progress, ProgressMap)),
        ?assert(is_integer(maps:get(elapsed_ms, ProgressMap)))
    end.

%% @doc Test encoding progress notification
encode_progress_notification(_Pid) ->
    fun() ->
        Token = make_ref(),
        Notification = erlmcp_progress:encode_progress_notification(Token, 75.5, 100),
        ?assertMatch(#{
            <<"jsonrpc">> := <<"2.0">>,
            <<"method">> := <<"notifications/progress">>
        }, Notification),
        Params = maps:get(<<"params">>, Notification),
        ?assertMatch(Token, maps:get(<<"progressToken">>, Params)),
        ?assertEqual(75.5, maps:get(<<"progress">>, Params)),
        ?assertEqual(100, maps:get(<<"total">>, Params))
    end.

%% @doc Test tracking tool call
track_tool_call(_Pid) ->
    fun() ->
        ToolName = <<"test_tool">>,
        ServerPid = self(),
        Token = erlmcp_progress:generate_token(),
        flush_notifications(),
        erlmcp_progress:track_tool_call(Token, ToolName, ServerPid),
        receive
            {mcp_notification, Notification} ->
                Params = maps:get(<<"params">>, Notification),
                ExpectedMessage = <<"Tool execution started: test_tool">>,
                ?assertEqual(ExpectedMessage, maps_get(<<"message">>, Params))
        after 1000 ->
            ?assert(false, "No notification received for tool call tracking")
        end
    end.

%% @doc Test cleanup completed
cleanup_completed(_Pid) ->
    fun() ->
        ClientPid = self(),
        Token = erlmcp_progress:create(ClientPid, <<"Starting">>),
        flush_notifications(),
        ?assertMatch({ok, _}, erlmcp_progress:get_progress(Token)),
        erlmcp_progress:cleanup_completed(Token),
        receive
            {mcp_notification, Notification} ->
                Params = maps:get(<<"params">>, Notification),
                ?assertEqual(100, maps_get(<<"progress">>, Params))
        after 1000 ->
            ?assert(false, "No completion notification received")
        end,
        ?assertEqual({error, not_found}, erlmcp_progress:get_progress(Token))
    end.

%% @doc Test multiple concurrent progress streams
multiple_progress_streams(_Pid) ->
    fun() ->
        ClientPid = self(),
        Token1 = erlmcp_progress:create(ClientPid, <<"Stream 1">>),
        Token2 = erlmcp_progress:create(ClientPid, <<"Stream 2">>),
        flush_notifications(),
        erlmcp_progress:update(Token1, #{current => 50, total => 100}),
        erlmcp_progress:update(Token2, #{current => 25, total => 50}),
        {ok, Progress1} = erlmcp_progress:get_progress(Token1),
        {ok, Progress2} = erlmcp_progress:get_progress(Token2),
        ?assertEqual(50, maps:get(current, Progress1)),
        ?assertEqual(25, maps:get(current, Progress2)),
        % Verify they are different tokens
        ?assert(Token1 =/= Token2)
    end.

%% @doc Test percentage calculation edge cases
progress_percentage_calculation(_Pid) ->
    fun() ->
        ClientPid = self(),
        % No total - percentage undefined
        Token1 = erlmcp_progress:create(ClientPid, <<"No total">>),
        flush_notifications(),
        erlmcp_progress:update(Token1, #{current => 50}),
        {ok, Progress1} = erlmcp_progress:get_progress(Token1),
        ?assertEqual(undefined, maps:get(progress, Progress1)),
        % Zero total - percentage undefined
        Token2 = erlmcp_progress:create(ClientPid, <<"Zero total">>),
        erlmcp_progress:update(Token2, #{current => 50, total => 0}),
        {ok, Progress2} = erlmcp_progress:get_progress(Token2),
        ?assertEqual(undefined, maps:get(progress, Progress2)),
        % Normal percentage
        Token3 = erlmcp_progress:create(ClientPid, <<"Normal">>),
        erlmcp_progress:update(Token3, #{current => 75, total => 100}),
        {ok, Progress3} = erlmcp_progress:get_progress(Token3),
        ?assertEqual(75, maps:get(progress, Progress3))
    end.

%% @doc Test progress notification format compliance
progress_notification_format(_Pid) ->
    fun() ->
        ClientPid = self(),
        Token = erlmcp_progress:create(ClientPid, <<"Testing format">>),
        receive
            {mcp_notification, Notification} ->
                % Verify JSON-RPC 2.0 compliance
                ?assert(maps:is_key(<<"jsonrpc">>, Notification)),
                ?assertEqual(<<"2.0">>, maps:get(<<"jsonrpc">>, Notification)),
                ?assert(maps:is_key(<<"method">>, Notification)),
                ?assertEqual(<<"notifications/progress">>, maps:get(<<"method">>, Notification)),
                ?assert(maps:is_key(<<"params">>, Notification)),
                Params = maps:get(<<"params">>, Notification),
                ?assert(maps:is_key(<<"progressToken">>, Params)),
                ?assert(maps:is_key(<<"progress">>, Params)),
                ?assert(maps:is_key(<<"total">>, Params)),
                ?assert(maps:is_key(<<"message">>, Params))
        after 1000 ->
            ?assert(false, "No notification received")
        end
    end.

%% @doc Test undefined client pid (should not crash)
undefined_client_pid(_Pid) ->
    fun() ->
        Token = erlmcp_progress:create(undefined, <<"No client">>),
        ?assert(is_reference(Token)),
        % Should not crash or send notification
        erlmcp_progress:update(Token, #{current => 50}),
        erlmcp_progress:complete(Token),
        % No notification should be received
        receive
            {mcp_notification, _} ->
                ?assert(false, "Unexpected notification with undefined client")
        after 100 ->
            ok
        end
    end.

%%%===================================================================
%%% Helper Functions
%%%===================================================================

%% @doc Flush all pending notifications from mailbox
flush_notifications() ->
    receive
        {mcp_notification, _} -> flush_notifications()
    after 0 ->
        ok
    end.

%% @doc Helper to get current from params (handles 0 default)
maps_get_current(Params) ->
    case maps:get(<<"progress">>, Params, 0) of
        undefined -> 0;
        Val -> Val
    end.

%% @doc Helper to safely get map key
maps_get(Key, Map) ->
    case maps:get(Key, Map, undefined) of
        undefined -> error({missing_key, Key});
        Val -> Val
    end.
