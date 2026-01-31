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
     fun() -> {ok, Pid} = erlmcp_progress:start_link(), Pid end,
     fun(Pid) -> gen_server:stop(erlmcp_progress), ok end,
     fun(_Pid) ->
         [
          ?_test(create_progress_token()),
          ?_test(update_progress_with_increment()),
          ?_test(update_progress_with_current()),
          ?_test(update_progress_with_total()),
          ?_test(update_progress_with_message()),
          ?_test(complete_progress()),
          ?_test(cancel_progress()),
          ?_test(get_progress_state()),
          ?_test(encode_progress_notification()),
          ?_test(track_tool_call()),
          ?_test(cleanup_completed()),
          ?_test(multiple_progress_streams()),
          ?_test(progress_percentage_calculation()),
          ?_test(progress_notification_format()),
          ?_test(undefined_client_pid())
         ]
     end}.

%%%===================================================================
%%% Test Cases
%%%===================================================================

%% @doc Test creating a progress token
create_progress_token() ->
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
    end.

%% @doc Test updating progress with increment
update_progress_with_increment() ->
    ClientPid = self(),
    Token = erlmcp_progress:create(ClientPid, <<"Initializing">>),
    % Consume initial notification for THIS token (has progress=0)
    wait_for_token_notification(Token, 0),
    timer:sleep(50),  % Let create finish processing
    % Do the update
    erlmcp_progress:update(Token, #{increment => 25}),
    timer:sleep(100),  % Let update finish processing
    % Verify the update was applied (synchronous check via get_progress)
    {ok, ProgressAfter} = erlmcp_progress:get_progress(Token),
    ?assertEqual(25, maps:get(current, ProgressAfter)),
    % Wait for update notification from THIS token
    % Note: Update without total sends progress=undefined
    wait_for_token_notification(Token, undefined).

%% @doc Test updating progress with absolute current value
update_progress_with_current() ->
    ClientPid = self(),
    Token = erlmcp_progress:create(ClientPid, <<"Starting">>),
    % Consume initial notification for THIS token (has progress=0)
    wait_for_token_notification(Token, 0),
    timer:sleep(50),  % Let create finish processing
    % Do the update
    erlmcp_progress:update(Token, #{current => 50}),
    timer:sleep(100),  % Let update finish processing
    % Verify the update was applied (synchronous check via get_progress)
    {ok, ProgressAfter} = erlmcp_progress:get_progress(Token),
    ?assertEqual(50, maps:get(current, ProgressAfter)),
    % Wait for update notification from THIS token
    % Note: Update without total sends progress=undefined
    wait_for_token_notification(Token, undefined).

%% @doc Test updating progress with total (for percentage)
update_progress_with_total() ->
    ClientPid = self(),
    Token = erlmcp_progress:create(ClientPid, <<"Starting">>),
    timer:sleep(50),  % Allow initial notification to be delivered
    flush_notifications(),
    erlmcp_progress:update(Token, #{current => 25, total => 100}),
    timer:sleep(50),  % Allow gen_server:cast to process
    receive
        {mcp_notification, Notification} ->
            Params = maps:get(<<"params">>, Notification),
            ?assertEqual(25, maps_get_current(Params)),
            ?assertEqual(100, maps:get(<<"total">>, Params)),
            ?assertEqual(25, maps_get(<<"progress">>, Params))
    after 1000 ->
        ?assert(false, "No notification received after total update")
    end.

%% @doc Test updating progress message
update_progress_with_message() ->
    ClientPid = self(),
    Token = erlmcp_progress:create(ClientPid, <<"Starting">>),
    timer:sleep(50),  % Allow initial notification to be delivered
    flush_notifications(),
    erlmcp_progress:update(Token, #{message => <<"Processing data">>}),
    timer:sleep(50),  % Allow gen_server:cast to process
    receive
        {mcp_notification, Notification} ->
            Params = maps:get(<<"params">>, Notification),
            ?assertEqual(<<"Processing data">>, maps_get(<<"message">>, Params))
    after 1000 ->
        ?assert(false, "No notification received after message update")
    end.

%% @doc Test completing progress
complete_progress() ->
    ClientPid = self(),
    Token = erlmcp_progress:create(ClientPid, <<"Starting">>),
    timer:sleep(50),  % Allow initial notification to be delivered
    flush_notifications(),
    erlmcp_progress:complete(Token),
    timer:sleep(50),  % Allow gen_server:cast to process
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
    ?assertEqual({error, not_found}, erlmcp_progress:get_progress(Token)).

%% @doc Test canceling progress
cancel_progress() ->
    ClientPid = self(),
    Token = erlmcp_progress:create(ClientPid, <<"Starting">>),
    % Wait for initial notification
    receive
        {mcp_notification, _} -> ok
    after 500 -> ?assert(false, "No initial notification received")
    end,
    flush_notifications(),
    erlmcp_progress:cancel(Token),
    timer:sleep(50),  % Allow gen_server:cast to process
    % Token should be removed
    ?assertEqual({error, not_found}, erlmcp_progress:get_progress(Token)),
    % No completion notification should be sent
    receive
        {mcp_notification, _} ->
            ?assert(false, "Unexpected notification after cancel")
    after 100 ->
        ok
    end.

%% @doc Test getting progress state
get_progress_state() ->
    ClientPid = self(),
    Token = erlmcp_progress:create(ClientPid, <<"Starting">>),
    flush_notifications(),
    erlmcp_progress:update(Token, #{current => 50, total => 100}),
    {ok, ProgressMap} = erlmcp_progress:get_progress(Token),
    ?assertMatch(Token, maps:get(token, ProgressMap)),
    ?assertEqual(50, maps:get(current, ProgressMap)),
    ?assertEqual(100, maps:get(total, ProgressMap)),
    ?assertEqual(50, maps:get(progress, ProgressMap)),
    ?assert(is_integer(maps:get(elapsed_ms, ProgressMap))).

%% @doc Test encoding progress notification
encode_progress_notification() ->
    Token = make_ref(),
    Notification = erlmcp_progress:encode_progress_notification(Token, 75.5, 100),
    ?assertMatch(#{
        <<"jsonrpc">> := <<"2.0">>,
        <<"method">> := <<"notifications/progress">>
    }, Notification),
    Params = maps:get(<<"params">>, Notification),
    ?assertMatch(Token, maps:get(<<"progressToken">>, Params)),
    ?assertEqual(75.5, maps:get(<<"progress">>, Params)),
    ?assertEqual(100, maps:get(<<"total">>, Params)).

%% @doc Test tracking tool call
track_tool_call() ->
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
    end.

%% @doc Test cleanup completed
cleanup_completed() ->
    ClientPid = self(),
    Token = erlmcp_progress:create(ClientPid, <<"Starting">>),
    timer:sleep(50),  % Allow initial notification to be delivered
    flush_notifications(),
    ?assertMatch({ok, _}, erlmcp_progress:get_progress(Token)),
    erlmcp_progress:cleanup_completed(Token),
    timer:sleep(50),  % Allow gen_server:cast to process
    receive
        {mcp_notification, Notification} ->
            Params = maps:get(<<"params">>, Notification),
            ?assertEqual(100, maps_get(<<"progress">>, Params))
    after 1000 ->
        ?assert(false, "No completion notification received")
    end,
    ?assertEqual({error, not_found}, erlmcp_progress:get_progress(Token)).

%% @doc Test multiple concurrent progress streams
multiple_progress_streams() ->
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
    ?assert(Token1 =/= Token2).

%% @doc Test percentage calculation edge cases
progress_percentage_calculation() ->
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
    ?assertEqual(75, maps:get(progress, Progress3)).

%% @doc Test progress notification format compliance
progress_notification_format() ->
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
    end.

%% @doc Test undefined client pid (should not crash)
undefined_client_pid() ->
    Token = erlmcp_progress:create(undefined, <<"No client">>),
    ?assert(is_reference(Token)),
    flush_notifications(),
    % Should not crash or send notification
    erlmcp_progress:update(Token, #{current => 50}),
    erlmcp_progress:complete(Token),
    timer:sleep(100),  % Allow operations to process
    % No notification should be received
    receive
        {mcp_notification, _} ->
            flush_notifications(),  % Flush any stray notifications
            ?assert(false, "Unexpected notification with undefined client")
    after 100 ->
        ok
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

%% @doc Wait for notification with specific token and progress value
%% This filters out notifications from other tokens
wait_for_token_notification(Token, ExpectedProgress) ->
    wait_for_token_notification(Token, ExpectedProgress, 20).

wait_for_token_notification(_Token, _ExpectedProgress, 0) ->
    ?assert(false, "Max retries exceeded waiting for notification");
wait_for_token_notification(Token, ExpectedProgress, Retries) ->
    receive
        {mcp_notification, Notification} ->
            Params = maps:get(<<"params">>, Notification),
            NotificationToken = maps:get(<<"progressToken">>, Params),
            case NotificationToken of
                Token ->
                    % Correct token, check progress value
                    % Get raw progress value (undefined is allowed)
                    CurrentValue = maps:get(<<"progress">>, Params, undefined),
                    ?assertEqual(ExpectedProgress, CurrentValue);
                _OtherToken ->
                    % Notification from different token, retry
                    wait_for_token_notification(Token, ExpectedProgress, Retries - 1)
            end
    after 500 ->
        ?assert(false, "No notification received within 500ms")
    end.
