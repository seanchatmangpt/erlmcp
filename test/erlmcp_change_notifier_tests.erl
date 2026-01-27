-module(erlmcp_change_notifier_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("erlmcp/include/erlmcp.hrl").

%%%===================================================================
%%% Test Suite for erlmcp_change_notifier
%%%
%%% This test suite validates the list change notification system that
%%% notifies clients when prompts, resources, or tools lists change.
%%%===================================================================

%%====================================================================
%% Test Fixtures
%%====================================================================

change_notifier_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
         fun test_notifier_starts/1,
         fun test_subscribe_to_prompts_changes/1,
         fun test_subscribe_to_resources_changes/1,
         fun test_subscribe_to_tools_changes/1,
         fun test_unsubscribe_from_changes/1,
         fun test_multiple_subscribers_same_feature/1,
         fun test_client_cleanup_on_exit/1,
         fun test_notify_single_subscriber/1,
         fun test_notify_multiple_subscribers/1,
         fun test_subscribe_multiple_features/1,
         fun test_unsubscribe_preserves_other_subscriptions/1,
         fun test_notification_format/1,
         fun test_get_subscribers_list/1,
         fun test_monitor_cleanup/1
     ]}.

%%====================================================================
%% Setup and Cleanup
%%====================================================================

setup() ->
    % Ensure gproc is started for any registry operations
    ok = ensure_gproc_started(),

    % Start change notifier
    {ok, NotifierPid} = erlmcp_change_notifier:start_link(),

    % Give it a moment to initialize
    timer:sleep(100),

    #{notifier => NotifierPid, test_pids => []}.

cleanup(#{notifier := NotifierPid} = State) ->
    TestPids = maps:get(test_pids, State, []),

    % Kill all test client processes
    lists:foreach(fun(Pid) ->
        catch unlink(Pid),
        catch exit(Pid, kill)
    end, TestPids),

    % Stop notifier
    catch erlmcp_change_notifier:stop(),
    timer:sleep(100),
    ok.

ensure_gproc_started() ->
    case application:ensure_started(gproc) of
        ok -> ok;
        {error, {already_started, gproc}} -> ok
    end.

%%====================================================================
%% Test Cases - Basic Functionality
%%====================================================================

test_notifier_starts(#{notifier := NotifierPid}) ->
    [
        ?_assert(is_pid(NotifierPid)),
        ?_assert(is_process_alive(NotifierPid))
    ].

test_subscribe_to_prompts_changes(#{notifier := _Notifier} = State) ->
    ClientPid = spawn(fun dummy_client/0),
    State1 = add_test_pid(ClientPid, State),

    Result = erlmcp_change_notifier:subscribe_to_changes(prompts, ClientPid),

    [
        ?_assertEqual(ok, Result),
        ?_assertMatch([ClientPid], erlmcp_change_notifier:get_subscribers(prompts)),
        State1
    ].

test_subscribe_to_resources_changes(#{notifier := _Notifier} = State) ->
    ClientPid = spawn(fun dummy_client/0),
    State1 = add_test_pid(ClientPid, State),

    Result = erlmcp_change_notifier:subscribe_to_changes(resources, ClientPid),

    [
        ?_assertEqual(ok, Result),
        ?_assertMatch([ClientPid], erlmcp_change_notifier:get_subscribers(resources)),
        State1
    ].

test_subscribe_to_tools_changes(#{notifier := _Notifier} = State) ->
    ClientPid = spawn(fun dummy_client/0),
    State1 = add_test_pid(ClientPid, State),

    Result = erlmcp_change_notifier:subscribe_to_changes(tools, ClientPid),

    [
        ?_assertEqual(ok, Result),
        ?_assertMatch([ClientPid], erlmcp_change_notifier:get_subscribers(tools)),
        State1
    ].

test_unsubscribe_from_changes(#{notifier := _Notifier} = State) ->
    ClientPid = spawn(fun dummy_client/0),
    State1 = add_test_pid(ClientPid, State),

    ok = erlmcp_change_notifier:subscribe_to_changes(prompts, ClientPid),
    Subscribers1 = erlmcp_change_notifier:get_subscribers(prompts),

    ok = erlmcp_change_notifier:unsubscribe_from_changes(prompts, ClientPid),
    Subscribers2 = erlmcp_change_notifier:get_subscribers(prompts),

    [
        ?_assert(length(Subscribers1) > 0),
        ?_assertEqual([], Subscribers2),
        State1
    ].

%%====================================================================
%% Test Cases - Multiple Subscribers
%%====================================================================

test_multiple_subscribers_same_feature(#{notifier := _Notifier} = State) ->
    Client1 = spawn(fun dummy_client/0),
    Client2 = spawn(fun dummy_client/0),
    Client3 = spawn(fun dummy_client/0),
    State1 = add_test_pid(Client1, add_test_pid(Client2, add_test_pid(Client3, State))),

    ok = erlmcp_change_notifier:subscribe_to_changes(prompts, Client1),
    ok = erlmcp_change_notifier:subscribe_to_changes(prompts, Client2),
    ok = erlmcp_change_notifier:subscribe_to_changes(prompts, Client3),

    Subscribers = erlmcp_change_notifier:get_subscribers(prompts),

    [
        ?_assertEqual(3, length(Subscribers)),
        ?_assert(lists:member(Client1, Subscribers)),
        ?_assert(lists:member(Client2, Subscribers)),
        ?_assert(lists:member(Client3, Subscribers)),
        State1
    ].

test_client_cleanup_on_exit(#{notifier := _Notifier} = State) ->
    % Create a client and subscribe it
    ClientPid = spawn(fun dummy_client/0),
    State1 = add_test_pid(ClientPid, State),

    ok = erlmcp_change_notifier:subscribe_to_changes(prompts, ClientPid),
    ok = erlmcp_change_notifier:subscribe_to_changes(resources, ClientPid),

    % Verify subscriptions exist
    Subscribers1 = erlmcp_change_notifier:get_subscribers(prompts),
    ?_assert(lists:member(ClientPid, Subscribers1)),

    % Kill the client
    exit(ClientPid, kill),
    timer:sleep(150),

    % Verify subscriptions are cleaned up
    Subscribers2 = erlmcp_change_notifier:get_subscribers(prompts),
    Subscribers3 = erlmcp_change_notifier:get_subscribers(resources),

    [
        ?_assertEqual(false, lists:member(ClientPid, Subscribers2)),
        ?_assertEqual(false, lists:member(ClientPid, Subscribers3)),
        State1
    ].

%%====================================================================
%% Test Cases - Notifications
%%====================================================================

test_notify_single_subscriber(#{notifier := _Notifier} = State) ->
    ClientPid = spawn(fun message_collecting_client/0),
    State1 = add_test_pid(ClientPid, State),

    ok = erlmcp_change_notifier:subscribe_to_changes(prompts, ClientPid),

    % Send notification
    erlmcp_change_notifier:notify_list_changed(prompts),
    timer:sleep(100),

    % Check if client received message
    ClientPid ! {get_messages, self()},
    Messages = receive
        {messages, M} -> M
    after 500 -> []
    end,

    [
        ?_assert(length(Messages) > 0),
        ?_assert(lists:any(fun({list_changed_notification, Method, _Data}) ->
            Method =:= ?MCP_METHOD_NOTIFICATIONS_PROMPTS_LIST_CHANGED
        end, Messages)),
        State1
    ].

test_notify_multiple_subscribers(#{notifier := _Notifier} = State) ->
    Client1 = spawn(fun message_collecting_client/0),
    Client2 = spawn(fun message_collecting_client/0),
    Client3 = spawn(fun message_collecting_client/0),
    State1 = add_test_pid(Client1, add_test_pid(Client2, add_test_pid(Client3, State))),

    % Subscribe all to same feature
    ok = erlmcp_change_notifier:subscribe_to_changes(resources, Client1),
    ok = erlmcp_change_notifier:subscribe_to_changes(resources, Client2),
    ok = erlmcp_change_notifier:subscribe_to_changes(resources, Client3),

    % Send notification
    erlmcp_change_notifier:notify_list_changed(resources),
    timer:sleep(100),

    % Collect messages from all clients
    Client1 ! {get_messages, self()},
    Client2 ! {get_messages, self()},
    Client3 ! {get_messages, self()},

    Msg1 = receive {messages, M} -> M after 500 -> [] end,
    Msg2 = receive {messages, M} -> M after 500 -> [] end,
    Msg3 = receive {messages, M} -> M after 500 -> [] end,

    [
        ?_assert(length(Msg1) > 0),
        ?_assert(length(Msg2) > 0),
        ?_assert(length(Msg3) > 0),
        State1
    ].

%%====================================================================
%% Test Cases - Multiple Features
%%====================================================================

test_subscribe_multiple_features(#{notifier := _Notifier} = State) ->
    ClientPid = spawn(fun dummy_client/0),
    State1 = add_test_pid(ClientPid, State),

    ok = erlmcp_change_notifier:subscribe_to_changes(prompts, ClientPid),
    ok = erlmcp_change_notifier:subscribe_to_changes(resources, ClientPid),
    ok = erlmcp_change_notifier:subscribe_to_changes(tools, ClientPid),

    PromptsSubscribers = erlmcp_change_notifier:get_subscribers(prompts),
    ResourcesSubscribers = erlmcp_change_notifier:get_subscribers(resources),
    ToolsSubscribers = erlmcp_change_notifier:get_subscribers(tools),

    [
        ?_assert(lists:member(ClientPid, PromptsSubscribers)),
        ?_assert(lists:member(ClientPid, ResourcesSubscribers)),
        ?_assert(lists:member(ClientPid, ToolsSubscribers)),
        State1
    ].

test_unsubscribe_preserves_other_subscriptions(#{notifier := _Notifier} = State) ->
    ClientPid = spawn(fun dummy_client/0),
    State1 = add_test_pid(ClientPid, State),

    % Subscribe to all three features
    ok = erlmcp_change_notifier:subscribe_to_changes(prompts, ClientPid),
    ok = erlmcp_change_notifier:subscribe_to_changes(resources, ClientPid),
    ok = erlmcp_change_notifier:subscribe_to_changes(tools, ClientPid),

    % Unsubscribe from one feature
    ok = erlmcp_change_notifier:unsubscribe_from_changes(prompts, ClientPid),

    % Check subscriptions
    PromptsSubscribers = erlmcp_change_notifier:get_subscribers(prompts),
    ResourcesSubscribers = erlmcp_change_notifier:get_subscribers(resources),
    ToolsSubscribers = erlmcp_change_notifier:get_subscribers(tools),

    [
        ?_assertEqual(false, lists:member(ClientPid, PromptsSubscribers)),
        ?_assert(lists:member(ClientPid, ResourcesSubscribers)),
        ?_assert(lists:member(ClientPid, ToolsSubscribers)),
        State1
    ].

%%====================================================================
%% Test Cases - Notification Format
%%====================================================================

test_notification_format(#{notifier := _Notifier} = State) ->
    ClientPid = spawn(fun message_collecting_client/0),
    State1 = add_test_pid(ClientPid, State),

    ok = erlmcp_change_notifier:subscribe_to_changes(tools, ClientPid),

    % Send notification
    erlmcp_change_notifier:notify_list_changed(tools),
    timer:sleep(100),

    % Get messages
    ClientPid ! {get_messages, self()},
    Messages = receive
        {messages, M} -> M
    after 500 -> []
    end,

    % Filter for list_changed_notification
    ListChangedMsgs = [M || M = {list_changed_notification, _, _} <- Messages],

    [
        ?_assert(length(ListChangedMsgs) > 0),
        ?_assert(is_notification_message(hd(ListChangedMsgs))),
        State1
    ].

%%====================================================================
%% Test Cases - Get Subscribers
%%====================================================================

test_get_subscribers_list(#{notifier := _Notifier} = State) ->
    Client1 = spawn(fun dummy_client/0),
    Client2 = spawn(fun dummy_client/0),
    State1 = add_test_pid(Client1, add_test_pid(Client2, State)),

    % Subscribe first client to prompts only
    ok = erlmcp_change_notifier:subscribe_to_changes(prompts, Client1),

    % Subscribe both clients to resources
    ok = erlmcp_change_notifier:subscribe_to_changes(resources, Client1),
    ok = erlmcp_change_notifier:subscribe_to_changes(resources, Client2),

    % Check get_subscribers
    PromptsSubs = erlmcp_change_notifier:get_subscribers(prompts),
    ResourcesSubs = erlmcp_change_notifier:get_subscribers(resources),
    ToolsSubs = erlmcp_change_notifier:get_subscribers(tools),

    [
        ?_assertEqual(1, length(PromptsSubs)),
        ?_assertEqual(2, length(ResourcesSubs)),
        ?_assertEqual(0, length(ToolsSubs)),
        ?_assert(lists:member(Client1, PromptsSubs)),
        ?_assert(lists:member(Client1, ResourcesSubs)),
        ?_assert(lists:member(Client2, ResourcesSubs)),
        State1
    ].

%%====================================================================
%% Test Cases - Monitor Cleanup
%%====================================================================

test_monitor_cleanup(#{notifier := _Notifier} = State) ->
    % This tests that monitors are properly cleaned up
    Client1 = spawn(fun dummy_client/0),
    Client2 = spawn(fun dummy_client/0),
    State1 = add_test_pid(Client1, add_test_pid(Client2, State)),

    % Subscribe Client1 to all features
    ok = erlmcp_change_notifier:subscribe_to_changes(prompts, Client1),
    ok = erlmcp_change_notifier:subscribe_to_changes(resources, Client1),
    ok = erlmcp_change_notifier:subscribe_to_changes(tools, Client1),

    % Subscribe Client2 to one feature
    ok = erlmcp_change_notifier:subscribe_to_changes(prompts, Client2),

    % Unsubscribe Client1 from all but keep in system
    ok = erlmcp_change_notifier:unsubscribe_from_changes(prompts, Client1),
    ok = erlmcp_change_notifier:unsubscribe_from_changes(resources, Client1),
    ok = erlmcp_change_notifier:unsubscribe_from_changes(tools, Client1),

    % Client1 should now have no monitors
    PromptsAfter = erlmcp_change_notifier:get_subscribers(prompts),
    ResourcesAfter = erlmcp_change_notifier:get_subscribers(resources),

    [
        ?_assertEqual(false, lists:member(Client1, PromptsAfter)),
        ?_assertEqual(false, lists:member(Client1, ResourcesAfter)),
        ?_assert(lists:member(Client2, PromptsAfter)),
        State1
    ].

%%====================================================================
%% Helper Functions
%%====================================================================

dummy_client() ->
    receive
        _ -> dummy_client()
    end.

message_collecting_client() ->
    message_collecting_client([]).

message_collecting_client(Messages) ->
    receive
        {get_messages, Requester} ->
            Requester ! {messages, Messages},
            message_collecting_client([]);
        Msg ->
            message_collecting_client([Msg | Messages])
    after 5000 ->
        erlang:exit(normal)
    end.

add_test_pid(Pid, State) ->
    TestPids = maps:get(test_pids, State, []),
    State#{test_pids := [Pid | TestPids]}.

is_notification_message({list_changed_notification, Method, Data}) ->
    is_binary(Method) andalso is_binary(Data).
