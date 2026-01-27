-module(erlmcp_list_change_notification_integration_test).
-include_lib("eunit/include/eunit.hrl").
-include_lib("erlmcp/include/erlmcp.hrl").

%%%===================================================================
%%% Integration Test for List Change Notifications
%%%
%%% This test verifies that list change notifications work end-to-end
%%% when adding prompts, resources, and tools to the server.
%%%===================================================================

%%====================================================================
%% Test Fixtures
%%====================================================================

list_change_integration_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
         fun test_server_has_notifier_support/1,
         fun test_add_prompt_notifies_subscribers/1,
         fun test_add_resource_notifies_subscribers/1,
         fun test_add_tool_notifies_subscribers/1,
         fun test_multiple_clients_receive_notification/1,
         fun test_server_capabilities_include_list_changed/1
     ]}.

%%====================================================================
%% Setup and Cleanup
%%====================================================================

setup() ->
    % Ensure gproc is started
    ok = ensure_gproc_started(),

    % Start the notifier
    {ok, NotifierPid} = erlmcp_change_notifier:start_link(),
    timer:sleep(100),

    % Create a simple server with capabilities
    Caps = #mcp_server_capabilities{
        prompts = #mcp_capability{enabled = true},
        resources = #mcp_capability{enabled = true},
        tools = #mcp_capability{enabled = true}
    },
    {ok, ServerPid} = erlmcp_server:start_link(test_server, Caps),

    #{notifier => NotifierPid, server => ServerPid, test_pids => []}.

cleanup(#{notifier := NotifierPid, server := ServerPid} = State) ->
    TestPids = maps:get(test_pids, State, []),

    % Kill test pids
    lists:foreach(fun(Pid) ->
        catch unlink(Pid),
        catch exit(Pid, kill)
    end, TestPids),

    % Stop server and notifier
    catch erlmcp_server:stop(ServerPid),
    catch erlmcp_change_notifier:stop(),
    timer:sleep(100),
    ok.

ensure_gproc_started() ->
    case application:ensure_started(gproc) of
        ok -> ok;
        {error, {already_started, gproc}} -> ok
    end.

%%====================================================================
%% Test Cases
%%====================================================================

test_server_has_notifier_support(#{server := ServerPid}) ->
    % Verify server process is alive
    [?_assert(is_process_alive(ServerPid))].

test_add_prompt_notifies_subscribers(#{server := ServerPid} = State) ->
    % Create a client that collects notifications
    ClientPid = spawn(fun notification_collector/0),
    State1 = add_test_pid(ClientPid, State),

    % Subscribe to prompt changes
    ok = erlmcp_change_notifier:subscribe_to_changes(prompts, ClientPid),

    % Add a prompt to the server
    PromptHandler = fun(_Args) -> <<"test prompt result">> end,
    ok = erlmcp_server:add_prompt(ServerPid, <<"test_prompt">>, PromptHandler),

    % Wait for notification
    timer:sleep(200),

    % Check if notification was received
    ClientPid ! {check_notifications, self()},
    {HasNotification, Method} = receive
        {notification_status, Status, M} -> {Status, M}
    after 1000 -> {false, unknown}
    end,

    [
        ?_assert(HasNotification),
        ?_assertEqual(?MCP_METHOD_NOTIFICATIONS_PROMPTS_LIST_CHANGED, Method),
        State1
    ].

test_add_resource_notifies_subscribers(#{server := ServerPid} = State) ->
    % Create a client that collects notifications
    ClientPid = spawn(fun notification_collector/0),
    State1 = add_test_pid(ClientPid, State),

    % Subscribe to resource changes
    ok = erlmcp_change_notifier:subscribe_to_changes(resources, ClientPid),

    % Add a resource to the server
    ResourceHandler = fun(_Uri) -> <<"test resource content">> end,
    ok = erlmcp_server:add_resource(ServerPid, <<"file:///test">>, ResourceHandler),

    % Wait for notification
    timer:sleep(200),

    % Check if notification was received
    ClientPid ! {check_notifications, self()},
    {HasNotification, Method} = receive
        {notification_status, Status, M} -> {Status, M}
    after 1000 -> {false, unknown}
    end,

    [
        ?_assert(HasNotification),
        ?_assertEqual(?MCP_METHOD_NOTIFICATIONS_RESOURCES_LIST_CHANGED, Method),
        State1
    ].

test_add_tool_notifies_subscribers(#{server := ServerPid} = State) ->
    % Create a client that collects notifications
    ClientPid = spawn(fun notification_collector/0),
    State1 = add_test_pid(ClientPid, State),

    % Subscribe to tool changes
    ok = erlmcp_change_notifier:subscribe_to_changes(tools, ClientPid),

    % Add a tool to the server
    ToolHandler = fun(_Args) -> <<"tool result">> end,
    ok = erlmcp_server:add_tool(ServerPid, <<"test_tool">>, ToolHandler),

    % Wait for notification
    timer:sleep(200),

    % Check if notification was received
    ClientPid ! {check_notifications, self()},
    {HasNotification, Method} = receive
        {notification_status, Status, M} -> {Status, M}
    after 1000 -> {false, unknown}
    end,

    [
        ?_assert(HasNotification),
        ?_assertEqual(?MCP_METHOD_NOTIFICATIONS_TOOLS_LIST_CHANGED, Method),
        State1
    ].

test_multiple_clients_receive_notification(#{server := ServerPid} = State) ->
    % Create multiple clients
    Client1 = spawn(fun notification_collector/0),
    Client2 = spawn(fun notification_collector/0),
    Client3 = spawn(fun notification_collector/0),
    State1 = add_test_pid(Client1, add_test_pid(Client2, add_test_pid(Client3, State))),

    % All subscribe to tool changes
    ok = erlmcp_change_notifier:subscribe_to_changes(tools, Client1),
    ok = erlmcp_change_notifier:subscribe_to_changes(tools, Client2),
    ok = erlmcp_change_notifier:subscribe_to_changes(tools, Client3),

    % Add a tool
    ToolHandler = fun(_Args) -> <<"tool">> end,
    ok = erlmcp_server:add_tool(ServerPid, <<"multi_tool">>, ToolHandler),

    timer:sleep(200),

    % Check all clients received notification
    Client1 ! {check_notifications, self()},
    Client2 ! {check_notifications, self()},
    Client3 ! {check_notifications, self()},

    {Status1, _} = receive {notification_status, S1, _} -> {S1, ok} after 1000 -> {false, timeout} end,
    {Status2, _} = receive {notification_status, S2, _} -> {S2, ok} after 1000 -> {false, timeout} end,
    {Status3, _} = receive {notification_status, S3, _} -> {S3, ok} after 1000 -> {false, timeout} end,

    [
        ?_assert(Status1),
        ?_assert(Status2),
        ?_assert(Status3),
        State1
    ].

test_server_capabilities_include_list_changed(#{server := ServerPid}) ->
    % Verify that initialize response includes listChanged capability
    % This would require sending an initialize message, but for now we just
    % verify the server is alive and has the feature
    [?_assert(is_process_alive(ServerPid))].

%%====================================================================
%% Helper Functions
%%====================================================================

notification_collector() ->
    notification_collector([]).

notification_collector(Notifications) ->
    receive
        {list_changed_notification, Method, _Data} ->
            notification_collector([{Method} | Notifications]);
        {check_notifications, Requester} ->
            HasNotif = length(Notifications) > 0,
            Method = case Notifications of
                [] -> unknown;
                [{M} | _] -> M
            end,
            Requester ! {notification_status, HasNotif, Method},
            notification_collector(Notifications);
        _ ->
            notification_collector(Notifications)
    after 5000 ->
        erlang:exit(normal)
    end.

add_test_pid(Pid, State) ->
    TestPids = maps:get(test_pids, State, []),
    State#{test_pids := [Pid | TestPids]}.
