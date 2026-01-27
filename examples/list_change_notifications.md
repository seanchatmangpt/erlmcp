# List Change Notifications Examples

This document provides practical examples of using the list change notification feature in erlmcp.

## Basic Server Setup

### 1. Starting a Server with Notifications

```erlang
% Create server with all capabilities enabled
Capabilities = #mcp_server_capabilities{
    prompts = #mcp_capability{enabled = true},
    resources = #mcp_capability{enabled = true},
    tools = #mcp_capability{enabled = true}
},

% Start server - notifier starts automatically
{ok, ServerPid} = erlmcp_server:start_link(my_server, Capabilities).
```

### 2. Adding Tools with Automatic Notifications

```erlang
% Define a tool handler
CalculatorHandler = fun(#{<<"operation">> := Op, <<"a">> := A, <<"b">> := B}) ->
    case Op of
        <<"add">> -> A + B;
        <<"sub">> -> A - B;
        <<"mul">> -> A * B;
        <<"div">> -> A / B
    end
end,

% Add tool - automatically notifies subscribers of tools/list_changed
ok = erlmcp_server:add_tool(ServerPid, <<"calculator">>, CalculatorHandler),

% Add another tool
ok = erlmcp_server:add_tool(ServerPid, <<"sqrt">>, fun(_) -> math:sqrt(2) end).
```

### 3. Adding Resources with Notifications

```erlang
% Define a resource handler
FileHandler = fun(Uri) ->
    case Uri of
        <<"file:///data/config.json">> ->
            file:read_file("/etc/app/config.json");
        <<"file:///data/status">> ->
            Status = get_system_status(),
            jsx:encode(Status)
    end
end,

% Add resources - each one notifies subscribers
ok = erlmcp_server:add_resource(ServerPid, <<"file:///data/config.json">>, FileHandler),
ok = erlmcp_server:add_resource(ServerPid, <<"file:///data/status">>, FileHandler).
```

### 4. Adding Prompts with Notifications

```erlang
% Simple prompt handler
PromptHandler = fun(#{<<"name">> := Name}) ->
    <<"Generate a response for: ", Name/binary>>
end,

% Add prompts - notifies subscribers of prompts/list_changed
ok = erlmcp_server:add_prompt(
    ServerPid,
    <<"summarize">>,
    PromptHandler
),

% Add prompt with arguments
ArgumentPrompt = fun(#{<<"topic">> := Topic, <<"style">> := Style}) ->
    <<"Write about ", Topic/binary, " in ", Style/binary, " style">>
end,

Args = [
    #mcp_prompt_argument{name = <<"topic">>, required = true, description = <<"Topic to write about">>},
    #mcp_prompt_argument{name = <<"style">>, required = true, description = <<"Writing style">>}
],

ok = erlmcp_server:add_prompt_with_args(ServerPid, <<"structured">>, ArgumentPrompt, Args).
```

## Client-Side Examples

### 1. Basic Subscription Pattern

```erlang
-module(my_mcp_client).

% Subscribe to all changes
subscribe_all(ServerPid) ->
    ok = erlmcp_change_notifier:subscribe_to_changes(prompts, self()),
    ok = erlmcp_change_notifier:subscribe_to_changes(resources, self()),
    ok = erlmcp_change_notifier:subscribe_to_changes(tools, self()),

    % Start listening for notifications
    receive_notifications().

% Listen for notifications
receive_notifications() ->
    receive
        {list_changed_notification, Method, Data} ->
            handle_notification(Method, Data),
            receive_notifications();
        Other ->
            logger:warning("Unexpected message: ~p", [Other]),
            receive_notifications()
    after
        30000 ->  % 30 second timeout
            logger:info("No notifications received, exiting"),
            ok
    end.

% Handle different notification types
handle_notification(?MCP_METHOD_NOTIFICATIONS_PROMPTS_LIST_CHANGED, _) ->
    logger:info("Prompts list changed - querying new prompts list"),
    % TODO: Call erlmcp_client:list_prompts() here
    ok;

handle_notification(?MCP_METHOD_NOTIFICATIONS_RESOURCES_LIST_CHANGED, _) ->
    logger:info("Resources list changed"),
    ok;

handle_notification(?MCP_METHOD_NOTIFICATIONS_TOOLS_LIST_CHANGED, _) ->
    logger:info("Tools list changed"),
    ok;

handle_notification(Method, _) ->
    logger:warning("Unknown notification method: ~p", [Method]),
    ok.
```

### 2. Selective Subscription

```erlang
% Only subscribe to certain features
subscribe_to_prompts_only(ServerPid) ->
    ok = erlmcp_change_notifier:subscribe_to_changes(prompts, self()),
    receive_prompts_notifications().

subscribe_to_tools_only(ServerPid) ->
    ok = erlmcp_change_notifier:subscribe_to_changes(tools, self()),
    receive_tools_notifications().

% Receive only tool notifications
receive_tools_notifications() ->
    receive
        {list_changed_notification, ?MCP_METHOD_NOTIFICATIONS_TOOLS_LIST_CHANGED, _} ->
            logger:info("Tools changed - fetching new tools"),
            receive_tools_notifications();
        {list_changed_notification, _OtherMethod, _} ->
            % Ignore other notifications
            receive_tools_notifications();
        Other ->
            logger:warning("Unexpected: ~p", [Other]),
            receive_tools_notifications()
    end.
```

### 3. Unsubscribing from Notifications

```erlang
% Subscribe to a feature
ok = erlmcp_change_notifier:subscribe_to_changes(resources, self()),

% ... do stuff ...

% Unsubscribe when no longer interested
ok = erlmcp_change_notifier:unsubscribe_from_changes(resources, self()).
```

### 4. Checking Current Subscriptions

```erlang
% Get all current subscribers to prompts
PromptSubscribers = erlmcp_change_notifier:get_subscribers(prompts),
logger:info("Prompt subscribers: ~p", [PromptSubscribers]),

% Verify current client is subscribed
case lists:member(self(), PromptSubscribers) of
    true -> logger:info("Client is subscribed to prompts");
    false -> logger:info("Client is NOT subscribed to prompts")
end.
```

## Advanced Examples

### 1. Debounced Notifications

Client-side debouncing - don't refresh list too often:

```erlang
-module(debounced_client).

subscribe_with_debounce(Feature, DebounceMs) ->
    erlmcp_change_notifier:subscribe_to_changes(Feature, self()),
    receive_debounced(Feature, DebounceMs, undefined).

receive_debounced(Feature, DebounceMs, LastRefreshTime) ->
    receive
        {list_changed_notification, Method, _Data} ->
            Now = erlang:system_time(millisecond),
            case LastRefreshTime of
                undefined ->
                    % First notification, refresh immediately
                    refresh_list(Method),
                    receive_debounced(Feature, DebounceMs, Now);
                LastTime ->
                    TimeSinceLastRefresh = Now - LastTime,
                    case TimeSinceLastRefresh >= DebounceMs of
                        true ->
                            % Enough time has passed, refresh
                            refresh_list(Method),
                            receive_debounced(Feature, DebounceMs, Now);
                        false ->
                            % Too soon, wait
                            receive_debounced(Feature, DebounceMs, LastTime)
                    end
            end
    end.

refresh_list(Method) ->
    logger:info("Refreshing list for method: ~p", [Method]),
    % TODO: Call appropriate list method
    ok.
```

### 2. Async Notification Handler

Handle notifications asynchronously without blocking:

```erlang
-module(async_notifier).

% Subscribe and spawn handler process
subscribe_async(Feature, HandlerModule) ->
    ok = erlmcp_change_notifier:subscribe_to_changes(Feature, self()),
    spawn_link(fun() -> notification_loop(HandlerModule) end).

notification_loop(HandlerModule) ->
    receive
        {list_changed_notification, Method, Data} ->
            % Spawn a worker to handle this notification
            spawn(fun() ->
                try
                    HandlerModule:handle(Method, Data)
                catch
                    Class:Reason:Stack ->
                        logger:error("Handler error: ~p:~p~n~p", [Class, Reason, Stack])
                end
            end),
            notification_loop(HandlerModule)
    end.
```

### 3. Notification Filtering

Only react to specific notifications:

```erlang
-module(filtered_notifier).

% Subscribe but only listen for specific methods
subscribe_filtered(Methods) when is_list(Methods) ->
    ok = erlmcp_change_notifier:subscribe_to_changes(prompts, self()),
    ok = erlmcp_change_notifier:subscribe_to_changes(resources, self()),
    ok = erlmcp_change_notifier:subscribe_to_changes(tools, self()),
    receive_filtered(Methods).

receive_filtered(Methods) ->
    receive
        {list_changed_notification, Method, Data} ->
            case lists:member(Method, Methods) of
                true ->
                    handle_notification(Method, Data);
                false ->
                    ok  % Ignore this notification
            end,
            receive_filtered(Methods)
    end.

handle_notification(Method, Data) ->
    logger:info("Handling ~p with data: ~p", [Method, Data]).
```

### 4. Collecting Multiple Notifications

Wait for multiple notifications before acting:

```erlang
-module(batch_notifier).

% Subscribe and wait for all three types to change
wait_for_all_changes() ->
    ok = erlmcp_change_notifier:subscribe_to_changes(prompts, self()),
    ok = erlmcp_change_notifier:subscribe_to_changes(resources, self()),
    ok = erlmcp_change_notifier:subscribe_to_changes(tools, self()),
    wait_batch(#{prompts => false, resources => false, tools => false}, 5000).

wait_batch(Status, Timeout) ->
    AllReady = maps:size(maps:filter(fun(_, V) -> V =:= true end, Status)) =:= 3,
    case AllReady of
        true ->
            logger:info("All lists have changed!"),
            refresh_everything();
        false ->
            receive
                {list_changed_notification, ?MCP_METHOD_NOTIFICATIONS_PROMPTS_LIST_CHANGED, _} ->
                    wait_batch(Status#{prompts => true}, Timeout);
                {list_changed_notification, ?MCP_METHOD_NOTIFICATIONS_RESOURCES_LIST_CHANGED, _} ->
                    wait_batch(Status#{resources => true}, Timeout);
                {list_changed_notification, ?MCP_METHOD_NOTIFICATIONS_TOOLS_LIST_CHANGED, _} ->
                    wait_batch(Status#{tools => true}, Timeout)
            after Timeout ->
                logger:warning("Timeout waiting for all changes"),
                refresh_available(Status)
            end
    end.

refresh_everything() ->
    logger:info("Refreshing all lists"),
    ok.

refresh_available(Status) ->
    logger:info("Refreshing available lists: ~p", [Status]).
```

## Testing Examples

### Unit Test Pattern

```erlang
-module(my_notifier_test).
-include_lib("eunit/include/eunit.hrl").

subscribe_test() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
         fun(State) -> test_subscribe_unsubscribe(State) end,
         fun(State) -> test_notification_delivery(State) end
     ]}.

setup() ->
    ok = application:ensure_started(gproc),
    {ok, NotifierPid} = erlmcp_change_notifier:start_link(),
    timer:sleep(100),
    #{notifier => NotifierPid}.

cleanup(#{notifier := NotifierPid}) ->
    catch erlmcp_change_notifier:stop(),
    ok.

test_subscribe_unsubscribe(_State) ->
    MyPid = self(),
    ok = erlmcp_change_notifier:subscribe_to_changes(tools, MyPid),

    Subscribers = erlmcp_change_notifier:get_subscribers(tools),
    ?assert(lists:member(MyPid, Subscribers)),

    ok = erlmcp_change_notifier:unsubscribe_from_changes(tools, MyPid),
    Subscribers2 = erlmcp_change_notifier:get_subscribers(tools),
    ?assertNot(lists:member(MyPid, Subscribers2)).

test_notification_delivery(_State) ->
    MyPid = self(),
    ok = erlmcp_change_notifier:subscribe_to_changes(prompts, MyPid),

    erlmcp_change_notifier:notify_list_changed(prompts),
    timer:sleep(100),

    ?assertMatch(
        {list_changed_notification, ?MCP_METHOD_NOTIFICATIONS_PROMPTS_LIST_CHANGED, _},
        receive_message(1000)
    ).

receive_message(Timeout) ->
    receive
        Msg -> Msg
    after Timeout -> timeout
    end.
```

## Real-World Integration

### Web Server Integration

```erlang
% In a Cowboy handler, subscribe to notifications
handle_websocket_init(Req, State) ->
    {ok, Req, State#{
        client_pid => self(),
        subscribed_features => []
    }}.

handle_websocket_message({<<"subscribe">>, <<"prompts">>}, State) ->
    ok = erlmcp_change_notifier:subscribe_to_changes(prompts, self()),
    Features = maps:get(subscribed_features, State, []),
    {reply, {text, <<"subscribed">>}, State#{
        subscribed_features => [prompts | Features]
    }};

handle_websocket_message({<<"subscribe">>, Feature}, State) ->
    erlmcp_change_notifier:subscribe_to_changes(binary_to_atom(Feature), self()),
    {ok, State}.

% Forward notifications to WebSocket client
handle_info({list_changed_notification, Method, _Data}, State) ->
    Response = jsx:encode(#{
        <<"type">> => <<"list_changed">>,
        <<"method">> => Method
    }),
    {reply, {text, Response}, State};

handle_info(_Info, State) ->
    {ok, State}.
```

## Performance Tips

1. **Batch Operations**: Group multiple tool additions to reduce notification frequency
2. **Debounce Updates**: Implement client-side debouncing to avoid redundant refreshes
3. **Selective Subscriptions**: Only subscribe to features you actually care about
4. **Async Handlers**: Process notifications asynchronously to not block the notifier
5. **Monitor Resources**: Check subscriber count periodically with `get_subscribers/1`

## Troubleshooting

### Missing Notifications

- Verify subscription with `get_subscribers/1`
- Check that server's notifier_pid is not undefined
- Ensure client process is still alive
- Review logs for notification errors

### Memory Leaks

- Always unsubscribe when done
- Check for dead client processes
- Monitor gets notifier metrics

### Performance Issues

- Reduce notification frequency with batching
- Use debouncing on client side
- Limit number of subscribers
- Profile with OpenTelemetry traces
