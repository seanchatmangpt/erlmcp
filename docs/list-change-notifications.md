# List Change Notifications

## Overview

The erlmcp server implements the MCP list change notification feature, allowing clients to subscribe to changes in the prompts, resources, and tools lists. This enables real-time synchronization when new items are added or modified on the server.

## Features

- **Automatic notifications** when prompts, resources, or tools are added to the server
- **Process monitoring** to automatically clean up subscriptions when clients disconnect
- **Multiple subscribers** per feature with efficient fan-out messaging
- **Zero-copy notification** using JSON-RPC 2.0 format
- **OpenTelemetry tracing** for observability

## Architecture

### Components

1. **erlmcp_change_notifier** (gen_server)
   - Manages subscriptions for each feature (prompts, resources, tools)
   - Monitors client processes for cleanup
   - Sends notifications via message passing
   - Uses ETS-backed sets for efficient subscriber management

2. **erlmcp_server** (updated)
   - Integrated change notifier into startup
   - Calls notifier when lists change
   - Includes `listChanged` capability in initialize response

## Notification Methods

The following JSON-RPC notification methods are sent to subscribers:

```
notifications/prompts/list_changed      - When prompts list changes
notifications/resources/list_changed    - When resources list changes
notifications/tools/list_changed        - When tools list changes
```

### Notification Format

```json
{
  "jsonrpc": "2.0",
  "method": "notifications/prompts/list_changed",
  "params": {}
}
```

All notifications include empty params, as the client should query the list endpoint to get the updated list.

## Server Capabilities

When the server initializes, it advertises support for list change notifications:

```erlang
#{
  <<"resources">> => #{
    <<"subscribe">> => true,
    <<"listChanged">> => true
  },
  <<"tools">> => #{
    <<"listChanged">> => true
  },
  <<"prompts">> => #{
    <<"listChanged">> => true
  }
}
```

## API Usage

### Starting the Change Notifier

The change notifier is automatically started when a server is created:

```erlang
{ok, ServerPid} = erlmcp_server:start_link(my_server, Capabilities)
```

### Subscribing to Changes

```erlang
ok = erlmcp_change_notifier:subscribe_to_changes(prompts, ClientPid)
ok = erlmcp_change_notifier:subscribe_to_changes(resources, ClientPid)
ok = erlmcp_change_notifier:subscribe_to_changes(tools, ClientPid)
```

### Unsubscribing

```erlang
ok = erlmcp_change_notifier:unsubscribe_from_changes(prompts, ClientPid)
```

### Getting Current Subscribers

```erlang
Subscribers = erlmcp_change_notifier:get_subscribers(prompts)
```

Returns a list of currently subscribed process IDs.

### Notifying Changes

Notifications are sent automatically when items are added:

```erlang
erlmcp_server:add_prompt(Server, <<"new_prompt">>, Handler)
% Automatically notifies all subscribers of prompts/list_changed

erlmcp_server:add_resource(Server, <<"file:///new">>, Handler)
% Automatically notifies all subscribers of resources/list_changed

erlmcp_server:add_tool(Server, <<"new_tool">>, Handler)
% Automatically notifies all subscribers of tools/list_changed
```

## Implementation Details

### Client Process Monitoring

The change notifier monitors all subscribed client processes. When a client process dies:

1. A 'DOWN' message is received
2. All subscriptions for that process are removed
3. Process monitors are cleaned up
4. Memory is freed

This ensures no stale subscriptions accumulate.

### Message Delivery

Notifications are sent via Erlang message passing:

```erlang
ClientPid ! {list_changed_notification, Method, EncodedNotification}
```

Where:
- `Method` is the binary method name (e.g., `<<"prompts/list_changed">>`)
- `EncodedNotification` is the JSON-RPC encoded message

### Tracing and Observability

All operations are instrumented with OpenTelemetry spans:

```
change_notifier.subscribe        - Subscribe operation
change_notifier.unsubscribe      - Unsubscribe operation
change_notifier.notify_list_changed  - Notify operation
change_notifier.cleanup_client   - Process cleanup
```

Attributes include feature name, client PID, and subscriber count.

## Error Handling

The change notifier uses defensive programming:

- Subscription calls are protected with try-catch
- Dead process cleanup is idempotent
- Failed notifications don't crash the notifier
- All errors are logged but don't propagate

## Testing

Comprehensive test suites are provided:

1. **erlmcp_change_notifier_tests.erl** (14 unit tests)
   - Basic subscription/unsubscription
   - Multiple subscribers
   - Client cleanup
   - Notification delivery
   - Monitor management

2. **erlmcp_list_change_notification_integration_test.erl** (6 integration tests)
   - End-to-end server integration
   - Prompt/resource/tool notifications
   - Multiple client coordination

### Running Tests

```bash
rebar3 eunit --module=erlmcp_change_notifier_tests
rebar3 eunit --module=erlmcp_list_change_notification_integration_test
```

## Performance Characteristics

- **Subscribe**: O(1) - adds to set and creates monitor
- **Unsubscribe**: O(1) - removes from set and demonitors if needed
- **Notify**: O(n) - sends message to all n subscribers
- **Memory**: O(n) - one reference per subscriber per feature

## Example: Client Implementation

```erlang
% Client subscribes to changes
subscribe_to_changes(Host, Port) ->
    % Connect to server
    {ok, Transport} = erlmcp_transport:connect(Host, Port),

    % Send initialize request
    ok = erlmcp_transport:send_request(Transport, initialize, #{}),

    % Subscribe to prompts changes
    ok = erlmcp_change_notifier:subscribe_to_changes(prompts, self()),

    % Receive notifications
    receive_notifications(Transport).

receive_notifications(Transport) ->
    receive
        {list_changed_notification, Method, Data} ->
            logger:info("Received: ~p", [Method]),

            % Query new list
            NewList = erlmcp_transport:send_request(Transport, prompts/list, #{}),

            handle_new_list(NewList),
            receive_notifications(Transport);
        Other ->
            logger:warning("Unexpected message: ~p", [Other]),
            receive_notifications(Transport)
    end.
```

## Compatibility

- MCP Protocol: 2025-06-18+
- OTP Version: 25+
- Erlang: 25+

## Future Enhancements

1. **Selective notifications** - Only notify for specific feature types
2. **Batching** - Group notifications within a time window
3. **Filtering** - Only notify for items matching criteria
4. **History** - Store recent changes for late subscribers
5. **Persistent subscriptions** - Store subscriptions across restarts
