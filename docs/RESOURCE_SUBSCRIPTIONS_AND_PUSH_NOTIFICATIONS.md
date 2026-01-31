# MCP Resource Subscriptions and Push Notifications Specification

## Overview

The Model Context Protocol (MCP) 2025-11-25 enables real-time, server-initiated notifications through a subscription-based push notification system. This document details the complete specification for resource subscriptions, notification delivery, and lifecycle management in erlmcp.

**Protocol Version**: MCP 2025-11-25
**Implementation**: erlmcp v2.2.0
**Architecture**: Erlang/OTP gen_server with gproc-based registry routing

---

## 1. Resource Subscription Mechanism

### 1.1 Overview

Resource subscriptions allow clients to register interest in specific resources and receive real-time `resources/updated` notifications when those resources change. The subscription system uses:

- **URI-based matching**: Exact URIs and URI templates
- **Process monitoring**: Automatic cleanup via Erlang monitor/2
- **Rate limiting**: Per-subscriber configurable limits
- **Filtering**: Optional message filters per subscription

### 1.2 Subscription Request (Client → Server)

The client initiates a subscription request using the `resources/subscribe` RPC method:

```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "method": "resources/subscribe",
  "params": {
    "uri": "file:///documents/report.md"
  }
}
```

**Request Parameters**:
- `uri` (string, required): The resource URI to subscribe to
  - Must be a valid URI format
  - Supports exact match or URI template patterns
  - Example: `"file:///path/to/resource.txt"`, `"weather://city/{location}"`

**Server Response** (Success):
```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "result": {}
}
```

**Server Response** (Error):
```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "error": {
    "code": -32022,
    "message": "Invalid URI",
    "data": {
      "error_type": "invalid_uri_format",
      "uri": "invalid://uri"
    }
  }
}
```

### 1.3 Unsubscription Request (Client → Server)

The client terminates a subscription using the `resources/unsubscribe` RPC method:

```json
{
  "jsonrpc": "2.0",
  "id": 2,
  "method": "resources/unsubscribe",
  "params": {
    "uri": "file:///documents/report.md"
  }
}
```

**Request Parameters**:
- `uri` (string, required): The resource URI to unsubscribe from
  - Must match exactly the URI from the subscription request
  - Caller's process identity is used to identify the subscriber

**Server Response** (Success):
```json
{
  "jsonrpc": "2.0",
  "id": 2,
  "result": {}
}
```

**Server Response** (Error - Not Found):
```json
{
  "jsonrpc": "2.0",
  "id": 2,
  "error": {
    "code": -32006,
    "message": "Subscription not found"
  }
}
```

---

## 2. Notification Message Format

### 2.1 resources/updated Notification (Server → Client)

When a subscribed resource changes, the server sends an asynchronous notification:

```json
{
  "jsonrpc": "2.0",
  "method": "resources/updated",
  "params": {
    "uri": "file:///documents/report.md",
    "metadata": {
      "updated_at": 1704067200000,
      "version": "2.1.0",
      "size": 4096,
      "checksum": "abc123def456"
    }
  }
}
```

**Notification Parameters**:

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `uri` | string | Yes | The URI of the changed resource |
| `metadata` | object | No | Optional metadata about the change |
| `metadata.updated_at` | integer | No | Unix timestamp (milliseconds) of the change |
| `metadata.version` | string | No | Version identifier or version string |
| `metadata.size` | integer | No | Updated size of resource (bytes) |
| `metadata.checksum` | string | No | Hash/checksum of resource content |
| `metadata.*` | any | No | Server-specific metadata fields |

**Example with Rich Metadata**:
```json
{
  "jsonrpc": "2.0",
  "method": "resources/updated",
  "params": {
    "uri": "weather://city/san-francisco",
    "metadata": {
      "updated_at": 1704067200000,
      "temperature": 72.5,
      "humidity": 65,
      "last_fetch": "2024-01-01T12:00:00Z",
      "confidence": 0.95,
      "source": "openweathermap"
    }
  }
}
```

### 2.2 Notification Guarantees

**Delivery Semantics**:
- **At-most-once delivery**: Notifications may be lost during network failure
- **No ordering guarantee**: Multiple notifications for the same resource may arrive out of order
- **No deduplication**: Duplicate notifications may be sent if resource changes rapidly
- **Fire-and-forget**: Server does not wait for client acknowledgment

**Rationale**: MCP is designed for AI assistant integrations where strict ordering/deduplication is handled by the client or wrapped by a higher-level protocol layer.

### 2.3 Notification Timing

Notifications are sent:
1. **Immediately** when a resource change is notified (subject to rate limiting)
2. **Batched within window**: Rapid successive changes within 100ms are collected and sent together
3. **Rate-limited**: Respects per-subscriber or per-resource rate limits
4. **Asynchronously**: Sent as JSON-RPC 2.0 notifications (no response expected)

---

## 3. Unsubscription Handling

### 3.1 Explicit Unsubscription (Client-Initiated)

The client sends an explicit `resources/unsubscribe` request:

```erlang
%% Erlang/OTP Pattern
ok = erlmcp_server:unsubscribe_resource(Server, <<"file:///resource.txt">>).
```

**Server Behavior**:
1. Matches the requesting process identity (caller PID)
2. Removes subscription from registry
3. Demonitors the process (cancels automatic cleanup monitoring)
4. Decrements subscription counter for the resource
5. Cleans up resource entry if no more subscribers
6. Sends success response

### 3.2 Automatic Unsubscription (Client Death)

When a subscriber process dies, the server automatically cleans up:

```erlang
%% Monitor trap fires when subscriber dies
handle_info({'DOWN', MonitorRef, process, SubscriberPid, Reason}, State) ->
    % Automatically remove all subscriptions for the dead process
    % Decrement subscription counters
    % Clean up empty resource entries
```

**Cleanup Details**:
- Triggered via Erlang's `monitor/2` mechanism
- Automatic removal of all subscriptions for the dead process
- No explicit unsubscribe message needed
- No error response returned (process is dead)
- Guaranteed cleanup within milliseconds

### 3.3 Graceful Disconnection

When a client connection closes gracefully:

```erlang
%% Server terminates connection
terminate(_Reason, #state{subscriptions = Subscriptions} = State) ->
    % Iterate through all subscriptions
    maps:foreach(fun(Uri, Subscribers) ->
        % For each subscriber in this URI's subscription set
        sets:fold(fun(SubscriberPid, _) ->
            erlmcp_resource_subscriptions:unsubscribe_from_resource(Uri, SubscriberPid)
        end, ok, Subscribers)
    end, Subscriptions)
```

**Cleanup Steps**:
1. Server process receives terminate signal
2. Iterates through all tracked subscriptions
3. Calls unsubscribe for each subscriber/URI pair
4. Demonitors each process (best effort)
5. Clean up complete on server shutdown

---

## 4. Resource Change Events

### 4.1 Change Notification from Server

The server notifies the subscription system when a resource changes:

```erlang
%% Server-side API
ok = erlmcp_server:notify_resource_updated(
    Server,
    <<"file:///documents/report.md">>,
    #{
        updated_at => erlang:system_time(millisecond),
        version => <<"2.1.0">>,
        size => 4096,
        checksum => <<"abc123def456">>
    }
).
```

**Module API** (erlmcp_resource_subscriptions):
```erlang
-spec notify_resource_changed(uri(), map()) -> ok.
notify_resource_changed(Uri, Metadata) when is_binary(Uri), is_map(Metadata)
```

### 4.2 Change Detection Architecture

```
┌─────────────────────────────────────────────────────┐
│ Application Layer                                    │
│ (File monitor, database watcher, external API)      │
└─────────────────┬───────────────────────────────────┘
                  │ detects change
                  ▼
┌─────────────────────────────────────────────────────┐
│ erlmcp_server:notify_resource_updated/3             │
│ (notifies subscription system)                      │
└─────────────────┬───────────────────────────────────┘
                  │ cast to manager
                  ▼
┌─────────────────────────────────────────────────────┐
│ erlmcp_resource_subscriptions (gen_server)          │
│ - Collects changes (100ms batch window)             │
│ - Applies rate limiting per subscriber              │
│ - Sends notifications to all subscribers            │
└─────────────────┬───────────────────────────────────┘
                  │ send notifications
                  ▼
┌─────────────────────────────────────────────────────┐
│ Subscriber Processes (JSON-RPC transports)          │
│ - Receive {'$mcp_resource', Notification}           │
│ - Encode as JSON-RPC 2.0 notification               │
│ - Send over transport (TCP, HTTP, WebSocket, etc.)  │
└─────────────────────────────────────────────────────┘
```

### 4.3 Change Batching

Rapid successive changes within a 100ms window are batched:

```erlang
%% Configuration in erlmcp_resource_subscriptions.erl
-define(BATCH_WINDOW_MS, 100).  % 100ms batching window

%% Example: Multiple changes within batch window
erlmcp_resource_subscriptions:notify_resource_changed(Uri, #{seq => 1}),
erlmcp_resource_subscriptions:notify_resource_changed(Uri, #{seq => 2}),
erlmcp_resource_subscriptions:notify_resource_changed(Uri, #{seq => 3}),
% Result: Single or multiple notifications sent after 100ms

% After 100ms, notifications are flushed and sent to subscribers
```

**Benefits**:
- Reduces network traffic for rapidly changing resources
- Prevents notification queue buildup
- Maintains responsive updates (100ms latency)
- Optional filtering within batch window

### 4.4 Change Metadata

Servers should include relevant metadata about the change:

```erlang
Metadata = #{
    % Temporal information
    updated_at => erlang:system_time(millisecond),

    % Version information
    version => <<"1.0.0">>,
    revision => 42,

    % Size and checksum
    size => 4096,
    checksum => <<"sha256:abc123...">>,

    % Change description (optional)
    change_type => <<"content_update">>,
    changed_fields => [<<"body">>, <<"updated_at">>],

    % Server-specific fields
    source => <<"file_system_monitor">>,
    confidence => 0.95
}
```

---

## 5. Subscription Lifecycle

### 5.1 Complete Subscription Lifecycle

```
1. CLIENT INITIALIZATION
   │
   ├─► Send resources/subscribe request
   │   └─► params: {uri: "resource://uri"}
   │
2. SERVER PROCESSES SUBSCRIPTION
   │
   ├─► Validate URI format
   │   └─► If invalid: return error -32022
   │
   ├─► Validate subscriber process
   │   ├─► Must be local, alive process
   │   └─► If dead: return error -32005 (no_process)
   │
   ├─► Create subscription entry
   │   └─► Monitor subscriber process
   │
   ├─► Add to resource subscriptions map
   │   └─► Map: Resource URI → Set of Subscriber PIDs
   │
   ├─► Return success response
   │   └─► params: {}
   │
3. SUBSCRIPTION ACTIVE
   │
   ├─► Client sends resources/read for same resource
   │   ├─► Server processes request
   │   └─► Returns resource content
   │
   ├─► Resource changes detected by application
   │   ├─► Application calls notify_resource_updated/3
   │   └─► Subscription system batches change
   │
   ├─► Batch window expires (100ms)
   │   ├─► Check rate limit for subscriber
   │   ├─► Apply message filter (if configured)
   │   └─► Send resources/updated notification
   │
   ├─► Client receives notification
   │   ├─► Method: "resources/updated"
   │   ├─► Params: {uri: "resource://uri", metadata: {...}}
   │   └─► Client may refresh resource via resources/read
   │
4. SUBSCRIPTION TERMINATION
   │
   ├─► CLIENT-INITIATED: Send resources/unsubscribe request
   │   ├─► Server validates subscription exists
   │   ├─► Remove from subscriptions map
   │   ├─► Demonitor subscriber process
   │   ├─► Return success response
   │   └─► No more notifications sent
   │
   └─► PROCESS-DEATH-INITIATED: Subscriber process dies
       ├─► Monitor trap fires (DOWN message)
       ├─► Server removes all subscriptions for dead PID
       ├─► Demonitor reference cleaned up
       └─► Resource cleaned from map if no subscribers
```

### 5.2 State Machine Diagram

```
┌─────────────────────┐
│  NOT_SUBSCRIBED     │
└──────────┬──────────┘
           │
    subscribe request
           │
           ▼
    ┌─────────────────────┐
    │  SUBSCRIBING        │ ◄─── Validate URI
    │  (validation)       │ ◄─── Validate process
    └──────────┬──────────┘
               │
               ├─► Invalid ──► Error Response ──► NOT_SUBSCRIBED
               │
               └─► Valid
                   │
                   ▼
        ┌─────────────────────┐
        │  SUBSCRIBED         │ ◄─── Success Response sent
        │  (active)           │ ◄─── Monitor active
        │                     │ ◄─── Ready for notifications
        └──────────┬──────────┘
                   │
        ┌──────────┴──────────┐
        │                     │
   unsubscribe request    process dies
        │                     │
        ▼                     ▼
    ┌─────────────────────┐  ┌─────────────────────┐
    │  UNSUBSCRIBING      │  │  CLEANUP_ON_DEATH   │
    │  (client-initiated) │  │  (automatic)        │
    └──────────┬──────────┘  └──────────┬──────────┘
               │                        │
               ├─ Demonitor ────────────┤
               ├─ Remove from map ──────┤
               ├─ Success response ─────┤
               │                        │
               ▼                        ▼
        ┌─────────────────────┐
        │  NOT_SUBSCRIBED     │
        └─────────────────────┘
```

### 5.3 State Tracking (Server)

```erlang
%% erlmcp_server.erl state record
-record(state, {
    subscriptions = #{} :: #{uri() => sets:set(pid())},
    % Maps: resource_uri → Set of subscriber PIDs

    resources = #{} :: #{uri() => {mcp_resource, handler()}},
    % Maps: resource_uri → {Resource record, Change handler function}

    resource_templates = #{} :: #{uri_template() => {mcp_resource_template, handler()}},
    % Maps: uri_template → {Template record, Handler function}

    notifier_pid :: pid() | undefined,
    % PID of change notifier process

    ...
}).
```

---

## 6. Server Push Patterns

### 6.1 Push Notification Flow

```
┌──────────────────┐
│   CLIENT         │
│  (Subscriber)    │
└────────┬─────────┘
         │
         │ resources/subscribe request
         ▼
┌──────────────────┐
│   SERVER         │
│  (Publisher)     │
│                  │
│  Maintains:      │
│  - Subscriptions │
│  - Rate limits   │
│  - Batch timer   │
└────────┬─────────┘
         │
         │◄─────────────────────────── Application notifies change
         │  erlmcp_server:notify_resource_updated/3
         │
         ├─ Add to pending_changes
         ├─ Start/reset batch timer (100ms)
         │
         │ (100ms passes)
         │
         ├─ Check rate limit
         ├─ Apply filters
         ├─ Create notification
         │
         ▼
    ┌──────────────────┐
    │   NOTIFICATION   │
    │   resources/     │
    │   updated        │
    └────────┬─────────┘
             │
             │ Send over transport
             ▼
         CLIENT
```

### 6.2 Transport-Agnostic Delivery

The subscription system is transport-agnostic. Notifications are delivered via:

**JSON-RPC 2.0 over Multiple Transports**:

**TCP/WebSocket**:
```
Client ←─► TCP Socket ←─► erlmcp_transport_tcp
Client ←─► WebSocket ←─► erlmcp_transport_ws
```

**HTTP/Server-Sent Events**:
```
Client ←─ HTTP GET /events ─► erlmcp_transport_sse
         (Long-poll or SSE)
```

**STDIO**:
```
Process ←─► STDIN/STDOUT ←─► erlmcp_transport_stdio
```

**Implementation Detail** (in erlmcp_resource_subscriptions.erl):
```erlang
%% Send notification to all subscribers
-spec notify_subscribers(uri(), change_notification(), [subscriber()]) -> ok.
notify_subscribers(_Uri, _ChangeNotification, []) -> ok;
notify_subscribers(Uri, ChangeNotification, [Subscriber | Rest]) ->
    Notification = #{
        jsonrpc => <<"2.0">>,
        method => <<"resources/updated">>,
        params => #{
            uri => maps:get(uri, ChangeNotification),
            timestamp => maps:get(timestamp, ChangeNotification)
        }
    },

    try
        % Send via Erlang message to subscriber process
        Subscriber ! {'$mcp_resource', Notification},
        notify_subscribers(Uri, ChangeNotification, Rest)
    catch _:_ ->
        % Subscriber died - cleanup will happen via DOWN
        notify_subscribers(Uri, ChangeNotification, Rest)
    end.
```

### 6.3 Backpressure Handling

**Problem**: What if subscribers can't keep up with notifications?

**Solutions Implemented**:

1. **Rate Limiting**: Per-subscriber configurable limits
   ```erlang
   Options = #{rate_limit => 1000},  % 1 notification per second
   erlmcp_resource_subscriptions:subscribe_to_resource(Uri, Subscriber, Options)
   ```

2. **Batching**: Rapid changes within 100ms window are coalesced
   ```erlang
   -define(BATCH_WINDOW_MS, 100).
   % Result: Fewer notifications, reduced network load
   ```

3. **Fire-and-Forget**: Notifications are sent asynchronously
   - Server doesn't wait for client confirmation
   - Lost notifications due to client overload are acceptable
   - Client can always refresh via `resources/read` request

4. **Filtering**: Optional per-subscriber filters
   ```erlang
   Filter = fun(Notification) ->
       maps:get(size, Notification) > 1000
   end,
   Options = #{filter => Filter},
   erlmcp_resource_subscriptions:subscribe_to_resource(Uri, Subscriber, Options)
   ```

---

## 7. Client Notification Handling

### 7.1 Client Subscription API

```erlang
%% Client-side subscription (if running as erlmcp process)
ok = erlmcp_client:subscribe_resource(ClientPid, <<"resource://uri">>).

%% Receive notifications in client mailbox
receive
    {'$mcp_resource', Notification} ->
        Uri = maps:get(uri, maps:get(params, Notification)),
        logger:info("Resource ~p updated", [Uri])
after 5000 ->
    timeout
end.
```

### 7.2 Notification Reception Patterns

**Pattern 1: Simple Polling**
```erlang
%% Wait for single notification
receive
    {'$mcp_resource', #{method := <<"resources/updated">>, params := Params}} ->
        Uri = maps:get(uri, Params),
        {ok, Content} = erlmcp_server:read_resource(Server, Uri),
        process_updated_resource(Uri, Content)
after 30000 ->
    handle_timeout()
end.
```

**Pattern 2: Notification Loop**
```erlang
notification_loop(State) ->
    receive
        {'$mcp_resource', #{method := <<"resources/updated">>, params := Params}} ->
            handle_resource_update(Params, State),
            notification_loop(State);

        stop ->
            ok;

        Other ->
            logger:warning("Unknown message: ~p", [Other]),
            notification_loop(State)
    after 30000 ->
        % Refresh all subscriptions periodically
        refresh_all_subscriptions(),
        notification_loop(State)
    end.
```

**Pattern 3: Rate-Limited Processing**
```erlang
notification_loop(State) ->
    receive
        {'$mcp_resource', Notification} ->
            % Coalesce rapid notifications
            Notifications = collect_all_notifications([Notification]),

            % Process batch
            lists:foreach(fun(Notif) ->
                process_update(Notif)
            end, Notifications),

            notification_loop(State)
    after 5000 ->
        % Batch window expired - process accumulated changes
        notification_loop(State)
    end.
```

### 7.3 Error Handling

**Subscription Failure**:
```erlang
case erlmcp_client:subscribe_resource(ClientPid, Uri) of
    ok ->
        logger:info("Subscribed to ~p", [Uri]);
    {error, {?JSONRPC_INVALID_PARAMS, Msg, Data}} ->
        logger:error("Invalid URI: ~p", [Data]);
    {error, {?MCP_ERROR_SUBSCRIPTION_FAILED, Msg, _}} ->
        logger:error("Subscription failed: ~p", [Msg])
end.
```

**Connection Loss**:
```erlang
% Client is monitoring server process
monitor(process, ServerPid),

receive
    {'DOWN', _Ref, process, ServerPid, Reason} ->
        logger:error("Server disconnected: ~p", [Reason]),
        % Resubscribe on reconnection
        reconnect_and_resubscribe()
after infinity ->
    ok
end.
```

---

## 8. Real-Time Update Semantics

### 8.1 Delivery Guarantees

| Guarantee | Supported | Details |
|-----------|-----------|---------|
| **At-most-once** | ✓ Yes | Notifications sent once; loss possible on network failure |
| **At-least-once** | ✗ No | No deduplication or retransmission |
| **Exactly-once** | ✗ No | Not guaranteed by MCP protocol |
| **Order** | ✗ No | Multiple notifications may arrive out of order |
| **FIFO** | ⚠ Mostly | Within batch window, order is preserved; across windows, no guarantee |

### 8.2 Latency Characteristics

**End-to-End Latency** (milliseconds):

| Component | Latency | Notes |
|-----------|---------|-------|
| Change detection | ~1ms | Application-dependent |
| Batching window | ~100ms | Configurable in erlmcp_resource_subscriptions |
| Rate limiting check | ~1ms | Token bucket lookup |
| Notification encoding | ~1-5ms | JSON encoding overhead |
| Transport send | 1-100ms | Depends on transport (TCP/WS/SSE) |
| **Total (typical)** | **103-207ms** | In-process: ~1-10ms, Remote: ~100-200ms |

**Optimization Tips**:
- Reduce batch window for lower latency (may increase CPU)
- Adjust rate limits to balance load vs responsiveness
- Use TCP for lowest latency (vs HTTP/SSE)
- Coalesce frequent changes on client side

### 8.3 Consistency Model

**Eventual Consistency**:
- Client eventually learns about resource changes
- No ordering guarantee between multiple resources
- Client should tolerate out-of-order notifications
- Implement version/checksum checking if strict ordering needed

**Example**: Temperature resource updates
```
Time  | Notification Received | Value
------|----------------------|-------
0ms   | None
50ms  | resources/updated    | 72°F
100ms | resources/updated    | 73°F
150ms | (lost on network)    | 74°F ✗
200ms | resources/updated    | 75°F
```

**Client Recovery**:
```erlang
% Instead of relying solely on notifications:
% - Track last known version from notification
% - Periodically call resources/read to verify state
% - Handle out-of-order updates using version numbers

case Notification of
    #{version := NewVersion} when NewVersion > CurrentVersion ->
        % Process update
        process_update(Notification),
        UpdatedState = State#{current_version => NewVersion};
    #{version := OldVersion} when OldVersion < CurrentVersion ->
        % Out-of-order update - ignore
        State;
    _ ->
        % No version - accept anyway
        process_update(Notification)
end.
```

### 8.4 Throughput Characteristics

**Maximum Notification Rate** (per resource, single server):

| Scenario | Rate | Notes |
|----------|------|-------|
| Single subscriber | 1000/sec | Limited by rate limiting (configurable) |
| 10 subscribers | 100/sec each | Rate limit shared or per-subscriber |
| 100 subscribers | 10/sec each | Network becomes bottleneck |
| Large payload | 1-10/sec | Depends on transport, network |

**Optimization**: Use URI templates for template subscriptions
```erlang
% Inefficient: Subscribe to each user individually
erlmcp_server:subscribe_resource(Server, <<"user://alice">>, Sub1),
erlmcp_server:subscribe_resource(Server, <<"user://bob">>, Sub2),
erlmcp_server:subscribe_resource(Server, <<"user://charlie">>, Sub3),

% Efficient: Use URI template
erlmcp_server:add_resource_template(Server, <<"user://{username}">>,
    <<"User Profile">>, Handler),
erlmcp_server:subscribe_resource(Server, <<"user://{username}">>, SingleSub),
% Single subscription matches all user URIs
```

---

## 9. Configuration and Tuning

### 9.1 Rate Limiting Configuration

```erlang
%% Global default rate limit (1000ms = 1 notification/second)
-define(DEFAULT_RATE_LIMIT, 1000).

%% Per-subscription rate limit (in milliseconds)
Options = #{
    rate_limit => 500  % 2 notifications per second for this subscriber
},
erlmcp_resource_subscriptions:subscribe_to_resource(Uri, Subscriber, Options).

%% Set rate limit for all subscribers of a resource
ok = erlmcp_resource_subscriptions:set_rate_limit(Uri, 2000).  % 1 per 2 seconds
```

### 9.2 Batch Window Configuration

```erlang
%% In erlmcp_resource_subscriptions.erl
-define(BATCH_WINDOW_MS, 100).  % 100ms batching window

%% To change: Recompile with different value
-define(BATCH_WINDOW_MS, 50).   % Lower latency, higher CPU
-define(BATCH_WINDOW_MS, 200).  % Higher latency, lower CPU
```

### 9.3 Filter Configuration

```erlang
%% Size-based filter (only notify for large changes)
Filter = fun(#{metadata := #{size := Size}}) ->
    Size > 10000  % Only notify if > 10KB
end,

Options = #{filter => Filter},
erlmcp_resource_subscriptions:subscribe_to_resource(Uri, Subscriber, Options).

%% Time-based filter (throttle rapid updates)
LastUpdateRef = erlang:make_ref(),
Filter = fun(#{metadata := #{updated_at := Time}}) ->
    Now = erlang:system_time(millisecond),
    (Now - Time) > 5000  % Only notify if > 5 seconds passed
end.
```

---

## 10. Error Codes and Handling

### 10.1 Subscription-Related Error Codes

| Code | Name | Description | Recovery |
|------|------|-------------|----------|
| -32022 | `INVALID_URI` | URI format invalid | Validate URI format before subscribing |
| -32005 | `NOT_INITIALIZED` | Subscriber not alive | Ensure process is running |
| -32006 | `SUBSCRIPTION_FAILED` | Subscription creation failed | Check server logs, retry |
| -32001 | `RESOURCE_NOT_FOUND` | Resource doesn't exist | Create resource first |
| -32004 | `CAPABILITY_NOT_SUPPORTED` | Server doesn't support subscriptions | Check `subscribe` capability in initialize response |

### 10.2 Notification Error Handling

```erlang
%% Client receives error in response (not a notification)
receive
    {error, {Code, Message}} ->
        case Code of
            -32005 -> % NOT_INITIALIZED
                logger:error("Server not initialized: ~p", [Message]);
            -32006 -> % SUBSCRIPTION_FAILED
                logger:error("Subscription failed: ~p", [Message]);
            -32001 -> % RESOURCE_NOT_FOUND
                logger:error("Resource not found: ~p", [Message]);
            Other ->
                logger:error("Unknown error ~p: ~p", [Other, Message])
        end
after 5000 ->
    timeout
end.
```

---

## 11. Example Implementation

### 11.1 Server Implementation

```erlang
-module(example_weather_server).
-behaviour(gen_server).

-export([start_link/0, init/1, handle_call/3, handle_cast/2]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, Server} = erlmcp_server:start_link(
        weather_service,
        #mcp_server_capabilities{
            resources = #mcp_resources_capability{
                subscribe = true,
                listChanged = false
            }
        }
    ),

    % Register resource
    ok = erlmcp_server:add_resource(Server,
        <<"weather://city/san-francisco">>,
        fun(Uri) -> fetch_weather(Uri) end
    ),

    % Start weather monitor (external process)
    spawn_link(fun() -> weather_monitor(Server) end),

    {ok, #{server => Server}}.

weather_monitor(Server) ->
    % Check weather every 30 seconds
    timer:sleep(30000),

    % Get current weather
    Weather = fetch_weather(<<"weather://city/san-francisco">>),

    % Notify subscribers
    ok = erlmcp_server:notify_resource_updated(Server,
        <<"weather://city/san-francisco">>,
        #{
            updated_at => erlang:system_time(millisecond),
            temperature => maps:get(temperature, Weather),
            humidity => maps:get(humidity, Weather),
            condition => maps:get(condition, Weather)
        }
    ),

    weather_monitor(Server).

fetch_weather(Uri) ->
    % Fetch from weather API, cache, etc.
    #{
        temperature => 72.5,
        humidity => 65,
        condition => <<"Partly Cloudy">>,
        updated_at => erlang:system_time(millisecond)
    }.
```

### 11.2 Client Implementation

```erlang
-module(example_weather_client).

-export([subscribe_to_weather/1, start_listening/0]).

subscribe_to_weather(Uri) ->
    % Subscribe to weather updates
    ok = erlmcp_client:call(weather_client,
        <<"resources/subscribe">>,
        #{<<"uri">> => Uri}
    ),
    logger:info("Subscribed to ~p", [Uri]).

start_listening() ->
    % Listen for weather updates
    listen_loop().

listen_loop() ->
    receive
        {'$mcp_resource', #{method := <<"resources/updated">>, params := Params}} ->
            Uri = maps:get(<<"uri">>, Params),
            Metadata = maps:get(<<"metadata">>, Params, #{}),

            Temperature = maps:get(<<"temperature">>, Metadata, <<"N/A">>),
            Humidity = maps:get(<<"humidity">>, Metadata, <<"N/A">>),
            Condition = maps:get(<<"condition">>, Metadata, <<"N/A">>),

            logger:info("Weather update for ~p:", [Uri]),
            logger:info("  Temperature: ~p°F", [Temperature]),
            logger:info("  Humidity: ~p%", [Humidity]),
            logger:info("  Condition: ~p", [Condition]),

            listen_loop();

        Other ->
            logger:warning("Unknown message: ~p", [Other]),
            listen_loop()
    after 60000 ->
        % Timeout - verify subscription still active
        logger:info("Still listening for weather updates..."),
        listen_loop()
    end.
```

---

## 12. Testing and Validation

### 12.1 Unit Tests

```erlang
%% Test subscription creation
subscribe_to_resource_test() ->
    {ok, _Pid} = erlmcp_resource_subscriptions:start_link(),
    Uri = <<"file:///test/resource.txt">>,
    Subscriber = self(),

    ?assertEqual(ok, erlmcp_resource_subscriptions:subscribe_to_resource(
        Uri, Subscriber, #{}
    )),

    Subscriptions = erlmcp_resource_subscriptions:list_resource_subscriptions(
        Uri, false
    ),
    ?assert(lists:member(Subscriber, Subscriptions)).

%% Test notification delivery
notify_resource_changed_test() ->
    {ok, _Pid} = erlmcp_resource_subscriptions:start_link(),
    Uri = <<"file:///notify/test.txt">>,
    Subscriber = self(),

    erlmcp_resource_subscriptions:subscribe_to_resource(Uri, Subscriber, #{}),

    erlmcp_resource_subscriptions:notify_resource_changed(Uri, #{changed => true}),
    timer:sleep(150),  % Wait for batch window

    receive
        {'$mcp_resource', Notification} ->
            ?assertEqual(<<"resources/updated">>, maps:get(method, Notification))
    after 1000 ->
        ?assert(false, "No notification received")
    end.
```

### 12.2 Integration Tests

See: `/home/user/erlmcp/apps/erlmcp_core/test/erlmcp_resource_subscriptions_tests.erl`

### 12.3 Stress Testing

```erlang
%% Create 1000 subscriptions, send 100 notifications/sec
stress_test() ->
    {ok, _Pid} = erlmcp_resource_subscriptions:start_link(),

    % Create subscribers
    Subscribers = [spawn(fun() -> receive after infinity -> ok end end)
                   || _ <- lists:seq(1, 1000)],

    % Subscribe to same resource
    Uri = <<"stress/test">>,
    lists:foreach(fun(Sub) ->
        erlmcp_resource_subscriptions:subscribe_to_resource(Uri, Sub, #{})
    end, Subscribers),

    % Send rapid notifications
    lists:foreach(fun(N) ->
        erlmcp_resource_subscriptions:notify_resource_changed(Uri, #{seq => N}),
        timer:sleep(10)  % 100/sec
    end, lists:seq(1, 1000)),

    timer:sleep(1000).
```

---

## 13. Best Practices

### 13.1 Server-Side

1. **Always include timestamps** in change metadata
2. **Version resources** for client-side reconciliation
3. **Filter at server** rather than client (more efficient)
4. **Use URI templates** for bulk subscriptions
5. **Implement change detection** externally (file monitor, database trigger)
6. **Monitor subscription counts** in observability/metrics
7. **Clean up resources** when all subscribers unsubscribe

### 13.2 Client-Side

1. **Always handle timeouts** in notification reception
2. **Verify versions** of notifications before processing
3. **Don't trust notification delivery** - use polling backup
4. **Implement exponential backoff** for subscription failures
5. **Batch client-side processing** to reduce overhead
6. **Monitor subscription health** (missing updates?)
7. **Gracefully handle server disconnection** and resubscribe

### 13.3 Error Handling

1. **Wrap subscription calls** in try-catch
2. **Log subscription failures** with context
3. **Implement retry logic** with backoff
4. **Use error codes** to determine recovery strategy
5. **Monitor error rates** for operational health
6. **Alert on subscription loss** (process died, etc.)

---

## 14. Summary Table

| Aspect | Details |
|--------|---------|
| **Protocol** | MCP 2025-11-25, JSON-RPC 2.0 |
| **Subscription Method** | `resources/subscribe` RPC |
| **Notification Method** | `resources/updated` (async notification) |
| **Batch Window** | 100ms (configurable) |
| **Default Rate Limit** | 1000ms (1 notification/sec) |
| **Delivery Guarantee** | At-most-once |
| **Ordering Guarantee** | None (out-of-order acceptable) |
| **Deduplication** | None (duplicates possible) |
| **Cleanup Trigger** | Process death (automatic) or explicit unsubscribe |
| **Max Subscriptions** | Unlimited (limited by memory) |
| **Max Subscribers/Resource** | Unlimited (limited by memory) |
| **Transport Support** | TCP, WebSocket, HTTP, STDIO |

---

## References

- **MCP Specification**: https://spec.modelcontextprotocol.io/ (v2025-11-25)
- **JSON-RPC 2.0**: https://www.jsonrpc.org/specification
- **Erlang Monitoring**: http://erlang.org/doc/man/erlang.html#monitor-2
- **erlmcp Source**: `/home/user/erlmcp/apps/erlmcp_core/src/`
  - `erlmcp_resource_subscriptions.erl`
  - `erlmcp_subscription.erl`
  - `erlmcp_server.erl`
- **Tests**: `/home/user/erlmcp/apps/erlmcp_core/test/erlmcp_resource_subscriptions_tests.erl`
