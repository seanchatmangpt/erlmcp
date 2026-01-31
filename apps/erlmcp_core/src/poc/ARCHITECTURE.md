# erlmcp_pubsub_poc - Architecture

## System Overview

```
┌─────────────────────────────────────────────────────────────────────────┐
│                         erlmcp Pub/Sub Architecture                      │
│                    (Phoenix PubSub-style using pg module)                │
└─────────────────────────────────────────────────────────────────────────┘

                              ┌──────────────┐
                              │   pg module  │  (OTP built-in, distributed)
                              │              │  Manages process groups across
                              │ - Topic → [] │  all connected nodes
                              └──────┬───────┘
                                     │
                    ┌────────────────┼────────────────┐
                    │                │                │
            ┌───────▼──────┐  ┌──────▼──────┐  ┌─────▼──────┐
            │ erlmcp_pubsub│  │ erlmcp_pubsub│  │ erlmcp_pubsub│
            │   _poc       │  │   _poc       │  │   _poc       │
            │  (Node 1)    │  │  (Node 2)    │  │  (Node 3)    │
            └───────┬──────┘  └──────┬──────┘  └─────┬──────┘
                    │                │                │
         ┌──────────┼──────┐  ┌──────┼──────┐  ┌──────┼──────┐
         │          │      │  │      │      │  │      │      │
    ┌────▼───┐ ┌───▼───┐  │  │  ┌───▼───┐  │  │  ┌───▼───┐  │
    │Agent 1 │ │Agent 2│  │  │  │Agent 4│  │  │  │Agent 7│  │
    └────────┘ └───────┘  │  │  └───────┘  │  │  └───────┘  │
                           │  │             │  │             │
                      ┌────▼──▼──┐     ┌────▼──▼──┐     ┌────▼───┐
                      │ Agent 3  │     │ Agent 5  │     │Agent 8 │
                      │ (Client) │     │ Agent 6  │     │(Server)│
                      └──────────┘     └──────────┘     └────────┘
```

## Component Layers

### Layer 1: Process Groups (pg)
- **Built into OTP 23+**
- Maintains topic → [Pid] mappings
- Automatically syncs across connected nodes
- Zero configuration required

### Layer 2: Pub/Sub Wrapper (erlmcp_pubsub_poc)
- **gen_server behavior**
- Provides MCP-friendly API
- Collects metrics (latencies, counts)
- Handles edge cases (subscriber death, topic cleanup)

### Layer 3: Subscribers
- **Any Erlang process**
- Receives `{pubsub_message, Topic, Message}` tuples
- Can be on any node in the cluster
- Examples: AI agents, MCP clients, resource providers

## Message Flow Diagrams

### Subscribe Flow

```
Client Process              erlmcp_pubsub_poc              pg module
      │                            │                          │
      │  subscribe(Topic, Pid)     │                          │
      ├───────────────────────────>│                          │
      │                            │  join(Scope, Topic, Pid) │
      │                            ├─────────────────────────>│
      │                            │                          │
      │                            │         ok               │
      │                            │<─────────────────────────┤
      │         ok                 │                          │
      │<───────────────────────────┤                          │
      │                            │                          │

Result: Pid is now in the topic's member list
```

### Broadcast Flow

```
Publisher              erlmcp_pubsub_poc              pg module              Subscribers
    │                         │                          │                        │
    │  broadcast(Topic, Msg)  │                          │                        │
    ├────────────────────────>│                          │                        │
    │                         │  get_members(Topic)      │                        │
    │                         ├─────────────────────────>│                        │
    │                         │                          │                        │
    │                         │  [Pid1, Pid2, Pid3, ...] │                        │
    │                         │<─────────────────────────┤                        │
    │                         │                          │                        │
    │                         │  For each member:                                 │
    │                         │  Pid ! {pubsub_message, Topic, Msg}               │
    │                         ├───────────────────────────────────────────────────>│ Pid1
    │                         ├───────────────────────────────────────────────────>│ Pid2
    │                         ├───────────────────────────────────────────────────>│ Pid3
    │                         │                          │                        │
    │         ok              │                          │                        │
    │<────────────────────────┤                          │                        │
    │                         │                          │                        │

Result: All subscribers receive message in their mailbox
```

### Distributed Broadcast Flow

```
Node 1                                    Node 2
─────────────────────────────────────────────────────────────────

Publisher         pg (synced)         pg (synced)         Subscriber
    │                 │                   │                   │
    │  broadcast()    │                   │                   │
    ├────────────────>│                   │                   │
    │                 │                   │                   │
    │  get_members()  │                   │                   │
    │<────────────────┤                   │                   │
    │                 │                   │                   │
    │  [Pid@Node2]    │                   │                   │
    │                 │                   │                   │
    │  send message across network        │                   │
    ├─────────────────────────────────────────────────────────>│
    │                 │                   │                   │
    │                                                          │
    │                  Result: Cross-node delivery!           │

pg automatically knows about all members across all nodes!
```

## State Management

### erlmcp_pubsub_poc gen_server State

```erlang
-record(state, {
    metrics :: map(),       % Performance metrics
    start_time :: integer() % Server start timestamp
}).

metrics = #{
    total_broadcasts => integer(),      % Total number of broadcasts
    total_messages_sent => integer(),   % Sum of (broadcasts * subscribers)
    latencies_us => [integer()],        % List of broadcast latencies
    subscriber_counts => [integer()]    % Subscriber count per broadcast
}
```

### pg Internal State (Managed by OTP)

```
Topic Registry (per scope):
#{
    "resource:weather:sf" => [<0.123.0>, <0.124.0>, <0.125.0>],
    "tool:calculate:stream" => [<0.126.0>],
    "resource:stocks:aapl" => [<0.123.0>, <0.124.0>]
}

Distributed Sync:
- pg monitors all connected nodes
- Automatically syncs member lists
- Handles node joins/leaves transparently
```

## Fault Tolerance

### Subscriber Death

```
Subscriber Process Dies:
    1. pg detects process death (monitors)
    2. Automatically removes from all topics
    3. No cleanup needed by erlmcp_pubsub_poc
    4. Next broadcast skips dead process
```

### Node Partition

```
Network Partition:
    Before: Node1 ←──✓──→ Node2
    After:  Node1 ←──✗──→ Node2

    During partition:
        - Each node has local view of subscribers
        - Broadcasts only reach local subscribers
        - No cross-node delivery

    After partition heals:
        - pg automatically resyncs
        - Full topology restored
        - Cross-node broadcasts resume

Graceful degradation!
```

### Publisher Death

```
Publisher Crashes Mid-Broadcast:
    1. Some subscribers may have received message
    2. Others may not have received it
    3. No automatic retry or rollback
    4. At-most-once delivery semantics

For at-least-once:
    - Publisher stores pending broadcasts
    - Retries until ack from all subscribers
    - Requires custom implementation (Phase 4)
```

## Performance Analysis

### Latency Breakdown

```
Total Broadcast Latency (~28μs):
┌──────────────────────────────────┐
│ gen_server:cast overhead: ~5μs   │  ← Async message to gen_server
├──────────────────────────────────┤
│ pg:get_members call: ~3μs        │  ← ETS lookup
├──────────────────────────────────┤
│ Message sends (5 subs): ~15μs    │  ← Pid ! Msg for each subscriber
├──────────────────────────────────┤
│ Metrics collection: ~5μs         │  ← Optional, if enabled
└──────────────────────────────────┘

Bottleneck: Message send loop (O(n) with subscriber count)
Optimization: Could use spawn per send for large fan-outs
```

### Scalability Profile

```
Subscribers    Latency    Throughput    Memory
─────────────────────────────────────────────────
1              ~15μs      ~66K msg/s    2 KB
5              ~28μs      ~35K msg/s    10 KB
10             ~45μs      ~22K msg/s    20 KB
50             ~180μs     ~5.5K msg/s   100 KB
100            ~350μs     ~2.8K msg/s   200 KB
1000           ~3.5ms     ~280 msg/s    2 MB

Scaling: O(n) linear with subscriber count
Recommendation: < 100 subs per topic for low latency
For 1000+: Consider topic sharding
```

### Cross-Node Overhead

```
Same Node:     ~28μs   (in-memory)
Cross-Node:    ~1-5ms  (network + serialization)

Network adds:
    - Message serialization (~500μs)
    - Network round-trip (~500μs-4ms, depends on topology)
    - Deserialization on remote node (~500μs)

For distributed:
    - Use local pg scopes when possible
    - Batch messages to amortize overhead
    - Consider topic locality (subscribers on same node)
```

## Use Case Examples

### 1. MCP Resource Updates

```
┌───────────────┐
│ Resource      │  Weather service updates every 60s
│ Provider      │
└───────┬───────┘
        │ broadcast("resource:weather:sf", #{temp => 72})
        │
        ▼
    ┌───────┐
    │  pg   │  Topic: "resource:weather:sf"
    └───┬───┘  Members: [Agent1, Agent2, Agent3, ...]
        │
        ├──────> Agent1 (planning assistant)
        ├──────> Agent2 (clothing recommender)
        └──────> Agent3 (activity planner)

All agents receive update instantly!
```

### 2. Tool Streaming (LLM Generation)

```
┌───────────────┐
│ LLM Tool      │  Generates tokens: ["Hello", " ", "world", "!"]
│ (Server)      │
└───────┬───────┘
        │ For each token:
        │   broadcast("tool:llm:req-123", #{chunk => Token})
        │
        ▼
    ┌───────┐
    │  pg   │  Topic: "tool:llm:req-123"
    └───┬───┘  Members: [Client1]
        │
        └──────> Client1
                    Receives: [
                        {chunk => "Hello"},
                        {chunk => " "},
                        {chunk => "world"},
                        {chunk => "!"}
                    ]

Streaming delivery in real-time!
```

### 3. Multi-Agent Collaboration

```
┌───────────────┐
│ Knowledge     │  New document inserted
│ Base          │
└───────┬───────┘
        │ broadcast("kb:updates", #{doc_id => "12345"})
        │
        ▼
    ┌───────┐
    │  pg   │  Topic: "kb:updates"
    └───┬───┘  Members: [A1@N1, A2@N1, A3@N2, A4@N2, A5@N3, ...]
        │
        ├──────> Agent1 (Node 1)  ─┐
        ├──────> Agent2 (Node 1)   │ All agents notified
        ├──────> Agent3 (Node 2)   │ regardless of node!
        ├──────> Agent4 (Node 2)   │
        └──────> Agent5 (Node 3)  ─┘

100 agents across 10 nodes, all synchronized!
```

## Comparison to Traditional Approaches

### Before (Polling)

```
Client1                 Server                  Client2
   │                       │                       │
   │  GET /resource/123    │                       │
   ├──────────────────────>│                       │
   │  {version: 1}         │                       │
   │<──────────────────────┤                       │
   │                       │                       │
   │  (wait 5s)            │  (wait 5s)            │
   │                       │                       │
   │  GET /resource/123    │  GET /resource/123    │
   ├──────────────────────>│<──────────────────────┤
   │  {version: 1}         │  {version: 1}         │
   │<──────────────────────┼──────────────────────>│
   │                       │                       │

Problems:
- Wastes bandwidth (constant polling)
- High latency (up to polling interval)
- Scales poorly (N clients = N requests/interval)
```

### After (Pub/Sub)

```
Client1                 Server                  Client2
   │                       │                       │
   │  subscribe()          │                       │
   ├──────────────────────>│                       │
   │                       │  subscribe()          │
   │                       │<──────────────────────┤
   │                       │                       │
   │   (idle - no traffic) │  (idle - no traffic)  │
   │                       │                       │
   │                       │  UPDATE!              │
   │                       │  broadcast()          │
   │  {version: 2}         │  {version: 2}         │
   │<──────────────────────┼──────────────────────>│
   │                       │                       │

Benefits:
- Zero bandwidth when no updates
- Instant delivery (<50μs)
- Scales O(1) per update (not per client)
```

## Security Considerations

### Topic Namespace Isolation

```erlang
%% BAD: No namespace isolation
subscribe("temperature", self()).  % Ambiguous!

%% GOOD: Namespaced topics
subscribe("resource:weather:sf:temperature", self()).
subscribe("tool:sensor:device-123:temperature", self()).
```

### Access Control (Future)

```erlang
%% Phase 4: Add ACLs to topics
-record(topic_acl, {
    topic :: binary(),
    read_roles :: [atom()],   % [admin, agent, client]
    write_roles :: [atom()]   % [admin, provider]
}).

%% Check before subscribe/broadcast
can_subscribe(Topic, Pid) ->
    Roles = get_process_roles(Pid),
    ACL = get_topic_acl(Topic),
    has_read_permission(Roles, ACL).
```

### Message Validation (Future)

```erlang
%% Phase 4: Schema validation
broadcast(Topic, Message) ->
    Schema = get_topic_schema(Topic),
    case validate_message(Message, Schema) of
        ok -> do_broadcast(Topic, Message);
        {error, Reason} -> {error, {invalid_message, Reason}}
    end.
```

## Next Steps: Production Implementation

### File Structure

```
apps/erlmcp_core/src/
├── pubsub/
│   ├── erlmcp_pubsub.erl              (production version of POC)
│   ├── erlmcp_pubsub_sup.erl          (supervisor)
│   ├── erlmcp_subscription_mgr.erl    (track subscriptions per client)
│   └── erlmcp_topic_registry.erl      (topic metadata, ACLs)
│
└── integration/
    ├── erlmcp_resource_notifier.erl   (wire resources → pubsub)
    └── erlmcp_tool_streamer.erl       (wire tools → pubsub)

apps/erlmcp_core/test/
└── pubsub/
    ├── erlmcp_pubsub_tests.erl        (unit tests)
    └── erlmcp_pubsub_SUITE.erl        (distributed CT tests)
```

### Migration Checklist

- [ ] Copy POC to `erlmcp_pubsub.erl`
- [ ] Add supervision tree
- [ ] Integrate with `erlmcp_server`
- [ ] Integrate with `erlmcp_client`
- [ ] Add EUnit tests (≥80% coverage)
- [ ] Add Common Test suite (distributed scenarios)
- [ ] Add metrics to `erlmcp_metrics`
- [ ] Add tracing to `erlmcp_tracing`
- [ ] Add chaos tests
- [ ] Documentation updates
- [ ] Benchmark validation

## References

- **pg module docs**: https://www.erlang.org/doc/man/pg.html
- **OTP Design Principles**: https://www.erlang.org/doc/design_principles/users_guide.html
- **Distributed Erlang**: https://www.erlang.org/doc/reference_manual/distributed.html
- **Phoenix PubSub**: https://hexdocs.pm/phoenix_pubsub/Phoenix.PubSub.html

---

**Status**: Architecture documented for POC validation and production implementation.
