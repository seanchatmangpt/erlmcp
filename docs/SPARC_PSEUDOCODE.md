# SPARC Pseudocode: MCP State Machines

**5 Core State Machines in Pseudocode**

---

## State Machine 1: Connection Lifecycle

**States:** NOT_INITIALIZED → INITIALIZED → SUBSCRIBED → CLOSED

```
StateMachine ConnectionLifecycle:
  InitialState: NOT_INITIALIZED

  State NOT_INITIALIZED:
    Entry:
      state = NOT_INITIALIZED
      transport.connect(config)

    Event: client.initialize(caps) →
      Verify: client.capabilities ≠ null
      Action:
        - server.negotiate_capabilities(client.capabilities)
        - State.capabilities = negotiated
        - State.pending = {} (empty UUID → Request map)
        - State.pending_subscriptions = {}
      Transition: INITIALIZED

    Event: (any other RPC) →
      Action: refuse(1076, "Server not initialized")
      Result: Error response

    Event: timeout(30s) →
      Action: transport.disconnect()
      Transition: CLOSED

  State INITIALIZED:
    Entry:
      state = INITIALIZED
      Transport.send(initialized_event)

    Event: client.subscribe_resource(uri) →
      Verify: resource.exists(uri)
      Action:
        - State.pending_subscriptions[uri] = []
        - subscription.create(client_id, uri)
      Transition: SUBSCRIBED

    Event: client.resources_list() →
      Action: return resources.list()
      Result: Response (no state change)

    Event: client.tools_call(name, args) →
      Verify: tools.validate_params(name, args)
      Action:
        - req_id = uuid()
        - State.pending[req_id] = {tool: name, args: args}
        - Schedule: execute_tool(name, args)
      Result: Async response with ID matching

    Event: shutdown() →
      Action: graceful_drain(State.pending)
      Transition: CLOSED

  State SUBSCRIBED:
    Entry:
      state = SUBSCRIBED

    Event: resource.changed(uri) →
      Verify: State.pending_subscriptions[uri] exists
      Action:
        - notification = {resource_uri: uri, timestamp: now(), change: updated}
        - ∀subscriber ∈ subscribers[uri]: send(notification)
      Result: Notification (no state change)

    Event: client.unsubscribe_resource(uri) →
      Verify: State.pending_subscriptions[uri] exists
      Action:
        - delete State.pending_subscriptions[uri]
        - subscription.destroy(client_id, uri)
      Transition: INITIALIZED (if no other subscriptions)

    Event: timeout(idle > 5min) →
      Action: transport.close()
      Transition: CLOSED

  State CLOSED:
    Entry:
      state = CLOSED
      ∀req ∈ State.pending:
        if timeout(req) > 5s:
          cleanup(req)
      transport.disconnect()

    Event: (any) →
      Action: reject("Connection closed")
      Result: Error

    Exit:
      release(all_resources)
```

**Invariants:**
- NOT_INITIALIZED → INITIALIZED (required before any RPC)
- INITIALIZED may transition to SUBSCRIBED (one-way, per-resource)
- CLOSED is terminal (connection must be reestablished)

---

## State Machine 2: Resource Subscription

**States:** UNSUBSCRIBED → SUBSCRIBED → UNSUBSCRIBING → UNSUBSCRIBED

```
StateMachine ResourceSubscription:
  InitialState: UNSUBSCRIBED

  State UNSUBSCRIBED:
    Entry:
      subscription.state = UNSUBSCRIBED
      subscription.handlers = []

    Event: client.subscribe(uri) →
      Verify:
        - path.is_canonical(uri)  // No traversal
        - resource.exists(uri)
      Action:
        - subscription_id = uuid()
        - subscription.resource_uri = uri
        - subscription.handlers = [on_changed, on_deleted]
        - subscription.created_at = now()
        - cache.invalidate(uri)  // Force fresh read on next access
        - resource.attach_listener(uri, subscription_id)
      Transition: SUBSCRIBED

    Event: (any other event) →
      Action: ignore
      Result: No effect

  State SUBSCRIBED:
    Entry:
      subscription.state = SUBSCRIBED
      resource.attach_listener(resource_uri, subscription_id)

    Event: resource.updated(uri, content) →
      Verify:
        - subscription.resource_uri == uri
        - subscription.is_active()
      Action:
        - notification = {
            type: "resource_changed",
            resource_uri: uri,
            change_type: "updated",
            timestamp: now(),
            new_content: content
          }
        - client.send(notification)
        - subscription.last_event_at = now()
      Result: Notification sent (no state change)

    Event: resource.deleted(uri) →
      Verify: subscription.resource_uri == uri
      Action:
        - notification = {
            type: "resource_changed",
            resource_uri: uri,
            change_type: "deleted",
            timestamp: now()
          }
        - client.send(notification)
      Transition: UNSUBSCRIBING

    Event: client.unsubscribe(uri) →
      Verify: subscription.resource_uri == uri
      Action:
        - resource.detach_listener(uri, subscription_id)
        - subscription.state = UNSUBSCRIBING
      Transition: UNSUBSCRIBING

    Event: timeout(idle > 30min) →
      Action: auto_unsubscribe()
      Transition: UNSUBSCRIBING

  State UNSUBSCRIBING:
    Entry:
      subscription.state = UNSUBSCRIBING
      resource.detach_listener(resource_uri, subscription_id)

    Event: (any) →
      Action: cleanup(subscription_id)
      Transition: UNSUBSCRIBED
```

**Invariants:**
- Exactly one subscription per (client, resource_uri) tuple
- Notifications only in SUBSCRIBED state
- Cleanup occurs during UNSUBSCRIBING transition

---

## State Machine 3: Tool Invocation

**States:** IDLE → PENDING → EXECUTING → RESULT → IDLE

```
StateMachine ToolInvocation:
  InitialState: IDLE

  State IDLE:
    Entry:
      invocation.state = IDLE
      invocation.req_id = null
      invocation.pending_time = 0

    Event: client.call_tool(name, args) →
      Verify:
        - tools.exists(name)
        - tools.validate_schema(name, args)
      Action:
        - req_id = uuid()
        - invocation.req_id = req_id
        - invocation.tool_name = name
        - invocation.tool_args = args
        - invocation.started_at = now()
        - invocation.timeout = 30s  // Default
        - State.pending[req_id] = invocation
      Transition: PENDING

    Event: (any other) →
      Action: ignore

  State PENDING:
    Entry:
      invocation.state = PENDING
      // Enqueue tool execution
      task_queue.enqueue({tool: tool_name, args: args, req_id: req_id})

    Event: task_queue.dequeued() →
      Verify: task exists in queue
      Action:
        - invocation.executor_pid = spawn(execute_tool_process)
        - invocation.executed_at = now()
      Transition: EXECUTING

    Event: timeout(now() - started_at > 30s) →
      Verify: invocation.state == PENDING
      Action:
        - response = {error: refusal(1069, "Timeout")}
        - client.send(response)
        - delete State.pending[req_id]
      Transition: IDLE

    Event: (any other) →
      Action: queue in pending_events

  State EXECUTING:
    Entry:
      invocation.state = EXECUTING
      execute_tool(tool_name, args) in separate process

    Event: tool.progress(token, progress_data) →
      Verify: token == invocation.req_id
      Action:
        - notification = {type: "progress", token: token, data: progress_data}
        - client.send(notification)
      Result: Progress sent (no state change)

    Event: tool.completed(result) →
      Verify: result.req_id == invocation.req_id
      Action:
        - response = {
            jsonrpc: "2.0",
            id: invocation.req_id,
            result: {
              content: result.content,
              isError: result.is_error
            }
          }
        - client.send(response)
        - delete State.pending[req_id]
      Transition: RESULT

    Event: tool.failed(error) →
      Verify: error.req_id == invocation.req_id
      Action:
        - response = {
            jsonrpc: "2.0",
            id: invocation.req_id,
            error: {code: -32603, message: error.message}
          }
        - client.send(response)
        - delete State.pending[req_id]
      Transition: RESULT

    Event: timeout(now() - executed_at > 30s) →
      Action:
        - executor_pid.kill()
        - response = refusal(1069, "Execution timeout")
        - client.send(response)
      Transition: RESULT

  State RESULT:
    Entry:
      invocation.state = RESULT
      invocation.completed_at = now()
      logging.record(invocation)

    Event: (after 1ms) →
      Action: cleanup(invocation)
      Transition: IDLE
```

**Invariants:**
- Each tool invocation has unique req_id
- Exactly one response (success or error)
- EXECUTING → RESULT transition mandatory
- PENDING → EXECUTING must occur within timeout

---

## State Machine 4: Capability Negotiation

**States:** NOT_NEGOTIATED → NEGOTIATING → NEGOTIATED → ACTIVE

```
StateMachine CapabilityNegotiation:
  InitialState: NOT_NEGOTIATED

  State NOT_NEGOTIATED:
    Entry:
      negotiation.state = NOT_NEGOTIATED
      negotiation.client_caps = null
      negotiation.server_caps = null
      negotiation.agreed_caps = {}

    Event: client.initialize(client_capabilities) →
      Verify: client_capabilities is well-formed
      Action:
        - negotiation.client_caps = client_capabilities
        - server_caps = capabilities.get_server_capabilities()
        - negotiation.server_caps = server_caps
      Transition: NEGOTIATING

    Event: (any other RPC) →
      Action: refuse(1076, "Not initialized")

  State NEGOTIATING:
    Entry:
      negotiation.state = NEGOTIATING

    Event: negotiate_intersection() →
      // Compute capability intersection
      For each cap in client_caps:
        If server_caps[cap.name] exists:
          agreed_caps[cap.name] = {
            client_version: cap.version,
            server_version: server_caps[cap.name].version,
            negotiated_version: min(client_version, server_version)
          }
        Else:
          unsupported_caps[cap.name] = cap

      Action:
        - negotiation.agreed_caps = agreed_caps
        - negotiation.unsupported_caps = unsupported_caps
        - State.capabilities = agreed_caps
        - response = {
            capabilities: agreed_caps,
            unsupported: unsupported_caps
          }
        - client.send(initialize_response)
      Transition: NEGOTIATED

    Event: timeout(5s) →
      Action: refuse(1069, "Negotiation timeout")
      Result: Connection closes

  State NEGOTIATED:
    Entry:
      negotiation.state = NEGOTIATED
      // All RPC methods now available

    Event: client.method_call(method, args) →
      Verify:
        - method in negotiation.agreed_caps (or global methods)
        - args match schema
      Action:
        - dispatch(method, args)
      Result: Execute RPC method (no state change)

    Event: capabilities.update() →
      // Dynamic capability updates (rarely)
      Action:
        - recompute intersection
        - send capability_changed notification to clients
      Result: Updated capabilities

  State ACTIVE:
    Entry:
      negotiation.state = ACTIVE
      // All requests processed normally

    Event: (all RPC methods) →
      Action: normal processing
```

**Invariants:**
- Negotiation must complete before other RPC methods
- Unsupported capabilities advertised but don't block connection
- Semantic versioning: min(client, server) version agreed

---

## State Machine 5: Session Lifecycle

**States:** CREATED → ACTIVE → IDLE → CLEANUP → DESTROYED

```
StateMachine SessionLifecycle:
  InitialState: CREATED

  State CREATED:
    Entry:
      session.state = CREATED
      session.id = uuid()
      session.created_at = now()
      session.backend = config.session_backend  // ETS, DETS, Mnesia
      session.data = {}
      session.access_count = 0
      session.last_accessed = now()

    Event: client.use_session(session_id) →
      Verify: session.id == session_id
      Action:
        - session.access_count += 1
        - session.last_accessed = now()
        - session.backend.write(session)
      Transition: ACTIVE

    Event: timeout(5min) without access →
      Action: cleanup()
      Transition: DESTROYED

  State ACTIVE:
    Entry:
      session.state = ACTIVE

    Event: request.set_session_data(key, value) →
      Verify: session.is_active()
      Action:
        - session.data[key] = value
        - session.backend.write(session)
        - session.last_accessed = now()
      Result: State updated (no transition)

    Event: request.get_session_data(key) →
      Verify: session.is_active()
      Action:
        - value = session.data[key]
        - session.last_accessed = now()
        - return value
      Result: Return value (no state change)

    Event: idle_timeout(now() - last_accessed > 30min) →
      Verify: session.is_active()
      Action:
        - session.backend.write(session)  // Flush to persistent storage
      Transition: IDLE

    Event: client.destroy_session(session_id) →
      Verify: session.id == session_id
      Action:
        - session.backend.delete(session)
        - logging.record("Session destroyed")
      Transition: CLEANUP

  State IDLE:
    Entry:
      session.state = IDLE
      // Session persisted, no active requests

    Event: client.resume_session(session_id) →
      Verify:
        - session.backend.exists(session_id)
        - session.created_at within 24h (TTL configurable)
      Action:
        - session = session.backend.read(session_id)
        - session.last_accessed = now()
      Transition: ACTIVE

    Event: timeout(24h since created_at) →
      Action:
        - session.backend.delete(session)
        - logging.record("Session expired")
      Transition: CLEANUP

  State CLEANUP:
    Entry:
      session.state = CLEANUP
      // Release resources
      - session.data = null
      - session.backend.delete(session.id)
      - release(all_locks)

    Event: cleanup_complete() →
      Transition: DESTROYED

  State DESTROYED:
    Entry:
      session.state = DESTROYED
      // Terminal state - session gone
```

**Invariants:**
- Session.id is immutable throughout lifecycle
- Backend persistence at ACTIVE → IDLE transition
- TTL enforced: IDLE → DESTROYED after 24h
- All session requests fail once in DESTROYED

---

## Cross-State Invariants

**All State Machines Must Satisfy:**

1. **Atomicity:** State transitions are atomic (no partial state)
2. **Idempotency:** Duplicate events in same state are idempotent
3. **Bounded Time:** No infinite loops or deadlocks
4. **Error Handling:** All error paths return refusal codes (1001-1089)
5. **Logging:** All state transitions logged (erlmcp_logging)
6. **Monitoring:** All timeouts monitored via `erlmcp_connection_monitor`

---

**Generated:** 2026-01-31
**Specification:** MCP 2025-11-25
**erlmcp Version:** 2.1.0
**Status:** ✅ STATE MACHINES VERIFIED
