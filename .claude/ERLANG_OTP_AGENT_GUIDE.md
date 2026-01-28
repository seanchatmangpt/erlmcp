# Erlang/OTP Agent Guide for erlmcp

**Last Updated**: 2026-01-27
**Target Project**: erlmcp v0.6.0 (Erlang/OTP MCP SDK)
**Agent Version**: 1.0.0

## Overview

This guide shows how to use the 10 core agents for erlmcp-specific Erlang/OTP development tasks. Each section demonstrates the **Research → Plan → Execute** workflow pattern with real erlmcp examples.

## Table of Contents

1. [Implementing gen_server Behaviors](#1-implementing-gen_server-behaviors)
2. [Building Transport Layers](#2-building-transport-layers)
3. [Writing Comprehensive Tests](#3-writing-comprehensive-tests)
4. [Understanding Codebase Architecture](#4-understanding-codebase-architecture)
5. [Designing Supervision Trees](#5-designing-supervision-trees)
6. [Benchmarking and Optimization](#6-benchmarking-and-optimization)
7. [Git Workflows and CI/CD](#7-git-workflows-and-cicd)
8. [Following SPARC Methodology](#8-following-sparc-methodology)

---

## 1. Implementing gen_server Behaviors

**Agent**: `erlang-otp-developer`
**Use Case**: Adding new gen_server to erlmcp

### Real erlmcp Example: Adding Cache Server

#### Research Phase
```javascript
// Spawn researcher to gather context
Task("Research gen_server Patterns",
  "Analyze erlmcp gen_server patterns in src/erlmcp_server.erl and src/erlmcp_client.erl. Focus on:
  - State record structure (#state{})
  - Callback implementations (init/1, handle_call/3, handle_cast/2, handle_info/2)
  - Supervision integration
  - Error handling patterns
  Document patterns for cache_server implementation.",
  "erlang-researcher")
```

**What erlang-researcher reads**:
- `src/erlmcp_server.erl` - Server implementation patterns
- `src/erlmcp_client.erl` - Client correlation patterns (request_id tracking)
- `src/erlmcp_sup.erl` - Supervision structure
- `docs/otp-patterns.md` - OTP best practices
- `test/erlmcp_server_tests.erl` - Testing patterns

**Researcher output** (preserves main context):
```
Summary:
- State records use pattern: #state{transport, transport_state, capabilities, pending}
- init/1 does minimal work, real setup in handle_cast after spawn
- handle_call/3 has 5000ms timeout for user-facing API
- handle_info/2 handles transport messages: {transport_data, Bin}, {transport_error, Reason}
- Supervision: simple_one_for_one with erlmcp_sup:start_child/3
- Tests: Use ?assertMatch patterns, test both success and error cases

Relevant patterns for cache_server:
- Use ETS table for storage (not in #state{})
- Monitor client processes, clean up on {'DOWN', ...}
- Implement handle_call for sync ops, handle_cast for async
- Add cache_server_tests.erl following erlmcp_server_tests.erl structure
```

#### Plan Phase
```javascript
// Spawn plan designer
Task("Plan Cache Server",
  "Design gen_server-based cache server for erlmcp. Requirements:
  - ETS-backed key-value storage
  - Supervised by erlmcp_sup
  - TTL support for entries
  - Monitoring of client processes for cleanup
  Use patterns from erlang-researcher summary.",
  "plan-designer")
```

**Plan designer output**:
```
Implementation Plan:

1. Module Structure (src/cache_server.erl)
   - Behavior: gen_server
   - State: #state{table :: ets:tid(), monitors :: map()}
   - Supervision: Add to erlmcp_sup as permanent child

2. API Functions
   - start_link() -> {ok, Pid}
   - put(Key, Value, TTL) -> ok
   - get(Key) -> {ok, Value} | {error, not_found}
   - delete(Key) -> ok

3. Callbacks
   - init/1: Create ETS table (set, public, named_table)
   - handle_call/3: get/1 (synchronous read)
   - handle_cast/2: put/3, delete/1 (async writes)
   - handle_info/2: TTL expirations, {'DOWN', ...} for cleanup

4. Testing Strategy (test/cache_server_tests.erl)
   - Basic CRUD operations
   - TTL expiration tests
   - Process cleanup on client death
   - Concurrent access tests

5. Quality Gates
   - EUnit tests with 80%+ coverage
   - Dialyzer clean (all types annotated)
   - Benchmark: 10k writes/sec, <1ms p95 latency

Delegation:
- erlang-otp-developer: Implement cache_server.erl
- erlang-test-engineer: Create cache_server_tests.erl
- erlang-performance: Benchmark performance
```

#### Execute Phase
```javascript
// Spawn implementer + tester in parallel
[Single Message - Parallel Execution]:
  Task("Implement Cache Server",
    "Create src/cache_server.erl following plan. Use gen_server with ETS backend. Follow erlmcp patterns from research summary.",
    "erlang-otp-developer")

  Task("Test Cache Server",
    "Create test/cache_server_tests.erl with EUnit tests. Cover CRUD, TTL, process cleanup, concurrency. Target 80%+ coverage.",
    "erlang-test-engineer")

  Task("Benchmark Cache",
    "Benchmark cache_server: throughput (ops/sec), latency (p50, p95, p99), memory usage. Verify 10k writes/sec requirement.",
    "erlang-performance")
```

**erlang-otp-developer output**:
```erlang
% src/cache_server.erl
-module(cache_server).
-behaviour(gen_server).

%% API
-export([start_link/0, put/3, get/1, delete/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-record(state, {
    table :: ets:tid(),
    monitors = #{} :: #{reference() => [term()]}
}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

put(Key, Value, TTL) ->
    gen_server:cast(?MODULE, {put, Key, Value, TTL, self()}).

get(Key) ->
    case ets:lookup(?MODULE, Key) of
        [{Key, Value, _Expiry, _Owner}] ->
            {ok, Value};
        [] ->
            {error, not_found}
    end.

delete(Key) ->
    gen_server:cast(?MODULE, {delete, Key}).

init([]) ->
    Table = ets:new(?MODULE, [set, public, named_table]),
    {ok, #state{table = Table}}.

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast({put, Key, Value, TTL, Owner}, #state{table = Table, monitors = Monitors} = State) ->
    Expiry = erlang:system_time(millisecond) + TTL,
    ets:insert(Table, {Key, Value, Expiry, Owner}),

    %% Monitor owner for cleanup
    case maps:is_key(Owner, Monitors) of
        false ->
            Ref = monitor(process, Owner),
            NewMonitors = Monitors#{Ref => [Key]};
        true ->
            NewMonitors = Monitors
    end,

    %% Schedule TTL expiration
    erlang:send_after(TTL, self(), {expire, Key}),
    {noreply, State#state{monitors = NewMonitors}};

handle_cast({delete, Key}, #state{table = Table} = State) ->
    ets:delete(Table, Key),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({expire, Key}, #state{table = Table} = State) ->
    case ets:lookup(Table, Key) of
        [{Key, _Value, Expiry, _Owner}] ->
            Now = erlang:system_time(millisecond),
            if Now >= Expiry ->
                ets:delete(Table, Key);
               true ->
                ok
            end;
        [] ->
            ok
    end,
    {noreply, State};

handle_info({'DOWN', Ref, process, _Pid, _Reason}, #state{table = Table, monitors = Monitors} = State) ->
    %% Clean up all keys owned by dead process
    case maps:get(Ref, Monitors, undefined) of
        undefined ->
            {noreply, State};
        Keys ->
            [ets:delete(Table, K) || K <- Keys],
            NewMonitors = maps:remove(Ref, Monitors),
            {noreply, State#state{monitors = NewMonitors}}
    end;

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.
```

**Quality Gate Report**:
```
✅ Tests: 12/12 passed (EUnit: 12, CT: 0, Proper: 0)
✅ Quality: Compile clean, Dialyzer clean, Xref clean, Format verified
✅ Benchmarks:
  - Throughput: 12,450 writes/sec (target: 10k) ✅
  - Latency: p50=0.08ms, p95=0.42ms, p99=1.2ms ✅
  - Memory: 2.4 MB for 10k entries ✅
✅ Coverage: 87% (minimum 80% required)

Ready for review: cache_server.erl with ETS backend, TTL support, process monitoring
```

---

## 2. Building Transport Layers

**Agent**: `erlang-transport-builder`
**Use Case**: Implementing new transport type for erlmcp

### Real erlmcp Example: Adding WebSocket Transport

#### Research Phase
```javascript
Task("Research Transport Behavior",
  "Analyze erlmcp transport abstraction. Read:
  - src/erlmcp_transport.erl (behavior definition)
  - src/erlmcp_transport_tcp.erl (reference implementation)
  - src/erlmcp_transport_stdio.erl (stdio implementation)
  - docs/v0.6.0-FINAL-PLAN.md (library integration patterns)
  Focus on: behavior callbacks, gun/ranch integration patterns, message formats.",
  "erlang-researcher")
```

**Researcher summary**:
```
Transport Behavior Callbacks:
- init(TransportId, Config) -> {ok, State} | {error, Reason}
- send(Data, State) -> {ok, NewState} | {error, Reason}
- close(State) -> ok

Message Formats (sent to registry):
- {transport_data, TransportId, Binary}
- {transport_connected, TransportId, Info}
- {transport_disconnected, TransportId, Reason}
- {transport_error, TransportId, Type, Reason}

Library Integration (v0.6.0):
- gun for HTTP/WebSocket clients
- ranch for TCP/WebSocket servers
- gproc for registry (replaces custom erlmcp_registry)

Reference: erlmcp_transport_tcp.erl uses ranch listeners
Pattern: Start ranch listener in init/2, handle connections in callbacks
```

#### Plan Phase
```javascript
Task("Plan WebSocket Transport",
  "Design erlmcp_transport_websocket.erl using:
  - gun for WebSocket client connections
  - ranch for WebSocket server (with cowboy protocol handler)
  - Follows -behaviour(erlmcp_transport) callbacks
  Use patterns from transport research summary.",
  "plan-designer")
```

**Plan**:
```
1. Module: src/erlmcp_transport_websocket.erl
   - Behavior: erlmcp_transport
   - Dependencies: gun (client), ranch + cowboy (server)

2. Configuration:
   - Client: {ws, #{host => "localhost", port => 8080, path => "/mcp"}}
   - Server: {ws_server, #{port => 8080, acceptors => 10}}

3. Callbacks:
   - init/2: Start gun connection (client) OR ranch listener (server)
   - send/2: gun:ws_send OR cowboy websocket frame
   - close/1: gun:close OR ranch:stop_listener

4. Message Handling:
   - Gun messages: {gun_ws, ConnPid, StreamRef, Frame}
   - Cowboy callbacks: websocket_handle/2, websocket_info/2

5. Testing: test/erlmcp_transport_websocket_tests.erl
   - Client-server connection test
   - Message round-trip test
   - Reconnection on failure
   - Benchmark: latency vs TCP transport

Delegation:
- erlang-transport-builder: Implement transport
- erlang-test-engineer: Create tests
- erlang-performance: Benchmark vs TCP
```

#### Execute Phase
```javascript
[Single Message]:
  Task("Implement WebSocket Transport",
    "Create src/erlmcp_transport_websocket.erl with gun client + ranch server. Follow plan and transport behavior.",
    "erlang-transport-builder")

  Task("Test WebSocket Transport",
    "Create test/erlmcp_transport_websocket_tests.erl. Test: connection, round-trip, reconnection, error handling.",
    "erlang-test-engineer")

  Task("Benchmark WebSocket",
    "Benchmark: latency (p50, p95, p99), throughput (msg/sec), compare to TCP transport. Document overhead.",
    "erlang-performance")
```

**Quality Gate**:
```
✅ Tests: 8/8 passed (EUnit: 6, CT: 2, Proper: 0)
✅ Quality: Compile clean, Dialyzer clean, Xref clean, Format verified
✅ Benchmarks:
  - Latency: p50=1.2ms, p95=3.4ms, p99=8.1ms (vs TCP: 0.8/2.1/5.2ms)
  - Throughput: 8,200 msg/sec (vs TCP: 11,500 msg/sec)
  - Overhead: +0.4ms median latency, -28% throughput (acceptable for WebSocket use case)
✅ Coverage: 82%

Ready for review: WebSocket transport with gun client + ranch server
```

---

## 3. Writing Comprehensive Tests

**Agent**: `erlang-test-engineer`
**Use Case**: Creating test suites for erlmcp modules

### Real erlmcp Example: Testing erlmcp_registry

#### Research Phase
```javascript
Task("Research Testing Patterns",
  "Analyze erlmcp test patterns:
  - test/erlmcp_server_tests.erl (EUnit patterns)
  - test/erlmcp_registry_tests.erl (registry testing)
  - rebar.config (test profiles: test, testlocal, proper)
  - docs/otp-patterns.md (testing best practices)
  Focus on: test structure, assertion patterns, setup/teardown, CT vs EUnit usage.",
  "erlang-researcher")
```

**Researcher summary**:
```
EUnit Patterns:
- One test file per module: <module>_tests.erl
- Test functions: <name>_test() or <name>_test_()
- Setup: Use ?setup macro with spawn + monitor
- Assertions: ?assertEqual, ?assertMatch, ?assertError
- Teardown: Stop processes in reverse order

Common Test Patterns:
- Used for: Multi-process integration tests
- Suites in test/*_SUITE.erl
- Callbacks: init_per_suite, end_per_suite, init_per_testcase, end_per_testcase

Property-Based Testing:
- Proper for invariants: always/1, ?FORALL
- Used for: Protocol encoding/decoding, state machines

Coverage Target: 80% minimum (enforced by make check)

Registry Testing Patterns (erlmcp_registry_tests.erl):
- Test registration: register_name, whereis_name
- Test message routing: send messages between processes
- Test cleanup: process death triggers deregistration
- Test concurrency: multiple concurrent registrations
```

#### Plan Phase
```javascript
Task("Plan Registry Test Suite",
  "Design comprehensive test suite for erlmcp_registry.erl:
  - EUnit tests for basic operations (register, whereis, unregister)
  - CT tests for multi-process scenarios
  - Proper tests for concurrent registration invariants
  - Target 85%+ coverage
  Use patterns from research summary.",
  "plan-designer")
```

**Plan**:
```
Test Suite: test/erlmcp_registry_tests.erl

1. EUnit Tests (Basic Operations)
   - register_name_test(): Register process, verify whereis returns Pid
   - register_duplicate_test(): Attempt duplicate registration, verify error
   - unregister_test(): Unregister, verify whereis returns undefined
   - whereis_unregistered_test(): Query non-existent name
   - send_registered_test(): Send message to registered process
   - send_unregistered_test(): Send to non-existent, verify error

2. EUnit Tests (Process Monitoring)
   - process_death_cleanup_test(): Register, kill process, verify auto-unregister
   - multiple_names_cleanup_test(): One process, multiple names, kill, verify all cleaned

3. Common Test Tests (Integration)
   - concurrent_registration_test(): 100 processes register simultaneously
   - message_routing_test(): Multiple senders → receivers via registry
   - registry_restart_test(): Kill registry, verify supervisor restarts, state clean

4. Property-Based Tests (Proper)
   - prop_registration_idempotent(): Register same name multiple times (should fail after first)
   - prop_message_delivery(): If registered, send always succeeds
   - prop_cleanup_on_death(): Process death always triggers cleanup

5. Coverage: Target 85% (registry.erl is critical infrastructure)

Delegation:
- erlang-test-engineer: Implement all tests
```

#### Execute Phase
```javascript
Task("Create Registry Test Suite",
  "Implement test/erlmcp_registry_tests.erl following plan:
  - EUnit: Basic ops + monitoring (8 tests)
  - CT: Integration (3 tests)
  - Proper: Properties (3 tests)
  Target 85%+ coverage. Follow erlmcp test patterns.",
  "erlang-test-engineer")
```

**Quality Gate**:
```
✅ Tests: 14/14 passed (EUnit: 8, CT: 3, Proper: 3)
✅ Quality: Compile clean, Dialyzer clean, Xref clean, Format verified
✅ Coverage: 89% (target: 85%)
✅ Property tests: 100 cases each, all passed

Ready for review: Comprehensive registry test suite with EUnit + CT + Proper
```

---

## 4. Understanding Codebase Architecture

**Agent**: `erlang-researcher`
**Use Case**: Exploring erlmcp structure and patterns

### Real erlmcp Example: Understanding Message Flow

#### Research Task
```javascript
Task("Understand MCP Message Flow",
  "Trace how MCP JSON-RPC messages flow through erlmcp:
  1. Start: Client sends tools/call request
  2. Transport layer receives bytes
  3. JSON-RPC decoding
  4. Server routing and handler invocation
  5. Response encoding and transport send

  Read relevant files in order, document data transformations at each step.",
  "erlang-researcher")
```

**Researcher process** (preserves context):
```
Step 1: Client Initiation (src/erlmcp_client.erl:call_tool/3)
  - Client receives: call_tool("calculator", "add", #{a => 1, b => 2})
  - Generates request_id: 42
  - Stores in pending: #{42 => {CallerPid, CallerRef}}
  - Encodes JSON-RPC: erlmcp_json_rpc:encode_request("tools/call", Params, 42)
  - Sends via transport: erlmcp_transport:send(JsonBinary, TransportState)

Step 2: Transport Layer (src/erlmcp_transport_stdio.erl:send/2)
  - Receives: JsonBinary
  - Writes to stdout: io:format("~s~n", [JsonBinary])
  - Returns: {ok, NewState}

Step 3: Server Reception (src/erlmcp_server.erl:handle_info/2)
  - Transport sends: {transport_data, TransportId, JsonBinary}
  - Server decodes: erlmcp_json_rpc:decode(JsonBinary)
  - Result: #{method => "tools/call", params => #{name => "calculator", arguments => ...}, id => 42}

Step 4: Tool Handler Invocation (src/erlmcp_server.erl:handle_method/3)
  - Looks up tool: maps:get("calculator", State#state.tools)
  - Finds handler: {HandlerFun, Schema}
  - Validates args: jesse:validate(Schema, Arguments)
  - Invokes: Result = HandlerFun(Arguments)
  - Result: #{result => 3}

Step 5: Response Encoding (src/erlmcp_server.erl:send_response/3)
  - Encodes: erlmcp_json_rpc:encode_response(Result, 42)
  - Sends via transport: erlmcp_transport:send(ResponseBinary, TransportState)

Step 6: Client Reception (src/erlmcp_client.erl:handle_info/2)
  - Transport sends: {transport_data, TransportId, ResponseBinary}
  - Client decodes: #{id => 42, result => #{result => 3}}
  - Looks up pending: maps:get(42, State#state.pending)
  - Replies to caller: gen_server:reply({CallerPid, CallerRef}, {ok, #{result => 3}})

Data Transformations:
1. Erlang map → JSON string (encode_request)
2. JSON string → binary (transport)
3. Binary → JSON string (transport receive)
4. JSON string → Erlang map (decode)
5. Validation → Handler invocation
6. Handler result → JSON response
7. JSON response → Caller reply

Key Files:
- src/erlmcp_client.erl:142 - call_tool/3 entry point
- src/erlmcp_json_rpc.erl:45 - encode_request/3
- src/erlmcp_transport_stdio.erl:78 - send/2
- src/erlmcp_server.erl:256 - handle_info({transport_data, ...})
- src/erlmcp_server.erl:312 - handle_method("tools/call", ...)
- src/erlmcp_json_rpc.erl:89 - encode_response/2
- src/erlmcp_client.erl:298 - handle_info({transport_data, ...})
```

**Context preserved**: Main agent receives summary, not full file contents.

---

## 5. Designing Supervision Trees

**Agent**: `erlang-architect`
**Use Case**: Designing OTP supervision strategy

### Real erlmcp Example: Adding Connection Pool Supervisor

#### Research Phase
```javascript
Task("Research erlmcp Supervision",
  "Analyze erlmcp supervision structure:
  - src/erlmcp_sup.erl (main application supervisor)
  - src/erlmcp_client_sup.erl (client supervisor)
  - src/erlmcp_server_sup.erl (server supervisor)
  - src/erlmcp_transport_sup.erl (transport supervisor)
  - docs/otp-patterns.md (supervision strategies)
  Focus on: restart strategies, child specs, fault isolation.",
  "erlang-researcher")
```

**Researcher summary**:
```
Current Supervision Tree:
erlmcp_sup (one_for_all)
├── erlmcp_registry (gen_server)
├── erlmcp_client_sup (simple_one_for_one)
│   └── erlmcp_client (dynamic workers)
├── erlmcp_server_sup (simple_one_for_one)
│   └── erlmcp_server (dynamic workers)
└── erlmcp_transport_sup (simple_one_for_one)
    └── erlmcp_transport_* (dynamic workers)

Restart Strategies:
- one_for_all: erlmcp_sup (if registry dies, restart all)
- simple_one_for_one: Dynamic worker pools (clients, servers, transports)

Child Spec Patterns:
#{id => Id,
  start => {Module, start_link, Args},
  restart => permanent | transient | temporary,
  shutdown => 5000,
  type => worker | supervisor,
  modules => [Module]}

Fault Isolation:
- Each client/server in separate process
- Transport failures don't affect other components
- Registry restart triggers full system restart (by design)
```

#### Plan Phase
```javascript
Task("Plan Connection Pool Supervisor",
  "Design poolboy-based connection pool supervisor for HTTP transport connections:
  - Supervised by erlmcp_sup (peer to client_sup, server_sup)
  - Manages pool of gun HTTP connections
  - Restart strategy: one_for_one (pool failures isolated)
  - Worker spec: gun connection processes
  Use erlmcp supervision patterns from research.",
  "plan-designer")
```

**Plan**:
```
New Supervisor: src/erlmcp_connection_pool_sup.erl

1. Supervision Tree Position:
   erlmcp_sup (one_for_all)
   ├── erlmcp_registry
   ├── erlmcp_client_sup
   ├── erlmcp_server_sup
   ├── erlmcp_transport_sup
   └── erlmcp_connection_pool_sup (NEW - one_for_one)
       └── poolboy (simple_one_for_one)
           └── gun HTTP connections (dynamic workers)

2. Configuration:
   - Pool size: 10 workers
   - Max overflow: 5
   - Restart strategy: one_for_one (isolated failures)
   - Worker: gun:open/3 connections

3. Child Spec:
   #{id => connection_pool,
     start => {erlmcp_connection_pool_sup, start_link, [PoolConfig]},
     restart => permanent,
     shutdown => 5000,
     type => supervisor,
     modules => [erlmcp_connection_pool_sup]}

4. Poolboy Integration:
   - Worker module: erlmcp_http_worker.erl
   - Checkout: poolboy:checkout(pool_name, block, Timeout)
   - Checkin: poolboy:checkin(pool_name, Worker)

5. Testing:
   - Supervisor restart on pool failure
   - Worker isolation (one failure doesn't affect others)
   - Pool exhaustion handling

Delegation:
- erlang-architect: Design supervisor structure
- erlang-otp-developer: Implement supervisor + worker
- erlang-test-engineer: Create supervision tests
```

#### Execute Phase
```javascript
[Single Message]:
  Task("Design Connection Pool",
    "Architect supervision structure for poolboy-based connection pool. Document restart strategies and fault isolation.",
    "erlang-architect")

  Task("Implement Pool Supervisor",
    "Create src/erlmcp_connection_pool_sup.erl and src/erlmcp_http_worker.erl. Integrate with erlmcp_sup.",
    "erlang-otp-developer")

  Task("Test Pool Supervision",
    "Create test/erlmcp_connection_pool_tests.erl. Test: pool exhaustion, worker crashes, supervisor restart.",
    "erlang-test-engineer")
```

**Quality Gate**:
```
✅ Tests: 6/6 passed (EUnit: 4, CT: 2, Proper: 0)
✅ Quality: Compile clean, Dialyzer clean, Xref clean, Format verified
✅ Architecture: Supervision tree reviewed, fault isolation verified
✅ Coverage: 84%

Ready for review: Connection pool supervisor with poolboy integration
```

---

## 6. Benchmarking and Optimization

**Agent**: `erlang-performance`
**Use Case**: Measuring and improving performance

### Real erlmcp Example: Benchmarking JSON-RPC Encoding

#### Research Phase
```javascript
Task("Research Performance Patterns",
  "Analyze erlmcp performance characteristics:
  - src/erlmcp_json_rpc.erl (encoding/decoding)
  - Identify hot paths in message flow
  - Document current performance baselines (if any)
  - Review profiling tools: recon_trace, fprof, eprof
  Focus on: JSON encoding overhead, transport latency, registry lookup time.",
  "erlang-researcher")
```

**Researcher summary**:
```
Hot Paths:
1. JSON-RPC encoding: erlmcp_json_rpc:encode_request/3
2. JSON-RPC decoding: erlmcp_json_rpc:decode/1
3. Registry lookup: gproc:lookup_local_name/1
4. Transport send: erlmcp_transport_*:send/2

Current Performance:
- No documented benchmarks in codebase
- No profiling data available
- Potential optimization: JSON encoding uses jsx library (fast but not fastest)

Profiling Tools:
- recon_trace: Live tracing without stopping system
- fprof: Detailed function-level profiling
- eprof: Time-based profiling
- erlang:system_time/1: Microsecond-precision timing

Benchmark Approach:
1. Measure baseline: 1000 iterations, report p50/p95/p99
2. Identify bottlenecks: Profile with fprof
3. Optimize: Cache, pre-allocate, avoid copies
4. Re-measure: Verify improvement
```

#### Plan Phase
```javascript
Task("Plan JSON-RPC Benchmark",
  "Design benchmark suite for JSON-RPC encoding/decoding:
  - Measure: Latency (p50, p95, p99), Throughput (ops/sec)
  - Test cases: Small message (10 fields), Large message (1000 fields), Nested structures
  - Profiling: Identify bottlenecks in jsx:encode/1
  - Optimization targets: <0.1ms p50 for small messages, <5ms p99 for large messages
  Use patterns from research.",
  "plan-designer")
```

**Plan**:
```
Benchmark Suite: src/benchmarks/json_rpc_benchmark.erl

1. Test Cases:
   a) Small message: #{method => "ping", id => 1}
   b) Medium message: tools/call with 50 arguments
   c) Large message: 1000-element list in params
   d) Nested: 10 levels deep nested maps

2. Metrics:
   - Latency: p50, p95, p99, max (microseconds)
   - Throughput: ops/sec for 10k iterations
   - Memory: Process heap size before/after

3. Profiling:
   - Run fprof on encode/decode functions
   - Identify: Time spent in jsx vs erlmcp code
   - Focus on: String concatenation, map operations

4. Optimization Strategies (if needed):
   - Pre-allocate iolist for known structures
   - Cache frequently-used JSON templates
   - Use jiffy instead of jsx (faster C-based JSON)

5. Reporting:
   - Tabular output with baseline vs optimized
   - Regression detection: Fail if >10% slower

Delegation:
- erlang-performance: Implement benchmarks + profile
```

#### Execute Phase
```javascript
Task("Benchmark JSON-RPC",
  "Create benchmarks/json_rpc_benchmark.erl:
  - Measure encoding/decoding latency for small/medium/large messages
  - Profile with fprof to identify bottlenecks
  - Report: Latency (p50/p95/p99), Throughput (ops/sec), Memory usage
  - Document baseline for regression detection.",
  "erlang-performance")
```

**Benchmark output**:
```
JSON-RPC Encoding Benchmark Results:

Small Message (10 fields):
  - Latency: p50=0.045ms, p95=0.092ms, p99=0.18ms ✅
  - Throughput: 22,000 ops/sec
  - Memory: +0.8 KB per operation

Medium Message (50 fields):
  - Latency: p50=0.21ms, p95=0.48ms, p99=1.1ms ✅
  - Throughput: 4,800 ops/sec
  - Memory: +3.2 KB per operation

Large Message (1000 fields):
  - Latency: p50=3.8ms, p95=8.2ms, p99=12.4ms ⚠️ (target: <5ms p99)
  - Throughput: 260 ops/sec
  - Memory: +180 KB per operation

Profiling Results (fprof):
  - 72% time in jsx:encode/1 (external library)
  - 18% time in map traversal
  - 10% time in list concatenation

Optimization Recommendations:
  1. Consider jiffy for large messages (2-3x faster)
  2. Pre-allocate iolist buffers for known structures
  3. Cache common request templates

Baseline established: Small/medium meet targets, large needs optimization.
```

**Quality Gate**:
```
✅ Benchmarks: Small ✅, Medium ✅, Large ⚠️ (documented)
✅ Profiling: Bottlenecks identified (jsx library)
✅ Documentation: Baseline established for regression detection
✅ Recommendations: Optimization path documented

Ready for review: Benchmark suite with profiling data and optimization recommendations
```

---

## 7. Git Workflows and CI/CD

**Agent**: `erlang-github-ops`
**Use Case**: Managing Git workflows, PRs, releases

### Real erlmcp Example: Creating Release v0.6.0

#### Research Phase
```javascript
Task("Research Release Process",
  "Analyze erlmcp release workflow:
  - Review: docs/v0.6.0-FINAL-PLAN.md (release plan)
  - Check: src/erlmcp.app.src (current version)
  - Review: .github/workflows/*.yml (CI/CD pipelines)
  - Check: CHANGELOG.md or release notes format
  - Review: rebar.config (release profile)
  Focus on: Version bump process, testing requirements, release artifacts.",
  "erlang-researcher")
```

**Researcher summary**:
```
Current Version: 0.5.0 (src/erlmcp.app.src)

Release Plan (v0.6.0):
- Major change: Library integration (gproc, gun, ranch, poolboy)
- Registry refactor: Custom → gproc
- HTTP transport: inets → gun
- TCP transport: gen_tcp → ranch

CI/CD Pipeline (.github/workflows/ci.yml):
- Triggers: push to main, pull requests
- Jobs: compile, dialyzer, xref, eunit, ct, proper
- Erlang versions: 25.0, 26.0, 27.0
- Success criteria: All tests pass, no dialyzer warnings

Release Checklist:
1. Update version in src/erlmcp.app.src
2. Update CHANGELOG.md with v0.6.0 changes
3. Run full test suite: make check
4. Create release branch: release/v0.6.0
5. Create GitHub release with artifacts
6. Tag: v0.6.0

Release Artifacts:
- Compiled release: rebar3 as prod release
- Documentation: Edoc HTML
- Checksums: SHA256 hashes
```

#### Plan Phase
```javascript
Task("Plan v0.6.0 Release",
  "Design release workflow for erlmcp v0.6.0:
  - Version bump: 0.5.0 → 0.6.0
  - CHANGELOG: Document library integration changes
  - Testing: Full test suite + manual verification
  - Release branch: release/v0.6.0
  - GitHub release: Artifacts + release notes
  Use patterns from research.",
  "plan-designer")
```

**Plan**:
```
Release Workflow for v0.6.0:

Phase 1: Pre-Release (Quality Gates)
1. Run full test suite: make check
   - Verify: All tests pass (0 failures)
   - Verify: Dialyzer clean (0 warnings)
   - Verify: Xref clean (0 undefined functions)
   - Verify: Coverage ≥80%

2. Manual verification:
   - Test: examples/simple/simple_client.erl
   - Test: examples/calculator/calculator_client.erl
   - Verify: gproc registry working
   - Verify: gun HTTP transport working
   - Verify: ranch TCP transport working

Phase 2: Version Bump
1. Update src/erlmcp.app.src: {vsn, "0.6.0"}
2. Update CHANGELOG.md:
   ```
   ## [0.6.0] - 2026-01-27
   ### Added
   - Library integration: gproc, gun, ranch, poolboy
   - Connection pooling with poolboy

   ### Changed
   - Registry: Custom erlmcp_registry → gproc
   - HTTP transport: inets → gun (HTTP/1.1 + HTTP/2)
   - TCP transport: gen_tcp → ranch

   ### Removed
   - 770 LOC of custom code replaced by libraries
   ```

3. Commit: "chore: Bump version to v0.6.0"

Phase 3: Release Branch
1. Create branch: git checkout -b release/v0.6.0
2. Push: git push origin release/v0.6.0
3. Create PR: release/v0.6.0 → main

Phase 4: GitHub Release
1. Build artifacts: rebar3 as prod release
2. Generate docs: rebar3 edoc
3. Create checksums: sha256sum _build/prod/rel/erlmcp/erlmcp-0.6.0.tar.gz
4. Create GitHub release:
   - Tag: v0.6.0
   - Title: "erlmcp v0.6.0 - Library Integration"
   - Body: CHANGELOG excerpt + upgrade instructions
   - Artifacts: Release tarball + checksums

Delegation:
- erlang-github-ops: Execute release workflow
- erlang-test-engineer: Run manual verification tests
```

#### Execute Phase
```javascript
[Single Message]:
  Task("Execute v0.6.0 Release",
    "Follow release plan:
    1. Run quality gates (make check)
    2. Update version in erlmcp.app.src
    3. Update CHANGELOG.md
    4. Create release branch
    5. Create GitHub release with artifacts
    Document each step completion.",
    "erlang-github-ops")

  Task("Manual Release Verification",
    "Run manual tests:
    - examples/simple/simple_client.erl
    - examples/calculator/calculator_client.erl
    - Verify gproc, gun, ranch integration
    Document test results.",
    "erlang-test-engineer")
```

**Quality Gate**:
```
✅ Pre-Release Quality Gates:
  - Tests: 142/142 passed (EUnit: 98, CT: 32, Proper: 12)
  - Dialyzer: 0 warnings
  - Xref: 0 undefined functions
  - Coverage: 84%
  - Manual tests: All passed ✅

✅ Version Bump:
  - src/erlmcp.app.src: 0.5.0 → 0.6.0
  - CHANGELOG.md: v0.6.0 section added
  - Commit: 3a57163 "chore: Bump version to v0.6.0"

✅ Release Branch:
  - Branch: release/v0.6.0 created
  - PR: #42 "Release v0.6.0" (release/v0.6.0 → main)

✅ GitHub Release:
  - Tag: v0.6.0
  - Artifacts: erlmcp-0.6.0.tar.gz (SHA256: abc123...)
  - Release notes: Published with upgrade instructions

Ready for merge: Release v0.6.0 complete, all quality gates passed
```

---

## 8. Following SPARC Methodology

**Agent**: `sparc-orchestrator`
**Use Case**: Coordinating full SPARC workflow for complex features

### Real erlmcp Example: Adding Subscription Support

#### SPARC Workflow
```javascript
// Single orchestrator coordinates all phases
Task("SPARC: Add Subscription Support",
  "Coordinate full SPARC workflow for adding resource subscription support to erlmcp:
  - Specification: Define subscription protocol (JSON-RPC methods)
  - Pseudocode: Algorithm for subscription management
  - Architecture: Design subscriber tracking and notification system
  - Refinement: Implement + test + optimize
  - Completion: Review + PR + documentation

  Delegate to phase-specific agents, coordinate dependencies.",
  "sparc-orchestrator")
```

**SPARC orchestrator process**:

##### Phase 1: Specification (plan-designer + erlang-researcher)
```javascript
Task("SPARC Spec: Subscription Protocol",
  "Define subscription protocol for erlmcp:
  - JSON-RPC methods: resources/subscribe, resources/unsubscribe, notifications/resources/updated
  - Resource URI format: mcp://server/resource/path
  - Notification payload structure
  Research MCP spec and existing erlmcp resource handlers.",
  "plan-designer")
```

**Spec output**:
```
Subscription Protocol Specification:

1. JSON-RPC Methods:
   - resources/subscribe: Client subscribes to resource URI
     Request: #{method => "resources/subscribe", params => #{uri => "mcp://server/data"}}
     Response: #{result => #{subscribed => true}}

   - resources/unsubscribe: Client unsubscribes
     Request: #{method => "resources/unsubscribe", params => #{uri => "mcp://server/data"}}
     Response: #{result => #{unsubscribed => true}}

   - notifications/resources/updated: Server notifies subscribers
     Notification: #{method => "notifications/resources/updated", params => #{uri => ..., contents => ...}}

2. Resource URI Format: mcp://server_id/resource/path
   - server_id: Unique server identifier
   - resource/path: Hierarchical resource path

3. Subscription Lifecycle:
   - Subscribe: Client sends resources/subscribe
   - Update: Server detects resource change, sends notifications/resources/updated to all subscribers
   - Unsubscribe: Client sends resources/unsubscribe OR client process dies (automatic cleanup)

4. Error Handling:
   - {error, resource_not_found}: URI doesn't exist
   - {error, already_subscribed}: Duplicate subscription
   - {error, not_subscribed}: Unsubscribe non-existent subscription
```

##### Phase 2: Pseudocode (plan-designer)
```javascript
Task("SPARC Pseudocode: Subscription Algorithm",
  "Design subscription management algorithm:
  - Data structure: URI → set of subscriber PIDs
  - Subscribe operation: Add PID to set, monitor process
  - Unsubscribe operation: Remove PID from set, demonitor
  - Notification: Iterate subscribers, send notification to each
  - Cleanup: On process death, remove from all subscriptions
  Use erlmcp patterns.",
  "plan-designer")
```

**Pseudocode output**:
```erlang
%% State structure
-record(state, {
    subscriptions = #{} :: #{uri() => sets:set(pid())},
    monitors = #{} :: #{reference() => {pid(), [uri()]}}
}).

%% Subscribe algorithm
subscribe(URI, SubscriberPid, State) ->
    %% Add to subscriptions
    Subscribers = maps:get(URI, State#state.subscriptions, sets:new()),
    NewSubscribers = sets:add_element(SubscriberPid, Subscribers),

    %% Monitor subscriber for cleanup
    case maps:is_key(SubscriberPid, State#state.monitors) of
        false ->
            Ref = monitor(process, SubscriberPid),
            NewMonitors = State#state.monitors#{Ref => {SubscriberPid, [URI]}};
        true ->
            {Ref, {Pid, URIs}} = ...,  %% Find existing monitor
            NewMonitors = State#state.monitors#{Ref => {Pid, [URI | URIs]}}
    end,

    State#state{
        subscriptions = State#state.subscriptions#{URI => NewSubscribers},
        monitors = NewMonitors
    }.

%% Notify algorithm
notify_subscribers(URI, Contents, State) ->
    case maps:get(URI, State#state.subscriptions, sets:new()) of
        EmptySet when sets:size(EmptySet) == 0 ->
            ok;  %% No subscribers
        Subscribers ->
            Notification = #{
                method => "notifications/resources/updated",
                params => #{uri => URI, contents => Contents}
            },
            [send_notification(Pid, Notification) || Pid <- sets:to_list(Subscribers)]
    end.

%% Cleanup algorithm
handle_info({'DOWN', Ref, process, Pid, _Reason}, State) ->
    case maps:get(Ref, State#state.monitors, undefined) of
        undefined ->
            State;
        {Pid, URIs} ->
            %% Remove from all subscriptions
            NewSubscriptions = lists:foldl(
                fun(URI, Acc) ->
                    Subscribers = maps:get(URI, Acc, sets:new()),
                    NewSubscribers = sets:del_element(Pid, Subscribers),
                    case sets:size(NewSubscribers) of
                        0 -> maps:remove(URI, Acc);
                        _ -> Acc#{URI => NewSubscribers}
                    end
                end,
                State#state.subscriptions,
                URIs
            ),
            State#state{
                subscriptions = NewSubscriptions,
                monitors = maps:remove(Ref, State#state.monitors)
            }
    end.
```

##### Phase 3: Architecture (erlang-architect)
```javascript
Task("SPARC Architecture: Subscription System",
  "Architect subscription system for erlmcp_server:
  - Module location: Add to erlmcp_server.erl (not separate module)
  - State management: Extend #state{} record
  - Integration: How resources trigger notifications
  - Supervision: No new processes (part of server gen_server)
  Review with pseudocode and OTP patterns.",
  "erlang-architect")
```

**Architecture output**:
```
Subscription System Architecture:

1. Module Location:
   - Implementation: src/erlmcp_server.erl (extend existing)
   - Rationale: Subscriptions are server-side feature, no need for separate process

2. State Extension:
   -record(state, {
       ...existing fields...,
       subscriptions = #{} :: #{uri() => sets:set(pid())},
       monitors = #{} :: #{reference() => {pid(), [uri()]}}
   }).

3. New Functions (erlmcp_server.erl):
   - subscribe_resource(URI, SubscriberPid, State) -> NewState
   - unsubscribe_resource(URI, SubscriberPid, State) -> NewState
   - notify_subscribers(URI, Contents, State) -> ok
   - handle_method("resources/subscribe", Params, State)
   - handle_method("resources/unsubscribe", Params, State)

4. Resource Integration:
   - When resource updated: call notify_subscribers(URI, NewContents, State)
   - Resource handlers remain unchanged, notifications triggered externally

5. Message Flow:
   Client → resources/subscribe → erlmcp_server:handle_method
   Resource update → notify_subscribers → send to all subscriber PIDs
   Client dies → {'DOWN', Ref, ...} → cleanup subscriptions

6. Testing Integration:
   - test/erlmcp_server_tests.erl: Add subscription tests
   - Test: subscribe, notify, unsubscribe, process death cleanup

No new supervision needed: Subscriptions managed by existing server gen_server.
```

##### Phase 4: Refinement (erlang-otp-developer + erlang-test-engineer + erlang-performance)
```javascript
[Single Message - Parallel Execution]:
  Task("Implement Subscription System",
    "Add subscription support to src/erlmcp_server.erl:
    - Extend #state{} record
    - Implement subscribe/unsubscribe functions
    - Add handle_method clauses for resources/subscribe, resources/unsubscribe
    - Implement notify_subscribers
    - Add handle_info for {'DOWN', ...} cleanup
    Follow pseudocode and architecture design.",
    "erlang-otp-developer")

  Task("Test Subscription System",
    "Add tests to test/erlmcp_server_tests.erl:
    - subscribe_test(): Subscribe to resource, verify subscription
    - notify_test(): Update resource, verify notification received
    - unsubscribe_test(): Unsubscribe, verify no notification
    - process_death_cleanup_test(): Kill subscriber, verify cleanup
    - concurrent_subscribers_test(): Multiple subscribers, all receive notifications
    Target 85%+ coverage for subscription code.",
    "erlang-test-engineer")

  Task("Benchmark Subscriptions",
    "Benchmark subscription system:
    - Subscription overhead: Time to subscribe/unsubscribe
    - Notification latency: Time from resource update to subscriber receives notification
    - Scale test: 1000 subscribers, verify notification fan-out time
    Document metrics.",
    "erlang-performance")
```

**Refinement outputs**:
```
✅ Implementation (erlang-otp-developer):
  - Added subscriptions/monitors fields to #state{}
  - Implemented subscribe_resource/3, unsubscribe_resource/3, notify_subscribers/3
  - Added handle_method("resources/subscribe", ...) and resources/unsubscribe
  - Added handle_info({'DOWN', ...}) for cleanup
  - 186 LOC added to erlmcp_server.erl

✅ Tests (erlang-test-engineer):
  - 5 new tests added to erlmcp_server_tests.erl
  - subscribe_test: ✅ passed
  - notify_test: ✅ passed
  - unsubscribe_test: ✅ passed
  - process_death_cleanup_test: ✅ passed
  - concurrent_subscribers_test: ✅ passed (100 subscribers)
  - Coverage: 87% for subscription code

✅ Benchmarks (erlang-performance):
  - Subscribe: 0.08ms per subscription
  - Unsubscribe: 0.05ms per unsubscription
  - Notification latency: p50=0.12ms, p95=0.48ms, p99=1.2ms (per subscriber)
  - Fan-out (1000 subscribers): 120ms total (parallelizable with spawn)
  - Recommendation: For >100 subscribers, spawn parallel notification sends
```

##### Phase 5: Completion (code-reviewer + erlang-github-ops)
```javascript
[Single Message]:
  Task("Review Subscription Code",
    "Review subscription implementation in erlmcp_server.erl:
    - Code quality: OTP patterns, error handling
    - Test coverage: ≥85% for new code
    - Documentation: Function specs, comments
    - Quality gates: Dialyzer, xref, format check
    Validate before PR creation.",
    "code-reviewer")

  Task("Create Subscription PR",
    "Create pull request for subscription feature:
    - Branch: feature/subscriptions → main
    - Title: 'feat: Add resource subscription support'
    - Description: SPARC phases summary + testing results + benchmarks
    - Docs: Update docs/protocol.md with subscription methods
    Include reviewer checklist.",
    "erlang-github-ops")
```

**Completion outputs**:
```
✅ Code Review (code-reviewer):
  - OTP patterns: ✅ Correct gen_server usage, proper monitoring
  - Error handling: ✅ All edge cases covered (resource_not_found, already_subscribed)
  - Test coverage: 87% (target: 85%) ✅
  - Documentation: ✅ Function specs added, algorithm commented
  - Quality gates:
    - Compile: ✅ 0 warnings
    - Dialyzer: ✅ 0 type errors
    - Xref: ✅ 0 undefined functions
    - Format: ✅ Properly formatted

  Approved for PR creation.

✅ Pull Request (erlang-github-ops):
  - Branch: feature/subscriptions created
  - PR #43: "feat: Add resource subscription support"
  - Description:
    ```
    ## Summary
    Adds resource subscription support following SPARC methodology.

    ## SPARC Phases
    - Specification: JSON-RPC methods defined (resources/subscribe, resources/unsubscribe, notifications/resources/updated)
    - Pseudocode: Subscription algorithm designed with process monitoring
    - Architecture: Integrated into erlmcp_server.erl (no new processes)
    - Refinement: Implemented + tested + benchmarked
    - Completion: Code reviewed, quality gates passed

    ## Testing
    - 5 new EUnit tests (all passed)
    - Coverage: 87% for subscription code
    - Benchmarks: Subscribe 0.08ms, Notify 0.12ms p50

    ## Reviewer Checklist
    - [ ] Code follows OTP patterns
    - [ ] Tests comprehensive (edge cases covered)
    - [ ] Documentation updated (docs/protocol.md)
    - [ ] Quality gates passed (dialyzer, xref, format)
    ```
  - Files changed:
    - src/erlmcp_server.erl (+186 LOC)
    - test/erlmcp_server_tests.erl (+124 LOC)
    - docs/protocol.md (+48 LOC)

  PR ready for review.
```

**SPARC Orchestrator Summary**:
```
SPARC Workflow Complete: Resource Subscription Support

✅ Specification (plan-designer + erlang-researcher)
  - JSON-RPC methods defined
  - Protocol documented

✅ Pseudocode (plan-designer)
  - Subscription algorithm designed
  - Data structures specified

✅ Architecture (erlang-architect)
  - System design reviewed
  - Integration approach approved

✅ Refinement (erlang-otp-developer + erlang-test-engineer + erlang-performance)
  - Implementation complete (186 LOC)
  - Tests passing (5 new tests, 87% coverage)
  - Benchmarks documented (0.12ms p50 notification latency)

✅ Completion (code-reviewer + erlang-github-ops)
  - Code review passed
  - PR #43 created and ready for merge

Total coordination: 5 agents, 4 phases, 1 complete feature ✅
```

---

## Summary: Agent Coordination Best Practices

### Key Takeaways

1. **Always start with Research**
   - Use `erlang-researcher` to gather context before planning
   - Preserves main context by delegating deep dives to separate agent
   - Example: Research transport patterns before implementing new transport

2. **Follow Research → Plan → Execute**
   - `erlang-researcher` → `plan-designer` → Specialist agents
   - Never skip planning for non-trivial tasks
   - Example: Plan subscription system before implementing

3. **Use SPARC for complex features**
   - `sparc-orchestrator` coordinates multi-phase workflows
   - Each phase delegated to appropriate agent
   - Example: Adding subscription support requires all 5 SPARC phases

4. **Parallel execution when possible**
   - Spawn implementer + tester + benchmarker in single message
   - Example: Implement cache server + write tests + benchmark in parallel

5. **Mandatory quality gates**
   - ALL agents must verify before reporting completion
   - Tests + Dialyzer + Xref + Coverage + Benchmarks
   - Example: Every implementation must pass `make check`

6. **Context management**
   - Use `erlang-researcher` for large codebases (>10 files)
   - Receive summaries, not full file contents
   - Example: Understanding message flow requires reading 7 files → delegate to researcher

7. **Specialized agents for specialized tasks**
   - Transport layer → `erlang-transport-builder`
   - OTP behaviors → `erlang-otp-developer`
   - Testing → `erlang-test-engineer`
   - Example: Don't use generic coder for transport implementation

### Common Pitfalls to Avoid

❌ **Don't skip research phase** - Leads to incorrect patterns
❌ **Don't implement without plan** - Results in rework
❌ **Don't skip quality gates** - Creates technical debt
❌ **Don't read full files in main context** - Wastes context window, delegate to erlang-researcher
❌ **Don't use generic agents** - Use erlmcp-specific agents for better results

### Quick Decision Tree

```
Need to implement something?
├─ Is it a gen_server/supervisor/OTP app? → erlang-otp-developer
├─ Is it a transport layer? → erlang-transport-builder
├─ Is it tests? → erlang-test-engineer
├─ Need to understand codebase first? → erlang-researcher
├─ Need implementation plan? → plan-designer
├─ Architecture decision? → erlang-architect
├─ Performance critical? → erlang-performance
├─ Git/PR/CI/CD? → erlang-github-ops
├─ Complex multi-phase feature? → sparc-orchestrator
└─ Final review before completion? → code-reviewer
```

---

**Related Documentation**:
- [AGENT_INDEX.md](AGENT_INDEX.md) - Complete agent directory
- [SYSTEM_GUIDE.md](SYSTEM_GUIDE.md) - Commands vs Agents vs Roo rules
- [agents/README.md](agents/README.md) - Agent technical details

---

**Last Updated**: 2026-01-27
**Agent Architecture Version**: 1.0.0
