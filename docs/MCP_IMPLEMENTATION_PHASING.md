# MCP Specification Implementation Phasing - erlmcp v2.1.0 → v3.0.0

**Document Version:** 1.0.0
**Created:** 2026-02-01
**Architect:** Erlang Architect Agent
**Target:** 100% MCP Specification Compliance (2025-11-25)

---

## Executive Summary

This document provides detailed architectural phasing for achieving 100% MCP specification support in erlmcp. The current baseline (v2.1.0) provides excellent performance (553K msg/s registry throughput, P50=5ms latency) and solid OTP supervision architecture, but has gaps in:

1. **Schema validation performance** (5-20ms bottleneck)
2. **Complete sampling implementation** (basic structure exists)
3. **Full prompt template support** (module exists, needs verification)
4. **Resource subscription completeness** (module exists, needs performance testing)
5. **JSON encoding performance** (jsx → jiffy migration)

**Timeline:** 5 phases over 6-9 months
**Risk Level:** Low-Medium (leveraging existing OTP patterns)
**Compatibility:** Backward compatible (no breaking changes)

---

## Phase 0: Gap Analysis and Planning (CURRENT)

**Duration:** 2 weeks (Completed)
**Effort:** 40 hours
**Risk:** Low

### Objectives

1. ✅ Analyze current architecture (apps/erlmcp_core, _transports, _observability, _validation)
2. ✅ Identify MCP spec gaps via performance analysis
3. ✅ Document supervision tree structure (3-tier one_for_one)
4. ✅ Baseline performance metrics (553K msg/s, P50=5ms, 40-50K connections)
5. ✅ Create implementation roadmap

### Architecture Analysis

**Current Supervision Tree (v2.1.0):**
```
erlmcp_sup (one_for_one)
├── erlmcp_core_sup (one_for_one)          [TIER 1: CORE]
│   ├── erlmcp_registry (gproc)
│   ├── erlmcp_session_manager
│   ├── erlmcp_resource_subscriptions      ← EXISTS
│   ├── erlmcp_schema_registry              ← Needs caching enhancement
│   ├── erlmcp_cache (L1/L2/L3)
│   └── [11+ other core workers]
├── erlmcp_server_sup (simple_one_for_one) [TIER 2: PROTOCOL]
│   └── [Dynamic erlmcp_server instances]
└── erlmcp_observability_sup (one_for_one) [TIER 3: OBSERVABILITY]
    └── [Metrics, OTEL, Chaos, Health]
```

**Existing Modules (Relevant to MCP Spec):**
- ✅ `erlmcp_sampling.erl` - Basic sampling gen_server (needs completion)
- ✅ `erlmcp_prompt_template.erl` - Prompt template handling (needs verification)
- ✅ `erlmcp_resource_subscriptions.erl` - Resource change notifications (needs testing)
- ✅ `erlmcp_schema_registry.erl` - JSON Schema management (needs caching)
- ⚠️ `jsx` JSON codec - Performance bottleneck (0.5-2ms per message)
- ⚠️ `jesse` schema validation - Performance bottleneck (5-20ms)

### Dependencies

**Foundation (Already Present):**
- OTP 28.3.1 (custom build)
- gproc 0.9.0 (registry)
- jsx 3.1.0 (JSON codec - **TO BE REPLACED**)
- jesse 1.8.1 (schema validation - **TO BE ENHANCED**)
- gun 2.0.1 (HTTP/2 client)
- ranch 2.1.0 (TCP acceptors)

**New Dependencies (Phases 1-4):**
- jiffy 1.1.1 (NIF-based JSON - Phase 1)
- poolboy 1.5.2 (already present, expand usage - Phase 1)
- opentelemetry_api 1.5.0 (already present, expand - Phase 3)

### Success Criteria

✅ All gaps documented
✅ Performance baseline established
✅ Phased roadmap created
✅ Risk assessment completed
✅ Resource allocation planned

### Deliverables

1. ✅ This document (MCP_IMPLEMENTATION_PHASING.md)
2. ✅ Performance analysis (MCP_SPEC_PERFORMANCE_ANALYSIS.md)
3. ✅ Architecture documentation (docs/architecture.md)
4. ✅ Module inventory (164 modules catalogued)

---

## Phase 1: Core Improvements (Performance Foundation)

**Duration:** 4-6 weeks
**Effort:** 160-240 hours
**Risk:** Low-Medium
**Target:** v2.2.0 Release

### Objectives

1. Eliminate schema validation bottleneck (5-20ms → 1-5ms)
2. Replace jsx with jiffy for JSON encoding (0.5-2ms → 0.2-0.7ms)
3. Implement async tool execution (unblock gen_server)
4. Add schema caching infrastructure (ETS-based)
5. Optimize resource subscription fan-out

### Architecture Design

#### 1.1 Schema Validation Caching

**Problem:** jesse recompiles schemas on every validation (5-20ms overhead)

**Solution:** ETS-backed compiled schema cache

**Supervision Tree Addition:**
```
erlmcp_core_sup (one_for_one)
├── erlmcp_schema_cache (gen_server)  ← NEW
│   └── Behavior: Compile-once, cache in ETS, LRU eviction
```

**Child Spec:**
```erlang
#{id => erlmcp_schema_cache,
  start => {erlmcp_schema_cache, start_link, []},
  restart => permanent,
  shutdown => 5000,
  type => worker,
  modules => [erlmcp_schema_cache]}
```

**Behavior Choice:** `gen_server`
- **Rationale:** Stateful caching, synchronous schema compilation, TTL management
- **State:** `#{schemas => ETS table, compile_stats => map(), lru => queue()}`

**API:**
```erlang
-module(erlmcp_schema_cache).
-behaviour(gen_server).

-export([validate/2, preload_schema/2, get_stats/0]).

%% Pre-compile and cache schema
-spec preload_schema(SchemaId :: binary(), Schema :: map()) -> ok.

%% Validate using cached compiled schema
-spec validate(SchemaId :: binary(), Data :: map()) ->
    {ok, map()} | {error, jesse:validation_error()}.

%% Get cache statistics
-spec get_stats() ->
    #{hits => non_neg_integer(),
      misses => non_neg_integer(),
      compile_time_saved_ms => non_neg_integer()}.
```

**Performance Impact:**
- **Before:** 5-20ms per validation (recompile every time)
- **After:** 1-5ms per validation (cached lookup + validate)
- **Improvement:** 75% latency reduction
- **Throughput:** 5x improvement for validation-heavy workloads

**Rationale:**
- Jesse's compilation is expensive but deterministic
- Schemas rarely change at runtime
- ETS provides O(1) lookup with minimal memory overhead
- LRU eviction prevents unbounded growth

---

#### 1.2 JSON Encoding Migration (jsx → jiffy)

**Problem:** jsx is pure Erlang, slow for large messages (0.5-2ms)

**Solution:** Migrate to jiffy (NIF-based, 2-3x faster)

**Module Changes:**
```
erlmcp_json_rpc.erl:
  -define(JSON_ENCODE, jsx:encode)     → jiffy:encode
  -define(JSON_DECODE, jsx:decode)     → jiffy:decode
  -define(JSON_OPTS, [return_maps])    → [return_maps, use_nil]
```

**Migration Strategy:**
1. Add jiffy to rebar.config dependencies
2. Create `erlmcp_json_codec` behavior abstraction
3. Implement both jsx and jiffy backends
4. Feature flag for A/B testing: `{json_codec, jiffy}`
5. Gradual rollout with monitoring

**Behavior Definition:**
```erlang
-module(erlmcp_json_codec).
-callback encode(term()) -> binary().
-callback decode(binary()) -> term() | {error, term()}.
-callback decode_options() -> [term()].

%% Implementations:
-module(erlmcp_json_codec_jsx).
-module(erlmcp_json_codec_jiffy).  ← Primary target
```

**Performance Impact:**
- **Before:** 0.5-2ms encoding + 0.5-2ms decoding = 1-4ms total
- **After:** 0.2-0.7ms encoding + 0.2-0.7ms decoding = 0.4-1.4ms total
- **Improvement:** 60-65% reduction
- **Throughput:** 30-60% improvement for message-heavy workloads

**Risk Assessment:**
- **Low:** jiffy is battle-tested (used by CouchDB, RabbitMQ)
- **Mitigation:** Feature flag allows instant rollback
- **Testing:** Property-based testing to ensure equivalence

**Rationale:**
- NIFs provide C-level performance for critical path
- jiffy is well-maintained and widely adopted
- Abstraction layer allows future codec swaps

---

#### 1.3 Async Tool Execution

**Problem:** Synchronous tool execution blocks gen_server (head-of-line blocking)

**Solution:** Spawn worker processes for tool execution with poolboy

**Architecture:**
```
erlmcp_core_sup (one_for_one)
├── erlmcp_tool_executor_sup (one_for_one)  ← NEW
│   └── poolboy pool of tool workers
```

**Supervision Tree:**
```
erlmcp_tool_executor_sup (one_for_one)
├── erlmcp_tool_pool (poolboy supervisor)
│   └── Pool of erlmcp_tool_worker (simple_one_for_one)
│       └── [10-50 worker processes]
```

**Child Specs:**
```erlang
%% Supervisor child spec
#{id => erlmcp_tool_executor_sup,
  start => {erlmcp_tool_executor_sup, start_link, []},
  restart => permanent,
  shutdown => infinity,
  type => supervisor}

%% Poolboy configuration
PoolArgs = [
    {name, {local, erlmcp_tool_pool}},
    {worker_module, erlmcp_tool_worker},
    {size, 10},           % Initial workers
    {max_overflow, 20}    % Additional on demand
]
```

**Behavior Choice:** `poolboy` + `gen_server` workers
- **Rationale:** Proven pool management, backpressure handling, worker reuse
- **Worker Behavior:** `gen_server` for timeout enforcement and monitoring

**Message Flow:**
```
Client Request
    ↓
erlmcp_server:handle_call({call_tool, Name, Args}, From, State)
    ↓
poolboy:transaction(erlmcp_tool_pool, fun(Worker) ->
    erlmcp_tool_worker:execute(Worker, Name, Args, Timeout)
end)
    ↓
Worker executes handler asynchronously
    ↓
gen_server:reply(From, Result)
```

**Performance Impact:**
- **Before:** 1 concurrent tool per server (serialized)
- **After:** 10-30 concurrent tools per server (pool size)
- **Throughput:** 10-30x for tool-heavy workloads
- **Latency:** No change for individual tools, but no head-of-line blocking

**Rationale:**
- Isolates long-running tools from gen_server event loop
- Provides timeout enforcement per tool
- Enables concurrent tool execution
- poolboy handles backpressure when all workers busy

---

#### 1.4 Resource Subscription Optimization

**Current:** `erlmcp_resource_subscriptions` exists but O(N) fan-out for N subscribers

**Enhancement:** Batch notifications + async sends

**Module Changes:**
```erlang
%% Before: Synchronous fan-out
notify_subscribers(Uri, Resource) ->
    Subscribers = get_subscribers(Uri),
    lists:foreach(fun(Pid) ->
        Pid ! {resource_updated, Uri, Resource}
    end, Subscribers).

%% After: Batched async fan-out
notify_subscribers(Uri, Resource) ->
    Subscribers = get_subscribers(Uri),
    spawn(fun() ->
        lists:foreach(fun(Pid) ->
            Pid ! {resource_updated, Uri, Resource}
        end, Subscribers)
    end).
```

**Additional Optimization:** Batch multiple resource updates
```erlang
%% Batch updates within 100ms window
notify_subscribers_batch(Updates) ->
    GroupedBySubscriber = group_by_subscriber(Updates),
    spawn(fun() ->
        maps:foreach(fun(Pid, Resources) ->
            Pid ! {resources_updated, Resources}
        end, GroupedBySubscriber)
    end).
```

**Performance Impact:**
- **Before:** O(N) blocking sends for N subscribers
- **After:** O(1) spawn + O(N) async sends
- **Improvement:** 30-50% for high subscriber counts (>1000)
- **Batching:** 70% reduction for bursty update patterns

---

### Dependencies

**External:**
- jiffy 1.1.1 (new dependency)
- poolboy 1.5.2 (already present, expand usage)

**Internal:**
- erlmcp_schema_cache → jesse, ETS
- erlmcp_tool_executor_sup → poolboy, erlmcp_registry
- erlmcp_json_codec → jiffy/jsx (abstraction)

### Effort Estimate

| Task | Hours | Risk |
|------|-------|------|
| Schema caching design | 16 | Low |
| Schema caching implementation | 40 | Low |
| Schema caching tests (EUnit + CT) | 24 | Low |
| jiffy migration design | 8 | Low |
| jiffy codec implementation | 24 | Medium |
| jiffy migration tests | 16 | Medium |
| Async tool execution design | 16 | Medium |
| Tool executor supervisor | 32 | Medium |
| Tool executor tests | 24 | Medium |
| Subscription optimization | 16 | Low |
| Performance benchmarking | 24 | Low |
| Documentation | 16 | Low |
| **TOTAL** | **256 hours** | **Low-Medium** |

### Success Criteria

1. ✅ Schema validation P95 latency < 5ms (from 20ms)
2. ✅ JSON encoding/decoding P95 latency < 1ms (from 2ms)
3. ✅ Tool execution supports 10+ concurrent tools per server
4. ✅ Subscription fan-out < 10ms for 1000 subscribers
5. ✅ All tests pass (EUnit + CT + Property)
6. ✅ Performance regression < 5% on non-optimized paths
7. ✅ Backward compatibility maintained

### Risk Assessment

| Risk | Probability | Impact | Mitigation |
|------|-------------|--------|------------|
| jiffy NIF crashes | Low | High | Feature flag, extensive testing, crash isolation |
| Schema cache memory leak | Low | Medium | LRU eviction, monitoring, configurable limits |
| Pool exhaustion | Medium | Medium | Configurable overflow, monitoring, backpressure |
| Async tool timeout issues | Medium | Medium | Per-tool timeout, worker monitoring, retry logic |
| Breaking changes in API | Low | High | Comprehensive integration tests, versioning |

### Deliverables

1. `erlmcp_schema_cache` module + supervisor integration
2. `erlmcp_json_codec` behavior + jiffy/jsx backends
3. `erlmcp_tool_executor_sup` + poolboy integration
4. Optimized `erlmcp_resource_subscriptions`
5. Performance benchmark suite updates
6. Migration guide (jsx → jiffy)
7. v2.2.0 release notes

---

## Phase 2: Missing Features (MCP Spec Completeness)

**Duration:** 6-8 weeks
**Effort:** 240-320 hours
**Risk:** Medium
**Target:** v2.3.0 Release

### Objectives

1. Complete sampling/createMessage implementation (full LLM provider support)
2. Verify and complete prompt template functionality
3. Full resource subscription lifecycle (subscribe/unsubscribe/list)
4. Implement missing MCP spec features (roots, completion, ping)
5. Add comprehensive integration tests for all MCP methods

### Architecture Design

#### 2.1 Complete Sampling Implementation

**Current State:** `erlmcp_sampling` exists as basic gen_server, providers exist

**Enhancement:** Full sampling capability with streaming support

**Module Structure:**
```
erlmcp_core_sup (one_for_one)
├── erlmcp_sampling (gen_server)           ← ENHANCE
│   └── Delegates to provider modules
├── LLM Provider Modules:
│   ├── erlmcp_llm_provider_anthropic      ← ENHANCE (streaming)
│   ├── erlmcp_llm_provider_openai         ← ENHANCE (streaming)
│   ├── erlmcp_llm_provider_local          ← ENHANCE (streaming)
│   └── erlmcp_llm_provider_mock           ← Already complete
```

**Behavior Definition:**
```erlang
-module(erlmcp_llm_provider).

-callback create_message(
    Messages :: [map()],
    Params :: map()
) -> {ok, map()} | {error, term()}.

-callback create_message_stream(
    Messages :: [map()],
    Params :: map(),
    StreamPid :: pid()
) -> {ok, stream_ref()} | {error, term()}.

-callback get_supported_models() -> [binary()].

-callback get_provider_info() ->
    #{name => binary(),
      version => binary(),
      capabilities => [atom()]}.
```

**Streaming Architecture:**
```
Client calls sampling/createMessage with includeContext=stream
    ↓
erlmcp_sampling spawns stream worker
    ↓
erlmcp_llm_provider_anthropic:create_message_stream(...)
    ↓
SSE/WebSocket chunks sent to client via notifications
    ↓
Final result returned with metadata
```

**New Supervisor:**
```
erlmcp_core_sup (one_for_one)
├── erlmcp_sampling_stream_sup (simple_one_for_one)  ← NEW
│   └── [Dynamic stream workers per request]
```

**Child Spec:**
```erlang
#{id => erlmcp_sampling_stream_sup,
  start => {erlmcp_sampling_stream_sup, start_link, []},
  restart => permanent,
  shutdown => infinity,
  type => supervisor}
```

**Performance Targets:**
- Non-streaming: P95 < 500ms (depends on LLM provider)
- Streaming: First chunk < 200ms, subsequent chunks < 50ms
- Concurrent streams: 100+ per server

---

#### 2.2 Complete Prompt Template Support

**Current State:** `erlmcp_prompt_template` exists

**Verification Tasks:**
1. Audit against MCP spec (prompts/list, prompts/get)
2. Test argument substitution ({{arg}} replacement)
3. Test nested prompts (prompt references)
4. Validate error handling (missing args, invalid templates)

**Enhancement Requirements:**

**Module Additions:**
```erlang
-module(erlmcp_prompt_template).

%% New APIs
-spec list_prompts() -> {ok, [map()]} | {error, term()}.
-spec get_prompt(binary()) -> {ok, map()} | {error, term()}.
-spec render_prompt(binary(), map()) -> {ok, [map()]} | {error, term()}.
-spec validate_prompt_args(binary(), map()) -> ok | {error, [binary()]}.
```

**Testing Matrix:**
| Feature | Test Coverage | Status |
|---------|---------------|--------|
| prompts/list | 10 test cases | ✅ EXISTS |
| prompts/get | 15 test cases | ⚠️ VERIFY |
| Argument substitution | 20 test cases | ⚠️ VERIFY |
| Nested prompts | 8 test cases | ⚠️ ADD |
| Error handling | 12 test cases | ⚠️ ADD |

**Rationale:**
- Prompts are static metadata (no complex supervision needed)
- gen_server provides centralized template registry
- Validation prevents runtime errors

---

#### 2.3 Full Resource Subscription Lifecycle

**Current State:** `erlmcp_resource_subscriptions` exists in core_sup

**Enhancement:** Complete subscribe/unsubscribe/list API

**Module API Completion:**
```erlang
-module(erlmcp_resource_subscriptions).

%% Existing (verify)
-spec subscribe(binary(), pid()) -> ok | {error, term()}.
-spec unsubscribe(binary(), pid()) -> ok | {error, term()}.

%% New APIs
-spec list_subscriptions() -> {ok, [map()]}.
-spec list_subscriptions(pid()) -> {ok, [binary()]}.
-spec get_subscriber_count(binary()) -> non_neg_integer().
-spec get_all_subscribers(binary()) -> [pid()].
```

**State Management:**
```erlang
-record(state, {
    subscriptions :: ets:tid(),  % ETS: {Uri, Pid} → true
    reverse_index :: ets:tid(),  % ETS: {Pid, Uri} → true (for cleanup)
    monitors :: #{pid() => reference()}  % Process monitors
}).
```

**Monitoring Strategy:**
```erlang
%% Automatic cleanup on subscriber crash
handle_info({'DOWN', Ref, process, Pid, _Reason}, State) ->
    cleanup_subscriber(Pid, State),
    {noreply, State}.
```

**Performance Requirements:**
- subscribe: O(1) ETS insert + O(1) monitor
- unsubscribe: O(1) ETS delete + O(1) demonitor
- list_subscriptions: O(N) ETS scan (acceptable for admin)
- notify: O(M) for M subscribers (optimized in Phase 1)

---

#### 2.4 Missing MCP Spec Features

**Implementation Matrix:**

| Feature | Spec Section | Module | Status | Effort |
|---------|-------------|--------|--------|--------|
| roots/list | Resources | erlmcp_roots_server | ✅ EXISTS | 0h |
| completion | Completions | erlmcp_completion | ✅ EXISTS | 0h |
| ping | Lifecycle | erlmcp_server | ⚠️ ADD | 8h |
| logging/setLevel | Logging | erlmcp_logging | ⚠️ ADD | 16h |

**New APIs:**

```erlang
%% erlmcp_server additions
-spec handle_ping(ServerPid :: pid()) -> {ok, #{}}.

%% erlmcp_logging additions
-spec set_level(pid(), atom()) -> ok | {error, term()}.
-spec get_level(pid()) -> atom().
```

---

### Dependencies

**External:** None (use existing libraries)

**Internal:**
- erlmcp_sampling → LLM providers, streaming supervisor
- erlmcp_prompt_template → validation, rendering
- erlmcp_resource_subscriptions → ETS, monitoring
- erlmcp_server → ping, logging integration

### Effort Estimate

| Task | Hours | Risk |
|------|-------|------|
| Sampling streaming design | 16 | Medium |
| LLM provider enhancements (3 providers) | 60 | Medium |
| Sampling tests (streaming) | 24 | Medium |
| Prompt template audit | 16 | Low |
| Prompt template enhancements | 24 | Low |
| Prompt template tests | 16 | Low |
| Resource subscription completion | 24 | Low |
| Resource subscription tests | 16 | Low |
| Ping implementation | 8 | Low |
| Logging/setLevel implementation | 16 | Low |
| Integration test suite | 40 | Medium |
| MCP spec compliance testing | 24 | Medium |
| Documentation | 24 | Low |
| **TOTAL** | **308 hours** | **Medium** |

### Success Criteria

1. ✅ sampling/createMessage supports all providers (Anthropic, OpenAI, Local)
2. ✅ Streaming sampling works with SSE/WebSocket transports
3. ✅ prompts/list, prompts/get, argument substitution all working
4. ✅ resources/subscribe, resources/unsubscribe, automatic cleanup
5. ✅ ping, logging/setLevel implemented
6. ✅ 100% MCP spec method coverage
7. ✅ Integration tests for all new features

### Risk Assessment

| Risk | Probability | Impact | Mitigation |
|------|-------------|--------|------------|
| LLM provider API changes | Medium | Medium | Version pinning, provider abstraction |
| Streaming complexity | Medium | Medium | Incremental implementation, property testing |
| Subscription memory leak | Low | High | Monitoring, automatic cleanup, stress testing |
| Spec interpretation errors | Low | Medium | Reference implementation comparison |

### Deliverables

1. Enhanced `erlmcp_sampling` with streaming
2. Complete `erlmcp_prompt_template` implementation
3. Full `erlmcp_resource_subscriptions` API
4. Ping and logging/setLevel support
5. Comprehensive integration test suite
6. MCP spec compliance report (100% coverage)
7. v2.3.0 release notes

---

## Phase 3: Optimization (Performance & Scale)

**Duration:** 8-10 weeks
**Effort:** 320-400 hours
**Risk:** Medium-High
**Target:** v2.4.0 Release

### Objectives

1. RuVector intelligence integration (semantic routing, pattern matching)
2. Swarm topology for distributed load balancing
3. Distributed session management (Mnesia clustering)
4. HTTP/2 multiplexing optimization
5. Zero-copy binary handling
6. Process pooling for servers (multi-instance)

### Architecture Design

#### 3.1 RuVector Intelligence Integration

**Concept:** Semantic routing of MCP requests using HNSW vector search

**Use Cases:**
- Route tool calls to specialized handlers based on semantic similarity
- Cache frequently used resources with semantic TTL
- Predict resource access patterns for preloading

**Architecture:**
```
erlmcp_core_sup (one_for_one)
├── erlmcp_ruvector_sup (one_for_one)  ← NEW
│   ├── erlmcp_ruvector_index (gen_server)
│   │   └── HNSW index management (via NIFs)
│   ├── erlmcp_ruvector_embeddings (gen_server)
│   │   └── Generate embeddings (ONNX runtime)
│   └── erlmcp_ruvector_router (gen_server)
│       └── Semantic request routing
```

**Supervision Tree:**
```
erlmcp_ruvector_sup (one_for_one)
├── erlmcp_ruvector_index
│   └── State: HNSW index, ETS cache
├── erlmcp_ruvector_embeddings
│   └── State: ONNX model, pooled workers
└── erlmcp_ruvector_router
    └── State: Routing policies, metrics
```

**Behavior Choice:**
- **erlmcp_ruvector_index:** `gen_server` (stateful index, synchronous queries)
- **erlmcp_ruvector_embeddings:** `gen_server` + worker pool (async embedding generation)
- **erlmcp_ruvector_router:** `gen_server` (routing logic, policy updates)

**Integration Points:**
```erlang
%% In erlmcp_server:handle_call({call_tool, Name, Args}, From, State)
Embedding = erlmcp_ruvector_embeddings:embed(Name),
{ok, ToolHandler} = erlmcp_ruvector_router:find_handler(Embedding),
Result = ToolHandler(Args),
...
```

**Performance Targets:**
- Embedding generation: P95 < 2ms (ONNX NIF)
- HNSW search: P95 < 0.5ms (for 100K vectors)
- Routing overhead: P95 < 3ms total

**Dependencies:**
- ONNX Runtime Erlang NIFs (custom or via Rustler)
- HNSW library (hnswlib via NIF)
- Vector embedding model (e.g., all-MiniLM-L6-v2)

**Rationale:**
- Semantic routing enables intelligent load distribution
- HNSW provides sub-millisecond vector search
- Improves cache hit rates for similar requests

---

#### 3.2 Swarm Topology (Distributed Coordination)

**Concept:** Multi-node coordination for horizontal scaling

**Architecture:**
```
Node 1 (erlmcp@node1)              Node 2 (erlmcp@node2)
├── erlmcp_swarm_coordinator       ├── erlmcp_swarm_coordinator
│   └── Leader election (Raft)     │   └── Follower
├── erlmcp_swarm_router            ├── erlmcp_swarm_router
│   └── Consistent hashing         │   └── Consistent hashing
└── erlmcp_server instances        └── erlmcp_server instances
```

**Supervision Tree Addition:**
```
erlmcp_core_sup (one_for_one)
├── erlmcp_swarm_sup (one_for_one)  ← NEW
│   ├── erlmcp_swarm_coordinator (gen_statem)
│   │   └── State: leader | follower | candidate
│   ├── erlmcp_swarm_router (gen_server)
│   │   └── State: consistent hash ring, node health
│   └── erlmcp_swarm_gossip (gen_server)
│       └── State: membership, heartbeats
```

**Behavior Choices:**
- **erlmcp_swarm_coordinator:** `gen_statem` (Raft state machine: follower → candidate → leader)
- **erlmcp_swarm_router:** `gen_server` (hash ring updates, routing decisions)
- **erlmcp_swarm_gossip:** `gen_server` (membership protocol, failure detection)

**Child Specs:**
```erlang
#{id => erlmcp_swarm_sup,
  start => {erlmcp_swarm_sup, start_link, []},
  restart => permanent,
  shutdown => infinity,
  type => supervisor}

%% Coordinator child spec
#{id => erlmcp_swarm_coordinator,
  start => {erlmcp_swarm_coordinator, start_link, []},
  restart => permanent,
  shutdown => 5000,
  type => worker,
  modules => [erlmcp_swarm_coordinator]}
```

**Routing Strategy:**
```erlang
%% Consistent hashing for request distribution
route_request(Request, SwarmState) ->
    Hash = erlang:phash2(Request),
    Node = consistent_hash:find_node(Hash, SwarmState#state.ring),
    case Node of
        node() ->
            handle_locally(Request);
        RemoteNode ->
            rpc:call(RemoteNode, erlmcp_server, handle_request, [Request])
    end.
```

**Performance Targets:**
- Leader election: < 2s for 5-node cluster
- Hash ring update: < 100ms for node join/leave
- Cross-node routing overhead: < 5ms

**Rationale:**
- Horizontal scaling for >100K concurrent clients
- Fault tolerance via Raft consensus
- Consistent hashing minimizes re-balancing

---

#### 3.3 Distributed Session Management

**Enhancement:** Mnesia-based session replication across nodes

**Current State:** `erlmcp_session_backend` supports ETS, DETS, Mnesia

**Architecture:**
```
erlmcp_core_sup (one_for_one)
├── erlmcp_session_manager (gen_server)
│   └── Delegates to backend
├── erlmcp_session_backend (behavior)
│   ├── erlmcp_session_backend_ets (local)
│   ├── erlmcp_session_backend_dets (persistent)
│   └── erlmcp_session_backend_mnesia (distributed)  ← ENHANCE
└── erlmcp_session_replicator (gen_server)
    └── Cross-node replication
```

**Mnesia Schema:**
```erlang
-record(session, {
    id :: binary(),
    client_info :: map(),
    server_capabilities :: map(),
    created_at :: integer(),
    last_active :: integer(),
    metadata :: map()
}).

%% Replicated across nodes
mnesia:create_table(session, [
    {attributes, record_info(fields, session)},
    {disc_copies, [node() | nodes()]},
    {type, set},
    {index, [last_active]}
]).
```

**Replication Strategy:**
```
Session created on Node1
    ↓
Mnesia write (synchronous to disc, async to other nodes)
    ↓
Session available on all nodes within 100ms
    ↓
Client reconnects to Node2 → Session restored
```

**Performance Targets:**
- Session create: P95 < 10ms (Mnesia write)
- Session restore: P95 < 5ms (local Mnesia read)
- Cross-node replication: < 100ms (async)

**Rationale:**
- Mnesia provides built-in replication and transactions
- Session continuity across node failures
- O(1) lookups with ETS backing

---

#### 3.4 HTTP/2 Multiplexing Optimization

**Current State:** gun client supports HTTP/2, but not fully optimized

**Enhancement:** Connection pooling + stream multiplexing

**Architecture:**
```
erlmcp_transport_sup (one_for_one)
├── erlmcp_transport_http_pool_sup (one_for_one)  ← NEW
│   └── Pool of HTTP/2 connections
└── erlmcp_transport_http (gen_server)
    └── Uses pooled connections
```

**Poolboy Configuration:**
```erlang
PoolArgs = [
    {name, {local, http2_connection_pool}},
    {worker_module, erlmcp_http2_connection},
    {size, 5},            % 5 persistent HTTP/2 connections
    {max_overflow, 5}     % Up to 10 total
]
```

**Stream Multiplexing:**
```
Single HTTP/2 connection
    ↓
100+ concurrent streams
    ↓
Each stream = 1 MCP request
    ↓
Eliminates connection setup overhead
```

**Performance Targets:**
- Connection reuse: 99%+ (vs. per-request connections)
- Connection setup eliminated for warm paths
- Throughput: 2-3x for HTTP transport

---

#### 3.5 Zero-Copy Binary Handling

**Optimization:** Minimize binary copying in message pipeline

**Implementation:**
```erlang
%% Before: Binary copying
handle_transport_data(Bin, State) ->
    Decoded = jsx:decode(Bin),  % Copy 1
    Parsed = parse_message(Decoded),  % Copy 2
    route_message(Parsed, State).  % Copy 3

%% After: Sub-binary references
handle_transport_data(<<Header:4/binary, Payload/binary>>, State) ->
    % Payload is a sub-binary reference (zero-copy)
    Decoded = jiffy:decode(Payload),  % Uses sub-binary
    route_message(Decoded, State).
```

**Binary Pool:**
```erlang
%% Pre-allocate large binaries for reuse
-module(erlmcp_binary_pool).
-export([get_buffer/1, return_buffer/1]).

get_buffer(Size) ->
    case ets:lookup(buffer_pool, Size) of
        [{Size, Bin}] ->
            ets:delete(buffer_pool, Size),
            Bin;
        [] ->
            <<0:(Size*8)>>
    end.
```

**Performance Impact:**
- GC pressure: 30-50% reduction for large messages (>10KB)
- Latency: 10-20% improvement for message handling

---

#### 3.6 Server Process Pooling

**Concept:** Multiple gen_server instances per logical server for parallelism

**Architecture:**
```
erlmcp_server_sup (one_for_one)
├── erlmcp_server_pool_sup (simple_one_for_one)  ← NEW
│   └── Pool of N erlmcp_server instances
└── erlmcp_server_router (gen_server)
    └── Routes requests to server instances
```

**Routing Strategy:**
```erlang
%% Round-robin or consistent hashing
route_to_server(ServerId, Request) ->
    Pool = get_server_pool(ServerId),
    Instance = select_instance(Pool, Request),
    erlmcp_server:handle_request(Instance, Request).
```

**State Partitioning:**
```erlang
%% Shard state by resource URI or tool name
Shard = erlang:phash2(ToolName, PoolSize),
Instance = lists:nth(Shard + 1, Pool),
...
```

**Performance Targets:**
- Throughput: 10x improvement (10 instances vs. 1)
- Latency: No degradation (independent instances)
- Complexity: Moderate (requires state sharding)

---

### Dependencies

**External:**
- ONNX Runtime (Rustler NIF) - RuVector
- hnswlib (NIF wrapper) - RuVector
- Mnesia (OTP built-in) - Session replication

**Internal:**
- erlmcp_ruvector_sup → ONNX, HNSW
- erlmcp_swarm_sup → gen_statem (Raft), consistent hashing
- erlmcp_session_backend_mnesia → Mnesia
- erlmcp_transport_http_pool_sup → poolboy, gun

### Effort Estimate

| Task | Hours | Risk |
|------|-------|------|
| RuVector design | 24 | High |
| RuVector ONNX integration | 60 | High |
| RuVector HNSW integration | 40 | High |
| RuVector routing logic | 32 | Medium |
| RuVector tests | 24 | High |
| Swarm coordinator (Raft) | 60 | High |
| Swarm routing (consistent hash) | 32 | Medium |
| Swarm tests | 24 | High |
| Mnesia session backend | 24 | Medium |
| Session replication | 16 | Medium |
| HTTP/2 pooling | 24 | Medium |
| Zero-copy optimizations | 32 | Medium |
| Server pooling | 40 | High |
| Integration testing | 40 | High |
| Performance benchmarking | 32 | Medium |
| Documentation | 24 | Low |
| **TOTAL** | **528 hours** | **High** |

### Success Criteria

1. ✅ RuVector semantic routing operational (P95 < 3ms overhead)
2. ✅ Swarm topology supports 5+ node cluster
3. ✅ Mnesia session replication < 100ms cross-node
4. ✅ HTTP/2 connection reuse > 99%
5. ✅ Zero-copy reduces GC pressure by 30%+
6. ✅ Server pooling achieves 8x+ throughput improvement
7. ✅ All optimizations maintain backward compatibility

### Risk Assessment

| Risk | Probability | Impact | Mitigation |
|------|-------------|--------|------------|
| ONNX NIF crashes | Medium | High | Crash isolation, fallback to non-semantic routing |
| Raft split-brain | Low | High | Network partition testing, fencing |
| Mnesia corruption | Low | High | Regular backups, repair procedures |
| Performance regression | Medium | High | Continuous benchmarking, feature flags |
| Complexity increase | High | Medium | Comprehensive documentation, examples |

### Deliverables

1. `erlmcp_ruvector_sup` + ONNX/HNSW integration
2. `erlmcp_swarm_sup` + Raft coordinator
3. Enhanced `erlmcp_session_backend_mnesia`
4. HTTP/2 connection pooling
5. Zero-copy binary optimizations
6. Server pooling infrastructure
7. Performance benchmark results
8. v2.4.0 release notes

---

## Phase 4: Advanced Features (Next-Gen Integration)

**Duration:** 10-12 weeks
**Effort:** 400-480 hours
**Risk:** High
**Target:** v3.0.0 Release

### Objectives

1. claude-flow agent integration (MCP + SONA hybrid)
2. SONA routing layer (Rust FFI for <0.05ms latency)
3. Rust FFI layer for hot path optimization
4. Advanced caching (predictive, semantic)
5. GraphQL transport optimization
6. Kubernetes operator for deployment

### Architecture Design

#### 4.1 claude-flow Agent Integration

**Concept:** Hybrid architecture with erlmcp (MCP protocol) + claude-flow (SONA routing)

**Architecture:**
```
┌─────────────────────────────────────────────┐
│         claude-flow (Rust)                  │
│  ┌──────────────┬───────────────────────┐   │
│  │ SONA Router  │  MCP Client (erlmcp)  │   │
│  │ (<0.05ms)    │  (via STDIO)          │   │
│  │              │                       │   │
│  │ Shared Mem   │  JSON-RPC over STDIO  │   │
│  └──────┬───────┴───────────┬───────────┘   │
│         │                   │               │
└─────────┼───────────────────┼───────────────┘
          │                   │
          ▼                   ▼
  ┌─────────────┐     ┌──────────────┐
  │ Cached Data │     │ erlmcp_server│
  │ (mmap)      │     │ (full MCP)   │
  └─────────────┘     └──────────────┘
```

**Shared Memory Protocol:**
```rust
// Rust side (claude-flow)
struct SharedCache {
    resources: HashMap<String, Vec<u8>>,  // Resource cache
    tools: HashMap<String, ToolSchema>,   // Tool schemas
    timestamp: AtomicU64,                  // Last update
}

// Memory mapping
let cache_fd = shm_open("/erlmcp_cache", O_RDWR, 0o666);
let cache = mmap(cache_fd, SharedCache::SIZE);
```

**Erlang Side:**
```erlang
%% erlmcp_sona_cache (gen_server)
-module(erlmcp_sona_cache).

%% Update shared memory cache
update_cache(Resources, Tools) ->
    % Write to /dev/shm/erlmcp_cache via NIFs
    erlmcp_sona_nif:write_cache(Resources, Tools).
```

**Dual-Path Routing:**
```
Client Request
    ↓
claude-flow SONA Router
    ↓
    ├─→ Cached? → Shared Memory Read (<0.05ms) → Return
    │
    └─→ Uncached? → erlmcp STDIO → MCP Protocol (1-5ms) → Return
```

**Performance Targets:**
- Cached read latency: < 0.05ms (SONA requirement)
- Cache hit rate: > 80% for static resources
- Fallback latency: 1-5ms (existing erlmcp performance)

---

#### 4.2 SONA Routing Layer (Rust FFI)

**Concept:** Rust-based hot path for sub-millisecond latency

**Architecture:**
```
erlmcp_core_sup (one_for_one)
├── erlmcp_sona_sup (one_for_one)  ← NEW
│   ├── erlmcp_sona_cache (gen_server)
│   │   └── Manages shared memory region
│   ├── erlmcp_sona_router_nif (NIF)
│   │   └── Rust FFI for routing decisions
│   └── erlmcp_sona_monitor (gen_server)
│       └── Monitors Rust process health
```

**NIF Interface:**
```rust
// Rust implementation
#[rustler::nif]
fn route_request(request: Binary) -> Result<Binary, RustlerError> {
    // Zero-copy routing logic
    let req = parse_request(&request)?;
    let handler = SONA_ROUTER.find_handler(&req)?;
    Ok(handler.id.into())
}

#[rustler::nif]
fn cache_lookup(key: Binary) -> Result<Option<Binary>, RustlerError> {
    // Sub-microsecond cache lookup
    Ok(SHARED_CACHE.get(&key).map(|v| v.clone().into()))
}
```

**Erlang Wrapper:**
```erlang
-module(erlmcp_sona_router_nif).
-on_load(init/0).

init() ->
    Path = code:priv_dir(erlmcp_core),
    erlang:load_nif(Path ++ "/sona_router", 0).

route_request(_Request) ->
    erlang:nif_error(nif_not_loaded).

cache_lookup(_Key) ->
    erlang:nif_error(nif_not_loaded).
```

**Safety Guarantees:**
```erlang
%% Isolate NIF crashes
erlmcp_sona_monitor:start_link() ->
    % Monitor Rust process via port
    % Restart on crash without affecting erlmcp
    ...
```

**Performance Targets:**
- NIF call overhead: < 0.01ms
- Cache lookup: < 0.02ms
- Total SONA path: < 0.05ms (meets requirement)

---

#### 4.3 Rust FFI Layer Design

**Modules with Rust FFI:**

1. **erlmcp_sona_router_nif** - Request routing
2. **erlmcp_cache_nif** - High-performance caching
3. **erlmcp_json_nif** - Ultra-fast JSON parsing (alternative to jiffy)
4. **erlmcp_hnsw_nif** - Vector search for RuVector

**Build System:**
```
rebar.config:
{plugins, [rebar3_cargo]}.

{cargo_opts, [
    {crates, [
        {sona_router, "0.1.0"},
        {json_parser, "0.1.0"},
        {hnsw_index, "0.1.0"}
    ]}
]}.
```

**Safety Protocols:**
1. All NIFs run in separate schedulers (async NIFs)
2. Timeout enforcement on Erlang side
3. Crash isolation via monitoring
4. Fallback to pure Erlang on NIF failure

---

#### 4.4 Advanced Caching Strategies

**Predictive Cache:**
```erlang
%% Predict next resource access based on patterns
-module(erlmcp_cache_predictor).

predict_next_resources(CurrentUri, History) ->
    % Analyze access patterns
    % Pre-fetch likely next resources
    ...
```

**Semantic Cache:**
```erlang
%% Cache by semantic similarity (RuVector)
-module(erlmcp_semantic_cache).

get_similar(Query, Threshold) ->
    % Find cached results for semantically similar queries
    % Reduce redundant LLM calls
    ...
```

**Adaptive TTL:**
```erlang
%% Adjust TTL based on access frequency
update_ttl(Key, AccessCount) ->
    TTL = base_ttl() * math:log(AccessCount + 1),
    erlmcp_cache:set(Key, Value, TTL).
```

---

#### 4.5 Kubernetes Operator

**Concept:** Automated deployment and scaling on Kubernetes

**Operator Responsibilities:**
1. Deploy erlmcp clusters (StatefulSet)
2. Configure Mnesia for distributed sessions
3. Auto-scale based on load metrics
4. Health checks and automatic recovery

**CRD Definition:**
```yaml
apiVersion: erlmcp.io/v1
kind: ErlmcpCluster
metadata:
  name: mcp-production
spec:
  replicas: 5
  version: "3.0.0"
  sessionBackend: mnesia
  observability:
    otel:
      enabled: true
      endpoint: "http://jaeger:4318"
  resources:
    limits:
      memory: "4Gi"
      cpu: "2"
```

**Operator Implementation:**
```rust
// Using kube-rs
async fn reconcile(cluster: ErlmcpCluster, ctx: Context) -> Result<Action> {
    // 1. Create StatefulSet with N replicas
    // 2. Configure Mnesia cluster
    // 3. Set up load balancer
    // 4. Monitor health
    ...
}
```

---

### Dependencies

**External:**
- Rustler (Rust NIF framework)
- kube-rs (Kubernetes operator framework)
- Shared memory libraries (POSIX shm)

**Internal:**
- erlmcp_sona_sup → Rustler NIFs, shared memory
- claude-flow integration → STDIO transport
- Kubernetes operator → External deployment tool

### Effort Estimate

| Task | Hours | Risk |
|------|-------|------|
| claude-flow integration design | 32 | High |
| Shared memory protocol | 48 | High |
| SONA router Rust implementation | 80 | High |
| Erlang NIF wrappers | 40 | High |
| Rust FFI safety protocols | 32 | High |
| Advanced caching design | 24 | Medium |
| Predictive cache | 40 | Medium |
| Semantic cache (RuVector) | 32 | High |
| Kubernetes operator design | 24 | Medium |
| Operator implementation (Rust) | 80 | High |
| Helm charts | 16 | Low |
| Integration testing | 60 | High |
| Performance validation | 32 | High |
| Documentation | 40 | Medium |
| **TOTAL** | **580 hours** | **High** |

### Success Criteria

1. ✅ SONA path achieves < 0.05ms latency for cached reads
2. ✅ claude-flow integration functional with fallback
3. ✅ Rust NIFs stable (no crashes in 7-day stress test)
4. ✅ Predictive cache improves hit rate by 20%+
5. ✅ Kubernetes operator deploys 5-node cluster successfully
6. ✅ All features backward compatible with v2.x

### Risk Assessment

| Risk | Probability | Impact | Mitigation |
|------|-------------|--------|------------|
| Rust NIF crashes | Medium | Critical | Crash isolation, monitoring, fallback |
| Shared memory corruption | Low | High | Checksums, validation, recovery |
| Kubernetes complexity | Medium | Medium | Comprehensive testing, examples |
| SONA latency not achieved | Medium | High | Profiling, optimization, caching layers |
| Integration issues | High | High | Incremental development, extensive testing |

### Deliverables

1. claude-flow integration module
2. `erlmcp_sona_sup` with Rust NIFs
3. Advanced caching infrastructure
4. Kubernetes operator (Rust)
5. Helm charts for deployment
6. Performance benchmarks (SONA latency)
7. Migration guide (v2.x → v3.0)
8. v3.0.0 release notes

---

## Summary: Implementation Timeline

| Phase | Duration | Effort | Risk | Deliverable |
|-------|----------|--------|------|-------------|
| Phase 0 | 2 weeks | 40h | Low | Gap analysis (DONE) |
| Phase 1 | 4-6 weeks | 256h | Low-Med | v2.2.0 (Performance) |
| Phase 2 | 6-8 weeks | 308h | Medium | v2.3.0 (Completeness) |
| Phase 3 | 8-10 weeks | 528h | Medium-High | v2.4.0 (Optimization) |
| Phase 4 | 10-12 weeks | 580h | High | v3.0.0 (Advanced) |
| **TOTAL** | **30-38 weeks** | **1,712h** | **Medium-High** | **100% MCP Spec** |

**Estimated Calendar Time:** 7-9 months (with parallelization)

---

## Risk Mitigation Strategy

### High-Risk Areas

1. **Rust NIFs (Phases 3-4)**
   - **Mitigation:** Feature flags, crash isolation, extensive testing
   - **Fallback:** Pure Erlang alternatives

2. **Distributed Coordination (Phase 3)**
   - **Mitigation:** Raft implementation from battle-tested library
   - **Fallback:** Single-node operation

3. **Performance Targets (All Phases)**
   - **Mitigation:** Continuous benchmarking, incremental optimization
   - **Acceptance:** Graceful degradation for non-critical paths

### Testing Strategy

**Per-Phase:**
1. Unit tests (EUnit) for all new modules
2. Integration tests (Common Test) for cross-module behavior
3. Property-based tests (PropEr) for state machines
4. Performance regression tests (custom benchmarks)
5. Chaos engineering tests (erlmcp_chaos)

**Pre-Release:**
1. Full MCP spec compliance validation
2. 7-day stress test (50K concurrent connections)
3. Network partition testing (for distributed features)
4. Memory leak detection (valgrind, observer)
5. Security audit (static analysis, penetration testing)

---

## Resource Requirements

### Team Composition

**Phase 1-2 (Core + Completeness):**
- 1 Senior Erlang Engineer (full-time)
- 1 QA Engineer (50%)
- 1 Technical Writer (25%)

**Phase 3 (Optimization):**
- 1 Senior Erlang Engineer (full-time)
- 1 Distributed Systems Engineer (50%)
- 1 Performance Engineer (50%)
- 1 QA Engineer (full-time)

**Phase 4 (Advanced):**
- 1 Senior Erlang Engineer (full-time)
- 1 Rust Engineer (full-time)
- 1 DevOps Engineer (Kubernetes, 50%)
- 1 QA Engineer (full-time)
- 1 Technical Writer (50%)

### Infrastructure

- CI/CD: GitHub Actions (existing)
- Benchmarking: Dedicated 32-core server
- Kubernetes: 5-node test cluster
- Observability: Jaeger + Prometheus + Grafana

---

## Success Metrics

### Technical Metrics

| Metric | v2.1.0 Baseline | v3.0.0 Target | Phase |
|--------|-----------------|---------------|-------|
| P50 Latency | 5ms | 2ms | Phase 1 |
| P95 Latency | 20ms | 8ms | Phase 1 |
| P99 Latency | 50ms | 20ms | Phase 1-3 |
| Throughput | 10K req/s | 100K req/s | Phase 3 |
| Concurrent Connections | 50K | 200K | Phase 3 |
| SONA Latency | N/A | <0.05ms | Phase 4 |
| MCP Spec Coverage | 85% | 100% | Phase 2 |
| Cache Hit Rate | 60% | 85% | Phase 4 |

### Business Metrics

1. **Adoption:** 100+ production deployments
2. **Community:** 500+ GitHub stars
3. **Documentation:** 95%+ API coverage
4. **Support:** <24h issue response time
5. **Ecosystem:** 10+ third-party integrations

---

## Appendix A: Module Additions by Phase

### Phase 1
- `erlmcp_schema_cache` (gen_server)
- `erlmcp_json_codec` (behavior)
- `erlmcp_json_codec_jiffy` (behavior implementation)
- `erlmcp_tool_executor_sup` (supervisor)
- `erlmcp_tool_worker` (gen_server)

### Phase 2
- `erlmcp_sampling_stream_sup` (supervisor)
- `erlmcp_sampling_stream_worker` (gen_server)
- Enhanced `erlmcp_llm_provider_*` modules

### Phase 3
- `erlmcp_ruvector_sup` (supervisor)
- `erlmcp_ruvector_index` (gen_server)
- `erlmcp_ruvector_embeddings` (gen_server)
- `erlmcp_ruvector_router` (gen_server)
- `erlmcp_swarm_sup` (supervisor)
- `erlmcp_swarm_coordinator` (gen_statem)
- `erlmcp_swarm_router` (gen_server)
- `erlmcp_swarm_gossip` (gen_server)
- `erlmcp_server_pool_sup` (supervisor)
- `erlmcp_server_router` (gen_server)
- `erlmcp_transport_http_pool_sup` (supervisor)

### Phase 4
- `erlmcp_sona_sup` (supervisor)
- `erlmcp_sona_cache` (gen_server)
- `erlmcp_sona_router_nif` (NIF)
- `erlmcp_sona_monitor` (gen_server)
- `erlmcp_cache_predictor` (gen_server)
- `erlmcp_semantic_cache` (gen_server)

**Total New Modules:** 31 (164 → 195 modules)

---

## Appendix B: Dependency Matrix

```
Phase 1:
  jiffy ────→ erlmcp_json_codec_jiffy ────→ erlmcp_json_rpc
  poolboy ───→ erlmcp_tool_executor_sup ──→ erlmcp_server

Phase 2:
  gun/cowboy → erlmcp_sampling_stream_sup → erlmcp_sampling

Phase 3:
  hnswlib ───→ erlmcp_ruvector_index ─────→ erlmcp_ruvector_router
  Mnesia ────→ erlmcp_session_backend_mnesia
  Raft lib ──→ erlmcp_swarm_coordinator

Phase 4:
  Rustler ───→ erlmcp_sona_router_nif ────→ erlmcp_sona_cache
  kube-rs ───→ Kubernetes Operator (external)
```

---

## Appendix C: Testing Coverage Targets

| Component | Unit Tests | Integration | Property | Performance |
|-----------|-----------|-------------|----------|-------------|
| Schema Cache | 20 | 5 | 3 | 5 benchmarks |
| JSON Codec | 15 | 3 | 5 | 3 benchmarks |
| Tool Executor | 25 | 8 | 4 | 4 benchmarks |
| Sampling | 30 | 10 | 2 | 3 benchmarks |
| Subscriptions | 25 | 12 | 3 | 4 benchmarks |
| RuVector | 35 | 15 | 5 | 8 benchmarks |
| Swarm | 40 | 20 | 6 | 10 benchmarks |
| SONA | 30 | 18 | 4 | 12 benchmarks |

**Total Tests:** 600+ across all phases

---

## Document Change History

| Version | Date | Author | Changes |
|---------|------|--------|---------|
| 1.0.0 | 2026-02-01 | Erlang Architect | Initial phasing document |

---

**End of MCP Implementation Phasing Document**
