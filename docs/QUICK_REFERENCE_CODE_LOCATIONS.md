# QUICK REFERENCE - CODE LOCATIONS FOR 57 ISSUES

**Purpose**: Map each issue to specific code files for rapid implementation
**Date**: 2026-01-27

---

## CRITICAL ISSUES (P0) - 10 ITEMS

### P0-1: Initialization State Machine
**Current Code**: `src/erlmcp_server.erl:150-200`
**Issue**: No strict state validation
**Files to Modify**:
- `src/erlmcp_server.erl` - Add state field + state machine
- `include/erlmcp.hrl` - Define state enum

**Quick Test**:
```erlang
% Add to erlmcp_server_tests.erl
test_init_state_machine() ->
    % Should reject messages in uninitialized state
    % Should timeout after 5s
    % Should accept init message
end.
```

---

### P0-2: Message ID Overflow
**Current Code**: `src/erlmcp_client.erl:200` (request_id = 1)
**Issue**: No overflow protection
**Files to Modify**:
- `src/erlmcp_client.erl` - Replace counter with unique_integer()
- `src/erlmcp_json_rpc.erl` - Validate ID format

**Quick Fix**:
```erlang
% Change from:
request_id = 1 :: integer(),

% To:
request_id = erlang:unique_integer() :: integer(),
```

---

### P0-3: Unsubscribe Missing
**Current Code**: `src/erlmcp_resource_subscriptions.erl` (~100 lines)
**Issue**: No unsubscribe, no cleanup
**Files to Modify**:
- `src/erlmcp_resource_subscriptions.erl` - Add unsubscribe/2
- `src/erlmcp_server.erl` - Add RPC handler for unsubscribe
- `src/erlmcp_json_rpc.erl` - Add method routing

**Quick Implementation**:
```erlang
% Add to erlmcp_resource_subscriptions.erl
unsubscribe(Uri, SubscriberPid) ->
    case gproc:lookup_local_name({resource_sub, Uri}) of
        undefined -> {error, not_found};
        Shard -> gen_server:call(Shard, {unsubscribe, Uri, SubscriberPid})
    end.

% In gen_server handle_call:
handle_call({unsubscribe, Uri, SubscriberPid}, _From, State) ->
    Subs = maps:get(Uri, State#state.subscriptions, sets:new()),
    NewSubs = sets:del_element(SubscriberPid, Subs),
    erlang:demonitor(SubscriberPid),  % Stop monitoring
    {reply, ok, State#state{subscriptions = maps:put(Uri, NewSubs, State#state.subscriptions)}}.
```

---

### P0-4: Tool Progress Timeout
**Current Code**: `src/erlmcp_progress.erl` (~500 lines)
**Issue**: No timeout enforcement
**Files to Modify**:
- `src/erlmcp_progress.erl` - Add timeout tracking
- `src/erlmcp_server.erl` - Start timeout on tool call
- `src/erlmcp_config.erl` - Add `tool_timeout_ms` config

**Quick Implementation**:
```erlang
% In erlmcp_server.erl handle_call for call_tool:
start_tool_timeout(ToolName, RequestId) ->
    Timeout = erlmcp_config:get_tool_timeout_ms(ToolName, 30000),
    erlang:send_after(Timeout, self(), {tool_timeout, RequestId}).

% In handle_info:
handle_info({tool_timeout, RequestId}, State) ->
    case maps:get(RequestId, State#state.pending_tools, undefined) of
        undefined -> {noreply, State};
        Tool ->
            Response = {error, <<"Tool call timeout">>, RequestId},
            send_response(Response, State),
            {noreply, maps:remove(RequestId, State#state.pending_tools)}
    end.
```

---

### P0-5: Path Traversal Security
**Current Code**: `src/erlmcp_uri_validator.erl` (~300 lines)
**Issue**: Incomplete RFC 3986 validation
**Files to Modify**:
- `src/erlmcp_uri_validator.erl` - Complete RFC 3986 implementation
- Add new module: `src/erlmcp_path_security.erl`

**Quick Check**:
```erlang
% Add to erlmcp_path_security.erl
validate_path_safe(Path) ->
    case string:find(Path, "..") of
        nomatch ->
            case string:find(Path, "//") of
                nomatch -> {ok, canonicalize_uri(Path)};
                _ -> {error, double_slash}
            end;
        _ -> {error, path_traversal}
    end.

% Test cases:
test_path_security() ->
    {error, path_traversal} = validate_path_safe("../../../etc/passwd"),
    {error, path_traversal} = validate_path_safe("/etc/../../../passwd"),
    {ok, _} = validate_path_safe("/etc/passwd"),
    {ok, _} = validate_path_safe("resources/file.txt"),
end.
```

---

### P0-6: Supervision Tree Redesign
**Current Code**: `src/erlmcp_sup.erl` (currently one_for_all)
**Issue**: Single point of failure
**Files to Modify**:
- `src/erlmcp_sup.erl` - Change to rest_for_one + hierarchical
- Create: `src/erlmcp_client_shard_sup.erl` (×8 supervisors)
- Create: `src/erlmcp_server_shard_sup.erl` (×8 supervisors)

**Current Tree**:
```
erlmcp_sup (one_for_all)  <-- PROBLEM
├── erlmcp_registry
├── erlmcp_client_sup
└── erlmcp_server_sup
```

**New Tree**:
```
erlmcp_sup (rest_for_one)
├── erlmcp_registry_sup (rest_for_one)
│   └── erlmcp_registry_sharded (64 shards)
├── erlmcp_client_shard_sup_0 (simple_one_for_one)
├── erlmcp_client_shard_sup_1
...
├── erlmcp_client_shard_sup_7
└── erlmcp_server_shard_sup_0 through _7
```

---

### P0-7: State Bloat Fixes
**Current Code**: `src/erlmcp_server.erl:80-120` (state record)
**Issue**: Maps grow unbounded
**Files to Modify**:
- `src/erlmcp_server.erl` - Move resources/tools/prompts to ETS
- `src/erlmcp_server.erl` - Add resource quota per connection

**Current Problem**:
```erlang
-record(state, {
    resources = map(),     % 100K connections × 10K resources = 1GB!
    tools = map(),
    prompts = map(),
    subscriptions = map(),
}).
```

**Quick Fix**:
```erlang
% Move to ETS in init:
init([ConnId]) ->
    % Don't store resources in process state
    % Instead: lookup from ETS on-demand
    State = #state{
        resource_quota = 100,  % Max resources per connection
        % resources = map(),  % REMOVE THIS!
    },
    {ok, State}.

% Retrieve resources from ETS:
get_resources(ConnId) ->
    ets:lookup(erlmcp_resources, ConnId).  % O(1) lookup
```

---

### P0-8: Backpressure Missing
**Current Code**: `src/erlmcp_backpressure.erl` (~300 lines, incomplete)
**Issue**: No flow control from network
**Files to Modify**:
- `src/erlmcp_backpressure.erl` - Complete implementation
- `src/erlmcp_transport_tcp.erl` - Add write queue monitoring
- `src/erlmcp_transport_http.erl` - Add write queue monitoring

**Quick Implementation**:
```erlang
% In erlmcp_backpressure.erl
check_backpressure() ->
    case process_info(self(), message_queue_len) of
        {_, QLen} when QLen > 10000 ->
            {backpressured, QLen};
        {_, QLen} ->
            {ok, QLen}
    end.

% In erlmcp_transport_tcp.erl handle_info:
handle_info({tcp, Socket, Data}, State) ->
    case check_backpressure() of
        {backpressured, _} ->
            % Slow down: don't call inet:setopts with {active, once}
            % Client TCP window will fill, causing sender slowdown
            {noreply, State#state{socket = Socket, active = false}};
        {ok, _} ->
            % Resume normal speed
            inet:setopts(Socket, [{active, once}]),
            {noreply, process_data(Data, State)}
    end.
```

---

### P0-9: HTTP Header Validation
**Current Code**: `src/erlmcp_http_header_validator.erl` (~500 lines)
**Issue**: RFC 2616 violations
**Files to Modify**:
- `src/erlmcp_http_header_validator.erl` - Add strict validation
- `src/erlmcp_http_middleware.erl` - Add validation middleware

**Quick Validation Rules**:
```erlang
validate_http_headers(Headers) ->
    % 1. Check Host header present
    case lists:keyfind('Host', 1, Headers) of
        false -> {error, missing_host};
        _ -> ok
    end,

    % 2. Check Content-Length vs Transfer-Encoding conflict
    ContentLength = lists:keyfind('Content-Length', 1, Headers),
    TransferEncoding = lists:keyfind('Transfer-Encoding', 1, Headers),
    case {ContentLength, TransferEncoding} of
        {{_, _}, {_, _}} -> {error, conflicting_headers};
        _ -> ok
    end,

    % 3. Check header size < 8KB
    HeaderSize = lists:sum([size(K) + size(V) || {K, V} <- Headers]),
    case HeaderSize > 8192 of
        true -> {error, header_too_large};
        false -> {ok, Headers}
    end.
```

---

### P0-10: Registry Sharding Expansion
**Current Code**: `src/erlmcp_registry_sharded.erl` (~500 lines)
**Issue**: 64 shards insufficient for 100K
**Files to Modify**:
- `src/erlmcp_registry_sharded.erl` - Change from 64 to 256 shards
- `src/erlmcp_config.erl` - Add `registry_shard_count` config

**Quick Change**:
```erlang
% In erlmcp_registry_sharded.erl
-define(DEFAULT_SHARD_COUNT, 256).  % Was 64, now 256

% During init:
init_shards() ->
    ShardCount = erlmcp_config:get(registry_shard_count, ?DEFAULT_SHARD_COUNT),
    [ets:new(shard_name(I), [...]) || I <- lists:seq(0, ShardCount - 1)].

shard_index(Key) ->
    erlang:phash2(Key, ?DEFAULT_SHARD_COUNT).  % Distributes across 256 shards
```

---

## HIGH-PRIORITY ISSUES (P1) - 11 ITEMS

### P1-1: Capability JSON Schema Validation
**File**: `src/erlmcp_capabilities.erl`
```erlang
% Add schema validation
validate_capabilities(Caps) ->
    Schema = capability_schema(),
    jesse:validate(Schema, Caps).
```

### P1-2: Error Response Validation
**File**: `src/erlmcp_json_rpc.erl:150-200`
```erlang
% Add error structure validation before sending
validate_error_response({error, Code, Message, Data}) when is_integer(Code), is_binary(Message) ->
    {ok, {error, Code, Message, Data}};
validate_error_response(Invalid) ->
    {error, {invalid_error, Invalid}}.
```

### P1-3: Sampling Completeness
**File**: `src/erlmcp_sampling.erl`
**Add**: Temperature, max_tokens, stop_sequences, caching

### P1-4: Pagination Cursor Support
**File**: `src/erlmcp_pagination.erl`
**Add**: Cursor-based pagination (not just offset/limit)

### P1-5: Token Rotation (Security)
**Files**: `src/erlmcp_http_auth.erl`
**Add**: Token rotation on each request, expiry tracking

### P1-6: Hot Code Reload
**File**: `src/erlmcp_server.erl`
**Add**: `sys:suspend/resume` callback handling

### P1-7: Distributed Clustering (Phase 1)
**Files**: Create `src/erlmcp_distributed.erl`, integrate gproc
**Benefit**: Foundation for multi-node 100K+ scaling

### P1-8: ETS Concurrent Access Flags
**File**: `src/erlmcp_registry_sharded.erl`
```erlang
% Add to ets:new calls:
ets:new(shard_name(I), [
    set,
    {read_concurrency, true},
    {write_concurrency, true},
    {decentralized_counters, true}
]).
```

### P1-9: WebSocket RFC 6455 Compliance
**File**: `src/erlmcp_transport_ws.erl`
**Add**: Ping/pong frames, opcode validation, reserved bits

### P1-10: SSE Retry Field Complete
**File**: `src/erlmcp_transport_sse.erl`
**Add**: Retry field on all events, heartbeat keepalive

### P1-11: Cascade Failure Detection
**File**: Create `src/erlmcp_cascade_detector.erl`
**Add**: Track failure rates, auto-isolation on cascade

---

## MEDIUM-PRIORITY ISSUES (P2) - 22 ITEMS

### P2-1 through P2-11: Medium Issues
Quick reference for each:

| # | Issue | File | Change | Effort |
|---|-------|------|--------|--------|
| P2-1 | Deadlock Detection | Create: `erlmcp_deadlock_detector.erl` | Monitor inter-process communication | 6h |
| P2-2 | Resource Exhaustion | Create: `erlmcp_resource_monitor.erl` | Track FD/memory/process count | 4h |
| P2-3 | Message Queue Monitor | `erlmcp_monitoring_sup.erl` | Monitor process mailboxes | 3h |
| P2-4 | Idempotency Keys | `erlmcp_server.erl` | Track processed keys in ETS | 5h |
| P2-5 | Idle Timeout | `erlmcp_transport_tcp.erl` | Close idle after 5min | 3h |
| P2-6 | Buffer Optimization | `erlmcp_queue_bounded.erl` | Dynamic buffer sizing | 2h |
| P2-7 | Memory Profiling | Create: `erlmcp_memory_profiler.erl` | Hourly memory reports | 4h |
| P2-8 | Dashboard | Create: `erlmcp_dashboard_handler.erl` | HTTP metrics endpoint | 5h |
| P2-9 | Regression Detection | Create: `erlmcp_regression_detector.erl` | Track metrics over time | 4h |
| P2-10 | Structured Logging | `erlmcp_logging.erl` | Add structured format + correlation IDs | 6h |
| P2-11-P2-22 | Various minor | Various | Minor spec completeness items | ~7h |

---

## TESTING CHECKLIST

### Unit Tests Required
- [ ] State machine tests (state transitions, timeouts)
- [ ] Message ID uniqueness tests (100K messages)
- [ ] Subscription lifecycle tests (subscribe/unsubscribe)
- [ ] Tool timeout tests
- [ ] Path validation security tests
- [ ] Supervision recovery tests (kill each shard)
- [ ] ETS concurrency tests (256 shards at 100K)
- [ ] Backpressure tests (force slow client)
- [ ] HTTP header validation tests (50+ cases)
- [ ] Registry expansion tests (256 shards)

### Integration Tests Required
- [ ] Full initialization flow
- [ ] Tool call with timeout
- [ ] Resource subscription lifecycle
- [ ] Connection recovery
- [ ] Message ordering
- [ ] Cascade failure prevention
- [ ] Load balancer compatibility (nginx)
- [ ] WebSocket fragmentation
- [ ] SSE retry mechanism

### Load Tests Required
- [ ] 100K concurrent connections
- [ ] 500K msg/sec sustained
- [ ] Memory stability (24h)
- [ ] GC pause time profiling
- [ ] CPU utilization tracking
- [ ] Connection cleanup

---

## VALIDATION TOOLS

### Dialyzer Check
```bash
rebar3 dialyzer
# Should pass with no warnings
```

### XRef Check
```bash
rebar3 xref
# Should pass with no undefined function calls
```

### Coverage Check
```bash
rebar3 cover
# Should maintain 80%+ coverage
```

### Load Test
```erlang
% Run in test console
erlmcp_performance_benchmark:run_100k_test().
```

---

## SUMMARY TABLE

| Phase | Issues | Files | Effort | Tests |
|-------|--------|-------|--------|-------|
| Phase 1 (P0) | 10 | 8-12 | 54h | 50+ |
| Phase 2 (P1-partial) | 5 | 5-8 | 20h | 30+ |
| Phase 3 | 2 | 3-5 | 16h | 20+ |
| Phase 4 | 4 | 3-4 | 18h | 40+ |
| Phase 5 | 11 | 6-10 | 24h | 25+ |
| Phase 6 | N/A | N/A | 30h | Load tests |
| **Total** | **57** | **30-50** | **162h** | **165+** |

---

## QUICK START

1. **Pick one P0 issue** to start (suggest: Initialization State Machine)
2. **Clone test file** from `test/erlmcp_server_tests.erl`
3. **Write test cases** first (TDD)
4. **Implement fix** in source file
5. **Run tests**: `rebar3 eunit --module=erlmcp_server_tests`
6. **Check dialyzer**: `rebar3 dialyzer`
7. **Create PR** with implementation

---

**Each implementation ticket should reference this document for code locations and quick fixes.**

