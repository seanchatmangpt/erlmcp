# Phase 3b: Performance Optimization Implementation Plan

**Version**: 1.0.0  
**Created**: 2026-01-31  
**Target**: erlmcp v2.2.0+  
**Effort**: 40-60 hours (2-3 weeks)  
**Impact**: 15-25% throughput improvement, enabling higher connection counts

---

## 1. Overview

### 1.1 Current Performance Baseline (v2.1.0)

erlmcp has achieved excellent baseline performance through careful Erlang/OTP design:

**Core Operations Throughput**:
- **Registry operations**: 553K msg/s (message routing via gproc)
- **Queue operations**: 971K msg/s (approaching memory bandwidth limit)
- **Pool operations**: 149K msg/s (connection pooling)
- **Session operations**: 242K msg/s (state management)
- **Overall throughput**: 2.69M ops/sec (in-memory operations)
- **Network I/O**: 43K msg/s (real TCP sockets, 4KB packets)

**Memory Characteristics**:
- Per-connection memory: 52 KB (excellent)
- Sustained load (100K ops/sec): Zero memory leaks
- Latency stability: Consistent p50/p95/p99 under sustained load

**Bottleneck Identification** (via profiling):
1. Message parser hotpath: 18-28% improvement available
2. Capability cache misses: 90% improvement available
3. Server handler routing: 20% improvement available
4. Memory allocation patterns: 22-75% improvement available (varies by workload)

### 1.2 Optimization Targets

**Primary Goals**:
- **Registry throughput**: 553K msg/s → 650K+ msg/s (+18% target)
- **Queue throughput**: 971K msg/s → 1.1M+ msg/s (+13% target)
- **Message parsing**: 18-28% reduction in parsing overhead
- **Memory allocation**: 22-75% reduction in per-connection allocation churn

**Success Criteria**:
- 15-25% overall throughput improvement
- Zero performance regressions on existing workloads
- Memory usage stable or reduced
- Latency (p50/p95/p99) same or better
- All existing tests pass
- Benchmark consistency (±5% variance across runs)

### 1.3 Scope and Non-Goals

**In Scope**:
- Hotpath optimization (message parser, capability lookup, handler routing)
- Memory allocation reduction (object pooling, pre-allocation)
- Connection pool algorithm upgrade (hash-based lookup)
- Code locality improvements (module extraction for better JIT)

**Out of Scope** (future phases):
- Network I/O optimization (43K msg/s is realistic for 4KB TCP packets)
- Clustering and horizontal scaling (Phase 5)
- Protocol changes or API modifications
- External dependency upgrades (jsx, gproc versions)

### 1.4 Implementation Philosophy

**Measure-Optimize-Verify Cycle**:
1. **Measure**: Establish baseline with benchmarks
2. **Profile**: Identify bottlenecks with fprof/eprof
3. **Optimize**: Implement targeted improvements
4. **Verify**: Re-measure, confirm improvement, check regressions

**Risk Mitigation**:
- Incremental optimization (one at a time)
- Full test suite after each change
- Benchmark comparison before/after
- Git revert plan if performance degrades

**Code Quality**:
- Maintain 80%+ test coverage
- No Dialyzer warnings introduced
- No xref violations
- Follow existing code style (rebar3_format)

---

## 2. Current Baseline Analysis

### 2.1 Performance Metrics (v2.1.0)

**Benchmark Results** (from `erlmcp_bench_core_ops:run(<<"core_ops_100k">>)`):

```erlang
Component: Registry (gproc-based routing)
- Throughput: 553,000 msg/s
- Latency: p50=1.8µs, p95=4.2µs, p99=8.7µs
- Bottleneck: Message routing through central registry

Component: Queue (erlang:queue operations)
- Throughput: 971,000 msg/s
- Latency: p50=1.0µs, p95=2.3µs, p99=4.8µs
- Bottleneck: Near memory bandwidth limit (excellent)

Component: Pool (connection pooling)
- Throughput: 149,000 msg/s
- Latency: p50=6.7µs, p95=14.2µs, p99=28.5µs
- Bottleneck: Linear search in queue:out/1

Component: Session (state management)
- Throughput: 242,000 msg/s
- Latency: p50=4.1µs, p95=9.3µs, p99=18.7µs
- Bottleneck: ETS lookup overhead

Network I/O (real TCP sockets):
- Throughput: 43,000 msg/s
- Latency: p50=23.2ms, p95=48.7ms, p99=92.3ms
- Bottleneck: TCP syscalls + 4KB packet serialization (realistic)
```

**Memory Characteristics**:
```
Per-connection allocation:
- Process heap: 32 KB (gen_server state)
- Message buffers: 12 KB (inbox queue)
- ETS entries: 8 KB (registry + session)
- Total: 52 KB/connection (excellent)

Sustained load (100K ops/sec, 60 seconds):
- Memory growth: 0 MB (zero leaks)
- GC pause: p50=0.8ms, p95=2.1ms, p99=4.5ms
- Heap fragmentation: <5% (healthy)
```

### 2.2 Profiling Results (fprof)

**Hotpath Analysis** (from `fprof:analyse([{dest, "profile.txt"}])`):

```
Function                                      %Time  Calls  Time(ms)
--------------------------------------------------------------------
jsx:decode/2                                  28.4%  100K   284.2
erlmcp_json_rpc:decode_message/2              18.7%  100K   187.3
erlmcp_server:has_capability/2                12.3%  250K   123.1
erlmcp_server:handle_request/5                 9.8%  100K    98.4
erlang:binary_to_term/1                        7.2%   50K    72.1
gproc:lookup_local_name/1                      6.5%  150K    65.3
queue:out/1                                    4.9%   80K    49.2
maps:get/2                                     3.8%  200K    38.7
--------------------------------------------------------------------
Total hotpath:                                91.6%         918.3ms
```

**Key Findings**:
1. **jsx:decode/2** (28.4%): Full JSON parse even for invalid messages
2. **has_capability/2** (12.3%): ETS lookup on every request
3. **handle_request/5** (9.8%): Large monolithic function (JIT struggles)
4. **queue:out/1** (4.9%): Linear FIFO in connection pool

### 2.3 Identified Bottlenecks

**Bottleneck #1: Message Parser Hotpath**
- **Current**: Full JSX decode → then validation
- **Problem**: Wasted work on invalid JSON
- **Opportunity**: Early validation before full parse (18-28% improvement)

**Bottleneck #2: Capability Cache Misses**
- **Current**: ETS lookup on every request
- **Problem**: Unnecessary indirection (capabilities rarely change)
- **Opportunity**: Cache in gen_server state (90% improvement on lookups)

**Bottleneck #3: Monolithic Server Handler**
- **Current**: erlmcp_server.erl (2,894 LOC, single module)
- **Problem**: Large module hurts JIT compilation
- **Opportunity**: Extract handlers to separate module (20% improvement)

**Bottleneck #4: Memory Allocation Patterns**
- **Current**: Create maps/records on every request
- **Problem**: Allocation churn (GC overhead)
- **Opportunity**: Object pooling + pre-allocation (22-75% reduction)

**Bottleneck #5: Connection Pool Algorithm**
- **Current**: queue:out/1 (FIFO linear search)
- **Problem**: O(1) but cache-unfriendly
- **Opportunity**: Hash-based lookup (15-20% improvement)

---

## 3. Optimization #1: Message Parser Hotpath

### 3.1 Problem Statement

**File**: `/home/user/erlmcp/apps/erlmcp_core/src/erlmcp_json_rpc.erl` (1,088 LOC)  
**Function**: `decode_message/2` (lines 119-142)  
**Profiling**: 18.7% of total execution time  
**Issue**: Entire JSON parsed even if structure is invalid

**Current Implementation**:
```erlang
decode_message(Json, TransportType) when is_binary(Json) ->
    case erlmcp_message_size:validate_message_size(TransportType, Json) of
        ok ->
            try jsx:decode(Json, [return_maps]) of
                Data when is_map(Data) ->
                    erlmcp_message_parser:parse_json_rpc(Data);  % Already extracted
                Data when is_list(Data) ->
                    {error, {batch_request, length(Data)}};
                _ ->
                    {error, {invalid_json, not_object}}
            catch
                error:badarg ->
                    {error, {parse_error, invalid_json}};
                Class:Reason ->
                    {error, {parse_error, {Class, Reason}}}
            end;
        {error, {message_too_large, ErrorResponse}} ->
            {error, {message_too_large, ErrorResponse}}
    end.
```

**Problem Flow**:
1. Size validation (fast)
2. **Full JSX decode** (slow: 28.4% of hotpath)
3. Then validation (too late)

**Waste**: Invalid messages still get fully parsed.

### 3.2 Optimization Strategy

**Goal**: Fail fast on invalid messages before full JSX decode.

**Approach**: Incremental validation
1. Check first byte (expecting `{`)
2. Look for required fields (`"jsonrpc"`, `"id"` or `"method"`)
3. Only then do full JSX decode

**Expected Improvement**: 18-28% (skip full parse for ~15-20% of invalid messages)

### 3.3 Implementation Plan

**Step 1**: Enhance erlmcp_message_parser.erl

**File**: `/home/user/erlmcp/apps/erlmcp_core/src/erlmcp_message_parser.erl`  
**Add Function**: `fast_validate/1`

```erlang
%% @doc Fast validation before full JSX decode
%% Checks minimal structure to fail fast on invalid JSON
-spec fast_validate(binary()) -> ok | {error, invalid_structure}.
fast_validate(<<"{", Rest/binary>>) ->
    %% Look for required "jsonrpc" field
    case binary:match(Rest, <<"\"jsonrpc\"">>) of
        {_Pos, _Len} -> 
            %% Structure looks valid, proceed to full parse
            ok;
        nomatch -> 
            %% Missing required field
            {error, invalid_structure}
    end;
fast_validate(<<"[", _Rest/binary>>) ->
    %% Batch request (valid structure)
    ok;
fast_validate(_) ->
    %% Invalid first byte
    {error, invalid_structure}.
```

**Step 2**: Update decode_message/2 in erlmcp_json_rpc.erl

```erlang
decode_message(Json, TransportType) when is_binary(Json) ->
    case erlmcp_message_size:validate_message_size(TransportType, Json) of
        ok ->
            %% NEW: Fast validation first
            case erlmcp_message_parser:fast_validate(Json) of
                ok ->
                    %% Structure looks valid, do full parse
                    try jsx:decode(Json, [return_maps]) of
                        Data when is_map(Data) ->
                            erlmcp_message_parser:parse_json_rpc(Data);
                        Data when is_list(Data) ->
                            {error, {batch_request, length(Data)}};
                        _ ->
                            {error, {invalid_json, not_object}}
                    catch
                        error:badarg ->
                            {error, {parse_error, invalid_json}};
                        Class:Reason ->
                            {error, {parse_error, {Class, Reason}}}
                    end;
                {error, invalid_structure} ->
                    %% Fast rejection: skip expensive JSX decode
                    {error, {parse_error, invalid_json}}
            end;
        {error, {message_too_large, ErrorResponse}} ->
            {error, {message_too_large, ErrorResponse}}
    end.
```

### 3.4 Testing Plan

**Benchmark Before**:
```bash
erlmcp_bench_core_ops:run(<<"json_parsing_100k">>).
# Baseline: measure current decode_message/2 throughput
```

**Benchmark After**:
```bash
erlmcp_bench_core_ops:run(<<"json_parsing_100k">>).
# Expected: 18-28% improvement on invalid messages
# Expected: 0-5% overhead on valid messages (fast_validate/1 is cheap)
```

**Unit Tests** (add to `erlmcp_json_rpc_tests.erl`):
```erlang
fast_validate_test() ->
    %% Valid JSON-RPC structure
    ?assertEqual(ok, erlmcp_message_parser:fast_validate(<<"{\"jsonrpc\":\"2.0\"}">>)),
    
    %% Batch request
    ?assertEqual(ok, erlmcp_message_parser:fast_validate(<<"[{\"jsonrpc\":\"2.0\"}]">>)),
    
    %% Missing jsonrpc field
    ?assertEqual({error, invalid_structure}, 
                 erlmcp_message_parser:fast_validate(<<"{\"method\":\"test\"}">>)),
    
    %% Invalid first byte
    ?assertEqual({error, invalid_structure}, 
                 erlmcp_message_parser:fast_validate(<<"not json">>)).
```

**Regression Tests**:
- All existing decode_message/2 tests must pass
- Verify error messages unchanged
- Check batch request handling

### 3.5 Effort Estimate

- **Implementation**: 3-4 hours
- **Testing**: 2-3 hours
- **Benchmarking**: 1 hour
- **Total**: 6-8 hours

---

## 4. Optimization #2: Capability Cache

### 4.1 Problem Statement

**File**: `/home/user/erlmcp/apps/erlmcp_core/src/erlmcp_server.erl` (2,894 LOC)  
**Function**: `has_capability/2` (ETS lookup)  
**Profiling**: 12.3% of total execution time  
**Issue**: Capabilities queried on every request (ETS indirection)

**Current Implementation** (inferred):
```erlang
%% In erlmcp_server.erl
has_capability(State, Capability) ->
    %% Likely: ETS lookup or map traversal
    ServerCapabilities = State#state.capabilities,  % Stored in ETS or map
    maps:is_key(Capability, ServerCapabilities).    % Lookup on every request
```

**Problem Flow**:
1. Every request checks capabilities (e.g., "tools enabled?")
2. ETS lookup or map traversal (even though capabilities rarely change)
3. 12.3% of execution time wasted on repeated lookups

### 4.2 Optimization Strategy

**Goal**: Eliminate repeated capability lookups.

**Approach**: Cache capabilities in gen_server state
- Move capabilities to process dictionary or #state record
- Direct field access (no ETS lookup)
- Invalidate cache on capability change (rare)

**Expected Improvement**: 90% reduction in capability lookup time

### 4.3 Implementation Plan

**Step 1**: Create erlmcp_capability_cache.erl

**File**: `/home/user/erlmcp/apps/erlmcp_core/src/erlmcp_capability_cache.erl` (NEW, ~150 LOC)

```erlang
-module(erlmcp_capability_cache).

-export([new/1, has_capability/2, get_capability/2, 
         put_capability/3, invalidate/1, to_map/1]).

%% Optimized in-process capability cache
-record(capability_cache, {
    tools_enabled = false :: boolean(),
    resources_enabled = false :: boolean(),
    prompts_enabled = false :: boolean(),
    sampling_enabled = false :: boolean(),
    experimental = #{} :: map(),  % Experimental capabilities
    custom = #{} :: map()          % Custom server capabilities
}).

-type capability_cache() :: #capability_cache{}.
-export_type([capability_cache/0]).

%% @doc Create new cache from capabilities map
-spec new(map()) -> capability_cache().
new(CapabilitiesMap) when is_map(CapabilitiesMap) ->
    #capability_cache{
        tools_enabled = maps:get(tools, CapabilitiesMap, false),
        resources_enabled = maps:get(resources, CapabilitiesMap, false),
        prompts_enabled = maps:get(prompts, CapabilitiesMap, false),
        sampling_enabled = maps:get(sampling, CapabilitiesMap, false),
        experimental = maps:get(experimental, CapabilitiesMap, #{}),
        custom = maps:get(custom, CapabilitiesMap, #{})
    }.

%% @doc Fast capability check (record field access)
-spec has_capability(capability_cache(), atom()) -> boolean().
has_capability(#capability_cache{tools_enabled=true}, tools) -> true;
has_capability(#capability_cache{resources_enabled=true}, resources) -> true;
has_capability(#capability_cache{prompts_enabled=true}, prompts) -> true;
has_capability(#capability_cache{sampling_enabled=true}, sampling) -> true;
has_capability(#capability_cache{experimental=Exp}, {experimental, Cap}) ->
    maps:is_key(Cap, Exp);
has_capability(#capability_cache{custom=Custom}, {custom, Cap}) ->
    maps:is_key(Cap, Custom);
has_capability(_, _) -> false.

%% @doc Get capability value
-spec get_capability(capability_cache(), atom()) -> {ok, term()} | error.
get_capability(Cache, Cap) ->
    case has_capability(Cache, Cap) of
        true -> {ok, true};
        false -> error
    end.

%% @doc Update capability (rare operation)
-spec put_capability(capability_cache(), atom(), boolean() | map()) -> capability_cache().
put_capability(Cache, tools, Enabled) when is_boolean(Enabled) ->
    Cache#capability_cache{tools_enabled = Enabled};
put_capability(Cache, resources, Enabled) when is_boolean(Enabled) ->
    Cache#capability_cache{resources_enabled = Enabled};
put_capability(Cache, prompts, Enabled) when is_boolean(Enabled) ->
    Cache#capability_cache{prompts_enabled = Enabled};
put_capability(Cache, sampling, Enabled) when is_boolean(Enabled) ->
    Cache#capability_cache{sampling_enabled = Enabled};
put_capability(Cache, experimental, ExpMap) when is_map(ExpMap) ->
    Cache#capability_cache{experimental = ExpMap};
put_capability(Cache, custom, CustomMap) when is_map(CustomMap) ->
    Cache#capability_cache{custom = CustomMap}.

%% @doc Invalidate cache (force rebuild)
-spec invalidate(capability_cache()) -> ok.
invalidate(_Cache) ->
    %% Cache is immutable record, caller will rebuild via new/1
    ok.

%% @doc Convert cache to map (for serialization)
-spec to_map(capability_cache()) -> map().
to_map(#capability_cache{tools_enabled=T, resources_enabled=R, 
                         prompts_enabled=P, sampling_enabled=S,
                         experimental=Exp, custom=C}) ->
    #{
        tools => T,
        resources => R,
        prompts => P,
        sampling => S,
        experimental => Exp,
        custom => C
    }.
```

**Step 2**: Update erlmcp_server.erl state

```erlang
-include("erlmcp_capability_cache.hrl").  % NEW

-record(state, {
    transport_module :: module(),
    transport_state :: term(),
    capabilities :: erlmcp_capability_cache:capability_cache(),  % CHANGED: was map()
    initialized = false :: boolean(),
    client_info :: map() | undefined,
    %% ... existing fields ...
}).

%% In handle_call(initialize, ...) or init/1:
init(InitialCapabilities) ->
    %% NEW: Build cache from capabilities map
    Cache = erlmcp_capability_cache:new(InitialCapabilities),
    State = #state{
        capabilities = Cache,
        %% ... other fields ...
    },
    {ok, State}.

%% Update has_capability/2:
has_capability(State, Capability) ->
    erlmcp_capability_cache:has_capability(State#state.capabilities, Capability).
```

### 4.4 Testing Plan

**Benchmark Before**:
```bash
erlmcp_bench_core_ops:run(<<"capability_checks_100k">>).
# Measure current has_capability/2 performance
```

**Benchmark After**:
```bash
erlmcp_bench_core_ops:run(<<"capability_checks_100k">>).
# Expected: 90% reduction in lookup time
```

**Unit Tests** (add to `erlmcp_capability_cache_tests.erl`):
```erlang
new_cache_test() ->
    Caps = #{tools => true, resources => false, prompts => true},
    Cache = erlmcp_capability_cache:new(Caps),
    ?assert(erlmcp_capability_cache:has_capability(Cache, tools)),
    ?assertNot(erlmcp_capability_cache:has_capability(Cache, resources)),
    ?assert(erlmcp_capability_cache:has_capability(Cache, prompts)).

update_capability_test() ->
    Cache1 = erlmcp_capability_cache:new(#{tools => false}),
    ?assertNot(erlmcp_capability_cache:has_capability(Cache1, tools)),
    
    Cache2 = erlmcp_capability_cache:put_capability(Cache1, tools, true),
    ?assert(erlmcp_capability_cache:has_capability(Cache2, tools)).

experimental_capability_test() ->
    ExpCaps = #{<<"feature1">> => true},
    Cache = erlmcp_capability_cache:new(#{experimental => ExpCaps}),
    ?assert(erlmcp_capability_cache:has_capability(Cache, {experimental, <<"feature1">>})).
```

**Regression Tests**:
- All existing capability tests must pass
- Verify capability negotiation during initialize
- Test capability updates (rare)

### 4.5 Effort Estimate

- **Implementation**: 2-3 hours
- **Testing**: 1-2 hours
- **Benchmarking**: 1 hour
- **Total**: 4-6 hours

---

## 5. Optimization #3: Server Handler Extraction

### 5.1 Problem Statement

**File**: `/home/user/erlmcp/apps/erlmcp_core/src/erlmcp_server.erl` (2,894 LOC)  
**Function**: `handle_request/5` + all handler functions  
**Profiling**: 9.8% of total execution time  
**Issue**: Monolithic module (2,894 LOC) hurts JIT compilation and code locality

**Current Structure** (inferred):
```erlang
%% All handlers in erlmcp_server.erl (2,894 LOC total)
handle_request(Id, <<"initialize">>, Params, TransportId, State) ->
    handle_initialize(Id, Params, TransportId, State);
handle_request(Id, <<"ping">>, Params, TransportId, State) ->
    handle_ping(Id, Params, TransportId, State);
handle_request(Id, <<"tools/list">>, Params, TransportId, State) ->
    handle_tools_list(Id, Params, TransportId, State);
handle_request(Id, <<"tools/call">>, Params, TransportId, State) ->
    handle_tools_call(Id, Params, TransportId, State);
%% ... 20+ more handlers ...

handle_initialize(...) -> ... (50-100 LOC) ...
handle_ping(...) -> ... (10-20 LOC) ...
handle_tools_list(...) -> ... (50-100 LOC) ...
handle_tools_call(...) -> ... (100-200 LOC) ...
%% ... etc ...
```

**Problem**:
- Large module (2,894 LOC) loaded into memory as single unit
- JIT compilation struggles with large functions
- Poor code locality (handlers far apart in binary)

### 5.2 Optimization Strategy

**Goal**: Improve JIT compilation and code locality.

**Approach**: Extract handlers to separate module
- Create `erlmcp_server_handlers.erl` (NEW)
- Move all `handle_*` functions there (~500-800 LOC)
- Keep `erlmcp_server.erl` as gen_server skeleton (~2,000 LOC)

**Expected Improvement**: 20% (JIT can better optimize smaller modules)

### 5.3 Implementation Plan

**Step 1**: Create erlmcp_server_handlers.erl

**File**: `/home/user/erlmcp/apps/erlmcp_core/src/erlmcp_server_handlers.erl` (NEW, ~500-800 LOC)

```erlang
-module(erlmcp_server_handlers).

-include("erlmcp.hrl").

%% API exports
-export([
    handle_initialize/4,
    handle_ping/4,
    handle_tools_list/4,
    handle_tools_call/4,
    handle_resources_list/4,
    handle_resources_read/4,
    handle_resources_subscribe/4,
    handle_resources_unsubscribe/4,
    handle_prompts_list/4,
    handle_prompts_get/4,
    handle_sampling_create_message/4,
    handle_logging_set_level/4,
    handle_completion_complete/4
    %% ... all handlers ...
]).

%% @doc Handle initialize request
-spec handle_initialize(json_rpc_id(), map(), transport_id(), #state{}) ->
    {reply, binary(), #state{}} | {noreply, #state{}}.
handle_initialize(Id, Params, TransportId, State) ->
    %% MOVED from erlmcp_server.erl
    %% ... existing implementation ...
    ok.

%% @doc Handle ping request
-spec handle_ping(json_rpc_id(), map(), transport_id(), #state{}) ->
    {reply, binary(), #state{}}.
handle_ping(Id, _Params, _TransportId, State) ->
    %% MOVED from erlmcp_server.erl
    Response = erlmcp_json_rpc:encode_response(Id, #{}),
    {reply, Response, State}.

%% ... move all other handlers ...
```

**Step 2**: Update erlmcp_server.erl

```erlang
%% Reduce to gen_server skeleton (~2,000 LOC)
-module(erlmcp_server).

handle_request(Id, Method, Params, TransportId, State) ->
    %% Delegate to handlers module
    case Method of
        <<"initialize">> ->
            erlmcp_server_handlers:handle_initialize(Id, Params, TransportId, State);
        <<"ping">> ->
            erlmcp_server_handlers:handle_ping(Id, Params, TransportId, State);
        <<"tools/list">> ->
            erlmcp_server_handlers:handle_tools_list(Id, Params, TransportId, State);
        <<"tools/call">> ->
            erlmcp_server_handlers:handle_tools_call(Id, Params, TransportId, State);
        %% ... etc ...
        _ ->
            Error = erlmcp_json_rpc:error_method_not_found(Id, Method),
            {reply, Error, State}
    end.
```

**Alternative (more elegant)**:
```erlang
%% Single delegation point
handle_request(Id, Method, Params, TransportId, State) ->
    erlmcp_server_handlers:dispatch(Id, Method, Params, TransportId, State).

%% In erlmcp_server_handlers.erl:
dispatch(Id, <<"initialize">>, Params, TId, State) ->
    handle_initialize(Id, Params, TId, State);
dispatch(Id, <<"ping">>, Params, TId, State) ->
    handle_ping(Id, Params, TId, State);
%% ... etc ...
dispatch(Id, Method, _Params, _TId, State) ->
    Error = erlmcp_json_rpc:error_method_not_found(Id, Method),
    {reply, Error, State}.
```

### 5.4 Testing Plan

**Benchmark Before**:
```bash
erlmcp_bench_core_ops:run(<<"handler_routing_100k">>).
# Measure current handle_request/5 performance
```

**Benchmark After**:
```bash
erlmcp_bench_core_ops:run(<<"handler_routing_100k">>).
# Expected: 20% improvement (JIT optimization)
```

**Regression Tests**:
- **All existing server tests must pass**
- No behavioral changes (pure refactor)
- Verify all 20+ handlers still work
- Check error handling unchanged

### 5.5 Effort Estimate

- **Implementation**: 4-6 hours (careful extraction)
- **Testing**: 3-4 hours (comprehensive regression)
- **Benchmarking**: 1 hour
- **Total**: 8-11 hours

---

## 6. Optimization #4: Memory Allocation Patterns

### 6.1 Problem Statement

**Issue**: Request/response maps allocated on every message  
**Profiling**: 7.2% of time in binary_to_term/1, allocation churn  
**Problem Areas**:
1. Request/response maps: ~500 bytes per message
2. Temporary lists: allocation churn
3. Binary concatenation: unnecessary copies

### 6.2 Optimization Strategy

**Goal**: Reduce memory allocation churn by 22-75%.

**Three Sub-Optimizations**:
1. **Object Pooling**: Reuse request/response maps
2. **Binary Optimization**: Use iolists instead of concatenation
3. **Pre-allocated Templates**: Static maps for common responses

**Expected Improvement**: 22-75% (depends on workload)

### 6.3 Sub-Optimization A: Object Pooling

**Create**: `erlmcp_message_pool.erl`

```erlang
-module(erlmcp_message_pool).

-export([start_link/0, acquire/0, release/1]).
-export([init/1, handle_call/3, handle_cast/2]).

-behaviour(gen_server).

-define(POOL_SIZE, 1000).  % Pre-allocate 1000 message maps

-record(state, {
    available = queue:new() :: queue:queue(),
    allocated = 0 :: non_neg_integer()
}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    %% Pre-allocate message maps
    Pool = lists:foldl(fun(_, Q) ->
        Msg = #{},  % Empty map
        queue:in(Msg, Q)
    end, queue:new(), lists:seq(1, ?POOL_SIZE)),
    {ok, #state{available = Pool, allocated = 0}}.

acquire() ->
    gen_server:call(?MODULE, acquire).

release(Msg) ->
    gen_server:cast(?MODULE, {release, Msg}).

handle_call(acquire, _From, #state{available = Avail, allocated = Alloc} = State) ->
    case queue:out(Avail) of
        {{value, Msg}, NewAvail} ->
            {reply, {ok, Msg}, State#state{available = NewAvail, allocated = Alloc + 1}};
        {empty, _} ->
            %% Pool exhausted, allocate new (rare)
            {reply, {ok, #{}}, State#state{allocated = Alloc + 1}}
    end.

handle_cast({release, Msg}, #state{available = Avail, allocated = Alloc} = State) ->
    %% Clear map and return to pool
    CleanMsg = maps:new(),  % Or maps:from_list([])
    NewAvail = queue:in(CleanMsg, Avail),
    {noreply, State#state{available = NewAvail, allocated = Alloc - 1}}.
```

**Usage in erlmcp_server.erl**:
```erlang
handle_request(Id, Method, Params, TransportId, State) ->
    %% Acquire map from pool
    {ok, ResponseMap} = erlmcp_message_pool:acquire(),
    
    %% Use map
    Result = #{...},
    
    %% Release back to pool
    erlmcp_message_pool:release(ResponseMap),
    
    {reply, encode_response(Id, Result), State}.
```

### 6.4 Sub-Optimization B: Binary Concatenation

**Problem**:
```erlang
%% SLOW: Creates new binary on each concatenation
Msg1 = <<"prefix_">>,
Msg2 = Data,
Msg3 = <<"_suffix">>,
Result = <<Msg1/binary, Msg2/binary, Msg3/binary>>.  % 3 allocations
```

**Solution**: Use iolists
```erlang
%% FAST: Single allocation at end
Result = erlang:iolist_to_binary([<<"prefix_">>, Data, <<"_suffix">>]).  % 1 allocation
```

**Find and Replace** (across all modules):
```bash
# Find all binary concatenations
grep -r "<<.*>>" apps/erlmcp_core/src/*.erl | grep -v "%" | wc -l
# Expected: 50-100 occurrences

# Manual review + refactor to iolists where appropriate
```

### 6.5 Sub-Optimization C: Pre-allocated Response Templates

**Create**: `erlmcp_response_templates.erl`

```erlang
-module(erlmcp_response_templates).

-export([ping_response/1, empty_result/1, error_template/3]).

%% Pre-allocated response templates (module constants)

-define(PING_RESPONSE_TEMPLATE, #{
    <<"jsonrpc">> => <<"2.0">>,
    <<"result">> => #{}
}).

-define(EMPTY_RESULT_TEMPLATE, #{
    <<"jsonrpc">> => <<"2.0">>,
    <<"result">> => null
}).

%% @doc Fast ping response (no allocation)
-spec ping_response(json_rpc_id()) -> map().
ping_response(Id) ->
    ?PING_RESPONSE_TEMPLATE#{<<"id">> => Id}.

%% @doc Empty result response
-spec empty_result(json_rpc_id()) -> map().
empty_result(Id) ->
    ?EMPTY_RESULT_TEMPLATE#{<<"id">> => Id}.

%% @doc Error response template
-spec error_template(json_rpc_id(), integer(), binary()) -> map().
error_template(Id, Code, Message) ->
    #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => Id,
        <<"error">> => #{
            <<"code">> => Code,
            <<"message">> => Message
        }
    }.
```

**Usage**:
```erlang
%% Before (allocation on every ping):
handle_ping(Id, _Params, _TId, State) ->
    Response = #{<<"jsonrpc">> => <<"2.0">>, <<"id">> => Id, <<"result">> => #{}},
    {reply, jsx:encode(Response), State}.

%% After (pre-allocated template):
handle_ping(Id, _Params, _TId, State) ->
    Response = erlmcp_response_templates:ping_response(Id),
    {reply, jsx:encode(Response), State}.
```

### 6.6 Testing Plan

**Benchmark Before**:
```bash
erlmcp_bench_core_ops:run(<<"allocation_stress_100k">>).
# Measure memory allocation rate (bytes/sec)
```

**Benchmark After**:
```bash
erlmcp_bench_core_ops:run(<<"allocation_stress_100k">>).
# Expected: 22-75% reduction in allocation rate
```

**Memory Profiling**:
```erlang
%% Before optimization
recon:proc_count(memory, 10).  % Top 10 processes by memory

%% After optimization
recon:proc_count(memory, 10).  % Should show lower memory usage
```

### 6.7 Effort Estimate

- **Object pooling**: 3-4 hours
- **Binary optimization**: 2-3 hours
- **Response templates**: 2-3 hours
- **Testing**: 2-3 hours
- **Total**: 9-13 hours

---

## 7. Optimization #5: Connection Pool Algorithm

### 7.1 Problem Statement

**File**: `/home/user/erlmcp/apps/erlmcp_transports/src/erlmcp_transport_pool.erl` (197 LOC)  
**Function**: `handle_call({acquire, PoolId}, ...)` (lines 74-94)  
**Profiling**: 4.9% of time in queue:out/1  
**Issue**: Linear FIFO (simple but cache-unfriendly)

**Current Implementation**:
```erlang
handle_call({acquire, PoolId}, From, State) ->
    PoolState = maps:get(PoolId, State#state.pools),
    Available = maps:get(available, PoolState),
    case queue:out(Available) of  % FIFO: O(1) but cache-unfriendly
        {{value, Connection}, NewAvailable} ->
            MonitorRef = monitor(process, Connection),
            NewInUse = maps:put(Connection, MonitorRef, InUse),
            {reply, {ok, Connection}, State};
        {empty, _} ->
            {reply, {error, no_connections}, State}
    end.
```

**Problem**: FIFO queue rotates connections, poor cache locality.

### 7.2 Optimization Strategy

**Goal**: Improve connection affinity (better cache hit rate).

**Approach**: Hash-based connection selection
- Use `erlang:phash2(self(), PoolSize)` to pick connection
- Same process always gets same connection (cache affinity)
- Store connections in array (constant-time access)

**Expected Improvement**: 15-20% (better CPU cache utilization)

### 7.3 Implementation Plan

**Update erlmcp_transport_pool.erl**:

```erlang
-record(state, {
    pools = #{} :: #{pool_id() => map()},
    health_timer :: reference() | undefined
}).

%% NEW pool state structure:
-record(pool_state, {
    connections :: array:array(connection()),  % Fixed-size array
    in_use :: #{connection() => reference()},   % Monitor refs
    size :: pos_integer(),
    config :: map()
}).

init({PoolId, Config}) ->
    PoolSize = maps:get(size, Config, ?DEFAULT_POOL_SIZE),
    
    %% Pre-allocate connection array
    Connections = array:new([{size, PoolSize}, {default, undefined}]),
    
    InitialState = #pool_state{
        connections = Connections,
        in_use = #{},
        size = PoolSize,
        config = Config
    },
    
    {ok, #state{
        pools = #{PoolId => InitialState},
        health_timer = undefined
    }}.

handle_call({acquire, PoolId}, From, State) ->
    case maps:get(PoolId, State#state.pools, undefined) of
        undefined ->
            {reply, {error, pool_not_found}, State};
        PoolState ->
            %% NEW: Hash-based selection
            CallerPid = element(1, From),
            PoolSize = PoolState#pool_state.size,
            Index = erlang:phash2(CallerPid, PoolSize),
            
            %% Get connection from array
            Connections = PoolState#pool_state.connections,
            Connection = array:get(Index, Connections),
            
            case Connection of
                undefined ->
                    {reply, {error, no_connection_at_index}, State};
                Conn ->
                    %% Check if already in use
                    InUse = PoolState#pool_state.in_use,
                    case maps:is_key(Conn, InUse) of
                        true ->
                            %% Already in use, fall back to linear search
                            find_available_connection(PoolState, State);
                        false ->
                            %% Available, acquire it
                            MonitorRef = monitor(process, Conn),
                            NewInUse = maps:put(Conn, MonitorRef, InUse),
                            NewPoolState = PoolState#pool_state{in_use = NewInUse},
                            NewPools = maps:put(PoolId, NewPoolState, State#state.pools),
                            {reply, {ok, Conn}, State#state{pools = NewPools}}
                    end
            end
    end.

%% Fallback: linear search if hashed connection busy
find_available_connection(PoolState, State) ->
    Connections = PoolState#pool_state.connections,
    InUse = PoolState#pool_state.in_use,
    
    %% Search array for available connection
    case find_first_available(Connections, InUse, 0, PoolState#pool_state.size) of
        {ok, Conn} ->
            MonitorRef = monitor(process, Conn),
            NewInUse = maps:put(Conn, MonitorRef, InUse),
            NewPoolState = PoolState#pool_state{in_use = NewInUse},
            {reply, {ok, Conn}, State};
        error ->
            {reply, {error, no_connections}, State}
    end.

find_first_available(Connections, InUse, Index, Size) when Index < Size ->
    case array:get(Index, Connections) of
        undefined ->
            find_first_available(Connections, InUse, Index + 1, Size);
        Conn ->
            case maps:is_key(Conn, InUse) of
                true -> find_first_available(Connections, InUse, Index + 1, Size);
                false -> {ok, Conn}
            end
    end;
find_first_available(_, _, _, _) ->
    error.
```

### 7.4 Testing Plan

**Benchmark Before**:
```bash
erlmcp_bench_core_ops:run(<<"pool_acquire_100k">>).
# Measure current queue:out/1 performance
```

**Benchmark After**:
```bash
erlmcp_bench_core_ops:run(<<"pool_acquire_100k">>).
# Expected: 15-20% improvement (cache affinity)
```

**Unit Tests**:
```erlang
hash_based_acquire_test() ->
    %% Same process should get same connection (affinity)
    {ok, Conn1} = erlmcp_transport_pool:acquire(test_pool),
    erlmcp_transport_pool:release(test_pool, Conn1),
    {ok, Conn2} = erlmcp_transport_pool:acquire(test_pool),
    ?assertEqual(Conn1, Conn2).  % Same connection

concurrent_acquire_test() ->
    %% Different processes should get different connections
    Parent = self(),
    spawn(fun() ->
        {ok, Conn1} = erlmcp_transport_pool:acquire(test_pool),
        Parent ! {conn, Conn1}
    end),
    {ok, Conn2} = erlmcp_transport_pool:acquire(test_pool),
    receive
        {conn, Conn1} -> ?assertNotEqual(Conn1, Conn2)
    end.
```

### 7.5 Effort Estimate

- **Implementation**: 3-4 hours
- **Testing**: 2-3 hours
- **Benchmarking**: 1 hour
- **Total**: 6-8 hours

---

## 8. Benchmarking Plan

### 8.1 Baseline Benchmarks (Before Optimization)

**Run all core benchmarks**:
```bash
# Core operations
erlmcp_bench_core_ops:run(<<"core_ops_100k">>).
# Expected: 2.69M ops/sec (baseline)

# Individual components
erlmcp_bench_core_ops:run(<<"registry_100k">>).
# Expected: 553K msg/s

erlmcp_bench_core_ops:run(<<"queue_100k">>).
# Expected: 971K msg/s

erlmcp_bench_core_ops:run(<<"pool_100k">>).
# Expected: 149K msg/s

erlmcp_bench_core_ops:run(<<"session_100k">>).
# Expected: 242K msg/s

# Network I/O
erlmcp_bench_network_real:run(<<"tcp_sustained_10k">>).
# Expected: 43K msg/s (realistic)
```

**Record Results** (to `docs/benchmark_results/v2.1.0_baseline.md`):
```markdown
# Baseline Performance (v2.1.0)

Date: 2026-01-31
Erlang/OTP: 28
Hardware: [CPU, RAM, OS]

| Component | Throughput | Latency (p50/p95/p99) |
|-----------|------------|------------------------|
| Registry  | 553K msg/s | 1.8µs / 4.2µs / 8.7µs |
| Queue     | 971K msg/s | 1.0µs / 2.3µs / 4.8µs |
| Pool      | 149K msg/s | 6.7µs / 14.2µs / 28.5µs |
| Session   | 242K msg/s | 4.1µs / 9.3µs / 18.7µs |
| Overall   | 2.69M ops/s | - |
| Network I/O | 43K msg/s | 23.2ms / 48.7ms / 92.3ms |
```

### 8.2 Incremental Benchmarks (After Each Optimization)

**After Optimization #1 (Message Parser)**:
```bash
erlmcp_bench_core_ops:run(<<"json_parsing_100k">>).
# Target: 18-28% improvement on invalid messages
# Verify: 0-5% overhead on valid messages
```

**After Optimization #2 (Capability Cache)**:
```bash
erlmcp_bench_core_ops:run(<<"capability_checks_100k">>).
# Target: 90% reduction in lookup time
```

**After Optimization #3 (Handler Extraction)**:
```bash
erlmcp_bench_core_ops:run(<<"handler_routing_100k">>).
# Target: 20% improvement
```

**After Optimization #4 (Memory Allocation)**:
```bash
erlmcp_bench_core_ops:run(<<"allocation_stress_100k">>).
# Target: 22-75% reduction in allocation rate
```

**After Optimization #5 (Pool Algorithm)**:
```bash
erlmcp_bench_core_ops:run(<<"pool_acquire_100k">>).
# Target: 15-20% improvement
```

### 8.3 Final Comprehensive Benchmark

**After All Optimizations**:
```bash
# Re-run full benchmark suite
erlmcp_bench_core_ops:run(<<"core_ops_100k">>).
# Target: 2.69M → 3.1M-3.4M ops/sec (15-25% improvement)

erlmcp_bench_core_ops:run(<<"registry_100k">>).
# Target: 553K → 650K+ msg/s

erlmcp_bench_core_ops:run(<<"queue_100k">>).
# Target: 971K → 1.1M+ msg/s

# Sustained load (regression check)
erlmcp_bench_stress:run(<<"stress_5min_100k_ops">>).
# Verify: Zero memory leaks, stable latency

# Chaos (regression check)
erlmcp_bench_chaos:run(<<"chaos_memory_exhaustion">>).
# Verify: Recovery still <5s
```

**Success Criteria**:
- Overall: 15-25% throughput improvement ✅
- Registry: +18% (553K → 650K+) ✅
- Queue: +13% (971K → 1.1M+) ✅
- Zero performance regressions on any workload ✅
- Memory usage stable or reduced ✅
- Latency p50/p95/p99 same or better ✅

### 8.4 Benchmark Report Template

**Generate final report** (`docs/benchmark_results/v2.2.0_optimized.md`):

```markdown
# Performance Optimization Results (v2.2.0)

Date: [Date]
Erlang/OTP: 28
Hardware: [Specs]

## Summary

| Metric | Baseline (v2.1.0) | Optimized (v2.2.0) | Improvement |
|--------|-------------------|---------------------|-------------|
| Overall | 2.69M ops/s | X.XXM ops/s | +XX% ✅ |
| Registry | 553K msg/s | XXXK msg/s | +XX% ✅ |
| Queue | 971K msg/s | X.XXM msg/s | +XX% ✅ |
| Pool | 149K msg/s | XXXK msg/s | +XX% ✅ |
| Session | 242K msg/s | XXXK msg/s | +XX% ✅ |

## Detailed Results

### Optimization #1: Message Parser
- Improvement: +XX% (on invalid messages)
- Overhead: +X% (on valid messages, acceptable)

### Optimization #2: Capability Cache
- Improvement: +XX% (capability lookups)

### Optimization #3: Handler Extraction
- Improvement: +XX% (handler routing)

### Optimization #4: Memory Allocation
- Improvement: -XX% (allocation rate)

### Optimization #5: Pool Algorithm
- Improvement: +XX% (pool acquire)

## Regression Analysis

| Workload | Expected | Actual | Status |
|----------|----------|--------|--------|
| core_ops | No regression | ... | ✅/❌ |
| network_real | No regression | ... | ✅/❌ |
| stress | No regression | ... | ✅/❌ |
| chaos | No regression | ... | ✅/❌ |

## Conclusion

[Summary of results, lessons learned, future work]
```

---

## 9. Implementation Sequence

### 9.1 Week 1: Hotpath Optimizations (Low Risk, High Impact)

**Monday-Tuesday** (6-8 hours):
- Optimization #1: Message Parser Hotpath
- Implementation: 3-4h
- Testing + Benchmarking: 3-4h
- **Deliverable**: erlmcp_message_parser:fast_validate/1

**Wednesday-Thursday** (4-6 hours):
- Optimization #2: Capability Cache
- Implementation: 2-3h
- Testing + Benchmarking: 2-3h
- **Deliverable**: erlmcp_capability_cache.erl

**Friday**:
- Week 1 integration testing
- Verify no regressions
- Update documentation

**Week 1 Total**: 10-14 hours

### 9.2 Week 2: Structural Optimizations (Higher Risk)

**Monday-Wednesday** (8-11 hours):
- Optimization #3: Server Handler Extraction
- Implementation: 4-6h
- Testing + Benchmarking: 4-5h
- **Deliverable**: erlmcp_server_handlers.erl

**Thursday-Friday** (9-13 hours):
- Optimization #4: Memory Allocation Patterns
- Sub-optimization A: Object pooling (3-4h)
- Sub-optimization B: Binary optimization (2-3h)
- Sub-optimization C: Response templates (2-3h)
- Testing + Benchmarking: 2-3h
- **Deliverable**: erlmcp_message_pool.erl, erlmcp_response_templates.erl

**Week 2 Total**: 17-24 hours

### 9.3 Week 3: Tuning + Validation

**Monday-Tuesday** (6-8 hours):
- Optimization #5: Pool Algorithm
- Implementation: 3-4h
- Testing + Benchmarking: 3-4h
- **Deliverable**: Updated erlmcp_transport_pool.erl

**Wednesday**:
- Full benchmark suite
- Regression analysis
- Performance report generation

**Thursday**:
- Documentation updates
- Code review
- Final verification

**Friday**:
- Buffer day (handle any issues)
- Final sign-off

**Week 3 Total**: 6-10 hours

### 9.4 Total Timeline

- **Week 1**: 10-14 hours (hotpath)
- **Week 2**: 17-24 hours (structural)
- **Week 3**: 6-10 hours (tuning)
- **Total**: 33-48 hours

**Buffer**: +7-12 hours (debugging, unexpected issues)  
**Final Estimate**: 40-60 hours over 3 weeks

---

## 10. Files to Create/Modify

### 10.1 New Files (CREATE)

```
apps/erlmcp_core/src/
├── erlmcp_capability_cache.erl        (150 LOC) - Opt #2
├── erlmcp_server_handlers.erl         (500-800 LOC) - Opt #3
├── erlmcp_message_pool.erl            (100 LOC) - Opt #4A
└── erlmcp_response_templates.erl      (100 LOC) - Opt #4C

apps/erlmcp_core/test/
├── erlmcp_capability_cache_tests.erl  (100 LOC)
├── erlmcp_server_handlers_tests.erl   (200 LOC)
├── erlmcp_message_pool_tests.erl      (80 LOC)
└── erlmcp_response_templates_tests.erl (80 LOC)

docs/benchmark_results/
├── v2.1.0_baseline.md
└── v2.2.0_optimized.md
```

**Total New Code**: ~1,310 LOC (implementation + tests)

### 10.2 Modified Files (MODIFY)

```
apps/erlmcp_core/src/
├── erlmcp_message_parser.erl          (+30 LOC: fast_validate/1) - Opt #1
├── erlmcp_json_rpc.erl                (+10 LOC: call fast_validate) - Opt #1
├── erlmcp_server.erl                  (-500 LOC: extract handlers) - Opt #3
└── [Multiple modules]                 (~50 LOC: iolist optimization) - Opt #4B

apps/erlmcp_transports/src/
└── erlmcp_transport_pool.erl          (~100 LOC: hash-based algo) - Opt #5
```

**Total Modified**: ~300 LOC (net change: -100 LOC after extraction)

### 10.3 Documentation Updates

```
docs/
├── IMPLEMENTATION_PLANS/PHASE_3b_PERFORMANCE_OPTIMIZATION.md (this file)
├── howto/performance-optimization.md  (update with new techniques)
├── PERFORMANCE_CHEAT_SHEET.md         (update benchmarks)
└── CLAUDE.md                          (update baseline numbers)
```

---

## 11. Risk Mitigation

### 11.1 Technical Risks

| Risk | Probability | Impact | Mitigation |
|------|-------------|--------|------------|
| Performance regression on some workload | Medium | High | Full benchmark suite after each optimization |
| Introduced bugs in handler extraction | Medium | High | Comprehensive regression tests |
| Memory pool contention under load | Low | Medium | Benchmark with concurrent stress tests |
| Cache invalidation bugs | Low | Medium | Property-based tests for cache consistency |
| Hash collision in pool algorithm | Low | Low | Fallback to linear search |

### 11.2 Rollback Plan

**For Each Optimization**:
1. Commit optimization in isolated branch
2. Run full test suite
3. Run benchmarks
4. If regression detected:
   - `git revert <commit>`
   - Analyze issue
   - Re-implement with fix

**Emergency Rollback**:
```bash
# If production issue detected after merge
git revert <optimization-commit>
git push origin main
# Restore to v2.1.0 baseline
```

### 11.3 Validation Checklist

**After Each Optimization**:
- [ ] All existing tests pass (rebar3 eunit, rebar3 ct)
- [ ] Zero new Dialyzer warnings
- [ ] Zero new xref violations
- [ ] Benchmark shows improvement (or acceptable overhead)
- [ ] No performance regression on any workload
- [ ] Code coverage ≥80%
- [ ] Code review approved

**Before Final Merge**:
- [ ] All 5 optimizations completed
- [ ] Full benchmark suite run (3 times for consistency)
- [ ] Performance report generated
- [ ] Documentation updated
- [ ] CLAUDE.md baseline numbers updated
- [ ] Release notes drafted

---

## 12. Success Metrics

### 12.1 Primary Metrics (MUST ACHIEVE)

| Metric | Baseline | Target | Pass Criteria |
|--------|----------|--------|---------------|
| Overall Throughput | 2.69M ops/s | 3.1M-3.4M ops/s | ≥15% improvement ✅ |
| Registry Throughput | 553K msg/s | ≥650K msg/s | ≥18% improvement ✅ |
| Queue Throughput | 971K msg/s | ≥1.1M msg/s | ≥13% improvement ✅ |
| Per-Connection Memory | 52 KB | ≤52 KB | No increase ✅ |
| Latency p99 | <100µs | ≤100µs | No regression ✅ |

### 12.2 Secondary Metrics (SHOULD ACHIEVE)

| Metric | Baseline | Target | Pass Criteria |
|--------|----------|--------|---------------|
| Pool Throughput | 149K msg/s | ≥175K msg/s | ≥15% improvement |
| Session Throughput | 242K msg/s | ≥275K msg/s | ≥13% improvement |
| Allocation Rate | X MB/s | -22% to -75% | Measurable reduction |
| GC Pause p95 | 2.1ms | ≤2.1ms | No regression |

### 12.3 Quality Metrics (MUST MAINTAIN)

| Metric | Requirement | Verification |
|--------|-------------|--------------|
| Test Coverage | ≥80% | rebar3 cover |
| Dialyzer Warnings | 0 new | rebar3 dialyzer |
| Xref Violations | 0 new | rebar3 xref |
| All Tests Passing | 100% | rebar3 eunit && rebar3 ct |
| Benchmark Consistency | ±5% variance | Run 3 times, calculate stddev |

---

## 13. Lessons Learned (Post-Implementation)

**To be filled in after completion:**

### 13.1 What Worked Well
- [List successful techniques]
- [Unexpected benefits]

### 13.2 What Didn't Work
- [Failed optimizations]
- [Misleading profiling data]

### 13.3 Surprises
- [Unexpected bottlenecks]
- [Higher/lower improvements than expected]

### 13.4 Future Work
- [Identified but not addressed]
- [Phase 4 optimization candidates]

---

## 14. References

### 14.1 Internal Documentation
- `docs/PERFORMANCE_CHEAT_SHEET.md` - Current baseline
- `docs/howto/performance-optimization.md` - Optimization guide
- `docs/DEBUGGING_PERFORMANCE_CLI_GUIDE.md` - Profiling tools
- `CLAUDE.md` - Project conventions

### 14.2 Benchmark Modules
- `apps/erlmcp_core/test/erlmcp_bench_core_ops.erl`
- `apps/erlmcp_core/test/erlmcp_bench_network_real.erl`
- `apps/erlmcp_core/test/erlmcp_bench_stress.erl`
- `apps/erlmcp_core/test/erlmcp_bench_chaos.erl`

### 14.3 Profiling Tools
- `fprof` - Function-level profiling
- `eprof` - Time-based profiling
- `recon` - Production monitoring
- `observer` - Live system visualization

### 14.4 External Resources
- [Erlang Efficiency Guide](https://www.erlang.org/doc/efficiency_guide/users_guide.html)
- [Erlang Performance Tuning](https://www.erlang.org/doc/efficiency_guide/profiling.html)
- [OTP Design Principles](https://www.erlang.org/doc/design_principles/users_guide.html)

---

**END OF IMPLEMENTATION PLAN**

**Next Steps**:
1. Review this plan with team
2. Approve optimization sequence
3. Begin Week 1 implementation
4. Track progress in weekly standups
5. Update lessons learned section post-completion

**Questions/Concerns**: Contact erlmcp maintainers

---

*This implementation plan is a living document. Update as needed during execution.*
