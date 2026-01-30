# 80/20 Consolidation - Prioritized Action List

**Quick Reference**: Top actions ranked by impact and effort

---

## 🔴 CRITICAL PATH (Do First)

### 1. Remove Dead Code (EFFORT: 2 hours, IMPACT: HIGH)

**Delete all `.broken` files immediately**:
```bash
git rm apps/erlmcp_core/src/*.broken
git rm apps/erlmcp_core/test/*.broken
git rm apps/erlmcp_transports/src/*.broken
git rm apps/erlmcp_transports/test/*.broken
git commit -m "chore: remove 14 broken files - reducing codebase noise"
```

**Why**: Reduces cognitive load, clarifies what's production-ready
**Risk**: None (broken code doesn't work)
**Outcome**: -3,000 LOC, -14 files

---

### 2. Consolidate Pricing Modules (EFFORT: 1 week, IMPACT: HIGH)

**Consolidate 11 pricing modules → 2 modules**:

**Remove**:
- erlmcp_pricing_cli.erl
- erlmcp_pricing_http.erl
- erlmcp_pricing_loader.erl
- erlmcp_pricing_util.erl
- erlmcp_pricing_validator.erl
- erlmcp_pricing_state.erl
- erlmcp_pricing_upgrade.erl
- tcps_poka_yoke.erl (move logic into erlmcp_pricing_poka_yoke.erl)

**Keep**:
- erlmcp_pricing_receipt.erl (747 LOC) - Core receipt verification
- erlmcp_pricing_plan.erl (479 LOC) - SKU/plans management
- erlmcp_sla_monitor.erl (414 LOC) - SLA enforcement
- tcps_poka_yoke_validator.erl - Validation logic

**Create new consolidated module**:
```erlang
%% erlmcp_pricing_engine.erl (500 LOC target)
%% Consolidates: CLI, HTTP, loader, util, validator, state, upgrade

-export([
    init/1,
    verify_receipt/2,
    check_sla/2,
    update_sku/2,
    list_skus/0,
    validate_poka_yoke/2
]).
```

**Why**: 8 modules for a simple pricing system is over-engineered
**Risk**: Medium (TCPS quality system depends on this)
**Outcome**: -2,500 LOC, -8 modules, simpler API

---

### 3. Complete or Remove Auth (EFFORT: 2 weeks, IMPACT: HIGH)

**erlmcp_auth.erl has 3 major TODOs**:

**Option A: Complete (Recommended)**:
```erlang
%% Implement JWT validation
-define(JWT_TODO, false).
-spec validate_jwt(binary()) -> {ok, claims()} | {error, term()}.
validate_jwt(Token) ->
    %% Use jose library
    jose:verify(Token, PublicKey).

%% Implement OAuth2 token introspection
-spec introspect_token(binary()) -> {ok, map()} | {error, term()}.
introspect_token(Token) ->
    %% HTTP call to OAuth2 introspection endpoint
    httpc:request(post, {"https://auth.example.com/introspect", [], "application/json", Token}, [], []).

%% Implement mTLS certificate validation
-spec validate_mtls_certificate(binary()) -> {ok, certificate()} | {error, term()}.
validate_mtls_certificate(CertPEM) ->
    %% Use public_key:pkix_verify_cert
    public_key:pkix_verify_cert(CertPEM, []).
```

**Option B: Simplify (Fallback)**:
- Remove JWT/OAuth2/mTLS stubs
- Keep only API key validation (what works now)
- Document as "Basic Auth Only"

**Why**: Auth is security-critical, incomplete implementation is dangerous
**Risk**: High (security)
**Outcome**: Complete auth OR honest documentation of limitations

---

## 🟡 HIGH VALUE (Do Second)

### 4. Split erlmcp_server.erl (EFFORT: 1 week, IMPACT: HIGH)

**Current**: 2,040 LOC, too large, violates SRP

**Split into**:
```
erlmcp_server_core.erl (600 LOC)
  - add_tool/3, add_resource/3, add_prompt/3
  - delete_tool/2, delete_resource/2, delete_prompt/2
  - list_tools/1, list_resources/1, list_prompts/1

erlmcp_server_protocol.erl (500 LOC)
  - MCP 2025-11-25 initialization state machine
  - Capability negotiation
  - Phase tracking (initialization → ready)

erlmcp_server_handlers.erl (400 LOC)
  - register_notification_handler/3
  - report_progress/4
  - subscribe_resource/3
  - Notification dispatch
```

**API compatibility layer**:
```erlang
%% erlmcp_server.erl (300 LOC) - Thin wrapper
-module(erlmcp_server).
-export([start_link/2, add_tool/3, ...]).

%% Delegate to core modules
add_tool(Server, ToolId, Handler) ->
    erlmcp_server_core:add_tool(Server, ToolId, Handler).
```

**Why**: 2,040 LOC is untestable, hard to understand
**Risk**: Medium (API breakage)
**Outcome**: Better testability, clearer responsibilities

---

### 5. Remove Redundant Abstractions (EFFORT: 3 days, IMPACT: MEDIUM)

**Remove 6 unnecessary modules**:

**erlmcp_transport_validation.erl** (397 LOC):
```erlang
%% BEFORE: Custom wrapper around jesse
erlmcp_transport_validation:validate_schema(Json, Schema)

%% AFTER: Use jesse directly
jesse:validate(Schema, Json, [{default, module}])
```

**erlmcp_pool_strategy.erl** (300 LOC):
```erlang
%% BEFORE: Abstract strategy pattern
erlmcp_pool_strategy:create(fifo, Config)

%% AFTER: Use poolboy directly
poolboy:start_link([{strategy, fifo}, ...])
```

**erlmcp_transport_adapter.erl** (200 LOC):
```erlang
%% BEFORE: Unnecessary adapter
erlmcp_transport_adapter:call(TransportPid, Message)

%% AFTER: Direct gen_server call
gen_server:call(TransportPid, Message)
```

**erlmcp_transport_registry.erl** (546 LOC):
```erlang
%% BEFORE: Duplicate registry
erlmcp_transport_registry:register_transport(Id, Pid)

%% AFTER: Use erlmcp_registry
erlmcp_registry:register_transport(Id, Pid, Config)
```

**erlmcp_transport_discovery.erl** (588 LOC):
```erlang
%% BEFORE: Dynamic transport discovery
%% AFTER: Static configuration (simpler, more reliable)
```

**erlmcp_transport_pipeline.erl** (389 LOC):
```erlang
%% BEFORE: Complex pipeline abstraction
%% AFTER: Direct function calls (clearer control flow)
```

**Why**: Premature optimization, adds complexity without benefit
**Risk**: Low (straightforward refactoring)
**Outcome**: -1,500 LOC, simpler architecture

---

### 6. Standardize Error Handling (EFFORT: 1 week, IMPACT: MEDIUM)

**Create canonical error types**:

```erlang
%% include/erlmcp_errors.hrl

%% All modules use consistent error types
-type error() ::
    {error, invalid_id} |
    {error, overflow} |
    {error, validation_failed} |
    {error, not_found} |
    {error, already_registered} |
    {error, unauthorized} |
    {error, rate_limited} |
    {error, internal_error}.

%% Helper functions for creating errors
-spec invalid_id() -> {error, invalid_id}.
invalid_id() -> {error, invalid_id}.

%% Example usage:
%% BEFORE: Inconsistent
{error, invalid_id}
{error, "Invalid ID"}
{error, {invalid_id, 123}}
exit(invalid_id)

%% AFTER: Consistent
{error, invalid_id}
```

**Update all modules to use canonical errors**:
1. Add `-include("erlmcp_errors.hrl")` to all modules
2. Replace ad-hoc errors with canonical types
3. Update type specs: `-spec foo() -> {ok, result()} | {error, error()}.`

**Why**: Inconsistent error handling causes confusion and bugs
**Risk**: Low (backward compatible)
**Outcome**: Predictable error handling, better type safety

---

## 🟢 MEDIUM PRIORITY (Do Third)

### 7. Move Experimental Code to Examples (EFFORT: 2 days, IMPACT: MEDIUM)

**Move or delete experimental modules**:

**Move to examples/**:
```bash
mkdir -p examples/experimental
git mv apps/erlmcp_observability/src/erlmcp_chaos.erl examples/experimental/mcp_chaos_test.erl
git mv apps/erlmcp_observability/src/erlmcp_evidence_path.erl examples/experimental/
```

**Delete entirely** (better alternatives exist):
```bash
git rm apps/erlmcp_observability/src/erlmcp_debugger.erl
# Use: observer_cli, recon

git rm apps/erlmcp_observability/src/erlmcp_profiler.erl
# Use: fprof, eprof

git rm apps/erlmcp_core/src/erlmcp_cpu_guard.erl
git rm apps/erlmcp_core/src/erlmcp_memory_guard.erl
git rm apps/erlmcp_core/src/erlmcp_cpu_quota.erl
# Use: OS-level limits, vm_args.erl
```

**Why**: Experimental code in production codebase is confusing
**Risk**: None (examples are clearly non-production)
**Outcome**: Clearer separation of core vs experimental

---

### 8. Complete Resource Subscriptions (EFFORT: 1 week, IMPACT: MEDIUM)

**erlmcp_resource_subscriptions.erl has 5 TODOs**:

```erlang
%% TODO: Implement subscription management
%% TODO: Implement unsubscription
%% TODO: Implement subscriber listing
%% TODO: Implement notification dispatch

%% IMPLEMENT:
-spec subscribe(ServerPid, ResourceUri, Callback) -> ok.
subscribe(ServerPid, ResourceUri, Callback) ->
    gen_server:call(ServerPid, {subscribe, ResourceUri, Callback}).

-spec unsubscribe(ServerPid, ResourceUri) -> ok.
unsubscribe(ServerPid, ResourceUri) ->
    gen_server:call(ServerPid, {unsubscribe, ResourceUri}).

-spec list_subscribers(ServerPid, ResourceUri) -> [pid()].
list_subscribers(ServerPid, ResourceUri) ->
    gen_server:call(ServerPid, {list_subscribers, ResourceUri}).

%% Notification dispatch
handle_info({resource_updated, ResourceUri, NewValue}, State) ->
    Subscribers = maps:get(ResourceUri, State#state.subscriptions, []),
    [send_update(Sub, ResourceUri, NewValue) || Sub <- Subscribers],
    {noreply, State}.
```

**Why**: Resource subscriptions are core MCP feature, incomplete is confusing
**Risk**: Medium (core protocol feature)
**Outcome**: Complete MCP compliance

---

### 9. Simplify Rate Limiter (EFFORT: 2 days, IMPACT: LOW)

**erlmcp_rate_limiter.erl has multiple algorithms**:

**Current**: Token bucket + sliding window + leaky bucket
**Problem**: Too many options, unclear which to use

**Simplify to**:
```erlang
%% Keep ONLY sliding window (best for MCP)
%% Remove token bucket, leaky bucket

-module(erlmcp_rate_limiter).
-export([
    check_rate_limit/2,  %% Sliding window only
    configure/2
]).
```

**Why**: Multiple algorithms create confusion, sliding window is best practice
**Risk**: Low (keep sliding window, most common)
**Outcome**: Simpler API, clearer documentation

---

## 🔵 LOW PRIORITY (Technical Debt)

### 10. Add Type Specs (EFFORT: Ongoing, IMPACT: HIGH)

**Target**: 100% type spec coverage for all public APIs

**Priority**:
1. Tier 1 modules (P0) - 9 modules
2. Tier 2 modules (P1) - 12 modules
3. Transport modules (P1) - 21 modules

**Process**:
```bash
# Check coverage
grep -c "^-spec" apps/erlmcp_core/src/*.erl

# Add specs to all public functions
-spec function_name(Type1, Type2) -> ReturnType.
```

**Why**: Dialyzer catches bugs at compile time
**Risk**: None (incremental improvement)
**Outcome**: Better type safety

---

### 11. Complete Documentation (EFFORT: Ongoing, IMPACT: HIGH)

**Add @doc to all public APIs**:

**Template**:
```erlang
%% @doc One-sentence summary of what this function does.
%%
%% Longer explanation if needed. Can span multiple lines.
%%
%% @param ParamName Description of parameter
%% @returns Description of return value
%%
%% @example
%%   %% Show usage example
%%   erlmcp_client:call_tool(Client, <<"my_tool">>, #{arg => value}).
%%
-spec function_name(ParamType) -> ReturnType.
```

**Priority**:
1. Tier 1 APIs (client, server, json_rpc, registry)
2. Tier 2 APIs (rate_limiter, auth, circuit_breaker)
3. Transport APIs

**Why**: Incomplete documentation hinders adoption
**Risk**: None (incremental improvement)
**Outcome**: Better developer experience

---

### 12. Evaluate Code Reload (EFFORT: 1 day, IMPACT: UNKNOWN)

**erlmcp_code_reload.erl (565 LOC)** - Is this used?

**Investigation**:
```bash
# Search for usage
grep -r "erlmcp_code_reload" apps/

# Check if any tests use it
grep -r "code_reload" apps/*/test/

# Check rebar.config for reload config
grep reload rebar.config
```

**Decision criteria**:
- If unused in 6 months → REMOVE
- If rarely used → MOVE to examples/
- If actively used → KEEP and document

**Why**: High maintenance cost if unused
**Risk**: Low (investigation only)
**Outcome**: Keep or remove based on data

---

## 📊 Metrics Dashboard

**Track these metrics throughout consolidation**:

| Metric | Before | Target | Current |
|--------|--------|--------|---------|
| Total modules | 106 | 75 | 106 |
| Total LOC | 35,549 | 25,000 | 35,549 |
| TODO count | 24 | 0 | 24 |
| Broken files | 14 | 0 | 14 |
| Test coverage | ~60% | 90% | ~60% |
| Type spec coverage | ~70% | 100% (Tier 1) | ~70% |
| Dialyzer warnings | ? | 0 (Tier 1) | ? |

**Update after each completed action**

---

## 🎯 Success Criteria

**Phase 1 Complete (Week 2)**:
- [x] All .broken files deleted
- [x] Pricing modules consolidated
- [x] Auth completed or simplified
- [x] 0 TODOs in production code

**Phase 2 Complete (Week 4)**:
- [x] erlmcp_server.erl split
- [x] Redundant abstractions removed
- [x] Experimental code moved
- [x] Error handling standardized

**Phase 3 Complete (Week 8)**:
- [x] 75 modules remaining
- [x] 25,000 LOC or fewer
- [x] 90%+ test coverage
- [x] 100% type spec coverage (Tier 1)

---

## 🚀 Quick Start

**Week 1 - Do this NOW**:

```bash
# Day 1-2: Delete broken files
git rm apps/*/*/*.broken
git commit -m "chore: remove broken files"

# Day 3-4: Consolidate pricing (create branch)
git checkout -b feature/consolidate-pricing
# ... work on consolidation ...

# Day 5: Complete auth or document limitations
# Decide: Implement JWT/OAuth2 OR simplify to "Basic Auth Only"
```

**Week 2 - Continue**:
```bash
# Split erlmcp_server.erl
# Remove redundant abstractions
# Standardize error handling
```

---

**Remember**: The goal is to make the codebase **simpler, clearer, and more maintainable**. Every line of code removed is a line that doesn't need testing, debugging, or maintenance.

**Questions?** See full analysis in `docs/80_20_CONSOLIDATION_PLAN.md`
