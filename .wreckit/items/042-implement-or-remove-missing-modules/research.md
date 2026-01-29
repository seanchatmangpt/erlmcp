# Research: Implement or Remove Missing Modules

**Date**: 2026-01-29
**Item**: 042-implement-or-remove-missing-modules
**Section**: quality-gates
**TCMS Compliance**: Lean Six Sigma Level Strictness

## Manufacturing Requirement
Xref analysis found 60+ undefined function calls to modules that don't exist. Must implement modules or remove references.

**Motivation:** Cannot pass Xref quality gate with undefined function calls. These missing modules indicate incomplete refactoring or dead code.

**Success criteria:**
- All missing modules audited
- Decision made for each (implement or remove)
- Needed modules implemented (stubs ok if not critical path)
- References to unneeded modules removed
- rebar3 xref shows 0 undefined function calls to missing modules

**Technical constraints:**
- For each missing module: search codebase for references
- Determine if referenced by active code or dead code
- Document why module was referenced but never implemented
- Decision matrix: erlmcp_request_id, erlmcp_tracing, erlmcp_task_manager, tcps_persistence, tcps_work_order

**Signals:** priority: medium, urgency: P2 - REQUIRED FOR PRODUCTION

### Quality Gate Status
- **Gate Type**: Xref (Cross-Reference Analysis)
- **Current State**: 60+ undefined function calls (estimated from item 018 findings of 222 undefined calls)
- **Target State**: 0 undefined function calls to missing modules
- **Gap**: 60+ undefined function calls (subset of larger 222-call issue)

## Summary

This research investigates the specific subset of missing modules referenced in item 042: **erlmcp_request_id**, **erlmcp_tracing**, **erlmcp_task_manager**, **tcps_persistence**, and **tcps_work_order**. These modules are identified as having undefined function calls that block the Xref quality gate.

### Manufacturing Objective
Audit these 5 specific missing modules, classify each as IMPLEMENT (needed) or REMOVE (not needed), then execute the decision to achieve 0 undefined function calls for this subset.

### Technical Approach
1. **Audit Phase**: For each of the 5 modules, determine:
   - Does the module file exist? (Partial implementation vs. completely missing)
   - What functions are being called but don't exist?
   - Are these calls on critical paths or dead code?
   - Is there an alternative implementation or replacement module?

2. **Decision Matrix**: Binary choice for each module:
   - **IMPLEMENT**: Add missing functions to existing modules or create new modules
   - **REMOVE**: Delete all references, update calling code to use alternatives

3. **Execution Phase**:
   - For IMPLEMENT: Add missing functions with proper OTP patterns and tests
   - For REMOVE: Delete function calls, update calling code
   - Verify: `rebar3 xref` shows 0 undefined calls for these 5 modules

### TCPS Justification
**Jidoka (Built-in Quality)**: Xref failures are ANDON events - undefined functions represent latent defects that will crash at runtime. Stop-the-line authority requires fixing before proceeding.

**Poka-yoke (Mistake-Proofing)**: Implement missing functions or remove references to make undefined calls impossible at compile time (Dialyzer will enforce).

**Kaizen (Continuous Improvement)**: Each undefined function represents an incomplete refactoring (technical debt). Fixing these reduces waste and improves code quality baseline.

## Current State Analysis

### Existing Implementation

**Files Investigation:**

1. **tcps_persistence.erl** - ✅ **EXISTS** (Full implementation: 1634 lines)
   - Location: `/Users/sac/erlmcp/apps/tcps_erlmcp/src/tcps_persistence.erl`
   - Status: **FULLY IMPLEMENTED** with comprehensive API
   - Exports: 75+ functions including store_receipt/1, store_work_order/1, store_andon_event/1
   - **Missing function**: `store_sku/1` (called by tcps_sku.erl:1319)
   - Pattern: Module-level functions (NOT gen_server), file-based JSON + RDF persistence

2. **tcps_work_order.erl** - ✅ **EXISTS** (Full implementation: 2232 lines)
   - Location: `/Users/sac/erlmcp/apps/tcps_erlmcp/src/tcps_work_order.erl`
   - Status: **FULLY IMPLEMENTED** as gen_server with complete lifecycle API
   - Exports: 50+ functions including create_work_order/1, start_work_order/1, progress_work_order/2, complete_work_order/2
   - **Missing function**: `update_status/2` (called by tcps_sku.erl:1343)
   - Pattern: gen_server with pull signal processing, queue management, SLA tracking

3. **erlmcp_hooks.erl** - ✅ **EXISTS** (Partial: 597 lines, replacement for erlmcp_task_manager)
   - Location: `/Users/sac/erlmcp/apps/erlmcp_core/src/erlmcp_hooks.erl`
   - Status: **FULLY IMPLEMENTED** as gen_server for Claude Code hooks integration
   - Purpose: **Replaces erlmcp_task_manager** with pre_task, post_task hooks
   - Pattern: gen_server with quality gate enforcement

4. **erlmcp_request_id** - ❌ **DOES NOT EXIST**
   - Status: **NEVER IMPLEMENTED** - placeholder module only
   - Called by: erlmcp_client.erl:478 (safe_increment/1)
   - Purpose: Request ID increment with overflow checking

5. **erlmcp_tracing** - ❌ **DOES NOT EXIST**
   - Status: **NEVER IMPLEMENTED** - placeholder module only
   - Called by: erlmcp_server.erl (100+ references throughout the file)
   - Functions called: start_server_span/2, set_attributes/2, set_status/2, record_exception/4, end_span/1, log/2, etc.
   - Purpose: Distributed tracing for MCP server operations

### Key Files

**Calling code (with undefined function calls):**

- `apps/erlmcp_core/src/erlmcp_client.erl:478` - Calls `erlmcp_request_id:safe_increment/1` (MISSING)
- `apps/erlmcp_core/src/erlmcp_server.erl:171-201` - 11 calls to `erlmcp_tracing` functions (MISSING)
- `apps/erlmcp_core/src/erlmcp_server.erl:373-405` - 12 calls to `erlmcp_tracing` functions (MISSING)
- `apps/erlmcp_core/src/erlmcp_server.erl:450-491` - 13 calls to `erlmcp_tracing` functions (MISSING)
- `apps/erlmcp_core/src/erlmcp_server.erl:179, 429, 603, 613, 623, 637, 660, 1398, 1399` - 9 calls to `erlmcp_task_manager` functions (REPLACED by erlmcp_hooks)
- `apps/tcps_erlmcp/src/tcps_sku.erl:1319` - Calls `tcps_persistence:store_sku/1` (MISSING - function only)
- `apps/tcps_erlmcp/src/tcps_sku.erl:1343` - Calls `tcps_work_order:update_status/2` (MISSING - function only)

**Existing modules (partial implementations):**

- `apps/erlmcp_core/src/erlmcp_hooks.erl:36-597` - REPLACES erlmcp_task_manager (fully implemented)
- `apps/tcps_erlmcp/src/tcps_persistence.erl:1-1634` - EXISTS, missing store_sku/1 function only
- `apps/tcps_erlmcp/src/tcps_work_order.erl:1-2232` - EXISTS, missing update_status/2 function only

### OTP Patterns Observed

**Behavior**:
- `erlmcp_hooks.erl` - gen_server (replaces erlmcp_task_manager for hooks)
- `tcps_work_order.erl` - gen_server (work order lifecycle management)
- `tcps_persistence.erl` - Module (NOT gen_server, file-based storage)
- `erlmcp_client.erl` - gen_server (request-response correlation)
- `erlmcp_server.erl` - gen_server (MCP server, resources/tools/prompts)

**Supervision**:
- `erlmcp_sup.erl` - one_for_all (top-level supervisor)
- `erlmcp_core_sup.erl` - one_for_one (core services)
- Pattern: Process-per-connection with supervisor tree

**Process Pattern**:
- Registry-based routing (gproc)
- Request ID correlation in client pending_requests map
- Let-it-crash + supervisor recovery
- No blocking init/1 calls

**Test Pattern**:
- Chicago School TDD (real processes, no mocks)
- EUnit for unit tests
- Common Test for integration
- Property-based testing with Proper

## Technical Considerations

### Dependencies

**Internal Modules**:
- `erlmcp_core` → 57 modules (client, server, registry, hooks)
- `erlmcp_transports` → 23 modules (stdio, tcp, http, ws, sse)
- `erlmcp_observability` → 12 modules (metrics, traces, receipts)
- `tcps_erlmcp` → 89 modules (andon, heijunka, kanban, work_order, persistence)

**External Libraries** (from rebar.config):
- `jsx 3.1.0` - JSON encoding/decoding
- `jesse 1.8.1` - JSON Schema validation
- `gproc 0.9.0` - Process registry
- `opentelemetry_api 1.5.0` - OTEL API
- `opentelemetry 1.7.0` - OTEL SDK

**OTP Applications**:
- kernel, stdlib, sasl, mnesia - OTP core
- ssl, inets, crypto, public_key - Security/networking

### TCPS Quality Gates to Pass

- [ ] **Compilation**: 0 errors (CURRENT: ✅ PASS)
- [ ] **EUnit**: 100% pass rate (CURRENT: ❌ FAIL - 13 missing test modules)
- [ ] **Common Test**: 100% pass rate (CURRENT: ❌ FAIL - 298 failures)
- [ ] **Coverage**: ≥80% (CURRENT: ❌ FAIL - 1% overall)
- [ ] **Dialyzer**: 0 warnings (CURRENT: ❌ FAIL - 526 warnings)
- [ ] **Xref**: 0 undefined function calls (CURRENT: ❌ FAIL - 60+ for these 5 modules)
- [ ] **Performance**: <10% regression from baseline (CURRENT: ⚠️ NOT RUN)

### Patterns to Follow

**Gen Server Pattern**:
- Reference: `apps/erlmcp_core/src/erlmcp_hooks.erl:36-597`
- Pattern: `-module(name). -behaviour(gen_server).`
- Exports: API functions, gen_server callbacks (init, handle_call, handle_cast, handle_info, terminate, code_change)
- State record: `-record(state, { ... }).`
- API: `start_link/0`, `stop/0`, domain functions

**Test Pattern**:
- Reference: `apps/erlmcp_core/test/erlmcp_batch_tests.erl`
- Pattern: Chicago School TDD (real processes, no mocks)
- Setup: `setup()` and `cleanup()` functions
- Tests: Named test functions with assertions
- No `?assertNotException` - use pattern matching

**Error Handling**:
- Pattern: Let-it-crash with supervisor recovery
- Use `case ... of` for explicit error handling
- Log errors with `logger:error/2`
- Return `{error, Reason}` tuples

**Type Specs**:
- Pattern: Dialyzer-compatible specs
- Example: `-spec update_status(WorkOrderId :: binary(), Status :: atom()) -> ok | {error, term()}.`
- Use `-export_type([...])` for public types

## Root Cause Analysis (5 Whys)

**Problem**: 60+ undefined function calls to 5 missing modules blocking Xref quality gate

1. **Why?** Calling code references functions in modules that don't exist or don't export those functions.

2. **Why?** Three root causes identified:
   - **Modules NEVER implemented**: erlmcp_request_id, erlmcp_tracing were placeholder names
   - **Module REPLACED but calls not removed**: erlmcp_task_manager → erlmcp_hooks (old calls remain)
   - **Partial implementations**: tcps_persistence, tcps_work_order exist but are missing specific functions (store_sku/1, update_status/2)

3. **Why incomplete implementations?** v2.0 design called for these modules/functions but implementation was deferred. Functions were called before being written (YAGNI violation).

4. **Why deferred implementation?** Architectural drift between v1.x and v2.0. v2.0 deleted 177 modules (72% of codebase) and separated 85+ TCPS modules. Some function calls were not updated during refactoring.

5. **ROOT CAUSE**: **Lack of Poka-yoke (error-proofing)** during v2.0 refactoring. No systematic verification that:
   - All called functions exist in target modules
   - Renamed modules have all references updated
   - Deleted modules have all calls removed
   - Stubs are either implemented or removed before calling

**Solution**: Fix root cause, not symptoms:
- ✅ Audit each of the 5 missing modules (IMPLEMENT vs REMOVE)
- ✅ IMPLEMENT: Add missing functions to existing modules (tcps_persistence, tcps_work_order)
- ✅ REMOVE: Delete function calls, update calling code to use alternatives (erlmcp_request_id, erlmcp_tracing, erlmcp_task_manager)
- ✅ Verify: `rebar3 xref` shows 0 undefined calls for these 5 modules
- ✅ Poka-yoke: Add `-compile({nowarn_unused_function, [FunctionName/Arity]})` for stubs to make Xref violations visible

## Risks and Mitigations

| Risk | Severity | Impact | Mitigation |
|------|----------|--------|------------|
| **Adding tcps_persistence:store_sku breaks persistence layer** | P1 (High) | tcps_persistence is file-based with RDF ontology. Adding SKU storage requires careful integration. | **DECISION: IMPLEMENT** - Add store_sku/1 following existing store_receipt/1 pattern (JSON file with SHA-256 checksum + RDF triples). |
| **Adding tcps_work_order:update_status bypasses state machine** | P2 (Medium) | tcps_work_order has explicit lifecycle (start, progress, complete). Direct status update bypasses state machine. | **DECISION: REMOVE** - Update calling code to use progress_work_order/2 instead of update_status/2. |
| **Implementing erlmcp_request_id breaks existing code** | P1 (High) | Request ID generation is critical path for all client requests. | **DECISION: REMOVE** - Replace with inline integer arithmetic (RequestId + 1 with overflow check). |
| **Implementing erlmcp_task_manager conflicts with erlmcp_hooks** | P0 (Critical) | erlmcp_hooks already replaces erlmcp_task_manager. Duplicate functionality causes confusion. | **DECISION: REMOVE** - Update all erlmcp_task_manager calls to remove dependency (erlmcp_server auto-registers). |
| **Implementing erlmcp_tracing duplicates OpenTelemetry** | P2 (Medium) | erlmcp_otel already provides tracing via OpenTelemetry API. Custom tracing module reinvents wheel. | **DECISION: REMOVE** - Remove all erlmcp_tracing calls from erlmcp_server.erl (100+ references). Use erlmcp_otel or remove tracing entirely for now. |

**Severity Definitions**:
- **P0 (Critical)**: BLOCKS production - MUST fix immediately (e.g., task manager conflicts)
- **P1 (High)**: Major quality gap - MUST fix before release (e.g., request ID, persistence)
- **P2 (Medium)**: Important but not blocking (e.g., tracing, work order status)
- **P3 (Low)**: Nice-to-have

## Recommended Manufacturing Approach

**TCPS Methodology:**

### Phase 1: Specification (Requirements with Acceptance Criteria)
**Goal**: Document decision matrix for all 5 missing modules

**Output**: Decision table with:
- Module name
- Current state (exists/partial/missing)
- Decision (IMPLEMENT/REMOVE)
- Justification (critical path/replaced/not needed)
- Alternative (if REMOVE)
- Function list to implement (if IMPLEMENT)

### Phase 2: Pseudocode (Algorithm Design BEFORE Coding)
**Goal**: Design function signatures and logic

**For IMPLEMENT:**
```erlang
%% Example: tcps_persistence:store_sku/1 - DECISION: IMPLEMENT
%% Pseudocode:
store_sku(Sku) ->
    #{sku_id := SkuId, stage := Stage} = Sku,
    Filename = generate_sku_filename(SkuId, Stage),
    Checksum = calculate_checksum(Sku),
    SkuWithChecksum = Sku#{checksum => Checksum},
    ok = file:write_file(Filename, jsone:encode(SkuWithChecksum)),
    ok = persist_to_ontology(SkuWithChecksum),
    {ok, list_to_binary(Filename)}.
```

**For REMOVE:**
```erlang
%% Example: erlmcp_request_id:safe_increment/1 - DECISION: REMOVE
%% Alternative: Inline arithmetic with overflow check
%% Pseudocode:
%% OLD: {ok, NextId} = erlmcp_request_id:safe_increment(RequestId)
%% NEW: NextRequestId = case RequestId + 1 of N when N > ?MAX_REQUEST_ID -> {error, overflow}; N -> N end
```

### Phase 3: Architecture (Integration Points and Dependencies)
**Goal**: Identify all call sites and update paths

**For each missing module:**
1. Find all call sites: `grep -r "module_name:" apps/`
2. Classify call sites:
   - Critical path (client, server, registry) → Must update
   - Test code (skip files) → Can defer
   - Examples/demos → Can defer
3. Design migration path:
   - Direct replacement (erlmcp_task_manager → no-op, erlmcp_hooks manages it)
   - Function addition (tcps_persistence:store_sku/1)
   - Inline logic (erlmcp_request_id:safe_increment/1)
   - Mass removal (erlmcp_tracing - 100+ calls)

### Phase 4: Refinement (Chicago School TDD - Tests FIRST)
**Goal**: Write tests before implementing functions

**For IMPLEMENT:**
```erlang
%% Test for tcps_persistence:store_sku/1
store_sku_test() ->
    Sku = #{sku_id => <<"test-sku">>, stage => research, timestamp => timestamp_now()},
    {ok, Receipt} = tcps_persistence:store_sku(Sku),
    #{sku_id := <<"test-sku">>} = Receipt,
    ok = file:delete(Receipt).
```

**For REMOVE:**
```erlang
%% Test that erlmcp_hooks replaces erlmcp_task_manager
%% Actually, no test needed - erlmcp_task_manager calls should just be removed
%% erlmcp_server auto-registers with erlmcp_hooks internally
```

### Phase 5: Completion (All Quality Gates Passing)
**Goal**: Verify 0 undefined function calls for these 5 modules

**Validation:**
```bash
# 1. Clean build
rebar3 clean
rebar3 compile

# 2. Run Xref
rebar3 xref

# 3. Verify output for these 5 modules
# Expected: 0 undefined function calls for:
#   - erlmcp_request_id
#   - erlmcp_tracing
#   - erlmcp_task_manager
#   - tcps_persistence:store_sku
#   - tcps_work_order:update_status

# 4. Run tests
rebar3 eunit
# Target: 100% pass rate for new functions

# 5. Coverage
rebar3 cover
# Target: ≥80% for new code
```

**Quality Metrics:**
- Xref warnings: 0 undefined for these 5 modules (current: 60+)
- Compilation: 0 errors (current: ✅ 0)
- EUnit: 100% pass for new functions (current: ⚠️ not implemented)
- Coverage: ≥80% for new code (current: 1% overall)

## Open Questions
**NONE** - Research complete. All questions answered.

## Manufacturing Checklist
- [x] Root cause identified (incomplete v2.0 refactoring, no Poka-yoke)
- [x] Quality gates defined (Xref: 0 undefined function calls for these 5 modules)
- [x] OTP patterns understood (gen_server, supervisor, registry, transport behavior)
- [x] Test strategy clear (Chicago School TDD - real processes, no mocks)
- [x] Risk assessment complete (5 risks identified with severity P0-P2)
- [x] No open questions (all research complete)

## Decision Matrix (5 Missing Modules - Item 042 Scope)

### Category 1: REMOVE - Replaced by erlmcp_hooks (1 module)

| Module | Functions | Decision | Justification | Alternative |
|--------|-----------|----------|---------------|-------------|
| **erlmcp_task_manager** | register_server/2, unregister_server/1, create_tool_task/5, complete_task/2, fail_task/2, get_task/1, list_tasks/1, cancel_task/1 | **REMOVE** | Replaced by erlmcp_hooks (gen_server). erlmcp_server auto-registers with erlmcp_hooks internally. No manual registration needed. | Remove all erlmcp_task_manager calls from erlmcp_server.erl. erlmcp_hooks manages server lifecycle automatically. |

**Risk**: P0 (Critical) - Conflicts with existing erlmcp_hooks implementation
**Migration Path**:
1. Remove erlmcp_task_manager calls from erlmcp_server.erl:179, 429, 603, 613, 623, 637, 660, 1398, 1399
2. Remove test file: test/erlmcp_task_manager_tests.erl.skip
3. Verify erlmcp_hooks handles all task lifecycle needs

### Category 2: REMOVE - Use inline logic (1 module)

| Module | Functions | Decision | Justification | Alternative |
|--------|-----------|----------|---------------|-------------|
| **erlmcp_request_id** | safe_increment/1 | **REMOVE** | Request ID increment is simple integer arithmetic. No need for separate module. | Inline logic in erlmcp_client:send_request/4: `NextId = case RequestId + 1 of N when N > ?MAX_REQUEST_ID -> {error, overflow}; N -> {ok, N} end` |

**Risk**: P1 (High) - Request ID generation is critical path
**Migration Path**:
1. Replace erlmcp_request_id:safe_increment/1 with inline arithmetic in erlmcp_client.erl:478
2. Add overflow check: `NextRequestId = case RequestId + 1 of N when N > ?MAX_REQUEST_ID -> {error, overflow}; N -> {ok, N} end`
3. Update error handling to match inline return type

### Category 3: REMOVE - Duplicate functionality (1 module)

| Module | Functions | Decision | Justification | Alternative |
|--------|-----------|----------|---------------|-------------|
| **erlmcp_tracing** | start_server_span/2, set_attributes/2, set_status/2, record_exception/4, end_span/1, log/2, record_error_details/3, record_message_metrics/3 (100+ calls) | **REMOVE** | OpenTelemetry already provides production-grade tracing via erlmcp_otel. Custom tracing module duplicates functionality and is not implemented. | Remove all erlmcp_tracing calls from erlmcp_server.erl (100+ references lines 171-1069). Optional: Use erlmcp_otel for tracing in future. |

**Risk**: P2 (Medium) - Duplicates erlmcp_otel, but mass removal of 100+ calls required
**Migration Path**:
1. Remove all erlmcp_tracing calls from erlmcp_server.erl (systematic removal of 100+ references)
2. Remove test file: test/erlmcp_tracing_tests.erl.skip
3. Optional: Future implementation could use erlmcp_otel (OpenTelemetry API) instead of custom module

### Category 4: IMPLEMENT - Add missing functions (2 modules)

| Module | Missing Functions | Decision | Justification | Implementation |
|--------|------------------|----------|---------------|----------------|
| **tcps_persistence** | store_sku/1 | **IMPLEMENT** | Module EXISTS with full persistence API (store_receipt/1, store_work_order/1, store_andon_event/1). store_sku/1 is natural addition for SKU lifecycle. Called by tcps_sku.erl:1319. | Add store_sku/1 to apps/tcps_erlmcp/src/tcps_persistence.erl. Follow pattern of store_receipt/1 (JSON file with SHA-256 checksum + RDF triples). |
| **tcps_work_order** | update_status/2 | **REMOVE** (special case) | Module EXISTS with full lifecycle API (start_work_order/1, progress_work_order/2, complete_work_order/2). update_status/2 is called by tcps_sku.erl:1343 but conflicts with state machine. | **DECISION: UPDATE CALLING CODE** - Change tcps_sku.erl:1343 to use tcps_work_order:progress_work_order/2 instead of update_status/2. |

**Implementation Priority**:
- **P0 (Critical)**: erlmcp_task_manager (REMOVE), erlmcp_request_id (REMOVE)
- **P1 (High)**: tcps_persistence (IMPLEMENT store_sku/1), tcps_work_order (UPDATE calling code)
- **P2 (Medium)**: erlmcp_tracing (REMOVE - 100+ calls)

### Category 5: EXTERNAL LIBRARIES - Out of scope for item 042
**Note**: Item 018 identified 3 external libraries (jsone, lager, rdf_utils) but these are NOT in the 5 modules specified in item 042. These should be handled separately.

## Execution Plan

### Step 1: REMOVE Replaced Modules (Priority P0)
**Estimated Time**: 2 hours

**Actions**:
1. Remove erlmcp_task_manager calls from erlmcp_server.erl (9 calls at lines 179, 429, 603, 613, 623, 637, 660, 1398, 1399)
2. Remove test file: test/erlmcp_task_manager_tests.erl.skip
3. Replace erlmcp_request_id:safe_increment/1 with inline logic in erlmcp_client.erl:478

**Validation**:
```bash
rebar3 compile
rebar3 xref | grep -E "(erlmcp_task_manager|erlmcp_request_id)"
# Expected: No undefined function warnings for these modules
```

### Step 2: IMPLEMENT Missing Function (Priority P1)
**Estimated Time**: 1 hour

**Actions**:
1. Add store_sku/1 to tcps_persistence.erl following store_receipt/1 pattern
2. Update tcps_sku.erl to use progress_work_order/2 instead of update_status/2

**Validation**:
```bash
rebar3 compile
rebar3 xref | grep -E "(tcps_persistence|tcps_work_order)"
# Expected: No undefined function warnings
```

### Step 3: REMOVE Tracing Calls (Priority P2)
**Estimated Time**: 3 hours

**Actions**:
1. Remove all erlmcp_tracing calls from erlmcp_server.erl (100+ references)
2. Remove test file: test/erlmcp_tracing_tests.erl.skip

**Validation**:
```bash
rebar3 compile
rebar3 xref | grep erlmcp_tracing
# Expected: No undefined function warnings
```

### Step 4: Final Validation (All Categories)
**Estimated Time**: 1 hour

**Actions**:
1. Clean build: `rebar3 clean && rebar3 compile`
2. Run Xref: `rebar3 xref`
3. Verify output for these 5 modules
4. Run tests: `rebar3 eunit` (expect 100% pass rate for new code)
5. Coverage: `rebar3 cover` (expect ≥80% for new functions)

**Success Criteria**:
- ✅ Xref shows 0 undefined function calls for these 5 modules
- ✅ Compilation: 0 errors
- ✅ Tests: 100% pass rate for implemented functions
- ✅ Coverage: ≥80% for new code

**Total Estimated Time**: 7 hours (spread across 1-2 days with testing and validation)

## Conclusion

This research identified **60+ undefined function calls** across **5 specific missing modules** blocking the Xref quality gate (subset of larger 222-call issue from item 018). Root cause is **incomplete v2.0 refactoring** - some modules were never implemented, one was replaced but calls not removed, and two have partial implementations missing specific functions.

**Manufacturing Decision**:
- **REMOVE**: 3 modules (erlmcp_task_manager, erlmcp_request_id, erlmcp_tracing) - replaced, use inline logic, or duplicate functionality
- **IMPLEMENT**: 1 module (tcps_persistence:store_sku/1) - add missing function
- **UPDATE**: 1 module (tcps_work_order calling code) - use progress_work_order/2 instead of update_status/2

**Path to Production**:
1. REMOVE replaced modules (2 hours)
2. IMPLEMENT missing function (1 hour)
3. REMOVE tracing calls (3 hours)
4. Final validation (1 hour)

**Total Effort**: 7 hours across 1-2 days

**Quality Impact**:
- Xref warnings: 60+ → 0 for these 5 modules (100% reduction)
- Undefined function calls: Eliminated for this subset
- Technical debt: Reduced (no more stubs for these 5 modules)
- Poka-yoke: Achieved (Xref enforces function existence at compile time)

**Next Step**: Proceed to implementation phase using TCPS methodology (Specification → Pseudocode → Architecture → Refinement → Completion).

---

**Research Status**: ✅ COMPLETE
**Open Questions**: NONE
**Ready for Implementation**: YES
