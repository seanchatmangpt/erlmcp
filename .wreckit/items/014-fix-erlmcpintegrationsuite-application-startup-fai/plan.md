# Fix erlmcp_integration_SUITE application startup failure Manufacturing Plan

**TCPS Compliance**: Lean Six Sigma Level Strictness
**Zero Defects**: 99.99966% defect-free delivery (3.4 defects per million)

## Manufacturing Objective

Fix the `erlmcp_integration_SUITE` Common Test suite which currently fails during `init_per_suite/1` due to three critical root causes:
1. Wrong application name (`erlmcp` vs `erlmcp_core`)
2. Missing high-level API module (`erlmcp` module)
3. Stale process references (removed in v1.4.0)

**WHY this matters**: This suite blocks ALL integration testing (100% coverage gap). Without it, we cannot verify end-to-end functionality, message flow, or system coordination.

### Quality Gate Requirements

- **Compilation**: 0 errors (MANDATORY)
- **EUnit**: N/A - this is a Common Test suite fix
- **Common Test**: 100% of tests can start (individual test outcomes may vary)
- **Coverage**: ≥80% for new `erlmcp` API module (MANDATORY)
- **Dialyzer**: 0 warnings (MANDATORY)
- **Xref**: 0 undefined function calls (MANDATORY)
- **Performance**: N/A - startup time not critical for tests

## Current State

### What Exists Now

**Modules**:
- `apps/erlmcp_core/test/erlmcp_integration_SUITE.erl` (1832 lines) - FAILS at init_per_suite/1:148
- `apps/erlmcp_core/src/erlmcp_app.erl` (14 lines) - Application callback for `erlmcp_core`
- `apps/erlmcp_core/src/erlmcp_sup.erl` (130 lines) - Top-level supervisor with 3-tier architecture
- `apps/erlmcp_core/src/erlmcp_server.erl` (1562 lines) - Server gen_server with direct API
- `apps/erlmcp_core/src/erlmcp_registry.erl` (411 lines) - gproc-based registry with full API
- `apps/erlmcp_core/src/erlmcp_server_sup.erl` (47 lines) - Dynamic server supervisor
- `apps/erlmcp_core/src/erlmcp_core.app.src` (47 lines) - Defines `erlmcp_core` application

**Tests**:
- 19 test cases across 6 groups
- 0% pass rate - init_per_suite/1 fails before any tests execute
- All tests blocked by startup failure

**Quality**:
- CRITICAL FAILURE - 100% integration test coverage lost
- Tests call non-existent `erlmcp` module functions
- Tests reference deleted `erlmcp_transport_sup` process

### What's Missing

**Gap**: 100% - NO integration tests can run due to startup failure

**Root Cause**: Architecture refactoring (v1.4.0 → v2.0.0) split monolith into umbrella apps but test suite was never updated:
- Tests assume single `erlmcp` application (NOW: 4 separate apps)
- Tests call non-existent `erlmcp` high-level API (NEVER IMPLEMENTED)
- Tests reference `erlmcp_transport_sup` removed in v1.4.0 (NOW: in erlmcp_transports app)

**Impact**: Blocks ALL integration testing. Cannot verify:
- End-to-end message flow
- Multi-transport coordination
- Server-registry integration
- Configuration management
- Failure recovery
- Performance under load

### Key Discoveries from Validated Research

1. **Application name confirmed**: `erlmcp_core.app.src:1` defines `{application, erlmcp_core, ...}` - tests at line 148 call wrong name
2. **Missing API confirmed**: Glob search returns 0 files matching `**/erlmcp.erl` - the convenience wrapper never existed
3. **Direct APIs exist**:
   - `erlmcp_sup:start_server/2` (line 25-33) - wrapper for supervisor + registry
   - `erlmcp_sup:stop_server/1` (line 35-43) - wrapper for registry + supervisor
   - `erlmcp_server:add_tool/3` (line 92-94) - requires Pid, not ServerId
   - `erlmcp_registry:list_servers/0` (line 124-126) - returns server list
4. **Stale reference confirmed**: `erlmcp_sup.erl:69` comment states "erlmcp_transport_sup moved to erlmcp_transports app" - tests at line 209 still reference it
5. **v1.4.0 architecture confirmed**: 3-tier supervision tree (core_sup, server_sup, observability_sup) - tests need updated expectations

## Desired End State

### Specification

**WHAT we're building**:

1. **Fix init_per_suite/1** to start `erlmcp_core` application instead of `erlmcp`
2. **Create `erlmcp` API module** as convenience wrapper around existing APIs
3. **Update process references** to match v1.4.0 architecture
4. **Enable all 19 test cases** to start (individual test outcomes may vary)

### Verification

**How to verify this meets requirements**:

1. **Automated**: `rebar3 ct --suite=erlmcp_integration_SUITE` - all 19 tests start without init failure
2. **Manual**: Inspect test output to confirm init_per_suite/1 completes successfully
3. **Metrics**: Count of passing tests may still be low, but NO tests blocked by startup

### Manufacturing Output

**Code**:
- Create `apps/erlmcp_core/src/erlmcp.erl` (~200 lines)
- Modify `apps/erlmcp_core/test/erlmcp_integration_SUITE.erl` (~20 line changes)

**Tests**:
- Existing `erlmcp_integration_SUITE.erl` tests become runnable
- Add `apps/erlmcp_core/test/erlmcp_tests.erl` for new `erlmcp` module (≥80% coverage)

**Documentation**:
- Update `apps/erlmcp_core/README.md` with `erlmcp` API usage

**Receipts**:
- Compilation log: 0 errors
- Test execution log: init_per_suite/1 passes
- Coverage report: ≥80% for `erlmcp` module
- Dialyzer: 0 warnings
- Xref: 0 undefined calls

## What We're NOT Doing

**OUT OF SCOPE** (to prevent scope creep):

- **Fixing individual test failures** - We're only making tests RUNNABLE, not fixing logic errors
- **Reason**: Individual tests may fail for other reasons (missing features, bugs, etc.). Those are separate issues.

- **Implementing missing transport features** - Tests call `erlmcp:start_transport/3` which may not be fully implemented
- **Reason**: `erlmcp_sup.erl:46-48` shows transport stub returns `{error, not_implemented}`. Transport implementation is separate work.

- **Adding new test cases** - We're not expanding test coverage beyond fixing startup
- **Reason**: Focus is on unblocking existing tests, not adding new ones

- **Refactoring test architecture** - Not changing test structure, only fixing initialization
- **Reason**: Minimize risk, stick to root cause fix

- **Performance optimization** - Not optimizing startup time or test execution speed
- **Reason**: Focus is on functionality, not performance

## Manufacturing Approach

### TCPS Methodology

Following Toyota Code Production System phases:

1. **Specification** ✓ (above) - Requirements with acceptance criteria documented
2. **Pseudocode** (below) - Algorithm design BEFORE coding
3. **Architecture** - Integration points and supervision tree (documented)
4. **Refinement** - Chicago School TDD (tests FIRST) - tests exist, need initialization fix
5. **Completion** - All quality gates passing (goal)

### Implementation Strategy

**WHY this strategy**:

We use a **layered approach** that fixes dependencies first:

1. **Phase 1 (P0)**: Fix `init_per_suite/1` - Start `erlmcp_core` instead of `erlmcp`
   - **WHY**: Unblocks ALL tests immediately
   - **RISK**: Low - single line change

2. **Phase 2 (P0)**: Implement `erlmcp` API shim module
   - **WHY**: All tests call this module, it's required infrastructure
   - **RISK**: Medium - new module, but simple wrapper logic
   - **PATTERN**: Facade pattern over existing `erlmcp_sup`, `erlmcp_registry`, `erlmcp_server` APIs

3. **Phase 3 (P1)**: Fix stale process references in test assertions
   - **WHY**: Prevents false failures from checking deleted processes
   - **RISK**: Low - assertion updates only

**Alternative approaches considered**:

- **Option A**: Replace all `erlmcp:*` calls with direct API calls
  - **PROS**: No new module
  - **CONS**: Massive test rewrites (100+ call sites), high risk, harder to maintain
  - **DECISION**: Rejected - too invasive

- **Option B**: Skip tests that require missing features
  - **PROS**: Quick unblock
  - **CONS**: Loses test coverage, hides gaps
  - **DECISION**: Rejected - defeats purpose of integration tests

- **Option C**: Implement full-featured `erlmcp` API
  - **PROS**: Complete solution
  - **CONS**: Scope creep, delays unblocking tests
  - **DECISION**: Rejected - use MVP shim now, enhance later

### Quality Integration

- **Pre-commit Hooks**: `.claude/hooks/pre-task-validate.sh` validates compilation + dialyzer
- **CI Gates**: GitHub Actions runs `rebar3 compile`, `rebar3 ct`, `rebar3 dialyzer`
- **Receipt Generation**: Test logs saved to `_build/test/logs`, coverage to `_build/cover`
- **Andon Signaling**:
  - ✅ Green: `init_per_suite/1` passes, all tests start
  - ❌ Red: `init_per_suite/1` fails, tests blocked
  - 🟡 Yellow: Tests start but some fail (expected during fix)

---

## Phases

### Phase 1: Fix init_per_suite/1 Application Startup

**Estimated Time**: 1 hour
**Priority**: P0 - CRITICAL (blocks all tests)

#### Overview

Fix `init_per_suite/1` to start `erlmcp_core` application instead of non-existent `erlmcp` application. This unblocks ALL 19 test cases immediately.

#### Specification

**WHAT we're building**:
- Modify `erlmcp_integration_SUITE.erl:init_per_suite/1` line 148
- Change `application:start(erlmcp)` to `application:start(erlmcp_core)`
- Add verification that core processes started successfully
- Update `end_per_suite/1` line 169 to stop `erlmcp_core`

#### Architecture

**INTEGRATION**:
- Depends on: `erlmcp_app` (application callback), `erlmcp_sup` (top supervisor)
- Starts: 3-tier supervision tree (core_sup, server_sup, observability_sup)
- No new processes - uses existing OTP application

**Process structure**:
```
erlmcp_sup (one_for_one)
├── erlmcp_core_sup (registry, infrastructure)
├── erlmcp_server_sup (dynamic server instances)
└── erlmcp_observability_sup (monitoring, metrics)
```

#### Changes Required

##### 1. Fix init_per_suite/1 Application Start

**File**: `apps/erlmcp_core/test/erlmcp_integration_SUITE.erl`
**Lines**: 122-163

**BEFORE** (line 148): `case application:start(erlmcp) of`
**AFTER** (line 148): `case application:start(erlmcp_core) of`

**BEFORE** (lines 160-161): Basic checks for `erlmcp_sup` and `erlmcp_registry`
**AFTER** (lines 160-167): Check all 4 core processes (3-tier architecture)

**BEFORE** (line 169): `application:stop(erlmcp)`
**AFTER** (line 169): `application:stop(erlmcp_core)`

**Reason**:
- Application name `erlmcp_core` matches `erlmcp_core.app.src:1`
- Verifies all 4 core processes exist (matches v1.4.0 architecture)
- Removed OpenTelemetry deps (may not be available)
- Improved error messages in assertions

#### Success Criteria

##### Automated Verification (MUST ALL PASS):
- [ ] **Compilation**: `TERM=dumb rebar3 compile` - 0 errors, 0 warnings
- [ ] **Common Test**: `rebar3 ct --suite=erlmcp_integration_SUITE` - init_per_suite/1 completes successfully
- [ ] **Pre-commit hook**: `.claude/hooks/pre-task-validate.sh` - PASS

##### Manual Verification:
- [ ] **Test output shows**: "Starting ErlMCP Integration Test Suite"
- [ ] **No error messages**: `application:start(erlmcp_core)` returns `ok`
- [ ] **Assertions pass**: All 4 whereis() checks succeed
- [ ] **Test count**: All 19 test cases appear in test run output

**Note**: Complete ALL automated verification BEFORE marking phase done. If ANY gate fails, STOP and fix.

---

### Phase 2: Implement erlmcp API Module

**Estimated Time**: 2 hours
**Priority**: P0 - CRITICAL (all tests depend on this)

#### Overview

Create `erlmcp` module as convenience wrapper around existing `erlmcp_sup`, `erlmcp_registry`, and `erlmcp_server` APIs. This enables all 19 test cases to call the functions they expect.

#### Specification

**WHAT we're building**:
- New module: `apps/erlmcp_core/src/erlmcp.erl` (~200 lines)
- Implements facade pattern over existing APIs
- Exports functions called by tests:
  - `start_server/2`, `stop_server/1`
  - `add_tool/3`, `add_resource/3`, `add_prompt/3`
  - `list_servers/0`, `list_transports/0`
  - `get_server_config/1`, `update_server_config/2`
  - `bind_transport_to_server/2`, `get_transport_bindings/0`
  - `validate_transport_config/2`
  - `start_transport/3`, `stop_transport/1`

#### Architecture

**INTEGRATION** - API wrapper pattern:
```
Test Suite (erlmcp_integration_SUITE)
    ↓ calls
erlmcp API module (NEW - convenience wrapper)
    ↓ delegates to
├─ erlmcp_sup (server management)
├─ erlmcp_registry (routing, queries)
└─ erlmcp_server (server operations)
```

**Process Flow**:
1. `erlmcp:start_server/2` → `erlmcp_sup:start_server/2` → starts server via `erlmcp_server_sup`
2. `erlmcp:add_tool/3` → `erlmcp_registry:find_server/1` → `erlmcp_server:add_tool/3`
3. `erlmcp:list_servers/0` → `erlmcp_registry:list_servers/0`

#### Changes Required

##### 1. Create erlmcp.erl API wrapper module

**File**: `apps/erlmcp_core/src/erlmcp.erl` (NEW FILE)
**Current**: Does not exist
**Changes**: Create new module with ~200 lines

**Reason**: Test suite calls functions from this module which doesn't exist. This is the ONLY way to unblock tests without rewriting 100+ test calls.

Full module specification is detailed in the complete plan document. Key functions:
- `start_server/2`, `stop_server/1` - wrap `erlmcp_sup`
- `add_tool/3`, `add_resource/3`, `add_prompt/3` - lookup server via registry, call server API
- `list_servers/0`, `list_transports/0` - passthrough to registry
- `validate_transport_config/2` - validation logic (stdio, tcp, http)
- Transport functions - return `{error, not_implemented}` (transport in separate app)

#### Success Criteria

##### Automated Verification (MUST ALL PASS):
- [ ] **Compilation**: `TERM=dumb rebar3 compile` - 0 errors, 0 warnings
- [ ] **Module Loading**: `erl -pa _build/default/lib/erlmcp_core/ebin -eval "erlmcp:start_server(test, #{})."` - loads successfully
- [ ] **Test Execution**: `rebar3 ct --suite=erlmcp_integration_SUITE` - tests can execute (may fail on logic)
- [ ] **Xref**: `rebar3 xref` - 0 undefined function calls for erlmcp module
- [ ] **Dialyzer**: `rebar3 dialyzer` - 0 warnings for erlmcp module
- [ ] **Pre-commit hook**: `.claude/hooks/pre-task-validate.sh` - PASS

##### Manual Verification:
- [ ] All 19 test cases can execute (init_per_suite passes)
- [ ] Test cases that start servers work (e.g., `test_system_startup_shutdown`)
- [ ] `erlmcp:start_server/2` successfully creates server instances
- [ ] `erlmcp:list_servers/0` returns list of registered servers
- [ ] `erlmcp:add_tool/3` successfully adds tools to servers

**Note**: Complete ALL automated verification BEFORE marking phase done. If ANY gate fails, STOP and fix.

---

### Phase 3: Fix Stale Process References

**Estimated Time**: 1 hour
**Priority**: P1 - HIGH (prevents false failures)

#### Overview

Update test assertions to match v1.4.0 3-tier architecture. Remove references to deleted `erlmcp_transport_sup` and add checks for current supervisors.

#### Specification

**WHAT we're building**:
- Updated `test_system_startup_shutdown/1` test case
- Remove `erlmcp_transport_sup` from StartupOrder list
- Add checks for `erlmcp_core_sup` and `erlmcp_server_sup`

#### Architecture

**INTEGRATION** - 3-tier supervision tree (v1.4.0):
```
erlmcp_sup (top-level)
  ├─ erlmcp_core_sup (TIER 1: registry, infrastructure)
  ├─ erlmcp_server_sup (TIER 2: dynamic server instances)
  └─ erlmcp_observability_sup (TIER 3: optional monitoring)
```

**Removed in v1.4.0**:
- `erlmcp_transport_sup` - moved to `erlmcp_transports` app

#### Changes Required

##### 1. Fix test_system_startup_shutdown/1 in erlmcp_integration_SUITE.erl

**File**: `apps/erlmcp_core/test/erlmcp_integration_SUITE.erl`
**Current**: Lines 205-233
**Changes**:
- Line 209: Remove `erlmcp_transport_sup` from StartupOrder
- Line 209: Add `erlmcp_core_sup` and `erlmcp_server_sup`
- Line 217: Update assertion from "at least 2 children" to "at least 3 children" (3-tier)

**Reason**: Align with v1.4.0 architecture. `erlmcp_transport_sup` no longer exists in core app.

**BEFORE**:
```erlang
StartupOrder = [erlmcp_sup, erlmcp_registry, erlmcp_transport_sup],
...
?assert(length(SupChildren) >= 2, "Supervisor should have at least 2 children"),
```

**AFTER**:
```erlang
StartupOrder = [erlmcp_sup, erlmcp_core_sup, erlmcp_server_sup, erlmcp_registry],
...
?assert(length(SupChildren) >= 3, "Supervisor should have at least 3 children (3-tier)"),
```

#### Success Criteria

##### Automated Verification (MUST ALL PASS):
- [ ] **Compilation**: `TERM=dumb rebar3 compile` - 0 errors, 0 warnings
- [ ] **Test Case**: `rebar3 ct --suite=erlmcp_integration_SUITE --case=test_system_startup_shutdown` - PASS
- [ ] **Process Checks**: All 3-tier processes verified (erlmcp_sup, erlmcp_core_sup, erlmcp_server_sup, erlmcp_registry)
- [ ] **No Undefined References**: No references to `erlmcp_transport_sup`
- [ ] **Pre-commit hook**: `.claude/hooks/pre-task-validate.sh` - PASS

##### Manual Verification:
- [ ] `test_system_startup_shutdown` test case passes
- [ ] StartupOrder list has 4 processes (not 3)
- [ ] Supervisor child count check expects ≥3 (not ≥2)
- [ ] No `erlmcp_transport_sup` references in test file

**Note**: Complete ALL automated verification BEFORE marking phase done. If ANY gate fails, STOP and fix.

---

## Testing Strategy

### Chicago School TDD (MANDATORY)

- **NO MOCKS** - Use real processes, real gen_servers
- **State-Based Verification** - Check #state{} record contents
- **Integration Testing** - Test with real dependencies
- **Race Condition Testing** - Concurrent operations

### Quality Gates

Every phase MUST pass:
1. **Compilation**: `TERM=dumb rebar3 compile` - 0 errors
2. **Common Test**: `rebar3 ct --suite=erlmcp_integration_SUITE` - init_per_suite passes
3. **Coverage**: `rebar3 cover` - ≥80% coverage
4. **Dialyzer**: `rebar3 dialyzer` - 0 warnings
5. **Xref**: `rebar3 xref` - 0 undefined function calls

## Manufacturing Checklist

### Before Implementation
- [x] Root cause identified (not symptoms) - 3 root causes with evidence
- [x] Scope confirmed (IN/OUT documented) - Clear boundaries defined
- [x] No open questions (all research complete) - All decisions made
- [x] Phases broken down (≤4 hours each) - 3 phases: 1hr, 2hr, 1hr
- [x] Acceptance criteria defined (measurable, specific) - All gates defined

### During Implementation
- [ ] Chicago School TDD followed (tests FIRST) - Tests exist, fixing init
- [ ] OTP patterns followed (gen_server, supervisor) - Using existing patterns
- [ ] Type specs added (Dialyzer clean) - Will add -spec attributes
- [ ] Error handling complete (all paths) - All error cases handled
- [ ] Quality gates passing (compilation, tests, coverage) - All gates checked

### After Implementation
- [ ] All tests passing (100% rate) - All 19 tests can execute
- [ ] Coverage ≥80% (verified) - rebar3 cover confirms
- [ ] Dialyzer 0 warnings (verified) - rebar3 dialyzer clean
- [ ] Xref 0 undefined calls (verified) - rebar3 xref clean
- [ ] Performance no regression >10% (verified) - Init time <2 seconds
- [ ] Documentation updated (README, API docs) - Module docs added
- [ ] Code review complete (OTP patterns verified) - Supervisor pattern correct

## Risk Management

### Known Risks

| Risk | Severity | Probability | Mitigation |
|------|----------|-------------|------------|
| **Application name mismatch causes silent failures** | P0 | High (100%) | Fixed in Phase 1 - change erlmcp → erlmcp_core |
| **Missing erlmcp API module breaks all test cases** | P0 | High (100%) | Fixed in Phase 2 - implement erlmcp.erl wrapper |
| **Stale erlmcp_transport_sup references** | P1 | High (100%) | Fixed in Phase 3 - remove from StartupOrder |
| **Dependency on erlmcp_observability_sup** | P2 | Medium (50%) | Handled in Phase 1 - optional startup with try/catch |
| **Test assumes transport layer in same app** | P2 | Medium (50%) | Handled in Phase 2 - stubs return {error, not_implemented} |
| **Backward compatibility breaks** | P3 | Low (10%) | Document migration path from erlmcp → erlmcp_core |

### Rollback Plan

**If something goes wrong**:

1. **Git Revert**: Revert commits to baseline before this work
   - Baseline: Current commit on `wreckit/014-fix-erlmcpintegrationsuite-application-startup-fai` branch
   - Command: `git revert HEAD~3..HEAD` (revert all 3 phases)

2. **Data Migration**: No data migration needed (test-only changes)

3. **Service Impact**: No production impact (test suite only)

**Recovery Steps**:
1. Revert changes via git
2. Verify compilation still works
3. Document why rollback was needed
4. Create follow-up item to address root cause differently

## References

- Research: `/Users/sac/erlmcp/.wreckit/items/014-fix-erlmcpintegrationsuite-application-startup-fai/research.md`
- CLAUDE.md: `/Users/sac/erlmcp/CLAUDE.md` (project rules)
- TCPS: `/Users/sac/erlmcp/docs/tcps/TCPS.md` (manufacturing principles)
- OTP Patterns: `/Users/sac/erlmcp/docs/otp-patterns.md`
- Test Reference: `apps/erlmcp_core/test/erlmcp_integration_SUITE.erl`
- Application File: `apps/erlmcp_core/src/erlmcp_core.app.src`
- Supervisor Module: `apps/erlmcp_core/src/erlmcp_sup.erl`
- Registry Module: `apps/erlmcp_core/src/erlmcp_registry.erl`
- Server Module: `apps/erlmcp_core/src/erlmcp_server.erl`
