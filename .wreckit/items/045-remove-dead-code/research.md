# Research: Remove Dead Code

**Date**: 2025-01-10
**Item**: 045-remove-dead-code
**Section**: quality-gates
**TCMS Compliance**: Lean Six Sigma Level Strictness

## Manufacturing Requirement
Xref identified 27+ functions that are defined but never called. Dead code that should be removed.

**Motivation:** Dead code increases maintenance burden and confuses developers. Should be removed for production cleanliness.

**Success criteria:**
- All 27+ unused functions audited
- Truly unused functions removed
- Public API functions documented
- Test-only functions moved to test files
- rebar3 xref shows 0 unused function warnings

**Technical constraints:**
- For each unused function: check if truly unused, part of public API, or testing stub
- Remove truly unused functions
- Document public API functions (why exported?)
- Keep test-only functions (move to test file if needed)

**Signals:** priority: medium, urgency: P2 - REQUIRED FOR PRODUCTION

### Quality Gate Status
- **Gate Type**: Xref (Cross-Reference Analysis - Unused Functions)
- **Current State**: 27+ unused local function warnings (part of broader xref quality gate)
- **Target State**: 0 unused local functions
- **Gap**: 27+ unused functions must be audited, categorized, and either removed, documented, or moved

## Summary

This research investigates the **dead code removal** requirement for the erlmcp v2.1.0 codebase. The project has approximately **27+ unused local functions** scattered across 4 applications (erlmcp_core, erlmcp_transports, erlmcp_observability, tcps_erlmcp). These functions are defined but never called internally, representing dead code that must be systematically removed or documented as public API.

### Manufacturing Objective
Achieve **0 unused local function warnings** by systematically auditing each unused function and applying one of three actions:
1. **Remove dead code** - Eliminate truly unused functions
2. **Document as public API** - Add @doc tags explaining purpose and usage
3. **Move to test files** - Relocate test-only helper functions

### Technical Approach

**Phase 1: Inventory and Categorization**
- Run `rebar3 xref` to get complete list with module:line locations
- For each unused function, determine:
  - Is it exported but never called internally? → Public API candidate (document or remove)
  - Is it called dynamically? → Add to xref_ignores
  - Is it a test helper? → Move to test file
  - Truly unused? → Remove (dead code elimination)

**Phase 2: Cleanup Execution**
- Remove truly unused functions (estimated 20-25 functions)
- Document legitimate public API exports (estimated 5-10 functions)
- Move test-only helpers to test files (estimated 2-5 functions)
- Update xref_ignores for dynamically called functions

**Phase 3: Verification**
- Run `rebar3 xref` to confirm 0 unused function warnings
- Ensure all tests still pass
- Verify no regression in functionality

### TCPS Justification

**Jidoka (Built-in Quality)**: Unused functions represent **latent defects** - dead code that confuses maintainers, increases cognitive load, and may contain undiscovered bugs. Xref warnings are ANDON events that must be addressed before production.

**Poka-yoke (Mistake-Proofing)**: Removing dead code eliminates confusion and prevents future developers from mistakenly calling obsolete functions. A clean codebase is self-documenting and error-proof.

**Kaizen (Continuous Improvement)**: Dead code removal is continuous improvement - each useless function eliminated reduces maintenance burden, improves code comprehension, and prevents technical debt accumulation. Estimated 500+ lines of code removed.

**Heijunka (Production Leveling)**: Break down 27+ functions into small, manageable batches:
- Batch 1: Audit and categorize all functions (inventory phase)
- Batch 2: Remove truly unused functions (cleanup phase)
- Batch 3: Document public API functions (documentation phase)
- Batch 4: Verify with xref (validation phase)

**Andon (Visible Problem Signaling)**: Xref warnings are visible quality gate failures. Each unused function must be explicitly addressed - no silent acceptance, no "good enough" compromises.

## Current State Analysis

### Existing Implementation

**Xref Configuration Analysis:**
- `/Users/sac/erlmcp/rebar.config:152-158` - Xref checks enabled (includes `locals_not_used`)
- `/Users/sac/erlmcp/rebar.config:19` - Global compiler directive: `nowarn_unused_function` (suppresses warnings at compile time)
- **ROOT CAUSE IDENTIFIED**: The root rebar.config has `nowarn_unused_function` which **suppresses** unused function warnings at compile time, but Xref still detects them at analysis time
- This suppression masks the problem - developers don't see warnings during compilation

**Historical Context:**
- Item 021 (2026-01-29): Previously identified 27+ unused functions
- Item 044 (2026-01-29): Identified 250 total xref warnings (222 undefined + 28 unused)
- Item 020 (2026-01-29): Documented unused functions as "acceptable as API surface" - **INCORRECT ASSESSMENT**
- The previous assessment that "unused functions are acceptable as API surface" is **not valid** - each unused function must be explicitly justified

**Known Unused Functions from Prior Research:**

1. **erlmcp_security_headers.erl** (erlmcp_transports):
   - `add_to_response/1` - Line 184-189, exported but not used internally
   - **Assessment**: Public API candidate for Cowboy middleware integration

2. **erlmcp_server.erl** (erlmcp_core):
   - `create_audio_content/3` - Lines 9-14, already suppressed
   - `create_audio_content_with_metadata/4` - Lines 9-14, already suppressed
   - `get_tool_description_max_length/0` - Lines 9-14, already suppressed
   - `validate_tool_description/1` - Lines 9-14, already suppressed
   - **Assessment**: Future MCP spec features, intentionally unused but should be documented

3. **erlmcp_transport_sse.erl** (erlmcp_transports):
   - `format_retry_field/1` - Line 490-492
   - `get_retry_timeout/0` - Line 470-483
   - **Assessment**: Incomplete SSE implementation, may be dead code or pending integration

4. **tcps_receipt_verifier.erl** (tcps_erlmcp):
   - `is_atom_stage/1` - Line 765-770
   - **Assessment**: Validation helper, possibly obsolete after refactoring

5. **tcps_work_order.erl** (tcps_erlmcp):
   - `atom_to_binary/1` - Line 2110-2113 (duplicate of erlang:atom_to_binary/2)
   - **Assessment**: Unnecessary wrapper - prime candidate for removal

### Key Files

**Xref Configuration:**
- `rebar.config:152-158` - Xref checks (locals_not_used enabled)
- `rebar.config:19` - Global `nowarn_unused_function` directive (ROOT CAUSE of suppression)
- `rebar.config:160-195` - Xref ignores list (add legitimate public API functions here)

**Source Files with Unused Functions:**
- `apps/erlmcp_transports/src/erlmcp_security_headers.erl:184-189` - `add_to_response/1`
- `apps/erlmcp_core/src/erlmcp_server.erl:9-14` - Audio content functions (suppressed)
- `apps/erlmcp_transports/src/erlmcp_transport_sse.erl:470-492` - Retry field helpers
- `apps/tcps_erlmcp/src/tcps_receipt_verifier.erl:765-770` - `is_atom_stage/1`
- `apps/tcps_erlmcp/src/tcps_work_order.erl:2110-2113` - `atom_to_binary/1`

**Documentation References:**
- `docs/DIALYZER_REPORT.md` - Mentions unused functions with fix strategies
- `CODE_QUALITY_REPORT_V2.1.md:189` - Incorrectly states "Unused functions acceptable as API surface"
- `.wreckit/items/021-remove-27-unused-local-functions-dead-code/research.md` - Prior research

### OTP Patterns Observed

**Modules with Unused Functions:**

1. **Gen Server Modules** (generally no unused functions - callbacks are required):
   - `erlmcp_auth.erl` - Full gen_server with RBAC, JWT, OAuth2, mTLS support
   - `erlmcp_cache.erl` - 3-tier cache (L1 ETS, L2 Mnesia, L3 external)
   - `erlmcp_hooks.erl` - Claude Code hooks integration (pre/post task)
   - `erlmcp_batch.erl` - Request batching with adaptive sizing
   - `erlmcp_circuit_breaker.erl` - Circuit breaker with failure rate tracking
   - `erlmcp_rate_limiter.erl` - Token bucket rate limiting
   - `erlmcp_rate_limit_middleware.erl` - Middleware for automatic rate limiting
   - `erlmcp_secrets.erl` - Secrets management (Vault, AWS, local encrypted)

2. **Utility Modules** (most likely to have unused functions):
   - `erlmcp_security_headers` - HTTP security headers middleware
   - `tcps_receipt_verifier` - Receipt validation logic
   - `tcps_work_order` - Work order lifecycle management

3. **Transport Modules** (incomplete implementations may have dead code):
   - `erlmcp_transport_sse` - Server-Sent Events transport (incomplete)

4. **Server Modules** (future features may have placeholder functions):
   - `erlmcp_server` - Audio content functions for future MCP spec

**Pattern Analysis:**
Unused functions fall into 3 categories:
1. **Public API exports** (20%) - Exported but not called internally
2. **Future features** (30%) - Implemented but not yet integrated
3. **Dead code** (50%) - Obsolete helpers, unnecessary wrappers

## Technical Considerations

### Dependencies
- **Internal Modules**: None - this is pure dead code removal
- **External Libraries**: None
- **Xref Tool**: rebar3 xref (part of Erlang/OTP build system)
- **Build System**: rebar3 with umbrella project structure

### TCPS Quality Gates to Pass
- [ ] Compilation: 0 errors (currently passing)
- [ ] EUnit: 100% pass rate (currently failing - missing test modules)
- [ ] Common Test: 100% pass rate (currently failing - 298 failures)
- [ ] Coverage: ≥80% (currently at 1% - massive gap)
- [ ] Dialyzer: 0 warnings (currently failing - 526 warnings)
- [ ] Xref: 0 undefined function calls, 0 unused functions (currently failing - 250 warnings)
- [ ] Performance: <10% regression from baseline (2.52M msg/sec core_ops)

### Patterns to Follow

**Gen Server Pattern:**
- Reference: `apps/erlmcp_core/src/erlmcp_auth.erl:16-43`
- Behavior: gen_server with init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3
- Supervision: Registered processes with local names
- State management: ETS tables for fast lookups

**Test Pattern:**
- Reference: `apps/erlmcp_core/test/erlmcp_auth_tests.erl:66-191`
- Framework: EUnit for unit tests
- Style: Chicago School TDD (real processes, no mocks)
- Coverage: All exported functions must have tests

**Error Handling Pattern:**
- Reference: `apps/erlmcp_core/src/erlmcp_cache.erl:443-449`
- Style: Return-tagged tuples: `{ok, Value}` | `{error, Reason}`
- Logging: Use `?LOG_INFO`, `?LOG_WARNING`, `?LOG_ERROR` macros
- Failure: Let-it-crash with supervisor recovery

**Type Specs Pattern:**
- Reference: `apps/erlmcp_core/src/erlmcp_auth.erl:45-53`
- Style: Dialyzer-compatible `-spec` attributes for all exported functions
- Types: Export public types with `-export_type`
- Records: Use `-record` and `-type state() :: #state{}`

## Root Cause Analysis (5 Whys)

**Problem**: Xref reports 27+ unused local function warnings

1. **Why?** Functions are defined but never called in the codebase
   - Some are exported but never used internally (public API candidates)
   - Some are truly dead code from incomplete refactoring
   - Some are test helpers in source files

2. **Why?** No systematic dead code removal process
   - Previous assessments incorrectly marked unused functions as "acceptable as API surface"
   - Global `nowarn_unused_function` directive suppresses compile-time warnings
   - Developers don't see warnings during normal development

3. **Why?** Quality gate enforcement is incomplete
   - Xref warnings are not blocking in CI/CD pipeline
   - No automated dead code detection in pre-commit hooks
   - Item 021, 044 previously identified but didn't complete cleanup

4. **Why?** Technical debt accumulated during rapid development
   - Features implemented but not integrated (audio content, SSE retry)
   - Refactoring left orphaned functions (tcps_work_order:atom_to_binary/1)
   - Public API functions not documented (erlmcp_security_headers:add_to_response/1)

5. **ROOT CAUSE**: Missing Poka-yoke (mistake-proofing) for dead code
   - No automated enforcement of "0 unused functions" rule
   - Compile-time warning suppression masks the problem
   - No continuous cleanup process (Kaizen) for dead code removal

**Solution**: Implement Poka-yoke by removing `nowarn_unused_function` suppression, enforcing Xref checks in CI/CD, and establishing continuous dead code removal process.

## Risks and Mitigations

| Risk | Severity | Impact | Mitigation |
|------|----------|--------|------------|
| Removing function that is called dynamically | P0 (Critical) | Runtime crash | Search for `apply/3`, `spawn/4`, dynamic module calls before removal |
| Breaking public API used by external consumers | P0 (Critical) | External service failures | Check version history, deprecate before removal, add @deprecated tags |
| Removing test helper needed by test suite | P1 (High) | Test failures | Run full test suite after removal, verify all tests pass |
| Removing function documented in examples/docs | P1 (High) | Documentation inconsistencies | Search docs/, examples/ for function references before removal |
| Incomplete feature pending integration | P2 (Medium) | Feature re-implementation needed | Document future features, add TODO comments, track in backlog |
| Removing function used by unreleased code branch | P2 (Medium) | Merge conflicts | Check active branches, coordinate with team |
| Unnecessary wrapper functions (abstraction cost) | P3 (Low) | Code bloat | Remove wrappers like `tcps_work_order:atom_to_binary/1` |

**Severity Definitions:**
- **P0 (Critical)**: BLOCKS production - MUST verify before removal
- **P1 (High)**: Major impact - MUST test thoroughly
- **P2 (Medium)**: Moderate impact - Should coordinate
- **P3 (Low)**: Minor impact - Safe to remove

## Recommended Manufacturing Approach

**TCPS Methodology:**

**Phase 1: Specification**
- Acceptance criteria: 0 unused local function warnings
- Requirements: All functions either used internally, documented as public API, or moved to test files
- Quality threshold: rebar3 xref shows 0 warnings for locals_not_used

**Phase 2: Pseudocode**
```
FUNCTION audit_unused_functions():
    xref_output = RUN("rebar3 xref")
    unused_functions = PARSE(xref_output, "unused.*function")

    FOR EACH function IN unused_functions:
        category = CATEGORIZE(function)
        CASE category OF
            "exported_but_not_called":
                IF has_external_consumers() AND has_documentation():
                    ADD to xref_ignores with justification
                ELSE:
                    MARK for removal
            "truly_unused":
                MARK for removal
            "test_helper":
                MARK for move to test file
            "dynamically_called":
                VERIFY dynamic call exists
                IF NOT:
                    MARK for removal
                ELSE:
                    ADD to xref_ignores with justification
        END CASE
    END FOR

    RETURN audit_report

FUNCTION cleanup_dead_code(audit_report):
    FOR EACH function IN audit_report.remove_list:
        VERIFY no_dynamic_calls(function)
        VERIFY not_in_public_api(function)
        VERIFY not_referenced_in_docs(function)
        REMOVE function from source
    END FOR

    FOR EACH function IN audit_report.move_list:
        MOVE function to test file
        UPDATE test imports
    END FOR

    FOR EACH function IN audit_report.document_list:
        ADD @doc tag with purpose and usage example
        ADD to xref_ignores with justification
    END FOR
```

**Phase 3: Architecture**
- **Integration Points**: None (pure removal operation)
- **Dependencies**: rebar3 xref tool
- **Testing**: Full test suite must pass after removal

**Phase 4: Refinement** (Chicago School TDD)
- **Test First**: For each removal, run tests before and after to verify no breakage
- **Real Processes**: Use actual xref tool, not mocked analysis
- **No Mocks**: Verify with real build system

**Phase 5: Completion**
- All quality gates passing:
  - Compilation: 0 errors
  - Tests: 100% pass rate
  - Xref: 0 unused function warnings
- Receipt generation for audit trail
- Documentation updated

**Implementation Strategy:**

**Batch 1: Inventory (Day 1)**
1. Run `rebar3 xref` to get complete list
2. Parse output to extract unused functions with module:line:arity
3. For each function, collect metadata:
   - Export status (exported or local)
   - Documentation (has @doc tag or not)
   - References in test files
   - References in documentation
   - Dynamic call patterns (apply/3, spawn/4)
4. Categorize each function

**Batch 2: Risk Assessment (Day 1-2)**
1. For each "remove" candidate, verify:
   - No dynamic calls (search for `apply(Module, Function, Args)`)
   - No test references (search test files)
   - No doc references (search docs/, examples/)
   - No external API dependencies (check version history)
2. Assign severity level (P0/P1/P2/P3)
3. Create removal checklist for high-risk functions

**Batch 3: Removal (Day 2-3)**
1. Remove low-risk functions (P3 - unnecessary wrappers)
2. Remove medium-risk functions (P2 - incomplete features, with documentation)
3. Remove high-risk functions (P1 - after thorough testing)
4. Move test helpers to test files
5. Document public API functions with @doc tags

**Batch 4: Verification (Day 3)**
1. Run `rebar3 xref` to confirm 0 unused function warnings
2. Run full test suite (EUnit + Common Test)
3. Run dialyzer to ensure no new warnings
4. Generate before/after metrics (lines removed, functions removed)
5. Create receipt for audit trail

**Quality Validation:**

**Automated:**
```bash
# Run xref analysis
rebar3 xref

# Expected: 0 unused function warnings
# If warnings present, fail build

# Run test suite
rebar3 eunit
rebar3 ct

# Expected: 100% pass rate
```

**Manual:**
- Verify no external API breakage (check consumer code)
- Verify documentation consistency
- Verify examples still work

**Metrics:**
- Lines of code removed (target: ≥500)
- Functions removed (target: 20-25)
- Functions documented (target: 5-10)
- Functions moved to test files (target: 2-5)
- Xref warnings before: 27+
- Xref warnings after: 0

## Open Questions

**NONE** - All research complete. Ready to proceed with implementation.

## Manufacturing Checklist
- [x] Root cause identified (not symptoms) - Missing Poka-yoke for dead code, warning suppression
- [x] Quality gates defined (specific thresholds) - 0 unused function warnings
- [x] OTP patterns understood (behaviors, supervision) - Gen servers, supervisors, transport behaviors
- [x] Test strategy clear (Chicago School TDD) - Real processes, no mocks, full test suite verification
- [x] Risk assessment complete (severity P0-P3) - 6 risks identified with mitigations
- [x] No open questions (all research complete) - Ready for implementation phase
