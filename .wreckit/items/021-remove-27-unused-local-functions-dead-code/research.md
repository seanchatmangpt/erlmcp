# Research: Remove 27+ unused local functions (dead code)

**Date**: 2026-01-29
**Item**: 021-remove-27-unused-local-functions-dead-code
**Section**: quality-gates
**TCMS Compliance**: Lean Six Sigma Level Strictness

## Manufacturing Requirement
27+ unused local functions cluttering the codebase

**Motivation:** Dead code confuses maintainers, increases cognitive load, and may contain bugs. Removing it improves code quality.

**Success criteria:**
- All 27+ unused functions audited
- Truly unused functions removed
- Public API functions documented
- Test-only functions moved to test files
- rebar3 xref shows 0 unused function warnings
- Lines of code removed ≥500 (estimated)

**Technical constraints:**
- Run rebar3 xref to get list
- For each unused function: check if truly unused, public API, or testing stub
- Remove truly unused functions
- Document public API functions
- Move test-only functions to test files

**Signals:** priority: medium, urgency: P2 - REQUIRED FOR PRODUCTION

### Quality Gate Status
- **Gate Type**: Xref (Cross-Reference Analysis - Unused Functions)
- **Current State**: 26-28 unused local function warnings (part of 250 total xref warnings)
- **Target State**: 0 unused local functions
- **Gap**: 26-28 unused functions must be audited, removed, or documented

## Summary

This research investigates the **unused local functions** component of the Xref quality gate failure. The erlmcp v2.1.0 codebase has approximately **26-28 unused local functions** scattered across 4 applications (erlmcp_core, erlmcp_transports, erlmcp_observability, tcps_erlmcp). These functions are defined but never called internally, representing dead code that should be removed or documented as public API.

### Manufacturing Objective
Achieve **0 unused local function warnings** by systematically auditing each unused function and either:
1. **Removing dead code** (truly unused functions)
2. **Documenting as public API** (exported functions intended for external use)
3. **Moving to test files** (test-only helper functions)

### Technical Approach

1. **Run Xref Analysis**: Execute `rebar3 xref` to get complete list of unused local functions with module:line locations
2. **Categorization Phase**: For each unused function, determine:
   - Is it exported? → Public API candidate (document or remove)
   - Is it called dynamically? → Add `-compile({nowarn_unused_function, [...]})`
   - Is it a test helper? → Move to test file
   - Truly unused? → Remove (dead code elimination)
3. **Cleanup Phase**: Remove dead code, document public API, reorganize test helpers
4. **Verification**: Run `rebar3 xref` to confirm 0 unused function warnings

### TCPS Justification

**Jidoka (Built-in Quality)**: Unused functions represent **latent defects** - dead code that confuses maintainers and may contain bugs. Xref warnings are ANDON events that must be addressed before production.

**Poka-yoke (Mistake-Proofing)**: Removing dead code eliminates confusion and prevents future developers from mistakenly calling obsolete functions. Xref enforces this at compile time.

**Kaizen (Continuous Improvement)**: Dead code removal is continuous improvement - each useless function eliminated reduces cognitive load and maintenance burden. Estimated 500+ lines of code removed.

**Heijunka (Production Leveling)**: Break down 26-28 functions into small batches:
- Batch 1: Audit and categorize all 26-28 functions (inventory phase)
- Batch 2: Remove truly unused functions (cleanup phase)
- Batch 3: Document public API functions (documentation phase)
- Batch 4: Verify with xref (validation phase)

**Andon (Visible Problem Signaling)**: Xref warnings are visible quality gate failures. Each unused function must be explicitly addressed - no silent acceptance.

## Current State Analysis

### Existing Implementation

**Xref Configuration**:
- `/Users/sac/erlmcp/rebar.config:152-158` - Xref checks enabled (includes `locals_not_used`)
- `/Users/sac/erlmcp/rebar.config:19` - Global compiler directive: `nowarn_unused_function` (suppresses warnings!)
- **ROOT CAUSE**: The root rebar.config has `nowarn_unused_function` which **suppresses** unused function warnings at compile time, but Xref still detects them at analysis time

**Current Warnings**:
- Item 020 research: **250 total Xref warnings** (222 undefined + 28 unused)
- CODE_QUALITY_REPORT_V2.1.md: "Xref analysis completed. Critical undefined functions documented. Unused functions acceptable as API surface."
- **Problem**: "Acceptable as API surface" is not a valid reason - each unused function must be explicitly justified

**Known Unused Functions** (from Item 020 research):

1. **erlmcp_security_headers.erl**:
   - `add_to_response/1` - Line 184-189, exported but not used internally
   - **Decision**: DOCUMENT as public API (intended for Cowboy middleware integration)

2. **erlmcp_server.erl**:
   - `create_audio_content/3` - Lines 9-14, already suppressed with `-compile({nowarn_unused_function, [...]})`
   - `create_audio_content_with_metadata/4` - Lines 9-14, already suppressed
   - `get_tool_description_max_length/0` - Lines 9-14, already suppressed
   - `validate_tool_description/1` - Lines 9-14, already suppressed
   - **Status**: Already marked as intentionally unused (audio content support for future MCP spec)

3. **erlmcp_transport_sse.erl**:
   - `format_retry_field/1` - Line 490-492
   - `get_retry_timeout/0` - Line 470-483
   - **Decision**: These are helper functions for SSE retry field (Gap #29), but may not be called yet. Verify if truly unused or part of incomplete feature.

4. **tcps_receipt_verifier.erl**:
   - `is_atom_stage/1` - Line 765-770
   - **Decision**: Appears to be validation helper. Check if used in receipt validation logic. May be obsolete after refactoring.

5. **tcps_work_order.erl**:
   - `atom_to_binary/1` - Line 2110-2113 (duplicate of erlang:atom_to_binary/2)
   - **Decision**: REMOVE - wrapper around erlang:atom_to_binary, unnecessary abstraction

### Key Files

**Xref Configuration**:
- `rebar.config:152-158` - Xref checks (locals_not_used enabled)
- `rebar.config:19` - Global `nowarn_unused_function` directive (ROOT CAUSE of suppression)
- `rebar.config:160-195` - Xref ignores list (should add legitimate public API functions here)

**Source Files with Unused Functions**:
- `apps/erlmcp_transports/src/erlmcp_security_headers.erl:184-189` - `add_to_response/1`
- `apps/erlmcp_core/src/erlmcp_server.erl:9-14` - Audio content functions (already suppressed)
- `apps/erlmcp_transports/src/erlmcp_transport_sse.erl:470-492` - Retry field helpers
- `apps/tcps_erlmcp/src/tcps_receipt_verifier.erl:765-770` - `is_atom_stage/1`
- `apps/tcps_erlmcp/src/tcps_work_order.erl:2110-2113` - `atom_to_binary/1`

**Documentation References**:
- `docs/DIALYZER_REPORT.md` - Mentions unused functions with fix strategies
- `CODE_QUALITY_REPORT_V2.1.md:189` - Incorrectly states "Unused functions acceptable as API surface"
- `BACKLOG.md:1315-1358` - Item 021 requirements

### OTP Patterns Observed

**Modules with Unused Functions**:
- **Gen Servers**: None (gen_server callbacks are always used)
- **Utility Modules**: erlmcp_security_headers, tcps_receipt_verifier, tcps_work_order
- **Transport Modules**: erlmcp_transport_sse (incomplete SSE implementation)
- **Server Modules**: erlmcp_server (audio content functions for future MCP spec)

**Pattern**: Unused functions fall into 3 categories:
1. **Public API exports** - Exported but not called internally (e.g., erlmcp_security_headers:add_to_response/1)
2. **Future features** - Implemented but not yet integrated (e.g., audio content functions)
3. **Dead code** - Obsolete helpers (e.g., tcps_work_order:atom_to_binary/1)

## Technical Considerations

### Dependencies
- **Internal Modules**: None - this is pure dead code removal
- **External Libraries**: None
- **Xref Tool**: rebar3 xref (part of Erlang/OTP)

### TCPS Quality Gates to Pass
- [ ] **Compilation**: 0 errors (MUST maintain after removing functions)
- [ ] **EUnit**: 100% pass rate (MUST not break tests)
- [ ] **Common Test**: 100% pass rate (MUST not break integration tests)
- [ ] **Coverage**: ≥80% (MAY decrease - removing dead code lowers LOC)
- [ ] **Dialyzer**: 0 warnings (MUST not introduce new type errors)
- [ ] **Xref**: 0 unused function warnings (PRIMARY GOAL)
- [ ] **Performance**: N/A (code removal should have no impact or slight improvement)

### Patterns to Follow

**Decision Matrix for Each Unused Function**:

| Category | Action | Example |
|----------|--------|---------|
| Exported function, public API | Document, add to xref_ignores | erlmcp_security_headers:add_to_response/1 |
| Future feature, not yet integrated | Suppress warning, document roadmap | erlmcp_server:create_audio_content/3 |
| Test helper | Move to test file (test/*_tests.erl) | Any function used only in tests |
| Dead code (truly unused) | Remove | tcps_work_order:atom_to_binary/1 |

**Example: Documenting Public API**:
```erlang
%% @doc Add security headers to HTTP response (Cowboy middleware integration)
%% Public API function - exported but not used internally
%% Intended for Cowboy middleware pipeline integration
-spec add_to_response(map()) -> map().
```

**Example: Removing Dead Code**:
```erlang
%% BEFORE (dead code):
atom_to_binary(Atom) when is_atom(Atom) ->
    erlang:atom_to_binary(Atom);
atom_to_binary(Binary) when is_binary(Binary) ->
    Binary.

%% AFTER (removed - callers use erlang:atom_to_binary/2 directly):
%% (function deleted)
```

**Example: Moving Test Helpers**:
```erlang
%% BEFORE (in src/erlmcp_server.erl):
%% @private Test helper - only used in tests
debug_state(State) ->
    {ok, State}.

%% AFTER (in test/erlmcp_server_tests.erl):
%% @private Test helper
debug_state(State) ->
    {ok, State}.
```

## Root Cause Analysis (5 Whys)

**Problem**: 26-28 unused local functions cluttering the codebase, Xref quality gate failing

1. **Why?** Functions are defined but never called internally, detected by Xref `locals_not_used` check
2. **Why?** Developers added functions for future features, public API, or as helpers that became obsolete
3. **Why?** No systematic code review process to remove dead code during refactoring
4. **Why?** Root rebar.config has `nowarn_unused_function` directive that **suppresses** compiler warnings, making dead code invisible during development
5. **ROOT CAUSE**: Missing **Poka-yoke** (mistake-proofing) - no automated enforcement to prevent dead code accumulation. The compiler warning suppression (`nowarn_unused_function` at rebar.config:19) allows dead code to accumulate silently.

**Solution**:
1. **Remove `nowarn_unused_function` from root rebar.config** (line 19) - make dead code visible at compile time
2. **Audit all 26-28 unused functions** using Item 020 decision matrix
3. **Remove truly unused functions** (dead code elimination)
4. **Document public API functions** with `@doc` comments explaining why they're exported
5. **Add intentional public API functions to xref_ignores** in rebar.config
6. **Establish Kaizen process** - include Xref check in code review checklist

## Risks and Mitigations

| Risk | Severity | Impact | Mitigation |
|------|----------|--------|------------|
| **Removing unused function breaks external API** | P1 (High) | External clients calling "unused" function will get `undefined function` error at runtime | **MITIGATION**: For EACH unused function: (1) Check if exported, (2) Search codebase for callers, (3) Check examples/ for usage, (4) If exported but no callers found, ADD TO xref_ignores with comment explaining it's public API |
| **Dead code removal eliminates needed functionality** | P2 (Medium) | Function appears unused but is called dynamically (apply/3, spawn/3) or used in hot code loading | **MITIGATION**: Search for `apply/3`, `spawn/3`, `Module:Function` patterns before removing. Add `-compile({nowarn_unused_function, [{Func, Arity}]})` for dynamic call sites |
| **Test helpers moved to test files break test isolation** | P2 (Medium) | Moving test-only functions may break test module dependencies | **MITIGATION**: Verify tests still pass after moving functions. Use `-include_lib("eunit/include/eunit.hrl").` in test files |
| **Removing nowarn_unused_function causes compiler warnings explosion** | P2 (Medium) | Root rebar.config line 19 suppresses warnings globally. Removing it may expose 100+ warnings | **MITIGATION**: Do NOT remove global directive yet. Instead, (1) Fix these 26-28 functions first, (2) Then gradually remove directive in phases, (3) Add module-level `-compile({nowarn_unused_function, [...]})` for intentional public API |
| **Xref false positives (function is used but Xref can't detect)** | P3 (Low) | Xref may not detect calls through macros, parse transforms, or generated code | **MITIGATION**: Manually verify each function is truly unused by searching codebase. Add to xref_ignores if legitimate false positive |
| **Public API functions not documented** | P3 (Low) | Exported functions lack documentation explaining their purpose | **MITIGATION**: Add `@doc Public API` comments to all exported unused functions. Document in module header |

**Severity Definitions**:
- **P0 (Critical)**: BLOCKS production - MUST fix immediately (none identified)
- **P1 (High)**: Major quality gap - MUST fix before release (external API breakage)
- **P2 (Medium)**: Important but not blocking (dynamic calls, test helpers)
- **P3 (Low)**: Nice-to-have (documentation)

## Recommended Manufacturing Approach

**TCPS Methodology:**

1. **Specification** (Requirements with acceptance criteria):
   - Goal: 0 unused local function warnings from `rebar3 xref`
   - Scope: 26-28 functions across 4 applications
   - Success: Xref passes, tests pass, compilation succeeds

2. **Pseudocode** (Algorithm design BEFORE coding):
   ```
   FOR each unused_function IN xref_output:
     IF exported AND has_callers:
       KEEP (public API)
       ADD @doc Public API comment
       ADD to xref_ignores
     ELSE IF exported AND no_callers:
       DECIDE: Document as public API OR Remove
     ELSE IF has_test_callers:
       MOVE to test/*_tests.erl
     ELSE IF called_dynamically:
       SUPPRESS with -compile directive
       ADD comment explaining dynamic call
     ELSE:
       REMOVE (dead code)
   END FOR

   RUN rebar3 xref
   VERIFY 0 unused function warnings
   RUN rebar3 eunit
   VERIFY 100% test pass rate
   ```

3. **Architecture** (Integration points and dependencies):
   - **Input**: `rebar3 xref` output (list of unused functions)
   - **Processing**: Manual audit of each function (categorization)
   - **Output**: Modified source files, updated xref_ignores
   - **Verification**: `rebar3 xref` (0 warnings), `rebar3 eunit` (100% pass)

4. **Refinement** (Chicago School TDD - tests FIRST):
   - No new tests needed (this is dead code removal)
   - Existing tests MUST continue to pass (regression prevention)
   - Run tests after EACH batch of function removals

5. **Completion** (All quality gates passing):
   - Xref: 0 unused function warnings
   - Compilation: 0 errors
   - EUnit: 100% pass rate
   - Common Test: 100% pass rate
   - Coverage: May decrease (expected - removing dead code lowers LOC)

**Implementation Strategy:**

**Phase 1: Inventory (1 hour)**
- Run `rebar3 xref 2>&1 | tee /tmp/xref_unused.txt`
- Extract all unused local function warnings
- Create spreadsheet: Module, Function, Arity, Line, Exported?, Decision

**Phase 2: Categorization (2 hours)**
- For each unused function:
  1. Check if exported (`-export([...])` directive)
  2. Search codebase for callers: `grep -r "function_name(" apps/`
  3. Check examples/ for usage
  4. Determine category: Public API / Test Helper / Dynamic Call / Dead Code

**Phase 3: Cleanup (3 hours)**
- **Batch 1: Remove dead code** (truly unused, not exported)
  - Remove function definition
  - Verify no callers exist
  - Commit: "Remove dead code: [function list]"

- **Batch 2: Document public API** (exported but unused internally)
  - Add `@doc Public API` comment
  - Add to xref_ignores in rebar.config
  - Commit: "Document public API functions"

- **Batch 3: Move test helpers** (used only in tests)
  - Move function to test/*_tests.erl
  - Update test imports
  - Commit: "Move test helpers to test files"

- **Batch 4: Suppress legitimate unused** (future features, dynamic calls)
  - Add module-level `-compile({nowarn_unused_function, [...]})`
  - Add comment explaining why function is kept
  - Commit: "Suppress warnings for intentional functions"

**Phase 4: Verification (1 hour)**
- Run `rebar3 clean`
- Run `rebar3 compile`
- Run `rebar3 xref` - MUST show 0 unused function warnings
- Run `rebar3 eunit` - MUST show 100% pass rate
- Run `rebar3 ct` - MUST show 100% pass rate
- Generate final report: Functions removed, documented, moved

**Quality Validation:**

**Automated**:
```bash
# Clean build
rebar3 clean

# Compile (MUST succeed)
rebar3 compile

# Xref (MUST show 0 unused function warnings)
rebar3 xref 2>&1 | grep -i "unused.*function" | wc -l
# Expected output: 0

# Tests (MUST pass)
rebar3 eunit
rebar3 ct
```

**Manual**:
- [ ] Review xref_ignores in rebar.config - ensure all public API functions documented
- [ ] Search examples/ for usage of "unused" functions
- [ ] Verify no dynamic calls (apply/3, spawn/3) to removed functions
- [ ] Check test files for moved test helpers

**Metrics**:
- **Functions removed**: [Count] (estimated: 15-20)
- **Functions documented as public API**: [Count] (estimated: 5-8)
- **Functions moved to test files**: [Count] (estimated: 2-3)
- **Lines of code removed**: [Count] (estimated: ≥500)
- **Xref unused warnings**: 0 (from 26-28)
- **Test pass rate**: 100% (must not regress)

## Open Questions

**Question 1**: Should we remove the global `nowarn_unused_function` directive from rebar.config:19?
- **Answer**: NO - Not yet. This would expose 100+ warnings. Fix these 26-28 functions first, then gradually remove directive in future PR (Item 022: "Enable all compiler warnings").

**Question 2**: What if an "unused" function is part of a public API that hasn't been released yet?
- **Answer**: Document as `@doc Public API (future release - not yet used internally)`. Add to xref_ignores. This is intentional dead code for future features.

**Question 3**: How do we distinguish between "public API" and "dead code" for exported functions?
- **Answer**: Search codebase + examples/ for callers. If no callers found, check module documentation. If function is in security/transport/API module, likely public API. If in internal utility module, likely dead code.

**Question 4**: Should we add `-compile({nowarn_unused_function, [...]})` for public API functions?
- **Answer**: NO - Better to add to xref_ignores with comment. Compiler directives suppress warnings, but xref_ignores explicitly document intentional exceptions. This is more transparent.

**Question 5**: What if removing a function breaks tests?
- **Answer**: Function is NOT dead code - it's a test helper. Move to test file instead of removing.

**Question 6**: Estimated time to complete?
- **Answer**: 7 hours total (1h inventory + 2h categorization + 3h cleanup + 1h verification)

**Question 7**: Should this be done in one PR or multiple phases?
- **Answer**: One PR with multiple commits (Phase 3 batches). Easier to review and rollback if needed.

## Manufacturing Checklist
- [x] Root cause identified (nowarn_unused_function suppression + no Poka-yoke)
- [x] Quality gates defined (Xref: 0 unused function warnings)
- [x] OTP patterns understood (gen_server, supervisor, transport behavior)
- [x] Test strategy clear (Chicago School TDD - existing tests must pass)
- [x] Risk assessment complete (6 risks identified, severity P1-P3)
- [x] Open questions answered (7 questions addressed)
- [x] Implementation strategy defined (4 phases, 7 hours estimated)
