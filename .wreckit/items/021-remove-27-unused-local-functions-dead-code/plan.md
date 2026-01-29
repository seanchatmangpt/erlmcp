# Remove 27+ unused local functions (dead code) Manufacturing Plan

**TCPS Compliance**: Lean Six Sigma Level Strictness
**Zero Defects**: 99.99966% defect-free delivery (3.4 defects per million)

## Manufacturing Objective

Eliminate **26-28 unused local function warnings** from the Xref quality gate by systematically auditing, categorizing, and either removing dead code or documenting public API functions. This reduces cognitive load, eliminates potential bugs, and achieves **0 unused function warnings** from `rebar3 xref`.

**Estimated Lines of Code Removed**: ≥500 LOC

### Quality Gate Requirements
- **Compilation**: 0 errors (MANDATORY)
- **EUnit**: 100% pass rate (MANDATORY)
- **Common Test**: 100% pass rate (if applicable)
- **Coverage**: ≥80% (MANDATORY - note: removing dead code may lower LOC percentage)
- **Dialyzer**: 0 warnings (MANDATORY)
- **Xref**: 0 unused local function warnings (PRIMARY GOAL - 26-28 → 0)
- **Performance**: <10% regression from baseline (N/A - code removal only)

## Current State

### What Exists Now

**Xref Configuration**:
- `/Users/sac/erlmcp/rebar.config:152-158` - Xref checks enabled (includes `locals_not_used`)
- `/Users/sac/erlmcp/rebar.config:19` - Global `nowarn_unused_function` directive (ROOT CAUSE - suppresses warnings at compile time)
- `/Users/sac/erlmcp/rebar.config:160-195` - Xref ignores list (should add legitimate public API functions here)

**Current Warnings**:
- **Total Xref warnings**: 250 (222 undefined function calls + 28 unused local functions)
- **Unused functions**: 26-28 scattered across 4 applications (erlmcp_core, erlmcp_transports, erlmcp_observability, tcps_erlmcp)
- **Status**: Item 020 (Fix 250 Xref warnings) documented these but did NOT fix unused functions

**Known Unused Functions** (from Item 020 research):

1. **erlmcp_security_headers.erl**:
   - `add_to_response/1` - Line 184-189
   - **Status**: Exported but not used internally (verified in research)
   - **Action**: DOCUMENT as public API (Cowboy middleware integration), add to xref_ignores

2. **erlmcp_server.erl**:
   - `create_audio_content/3` - Lines 9-14
   - `create_audio_content_with_metadata/4` - Lines 9-14
   - `get_tool_description_max_length/0` - Lines 9-14
   - `validate_tool_description/1` - Lines 9-14
   - **Status**: Already suppressed with `-compile({nowarn_unused_function, [...]})`
   - **Action**: KEEP - future MCP spec audio support (document roadmap)

3. **erlmcp_transport_sse.erl**:
   - `format_retry_field/1` - Line 490-492
   - `get_retry_timeout/0` - Line 470-483
   - **Status**: Marked `@private` - may be truly unused (Gap #29 incomplete feature)
   - **Action**: VERIFY - remove if unused, or document if needed

4. **tcps_receipt_verifier.erl**:
   - `is_atom_stage/1` - Line 765-770
   - **Status**: Not exported - may be obsolete helper
   - **Action**: VERIFY - remove if truly unused

5. **tcps_work_order.erl**:
   - `atom_to_binary/1` - Line 2110-2113
   - **Status**: Wrapper around `erlang:atom_to_binary/2` - unnecessary abstraction
   - **Action**: REMOVE

**Additional Functions**:
- Research indicates 26-28 total unused functions, but only 5 specific functions documented in Item 020
- **Missing data**: Complete Xref output with all 26-28 functions not captured in research

### What's Missing

**Gap**: 26-28 unused local function warnings must be eliminated to pass Xref quality gate

**Root Cause** (5 Whys Analysis):
1. **Why?** Functions defined but never called internally, detected by Xref `locals_not_used` check
2. **Why?** Developers added functions for future features, public API, or helpers that became obsolete
3. **Why?** No systematic code review process to remove dead code during refactoring
4. **Why?** Root rebar.config has `nowarn_unused_function` directive that **suppresses** compiler warnings, making dead code invisible during development
5. **ROOT CAUSE**: Missing **Poka-yoke** (mistake-proofing) - no automated enforcement to prevent dead code accumulation

**Impact**:
- **Blocks**: Item 020 (Fix 250 Xref warnings) completion - unused functions component not addressed
- **Quality**: Dead code confuses maintainers, increases cognitive load, may contain bugs
- **Production**: P2 urgency - REQUIRED FOR PRODUCTION but not blocking

### Key Discoveries from Research

**Discovery 1**: `erlmcp_security_headers:add_to_response/1` is NOT exported (file:line 184-189, export list at lines 19-25)
- **Verification**: Check export list - function NOT in `-export([...])` directive
- **Decision**: REMOVE - truly dead code, not public API

**Discovery 2**: `erlmcp_server` audio content functions already suppressed (lines 9-14)
- **Verification**: Module has `-compile({nowarn_unused_function, [{create_audio_content, 3}, ...]})`
- **Decision**: KEEP - add roadmap comment documenting future MCP spec support

**Discovery 3**: `erlmcp_transport_sse` retry functions marked `@private` (lines 470-492)
- **Verification**: Marked as `@private`, may be incomplete Gap #29 implementation
- **Decision**: VERIFY - search codebase for callers, remove if unused

**Discovery 4**: `tcps_work_order:atom_to_binary/1` is duplicate wrapper (lines 2110-2113)
- **Verification**: Wrapper around `erlang:atom_to_binary/2`, adds no value
- **Decision**: REMOVE - unnecessary abstraction

**Discovery 5**: Global `nowarn_unused_function` at rebar.config:19 suppresses warnings
- **Verification**: Confirmed in rebar.config line 19
- **Decision**: DO NOT REMOVE yet - fix these 26-28 functions first, then gradual removal in future PR

## Desired End State

### Specification

**Goal**: Achieve 0 unused local function warnings from `rebar3 xref` by:

1. **Running Xref Analysis**: Execute `rebar3 xref 2>&1 | tee /tmp/xref_unused.txt` to get complete list of all 26-28 unused functions with module:line locations

2. **Categorization Phase**: For each unused function, determine:
   - **Is it exported?** → Public API candidate (document or remove)
   - **Is it called dynamically?** → Add `-compile({nowarn_unused_function, [...]})`
   - **Is it a test helper?** → Move to test file
   - **Truly unused?** → Remove (dead code elimination)

3. **Cleanup Phase**: Execute decisions from categorization:
   - **Remove dead code** (truly unused functions)
   - **Document public API** (exported functions with `@doc Public API` comments)
   - **Add to xref_ignores** (legitimate public API functions)
   - **Move test helpers** (test-only functions to `test/*_tests.erl`)

4. **Verification Phase**: Run `rebar3 xref` to confirm 0 unused function warnings

### Verification

**Automated Verification**:
```bash
# Clean build
rebar3 clean

# Compile (MUST succeed - 0 errors)
TERM=dumb rebar3 compile

# Xref (MUST show 0 unused function warnings)
rebar3 xref 2>&1 | grep -i "unused.*function" | wc -l
# Expected output: 0

# Tests (MUST pass - 100% rate)
rebar3 eunit
rebar3 ct

# Coverage (MUST be ≥80%)
rebar3 cover

# Dialyzer (MUST be 0 warnings)
rebar3 dialyzer
```

**Manual Verification**:
- [ ] Review xref_ignores in rebar.config - ensure all public API functions documented with comments
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

### Manufacturing Output

**Code Modified**:
- `apps/erlmcp_transports/src/erlmcp_security_headers.erl` - Remove `add_to_response/1`
- `apps/erlmcp_core/src/erlmcp_server.erl` - Add roadmap comment for audio functions
- `apps/erlmcp_transports/src/erlmcp_transport_sse.erl` - Verify/remove retry functions
- `apps/tcps_erlmcp/src/tcps_receipt_verifier.erl` - Verify/remove `is_atom_stage/1`
- `apps/tcps_erlmcp/src/tcps_work_order.erl` - Remove `atom_to_binary/1`
- Additional modules based on complete Xref analysis

**Configuration Modified**:
- `rebar.config:160-195` - Add legitimate public API functions to xref_ignores with documentation comments

**Documentation Updated**:
- `docs/XREF_CLEANUP_REPORT.md` - Created to document unused function audit results

**Receipts Generated**:
- `/tmp/xref_unused.txt` - Complete Xref output before cleanup
- `/tmp/xref_clean.txt` - Complete Xref output after cleanup (should show 0 unused)

## What We're NOT Doing

**OUT OF SCOPE** (to prevent scope creep):

- **Removing global `nowarn_unused_function` directive** - This is deferred to Item 022 (Enable all compiler warnings). Removing it now would expose 100+ warnings. We fix these 26-28 functions first.

- **Fixing undefined function calls** - This is Item 020's responsibility. We only address unused functions.

- **Adding tests for removed functions** - Dead code by definition has no tests. We verify existing tests still pass after removal.

- **Refactoring Xref configuration** - We only add entries to xref_ignores, no changes to Xref checks or configuration structure.

- **Performance optimization** - Code removal may slightly improve performance, but this is not the goal.

- **Documentation of public API** - We add minimal `@doc` comments for public API functions, but full API documentation is out of scope.

## Manufacturing Approach

### TCPS Methodology

Following Toyota Code Production System phases:

1. **Specification** - Requirements with acceptance criteria (0 unused function warnings)
2. **Pseudocode** - Algorithm design BEFORE coding (audit → categorize → cleanup → verify)
3. **Architecture** - Integration points (rebar3 xref, no code dependencies)
4. **Refinement** - Chicago School TDD (existing tests must pass - no new tests needed)
5. **Completion** - All quality gates passing (Xref: 0, EUnit: 100%, CT: 100%)

### Implementation Strategy

**Approach**: Systematic audit and removal of dead code in 4 phases:

**Phase 1: Inventory** (1 hour)
- Run `rebar3 xref 2>&1 | tee /tmp/xref_unused.txt`
- Extract all unused local function warnings
- Create spreadsheet: Module, Function, Arity, Line, Exported?, Decision

**Phase 2: Categorization** (2 hours)
- For each unused function:
  1. Check if exported (`grep -A 5 "^-export" module.erl`)
  2. Search codebase for callers (`grep -r "function_name(" apps/`)
  3. Check examples/ for usage
  4. Determine category: Public API / Test Helper / Dynamic Call / Dead Code

**Phase 3: Cleanup** (3 hours)
- **Batch 1**: Remove dead code (truly unused, not exported)
- **Batch 2**: Document public API (exported but unused internally)
- **Batch 3**: Move test helpers (used only in tests)
- **Batch 4**: Suppress legitimate unused (future features, dynamic calls)

**Phase 4: Verification** (1 hour)
- Run `rebar3 xref` - MUST show 0 unused function warnings
- Run `rebar3 eunit` - MUST show 100% pass rate
- Run `rebar3 ct` - MUST show 100% pass rate
- Generate final report with metrics

**Why This Strategy?**:
- **Minimizes risk**: Small batches, easy to rollback
- **Maximizes visibility**: Each batch has clear success criteria
- **Follows TCPS principles**: Heijunka (production leveling), Poka-yoke (quality gates), Jidoka (stop on failure)

### Quality Integration

**Pre-commit Hooks**: Not applicable - manual verification only

**CI Gates**: All quality gates must pass before PR merge:
- Compilation: 0 errors
- EUnit: 100% pass rate
- Common Test: 100% pass rate
- Coverage: ≥80%
- Dialyzer: 0 warnings
- Xref: 0 unused function warnings (PRIMARY GOAL)

**Receipt Generation**:
- `/tmp/xref_unused.txt` - Before cleanup (baseline)
- `/tmp/xref_clean.txt` - After cleanup (validation)
- `docs/XREF_CLEANUP_REPORT.md` - Audit results with decisions

**Andon Signaling**:
- **Red flag**: Xref shows >0 unused function warnings after cleanup - STOP and fix
- **Red flag**: Tests fail after removing function - function was NOT dead code, restore and investigate
- **Green flag**: All quality gates passing - ready for review

---

## Phases

### Phase 1: Inventory - Run Xref Analysis

#### Overview
Generate complete inventory of all 26-28 unused local functions with module:line locations for systematic audit and categorization.

#### Specification
**Output**: Spreadsheet (or Markdown table) listing:
- Module name
- Function name
- Arity
- Line number
- Exported? (yes/no)
- Decision (REMOVE / DOCUMENT / MOVE / SUPPRESS)
- Notes (why this decision)

#### Pseudocode
```bash
# Step 1: Run Xref and capture output
rebar3 xref 2>&1 | tee /tmp/xref_unused.txt

# Step 2: Extract unused function warnings
grep "unused.*function" /tmp/xref_unused.txt > /tmp/unused_only.txt

# Step 3: Parse and create inventory
# Parse format: "Module:Line: FunctionName/Arity is unused"
# Create table with columns: Module, Function, Arity, Line, Exported?, Decision, Notes

# Step 4: For each function, check if exported
for each unused_function:
    module = extract_module(unused_function)
    function = extract_function(unused_function)
    arity = extract_arity(unused_function)
    line = extract_line(unused_function)

    # Check if exported
    exports = run("grep -A 10 '^-export' apps/*/src/${module}.erl")
    exported = check_if_function_in_exports(function, arity, exports)

    add_to_inventory(module, function, arity, line, exported, "", "")
```

#### Architecture
**Input**: `rebar3 xref` output (26-28 unused function warnings)

**Processing**: Manual parsing of Xref output to extract module:function/arity information

**Output**: `docs/XREF_INVENTORY.md` - Markdown table with all unused functions and initial data

**Dependencies**: None - pure analysis phase

#### Changes Required:

##### 1. Xref Analysis Execution
**Command**: `rebar3 xref 2>&1 | tee /tmp/xref_unused.txt`
**Current**: Xref shows 26-28 unused local function warnings
**Output**: Complete Xref output saved to `/tmp/xref_unused.txt`
**Reason**: Baseline measurement - we need complete list before cleanup

##### 2. Inventory Creation
**File**: `docs/XREF_INVENTORY.md` (CREATE NEW)
**Current**: Does not exist
**Changes**: Create Markdown table with columns:
| Module | Function | Arity | Line | Exported? | Decision | Notes |
|--------|----------|-------|------|-----------|----------|-------|
| ... | ... | ... | ... | ... | ... | ... |

**Reason**: Structured inventory enables systematic audit - prevents missing functions

#### Success Criteria:

##### Automated Verification (MUST ALL PASS):
- [ ] Xref output captured: `test -f /tmp/xref_unused.txt` - PASS
- [ ] Inventory created: `test -f docs/XREF_INVENTORY.md` - PASS
- [ ] Inventory complete: `grep -c "|" docs/XREF_INVENTORY.md` ≥ 27 (26-28 functions + header)

##### Manual Verification:
- [ ] Review Xref output for "unused.*function" warnings
- [ ] Verify inventory captures ALL unused functions (count matches 26-28)
- [ ] Verify each function has module, name, arity, line number populated

**Note**: This phase is ANALYSIS ONLY - no code changes. Complete inventory BEFORE proceeding to Phase 2.

---

### Phase 2: Categorization - Audit Each Unused Function

#### Overview
Systematically audit each of the 26-28 unused functions to determine correct action: REMOVE (dead code), DOCUMENT (public API), MOVE (test helper), or SUPPRESS (future feature).

#### Specification
**Input**: `docs/XREF_INVENTORY.md` from Phase 1 (26-28 unused functions)

**Output**: Completed inventory with "Decision" column populated for all functions:
- **REMOVE**: Dead code (not exported, no callers)
- **DOCUMENT**: Public API (exported, intended for external use)
- **MOVE**: Test helper (used only in test files)
- **SUPPRESS**: Future feature (intentionally kept but not yet integrated)

**Decision Matrix**:

| If... | Then... | Action |
|-------|---------|--------|
| Exported AND has callers (examples/, tests/) | Public API | DOCUMENT + add to xref_ignores |
| Exported AND no callers found | Potential public API | DOCUMENT intention OR REMOVE if not needed |
| Not exported AND called by tests only | Test helper | MOVE to test/*_tests.erl |
| Not exported AND called dynamically | Dynamic call site | SUPPRESS with -compile directive |
| Not exported AND no callers | Dead code | REMOVE |

#### Pseudocode
```erlang
%% For each unused_function IN inventory:
%%   Step 1: Check if exported
%%     exports = grep("-export", module_src)
%%     is_exported = function_name IN exports
%%
%%   Step 2: Search for callers
%%     callers_codebase = grep("function_name(", "apps/")
%%     callers_examples = grep("function_name(", "examples/")
%%     callers_tests = grep("function_name(", "test/")
%%     callers = callers_codebase + callers_examples + callers_tests
%%
%%   Step 3: Check for dynamic calls
%%     dynamic_calls = grep("apply.*function_name", "apps/")
%%     dynamic_calls += grep("spawn.*function_name", "apps/")
%%     dynamic_calls += grep("Module:function_name", "apps/")
%%
%%   Step 4: Make decision
%%     IF is_exported AND callers > 0:
%%       decision = "DOCUMENT"  % Public API
%%       notes = "Exported function, " ++ callers ++ " callers found"
%%     ELSE IF is_exported AND callers == 0:
%%       IF module_name == "erlmcp_server" AND function_name == "create_audio":
%%         decision = "SUPPRESS"  % Future feature
%%         notes = "Audio support for future MCP spec"
%%       ELSE:
%%         decision = "REMOVE"  % Unused export
%%         notes = "Exported but no callers found"
%%     ELSE IF NOT is_exported AND callers_tests > 0:
%%       decision = "MOVE"  % Test helper
%%       notes = "Used only in tests: " ++ callers_tests
%%     ELSE IF NOT is_exported AND dynamic_calls > 0:
%%       decision = "SUPPRESS"  % Dynamic call site
%%       notes = "Called dynamically: " ++ dynamic_calls
%%     ELSE:
%%       decision = "REMOVE"  % Dead code
%%       notes = "Not exported, no callers found"
%%
%%   Step 5: Update inventory
%%     inventory[function].decision = decision
%%     inventory[function].notes = notes
```

#### Architecture
**Input Phase**: Inventory from Phase 1 (26-28 functions)

**Processing Phase**: Manual audit of each function using grep searches to find callers

**Output Phase**: Completed inventory with decisions documented

**Dependencies**: Phase 1 must be complete (inventory exists)

**Tools**: `grep`, `find`, manual code review

#### Changes Required:

##### 1. Audit All Unused Functions
**File**: `docs/XREF_INVENTORY.md`
**Current**: Inventory with blank "Decision" and "Notes" columns
**Changes**: Populate decision for each function based on decision matrix

**Example**:
```markdown
| Module | Function | Arity | Line | Exported? | Decision | Notes |
|--------|----------|-------|------|-----------|----------|-------|
| erlmcp_security_headers | add_to_response | 1 | 184 | No | REMOVE | Not exported, no callers found - dead code |
| erlmcp_server | create_audio_content | 3 | 400 | No | SUPPRESS | Future MCP spec audio support (already suppressed) |
| erlmcp_transport_sse | format_retry_field | 1 | 490 | No | VERIFY | Marked @private, check for callers in Gap #29 code |
| tcps_receipt_verifier | is_atom_stage | 1 | 765 | No | REMOVE | Not exported, likely obsolete helper |
| tcps_work_order | atom_to_binary | 1 | 2110 | No | REMOVE | Wrapper around erlang:atom_to_binary/2 - unnecessary |
```

**Reason**: Systematic audit ensures correct decision for each function - prevents breaking public API or removing needed code

##### 2. Document Decision Rationale
**File**: `docs/XREF_CATEGORIZATION_REPORT.md` (CREATE NEW)
**Current**: Does not exist
**Changes**: Create report documenting:
- Total functions audited: 26-28
- Decision counts: X REMOVE, Y DOCUMENT, Z MOVE, W SUPPRESS
- Rationale for each decision category
- Risk assessment (public API breakage risk)

**Reason**: Transparency - code reviewers can see WHY each decision was made

#### Success Criteria:

##### Automated Verification (MUST ALL PASS):
- [ ] Inventory complete: All 26-28 functions have "Decision" populated (no blank cells)
- [ ] Decisions valid: Each decision is one of REMOVE/DOCUMENT/MOVE/SUPPRESS
- [ ] Document created: `test -f docs/XREF_CATEGORIZATION_REPORT.md` - PASS

##### Manual Verification:
- [ ] Review each REMOVE decision - verify function is NOT exported
- [ ] Review each DOCUMENT decision - verify function IS exported or intended as public API
- [ ] Review each MOVE decision - verify function is called only from tests
- [ ] Review each SUPPRESS decision - verify legitimate reason (future feature, dynamic call)
- [ ] Spot-check 5 random functions by manually reading source code
- [ ] Verify no "REMOVE" decisions for exported functions (P1 risk)

**Note**: This phase is ANALYSIS ONLY - no code changes. Complete audit BEFORE proceeding to Phase 3.

**Quality Gate**: If ANY REMOVE decision is for an exported function, STOP and re-audit. This is P1 risk (external API breakage).

---

### Phase 3: Cleanup - Execute Decisions

#### Overview
Execute the decisions from Phase 2 by removing dead code, documenting public API functions, moving test helpers, and suppressing intentional functions. Implementation is split into 4 batches for risk mitigation.

#### Specification
**Input**: Completed inventory from Phase 2 (all 26-28 functions have decisions)

**Output**: Modified source files, updated xref_ignores, created documentation

**Batches**:
- **Batch 1**: Remove dead code (highest risk - verify NO external callers)
- **Batch 2**: Document public API (add @doc comments, add to xref_ignores)
- **Batch 3**: Move test helpers (relocate to test/*_tests.erl)
- **Batch 4**: Suppress legitimate unused (add -compile directives)

#### Pseudocode
```erlang
%% Batch 1: Remove Dead Code
%% FOR each unused_function WITH decision == "REMOVE":
%%   VERIFY: function is NOT exported
%%   VERIFY: no callers in apps/, examples/, test/
%%   REMOVE: delete function definition from source file
%%   COMMIT: "Remove dead code: Module:function/arity - [reason from notes]"

%% Batch 2: Document Public API
%% FOR each unused_function WITH decision == "DOCUMENT":
%%   VERIFY: function IS exported OR should be public API
%%   ADD @doc comment above function:
%%     "%% @doc Public API function - exported but not used internally"
%%     "%% Intended for [use case from notes]"
%%   ADD to rebar.config xref_ignores:
%%     "{Module, function, arity},  % Public API - [use case]"
%%   COMMIT: "Document public API: Module:function/arity - [use case]"

%% Batch 3: Move Test Helpers
%% FOR each unused_function WITH decision == "MOVE":
%%   VERIFY: function is called only from test files
%%   CUT function from src/module.erl
%%   PASTE function to test/module_tests.erl
%%   UPDATE test imports if needed
%%   COMMIT: "Move test helper: Module:function/arity to test/module_tests.erl"

%% Batch 4: Suppress Legitimate Unused
%% FOR each unused_function WITH decision == "SUPPRESS":
%%   VERIFY: legitimate reason (future feature, dynamic call, public API)
%%   ADD module-level -compile directive:
%%     "-compile({nowarn_unused_function, [{function, arity}]})."
%%   ADD comment explaining why function is kept:
%%     "%% Future feature: [description] - tracked in [item/backlog]"
%%   COMMIT: "Suppress warnings for intentional function: Module:function/arity - [reason]"
```

#### Architecture
**Input Phase**: Completed inventory from Phase 2

**Processing Phase**: 4 sequential batches of code changes, each committed separately

**Output Phase**: Modified source files, updated rebar.config, created documentation

**Dependencies**: Phase 2 must be complete (all decisions made)

**Risk Mitigation**:
- Each batch is a separate commit (easy to rollback)
- Batch 1 (REMOVE) has highest risk - verify NO external callers before deleting
- Run tests after EACH batch (catch breakage early)

#### Changes Required:

##### Batch 1: Remove Dead Code (REMOVE decisions)

##### 1. erlmcp_security_headers.erl - Remove add_to_response/1
**File**: `apps/erlmcp_transports/src/erlmcp_security_headers.erl`
**Current**: Lines 184-189 define `add_to_response/1` function (NOT exported)
**Changes**: DELETE lines 184-189
**Reason**: Function is NOT exported (export list at lines 19-25 does not include it), no callers found - truly dead code

```erlang
%% BEFORE (lines 184-189):
%% @doc Add security headers to HTTP response.
-spec add_to_response(map()) -> map().
add_to_response(Response) ->
    Headers = maps:get(headers, Response, []),
    Config = application:get_env(erlmcp, security_headers_config, #{}),
    NewHeaders = add_headers(Headers, Config),
    Response#{headers => NewHeaders}.

%% AFTER (deleted - function removed entirely):
```

**Verification**:
```bash
# Verify function is NOT exported
grep "add_to_response" apps/erlmcp_transports/src/erlmcp_security_headers.erl
# Expected: No results (function removed)

# Verify no callers exist
grep -r "add_to_response" apps/ examples/
# Expected: No results (no callers)
```

##### 2. tcps_work_order.erl - Remove atom_to_binary/1
**File**: `apps/tcps_erlmcp/src/tcps_work_order.erl`
**Current**: Lines 2110-2113 define `atom_to_binary/1` wrapper function
**Changes**: DELETE lines 2110-2113
**Reason**: Wrapper around `erlang:atom_to_binary/2`, adds no value - unnecessary abstraction

```erlang
%% BEFORE (lines 2110-2113):
atom_to_binary(Atom) when is_atom(Atom) ->
    erlang:atom_to_binary(Atom);
atom_to_binary(Binary) when is_binary(Binary) ->
    Binary.

%% AFTER (deleted - function removed entirely):
%% Callers should use erlang:atom_to_binary/2 directly
```

**Verification**:
```bash
# Verify no callers in codebase
grep -r "atom_to_binary(" apps/tcps_erlmcp/src/ | grep -v "erlang:atom_to_binary"
# Expected: No results (function removed)

# If callers exist, update them to use erlang:atom_to_binary(Atom, utf8)
```

##### 3. Additional Dead Code Removal (from complete inventory)
**File**: [Various modules from inventory]
**Current**: Unused functions marked REMOVE in inventory
**Changes**: DELETE each function definition
**Reason**: Not exported, no callers - dead code elimination

**Verification** (for each function):
```bash
# Verify function removed
grep "function_name" apps/*/src/module.erl
# Expected: Function definition deleted

# Verify no callers
grep -r "function_name(" apps/
# Expected: No results (no callers)
```

---

##### Batch 2: Document Public API (DOCUMENT decisions)

##### 1. erlmcp_server.erl - Add Roadmap Comment for Audio Functions
**File**: `apps/erlmcp_core/src/erlmcp_server.erl`
**Current**: Lines 9-14 have `-compile({nowarn_unused_function, [...]})` for audio functions
**Changes**: ADD comment block explaining roadmap BEFORE the -compile directive

```erlang
%% BEFORE (lines 9-14):
-compile([{nowarn_unused_function, [
    {create_audio_content, 3},
    {create_audio_content_with_metadata, 4},
    {get_tool_description_max_length, 0},
    {validate_tool_description, 1}
]}]).

%% AFTER (lines 9-23):
%% ============================================================================
%% ROADMAP: Audio Content Support (Future MCP Specification)
%% ============================================================================
%% The following functions are implemented for a future MCP specification
%% that will include audio content types (currently not in MCP 2025-11-25 spec).
%%
%% These functions are INTENTIONALLY UNUSED and will be integrated when
%% the MCP specification adds audio content support.
%%
%% Tracking: See MCP specification evolution at https://spec.modelcontextprotocol.io/
%% ============================================================================

-compile([{nowarn_unused_function, [
    {create_audio_content, 3},          % Future: Create audio content items
    {create_audio_content_with_metadata, 4},  % Future: Audio with metadata
    {get_tool_description_max_length, 0},     % Future: Tool description validation
    {validate_tool_description, 1}            % Future: Tool description validation
]}]).
```

**Reason**: Documents WHY these functions exist - prevents future developers from removing them as "dead code"

---

##### 2. Add Public API Functions to xref_ignores
**File**: `rebar.config`
**Current**: Lines 160-195 define xref_ignores list
**Changes**: ADD entries for public API functions marked DOCUMENT in inventory

```erlang
%% BEFORE (line 195):
{rebar_state, dir, 1}
]}

%% AFTER (insert after line 195):
    {rebar_state, dir, 1},

    %% Public API functions (exported but not used internally)
    %% Documented in docs/XREF_CATEGORIZATION_REPORT.md
    %% {module, function, arity},  % Use case description
]}

%% Example (if we find public API functions):
{rebar_state, dir, 1},

%% Public API functions (exported but not used internally)
%% Documented in docs/XREF_CATEGORIZATION_REPORT.md
%% {erlmcp_security_headers, add_to_response, 1},  % Cowboy middleware integration
]}
```

**Note**: Based on initial research, we may have ZERO public API functions to document (add_to_response is NOT exported). This section will be populated based on Phase 2 audit results.

**Reason**: Xref ignores list documents intentional exceptions - transparent why certain functions are "unused"

---

##### Batch 3: Move Test Helpers (MOVE decisions)

##### 1. [If test helpers found in inventory]
**File**: `apps/*/src/module.erl` → `apps/*/test/module_tests.erl`
**Current**: Test helper functions in src/ directory
**Changes**: CUT from src/module.erl, PASTE to test/module_tests.erl

```erlang
%% BEFORE (in apps/erlmcp_core/src/erlmcp_server.erl):
%% @private Test helper - only used in tests
debug_state(State) ->
    {ok, State}.

%% AFTER (in apps/erlmcp_core/test/erlmcp_server_tests.erl):
%% @private Test helper
debug_state(State) ->
    {ok, State}.
```

**Reason**: Test helpers belong in test files - clarifies they're not part of production API

**Verification**:
```bash
# Verify function removed from src/
grep "debug_state" apps/*/src/module.erl
# Expected: No results (removed)

# Verify function in test/
grep "debug_state" apps/*/test/module_tests.erl
# Expected: Function definition found

# Verify tests still pass
rebar3 eunit --module=module_tests
# Expected: 100% pass rate
```

---

##### Batch 4: Suppress Legitimate Unused (SUPPRESS decisions)

##### 1. [If additional legitimate unused functions found]
**File**: `apps/*/src/module.erl`
**Current**: Functions kept for future features or dynamic calls
**Changes**: ADD module-level `-compile({nowarn_unused_function, [{Func, Arity}]})` with comment

```erlang
%% BEFORE:
-module(my_module).
-export([public_api/1]).

my_helper(Arg) ->
    %% Helper for future feature
    {ok, Arg}.

%% AFTER:
-module(my_module).
-export([public_api/1]).

%% Future feature: my_helper/1 will be integrated in Item XXX
%% TODO: Remove -compile directive when feature is implemented
-compile([{nowarn_unused_function, [{my_helper, 1}]}]).

my_helper(Arg) ->
    %% Helper for future feature
    {ok, Arg}.
```

**Reason**: Explicit suppression with documentation prevents confusion - clear WHY function is kept

---

#### Success Criteria:

##### Automated Verification (MUST ALL PASS after EACH batch):
- [ ] Compilation: `TERM=dumb rebar3 compile` - 0 errors, 0 warnings
- [ ] EUnit: `rebar3 eunit` - 100% pass rate
- [ ] Common Test: `rebar3 ct` - 100% pass rate
- [ ] Coverage: `rebar3 cover` - ≥80% (may decrease - removing dead code lowers LOC)
- [ ] Dialyzer: `rebar3 dialyzer` - 0 warnings

##### Manual Verification (after ALL batches):
- [ ] Review git log - verify 4 commits (one per batch)
- [ ] Verify Xref output: `rebar3 xref 2>&1 | tee /tmp/xref_clean.txt`
- [ ] Count unused warnings: `grep -c "unused.*function" /tmp/xref_clean.txt`
- [ ] Expected output: 0 (all 26-28 unused functions eliminated)
- [ ] Compare before/after: `diff /tmp/xref_unused.txt /tmp/xref_clean.txt`

##### Quality Gates (ALL MUST PASS):
- [ ] Xref: 0 unused function warnings (PRIMARY GOAL)
- [ ] Tests: 100% pass rate (no regressions)
- [ ] Compilation: 0 errors, 0 warnings
- [ ] Documentation: All decisions documented in XREF_CATEGORIZATION_REPORT.md

**Note**: Run quality gates after EACH batch. If ANY gate fails, STOP and fix before proceeding to next batch. This is Jidoka (built-in quality).

---

### Phase 4: Verification - Validate Zero Defects

#### Overview
Validate that all 26-28 unused functions have been properly handled and Xref quality gate passes with 0 unused function warnings. Generate final report with metrics.

#### Specification
**Goal**: Achieve 0 unused local function warnings from `rebar3 xref`

**Verification Steps**:
1. Clean build: `rebar3 clean`
2. Compile: `rebar3 compile` (MUST succeed - 0 errors)
3. Xref: `rebar3 xref 2>&1 | tee /tmp/xref_final.txt` (MUST show 0 unused function warnings)
4. Tests: `rebar3 eunit` (MUST pass - 100% rate)
5. Coverage: `rebar3 cover` (MUST be ≥80%)
6. Dialyzer: `rebar3 dialyzer` (MUST be 0 warnings)

**Metrics Collection**:
- Functions removed: [Count]
- Functions documented: [Count]
- Functions moved: [Count]
- Functions suppressed: [Count]
- Lines of code removed: [Count]
- Xref unused warnings before: 26-28
- Xref unused warnings after: 0
- Test pass rate: 100%

#### Pseudocode
```bash
#!/bin/bash
# Verification script

# Step 1: Clean build
echo "=== Step 1: Clean build ==="
rebar3 clean

# Step 2: Compile
echo "=== Step 2: Compile ==="
TERM=dumb rebar3 compile
if [ $? -ne 0 ]; then
    echo "FAIL: Compilation failed"
    exit 1
fi

# Step 3: Run Xref
echo "=== Step 3: Xref Analysis ==="
rebar3 xref 2>&1 | tee /tmp/xref_final.txt

# Count unused function warnings
UNUSED_COUNT=$(grep -i "unused.*function" /tmp/xref_final.txt | wc -l | tr -d ' ')
echo "Unused function warnings: $UNUSED_COUNT"

if [ "$UNUSED_COUNT" -ne 0 ]; then
    echo "FAIL: Xref still has $UNUSED_COUNT unused function warnings"
    grep -i "unused.*function" /tmp/xref_final.txt
    exit 1
fi

# Step 4: Run EUnit
echo "=== Step 4: EUnit Tests ==="
rebar3 eunit
if [ $? -ne 0 ]; then
    echo "FAIL: EUnit tests failed"
    exit 1
fi

# Step 5: Run Common Test
echo "=== Step 5: Common Test ==="
rebar3 ct
if [ $? -ne 0 ]; then
    echo "FAIL: Common Test failed"
    exit 1
fi

# Step 6: Check Coverage
echo "=== Step 6: Coverage ==="
rebar3 cover
# Note: Coverage may decrease - removing dead code lowers LOC
# Verify ≥80% coverage (not percentage)

# Step 7: Run Dialyzer
echo "=== Step 7: Dialyzer ==="
rebar3 dialyzer
if [ $? -ne 0 ]; then
    echo "FAIL: Dialyzer warnings found"
    exit 1
fi

# Step 8: Generate Metrics
echo "=== Step 8: Metrics ==="
echo "Functions removed: [from git diff]"
echo "Functions documented: [from xref_ignores]"
echo "Lines removed: [from git diff --stat]"
echo "Xref unused warnings: 26-28 → 0"

echo "=== SUCCESS: All quality gates passing ==="
exit 0
```

#### Architecture
**Input Phase**: Completed cleanup from Phase 3 (all decisions executed)

**Processing Phase**: Run full quality gate suite (compile, xref, tests, coverage, dialyzer)

**Output Phase**:
- `/tmp/xref_final.txt` - Final Xref output (should show 0 unused)
- `docs/XREF_CLEANUP_REPORT.md` - Final report with metrics
- Git commits: 4 commits (one per batch)

**Dependencies**: Phase 3 must be complete (all cleanup batches done)

#### Changes Required:

##### 1. Final Xref Execution
**Command**: `rebar3 xref 2>&1 | tee /tmp/xref_final.txt`
**Current**: Xref shows 26-28 unused function warnings (before cleanup)
**Expected**: Xref shows 0 unused function warnings (after cleanup)
**Reason**: Primary quality gate - validates success

##### 2. Final Report Generation
**File**: `docs/XREF_CLEANUP_REPORT.md` (CREATE NEW)
**Current**: Does not exist
**Changes**: Create final report documenting:

```markdown
# Xref Unused Functions Cleanup Report

**Date**: [Date]
**Item**: 021-remove-27-unused-local-functions-dead-code
**TCPS Compliance**: Lean Six Sigma Level Strictness

## Summary

**Objective**: Eliminate 26-28 unused local function warnings from Xref quality gate

**Result**: ✅ SUCCESS - 0 unused function warnings (from 26-28)

## Metrics

| Metric | Before | After | Change |
|--------|--------|-------|--------|
| Unused function warnings | 26-28 | 0 | -26 to -28 |
| Functions removed | - | [Count] | -[Count] LOC |
| Functions documented as public API | - | [Count] | - |
| Functions moved to test files | - | [Count] | - |
| Functions suppressed (future features) | - | [Count] | - |
| Lines of code removed | - | [Count] | ≥500 LOC |
| Test pass rate | 100% | 100% | No regression |
| Coverage % | [Before%] | [After%] | May decrease |
| Dialyzer warnings | 0 | 0 | No regression |

## Actions Taken

### Batch 1: Remove Dead Code ([Count] functions)
- erlmcp_security_headers:add_to_response/1 - Not exported, no callers
- tcps_work_order:atom_to_binary/1 - Unnecessary wrapper
- [Additional functions from inventory]

### Batch 2: Document Public API ([Count] functions)
- erlmcp_server:create_audio_content/3 - Future MCP spec support
- [Additional functions from inventory]

### Batch 3: Move Test Helpers ([Count] functions)
- [Functions from inventory]

### Batch 4: Suppress Legitimate Unused ([Count] functions)
- [Functions from inventory]

## Quality Gates

All quality gates passing:

- [x] Compilation: 0 errors
- [x] EUnit: 100% pass rate
- [x] Common Test: 100% pass rate
- [x] Coverage: ≥80%
- [x] Dialyzer: 0 warnings
- [x] Xref: 0 unused function warnings ✅

## Files Modified

### Source Files
- apps/erlmcp_transports/src/erlmcp_security_headers.erl - Removed add_to_response/1
- apps/erlmcp_core/src/erlmcp_server.erl - Added roadmap comment
- apps/tcps_erlmcp/src/tcps_work_order.erl - Removed atom_to_binary/1
- [Additional files from inventory]

### Configuration
- rebar.config - Added public API functions to xref_ignores

### Documentation
- docs/XREF_INVENTORY.md - Created inventory
- docs/XREF_CATEGORIZATION_REPORT.md - Created audit results
- docs/XREF_CLEANUP_REPORT.md - This file

## Verification

**Before Xref**: [First 10 lines from /tmp/xref_unused.txt]
**After Xref**: [First 10 lines from /tmp/xref_final.txt]

**Unused Warnings Before**: 26-28
**Unused Warnings After**: 0

## Lessons Learned

1. **Global nowarn_unused_function directive hides dead code** - The root rebar.config line 19 suppresses warnings, making dead code invisible during development. Recommendation: Remove in future PR (Item 022) after fixing all warnings.

2. **Xref is more thorough than compiler** - Xref detects unused functions even when compiler warnings are suppressed. Xref should be the source of truth for dead code detection.

3. **Exported functions are not always public API** - Some exported functions are obsolete and should be removed. Need to audit exports regularly.

4. **Documentation is critical for future features** - Functions kept for future features need clear roadmap documentation to prevent confusion.

## Next Steps

1. ✅ Complete Item 020 (Fix 250 Xref warnings) - undefined functions component
2. ✅ Item 022 (Enable all compiler warnings) - Remove global nowarn_unused_function directive
3. 🔄 Establish Kaizen process - Include Xref check in code review checklist
4. 🔄 Add pre-commit hook - Run Xref before commit (optional)

## Sign-off

**Manufactured By**: [Your Name]
**Date**: [Date]
**TCPS Principles Applied**: Standard Work, Andon, Heijunka, Poka-yoke, Jidoka
**Quality Gates**: All passing ✅
```

**Reason**: Complete documentation of cleanup effort - transparency, audit trail, lessons learned

#### Success Criteria:

##### Automated Verification (MUST ALL PASS):
- [ ] Xref: `rebar3 xref 2>&1 | grep -c "unused.*function"` returns 0
- [ ] Compilation: `TERM=dumb rebar3 compile` - 0 errors, 0 warnings
- [ ] EUnit: `rebar3 eunit` - 100% pass rate
- [ ] Common Test: `rebar3 ct` - 100% pass rate
- [ ] Coverage: `rebar3 cover` - ≥80%
- [ ] Dialyzer: `rebar3 dialyzer` - 0 warnings

##### Manual Verification:
- [ ] Compare Xref output: `diff /tmp/xref_unused.txt /tmp/xref_final.txt` shows reduction from 26-28 to 0
- [ ] Review git log: Verify 4 commits (Phase 3 batches)
- [ ] Review final report: All metrics populated, all sections complete
- [ ] Verify documentation: XREF_INVENTORY.md, XREF_CATEGORIZATION_REPORT.md, XREF_CLEANUP_REPORT.md all exist

##### Quality Validation:
- [ ] All 26-28 unused functions addressed (inventory complete)
- [ ] All decisions documented in XREF_CATEGORIZATION_REPORT.md
- [ ] No exported functions removed (P1 risk check)
- [ ] No test failures (regression check)
- [ ] Coverage ≥80% (quality check)
- [ ] Lines removed ≥500 (estimated - verify with git diff --stat)

**Note**: This phase VALIDATES success. If ANY quality gate fails, STOP and return to Phase 3 to fix. Do NOT proceed to code review until ALL gates pass.

---

## Testing Strategy

### Chicago School TDD (MANDATORY)

**Principle**: NO MOCKS - Use real processes, real gen_servers, real integration

**Application to Dead Code Removal**:
- **NO NEW TESTS NEEDED** - Dead code by definition has no tests
- **EXISTING TESTS MUST PASS** - Regression prevention
- **STATE-BASED VERIFICATION** - If we remove a function, verify tests still pass (proves function was unused)

### Unit Tests (EUnit)

**What to Test**: Nothing new - this is dead code removal

**Regression Testing**:
- Run `rebar3 eunit` after EACH batch of function removals
- Verify 100% pass rate (no failures after removing functions)
- If tests fail, function was NOT dead code - restore and investigate

**Coverage Target**: ≥80% per module
- **Note**: Removing dead code LOWERS lines of code, which may INCREASE coverage percentage
- Example: If module has 100 LOC and 80 lines covered (80%), removing 20 lines of dead code leaves 80 LOC with 80 lines covered (100%)

### Integration Tests (Common Test)

**End-to-End Scenarios**:
- MCP client-server communication (stdio transport)
- HTTP server with security headers (verify add_to_response removal doesn't break)
- SSE transport with retry fields (verify format_retry_field removal doesn't break)
- Receipt validation (verify is_atom_stage removal doesn't break)

**Failure Scenarios**:
- Crashes: Ensure no undefined function errors after removing functions
- Timeouts: Ensure no missing functions cause timeouts
- Errors: Ensure proper error handling still works

### Manual Testing Steps

1. **Verify Xref Output**:
   ```bash
   rebar3 xref 2>&1 | tee /tmp/xref_final.txt
   grep -i "unused.*function" /tmp/xref_final.txt
   # Expected: No results (0 unused functions)
   ```

2. **Verify Compilation**:
   ```bash
   TERM=dumb rebar3 compile
   # Expected: 0 errors, 0 warnings
   ```

3. **Verify Tests Pass**:
   ```bash
   rebar3 eunit
   rebar3 ct
   # Expected: 100% pass rate
   ```

4. **Verify No Undefined Function Errors**:
   ```bash
   # Start Erlang shell
   rebar3 shell

   # Try calling removed functions (should fail with undefined)
   (erlmcp@127.0.0.1)1> erlmcp_security_headers:add_to_response(#{}).
   # Expected: {error, undefined_function} or similar

   # Verify public API still works
   (erlmcp@127.0.0.1)2> erlmcp_security_headers:add_headers([]).
   # Expected: {ok, Headers}
   ```

5. **Verify Coverage**:
   ```bash
   rebar3 cover
   # Expected: ≥80% coverage (may increase due to LOC reduction)
   ```

### Quality Gates

**Every phase MUST pass**:

1. **Compilation**: `TERM=dumb rebar3 compile`
   - **Success**: 0 errors, 0 warnings
   - **Failure**: STOP - fix compilation errors before proceeding

2. **EUnit**: `rebar3 eunit`
   - **Success**: 100% pass rate (all tests pass)
   - **Failure**: STOP - test failure means removed function was NOT dead code

3. **Common Test**: `rebar3 ct`
   - **Success**: 100% pass rate
   - **Failure**: STOP - integration test failure indicates removed function was needed

4. **Coverage**: `rebar3 cover`
   - **Success**: ≥80% coverage
   - **Failure**: STOP - investigate coverage drop (may be acceptable if LOC decreased)

5. **Dialyzer**: `rebar3 dialyzer`
   - **Success**: 0 warnings
   - **Failure**: STOP - type error introduced by removing function

6. **Xref**: `rebar3 xref`
   - **Success**: 0 unused function warnings (PRIMARY GOAL)
   - **Failure**: STOP - unused functions remain, return to Phase 3

**Quality Gate Enforcement**:
- Run ALL gates after Phase 3 (cleanup)
- Run Xref gate after Phase 4 (verification)
- If ANY gate fails, STOP and fix - this is Jidoka (built-in quality)

---

## Manufacturing Checklist

### Before Implementation

- [x] Research verified (read actual source code)
  - [x] erlmcp_security_headers.erl - add_to_response/1 NOT exported
  - [x] erlmcp_server.erl - audio functions already suppressed
  - [x] erlmcp_transport_sse.erl - retry functions marked @private
  - [x] tcps_receipt_verifier.erl - is_atom_stage/1 not exported
  - [x] tcps_work_order.erl - atom_to_binary/1 is duplicate wrapper
- [x] Scope confirmed (IN/OUT documented)
  - [x] IN: Audit 26-28 unused functions, remove dead code, document public API
  - [x] OUT: Removing global nowarn_unused_function (Item 022), fixing undefined calls (Item 020)
- [x] No open questions (all decisions made)
  - [x] Q1: Should we remove global nowarn_unused_function? NO - deferred to Item 022
  - [x] Q2: How to distinguish public API from dead code? Use decision matrix
  - [x] Q3: What if removing function breaks tests? It's NOT dead code - move to test file
  - [x] Q4: Should we add -compile directives for public API? NO - use xref_ignores
  - [x] Q5: Estimated time? 7 hours (1h inventory + 2h categorization + 3h cleanup + 1h verification)
  - [x] Q6: One PR or multiple? One PR with 4 commits (batches)
  - [x] Q7: What if xref shows false positives? Manual verification before removal
- [x] Phases broken down (≤4 hours each)
  - [x] Phase 1: Inventory (1 hour)
  - [x] Phase 2: Categorization (2 hours)
  - [x] Phase 3: Cleanup (3 hours)
  - [x] Phase 4: Verification (1 hour)
- [x] Acceptance criteria defined (measurable, specific)
  - [x] Xref: 0 unused function warnings (from 26-28)
  - [x] Tests: 100% pass rate (no regressions)
  - [x] Compilation: 0 errors, 0 warnings
  - [x] Coverage: ≥80%
  - [x] Dialyzer: 0 warnings
  - [x] LOC removed: ≥500

### During Implementation

- [ ] Chicago School TDD followed (tests FIRST)
  - [ ] NO NEW TESTS - dead code removal only
  - [ ] EXISTING TESTS MUST PASS - regression prevention
  - [ ] NO MOCKS - real processes only
- [ ] OTP patterns followed (gen_server, supervisor)
  - [ ] Not applicable - dead code removal only
- [ ] Type specs added (Dialyzer clean)
  - [ ] Not removing any type specs - only dead functions
- [ ] Error handling complete (all paths)
  - [ ] Not applicable - dead code removal only
- [ ] Quality gates passing (compilation, tests, coverage)
  - [ ] Verify after EACH batch in Phase 3
  - [ ] Verify complete after Phase 4

### After Implementation

- [ ] All tests passing (100% rate)
- [ ] Coverage ≥80% (verified)
- [ ] Dialyzer 0 warnings (verified)
- [ ] Xref 0 undefined calls (verified)
- [ ] Performance no regression >10% (N/A - code removal only)
- [ ] Documentation updated (XREF_CLEANUP_REPORT.md created)
- [ ] Code review complete (OTP patterns verified)
  - [ ] Verify no exported functions removed (P1 risk)
  - [ ] Verify all decisions documented
  - [ ] Verify git log shows 4 commits (batches)

---

## Risk Management

### Known Risks

| Risk | Severity | Probability | Mitigation |
|------|----------|-------------|------------|
| **Removing unused function breaks external API** | P1 (High) | Medium | **MITIGATION**: For EACH unused function: (1) Check if exported, (2) Search codebase for callers, (3) Check examples/ for usage, (4) If exported but no callers found, ADD TO xref_ignores with comment explaining it's public API. NEVER remove an exported function without verifying no external callers exist. |
| **Dead code removal eliminates needed functionality** | P2 (Medium) | Low | **MITIGATION**: Search for `apply/3`, `spawn/3`, `Module:Function` patterns before removing. Add `-compile({nowarn_unused_function, [{Func, Arity}]})` for dynamic call sites. Run tests after EACH batch - if tests fail, function was NOT dead code. |
| **Test helpers moved to test files break test isolation** | P2 (Medium) | Low | **MITIGATION**: Verify tests still pass after moving functions. Use `-include_lib("eunit/include/eunit.hrl").` in test files. Ensure moved functions have proper exports in test modules. |
| **Removing nowarn_unused_function causes compiler warnings explosion** | P2 (Medium) | N/A | **MITIGATION**: OUT OF SCOPE - We are NOT removing the global directive (Item 022). We only fix these 26-28 specific functions. No risk. |
| **Xref false positives (function is used but Xref can't detect)** | P3 (Low) | Low | **MITIGATION**: Manually verify each function is truly unused by searching codebase. Check for macro calls, parse transforms, generated code. Add to xref_ignores if legitimate false positive. |
| **Public API functions not documented** | P3 (Low) | Medium | **MITIGATION**: Add `@doc Public API` comments to all exported unused functions. Document in xref_ignores with use case description. Create XREF_CATEGORIZATION_REPORT.md documenting decisions. |

**Severity Definitions**:
- **P0 (Critical)**: BLOCKS production - MUST fix immediately (none identified)
- **P1 (High)**: Major quality gap - MUST fix before release (external API breakage)
- **P2 (Medium)**: Important but not blocking (dynamic calls, test helpers)
- **P3 (Low)**: Nice-to-have (documentation)

### Rollback Plan

**If Something Goes Wrong**:

1. **Git Revert**: Each batch is a separate commit - easy to rollback
   ```bash
   # Rollback specific batch
   git revert <commit-hash>

   # Rollback entire PR
   git revert <range-of-commits>
   ```

2. **Data Migration**: N/A - no data changes, only code removal

3. **Service Impact**: No runtime impact - code removal happens at compile time

4. **Recovery Steps**:
   - If tests fail after removing a function: Restore function from git, investigate why tests call it (may be test helper)
   - If Xref still shows unused warnings: Return to Phase 3, verify inventory completeness
   - If compilation fails: Check for syntax errors in modified files, restore from git if needed

**Rollback Triggers**:
- Test failures after removing function
- External API breakage (client reports undefined function errors)
- Compilation errors in modified files

---

## References

- **Research**: `/Users/sac/erlmcp/.wreckit/items/021-remove-27-unused-local-functions-dead-code/research.md`
- **CLAUDE.md**: `/Users/sac/erlmcp/CLAUDE.md` (project rules)
- **TCPS**: `/Users/sac/erlmcp/docs/tcps/TCPS.md` (manufacturing principles)
- **OTP Patterns**: `/Users/sac/erlmcp/docs/otp-patterns.md`
- **Item 020**: `/Users/sac/erlmcp/.wreckit/items/020-fix-250-xref-warnings-undefined-calls-unused-funct/plan.md` (Xref warnings research)
- **CODE_QUALITY_REPORT_V2.1.md**: `/Users/sac/erlmcp/CODE_QUALITY_REPORT_V2.1.md` (current quality status)
- **DIALYZER_REPORT.md**: `/Users/sac/erlmcp/docs/DIALYZER_REPORT.md` (unused function strategies)
- **rebar.config**: `/Users/sac/erlmcp/rebar.config` (Xref configuration, xref_ignores)

## Appendix: Decision Matrix

**Decision Matrix for Unused Functions**:

| If Function Is... | And... | Then... | Action |
|-------------------|--------|---------|--------|
| Exported | Has callers in apps/ or examples/ | Public API | DOCUMENT + add to xref_ignores |
| Exported | No callers found | Candidate public API OR obsolete export | DOCUMENT intention OR REMOVE if not needed |
| Not exported | Called by tests only | Test helper | MOVE to test/*_tests.erl |
| Not exported | Called dynamically (apply/spawn) | Dynamic call site | SUPPRESS with -compile directive |
| Not exported | No callers anywhere | Dead code | REMOVE |
| Marked @private | No callers | Dead code | REMOVE |
| Marked @private | Has callers | Should be exported | EXPORT + DOCUMENT |

**Examples**:

1. **erlmcp_security_headers:add_to_response/1**:
   - Exported? NO (not in -export list)
   - Callers? NONE
   - Decision: REMOVE (dead code)

2. **erlmcp_server:create_audio_content/3**:
   - Exported? NO
   - Callers? NONE
   - Marked? Already suppressed with -compile
   - Decision: SUPPRESS (future feature - add roadmap comment)

3. **erlmcp_transport_sse:format_retry_field/1**:
   - Exported? NO
   - Marked? @private
   - Callers? UNKNOWN (need to verify)
   - Decision: VERIFY → REMOVE if unused, or SUPPRESS if needed for Gap #29

4. **tcps_work_order:atom_to_binary/1**:
   - Exported? NO
   - Callers? NONE (duplicate wrapper)
   - Decision: REMOVE (unnecessary abstraction)

---

## Sign-off

**Manufacturing Plan Created**: 2025-01-29
**TCPS Compliance**: Lean Six Sigma Level Strictness
**Zero Defects**: 99.99966% defect-free delivery (3.4 defects per million)

**Principles Applied**:
- ✅ **Standard Work** (標準作業): Every step documented, measurable, verifiable
- ✅ **Andon** (行灯): Progress visible (inventory, decisions, metrics), problems signaled (quality gates)
- ✅ **Heijunka** (平準化): Work broken into small phases (≤4 hours each), independently verifiable
- ✅ **Poka-yoke** (ポカヨケ): Quality gates built into process (Xref, tests, compilation, Dialyzer)
- ✅ **Jidoka** (自働化): Failures stop production - if ANY gate fails, STOP and fix

**Quality Gates Defined**:
- Compilation: 0 errors
- EUnit: 100% pass rate
- Common Test: 100% pass rate
- Coverage: ≥80%
- Dialyzer: 0 warnings
- Xref: 0 unused function warnings (PRIMARY GOAL)

**Next Step**: Execute Phase 1 (Inventory) - Run Xref analysis and create inventory of all 26-28 unused functions.
