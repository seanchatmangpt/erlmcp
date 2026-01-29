# Planning Phase - TCPS Manufacturing Standards

You are tasked with creating a **Toyota Code Production System (TCPS)** manufacturing plan. Apply **Lean Six Sigma** principles: **99.99966% defect-free delivery**.

## TCPS Manufacturing Principles

**MANDATORY QUALITY STANDARDS:**

1. **Standard Work (標準作業)** - Documented, repeatable process
   - Every step is documented
   - Every step is measurable
   - Every step has verification

2. **Andon (行灯)** - Visual management
   - Progress is visible at all times
   - Problems are signaled immediately
   - Status is clear (raw → researched → planned → implementing → in_pr → done)

3. **Heijunka (平準化)** - Production leveling
   - Work is broken into SMALL, testable phases
   - Each phase is independently verifiable
   - No big-bang changes

4. **Poka-yoke (ポカヨケ)** - Mistake-proofing
   - Quality gates are built into the process
   - Failures stop the line immediately
   - No "skip this gate" - ALL gates must pass

## Item Details
- **ID:** {{id}}
- **Title:** {{title}}
- **Section:** {{section}}
- **Overview:** {{overview}}
- **Branch:** {{branch_name}}
- **Base Branch:** {{base_branch}}
- **Working Directory:** {{item_path}}
- **Language:** Erlang/OTP 25+
- **Build:** rebar3
- **Test:** EUnit, Common Test, Proper

## Research Summary
{{research}}

## Planning Process

### Step 1: Validate Understanding (MUST DO FIRST)

**VERIFY the research findings:**
1. Read the source code files mentioned in research
2. Cross-reference requirements with ACTUAL code
3. Identify discrepancies immediately
4. DO NOT proceed until understanding is VERIFIED

**CONFIRM scope:**
1. What is explicitly IN scope? (specific modules, functions, gates)
2. What is explicitly OUT of scope? (prevent scope creep)
3. What are the dependencies on other work? (blocking items)

**NO OPEN QUESTIONS:**
- If you find an open question, RESEARCH it immediately
- DO NOT write the plan with unresolved questions
- EVERY decision must be made before finalizing

### Step 2: Design the Solution

**Evaluate multiple approaches:**
- Consider 2-3 implementation options
- Weigh pros/cons of each
- Select the approach that BEST fits existing OTP patterns
- Document WHY this approach was chosen

**Break down into SMALL, TESTABLE phases:**
- Each phase should be ≤4 hours of work
- Each phase should be independently verifiable
- Each phase should have automated quality gates
- Order phases to minimize risk (dependencies first)

**Apply TCPS phase structure:**
1. **Specification** - What are we building?
2. **Pseudocode** - How will it work? (algorithm design)
3. **Architecture** - How does it integrate? (dependencies, supervision)
4. **Refinement** - Chicago School TDD (tests FIRST)
5. **Completion** - All quality gates passing

### Step 3: Create User Stories with Acceptance Criteria

**For EACH discrete piece of work:**
- Clear, specific acceptance criteria (testable)
- Priority (P0=Critical, P1=High, P2=Medium, P3=Low)
- Estimated complexity (hours)
- Dependencies on other stories

**Acceptance Criteria Format:**
- MUST be measurable (pass/fail)
- MUST include quality gates (compile, test, coverage)
- MUST include file paths (what to create/modify)
- MUST include verification commands (rebar3 commands)

## Output

Create TWO files at `{{item_path}}`:

### 1. plan.md - Detailed Manufacturing Plan

```markdown
# {{title}} Manufacturing Plan

**TCPS Compliance**: Lean Six Sigma Level Strictness
**Zero Defects**: 99.99966% defect-free delivery (3.4 defects per million)

## Manufacturing Objective
[Brief description of what we're building and WHY]

### Quality Gate Requirements
- **Compilation**: 0 errors (MANDATORY)
- **EUnit**: 100% pass rate (MANDATORY)
- **Common Test**: 100% pass rate (if applicable)
- **Coverage**: ≥80% (MANDATORY)
- **Dialyzer**: 0 warnings (MANDATORY)
- **Xref**: 0 undefined function calls (MANDATORY)
- **Performance**: <10% regression from baseline (if applicable)

## Current State

### What Exists Now
- **Modules**: [List with file:line references]
- **Tests**: [Coverage %, test counts]
- **Quality**: [Current gate status with specific failures]

### What's Missing
- **Gap**: [Quantified gap - coverage %, test counts]
- **Root Cause**: [From 5 Whys analysis]
- **Impact**: [What this blocks - production, integration, etc.]

### Key Discoveries from Research
- [Finding 1 with file:line reference]
- [Finding 2 - OTP pattern to follow]
- [Finding 3 - Constraint to work within]

## Desired End State

### Specification
[Exact specification of what we're building - module names, function signatures, behavior]

### Verification
[How to verify this meets requirements - automated tests, manual checks, metrics]

### Manufacturing Output
- **Code**: [What .erl files are created/modified]
- **Tests**: [What _tests.erl files are created]
- **Documentation**: [What .md files are updated]
- **Receipts**: [What quality receipts are generated]

## What We're NOT Doing
[Explicitly list OUT OF SCOPE items to prevent scope creep]
- [Item 1] - [Reason it's out of scope]
- [Item 2] - [Reason it's out of scope]

## Manufacturing Approach

### TCPS Methodology
Following Toyota Code Production System phases:

1. **Specification** - Requirements with acceptance criteria
2. **Pseudocode** - Algorithm design BEFORE coding
3. **Architecture** - Integration points and supervision tree
4. **Refinement** - Chicago School TDD (tests FIRST)
5. **Completion** - All quality gates passing

### Implementation Strategy
[High-level approach with reasoning WHY this strategy]

### Quality Integration
- **Pre-commit Hooks**: [What hooks enforce quality]
- **CI Gates**: [What CI checks must pass]
- **Receipt Generation**: [What receipts are created]
- **Andon Signaling**: [How failures are visible]

---

## Phases

### Phase 1: [Descriptive Name - ≤4 hours]

#### Overview
[What this phase accomplishes - specific, measurable outcome]

#### Specification
[WHAT we're building - exact module/function names]

#### Pseudocode
[HOW it will work - algorithm design, BEFORE coding]

#### Architecture
[INTEGRATION - supervision tree, dependencies, process structure]

#### Changes Required:

##### 1. [Component/File Group]
**File**: `path/to/module.erl`
**Current**: [What exists now - file:line]
**Changes**: [Specific changes - function signatures, logic]
**Reason**: [WHY this change is needed]

```erlang
%% BEFORE (existing code)
-module(example).
-export([function_name/1]).

function_name(Arg) ->
    existing_logic.

%% AFTER (proposed code)
-module(example).
-export([function_name/1]).

-spec function_name(integer()) -> integer().
function_name(Arg) when is_integer(Arg) ->
    new_logic_with_type_spec.
```

##### 2. [Another Component]
**File**: `path/to/test_module_tests.erl`
**Current**: [Does not exist OR existing coverage]
**Changes**: [Test functions to add]
**Reason**: [Achieve ≥80% coverage]

```erlang
-module(example_tests).
-include_lib("eunit/include/eunit.hrl").

%% Test fixture
example_setup() ->
    {ok, Pid} = example:start_link(),
    Pid.

example_cleanup(Pid) ->
    gen_server:stop(Pid).

%% Tests
function_name_test_() ->
    {setup,
     fun example_setup/0,
     fun example_cleanup/1,
     fun(_) -> [
         ?_test(begin
                   % Test normal case
                   ?assertEqual(Expected, example:function_name(Input))
               end),
         ?_test(begin
                   % Test error case
                   ?assertError(badarg, example:function_name(bad_input))
               end)
     ]end}.
```

#### Success Criteria:

##### Automated Verification (MUST ALL PASS):
- [ ] Compilation: `TERM=dumb rebar3 compile` - 0 errors, 0 warnings
- [ ] EUnit: `rebar3 eunit --module=module_tests` - 100% pass rate
- [ ] Coverage: `rebar3 cover` - ≥80% coverage
- [ ] Dialyzer: `rebar3 dialyzer` - 0 warnings
- [ ] Xref: `rebar3 xref` - 0 undefined function calls
- [ ] Pre-commit hook: `.claude/hooks/pre-task-validate.sh` - PASS

##### Manual Verification:
- [ ] Code review: OTP patterns followed correctly
- [ ] Integration: Works with existing modules
- [ ] Edge cases: Error paths tested
- [ ] Performance: No regression >10%

**Note**: Complete ALL automated verification BEFORE marking phase done. If ANY gate fails, STOP and fix. This is Jidoka (built-in quality).

---

### Phase 2: [Descriptive Name - ≤4 hours]
[Same structure as Phase 1]

---

### Phase 3: [Descriptive Name - ≤4 hours]
[Same structure as Phase 1]

---

## Testing Strategy

### Chicago School TDD (MANDATORY)
- **NO MOCKS** - Use real processes, real gen_servers
- **State-Based Verification** - Check #state{} record contents
- **Integration Testing** - Test with real dependencies
- **Race Condition Testing** - Concurrent operations

### Unit Tests (EUnit)
- **What to Test**: [All public functions, all error paths]
- **Test Pattern**: [Reference test file:line]
- **Coverage Target**: ≥80% per module
- **Pass Rate**: 100% (all tests must pass)

### Integration Tests (Common Test)
- **End-to-End Scenarios**: [Specific workflows]
- **Multi-Process**: [Concurrent operations]
- **Failure Scenarios**: [Crashes, timeouts, errors]

### Manual Testing Steps
1. [Specific verification step]
2. [Another verification step]
3. [Edge case verification]

### Quality Gates
Every phase MUST pass:
1. **Compilation**: `TERM=dumb rebar3 compile`
2. **EUnit**: `rebar3 eunit`
3. **Coverage**: `rebar3 cover` (verify ≥80%)
4. **Dialyzer**: `rebar3 dialyzer`
5. **Xref**: `rebar3 xref`

## Manufacturing Checklist

### Before Implementation
- [ ] Research verified (read actual source code)
- [ ] Scope confirmed (IN/OUT documented)
- [ ] No open questions (all decisions made)
- [ ] Phases broken down (≤4 hours each)
- [ ] Acceptance criteria defined (measurable, specific)

### During Implementation
- [ ] Chicago School TDD followed (tests FIRST)
- [ ] OTP patterns followed (gen_server, supervisor)
- [ ] Type specs added (Dialyzer clean)
- [ ] Error handling complete (all paths)
- [ ] Quality gates passing (compilation, tests, coverage)

### After Implementation
- [ ] All tests passing (100% rate)
- [ ] Coverage ≥80% (verified)
- [ ] Dialyzer 0 warnings (verified)
- [ ] Xref 0 undefined calls (verified)
- [ ] Performance no regression >10% (verified)
- [ ] Documentation updated (README, API docs)
- [ ] Code review complete (OTP patterns verified)

## Risk Management

### Known Risks
| Risk | Severity | Probability | Mitigation |
|------|----------|-------------|------------|
| [Risk 1] | P0/P1/P2/P3 | High/Med/Low | [Mitigation] |
| [Risk 2] | P0/P1/P2/P3 | High/Med/Low | [Mitigation] |

### Rollback Plan
[How to rollback if something goes wrong]
- Git revert: [Specific commit to revert to]
- Data migration: [How to handle data rollback]
- Service impact: [What users experience]

## References
- Research: `{{item_path}}/research.md`
- CLAUDE.md: `/Users/sac/erlmcp/CLAUDE.md` (project rules)
- TCPS: `/Users/sac/erlmcp/docs/tcps/TCPS.md` (manufacturing principles)
- OTP Patterns: `/Users/sac/erlmcp/docs/otp-patterns.md`
- Test Reference: [Reference test file with line numbers]
```

### 2. PRD - Structured User Stories

Call the `save_prd` MCP tool with the PRD data. The PRD schema:

```json
{
  "schema_version": 1,
  "id": "{{id}}",
  "branch_name": "{{branch_name}}",
  "user_stories": [
    {
      "id": "US-001",
      "title": "[Short, specific title]",
      "acceptance_criteria": [
        "[Specific, measurable criterion 1]",
        "[Specific, measurable criterion 2]",
        "[Quality gate: compilation, tests, coverage, etc.]"
      ],
      "priority": 1,
      "status": "pending",
      "notes": "[Implementation notes or empty string]"
    }
  ]
}
```

**Acceptance Criteria MUST:**
- Be measurable (pass/fail)
- Include file paths (what to create/modify)
- Include quality gates (compile, test, coverage commands)
- Be specific (not "implement X" but "create function X in module Y at path Z")

## TCPS Planning Guidelines

**MANDATORY QUALITY STANDARDS:**

1. **Be SPECIFIC:**
   - Exact module names, function signatures
   - Exact file paths (apps/*/src/*.erl)
   - Exact quality gate commands (rebar3 compile, etc.)

2. **Be MEASURABLE:**
   - Every criterion has pass/fail verification
   - Quantified thresholds (≥80% coverage, 100% pass rate)
   - Specific test counts (≥20 tests, etc.)

3. **Be THOROUGH:**
   - Include ALL error paths
   - Include ALL edge cases
   - Include ALL quality gates
   - No "TODO" or "implement this later"

4. **Apply TCPS PRINCIPLES:**
   - **Standard Work**: Every step documented
   - **Heijunka**: Small phases (≤4 hours each)
   - **Poka-yoke**: Quality gates built into every phase
   - **Andon**: Progress visible, failures signaled

5. **Focus on ZERO DEFECTS:**
   - Every phase must pass ALL quality gates
   - No "skip this gate" - ALL gates mandatory
   - Failures stop production (Jidoka)
   - No "good enough" - perfect or nothing

## Story Prioritization

**TCPS Priority Levels:**
- **Priority 1 (P0 - CRITICAL)**: Blocks production, MUST fix immediately
- **Priority 2 (P1 - HIGH)**: Major quality gap, MUST fix before release
- **Priority 3 (P2 - MEDIUM)**: Important but not blocking
- **Priority 4 (P3 - LOW)**: Nice-to-have, can defer

**Execution Order:**
1. All Priority 1 stories (in dependency order)
2. All Priority 2 stories (in dependency order)
3. Priority 3 stories (if time permits)
4. Priority 4 stories (defer to future)

## Common Patterns

### For Gen Server Modules:
1. Define callback specifications ( -spec )
2. Implement init/1 with state initialization
3. Implement handle_call/3 for synchronous API
4. Implement handle_cast/2 for async messages
5. Implement handle_info/2 for timeouts/signals
6. Implement terminate/2 for cleanup
7. Add type specs for all public functions
8. Test ALL callbacks (including error paths)

### For Test Modules:
1. Create test fixtures (setup/cleanup)
2. Test normal operation paths
3. Test error paths (bad arguments, crashes)
4. Test concurrent operations (race conditions)
5. Verify state changes (not just return values)
6. Use Chicago School TDD (real processes, no mocks)

### For Quality Gate Fixes:
1. Identify ROOT CAUSE (not symptoms)
2. Fix at source (don't suppress warnings)
3. Add regression test (prevent future occurrences)
4. Verify ALL gates pass (compile, test, coverage, dialyzer, xref)

## Completion

When you have:
1. Created `{{item_path}}/plan.md` with the EXACT structure above
2. Called `save_prd` tool with PRD data (user stories with measurable acceptance criteria)
3. Verified NO open questions (all decisions made)
4. Verified phases are ≤4 hours each (small, testable)
5. Verified ALL quality gates defined (compilation, tests, coverage, dialyzer, xref)

Output the following signal:
{{completion_signal}}

**REMEMBER**: This is manufacturing. We ship ZERO DEFECTS. Plan thoroughly, measure everything, document every step.
