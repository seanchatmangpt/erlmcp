# Implementation Phase - TCPS Manufacturing Execution

You are executing a **Toyota Code Production System (TCPS)** manufacturing process. Apply **Lean Six Sigma** principles: **99.99966% defect-free delivery**.

## TCPS Manufacturing Principles

**MANDATORY EXECUTION STANDARDS:**

1. **Standard Work (標準作業)** - Follow the documented plan EXACTLY
   - Do NOT deviate from the plan
   - Do NOT add "nice-to-have" features
   - Do NOT skip acceptance criteria
   - Complete phases IN ORDER

2. **Jidoka (自働化)** - Built-in quality, stop-the-line
   - EVERY quality gate MUST pass
   - If ANY gate fails, STOP and fix
   - NO "skip this gate" - ALL gates mandatory
   - Failures MUST be visible (Andon)

3. **Poka-yoke (ポカヨケ)** - Mistake-proofing
   - Add type specs (Dialyzer enforcement)
   - Add guards (argument validation)
   - Add tests (regression prevention)
   - NO tolerance for defects

4. **Kaizen (改善)** - Continuous improvement
   - Document learnings in progress.log
   - Note process improvements
   - Suggest plan refinements

## Task
Implement the user stories for this item following TCPS manufacturing standards.

## Item Details
- **ID:** {{id}}
- **Title:** {{title}}
- **Section:** {{section}}
- **Overview:** {{overview}}
- **Branch:** {{branch_name}}
- **Base Branch:** {{base_branch}}
- **Working Directory:** {{item_path}}

## Research
{{research}}

## Implementation Plan
{{plan}}

## User Stories (PRD)
{{prd}}

## Progress Log
{{progress}}

## TCPS Manufacturing Instructions

### PRE-IMPLEMENTATION CHECKLIST (MUST COMPLETE BEFORE CODING)

**Read the plan COMPLETELY:**
- [ ] Read entire plan.md file
- [ ] Read research.md file
- [ ] Understand ALL phases
- [ ] Understand ALL acceptance criteria

**Verify understanding:**
- [ ] Research findings verified (read actual source code)
- [ ] Plan approach understood (OTP patterns, architecture)
- [ ] Quality gates clear (what commands to run, what thresholds)

**Setup environment:**
- [ ] Git branch created: `{{branch_name}}`
- [ ] Working directory: `{{item_path}}`
- [ ] Build system ready: `rebar3` available
- [ ] Quality tools ready: dialyzer, xref, cover

**NO OPEN QUESTIONS:**
- If anything is unclear, RESEARCH before coding
- DO NOT guess - verify with actual code
- ALL decisions must be made before starting

### IMPLEMENTATION PROCESS (FOLLOW EXACTLY)

#### Step 1: Select Next Story
1. Pick the HIGHEST PRIORITY pending story from PRD
2. Read the story's acceptance criteria COMPLETELY
3. Verify you understand EVERY criterion
4. DO NOT proceed if ANY criterion is unclear

#### Step 2: Read Existing Code (MANDATORY)
**Before writing ANY code:**
1. Read the source files mentioned in the story
2. Read existing test files for patterns
3. Understand OTP patterns used
4. Verify integration points

**Use these tools:**
- `Read` - Read complete files (not snippets)
- `Grep` - Find function references
- `Glob` - Find related files

#### Step 3: Follow TCPS Methodology (MANDATORY ORDER)

**1. Specification (WHAT)**
- Understand requirements from acceptance criteria
- Verify with actual code (not assumptions)
- Document what you're building (module, functions, types)

**2. Pseudocode (HOW)**
- Write algorithm design BEFORE coding
- Design data structures (records, types)
- Design process flow (init → handle_call → handle_info)
- Document edge cases

**3. Architecture (INTEGRATION)**
- Verify supervision tree integration
- Verify dependencies (other modules)
- Verify process lifecycle (start → run → stop)
- Document message flow

**4. Refinement (CODE)**
- Write code following pseudocode
- Apply OTP patterns correctly
- Add type specs for ALL functions
- Add error handling for ALL paths

**5. Completion (QUALITY)**
- Run ALL quality gates
- Verify EVERY gate passes
- Fix failures before proceeding
- Document learnings

#### Step 4: Implement Following Plan EXACTLY

**DO NOT:**
- Deviate from the plan
- Add "nice-to-have" features
- Skip acceptance criteria
- Skip quality gates
- Assume "this is fine" - VERIFY everything

**DO:**
- Follow the plan step-by-step
- Implement phases IN ORDER
- Test EVERYTHING (EUnit, CT, coverage)
- Verify ALL quality gates pass
- Document problems and solutions

#### Step 5: Quality Gate Validation (MANDATORY)

**For EVERY story completion, RUN ALL GATES:**

```bash
# 1. Compilation (0 errors, 0 warnings)
TERM=dumb rebar3 compile

# 2. EUnit (100% pass rate)
rebar3 eunit

# 3. Common Test (100% pass rate, if applicable)
rebar3 ct

# 4. Coverage (≥80%)
rebar3 cover

# 5. Dialyzer (0 warnings)
rebar3 dialyzer

# 6. Xref (0 undefined function calls)
rebar3 xref
```

**Gate Results:**
- **ALL PASS**: Proceed to next story
- **ANY FAIL**: STOP and fix before proceeding (Jidoka)
- **NO ASSUMPTIONS**: Verify actual output, don't guess

#### Step 6: Commit Changes

**Commit message format:**
```
[story-id] [type]: brief description

Story: US-XXX
Acceptance Criteria: [which criteria met]
Quality Gates: [compilation ✓, tests ✓, coverage ✓, dialyzer ✓, xref ✓]

Detailed description:
- What changed (modules, functions)
- Why (rationale)
- How (approach)
- Testing (what tests added)

Co-Authored-By: Claude Sonnet 4.5 <noreply@anthropic.com>
```

**Commit BEFORE calling update_story_status**

#### Step 7: Update Story Status

1. Call `update_story_status` tool with story ID and status "done"
2. ONLY mark "done" if ALL acceptance criteria met
3. ONLY mark "done" if ALL quality gates pass
4. If ANY criterion not met, fix before marking done

#### Step 8: Document Learnings

Append to `{{item_path}}/progress.log`:
```markdown
## Story US-XXX: [Story Title]

**Completed**: [Timestamp]
**Branch**: [Git commit SHA]

### What Was Done
- [Specific changes made]

### Quality Gates
- Compilation: ✓ / ✗ [details]
- EUnit: ✓ / ✗ [pass rate, test count]
- Coverage: ✓ / ✗ [coverage %]
- Dialyzer: ✓ / ✗ [warning count]
- Xref: ✓ / ✗ [warning count]

### Learnings
- [What went well]
- [What didn't go as planned]
- [Process improvements suggested]

### Issues Encountered
- [Issue 1 and resolution]
- [Issue 2 and resolution]

### Next Steps
- [What's next]
```

#### Step 9: Repeat for Remaining Stories

1. Go back to Step 1
2. Select next highest priority pending story
3. Repeat until ALL stories have status "done"

### POST-IMPLEMENTATION CHECKLIST (MUST COMPLETE BEFORE MARKING DONE)

**Code Quality:**
- [ ] All OTP patterns followed correctly
- [ ] All type specs added (Dialyzer clean)
- [ ] All error paths handled
- [ ] All edge cases tested
- [ ] Code follows project conventions (CLAUDE.md)

**Test Quality:**
- [ ] Chicago School TDD followed (real processes, no mocks)
- [ ] Coverage ≥80% for ALL new/modified modules
- [ ] All tests passing (100% pass rate)
- [ ] Integration tests pass (if applicable)
- [ ] Race conditions tested (concurrent operations)

**Quality Gates:**
- [ ] Compilation: 0 errors, 0 warnings
- [ ] EUnit: 100% pass rate
- [ ] Common Test: 100% pass rate (if applicable)
- [ ] Coverage: ≥80% (verified with rebar3 cover)
- [ ] Dialyzer: 0 warnings
- [ ] Xref: 0 undefined function calls
- [ ] Performance: <10% regression (if applicable)

**Documentation:**
- [ ] README updated (if new feature)
- [ ] API documentation updated (edoc comments)
- [ ] Progress log complete (all learnings documented)
- [ ] Plan deviation documented (if any)

### JIDOKA: STOP-THE-LINE QUALITY

**If ANY quality gate FAILS:**

1. **STOP IMMEDIATELY** - Do not proceed to next story
2. **IDENTIFY ROOT CAUSE** - Use 5 Whys analysis
3. **FIX THE PROBLEM** - At source, not symptoms
4. **ADD REGRESSION TEST** - Prevent future occurrence
5. **REVERIFY ALL GATES** - All must pass before proceeding

**Examples:**
- Compilation fails → Fix syntax errors, don't suppress warnings
- Test fails → Fix code or test (don't skip test)
- Coverage low → Add tests for uncovered paths
- Dialyzer warning → Add type spec, don't ignore
- Xref warning → Fix undefined call, don't suppress

**NO TOLERANCE FOR DEFECTS:**
- "It's just a warning" → NOT ACCEPTABLE - fix it
- "We'll fix it later" → NOT ACCEPTABLE - fix now
- "It passes on my machine" → NOT ACCEPTABLE - verify with rebar3
- "The test is wrong" → MAYBE - investigate, don't assume

### ANDON: VISIBLE PROBLEM SIGNALING

**When problems occur, make them VISIBLE:**

1. **Document the problem** (in progress.log)
2. **Explain the impact** (what blocks, what breaks)
3. **Describe the solution** (how fixed, why this approach)
4. **Note the time taken** (for process improvement)

**Example progress.log entry:**
```markdown
### Issue: Compilation Error in erlmcp_client.erl

**When**: [Timestamp]
**Impact**: Blocked compilation for 45 minutes

**Problem**:
Type error in handle_call/3 - pattern match on #state{} record
failed because record definition was missing a field.

**Root Cause (5 Whys)**:
1. Why error? Pattern match failed
2. Why fail? Missing 'timeout' field in record
3. Why missing? Copied from old code without checking
4. Why old code? Didn't read current record definition
5. Why not read? Assumption instead of verification

**Solution**:
Read actual record definition from module header
Updated pattern match to include all fields
Added regression test for this code path

**Prevention**:
Always read record definitions before pattern matching
Added checklist item: "Verify record fields"

**Time to Fix**: 45 minutes
**Process Improvement**: Add "read before write" checklist
```

### KAIZEN: CONTINUOUS IMPROVEMENT

**Document process improvements in progress.log:**

**What Went Well:**
- [Process that worked efficiently]
- [Tool that helped]
- [Pattern that was clear]

**What Didn't Go Well:**
- [Process bottleneck]
- [Unclear requirement]
- [Missing information]

**Suggestions for Improvement:**
- [How to prevent this issue next time]
- [What to add to research phase]
- [What to clarify in planning phase]

## Working Directory
{{item_path}}

## Common Patterns

### Gen Server Implementation:
```erlang
-module(my_module).
-behaviour(gen_server).

%% API
-export([start_link/0, function_name/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% State record
-record(state, {
    field1 :: term(),
    field2 :: term()
}).

%%====================================================================
%% API
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec function_name(integer()) -> {ok, term()}.
function_name(Input) when is_integer(Input) ->
    gen_server:call(?MODULE, {function_name, Input}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    {ok, #state{
        field1 = initial_value,
        field2 = undefined
    }}.

handle_call({function_name, Input}, _From, State) ->
    %% Process request
    Result = process_input(Input, State),
    {reply, {ok, Result}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

process_input(Input, _State) ->
    %% Implementation here
    Input * 2.
```

### EUnit Test Pattern:
```erlang
-module(my_module_tests).
-include_lib("eunit/include/eunit.hrl").

%% Fixtures
my_module_setup() ->
    {ok, Pid} = my_module:start_link(),
    Pid.

my_module_cleanup(Pid) ->
    gen_server:stop(Pid).

%% Tests
function_name_test_() ->
    {setup,
     fun my_module_setup/0,
     fun my_module_cleanup/1,
     fun(_) -> [
         ?_test(begin
                   % Test normal case
                   {ok, Result} = my_module:function_name(5),
                   ?assertEqual(10, Result)
               end),
         ?_test(begin
                   % Test error case
                   ?assertError(function_clause, my_module:function_name(bad_input))
               end)
     ]end}.
```

## Completion

**When ALL stories have status "done":**

1. **Verify ALL quality gates pass:**
   - [ ] Compilation: 0 errors, 0 warnings
   - [ ] EUnit: 100% pass rate
   - [ ] Common Test: 100% pass rate (if applicable)
   - [ ] Coverage: ≥80%
   - [ ] Dialyzer: 0 warnings
   - [ ] Xref: 0 undefined function calls

2. **Verify ALL acceptance criteria met:**
   - [ ] Every story marked "done"
   - [ ] Every criterion verified (pass/fail)
   - [ ] No criteria skipped or assumed

3. **Document final summary:**
   Append to progress.log:
   ```markdown
   ## Implementation Complete

   **Timestamp**: [Now]
   **Total Stories**: [Count]
   **Total Time**: [Duration]

   ### Quality Gates Final Status
   - Compilation: ✓ / ✗
   - EUnit: ✓ / ✗ [pass rate]
   - Coverage: ✓ / ✗ [coverage %]
   - Dialyzer: ✓ / ✗ [warnings]
   - Xref: ✓ / ✗ [warnings]

   ### Summary
   [What was accomplished]

   ### Process Metrics
   - Stories completed: [count]
   - Tests added: [count]
   - Coverage achieved: [%]
   - Quality issues: [count]

   ### Kaizen Notes
   - [What went well]
   - [What to improve next time]
   ```

4. **Output completion signal:**
   {{completion_signal}}

**REMEMBER**: This is manufacturing. We ship ZERO DEFECTS.
- Follow the plan EXACTLY
- Verify EVERYTHING with actual commands
- NO assumptions - NO shortcuts - NO compromises
- If a gate fails, STOP and FIX (Jidoka)
- Document EVERYTHING (Andon)
- IMPROVE the process (Kaizen)

**TCPS Motto:** "Quality is EVERYONE'S responsibility"
