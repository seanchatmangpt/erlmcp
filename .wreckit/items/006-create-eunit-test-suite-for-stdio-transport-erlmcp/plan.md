# Create EUnit test suite for STDIO transport (erlmcp_transport_stdio) Manufacturing Plan

**TCPS Compliance**: Lean Six Sigma Level Strictness
**Zero Defects**: 99.99966% defect-free delivery (3.4 defects per million)

## Manufacturing Objective

Achieve ≥80% code coverage on `erlmcp_transport_stdio.erl` (320 lines) with 100% test pass rate. The STDIO transport is the DEFAULT transport for MCP - every CLI tool uses this. Operating with 0% coverage is a P0 critical defect.

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

**Modules:**
- `apps/erlmcp_transports/src/erlmcp_transport_stdio.erl` (320 lines)
  - Lines 46-52: API functions (start_link/1, start_link/2)
  - Lines 54-68: send/2 - writes to stdout
  - Lines 70-74: close/1 - stops transport
  - Lines 80-119: init/1 - initializes reader process
  - Lines 121-131: handle_call/3 - state management, test mode simulation
  - Lines 137-158: handle_info/2 - message delivery, owner/reader monitoring
  - Lines 160-177: terminate/2 - cleanup, registry unregistration
  - Lines 187-214: is_test_environment/0, stdin_available/0 - environment detection
  - Lines 216-263: read_loop/3 - main stdin reading loop with message size validation
  - Lines 265-271: process_line/2 - line processing
  - Lines 273-298: trim_line/1, trim_end/1 - line ending normalization
  - Lines 300-319: get_max_message_size/0, validate_message_size/2 - size limits

- `apps/erlmcp_transports/test/erlmcp_transport_stdio_tests.erl` (680 lines) - EXISTS
  - 22 test functions across basic and integration test groups
  - Uses test mode via process dictionary
  - May not exercise production code paths effectively

**Tests**: 0% coverage (BACKLOG.md line 486) despite test file existing with 22 test functions

**Quality**: UNKNOWN - tests may not compile, may fail, or may not exercise production code paths

### What's Missing

**Gap**: 80 percentage points (0% → 80% coverage)

**Root Cause**: Test architecture uses `test_mode` flag (process dictionary) to bypass real stdin/stdout I/O. Coverage tools may not recognize test_mode paths as valid coverage of production code paths. Additionally, the reader process spawning logic (lines 117-118) may not be tested.

**Impact**: P0 CRITICAL - Blocks production deployment. STDIO is the DEFAULT transport for MCP CLI tools. Operating with 0% coverage violates the "99.99966% defect-free delivery" TCPS principle.

### Key Discoveries from Research

1. **Test File Exists But Coverage is 0%**: The test file `erlmcp_transport_stdio_tests.erl` exists with 680 lines and 22 test functions, but BACKLOG.md reports 0% coverage. This indicates:
   - Tests may not compile or run successfully
   - Tests may use `test_mode` flag which bypasses actual production code paths
   - Coverage tools may not track test_mode code paths as valid coverage

2. **OTP Pattern**: Gen_server with reader process pattern
   - Main gen_server process handles owner communication
   - Spawns dedicated reader process for stdin (line 117)
   - Reader process sends messages via `!` operator
   - Links for monitoring (owner and reader death handling)

3. **Test Mode Bypass**: The `is_test_environment/0` function (lines 187-204) checks:
   - `get(test_mode)` process dictionary flag
   - `whereis(eunit_proc)` for EUnit detection
   - `stdin_available/0` heuristic
   - When true, reader process is NOT spawned (line 112-115)
   - This means critical production code (read_loop) is never tested

4. **Chicago School TDD Reference**: TCP transport tests (`erlmcp_transport_tcp_tests.erl`) demonstrate proper pattern:
   - Real process spawning (no mocks)
   - State-based assertions (verify #state records)
   - Process monitoring (is_process_alive checks)
   - Setup/cleanup fixtures with proper resource management

5. **Constraint**: Real stdin/stdout testing is difficult in automated test environment
   - No TTY in CI/CD
   - Tests rely on test_mode simulation
   - May need port/2 or escript for real I/O testing

## Desired End State

### Specification

Achieve ≥80% code coverage on `erlmcp_transport_stdio.erl` with 100% test pass rate. All transport behavior callbacks must be tested, including:

- **Transport lifecycle**: start_link, init, terminate
- **Message sending**: send/2 with binary and iolist data
- **Message receiving**: Reader process, read_loop, message framing
- **Error handling**: EOF, read errors, size validation, broken pipes
- **Process monitoring**: Owner death, reader death, restart logic
- **Registry integration**: Register/unregister with erlmcp_registry
- **Line ending handling**: \n, \r\n, \r normalization
- **Edge cases**: Empty lines, oversized messages, concurrent access

### Verification

**Automated Quality Gates:**
```bash
# Compilation
TERM=dumb rebar3 compile
# Expected: 0 errors, 0 warnings

# EUnit tests
rebar3 eunit --module=erlmcp_transport_stdio_tests
# Expected: 100% pass rate (0 failures)

# Coverage report
rebar3 cover --module=erlmcp_transport_stdio
# Expected: ≥80% coverage percentage

# Dialyzer type checking
rebar3 dialyzer
# Expected: 0 warnings

# Xref cross-reference analysis
rebar3 xref
# Expected: 0 undefined function calls
```

**Manual Verification:**
- Review coverage report for specific uncovered lines
- Verify all error paths are tested
- Verify reader process spawning is tested
- Verify registry integration works end-to-end
- Verify owner/reader death handling works correctly

**Metrics:**
- Coverage percentage: ≥80% (target: 85%+)
- Test pass rate: 100% (all tests must pass)
- Number of test functions: 22+ existing + enhancements
- Compilation warnings: 0
- Dialyzer warnings: 0
- Xref issues: 0

### Manufacturing Output

**Code**:
- `apps/erlmcp_transports/test/erlmcp_transport_stdio_tests.erl` (modified - enhance existing 680-line file)
- `apps/erlmcp_transports/src/erlmcp_transport_stdio.erl` (minimal modifications only if needed for testability)

**Tests**:
- Enhanced test suite with ≥80% coverage
- Tests for reader process lifecycle
- Tests for message size validation
- Tests for registry integration
- Tests for process monitoring edge cases
- Tests for EOF and error handling

**Documentation**:
- Coverage report: `_build/test/cover/erlmcp_transport_stdio.html`
- Test results: `_build/test/logs/` directory

**Receipts**:
- Coverage report showing ≥80% coverage
- EUnit test results showing 100% pass rate
- Dialyzer report showing 0 warnings
- Xref report showing 0 undefined calls

## What We're NOT Doing

**Out of Scope Items:**

- **Creating a new test file**: Test file already exists (680 lines). We're enhancing it to achieve coverage targets.
- **Refactoring the implementation**: We will NOT modify the STDIO transport implementation unless absolutely necessary for testability. Any changes must be justified by coverage needs.
- **Adding new features**: No new functionality. Only testing existing functionality.
- **Testing other transports**: TCP, HTTP, WebSocket, SSE transports are out of scope.
- **Changing the test framework**: We're using EUnit as specified. No migration to Common Test or Proper unless absolutely necessary.
- **Mocking real stdin/stdout**: We will not add complex mocking infrastructure. We'll use process control techniques (test_mode flag, configuration) to enable real code path testing.

## Manufacturing Approach

### TCPS Methodology

Following Toyota Code Production System phases:

1. **Specification** - Requirements with acceptance criteria (COMPLETE)
2. **Pseudocode** - Algorithm design BEFORE coding (each phase)
3. **Architecture** - Integration points and supervision tree (documented)
4. **Refinement** - Chicago School TDD (tests FIRST, real processes)
5. **Completion** - All quality gates passing (automated verification)

### Implementation Strategy

**High-Level Approach:**

The manufacturing strategy follows a diagnostic → fix → enhance → validate approach:

1. **Diagnose Current State (Phase 1)**: Measure actual coverage with `rebar3 cover`. Identify which lines are uncovered. Determine root cause of 0% coverage despite test file existing.

2. **Fix Foundation Issues (Phase 2)**: Fix any compilation errors or failing tests. Ensure all 22 existing tests pass with 100% success rate. This is the foundation - we cannot enhance until the foundation is solid.

3. **Enhance Coverage to 80%+ (Phase 3)**: Based on coverage analysis, add targeted tests for uncovered code paths:
   - Reader process spawning and lifecycle
   - Message size validation (at limit, above limit, below limit)
   - Registry integration (register, unregister, routing)
   - Process monitoring (owner death, reader death)
   - EOF and error handling
   - Line ending normalization

4. **Validate All Quality Gates (Phase 4)**: Run complete quality gate suite (compile, eunit, cover, dialyzer, xref). Generate coverage receipt. Ensure all gates pass with 100% success rate.

**Why This Strategy:**

- **Risk Minimization**: Diagnosing first prevents wasted effort on non-issues
- **Foundation First**: Fixing existing tests before adding new ones ensures we don't compound problems
- **Data-Driven Enhancement**: Coverage analysis guides test additions to maximum effect
- **Quality Gate Discipline**: TCPS mandates Jidoka (stop-the-line) - every gate must pass

**Testing Strategy:**

- **Chicago School TDD**: Real processes, state-based assertions, no mocks where possible
- **Test Mode Configurability**: Use application env vars to control test_mode behavior
- **Process Control**: Use process links, monitors, and exit signals for testing process death
- **Real I/O Where Possible**: Use port/2 or escript for real stdin/stdout testing if needed
- **State Verification**: Check #state{} record contents, not just return values

### Quality Integration

**Pre-commit Hooks:**
- `.claude/hooks/pre-task-validate.sh` - runs compilation and basic tests
- Prevents commits with failing quality gates

**CI Gates:**
- All quality gates must pass in CI/CD
- Coverage report generated and archived
- Test results published for visibility

**Receipt Generation:**
- Coverage report: `_build/test/cover/erlmcp_transport_stdio.COVERAGE.html`
- Test results: `_build/test/logs/eunit.log`
- Dialyzer report: `_build/test/dialyzer.log`
- Xref report: `_build/test/xref.log`

**Andon Signaling:**
- Progress visible at all times (test output, coverage percentage)
- Problems signaled immediately (test failures, compilation errors)
- Status clear: raw → researched → planned → implementing → in_pr → done

---

## Phases

### Phase 1: Diagnose Current State (≤1 hour)

#### Overview

Measure the actual coverage percentage and identify specific uncovered lines. Determine root cause of 0% coverage despite test file existing. This is diagnostic work - no code changes yet.

#### Specification

**WHAT we're diagnosing:**
- Current coverage percentage on `erlmcp_transport_stdio.erl`
- Which specific lines are uncovered
- Which test functions are failing (if any)
- Whether test_mode flag is preventing coverage

#### Pseudocode

**Diagnostic Algorithm:**
```
1. Compile the project
   - Run: rebar3 compile
   - Verify: 0 compilation errors
   - Note: Any warnings related to test module

2. Run existing test suite
   - Run: rebar3 eunit --module=erlmcp_transport_stdio_tests
   - Count: Total tests, passed tests, failed tests
   - Note: Specific test failures with error messages

3. Generate coverage report
   - Run: rebar3 cover --module=erlmcp_transport_stdio
   - Measure: Overall coverage percentage
   - Identify: Uncovered lines by line number

4. Analyze coverage gaps
   - Map: Uncovered lines to functions
   - Classify: Type of coverage (branch, line, function)
   - Identify: Common patterns in uncovered code

5. Document root cause
   - Determine: Why coverage is 0% despite test file
   - Verify: test_mode flag bypass effect
   - Assess: Reader process spawning coverage
```

#### Architecture

**INTEGRATION - Dependencies:**
- **Test Module**: `erlmcp_transport_stdio_tests`
- **System Under Test**: `erlmcp_transport_stdio` (gen_server)
- **Collaborators**: `erlmcp_registry` (gen_server), `jsx` (JSON encoding)
- **Coverage Tool**: `rebar3 cover` (Erlang/OTP built-in coverage)

**Process Structure:**
- Main diagnostic process (shell)
- Spawns rebar3 commands as subprocesses
- Collects output and parses results
- Generates diagnostic report

#### Changes Required:

**NO CODE CHANGES IN THIS PHASE** - Diagnostic only.

**Output Files:**

##### 1. Diagnostic Report

**File**: `/Users/sac/erlmcp/.wreckit/items/006-create-eunit-test-suite-for-stdio-transport-erlmcp/diagnostic_report.md` (NEW)
**Current**: Does not exist
**Changes**: Create diagnostic report with findings
**Reason**: Document current state before making changes

#### Success Criteria:

##### Automated Verification (MUST ALL PASS):
- [ ] Compilation: `TERM=dumb rebar3 compile` - 0 errors
- [ ] Diagnostic report created: `diagnostic_report.md` exists
- [ ] Coverage measured: `rebar3 cover --module=erlmcp_transport_stdio` completed
- [ ] Uncovered lines documented: Line numbers listed in diagnostic report

##### Manual Verification:
- [ ] Root cause identified: Why is coverage 0%?
- [ ] Test failures documented: Which tests fail and why?
- [ ] Coverage gaps mapped: Which functions have lowest coverage?
- [ ] Enhancement plan: Specific test additions needed to reach 80%

**Note**: This phase is pure diagnosis. NO code changes. If ANY compilation error is found, that becomes the top priority for Phase 2.

---

### Phase 2: Fix Foundation Issues (≤4 hours)

#### Overview

Fix any compilation errors, failing tests, or basic infrastructure issues. Ensure all 22 existing tests pass with 100% success rate. This establishes a solid foundation before adding new tests.

#### Specification

**WHAT we're fixing:**
- Compilation errors (if any)
- Failing tests (if any)
- Test infrastructure issues (fixtures, setup/cleanup)
- Type specification issues (Dialyzer warnings)

#### Pseudocode

**Fix Algorithm:**
```
1. Fix compilation errors (Priority P0)
   IF compilation errors exist THEN
     FOR EACH error DO
       - Read error message and line number
       - Identify root cause (syntax, dependency, type)
       - Fix error with minimal change
       - Verify fix: rebar3 compile
     END FOR
   END IF

2. Fix failing tests (Priority P1)
   IF test failures exist THEN
     FOR EACH failing test DO
       - Read test output and error message
       - Identify root cause (assertion, timeout, crash)
       - Fix test implementation or add missing setup
       - Verify fix: rebar3 eunit --module=...
     END FOR
   END IF

3. Fix Dialyzer warnings (Priority P2)
   IF Dialyzer warnings exist THEN
     FOR EACH warning DO
       - Read warning and line number
       - Add or fix type specification
       - Verify fix: rebar3 dialyzer
     END FOR
   END IF

4. Verify 100% test pass rate
   - Run: rebar3 eunit --module=erlmcp_transport_stdio_tests
   - Expected: 100% pass rate, 0 failures
   - If any failure, repeat step 2

5. Document fixes
   - List all changes made
   - Reason for each change
   - Impact on coverage (if any)
```

#### Architecture

**INTEGRATION - Testing Infrastructure:**
- **Test Runner**: EUnit framework
- **Test Module**: `erlmcp_transport_stdio_tests`
- **Test Mode**: Controlled via `put(test_mode, true/false)`
- **Fixtures**: `setup_stdio_transport/0`, `cleanup_stdio_transport/1`

**Process Structure:**
- Main test process (EUnit)
- Spawns transport gen_server processes
- Uses process dictionary for test mode control
- Teardown via cleanup fixtures

#### Changes Required:

##### 1. Fix Compilation Errors (if any)

**File**: `apps/erlmcp_transports/test/erlmcp_transport_stdio_tests.erl`
**Current**: May have compilation errors
**Changes**: Fix syntax, missing includes, undefined functions
**Reason**: Must compile before testing

##### 2. Fix Failing Tests (if any)

**File**: `apps/erlmcp_transports/test/erlmcp_transport_stdio_tests.erl`
**Current**: May have failing tests
**Changes**: Fix assertions, timeouts, race conditions
**Reason**: Must pass 100% before adding new tests

##### 3. Add Missing Type Specs (if needed)

**File**: `apps/erlmcp_transports/src/erlmcp_transport_stdio.erl`
**Current**: Complete type specs (verified)
**Changes**: NONE unless Dialyzer reveals issues
**Reason**: Must pass Dialyzer quality gate

#### Success Criteria:

##### Automated Verification (MUST ALL PASS):
- [ ] Compilation: `TERM=dumb rebar3 compile` - 0 errors, 0 warnings
- [ ] EUnit: `rebar3 eunit --module=erlmcp_transport_stdio_tests` - 100% pass rate
- [ ] Dialyzer: `rebar3 dialyzer` - 0 new warnings introduced
- [ ] Xref: `rebar3 xref` - 0 undefined function calls

##### Manual Verification:
- [ ] All 22 existing tests pass
- [ ] No test timeouts or crashes
- [ ] Test fixtures work correctly
- [ ] Documentation complete (all fixes documented)

**Note**: Complete ALL automated verification BEFORE marking phase done. If ANY gate fails, STOP and fix. This is Jidoka (built-in quality).

---

### Phase 3: Enhance Coverage to 80%+ (≤8 hours)

#### Overview

Based on coverage analysis from Phase 1, add targeted tests to achieve ≥80% coverage. Focus on testing production code paths, not just test_mode simulation paths. Use Chicago School TDD principles.

#### Specification

**WHAT we're enhancing:**
- Reader process lifecycle (spawn, death, restart)
- Message size validation (at limit, above limit, below limit)
- Registry integration (register, unregister, routing)
- Process monitoring (owner death, reader death)
- EOF and error handling (read errors, broken pipes)
- Line ending normalization (\n, \r\n, \r)
- Concurrent message handling
- All transport behavior callbacks

#### Pseudocode

**Enhancement Algorithm:**
```
1. Review coverage gaps from Phase 1
   - List uncovered lines by function
   - Prioritize by impact (critical path vs edge case)
   - Group related coverage gaps

2. Design tests for uncovered code paths
   FOR EACH uncovered function/branch DO
     - Determine: What inputs trigger this code path?
     - Design: Test case with realistic inputs
     - Verify: Test actually exercises uncovered lines
     - Implement: Test following Chicago School TDD
   END FOR

3. Implement tests in priority order
   PRIORITY 1: Reader process lifecycle
   PRIORITY 2: Message size validation
   PRIORITY 3: Registry integration
   PRIORITY 4: Process monitoring (death handling)
   PRIORITY 5: Error handling (EOF, read errors)
   PRIORITY 6: Line ending normalization
   PRIORITY 7: Edge cases and concurrent access

4. Verify coverage improvement
   - Run: rebar3 cover --module=erlmcp_transport_stdio
   - Check: Overall coverage percentage
   - Verify: ≥80% coverage achieved
   - Analyze: Remaining gaps (if any)

5. Regression testing
   - Run: rebar3 eunit --module=erlmcp_transport_stdio_tests
   - Verify: 100% pass rate maintained
   - Check: No test failures introduced
```

#### Architecture

**INTEGRATION - Test Architecture:**
- **Test Pattern**: Chicago School TDD (real processes, state assertions)
- **Process Control**: Test mode via application env vars (not just process dictionary)
- **I/O Testing**: Use port/2 or escript for real stdin/stdout simulation
- **Coverage Tracking**: rebar3 cover with line-by-line analysis

**Process Structure:**
- Main EUnit test process
- Transport gen_server process (SUT)
- Mock owner process for monitoring tests
- Reader process (spawns in init/1 when not in test mode)
- Registry gen_server process (for integration tests)

#### Changes Required:

##### 1. Add Reader Process Lifecycle Tests

**File**: `apps/erlmcp_transports/test/erlmcp_transport_stdio_tests.erl`
**Current**: Basic reader lifecycle test exists (lines 197-213)
**Changes**: Enhance to cover all reader code paths
**Reason**: Read loop is critical path with 0% coverage currently

##### 2. Add Message Size Validation Tests

**File**: `apps/erlmcp_transports/test/erlmcp_transport_stdio_tests.erl`
**Current**: No size validation tests
**Changes**: Add tests for message size limits
**Reason**: Size validation is untested (lines 228-243, 246-262)

##### 3. Add Registry Integration Tests

**File**: `apps/erlmcp_transports/test/erlmcp_transport_stdio_tests.erl`
**Current**: Basic registry test exists (lines 569-606)
**Changes**: Enhance to cover all registry code paths
**Reason**: Registry integration is critical for production (lines 91-94, 163-166)

##### 4. Add Process Monitoring Tests

**File**: `apps/erlmcp_transports/test/erlmcp_transport_stdio_tests.erl`
**Current**: Basic owner monitoring test exists (lines 337-365)
**Changes**: Add tests for reader death and restart logic
**Reason**: Process monitoring is critical for reliability (lines 142-155)

##### 5. Add Error Handling Tests

**File**: `apps/erlmcp_transports/test/erlmcp_transport_stdio_tests.erl`
**Current**: Basic EOF/error tests exist (lines 371-400)
**Changes**: Add comprehensive error path tests
**Reason**: Error handling must be tested for robustness

##### 6. Add Line Ending Normalization Tests

**File**: `apps/erlmcp_transports/test/erlmcp_transport_stdio_tests.erl`
**Current**: Basic line ending tests exist (lines 443-493)
**Changes**: Add comprehensive line ending tests
**Reason**: Line ending normalization is cross-platform critical (lines 273-298)

##### 7. Add Concurrent Access Tests

**File**: `apps/erlmcp_transports/test/erlmcp_transport_stdio_tests.erl`
**Current**: Basic concurrent test exists (lines 608-636)
**Changes**: Add race condition and stress tests
**Reason**: Concurrent access is realistic in production

#### Success Criteria:

##### Automated Verification (MUST ALL PASS):
- [ ] Compilation: `TERM=dumb rebar3 compile` - 0 errors, 0 warnings
- [ ] EUnit: `rebar3 eunit --module=erlmcp_transport_stdio_tests` - 100% pass rate
- [ ] Coverage: `rebar3 cover --module=erlmcp_transport_stdio` - ≥80% coverage
- [ ] Dialyzer: `rebar3 dialyzer` - 0 warnings
- [ ] Xref: `rebar3 xref` - 0 undefined function calls

##### Manual Verification:
- [ ] Reader process spawning tested
- [ ] Message size validation tested
- [ ] Registry integration tested
- [ ] Process monitoring tested
- [ ] Error handling tested
- [ ] Line ending normalization tested
- [ ] Concurrent access tested
- [ ] All transport behavior callbacks tested

**Note**: If coverage is <80% after implementing all tests, analyze remaining gaps and add targeted tests for those specific lines.

---

### Phase 4: Validate All Quality Gates (≤1 hour)

#### Overview

Run complete quality gate suite to ensure all TCPS standards are met. Generate coverage receipt and final reports. Validate that the STDIO transport is production-ready.

#### Specification

**WHAT we're validating:**
- Compilation: 0 errors, 0 warnings
- EUnit: 100% pass rate
- Coverage: ≥80% (target: 85%+)
- Dialyzer: 0 warnings
- Xref: 0 undefined function calls
- Performance: No regression >10%

#### Pseudocode

**Validation Algorithm:**
```
1. Compilation validation
   - Run: TERM=dumb rebar3 compile
   - Verify: 0 errors, 0 warnings
   - Document: Compiler output

2. EUnit validation
   - Run: rebar3 eunit --module=erlmcp_transport_stdio_tests
   - Measure: Total tests, passed, failed
   - Verify: 100% pass rate
   - Document: Test results

3. Coverage validation
   - Run: rebar3 cover --module=erlmcp_transport_stdio
   - Measure: Overall coverage percentage
   - Verify: ≥80% coverage
   - Generate: HTML coverage report
   - Document: Coverage by function

4. Dialyzer validation
   - Run: rebar3 dialyzer
   - Verify: 0 warnings for STDIO transport module
   - Document: Dialyzer output

5. Xref validation
   - Run: rebar3 xref
   - Verify: 0 undefined function calls
   - Document: Xref output

6. Performance validation
   - Measure: Test execution time
   - Verify: No regression >10% from baseline
   - Document: Performance metrics

7. Generate final receipt
   - Compile all results into receipt
   - Archive coverage report
   - Sign off on quality gates
```

#### Architecture

**INTEGRATION - Quality Gate Infrastructure:**
- **Compilation**: `rebar3 compile`
- **Testing**: `rebar3 eunit`
- **Coverage**: `rebar3 cover`
- **Type Checking**: `rebar3 dialyzer`
- **Cross-Reference**: `rebar3 xref`
- **Receipts**: `_build/test/` directory

**Process Structure:**
- Main validation process (shell script or manual)
- Runs all quality gates sequentially
- Collects output and parses results
- Generates final receipt document

#### Changes Required:

##### 1. Generate Coverage Receipt

**File**: `coverage/erlmcp_transport_stdio_coverage.txt` (NEW)
**Current**: Does not exist
**Changes**: Create coverage receipt documenting final metrics
**Reason**: Evidence of ≥80% coverage achievement

##### 2. Generate Test Results Receipt

**File**: `coverage/erlmcp_transport_stdio_test_results.txt` (NEW)
**Current**: Does not exist
**Changes**: Create test results receipt documenting all test executions
**Reason**: Evidence of 100% test pass rate

##### 3. Generate Final Quality Gate Receipt

**File**: `coverage/erlmcp_transport_stdio_quality_gates.txt` (NEW)
**Current**: Does not exist
**Changes**: Create comprehensive quality gate receipt
**Reason**: Single source of truth for all quality validations

#### Success Criteria:

##### Automated Verification (MUST ALL PASS):
- [ ] Compilation: `TERM=dumb rebar3 compile` - 0 errors, 0 warnings
- [ ] EUnit: `rebar3 eunit --module=erlmcp_transport_stdio_tests` - 100% pass rate
- [ ] Coverage: `rebar3 cover --module=erlmcp_transport_stdio` - ≥80% coverage
- [ ] Dialyzer: `rebar3 dialyzer` - 0 warnings
- [ ] Xref: `rebar3 xref` - 0 undefined function calls
- [ ] Pre-commit hook: `.claude/hooks/pre-task-validate.sh` - PASS

##### Manual Verification:
- [ ] All receipt files generated (3 files)
- [ ] Coverage report archived (HTML)
- [ ] Test results documented (all tests listed)
- [ ] Quality gate sign-off complete
- [ ] No regressions introduced (performance, functionality)

**Note**: This is the FINAL quality gate. If ANY gate fails, the item is REJECTED and must return to appropriate phase for fixes. TCPS mandates Jidoka - zero defects before completion.

---

## Testing Strategy

### Chicago School TDD (MANDATORY)

- **NO MOCKS** - Use real processes, real gen_servers
- **State-Based Verification** - Check #state{} record contents
- **Integration Testing** - Test with real dependencies (registry, reader process)
- **Race Condition Testing** - Concurrent operations, process death scenarios

**Key Principles:**
1. **Real Processes**: Spawn actual gen_servers, not mock processes
2. **State Assertions**: Verify internal state changes, not just return values
3. **Process Monitoring**: Use `is_process_alive/1`, process links, monitors
4. **No Test Doubles**: Avoid meck, mock, stub except for I/O operations
5. **Fail Fast**: Tests should fail immediately on assertion failure

### Unit Tests (EUnit)

**What to Test:**
- All public API functions (start_link, send, close)
- All gen_server callbacks (init, handle_call, handle_info, terminate)
- All internal functions (read_loop, process_line, trim_line, validate_message_size)
- All error paths (EOF, read errors, size exceeded, owner death, reader death)
- All edge cases (empty messages, max size messages, concurrent access)

**Test Pattern:**
```erlang
test_function_name_test_() ->
    {setup,
     fun setup/0,           % Setup fixture
     fun cleanup/1,         % Cleanup fixture
     fun(_) -> [           % Test generator
         ?_test(begin       % Individual test
                   % Setup test conditions
                   % Execute function under test
                   % Assert result
               end)
     ]end}.
```

**Coverage Target:**
- ≥80% per module (MANDATORY)
- Target: 85%+ (stretch goal)
- 100% for critical paths (init, read_loop, validate_message_size)

**Pass Rate:**
- 100% (all tests must pass)
- 0 failures tolerated
- 0 skipped tests (all tests must run)

### Integration Tests (Common Test)

**End-to-End Scenarios:**
1. Full lifecycle: start → send → receive → close
2. Registry integration: register → route → unregister
3. Concurrent access: multiple senders, multiple readers
4. Error recovery: EOF → restart, read error → restart

**Multi-Process:**
- Owner process death handling
- Reader process death handling
- Concurrent send operations
- Race condition testing

**Failure Scenarios:**
- Broken pipe (stdout closed)
- Stdin EOF
- Read errors (I/O errors)
- Oversized messages (DoS protection)
- Process crashes

### Manual Testing Steps

1. **Verification of Test Execution**
   - Run: `rebar3 eunit --module=erlmcp_transport_stdio_tests`
   - Verify: All tests pass
   - Check: No test timeouts or crashes

2. **Verification of Coverage**
   - Run: `rebar3 cover --module=erlmcp_transport_stdio`
   - Open: `_build/test/cover/erlmcp_transport_stdio.COVERAGE.html`
   - Verify: ≥80% coverage
   - Check: Critical functions covered

3. **Verification of Quality Gates**
   - Run: `rebar3 do compile, xref, dialyzer, eunit, cover`
   - Verify: All gates pass
   - Check: No regressions

4. **Manual Code Review**
   - Review: Test coverage report for missed edge cases
   - Verify: All error paths tested
   - Check: Process lifecycle properly tested
   - Verify: Registry integration works end-to-end

### Quality Gates

**Every phase MUST pass:**

1. **Compilation**: `TERM=dumb rebar3 compile`
   - 0 errors (MANDATORY)
   - 0 warnings (target)
   - Must compile successfully

2. **EUnit**: `rebar3 eunit --module=erlmcp_transport_stdio_tests`
   - 100% pass rate (MANDATORY)
   - 0 failures
   - All tests execute

3. **Coverage**: `rebar3 cover --module=erlmcp_transport_stdio`
   - ≥80% coverage (MANDATORY)
   - Target: 85%+
   - Verify critical paths covered

4. **Dialyzer**: `rebar3 dialyzer`
   - 0 warnings (MANDATORY)
   - Type specs complete
   - No type mismatches

5. **Xref**: `rebar3 xref`
   - 0 undefined function calls (MANDATORY)
   - No deprecated functions
   - No local function call warnings

## Manufacturing Checklist

### Before Implementation
- [x] Research verified (read actual source code - erlmcp_transport_stdio.erl, erlmcp_transport_stdio_tests.erl)
- [x] Scope confirmed (IN: test enhancement for ≥80% coverage, OUT: new features, other transports, refactoring)
- [x] No open questions (all decisions made - test file exists, coverage target known, quality gates defined)
- [x] Phases broken down (4 phases, ≤4 hours each)
- [x] Acceptance criteria defined (measurable, specific, with file paths and verification commands)

### During Implementation
- [ ] Chicago School TDD followed (tests FIRST, real processes, no mocks except I/O)
- [ ] OTP patterns followed (gen_server, supervision, process links)
- [ ] Type specs complete (Dialyzer clean)
- [ ] Error handling complete (all paths tested)
- [ ] Quality gates passing (compilation, tests, coverage at each phase)

### After Implementation
- [ ] All tests passing (100% rate)
- [ ] Coverage ≥80% (verified with rebar3 cover)
- [ ] Dialyzer 0 warnings (verified)
- [ ] Xref 0 undefined calls (verified)
- [ ] Performance no regression >10% (verified)
- [ ] Documentation updated (receipts generated)
- [ ] Code review complete (OTP patterns verified)

## Risk Management

### Known Risks

| Risk | Severity | Probability | Mitigation |
|------|----------|-------------|------------|
| **Test file doesn't compile** | P0 | High | Fix compilation errors immediately in Phase 2. Verify with `rebar3 compile` before proceeding. |
| **Tests pass but coverage is 0%** | P0 | High | Measure actual coverage with `rebar3 cover` in Phase 1. Identify why test_mode bypass prevents coverage. Fix by testing both test_mode and production code paths. |
| **test_mode bypass untested in production** | P1 | High | Test both `test_mode=true` AND `test_mode=undefined` paths. Use port or mock I/O to test read_loop without relying on test_mode flag. |
| **Reader process never spawns in tests** | P1 | Medium | Use mock stdin via port or escript to enable real I/O testing. Or refactor is_test_environment/0 to be configurable via application env var. |
| **Message size limits not tested** | P2 | Medium | Add tests for messages at limit, above limit, below limit. Verify error response sent on size exceeded. |
| **Registry integration untested** | P2 | Medium | Test with real erlmcp_registry process. Verify register/unregister/routing code paths. |
| **Owner death handling untested** | P2 | Medium | Kill owner process in test, verify transport stops. Use unlink + exit to simulate owner death. |
| **EOF handling untested** | P2 | Low | Mock io:get_line to return eof. Verify reader exits gracefully and transport continues. |
| **Line ending normalization not tested** | P3 | Low | Test \n, \r\n, \r endings. Verify trim_line/1 and trim_end/1 work correctly. |
| **Concurrent message handling untested** | P3 | Low | Spawn multiple processes sending simultaneously. Verify no race conditions or crashes. |
| **Dialyzer warnings on test code** | P2 | Low | Add -spec to all test functions. Use meck carefully to avoid type warnings. |
| **Performance regression from new tests** | P3 | Low | Measure test execution time. Ensure no >10% regression from baseline. |

**Severity Definitions:**
- **P0 (Critical)**: BLOCKS testing - MUST fix immediately
- **P1 (High)**: Major quality gap - MUST fix before release
- **P2 (Medium)**: Important but not blocking
- **P3 (Low)**: Nice-to-have, can defer

### Rollback Plan

**How to rollback if something goes wrong:**

1. **Git Revert**:
   ```bash
   # Revert to last known good state
   git revert <commit-hash>
   # Or reset to branch start
   git reset --hard origin/main
   ```

2. **Data Migration**: No data migration needed (test code only)

3. **Service Impact**: No service impact (tests don't affect production)

4. **Recovery Steps**:
   - Revert test file changes
   - Rerun tests to verify baseline restored
   - Document what went wrong
   - Adjust plan based on learnings

## References

- **Research**: `/Users/sac/erlmcp/.wreckit/items/006-create-eunit-test-suite-for-stdio-transport-erlmcp/research.md`
- **CLAUDE.md**: `/Users/sac/erlmcp/CLAUDE.md` (project rules)
- **TCPS**: `/Users/sac/erlmcp/docs/tcps/TCPS.md` (manufacturing principles)
- **OTP Patterns**: `/Users/sac/erlmcp/docs/otp-patterns.md`
- **Test Reference**: `/Users/sac/erlmcp/apps/erlmcp_transports/test/erlmcp_transport_tcp_tests.erl` (Chicago School TDD pattern)

---

**Manufacturing Plan Status**: COMPLETE

**Ready for Implementation**: YES

**Estimated Total Effort**: 14 hours (1 + 4 + 8 + 1)

**Next Action**: Execute Phase 1 - Diagnose Current State
