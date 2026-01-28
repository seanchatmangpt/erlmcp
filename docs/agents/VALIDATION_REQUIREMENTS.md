# Validation Requirements

**Version:** 1.0.0
**Status:** MANDATORY
**Applies to:** All erlmcp agents and contributors

## Purpose

Define non-negotiable quality standards and validation requirements for erlmcp development.

---

## Core Philosophy

**Zero-Defect Delivery:** Code is not "done" until it's production-ready.

**Production-Ready Defined:**
- Compiles without errors (0 compilation errors)
- Passes all tests (≥90% pass rate, strict: 100%)
- Meets coverage targets (≥80% code coverage)
- Type-safe (0 Dialyzer errors)
- Cross-reference clean (0 undefined calls)
- Performance validated (≤10% regression)
- Integrates correctly (end-to-end flows work)

**Culture:** We ship quality, not just features. Validation is not overhead, it's the definition of done.

---

## Quality Gates (Mandatory)

### Gate 1: Compilation
**Requirement:** `rebar3 compile` exits with status 0

**Acceptance Criteria:**
- 0 compilation errors (hard requirement)
- Warnings documented (if unavoidable)
- All modules produce BEAM files
- No syntax errors, no missing includes

**Validation Command:**
```bash
TERM=dumb rebar3 compile
echo "Exit status: $?"
```

**Expected Output:**
```
Compiling src/module1.erl
Compiling src/module2.erl
...
Exit status: 0
```

**Failure Response:**
If compilation fails, work is NOT complete. Fix errors before proceeding.

---

### Gate 2: Unit Testing
**Requirement:** All EUnit tests pass

**Acceptance Criteria:**
- Pass rate: ≥90% (strict mode: 100%)
- 0 test failures (hard requirement for merge)
- 0 crashes during test execution
- No skipped tests (without documented reason)

**Validation Command:**
```bash
rebar3 eunit
```

**Expected Output:**
```
  All X tests passed.
```

**Failure Response:**
If tests fail:
1. Do NOT report "done"
2. Fix failing tests
3. Re-run validation
4. Only report "done" after 100% pass

---

### Gate 3: Integration Testing
**Requirement:** Common Test suites pass (if applicable)

**Acceptance Criteria:**
- All CT suites execute successfully
- 0 test case failures
- Integration flows work end-to-end
- Example applications run correctly

**Validation Command:**
```bash
rebar3 ct
# Or specific suite
rebar3 ct --suite=test/integration_SUITE
```

**Expected Output:**
```
Testing integration.integration_SUITE: OK
All tests passed.
```

**Failure Response:**
Fix integration issues before reporting "done".

---

### Gate 4: Code Coverage
**Requirement:** ≥80% code coverage

**Acceptance Criteria:**
- Total coverage: ≥80% (minimum)
- Public API coverage: 100% (all exported functions tested)
- Critical paths: 100% (supervision, message handling, error paths)

**Validation Command:**
```bash
rebar3 do eunit, cover
# View coverage report
rebar3 cover --verbose
```

**Expected Output:**
```
Total coverage: 85%
  module1: 90%
  module2: 82%
  module3: 88%
```

**Failure Response:**
If coverage <80%:
1. Add tests for uncovered code paths
2. Focus on public APIs and error handling
3. Re-measure until ≥80%

---

### Gate 5: Type Checking (Dialyzer)
**Requirement:** 0 Dialyzer errors

**Acceptance Criteria:**
- 0 type errors (hard requirement)
- 0 contract violations
- 0 unreachable code warnings
- All public functions have -spec declarations

**Validation Command:**
```bash
rebar3 dialyzer
```

**Expected Output:**
```
Checking modules...
No warnings.
```

**Failure Response:**
If Dialyzer reports errors:
1. Add missing type specs
2. Fix contract violations
3. Remove unreachable code
4. Re-run until clean

---

### Gate 6: Cross-Reference Analysis
**Requirement:** 0 undefined function calls

**Acceptance Criteria:**
- 0 undefined function calls
- 0 unused exports (or documented)
- 0 unused functions (or documented as helpers)

**Validation Command:**
```bash
rebar3 xref
```

**Expected Output:**
```
Cross-reference analysis complete.
No undefined calls.
```

**Failure Response:**
If xref reports undefined calls:
1. Add missing dependencies
2. Fix incorrect function names
3. Remove dead code
4. Re-run until clean

---

### Gate 7: Code Formatting
**Requirement:** Code follows rebar3_format standard

**Acceptance Criteria:**
- All code formatted consistently
- 100-char line length respected
- No manual formatting deviations

**Validation Command:**
```bash
rebar3 format --verify
```

**Expected Output:**
```
All files formatted correctly.
```

**Failure Response:**
If formatting check fails:
```bash
rebar3 format
git add .
# Commit formatted code
```

---

### Gate 8: Performance (if applicable)
**Requirement:** No regression >10% from baseline

**Acceptance Criteria:**
- Throughput: ≤10% decrease (or documented)
- Latency p95: ≤10% increase (or documented)
- Memory: ≤20% increase (or documented)
- New features benchmarked

**Validation Command:**
```bash
# Run relevant benchmark
erl -pa _build/default/lib/*/ebin -noshell -eval \
  "erlmcp_bench_core_ops:run(<<\"core_ops_100k\">>), init:stop()."
```

**Expected Output:**
```
Benchmark: core_ops_100k
Throughput: 2.69M msg/s (baseline: 2.65M msg/s) [+1.5%]
Latency p95: 450 μs (baseline: 445 μs) [+1.1%]
Status: PASSED (no regression)
```

**Failure Response:**
If regression >10%:
1. Profile to find bottleneck
2. Optimize hot path
3. Re-benchmark
4. Document if performance tradeoff justified

---

### Gate 9: Integration (Examples)
**Requirement:** Example applications work

**Acceptance Criteria:**
- All examples compile
- Example flows execute correctly
- README instructions accurate

**Validation Command:**
```bash
# For each example
cd examples/calculator
rebar3 compile
rebar3 shell
# Test example flows manually or with script
```

**Expected Output:**
```
Example: calculator
  ✓ Compiles
  ✓ Server starts
  ✓ Tool calls work
  ✓ Cleanup works
```

**Failure Response:**
Fix broken examples before reporting "done".

---

## Agent-Specific Requirements

### All Agents (1-19)
**MUST:**
- Self-validate phases 1-4 (Implementation → Quality Gates) before reporting to Agent 20
- Run local tests: `rebar3 compile && rebar3 eunit`
- Check Dialyzer: `rebar3 dialyzer` (if types added/changed)
- Report blocking issues immediately (don't hide failures)
- NEVER report "done" without self-validation

**MUST NOT:**
- Skip validation "to save time"
- Report "done" with failing tests
- Ignore Dialyzer errors as "minor"
- Assume "manual testing is enough"

---

### Agent 20: Final Validator
**MUST:**
- Run full validation pipeline: `./tools/agent-validator.sh --full`
- Validate ALL 6 phases (no exceptions)
- Generate completion report
- Report "done" ONLY after all gates pass
- If any gate fails: spawn fix agents, re-validate

**MUST NOT:**
- Report "done" with any validation failures
- Skip phases (all 6 are mandatory)
- Trust agent self-reports without verification
- Allow "good enough" quality

---

## Validation Workflow

### Step 1: Agent Self-Validation (Agents 1-19)
```bash
# Agent runs locally before reporting to Agent 20
TERM=dumb rebar3 compile               # Gate 1
rebar3 eunit                           # Gate 2
rebar3 dialyzer                        # Gate 5 (if types changed)

# If all pass, report to Agent 20:
"Implementation complete. Self-validation passed (phases 1-4)."
```

### Step 2: Agent 20 Full Validation
```bash
# Agent 20 runs comprehensive validation
./tools/agent-validator.sh --full

# Output: Completion report with all phase statuses
# If all pass → Report "DONE" to user
# If any fail → Spawn fix agents, repeat
```

### Step 3: Pre-Merge Validation (CI)
```bash
# Automated in CI pipeline
rebar3 do compile, eunit, ct, dialyzer, xref, cover
./tools/agent-validator.sh --full

# Merge blocked if validation fails
```

---

## Quality Targets

| Metric | Minimum | Target | Strict |
|--------|---------|--------|--------|
| **Compilation** | 0 errors | 0 errors | 0 errors |
| **Test Pass Rate** | 90% | 95% | 100% |
| **Code Coverage** | 80% | 85% | 90% |
| **Dialyzer Errors** | 0 | 0 | 0 |
| **Xref Undefined** | 0 | 0 | 0 |
| **Performance Regression** | <10% | <5% | 0% |
| **First-Pass Validation** | 60% | 80% | 90% |

**First-Pass Validation:** Percentage of time Agent 20 validation passes without needing re-work.

---

## Failure Handling Protocol

### Scenario 1: Compilation Fails
```
Agent: "Implementation complete."
Agent 20: Runs validation → Compilation fails
Action:
  1. Agent 20 does NOT report "done"
  2. Spawns Agent 3 (erlang-otp-developer) to fix compilation
  3. Agent 3 fixes errors
  4. Agent 20 re-runs validation
  5. If pass → Report "done"
```

### Scenario 2: Tests Fail
```
Agent: "Test suite complete."
Agent 20: Runs validation → 8/10 tests pass (80%)
Action:
  1. Agent 20 does NOT report "done" (below 90% threshold)
  2. Spawns Agent 5 (erlang-test-engineer) to fix failing tests
  3. Agent 5 debugs and fixes tests
  4. Agent 20 re-runs validation
  5. If 10/10 pass → Report "done"
```

### Scenario 3: Performance Regression
```
Agent: "Optimization complete."
Agent 20: Runs validation → 15% throughput regression
Action:
  1. Agent 20 does NOT report "done" (>10% regression)
  2. Spawns Agent 6 (erlang-performance) to investigate
  3. Agent 6 profiles and fixes bottleneck
  4. Agent 20 re-runs benchmark
  5. If <10% regression → Report "done"
```

---

## Documentation Requirements

### Code Documentation
**MUST:**
- All modules have `-module()` and `-moduledoc()` attributes
- All exported functions have `-spec` declarations
- Complex logic has inline comments explaining "why"
- Public API documented with examples

**Example:**
```erlang
-module(erlmcp_cache).
-moduledoc """
In-memory cache using ETS for fast lookups.
Supports TTL-based expiration and size limits.
""".

-export([start_link/1, get/2, put/3]).

-spec start_link(Options :: map()) -> {ok, pid()} | {error, term()}.
%% @doc Start cache server with given options.
%% Options:
%%   - max_size: Maximum number of entries (default: 10000)
%%   - ttl_ms: Time-to-live in milliseconds (default: 60000)
start_link(Options) ->
    gen_server:start_link(?MODULE, Options, []).
```

### Technical Documentation
**MUST:**
- Architecture documented: docs/architecture.md
- API reference complete: docs/api-reference.md
- OTP patterns explained: docs/otp-patterns.md
- Examples working: examples/*/README.md

---

## Metrics & Monitoring

### Track Agent Performance
```bash
# Agent validation success rate
./tools/agent-validator.sh --full
# Record: timestamp, agent, phase_statuses, pass/fail

# Aggregate over time:
# - First-pass validation rate (target: 80%)
# - Average re-validation cycles (target: <2)
# - Time to validation (target: <5 min)
```

### Quality Dashboard (Future)
- Real-time validation status
- Test pass rate trends
- Coverage trends
- Performance regression alerts
- Agent effectiveness metrics

---

## Training & Enforcement

### New Agent Onboarding
1. Read COMPLETION_PROTOCOL.md
2. Read VALIDATION_REQUIREMENTS.md (this doc)
3. Read AGENT_TASK_TEMPLATES.md
4. Review tools/agent-validator.sh
5. Complete practice task with full validation

### Protocol Violations
**Violation:** Agent reports "done" with failing validation

**Response:**
1. Immediate halt (do not merge)
2. Agent re-training (review protocol)
3. Fix failures
4. Re-validate until pass
5. Document lesson learned

**Repeat Violations:**
- Agent redesign required
- Supervisor intervention
- Process improvement

---

## Continuous Improvement (Kaizen)

### After Each Project
1. Review validation failures
2. Identify root causes (5 Whys analysis)
3. Update protocols if needed
4. Share learnings with team
5. Measure improvement

### Quality Metrics Review
- Monthly: Review pass rates, coverage, regression trends
- Quarterly: Update targets based on capability
- Annually: Major protocol revisions

---

## Exceptions & Edge Cases

### When to Skip Phases
**Phase 5 (Performance):** Can skip if:
- Changes are documentation-only
- Changes are test-only (no src/ modifications)
- Changes don't touch hot paths

**Document skip reason:**
```
Phase 5: SKIPPED (documentation-only change, no performance impact)
```

### Acceptable Warnings
**Dialyzer warnings:** Some warnings acceptable if:
- External library types incomplete (document upstream issue)
- Intentional dynamic behavior (document justification)

**Xref warnings:** Unused exports acceptable if:
- API functions not yet used (document roadmap)
- Test helpers (mark with -ifdef(TEST))

---

## Tools & Automation

### Available Tools
- `tools/agent-validator.sh` - Full validation pipeline
- `rebar3 compile` - Compilation gate
- `rebar3 eunit` - Unit testing gate
- `rebar3 ct` - Integration testing gate
- `rebar3 dialyzer` - Type checking gate
- `rebar3 xref` - Cross-reference gate
- `rebar3 cover` - Coverage measurement
- `rebar3 format` - Code formatting

### CI Integration
```yaml
# .github/workflows/ci.yml (example)
- name: Validate
  run: |
    ./tools/agent-validator.sh --full
    if [ $? -ne 0 ]; then
      echo "Validation failed"
      exit 1
    fi
```

---

## Summary

**Validation is NOT optional. It defines "done".**

**Agent Responsibilities:**
- Agents 1-19: Self-validate phases 1-4
- Agent 20: Validate ALL phases 1-6 before reporting "done"

**Quality Targets:**
- Compilation: 0 errors
- Tests: ≥90% pass (strict: 100%)
- Coverage: ≥80%
- Dialyzer: 0 errors
- Xref: 0 undefined calls
- Performance: <10% regression

**Culture:**
- Pride in zero-defect delivery
- Validation is craft, not overhead
- "Done" means "production-ready"
- Continuous improvement (Kaizen)

**Enforcement:**
- Agent 20 is mandatory gatekeeper
- CI blocks merges on validation failure
- Protocol violations trigger re-training
- Quality metrics tracked and reviewed

---

## References

- **Completion Protocol:** docs/agents/COMPLETION_PROTOCOL.md
- **Task Templates:** docs/agents/AGENT_TASK_TEMPLATES.md
- **Validator Script:** tools/agent-validator.sh
- **OTP Patterns:** docs/otp-patterns.md
- **Architecture:** docs/architecture.md

---

**VERSION:** 1.0.0
**LAST UPDATED:** 2026-01-28
**STATUS:** ACTIVE - MANDATORY FOR ALL AGENTS
