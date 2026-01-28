# Agent Completion Protocol

**Version:** 1.0.0
**Status:** MANDATORY
**Applies to:** All agents in erlmcp development

## Purpose

Prevent premature "done" reporting by enforcing strict multi-phase validation gates.

## Core Principle

**NO AGENT may report "done" until ALL validation phases complete successfully.**

Reporting "done" with failing tests, compilation errors, or incomplete validation is a PROTOCOL VIOLATION.

## The Six Mandatory Phases

### Phase 1: Implementation
**Exit Criteria:**
- All requested code written
- All files created/modified
- Code follows OTP patterns (gen_server, supervision)
- Dialyzer specs present on public functions
- NO placeholder TODOs or incomplete functions

**Validation:**
```bash
# Check for incomplete code
grep -r "TODO\|FIXME\|XXX" src/
# Must return empty (or only documented technical debt)
```

**Blocking Behavior:** Cannot proceed to Phase 2 if implementation incomplete.

---

### Phase 2: Compilation
**Exit Criteria:**
- `TERM=dumb rebar3 compile` exits with status 0
- 0 compilation errors
- Warnings documented (if unavoidable)
- All BEAM files generated

**Validation:**
```bash
TERM=dumb rebar3 compile
echo "Exit status: $?"
```

**Output Format:**
```
✅ Phase 2: Compilation PASSED
   - Compiled: X modules
   - BEAM files: Y
   - Errors: 0
   - Warnings: 0 (or documented)
```

**Blocking Behavior:** Cannot proceed to Phase 3 if compilation fails.

---

### Phase 3: Testing
**Exit Criteria:**
- All unit tests pass: `rebar3 eunit`
- All CT suites pass: `rebar3 ct` (if applicable)
- Pass rate: ≥90% (strict: 100%)
- No skipped tests without justification
- Test output captured and reported

**Validation:**
```bash
# Unit tests
rebar3 eunit --module=<module>_tests
# Integration tests (if applicable)
rebar3 ct --suite=test/<suite>_SUITE
```

**Output Format:**
```
✅ Phase 3: Testing PASSED
   - Unit tests: X/X passed (100%)
   - CT suites: Y/Y passed (100%)
   - Skipped: 0
   - Failed: 0
```

**Blocking Behavior:** Cannot proceed to Phase 4 if tests fail or pass rate <90%.

---

### Phase 4: Quality Gates
**Exit Criteria:**
- Dialyzer: 0 type errors
- Xref: 0 undefined function calls
- Coverage: ≥80% (measure with `rebar3 cover`)
- Code formatted: `rebar3 format --verify`

**Validation:**
```bash
# Type checking
rebar3 dialyzer
# Cross-reference analysis
rebar3 xref
# Code coverage
rebar3 do eunit, cover
# Format check
rebar3 format --verify
```

**Output Format:**
```
✅ Phase 4: Quality Gates PASSED
   - Dialyzer: 0 errors
   - Xref: 0 undefined calls
   - Coverage: XX% (≥80%)
   - Format: verified
```

**Blocking Behavior:** Cannot proceed to Phase 5 if any gate fails.

---

### Phase 5: Performance Validation
**Exit Criteria (if performance-critical code changed):**
- Relevant benchmarks executed
- No regression >10% from baseline
- New features benchmarked (if applicable)
- Results documented

**Validation:**
```bash
# Quick benchmark (<2 min)
make benchmark-quick
# Or specific benchmark
erl -pa _build/default/lib/*/ebin -eval \
  "erlmcp_bench_core_ops:run(<<\"core_ops_100k\">>), init:stop()."
```

**Output Format:**
```
✅ Phase 5: Performance PASSED
   - Benchmark: core_ops_100k
   - Throughput: 2.69M msg/s (no regression)
   - Latency p95: XXX μs (+Y% acceptable)
```

**Blocking Behavior:** Cannot proceed to Phase 6 if regression >10% (without justification).

**Skip Condition:** If code changes are NOT performance-critical (docs, tests only), document skip reason.

---

### Phase 6: Integration & Reporting
**Exit Criteria:**
- End-to-end examples work (if applicable)
- No manual testing required
- All phases 1-5 green
- Comprehensive status report generated

**Validation:**
```bash
# Run example (if applicable)
cd examples/calculator && rebar3 shell
# Test example flow
```

**Output Format:**
```
✅ Phase 6: Integration PASSED
   - Example: calculator - working
   - MCP protocol: compliant
   - Ready for merge

━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
COMPLETION REPORT (Agent: <name>)
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
✅ Phase 1: Implementation - COMPLETE
✅ Phase 2: Compilation - 0 errors, X modules
✅ Phase 3: Testing - 100% pass rate (X/X tests)
✅ Phase 4: Quality Gates - 0 errors, XX% coverage
✅ Phase 5: Performance - 0% regression
✅ Phase 6: Integration - All examples working

STATUS: VALIDATED - READY FOR MERGE
```

**Blocking Behavior:** ONLY after this report may agent report "done".

---

## Protocol Violations

**VIOLATION 1: Premature "Done" Reporting**
- Agent reports "done" with failing tests
- **Action:** Halt, fix failures, re-validate ALL phases

**VIOLATION 2: Skipped Validation**
- Agent skips quality gates (e.g., "dialyzer takes too long")
- **Action:** Mandatory validation, no exceptions

**VIOLATION 3: Incomplete Implementation**
- Code has TODO/FIXME without completing
- **Action:** Complete implementation, re-validate Phase 1-6

**VIOLATION 4: Undocumented Failures**
- Tests fail but agent doesn't mention it
- **Action:** Transparent reporting required, fix failures

---

## Agent Responsibilities

### All Agents (1-19)
1. Execute assigned implementation
2. Self-validate phases 1-4 before handoff
3. Report blocking issues immediately
4. NEVER claim "done" without running validations

### Agent 20: Final Validator (Mandatory Gatekeeper)
1. Re-run ALL validations (phases 1-6)
2. Use `tools/agent-validator.sh` (automated)
3. If ANY phase fails: spawn fix agents, repeat
4. ONLY report "done" after complete validation
5. Generate comprehensive completion report

---

## Validation Automation

**Script:** `tools/agent-validator.sh`

**Usage:**
```bash
# Run full validation pipeline
./tools/agent-validator.sh --full

# Quick validation (skip benchmarks)
./tools/agent-validator.sh --quick

# Specific phase
./tools/agent-validator.sh --phase 3
```

**Exit Codes:**
- `0` - All phases passed, OK to report "done"
- `1` - Validation incomplete, DO NOT report "done"
- `2` - Blocking error, spawn fix agents

**Output:** Detailed status for each phase + actionable next steps.

---

## Task Template

**All agent tasks MUST include:**

```
Task: <description>
Agent: <agent-name>
Validation Required: YES (all 6 phases)

Instructions:
1. Implement <feature>
2. Follow completion protocol (docs/agents/COMPLETION_PROTOCOL.md)
3. Self-validate phases 1-4
4. Report status to Agent 20 for final validation
5. DO NOT report "done" until Agent 20 validates

Blocking Condition:
- Report "done" ONLY after Agent 20 validates ALL phases
- If validation fails, fix and re-validate (do not skip)

Output Format: Use Phase 6 completion report template
```

---

## Enforcement

**Pre-merge Requirement:**
- All PRs MUST include completion report
- CI runs `tools/agent-validator.sh --full`
- Merge blocked if validation fails

**Agent Training:**
- All agents review this protocol before first task
- Protocol violations result in re-training
- Successful validations reinforce correct behavior

---

## Metrics

**Track agent performance:**
- Time to complete each phase
- Number of validation failures (goal: 0)
- Protocol violations (goal: 0)
- Re-validation cycles (goal: <2)

**Quality Targets:**
- Compilation: 100% success (0 errors)
- Tests: ≥90% pass rate (strict: 100%)
- Coverage: ≥80%
- Performance: <10% regression
- First-pass validation: ≥80% (no re-work)

---

## Examples

### Example 1: Successful Flow
```
Agent 3 (erlang-otp-developer):
1. Implements new gen_server
2. Self-validates phases 1-4 locally
3. Reports to Agent 20: "Implementation complete, self-validation passed"

Agent 20 (final-validator):
1. Runs tools/agent-validator.sh --full
2. All phases pass
3. Generates completion report
4. Reports "done" to user

Result: ✅ Merge ready
```

### Example 2: Validation Failure
```
Agent 5 (erlang-test-engineer):
1. Writes tests for new feature
2. Self-validates phases 1-4
3. Reports to Agent 20: "Tests written, coverage 85%"

Agent 20 (final-validator):
1. Runs tools/agent-validator.sh --full
2. Phase 3 fails: 2/10 tests failing
3. Does NOT report "done"
4. Spawns Agent 5 to fix test failures

Agent 5:
1. Fixes 2 failing tests
2. Re-validates phases 1-4
3. Reports to Agent 20 again

Agent 20:
1. Re-runs validation
2. All phases pass
3. Reports "done"

Result: ✅ Merge ready (after 1 re-validation cycle)
```

---

## Summary

**The Golden Rule:**

> An agent's work is NOT complete until ALL six phases validate successfully.
> Reporting "done" is a commitment that the code compiles, tests pass,
> quality gates clear, and the system is merge-ready.

**Enforcement:** Agent 20 is the mandatory gatekeeper. No exceptions.

**Culture:** Pride in zero-defect delivery. Validation is not overhead, it's craft.
