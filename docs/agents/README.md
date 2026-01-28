# Agent Completion Protocol - Documentation Index

**Version:** 1.0.0
**Status:** ACTIVE - MANDATORY

---

## Overview

This directory contains the complete agent completion protocol for erlmcp development. The protocol enforces strict multi-phase validation gates to prevent premature "done" reporting and ensure zero-defect delivery.

---

## Quick Start

### For All Agents (1-19)
1. **Read:** [COMPLETION_PROTOCOL.md](COMPLETION_PROTOCOL.md) - Understand the 6 mandatory phases
2. **Use:** [AGENT_TASK_TEMPLATES.md](AGENT_TASK_TEMPLATES.md) - Follow standard task formats
3. **Self-validate:** Run phases 1-4 locally before reporting to Agent 20
4. **Report:** Only report completion to Agent 20, never claim "done" to user

### For Agent 20 (Final Validator)
1. **Read:** [AGENT_20_CHECKLIST.md](AGENT_20_CHECKLIST.md) - Your mandatory checklist
2. **Run:** `./tools/agent-validator.sh --full` - Automated validation pipeline
3. **Validate:** ALL 6 phases must pass (no exceptions)
4. **Report:** Only report "done" to user after full validation passes

---

## Documentation Structure

### 1. [COMPLETION_PROTOCOL.md](COMPLETION_PROTOCOL.md)
**Purpose:** Define the 6 mandatory validation phases

**Contents:**
- Phase 1: Implementation (code complete, no TODOs)
- Phase 2: Compilation (0 errors)
- Phase 3: Testing (≥90% pass rate)
- Phase 4: Quality Gates (dialyzer, xref, coverage ≥80%)
- Phase 5: Performance (<10% regression)
- Phase 6: Integration (examples work)
- Protocol violations and enforcement
- Agent responsibilities

**Audience:** All agents (1-20)

---

### 2. [AGENT_TASK_TEMPLATES.md](AGENT_TASK_TEMPLATES.md)
**Purpose:** Standard task formats for consistent agent execution

**Contents:**
- Core template structure
- Template 1: Feature Implementation (OTP components)
- Template 2: Test Suite Creation (EUnit, CT, PropER)
- Template 3: Transport Implementation (stdio, TCP, HTTP)
- Template 4: Performance Optimization (benchmarks)
- Template 5: Bug Fix (reproduction tests)
- Template 6: Documentation (API reference, guides)
- Template 7: Integration Task (multi-agent coordination)
- Agent 20 final validator checklist
- Anti-patterns to avoid

**Audience:** All agents (1-20), primarily for task assignment

---

### 3. [VALIDATION_REQUIREMENTS.md](VALIDATION_REQUIREMENTS.md)
**Purpose:** Define non-negotiable quality standards

**Contents:**
- Zero-defect delivery philosophy
- 9 mandatory quality gates
- Agent-specific requirements (agents 1-19 vs Agent 20)
- Validation workflow (self-validation → Agent 20 → CI)
- Quality targets (compilation, tests, coverage, performance)
- Failure handling protocol
- Documentation requirements
- Metrics tracking for continuous improvement

**Audience:** All agents (1-20), project contributors

---

### 4. [AGENT_20_CHECKLIST.md](AGENT_20_CHECKLIST.md)
**Purpose:** Agent 20's mandatory validation checklist

**Contents:**
- Pre-validation checklist (work scope complete?)
- Validation pipeline (automated + manual steps)
- Detailed phase validation (fill-in checklist)
- Exit decision tree (pass → report, fail → spawn fixes)
- Failure protocol (spawn fix agents, re-validate)
- Success protocol (generate report, report "done")
- Special cases (docs-only, test-only, perf-critical)
- Metrics tracking
- Anti-patterns to avoid

**Audience:** Agent 20 exclusively

---

## The Six Mandatory Phases

| Phase | Name | Exit Criteria | Blocker |
|-------|------|---------------|---------|
| **1** | Implementation | No TODO/FIXME, code complete | Incomplete implementation |
| **2** | Compilation | `rebar3 compile` exits 0 | Syntax errors, missing deps |
| **3** | Testing | ≥90% pass rate (strict: 100%) | Test failures |
| **4** | Quality Gates | Dialyzer 0 errors, coverage ≥80% | Type errors, low coverage |
| **5** | Performance | <10% regression from baseline | Performance degradation |
| **6** | Integration | Examples work, e2e flows pass | Integration failures |

**Blocking Rule:** Cannot proceed to next phase until current phase passes.

---

## Tools

### Primary: `tools/agent-validator.sh`
**Purpose:** Automated validation pipeline

**Usage:**
```bash
# Full validation (all 6 phases)
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

**Output:** Detailed status for each phase + actionable next steps

---

### Secondary: Individual Commands
**For manual verification:**

```bash
# Phase 1: Implementation
grep -r "TODO\|FIXME\|XXX" src/

# Phase 2: Compilation
TERM=dumb rebar3 compile

# Phase 3: Testing
rebar3 eunit
rebar3 ct  # if applicable

# Phase 4: Quality Gates
rebar3 dialyzer
rebar3 xref
rebar3 do eunit, cover
rebar3 format --verify

# Phase 5: Performance
make benchmark-quick
# or specific benchmark
erl -pa _build/default/lib/*/ebin -eval \
  "erlmcp_bench_core_ops:run(<<\"core_ops_100k\">>), init:stop()."

# Phase 6: Integration
cd examples/calculator && rebar3 shell
# Test example flows
```

---

## Workflow Diagrams

### Agent 1-19: Self-Validation Flow
```
┌─────────────────────────────────────────┐
│ 1. Receive task from coordinator        │
│    (use AGENT_TASK_TEMPLATES.md)        │
└─────────────────────────────────────────┘
                    ↓
┌─────────────────────────────────────────┐
│ 2. Implement solution                   │
│    - Write code                         │
│    - Follow OTP patterns                │
│    - Add specs                          │
└─────────────────────────────────────────┘
                    ↓
┌─────────────────────────────────────────┐
│ 3. Self-validate phases 1-4             │
│    - Phase 1: No TODOs                  │
│    - Phase 2: rebar3 compile ✅         │
│    - Phase 3: rebar3 eunit ✅           │
│    - Phase 4: dialyzer, coverage ✅     │
└─────────────────────────────────────────┘
                    ↓
           ┌────────┴────────┐
           │  All passed?    │
           └────────┬────────┘
                    │
        ┌───────────┼───────────┐
        │ YES                NO │
        ↓                       ↓
┌───────────────┐       ┌──────────────┐
│ 4. Report to  │       │ 4. Fix       │
│    Agent 20:  │       │    failures  │
│    "Complete, │       │              │
│    validated" │       │ 5. Re-       │
└───────────────┘       │    validate  │
                        └──────────────┘
                                │
                                ↓
                        (loop until pass)
```

### Agent 20: Final Validation Flow
```
┌─────────────────────────────────────────┐
│ 1. Receive completion report from       │
│    implementing agent (1-19)            │
└─────────────────────────────────────────┘
                    ↓
┌─────────────────────────────────────────┐
│ 2. Run automated validation:            │
│    ./tools/agent-validator.sh --full    │
└─────────────────────────────────────────┘
                    ↓
┌─────────────────────────────────────────┐
│ 3. Review validation results            │
│    (use AGENT_20_CHECKLIST.md)          │
└─────────────────────────────────────────┘
                    ↓
           ┌────────┴────────┐
           │  All 6 phases   │
           │  passed?        │
           └────────┬────────┘
                    │
        ┌───────────┼───────────┐
        │ YES                NO │
        ↓                       ↓
┌───────────────┐       ┌──────────────┐
│ 4. Generate   │       │ 4. Spawn fix │
│    completion │       │    agents    │
│    report     │       │              │
│               │       │ 5. Wait for  │
│ 5. Report     │       │    fixes     │
│    "DONE" to  │       │              │
│    user ✅    │       │ 6. Re-run    │
└───────────────┘       │    validation│
                        └──────────────┘
                                │
                                ↓
                        (loop until pass)
```

---

## Quality Targets

| Metric | Minimum | Target | Strict |
|--------|---------|--------|--------|
| Compilation errors | 0 | 0 | 0 |
| Test pass rate | 90% | 95% | 100% |
| Code coverage | 80% | 85% | 90% |
| Dialyzer errors | 0 | 0 | 0 |
| Xref undefined | 0 | 0 | 0 |
| Performance regression | <10% | <5% | 0% |
| First-pass validation | 60% | 80% | 90% |

**First-pass validation:** Percentage of time Agent 20 validation passes on first try (no re-work needed).

---

## Examples

### Example 1: Feature Implementation (Success)
```
User: "Implement cache gen_server"

Coordinator:
  Task("Implement cache", "Create erlmcp_cache.erl with gen_server...", "erlang-otp-developer")

Agent 3 (erlang-otp-developer):
  1. Reads AGENT_TASK_TEMPLATES.md (Template 1: Feature Implementation)
  2. Implements src/erlmcp_cache.erl
  3. Creates test/erlmcp_cache_tests.erl
  4. Self-validates phases 1-4:
     - rebar3 compile ✅
     - rebar3 eunit ✅ (15/15 tests pass)
     - rebar3 dialyzer ✅
     - Coverage: 87% ✅
  5. Reports to Agent 20: "Implementation complete, self-validation passed"

Agent 20:
  1. Runs ./tools/agent-validator.sh --full
  2. All phases pass ✅
  3. Generates completion report
  4. Reports "DONE" to user

Result: ✅ Merge-ready (first-pass validation)
```

### Example 2: Test Suite with Failures (Re-validation)
```
User: "Add tests for erlmcp_transport_tcp"

Coordinator:
  Task("Create test suite", "Test all TCP transport scenarios...", "erlang-test-engineer")

Agent 5 (erlang-test-engineer):
  1. Creates test/erlmcp_transport_tcp_tests.erl
  2. Writes 20 tests
  3. Self-validates:
     - rebar3 compile ✅
     - rebar3 eunit ❌ (18/20 pass, 2 fail)
  4. Fixes 2 failing tests (timeout handling)
  5. Re-validates:
     - rebar3 eunit ✅ (20/20 pass)
  6. Reports to Agent 20: "Test suite complete, all passing"

Agent 20:
  1. Runs ./tools/agent-validator.sh --full
  2. All phases pass ✅
  3. Reports "DONE"

Result: ✅ Merge-ready (after 1 agent self-correction)
```

### Example 3: Performance Regression (Fix Required)
```
User: "Optimize registry lookup performance"

Coordinator:
  Task("Optimize registry", "Reduce lookup latency...", "erlang-performance")

Agent 6 (erlang-performance):
  1. Profiles current performance (baseline: 553K msg/s)
  2. Implements optimization (ETS caching)
  3. Self-validates phases 1-4 ✅
  4. Reports to Agent 20

Agent 20:
  1. Runs ./tools/agent-validator.sh --full
  2. Phase 5 fails: Throughput decreased to 450K msg/s (-18% regression)
  3. Does NOT report "done"
  4. Spawns Agent 6 again: "Fix regression: -18% throughput"

Agent 6:
  1. Investigates: ETS cache lock contention
  2. Fixes: Use read_concurrency option
  3. Re-benchmarks: 620K msg/s (+12% improvement)
  4. Reports to Agent 20

Agent 20:
  1. Re-runs validation
  2. All phases pass ✅
  3. Reports "DONE"

Result: ✅ Merge-ready (after 1 re-validation cycle)
```

---

## Protocol Enforcement

### Pre-Merge Requirements
**ALL pull requests MUST include:**
- Completion report from Agent 20
- All 6 phases validated
- Quality gates passed
- No outstanding validation failures

**CI Pipeline:**
```yaml
# .github/workflows/ci.yml
- name: Validate
  run: |
    ./tools/agent-validator.sh --full
    if [ $? -ne 0 ]; then
      echo "❌ Validation failed - cannot merge"
      exit 1
    fi
```

### Agent Training
**New agents MUST:**
1. Read all 4 protocol documents
2. Review `tools/agent-validator.sh` source
3. Complete practice task with full validation
4. Pass protocol quiz (understand blocking conditions)

### Protocol Violations
**Violation:** Agent reports "done" with failing validation

**Response:**
1. Immediate halt (do not merge)
2. Agent re-training
3. Fix failures
4. Re-validate until pass
5. Document lesson learned

---

## Continuous Improvement (Kaizen)

### Metrics to Track
- First-pass validation rate (target: ≥80%)
- Average re-validation cycles (target: <2)
- Time to validation (target: <5 min)
- Quality gate pass rates by phase

### Monthly Review
1. Analyze validation failures (root causes)
2. Update protocols based on learnings
3. Improve agent training materials
4. Celebrate improvements (recognition)

### Quarterly Assessment
- Review quality targets (adjust if needed)
- Agent effectiveness evaluation
- Tool improvements (agent-validator.sh enhancements)
- Protocol refinements

---

## FAQ

### Q: Can agents skip phases to save time?
**A:** NO. All 6 phases are mandatory. Shortcuts compromise quality.

### Q: What if tests fail but the feature works manually?
**A:** Manual testing is NOT sufficient. Automated tests must pass. Fix the tests.

### Q: Can Agent 3 report "done" directly to the user?
**A:** NO. Only Agent 20 may report "done" to the user after full validation.

### Q: What if dialyzer reports errors in external dependencies?
**A:** Document the issue. Your code must be dialyzer-clean. External issues can be noted.

### Q: Can we lower coverage target from 80% to save time?
**A:** NO. 80% is the minimum. Quality is non-negotiable.

### Q: What if benchmarks take too long to run?
**A:** Use `--quick` mode for development, but full benchmarks mandatory before reporting "done".

---

## Support

### Issues
If you encounter problems with the validation protocol:
1. Check error messages from `agent-validator.sh`
2. Review relevant protocol document
3. Consult Agent 20 checklist
4. Ask coordinator for clarification

### Updates
This protocol is versioned and maintained. Check version number at top of each document.

**Current Version:** 1.0.0
**Last Updated:** 2026-01-28

---

## Summary

**The Golden Rule:**
> An agent's work is NOT complete until ALL six phases validate successfully.
> Reporting "done" is a commitment that code is production-ready.

**Key Principles:**
1. Zero-defect delivery (not "good enough")
2. Agent 20 is mandatory gatekeeper (no bypassing)
3. Self-validation before reporting (agents 1-19)
4. Full validation before "done" (Agent 20)
5. Continuous improvement (metrics, Kaizen)

**Culture:**
- Pride in quality delivery
- Validation is craft, not overhead
- "Done" means "production-ready"
- No shortcuts, no exceptions

---

## Document Index

1. **COMPLETION_PROTOCOL.md** - The 6 mandatory phases
2. **AGENT_TASK_TEMPLATES.md** - Standard task formats
3. **VALIDATION_REQUIREMENTS.md** - Quality standards and gates
4. **AGENT_20_CHECKLIST.md** - Final validator checklist
5. **README.md** - This index document

**Start here:** New to the protocol? Read COMPLETION_PROTOCOL.md first.

**Agent 20?** Read AGENT_20_CHECKLIST.md for your mandatory checklist.

**Assigning tasks?** Use AGENT_TASK_TEMPLATES.md for standard formats.

---

**STATUS:** ACTIVE - MANDATORY FOR ALL ERLMCP AGENTS
