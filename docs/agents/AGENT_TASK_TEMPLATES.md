# Agent Task Templates

**Version:** 1.0.0
**Purpose:** Standard task formats for all erlmcp agents

## Overview

All agent tasks MUST follow these templates to ensure consistent validation and completion reporting.

## Core Template Structure

```
Task: <Feature/Fix Description>
Agent: <agent-name>
Priority: <high|medium|low>
Validation Required: YES
Completion Protocol: docs/agents/COMPLETION_PROTOCOL.md

Instructions:
1. <Specific implementation step>
2. <Specific implementation step>
3. Self-validate phases 1-4 (Implementation → Quality Gates)
4. Report status to Agent 20 for final validation
5. DO NOT report "done" until Agent 20 validates ALL phases

Blocking Condition:
- Report "done" ONLY after Agent 20 validates ALL 6 phases
- If validation fails, fix and re-validate (do not skip)
- If blocked by dependencies, spawn dependency agents first

Exit Criteria:
- <Specific measurable outcome>
- <Quality target>
- <Integration requirement>

Output Format:
Use Phase 6 completion report template (COMPLETION_PROTOCOL.md)

Context:
- Related files: <list>
- Dependencies: <list>
- Prior work: <reference>
```

---

## Template 1: Feature Implementation

**Use Case:** Implementing new OTP components (gen_server, supervisor, etc.)

```
Task: Implement <Feature Name>
Agent: erlang-otp-developer
Priority: high
Validation Required: YES
Completion Protocol: docs/agents/COMPLETION_PROTOCOL.md

Instructions:
1. Create new module: src/<module>.erl
2. Implement OTP behavior: gen_server / supervisor / gen_statem
3. Add Dialyzer specs for all public functions
4. Follow OTP patterns: init/1 async, handle timeouts, monitor dependencies
5. Self-validate phases 1-4:
   - Phase 1: No TODO/FIXME markers
   - Phase 2: rebar3 compile passes
   - Phase 3: Unit tests pass (create if needed)
   - Phase 4: dialyzer, xref, coverage ≥80%
6. Report to Agent 20 for final validation

Blocking Condition:
- DO NOT report "done" until Agent 20 validates ALL 6 phases
- If tests fail, fix before reporting
- If compilation fails, resolve errors before reporting

Exit Criteria:
- Module compiles without errors
- All tests pass (≥90% pass rate)
- Coverage ≥80%
- Dialyzer 0 errors
- Follows OTP patterns (validated by reviewer)

Output Format:
Phase 6 completion report with all validation statuses

Context:
- Related files: src/erlmcp_sup.erl (supervision), docs/otp-patterns.md
- Dependencies: None
- Prior work: See examples/calculator/calculator_server.erl for pattern
```

---

## Template 2: Test Suite Creation

**Use Case:** Writing comprehensive tests (EUnit, CT, PropER)

```
Task: Create test suite for <Module>
Agent: erlang-test-engineer
Priority: high
Validation Required: YES
Completion Protocol: docs/agents/COMPLETION_PROTOCOL.md

Instructions:
1. Create test module: test/<module>_tests.erl (EUnit)
2. Test coverage targets:
   - All public functions
   - Edge cases (empty, invalid, boundary values)
   - Error conditions (crashes, timeouts, bad input)
   - OTP lifecycle (init, terminate, code_change)
3. Follow Chicago School TDD:
   - No mocks for OTP processes (use real processes)
   - Test observable behavior, not internals
   - Setup/cleanup with real supervision trees
4. Achieve ≥80% code coverage (measure with rebar3 cover)
5. Self-validate phases 1-4 before reporting
6. Report to Agent 20 for final validation

Blocking Condition:
- DO NOT report "done" if ANY test fails
- If coverage <80%, add more tests
- If tests are flaky, fix non-determinism

Exit Criteria:
- All tests pass (100% pass rate)
- Coverage ≥80% (verified with rebar3 cover)
- No flaky tests (run 10 times, all pass)
- Tests follow Chicago School patterns

Output Format:
Phase 6 completion report + coverage report

Context:
- Module under test: src/<module>.erl
- Test patterns: See test/erlmcp_client_tests.erl for examples
- Coverage target: 80% minimum
```

---

## Template 3: Transport Implementation

**Use Case:** Building new transport modules (stdio, TCP, HTTP, WebSocket)

```
Task: Implement <Transport> transport
Agent: erlang-transport-builder
Priority: medium
Validation Required: YES
Completion Protocol: docs/agents/COMPLETION_PROTOCOL.md

Instructions:
1. Create module: src/erlmcp_transport_<name>.erl
2. Implement -behaviour(erlmcp_transport) callbacks:
   - init/2 - Setup connection
   - send/2 - Send binary data
   - close/1 - Cleanup resources
3. Handle transport messages:
   - {transport_data, Binary}
   - {transport_connected, Info}
   - {transport_disconnected, Reason}
4. Use production libraries:
   - TCP: ranch (not raw gen_tcp)
   - HTTP: gun + cowboy (not hackney)
   - WebSocket: gun (not custom)
5. Add integration tests (test/<transport>_SUITE.erl)
6. Self-validate phases 1-4 + Phase 5 (performance)
7. Report to Agent 20 for final validation

Blocking Condition:
- DO NOT report "done" without integration tests
- DO NOT skip Phase 5 (performance) for network transports
- If library dependency missing, add to rebar.config first

Exit Criteria:
- Transport module compiles
- Integration tests pass (connect, send, receive, disconnect)
- Performance benchmark <10% regression from TCP baseline
- Library dependencies documented in rebar.config

Output Format:
Phase 6 completion report + benchmark results

Context:
- Transport behavior: src/erlmcp_transport.erl
- Reference impl: src/erlmcp_transport_tcp.erl
- Benchmark: bench/erlmcp_bench_network_real.erl
```

---

## Template 4: Performance Optimization

**Use Case:** Optimizing hot paths, reducing latency, improving throughput

```
Task: Optimize <Component> performance
Agent: erlang-performance
Priority: medium
Validation Required: YES
Completion Protocol: docs/agents/COMPLETION_PROTOCOL.md

Instructions:
1. Profile current performance:
   - Run baseline benchmark: bench/erlmcp_bench_<component>.erl
   - Capture metrics: throughput_msg_per_s, latency_p95_us, memory_heap_mib
   - Document baseline in /tmp/<component>_baseline.txt
2. Identify bottlenecks:
   - Use recon:proc_count/2 for process analysis
   - Use erlang:trace/3 for call tracing (if needed)
   - Check for: large message copies, unnecessary spawns, ETS contention
3. Implement optimizations:
   - <Specific optimization 1>
   - <Specific optimization 2>
4. Re-benchmark and compare:
   - Re-run same benchmark
   - Calculate % improvement
   - Ensure <10% regression in other metrics
5. Self-validate phases 1-5 (MUST include Phase 5)
6. Report to Agent 20 for final validation

Blocking Condition:
- DO NOT skip Phase 5 (performance validation)
- If regression >10%, rollback and try different approach
- If improvement <5%, document "not worth complexity"

Exit Criteria:
- Target metric improved by ≥20%
- No regressions >10% in other metrics
- Code still passes all tests (Phase 3)
- Benchmark results documented

Output Format:
Phase 6 completion report + before/after benchmark comparison

Context:
- Baseline: <current throughput/latency>
- Target: <desired improvement>
- Constraints: Cannot break API compatibility
```

---

## Template 5: Bug Fix

**Use Case:** Fixing crashes, logic errors, or protocol violations

```
Task: Fix bug: <Brief description>
Agent: erlang-otp-developer
Priority: high
Validation Required: YES
Completion Protocol: docs/agents/COMPLETION_PROTOCOL.md

Instructions:
1. Reproduce bug:
   - Create minimal reproduction test: test/<module>_bug_<id>_test.erl
   - Verify test fails with current code
2. Root cause analysis:
   - Identify exact line causing failure
   - Understand why it fails (race condition, bad pattern match, etc.)
   - Document in commit message
3. Implement fix:
   - Modify minimal code to fix root cause
   - Do NOT refactor unrelated code
4. Verify fix:
   - Reproduction test now passes
   - All existing tests still pass
   - No regressions introduced
5. Self-validate phases 1-4
6. Report to Agent 20 for final validation

Blocking Condition:
- DO NOT report "done" if reproduction test still fails
- DO NOT skip writing reproduction test
- If fix introduces regressions, revise fix

Exit Criteria:
- Reproduction test passes
- All existing tests pass (100%)
- No performance regressions
- Bug documented in test name and commit

Output Format:
Phase 6 completion report + reproduction test location

Context:
- Bug report: <issue number or description>
- Affected module: src/<module>.erl
- Symptoms: <observable failure>
```

---

## Template 6: Documentation

**Use Case:** Writing technical documentation, API references, guides

```
Task: Document <Component/Feature>
Agent: erlang-researcher
Priority: low
Validation Required: PARTIAL (Phases 1, 2, 6 only)
Completion Protocol: docs/agents/COMPLETION_PROTOCOL.md (adapted)

Instructions:
1. Create documentation: docs/<topic>.md
2. Follow structure:
   - Overview: What it is
   - Architecture: How it works
   - API Reference: Public functions with examples
   - Usage: Step-by-step guide
   - Examples: Code snippets
3. Validate accuracy:
   - Run all code examples (must compile and work)
   - Cross-reference with actual code (no stale docs)
4. Self-validate adapted phases:
   - Phase 1: No TODO markers
   - Phase 2: Code examples compile
   - Phase 6: Examples work end-to-end
5. Report to Agent 20 for final validation

Blocking Condition:
- DO NOT report "done" if code examples don't work
- DO NOT create documentation without testing examples

Exit Criteria:
- Documentation is complete (all sections)
- All code examples compile and run
- Cross-references are accurate
- Markdown linting passes

Output Format:
Phase 6 completion report (adapted for docs)

Context:
- Component: <name>
- Related docs: docs/architecture.md, docs/api-reference.md
- Examples to test: <list>
```

---

## Template 7: Integration Task (Multi-Agent)

**Use Case:** Complex features requiring multiple agents

```
Task: Implement <Complex Feature>
Coordinator: Agent 20
Priority: high
Validation Required: YES
Completion Protocol: docs/agents/COMPLETION_PROTOCOL.md

Instructions:
1. Break down into sub-tasks:
   - Sub-task 1: <description> (assign to Agent A)
   - Sub-task 2: <description> (assign to Agent B)
   - Sub-task 3: <description> (assign to Agent C)
2. Spawn agents concurrently (use Task tool):
   Task("<description>", "<details>", "agent-a")
   Task("<description>", "<details>", "agent-b")
   Task("<description>", "<details>", "agent-c")
3. Each agent self-validates phases 1-4
4. Agent 20 validates integration:
   - Phase 6: End-to-end flow works
   - All components integrate correctly
   - No cross-module errors
5. Agent 20 runs full validation pipeline
6. Agent 20 reports final "done" ONLY after ALL agents complete

Blocking Condition:
- DO NOT report "done" until ALL sub-agents complete
- If any sub-agent fails, fix before integration
- If integration issues found, spawn fix agents

Exit Criteria:
- All sub-tasks complete (validated)
- Integration tests pass
- End-to-end flow works
- Full validation (phases 1-6) passes

Output Format:
Phase 6 completion report + sub-agent summary

Context:
- Sub-agents: <list>
- Dependencies: <order of execution>
- Integration points: <where components connect>
```

---

## Agent 20: Final Validator Checklist

**MANDATORY for Agent 20 before reporting "done":**

```markdown
# Agent 20 Final Validation Checklist

Task: <task description>
Date: <ISO 8601 date>

## Pre-Validation
- [ ] All implementation agents reported complete
- [ ] No agents reported blocking errors

## Phase Validation (run tools/agent-validator.sh)
- [ ] Phase 1: Implementation - PASSED
- [ ] Phase 2: Compilation - 0 errors, X modules
- [ ] Phase 3: Testing - ≥90% pass rate, X/X tests passed
- [ ] Phase 4: Quality Gates - 0 dialyzer errors, 0 xref errors, ≥80% coverage
- [ ] Phase 5: Performance - 0% regression (or documented)
- [ ] Phase 6: Integration - All examples working

## Manual Verification
- [ ] Reviewed agent outputs for quality
- [ ] Checked for TODO/FIXME markers
- [ ] Verified test coverage report
- [ ] Confirmed no suppression comments (or justified)

## Exit Decision
IF ALL CHECKED:
  ✅ REPORT "DONE" with Phase 6 completion report
ELSE:
  ❌ DO NOT REPORT "DONE"
  - Spawn fix agents for failed phases
  - Re-run validation after fixes
  - Repeat until all checked

## Completion Report
<Paste Phase 6 completion report from agent-validator.sh>
```

---

## Usage Examples

### Example 1: Agent 3 (OTP Developer) receives task

```
Task: Implement cache gen_server
Agent: erlang-otp-developer
Priority: high
Validation Required: YES

Agent 3 actions:
1. Reads task template (Template 1: Feature Implementation)
2. Implements src/erlmcp_cache.erl (gen_server)
3. Adds specs, follows OTP patterns
4. Self-validates locally:
   - rebar3 compile ✅
   - Created test/erlmcp_cache_tests.erl ✅
   - rebar3 eunit ✅ (10/10 passed)
   - rebar3 dialyzer ✅
5. Reports to Agent 20:
   "Implementation complete. Self-validation passed (phases 1-4).
   Ready for final validation."

Agent 20 receives report:
1. Runs tools/agent-validator.sh --full
2. All phases pass
3. Generates Phase 6 completion report
4. Reports "DONE" to user

Result: ✅ Merge-ready
```

### Example 2: Agent 5 (Test Engineer) receives task, tests fail

```
Task: Create test suite for erlmcp_registry
Agent: erlang-test-engineer
Priority: high

Agent 5 actions:
1. Creates test/erlmcp_registry_tests.erl
2. Writes 15 tests (public API, edge cases, errors)
3. Runs self-validation:
   - rebar3 compile ✅
   - rebar3 eunit ❌ (13/15 passed, 2 failed)
4. Does NOT report to Agent 20 yet (protocol requires 100% pass)
5. Fixes 2 failing tests
6. Re-runs validation:
   - rebar3 eunit ✅ (15/15 passed)
7. Reports to Agent 20:
   "Test suite complete. All tests passing. Coverage: 85%."

Agent 20 validates:
1. Runs tools/agent-validator.sh --full ✅
2. Reports "DONE"

Result: ✅ Merge-ready (after 1 self-correction)
```

---

## Anti-Patterns (DO NOT DO THIS)

### ❌ Anti-Pattern 1: Premature "Done" Reporting
```
Agent: "I've implemented the feature. Done!"
Reality: Code doesn't compile, no tests written
Violation: Skipped phases 2-6
```

### ❌ Anti-Pattern 2: Skipping Tests
```
Agent: "Feature implemented. I tested manually, it works. Done!"
Reality: No automated tests, can't verify
Violation: Skipped phase 3 (testing)
```

### ❌ Anti-Pattern 3: Ignoring Validation Failures
```
Agent: "Implementation done. Dialyzer has some errors but they're minor."
Reality: Type errors can cause runtime crashes
Violation: Failed phase 4 but reported "done"
```

### ❌ Anti-Pattern 4: No Agent 20 Validation
```
Agent 3: "I validated locally, everything passes. Done!"
User: "But when I run it, tests fail..."
Violation: Bypassed Agent 20 mandatory validation
```

---

## Summary

**Key Rules:**
1. All tasks use standardized templates
2. All agents self-validate phases 1-4 before reporting
3. Agent 20 MUST validate phases 1-6 before "done"
4. NO agent may report "done" without validation
5. If validation fails, fix and re-validate (no exceptions)

**Quality Culture:**
- Validation is not overhead, it's craft
- Pride in zero-defect delivery
- Agent 20 is the quality gatekeeper
- "Done" means "production-ready"
