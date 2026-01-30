# SPARC Execution Plan: MCP 2025-11-25 Compliance + Code Consolidation

**Project**: erlmcp MCP Compliance & 80/20 Consolidation
**Version**: 0.6.0 → 0.7.0
**Date**: 2026-01-30
**Branch**: claude/mcp-compliance-consolidation-5bvSP
**Methodology**: SPARC (Specification, Pseudocode, Architecture, Refinement, Completion)
**Status**: Phase 1 - Specification

---

## Executive Summary

This document orchestrates a combined workstream implementing **MCP 2025-11-25 specification compliance** alongside **benchmark consolidation** using the SPARC methodology. The plan sequences work to minimize churn, identifies critical path items, and enables parallel execution where possible.

**Key Objectives**:
- Implement 5 MCP feature gaps (Task Management, Cancellation, Sampling, Completions, Elicitation)
- Consolidate 93 benchmark files to 40-50 files (53% LOC reduction)
- Achieve ≥80% test coverage using Chicago School TDD
- Maintain backward compatibility and zero performance regression
- Coordinate workstreams to avoid rework

**Timeline**: 14 weeks (3.5 months)
**Success Criteria**:
- ✅ All MCP gaps implemented with tests
- ✅ Benchmark suite consolidated with standardized measurements
- ✅ 100% test pass rate, ≥80% coverage
- ✅ 0 compilation errors, 0 Dialyzer warnings, 0 Xref issues
- ✅ <10% performance regression from baseline

---

## Table of Contents

1. [Phase 1: Specification (SPARC Phase 1)](#phase-1-specification-sparc-phase-1)
2. [Phase 2: Pseudocode (SPARC Phase 2)](#phase-2-pseudocode-sparc-phase-2)
3. [Phase 3: Architecture (SPARC Phase 3)](#phase-3-architecture-sparc-phase-3)
4. [Phase 4: Refinement (SPARC Phase 4)](#phase-4-refinement-sparc-phase-4)
5. [Phase 5: Completion (SPARC Phase 5)](#phase-5-completion-sparc-phase-5)
6. [Critical Path Analysis](#critical-path-analysis)
7. [Parallel Work Streams](#parallel-work-streams)
8. [Risk Management](#risk-management)
9. [Agent Assignments](#agent-assignments)

---

## Phase 1: Specification (SPARC Phase 1)

**Duration**: Week 1 (5 days)
**Goal**: Define requirements for MCP compliance and consolidation
**Status**: COMPLETED (existing documents)

### 1.1 MCP Compliance Specification

**Deliverable**: `docs/MCP_2025-11-25_SPECIFICATION_GAPS.md` ✅

**Requirements Summary**:
- Gap #1: Task Management (tasks/create, tasks/list, tasks/get, tasks/result, tasks/cancel)
- Gap #2: Request Cancellation (requests/cancel, graceful shutdown)
- Gap #3: Progress Tokens (already implemented, needs integration)
- Gap #4: Sampling Capability (sampling/create, LLM integration)
- Gap #5: Completions API (completions/complete, prompt expansion)
- Gap #6: Elicitation Features (argument elicitation, form-based input)

**Priority**:
1. **CRITICAL**: Task Management (foundation for other features)
2. **HIGH**: Request Cancellation, Completions API
3. **MEDIUM**: Sampling Capability, Elicitation Features

### 1.2 Consolidation Specification

**Deliverable**: `BENCHMARK_CONSOLIDATION_ROADMAP.md` ✅

**Requirements Summary**:
- Delete 9 redundant files (750 LOC)
- Consolidate 4 file groups (benchmark_100k, registry, queue, chaos)
- Standardize metrology (microseconds, percentiles, ISO compliance)
- Create unified benchmark framework
- Implement baseline comparison and regression detection

**Risks Identified**:
- Consolidation during MCP implementation could cause test churn
- Benchmark baselines may need recalibration after MCP changes
- Performance regression risk if not properly sequenced

### 1.3 Integration Requirements

**Critical Dependencies**:
- Task Management must be implemented BEFORE Completions API
- Progress Tokens integration required for Task Management
- Benchmark consolidation should occur AFTER core MCP features to avoid test churn
- Cancellation mechanism needed for both tasks and completions

---

## Phase 2: Pseudocode (SPARC Phase 2)

**Duration**: Week 2 (5 days)
**Goal**: Algorithm design for MCP features and consolidation logic
**Deliverable**: `docs/MCP_2025-11-25_PSEUDOCODE_DESIGN.md` ✅

### 2.1 MCP Feature Algorithms

#### Task Management State Machine
```pseudocode
FUNCTION create_task(action, metadata):
    task_id = generate_unique_id()
    task = {
        id: task_id,
        status: "pending",
        action: action,
        created_at: now(),
        metadata: metadata
    }

    ETS:insert(tasks_table, task)
    emit_telemetry(task_created, task_id)

    RETURN {task_id, status: "pending"}

FUNCTION execute_task(task_id):
    task = ETS:lookup(tasks_table, task_id)

    IF task.status != "pending":
        RETURN {error, invalid_state}

    update_status(task_id, "working")

    TRY:
        result = perform_action(task.action)
        update_task(task_id, status: "completed", result: result)
        RETURN {ok, result}
    CATCH error:
        update_task(task_id, status: "failed", error: error)
        RETURN {error, error}

FUNCTION cancel_task(task_id, reason):
    task = ETS:lookup(tasks_table, task_id)

    IF task.status IN ["completed", "failed", "cancelled"]:
        RETURN {error, already_terminal}

    IF task.status == "working":
        send_cancellation_signal(task.worker_pid)
        wait_for_acknowledgment(timeout: 5000)

    update_status(task_id, "cancelled", cancelled_at: now())
    RETURN {ok, cancelled}
```

#### Request Cancellation Flow
```pseudocode
FUNCTION cancel_request(request_id, reason):
    request = get_pending_request(request_id)

    IF request == undefined:
        RETURN {error, not_found}

    send_cancel_notification(request.handler_pid, request_id, reason)

    mark_request_cancelled(request_id)

    cleanup_resources(request_id)

    RETURN {ok, {request_id, status: "cancelled"}}
```

### 2.2 Consolidation Algorithm

```pseudocode
FUNCTION consolidate_benchmarks():
    # Phase 1: Cleanup
    FOR each redundant_file IN [simple_benchmark, simple_stress, ...]:
        verify_no_unique_tests(redundant_file)
        git_rm(redundant_file)

    # Phase 2: Merge
    FOR each (source, target) IN consolidation_pairs:
        extract_unique_functions(source)
        merge_into_target(target, unique_functions)
        verify_tests_pass(target)
        git_rm(source)

    # Phase 3: Standardize
    create_metrology_module()
    FOR each benchmark_file IN remaining_files:
        update_measurements(file, use_metrology_module)
        add_missing_percentiles(file, [p99_9, p99_99])
        verify_tests_pass(file)

    # Phase 4: Framework
    create_baseline_manager()
    create_unified_runner()
    integrate_with_ci()
```

---

## Phase 3: Architecture (SPARC Phase 3)

**Duration**: Week 3 (5 days)
**Goal**: System design and supervision tree architecture
**Deliverable**: `docs/MCP_2025-11-25_ARCHITECTURE_DESIGN.md` ✅

### 3.1 MCP Architecture Additions

#### New Supervision Tree Components

```
erlmcp_sup (one_for_one)
├── erlmcp_core_sup (one_for_one)
│   ├── erlmcp_task_manager          [NEW] ← Task lifecycle management
│   ├── erlmcp_cancellation          [ENHANCED] ← Request cancellation
│   ├── erlmcp_completion            [NEW] ← Completions API
│   ├── erlmcp_sampling              [NEW] ← Sampling capability
│   ├── erlmcp_elicitation           [NEW] ← Elicitation features
│   ├── erlmcp_progress              [EXISTING] ← Progress tokens
│   └── ... [existing components]
```

**Key Design Decisions**:
- **Task Manager**: gen_server with ETS backing (10K+ concurrent tasks)
- **Cancellation**: Process monitoring with graceful shutdown (5s timeout)
- **Completions**: Integration with server tool handlers
- **Sampling**: Optional LLM integration (mock for testing)
- **Elicitation**: Form-based argument validation

### 3.2 Benchmark Architecture

#### Consolidated Structure

```
bench/
├── erlmcp_bench_core_ops.erl        [EXISTING] ← Registry, queue, pool, session
├── erlmcp_bench_network_real.erl    [EXISTING] ← TCP/HTTP real sockets
├── erlmcp_bench_stress.erl          [EXISTING] ← Sustained load
├── erlmcp_bench_chaos.erl           [EXISTING] ← Failure injection
├── erlmcp_bench_integration.erl     [EXISTING] ← MCP workflows
├── erlmcp_bench_helpers.erl         [EXISTING] ← Shared utilities
└── erlmcp_bench_metrology.erl       [NEW] ← Measurement standards

src/
├── erlmcp_benchmark.erl             [NEW] ← Unified runner
├── erlmcp_benchmark_baseline.erl    [NEW] ← Baseline comparison
└── erlmcp_benchmark_results.erl     [NEW] ← Result aggregation
```

**Integration Points**:
- Benchmark suite runs AFTER MCP implementation (avoid test churn)
- New MCP features measured in `erlmcp_bench_integration.erl`
- Task management benchmarked in `erlmcp_bench_core_ops.erl`
- Cancellation tested in `erlmcp_bench_chaos.erl`

---

## Phase 4: Refinement (SPARC Phase 4)

**Duration**: Weeks 4-11 (8 weeks)
**Goal**: Implementation with Chicago School TDD
**Test Coverage Target**: ≥80%

### 4.1 Critical Path: Sequential Implementation

#### Week 4-5: Task Management (CRITICAL PATH)

**Module**: `erlmcp_task_manager.erl`
**Agent**: erlang-otp-developer
**Dependencies**: erlmcp_progress (existing)

**Implementation Order**:
1. Day 1-2: Gen_server skeleton + basic tests
2. Day 3-4: Task creation + listing + retrieval
3. Day 5-6: Task execution with state machine
4. Day 7-8: Task cancellation + cleanup
5. Day 9-10: Integration tests + documentation

**Acceptance Criteria**:
- ✅ All 5 task methods implemented (create, get, list, result, cancel)
- ✅ State machine validates transitions
- ✅ EUnit + CT tests pass (≥80% coverage)
- ✅ Performance: create <10ms (p99), retrieve <50ms (p99)
- ✅ Supervisor integration working
- ✅ Telemetry events emitted

**Quality Gate**:
```bash
TERM=dumb rebar3 compile                      # 0 errors
rebar3 eunit --module=erlmcp_task_manager_tests
rebar3 ct --suite=erlmcp_task_manager_SUITE
rebar3 dialyzer                               # 0 warnings
make benchmark-quick                          # <10% regression
```

#### Week 6-7: Request Cancellation + Completions API (PARALLEL)

**Stream A: Request Cancellation**
**Module**: `erlmcp_cancellation.erl` (enhancement)
**Agent**: erlang-otp-developer

**Implementation**:
1. Add `cancel_request/2` API
2. Process monitoring for in-flight requests
3. Graceful shutdown with timeout
4. Integration with client/server

**Stream B: Completions API**
**Module**: `erlmcp_completion.erl`
**Agent**: erlang-otp-developer (separate instance)

**Implementation**:
1. Completions handler registration
2. Prompt expansion logic
3. Integration with task manager (async completions)
4. Result caching

**Acceptance Criteria**:
- ✅ Both modules implemented independently
- ✅ Cancellation works for tools, resources, completions
- ✅ Completions integrate with task manager
- ✅ Tests pass, no performance regression

#### Week 8-9: Sampling + Elicitation (PARALLEL)

**Stream C: Sampling Capability**
**Module**: `erlmcp_sampling.erl`
**Agent**: erlang-otp-developer

**Implementation**:
1. Sampling API (sampling/create)
2. Mock LLM integration for testing
3. Optional external LLM integration points
4. Rate limiting and quota management

**Stream D: Elicitation Features**
**Module**: `erlmcp_elicitation.erl`
**Agent**: erlang-otp-developer (separate instance)

**Implementation**:
1. Argument elicitation state machine
2. Form-based input validation
3. Integration with tool execution
4. Error handling for incomplete forms

**Acceptance Criteria**:
- ✅ Sampling works with mock and external LLMs
- ✅ Elicitation validates arguments before execution
- ✅ Both features integrated with server
- ✅ Tests pass, documentation complete

#### Week 10-11: MCP Integration + Server/Client Updates

**Agent**: erlang-otp-developer + erlang-test-engineer

**Implementation**:
1. Update `erlmcp_server.erl` with new method handlers
2. Update `erlmcp_client.erl` with new API methods
3. Integration tests (full MCP workflow)
4. Example applications (calculator, weather)
5. Documentation updates

**Acceptance Criteria**:
- ✅ All 5 MCP gaps fully integrated
- ✅ Client/server support all new methods
- ✅ Examples demonstrate new features
- ✅ Protocol compliance verified

**Quality Gate (MCP Implementation Complete)**:
```bash
TERM=dumb rebar3 compile                      # 0 errors
rebar3 eunit                                  # 100% pass
rebar3 ct                                     # 100% pass
rebar3 dialyzer                               # 0 warnings
rebar3 xref                                   # 0 issues
make benchmark-quick                          # <10% regression
```

### 4.2 Parallel Stream: Benchmark Consolidation

**Timing**: Weeks 10-13 (AFTER MCP implementation)
**Rationale**: Avoid test churn during feature development

#### Week 10: Cleanup Phase

**Agent**: erlang-performance

**Tasks**:
1. Delete 9 redundant files (verify no unique tests)
2. Consolidate benchmark_100k into SUITE
3. Consolidate registry stress tests
4. Consolidate queue benchmarks

**Acceptance Criteria**:
- ✅ 750 LOC removed
- ✅ All existing tests still pass
- ✅ No unique functionality lost

#### Week 11: Metrology Standardization

**Agent**: erlang-performance

**Tasks**:
1. Create `erlmcp_bench_metrology.erl`
2. Fix percentile calculations (use ceil, not round)
3. Add p99.9/p99.99 measurements
4. Calibrate measurement overhead
5. Document all standard definitions

**Acceptance Criteria**:
- ✅ All benchmarks use metrology module
- ✅ Measurements consistent (microseconds)
- ✅ ISO-compliant definitions
- ✅ Validation functions working

#### Week 12: Framework Infrastructure

**Agent**: erlang-performance

**Tasks**:
1. Create baseline manager (`erlmcp_benchmark_baseline.erl`)
2. Create unified runner (`erlmcp_benchmark.erl`)
3. Create result aggregator (`erlmcp_benchmark_results.erl`)
4. Implement regression detection

**Acceptance Criteria**:
- ✅ `make benchmark-all` runs full suite
- ✅ Baseline comparison working
- ✅ Regression detection accurate
- ✅ Results stored and queryable

#### Week 13: CI Integration

**Agent**: erlang-github-ops

**Tasks**:
1. Create GitHub Actions workflow
2. Add Makefile targets
3. Write benchmarking guide
4. Integrate with quality gates

**Acceptance Criteria**:
- ✅ CI runs benchmarks daily
- ✅ Regression alerts on PR
- ✅ Documentation complete
- ✅ One-command execution

---

## Phase 5: Completion (SPARC Phase 5)

**Duration**: Week 14 (5 days)
**Goal**: Validation, PR creation, release preparation

### 5.1 Quality Validation

**Agent**: code-reviewer

**Day 1-2: Code Review**
- Review all new modules for OTP patterns
- Verify supervision tree correctness
- Check error handling and edge cases
- Validate test coverage (≥80%)

**Day 3: Performance Validation**
```bash
make benchmark-all
make benchmark-compare
```

**Acceptance Criteria**:
- ✅ Throughput: <10% regression from baseline
- ✅ Latency: p99 within acceptable bounds
- ✅ Memory: No leaks detected
- ✅ All benchmarks pass

**Day 4: Compliance Validation**
- Run MCP protocol compliance tests
- Verify all 5 gaps implemented
- Test backward compatibility
- Check API documentation

### 5.2 Release Preparation

**Agent**: erlang-github-ops

**Deliverables**:
1. PR creation with comprehensive description
2. Release notes (v0.7.0)
3. Migration guide (v0.6.0 → v0.7.0)
4. Updated CHANGELOG.md
5. Version bump in `erlmcp.app.src`

**PR Description Template**:
```markdown
## Summary
Implements MCP 2025-11-25 specification compliance + benchmark consolidation

## MCP Features Implemented
- Task Management (tasks/*)
- Request Cancellation (requests/cancel)
- Completions API (completions/complete)
- Sampling Capability (sampling/create)
- Elicitation Features (argument validation)

## Consolidation Results
- Reduced benchmark files: 93 → 47 (50% reduction)
- Reduced LOC: 25,490 → 12,800 (50% reduction)
- Standardized measurements (ISO-compliant)
- Unified benchmark runner
- Baseline comparison framework

## Test Results
- EUnit: 387/387 passed (100%)
- CT: 52/52 suites passed (100%)
- Coverage: 84.3% (target: ≥80%)
- Dialyzer: 0 warnings
- Xref: 0 issues

## Performance
- Throughput: 2.69M ops/s (baseline: 2.71M, -0.7%)
- Latency p99: 453µs (baseline: 448µs, +1.1%)
- Memory: Stable, no leaks
- All benchmarks within <10% regression tolerance

## Breaking Changes
None. Backward compatible with v0.6.0.

## Documentation
- API reference updated
- Architecture docs updated
- Examples added (task management, completions)
- Benchmarking guide added

https://claude.ai/code/session_018igga7R3JKL2TCSDqy27ro
```

### 5.3 Acceptance Checklist

**Critical Gates (MANDATORY)**:
- [ ] Compilation: 0 errors
- [ ] Tests: 100% pass rate
- [ ] Coverage: ≥80%
- [ ] Dialyzer: 0 warnings
- [ ] Xref: 0 issues
- [ ] Performance: <10% regression
- [ ] Backward compatibility: Verified

**Documentation Gates**:
- [ ] API reference updated
- [ ] Architecture docs updated
- [ ] Examples working
- [ ] Migration guide complete
- [ ] CHANGELOG.md updated

**Release Gates**:
- [ ] Version bumped (0.7.0)
- [ ] Git tag created
- [ ] PR approved and merged
- [ ] Release notes published

---

## Critical Path Analysis

### Sequential Dependencies (Cannot Parallelize)

```
Week 1:  Specification (DONE)
           ↓
Week 2:  Pseudocode (DONE)
           ↓
Week 3:  Architecture (DONE)
           ↓
Week 4-5: Task Management (CRITICAL PATH - blocks completions)
           ↓
Week 6-7: [Cancellation || Completions] (parallel, but both needed)
           ↓
Week 8-9: [Sampling || Elicitation] (parallel)
           ↓
Week 10-11: MCP Integration
           ↓
Week 12-13: Benchmark Consolidation (AFTER MCP to avoid churn)
           ↓
Week 14: Quality Gates + Release
```

### Critical Path Duration
- **Minimum Timeline**: 14 weeks (assumes parallelization where possible)
- **Critical Path Items**: Task Management → MCP Integration → Consolidation → Release

---

## Parallel Work Streams

### Stream Matrix

| Week    | Stream A (Primary)        | Stream B (Parallel)      | Stream C (Parallel)     |
|---------|---------------------------|--------------------------|-------------------------|
| 1       | Specification             | -                        | -                       |
| 2       | Pseudocode                | -                        | -                       |
| 3       | Architecture              | -                        | -                       |
| 4-5     | Task Management           | -                        | -                       |
| 6-7     | Request Cancellation      | Completions API          | -                       |
| 8-9     | Sampling Capability       | Elicitation Features     | -                       |
| 10-11   | MCP Integration           | -                        | Benchmark Cleanup       |
| 12      | -                         | Metrology Standards      | Framework Infrastructure|
| 13      | -                         | CI Integration           | -                       |
| 14      | Quality Gates             | Release Prep             | -                       |

### Parallelization Opportunities

**Weeks 6-7**: 2 developers can work independently
- Developer A: Request Cancellation (erlmcp_cancellation.erl)
- Developer B: Completions API (erlmcp_completion.erl)
- **Coordination**: Weekly sync, shared test infrastructure

**Weeks 8-9**: 2 developers can work independently
- Developer A: Sampling (erlmcp_sampling.erl)
- Developer B: Elicitation (erlmcp_elicitation.erl)
- **Coordination**: Weekly sync, integration tests at end

**Weeks 10-11**: Consolidation can START during MCP integration
- Developer A: MCP integration (primary focus)
- Developer B: Benchmark cleanup (deletion phase only)
- **Coordination**: Daily sync to avoid conflicts

**Week 12**: Full parallel execution
- Developer A: Metrology standardization
- Developer B: Framework infrastructure
- **Coordination**: Shared metrology module interface

---

## Risk Management

### Risk Matrix

| Risk ID | Description | Probability | Impact | Mitigation |
|---------|-------------|-------------|--------|------------|
| R1 | Task Manager delays block completions | MEDIUM | HIGH | Start Task Manager early (Week 4) |
| R2 | Consolidation causes test churn | HIGH | MEDIUM | Do consolidation AFTER MCP (Week 12) |
| R3 | Performance regression undetected | LOW | HIGH | Benchmark before/after each feature |
| R4 | Integration breaks existing features | MEDIUM | HIGH | Backward compatibility tests |
| R5 | Parallel streams conflict | LOW | MEDIUM | Daily syncs, branch protection |
| R6 | Quality gates fail at end | MEDIUM | HIGH | Run gates after each module |

### Risk Mitigation Strategy

**R1: Task Manager Delays**
- Allocate 2 full weeks (buffer included)
- Start with comprehensive tests (TDD)
- Daily progress tracking
- Fallback: Reduce scope (cancel can come later)

**R2: Consolidation Test Churn**
- **Key Mitigation**: Do consolidation AFTER MCP implementation
- Create baseline BEFORE consolidation
- Consolidate one file at a time, verify tests
- Git tags for rollback if needed

**R3: Performance Regression**
- Run `make benchmark-quick` after each module
- Compare to baseline stored at Week 4
- Alert if >10% regression
- Investigate immediately (don't accumulate debt)

**R4: Integration Breaks Features**
- Run full test suite after each integration
- Maintain backward compatibility tests
- Use feature flags for new capabilities
- Smoke tests in examples/ directory

**R5: Parallel Stream Conflicts**
- Use feature branches (task-management, completions, etc.)
- Daily syncs between parallel developers
- Merge to integration branch weekly
- Main branch protected (requires review)

**R6: Quality Gate Failures**
- Run gates after EACH module (not just at end)
- Fix issues immediately (don't accumulate)
- Automated CI checks on every commit
- Pre-merge validation required

---

## Agent Assignments

### Phase-Specific Ownership

| Phase | Weeks | Primary Agent | Secondary Agent | Deliverable |
|-------|-------|---------------|-----------------|-------------|
| Spec  | 1     | plan-designer | erlang-researcher | Requirements docs ✅ |
| Pseudo| 2     | plan-designer | erlang-architect | Algorithm design ✅ |
| Arch  | 3     | erlang-architect | erlang-otp-developer | System design ✅ |
| Task Mgmt | 4-5 | erlang-otp-developer | erlang-test-engineer | Task manager + tests |
| Cancel | 6-7 | erlang-otp-developer | - | Cancellation + tests |
| Completions | 6-7 | erlang-otp-developer | - | Completions + tests |
| Sampling | 8-9 | erlang-otp-developer | - | Sampling + tests |
| Elicitation | 8-9 | erlang-otp-developer | - | Elicitation + tests |
| Integration | 10-11 | erlang-otp-developer | erlang-test-engineer | Full MCP integration |
| Consolidation | 12-13 | erlang-performance | - | Benchmark suite |
| CI | 13 | erlang-github-ops | - | GitHub Actions |
| Review | 14 | code-reviewer | - | Quality validation |
| Release | 14 | erlang-github-ops | - | PR + release |

### Coordination Protocol

**Daily Standups** (async, written):
- What did you complete yesterday?
- What are you working on today?
- Any blockers?

**Weekly Syncs** (synchronous, 30 min):
- Demo completed work
- Integration planning
- Risk review
- Next week planning

**Quality Gates** (after each module):
- Run full validation suite
- Report results to team
- No proceed until gates pass

---

## Sequencing Strategy: Minimize Churn

### The Problem
- Implementing MCP features WHILE consolidating benchmarks causes:
  - Tests change during implementation
  - Baselines invalidated
  - Performance comparisons unreliable
  - Rework if consolidation breaks MCP tests

### The Solution: Sequential Phasing

```
┌─────────────────────────────────────────────────────────────┐
│ Weeks 4-11: MCP Implementation (Feature Focus)              │
│ - Implement all 5 MCP gaps                                  │
│ - Use EXISTING benchmark infrastructure                      │
│ - Establish baseline at Week 4 (pre-implementation)         │
│ - Measure each feature against baseline                     │
│ - DO NOT touch benchmark files                              │
└─────────────────────────────────────────────────────────────┘
                            ↓
┌─────────────────────────────────────────────────────────────┐
│ Week 11: Baseline Capture (Freeze Point)                    │
│ - Run full benchmark suite                                  │
│ - Store as "v0.7.0 MCP Implementation Baseline"            │
│ - Git tag: erlmcp-v0.7.0-mcp-complete                      │
│ - All MCP features implemented and tested                   │
└─────────────────────────────────────────────────────────────┘
                            ↓
┌─────────────────────────────────────────────────────────────┐
│ Weeks 12-13: Benchmark Consolidation (Cleanup Focus)        │
│ - Delete redundant files                                    │
│ - Consolidate duplicates                                    │
│ - Standardize measurements                                  │
│ - Build framework infrastructure                            │
│ - Compare to baseline at each step                          │
└─────────────────────────────────────────────────────────────┘
                            ↓
┌─────────────────────────────────────────────────────────────┐
│ Week 14: Final Validation                                   │
│ - Verify performance unchanged                              │
│ - All tests pass                                            │
│ - Quality gates pass                                        │
│ - Release v0.7.0                                            │
└─────────────────────────────────────────────────────────────┘
```

### Why This Works

1. **No Test Churn**: Benchmark files stable during MCP implementation
2. **Valid Baselines**: Capture baseline AFTER MCP, BEFORE consolidation
3. **Clear Regression Detection**: Any issues from consolidation immediately visible
4. **Rollback Safety**: Can revert consolidation without losing MCP work
5. **Focus**: Developers work on one thing at a time (feature OR cleanup)

---

## Success Metrics

### MCP Compliance Metrics

| Metric | Target | Validation Method |
|--------|--------|-------------------|
| Features Implemented | 5/5 (100%) | Protocol compliance tests |
| Test Coverage | ≥80% | `rebar3 cover` |
| Test Pass Rate | 100% | `rebar3 eunit && rebar3 ct` |
| Dialyzer Warnings | 0 | `rebar3 dialyzer` |
| Xref Issues | 0 | `rebar3 xref` |
| Backward Compatibility | 100% | Legacy example apps |
| Performance Regression | <10% | Baseline comparison |

### Consolidation Metrics

| Metric | Before | After | Target |
|--------|--------|-------|--------|
| Total Files | 93 | ? | 40-50 |
| Total LOC | 25,490 | ? | 12,000-13,000 |
| Duplicate Scenarios | 15+ | 0 | 0 |
| Standardized Measurements | ~30% | 100% | 100% |
| Regression Detection | No | Yes | Yes |
| Unified Entry Point | No | Yes | Yes |

### Quality Metrics

| Gate | Requirement | Status |
|------|-------------|--------|
| Compilation | 0 errors | □ |
| EUnit Tests | 100% pass | □ |
| CT Tests | 100% pass | □ |
| Coverage | ≥80% | □ |
| Dialyzer | 0 warnings | □ |
| Xref | 0 issues | □ |
| Performance | <10% regression | □ |
| Documentation | Complete | □ |

---

## Next Steps

### Week 1-3: Already Complete ✅
- Specification documents exist
- Pseudocode designs exist
- Architecture designs exist

### Week 4: Immediate Actions
1. [ ] Create feature branch: `feature/task-management`
2. [ ] Assign erlang-otp-developer agent
3. [ ] Establish performance baseline (run benchmarks, store results)
4. [ ] Begin Task Manager implementation (TDD)

### Week 4 Checklist
- [ ] Run full benchmark suite: `make benchmark-all`
- [ ] Store baseline: `erlmcp_benchmark_baseline:set_baseline(all, Results)`
- [ ] Create module: `apps/erlmcp_core/src/erlmcp_task_manager.erl`
- [ ] Create tests: `apps/erlmcp_core/test/erlmcp_task_manager_tests.erl`
- [ ] Create supervisor: `apps/erlmcp_core/src/erlmcp_task_manager_sup.erl`
- [ ] Daily quality gates: compile + tests after each day

### Communication Plan
- **Daily**: Written status updates (blockers, progress, next)
- **Weekly**: Demo + integration planning (30 min sync)
- **Bi-weekly**: Risk review + timeline adjustment
- **End of phase**: Quality gate report + next phase kickoff

---

## Appendix: Deliverables Index

### Phase 1-3: Planning (COMPLETE)
- [x] `docs/MCP_2025-11-25_SPECIFICATION_GAPS.md`
- [x] `docs/MCP_2025-11-25_PSEUDOCODE_DESIGN.md`
- [x] `docs/MCP_2025-11-25_ARCHITECTURE_DESIGN.md`
- [x] `BENCHMARK_CONSOLIDATION_ROADMAP.md`

### Phase 4: Implementation (IN PROGRESS)
- [ ] `apps/erlmcp_core/src/erlmcp_task_manager.erl`
- [ ] `apps/erlmcp_core/src/erlmcp_task_manager_sup.erl`
- [ ] `apps/erlmcp_core/src/erlmcp_cancellation.erl` (enhanced)
- [ ] `apps/erlmcp_core/src/erlmcp_completion.erl`
- [ ] `apps/erlmcp_core/src/erlmcp_sampling.erl`
- [ ] `apps/erlmcp_core/src/erlmcp_elicitation.erl`
- [ ] `bench/erlmcp_bench_metrology.erl`
- [ ] `src/erlmcp_benchmark.erl`
- [ ] `src/erlmcp_benchmark_baseline.erl`
- [ ] `src/erlmcp_benchmark_results.erl`

### Phase 5: Release (PENDING)
- [ ] PR description (comprehensive)
- [ ] Release notes (v0.7.0)
- [ ] Migration guide (v0.6.0 → v0.7.0)
- [ ] CHANGELOG.md update
- [ ] Git tag: v0.7.0
- [ ] GitHub release with artifacts

---

## Document History

| Version | Date | Author | Changes |
|---------|------|--------|---------|
| 1.0 | 2026-01-30 | sparc-orchestrator | Initial SPARC execution plan |

---

**Status**: Phase 1-3 Complete, Phase 4 Ready to Begin
**Next Milestone**: Week 4 - Task Management Implementation
**Owner**: erlang-otp-developer
**Review Date**: End of Week 5 (Task Manager complete)

https://claude.ai/code/session_018igga7R3JKL2TCSDqy27ro
