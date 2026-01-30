# SPARC Methodology Roadmap: erlmcp v3 Rewrite

**Project**: erlmcp (Erlang/OTP MCP SDK)
**Version**: 0.6.0 → 0.7.0 (v3 Compliance Release)
**Target**: MCP 2025-11-25 Specification Full Compliance
**Current Status**: 78% Compliant (15/19 capabilities)
**Target Status**: 100% Compliant (19/19 capabilities)
**Methodology**: SPARC (Specification → Pseudocode → Architecture → Refinement → Completion)
**Date**: 2026-01-30
**Branch**: claude/mcp-compliance-erlmcp-v3-Faf8c

---

## Executive Summary

This roadmap orchestrates the SPARC methodology across all 5 phases to achieve full MCP 2025-11-25 specification compliance. The v3 rewrite focuses on implementing 6 major feature gaps through systematic specification, design, implementation, testing, and delivery.

**Compliance Progression:**
- **Current**: 78% (15/19 capabilities)
- **After Phase 1-3**: 85% (specification + architecture complete)
- **After Phase 4**: 95% (implementation + tests complete)
- **After Phase 5**: 100% (production release)

**Timeline**: 3 weeks (18-24 hours focused development)

---

## SPARC Phase Overview

```
┌─────────────────────────────────────────────────────────────────┐
│ PHASE 1: SPECIFICATION (Week 1, Days 1-2)                       │
│ - Requirements gathering ✅ COMPLETE                             │
│ - Gap analysis ✅ COMPLETE                                       │
│ - API contracts defined ✅ COMPLETE                              │
│ - Edge cases documented ✅ COMPLETE                              │
│ Output: /home/user/erlmcp/docs/MCP_2025-11-25_SPECIFICATION_GAPS.md │
└─────────────────────────────────────────────────────────────────┘
           ↓
┌─────────────────────────────────────────────────────────────────┐
│ PHASE 2: PSEUDOCODE (Week 1, Days 3-4)                          │
│ - Algorithm design (task state machine) 🔄 IN PROGRESS          │
│ - Data structure design (completion ranking) ⏳ PENDING         │
│ - Flow control (elicitation lifecycle) ⏳ PENDING               │
│ Output: /home/user/erlmcp/docs/MCP_2025-11-25_PSEUDOCODE_DESIGN.md │
└─────────────────────────────────────────────────────────────────┘
           ↓
┌─────────────────────────────────────────────────────────────────┐
│ PHASE 3: ARCHITECTURE (Week 1, Days 5-7)                        │
│ - Supervision tree design ✅ COMPLETE                            │
│ - Module decomposition ✅ COMPLETE                               │
│ - Failure modes analysis ✅ COMPLETE                             │
│ Output: /home/user/erlmcp/docs/MCP_2025-11-25_ARCHITECTURE_DESIGN.md │
└─────────────────────────────────────────────────────────────────┘
           ↓
┌─────────────────────────────────────────────────────────────────┐
│ PHASE 4: REFINEMENT (Week 2-3, Days 8-18)                       │
│ - TDD implementation (Chicago School) ⏳ PENDING                 │
│ - Comprehensive testing ⏳ PENDING                               │
│ - Performance benchmarking ⏳ PENDING                            │
│ Output: Production-ready code with ≥80% test coverage           │
└─────────────────────────────────────────────────────────────────┘
           ↓
┌─────────────────────────────────────────────────────────────────┐
│ PHASE 5: COMPLETION (Week 3, Days 19-21)                        │
│ - Quality validation (Dialyzer, Xref) ⏳ PENDING                 │
│ - Code review ⏳ PENDING                                         │
│ - PR creation ⏳ PENDING                                         │
│ - Release preparation ⏳ PENDING                                 │
│ Output: Merged PR, v0.7.0 release, 100% MCP compliance          │
└─────────────────────────────────────────────────────────────────┘
```

---

## Phase 1: Specification ✅ COMPLETE

**Status**: ✅ Complete (100%)
**Effort**: 4-6 hours (COMPLETED)
**Owner**: plan-designer + erlang-researcher
**Deliverables**: Requirements documents, API contracts, edge cases

### Completed Artifacts

1. ✅ **Gap Analysis Document**
   `/home/user/erlmcp/docs/MCP_2025-11-25_SPECIFICATION_GAPS.md`
   - 6 major feature gaps identified
   - API contracts defined for all methods
   - Error codes specified
   - Performance requirements established

2. ✅ **Compliance Roadmap**
   `/home/user/erlmcp/docs/MCP_COMPLIANCE_ROADMAP.md`
   - 3-phase implementation plan
   - Task dependencies mapped
   - Risk assessment completed
   - Success metrics defined

3. ✅ **Feature Matrix**
   Multiple compliance analysis documents created
   - Current: 78% compliance (15/19 capabilities)
   - Gaps: 4 partial, 2 not implemented

### Specification Outputs

#### Feature Gap #1: Task Management (CRITICAL)
- **Methods**: tasks/create, tasks/list, tasks/get, tasks/cancel, tasks/result
- **State Machine**: pending → working → completed/failed/cancelled
- **Error Codes**: -32081 (not found), -32082 (already exists), -32087 (limit exceeded)
- **Performance**: <10ms create, <50ms retrieve, 10K+ concurrent tasks

#### Feature Gap #2: Request Cancellation (HIGH)
- **Methods**: requests/cancel
- **Notifications**: notifications/cancelled
- **Integration**: erlmcp_cancellation.erl (already exists, needs wiring)
- **Edge Cases**: Cancel completed, invalid ID, concurrent cancellation

#### Feature Gap #3: Progress Tokens (HIGH)
- **Status**: Partially implemented (erlmcp_progress.erl exists)
- **Missing**: _meta.progressToken extraction, cleanup on completion
- **Integration**: Wire to erlmcp_server request handling

#### Feature Gap #4: Sampling Capability (MEDIUM)
- **Methods**: sampling/createMessage
- **Missing**: Model preferences, multimodal content, provider abstraction
- **Status**: erlmcp_sampling.erl exists but incomplete

#### Feature Gap #5: Completions API (HIGH)
- **Methods**: completion/complete
- **Use Cases**: Resource paths, tool arguments, command names
- **Algorithm**: Fuzzy matching, relevance ranking, caching
- **Performance**: <100ms response time

#### Feature Gap #6: Elicitation Features (MEDIUM)
- **Methods**: elicitation/create, notifications/elicitation/complete
- **Security**: HTTPS validation, rate limiting, audit logging
- **Lifecycle**: Create → waiting → completed/expired

### Acceptance Criteria (Phase 1)
- [x] All feature gaps documented with API contracts
- [x] Error codes defined for all new methods
- [x] Performance requirements specified
- [x] Edge cases identified and documented
- [x] Security requirements defined
- [x] Integration points mapped

---

## Phase 2: Pseudocode 🔄 IN PROGRESS

**Status**: 🔄 Partial (40%)
**Effort**: 4-6 hours (ESTIMATED)
**Owner**: plan-designer
**Deliverables**: Algorithm designs, data structure specs, flow diagrams

### Work Completed

1. ✅ **Partial Pseudocode Document**
   `/home/user/erlmcp/docs/MCP_2025-11-25_PSEUDOCODE_DESIGN.md`
   - Task state machine algorithm outlined
   - Basic flow control described
   - Some data structures defined

### Remaining Work

#### Task 2.1: Complete Task Management Algorithm Design
**Effort**: 1-2 hours
**Output**: Detailed pseudocode for all task operations

```pseudocode
FUNCTION create_task(action, metadata)
  task_id = generate_uuid()

  IF concurrent_tasks >= max_concurrent_tasks THEN
    RETURN error(MCP_ERROR_MAX_CONCURRENT_TASKS)
  END IF

  task = {
    id: task_id,
    status: pending,
    action: action,
    metadata: metadata,
    created_at: now(),
    updated_at: now()
  }

  store_task(task)
  emit_event(task_created, task)

  SPAWN execute_task_async(task_id)

  RETURN {ok, task_id}
END FUNCTION

FUNCTION execute_task_async(task_id)
  update_task_status(task_id, working)

  TRY
    result = execute_task_logic(task_id)
    update_task_result(task_id, result, completed)
    emit_event(task_completed, task_id)
  CATCH error
    update_task_error(task_id, error, failed)
    emit_event(task_failed, task_id)
  END TRY
END FUNCTION

FUNCTION cancel_task(task_id, reason)
  task = get_task(task_id)

  IF task == not_found THEN
    RETURN error(MCP_ERROR_TASK_NOT_FOUND)
  END IF

  IF task.status IN [completed, failed, cancelled] THEN
    RETURN error(MCP_ERROR_TASK_ALREADY_COMPLETED)
  END IF

  send_cancellation_signal(task_id, reason)
  update_task_status(task_id, cancelled)
  emit_event(task_cancelled, task_id)

  RETURN {ok, cancelled}
END FUNCTION
```

#### Task 2.2: Completion Ranking Algorithm Design
**Effort**: 1-2 hours
**Output**: Pseudocode for completion generation and ranking

```pseudocode
FUNCTION complete(ref, argument)
  cache_key = hash(ref, argument)

  IF cache_hit(cache_key) THEN
    RETURN cached_result
  END IF

  candidates = generate_candidates(ref, argument)
  ranked = rank_by_relevance(candidates, argument)
  top_n = take(ranked, 100)

  cache_result(cache_key, top_n, ttl=300)

  RETURN {ok, {values: top_n, total: length(candidates), has_more: length(candidates) > 100}}
END FUNCTION

FUNCTION rank_by_relevance(candidates, partial_value)
  scored = []

  FOR EACH candidate IN candidates DO
    score = 0.0

    # Fuzzy match score (0.0 - 1.0)
    fuzzy_score = levenshtein_similarity(candidate, partial_value)
    score += fuzzy_score * 0.4

    # Frequency score (normalized)
    frequency = get_usage_count(candidate)
    score += normalize(frequency) * 0.3

    # Recency score (exponential decay)
    last_used = get_last_used_timestamp(candidate)
    recency = exp_decay(now() - last_used)
    score += recency * 0.3

    scored.append({candidate, score})
  END FOR

  RETURN sort_by_score_desc(scored)
END FUNCTION
```

#### Task 2.3: Elicitation Lifecycle Design
**Effort**: 1 hour
**Output**: Pseudocode for elicitation creation and expiry

```pseudocode
FUNCTION create_elicitation(requests, client_pid)
  elicitations = []

  FOR EACH request IN requests DO
    elicitation_id = generate_uuid()
    url = generate_elicitation_url(elicitation_id, request.name)

    elicitation = {
      id: elicitation_id,
      type: request.type,
      name: request.name,
      description: request.description,
      url: url,
      client_pid: client_pid,
      status: waiting,
      created_at: now(),
      expires_at: now() + expiry_ttl
    }

    store_elicitation(elicitation)
    start_expiry_timer(elicitation_id, expiry_ttl)

    elicitations.append({type: url, name: request.name, url: url})
  END FOR

  RETURN {ok, elicitations}
END FUNCTION

FUNCTION handle_expiry_timeout(elicitation_id)
  elicitation = get_elicitation(elicitation_id)

  IF elicitation.status == waiting THEN
    update_elicitation_status(elicitation_id, expired)
    notify_client(elicitation.client_pid, elicitation_expired, elicitation_id)
    schedule_cleanup(elicitation_id, 60_seconds)
  END IF
END FUNCTION

FUNCTION complete_elicitation(elicitation_id, value)
  elicitation = get_elicitation(elicitation_id)

  IF elicitation == not_found THEN
    RETURN error(not_found)
  END IF

  IF NOT validate_url(value) THEN
    RETURN error(invalid_url)
  END IF

  update_elicitation_value(elicitation_id, value, completed)
  cancel_expiry_timer(elicitation_id)
  notify_client(elicitation.client_pid, elicitation_completed, {elicitation_id, value})
  schedule_cleanup(elicitation_id, 60_seconds)

  RETURN {ok, completed}
END FUNCTION
```

### Acceptance Criteria (Phase 2)
- [ ] Task management algorithm fully defined in pseudocode
- [ ] Completion ranking algorithm specified with weights
- [ ] Elicitation lifecycle documented step-by-step
- [ ] Data structures defined for all operations
- [ ] State transitions documented for all state machines
- [ ] Edge case handling specified in pseudocode

---

## Phase 3: Architecture ✅ COMPLETE

**Status**: ✅ Complete (100%)
**Effort**: 6-8 hours (COMPLETED)
**Owner**: erlang-architect + erlang-otp-developer
**Deliverables**: Supervision tree, module design, failure analysis

### Completed Artifacts

1. ✅ **Architecture Design Document**
   `/home/user/erlmcp/docs/MCP_2025-11-25_ARCHITECTURE_DESIGN.md`
   - Supervision tree with fault isolation
   - 3 new modules specified
   - Data flow diagrams created
   - Performance considerations documented
   - Security analysis completed

### Architecture Highlights

#### Supervision Tree
```
erlmcp_core_sup (one_for_all)
├── erlmcp_server_sup (simple_one_for_one)
│   ├── erlmcp_server (per connection)
│   └── erlmcp_connection_manager
├── erlmcp_task_manager_sup (one_for_one)
│   └── erlmcp_task_manager (singleton) ← NEW
├── erlmcp_completion_sup (one_for_one)
│   └── erlmcp_completion (singleton) ← NEW
├── erlmcp_elicitation_sup (one_for_one)
│   └── erlmcp_elicitation (singleton) ← NEW
├── erlmcp_cancellation_sup (one_for_one)
│   └── erlmcp_cancellation (existing, enhanced)
├── erlmcp_progress_sup (one_for_one)
│   └── erlmcp_progress (existing, enhanced)
└── erlmcp_sampling_sup (one_for_one)
    └── erlmcp_sampling (existing, enhanced)
```

#### Module Specifications

**erlmcp_task_manager.erl** (NEW)
- Behavior: gen_server
- State: ETS table for tasks, Mnesia for persistence
- API: create_task/2, list_tasks/2, get_task/1, cancel_task/2, get_task_result/1
- Concurrency: Spawn separate process per task execution

**erlmcp_completion.erl** (NEW)
- Behavior: gen_server
- State: ETS cache with read_concurrency
- API: complete/2, invalidate_cache/1
- Performance: <100ms response, LRU eviction

**erlmcp_elicitation.erl** (NEW)
- Behavior: gen_server
- State: In-memory map for active elicitations
- API: create/2, complete/2, get_elicitation/1
- Lifecycle: Timer wheel for efficient expiry management

### Acceptance Criteria (Phase 3)
- [x] Supervision tree designed with proper restart strategies
- [x] Module responsibilities clearly defined
- [x] API interfaces specified for all new modules
- [x] Failure modes analyzed with recovery strategies
- [x] Performance bottlenecks identified with mitigations
- [x] Security considerations documented

---

## Phase 4: Refinement ⏳ PENDING

**Status**: ⏳ Not Started (0%)
**Effort**: 12-16 hours (ESTIMATED)
**Owner**: erlang-otp-developer + erlang-test-engineer + erlang-performance
**Deliverables**: Production code, comprehensive tests, benchmarks

### Implementation Plan

#### Week 2: Core Implementation (Days 8-14)

##### Task 4.1: Implement erlmcp_task_manager.erl
**Effort**: 4-5 hours
**Owner**: erlang-otp-developer
**TDD Approach**: Chicago School (no mocks, real processes)

**Test-First Sequence:**
1. Write test: `task_creation_generates_unique_id_test/0`
2. Implement: `create_task/2` minimal
3. Write test: `task_state_transitions_correctly_test/0`
4. Implement: State machine transitions
5. Write test: `concurrent_task_limit_enforced_test/0`
6. Implement: Limit checking
7. Write test: `task_persistence_survives_restart_test/0`
8. Implement: Mnesia integration

**Files to Create:**
- `/home/user/erlmcp/src/erlmcp_task_manager.erl`
- `/home/user/erlmcp/test/erlmcp_task_manager_tests.erl` (EUnit)
- `/home/user/erlmcp/test/erlmcp_task_manager_SUITE.erl` (CT)

**Acceptance Criteria:**
- [ ] All task methods implemented (create, list, get, cancel, result)
- [ ] State machine transitions validated
- [ ] Concurrent task limit enforced
- [ ] Mnesia persistence working
- [ ] ≥80% code coverage
- [ ] 0 Dialyzer warnings

##### Task 4.2: Implement erlmcp_completion.erl
**Effort**: 3-4 hours
**Owner**: erlang-otp-developer

**Test-First Sequence:**
1. Write test: `completion_generation_basic_test/0`
2. Implement: `complete/2` with stub ranking
3. Write test: `completion_cache_hit_test/0`
4. Implement: ETS cache
5. Write test: `completion_ranking_by_relevance_test/0`
6. Implement: Ranking algorithm
7. Write test: `completion_fuzzy_matching_test/0`
8. Implement: Levenshtein distance

**Files to Create:**
- `/home/user/erlmcp/src/erlmcp_completion.erl`
- `/home/user/erlmcp/test/erlmcp_completion_tests.erl` (EUnit)

**Acceptance Criteria:**
- [ ] Completion generation working for all ref types
- [ ] Ranking algorithm implemented with configurable weights
- [ ] Cache with LRU eviction functional
- [ ] Fuzzy matching supports typo tolerance
- [ ] <100ms response time (p99)
- [ ] ≥80% code coverage

##### Task 4.3: Implement erlmcp_elicitation.erl
**Effort**: 2-3 hours
**Owner**: erlang-otp-developer

**Test-First Sequence:**
1. Write test: `elicitation_creation_generates_url_test/0`
2. Implement: `create/2` with URL generation
3. Write test: `elicitation_expiry_timeout_test/0`
4. Implement: Timer management
5. Write test: `elicitation_url_validation_test/0`
6. Implement: URL scheme validation
7. Write test: `elicitation_rate_limiting_test/0`
8. Implement: Rate limiter

**Files to Create:**
- `/home/user/erlmcp/src/erlmcp_elicitation.erl`
- `/home/user/erlmcp/test/erlmcp_elicitation_tests.erl` (EUnit)

**Acceptance Criteria:**
- [ ] Elicitation creation with URL generation
- [ ] Expiry timer management efficient (timer wheel)
- [ ] URL validation (HTTPS required)
- [ ] Rate limiting per client
- [ ] ≥80% code coverage

##### Task 4.4: Enhance Existing Modules
**Effort**: 2-3 hours
**Owner**: erlang-otp-developer

**Files to Modify:**
- `/home/user/erlmcp/src/erlmcp_server.erl` - Add method handlers
- `/home/user/erlmcp/src/erlmcp_client.erl` - Add API functions
- `/home/user/erlmcp/src/erlmcp_json_rpc.erl` - Add encoders/decoders
- `/home/user/erlmcp/src/erlmcp_cancellation.erl` - Wire to tasks
- `/home/user/erlmcp/src/erlmcp_progress.erl` - Add _meta extraction

**Acceptance Criteria:**
- [ ] All new methods registered in erlmcp_server
- [ ] Client API functions implemented
- [ ] JSON-RPC encoding/decoding complete
- [ ] Integration tests passing
- [ ] 0 xref warnings

#### Week 3: Testing & Optimization (Days 15-18)

##### Task 4.5: Comprehensive Test Suite
**Effort**: 3-4 hours
**Owner**: erlang-test-engineer

**Test Coverage Targets:**
- **Unit Tests (EUnit)**: ≥80% line coverage per module
- **Integration Tests (CT)**: 15+ end-to-end scenarios
- **Property Tests (Proper)**: 5+ properties verified

**Test Suites to Create:**
```
test/
├── erlmcp_task_manager_tests.erl      (30+ test cases)
├── erlmcp_completion_tests.erl        (20+ test cases)
├── erlmcp_elicitation_tests.erl       (15+ test cases)
├── erlmcp_mcp_2025_SUITE.erl          (15+ integration tests)
└── erlmcp_v3_properties.erl           (5+ property tests)
```

**Property-Based Tests (Proper):**
```erlang
%% Task ID uniqueness property
prop_task_ids_unique() ->
    ?FORALL(Actions, list(binary()),
        begin
            TaskIds = [create_task(A) || A <- Actions],
            length(TaskIds) == length(lists:usort(TaskIds))
        end).

%% Completion ranking stability property
prop_completion_ranking_stable() ->
    ?FORALL({Candidates, Partial}, {list(binary()), binary()},
        begin
            Ranked1 = rank(Candidates, Partial),
            Ranked2 = rank(Candidates, Partial),
            Ranked1 == Ranked2
        end).
```

**Acceptance Criteria:**
- [ ] ≥80% overall code coverage (per CLAUDE.md)
- [ ] 100% test pass rate
- [ ] 15+ integration test scenarios
- [ ] 5+ property-based tests
- [ ] Edge cases covered (cancellation, timeouts, errors)

##### Task 4.6: Performance Benchmarking
**Effort**: 2-3 hours
**Owner**: erlang-performance

**Benchmarks to Create:**
```
bench/
├── erlmcp_bench_task_manager.erl
│   ├── task_creation_throughput      (target: >1000 tasks/sec)
│   ├── task_listing_pagination       (target: <50ms p99)
│   └── concurrent_task_execution     (target: 10K concurrent)
├── erlmcp_bench_completion.erl
│   ├── completion_generation         (target: <100ms p99)
│   ├── cache_hit_performance         (target: <5ms p99)
│   └── ranking_algorithm             (target: <50ms for 1000 candidates)
└── erlmcp_bench_elicitation.erl
    ├── elicitation_creation          (target: <20ms p99)
    └── timer_wheel_efficiency        (target: 100K timers, <10% overhead)
```

**Metrology Compliance:**
All benchmarks output canonical units per `docs/metrology/METRICS_GLOSSARY.md`:
- `throughput_tasks_per_s`
- `latency_p50_us`, `latency_p95_us`, `latency_p99_us`
- `memory_heap_mib_per_task`

**Performance Baseline Validation:**
```bash
# Run quick benchmarks
make benchmark-quick

# Validate against baseline (must not regress >10%)
./scripts/bench/validate_baseline.sh
```

**Acceptance Criteria:**
- [ ] All benchmarks meet performance targets
- [ ] <10% regression vs baseline (per CLAUDE.md)
- [ ] Metrology units canonical (no ambiguities)
- [ ] Results documented in benchmark reports

### Quality Gates (Mandatory Before "Done")

Per `/home/user/erlmcp/CLAUDE.md`, MUST execute and report:

#### 1. Compilation
```bash
TERM=dumb rebar3 compile
```
**Target**: 0 errors, 0 warnings

#### 2. Tests
```bash
rebar3 eunit
rebar3 ct
rebar3 proper
```
**Target**: 100% pass rate, ≥80% coverage

#### 3. Static Analysis
```bash
rebar3 dialyzer
rebar3 xref
```
**Target**: 0 warnings, 0 issues

#### 4. Benchmarks
```bash
make benchmark-quick
```
**Target**: <10% regression vs baseline

### Acceptance Criteria (Phase 4)
- [ ] 3 new modules implemented (task_manager, completion, elicitation)
- [ ] All existing modules enhanced with new methods
- [ ] ≥80% test coverage across all modules
- [ ] 0 compilation errors/warnings
- [ ] 0 Dialyzer warnings
- [ ] 0 Xref issues
- [ ] <10% performance regression
- [ ] All benchmarks meet targets
- [ ] Property tests validate key invariants

---

## Phase 5: Completion ⏳ PENDING

**Status**: ⏳ Not Started (0%)
**Effort**: 4-6 hours (ESTIMATED)
**Owner**: code-reviewer + erlang-github-ops
**Deliverables**: Merged PR, v0.7.0 release, 100% MCP compliance

### Completion Checklist

#### Task 5.1: Quality Validation
**Effort**: 1-2 hours
**Owner**: code-reviewer

**Pre-Merge Checklist:**
```bash
# 1. Full compilation
TERM=dumb rebar3 compile
# Verify: 0 errors, 0 warnings

# 2. Complete test suite
rebar3 eunit && rebar3 ct && rebar3 proper
# Verify: 100% pass rate

# 3. Coverage report
rebar3 cover
# Verify: ≥80% coverage

# 4. Dialyzer
rebar3 dialyzer
# Verify: 0 warnings

# 5. Xref
rebar3 xref
# Verify: 0 undefined function calls

# 6. Benchmark validation
make benchmark-quick
# Verify: <10% regression

# 7. Format check
rebar3 format --verify
# Verify: All files formatted
```

**Acceptance Criteria:**
- [ ] All quality gates pass (compilation, tests, coverage, dialyzer, xref)
- [ ] Code formatted per rebar3_format
- [ ] No performance regressions
- [ ] Documentation updated

#### Task 5.2: Code Review
**Effort**: 1-2 hours
**Owner**: code-reviewer

**Review Checklist:**
- [ ] OTP principles followed (gen_server, supervision)
- [ ] Error handling comprehensive (all error codes used)
- [ ] Security validated (input validation, rate limiting)
- [ ] Performance acceptable (benchmarks meet targets)
- [ ] Tests comprehensive (unit, integration, property)
- [ ] Documentation complete (API, protocol, examples)
- [ ] No hardcoded values (all config in sys.config)
- [ ] Logging appropriate (info, warning, error levels)

**Review Artifacts:**
- Code review comments in GitHub PR
- Architecture validation report
- Security review sign-off

#### Task 5.3: PR Creation and Merge
**Effort**: 1 hour
**Owner**: erlang-github-ops

**PR Preparation:**
```bash
# 1. Ensure branch is up to date
git checkout claude/mcp-compliance-erlmcp-v3-Faf8c
git pull origin main
git merge main

# 2. Run full validation
make check

# 3. Create PR
gh pr create --title "feat: MCP 2025-11-25 full compliance (v0.7.0)" \
  --body "$(cat <<'EOF'
## Summary
- Implement task management (tasks/create, tasks/list, tasks/get, tasks/cancel, tasks/result)
- Implement completions API (completion/complete)
- Implement elicitation features (elicitation/create, notifications/elicitation/complete)
- Enhance progress tokens and cancellation support
- Achieve 100% MCP 2025-11-25 specification compliance

## Changes
- **New Modules**: erlmcp_task_manager.erl, erlmcp_completion.erl, erlmcp_elicitation.erl
- **Enhanced Modules**: erlmcp_server.erl, erlmcp_client.erl, erlmcp_json_rpc.erl
- **Tests**: 65+ new test cases, ≥80% coverage
- **Benchmarks**: 3 new benchmark suites
- **Documentation**: Updated API docs, protocol docs, examples

## Testing
- [x] Unit tests: 100% pass (65+ cases)
- [x] Integration tests: 100% pass (15+ scenarios)
- [x] Property tests: 100% pass (5+ properties)
- [x] Benchmarks: <10% regression
- [x] Coverage: ≥80%

## Quality Gates
- [x] Compilation: 0 errors, 0 warnings
- [x] Dialyzer: 0 warnings
- [x] Xref: 0 issues
- [x] Format: All files formatted
- [x] Code review: Approved by 2+ reviewers

## Compliance
- Before: 78% (15/19 capabilities)
- After: 100% (19/19 capabilities) ✅

https://claude.ai/code/session_01H4fCu86xtPU1W8uykAQJPG
EOF
)"

# 4. Wait for CI/CD to pass
# 5. Merge PR
gh pr merge --squash --auto
```

**Acceptance Criteria:**
- [ ] PR created with detailed summary
- [ ] CI/CD passes all checks
- [ ] 2+ code reviews approved
- [ ] PR merged to main branch

#### Task 5.4: Release Preparation
**Effort**: 1 hour
**Owner**: erlang-github-ops

**Release Checklist:**
1. **Version Bump**
   ```bash
   # Update version in src/erlmcp.app.src
   {vsn, "0.7.0"}
   ```

2. **CHANGELOG Update**
   ```markdown
   ## [0.7.0] - 2026-02-XX

   ### Added
   - Task management API (tasks/create, tasks/list, tasks/get, tasks/cancel, tasks/result)
   - Completions API (completion/complete)
   - Elicitation features (elicitation/create, notifications/elicitation/complete)
   - Enhanced progress tokens support
   - Enhanced cancellation support

   ### Changed
   - MCP compliance: 78% → 100%
   - Test coverage: 72% → 85%

   ### Performance
   - Task creation: <10ms (p99)
   - Completion generation: <100ms (p99)
   - No regression vs v0.6.0
   ```

3. **Release Build**
   ```bash
   # Create production release
   rebar3 as prod release
   rebar3 as prod tar

   # Create release notes
   gh release create v0.7.0 \
     --title "v0.7.0 - Full MCP 2025-11-25 Compliance" \
     --notes-file RELEASE_NOTES.md \
     _build/prod/rel/erlmcp/erlmcp-0.7.0.tar.gz
   ```

4. **Documentation Deployment**
   ```bash
   # Update docs site
   make docs
   ./scripts/deploy_docs.sh v0.7.0
   ```

**Acceptance Criteria:**
- [ ] Version bumped to 0.7.0
- [ ] CHANGELOG updated
- [ ] Release created on GitHub
- [ ] Release artifacts uploaded
- [ ] Documentation deployed
- [ ] Announcement prepared

### Success Metrics (Phase 5)

**Compliance:**
- ✅ 100% MCP 2025-11-25 specification compliance (19/19 capabilities)

**Quality:**
- ✅ 0 compilation errors
- ✅ 0 Dialyzer warnings
- ✅ 0 Xref issues
- ✅ ≥80% test coverage
- ✅ 100% test pass rate

**Performance:**
- ✅ <10% regression vs baseline
- ✅ Task creation <10ms (p99)
- ✅ Completion generation <100ms (p99)
- ✅ Elicitation creation <20ms (p99)

**Delivery:**
- ✅ PR merged to main
- ✅ v0.7.0 release published
- ✅ Documentation updated
- ✅ Announcement sent

---

## SPARC Orchestration Matrix

| Phase | Owner | Status | Effort | Deliverables | Dependencies |
|-------|-------|--------|--------|--------------|--------------|
| **1. Specification** | plan-designer + erlang-researcher | ✅ Complete | 4-6h | Requirements docs, API contracts | None |
| **2. Pseudocode** | plan-designer | 🔄 Partial | 4-6h | Algorithm designs, flow diagrams | Phase 1 |
| **3. Architecture** | erlang-architect + erlang-otp-developer | ✅ Complete | 6-8h | Supervision tree, module design | Phase 2 |
| **4. Refinement** | erlang-otp-developer + erlang-test-engineer + erlang-performance | ⏳ Pending | 12-16h | Production code, tests, benchmarks | Phase 3 |
| **5. Completion** | code-reviewer + erlang-github-ops | ⏳ Pending | 4-6h | Merged PR, v0.7.0 release | Phase 4 |
| **TOTAL** | All agents | 40% | 30-42h | 100% MCP compliance | Sequential |

---

## Agent Delegation Strategy

### Phase-Specific Agents

**Phase 1: Specification**
- **Primary**: plan-designer (requirements gathering, API design)
- **Secondary**: erlang-researcher (codebase analysis, pattern research)
- **Output**: Specification documents validated by both agents

**Phase 2: Pseudocode**
- **Primary**: plan-designer (algorithm design, flow control)
- **Review**: erlang-architect (architecture feasibility check)
- **Output**: Pseudocode reviewed for OTP compatibility

**Phase 3: Architecture**
- **Primary**: erlang-architect (supervision tree, module decomposition)
- **Secondary**: erlang-otp-developer (OTP pattern validation)
- **Output**: Architecture design validated for production readiness

**Phase 4: Refinement**
- **Primary**: erlang-otp-developer (TDD implementation)
- **Secondary**: erlang-test-engineer (comprehensive testing)
- **Tertiary**: erlang-performance (benchmarking, optimization)
- **Output**: Production-ready code with tests and benchmarks

**Phase 5: Completion**
- **Primary**: code-reviewer (quality validation, approval)
- **Secondary**: erlang-github-ops (PR creation, release management)
- **Output**: Merged PR and released v0.7.0

### Agent Coordination Protocol

```
SPARC Orchestrator
    ↓
┌───────────────────────────────────────────────────────┐
│ Phase 1: Specification                                 │
│ orchestrator → plan-designer (requirements)            │
│ orchestrator → erlang-researcher (codebase analysis)   │
│ plan-designer + erlang-researcher → specification docs │
└───────────────────────────────────────────────────────┘
    ↓
┌───────────────────────────────────────────────────────┐
│ Phase 2: Pseudocode                                    │
│ orchestrator → plan-designer (algorithm design)        │
│ plan-designer → pseudocode docs                        │
│ erlang-architect → feasibility review                  │
└───────────────────────────────────────────────────────┘
    ↓
┌───────────────────────────────────────────────────────┐
│ Phase 3: Architecture                                  │
│ orchestrator → erlang-architect (system design)        │
│ erlang-architect → erlang-otp-developer (validation)   │
│ erlang-architect → architecture docs                   │
└───────────────────────────────────────────────────────┘
    ↓
┌───────────────────────────────────────────────────────┐
│ Phase 4: Refinement                                    │
│ orchestrator → erlang-otp-developer (implementation)   │
│ erlang-otp-developer → erlang-test-engineer (testing)  │
│ erlang-test-engineer → erlang-performance (benchmarks) │
│ All agents → production code + tests + benchmarks      │
└───────────────────────────────────────────────────────┘
    ↓
┌───────────────────────────────────────────────────────┐
│ Phase 5: Completion                                    │
│ orchestrator → code-reviewer (quality validation)      │
│ code-reviewer → erlang-github-ops (PR + release)       │
│ All agents → merged PR + v0.7.0 release                │
└───────────────────────────────────────────────────────┘
```

---

## Risk Management

### High-Risk Items

#### Risk 1: Task Persistence Complexity
**Impact**: High
**Probability**: Medium
**Mitigation**:
- Use Mnesia with proper schema migration
- Support both ETS (ephemeral) and Mnesia (persistent) backends
- Test restart scenarios thoroughly
- Document recovery procedures

#### Risk 2: Completion Ranking Quality
**Impact**: Medium
**Probability**: High
**Mitigation**:
- Implement feedback loop for relevance tuning
- Use property-based testing for ranking stability
- Allow configurable ranking weights
- Cache pre-computed scores

#### Risk 3: Performance Regression
**Impact**: Medium
**Probability**: Low
**Mitigation**:
- Benchmark before/after implementation
- Use profiling tools (fprof, eprof, recon)
- Optimize hot paths (ETS, spawning)
- Set performance gates in CI/CD

### Medium-Risk Items

#### Risk 4: Integration Test Complexity
**Impact**: Medium
**Probability**: Medium
**Mitigation**:
- Reuse existing test patterns from mcp_compliance_SUITE
- Use Common Test hooks for setup/teardown
- Mock external dependencies (LLM providers)
- Parallel test execution

#### Risk 5: Documentation Drift
**Impact**: Low
**Probability**: High
**Mitigation**:
- Update docs in same PR as code changes
- Use EDoc for inline documentation
- Validate examples in CI/CD
- Link docs to code via anchors

---

## Success Criteria

### Functional Success
- ✅ All 6 feature gaps implemented
- ✅ 100% MCP 2025-11-25 specification compliance
- ✅ All methods registered and functional
- ✅ Error handling comprehensive

### Quality Success
- ✅ 0 compilation errors/warnings
- ✅ 0 Dialyzer warnings
- ✅ 0 Xref issues
- ✅ ≥80% test coverage
- ✅ 100% test pass rate

### Performance Success
- ✅ <10% regression vs v0.6.0 baseline
- ✅ Task creation <10ms (p99)
- ✅ Completion generation <100ms (p99)
- ✅ Elicitation creation <20ms (p99)

### Delivery Success
- ✅ PR merged to main branch
- ✅ v0.7.0 release published
- ✅ Documentation updated and deployed
- ✅ Announcement sent to community

---

## Timeline and Milestones

### Week 1: Specification + Pseudocode + Architecture
**Days 1-7**: SPARC Phases 1-3
**Status**: ✅ Specification Complete, 🔄 Pseudocode Partial, ✅ Architecture Complete

**Milestones:**
- [x] Day 1-2: Requirements documented (Phase 1)
- [ ] Day 3-4: Pseudocode designed (Phase 2) ← **IN PROGRESS**
- [x] Day 5-7: Architecture designed (Phase 3)

### Week 2: Implementation + Testing
**Days 8-14**: SPARC Phase 4 (Part 1)
**Status**: ⏳ Pending

**Milestones:**
- [ ] Day 8-9: erlmcp_task_manager.erl implemented
- [ ] Day 10-11: erlmcp_completion.erl implemented
- [ ] Day 12: erlmcp_elicitation.erl implemented
- [ ] Day 13-14: Integration of new modules into erlmcp_server/client

### Week 3: Testing + Quality + Release
**Days 15-21**: SPARC Phase 4 (Part 2) + Phase 5
**Status**: ⏳ Pending

**Milestones:**
- [ ] Day 15-16: Comprehensive test suite (65+ tests)
- [ ] Day 17-18: Performance benchmarking
- [ ] Day 19: Quality validation (dialyzer, xref, coverage)
- [ ] Day 20: Code review and PR creation
- [ ] Day 21: Release v0.7.0

---

## Next Actions

### Immediate (Next 24 Hours)
1. **Complete Phase 2 (Pseudocode)**: 🔄 IN PROGRESS
   - Finish task management algorithm design
   - Complete completion ranking algorithm
   - Document elicitation lifecycle in detail
   - **Owner**: plan-designer
   - **Deliverable**: `/home/user/erlmcp/docs/MCP_2025-11-25_PSEUDOCODE_DESIGN.md` (complete)

### Short-Term (Next 3 Days)
2. **Begin Phase 4 (Refinement)**:
   - Create module stubs (erlmcp_task_manager.erl, erlmcp_completion.erl, erlmcp_elicitation.erl)
   - Write first test cases (TDD)
   - Implement minimal task creation
   - **Owner**: erlang-otp-developer
   - **Deliverable**: First passing tests

### Medium-Term (Week 2)
3. **Complete Implementation**:
   - Finish all 3 new modules
   - Enhance existing modules
   - Write comprehensive tests
   - **Owner**: erlang-otp-developer + erlang-test-engineer

### Long-Term (Week 3)
4. **Quality + Release**:
   - Benchmark performance
   - Validate quality gates
   - Code review
   - Create PR and release v0.7.0
   - **Owner**: erlang-performance + code-reviewer + erlang-github-ops

---

## References

### Documentation
- **Specification**: `/home/user/erlmcp/docs/MCP_2025-11-25_SPECIFICATION_GAPS.md`
- **Pseudocode**: `/home/user/erlmcp/docs/MCP_2025-11-25_PSEUDOCODE_DESIGN.md`
- **Architecture**: `/home/user/erlmcp/docs/MCP_2025-11-25_ARCHITECTURE_DESIGN.md`
- **Roadmap**: `/home/user/erlmcp/docs/MCP_COMPLIANCE_ROADMAP.md`
- **CLAUDE.md**: `/home/user/erlmcp/CLAUDE.md`

### MCP Specification
- **Official Spec**: https://modelcontextprotocol.io/specification/
- **Version**: 2025-11-25
- **Target Compliance**: 100% (19/19 capabilities)

### Agent Documentation
- **SPARC Methodology**: `/home/user/erlmcp/.claude/SPARC_QUICK_REFERENCE.md`
- **Agent Index**: `/home/user/erlmcp/.claude/AGENT_INDEX.md`
- **OTP Patterns**: `/home/user/erlmcp/docs/otp-patterns.md`

---

## Appendix: SPARC Phase Dependencies

```
Phase 1: Specification
    ↓ (Defines requirements for Phase 2)
Phase 2: Pseudocode
    ↓ (Provides algorithms for Phase 3)
Phase 3: Architecture
    ↓ (Establishes structure for Phase 4)
Phase 4: Refinement
    ↓ (Produces artifacts for Phase 5)
Phase 5: Completion
    ↓
v0.7.0 Release (100% MCP Compliance)
```

**Critical Path**: All phases sequential (cannot parallelize)
**Estimated Total Effort**: 30-42 hours over 3 weeks
**Current Progress**: 40% (Phases 1 + 3 complete)
**Remaining Effort**: 18-25 hours (Phases 2, 4, 5)

---

**Document Version**: 1.0.0
**Last Updated**: 2026-01-30
**Status**: Active Roadmap
**Next Review**: After Phase 2 completion (pseudocode)
