# Antipattern #6: Non-Idempotent Cloud Operations

**Date**: 2026-02-01
**Scope**: .claude/hooks/*.sh, scripts/*.sh, apps/erlmcp_*/src/*.erl
**Severity**: CRITICAL (Cloud deployment safety)

## Executive Summary

Non-idempotent operations cause failures when retried in cloud environments. This analysis identifies operations that produce different results when executed multiple times, violating the cloud safety principle: **∀command. idempotent(command) ∨ guarded(command)**.

**Key Findings**:
- ✅ Most operations are idempotent or properly guarded
- ⚠️ 12 non-idempotent patterns found (8 in benchmarks/tests, 4 in production code)
- 🔴 2 critical issues in production code (git push, ETS counter increments)
- 💡 Remediation strategies provided for all findings

---

## Table of Contents

1. [Non-Idempotent Patterns Found](#non-idempotent-patterns-found)
2. [Cloud Deployment Impact](#cloud-deployment-impact)
3. [Remediation Strategies](#remediation-strategies)
4. [Retry Safety Guarantees](#retry-safety-guarantees)
5. [Recommendations](#recommendations)

---

## Non-Idempotent Patterns Found

### 1. Git Push Operations Without Guards (CRITICAL)

**Location**: `scripts/release-cli.sh:289-290`

```bash
git push origin "$CURRENT_BRANCH" || echo -e "${YELLOW}⚠ Failed to push branch${NC}"
git push origin "$TAG_NAME" || echo -e "${YELLOW}⚠ Failed to push tag${NC}"
```

**Failure Scenario**:
1. First execution: Push succeeds
2. Network timeout occurs before command completes
3. Retry: Git push fails with "already up to date" or "tag already exists"
4. Cloud retry logic sees non-zero exit code → fails entire workflow

**Current Mitigation**: Uses `|| echo` to suppress errors (silent failure)

**Severity**: HIGH - Silent failures in release process can cause tag/branch inconsistencies

**Recommended Pattern**:
```bash
# Idempotent push (check remote state first)
if ! git ls-remote --exit-code origin "refs/tags/$TAG_NAME" &>/dev/null; then
    git push origin "$TAG_NAME" || {
        echo -e "${RED}✗ Failed to push tag${NC}"
        exit 1
    }
else
    echo -e "${BLUE}ℹ Tag $TAG_NAME already exists on remote (idempotent)${NC}"
fi
```

**Additional Locations**:
- `scripts/release-create-v0.6.0.sh:298-304` (3 push operations)
- `scripts/release-prepare-v0.6.0.sh:505-506` (documented, not executed)

---

### 2. ETS Counter Increments (CRITICAL - Production Code)

**Location**: `apps/erlmcp_core/src/erlmcp_tasks.erl:430`

```erlang
{{ok, TaskId}, State#state{task_count = State#state.task_count + 1}}.
```

**Failure Scenario**:
1. Process crashes after incrementing task_count but before persisting to ETS
2. Supervisor restarts process with old state
3. task_count is now inconsistent with actual task count

**Severity**: HIGH - Counter drift accumulates over time

**Recommended Pattern**:
```erlang
% Use ETS counter (atomic) instead of state counter
task_count() ->
    ets:update_counter(?TASKS_TABLE, task_counter, {2, 1}, {task_counter, 0}).

% In gen_server
{{ok, TaskId}, State}.  % Remove task_count from state
```

**Additional Locations**:
- `apps/erlmcp_transports/src/erlmcp_transport_ws.erl:696` (messages_pending counter)
- `apps/erlmcp_transports/src/erlmcp_pool_manager.erl:205` (failed_checkouts counter)
- `apps/erlmcp_core/src/erlmcp_schema_cache.erl:220-224` (cache_hits, cache_misses)
- `apps/erlmcp_observability/src/erlmcp_event_audit.erl:71-144` (event_count - 9 locations)
- `apps/erlmcp_observability/src/erlmcp_event_metrics.erl:99-159` (5 counters)
- `apps/erlmcp_observability/src/erlmcp_event_logger.erl:66-71` (event_count)

---

### 3. Non-Atomic ETS Counter Pattern (Race Condition)

**Location**: `bench/race_condition_bombardment.erl:131-134`

```erlang
case ets:lookup(?ETS_TABLE, count) of
    [{count, Current}] when Current >= 0 ->
        NewVal = Current + 1,
        ets:insert(?ETS_TABLE, {count, NewVal}),  % NON-ATOMIC
```

**Failure Scenario**:
1. Process A reads count=5
2. Process B reads count=5
3. Process A writes count=6
4. Process B writes count=6 (lost update!)
5. Expected: 7, Actual: 6

**Severity**: MEDIUM - This is a **benchmark/test** file (intentionally demonstrating race conditions), but the pattern exists in production code

**Recommended Pattern**:
```erlang
% Use atomic update_counter
ets:update_counter(?ETS_TABLE, count, {2, 1}, {count, 0})
% Returns: New value after increment (atomic)
```

**Additional Locations (Benchmarks/Tests)**:
- `bench/erlmcp_bench_race_conditions.erl:506` (test race conditions)
- `bench/race_test_report.erl:91` (test race conditions)
- `bench/race_test_quick.erl:67` (test race conditions)
- `apps/erlmcp_core/test/erlmcp_ets_race_condition_tests.erl:207` (test race conditions)
- `apps/erlmcp_core/test/erlmcp_phase2_integration_SUITE.erl:384` (test call counting)

**Note**: Most of these are **intentional** race condition tests, not production bugs.

---

### 4. Bash Counter Increments (Low Severity)

**Location**: `.claude/hooks/SessionStart.sh:142`

```bash
retry_count=$((retry_count + 1))
```

**Failure Scenario**:
1. Counter increments work correctly in bash (single-threaded)
2. However, if script is sourced multiple times concurrently, counter is per-shell

**Severity**: LOW - This is in local function scope, safe in practice

**Current State**: ACCEPTABLE (local scope, no concurrency)

**Additional Locations** (All in test scripts, counters, or reporting):
- `.claude/hooks/test_policy_bash.sh:26,43,49` (test counters - safe)
- `scripts/health_check.sh:95-96` (local counters - safe)
- `scripts/check_network_compliance.sh:38,55,84,129` (local counters - safe)
- `scripts/verify_ggen.sh:35-44` (local counters - safe)

---

### 5. ETS Insert Without Existence Check (Medium Severity)

**Location**: `apps/erlmcp_observability/src/erlmcp_receipt_chain.erl:47`

```erlang
ets:insert(?EVENT_TABLE, {EventId, EventWithId}),
```

**Failure Scenario**:
1. First call: Inserts {EventId, Event1}
2. Retry with same EventId: Overwrites with {EventId, Event2}
3. Result: Event1 lost, duplicate EventId

**Severity**: MEDIUM - Event loss possible on retry

**Recommended Pattern**:
```erlang
% Use insert_new for idempotency
case ets:insert_new(?EVENT_TABLE, {EventId, EventWithId}) of
    true -> ok;
    false -> {error, duplicate_event_id}
end
```

**Additional Locations**:
- `apps/erlmcp_observability/src/erlmcp_receipt_chain.erl:86` (same pattern)

---

### 6. File Write Operations (Generally Idempotent)

**Location**: `apps/erlmcp_core/src/erlmcp_secrets.erl:1424`

```erlang
file:write_file(State#state.storage_path, EncryptedData).
```

**Analysis**: `file:write_file/2` **overwrites** existing files → **IDEMPOTENT** ✅

**Severity**: NONE - These operations are safe (overwrite semantics)

**Locations Analyzed** (All idempotent):
- `apps/erlmcp_observability/src/erlmcp_profiler.erl:311` (flamegraph output)
- `apps/erlmcp_observability/src/erlmcp_audit_log.erl:530,535,539` (report generation)
- `apps/erlmcp_observability/src/erlmcp_evidence_path.erl:154,330` (evidence bundles)
- `apps/erlmcp_validation/src/erlmcp_compliance_report.erl:341,354,427,451` (reports)
- `apps/erlmcp_core/src/erlmcp_reproducer.erl:213` (reproducer scripts)

---

## Cloud Deployment Impact

### Retry Scenarios

Cloud environments retry operations on:
1. **Network timeouts** (30s default)
2. **Process crashes** (supervisor restart)
3. **Deployment failures** (rollback + retry)
4. **Load balancer failover** (request replay)

### Impact Analysis by Category

| Category | Impact | Risk Level | Frequency |
|----------|--------|------------|-----------|
| Git operations | Silent failures, inconsistent tags | HIGH | Rare (releases only) |
| ETS counters (prod) | Counter drift, stale metrics | HIGH | Continuous |
| ETS counters (test) | Test flakiness, false failures | LOW | Test runs only |
| State counters | Memory leaks, incorrect counts | MEDIUM | Continuous |
| File writes | None (idempotent) | NONE | N/A |

### Cloud-Specific Risks

**1. Git Push Failures**:
```
Scenario: Release workflow in cloud CI/CD
1. CI runner pushes tag v2.1.0
2. Network glitch → timeout
3. CI retries → "tag already exists" → fails pipeline
4. Release incomplete, manual intervention required
```

**2. Counter Drift**:
```
Scenario: High-traffic erlmcp server in cloud
1. Task server creates 1000 tasks → task_count = 1000
2. Server crashes, supervisor restarts
3. State recovered from last checkpoint → task_count = 950
4. Server continues → creates 50 more tasks
5. Reported: 1000 tasks, Actual: 1050 tasks (10 drift)
```

**3. Event Duplication**:
```
Scenario: Receipt chain in distributed cloud environment
1. Node A records event with ID=123456789
2. Network partition
3. Node A retries → overwrites event 123456789
4. Original event data lost
5. Audit trail broken
```

---

## Remediation Strategies

### Strategy 1: Idempotent Git Operations

**Current** (non-idempotent):
```bash
git push origin "$TAG_NAME" || echo "Failed"
```

**Recommended** (idempotent):
```bash
# Function: idempotent_git_push
# Returns: 0 (success or already exists), 1 (genuine failure)
idempotent_git_push() {
    local remote="$1"
    local refspec="$2"
    local ref_type="$3"  # "tag" or "branch"

    # Check if already exists on remote
    if git ls-remote --exit-code "$remote" "refs/${ref_type}s/$refspec" &>/dev/null; then
        echo "✓ $refspec already exists on $remote (idempotent)"
        return 0
    fi

    # Push with retry logic
    local retry_count=0
    local max_retries=3

    while [[ $retry_count -lt $max_retries ]]; do
        if git push "$remote" "$refspec"; then
            echo "✓ Pushed $refspec to $remote"
            return 0
        fi

        # Check if it appeared during retry (race condition)
        if git ls-remote --exit-code "$remote" "refs/${ref_type}s/$refspec" &>/dev/null; then
            echo "✓ $refspec appeared during push (idempotent)"
            return 0
        fi

        retry_count=$((retry_count + 1))
        [[ $retry_count -lt $max_retries ]] && sleep $((retry_count * 2))
    done

    echo "✗ Failed to push $refspec after $max_retries attempts"
    return 1
}

# Usage
idempotent_git_push origin "$TAG_NAME" tag || exit 1
```

**Implementation Locations**:
- `scripts/release-cli.sh:289-290`
- `scripts/release-create-v0.6.0.sh:298-304`

---

### Strategy 2: Atomic ETS Counters

**Current** (non-atomic):
```erlang
-record(state, {
    task_count = 0 :: non_neg_integer()
}).

handle_call(create_task, _From, State) ->
    % ... create task ...
    NewState = State#state{task_count = State#state.task_count + 1},
    {reply, ok, NewState}.
```

**Recommended** (atomic):
```erlang
-define(COUNTERS_TABLE, erlmcp_counters).

% In init/1
init([]) ->
    ets:new(?COUNTERS_TABLE, [named_table, public, set]),
    ets:insert(?COUNTERS_TABLE, {task_count, 0}),
    {ok, #state{}}.

% Atomic increment
handle_call(create_task, _From, State) ->
    % ... create task ...
    NewCount = ets:update_counter(?COUNTERS_TABLE, task_count, {2, 1}, {task_count, 0}),
    logger:debug("Task created, total: ~p", [NewCount]),
    {reply, ok, State}.

% Atomic read
get_task_count() ->
    case ets:lookup(?COUNTERS_TABLE, task_count) of
        [{task_count, Count}] -> Count;
        [] -> 0
    end.
```

**Benefits**:
- ✅ Atomic (no race conditions)
- ✅ Survives process restarts (if ETS table is public)
- ✅ No counter drift
- ✅ Thread-safe across multiple processes

**Implementation Locations**:
- `apps/erlmcp_core/src/erlmcp_tasks.erl:430`
- `apps/erlmcp_transports/src/erlmcp_transport_ws.erl:696`
- `apps/erlmcp_transports/src/erlmcp_pool_manager.erl:205`
- `apps/erlmcp_core/src/erlmcp_schema_cache.erl:220-224`
- `apps/erlmcp_observability/src/erlmcp_event_*.erl` (multiple files)

---

### Strategy 3: Idempotent Event Recording

**Current** (overwrites on retry):
```erlang
add_event(Event) ->
    EventId = erlang:system_time(microsecond),
    ets:insert(?EVENT_TABLE, {EventId, Event}),
    ok.
```

**Recommended** (idempotent with unique ID check):
```erlang
add_event(Event) ->
    % Generate deterministic ID from event content
    EventId = generate_event_id(Event),
    EventWithId = Event#{id => EventId, recorded_at => erlang:system_time(millisecond)},

    % Use insert_new (fails if exists)
    case ets:insert_new(?EVENT_TABLE, {EventId, EventWithId}) of
        true -> {ok, EventId};
        false -> {error, duplicate_event_id}
    end.

% Alternative: Use monotonic counter for guaranteed uniqueness
add_event_monotonic(Event) ->
    EventId = ets:update_counter(?EVENT_TABLE, event_id_counter, {2, 1}, {event_id_counter, 0}),
    EventWithId = Event#{id => EventId, recorded_at => erlang:system_time(millisecond)},
    ets:insert(?EVENT_TABLE, {EventId, EventWithId}),
    {ok, EventId}.
```

**Implementation Locations**:
- `apps/erlmcp_observability/src/erlmcp_receipt_chain.erl:47,86`

---

### Strategy 4: Test/Benchmark Annotations

For intentional race condition tests, add explicit annotations:

```erlang
%% @doc NON-IDEMPOTENT: Intentional race condition test
%% This function demonstrates lost updates in non-atomic ETS operations.
%% DO NOT use this pattern in production code.
perform_non_atomic_increment() ->
    [{count, Current}] = ets:lookup(?ETS_TABLE, count),
    NewVal = Current + 1,
    ets:insert(?ETS_TABLE, {count, NewVal}).  % RACE CONDITION HERE
```

**Implementation Locations**:
- `bench/race_condition_bombardment.erl:131-134`
- `bench/erlmcp_bench_race_conditions.erl:506`
- `apps/erlmcp_core/test/erlmcp_ets_race_condition_tests.erl:207`

---

## Retry Safety Guarantees

### Idempotency Contract

**Definition**: An operation `f` is idempotent if `f(f(x)) = f(x)` for all `x`.

**Cloud Requirement**: All cloud operations must satisfy:
```
∀op ∈ CloudOps. idempotent(op) ∨ guarded(op)
```

Where:
- `idempotent(op)` → Operation can be retried safely
- `guarded(op)` → Operation detects retries and returns success without re-executing

### Current Compliance

| Operation Class | Idempotent? | Guarded? | Compliant? |
|----------------|-------------|----------|------------|
| File writes | ✅ Yes | N/A | ✅ PASS |
| Git push | ❌ No | ⚠️ Partial (silent failure) | 🔴 FAIL |
| ETS counters (prod) | ❌ No | ❌ No | 🔴 FAIL |
| ETS counters (test) | ❌ No | N/A (test only) | ⚠️ ADVISORY |
| State counters | ❌ No | ❌ No | 🔴 FAIL |
| Event recording | ⚠️ Partial | ❌ No | 🔴 FAIL |

### Proposed Compliance (Post-Remediation)

| Operation Class | Idempotent? | Guarded? | Compliant? |
|----------------|-------------|----------|------------|
| File writes | ✅ Yes | N/A | ✅ PASS |
| Git push | ⚠️ No | ✅ Yes | ✅ PASS |
| ETS counters (prod) | ✅ Yes (atomic) | N/A | ✅ PASS |
| ETS counters (test) | ❌ No (intentional) | N/A | ✅ PASS (annotated) |
| State counters | ✅ Yes (moved to ETS) | N/A | ✅ PASS |
| Event recording | ⚠️ No | ✅ Yes (unique ID check) | ✅ PASS |

---

## Recommendations

### Priority 1: Critical Production Fixes

1. **Git Push Guards** (scripts/release-cli.sh, scripts/release-create-v0.6.0.sh)
   - Implement `idempotent_git_push` function
   - Add remote existence checks before push
   - Estimated effort: 2 hours
   - Risk: HIGH (release process breakage)

2. **ETS Counter Migration** (apps/erlmcp_core/src/erlmcp_tasks.erl)
   - Replace state counters with atomic ETS counters
   - Add migration code for existing deployments
   - Estimated effort: 4 hours
   - Risk: MEDIUM (counter drift accumulation)

3. **Event Recording Idempotency** (apps/erlmcp_observability/src/erlmcp_receipt_chain.erl)
   - Use `insert_new` or monotonic counters
   - Add duplicate detection
   - Estimated effort: 2 hours
   - Risk: MEDIUM (audit trail corruption)

### Priority 2: Code Quality Improvements

4. **State Counter Refactoring** (apps/erlmcp_transports, apps/erlmcp_observability)
   - Migrate all state counters to atomic ETS
   - Update counter access patterns
   - Estimated effort: 8 hours
   - Risk: LOW (code quality, metrics accuracy)

5. **Test Annotations** (bench/, apps/erlmcp_core/test/)
   - Add NON-IDEMPOTENT comments to intentional race condition tests
   - Document expected behavior
   - Estimated effort: 1 hour
   - Risk: NONE (documentation only)

### Priority 3: Infrastructure Enhancements

6. **Retry Policy Documentation**
   - Document retry semantics in CLAUDE.md
   - Add retry safety checks to pre-commit hooks
   - Estimated effort: 2 hours
   - Risk: NONE (process improvement)

7. **Cloud Testing**
   - Add retry integration tests
   - Simulate network timeouts + retries
   - Estimated effort: 6 hours
   - Risk: NONE (improved test coverage)

### Implementation Plan

**Week 1** (Critical fixes):
```
Day 1-2: Git push guards (Priority 1.1)
Day 3-4: ETS counter migration for erlmcp_tasks (Priority 1.2)
Day 5: Event recording idempotency (Priority 1.3)
```

**Week 2** (Quality improvements):
```
Day 1-3: State counter refactoring (Priority 2.1)
Day 4: Test annotations (Priority 2.2)
Day 5: Documentation + review (Priority 3)
```

---

## Testing Strategy

### Unit Tests

```erlang
%% Test idempotent counter increments
counter_idempotency_test() ->
    ets:new(test_counters, [named_table, public, set]),
    ets:insert(test_counters, {count, 0}),

    % Simulate concurrent increments
    Pids = [spawn(fun() ->
        ets:update_counter(test_counters, count, {2, 1}, {count, 0})
    end) || _ <- lists:seq(1, 100)],

    % Wait for all processes
    [receive after 100 -> ok end || _ <- Pids],

    % Verify count is correct (no lost updates)
    [{count, FinalCount}] = ets:lookup(test_counters, count),
    ?assertEqual(100, FinalCount),

    ets:delete(test_counters).
```

### Integration Tests

```bash
#!/usr/bin/env bash
# test_git_push_idempotency.sh

# Setup test repo
git init /tmp/test-repo
cd /tmp/test-repo
git commit --allow-empty -m "Initial commit"

# Test idempotent push
TAG="v1.0.0"
git tag "$TAG"

# First push (should succeed)
idempotent_git_push origin "$TAG" tag
assert_success "First push failed"

# Second push (should succeed with 'already exists' message)
idempotent_git_push origin "$TAG" tag
assert_success "Second push failed (should be idempotent)"

# Cleanup
rm -rf /tmp/test-repo
```

### Chaos Testing

```erlang
%% Test counter behavior under process crashes
chaos_counter_test() ->
    % Start counter server
    {ok, Pid} = counter_server:start_link(),

    % Increment counter 10 times
    [counter_server:increment() || _ <- lists:seq(1, 10)],
    ?assertEqual(10, counter_server:get_count()),

    % Crash server
    exit(Pid, kill),
    timer:sleep(100),  % Wait for supervisor restart

    % Counter should persist (ETS-backed)
    ?assertEqual(10, counter_server:get_count()),

    ok.
```

---

## Appendix: Complete File List

### Critical (Production Code)

1. `scripts/release-cli.sh:289-290` - Git push without guards
2. `scripts/release-create-v0.6.0.sh:298-304` - Git push without guards
3. `apps/erlmcp_core/src/erlmcp_tasks.erl:430` - State counter increment
4. `apps/erlmcp_transports/src/erlmcp_transport_ws.erl:696` - State counter increment
5. `apps/erlmcp_transports/src/erlmcp_pool_manager.erl:205` - State counter increment
6. `apps/erlmcp_core/src/erlmcp_schema_cache.erl:220-224` - State counter increments
7. `apps/erlmcp_observability/src/erlmcp_event_audit.erl:71-144` - State counter increments (9 locations)
8. `apps/erlmcp_observability/src/erlmcp_event_metrics.erl:99-159` - State counter increments (5 locations)
9. `apps/erlmcp_observability/src/erlmcp_event_logger.erl:66-71` - State counter increments
10. `apps/erlmcp_observability/src/erlmcp_receipt_chain.erl:47,86` - ETS insert without uniqueness check

### Advisory (Test/Benchmark Code)

11. `bench/race_condition_bombardment.erl:131-134` - Intentional race condition (test)
12. `bench/erlmcp_bench_race_conditions.erl:506` - Intentional race condition (benchmark)
13. `bench/race_test_report.erl:91` - Intentional race condition (test)
14. `bench/race_test_quick.erl:67` - Intentional race condition (test)
15. `apps/erlmcp_core/test/erlmcp_ets_race_condition_tests.erl:207` - Intentional race condition (test)
16. `apps/erlmcp_core/test/erlmcp_phase2_integration_SUITE.erl:384` - Counter increment (test)

### Acceptable (Idempotent or Safe)

- All `file:write_file/2` operations (overwrite semantics)
- All bash counter increments in local function scope
- All `mkdir -p` operations (idempotent)
- All `rm -rf ... || true` operations (guarded)

---

## Verification

Run these commands to verify findings:

```bash
# 1. Search for git push without guards
grep -rn "git push" scripts/ .claude/hooks/ | grep -v "ls-remote"

# 2. Search for state counter increments
grep -rn "State#.*{.*=.*+.*}" apps/erlmcp_*/src/*.erl

# 3. Search for non-atomic ETS insert with counter
grep -rn "ets:insert.*count.*+" apps/erlmcp_*/src/*.erl bench/*.erl

# 4. Verify file write operations (should be idempotent)
grep -rn "file:write_file" apps/erlmcp_*/src/*.erl
```

---

## Conclusion

**Status**: 🔴 **NON-COMPLIANT** (4 critical issues in production code)

**Next Steps**:
1. Implement Priority 1 fixes (git push guards, ETS counter migration, event idempotency)
2. Add retry safety tests to CI/CD pipeline
3. Update CLAUDE.md with idempotency requirements
4. Schedule code review for state counter refactoring

**Estimated Remediation Time**: 2 weeks (10 critical issues + testing)

**Risk Level**: HIGH (cloud deployment failures possible without fixes)

---

**Report Generated**: 2026-02-01
**Analyzer**: build-engineer (antipattern search)
**Next Review**: After Priority 1 fixes implemented
