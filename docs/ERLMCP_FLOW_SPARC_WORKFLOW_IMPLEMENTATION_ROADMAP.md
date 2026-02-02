# erlmcp-flow SPARC Workflow Implementation Roadmap

**Version**: 1.0.0
**Date**: 2026-02-02
**Status**: Ready for Implementation

---

## Overview

This document provides a comprehensive roadmap for implementing the erlmcp-flow SPARC Workflow system using Chicago School TDD and EPIC 9 parallel execution strategy.

**Total Timeline**: 8 days
**Expected Speedup**: 2.8x - 4.4x (EPIC 9 workflow)

---

## Phase 1: Foundation (Day 1-2)

### Day 1: Core Infrastructure

#### Morning: Supervision Tree & Registry

**Agent**: erlang-architect + erlang-otp-developer

```erlang
%% Files to create:
apps/erlmcp_flow/src/erlmcp_flow_sup.erl
apps/erlmcp_flow/src/erlmcp_flow_sparc_sup.erl
apps/erlmcp_flow/src/erlmcp_flow_registry.erl

%% Tests to create:
apps/erlmcp_flow/test/erlmcp_flow_sup_tests.erl
apps/erlmcp_flow/test/erlmcp_flow_registry_tests.erl
```

**Tasks**:
1. Implement `erlmcp_flow_sup` (top-level supervisor, one_for_all)
2. Implement `erlmcp_flow_sparc_sup` (SPARC components, one_for_one)
3. Implement `erlmcp_flow_registry` (gproc wrapper, O(log N) lookups)
4. Write EUnit tests (Chicago TDD):
   - Registry registration/lookup
   - Supervisor restart strategies
   - Process isolation

**Quality Gates**:
- `rebar3 compile` → 0 errors
- `rebar3 eunit` → 0 failures
- Coverage ≥ 80%

#### Afternoon: Application & API Facade

**Agent**: erlang-otp-developer

```erlang
%% Files to create:
apps/erlmcp_flow/src/erlmcp_flow_app.erl
apps/erlmcp_flow/src/erlmcp_flow.erl

%% Tests to create:
apps/erlmcp_flow/test/erlmcp_flow_tests.erl
```

**Tasks**:
1. Implement `erlmcp_flow_app` (application callback)
2. Implement `erlmcp_flow` (public API facade)
3. API functions:
   - `start_workflow/1,2`
   - `get_workflow_status/1`
   - `cancel_workflow/1`
   - `get_receipt/1`
   - `list_workflows/0`
   - `cleanup_old_workflows/1`
4. Write EUnit tests

**Quality Gates**:
- All API functions have specs
- All API functions have tests
- Dialyzer warnings = 0

---

### Day 2: State Machines (Orchestrator + Parser)

#### Morning: Main Orchestrator

**Agent**: erlang-otp-developer

```erlang
%% Files to create:
apps/erlmcp_flow/src/erlmcp_flow_sparc_orchestrator.erl
apps/erlmcp_flow/src/erlmcp_flow_sparc_orchestrator_sup.erl

%% Tests to create:
apps/erlmcp_flow/test/erlmcp_flow_sparc_orchestrator_tests.erl
```

**Tasks**:
1. Implement orchestrator FSM:
   - States: idle → specification → pseudocode → architecture → refinement → completion → idle
   - Error state with recovery
   - State entry actions
   - Priority message handling
2. Implement simple_one_for_one supervisor
3. Write EUnit tests:
   - State transitions
   - Timeout handling
   - Error recovery
   - Cancel signal (priority)

**Quality Gates**:
- All states tested
- All transitions tested
- Coverage ≥ 80%

#### Afternoon: Spec Parser

**Agent**: erlang-otp-developer

```erlang
%% Files to create:
apps/erlmcp_flow/src/erlmcp_flow_spec_parser.erl
apps/erlmcp_flow/src/erlmcp_flow_spec_parser_sup.erl

%% Tests to create:
apps/erlmcp_flow/test/erlmcp_flow_spec_parser_tests.erl
```

**Tasks**:
1. Implement parser FSM:
   - States: idle → parsing → validating → complete/error
   - Tokenizer
   - Grammar parser
   - Validation logic
2. Implement simple_one_for_one supervisor
3. Write EUnit tests:
   - Valid input parsing
   - Invalid syntax handling
   - Edge cases (empty, malformed)

**Test Examples**:
```erlang
parser_valid_simple_test() ->
    Input = <<"erlang-otp-developer, erlang-test-engineer">>,
    {ok, Parser} = erlmcp_flow_spec_parser:start_link(Input),
    {ok, Result} = erlmcp_flow_spec_parser:parse(Parser),
    ?assertMatch(#{agents := [_,_]}, Result).

parser_with_parallelism_test() ->
    Input = <<"erlang-otp-developer, erlang-test-engineer parallel 2">>,
    {ok, Parser} = erlmcp_flow_spec_parser:start_link(Input),
    {ok, Result} = erlmcp_flow_spec_parser:parse(Parser),
    ?assertEqual(2, maps:get(parallelism, Result)).

parser_invalid_syntax_test() ->
    Input = <<"invalid syntax here!">>,
    {ok, Parser} = erlmcp_flow_spec_parser:start_link(Input),
    ?assertMatch({error, _}, erlmcp_flow_spec_parser:parse(Parser)).
```

---

## Phase 2: Planning & Execution (Day 3-4)

### Day 3: Planner + Topology + Routing

#### Morning: Planner FSM

**Agent**: erlang-otp-developer

```erlang
%% Files to create:
apps/erlmcp_flow/src/erlmcp_flow_planner.erl
apps/erlmcp_flow/src/erlmcp_flow_planner_sup.erl

%% Tests to create:
apps/erlmcp_flow/test/erlmcp_flow_planner_tests.erl
```

**Tasks**:
1. Implement planner FSM:
   - States: idle → analyzing → assigning → routing → complete/error
   - Dependency DAG builder (digraph)
   - Agent assignment (load balancing)
   - Routing table construction
2. Write EUnit tests

#### Afternoon: Topology & Router (Pure Modules)

**Agent**: erlang-otp-developer

```erlang
%% Files to create:
apps/erlmcp_flow/src/erlmcp_flow_topology.erl
apps/erlmcp_flow/src/erlmcp_flow_router.erl

%% Tests to create:
apps/erlmcp_flow/test/erlmcp_flow_topology_tests.erl
apps/erlmcp_flow/test/erlmcp_flow_router_tests.erl
```

**Tasks**:
1. Implement `erlmcp_flow_topology`:
   - `select_topology/2` (agent_count, complexity)
   - `build_mesh_topology/1`
   - `build_hierarchical_topology/2`
2. Implement `erlmcp_flow_router`:
   - `calculate_routes/2`
   - `calculate_health_score/1`
   - `adjust_routing/2`
   - `find_alternative_route/3`
3. Write EUnit tests (pure functions, easy to test)

---

### Day 4: Executor + Monitor

#### Morning: Executor FSM

**Agent**: erlang-otp-developer

```erlang
%% Files to create:
apps/erlmcp_flow/src/erlmcp_flow_executor.erl
apps/erlmcp_flow/src/erlmcp_flow_executor_sup.erl

%% Tests to create:
apps/erlmcp_flow/test/erlmcp_flow_executor_tests.erl
```

**Tasks**:
1. Implement executor FSM:
   - States: idle → dispatching → executing → monitoring → complete/error
   - Task dispatch with monitoring
   - Timeout handling
   - Result collection
   - Adaptive routing integration
2. Write EUnit tests

#### Afternoon: Monitor FSM

**Agent**: erlang-otp-developer

```erlang
%% Files to create:
apps/erlmcp_flow/src/erlmcp_flow_monitor.erl
apps/erlmcp_flow/src/erlmcp_flow_monitor_sup.erl

%% Tests to create:
apps/erlmcp_flow/test/erlmcp_flow_monitor_tests.erl
```

**Tasks**:
1. Implement monitor FSM:
   - States: observing → alerting → recovering → complete
   - Metrics collection
   - Alert rules evaluation
   - Recovery action triggering
2. Write EUnit tests

---

## Phase 3: Consensus & Error Recovery (Day 5)

### Day 5: Consensus Protocols + Error Recovery

#### Morning: Consensus (Raft + PBFT)

**Agent**: erlang-otp-developer

```erlang
%% Files to create:
apps/erlmcp_flow/src/erlmcp_flow_consensus.erl (behavior)
apps/erlmcp_flow/src/erlmcp_flow_raft.erl
apps/erlmcp_flow/src/erlmcp_flow_pbft.erl

%% Tests to create:
apps/erlmcp_flow/test/erlmcp_flow_raft_tests.erl
apps/erlmcp_flow/test/erlmcp_flow_pbft_tests.erl
```

**Tasks**:
1. Define consensus behavior:
   - `append_entry/2`
   - `read_log/1`
   - `get_state/1`
2. Implement Raft FSM:
   - States: follower → candidate → leader
   - Election algorithm
   - Log replication
3. Implement PBFT FSM:
   - States: pre_prepare → prepare → commit → reply
   - Byzantine fault tolerance
   - View change
4. Write EUnit tests

#### Afternoon: Error Recovery

**Agent**: erlang-otp-developer

```erlang
%% Files to create:
apps/erlmcp_flow/src/erlmcp_flow_error_recovery.erl
apps/erlmcp_flow/src/erlmcp_flow_error_recovery_sup.erl

%% Tests to create:
apps/erlmcp_flow/test/erlmcp_flow_error_recovery_tests.erl
```

**Tasks**:
1. Implement error recovery FSM:
   - States: monitoring → {task_failed | byzantine_detected | partition_detected} → recovering → recovered/failed
   - Task failure → replan strategy
   - Byzantine → switch consensus strategy
   - Partition → wait for heal strategy
2. Write EUnit tests

---

## Phase 4: Receipt & Integration (Day 6)

### Day 6: Receipt Generator + End-to-End Integration

#### Morning: Receipt Server

**Agent**: erlang-otp-developer

```erlang
%% Files to create:
apps/erlmcp_flow/src/erlmcp_flow_receipt.erl

%% Tests to create:
apps/erlmcp_flow/test/erlmcp_flow_receipt_tests.erl
```

**Tasks**:
1. Implement receipt server (gen_server):
   - Receipt generation
   - HMAC-SHA256 signing
   - ETS storage
   - Retrieval API
   - Cleanup (retention policy)
2. Write EUnit tests

#### Afternoon: Integration Tests

**Agent**: erlang-test-engineer

```erlang
%% Files to create:
apps/erlmcp_flow/test/erlmcp_flow_sparc_workflow_SUITE.erl
apps/erlmcp_flow/test/erlmcp_flow_consensus_SUITE.erl
apps/erlmcp_flow/test/erlmcp_flow_error_recovery_SUITE.erl
```

**Tasks**:
1. Write Common Test suites:
   - Simple workflow (2 agents, no errors)
   - Parallel workflow (10 agents)
   - Hierarchical workflow (>10 agents)
   - Raft consensus integration
   - PBFT consensus integration
   - Task failure recovery
   - Byzantine fault detection
   - Network partition recovery
2. Run all tests: `rebar3 ct`

---

## Phase 5: Quality & Performance (Day 7)

### Day 7: Quality Gates + Benchmarks

#### Morning: Quality Checks

**Agent**: code-reviewer + agent-12-dialyzer + agent-13-xref

**Tasks**:
1. Run Dialyzer: `rebar3 dialyzer`
   - Fix all warnings
   - Target: 0 warnings
2. Run Xref: `rebar3 xref`
   - Fix undefined functions
   - Target: 0 undefined
3. Check coverage: `rebar3 cover --verbose`
   - Identify uncovered lines
   - Add tests for <80% modules
   - Target: ≥80% overall
4. Format check: `rebar3 format --verify`
   - Fix formatting issues

#### Afternoon: Benchmarks

**Agent**: erlang-performance

```erlang
%% Files to create:
apps/erlmcp_flow/bench/erlmcp_flow_workflow_bench.erl
apps/erlmcp_flow/bench/erlmcp_flow_consensus_bench.erl
apps/erlmcp_flow/bench/erlmcp_flow_routing_bench.erl
```

**Tasks**:
1. Benchmark workflow throughput:
   - Target: 50K tasks/s
   - Measure: p50, p95, p99 latency
2. Benchmark consensus protocols:
   - Raft: Target <200ms p99
   - PBFT: Target <500ms p99
3. Benchmark routing lookup:
   - Target: <100μs p99
4. Benchmark parser:
   - Target: <10ms p99
5. Generate performance report

---

## Phase 6: Documentation & Polish (Day 8)

### Day 8: Documentation + Examples

#### Morning: API Documentation

**Agent**: Documentation specialist

**Tasks**:
1. Generate EDoc: `rebar3 edoc`
2. Write API reference guide
3. Write usage examples:
   - Simple workflow
   - Parallel workflow
   - Error handling
   - Monitoring integration
   - Receipt retrieval

#### Afternoon: Final Polish

**Agent**: QA team

**Tasks**:
1. Run full quality check: `make check`
2. Review all documentation
3. Verify examples work
4. Create release artifacts
5. Generate final receipt

---

## EPIC 9 Workflow Strategy

### Phase 1: Fan Out (Day 1-2)

**Parallel work streams** (independent):
- Stream A: Supervision tree + Registry
- Stream B: Application + API
- Stream C: Orchestrator FSM
- Stream D: Parser FSM

**Agents**: 4 parallel agents (erlang-otp-developer x4)

### Phase 2: Independent Construction (Day 3-4)

**Parallel work streams**:
- Stream A: Planner + Topology
- Stream B: Executor + Monitor
- Stream C: Router (pure module)

**Agents**: 3 parallel agents

### Phase 3: Collision Detection (Day 5)

**Integration points**:
- Orchestrator ↔ Parser
- Orchestrator ↔ Planner
- Planner ↔ Router
- Executor ↔ Monitor
- Consensus ↔ Error Recovery

**Tasks**:
- Identify interface mismatches
- Resolve conflicts
- Refactor if needed

### Phase 4: Convergence (Day 6)

**Integration testing**:
- End-to-end workflows
- All components working together
- Error scenarios tested

### Phase 5: Refactoring (Day 7)

**Quality improvements**:
- Code quality (Dialyzer, Xref)
- Performance optimization
- Test coverage

### Phase 6: Closure (Day 8)

**Finalization**:
- Documentation
- Examples
- Release preparation

---

## Chicago TDD Workflow

For **every** module:

1. **RED**: Write test first (should fail)
2. **GREEN**: Write minimal code to pass
3. **REFACTOR**: Improve code quality
4. **REPEAT**: Next test

Example:
```erlang
%% 1. RED - Write failing test
parser_simple_input_test() ->
    Input = <<"agent-1">>,
    {ok, Pid} = erlmcp_flow_spec_parser:start_link(Input),
    {ok, Result} = erlmcp_flow_spec_parser:parse(Pid),
    ?assertMatch(#{agents := [#{role := 'agent-1'}]}, Result).

%% Run: rebar3 eunit --module erlmcp_flow_spec_parser_tests
%% → Test fails (module not implemented)

%% 2. GREEN - Write minimal implementation
-module(erlmcp_flow_spec_parser).
-export([start_link/1, parse/1]).

start_link(Input) ->
    {ok, spawn_link(fun() -> loop(Input) end)}.

parse(Pid) ->
    Pid ! {parse, self()},
    receive
        {ok, Result} -> {ok, Result}
    end.

loop(Input) ->
    receive
        {parse, From} ->
            Result = #{agents => [#{role => binary_to_atom(Input)}]},
            From ! {ok, Result}
    end.

%% Run: rebar3 eunit --module erlmcp_flow_spec_parser_tests
%% → Test passes

%% 3. REFACTOR - Improve to gen_statem, add proper parsing
%% (Continue iterating with more tests)
```

---

## Testing Checklist

### Unit Tests (EUnit)

- [ ] erlmcp_flow_sup
- [ ] erlmcp_flow_registry
- [ ] erlmcp_flow_app
- [ ] erlmcp_flow (API)
- [ ] erlmcp_flow_sparc_orchestrator
- [ ] erlmcp_flow_spec_parser
- [ ] erlmcp_flow_planner
- [ ] erlmcp_flow_executor
- [ ] erlmcp_flow_monitor
- [ ] erlmcp_flow_receipt
- [ ] erlmcp_flow_error_recovery
- [ ] erlmcp_flow_topology
- [ ] erlmcp_flow_router
- [ ] erlmcp_flow_raft
- [ ] erlmcp_flow_pbft

### Integration Tests (Common Test)

- [ ] Simple workflow (2 agents)
- [ ] Parallel workflow (10 agents)
- [ ] Hierarchical workflow (>10 agents)
- [ ] Raft consensus
- [ ] PBFT consensus
- [ ] Task failure recovery
- [ ] Byzantine fault detection
- [ ] Network partition recovery
- [ ] Receipt generation
- [ ] Monitoring & alerting

### Property Tests (PropEr)

- [ ] Parser roundtrip: `parse(format(spec)) = spec`
- [ ] Parser never crashes
- [ ] Planner valid DAG (no cycles)
- [ ] Planner all tasks assigned
- [ ] Raft eventually elects leader
- [ ] PBFT tolerates f Byzantine faults

---

## Quality Gates

| Day | Gate | Command | Pass Criteria |
|-----|------|---------|---------------|
| 1-6 | Compile | `rebar3 compile` | 0 errors |
| 1-6 | EUnit | `rebar3 eunit` | 0 failures |
| 6 | CT | `rebar3 ct` | 0 failures |
| 7 | Coverage | `rebar3 cover` | ≥80% |
| 7 | Dialyzer | `rebar3 dialyzer` | 0 warnings |
| 7 | Xref | `rebar3 xref` | 0 undefined |
| 7 | Format | `rebar3 format --verify` | Pass |
| 7 | Benchmarks | Custom | <10% regression |
| 8 | Full Check | `make check` | All pass |

---

## Success Metrics

| Metric | Target | Actual | Status |
|--------|--------|--------|--------|
| Modules | 20 | __ | ⬜ |
| Tests (EUnit) | 200+ | __ | ⬜ |
| Tests (CT) | 10 suites | __ | ⬜ |
| Coverage | ≥80% | __% | ⬜ |
| Dialyzer warnings | 0 | __ | ⬜ |
| Xref undefined | 0 | __ | ⬜ |
| Parse latency | <10ms | __ms | ⬜ |
| Routing lookup | <100μs | __μs | ⬜ |
| Raft consensus | <200ms | __ms | ⬜ |
| Workflow throughput | 50K tasks/s | __K tasks/s | ⬜ |

---

## Next Steps

1. Review all design documents
2. Set up development environment
3. Create initial project structure
4. Begin Day 1 implementation
5. Follow Chicago TDD workflow
6. Run quality gates daily
7. Track progress against roadmap

---

**Implementation Starts**: Day 1, 0900
**Target Completion**: Day 8, 1700
**Status Updates**: Daily standup + receipt generation

---

**End of Roadmap**
