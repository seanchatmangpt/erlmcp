# SPARC Execution Roadmap - MCP 2025-11-25 Implementation

**Project**: erlmcp (Erlang/OTP MCP SDK)
**Version**: 0.6.0 → 0.7.0
**Date**: 2026-01-29
**Status**: Ready for Phase 4 (Refinement - Implementation)

---

## SPARC Phases Summary

### ✅ Phase 1: Specification (COMPLETED)

**Deliverables**:
- [x] Requirements document (`docs/MCP_2025-11-25_SPECIFICATION_GAPS.md`)
- [x] API contracts for all 6 feature gaps
- [x] Error code definitions (-32001 to -32099)
- [x] Functional requirements documented
- [x] Non-functional requirements defined
- [x] Integration points identified

**Key Decisions**:
- Task management uses state machine (pending → working → completed/failed/cancelled)
- Completion API uses caching with ETS (read_concurrency)
- Elicitation uses URL-based flow with 5-minute expiry
- All modules follow gen_server behavior
- Chicago School TDD for implementation

---

### ✅ Phase 2: Pseudocode (COMPLETED)

**Deliverables**:
- [x] Algorithm designs (`docs/MCP_2025-11-25_PSEUDOCODE_DESIGN.md`)
- [x] Data structure definitions
- [x] Task creation/cancellation algorithms
- [x] Completion generation/ranking algorithms
- [x] Elicitation URL generation/validation algorithms
- [x] Cross-feature integration patterns

**Key Algorithms**:
- Task ID generation with collision retry
- Cursor-based pagination for task listing
- Fuzzy matching for completions (Jaro-Winkler)
- URL signature generation for elicitations
- Progress token integration with task execution

---

### ✅ Phase 3: Architecture (COMPLETED)

**Deliverables**:
- [x] Architecture design (`docs/MCP_2025-11-25_ARCHITECTURE_DESIGN.md`)
- [x] Supervision tree defined
- [x] Module decomposition complete
- [x] Data flow diagrams documented
- [x] Failure scenarios and recovery strategies
- [x] Performance considerations identified

**Supervision Structure**:
```
erlmcp_core_sup
├── erlmcp_task_manager_sup → erlmcp_task_manager
├── erlmcp_completion_sup → erlmcp_completion
├── erlmcp_elicitation_sup → erlmcp_elicitation
├── erlmcp_cancellation_sup → erlmcp_cancellation (existing)
├── erlmcp_progress_sup → erlmcp_progress (existing)
└── erlmcp_sampling_sup → erlmcp_sampling (existing)
```

---

### 🔄 Phase 4: Refinement (IN PROGRESS)

**Objective**: Implement all 6 feature gaps with TDD

**Sub-Phases**:

#### 4a: Task Management Implementation
- [ ] Create erlmcp_task_manager.erl gen_server
- [ ] Implement task creation algorithm
- [ ] Implement task state machine
- [ ] Implement task listing with pagination
- [ ] Implement task cancellation
- [ ] Write EUnit tests (≥80% coverage)
- [ ] Write CT integration tests
- [ ] Benchmark performance (task creation <10ms)

#### 4b: Completion API Implementation
- [ ] Create erlmcp_completion.erl gen_server
- [ ] Implement resource completion (file paths)
- [ ] Implement command completion (tool names)
- [ ] Implement ranking algorithm
- [ ] Add ETS caching layer
- [ ] Write EUnit tests (≥80% coverage)
- [ ] Write CT integration tests
- [ ] Benchmark performance (completion <100ms)

#### 4c: Elicitation Features Implementation
- [ ] Create erlmcp_elicitation.erl gen_server
- [ ] Implement URL generation
- [ ] Implement URL validation
- [ ] Implement expiry timer management
- [ ] Write EUnit tests (≥80% coverage)
- [ ] Write CT integration tests
- [ ] Benchmark performance (creation <50ms)

#### 4d: Existing Module Enhancement
- [ ] Update erlmcp_server.erl with new handlers
- [ ] Update erlmcp_client.erl with new API
- [ ] Update erlmcp_json_rpc.erl with new message types
- [ ] Add records to erlmcp.hrl
- [ ] Update capability negotiation
- [ ] Ensure backward compatibility
- [ ] Write integration tests

---

### ⏳ Phase 5: Completion (PENDING)

**Objective**: Quality validation and production readiness

**Tasks**:
- [ ] End-to-end integration tests
- [ ] Performance benchmark suite (no regression >10%)
- [ ] Dialyzer validation (0 warnings)
- [ ] Xref analysis (0 issues)
- [ ] Code review checklist
- [ ] Documentation updates
- [ ] Release preparation

---

## Implementation Timeline

### Week 1-2: Phase 4a (Task Management)
- Day 1-2: Module skeleton + basic tests
- Day 3-4: Task creation + state machine
- Day 5-6: Task listing + pagination
- Day 7-8: Task cancellation + integration
- Day 9-10: Tests + benchmarks

### Week 3-4: Phase 4b (Completion API)
- Day 1-2: Module skeleton + basic tests
- Day 3-4: Resource completion generation
- Day 5-6: Ranking algorithm + caching
- Day 7-8: Command completion
- Day 9-10: Tests + benchmarks

### Week 5-6: Phase 4c (Elicitation)
- Day 1-2: Module skeleton + basic tests
- Day 3-4: URL generation + validation
- Day 5-6: Expiry timer management
- Day 7-8: Client notifications
- Day 9-10: Tests + benchmarks

### Week 7-8: Phase 4d (Integration)
- Day 1-3: Server handler integration
- Day 4-6: Client API integration
- Day 7-9: JSON-RPC message encoding
- Day 10: Compatibility testing

### Week 9-10: Phase 5 (Quality Gates)
- Day 1-3: Integration test suite
- Day 4-5: Performance benchmarks
- Day 6-7: Dialyzer + Xref validation
- Day 8-9: Code review
- Day 10: Documentation + release

---

## Quality Gates

### Gate 1: Compilation
```bash
TERM=dumb rebar3 compile
```
**Criteria**: 0 errors, 0 warnings

### Gate 2: Unit Tests
```bash
rebar3 eunit
```
**Criteria**: 100% pass rate, ≥80% coverage

### Gate 3: Integration Tests
```bash
rebar3 ct
```
**Criteria**: 100% pass rate

### Gate 4: Type Checking
```bash
rebar3 dialyzer
```
**Criteria**: 0 warnings

### Gate 5: Cross-Reference
```bash
rebar3 xref
```
**Criteria**: 0 undefined functions, 0 unused calls

### Gate 6: Performance Benchmarks
```bash
make benchmark-quick
```
**Criteria**: <10% regression from baseline

---

## Success Metrics

### Functional Completeness
- ✅ All 6 MCP gaps implemented
- ✅ 100% API contract coverage
- ✅ Backward compatibility maintained

### Code Quality
- ✅ ≥80% test coverage
- ✅ 0 Dialyzer warnings
- ✅ 0 Xref issues
- ✅ All code reviewed and approved

### Performance
- ✅ Task creation < 10ms (p99)
- ✅ Task retrieval < 50ms (p99)
- ✅ Completion generation < 100ms (p99)
- ✅ Elicitation creation < 50ms (p99)
- ✅ Sampling latency < 2s (p99)

### Reliability
- ✅ 0 critical bugs in production
- ✅ 100% test pass rate
- ✅ Graceful failure handling

---

## Risk Management

### High-Risk Items

1. **Task Persistence Complexity**
   - **Risk**: Mnesia schema migration issues
   - **Mitigation**: Start with ETS, add Mnesia later
   - **Contingency**: Document persistence as optional feature

2. **Sampling Provider Integration**
   - **Risk**: Third-party API changes
   - **Mitigation**: Abstract provider interface
   - **Contingency**: Use mock provider for testing

3. **Completion Ranking Quality**
   - **Risk**: Poor relevance rankings
   - **Mitigation**: Implement feedback loop
   - **Contingency**: Allow custom ranking functions

4. **Performance Regression**
   - **Risk**: New features slow down existing operations
   - **Mitigation**: Benchmark before/after
   - **Contingency**: Optimize hot paths

---

## Continuous Integration

### GitHub Actions Workflow

```yaml
name: MCP 2025-11-25 Implementation

on: [push, pull_request]

jobs:
  test:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - uses: erlef/setup-beam@v1
        with:
          otp-version: "25.3"
      - name: Compile
        run: TERM=dumb rebar3 compile
      - name: EUnit Tests
        run: rebar3 eunit
      - name: CT Tests
        run: rebar3 ct
      - name: Dialyzer
        run: rebar3 dialyzer
      - name: Xref
        run: rebar3 xref
      - name: Coverage
        run: rebar3 cover
```

### Quality Gate Enforcement

```bash
#!/bin/bash
# Pre-commit hook

# Run compilation
TERM=dumb rebar3 compile
if [ $? -ne 0 ]; then
    echo "❌ Compilation failed"
    exit 1
fi

# Run tests
rebar3 eunit
if [ $? -ne 0 ]; then
    echo "❌ Tests failed"
    exit 1
fi

# Check coverage
rebar3 cover
COVERAGE=$(rebar3 cover | grep "100%" | wc -l)
if [ $COVERAGE -lt 80 ]; then
    echo "⚠️ Coverage below 80%"
fi

echo "✅ All quality gates passed"
```

---

## Documentation Requirements

### API Documentation
- [ ] Update `docs/api-reference.md` with new APIs
- [ ] Add function specifications with types
- [ ] Include usage examples for each function

### Protocol Documentation
- [ ] Update `docs/protocol.md` with new methods
- [ ] Document request/response formats
- [ ] Include error codes and meanings

### Architecture Documentation
- [ ] Update `docs/architecture.md` with new modules
- [ ] Document supervision tree changes
- [ ] Include data flow diagrams

### Examples
- [ ] Task management example (`examples/task_manager_example.erl`)
- [ ] Completion example (`examples/completion_example.erl`)
- [ ] Elicitation example (`examples/elicitation_example.erl`)
- [ ] End-to-end workflow example

---

## Communication Plan

### Daily Standups
- Progress on current phase
- Blockers encountered
- Next steps planned

### Weekly Reviews
- Phase completion status
- Quality gate results
- Risk assessment updates

### Milestone Reports
- Specification complete
- Architecture approved
- Implementation complete
- Quality gates passed
- Release ready

---

## Next Actions (Immediate)

1. ✅ **Review specification document** with team
2. ✅ **Review pseudocode designs** for algorithm correctness
3. ✅ **Review architecture design** for OTP compliance
4. 🔄 **Create module stubs** for erlmcp_task_manager, erlmcp_completion, erlmcp_elicitation
5. ⏳ **Write initial tests** using Chicago School TDD
6. ⏳ **Implement task creation** in erlmcp_task_manager
7. ⏳ **Integrate with existing modules** (erlmcp_server, erlmcp_client)

---

## Appendix: SPARC Checklist

### Specification Phase
- [x] Requirements documented
- [x] API contracts defined
- [x] Error codes specified
- [x] Edge cases identified
- [x] Integration points mapped

### Pseudocode Phase
- [x] Algorithms designed
- [x] Data structures defined
- [x] Flows documented
- [x] Edge cases handled
- [x] Performance considered

### Architecture Phase
- [x] Supervision tree designed
- [x] Modules decomposed
- [x] Dependencies mapped
- [x] Failure modes defined
- [x] Security considered

### Refinement Phase
- [ ] Modules implemented
- [ ] Tests written (TDD)
- [ ] Benchmarks run
- [ ] Integration complete
- [ ] Code reviewed

### Completion Phase
- [ ] Quality gates passed
- [ ] Documentation complete
- [ ] Release prepared
- [ ] Stakeholders notified
- [ ] Lessons learned documented

---

**Status**: Ready to begin Phase 4 (Refinement - Implementation)

**Next Step**: Create module stubs and begin TDD implementation
