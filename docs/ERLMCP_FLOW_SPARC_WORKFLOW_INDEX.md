# erlmcp-flow SPARC Workflow Documentation Index

**Version**: 1.0.0
**Date**: 2026-02-02
**Status**: Design Complete, Ready for Implementation

---

## Quick Start

**New to the project?** Start here:

1. Read [Summary](#summary) (5 minutes)
2. Review [Architecture](#architecture) (15 minutes)
3. Check [Quick Reference](#quick-reference) (10 minutes)
4. Follow [Implementation Roadmap](#implementation-roadmap) (when ready to code)

---

## Documentation Overview

### Summary

**File**: `/home/user/erlmcp/docs/ERLMCP_FLOW_SPARC_WORKFLOW_SUMMARY.md`

**What it covers**:
- Executive overview
- Key design decisions
- Module structure
- API usage examples
- Data flow walkthrough
- Performance targets

**Read this for**: High-level understanding of the system

**Time**: 10-15 minutes

---

### Specification

**File**: `/home/user/erlmcp/docs/ERLMCP_FLOW_SPARC_WORKFLOW_SPECIFICATION.md`

**What it covers**:
- Functional requirements (FR-1.1 - FR-1.10)
- Non-functional requirements (NFR-1.1 - NFR-1.8)
- Constraints and edge cases
- 5 SPARC phases detailed
- State machine specifications:
  - Orchestrator FSM (7 states)
  - Spec Parser FSM (5 states)
  - Planner FSM (5 states)
  - Executor FSM (5 states)
  - Monitor FSM (4 states)
  - Error Recovery FSM (7 states)
- Supervision tree structure
- API specification
- Testing strategy (Chicago TDD)
- Quality gates

**Read this for**: Complete requirements and specifications

**Time**: 30-45 minutes

---

### Architecture

**File**: `/home/user/erlmcp/docs/ERLMCP_FLOW_SPARC_WORKFLOW_ARCHITECTURE.md`

**What it covers**:
- System architecture diagram
- Component interaction flow
- State machine diagrams (all 6 FSMs)
- Supervision trees with strategies
- Module design and responsibilities
- Data flow with examples
- Consensus protocols:
  - Raft (leader election, log replication)
  - PBFT (Byzantine fault tolerance)
- Error recovery strategies:
  - Task failure → replan
  - Byzantine fault → switch consensus
  - Network partition → wait for heal
- Performance optimization techniques

**Read this for**: Detailed design and implementation patterns

**Time**: 45-60 minutes

---

### Diagrams

**File**: `/home/user/erlmcp/docs/ERLMCP_FLOW_SPARC_WORKFLOW_DIAGRAMS.md`

**What it covers**:
- Main orchestrator state machine diagram
- Component interaction diagram
- Error recovery state machine
- Consensus protocol selection flow
- Topology selection (mesh vs hierarchical)
- Adaptive routing flow
- Supervision tree diagram
- Receipt chain diagram

**Read this for**: Visual understanding of the system

**Time**: 15-20 minutes

---

### Quick Reference

**File**: `/home/user/erlmcp/docs/ERLMCP_FLOW_SPARC_WORKFLOW_QUICK_REFERENCE.md`

**What it covers**:
- State machine quick reference (all FSMs)
- API cheat sheet
- Module index
- Error recovery patterns
- Performance targets table
- Testing commands
- gen_statem template
- Common patterns (async init, state timeout, monitoring)
- Registry lookup examples
- Data structure types
- Helpful commands

**Read this for**: Quick lookup during development

**Time**: 5 minutes (reference)

---

### Implementation Roadmap

**File**: `/home/user/erlmcp/docs/ERLMCP_FLOW_SPARC_WORKFLOW_IMPLEMENTATION_ROADMAP.md`

**What it covers**:
- 8-day implementation timeline
- Phase-by-phase breakdown:
  - Day 1-2: Foundation (supervision, registry, API)
  - Day 3-4: Planning & Execution (planner, executor, monitor)
  - Day 5: Consensus & Error Recovery
  - Day 6: Receipt & Integration
  - Day 7: Quality & Performance
  - Day 8: Documentation & Polish
- EPIC 9 workflow strategy
- Chicago TDD workflow examples
- Testing checklist
- Quality gates per day
- Success metrics

**Read this for**: Step-by-step implementation guide

**Time**: 30 minutes (study), 8 days (execution)

---

## Documentation Map

```
erlmcp-flow SPARC Workflow
│
├── SUMMARY.md (Overview, key decisions)
│   ├─→ Quick understanding
│   └─→ Data flow examples
│
├── SPECIFICATION.md (Requirements, constraints)
│   ├─→ Functional requirements
│   ├─→ Non-functional requirements
│   ├─→ State machine specs
│   ├─→ API specification
│   └─→ Testing strategy
│
├── ARCHITECTURE.md (Design details)
│   ├─→ System architecture
│   ├─→ Component interaction
│   ├─→ Consensus protocols
│   ├─→ Error recovery
│   └─→ Performance optimization
│
├── DIAGRAMS.md (Visual representations)
│   ├─→ State machines
│   ├─→ Data flow
│   ├─→ Supervision trees
│   └─→ Receipt chain
│
├── QUICK_REFERENCE.md (Development lookup)
│   ├─→ State transitions
│   ├─→ API cheat sheet
│   ├─→ Code patterns
│   └─→ Testing commands
│
└── IMPLEMENTATION_ROADMAP.md (8-day plan)
    ├─→ Phase 1: Foundation
    ├─→ Phase 2: Planning & Execution
    ├─→ Phase 3: Consensus & Recovery
    ├─→ Phase 4: Receipt & Integration
    ├─→ Phase 5: Quality & Performance
    └─→ Phase 6: Documentation
```

---

## Key Concepts

### SPARC Methodology

**S**pecification → **P**seudocode → **A**rchitecture → **R**efinement → **C**ompletion

Each phase is orchestrated by the main FSM, with dedicated sub-FSMs handling:
- **Specification**: Parse and validate user input
- **Pseudocode**: Plan task assignments and dependencies
- **Architecture**: Design topology and consensus protocol
- **Refinement**: Execute tasks with monitoring and adaptive routing
- **Completion**: Generate receipt with audit trail

### State Machines

All components use `gen_statem` with:
- **Callback mode**: `[handle_event_function, state_enter_calls]`
- **Priority messages**: OTP 28 feature for health checks and cancel signals
- **State entry actions**: Setup/teardown in state_enter_calls
- **Let-it-crash**: Supervision isolates failures

### Error Recovery

Three primary error scenarios:

1. **Task Failure**: Replan with alternative agent
2. **Byzantine Fault**: Switch from Raft to PBFT, exclude bad actor
3. **Network Partition**: Halt writes, wait for heal with exponential backoff

### Consensus Protocols

- **Raft**: Low latency (<200ms), trusted environment, internal agents
- **PBFT**: High fault tolerance (<500ms), untrusted environment, external agents

Selection based on risk assessment (external agent ratio + environment trust).

### Topology

- **Mesh**: Full connectivity (O(N²) edges), ≤10 agents
- **Hierarchical**: Tree structure (O(N) edges), >10 agents

---

## Module Overview

| Module | Type | Lines | Tests | Description |
|--------|------|-------|-------|-------------|
| erlmcp_flow | API | ~200 | 20 | Public API facade |
| erlmcp_flow_sparc_orchestrator | FSM | ~400 | 30 | Main coordinator |
| erlmcp_flow_spec_parser | FSM | ~300 | 25 | Input parser |
| erlmcp_flow_planner | FSM | ~500 | 35 | Task planner |
| erlmcp_flow_executor | FSM | ~400 | 30 | Task executor |
| erlmcp_flow_monitor | FSM | ~300 | 25 | Monitoring |
| erlmcp_flow_receipt | Server | ~300 | 20 | Receipt generation |
| erlmcp_flow_error_recovery | FSM | ~350 | 28 | Error recovery |
| erlmcp_flow_registry | Server | ~250 | 18 | Agent registry |
| erlmcp_flow_router | Pure | ~200 | 15 | Routing logic |
| erlmcp_flow_topology | Pure | ~150 | 12 | Topology selection |
| erlmcp_flow_raft | FSM | ~500 | 40 | Raft consensus |
| erlmcp_flow_pbft | FSM | ~600 | 45 | PBFT consensus |
| **Total** | | **~4,450** | **343** | 13 modules |

Plus 7 supervisors (~150 lines each) = **~5,500 total lines**

---

## Testing Overview

### Unit Tests (EUnit)

- 13 module test suites
- ~343 test cases
- Target: ≥80% coverage
- Black-box testing only (Chicago TDD)

### Integration Tests (Common Test)

- 10 test suites
- End-to-end workflows
- Consensus protocol integration
- Error recovery scenarios

### Property Tests (PropEr)

- Parser roundtrip
- Planner DAG validity
- Consensus properties (leader election, Byzantine tolerance)

### Quality Gates

- Compile: 0 errors
- EUnit: 0 failures
- CT: 0 failures
- Coverage: ≥80%
- Dialyzer: 0 warnings
- Xref: 0 undefined
- Benchmarks: <10% regression

---

## Performance Targets

| Metric | Target | Threshold |
|--------|--------|-----------|
| Parse latency (p99) | <10ms | 50ms |
| Plan latency (p99) | <50ms | 200ms |
| Routing lookup (p99) | <100μs | 1ms |
| Task dispatch (p99) | <5ms | 20ms |
| Raft consensus (p99) | <200ms | 500ms |
| PBFT consensus (p99) | <500ms | 1000ms |
| Byzantine detection (p99) | <1s | 5s |
| Partition heal RTO (p99) | <2s | 10s |
| Workflow throughput | 50K tasks/s | 40K tasks/s |
| Memory per workflow | <1KB | 10KB |

---

## Development Workflow

1. **Read specification** (ERLMCP_FLOW_SPARC_WORKFLOW_SPECIFICATION.md)
2. **Study architecture** (ERLMCP_FLOW_SPARC_WORKFLOW_ARCHITECTURE.md)
3. **Review diagrams** (ERLMCP_FLOW_SPARC_WORKFLOW_DIAGRAMS.md)
4. **Follow roadmap** (ERLMCP_FLOW_SPARC_WORKFLOW_IMPLEMENTATION_ROADMAP.md)
5. **Use quick reference** during coding (ERLMCP_FLOW_SPARC_WORKFLOW_QUICK_REFERENCE.md)
6. **Chicago TDD**: Red → Green → Refactor
7. **Quality gates**: Run after each module
8. **Integration tests**: After Phase 4
9. **Benchmarks**: After Phase 5
10. **Documentation**: Phase 6

---

## Related Documentation

### Existing erlmcp-flow Docs

- `/home/user/erlmcp/docs/ERLMCP-FLOW-README.md` - Original erlmcp-flow design
- `/home/user/erlmcp/docs/erlmcp-flow-diagrams.md` - Original diagrams
- `/home/user/erlmcp/docs/ERLMCP_FLOW_ROUTING_SUMMARY.md` - Routing layer

### General erlmcp Docs

- `/home/user/erlmcp/CLAUDE.md` - Project guidelines and conventions
- `/home/user/erlmcp/SPARC_MASTER_DOCUMENT.md` - Original SPARC methodology
- `/home/user/erlmcp/docs/architecture.md` - Core architecture
- `/home/user/erlmcp/docs/otp-patterns.md` - OTP patterns and conventions

---

## FAQ

**Q: Why gen_statem instead of gen_server?**
A: State machines provide clear semantics for phase transitions, state entry/exit actions, and handle_event_function for priority messages.

**Q: Why separate FSMs for each phase?**
A: Isolation of concerns, independent testing, parallel development (EPIC 9 workflow).

**Q: Why both Raft and PBFT?**
A: Raft for trusted environments (lower latency), PBFT for untrusted environments (Byzantine fault tolerance).

**Q: Why mesh and hierarchical topologies?**
A: Mesh for small teams (≤10 agents, full connectivity), hierarchical for large teams (>10 agents, scalability).

**Q: Why Chicago TDD?**
A: Black-box testing focuses on observable behavior, not implementation details. Easier to refactor.

**Q: Why no mocks?**
A: Real processes provide accurate testing. Mocks can hide integration issues.

**Q: What's EPIC 9 workflow?**
A: Parallel development strategy: fan_out → independent_construction → collision_detection → convergence → refactoring → closure. Expected speedup: 2.8x - 4.4x.

---

## Getting Help

1. **Design questions**: Check SPECIFICATION.md and ARCHITECTURE.md
2. **Implementation patterns**: Check QUICK_REFERENCE.md
3. **Timeline/process**: Check IMPLEMENTATION_ROADMAP.md
4. **Visual understanding**: Check DIAGRAMS.md
5. **Quick lookup**: Check QUICK_REFERENCE.md

---

## Version History

| Version | Date | Changes |
|---------|------|---------|
| 1.0.0 | 2026-02-02 | Initial design complete |

---

## Next Steps

1. Review all documentation (2-3 hours)
2. Set up development environment
3. Begin Day 1 of Implementation Roadmap
4. Follow Chicago TDD workflow
5. Run quality gates daily
6. Track progress with receipts

---

**Status**: Design Complete ✅
**Next Phase**: Implementation
**Start Date**: TBD
**Estimated Completion**: 8 days from start

---

**End of Index**
