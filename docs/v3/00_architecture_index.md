# erlmcp v3.0.0 Architecture Index

**Version**: 3.0.0  
**Status**: OSS Release Design Complete  
**Date**: 2026-01-31  
**OTP Requirement**: 28.3.1+  

---

## Core Architecture Documents (10-50)

### 10. Architecture Design Plan
**File**: `10_architecture_design_plan.md`  
**Purpose**: Overall v3.0.0 architecture design for OSS release

**Topics**:
- Executive summary of v3.0.0 changes
- Current state analysis (v2.1.0 → v3.0.0)
- OSS vs Commercial component boundaries
- Minimal vs Full component sets
- Supervision tree updates
- Component dependency matrix
- Code cleanup plan (POC removal)
- Migration path (v2.1.0 → v3.0.0)

**Key Decisions**:
- Remove Mermaid POC (7 modules, ~185KB)
- Remove experimental POC code (5+ modules)
- Update all versions: 2.1.0 → 3.0.0
- Define OSS/Commercial boundaries
- Create deployment profiles

---

### 20. Supervision Tree (v3.0.0)
**File**: `20_supervision_tree_v3.md`  
**Purpose**: Complete OTP supervision hierarchy

**Topics**:
- 3-tier supervision tree structure
- TIER 1: Core Foundation (27 workers + 5 supervisors)
- TIER 2: Protocol Servers (dynamic instances)
- TIER 3: Observability (9 workers)
- TIER 4: Transports (dynamic instances)
- Failure isolation matrix
- Startup/shutdown sequences
- Validation checklist

**Key Components**:
```
TIER 1 (erlmcp_core_sup): Registry, Sessions, Protection, MCP Features
TIER 2 (erlmcp_server_sup): Dynamic server instances
TIER 3 (erlmcp_observability_sup): Metrics, OTEL, Health, Chaos
TIER 4 (erlmcp_transport_sup): STDIO, TCP, HTTP, WS, SSE
```

---

### 30. Component Dependency Matrix
**File**: `30_component_dependency_matrix.md`  
**Purpose**: Complete dependency analysis

**Topics**:
- Internal application dependencies
- External library dependencies (with versions)
- Optional dependencies (TCPS, commercial)
- License compatibility matrix
- Dependency cleanup actions
- Maintenance procedures

**Dependency Graph**:
```
erlmcp_core (no internal deps)
    ↓
    ├── erlmcp_transports (depends on: core)
    ├── erlmcp_observability (depends on: core)
    └── erlmcp_validation (depends on: core, transports)
            ↓
        [OPTIONAL] tcps_erlmcp (depends on: core, observability)
```

**Removed Dependencies**:
- jsx: Replaced by OTP 28+ native `json` module

---

### 40. Component Diagrams
**File**: `40_component_diagrams.md`  
**Purpose**: Visual architecture reference

**Topics**:
- System architecture overview diagram
- Message flow diagrams (client request, server request)
- Deployment patterns (minimal, standard, enterprise)
- Data flow examples (subscriptions, tool execution)
- Mermaid diagrams for documentation generation

**Diagrams**:
- Component hierarchy (4 apps, 165 modules)
- Complete data flow patterns
- Clustered deployment topology
- Mermaid source for auto-generation

---

### 50. Cleanup Execution Checklist
**File**: `50_cleanup_execution_checklist.md`  
**Purpose**: Step-by-step OSS release preparation

**Topics**:
- Pre-cleanup preparation (backup, branch)
- Phase 1: Remove POC code (14 files)
- Phase 2: Update version numbers
- Phase 3: Update module lists
- Phase 4: Build and test
- Phase 5: Update documentation
- Phase 6: Validation
- Phase 7: Git commit
- Phase 8: Merge and tag (v3.0.0-oss)
- Phase 9: Post-release tasks

**Estimated Time**: 2-4 hours

---

## Supporting Documents (00-09)

### 00. SSE Resumption Plan
**File**: `00_sse_resumption_plan.md`  
**Purpose**: Server-Sent Events resumption strategy

### 01. OSS Release Strategy
**File**: `01_oss_release_strategy.md`  
**Purpose**: Overall OSS release planning

### 02. Tasks Completion Plan
**File**: `02_tasks_completion_plan.md`  
**Purpose**: Task tracking and completion

### 03. Initialization State Machine
**File**: `03_initialization_state_machine_plan.md`  
**Purpose**: Component initialization patterns

### 04. OTP Compat Layer
**File**: `04_otp_compat_layer_plan.md`  
**Purpose**: OTP 28+ compatibility planning

### 05. Priority Messages
**File**: `05_priority_messages_plan.md`  
**Purpose**: OTP 28+ priority message usage

### 06. Build Config
**File**: `06_build_config_plan.md`  
**Purpose**: Build configuration (rebar3)

---

## Planning Documents (10-19)

### 11. Security Architecture
**File**: `11_security_architecture_plan.md`  
**Purpose**: Security design and hardening

### 12. Resources Expansion
**File**: `12_resources_expansion_plan.md`  
**Purpose**: MCP resources implementation

### 13. Documentation Plan
**File**: `13_documentation_plan.md`  
**Purpose**: Documentation structure

### 15. CI/CD Plan
**File**: `15_cicd_plan.md`  
**Purpose**: Continuous integration/deployment

### 16. Examples Plan
**File**: `16_examples_plan.md`  
**Purpose**: Example implementations

### 17. Validation Tests
**File**: `17_validation_tests_plan.md`  
**Purpose**: Test strategy and validation

### 18. Performance Plan
**File**: `18_performance_plan.md`  
**Purpose**: Performance optimization

### 19. Release Checklist
**File**: `19_release_checklist_plan.md`  
**Purpose**: Release preparation checklist

---

## Quick Reference

### Module Counts

| Component | v2.0.0 Docs | v3.0.0 Pre-Cleanup | v3.0.0 OSS |
|-----------|-------------|--------------------|------------|
| erlmcp_core | 14 | 108 | 101 |
| erlmcp_transports | 8 | 28 | 28 |
| erlmcp_observability | 9 | 38 | 38 |
| erlmcp_validation | - | 16 | 16 |
| **TOTAL** | **94** | **192** | **~183** |

**Removed for OSS**:
- Mermaid: 7 modules (~185KB)
- POC: ~20 modules (estimated)
- **Final OSS**: ~165 modules

### Deployment Profiles

**Minimal** (~50MB, 120 modules):
- Core protocol + basic transports
- Development use

**Standard** (~75MB, 165 modules):
- Full OSS feature set
- Production use

**Enterprise** (~100MB, 230 modules):
- OSS + commercial add-ons
- High-availability clustering

---

## Execution Order

1. Read `10_architecture_design_plan.md` (overview)
2. Read `20_supervision_tree_v3.md` (supervision)
3. Read `30_component_dependency_matrix.md` (dependencies)
4. Review `40_component_diagrams.md` (visuals)
5. Execute `50_cleanup_execution_checklist.md` (cleanup)

---

## Status

✅ **Architecture Design**: Complete  
✅ **Supervision Tree**: Documented  
✅ **Dependencies**: Analyzed  
✅ **Diagrams**: Created  
✅ **Cleanup Plan**: Ready  

**Next Action**: Execute cleanup checklist

---

**Last Updated**: 2026-01-31  
**Maintained By**: Erlang Architect
