# erlmcp Diagram Index

Quick reference for all Mermaid diagrams in the erlmcp project.

## Diagrams by Category

### Architecture & Design

| File | Type | Layers | Modules | Description |
|------|------|--------|---------|-------------|
| [system-architecture.mmd](system-architecture.mmd) | Graph | 4 | 164 | Complete system overview with all modules, transports, and dependencies |
| [supervision-tree.mmd](supervision-tree.mmd) | Graph | 3 | 50+ | OTP supervision hierarchy with failure isolation patterns |

### Behavior & Flow

| File | Type | Actors | Steps | Description |
|------|------|--------|-------|-------------|
| [data-flow.mmd](data-flow.mmd) | Sequence | 16 | 75+ | End-to-end message flow including chaos testing and security |

### Detailed Design

| File | Type | Nodes | Edges | Description |
|------|------|-------|-------|-------------|
| [module-dependencies.mmd](module-dependencies.mmd) | Graph | 164 | 200+ | Complete dependency graph across all layers |
| [transport-interfaces.mmd](transport-interfaces.mmd) | Graph | 40+ | 80+ | Transport behavior contracts and implementations |

## Diagrams by Complexity

### Simple (Quick Overview)

- None - All diagrams are complex due to system size

### Medium (Detailed View)

- [supervision-tree.mmd](supervision-tree.mmd) - OTP patterns and supervision
- [transport-interfaces.mmd](transport-interfaces.mmd) - Transport design

### Complex (Comprehensive)

- [system-architecture.mmd](system-architecture.mmd) - Full system topology
- [data-flow.mmd](data-flow.mmd) - Complete protocol behavior
- [module-dependencies.mmd](module-dependencies.mmd) - All dependencies

## Diagrams by Use Case

### Onboarding New Developers

1. **Start:** [system-architecture.mmd](system-architecture.mmd) - Understand the big picture
2. **Then:** [supervision-tree.mmd](supervision-tree.mmd) - Learn OTP patterns
3. **Finally:** [data-flow.mmd](data-flow.mmd) - See protocol in action

### Architecture Review

1. **Overview:** [system-architecture.mmd](system-architecture.mmd)
2. **Dependencies:** [module-dependencies.mmd](module-dependencies.mmd)
3. **Supervision:** [supervision-tree.mmd](supervision-tree.mmd)

### Troubleshooting Issues

1. **Trace flows:** [data-flow.mmd](data-flow.mmd)
2. **Check failures:** [supervision-tree.mmd](supervision-tree.mmd)
3. **Transport debug:** [transport-interfaces.mmd](transport-interfaces.mmd)

### Implementing Features

1. **Find context:** [system-architecture.mmd](system-architecture.mmd)
2. **Check impact:** [module-dependencies.mmd](module-dependencies.mmd)
3. **Follow patterns:** [supervision-tree.mmd](supervision-tree.mmd)

### Performance Optimization

1. **Identify bottlenecks:** [data-flow.mmd](data-flow.mmd)
2. **Transport tuning:** [transport-interfaces.mmd](transport-interfaces.mmd)
3. **Module analysis:** [module-dependencies.mmd](module-dependencies.mmd)

### Security Analysis

1. **Data flows:** [data-flow.mmd](data-flow.mmd)
2. **Auth flow:** See security section in [data-flow.mmd](data-flow.mmd)
3. **Transport security:** [transport-interfaces.mmd](transport-interfaces.mmd)

## Diagrams by Layer

### Core Layer (97 modules)

- **system-architecture.mmd** - Core processing, sessions, MCP capabilities, security
- **module-dependencies.mmd** - Core protocol, registry, session, capabilities dependencies
- **data-flow.mmd** - Core message processing flow

### Transport Layer (23 modules)

- **system-architecture.mmd** - Transport implementations (stdio, tcp, http, ws, sse)
- **transport-interfaces.mmd** - Complete transport behavior and design
- **module-dependencies.mmd** - Transport module dependencies

### Observability Layer (31 modules)

- **system-architecture.mmd** - OTEL, metrics, chaos engineering, monitoring
- **module-dependencies.mmd** - Observability dependencies
- **data-flow.mmd** - Observability integration points

### Validation Layer (13 modules)

- **system-architecture.mmd** - Protocol, transport, security, performance validators
- **module-dependencies.mmd** - Validation dependencies
- **data-flow.mmd** - Compliance validation flows

## Diagrams by Technology

### OTP Patterns

- [supervision-tree.mmd](supervision-tree.mmd)
  - Supervision strategies (one_for_all, one_for_one, simple_one_for_one)
  - Process isolation patterns
  - Failure domains

### gproc Registry

- [system-architecture.mmd](system-architecture.mmd) - Registry in context
- [supervision-tree.mmd](supervision-tree.mmd) - Registry as standalone process
- [data-flow.mmd](data-flow.mmd) - Registry routing

### Transport Implementations

- [transport-interfaces.mmd](transport-interfaces.mmd) - Complete transport design
  - STDIO (process I/O)
  - TCP (ranch acceptors)
  - HTTP (gun client)
  - WebSocket (full-duplex)
  - SSE (unidirectional)

### Session Management

- [system-architecture.mmd](system-architecture.mmd) - Session architecture
- [module-dependencies.mmd](module-dependencies.mmd) - Session dependencies
- [data-flow.mmd](data-flow.mmd) - Session lifecycle and failover

### Security

- [system-architecture.mmd](system-architecture.mmd) - Auth, mTLS, rate limiting, secrets
- [transport-interfaces.mmd](transport-interfaces.mmd) - TLS, CORS validation
- [data-flow.mmd](data-flow.mmd) - Security flow and auth validation

### Chaos Engineering

- [system-architecture.mmd](system-architecture.mmd) - Chaos coordinator and workers
- [data-flow.mmd](data-flow.mmd) - Chaos injection scenarios
- [supervision-tree.mmd](supervision-tree.mmd) - Chaos supervision

## Diagram Statistics

| Metric | Value |
|--------|-------|
| **Total Diagrams** | 5 |
| **Total Lines** | 1,178 |
| **Total Nodes** | 350+ |
| **Total Edges** | 400+ |
| **Layers Documented** | 4 |
| **Modules Covered** | 164 / 164 (100%) |
| **Transports Covered** | 5 / 5 (100%) |
| **Supervision Tiers** | 3 / 3 (100%) |

## Diagram Relationships

```
system-architecture.mmd (Overview)
        ↓
        ├─→ supervision-tree.mmd (OTP patterns)
        ├─→ transport-interfaces.mmd (Transport design)
        └─→ module-dependencies.mmd (Dependencies)
                ↓
        data-flow.mmd (Behavior integration)
```

## Quick Lookup

### I want to understand...

**The big picture**
→ [system-architecture.mmd](system-architecture.mmd)

**How processes are supervised**
→ [supervision-tree.mmd](supervision-tree.mmd)

**How messages flow through the system**
→ [data-flow.mmd](data-flow.mmd)

**Module dependencies and coupling**
→ [module-dependencies.mmd](module-dependencies.mmd)

**Transport design and implementation**
→ [transport-interfaces.mmd](transport-interfaces.mmd)

**Where a module fits in the architecture**
→ [system-architecture.mmd](system-architecture.mmd) then [module-dependencies.mmd](module-dependencies.mmd)

**How to implement a custom transport**
→ [transport-interfaces.mmd](transport-interfaces.mmd)

**Failure isolation and recovery**
→ [supervision-tree.mmd](supervision-tree.mmd) and [data-flow.mmd](data-flow.mmd)

**Security implementation**
→ [data-flow.mmd](data-flow.mmd) security section

**Performance characteristics**
→ [transport-interfaces.mmd](transport-interfaces.mmd) benchmarks section

**Chaos testing scenarios**
→ [data-flow.mmd](data-flow.mmd) chaos injection section

## File Size Reference

| File | Lines | Size (KB) | Render Time |
|------|-------|-----------|-------------|
| system-architecture.mmd | 226 | ~8.5 | Fast |
| supervision-tree.mmd | 205 | ~7.7 | Fast |
| data-flow.mmd | 175 | ~6.6 | Fast |
| module-dependencies.mmd | 349 | ~13.1 | Medium |
| transport-interfaces.mmd | 223 | ~8.4 | Fast |

## Rendering Compatibility

| Platform | System Arch | Supervision | Data Flow | Dependencies | Transport |
|----------|-------------|-------------|-----------|--------------|-----------|
| GitHub | ✅ | ✅ | ✅ | ✅ | ✅ |
| GitLab | ✅ | ✅ | ✅ | ✅ | ✅ |
| VS Code | ✅ | ✅ | ✅ | ⚠️ | ✅ |
| Mermaid Live | ✅ | ✅ | ✅ | ✅ | ✅ |
| CLI (mmdc) | ✅ | ✅ | ✅ | ✅ | ✅ |

✅ = Full support
⚠️ = May need zoom for large diagrams

## Related Documentation

- [README.md](README.md) - Main diagram documentation
- [usage.md](usage.md) - How to use and customize
- [glossary.md](glossary.md) - Mermaid syntax reference

---

**Version:** 2.1.0
**Total Diagrams:** 5
**Last Updated:** 2026-01-31
