---
name: sparc-spec
description: Specification and Pseudocode phase - Requirements analysis and algorithm design
category: sparc
invokes_agent: plan-designer + erlang-researcher
uses_rules: rules-spec-pseudocode
consolidates: [spec-pseudocode]
---

# Command: /sparc spec

## Purpose
Execute the SPARC Specification and Pseudocode phase to analyze requirements and design algorithms before implementation.

## Usage
```bash
/sparc spec [module]
```

## Agent Invocation
Spawns: `plan-designer` (Research → Plan → Execute workflow) and `erlang-researcher` (codebase analysis)
Loads: `.roo/rules-spec-pseudocode/` rules

## Examples

### Specify New gen_server Module
```bash
/sparc spec erlmcp_cache
```

This will:
1. Research existing gen_server patterns in erlmcp
2. Analyze caching requirements
3. Define API contracts and edge cases
4. Create pseudocode for cache logic

### Specify Transport Layer
```bash
/sparc spec transport-websocket
```

This will:
1. Research erlmcp_transport behavior
2. Analyze WebSocket protocol requirements
3. Define callback specifications
4. Create pseudocode for message handling

## Workflow

### Phase 1: Research (erlang-researcher)
- Explore erlmcp codebase structure
- Identify existing patterns and conventions
- Document architectural decisions
- Preserve main context by delegating research

### Phase 2: Specification (plan-designer)
- Gather functional requirements
- Identify edge cases and constraints
- Define API contracts and type specifications
- Document quality requirements (coverage, performance)

### Phase 3: Pseudocode (plan-designer)
- Translate requirements into algorithm design
- Design state management approach
- Plan supervision strategy
- Define test strategy

## Behavioral Rules (from rules-spec-pseudocode/)

When in specification phase:

**MUST DO**:
- Document ALL requirements before implementation
- Identify ALL edge cases
- Define API contracts clearly
- Specify quality gates (80%+ coverage)

**MUST NOT**:
- Write implementation code (only pseudocode)
- Skip pseudocode phase
- Make undocumented assumptions

## Output Artifacts

After `/sparc spec` completion:
- `docs/specs/<module>-specification.md` - Functional requirements
- `docs/specs/<module>-pseudocode.md` - Algorithm design
- `docs/specs/<module>-api-contract.md` - API specifications

## See Also
- Next phase: `/sparc architect` - Architecture design
- Related agents: `erlang-researcher`, `plan-designer`
- Related rules: `.roo/rules-spec-pseudocode/`

---

**Command Version**: 1.0.0
**Consolidates**: spec-pseudocode (renamed for clarity)
