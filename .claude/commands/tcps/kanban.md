---
name: tcps-kanban
description: Kanban (看板) - WIP limits, prevent bottlenecks
category: tcps
invokes_agent: sparc-orchestrator
uses_rules: rules-kanban
japanese_term: 看板 (Kanban - Work Signal)
manufacturing_stage: "Stages 3-5: Generate/Extract/Render"
consolidates: [sparc/integrate, sparc/integration, sparc/mcp]
---

# Command: /tcps kanban

## Purpose
**Japanese**: 看板 (Kanban - Signboard)
**Manufacturing Stage**: Stages 3-5 - Limit work-in-progress
**TCPS Pillar**: Kanban (WIP limits, visible flow, surface bottlenecks)

Manage work-in-progress limits (max 3 active builds, max 2 in testing). Prevents context switching and surfaces bottlenecks immediately.

## Usage
```bash
/tcps kanban [status|limits|board]
```

## WIP Limits

```erlang
-define(MAX_WIP_ACTIVE_BUILDS, 3).
-define(MAX_WIP_QUEUED, 10).
-define(MAX_WIP_BLOCKED, 5).
```

## Examples

### Check Kanban Status
```bash
/tcps kanban status
```

Output:
```
┌─────────────┬──────────────┬──────────────┬─────────────┐
│   Backlog   │  In Progress │   Testing    │  Released   │
│             │   (WIP 2/3)  │  (WIP 1/2)   │             │
├─────────────┼──────────────┼──────────────┼─────────────┤
│ wo-003      │ wo-001 ████  │ wo-002 ████  │ v0.5.0 ✓    │
│ wo-004      │ wo-005 ███   │              │ v0.6.0 ✓    │
│ wo-006      │              │              │             │
│ wo-008      │              │              │             │
└─────────────┴──────────────┴──────────────┴─────────────┘
```

### Show WIP Limits
```bash
/tcps kanban limits
```

Output:
```
Active Builds: 2/3 (66% capacity)
Testing: 1/2 (50% capacity)
Queued: 4/10 (40% capacity)
Blocked: 0/5 (0% capacity)

Can accept 1 more work order.
```

## See Also
- Previous: `/tcps heijunka` - Production leveling
- Next: `/tcps build` - Standard work
- Docs: `docs/tcps/TCPS.md` - Pillar 4: Kanban

---

**Command Version**: 1.0.0 (TCPS-aligned)
**Replaces**: /sparc integrate, /sparc integration, /sparc mcp
**Manufacturing Stage**: Stages 3-5 of 8
