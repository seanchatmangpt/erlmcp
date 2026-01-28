---
name: work-order-assign
description: Assign work order to agent
category: work-order
japanese_term: 作業指示 (Sagyou Shiji)
consolidates: [swarm/spawn, hive-mind/hive-mind-spawn, coordination/agent-spawn]
---

# Command: /work-order-assign

## Purpose
Assign work order to specific agent for execution.

## Usage
```bash
/work-order-assign <wo-id> <agent-name>
```

## Examples
```bash
/work-order-assign wo-001 erlang-otp-developer
```

---

**Command Version**: 1.0.0 (TCPS-aligned)
**Replaces**: /swarm spawn, /hive-mind-spawn
