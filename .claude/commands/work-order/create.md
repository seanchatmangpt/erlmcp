---
name: work-order-create
description: Create work order from demand signal
category: work-order
invokes_agent: sparc-orchestrator
japanese_term: 作業指示 (Sagyou Shiji - Work Order)
consolidates: [swarm/init, hive-mind/hive-mind-init, coordination/swarm-init]
---

# Command: /work-order-create

## Purpose
**Japanese**: 作業指示 (Sagyou Shiji - Work Order)
Create work order from demand signal (marketplace, GitHub issues, security advisories).

## Usage
```bash
/work-order-create [sku=<name>] [priority=<high|medium|low>]
```

## Examples
```bash
/work-order-create sku=erlmcp-http2 priority=high
```

---

**Command Version**: 1.0.0 (TCPS-aligned)
**Replaces**: /swarm init, /hive-mind-init, /coordination/swarm-init
