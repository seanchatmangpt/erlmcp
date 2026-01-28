---
name: tcps-heijunka
description: Heijunka (平準化) - Production leveling, balance work types
category: tcps
invokes_agent: erlang-architect
uses_rules: rules-architect
japanese_term: 平準化 (Heijunka - Production Leveling)
manufacturing_stage: "Stage 2: Plan"
consolidates: [sparc/architect, sparc/architecture]
---

# Command: /tcps-heijunka

## Purpose
**Japanese**: 平準化 (Heijunka - Production Leveling)
**Manufacturing Stage**: Stage 2 - Plan production schedule, level load
**TCPS Pillar**: Heijunka (smooth throughput, no batching, avoid mura/muri)

Level production load across work types (reliability/security/cost/features) to avoid peaks, valleys, and worker overburden. Balance the production mix for consistent flow.

## Usage
```bash
/tcps-heijunka [balance|schedule]
```

## Agent Invocation
Spawns: `erlang-architect` (designs heijunka schedule)
Loads: `.roo/rules-architect/` rules

## Manufacturing Principle

> **"Level the production load to avoid peaks and valleys. Batching creates waste."**

**Work Buckets** (balanced mix):
- 40% Reliability (bug fixes, performance)
- 30% Security (patches, audits)
- 20% Cost Reduction (optimize resources)
- 10% New Features (marketplace demands)

## Examples

### Balance Work Orders
```bash
/tcps-heijunka balance
```

Analyzes pending work orders and creates leveled schedule:
```
Week 1: 2 reliability + 2 security + 1 cost + 1 feature
Week 2: 2 reliability + 1 security + 1 cost + 1 feature
Week 3: 2 reliability + 2 security + 1 cost + 0 feature
```

### Create Production Schedule
```bash
/tcps-heijunka schedule next-batch
```

Generates `receipts/plan/schedule-20260127.json` with balanced work order sequence.

## Receipt Output

```json
{
  "receipt_id": "heijunka-20260127-001",
  "timestamp": "2026-01-27T11:00:00Z",
  "stage": "heijunka",
  "schedule": [
    {"work_order": "wo-001", "bucket": "reliability", "weight": 0.4},
    {"work_order": "wo-003", "bucket": "security", "weight": 0.3},
    {"work_order": "wo-005", "bucket": "cost-reduction", "weight": 0.2},
    {"work_order": "wo-007", "bucket": "new-feature", "weight": 0.1}
  ],
  "total_work_orders": 4,
  "duration_estimate": "2 weeks",
  "next_stage": "kanban",
  "hash": "sha256:def456..."
}
```

## See Also
- Previous: `/tcps-pull` - Just-In-Time work orders
- Next: `/tcps-kanban` - WIP management
- Docs: `docs/tcps/TCPS.md` - Pillar 5: Heijunka

---

**Command Version**: 1.0.0 (TCPS-aligned)
**Replaces**: /sparc architect
**Manufacturing Stage**: Stage 2 of 8
