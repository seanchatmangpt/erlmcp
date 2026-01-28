---
name: tcps-pull
description: Just-In-Time (JIT) - Pull work orders from demand signals
category: tcps
invokes_agent: plan-designer + erlang-researcher
uses_rules: rules-spec-pseudocode
japanese_term: Just-In-Time (JIT)
manufacturing_stage: "Stage 1: Pull"
consolidates: [sparc/spec, sparc/spec-pseudocode]
---

# Command: /tcps pull

## Purpose
**Japanese**: Just-In-Time (JIT)
**Manufacturing Stage**: Stage 1 - Pull demand signals, create work orders
**TCPS Pillar**: Just-In-Time (build only what's pulled, no push)

Create work orders from actual demand signals (marketplace installs, GitHub issues, security advisories). No speculative features—build only what customers demand.

## Usage
```bash
/tcps pull [sku-name]
```

## Agent Invocation
Spawns: `plan-designer` (creates work orders) + `erlang-researcher` (analyzes demand)
Loads: `.roo/rules-spec-pseudocode/` rules

## Manufacturing Flow

```
Demand Signal (marketplace spike)
       ↓
   Pull Signal
       ↓
Create Work Order (ontology/work_orders.ttl)
       ↓
Emit Receipt (receipts/pull/wo-*.json)
```

## Examples

### Pull from Marketplace Demand
```bash
/tcps pull erlmcp-http2-transport
```

**What happens**:
1. Detect demand signal (marketplace install spike for HTTP/2 feature)
2. Create work order in `ontology/work_orders.ttl`:
   ```turtle
   :wo-001 a taiea:WorkOrder ;
       taiea:requestedSKU "erlmcp-http2-transport" ;
       taiea:priority "high" ;
       taiea:dueDate "2026-02-15"^^xsd:date ;
       taiea:demandSource :marketplace-install-spike .
   ```
3. Emit receipt: `receipts/pull/wo-001.json`
4. Ready for heijunka scheduling

### Pull from Security Advisory
```bash
/tcps pull security-patch-tls
```

**What happens**:
1. Detect security advisory (CVE-2026-001)
2. Create high-priority work order
3. Auto-assign to security work bucket
4. Emit receipt with CVE reference

## Manufacturing Principle

> **"Build only what is pulled. No push, no guessing, no batching."**

Traditional software builds features from roadmaps (push). TCPS builds from actual demand (pull).

**Benefits**:
- No wasted effort on unused features
- Inventory (WIP) stays minimal
- Production matches actual demand
- Resources allocated to highest value

## Work Order Schema

Every pull creates a work order with:
- **requestedSKU**: What to build (e.g., "erlmcp-http-transport-gun")
- **priority**: high | medium | low
- **dueDate**: Target completion
- **demandSource**: Where signal came from (marketplace | github | security | customer)
- **workBucket**: reliability | security | cost-reduction | new-feature

## Receipt Output

```json
{
  "receipt_id": "pull-20260127-001",
  "timestamp": "2026-01-27T10:30:00Z",
  "stage": "pull",
  "work_order_id": "wo-001",
  "demand_signal": {
    "source": "marketplace",
    "type": "install_spike",
    "feature": "http2-transport",
    "count": 47
  },
  "sku": "erlmcp-http2-transport",
  "priority": "high",
  "work_bucket": "new-feature",
  "next_stage": "heijunka",
  "hash": "sha256:abc123..."
}
```

## Failure Modes

| Failure | Cause | Recovery |
|---------|-------|----------|
| Invalid demand source | Unrecognized signal type | Verify demand source, update ontology |
| Duplicate work order | SKU already in progress | Check Kanban board, may queue |
| Missing priority | Work order incomplete | Trigger Andon, fix work order template |

## Integration

**After pull**:
1. Work order → Heijunka (leveling)
2. Heijunka → Kanban (WIP management)
3. Kanban → Build (standard work)

**Next command**: `/tcps heijunka` - Schedule work orders

## See Also
- Next stage: `/tcps heijunka` - Production leveling
- Related: `/work-order create` - Manual work order creation
- Related: `/demand signal` - Detect demand signals
- Docs: `docs/tcps/TCPS.md` - Pillar 1: Just-In-Time

---

**Command Version**: 1.0.0 (TCPS-aligned)
**Replaces**: /sparc spec, /sparc spec-pseudocode
**Manufacturing Stage**: Stage 1 of 8
