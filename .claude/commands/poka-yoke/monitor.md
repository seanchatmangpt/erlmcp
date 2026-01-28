---
name: poka-yoke-monitor
description: Poka-yoke (ポカヨケ) - SLA monitoring and compliance
category: poka-yoke
invokes_agent: erlang-performance
japanese_term: ポカヨケ (Poka-yoke)
consolidates: [perf/monitor, monitoring/agent-metrics, monitoring/real-time-view]
---

# Command: /poka-yoke-monitor

## Purpose
Monitor SLA compliance in real-time. Detect violations before they impact customers.

## Usage
```bash
/poka-yoke-monitor sla [team|enterprise|gov]
```

## Examples
```bash
/poka-yoke-monitor sla team
```

Output:
```
Team Tier SLA Status:
✓ Throughput: 450 req/s (target: 450 req/s)
✓ Latency p99: 85ms (target: <150ms)
✓ Failover: <5s (target: <5s)
✓ Compliance: 100% (60-minute rolling window)
```

---

**Command Version**: 1.0.0 (TCPS-aligned)
**Replaces**: /perf monitor, /monitoring/*
