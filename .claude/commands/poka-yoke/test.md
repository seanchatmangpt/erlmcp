---
name: poka-yoke-test
description: Poka-yoke (ポカヨケ) - Conformance testing
category: poka-yoke
invokes_agent: erlang-performance
japanese_term: ポカヨケ (Poka-yoke)
consolidates: [perf/optimize, optimization/cache-manage, optimization/parallel-execute]
---

# Command: /poka-yoke-test

## Purpose
Run conformance tests, verify envelopes match reality.

## Usage
```bash
/poka-yoke-test [team|enterprise|gov]
```

## Test Suite
- Throughput test (req/s sustained)
- Latency test (p99 under target)
- Memory test (per-connection footprint)
- Failover test (recovery time)
- Concurrency test (max connections)

## Examples
```bash
/poka-yoke-test enterprise
```

Runs 21 conformance tests against enterprise envelope (1500 req/s, <100ms p99).

---

**Command Version**: 1.0.0 (TCPS-aligned)
**Replaces**: /perf optimize, /optimization/*
