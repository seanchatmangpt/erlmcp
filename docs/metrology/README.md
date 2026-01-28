# Metrology v1.5.0 Documentation Index

**Canonical Performance Metrics for erlmcp**

---

## Quick Links

| Document | Purpose |
|----------|---------|
| **[METRICS_GLOSSARY.md](METRICS_GLOSSARY.md)** | Canonical metric definitions (v1.5.0) |
| **[V1.5.0_MIGRATION_GUIDE.md](V1.5.0_MIGRATION_GUIDE.md)** | Step-by-step migration from v1.4.0 |
| **[V1.5.0_VALIDATION_REPORT.md](V1.5.0_VALIDATION_REPORT.md)** | Comprehensive validation results |

---

## What is Metrology v1.5.0?

v1.5.0 introduces **canonical metrology standards** that eliminate ambiguous performance metrics and establish reproducible benchmarking practices for erlmcp.

**Key Changes**:
- ❌ Deprecated: `req/s`, `MiB/conn`, ambiguous `memory_mb`
- ✅ New: `msg_per_s`, component-specific memory breakdown
- ✅ Required: Every metric MUST reference a `workload_id`

---

## Documentation Structure

### 1. METRICS_GLOSSARY.md (Source of Truth)

**Canonical metric definitions**:
- `msg_per_s` - Messages per second (requests + responses)
- `per_connection_heap_mib` - Process heap memory
- `per_connection_state_mib` - Application state size
- `per_node_base_overhead_mib` - Fixed VM overhead
- `per_node_total_rss_mib` - Total Resident Set Size
- Latency percentiles (p50, p95, p99)
- Transport types (tcp, http_sse, stdio, websocket)
- Scope definitions (per_node, per_cluster, per_connection)

**Use this when**: You need to understand what a metric means or how to use it.

### 2. V1.5.0_MIGRATION_GUIDE.md (How-To)

**Step-by-step migration instructions**:
1. Identify ambiguous metrics
2. Create workload definitions
3. Transform plan files
4. Add metrology compliance section
5. Validate with zero violations

**Use this when**: You're migrating from v1.4.0 to v1.5.0.

### 3. V1.5.0_VALIDATION_REPORT.md (Evidence)

**Comprehensive validation results**:
- Plan-by-plan validation details
- Before/after transformation examples
- Complete file inventory
- Compliance summary

**Use this when**: You need proof of compliance or transformation details.

---

## Workload Definitions

Location: `bench/workloads/*.json`

| Workload ID | Tier | Description |
|-------------|------|-------------|
| `tcp_sustained_25k_1kib` | Team | 25K TCP connections, 1 KiB messages |
| `tcp_sustained_100k_1kib` | Enterprise | 100K TCP connections, 1 KiB messages |
| `tcp_sustained_50k_1kib_gov` | Government | 50K TCP, FIPS-140-2 encrypted |
| `ha_failover_test` | Enterprise | HA cluster failover test |
| `ha_failover_test_fips` | Government | HA failover with FIPS-140-2 |
| `http_sse_sustained_50k` | All | HTTP SSE transport, 50K connections |
| `stdio_sequential_1k` | All | Standard I/O, 1K sequential messages |

---

## Environment Specifications

Location: `bench/environments/*.json`

| Environment | CPU | RAM | Use Case |
|-------------|-----|-----|----------|
| `prod_hw_spec_01` | 16 vCPU | 64 GB | Production tier 1 |
| `prod_hw_spec_02` | 32 vCPU | 128 GB | Production tier 2 |
| `gov_fips_hardware` | 16 vCPU | 64 GB | FIPS-140-2 certified |
| `dev_laptop` | 8 vCPU | 16 GB | Local development |

---

## Validation Tools

Location: `scripts/`

**validate_plan_metrology.py** (Primary):
```bash
python3 scripts/validate_plan_metrology.py
```

**validate_plan_metrology.sh** (Bash):
```bash
./scripts/validate_plan_metrology.sh
```

Both tools validate:
- ✅ No prohibited patterns (req/s, MiB/conn)
- ✅ All workload_id references exist
- ✅ Required metrology fields present
- ✅ Metrology compliance version = v1.5.0

---

## Quick Reference: Metric Transformations

### Throughput

```
v1.4.0: "throughput_req_s": 450
   ↓
v1.5.0: "throughput": {
  "value": 900,
  "unit": "msg_per_s",
  "workload_id": "tcp_sustained_25k_1kib",
  "transport": "tcp"
}
```

### Memory

```
v1.4.0: "memory_limit_mb": 512
   ↓
v1.5.0: "memory": {
  "per_connection_heap_mib": 0.048,
  "per_connection_state_mib": 0.012,
  "per_node_total_rss_mib": 1536,
  "workload_id": "tcp_sustained_25k_1kib"
}
```

### Latency

```
v1.4.0: "p99_latency_ms": 150
   ↓
v1.5.0: "latency": {
  "p99_ms": 150,
  "workload_id": "tcp_sustained_25k_1kib",
  "transport": "tcp",
  "measurement_point": "client_to_client"
}
```

---

## Compliance Status

| Plan File | Status | Violations | Version |
|-----------|--------|------------|---------|
| team.plan.json | ✅ PASS | 0 | v1.5.0 |
| enterprise.plan.json | ✅ PASS | 0 | v1.5.0 |
| gov.plan.json | ✅ PASS | 0 | v1.5.0 |

**Last Validated**: 2026-01-27

---

## Related Documentation

- **[Pricing Plans README](../PRICING_PLANS_README.md)** - Overall pricing system
- **[Plan Files](../../plans/)** - Actual plan specifications
- **[Workload Definitions](../../bench/workloads/)** - Benchmark workloads
- **[Environment Specs](../../bench/environments/)** - Hardware profiles

---

## Contact

Questions about metrology standards?
- **Issue Tracker**: github.com/erlmcp/erlmcp/issues
- **Tag**: `metrology`, `v1.5.0`

---

**Last Updated**: 2026-01-27
**Metrology Version**: v1.5.0
**Status**: CANONICAL
