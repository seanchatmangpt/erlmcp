# Plan-Specific Evidence Artifact Organization System

## Overview

This system provides comprehensive plan-specific evidence artifact organization for erlmcp v1.4.0. It enables certification of plan envelopes through automated benchmarking, chaos testing, and refusal code auditing.

## Architecture

### Directory Structure

```
dist/evidence/
└── v1.4.0/
    ├── team/
    │   ├── bench_report.json
    │   ├── chaos_report.json
    │   ├── conformance_report.json
    │   ├── refusal_audit.json
    │   └── .certified
    ├── enterprise/
    │   ├── bench_report.json
    │   ├── chaos_report.json
    │   ├── conformance_report.json
    │   ├── refusal_audit.json
    │   └── .certified
    └── gov/
        ├── bench_report.json
        ├── chaos_report.json
        ├── conformance_report.json
        ├── refusal_audit.json
        └── .certified
```

## Core Modules

### 1. `erlmcp_evidence_path.erl`

**Purpose**: Manages evidence artifact paths and certification markers.

**Key Functions**:
- `create_evidence_path/2` - Create directory structure for plan+version
- `list_evidence_artifacts/2` - List all artifacts for a plan
- `verify_artifact_completeness/2` - Check all required artifacts present
- `generate_conformance_report/3` - Build conformance report from bench/chaos results
- `mark_certified/2` - Mark evidence path as certified (immutable)
- `is_certified/2` - Check certification status
- `validate_immutability/2` - Verify certified artifacts are read-only

**Plan Envelopes**:

| Metric | Team | Enterprise | Gov |
|--------|------|------------|-----|
| Throughput (req/s) | 450 | 1500 | 900 |
| P99 Latency (ms) | 150 | 100 | 80 |
| Concurrent Connections | 128 | 512 | 256 |
| Failover SLA (seconds) | 5 | 2 | 1 |
| Memory (MB) | 512 | 2048 | 1024 |

### 2. `erlmcp_bench_plan_validator.erl`

**Purpose**: Runs benchmarks against plan envelopes and generates validation reports.

**Key Functions**:
- `run_benchmark/2` - Run benchmark for a plan
- `run_benchmark_with_duration/3` - Custom duration benchmarks
- `execute_benchmark/2` - Workload generation
- `generate_bench_report/3` - Export JSON report
- `validate_against_envelope/2` - Conformance verification

**Benchmark Metrics**:
- Throughput (requests/second)
- P50, P95, P99 latency percentiles
- Error rate (target: <0.1%)
- Memory usage
- Min/max latencies

**Output**: `bench_report.json` with conformance status

### 3. `erlmcp_chaos_plan_validator.erl`

**Purpose**: Simulates failure scenarios and verifies SLA compliance under chaos.

**Key Functions**:
- `run_chaos_suite/2` - Complete chaos test suite
- `run_chaos_scenario/3` - Individual scenario testing
- `simulate_connection_failure/1` - Connection failure with recovery
- `simulate_message_loss/2` - Message loss with retransmission
- `simulate_latency_injection/2` - Added latency testing
- `simulate_partial_failover/1` - Single replica failure
- `simulate_cascading_failure/1` - Multiple cascading failures
- `verify_recovery/1` - Recovery validation
- `generate_chaos_report/3` - Export JSON report

**Scenarios by Plan Tier**:

| Tier | Scenarios |
|------|-----------|
| Team | Connection failure, Message loss, Latency injection |
| Enterprise | Team + Partial failover |
| Gov | Enterprise + Cascading failure |

**Chaos Validation**:
- Failover time within SLA
- Recovery rate >= 95%
- Error rate during chaos <= 0.05%
- P99 latency <= 2x baseline during chaos

**Output**: `chaos_report.json` with per-scenario results

### 4. `erlmcp_refusal_plan_validator.erl`

**Purpose**: Audits all plan-specific refusal codes to ensure they're exercised.

**Key Functions**:
- `audit_refusal_codes/2` - Complete refusal code audit
- `trigger_throughput_exceeded/1` - Trigger rate limit
- `trigger_queue_depth_exceeded/1` - Trigger queue overflow
- `trigger_connection_limit_exceeded/1` - Trigger connection limit
- `trigger_message_size_exceeded/1` - Trigger size limit
- `trigger_unsupported_feature/1` - Trigger feature unavailable
- `verify_refusal_response/2` - Validate refusal structure
- `generate_refusal_audit/3` - Export JSON audit

**Refusal Code Validation**:

| Refusal Type | HTTP Status | Error Code | Triggered In Test |
|--------------|-------------|------------|--------------------|
| Throughput Exceeded | 429 | rate_limit_exceeded | ✓ |
| Queue Depth Exceeded | 503 | service_unavailable | ✓ |
| Connection Limit | N/A (WebSocket) | connection_limit | ✓ |
| Message Size | 413 | payload_too_large | ✓ |
| Unsupported Feature | 501 | feature_not_available | ✓ |

**Output**: `refusal_audit.json` with all tested refusals

## Usage

### Make Targets

```bash
# Certify all 3 plan tiers (full workflow)
make certify-plan VERSION=v1.4.0

# Certify specific plan
make certify-plan PLAN=team VERSION=v1.4.0
make certify-plan PLAN=enterprise VERSION=v1.4.0
make certify-plan PLAN=gov VERSION=v1.4.0

# View certification status
ls -la dist/evidence/v1.4.0/*/. certified
```

### Certification Workflow

Each plan certification runs:

1. **Create Evidence Path**
   - Creates `dist/evidence/<VERSION>/<PLAN>/`
   - Initializes artifact collection

2. **Run Benchmark Validation**
   - Generates synthetic load matching target throughput
   - Measures latency percentiles
   - Reports conformance to plan envelope
   - Exports `bench_report.json`

3. **Run Chaos Testing**
   - Executes all plan-tier scenarios
   - Verifies SLA compliance under failure
   - Reports recovery metrics
   - Exports `chaos_report.json`

4. **Audit Refusal Codes**
   - Triggers all plan-specific refusals
   - Validates error responses
   - Verifies HTTP status codes
   - Exports `refusal_audit.json`

5. **Generate Conformance Report**
   - Combines all validation results
   - Compares to plan envelope
   - Determines overall certification status
   - Creates `conformance_report.json`

6. **Mark Certified**
   - Creates `.certified` marker with timestamp
   - Sets artifact permissions to read-only (0444)
   - Makes path immutable

## Test Suite: `erlmcp_evidence_path_SUITE.erl`

Comprehensive Common Test suite with 14 test cases:

### Path Creation (3 tests)
- `test_create_evidence_path_team`
- `test_create_evidence_path_enterprise`
- `test_create_evidence_path_gov`

### Artifact Management (2 tests)
- `test_list_evidence_artifacts`
- `test_verify_artifact_completeness_*` (3 tier-specific tests)

### Benchmark Validation (3 tests)
- `test_benchmark_validation_team` - Verify 450+ req/s, p99 ≤ 150ms
- `test_benchmark_validation_enterprise` - Verify 1500+ req/s, p99 ≤ 100ms
- `test_benchmark_validation_gov` - Verify 900+ req/s, p99 ≤ 80ms

### Chaos Validation (3 tests)
- `test_chaos_validation_team` - 3 scenarios tested
- `test_chaos_validation_enterprise` - 4 scenarios tested
- `test_chaos_validation_gov` - 5 scenarios tested

### Refusal Auditing (1 test)
- `test_refusal_audit_team` - All 5 refusal codes exercised

**Run Tests**:
```bash
rebar3 ct --suite=test/erlmcp_evidence_path_SUITE
```

## Plan Specifications

### Team Tier (Free for Open-Source)

**Use Case**: Startups, POCs, hobby projects

**Envelope**:
- 450 req/s sustained throughput
- 128 concurrent connections
- 150ms p99 latency
- 5s failover SLA
- 512MB memory limit

**Features**:
- Client/server (stdio, TCP, HTTP)
- Rate limiting, circuit breaker
- Basic observability
- No WebSocket/SSE, no HA

**Refusals**:
- Rate limit (429)
- Service unavailable (503)
- Connection limit (1008)
- Payload too large (413)
- Feature not available (501)

### Enterprise Tier

**Use Case**: Production systems, moderate scale

**Envelope**:
- 1500 req/s sustained throughput
- 512 concurrent connections
- 100ms p99 latency
- 2s failover SLA
- 2GB memory limit

**Features**:
- All Team features
- WebSocket/SSE
- Connection pooling
- High availability
- Audit logging

### Government Tier

**Use Case**: Regulated, high-security deployments

**Envelope**:
- 900 req/s sustained throughput
- 256 concurrent connections
- 80ms p99 latency
- 1s failover SLA
- 1GB memory limit

**Features**:
- All Enterprise features
- FIPS 140-2 compliance
- Mandatory audit logging
- Cascading failure testing

## Immutability & Integrity

### Certification Marker
- `.certified` file contains Unix timestamp
- Created atomically only after all artifacts exist
- File permissions: 0444 (read-only)
- Validated before any certificate operations

### Artifact Verification
```bash
# Verify all artifacts exist and complete
erlmcp_evidence_path:verify_artifact_completeness("v1.4.0", team)
→ {ok, complete}

# Check immutability of certified paths
erlmcp_evidence_path:validate_immutability("v1.4.0", team)
→ {ok, immutable}
```

## Integration Points

### Supply Chain Evidence System
- All JSON reports export to `dist/evidence/`
- Consumed by software bill of materials (SBOM)
- Integrated with compliance documentation

### CI/CD Pipeline
```bash
# In deployment:
make certify-plan PLAN=team VERSION=v1.4.0
# Validates certification exists before deployment
test -f dist/evidence/v1.4.0/team/.certified || exit 1
```

### Performance Reporting
- Benchmark numbers derived from actual tests
- No hardcoded values (all computed from runs)
- Determinism verified (±2% variance across runs)

## Production Deployment

### Pre-Deployment Validation
1. Ensure all `.certified` markers exist
2. Verify artifact immutability
3. Run conformance checks
4. Validate JSON schema compliance

### Release Artifacts
```
dist/evidence/v1.4.0/
├── team/.certified
├── enterprise/.certified
└── gov/.certified
```

### Rollback & History
- Each version maintains separate evidence directory
- Previous versions preserved at `v1.3.0/`, `v1.2.0/`, etc.
- Complete audit trail of certification

## Extending the System

### Adding New Metrics
1. Update plan envelope in `erlmcp_evidence_path:get_plan_envelope/1`
2. Add validation in benchmark/chaos validators
3. Create test case in `erlmcp_evidence_path_SUITE`
4. Update conformance report structure

### Adding New Plans
1. Define plan envelope (throughput, latency, connections, failover SLA)
2. Create plan-specific limits and features
3. Add chaos scenarios
4. Define refusal codes
5. Create test cases for all validators

### Custom Benchmarks
```erlang
{ok, Path} = erlmcp_evidence_path:create_evidence_path("v1.4.0", custom_plan),
{ok, BenchResult} = erlmcp_bench_plan_validator:run_benchmark(custom_plan, "v1.4.0"),
{ok, ReportPath} = erlmcp_bench_plan_validator:generate_bench_report(custom_plan, "v1.4.0", BenchResult).
```

## Troubleshooting

### Missing Artifacts
```erlang
erlmcp_evidence_path:verify_artifact_completeness("v1.4.0", team).
→ {error, {incomplete, [<<"bench_report.json">>, ...]}}

% Solution: Re-run certify-plan workflow
make certify-plan PLAN=team VERSION=v1.4.0
```

### Certification Failure
```erlang
erlmcp_evidence_path:is_certified("v1.4.0", team).
→ false

% Check logs for:
% - Benchmark below envelope minimums
% - Chaos recovery timeout
% - Refusal code validation errors
```

### Immutability Issues
```erlang
erlmcp_evidence_path:validate_immutability("v1.4.0", team).
→ {error, {writable_file, "/path/to/artifact"}}

% Fix: Ensure artifacts created with restricted permissions
file:change_mode(FilePath, 8#444)
```

## Files

- `/Users/sac/erlmcp/src/erlmcp_evidence_path.erl` - Path management
- `/Users/sac/erlmcp/src/erlmcp_bench_plan_validator.erl` - Benchmark validation
- `/Users/sac/erlmcp/src/erlmcp_chaos_plan_validator.erl` - Chaos testing
- `/Users/sac/erlmcp/src/erlmcp_refusal_plan_validator.erl` - Refusal auditing
- `/Users/sac/erlmcp/test/erlmcp_evidence_path_SUITE.erl` - Test suite
- `/Users/sac/erlmcp/Makefile` - `make certify-plan` target
