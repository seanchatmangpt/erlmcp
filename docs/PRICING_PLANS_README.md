# ErlMCP Pricing Plans & Tier Documentation System

## Overview

This document describes the comprehensive pricing plans documentation system for erlmcp, which provides three service tiers with deterministic refusal behavior, documented SLAs, and machine-readable specifications.

The system includes:

1. **Plan Specifications** (`plans/*.plan.json`) - Auto-generated specifications for each tier
2. **Tier Documentation** (`docs/plans/*-tier.md`) - User-facing markdown guides
3. **CLI Management** (`erlmcp_plan_cli.erl`) - Command-line interface for plan management
4. **Plan Module** (`erlmcp_plan.erl`) - Programmatic plan API
5. **Documentation Generator** (`erlmcp_plan_docs_generator.erl`) - Auto-generates docs from specs
6. **Validation Tests** (`test/erlmcp_pricing_docs_SUITE.erl`) - 9 comprehensive tests

## Service Tiers

### Team Tier (450 req/s)

**Ideal for:** Startups, proof-of-concepts, development environments, hobby projects

- **Throughput:** 450 req/s
- **Concurrent Connections:** 128
- **P99 Latency:** 250ms
- **Features:** Basic transport, rate limiting, circuit breaker
- **Pricing:** Free for open-source, custom for commercial
- **Documentation:** `/docs/plans/team-tier.md`

### Enterprise Tier (1500 req/s)

**Ideal for:** Production applications, web services, SaaS, microservices

- **Throughput:** 1500 req/s
- **Concurrent Connections:** 512
- **P99 Latency:** 100ms
- **Features:** All transports, connection pooling, audit logging, HA
- **SLA:** 99.95% availability
- **Pricing:** Enterprise license pricing
- **Documentation:** `/docs/plans/enterprise-tier.md`

### Government Tier (900 req/s)

**Ideal for:** Government agencies, regulated environments, healthcare, finance

- **Throughput:** 900 req/s
- **Concurrent Connections:** 256
- **P99 Latency:** 150ms
- **Features:** FIPS-140-2 encryption, immutable audit logs, compliance reporting
- **SLA:** 99.99% availability
- **Audit Retention:** 7 years
- **Pricing:** Government license pricing
- **Documentation:** `/docs/plans/gov-tier.md`

## Architecture

### Plan Specification Format

Each tier has a JSON specification (`plans/{tier}.plan.json`):

```json
{
  "tier": "team",
  "name": "Team Tier - Startups, POCs, Low-Scale",
  "envelope": {
    "throughput_req_s": 450,
    "concurrent_connections": 128,
    "queue_depth_messages": 2048,
    "p99_latency_ms": 250,
    "failover_sla_seconds": 30,
    "connection_timeout_seconds": 60
  },
  "limits": {
    "max_message_size_bytes": 1048576,
    "max_payload_size_mb": 10,
    "max_concurrent_requests_per_conn": 32,
    "memory_limit_mb": 512,
    "cpu_time_limit_seconds": 300,
    "backpressure_threshold_bytes": 8388608
  },
  "features": {
    "client": true,
    "server": true,
    "websocket_transport": false,
    "audit_logging": false,
    "fips_140_2": false,
    "high_availability": false
  },
  "refusal_behavior": {
    "throughput_exceeded": {
      "http_status": 429,
      "error_code": "rate_limit_exceeded",
      "message": "Request rate exceeds tier limit (450 req/s)",
      "retry_after_seconds": 60
    }
  },
  "evidence": {
    "sbom": "plans/team-sbom.json",
    "provenance": "plans/team-provenance.json",
    "chaos_report": "docs/plans/team-chaos-report.md",
    "benchmark_report": "docs/plans/team-benchmark-report.md"
  }
}
```

### Documentation Generation

Auto-generated markdown documentation is created from plan specs:

```
docs/plans/
├── team-tier.md          # Auto-generated from plans/team.plan.json
├── enterprise-tier.md    # Auto-generated from plans/enterprise.plan.json
└── gov-tier.md          # Auto-generated from plans/gov.plan.json
```

Each documentation file includes:

1. **Envelope Summary** - Table of throughput, connections, latency, SLA
2. **Typical Use Cases** - Real-world scenarios for the tier
3. **Refusal Behavior** - HTTP status codes and error codes
4. **Hard Limits** - Enforcement boundaries with deterministic behavior
5. **Supported Features** - Feature matrix for the tier
6. **Evidence Bundle** - Links to SBOM, provenance, reports
7. **Pricing Model** - Flat-per-deployment with no surprises
8. **CLI Commands** - Examples for common operations
9. **Runnable Examples** - Code snippets demonstrating tier behavior

## Usage

### CLI Commands

```bash
# Check current plan
erlmcp plan status

# List all available plans
erlmcp plan list

# Show tier specification
erlmcp plan show team
erlmcp plan show enterprise
erlmcp plan show gov

# Run tier-specific benchmark
erlmcp bench run --suite throughput --plan team --target 450

# Test refusal behavior
erlmcp plan test-refusal throughput_exceeded

# Export audit trail
erlmcp receipt export team json

# Upgrade plan
erlmcp upgrade plan team enterprise
```

### Programmatic API

```erlang
%% Get current active plan
{ok, {Tier, Spec}} = erlmcp_plan:current_plan().

%% Get tier specification
{ok, Spec} = erlmcp_plan:show(team).

%% Get envelope for tier
{ok, Envelope} = erlmcp_plan:get_envelope(team).

%% Get limits for tier
{ok, Limits} = erlmcp_plan:get_limits(team).

%% Check throughput compliance
ok = erlmcp_plan:check_throughput_limit(team, 400).
{error, rate_limit_exceeded} = erlmcp_plan:check_throughput_limit(team, 500).

%% Check connection limit
ok = erlmcp_plan:check_connection_limit(team, 100).
{error, connection_limit_exceeded} = erlmcp_plan:check_connection_limit(team, 200).

%% Check message size
ok = erlmcp_plan:check_message_size(team, 500000).
{error, payload_too_large} = erlmcp_plan:check_message_size(team, 2000000).

%% Verify SLA
{ok, SLA} = erlmcp_plan:verify_sla(enterprise).
```

### Manual Documentation Generation

```erlang
%% Generate all tier documentation
{ok, Files} = erlmcp_plan_docs_generator:generate_all_docs().

%% Generate specific tier
{ok, TeamFile} = erlmcp_plan_docs_generator:generate_tier_doc(team).

%% Load plan specification
Spec = erlmcp_plan_docs_generator:load_plan_spec(team).

%% Format tables for display
EnvelopeTable = erlmcp_plan_docs_generator:format_envelope_table(
    maps:get(<<"envelope">>, Spec)
).
```

## Validation & Testing

### Test Suite: `erlmcp_pricing_docs_SUITE`

9 comprehensive tests (3 per tier):

```bash
# Run all pricing documentation tests
rebar3 ct --suite erlmcp_pricing_docs_SUITE

# Run specific tier tests
rebar3 ct --suite erlmcp_pricing_docs_SUITE --case test_team_tier_doc_exists_and_valid
rebar3 ct --suite erlmcp_pricing_docs_SUITE --case test_enterprise_tier_examples_executable
rebar3 ct --suite erlmcp_pricing_docs_SUITE --case test_gov_tier_numbers_match_specs
```

### Test Coverage

For each tier, tests verify:

1. **Documentation File Validation**
   - File exists and is valid markdown
   - Contains all required sections
   - Markdown table syntax is correct

2. **Runnable Examples Validation**
   - Example code is syntactically correct
   - Plan specifications are reasonable (positive values)
   - Numbers are within expected ranges

3. **Specification Validation**
   - Numbers in docs match JSON specs
   - Throughput, connections, latency values appear in text
   - Tier names used consistently in CLI examples

## Deterministic Refusal Behavior

### Boundary Testing

Each tier enforces deterministic limits at boundaries:

```erlang
%% Team tier: 450 req/s limit
%% Request 449 succeeds, request 451 fails deterministically

rebar3 run -- bench run --suite throughput --plan team --target 450 --duration 10s

%% Output shows:
%% - Requests at 450 req/s: Success rate 100%
%% - Requests at 451 req/s: 429 rate_limit_exceeded
```

### Refusal Status Codes

**Team Tier:**
- `429 rate_limit_exceeded` - Throughput exceeded
- `503 service_unavailable` - Queue depth exceeded
- `413 payload_too_large` - Message size exceeded

**Enterprise Tier:**
- `429 rate_limit_exceeded` - Throughput exceeded
- `503 service_unavailable` - Queue depth exceeded
- `413 payload_too_large` - Message size exceeded
- `500 audit_log_failure` - Audit logging failed

**Government Tier:**
- `429 rate_limit_exceeded` - Throughput exceeded (with audit logging)
- `503 service_unavailable` - Queue depth exceeded (with audit logging)
- `413 payload_too_large` - Message size exceeded (with audit logging)
- `403 fips_violation` - FIPS-140-2 compliance violation
- `500 encryption_error` - Encryption failure

## Evidence Bundle

Each tier includes production evidence:

- **SBOM** - Software Bill of Materials (all dependencies)
- **Provenance** - Build provenance and supply-chain metadata
- **Chaos Report** - Failure mode testing results
- **Benchmark Report** - Performance profiling data
- **FIPS Certificate** (Gov only) - FIPS-140-2 compliance certification
- **Compliance Report** (Gov only) - Audit and regulatory alignment

## File Structure

```
/Users/sac/erlmcp/
├── plans/                              # Plan specifications
│   ├── team.plan.json                 # Team tier spec (450 req/s)
│   ├── enterprise.plan.json           # Enterprise tier spec (1500 req/s)
│   └── gov.plan.json                  # Gov tier spec (900 req/s)
├── src/
│   ├── erlmcp_plan.erl                # Plan module (API, checks)
│   ├── erlmcp_plan_cli.erl            # CLI commands
│   └── erlmcp_plan_docs_generator.erl # Doc generation from specs
├── test/
│   └── erlmcp_pricing_docs_SUITE.erl  # 9 validation tests
└── docs/
    └── plans/                          # Auto-generated tier docs
        ├── team-tier.md               # Team tier guide (500 req/s)
        ├── enterprise-tier.md         # Enterprise tier guide (1500 req/s)
        └── gov-tier.md                # Government tier guide (900 req/s)
```

## Pricing Model

All tiers use **flat-per-deployment pricing**:

- No per-request metering
- No usage-based overage charges
- No hidden fees
- Transparent SLA guarantees
- Deterministic refusal behavior
- Clear upgrade paths

### Cost Structure

- **Team**: Free for open-source, custom pricing for commercial
- **Enterprise**: Enterprise license pricing
- **Government**: Government license pricing

## No Surprises Philosophy

The pricing system is designed with complete transparency:

1. **Deterministic Limits** - Refusals happen at predictable boundaries
2. **Clear Specifications** - JSON specs define exact behavior
3. **Auto-Generated Docs** - Docs stay in sync with specs
4. **Validated Boundaries** - Tests verify boundary behavior
5. **SLA Guarantees** - Explicit uptime and performance targets
6. **Evidence Trail** - SBOM, provenance, benchmarks included
7. **No Metering** - Flat rate, no surprise bills
8. **Upgrade Clarity** - Clear paths between tiers

## Integration with mkdocs

All tier documentation renders in mkdocs:

```yaml
# mkdocs.yml
nav:
  - Pricing Plans:
    - Overview: pricing/overview.md
    - Team Tier: plans/team-tier.md
    - Enterprise Tier: plans/enterprise-tier.md
    - Government Tier: plans/gov-tier.md
```

## Maintenance

### Updating Plan Specifications

1. Edit `plans/{tier}.plan.json`
2. Run tests to validate: `rebar3 ct --suite erlmcp_pricing_docs_SUITE`
3. Regenerate docs: `erlmcp_plan_docs_generator:generate_all_docs()`
4. Verify docs with: `eunit` and `ct` tests

### Adding a New Tier

1. Create `plans/newname.plan.json` with tier spec
2. Add new tier atom to `erlmcp_plan_docs_generator.erl`
3. Add tests to `erlmcp_pricing_docs_SUITE.erl`
4. Regenerate documentation
5. Update pricing pages

## Implementation Details

### Key Modules

**erlmcp_plan.erl** - Core plan management
- `current_plan/0` - Get active tier
- `show/1` - Get tier specification
- `get_envelope/1` - Get tier capacity
- `check_throughput_limit/2` - Verify throughput compliance
- `check_connection_limit/2` - Verify connection compliance
- `check_message_size/2` - Verify message size compliance

**erlmcp_plan_cli.erl** - Command-line interface
- `list_plans/0` - List all tiers
- `show_plan/1` - Show tier details
- `current_plan/0` - Show active tier
- `test_refusal/1` - Test refusal behavior
- `upgrade_path/2` - Show upgrade details

**erlmcp_plan_docs_generator.erl** - Auto-generation
- `generate_all_docs/0` - Generate all tier docs
- `generate_tier_doc/1` - Generate specific tier
- `load_plan_spec/1` - Load JSON specification
- `format_envelope_table/1` - Format table markdown
- `format_limits_table/1` - Format limits markdown
- `format_features_table/1` - Format features markdown
- `format_refusal_table/1` - Format refusal table

### Configuration

Current active plan is determined by (in order):

1. `erlmcp` application environment: `application:get_env(erlmcp, current_plan, team)`
2. `ERLMCP_PLAN` environment variable: `os:getenv("ERLMCP_PLAN")`
3. Default: `team`

## Future Enhancements

Potential additions:

1. **Dynamic Tier Creation** - User-defined tier specifications
2. **Usage Monitoring** - Real-time usage tracking against tier limits
3. **Automatic Upgrade** - Auto-upgrade when approaching limits
4. **Tier Comparison** - Side-by-side tier comparison tool
5. **Cost Calculator** - Estimate tier costs based on usage
6. **Compliance Dashboard** - Real-time compliance monitoring
7. **Quota Notifications** - Alerts when approaching limits
8. **Multi-Tier Support** - Different tiers for different services

## Documentation

- **API Reference**: See `erlmcp_plan.erl` module documentation
- **CLI Help**: `erlmcp plan --help`
- **Tier Guides**: `/docs/plans/{tier}-tier.md`
- **Pricing**: Check commercial pricing page for rates

## License & Support

See main project README for license information and support channels.
