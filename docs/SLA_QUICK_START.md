# SLA Enforcement - Quick Start Guide

## One-Minute Setup

### Pre-Deployment Verification

```bash
# Run before production deployment
make verify-sla PLAN=team

# Output: PASS ✓ or FAIL ✗ (exit code 0 or 1)
```

## 30-Second Reference

### Makefile Commands

```bash
# Verify team plan (2-minute test)
make verify-sla PLAN=team

# Verify enterprise plan (2-minute test)
make verify-sla PLAN=enterprise

# Verify government plan (2-minute test)
make verify-sla PLAN=gov

# Run SLA monitor tests
make test-sla
```

### Erlang API

```erlang
%% Start monitoring
erlmcp_plan_sla_monitor:monitor_envelope(team, <<"1.0.0">>).

%% Check status
erlmcp_plan_sla_monitor:get_sla_status(team).

%% Get dashboard
erlmcp_plan_sla_monitor:get_sla_dashboard(team).

%% Export metrics
erlmcp_plan_sla_monitor:export_sla_metrics(team, "sla.json").
```

### HTTP Dashboard

```bash
curl http://localhost:8080/metrics/sla/team | jq .
```

## Plan Envelopes at a Glance

| Plan | Throughput | P99 Latency | Failover |
|------|-----------|-------------|----------|
| Team | 450+ req/s | ≤150ms | ≤5s |
| Enterprise | 1500+ req/s | ≤100ms | ≤2s |
| Gov | 900+ req/s | ≤80ms | ≤1s |

## SLA Status Response

```json
{
  "plan": "team",
  "compliance_status": "PASS",
  "metrics": {
    "throughput": {"current_req_s": 525, "minimum_req_s": 450, "status": "PASS"},
    "latency": {"current_p99_ms": 128, "maximum_ms": 150, "status": "PASS"},
    "failover": {"current_s": 0.8, "maximum_s": 5, "status": "PASS"}
  },
  "violations_count": 0
}
```

## Common Tasks

### Pre-Deployment Gate

```bash
#!/bin/bash
make verify-sla PLAN=$PLAN || exit 1
# Deployment proceeds only if SLA verified
```

### Continuous Monitoring

```erlang
%% In application startup
erlmcp_plan_sla_monitor:monitor_envelope(team, Version),
%% Automatically checks SLA every 5 minutes
%% Alerts logged to receipt chain
```

### Dashboard Integration

```bash
curl -s http://localhost:8080/metrics/sla/team \
  | jq '.compliance_status'
# Returns: "PASS" or "FAIL"
```

### Export Compliance Report

```erlang
erlmcp_plan_sla_monitor:export_sla_metrics(team,
  "dist/sla-audit-" ++ erlang:date_str() ++ ".json").
```

## Failure Scenarios

### Throughput Too Low
```
make verify-sla PLAN=team
✗ SLA VERIFICATION FAILED
Throughput 380 req/s < 450 req/s
```
**Fix**: Reduce background load, increase VM schedulers

### Latency Too High
```
make verify-sla PLAN=team
✗ SLA VERIFICATION FAILED
P99 Latency 175ms > 150ms
```
**Fix**: Tune GC parameters, disable CPU scaling

### Failover Timeout Exceeded
```
make verify-sla PLAN=team
✗ SLA VERIFICATION FAILED
Failover 6.5s > 5s
```
**Fix**: Optimize circuit breaker settings

## Report Location

Verification report saved to:
```
dist/sla-verify-<plan>-<timestamp>.json
```

Example:
```
dist/sla-verify-team-1706383265.json
```

## Monitoring Key Metrics

### Real-Time Dashboard
```bash
watch -n 5 'curl -s http://localhost:8080/metrics/sla/team | jq .'
```

### Violations in Last Hour
```erlang
{ok, Status} = erlmcp_plan_sla_monitor:get_sla_status(team),
ViolationCount = maps:get(violations_count, Status).
```

### Export for External Tools
```erlang
erlmcp_plan_sla_monitor:export_sla_metrics(team, "/prometheus/sla.json").
```

## Files

- **Monitor**: `src/erlmcp_plan_sla_monitor.erl`
- **HTTP Handler**: `src/erlmcp_sla_http_handler.erl`
- **Tests**: `test/erlmcp_plan_sla_monitor_SUITE.erl`
- **Verification**: `scripts/verify_sla.sh`
- **Docs**: `docs/SLA_ENFORCEMENT_SYSTEM.md`

## Verify Installation

```bash
# Check files exist
ls -lh src/erlmcp_plan_sla_monitor.erl
ls -lh src/erlmcp_sla_http_handler.erl
ls -lh test/erlmcp_plan_sla_monitor_SUITE.erl
ls -lh scripts/verify_sla.sh

# Check Makefile has targets
grep "verify-sla\|test-sla" Makefile

# Compile modules
erlc -I include/ src/erlmcp_plan_sla_monitor.erl
erlc -I include/ src/erlmcp_sla_http_handler.erl
erlc -I include/ test/erlmcp_plan_sla_monitor_SUITE.erl

# No errors = ready to use
```

## CI/CD Integration

### GitHub Actions Example
```yaml
- name: Verify SLA before deployment
  run: make verify-sla PLAN=${{ env.PLAN }}
```

### Exit Codes
- `0` = PASS (proceed with deployment)
- `1` = FAIL (abort deployment)

## Support

See full documentation:
```bash
cat docs/SLA_ENFORCEMENT_SYSTEM.md
```

---

**Last Updated**: 2026-01-27
**Status**: Production Ready
