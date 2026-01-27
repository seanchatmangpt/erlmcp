# TCPS Staging Deployment - Quick Start Guide

**Status**: ⚠️ **BLOCKED** - Awaiting code fixes
**Readiness**: 60% (Infrastructure ready, code compilation blocked)
**Agent**: Agent 9 - Staging Deployment and Validation Specialist
**Date**: 2026-01-26

---

## Current Status

### ✅ Ready Components

1. **Docker Infrastructure** - 7-service stack configured
2. **Environment Configuration** - Staging variables set
3. **Smoke Test Suite** - Automated validation ready
4. **Documentation** - Comprehensive deployment guide

### ❌ Blocking Issues

1. **Compilation Errors** - 17 missing function implementations in `tcps_persistence.erl`
2. **Test Failures** - Cannot run tests until compilation succeeds
3. **Release Build** - Cannot build release artifact

---

## Quick Reference

### Files Created

```
/Users/sac/erlmcp/
├── .env.staging                          # ✅ Staging environment config
├── scripts/
│   └── staging_smoke_tests.sh            # ✅ Smoke test suite (executable)
└── docs/
    ├── STAGING_DEPLOYMENT_VALIDATION.md  # ✅ Detailed validation report
    └── DEPLOYMENT_ARTIFACTS_SUMMARY.md   # ✅ Artifacts overview
```

### Commands

#### When Blockers Are Resolved

```bash
# 1. Verify compilation
rebar3 compile

# 2. Run tests
rebar3 eunit
rebar3 ct --dir=test/integration

# 3. Check coverage
./scripts/generate_coverage.sh
./scripts/check_coverage_threshold.sh 80

# 4. Build release
rebar3 as staging release

# 5. Load environment
export $(cat .env.staging | xargs)

# 6. Deploy
docker compose -f docker/docker-compose.yml up -d

# 7. Wait for services
sleep 30

# 8. Run smoke tests
./scripts/staging_smoke_tests.sh staging http://localhost:8080

# 9. View services
docker compose ps

# 10. View logs
docker compose logs -f erlmcp

# 11. Access UIs
open http://localhost:3000    # Dashboard
open http://localhost:16686   # Jaeger
open http://localhost:3001    # Grafana

# 12. Shutdown
docker compose down
```

---

## Deployment Blockers

### Critical: Missing Function Implementations

**File**: `src/tcps_persistence.erl`
**Functions**: 17 exported but not implemented

**Work Order Storage** (7):
- `store_work_order/1`
- `get_work_order/1`
- `list_work_orders/0`
- `list_work_orders_by_status/1`
- `list_work_orders_by_bucket/1`
- `update_work_order/1`
- `delete_work_order/1`

**Andon Event Storage** (5):
- `store_andon_event/1`
- `get_andon_event/1`
- `list_andon_events/0`
- `list_andon_events_by_status/1`
- `update_andon_event/1`

**Ontology Operations** (5):
- `query_ontology/2`
- `rebuild_ontology/0`
- `backup/1`
- `restore/1`
- `verify_integrity/0`

**Estimated Fix Time**: 8-12 hours

---

## Smoke Test Suite

**Location**: `/Users/sac/erlmcp/scripts/staging_smoke_tests.sh`

**Tests** (7 total):
1. Health endpoint (`/health`)
2. Readiness endpoint (`/health/ready`)
3. Liveness endpoint (`/health/live`)
4. Response time (<1000ms)
5. Metrics endpoint (`/metrics`)
6. Dashboard accessibility
7. Docker services (7 running)

**Usage**:
```bash
./scripts/staging_smoke_tests.sh staging http://localhost:8080 http://localhost:3000
```

**Expected Output**:
```
==========================================
TCPS Smoke Tests - staging Environment
==========================================
✅ PASS: Health endpoint (200 OK, 45ms)
✅ PASS: Readiness endpoint (200 OK, 12ms)
✅ PASS: Liveness endpoint (200 OK, 8ms)
✅ PASS: Response time (avg: 21ms, target: <1000ms)
✅ PASS: Metrics endpoint (200 OK)
✅ PASS: Dashboard accessible (200 OK)
✅ PASS: Docker services (7/7 running)

Summary: 7/7 tests passed (100%)
```

---

## Docker Stack

### Services (7)

| Service | Port(s) | Purpose |
|---------|---------|---------|
| **erlmcp** | 8080, 9090 | Main application + metrics |
| **dashboard** | 3000 | TCPS Dashboard UI |
| **postgres** | 5432 | PostgreSQL database |
| **otel-collector** | 4317, 4318 | OpenTelemetry collector |
| **jaeger** | 16686 | Distributed tracing UI |
| **prometheus** | 9091 | Metrics storage |
| **grafana** | 3001 | Visualization dashboards |

### Health Checks

```bash
# Application
curl http://localhost:8080/health | jq

# Dashboard
curl http://localhost:3000/health

# PostgreSQL
docker compose exec postgres pg_isready -U erlmcp

# All services
docker compose ps
```

---

## Environment Configuration

**File**: `/Users/sac/erlmcp/.env.staging`

**Key Variables**:
```bash
ERLMCP_ENV=staging
ERLMCP_DB_HOST=postgres
TCPS_QUALITY_PASS_RATE=0.80
OTEL_EXPORTER_OTLP_ENDPOINT=http://otel-collector:4318
```

**Security Notes**:
- Change `ERLMCP_DB_PASSWORD` before deployment
- Change `GRAFANA_PASSWORD` before deployment
- Change `ERLMCP_AUTH_PASSWORD` before deployment
- Never commit `.env.staging` to version control

---

## Validation Workflow

### Pre-Deployment Validation

- [ ] Compilation succeeds (`rebar3 compile`)
- [ ] Unit tests pass (`rebar3 eunit`)
- [ ] Integration tests pass (`rebar3 ct`)
- [ ] Coverage ≥80% (`./scripts/check_coverage_threshold.sh 80`)
- [ ] Release builds (`rebar3 as staging release`)

### Deployment

- [ ] Environment loaded (`export $(cat .env.staging | xargs)`)
- [ ] Docker stack deployed (`docker compose up -d`)
- [ ] All services healthy (`docker compose ps`)

### Post-Deployment Validation

- [ ] Smoke tests pass (7/7)
- [ ] Health endpoints respond
- [ ] Dashboard accessible
- [ ] Metrics flowing to Prometheus
- [ ] Traces visible in Jaeger
- [ ] Grafana dashboards rendering

### Functional Validation

- [ ] Create work order
- [ ] Trigger Andon event
- [ ] Generate receipts
- [ ] Verify quality gates
- [ ] Test SSE streaming
- [ ] Load testing passes

---

## Documentation

### Comprehensive Reports

1. **STAGING_DEPLOYMENT_VALIDATION.md** (18 KB)
   - Detailed pre-deployment analysis
   - Blocker root cause analysis
   - Deployment timeline scenarios
   - Complete command reference

2. **DEPLOYMENT_ARTIFACTS_SUMMARY.md** (16 KB)
   - Artifacts overview
   - Infrastructure status
   - Deployment workflow
   - Success criteria

### Quick References

- **This file** - Quick start guide
- **docker/docker-compose.yml** - Service definitions
- **.env.staging** - Environment variables
- **scripts/staging_smoke_tests.sh** - Automated tests

---

## Troubleshooting

### Compilation Fails

```bash
# Check errors
rebar3 compile 2>&1 | grep -E "error|undefined"

# Clean build
rebar3 clean
rebar3 compile
```

### Tests Fail

```bash
# Run specific test
rebar3 eunit --module=tcps_work_order_tests

# Run with verbose output
rebar3 ct --dir=test/integration --verbose
```

### Docker Services Won't Start

```bash
# Check logs
docker compose logs erlmcp

# Restart specific service
docker compose restart erlmcp

# Full restart
docker compose down
docker compose up -d
```

### Health Checks Fail

```bash
# Check application logs
docker compose logs erlmcp | tail -50

# Check database connectivity
docker compose exec postgres psql -U erlmcp -c "SELECT version();"

# Check network
docker network inspect erlmcp_erlmcp-network
```

---

## Next Steps

### For Development Team

1. Implement missing functions in `tcps_persistence.erl`
2. Verify compilation succeeds
3. Run full test suite
4. Notify for deployment

### For Agent 9 (When Ready)

1. Execute pre-deployment checks
2. Build staging release
3. Deploy Docker stack
4. Run smoke tests
5. Perform functional validation
6. Create final deployment report

---

## Contact

**Agent**: Agent 9 - Staging Deployment and Validation Specialist
**Task**: Deploy TCPS to staging and perform validation
**Status**: Artifacts created, awaiting code fixes
**Next**: Re-run deployment after persistence implementation

---

## Summary

**Infrastructure**: ✅ 100% Ready
**Configuration**: ✅ 100% Ready
**Testing**: ✅ 100% Ready
**Application Code**: ❌ 0% Ready (compilation blocked)

**Overall Readiness**: 60%

**Estimated Time to Deployment**: 10-14 hours
- 8-12 hours: Implement missing functions
- 2 hours: Deployment and validation

---

**Last Updated**: 2026-01-26
**Created By**: Agent 9
