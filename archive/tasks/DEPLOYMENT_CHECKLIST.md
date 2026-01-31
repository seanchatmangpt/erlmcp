# TCPS Staging Deployment Checklist

**Agent**: Agent 9 - Staging Deployment and Validation Specialist
**Date**: 2026-01-26
**Status**: In Progress (60% ready)

Use this checklist to track deployment progress from code fixes through validation.

---

## Phase 1: Code Completion (CURRENT PHASE)

### Persistence Implementation

- [ ] **Implement Work Order Storage Functions**
  - [ ] `store_work_order/1` - Store work order to disk/ETS
  - [ ] `get_work_order/1` - Retrieve work order by ID
  - [ ] `list_work_orders/0` - List all work orders
  - [ ] `list_work_orders_by_status/1` - Filter by status
  - [ ] `list_work_orders_by_bucket/1` - Filter by bucket
  - [ ] `update_work_order/1` - Update existing work order
  - [ ] `delete_work_order/1` - Remove work order

- [ ] **Implement Andon Event Storage Functions**
  - [ ] `store_andon_event/1` - Store Andon event
  - [ ] `get_andon_event/1` - Retrieve Andon event by ID
  - [ ] `list_andon_events/0` - List all Andon events
  - [ ] `list_andon_events_by_status/1` - Filter by status
  - [ ] `update_andon_event/1` - Update existing event

- [ ] **Implement Ontology Operations**
  - [ ] `query_ontology/2` - SPARQL query interface
  - [ ] `rebuild_ontology/0` - Rebuild from receipts
  - [ ] `backup/1` - Backup data to file
  - [ ] `restore/1` - Restore from backup
  - [ ] `verify_integrity/0` - Verify data consistency

- [ ] **Implement Helper Functions**
  - [ ] `receipt_path/3` - Generate receipt file paths
  - [ ] Add any other missing helpers

### Unit Testing

- [ ] **Create Unit Tests**
  - [ ] Tests for work order storage (CRUD operations)
  - [ ] Tests for Andon event storage (CRUD operations)
  - [ ] Tests for ontology operations
  - [ ] Tests for error handling
  - [ ] Tests for edge cases (empty data, invalid IDs, etc.)

- [ ] **Run Unit Tests**
  - [ ] Execute: `rebar3 eunit`
  - [ ] Target: ≥99% passing
  - [ ] Fix any failures

### Compilation

- [ ] **Verify Compilation**
  - [ ] Execute: `rebar3 compile`
  - [ ] Zero compilation errors
  - [ ] Resolve any warnings
  - [ ] Clean build: `rebar3 clean && rebar3 compile`

**Estimated Time**: 8-12 hours
**Status**: ⬜ Not Started

---

## Phase 2: Testing & Validation

### Integration Testing

- [ ] **Run Integration Tests**
  - [ ] Execute: `rebar3 ct --dir=test/integration`
  - [ ] Target: 100% passing (105 tests)
  - [ ] Fix any test failures
  - [ ] Verify all test suites pass:
    - [ ] `tcps_work_order_tests`
    - [ ] `tcps_andon_integration_SUITE`
    - [ ] `tcps_quality_gates_SUITE`
    - [ ] `tcps_receipt_chain_SUITE`
    - [ ] `tcps_persistence_performance_SUITE`
    - [ ] Other suites (7 more)

### Code Coverage

- [ ] **Generate Coverage Report**
  - [ ] Execute: `./scripts/generate_coverage.sh`
  - [ ] Execute: `./scripts/check_coverage_threshold.sh 80`
  - [ ] Target: ≥80% overall coverage
  - [ ] Verify TCPS modules have coverage:
    - [ ] `tcps_persistence` ≥80%
    - [ ] `tcps_work_order` ≥80%
    - [ ] `tcps_andon` ≥80%
    - [ ] Other TCPS modules ≥70%

### Code Quality

- [ ] **Dialyzer Analysis**
  - [ ] Execute: `rebar3 dialyzer`
  - [ ] Target: ≤32 warnings (current baseline)
  - [ ] Document any new warnings

- [ ] **TCPS Validation**
  - [ ] Execute: `./tools/tcps help` (verify CLI works)
  - [ ] Test key commands:
    - [ ] `./tools/tcps work-order create --bucket security --priority 10`
    - [ ] `./tools/tcps andon trigger --type test_failure`
    - [ ] `./tools/tcps quality gates <sku_id>`
    - [ ] `./tools/tcps kanban status`

**Estimated Time**: 4 hours
**Status**: ⬜ Not Started

---

## Phase 3: Build & Pre-Deployment

### Release Build

- [ ] **Build Staging Release**
  - [ ] Execute: `rebar3 as staging release`
  - [ ] Verify release structure:
    - [ ] `_build/staging/rel/erlmcp/bin/erlmcp` exists
    - [ ] `_build/staging/rel/erlmcp/lib/` contains all apps
    - [ ] `_build/staging/rel/erlmcp/releases/` contains release metadata
  - [ ] Test release locally:
    - [ ] Start: `_build/staging/rel/erlmcp/bin/erlmcp console`
    - [ ] Check apps: `application:which_applications().`
    - [ ] Stop: `q().`

### Pre-Deployment Verification

- [ ] **Verify Deployment Artifacts**
  - [ ] `.env.staging` exists and configured
  - [ ] `docker/docker-compose.yml` exists
  - [ ] `scripts/staging_smoke_tests.sh` executable
  - [ ] All documentation present

- [ ] **Environment Configuration**
  - [ ] Review `.env.staging` settings
  - [ ] Update passwords (NOT default values):
    - [ ] `ERLMCP_DB_PASSWORD`
    - [ ] `GRAFANA_PASSWORD`
    - [ ] `ERLMCP_AUTH_PASSWORD`
    - [ ] `ERLMCP_COOKIE`
  - [ ] Verify TCPS thresholds appropriate for staging

**Estimated Time**: 2 hours
**Status**: ⬜ Not Started

---

## Phase 4: Deployment

### Docker Stack Deployment

- [ ] **Load Environment**
  - [ ] Execute: `export $(cat .env.staging | xargs)`
  - [ ] Verify environment loaded: `echo $ERLMCP_ENV`
  - [ ] Should show: `staging`

- [ ] **Deploy Services**
  - [ ] Execute: `docker compose -f docker/docker-compose.yml up -d`
  - [ ] Wait 30 seconds for startup
  - [ ] Check services: `docker compose ps`
  - [ ] Verify all 7 services running:
    - [ ] `erlmcp` - State: Up, Health: healthy
    - [ ] `dashboard` - State: Up, Health: healthy
    - [ ] `postgres` - State: Up, Health: healthy
    - [ ] `otel-collector` - State: Up
    - [ ] `jaeger` - State: Up
    - [ ] `prometheus` - State: Up
    - [ ] `grafana` - State: Up

- [ ] **Check Logs**
  - [ ] View erlmcp logs: `docker compose logs erlmcp | tail -50`
  - [ ] No error messages
  - [ ] Application started successfully
  - [ ] TCPS system initialized

**Estimated Time**: 30 minutes
**Status**: ⬜ Not Started

---

## Phase 5: Smoke Testing

### Automated Smoke Tests

- [ ] **Run Smoke Test Suite**
  - [ ] Execute: `./scripts/staging_smoke_tests.sh staging http://localhost:8080`
  - [ ] All 7 tests pass:
    - [ ] Health endpoint (200 OK)
    - [ ] Readiness endpoint (200 OK)
    - [ ] Liveness endpoint (200 OK)
    - [ ] Response time (<1000ms)
    - [ ] Metrics endpoint (200 OK)
    - [ ] Dashboard accessible
    - [ ] Docker services (7/7)
  - [ ] Save results to file

### Manual Health Checks

- [ ] **Application Health**
  - [ ] Execute: `curl http://localhost:8080/health | jq`
  - [ ] Response shows:
    - [ ] `"status": "healthy"`
    - [ ] All subsystem checks OK (gproc, poolboy, tcps, disk, memory, database)
  - [ ] Execute: `curl http://localhost:8080/health/ready`
  - [ ] Returns: `200 OK`
  - [ ] Execute: `curl http://localhost:8080/health/live`
  - [ ] Returns: `200 OK`

- [ ] **Database Health**
  - [ ] Execute: `docker compose exec postgres psql -U erlmcp -d erlmcp_staging -c "SELECT version();"`
  - [ ] PostgreSQL version displayed
  - [ ] No connection errors

- [ ] **Dashboard Health**
  - [ ] Open: `http://localhost:3000`
  - [ ] Dashboard loads successfully
  - [ ] Metrics display correctly
  - [ ] SSE stream connected (real-time updates)
  - [ ] Charts rendering

- [ ] **Observability Stack**
  - [ ] Jaeger UI: `http://localhost:16686`
    - [ ] UI loads
    - [ ] Services listed (erlmcp)
    - [ ] Traces visible
  - [ ] Prometheus: `http://localhost:9091`
    - [ ] UI loads
    - [ ] Targets showing (erlmcp, otel-collector)
    - [ ] Metrics queryable
  - [ ] Grafana: `http://localhost:3001`
    - [ ] Login: admin / <GRAFANA_PASSWORD>
    - [ ] Datasources configured
    - [ ] Dashboards available

**Estimated Time**: 30 minutes
**Status**: ⬜ Not Started

---

## Phase 6: Functional Validation

### TCPS Functional Tests

- [ ] **Work Order Management**
  - [ ] Create work order via API:
    ```bash
    curl -X POST http://localhost:8080/api/work-order \
      -H "Content-Type: application/json" \
      -d '{"pull_signal": {"type": "github_issue", "source": "test", "bucket": "security", "priority": 10}}'
    ```
  - [ ] Response: `{"ok": "work_order_id_XXX"}`
  - [ ] Retrieve work order:
    ```bash
    curl http://localhost:8080/api/work-order/<id> | jq
    ```
  - [ ] Work order displayed correctly
  - [ ] List work orders:
    ```bash
    curl http://localhost:8080/api/work-orders | jq
    ```
  - [ ] Work order appears in list

- [ ] **Andon Event Triggering**
  - [ ] Trigger Andon:
    ```bash
    curl -X POST http://localhost:8080/api/andon/trigger \
      -H "Content-Type: application/json" \
      -d '{"type": "test_failure", "sku_id": "test_sku", "context": {"stage": "test"}}'
    ```
  - [ ] Response: `{"ok": "andon_event_id_XXX"}`
  - [ ] Verify visible on dashboard at `http://localhost:3000`
  - [ ] Andon alert panel shows event
  - [ ] Resolve Andon:
    ```bash
    curl -X POST http://localhost:8080/api/andon/resolve/<id>
    ```
  - [ ] Event marked resolved

- [ ] **SKU Pipeline Processing**
  - [ ] Create SKU:
    ```bash
    curl -X POST http://localhost:8080/api/sku/create \
      -H "Content-Type: application/json" \
      -d '{"work_order_id": "<work_order_id>"}'
    ```
  - [ ] Response: `{"ok": "sku_id_XXX"}`
  - [ ] Check SKU status:
    ```bash
    curl http://localhost:8080/api/sku/<sku_id> | jq
    ```
  - [ ] SKU shows:
    - [ ] Current stage
    - [ ] Status (in_production)
    - [ ] Completion percentage
    - [ ] Receipt count

- [ ] **Receipt Verification**
  - [ ] List receipts:
    ```bash
    curl http://localhost:8080/api/receipts?sku_id=<sku_id> | jq
    ```
  - [ ] Receipts returned
  - [ ] Verify receipt chain:
    ```bash
    curl http://localhost:8080/api/receipt/verify-chain/<sku_id> | jq
    ```
  - [ ] Chain integrity verified

- [ ] **Quality Gates**
  - [ ] Check quality gates:
    ```bash
    curl http://localhost:8080/api/quality/gates/<sku_id> | jq
    ```
  - [ ] Quality metrics displayed
  - [ ] Gates show pass/fail status

**Estimated Time**: 1 hour
**Status**: ⬜ Not Started

---

## Phase 7: Performance Testing

### Load Testing

- [ ] **Install wrk** (if not installed)
  - [ ] macOS: `brew install wrk`
  - [ ] Linux: `sudo apt-get install wrk`

- [ ] **Run Load Test**
  - [ ] Execute: `wrk -t4 -c100 -d30s http://localhost:8080/health`
  - [ ] Record results:
    - [ ] Requests/sec: _______ (target: >500)
    - [ ] Latency P50: _______ (target: <50ms)
    - [ ] Latency P95: _______ (target: <200ms)
    - [ ] Latency P99: _______ (target: <500ms)
    - [ ] Errors: _______ (target: 0%)
  - [ ] Results meet targets

- [ ] **Monitor During Load**
  - [ ] Check Grafana CPU/Memory dashboards
  - [ ] Check Jaeger trace counts
  - [ ] Check Dashboard metrics updates
  - [ ] System remains stable

**Estimated Time**: 30 minutes
**Status**: ⬜ Not Started

---

## Phase 8: Documentation

### Validation Report

- [ ] **Create Final Deployment Report**
  - [ ] Copy template: `docs/STAGING_DEPLOYMENT_VALIDATION.md`
  - [ ] Update with actual results:
    - [ ] Deployment time
    - [ ] All services status
    - [ ] Smoke test results (7/7 passing)
    - [ ] Functional test results
    - [ ] Load test results
    - [ ] Performance metrics
  - [ ] Document any issues encountered
  - [ ] Add recommendations for production

- [ ] **Update Checklist**
  - [ ] Mark all items complete
  - [ ] Add notes on any deviations
  - [ ] Record actual time spent per phase

**Estimated Time**: 30 minutes
**Status**: ⬜ Not Started

---

## Phase 9: Handoff

### Production Readiness

- [ ] **Review with Team**
  - [ ] Present deployment validation report
  - [ ] Discuss any issues or concerns
  - [ ] Get approval for production deployment

- [ ] **Create Production Plan**
  - [ ] Based on staging learnings
  - [ ] Update configurations for production
  - [ ] Plan rollback procedures
  - [ ] Schedule production deployment

**Estimated Time**: 1 hour
**Status**: ⬜ Not Started

---

## Summary

### Progress Tracking

| Phase | Status | Est. Time | Actual Time |
|-------|--------|-----------|-------------|
| 1. Code Completion | ⬜ Not Started | 8-12h | - |
| 2. Testing & Validation | ⬜ Not Started | 4h | - |
| 3. Build & Pre-Deployment | ⬜ Not Started | 2h | - |
| 4. Deployment | ⬜ Not Started | 0.5h | - |
| 5. Smoke Testing | ⬜ Not Started | 0.5h | - |
| 6. Functional Validation | ⬜ Not Started | 1h | - |
| 7. Performance Testing | ⬜ Not Started | 0.5h | - |
| 8. Documentation | ⬜ Not Started | 0.5h | - |
| 9. Handoff | ⬜ Not Started | 1h | - |
| **TOTAL** | **0/9** | **18-22h** | **-** |

### Current Blockers

1. **Missing Persistence Functions** (17 functions in `tcps_persistence.erl`)
   - Priority: P0 (Critical)
   - Blocking: Compilation → All subsequent phases
   - Owner: Development Team
   - ETA: 8-12 hours

### Next Actions

**For Development Team**:
1. Start Phase 1: Implement persistence functions
2. Run tests after each function implemented
3. Update this checklist as work progresses
4. Notify when Phase 1 complete

**For Agent 9**:
1. Monitor checklist progress
2. When Phase 1 complete, execute Phases 2-9
3. Create final validation report
4. Present to team for production approval

---

## Notes

- Update this checklist as you work through phases
- Mark items with ✅ when complete
- Add notes on any issues or deviations
- Record actual time spent for future planning
- Use this as input to final validation report

---

**Created**: 2026-01-26
**Last Updated**: 2026-01-26
**Owner**: Agent 9 - Staging Deployment and Validation Specialist
**Status**: Awaiting Phase 1 (Code Completion)
