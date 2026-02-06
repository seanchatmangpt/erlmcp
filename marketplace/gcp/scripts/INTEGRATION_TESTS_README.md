# ERLMCP Marketplace Integration Tests

**Location:** `/home/user/erlmcp/marketplace/gcp/scripts/integration-tests.sh`

## Purpose

End-to-end integration testing for deployed ERLMCP services on GCP Marketplace. Tests validate production readiness under real conditions following the DOCKER-ONLY constitution.

## Constitution Compliance

**DOCKER-ONLY:** All Erlang/OTP execution runs via `docker compose`. Zero host execution.

- Erlang tests: `docker compose run erlmcp-ct`
- Service verification: curl/gcloud/kubectl (infrastructure tools)
- Evidence collection: Provenance receipts with cryptographic hashes

## Test Suites

| Suite | Description | Tests |
|-------|-------------|-------|
| **smoke** | Basic health/readiness checks | Health endpoint, readiness endpoint |
| **api** | Functional API testing | List tools, call tool (JSON-RPC 2.0) |
| **observability** | Metrics & logging | Prometheus metrics, structured logs |
| **performance** | Load testing | Concurrent requests, throughput |
| **chaos** | Resilience testing | Service restart recovery |
| **security** | Security validation | HTTPS enforcement, security headers |
| **docker** | Docker-based CT suites | Integration test suites via Docker |
| **full** | All suites | Complete integration test run |

## Usage

### Test Cloud Run Deployment

```bash
export PROJECT_ID="my-gcp-project"
export SERVICE_URL="https://erlmcp-xyz.run.app"

/home/user/erlmcp/marketplace/gcp/scripts/integration-tests.sh \
  --service-url "$SERVICE_URL" \
  --suite full
```

### Test GKE Deployment

```bash
export PROJECT_ID="my-gcp-project"
export CLUSTER_NAME="erlmcp-prod-cluster"

/home/user/erlmcp/marketplace/gcp/scripts/integration-tests.sh \
  --cluster-name "$CLUSTER_NAME" \
  --project "$PROJECT_ID" \
  --region us-central1 \
  --suite full
```

### Test GCE Deployment

```bash
export INSTANCE_IP="35.1.2.3"

/home/user/erlmcp/marketplace/gcp/scripts/integration-tests.sh \
  --instance-ip "$INSTANCE_IP" \
  --suite smoke
```

### Run Specific Test Suite

```bash
# API tests only
./integration-tests.sh --service-url https://erlmcp.run.app --suite api

# Performance tests only
./integration-tests.sh --service-url https://erlmcp.run.app --suite performance
```

## Docker Execution Pattern

All Erlang tests execute via Docker Compose using quality gates:

```bash
# Example: Running CT integration suite
docker compose -f /home/user/erlmcp/docker-compose.yml run --rm \
  -e ERLMCP_ENV=test \
  -e ERLMCP_SERVICE_URL="$SERVICE_URL" \
  erlmcp-ct \
  sh -c "rebar3 ct --suite=test/integration/api_integration_SUITE.erl"
```

**Quality Gates:**
- `erlmcp-ct` - Common Test integration gate
- `erlmcp-build` - Compilation gate
- `erlmcp-unit` - Unit test gate
- `erlmcp-check` - Quality analysis gate

## Evidence Collection

Every test generates cryptographic evidence receipts:

```json
{
  "test_name": "integration_api",
  "timestamp": "2026-02-06T18:03:00+00:00",
  "git_sha": "abc123...",
  "image_digest": "sha256:def456...",
  "docker_service": "erlmcp-ct",
  "docker_command": "rebar3 ct --suite=test/integration/api_integration_SUITE.erl",
  "exit_code": 0,
  "duration_seconds": 45,
  "status": "PASS",
  "evidence_log": "integration_api.log",
  "receipt_hash": "789ghi..."
}
```

**Receipt Hash:** `hash(git_sha || image_digest || service || cmd || exit || timestamp)`

## Quality Invariants

**MUST** satisfy:
- `errors = 0` (no test failures)
- `failures = 0` (no assertion failures)
- `coverage ≥ 0.8` (80%+ code coverage)
- `regression < 0.1` (< 10% performance regression)

Tests **FAIL** if any invariant violated.

## Evidence Directory

All test evidence saved to:
```
/home/user/erlmcp/marketplace/gcp/test-evidence/integration/
├── *.log                          # Test execution logs
├── *.receipt                      # Cryptographic receipts
├── integration-tests-summary.json # Final summary with all results
├── health-response.json           # Health check responses
├── api-*.json                     # API test responses
├── metrics.txt                    # Prometheus metrics
└── concurrent-results.txt         # Load test results
```

## Prerequisites

**Required:**
- Docker + Docker Compose (CONSTITUTION)
- curl (HTTP endpoint testing)

**Optional:**
- gcloud CLI (Cloud Run/GCE testing)
- kubectl (GKE testing)
- jq (JSON parsing)

## Exit Codes

- `0` - All tests passed, quality invariants satisfied
- `1` - Test failures OR quality invariants violated

## Examples

### Smoke Test Before Production

```bash
# Quick smoke test (30 seconds)
./integration-tests.sh \
  --service-url https://erlmcp-staging.run.app \
  --suite smoke
```

### Full Pre-Release Validation

```bash
# Complete test suite (10-15 minutes)
./integration-tests.sh \
  --cluster-name erlmcp-prod \
  --project prod-project-123 \
  --suite full
```

### Performance Validation

```bash
# Test concurrent request handling
export CONCURRENT_REQUESTS=50
export REQUESTS_PER_WORKER=10

./integration-tests.sh \
  --service-url https://erlmcp.run.app \
  --suite performance
```

### Chaos Engineering

```bash
# Test resilience to pod failures (GKE only)
./integration-tests.sh \
  --cluster-name erlmcp-test \
  --project test-project \
  --suite chaos
```

## Integration with CI/CD

### Cloud Build

```yaml
steps:
  - name: gcr.io/cloud-builders/docker
    args:
      - compose
      - -f
      - docker-compose.yml
      - build
      - erlmcp-ct

  - name: bash
    args:
      - marketplace/gcp/scripts/integration-tests.sh
      - --service-url
      - ${_SERVICE_URL}
      - --suite
      - full
    env:
      - PROJECT_ID=$PROJECT_ID
```

### GitHub Actions

```yaml
- name: Run Integration Tests
  run: |
    export SERVICE_URL="${{ secrets.STAGING_URL }}"
    ./marketplace/gcp/scripts/integration-tests.sh \
      --service-url "$SERVICE_URL" \
      --suite full
```

## Troubleshooting

### "docker not found"
Install Docker. Constitution requires DOCKER-ONLY execution.

### "No deployment target specified"
Set one of: `--service-url`, `--cluster-name`, `--instance-ip`

### "Health check failed"
Service may not be fully started. Wait 30-60s after deployment.

### "Docker test failed"
Check evidence logs in `test-evidence/integration/*.log`

## Architecture

```
integration-tests.sh
│
├── Prerequisites Check
│   └── docker, docker-compose (REQUIRED)
│
├── Deployment Target Validation
│   ├── Cloud Run (SERVICE_URL)
│   ├── GKE (CLUSTER_NAME)
│   └── GCE (INSTANCE_IP)
│
├── Test Suites
│   ├── Smoke Tests (curl)
│   ├── API Tests (curl + JSON-RPC)
│   ├── Observability (metrics, logs)
│   ├── Performance (concurrent requests)
│   ├── Chaos (pod restarts)
│   ├── Security (HTTPS, headers)
│   └── Docker CT (via docker compose run erlmcp-ct)
│
├── Evidence Collection
│   ├── Execution logs
│   ├── Cryptographic receipts
│   └── Test summary JSON
│
└── Quality Gate
    └── Exit 1 if errors > 0 OR failures > 0
```

## WHY This Matters

**Production Readiness:** Tests prove deployed services work under real conditions.

**No Mocks:** Real HTTP requests, real processes, real failures.

**Reproducible:** Cryptographic receipts enable deterministic replay.

**Constitution Compliant:** DOCKER-ONLY execution. Zero host commands.

**Enterprise Grade:** Fortune 500 quality standards. Full observability.

---

**Constitution:** DOCKER-ONLY | All execution via `docker compose run`

**Deadline:** 1 hour to production deployment worldwide

**Status:** READY FOR DEPLOYMENT
