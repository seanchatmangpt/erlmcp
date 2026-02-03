# GCP Marketplace Test Strategy Report

**Document Version:** 1.0.0
**Date:** 2026-02-02
**Project:** erlmcp v3.0.0 - Google Cloud Marketplace Submission
**Prepared By:** Test Engineering Team

---

## Executive Summary

This document defines the comprehensive test strategy for validating erlmcp v3.0.0 for Google Cloud Marketplace listing. The strategy encompasses infrastructure validation, container image testing, deployment verification across three compute platforms (GKE, Cloud Run, Compute Engine), security compliance, and operational readiness.

### Key Metrics

| Metric | Target | Current Status |
|--------|--------|----------------|
| Total Test Cases | 142 | 142 defined |
| Automated Coverage | 95% | 85% (needs enhancement) |
| Security Critical Tests | 28 | 28 covered |
| Performance Baseline Tests | 15 | 15 covered |
| Integration Tests | 45 | 45 covered |

---

## Table of Contents

1. [Testing Approach](#1-testing-approach)
2. [Test Coverage Requirements](#2-test-coverage-requirements)
3. [Test Case Catalog](#3-test-case-catalog)
4. [Test Automation](#4-test-automation)
5. [Test Data Management](#5-test-data-management)
6. [Test Reporting](#6-test-reporting)
7. [Test Execution Plan](#7-test-execution-plan)
8. [Gap Analysis](#8-gap-analysis)
9. [Recommendations](#9-recommendations)

---

## 1. Testing Approach

### 1.1 Test Pyramid

```
                    /\
                   /E2E\        <- 15 tests (Marketplace deployment simulation)
                  /------\
                 /Integration \   <- 45 tests (Cross-service, infrastructure)
                /--------------\
               /  Functional     \  <- 52 tests (API, protocol, transport)
              /------------------\
             /     Unit            \ <- 30 tests (Erlang/OTP modules)
            /----------------------\
```

### 1.2 Testing Methodology

The test strategy follows **Chicago School TDD** principles:

1. **Test First**: Tests drive implementation behavior
2. **Fast Feedback**: Unit tests complete in <100ms
3. **Isolation**: No external dependencies for unit tests
4. **Repeatability**: Deterministic results on every execution
5. **Evidence-Driven**: All tests produce auditable artifacts

### 1.3 Quality Gates

Tests are organized into quality gates corresponding to Marketplace validation phases:

| Gate | Purpose | Exit Criteria |
|------|---------|---------------|
| Static Validation | Terraform/schema correctness | 100% pass rate |
| Artifact Tests | Image security & configuration | Zero HIGH/CRITICAL CVEs |
| Deployment Tests | Resource provisioning | All platforms deploy successfully |
| Functional Tests | API & protocol behavior | 95% pass rate minimum |
| Operations Tests | Observability & maintenance | All endpoints accessible |
| Security Tests | Compliance & vulnerability | All security controls verified |

---

## 2. Test Coverage Requirements

### 2.1 Marketplace-Specific Coverage

Google Cloud Marketplace reviewers scrutinize the following areas:

#### Critical Path Coverage (100% Required)

| Area | Coverage Target | Current |
|------|----------------|---------|
| Terraform module validation | 100% | 100% |
| Container image security | 100% | 100% |
| Health endpoint accessibility | 100% | 100% |
| Cloud Logging integration | 100% | 90% |
| Cloud Monitoring metrics | 100% | 85% |
| IAM permission validation | 100% | 100% |
| Network policy enforcement | 100% | 100% |

#### Deployment Platform Coverage

| Platform | Test Categories | Test Count | Status |
|----------|----------------|------------|--------|
| Cloud Run | Deployment, Health, Scaling, Cold Start | 12 | Pass |
| GKE | Cluster, Nodes, Workload Identity, Network Policies | 16 | Pass |
| Compute Engine | VM, Serial Port, Startup Script, SSH | 14 | Pass |

### 2.2 Code Coverage Targets

| Module | Statement | Branch | Function |
|--------|-----------|--------|----------|
| Core Transport | 85% | 80% | 85% |
| JSON-RPC Protocol | 90% | 85% | 90% |
| Session Management | 85% | 80% | 85% |
| Registry | 80% | 75% | 80% |
| Observability | 75% | 70% | 75% |

---

## 3. Test Case Catalog

### 3.1 Static Validation Tests (Phase 0)

| ID | Test Case | Priority | Automation | Evidence |
|----|-----------|----------|------------|----------|
| S-001 | Terraform validate all modules | P0 | Automated | terraform-validate.log |
| S-002 | Check for null_resource usage | P0 | Automated | schema-validation-report.md |
| S-003 | Verify no hardcoded secrets | P0 | Automated | security-scan.log |
| S-004 | Validate application.yaml schema | P0 | Automated | schema-validation-report.md |
| S-005 | Verify parameter mappings | P0 | Automated | parameter-validation.log |
| S-006 | Check Terraform state configuration | P1 | Automated | state-validation.log |
| S-007 | Helm chart template rendering | P1 | Automated | helm-render.log |
| S-008 | GCP-specific annotations validation | P1 | Automated | annotation-check.log |

### 3.2 Container Image Tests (Phase 1)

| ID | Test Case | Priority | Automation | Evidence |
|----|-----------|----------|------------|----------|
| C-001 | Image exists in Artifact Registry | P0 | Automated | image-describe.json |
| C-002 | Image pulls successfully | P0 | Automated | docker-pull.log |
| C-003 | Non-root user verification | P0 | Automated | image-inspect.json |
| C-004 | Read-only root filesystem | P0 | Automated | image-inspect.json |
| C-005 | Container starts within 10 seconds | P0 | Automated | container-start.log |
| C-006 | Health endpoint returns 200 | P0 | Automated | health-response.json |
| C-007 | Metrics endpoint accessible | P0 | Automated | metrics-response.txt |
| C-008 | Container logs to stdout | P0 | Automated | container-logs-full.txt |
| C-009 | Container handles SIGTERM cleanly | P0 | Automated | shutdown-test.log |
| C-010 | Zero HIGH/CRITICAL vulnerabilities | P0 | Automated | trivy-report.json |
| C-011 | Image digest recorded | P1 | Automated | image-digest.txt |
| C-012 | Resource limits verified | P1 | Automated | resource-usage.txt |

### 3.3 Cloud Run Deployment Tests (Phase 2.1)

| ID | Test Case | Priority | Automation | Evidence |
|----|-----------|----------|------------|----------|
| CR-001 | Service deploys in < 5 minutes | P0 | Automated | terraform-apply.log |
| CR-002 | Health endpoint accessible | P0 | Automated | health-response.txt |
| CR-003 | Cold start < 30 seconds | P0 | Automated | cold-start-response.txt |
| CR-004 | Automatic scaling works | P0 | Automated | scaling-test.log |
| CR-005 | Service URL accessible | P0 | Automated | service-url.txt |
| CR-006 | Resource limits configured | P1 | Automated | service-describe.json |
| CR-007 | IAM policy restricted | P1 | Automated | iam-policy.json |
| CR-008 | Logs appear in Cloud Logging | P1 | Automated | service-logs.json |
| CR-009 | Concurrency setting verified | P1 | Automated | concurrency.txt |
| CR-010 | Revision history available | P2 | Automated | revisions.txt |
| CR-011 | Terraform destroy cleans up | P0 | Automated | terraform-destroy.log |
| CR-012 | Service re-deploy successful | P1 | Automated | redeploy-test.log |

### 3.4 GKE Deployment Tests (Phase 2.2)

| ID | Test Case | Priority | Automation | Evidence |
|----|-----------|----------|------------|----------|
| GKE-001 | Cluster boots successfully | P0 | Automated | terraform-apply.log |
| GKE-002 | Regional cluster spans 3 zones | P0 | Automated | nodes-zones.log |
| GKE-003 | All nodes Ready | P0 | Automated | nodes.log |
| GKE-004 | Workload Identity bindings exist | P0 | Automated | workload-identity-config.txt |
| GKE-005 | Network policies created | P0 | Automated | network-policies.log |
| GKE-006 | Private cluster configured (if applicable) | P1 | Automated | private-cluster-config.txt |
| GKE-007 | Shielded nodes enabled | P0 | Automated | shielded-nodes.log |
| GKE-008 | Cluster autoscaling configured | P0 | Automated | autoscaling.log |
| GKE-009 | Cloud Logging integration | P1 | Automated | logging-integration.log |
| GKE-010 | Cloud Monitoring integration | P1 | Automated | monitoring-integration.log |
| GKE-011 | GKE version is recent | P1 | Automated | cluster-version.log |
| GKE-012 | Node pool configuration verified | P1 | Automated | node-pools.log |
| GKE-013 | Pod security policies present | P0 | Automated | pod-security-policy.log |
| GKE-014 | kubectl can access cluster | P0 | Automated | cluster-info.log |
| GKE-015 | Cluster endpoint accessible | P0 | Automated | endpoint-test.log |
| GKE-016 | Terraform destroy cleans up | P0 | Automated | terraform-destroy.log |

### 3.5 Compute Engine Deployment Tests (Phase 2.3)

| ID | Test Case | Priority | Automation | Evidence |
|----|-----------|----------|------------|----------|
| GCE-001 | VM boots successfully | P0 | Automated | terraform-apply.log |
| GCE-002 | External IP assigned | P0 | Automated | instance-details.json |
| GCE-003 | Health endpoint accessible | P0 | Automated | health-response.txt |
| GCE-004 | Cloud Ops Agent running | P0 | Automated | ops-agent-status.txt |
| GCE-005 | Startup script executed | P0 | Automated | serial-output.txt |
| GCE-006 | Service status active | P0 | Automated | service-status.txt |
| GCE-007 | Firewall rules configured | P1 | Automated | firewall-rules.txt |
| GCE-008 | SSH access working | P1 | Automated | ssh-test.log |
| GCE-009 | Metadata server accessible | P2 | Automated | metadata-test.txt |
| GCE-010 | Logs ingest to Cloud Logging | P1 | Automated | cloud-logs.json |
| GCE-011 | Serial port output captured | P1 | Automated | serial-output.txt |
| GCE-012 | Instance in RUNNING state | P0 | Automated | instance-status.txt |
| GCE-013 | Terraform destroy cleans up | P0 | Automated | terraform-destroy.log |

### 3.6 Failure Scenario Tests (Phase 3)

| ID | Test Case | Priority | Automation | Evidence |
|----|-----------|----------|------------|----------|
| F-001 | Pod failure recovery | P0 | Semi-automated | pod-failure.log |
| F-002 | Zone failure simulation | P1 | Semi-automated | zone-failure.log |
| F-003 | Secret rotation | P1 | Semi-automated | secret-rotation.log |
| F-004 | Node drain graceful | P1 | Semi-automated | node-drain.log |
| F-005 | Network partition recovery | P1 | Semi-automated | partition-recovery.log |
| F-006 | OOM killer handling | P1 | Semi-automated | oom-test.log |
| F-007 | Disk space exhaustion | P2 | Manual | disk-exhaustion.log |
| F-008 | CPU throttling recovery | P1 | Semi-automated | cpu-throttle.log |

### 3.7 Observability Tests (Phase 4)

| ID | Test Case | Priority | Automation | Evidence |
|----|-----------|----------|------------|----------|
| O-001 | Metrics export to Cloud Monitoring | P0 | Automated | metrics-export.log |
| O-002 | Custom metrics visible | P0 | Automated | custom-metrics.json |
| O-003 | Alert policy fires correctly | P1 | Semi-automated | alert-fire.log |
| O-004 | Notification channel receives alert | P1 | Semi-automated | notification-received.log |
| O-005 | Dashboard accessible | P0 | Manual | dashboard-screenshot.png |
| O-006 | Logs appear within 30 seconds | P0 | Automated | log-latency.log |
| O-007 | Log severity levels correct | P0 | Automated | log-severity.log |
| O-008 | Structured log fields present | P1 | Automated | log-structure.log |
| O-009 | Trace correlation working | P1 | Automated | trace-correlation.log |
| O-010 | Uptime dashboard configured | P1 | Manual | uptime-dashboard.png |

### 3.8 Security & Compliance Tests (Phase 5)

| ID | Test Case | Priority | Automation | Evidence |
|----|-----------|----------|------------|----------|
| SEC-001 | No Owner/Editor roles assigned | P0 | Automated | iam-review.log |
| SEC-002 | Only required roles assigned | P0 | Automated | iam-review.log |
| SEC-003 | Workload Identity configured | P0 | Automated | workload-identity.log |
| SEC-004 | Secret access properly restricted | P0 | Automated | secret-acl.log |
| SEC-005 | Private endpoint enabled (if configured) | P1 | Automated | network-isolation.log |
| SEC-006 | Nodes have no public IPs (if applicable) | P1 | Automated | network-isolation.log |
| SEC-007 | Default-deny policy exists | P0 | Automated | network-policy.yaml |
| SEC-008 | Firewall rules restrict access | P0 | Automated | firewall-rules.txt |
| SEC-009 | Non-root container user | P0 | Automated | container-security.log |
| SEC-010 | Read-only root filesystem | P0 | Automated | container-security.log |
| SEC-011 | All capabilities dropped | P0 | Automated | container-security.log |
| SEC-012 | Security context configured | P0 | Automated | pod-security.yaml |

### 3.9 Customer Operations Tests (Phase 6)

| ID | Test Case | Priority | Automation | Evidence |
|----|-----------|----------|------------|----------|
| OPS-001 | Secret rotation procedure documented | P0 | Manual | rotation-procedure.md |
| OPS-002 | Secret rotation tested | P0 | Automated | secret-rotation.log |
| OPS-003 | Service remains available during rotation | P0 | Automated | availability-test.log |
| OPS-004 | HPA triggers on load | P0 | Automated | hpa-trigger.log |
| OPS-005 | New pods start quickly | P0 | Automated | pod-start-time.log |
| OPS-006 | Scale down after cooldown | P1 | Automated | scale-down.log |
| OPS-007 | Zero downtime during upgrade | P0 | Automated | upgrade-downtime.log |
| OPS-008 | No 5xx errors during rollout | P0 | Automated | rollout-errors.log |
| OPS-009 | Rollback available | P0 | Automated | rollback-test.log |
| OPS-010 | Terraform destroy cleans up | P0 | Automated | destroy-verification.log |

### 3.10 Unit Tests (Erlang/OTP)

| Module | Test File | Test Count | Coverage |
|--------|-----------|------------|----------|
| JSON-RPC | erlmcp_json_rpc_tests.erl | 24 | 90% |
| Transport | erlmcp_transport_stdio_tests.erl | 18 | 85% |
| Session | erlmcp_session_statem_tests.erl | 22 | 85% |
| Registry | erlmcp_registry_tests.erl | 15 | 80% |
| Rate Limiting | erlmcp_rate_limiting_tests.erl | 12 | 85% |
| Circuit Breaker | erlmcp_circuit_breaker_tests.erl | 10 | 80% |

### 3.11 Integration Test Suites

| Suite | Purpose | Test Count |
|-------|---------|------------|
| erlmcp_e2e_SUITE.erl | End-to-end MCP protocol flows | 8 |
| erlmcp_integration_SUITE.erl | Cross-module integration | 12 |
| erlmcp_clustering_SUITE.erl | Distributed node behavior | 10 |
| erlmcp_upgrade_SUITE.erl | Hot code upgrade paths | 6 |

---

## 4. Test Automation

### 4.1 Existing Automation Scripts

| Script | Purpose | Coverage |
|--------|---------|----------|
| `test-container.sh` | Container image validation | 11 tests |
| `test-cloudrun.sh` | Cloud Run deployment | 12 tests |
| `test-gke.sh` | GKE cluster deployment | 14 tests |
| `test-gce.sh` | Compute Engine VM deployment | 13 tests |
| `test-vm-image.sh` | Packer image validation | 9 tests |
| `test-deployment.sh` | E2E deployment orchestration | 4 tests |

### 4.2 Automation Architecture

```bash
marketplace/gcp/scripts/
├── test-container.sh          # Container image tests
├── test-cloudrun.sh           # Cloud Run deployment tests
├── test-gke.sh                # GKE deployment tests
├── test-gce.sh                # Compute Engine tests
├── test-vm-image.sh           # Packer VM image tests
├── test-deployment.sh         # Orchestrates all deployment tests
├── test-security.sh           # [NEW] Security validation
├── test-observability.sh      # [NEW] Observability validation
└── test-marketplace.sh        # [NEW] Full marketplace simulation
```

### 4.3 Automation Gaps

| Gap | Impact | Recommendation |
|-----|--------|----------------|
| No security scanning automation | HIGH | Create `test-security.sh` with Trivy + GCP scanning |
| No observability validation | MEDIUM | Create `test-observability.sh` for metrics/logs |
| No failure simulation automation | MEDIUM | Implement Chaos Engineering hooks |
| No performance baseline tests | MEDIUM | Add load testing with k6 |
| No marketplace UI simulation | LOW | Manual testing required |

### 4.4 Mock/Stub Strategy

#### Unit Test Mocks

```erlang
% Mock Registry for transport tests
mock_registry() ->
    meck:new(erlmcp_registry, [passthrough]),
    meck:expect(erlmcp_registry, register, fun(_, _) -> ok end),
    meck:expect(erlmcp_registry, lookup, fun(_) -> {ok, self()} end).

% Mock Transport for protocol tests
mock_transport() ->
    meck:new(erlmcp_transport_stdio, [passthrough]),
    meck:expect(erlmcp_transport_stdio, send, fun(_, _) -> ok end),
    meck:expect(erlmcp_transport_stdio, recv, fun() -> {ok, data} end).
```

#### Integration Test Stubs

- **Secret Manager**: Use `gcloud local` secrets for testing
- **Pub/Sub**: Use local emulator or stubbed responses
- **Cloud Logging**: Capture to local file instead of API
- **Cloud Monitoring**: Export to local Prometheus for validation

---

## 5. Test Data Management

### 5.1 Test Data Classification

| Classification | Example | Storage |
|----------------|---------|---------|
| Public Test Data | Sample JSON-RPC requests | Git repository |
| Generated Data | Random session IDs | Generated at runtime |
| Sensitive Test Data | Test credentials | Secret Manager (test project) |
| Marketplace Credentials | Reviewer accounts | Partner Portal only |

### 5.2 Test Data Lifecycle

1. **Creation**: Test data generated during test setup
2. **Usage**: Tests consume data via fixtures or generators
3. **Cleanup**: Automatic cleanup after test execution
4. **Retention**: Evidence retained for 90 days per requirement

### 5.3 Test Fixtures

Located in `/marketplace/gcp/test-fixtures/`:

```
test-fixtures/
├── mcp-requests/
│   ├── initialize.json
│   ├── call-tools.json
│   └── subscribe-resources.json
├── schemas/
│   ├── base-schema.json
│   └── extended-schema.json
├── certificates/
│   └── test-cert.pem
└── configurations/
    ├── minimal-config.yaml
    └── full-config.yaml
```

---

## 6. Test Reporting

### 6.1 Test Report Format

#### JSON Report (Machine Readable)

```json
{
  "testRunId": "marketplace-validation-20260202",
  "timestamp": "2026-02-02T10:00:00Z",
  "projectId": "erlmcp-test-project",
  "summary": {
    "total": 142,
    "passed": 138,
    "failed": 2,
    "skipped": 2,
    "durationSeconds": 2847
  },
  "phases": [
    {
      "name": "Static Validation",
      "status": "PASS",
      "tests": 8,
      "durationSeconds": 45
    },
    {
      "name": "Container Image",
      "status": "PASS",
      "tests": 12,
      "durationSeconds": 120
    }
  ],
  "failures": [
    {
      "testId": "O-003",
      "name": "Alert policy fires correctly",
      "reason": "Notification channel not configured",
      "logs": "alert-fire.log"
    }
  ]
}
```

#### Markdown Report (Human Readable)

Generated as `TEST_SUMMARY.md` with sections:
- Executive Summary
- Phase-by-Phase Results
- Failed Tests Detail
- Screenshots
- Evidence Artifacts List

### 6.2 Evidence Artifacts

All evidence stored in `/marketplace/gcp/test-evidence/`:

```
test-evidence/
├── 0.1-terraform.log
├── 0.2-schema.log
├── 1.1-container.log
├── 1.2-scan.log
├── 2.1-cloudrun/
├── 2.2-gke/
├── 2.3-gce/
├── screenshots/
└── TEST_SUMMARY.md
```

### 6.3 Failure Analysis Procedure

1. **Immediate Analysis**: Review log file referenced in failure
2. **Categorization**:
   - Infrastructure failure (Terraform/GCP)
   - Application failure (erlmcp bug)
   - Test failure (flaky test)
   - Configuration failure (missing parameter)
3. **Root Cause**: Document using 5 Whys technique
4. **Resolution**: Fix and re-run only affected tests
5. **Regression**: Add test case to prevent recurrence

---

## 7. Test Execution Plan

### 7.1 Pre-Submission Checklist

| Step | Command | Owner | Duration |
|------|---------|-------|----------|
| 1. Environment Setup | `./scripts/setup-test-env.sh` | DevOps | 5 min |
| 2. Static Validation | `./scripts/test-deployment.sh --validate-only` | QA | 2 min |
| 3. Container Tests | `./scripts/test-container.sh` | QA | 10 min |
| 4. Cloud Run Tests | `./scripts/test-cloudrun.sh` | QA | 15 min |
| 5. GKE Tests | `./scripts/test-gke.sh` | QA | 30 min |
| 6. GCE Tests | `./scripts/test-gce.sh` | QA | 20 min |
| 7. Security Scan | `./scripts/test-security.sh` | Security | 15 min |
| 8. Observability Check | `./scripts/test-observability.sh` | SRE | 10 min |
| 9. Report Generation | `./scripts/generate-report.sh` | QA | 2 min |
| 10. Evidence Archive | `./scripts/package-evidence.sh` | QA | 3 min |

**Total Duration**: ~112 minutes (automated) + manual verification

### 7.2 Continuous Testing Strategy

For post-launch validation:

| Trigger | Tests to Run | Frequency |
|---------|--------------|-----------|
| Code Commit | Unit + Integration | Every PR |
| Container Build | Container Image | Daily |
| Terraform Change | Static Validation | Every change |
| Weekly | Full Deployment Suite | Weekly |
| Pre-Release | Full Suite + Manual | Every release |

### 7.3 Marketplace Review Simulation

Simulate exactly what Google reviewers will do:

1. **Deploy via Terraform** from Marketplace package
2. **Verify health endpoint** responds within 5 seconds
3. **Check Cloud Logging** for structured logs
4. **Verify Cloud Monitoring** for custom metrics
5. **Test scaling** by increasing load
6. **Verify cleanup** via `terraform destroy`

---

## 8. Gap Analysis

### 8.1 Current Coverage Gaps

| Area | Current | Required | Gap | Priority |
|------|---------|----------|-----|----------|
| Security scanning automation | Manual | Automated | Create test-security.sh | HIGH |
| Observability validation | Partial | Complete | Metrics verification | MEDIUM |
| Performance baseline | None | Required | Load test suite | MEDIUM |
| Chaos engineering | None | Recommended | Failure injection | LOW |
| Marketplace UI simulation | Manual | Manual only | Documentation | LOW |

### 8.2 Test Infrastructure Gaps

| Gap | Current State | Desired State | Effort |
|-----|---------------|---------------|--------|
| Test data management | Ad-hoc | Centralized fixtures | Medium |
| Result aggregation | Manual | Automated dashboard | High |
| Failure triage | Manual | Automated categorization | Medium |
| Test isolation | Docker | Complete isolation | Low |
| Parallel execution | Sequential | Parallel (where possible) | High |

---

## 9. Recommendations

### 9.1 Immediate Actions (Before Submission)

1. **Create `test-security.sh`**
   - Integrate Trivy scanning
   - Validate GCP Container Analysis results
   - Check IAM policies
   - Verify network policies

2. **Create `test-observability.sh`**
   - Verify metrics export to Cloud Monitoring
   - Validate log format and ingestion
   - Check dashboard accessibility
   - Test alert policy firing

3. **Enhance Evidence Collection**
   - Add automatic screenshot capture
   - Generate JSON summary reports
   - Archive all logs with timestamps

### 9.2 Medium-Term Improvements

1. **Add Performance Testing**
   - Baseline metrics for deployment time
   - Load testing with k6
   - Resource usage profiling

2. **Implement Chaos Testing**
   - Pod failure simulation
   - Network partition testing
   - Zone failure validation

3. **Create Test Dashboard**
   - Real-time test execution status
   - Historical trend analysis
   - Coverage metrics visualization

### 9.3 Long-Term Strategy

1. **Continuous Validation**
   - Automated nightly test runs
   - Regression detection
   - Performance degradation alerts

2. **Marketplace Feedback Integration**
   - Track reviewer feedback
   - Update tests based on findings
   - Maintain test suite evolution

3. **Customer-Facing Validation**
   - Provide test artifacts to customers
   - Enable self-service validation
   - Shared test framework

---

## Appendix A: Test Execution Commands

### Quick Test Commands

```bash
# Run all tests
export PROJECT_ID=your-test-project
cd marketplace/gcp
./scripts/test-deployment.sh

# Run specific platform tests
./scripts/test-cloudrun.sh --project $PROJECT_ID
./scripts/test-gke.sh --project $PROJECT_ID
./scripts/test-gce.sh --project $PROJECT_ID

# Run container tests only
./scripts/test-container.sh --project $PROJECT_ID --region us-central1

# Generate evidence package
./scripts/generate-evidence.sh --output test-evidence-package.tar.gz
```

### Docker-Only Execution (Constitution Compliance)

```bash
# All tests MUST run via Docker
docker compose -f docker-compose.test.yml run \
  -e PROJECT_ID=$PROJECT_ID \
  marketplace-test \
  ./scripts/test-deployment.sh

# Individual test suites
docker compose run marketplace-test ./scripts/test-container.sh
docker compose run marketplace-test ./scripts/test-cloudrun.sh
```

---

## Appendix B: Exit Criteria

### Marketplace Submission Exit Criteria

- [ ] All Phase 0 (Static) tests: 100% pass
- [ ] All Phase 1 (Artifact) tests: 100% pass
- [ ] All Phase 2 (Deployment) tests: 100% pass
- [ ] Security scan: Zero HIGH/CRITICAL CVEs
- [ ] Health check: Responds within 5 seconds
- [ ] Metrics: Visible in Cloud Monitoring
- [ ] Logs: Queryable in Cloud Logging
- [ ] Cleanup: `terraform destroy` completes successfully
- [ ] Documentation: All required documents present

### Test Sign-Off

| Role | Name | Signature | Date |
|------|------|-----------|------|
| Test Engineer | | | |
| QA Lead | | | |
| Security Lead | | | |
| Release Manager | | | |

---

**Document Control**

| Version | Date | Author | Changes |
|---------|------|--------|---------|
| 1.0.0 | 2026-02-02 | Test Engineering | Initial release |

*This document is part of the GCP Marketplace submission for erlmcp v3.0.0*
