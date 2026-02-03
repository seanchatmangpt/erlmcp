# Google Cloud Marketplace Test Evidence

## Test Execution Summary

**Test Date:** YYYY-MM-DD
**Test ID:** TBD
**Project ID:** TBD
**Tester:** TBD
**Validation Suite:** gcp-marketplace-validator v1.0.0

## Overall Result

[ ] PASS - All validation checks passed
[ ] FAIL - One or more validation checks failed
[ ] CONDITIONAL PASS - Passed with caveats

---

## Phase 0: Static Validation (No Cloud Resources)

### 0.1 Terraform Correctness

**Status:** [ ] PASS [ ] FAIL

**Evidence:**
- [ ] All Terraform modules validate (`terraform validate`)
- [ ] No `null_resource` or `local-exec` usage
- [ ] Terraform state configuration verified
- [ ] No hardcoded secrets

**Artifacts:**
- `0.1-terraform.log`

**Findings:**
<!-- Document any findings or issues -->

---

### 0.2 Marketplace Schema Validation

**Status:** [ ] PASS [ ] FAIL

**Evidence:**
- [ ] `application.yaml` has all required fields
- [ ] `schema.yaml` defines all input parameters
- [ ] `parameters.yaml` maps inputs to Terraform variables
- [ ] Default values exist for all non-required parameters
- [ ] Parameter types match Terraform variable types

**Artifacts:**
- `0.2-schema.log`
- `schema-validation-report.md`

**Findings:**
<!-- Document any findings or issues -->

---

### 0.3 Helm Chart Validation

**Status:** [ ] PASS [ ] FAIL [ ] SKIP

**Evidence:**
- [ ] All templates render without errors
- [ ] No undefined variables
- [ ] GCP-specific annotations are valid
- [ ] Workload Identity annotations present

**Artifacts:**
- `0.3-helm.log`

**Findings:**
<!-- Document any findings or issues -->

---

## Phase 1: Artifact Tests (Images & Containers)

### 1.1 Container Image Test

**Status:** [ ] PASS [ ] FAIL [ ] SKIP

**Evidence:**
- [ ] Image exists in Artifact Registry
- [ ] Image pulls successfully
- [ ] Container starts within 10 seconds
- [ ] Health endpoint returns 200
- [ ] Metrics endpoint accessible
- [ ] Container logs to stdout
- [ ] Container handles SIGTERM cleanly

**Artifacts:**
- `1.1-container.log`
- `container-logs-full.txt`
- `health-response.json`

**Findings:**
<!-- Document any findings or issues -->

---

### 1.2 Security Vulnerability Scan

**Status:** [ ] PASS [ ] FAIL

**Evidence:**
- [ ] Zero HIGH severity vulnerabilities
- [ ] Zero CRITICAL severity vulnerabilities
- [ ] Scan results saved (Trivy + GCP Container Analysis)
- [ ] Image digest recorded

**Vulnerability Summary:**

| Severity | Count |
|----------|-------|
| CRITICAL | 0 |
| HIGH     | 0 |
| MEDIUM   | TBD |
| LOW      | TBD |
| TOTAL    | TBD |

**Artifacts:**
- `1.2-scan.log`
- `trivy-report.json`
- `gcp-scan.json`
- `scan-summary.md`

**Findings:**
<!-- Document any findings or issues -->

---

### 1.3 VM Image Test (Packer)

**Status:** [ ] PASS [ ] FAIL [ ] SKIP

**Evidence:**
- [ ] Packer template validates
- [ ] Image builds without manual SSH
- [ ] Cloud Ops Agent installs cleanly
- [ ] Systemd service enabled
- [ ] Logs visible in Cloud Logging

**Artifacts:**
- `packer-build.log`
- `packer-validate.log`
- `serial-output.log`

**Findings:**
<!-- Document any findings or issues -->

---

## Phase 2: Deployment Tests (Core Validation)

### 2.1 Compute Engine Deployment

**Status:** [ ] PASS [ ] FAIL [ ] SKIP

**Evidence:**
- [ ] VM boots successfully
- [ ] External IP assigned
- [ ] Health endpoint accessible (http://IP/health)
- [ ] Cloud Ops Agent running
- [ ] Startup script executed
- [ ] Service status active

**Artifacts:**
- `2.3-gce/terraform-apply.log`
- `2.3-gce/instance-details.json`
- `2.3-gce/serial-output.txt`
- `2.3-gce/health-response.txt`

**Findings:**
<!-- Document any findings or issues -->

---

### 2.2 GKE Deployment (Most Scrutinized)

**Status:** [ ] PASS [ ] FAIL [ ] SKIP

**Evidence:**
- [ ] Regional cluster spans 3 zones
- [ ] All nodes are Ready
- [ ] Workload Identity bindings exist
- [ ] Network policies created
- [ ] Private cluster configured (if applicable)
- [ ] Shielded nodes enabled
- [ ] Cluster autoscaling configured

**Artifacts:**
- `2.2-gke/terraform-apply.log`
- `2.2-gke/nodes.log`
- `2.2-gke/network-policies.log`
- `2.2-gke/service-describe.json`

**Findings:**
<!-- Document any findings or issues -->

---

### 2.3 Cloud Run Deployment (Fastest)

**Status:** [ ] PASS [ ] FAIL [ ] SKIP

**Evidence:**
- [ ] Service deploys in < 5 minutes
- [ ] Health endpoint accessible
- [ ] Cold start time < 30 seconds
- [ ] Automatic scaling works
- [ ] Service URL accessible

**Artifacts:**
- `2.1-cloudrun/terraform-apply.log`
- `2.1-cloudrun/service-url.txt`
- `2.1-cloudrun/health-response.txt`
- `2.1-cloudrun/cold-start-response.txt`

**Findings:**
<!-- Document any findings or issues -->

---

## Phase 3: Failure Scenario Tests

### 3.1 Pod Failure Recovery

**Status:** [ ] PASS [ ] FAIL [ ] SKIP

**Evidence:**
- [ ] New pod starts within 2 minutes
- [ ] Health check passes on new pod
- [ ] No data loss

**Artifacts:**
- `3.1-pod-failure.log`

**Findings:**
<!-- Document any findings or issues -->

---

### 3.2 Zone Failure Simulation

**Status:** [ ] PASS [ ] FAIL [ ] SKIP

**Evidence:**
- [ ] Pods rescheduled to remaining zones
- [ ] Service remains accessible
- [ ] No downtime > 30 seconds

**Artifacts:**
- `3.2-zone-failure.log`

**Findings:**
<!-- Document any findings or issues -->

---

### 3.3 Secret Rotation

**Status:** [ ] PASS [ ] FAIL [ ] SKIP

**Evidence:**
- [ ] Service remains available during rotation
- [ ] All pods use new secret after rotation
- [ ] No authentication failures

**Artifacts:**
- `3.3-secret-rotation.log`

**Findings:**
<!-- Document any findings or issues -->

---

## Phase 4: Observability & SLO Validation

### 4.1 Metrics Export

**Status:** [ ] PASS [ ] FAIL [ ] SKIP

**Evidence:**
- [ ] Custom metrics appear in Cloud Monitoring
- [ ] Metric data is not empty
- [ ] Dashboard is accessible

**Artifacts:**
- `4.1-metrics.log`
- Screenshot of Cloud Monitoring dashboard

**Findings:**
<!-- Document any findings or issues -->

---

### 4.2 Alert Policy Test

**Status:** [ ] PASS [ ] FAIL [ ] SKIP

**Evidence:**
- [ ] Alert fires when threshold exceeded
- [ ] Notification channel receives alert
- [ ] Alert appears in Console

**Artifacts:**
- `4.2-alerts.log`
- Screenshot of alert notification

**Findings:**
<!-- Document any findings or issues -->

---

### 4.3 Logging Verification

**Status:** [ ] PASS [ ] FAIL [ ] SKIP

**Evidence:**
- [ ] Logs appear within 30 seconds
- [ ] Severity levels are correct
- [ ] Log entries have required fields

**Artifacts:**
- `4.3-logs.log`
- Sample log entries

**Findings:**
<!-- Document any findings or issues -->

---

## Phase 5: Security & Compliance Tests

### 5.1 IAM Review

**Status:** [ ] PASS [ ] FAIL [ ] SKIP

**Evidence:**
- [ ] No Owner/Editor roles assigned
- [ ] Only required roles assigned
- [ ] Workload Identity configured
- [ ] Secret access properly restricted

**Artifacts:**
- `5.1-iam.log`
- IAM policy export

**Findings:**
<!-- Document any findings or issues -->

---

### 5.2 Network Isolation

**Status:** [ ] PASS [ ] FAIL [ ] SKIP

**Evidence:**
- [ ] Private endpoint enabled (if configured)
- [ ] Nodes have no public IPs (if applicable)
- [ ] Default-deny policy exists
- [ ] Firewall rules restrict access

**Artifacts:**
- `5.2-network.log`
- Network policy YAML

**Findings:**
<!-- Document any findings or issues -->

---

### 5.3 Container Security

**Status:** [ ] PASS [ ] FAIL [ ] SKIP

**Evidence:**
- [ ] Non-root user (UID != 0)
- [ ] Read-only root filesystem
- [ ] All capabilities dropped
- [ ] Security context configured

**Artifacts:**
- `5.3-container-security.log`
- Pod security policy

**Findings:**
<!-- Document any findings or issues -->

---

## Phase 6: Customer Operations Tests

### 6.1 Secret Rotation Procedure

**Status:** [ ] PASS [ ] FAIL [ ] SKIP

**Evidence:**
- [ ] Rotation procedure documented
- [ ] Procedure tested successfully
- [ ] Service remains available

**Artifacts:**
- `6.1-secret-rotation.log`
- Rotation procedure documentation

**Findings:**
<!-- Document any findings or issues -->

---

### 6.2 Scaling Test

**Status:** [ ] PASS [ ] FAIL [ ] SKIP

**Evidence:**
- [ ] HPA triggers on load
- [ ] New pods start quickly
- [ ] Scale down after cooldown

**Artifacts:**
- `6.2-scaling.log`
- HPA metrics

**Findings:**
<!-- Document any findings or issues -->

---

### 6.3 Upgrade Test

**Status:** [ ] PASS [ ] FAIL [ ] SKIP

**Evidence:**
- [ ] Zero downtime during upgrade
- [ ] No 5xx errors during rollout
- [ ] Rollback available

**Artifacts:**
- `6.3-upgrade.log`
- Rollout history

**Findings:**
<!-- Document any findings or issues -->

---

## Phase 7: Marketplace Package Validation

### 7.1 Package Structure

**Status:** [ ] PASS [ ] FAIL

**Evidence:**
- [ ] All required files present
- [ ] File structure matches Marketplace requirements
- [ ] SHA256SUMS file generated

**Artifacts:**
- `7.1-package-structure.log`
- Package file listing

**Findings:**
<!-- Document any findings or issues -->

---

### 7.2 Schema Completeness

**Status:** [ ] PASS [ ] FAIL

**Evidence:**
- [ ] All schema properties have Terraform variables
- [ ] All Terraform variables documented
- [ ] No hidden/undocumented parameters

**Artifacts:**
- `7.2-schema-completeness.log`

**Findings:**
<!-- Document any findings or issues -->

---

## Final Checklist

### Marketplace Requirements

- [ ] Deployment completes without manual intervention
- [ ] Health endpoints respond within 5 seconds
- [ ] Metrics visible in Cloud Monitoring
- [ ] Logs queryable in Cloud Logging
- [ ] Zero HIGH/CRITICAL vulnerabilities
- [ ] Secrets in Secret Manager (not env vars)
- [ ] Private cluster works (if configured)
- [ ] Autoscaling functional
- [ ] Terraform destroy cleans up resources
- [ ] Documentation complete and accurate

### Reviewer Simulation Results

- [ ] Deploy from Marketplace UI: Tested/Passed
- [ ] Verify Health Endpoints: Tested/Passed
- [ ] Check Observability: Tested/Passed
- [ ] Test Scaling: Tested/Passed
- [ ] Verify Security: Tested/Passed
- [ ] Test Cleanup: Tested/Passed

---

## Known Limitations

<!-- List any known limitations or issues that should be communicated to Marketplace reviewers -->

Example:
1. Deployment time may exceed 15 minutes on first run (image download)
2. Some metrics may take 5-10 minutes to appear in Cloud Monitoring

---

## Screenshots

<!-- Attach screenshots for visual evidence -->

Required Screenshots:
1. [ ] Successful deployment message
2. [ ] Health check response
3. [ ] Cloud Monitoring dashboard with metrics
4. [ ] Cloud Logging with structured logs
5. [ ] Infrastructure overview (GKE cluster / Cloud Run service / VM instances)

---

## Sign-Off

**Tested By:** ______________________
**Date:** ______________________
**Result:** [ ] APPROVED FOR MARKETPLACE [ ] NEEDS REVISION

**Comments:**
<!-- Any additional comments for the review team -->

---

## Appendix: Evidence Files

All test evidence is located in: `marketplace/gcp/test-evidence/`

### Log Files
- `0.1-terraform.log`
- `0.2-schema.log`
- `0.3-helm.log`
- `1.1-container.log`
- `1.2-scan.log`
- `2.1-cloudrun/` - Cloud Run deployment evidence
- `2.2-gke/` - GKE deployment evidence
- `2.3-gce/` - Compute Engine deployment evidence

### Reports
- `VALIDATION_SUMMARY.md` - This file
- `marketplace-validation-report.json` - Machine-readable results
- `schema-validation-report.md` - Detailed schema validation
- `scan-summary.md` - Container image scan summary

### Configuration
- `terraform-outputs.json` - Terraform output values
- `service-describe.json` - Service configurations

---

*This evidence document is part of the Google Cloud Marketplace submission for erlmcp.*
