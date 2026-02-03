# Evidence Collection Template for Marketplace Review

**Version**: 1.0.0
**Date**: 2026-02-02
**Purpose**: Template for documenting evidence during marketplace reviewer simulation

---

## How to Use This Template

1. **Copy this template** for each simulation run
2. **Rename** with timestamp: `evidence-YYYYMMDD-HHMMSS.md`
3. **Fill in** all sections during the simulation
4. **Attach** supporting artifacts (logs, screenshots, configs)
5. **Review** completeness before submission

---

## Section 1: Simulation Metadata

| Field | Value |
|-------|-------|
| **Simulation ID** | `sim-$(date +%Y%m%d-%H%M%S)` |
| **Date/Time** | `[UTC timestamp]` |
| **Reviewer** | `[Name or "Automated"]` |
| **Project ID** | `[GCP Project ID]` |
| **Region** | `[us-central1, etc]` |
| **Zone** | `[us-central1-a, etc]` |
| **erlmcp Version** | `3.0.0` |
| **Git Commit SHA** | `[git rev-parse HEAD]` |
| **Terraform Version** | `[terraform version output]` |
| **Packer Version** | `[packer version output]` |

---

## Section 2: Phase 0 Evidence - Static Validation

### 2.1 Terraform Validation

**Status**: [ ] PASS / [ ] FAIL / [ ] SKIP

**Command Executed**:
```bash
terraform init -upgrade
terraform fmt -check
terraform validate
```

**Evidence**:

| Check | Result | Notes |
|-------|--------|-------|
| Format check | [ ] Pass / [ ] Fail | Number of files needing format: ___ |
| Validation check | [ ] Pass / [ ] Fail | Error count: ___ |
| Required providers | [ ] Pass / [ ] Fail | All providers pinned |

**Artifacts**:
- Log file: `___`
- Output snippet: ```___```

**Issues Found**:
- ___
- ___

---

### 2.2 Schema Validation

**Status**: [ ] PASS / [ ] FAIL / [ ] SKIP

**Command Executed**:
```bash
./scripts/validate-schema.sh
```

**Evidence**:

| Check | Result | Notes |
|-------|--------|-------|
| YAML syntax (application.yaml) | [ ] Pass / [ ] Fail | |
| YAML syntax (schema.yaml) | [ ] Pass / [ ] Fail | |
| YAML syntax (parameters.yaml) | [ ] Pass / [ ] Fail | |
| Required fields present | [ ] Pass / [ ] Fail | Missing: ___ |
| Schema-to-Terraform mapping | [ ] Pass / [ ] Fail | Unmapped properties: ___ |

**Artifacts**:
- Log file: `___`
- Schema validation report: `___`

**Issues Found**:
- ___
- ___

---

### 2.3 Helm Chart Validation

**Status**: [ ] PASS / [ ] FAIL / [ ] SKIP

**Command Executed**:
```bash
helm lint helm/erlmcp-marketplace
```

**Evidence**:

| Check | Result | Notes |
|-------|--------|-------|
| Chart lint | [ ] Pass / [ ] Fail | |
| Template rendering | [ ] Pass / [ ] Fail | |
| Values validation | [ ] Pass / [ ] Fail | |

**Artifacts**:
- Log file: `___`

**Issues Found**:
- ___
- ___

---

## Section 3: Phase 1 Evidence - Artifact Tests

### 3.1 Container Image Test

**Status**: [ ] PASS / [ ] FAIL / [ ] SKIP

**Image Details**:

| Field | Value |
|-------|-------|
| Image URI | `___` |
| Image Digest | `___` |
| Image Size (compressed) | `___ MB` |
| Base Image | `___` |
| Build Date | `___` |

**Command Executed**:
```bash
./scripts/test-container.sh --project $PROJECT_ID --region $REGION
```

**Evidence**:

| Check | Result | Notes |
|-------|--------|-------|
| Image pulls successfully | [ ] Pass / [ ] Fail | |
| Image starts successfully | [ ] Pass / [ ] Fail | |
| Health check responds | [ ] Pass / [ ] Fail | Response time: ___ ms |
| Required labels present | [ ] Pass / [ ] Fail | Missing: ___ |
| Non-root user | [ ] Pass / [ ] Fail | UID: ___ |

**Artifacts**:
- Log file: `___`
- Docker inspect output: `___`

**Issues Found**:
- ___
- ___

---

### 3.2 Security Vulnerability Scan

**Status**: [ ] PASS / [ ] FAIL / [ ] SKIP

**Command Executed**:
```bash
./scripts/scan-image.sh --project $PROJECT_ID --region $REGION --mode strict
```

**Evidence**:

| Severity | Found | Allowable | Status |
|----------|-------|-----------|--------|
| CRITICAL | ___ | 0 | [ ] Pass / [ ] Fail |
| HIGH | ___ | 0 | [ ] Pass / [ ] Fail |
| MEDIUM | ___ | 10 | [ ] Pass / [ ] Fail |
| LOW | ___ | Any | [ ] Pass / [ ] Fail |

**Vulnerability Details**:

| CVE ID | Severity | Package | Fixed In |
|--------|----------|---------|----------|
| ___ | ___ | ___ | ___ |
| ___ | ___ | ___ | ___ |

**Artifacts**:
- Scan log: `___`
- Scan summary: `___`

**Remediation Required**:
- ___
- ___

---

## Section 4: Phase 2 Evidence - Deployment Tests

### 4.1 Cloud Run Deployment

**Status**: [ ] PASS / [ ] FAIL / [ ] SKIP

**Deployment Details**:

| Field | Value |
|-------|-------|
| Service Name | `___` |
| Region | `___` |
| Service URL | `___` |
| Health Check URL | `___` |
| Deploy Time | `___ seconds` |

**Command Executed**:
```bash
./scripts/test-cloudrun.sh --project $PROJECT_ID --region $REGION
```

**Evidence**:

| Check | Result | Notes |
|-------|--------|-------|
| Terraform apply | [ ] Pass / [ ] Fail | Resources created: ___ |
| Service healthy | [ ] Pass / [ ] Fail | HTTP status: ___ |
| Health endpoint | [ ] Pass / [ ] Fail | Response: `{"status": "___"}` |
| Logs in Cloud Logging | [ ] Pass / [ ] Fail | Log entries: ___ |
| Metrics in Monitoring | [ ] Pass / [ ] Fail | Metrics visible: ___ |
| Terraform destroy | [ ] Pass / [ ] Fail | Resources cleaned up: ___ |

**Health Check Response**:
```json
{
  "status": "___",
  "version": "___",
  "uptime": "___"
}
```

**Artifacts**:
- Deployment log: `___`
- Terraform output: `___`
- Screenshot of deployment: `___`
- Screenshot of logs: `___`

**Issues Found**:
- ___
- ___

---

### 4.2 GKE Deployment

**Status**: [ ] PASS / [ ] FAIL / [ ] SKIP

**Deployment Details**:

| Field | Value |
|-------|-------|
| Cluster Name | `___` |
| Region | `___` |
| Node Count | `___` |
| Machine Type | `___` |
| Deploy Time | `___ seconds` |

**Command Executed**:
```bash
./scripts/test-gke.sh --project $PROJECT_ID --region $REGION
```

**Evidence**:

| Check | Result | Notes |
|-------|--------|-------|
| Cluster creation | [ ] Pass / [ ] Fail | Time: ___ seconds |
| Node provisioning | [ ] Pass / [ ] Fail | Nodes ready: ___ |
| Pod deployment | [ ] Pass / [ ] Fail | Pods ready: ___ / ___ |
| Service exposure | [ ] Pass / [ ] Fail | Ingress IP: ___ |
| Health endpoint | [ ] Pass / [ ] Fail | HTTP status: ___ |
| Scaling test | [ ] Pass / [ ] Fail | Scaled to: ___ replicas |
| Logs integration | [ ] Pass / [ ] Fail | |
| Metrics integration | [ ] Pass / [ ] Fail | |
| Terraform destroy | [ ] Pass / [ ] Fail | Resources cleaned up: ___ |

**kubectl Output**:
```bash
# Pods
kubectl get pods -l app=erlmcp
# NAME                          READY   STATUS    RESTARTS   AGE
# erlmcp-xxxxx-xxxxx            1/1     Running   0          2m

# Services
kubectl get services
# NAME      TYPE           CLUSTER-IP      EXTERNAL-IP
# erlmcp    LoadBalancer   xx.xx.xx.xx     xx.xx.xx.xx
```

**Artifacts**:
- Deployment log: `___`
- Terraform output: `___`
- kubectl output: `___`
- Screenshot of GKE console: `___`
- Screenshot of workload dashboard: `___`

**Issues Found**:
- ___
- ___

---

### 4.3 Compute Engine Deployment

**Status**: [ ] PASS / [ ] FAIL / [ ] SKIP

**Deployment Details**:

| Field | Value |
|-------|-------|
| Instance Name | `___` |
| Zone | `___` |
| Machine Type | `___` |
| Image Family | `___` |
| External IP | `___` |
| Deploy Time | `___ seconds` |

**Command Executed**:
```bash
./scripts/test-gce.sh --project $PROJECT_ID --zone $ZONE
```

**Evidence**:

| Check | Result | Notes |
|-------|--------|-------|
| Instance creation | [ ] Pass / [ ] Fail | Time: ___ seconds |
| VM boot | [ ] Pass / [ ] Fail | Boot time: ___ seconds |
| Service startup | [ ] Pass / [ ] Fail | Systemd status: ___ |
| Health endpoint | [ ] Pass / [ ] Fail | HTTP status: ___ |
| SSH access | [ ] Pass / [ ] Fail | [ ] Tested / [ ] Not needed |
| Ops Agent installed | [ ] Pass / [ ] Fail | |
| Logs integration | [ ] Pass / [ ] Fail | |
| Metrics integration | [ ] Pass / [ ] Fail | |
| Terraform destroy | [ ] Pass / [ ] Fail | Resources cleaned up: ___ |

**Service Status**:
```bash
systemctl status erlmcp
# erlmcp.service - erlmcp Erlang/OTP MCP Server
#    Loaded: loaded (/etc/systemd/system/erlmcp.service)
#    Active: active (running) since ...
```

**Artifacts**:
- Deployment log: `___`
- Terraform output: `___`
- Serial port output: `___`
- Screenshot of VM console: `___`
- Screenshot of Ops Agent status: `___`

**Issues Found**:
- ___
- ___

---

## Section 5: Observability Evidence

### 5.1 Cloud Logging Integration

**Status**: [ ] PASS / [ ] FAIL

**Evidence**:

| Check | Result | Notes |
|-------|--------|-------|
| Logs appear in console | [ ] Pass / [ ] Fail | |
| Structured JSON format | [ ] Pass / [ ] Fail | |
| Severity levels present | [ ] Pass / [ ] Fail | DEBUG, INFO, WARNING, ERROR |
| Correlation IDs present | [ ] Pass / [ ] Fail | Request ID tracking |
| Log entries count | [ ] Pass / [ ] Fail | Found: ___ entries |

**Log Query**:
```
resource.labels.container_name="erlmcp"
```

**Sample Log Entry**:
```json
{
  "severity": "___",
  "timestamp": "___",
  "service": "erlmcp",
  "message": "___",
  "labels": {
    "app": "erlmcp",
    "version": "3.0.0"
  }
}
```

**Artifacts**:
- Screenshot of Cloud Logging console: `___`
- Log sample export: `___`

---

### 5.2 Cloud Monitoring Integration

**Status**: [ ] PASS / [ ] FAIL

**Evidence**:

| Check | Result | Notes |
|-------|--------|-------|
| Custom metrics visible | [ ] Pass / [ ] Fail | Count: ___ metrics |
| Metric labels present | [ ] Pass / [ ] Fail | Labels: ___ |
| Dashboard available | [ ] Pass / [ ] Fail | Dashboard URL: ___ |
| Alert policies created | [ ] Pass / [ ] Fail | Policies: ___ |

**Metrics Verified**:

| Metric Name | Type | Visible | Notes |
|-------------|------|---------|-------|
| erlmcp_http_requests | CUMULATIVE | [ ] Yes / [ ] No | |
| erlmcp_http_latency | GAUGE | [ ] Yes / [ ] No | |
| erlmcp_connections_active | GAUGE | [ ] Yes / [ ] No | |
| erlmcp_memory_usage | GAUGE | [ ] Yes / [ ] No | |
| erlmcp_cpu_usage | GAUGE | [ ] Yes / [ ] No | |

**Artifacts**:
- Screenshot of Metrics Explorer: `___`
- Screenshot of dashboard: `___`
- Alert policy JSON: `___`

---

## Section 6: Security Evidence

### 6.1 Secrets Management

**Status**: [ ] PASS / [ ] FAIL

**Evidence**:

| Check | Result | Notes |
|-------|--------|-------|
| No hardcoded secrets | [ ] Pass / [ ] Fail | Scan result: ___ |
| Secret Manager used | [ ] Pass / [ ] Fail | Secrets: ___ |
| Workload Identity used | [ ] Pass / [ ] Fail | IAM annotation: ___ |
| Service account minimal | [ ] Pass / [ ] Fail | Roles: ___ |

**Secrets Scan Results**:
```bash
# Trufflehog scan
trufflehog filesystem /Users/sac/erlmcp/marketplace/gcp
# Found: ___ potential secrets
```

**Artifacts**:
- Secrets scan output: `___`
- Secret Manager configuration: `___`
- IAM policy: `___`

---

### 6.2 Network Security

**Status**: [ ] PASS / [ ] FAIL

**Evidence**:

| Check | Result | Notes |
|-------|--------|-------|
| Private cluster (GKE) | [ ] Pass / [ ] Fail / [ ] N/A | |
| Network policies (GKE) | [ ] Pass / [ ] Fail / [ ] N/A | |
| Firewall rules (GCE) | [ ] Pass / [ ] Fail / [ ] N/A | |
| TLS enabled | [ ] Pass / [ ] Fail | |
| No open 0.0.0.0/0 rules | [ ] Pass / [ ] Fail | |

**Artifacts**:
- Network diagram: `___`
- Firewall rules list: `___`
- Network policies: `___`

---

## Section 7: Documentation Evidence

### 7.1 Quickstart Verification

**Status**: [ ] PASS / [ ] FAIL

**Tester**: [ ] Self / [ ] Colleague / [ ] Fresh user

**Steps Tested**:

| Step | From Docs | Actual Result | Match? |
|------|-----------|---------------|--------|
| 1. Project setup | [ ] | [ ] | [ ] Yes / [ ] No |
| 2. API enablement | [ ] | [ ] | [ ] Yes / [ ] No |
| 3. Deployment command | [ ] | [ ] | [ ] Yes / [ ] No |
| 4. Health check | [ ] | [ ] | [ ] Yes / [ ] No |
| 5. Access service | [ ] | [ ] | [ ] Yes / [ ] No |

**Deviations Found**:
- ___
- ___

---

### 7.2 Link Validation

**Status**: [ ] PASS / [ ] FAIL

**All Links Checked**:

| URL | Status | Notes |
|-----|--------|-------|
| documentationURL | [ ] 200 / [ ] 404 / [ ] Other | |
| supportURL | [ ] 200 / [ ] 404 / [ ] Other | |
| icon URL | [ ] 200 / [ ] 404 / [ ] Other | |
| logo URL | [ ] 200 / [ ] 404 / [ ] Other | |

**Broken Links Found**:
- ___
- ___

---

### 7.3 Screenshot Accuracy

**Status**: [ ] PASS / [ ] FAIL

| Screenshot Location | Matches Current UI | Notes |
|---------------------|-------------------|-------|
| README.md deployment | [ ] Yes / [ ] No | |
| Architecture diagram | [ ] Yes / [ ] No | |
| Console screenshots | [ ] Yes / [ ] No | |

**Outdated Screenshots**:
- ___
- ___

---

## Section 8: Operations Evidence

### 8.1 Scaling Test

**Status**: [ ] PASS / [ ] FAIL / [ ] N/A

**Test Procedure**:
1. Deploy with 3 replicas
2. Trigger scale to 10 replicas
3. Verify new replicas healthy
4. Scale down to 3 replicas

**Results**:

| Operation | Expected Time | Actual Time | Status |
|-----------|--------------|-------------|--------|
| Scale up | < 5 min | ___ | [ ] Pass / [ ] Fail |
| Scale down | < 10 min | ___ | [ ] Pass / [ ] Fail |

**Artifacts**:
- Scaling log: `___`
- kubectl/hpa output: `___`

---

### 8.2 Update Test

**Status**: [ ] PASS / [ ] FAIL / [ ] N/A

**Test Procedure**:
1. Deploy version N
2. Update to version N+1
3. Verify rollback capability

**Results**:

| Operation | Status | Notes |
|-----------|--------|-------|
| Rolling update | [ ] Pass / [ ] Fail | Downtime: ___ |
| Rollback | [ ] Pass / [ ] Fail | Rollback time: ___ |

**Artifacts**:
- Update log: `___`
- Rollback log: `___`

---

### 8.3 Cleanup Test

**Status**: [ ] PASS / [ ] FAIL

**Command Executed**:
```bash
terraform destroy -auto-approve
```

**Evidence**:

| Resource Type | Before Destroy | After Destroy | Orphaned? |
|---------------|---------------|---------------|-----------|
| Instances | ___ | 0 | [ ] Yes / [ ] No |
| Disks | ___ | 0 | [ ] Yes / [ ] No |
| Addresses | ___ | 0 | [ ] Yes / [ ] No |
| Firewall rules | ___ | 0 | [ ] Yes / [ ] No |

**Orphaned Resources** (if any):
- Resource: ___
- Reason: ___
- Manual cleanup required: [ ] Yes / [ ] No

---

## Section 9: Final Assessment

### 9.1 Go/No-Go Decision

**Overall Status**: [ ] GO / [ ] NO-GO

**Decision Matrix**:

| Category | Weight | Score | Weighted Score |
|----------|--------|-------|----------------|
| Static Validation | 20% | ___/100 | ___/20 |
| Artifact Tests | 20% | ___/100 | ___/20 |
| Deployment Tests | 30% | ___/100 | ___/30 |
| Observability | 10% | ___/100 | ___/10 |
| Security | 15% | ___/100 | ___/15 |
| Documentation | 5% | ___/100 | ___/5 |
| **TOTAL** | **100%** | | **___/100** |

**Go Criteria**: Score >= 90/100 with no CRITICAL failures

**Actual Score**: ___/100

---

### 9.2 Critical Findings

Any CRITICAL issues must be listed here:

| ID | Issue | Severity | Status |
|----|-------|----------|--------|
| C1 | ___ | CRITICAL | [ ] Open / [ ] Fixed |
| C2 | ___ | CRITICAL | [ ] Open / [ ] Fixed |
| C3 | ___ | CRITICAL | [ ] Open / [ ] Fixed |

**ALL CRITICAL ISSUES MUST BE RESOLVED BEFORE SUBMISSION**

---

### 9.3 Recommendations

**For Production**:
1. ___
2. ___

**For Documentation**:
1. ___
2. ___

**For Future Simulations**:
1. ___
2. ___

---

### 9.4 Sign-Off

| Role | Name | Signature | Date |
|------|------|-----------|------|
| Reviewer | | | |
| Technical Lead | | | |
| Approval Authority | | | |

**Recommendation**: [ ] APPROVED for submission / [ ] NOT APPROVED - fixes required

---

## Appendix: Artifact Checklist

Attach these artifacts to evidence:

- [ ] All phase logs (0.1-terraform.log, 0.2-schema.log, etc.)
- [ ] Security scan report
- [ ] Terraform apply/destroy outputs
- [ ] kubectl outputs (for GKE)
- [ ] Cloud Logging screenshots
- [ ] Cloud Monitoring screenshots
- [ ] Deployment console screenshots
- [ ] Health check responses
- [ ] Resource inventory
- [ ] Network diagram

---

**Template Version**: 1.0.0
**Last Updated**: 2026-02-02
