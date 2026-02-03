# GCP Marketplace Submission Checklist
## erlmcp v3.0.0 - Complete Submission Guide

**Document Version:** 1.0
**Last Updated:** 2026-02-02
**Purpose:** Ensure all requirements are met before submitting to Google Cloud Marketplace

---

## Quick Reference

| Category | Total Items | Required | Completed |
|----------|-------------|----------|-----------|
| Technical | 25 | 25 | 0 |
| Schema | 18 | 18 | 0 |
| Documentation | 12 | 12 | 0 |
| Security | 10 | 10 | 0 |
| Business | 8 | 8 | 0 |
| **TOTAL** | **73** | **73** | **0** |

---

## 1. Technical Requirements

### 1.1 Container Image

- [ ] **Image Location**
  - [ ] Image stored in Google Artifact Registry
  - [ ] Image follows naming convention: `{region}-docker.pkg.dev/{project}/{image}:{tag}`
  - [ ] Example: `us-central1-docker.pkg.dev/myproject/erlmcp/erlmcp:3.0.0`

- [ ] **Image Size**
  - [ ] Total image size < 500MB (recommended)
  - [ ] Base image is minimal (alpine, distroless, or slim)
  - [ ] Multi-stage build used to reduce size

- [ ] **Image Tags**
  - [ ] Semantic version tag present (e.g., `3.0.0`)
  - [ ] `latest` tag points to current stable version
  - [ ] Tags are immutable once published

- [ ] **Startup Requirements**
  - [ ] Application starts within 5 minutes
  - [ ] Health check endpoint available at `/health`
  - [ ] Graceful shutdown implemented (SIGTERM handling)

- [ ] **Configuration**
  - [ ] No hardcoded credentials or secrets
  - [ ] Environment variables for all configuration
  - [ ] Configuration validation at startup

### 1.2 Health Checks

- [ ] **Required Endpoints**
  - [ ] GET `/health` - Returns service health status
  - [ ] GET `/ready` - Returns readiness state
  - [ ] Optionally: GET `/metrics` - Prometheus metrics

- [ ] **Health Response Format**
  ```json
  {
    "status": "ok",
    "version": "3.0.0",
    "timestamp": "2026-02-02T10:00:00Z",
    "checks": {
      "database": "ok",
      "redis": "ok",
      "cluster": "ok"
    }
  }
  ```

- [ ] **HTTP Status Codes**
  - [ ] 200 OK when healthy
  - [ ] 503 Service Unavailable when unhealthy
  - [ ] Consistent response format

### 1.3 Logging

- [ ] **Log Format**
  - [ ] Structured JSON logging recommended
  - [ ] Logs include timestamp, severity, message
  - [ ] Logs include correlation IDs for request tracing

- [ ] **Log Destinations**
  - [ ] stdout/stderr for container logs
  - [ ] Cloud Logging integration configured
  - [ ] Log levels configurable

- [ ] **Log Content**
  - [ ] No sensitive data in logs (passwords, tokens)
  - [ ] Request/response logging for errors
  - [ ] Startup/shutdown messages

### 1.4 Metadata

- [ ] **Image Labels**
  ```dockerfile
  LABEL org.opencontainers.image.title="erlmcp"
  LABEL org.opencontainers.image.description="Erlang/OTP MCP Server"
  LABEL org.opencontainers.image.version="3.0.0"
  LABEL org.opencontainers.image.created="2026-02-02T10:00:00Z"
  LABEL org.opencontainers.image.revision="abc123"
  LABEL org.opencontainers.image.vendor="banyan-platform"
  LABEL org.opencontainers.image.licenses="Apache-2.0"
  ```

---

## 2. Schema Requirements

### 2.1 Application.yaml

- [ ] **Required Metadata Fields**
  - [ ] `apiVersion: marketplace.cloud.google.com/v1`
  - [ ] `kind: Application`
  - [ ] `metadata.name` - Unique identifier
  - [ ] `metadata.displayName` - Human-readable name
  - [ ] `metadata.description` - Detailed description
  - [ ] `metadata.version` - Current version
  - [ ] `metadata.publisher` - Publisher ID

- [ ] **Required Spec Fields**
  - [ ] `spec.runtimePolicy.minDeploymentDuration`
  - [ ] `spec.runtimePolicy.estimatedDeploymentDuration`
  - [ ] `spec.inputSchema` - Input configuration schema
  - [ ] `spec.deployment.manifests` - Deployment definitions
  - [ ] `spec.properties` - Application properties
  - [ ] `spec.outputSchema` - Output schema

- [ ] **Deployment Types**
  - [ ] At least one deployment type defined (gke/cloud-run/gce)
  - [ ] Each deployment type has selector matching inputSchema
  - [ ] Template paths are valid

### 2.2 Schema.yaml

- [ ] **JSON Schema Compliance**
  - [ ] `$schema: http://json-schema.org/draft-07/schema#`
  - [ ] `type: object`
  - [ ] All properties have types
  - [ ] Enums have valid values

- [ ] **Required Parameters**
  - [ ] `deployment_type` with valid enum values
  - [ ] `region` with supported GCP regions
  - [ ] `instance_tier` or similar sizing parameter

- [ ] **Parameter Metadata**
  - [ ] All parameters have descriptions
  - [ ] All parameters have titles
  - [ ] Enums have enumLabels
  - [ ] Default values for optional parameters

- [ ] **Marketplace Extensions**
  - [ ] `x-google-marketplace` tags where needed
  - [ ] `x-google-property` for UI hints
  - [ ] `x-google-visibility` for conditional fields

### 2.3 Parameters.yaml

- [ ] **Terraform Variable Mapping**
  - [ ] Each schema parameter maps to Terraform variable
  - [ ] Module mappings are correct
  - [ ] Value transformations defined where needed

- [ ] **Output Mappings**
  - [ ] All outputs defined in application.yaml
  - [ ] Output sources are correct Terraform outputs
  - [ ] Conditional outputs have conditions defined

- [ ] **Tier Mappings**
  - [ ] Instance tiers map to machine types
  - [ ] HA modes map to cluster configurations
  - [ ] All mappings have valid values

### 2.4 Metadata.display.yaml

- [ ] **Display Information**
  - [ ] `name` - Display name
  - [ ] `tagline` - Short description
  - [ ] `description` - Full description with HTML support
  - [ ] `icon` - Icon URL (storage bucket)
  - [ ] `logo` - Logo information

- [ ] **Marketing Content**
  - [ ] `features` - List of key features
  - [ ] `solutions` - Use cases addressed
  - [ ] `categories` - Marketplace categories
  - [ ] `tags` - Search keywords

- [ ] **Support Information**
  - [ ] `documentationURL`
  - [ ] `supportURL`
  - [ ] `email` for support
  - [ ] `privacyPolicyURL`
  - [ ] `termsOfServiceURL`

---

## 3. Documentation Requirements

### 3.1 README.md

- [ ] **Required Sections**
  - [ ] Overview and description
  - [ ] Prerequisites
  - [ ] Quick start guide
  - [ ] Configuration options
  - [ ] Deployment instructions
  - [ ] Troubleshooting
  - [ ] Support and contact information

- [ ] **Code Examples**
  - [ ] At least one working example
  - [ ] Example is tested and current
  - [ ] Comments explain key steps

### 3.2 Architecture Documentation

- [ ] **Architecture Diagram**
  - [ ] System architecture overview
  - [ ] Component interactions
  - [ ] Data flow diagram

- [ ] **Technical Details**
  - [ ] Technology stack
  - [ ] Dependencies and versions
  - [ ] Network configuration
  - [ ] Storage configuration

### 3.3 API Documentation

- [ ] **API Reference**
  - [ ] All endpoints documented
  - [ ] Request/response formats
  - [ ] Authentication requirements
  - [ ] Error responses

- [ ] **Examples**
  - [ ] Request examples for each endpoint
  - [ ] Response examples for each endpoint
  - [ ] Error scenario examples

### 3.4 Deployment Guide

- [ ] **Deployment Options**
  - [ ] Cloud Run deployment instructions
  - [ ] GKE deployment instructions
  - [ ] Compute Engine deployment instructions (if applicable)

- [ ] **Configuration**
  - [ ] All configuration options documented
  - [ ] Default values explained
  - [ ] Production recommendations

- [ ] **Post-Deployment**
  - [ ] Verification steps
  - [ ] Health check instructions
  - [ ] Monitoring setup

### 3.5 Migration Guide

- [ ] **Version Changes**
  - [ ] Breaking changes listed
  - [ ] Migration steps provided
  - [ ] Rollback instructions

- [ ] **Data Migration**
  - [ ] Schema changes documented
  - [ ] Data migration steps
  - [ ] Backup/restore procedures

---

## 4. Security Requirements

### 4.1 Vulnerability Scanning

- [ ] **Pre-Submission Scan**
  - [ ] Trivy scan completed
  - [ ] GCP Container Analysis completed
  - [ ] Zero CRITICAL vulnerabilities
  - [ ] Zero HIGH vulnerabilities (strict mode)

- [ ] **Scan Reports**
  - [ ] Trivy report saved
  - [ ] GCP scan results saved
  - [ ] VEX document available
  - [ ] Remediation plan for any findings

### 4.2 Container Security

- [ ] **Base Image**
  - [ ] Official or verified base image
  - [ ] Base image up to date
  - [ ] Minimal base image preferred

- [ ] **Runtime Security**
  - [ ] Non-root user configured
  - [ ] Read-only root filesystem where possible
  - [ ] Resource limits defined
  - [ ] Security context configured

- [ ] **Secrets Management**
  - [ ] No secrets in image
  - [ ] Environment variables for secrets
  - [ ] Secret Manager integration documented
  - [ ] Workload Identity configured (GKE)

### 4.3 Network Security

- [ ] **TLS Configuration**
  - [ ] TLS enabled by default
  - [ ] Certificate management documented
  - [ ] mTLS support documented (if applicable)

- [ ] **Network Policies**
  - [ ] VPC configuration documented
  - [ ] Firewall rules documented
  - [ ] Private cluster support (GKE)
  - [ ] Network policies defined (GKE)

### 4.4 IAM and Access

- [ ] **Service Accounts**
  - [ ] Dedicated service account
  - [ ] Minimal required permissions
  - [ ] Workload Identity configured

- [ ] **Access Control**
  - [ ] IAM roles documented
  - [ ] Least privilege applied
  - [ ] Audit logging enabled

---

## 5. Business Requirements

### 5.1 Publisher Information

- [ ] **Google Cloud Partner**
  - [ ] Partner account created
  - [ ] Publisher ID verified
  - [ ] Billing information current

- [ ] **Company Information**
  - [ ] Company name verified
  - [ ] Website URL current
  - [ ] Support email monitored
  - [ ] Privacy policy published

### 5.2 Pricing

- [ ] **Pricing Model**
  - [ ] Pricing strategy defined
  - [ ] Free tier documented (if applicable)
  - [ ] Usage-based pricing defined (if applicable)
  - [ ] Support tiers documented

- [ ] **Billing Integration**
  - [ ] Marketplace billing configured
  - [ ] Metering configured (if usage-based)
  - [ ] Billing reports configured

### 5.3 Support

- [ ] **Support Channels**
  - [ ] Email support configured
  - [ ] Issue tracker linked
  - [ ] Documentation URL current
  - [ ] Response time commitments defined

- [ ] **SLA**
  - [ ] Uptime commitment defined
  - [ ] Support response times defined
  - [ ] Credit policy for outages defined

### 5.4 Compliance

- [ ] **Legal Requirements**
  - [ ] Terms of service published
  - [ ] Privacy policy published
  - [ ] Data processing agreement available
  - [ ] Compliance documentation (SOC2, ISO, etc.)

- [ ] **Data Handling**
  - [ ] Data location documented
  - [ ] Data retention policy defined
  - [ ] Data deletion process documented
  - [ ] Export process documented

---

## 6. Testing Requirements

### 6.1 Pre-Submission Testing

- [ ] **Deployment Testing**
  - [ ] Cloud Run deployment successful
  - [ ] GKE deployment successful
  - [ ] Compute Engine deployment successful (if applicable)
  - [ ] All deployments produce healthy state

- [ ] **Functional Testing**
  - [ ] Smoke tests pass
  - [ ] Integration tests pass
  - [ ] Health checks respond correctly
  - [ ] Sample workflows complete successfully

### 6.2 Cleanup Testing

- [ ] **Resource Cleanup**
  - [ ] `terraform destroy` completes successfully
  - [ ] All resources removed
  - [ ] No orphaned resources
  - [ ] No billing after cleanup

---

## 7. Marketplace Submission Process

### 7.1 Pre-Submission Checklist Validation

Run the automated validation:

```bash
cd /Users/sac/erlmcp/marketplace/gcp

# Run full validation suite
bash scripts/run-marketplace-validation.sh \
  --project $PROJECT_ID \
  --run-deployment-tests

# Verify all checks pass
```

### 7.2 Create Submission Package

```bash
# Create deployment package
mkdir -p submission-package

# Copy required files
cp -r marketplace-schema submission-package/
cp -r terraform submission-package/
cp -r helm submission-package/
cp -r docs submission-package/
cp README.md submission-package/

# Generate checksums
find submission-package -type f -exec sha256sum {} \; > submission-package/checksums.txt

# Create archive
tar -czf erlmcp-marketplace-3.0.0.tar.gz submission-package/
```

### 7.3 Submit to Marketplace

1. **Navigate to Partner Portal**
   - Go to https://console.cloud.google.com/partner
   - Select your publisher account

2. **Create New Product**
   - Click "Add Product"
   - Select "Solution" type

3. **Upload Package**
   - Upload the submission archive
   - Fill in required metadata
   - Upload marketing assets

4. **Submit for Review**
   - Review all information
   - Click "Submit for Review"

5. **Monitor Review Status**
   ```bash
   # Check submission status
   gcloud marketplace solutions list \
     --project=$PROJECT_ID \
     --filter="name=erlmcp"
   ```

---

## 8. Post-Submission Activities

### 8.1 Review Monitoring

| Day | Activity |
|-----|----------|
| Day 1 | Verify submission received |
| Day 3 | Check for initial feedback |
| Day 5 | Check review status |
| Day 7 | Address any feedback |
| Day 10 | Final review check |
| Day 14 | Expected approval |

### 8.2 Common Feedback Issues

Be prepared to address:

- **Schema Issues**
  - Missing required fields
  - Invalid enum values
  - Type mismatches

- **Documentation Issues**
  - Incomplete instructions
  - Broken links
  - Missing examples

- **Security Issues**
  - Vulnerabilities found
  - Secrets detected
  - Permission issues

- **Deployment Issues**
  - Failed deployments
  - Timeout issues
  - Configuration problems

---

## 9. Approval Criteria

The submission will be approved when:

- [ ] All technical requirements met
- [ ] All security requirements met
- [ ] All documentation complete
- [ ] All deployment tests pass
- [ ] Marketplace review passes
- [ ] Partner account in good standing

---

**Checklist Completion**

- **Total Items:** 73
- **Completed:** 0
- **Remaining:** 73
- **Ready for Submission:** No

---

**Next Steps:**

1. Complete all checklist items
2. Run validation suite
3. Create submission package
4. Submit to Marketplace
5. Monitor review status
