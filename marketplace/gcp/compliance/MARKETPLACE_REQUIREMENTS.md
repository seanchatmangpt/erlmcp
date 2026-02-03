# Google Cloud Marketplace Requirements for erlmcp

## Overview

This document outlines the technical requirements for listing erlmcp on Google Cloud Marketplace.

## Marketplace Technical Requirements

### 1. Package Format

- **Format**: Terraform modules or Deployment Manager templates
- **Structure**: Modular with reusable components
- **Compatibility**: GKE, Cloud Run, and Compute Engine support

### 2. Application Definition (application.yaml)

Required fields:
- `apiVersion`: marketplace.cloud.google.com/v1
- `kind`: Application
- `metadata.name`: Unique identifier
- `metadata.displayName`: Human-readable name
- `metadata.description`: Detailed description (markdown supported)
- `spec.version`: SemVer format
- `spec.runtimePolicy.minDeploymentDuration`: Minimum deployment time
- `spec.inputSchema`: JSON Schema for input validation

### 3. Input Schema (schema.yaml)

Required components:
- `$schema`: JSON Schema version
- `title`: Schema title
- `type`: "object"
- `required`: List of required fields
- `properties`: Field definitions with:
  - `type`: Data type
  - `title`: Display name
  - `description`: Help text
  - `enum`: Allowed values (for dropdowns)
  - `default`: Default value
  - `x-google-property`: GCP-specific UI hints

### 4. Deployment Package

#### File Structure
```
deployment-package/
├── application.yaml          # Application definition
├── schema.yaml               # Input schema
├── parameters.yaml           # Parameter mappings
├── terraform/                # Terraform modules
│   ├── modules/              # Reusable modules
│   │   ├── gke/
│   │   ├── cloud-run/
│   │   └── compute-engine/
│   └── examples/             # Example deployments
├── README.md                 # Deployment guide
└── LICENSE                   # License information
```

#### Terraform Requirements
- Use Terraform >= 1.5.0
- Pin provider versions
- Use remote state (GCS backend)
- Output required values (service_url, health_check_url)
- Implement `terraform destroy` support

### 5. Container Image Requirements

#### Image Specifications
- **Base**: Alpine Linux or Distroless
- **Size**: < 500MB compressed
- **Architecture**: linux/amd64, linux/arm64
- **Registry**: Artifact Registry (required for Marketplace)

#### Image Labels
```dockerfile
LABEL org.opencontainers.image.title="erlmcp"
LABEL org.opencontainers.image.description="Erlang/OTP MCP Server"
LABEL org.opencontainers.image.version="3.0.0"
LABEL org.opencontainers.image.vendor="banyan-platform"
LABEL org.opencontainers.image.licenses="Apache-2.0"
```

#### Security Scanning
- No CVEs with severity >= HIGH
- Non-root user required
- Read-only root filesystem recommended
- Minimal attack surface

### 6. Required Permissions

#### APIs to Enable
```bash
gcloud services enable \
  compute.googleapis.com \
  container.googleapis.com \
  artifactregistry.googleapis.com \
  secretmanager.googleapis.com \
  run.googleapis.com \
  cloudbuild.googleapis.com \
  monitoring.googleapis.com \
  logging.googleapis.com
```

#### IAM Roles Required
- `roles/container.developer`
- `roles/iam.serviceAccountUser`
- `roles/resourcemanager.projectIamAdmin`
- `roles/compute.admin`
- `roles/secretmanager.admin`

### 7. Health Check Requirements

#### Endpoint Requirements
- **Path**: `/health` (required)
- **Method**: GET
- **Response**: `{"status": "ok"}` (200 OK)
- **Timeout**: 5 seconds
- **Frequency**: Every 30 seconds

#### Additional Endpoints
- `/ready`: Readiness check
- `/metrics`: Prometheus metrics (port 9090)
- `/`: API endpoint

### 8. Logging Requirements

#### Log Format (Cloud Logging)
```json
{
  "severity": "INFO",
  "timestamp": "2024-01-01T00:00:00Z",
  "service": "erlmcp",
  "message": "Application started",
  "labels": {
    "app": "erlmcp",
    "version": "3.0.0"
  }
}
```

#### Required Log Fields
- `severity`: DEBUG, INFO, WARNING, ERROR, CRITICAL
- `timestamp`: ISO 8601 format
- `service`: Service name
- `message`: Log message

### 9. Monitoring Requirements

#### Required Metrics
- Request count
- Request latency (p50, p95, p99)
- Error rate
- Active connections
- Memory usage
- CPU usage

#### Cloud Monitoring Integration
```yaml
custom.googleapis.com/erlmcp/http/requests: CUMULATIVE, INT64
custom.googleapis.com/erlmcp/http/latency: GAUGE, DOUBLE
custom.googleapis.com/erlmcp/connections/active: GAUGE, INT64
```

### 10. SLA Requirements

#### SLA Definitions
- **Cloud Run**: 99.9% (no SLA provided)
- **GKE Regional**: 99.95% (monthly uptime)
- **GKE Multi-Region**: 99.99% (monthly uptime)
- **Compute Engine**: 99.9% (monthly uptime)

#### SLA Exclusions
- Planned maintenance
- Customer actions
- Third-party dependencies
- Force majeure events

### 11. Data Residency

#### Data Storage Locations
- **Metadata**: Stored in deployment region
- **Logs**: Cloud Logging (regional)
- **Metrics**: Cloud Monitoring (global)
- **User Data**: Customer-controlled via Secret Manager

### 12. Security Compliance

#### Required Security Controls
- TLS 1.3 for data in transit
- AES-256 for data at rest (GCP default)
- Secret Manager for sensitive data
- IAM least privilege access
- Network policies (zero-trust)
- Regular security updates

### 13. Billing Integration

#### Billing Model
- **Pay-as-you-go**: Based on GCP resource usage
- **No additional fees**: Marketplace listing is free
- **Support**: Included with GCP Support plans

### 14. Documentation Requirements

#### Required Documents
- README.md: Quick start guide
- ARCHITECTURE.md: System architecture
- SECURITY.md: Security considerations
- TROUBLESHOOTING.md: Common issues
- CHANGELOG.md: Version history

#### Documentation Standards
- Markdown format
- Screenshots for UI steps
- Code examples for CLI usage
- Troubleshooting section
- Contact information

### 15. Testing Requirements

#### Pre-Marketplace Testing
- Deploy in test project
- Verify all deployment options
- Test health checks
- Verify observability integration
- Test scaling behavior
- Validate security controls

#### Automated Tests
```bash
# Terraform validation
terraform fmt -check
terraform init
terraform validate
terraform plan

# Image scanning
gcloud artifacts docker images scan \
  us-central1-docker.pkg.dev/project/erlmcp/erlmcp:3.0.0 \
  --format='value(scanResults.securityResults[0].severity)'

# Deployment test
terraform apply
kubectl rollout status deployment/erlmcp
curl http://$(kubectl get svc erlmcp -o jsonpath='{.status.loadBalancer.ingress[0].ip}')/health
```

### 16. Validation Checklist

Before submitting to Marketplace:
- [ ] Application.yaml is valid
- [ ] Schema.yaml passes validation
- [ ] Terraform modules apply successfully
- [ ] Container image passes security scan
- [ ] Health check endpoint responds
- [ ] Metrics are exported
- [ ] Logs are formatted correctly
- [ ] Documentation is complete
- [ ] Support contact is configured
- [ ] Billing integration works

### 17. Submission Process

1. **Prepare Package**: Create deployment package
2. **Self-Test**: Deploy in test project
3. **Submit**: Upload via Partner Portal
4. **Review**: Google reviews submission
5. **Feedback**: Address any issues
6. **Launch**: Go live on Marketplace

### 18. Post-Launch Requirements

- Monitor deployment success rate
- Track usage metrics
- Respond to support requests
- Update documentation
- Release new versions

## References

- [Google Cloud Marketplace Documentation](https://cloud.google.com/marketplace)
- [Producer Portal](https://console.cloud.google.com/producer-portal)
- [Terraform Provider Documentation](https://registry.terraform.io/providers/hashicorp/google/latest/docs)
- [Artifact Registry](https://cloud.google.com/artifact-registry)
