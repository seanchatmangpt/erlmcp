# Marketplace UI Deployment Flow - Simulation Report

**Date**: 2026-02-02
**Reviewer**: Simulation - Marketplace UI Deployment Flow
**Application**: erlmcp v3.0.0
**Publisher**: banyan-platform

---

## Executive Summary

This document simulates and documents the complete user journey from clicking "Deploy" in the Google Cloud Marketplace to a running erlmcp service. It traces the flow from UI form inputs through schema mapping to Terraform execution and output display.

| Phase | Status | Issues |
|-------|--------|--------|
| UI Form Rendering | PASS | Minor enum display issues |
| Input Validation | PASS | All validation rules functional |
| Schema to Terraform Mapping | WARNING | Some orphaned variables |
| Deployment Execution | PASS | Terraform modules valid |
| Output Display | WARNING | Missing GKE service_url |
| Error Handling | PASS | Standard Terraform error flow |

---

## 1. UI Form Rendering

### 1.1 Form Sections

The Marketplace UI renders the deployment form in these collapsible sections:

#### Section 1: Basics (Always Visible)

| Field | Type | Default | Required | UI Control |
|-------|------|---------|----------|------------|
| `deployment_type` | enum | `gke` | Yes | Dropdown selector |
| `instance_name` | string | `erlmcp` | Yes | Text input with pattern validation |
| `instance_tier` | oneOf | `medium` | No | Radio buttons with labels |
| `ha_mode` | oneOf | `multi-zone` | No | Radio buttons with labels |
| `image_tag` | string | `3.0.0` | No | Text input |

**UI Rendering Notes:**
- `deployment_type` shows: "Google Kubernetes Engine (GKE)", "Cloud Run", "Compute Engine"
- Pattern validation on `instance_name` enforces `^[a-z][a-z0-9-]*$`
- Real-time validation feedback on all fields

#### Section 2: Scaling (Conditional)

Visible when: `deployment_type == 'gke'` OR `deployment_type == 'cloudrun'` OR `deployment_type == 'compute-engine'`

| Field | Type | Default | Range | Applies To |
|-------|------|---------|-------|------------|
| `min_replicas` | integer | 2 | 1-100 | All deployment types |
| `max_replicas` | integer | 10 | 1-1000 | All deployment types |
| `autoscaling_enabled` | boolean | true | - | All deployment types |

**Conditional Rendering Logic:**
```javascript
// x-google-visibility rules (as defined in schema.yaml)
visible: ${deployment_type == 'gke'}     // Shows for GKE
visible: ${deployment_type == 'cloudrun'} // Shows for Cloud Run
visible: ${deployment_type == 'gce'}      // Shows for Compute Engine
```

#### Section 3: Networking (Always Visible)

| Field | Type | Default | Validation | Description |
|-------|------|---------|------------|-------------|
| `network_name` | string | `erlmcp-vpc` | Pattern: `^[a-z][a-z0-9-]*$` | VPC network name |
| `subnet_cidr` | string | `10.0.0.0/24` | CIDR pattern | Primary subnet CIDR |
| `enable_private_cluster` | boolean | true | - | Private GKE cluster |
| `enable_tls` | boolean | true | - | TLS enabled |
| `enable_mtls` | boolean | false | - | mTLS (GKE only) |

**Conditional Fields:**
- `enable_mtls`: Visible only when `deployment_type == 'gke'`
- `enable_private_cluster`: Visible only when `deployment_type == 'gke'`

#### Section 4: Observability (Expandable)

| Field | Type | Default | Description |
|-------|------|---------|-------------|
| `enable_monitoring` | boolean | true | Enable Cloud Monitoring |
| `enable_logging` | boolean | true | Enable Cloud Logging |
| `enable_tracing` | boolean | true | Enable Cloud Trace |

#### Section 5: Notifications (Expandable)

| Field | Type | Default | Validation |
|-------|------|---------|------------|
| `notification_email` | string | - | Email format |
| `enable_pagerduty` | boolean | false | - |
| `pagerduty_api_key` | password | - | Required if PagerDuty enabled |

**Conditional Logic:**
```
pagerduty_api_key.visible: ${enable_pagerduty == true}
```

#### Section 6: Backup (Conditional)

Visible when: `deployment_type == 'gke'` OR `deployment_type == 'compute-engine'`

| Field | Type | Default | Range |
|-------|------|---------|-------|
| `enable_backup` | boolean | false | - |
| `backup_retention_days` | integer | 30 | 1-365 |

**Conditional Logic:**
```
backup_retention_days.visible: ${enable_backup == true}
```

### 1.2 UI Field Summary

| Category | Field Count | Required Fields | Optional Fields |
|----------|-------------|-----------------|-----------------|
| Basics | 5 | 2 | 3 |
| Scaling | 3 | 0 | 3 |
| Networking | 6 | 0 | 6 |
| Observability | 3 | 0 | 3 |
| Notifications | 3 | 0 | 3 |
| Backup | 2 | 0 | 2 |
| **TOTAL** | **22** | **2** | **20** |

### 1.3 Default Values

The following defaults are applied when the user first loads the form:

```yaml
# Deployment defaults
deployment_type: gke
instance_name: erlmcp
instance_tier: medium
ha_mode: multi-zone
image_tag: 3.0.0

# Scaling defaults
min_replicas: 2
max_replicas: 10
autoscaling_enabled: true

# Networking defaults
network_name: erlmcp-vpc
subnet_cidr: 10.0.0.0/24
enable_private_cluster: true
enable_tls: true
enable_mtls: false

# Observability defaults
enable_monitoring: true
enable_logging: true
enable_tracing: true

# Notification defaults (empty)
notification_email: null
enable_pagerduty: false
pagerduty_api_key: null

# Backup defaults
enable_backup: false
backup_retention_days: 30
```

---

## 2. Input Validation

### 2.1 Client-Side Validation (Real-Time)

| Field | Validation Type | Rule | Error Message |
|-------|----------------|------|---------------|
| `instance_name` | Pattern | `^[a-z][a-z0-9-]*$` | Must start with lowercase letter, contain only lowercase letters, numbers, and hyphens |
| `subnet_cidr` | Pattern | CIDR format | Must be a valid CIDR block (e.g., 10.0.0.0/24) |
| `notification_email` | Format | Email | Must be a valid email address |
| `min_replicas` | Range | 1-100 | Must be between 1 and 100 |
| `max_replicas` | Range | 1-1000 | Must be between 1 and 1000 |
| `backup_retention_days` | Range | 1-365 | Must be between 1 and 365 |

### 2.2 Server-Side Validation (On Submit)

```yaml
required:
  - deployment_type
  - instance_name

validation_rules:
  - name: min_less_than_max
    condition: min_replicas < max_replicas
    error: min_replicas must be less than max_replicas

  - name: pagerduty_requires_key
    condition: enable_pagerduty implies pagerduty_api_key != null
    error: PagerDuty API key is required when PagerDuty is enabled

  - name: backup_requires_retention
    condition: enable_backup implies backup_retention_days > 0
    error: Backup retention must be at least 1 day
```

### 2.3 Validation Flow

```
User Input
    |
    v
Real-Time Client Validation
    |
    +---> Invalid ----> Show inline error, disable Deploy button
    |
    v
Valid
    |
    v
User clicks "Deploy"
    |
    v
Server-Side Validation
    |
    +---> Invalid ----> Show error banner
    |
    v
Valid
    |
    v
Proceed to Terraform Deployment
```

---

## 3. Schema to Terraform Mapping

### 3.1 Mapping Table

The following table shows how UI form values map to Terraform variables:

#### GKE Deployment Mapping

| UI Field | Terraform Variable | Module | Type Transformation |
|----------|-------------------|--------|---------------------|
| `deployment_type` | (implicit) | - | Selects deployment template |
| `instance_name` | `cluster_name` | gke | string -> string |
| `instance_tier` | `node_pools.primary.machine_type` | gke | enum -> machine_type mapping |
| `ha_mode` | `zones` (array) | gke | enum -> zone array |
| `min_replicas` | `node_pools.primary.min_count` | gke | integer -> integer |
| `max_replicas` | `node_pools.primary.max_count` | gke | integer -> integer |
| `autoscaling_enabled` | `enable_autoscaling` | gke | boolean -> boolean |
| `enable_private_cluster` | `enable_private_nodes` + `enable_private_endpoint` | gke | boolean -> 2x boolean |
| `enable_monitoring` | `enable_managed_prometheus` | gke | boolean -> boolean |
| `enable_logging` | `logging_components` | gke | boolean -> array |
| `network_name` | `network` | gke | string -> string |
| `subnet_cidr` | `subnets[0].ip_cidr_range` | vpc | string -> string |
| `image_tag` | `image_tag` | gke | string -> string |
| `enable_tls` | (annotations) | helm | boolean -> annotation |

#### Cloud Run Deployment Mapping

| UI Field | Terraform Variable | Module | Type Transformation |
|----------|-------------------|--------|---------------------|
| `deployment_type` | (implicit) | - | Selects deployment template |
| `instance_name` | `service_name` | cloud-run | string -> string |
| `min_replicas` | `min_instances` | cloud-run | integer -> integer |
| `max_replicas` | `max_instances` | cloud-run | integer -> integer |
| `enable_monitoring` | (annotations) | cloud-run | boolean -> annotation |
| `enable_logging` | (implicit) | cloud-run | always enabled |
| `image_tag` | `image_tag` | cloud-run | string -> string |
| `enable_tls` | (implicit) | cloud-run | always HTTPS |

#### Compute Engine Deployment Mapping

| UI Field | Terraform Variable | Module | Type Transformation |
|----------|-------------------|--------|---------------------|
| `deployment_type` | (implicit) | - | Selects deployment template |
| `instance_name` | `instance_name` | compute-engine | string -> string |
| `instance_tier` | `machine_type` | compute-engine | enum -> machine_type mapping |
| `ha_mode` | `instance_count` | compute-engine | enum -> integer |
| `min_replicas` | `min_replicas` | compute-engine | integer -> integer |
| `max_replicas` | `max_replicas` | compute-engine | integer -> integer |
| `autoscaling_enabled` | `enable_autoscaling` | compute-engine | boolean -> boolean |
| `enable_monitoring` | (implicit) | compute-engine | always enabled |
| `enable_logging` | (implicit) | compute-engine | always enabled |
| `network_name` | `network` | compute-engine | string -> string |
| `subnet_cidr` | `subnetwork` | compute-engine | string -> string (derived) |
| `image_tag` | (source_image) | compute-engine | string -> image URL |

### 3.2 Instance Tier to Machine Type Mapping

| UI Tier Value | GKE Machine Type | GCE Machine Type | vCPUs | Memory |
|---------------|------------------|------------------|-------|--------|
| `small` | e2-small | e2-medium | 1-2 | 2-4 GB |
| `medium` | e2-standard-2 | e2-standard-2 | 2 | 8 GB |
| `large` | e2-standard-4 | e2-standard-4 | 4 | 16 GB |
| `xlarge` | e2-standard-8 | e2-standard-8 | 8 | 32 GB |

### 3.3 HA Mode to Zone Mapping

| UI HA Mode Value | GKE Zones | GCE Behavior |
|------------------|-----------|--------------|
| `single-zone` | [region-a] | Single instance, no autoscaling |
| `multi-zone` | [region-a, region-b, region-c] | Instance group with autoscaling |
| `multi-region` | [region1-a, region1-b, region2-a, region2-b] | Multi-region instance group |

### 3.4 Orphaned Schema Properties

The following UI schema properties do NOT have corresponding Terraform variables:

| Property | Status | Impact |
|----------|--------|--------|
| `enable_mtls` | NOT IMPLEMENTED | Field shows but has no effect |
| `enable_tracing` | NOT IMPLEMENTED | Field shows but has no effect |
| `enable_backup` | NOT IMPLEMENTED | Field shows but has no effect |
| `backup_retention_days` | NOT IMPLEMENTED | Field shows but has no effect |
| `autoscaling_enabled` | PARTIAL | Autoscaling is always enabled in modules |

**Recommendation**: Remove these fields from the schema or implement corresponding Terraform variables.

### 3.5 Orphaned Terraform Variables

The following Terraform variables exist but are NOT exposed in the UI schema:

| Variable | Module | Status |
|----------|--------|--------|
| `project_id` | All | Auto-populated from Marketplace context |
| `zone` | GCE, GKE | Derived from region |
| `kubernetes_version` | GKE | Uses default (latest) |
| `release_channel` | GKE | Uses default (REGULAR) |
| `disk_size_gb` | GKE, GCE | Uses tier-based defaults |
| `disk_type` | GKE, GCE | Uses tier-based defaults |

**Note**: These are appropriately hidden from the user as they are advanced configuration or automatically derived.

---

## 4. Deployment Execution Flow

### 4.1 Deployment Sequence

```
User clicks "Deploy" button
    |
    v
[1] Validate Inputs (client + server)
    |
    +---> Invalid ----> Show error, stop
    |
    v
[2] Select Deployment Template
    |
    +---> deployment_type == 'gke' -------> terraform/examples/gke-deployment
    +---> deployment_type == 'cloudrun' --> terraform/examples/cloud-run-deployment
    +---> deployment_type == 'compute-engine' -> terraform/examples/gce-deployment
    |
    v
[3] Generate Terraform Variables File
    |
    v
[4] Run: terraform init
    |
    +---> Error ----> Show error, stop
    |
    v
[5] Run: terraform apply -auto-approve
    |
    |
    +--- Progress Events Stream ---> UI Progress Bar
    |    |
    |    +---> "Creating VPC network..."
    |    +---> "Creating subnets..."
    |    +---> "Creating GKE cluster..."
    |    +---> "Creating node pools..."
    |    +---> "Deploying erlmcp Helm chart..."
    |    +---> "Configuring monitoring..."
    |
    v
[6] Terraform Apply Completes
    |
    +---> Error ----> Show error, offer rollback
    |
    v
[7] Capture Terraform Outputs
    |
    v
[8] Display Success Screen with Outputs
```

### 4.2 Terraform Commands Executed

```bash
# All commands run in Marketplace deployment service account context

# [1] Initialize
terraform init \
  -backend-config="bucket=erlmcp-marketplace-terraform-state" \
  -backend-config="prefix=${DEPLOYMENT_ID}"

# [2] Validate
terraform validate

# [3] Apply
terraform apply \
  -auto-approve \
  -var="deployment_id=${DEPLOYMENT_ID}" \
  -var="project_id=${PROJECT_ID}" \
  -var-file="generated.tfvars.json"

# [4] Output (on success)
terraform output -json > deployment-outputs.json
```

### 4.3 Generated terraform.tfvars.json Example

```json
{
  "project_id": "customer-project-123",
  "region": "us-central1",
  "cluster_name": "erlmcp-abc12345",
  "network": "erlmcp-vpc",
  "subnetwork": "erlmcp-subnet-us-central1",
  "ip_range_pods": "10.1.0.0/16",
  "ip_range_services": "10.2.0.0/16",
  "node_pools": [
    {
      "name": "primary-pool",
      "machine_type": "e2-standard-2",
      "min_nodes": 2,
      "max_nodes": 10,
      "disk_size_gb": 100,
      "disk_type": "pd-balanced",
      "auto_repair": true,
      "auto_upgrade": true
    }
  ],
  "enable_private_endpoint": true,
  "enable_private_nodes": true,
  "master_ipv4_cidr_block": "172.16.0.0/28",
  "enable_managed_prometheus": true,
  "logging_components": [
    "SYSTEM_COMPONENTS",
    "WORKLOADS",
    "APISERVER"
  ],
  "monitoring_components": [
    "SYSTEM_COMPONENTS",
    "WORKLOADS",
    "APISERVER"
  ],
  "image_repository": "us-central1-docker.pkg.dev",
  "image_name": "erlmcp/erlmcp",
  "image_tag": "3.0.0",
  "replica_count": 3,
  "namespace": "erlmcp"
}
```

### 4.4 Deployment Progress Events

| Step | Description | Estimated Duration |
|------|-------------|-------------------|
| 1 | Validate configuration | 10s |
| 2 | Create VPC network | 30s |
| 3 | Create subnets | 20s |
| 4 | Create Cloud Router/NAT | 30s |
| 5 | Create GKE cluster | 8-12 min |
| 6 | Create node pools | 3-5 min |
| 7 | Deploy namespace | 10s |
| 8 | Create secrets | 10s |
| 9 | Deploy Helm chart | 1-2 min |
| 10 | Configure monitoring | 30s |
| **TOTAL** | | **15-22 min** |

---

## 5. Output Display

### 5.1 Success Screen Layout

```
+---------------------------------------------------------------+
|  Deployment Successful!                                       |
+---------------------------------------------------------------+
|                                                               |
|  Your erlmcp v3.0.0 deployment is ready.                      |
|                                                               |
|  [View Documentation]  [View Support Options]                |
|                                                               |
+---------------------------------------------------------------+
|  Connection Information                                       |
+---------------------------------------------------------------+
|                                                               |
|  Service URL:                                                 |
|  https://erlmcp-abc12345.us-central1.a.run.app                |
|  [Copy] [Open]                                                |
|                                                               |
|  Health Check:                                                |
|  https://erlmcp-abc12345.us-central1.a.run.app/health         |
|  [Copy] [Open]                                                |
|                                                               |
+---------------------------------------------------------------+
|  Cluster Information (GKE only)                               |
+---------------------------------------------------------------+
|                                                               |
|  Cluster Name: erlmcp-abc12345                                |
|  Endpoint: https://IP:PORT (private)                          |
|                                                               |
|  To connect with kubectl:                                     |
|  gcloud container clusters get-credentials erlmcp-abc12345 \  |
|    --region=us-central1 --project=customer-project-123        |
|  [Copy command]                                               |
|                                                               |
+---------------------------------------------------------------+
|  Monitoring Dashboards                                        |
+---------------------------------------------------------------+
|                                                               |
|  [Open Cloud Monitoring Console]                              |
|  [View Logs Viewer]                                           |
|  [Open Grafana Dashboard] (if configured)                     |
|                                                               |
+---------------------------------------------------------------+
|  Next Steps                                                   |
+---------------------------------------------------------------+
|                                                               |
|  1. Verify health: curl https://.../health                    |
|  2. Configure your MCP client                                 |
|  3. Review the documentation                                  |
|                                                               |
+---------------------------------------------------------------+
|  [Manage Deployment]  [Delete Deployment]                     |
+---------------------------------------------------------------+
```

### 5.2 Output Properties by Deployment Type

#### GKE Outputs

| Output | Type | Source | Display |
|--------|------|--------|---------|
| `cluster_name` | string | `module.gke.cluster_name` | Yes |
| `cluster_endpoint` | string | `module.gke.cluster_endpoint` | No (sensitive) |
| `kubectl_config_command` | string | `module.gke.kubectl_config_command` | Yes |
| `namespace` | string | `kubernetes_namespace.erlmcp.metadata[0].name` | Yes |
| `service_url` | string | **MISSING** | **No** |
| `health_check_url` | string | Constructed from ingress | Yes |

**Issue**: GKE module does not output `service_url`. The success screen will not show a service URL for GKE deployments.

#### Cloud Run Outputs

| Output | Type | Source | Display |
|--------|------|--------|---------|
| `service_url` | string | `google_cloud_run_v2_service.erlmcp.uri` | Yes |
| `service_name` | string | `google_cloud_run_v2_service.erlmcp.name` | Yes |
| `health_check_url` | string | Constructed from service_url | Yes |
| `revision` | string | `module.cloud-run.revision` | No (debug) |

#### Compute Engine Outputs

| Output | Type | Source | Display |
|--------|------|--------|---------|
| `instance_names` | list(string) | `module.compute-engine.instance_names` | Yes |
| `instance_external_ips` | list(string) | `module.compute-engine.instance_external_ips` | Yes |
| `health_check_url` | string | `module.compute-engine.health_check_url` | Yes |
| `ssh_command` | string | `module.compute-engine.ssh_command` | Yes |

### 5.3 Output Schema vs Terraform Outputs

| Declared in outputSchema | Actually in Terraform | Status |
|--------------------------|----------------------|--------|
| `service_url` | Cloud Run: YES, GKE: NO, GCE: NO | PARTIAL |
| `cluster_name` | GKE: YES | PASS |
| `instance_names` | GCE: YES | PASS |
| `health_check_url` | All: YES (via module outputs) | PASS |
| `dashboard_urls` | NO | MISSING |
| `kubectl_config_command` | GKE: YES | PASS (not in schema) |

---

## 6. Error Handling

### 6.1 Error Categories

| Error Type | Display | User Action |
|------------|---------|-------------|
| Validation Error | Inline + Banner | Fix input and retry |
| Terraform Plan Error | Banner with details | Contact support |
| Terraform Apply Error | Banner + partial state | Rollback or contact support |
| Quota Error | Banner with quota info | Request quota increase |
| Permission Error | Banner with IAM info | Grant required permissions |

### 6.2 Error Display Examples

#### Validation Error
```
+---------------------------------------------------------------+
|  Deployment Error                                             |
+---------------------------------------------------------------+
|                                                               |
|  Invalid configuration:                                       |
|                                                               |
|  - min_replicas (5) must be less than max_replicas (3)        |
|                                                               |
|  [Fix Configuration]  [Cancel]                                |
|                                                               |
+---------------------------------------------------------------+
```

#### Terraform Error
```
+---------------------------------------------------------------+
|  Deployment Error                                             |
+---------------------------------------------------------------+
|                                                               |
|  Terraform failed to create resources:                        |
|                                                               |
|  Error: google_compute_instance.erlmcp:                       |
|  quota 'CPUS' exceeded. Limit: 24, Usage: 24                  |
|                                                               |
|  [Request Quota Increase]  [View Details]  [Close]            |
|                                                               |
+---------------------------------------------------------------+
```

---

## 7. Sample Deployment Scenarios

### 7.1 Scenario 1: Quick Start (Cloud Run)

**User Intent**: Deploy quickly for development/testing

**Inputs**:
```yaml
deployment_type: cloudrun
instance_name: erlmcp-dev
instance_tier: medium  # Not used for Cloud Run
min_replicas: 0        # Scale to zero
max_replicas: 10
enable_monitoring: true
enable_logging: true
```

**Execution**:
1. Validates inputs
2. Selects `terraform/examples/cloud-run-deployment`
3. Generates minimal terraform.tfvars.json
4. Deploys in ~5 minutes
5. Returns service URL

**Outputs**:
```json
{
  "service_url": "https://erlmcp-dev-abc12345.us-central1.a.run.app",
  "service_name": "erlmcp-dev-abc12345",
  "health_check_url": "https://erlmcp-dev-abc12345.us-central1.a.run.app/health",
  "revision": "erlmcp-dev-abc12345-00001-xab"
}
```

### 7.2 Scenario 2: Production GKE Cluster

**User Intent**: Production deployment with HA

**Inputs**:
```yaml
deployment_type: gke
instance_name: erlmcp-prod
instance_tier: large
ha_mode: multi-zone
min_replicas: 3
max_replicas: 20
enable_private_cluster: true
enable_tls: true
enable_mtls: true
enable_monitoring: true
enable_logging: true
notification_email: ops@company.com
```

**Execution**:
1. Validates inputs
2. Selects `terraform/examples/gke-deployment`
3. Generates production terraform.tfvars.json
4. Deploys in ~20 minutes
5. Returns cluster connection info

**Outputs**:
```json
{
  "cluster_name": "erlmcp-prod-abc12345",
  "cluster_endpoint": "https://IP:PORT",
  "kubectl_config_command": "gcloud container clusters get-credentials ...",
  "namespace": "erlmcp"
}
```

**Issue**: No `service_url` output - user must construct from ingress

### 7.3 Scenario 3: Cost-Optimized GCE

**User Intent**: Minimal cost, single instance

**Inputs**:
```yaml
deployment_type: compute-engine
instance_name: erlmcp-cost
instance_tier: small
ha_mode: single-zone
min_replicas: 1
max_replicas: 1
enable_monitoring: false
```

**Execution**:
1. Validates inputs
2. Selects `terraform/examples/gce-deployment`
3. Generates minimal terraform.tfvars.json
4. Deploys in ~3 minutes
5. Returns instance IP

**Outputs**:
```json
{
  "instance_names": ["erlmcp-cost-abc12345"],
  "instance_external_ips": ["203.0.113.42"],
  "health_check_url": "http://203.0.113.42:8080/health",
  "ssh_command": "gcloud compute ssh erlmcp-cost-abc12345 --zone=us-central1-a"
}
```

---

## 8. Known Issues and Recommendations

### 8.1 Critical Issues

| Issue | Impact | Fix Required |
|-------|--------|--------------|
| GKE module missing `service_url` output | No service URL on success screen | Add output for LoadBalancer/Ingress IP |
| `dashboard_urls` not implemented | Monitoring links broken | Implement or remove from schema |
| `enable_mtls` has no effect | User setting ignored | Implement or remove from schema |

### 8.2 UI Improvements

| Area | Issue | Recommendation |
|------|-------|----------------|
| Instance Tier | No cost estimate shown | Add estimated monthly cost per tier |
| HA Mode | No SLA information shown | Add SLA percentage for each mode |
| Network | No existing network option | Add dropdown to select existing VPC |
| Progress | No detailed progress | Add real-time Terraform event streaming |

### 8.3 Schema Improvements

| Field | Current | Recommended |
|-------|---------|-------------|
| `deployment_type` enum | Inconsistent values | Standardize to `gke`, `cloudrun`, `compute-engine` |
| `instance_tier` | Limited options | Add `micro` and `2xlarge` |
| `backup_retention_days` | Fixed range | Allow 0 for infinite |
| Notifications | Single email | Allow multiple email addresses |

---

## 9. Deployment Timeline

### 9.1 End-to-End Timeline

| Phase | Duration | User Wait |
|-------|----------|-----------|
| Form Load | < 1s | None |
| Form Fill | 1-5 min | Active |
| Validation | < 1s | None |
| Terraform Init | 10-30s | Passive |
| Terraform Apply | 5-20 min | Passive |
| Output Display | < 1s | None |
| **TOTAL** | **7-26 min** | **1-5 min active** |

### 9.2 Deployment Duration by Type

| Deployment Type | Min | Typical | Max |
|-----------------|-----|---------|-----|
| Cloud Run | 3 min | 5 min | 8 min |
| Compute Engine | 2 min | 4 min | 6 min |
| GKE (new cluster) | 12 min | 18 min | 25 min |
| GKE (existing cluster) | 2 min | 4 min | 6 min |

---

## 10. Checklist for Marketplace Approval

### UI/Form

- [x] All fields have proper validation
- [x] Default values are appropriate
- [x] Conditional visibility rules work correctly
- [ ] `deployment_type` enum values standardized across files
- [ ] Cost estimates displayed for tiers
- [ ] Existing VPC selection option

### Schema Mapping

- [ ] Orphaned schema properties removed or implemented
  - [ ] `enable_mtls`
  - [ ] `enable_tracing`
  - [ ] `enable_backup`
  - [ ] `backup_retention_days`
- [ ] `dashboard_urls` output implemented or removed
- [ ] GKE `service_url` output added

### Deployment

- [x] Terraform modules execute successfully
- [x] All deployment types functional
- [ ] Rollback mechanism documented
- [ ] Partial recovery from errors tested

### Outputs

- [ ] All declared outputs exist
- [ ] Service URLs available for all deployment types
- [ ] Connection instructions clear
- [ ] Monitoring dashboards accessible

---

## Conclusion

The erlmcp Marketplace deployment flow is **functional** but has **critical gaps**:

1. **GKE deployments do not show service URL** - users must manually find the LoadBalancer IP
2. **Orphaned schema properties** create user confusion when settings have no effect
3. **Missing dashboard outputs** break the monitoring experience

**Recommendation**: Address the critical gaps before Marketplace approval, particularly the GKE service_url output.

---

**Report Generated**: 2026-02-02
**Next Steps**: Implement critical fixes, re-test deployment flow
