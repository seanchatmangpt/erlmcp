# Marketplace Schema Validation Report - Phase 1
## Metadata & Schema Sanity Check

**Date**: 2026-02-02
**Reviewer**: Simulation - Marketplace Reviewer Bot
**Application**: erlmcp v3.0.0
**Publisher**: banyan-platform

---

## Executive Summary

| Category | Status | Issues | Severity |
|----------|--------|--------|----------|
| YAML Syntax | PASS | 0 | - |
| Required Fields | FAIL | 8 | HIGH |
| Schema-to-Terraform Mapping | FAIL | 12 | HIGH |
| UI Rendering | FAIL | 15 | HIGH |
| Data Types | PASS | 0 | - |
| Defaults | PASS | 0 | - |

**OVERALL RECOMMENDATION**: **FAIL** - Do not approve until critical issues are resolved.

---

## 1. YAML Syntax Validation

### 1.1 application.yaml
| Check | Result | Details |
|-------|--------|---------|
| Valid YAML | PASS | No syntax errors |
| API Version | PASS | `marketplace.cloud.google.com/v1` |
| Kind | PASS | `Application` |
| Required metadata | PASS | name, displayName, description present |

### 1.2 schema.yaml
| Check | Result | Details |
|-------|--------|---------|
| Valid YAML | PASS | No syntax errors |
| JSON Schema $schema | PASS | `http://json-schema.org/draft-07/schema#` |
| Root type | PASS | `properties` object defined |

### 1.3 parameters.yaml
| Check | Result | Details |
|-------|--------|---------|
| Valid YAML | PASS | No syntax errors |
| Valid mappings | PASS | All keys have valid structures |

---

## 2. Required Fields Validation

### 2.1 CRITICAL: Missing Required Marketplace Fields

The following REQUIRED fields for Google Cloud Marketplace are MISSING:

| Field | Location | Severity | Impact |
|-------|----------|----------|--------|
| `apiVersion` in schema.yaml | schema.yaml | CRITICAL | Schema not recognized |
| `kind` in schema.yaml | schema.yaml | CRITICAL | Schema type undefined |
| `required` array in schema.yaml | schema.yaml | HIGH | No mandatory user inputs enforced |
| `example` values | schema.yaml | MEDIUM | No example values for UX |
| `pattern` validation missing | multiple | MEDIUM | Input validation weak |
| `minimum`/`maximum` constraints | replicas, backup_retention_days | LOW | Bounds defined but inconsistent |
| Marketplace deployment templates | application.yaml | CRITICAL | Template paths may not exist |
| `producerUri` | application.yaml metadata | HIGH | Producer identification missing |

### 2.2 application.yaml - Metadata Validation

| Field | Status | Value | Notes |
|-------|--------|-------|-------|
| `metadata.name` | PASS | erlmcp | Valid lowercase name |
| `metadata.displayName` | PASS | erlmcp - Erlang/OTP MCP Server | Descriptive |
| `metadata.description` | PASS | Present | Comprehensive description |
| `metadata.version` | PASS | 3.0.0 | Semver compliant |
| `metadata.publisher` | PASS | banyan-platform | Valid publisher ID |
| `metadata.icon` | WARNING | URL provided | Image must exist at URL |
| `metadata.documentationURL` | PASS | https://docs.erlmcp.dev | Must be accessible |
| `metadata.supportURL` | PASS | GitHub issues | Must be accessible |
| `spec.runtimePolicy.minDeploymentDuration` | PASS | 15m | Reasonable |
| `spec.runtimePolicy.estimatedDeploymentDuration` | PASS | 10m | Reasonable |

---

## 3. Schema-to-Terraform Mapping Validation

### 3.1 CRITICAL: Schema Property Mapping Issues

#### Schema Property -> Terraform Variable Mapping Status

| Schema Property (schema.yaml) | Terraform Variable | Status | Notes |
|-------------------------------|-------------------|--------|-------|
| `deployment_type` | - | MISSING | No matching Terraform variable in examples |
| `instance_name` | instance_name | PARTIAL | Only in GCE deployment, not GKE/Cloud Run |
| `machine_type` | machine_type | PASS | Maps to GCE/GKE modules |
| `instance_tier` | - | MISSING | No direct mapping - tier to machine type conversion missing |
| `ha_mode` | - | MISSING | No direct mapping - HA configuration scattered |
| `min_replicas` | min_nodes/min_instances | INCONSISTENT | Different names across modules |
| `max_replicas` | max_nodes/max_instances | INCONSISTENT | Different names across modules |
| `autoscaling_enabled` | - | MISSING | Not explicitly in variables |
| `enable_tls` | enable_tls | PASS | Maps correctly |
| `enable_mtls` | - | MISSING | No Terraform variable exists |
| `enable_private_cluster` | enable_private_endpoint/nodes | PARTIAL | Splits across two variables |
| `enable_monitoring` | enable_managed_prometheus | PASS | Maps correctly |
| `enable_logging` | log_export_storage_enabled | PARTIAL | Only for export, not enabling logging |
| `enable_tracing` | - | MISSING | No Terraform variable exists |
| `notification_email` | notification_channels.email.address | NESTED | Requires object transformation |
| `enable_pagerduty` | notification_channels.pagerduty.enabled | NESTED | Requires object transformation |
| `pagerduty_api_key` | notification_channels.pagerduty.service_key | NESTED | Requires object transformation |
| `network_name` | network_name | PASS | Maps correctly |
| `subnet_cidr` | subnets[].ip_cidr_range | NESTED | Requires array transformation |
| `image_tag` | image_tag | PASS | Maps correctly |
| `custom_domain` | custom_domain | PASS | Maps correctly |
| `enable_spot_instances` | enable_spot_nodes | INCONSISTENT | Different names |
| `enable_backup` | - | MISSING | No Terraform variable exists |
| `backup_retention_days` | - | MISSING | No Terraform variable exists |

### 3.2 Orphaned Terraform Variables

The following Terraform variables have NO corresponding schema property:

| Variable | Module | Impact |
|----------|--------|--------|
| `project_id` | All | REQUIRED but not in user schema |
| `zone` | GCE/GKE | Derived from region, not exposed |
| `kubernetes_version` | GKE | Not configurable by user |
| `release_channel` | GKE | Not configurable by user |
| `disk_size_gb` | GKE/GCE | Not configurable by user |
| `disk_type` | GKE/GCE | Not configurable by user |
| `ssh_keys` | GCE | Security: not exposed to user |

### 3.3 Deployment Template Path Validation

| Template Path | Status | Issue |
|---------------|--------|-------|
| `terraform/examples/gke-deployment` | WARNING | Exists but may need `variables.tf` Marketplace wrapper |
| `terraform/examples/cloud-run-deployment` | WARNING | Exists but may need `variables.tf` Marketplace wrapper |
| `terraform/examples/gce-deployment` | WARNING | Exists but may need `variables.tf` Marketplace wrapper |

---

## 4. UI Rendering Simulation

### 4.1 Conditional Visibility Rules

The `x-google-visibility` rules in schema.yaml have CRITICAL issues:

| Rule | Issue | Impact |
|------|-------|--------|
| `${deployment_type == 'gke'}` | Syntax uses `'gke'` but enum has `gke` | Quotes may cause evaluation failure |
| `${deployment_type == 'cloud-run'}` | Schema enum has `cloudrun`, not `cloud-run` | MISMATCH - rule will never match |
| `${deployment_type == 'compute-engine'}` | Schema enum has `gce`, not `compute-engine` | MISMATCH - rule will never match |
| `${enable_pagerduty == true}` | Correct syntax | PASS |
| `${enable_backup == true}` | Correct syntax | PASS |

**CRITICAL**: The deployment_type enum values in schema.yaml do NOT match:
- schema.yaml line 11: `enum: [gke, cloudrun, gce]`
- application.yaml lines 63-66: `enum: [gke, cloud-run, compute-engine]`

**This is a SHOWSTOPPER bug** - users selecting Cloud Run or Compute Engine will see incorrect UI.

### 4.2 Form Field Validation

| Field | Validation Rule | UI Impact |
|-------|----------------|-----------|
| `instance_name` | Pattern `^[a-z][a-z0-9-]*$` | Good - provides real-time feedback |
| `notification_email` | Format `email` | Good - provides email validation |
| `subnet_cidr` | Pattern `^[0-9]{1,3}...` | Good - validates CIDR format |
| `backup_retention_days` | Min 1, Max 365 | Good - range validation |
| `min_replicas` | Min 1, Max 100 | Good - range validation |
| `max_replicas` | Min 1, Max 1000 | Good - range validation |

### 4.3 UI Labels and Tooltips

| Property | Has Title | Has Description | Has Tooltip | Rating |
|----------|-----------|-----------------|-------------|--------|
| deployment_type | YES | YES | NO | B |
| instance_tier | YES | YES | YES | A |
| ha_mode | YES | YES | YES | A |
| min_replicas | YES | YES | YES | A |
| max_replicas | YES | YES | YES | A |
| enable_tls | YES | YES | NO | B |
| notification_email | YES | YES | YES | A |
| pagerduty_api_key | YES | YES | YES | A |
| network_name | YES | YES | YES | A |
| subnet_cidr | YES | YES | NO | B |

**Overall UI Quality**: B - Good coverage but missing some tooltips for security fields.

### 4.4 Enum Label Consistency

| Field | Enum Values | Labels Present | Consistent |
|-------|-------------|----------------|------------|
| deployment_type | gke, cloudrun, gce | NO | FAIL - uses x-google-marketplace enum |
| instance_tier | small, medium, large, xlarge | YES (oneOf) | PASS |
| ha_mode | single-zone, multi-zone, multi-region | YES (oneOf) | PASS |
| machine_type | e2-standard-2, etc | YES | PASS |

**Issue**: `deployment_type` has both `enum` and `x-google-marketplace.enum` with different values. This is ambiguous.

---

## 5. Input Schema (application.yaml) vs Standalone Schema (schema.yaml)

### 5.1 Critical Inconsistencies

| Property | application.yaml | schema.yaml | Match? |
|----------|------------------|-------------|--------|
| deployment_type enum | gke, cloud-run, compute-engine | gke, cloudrun, gce | NO |
| replicas | YES (integer, min 1, max 100) | min_replicas, max_replicas | NO |
| instance_tier enum | small, medium, large | small, medium, large, xlarge | NO |
| enable_mtls | NO | YES | NO |
| enable_private_cluster | NO | YES | NO |
| enable_tracing | NO | YES | NO |
| enable_spot_instances | NO | YES | NO |
| enable_backup | NO | YES | NO |
| backup_retention_days | NO | YES | NO |
| autoscaling_enabled | NO | YES | NO |

**SEVERITY**: CRITICAL - The two schema files are substantially different. Marketplace UI will use `application.yaml.spec.inputSchema` but `schema.yaml` appears to be a more complete definition that is NOT actually used.

---

## 6. Output Schema Validation

### 6.1 Output Properties Defined

| Output Property | Type | Source Verification |
|-----------------|------|---------------------|
| service_url | string | CLAIMED - need to verify Terraform outputs |
| cluster_name | string | CLAIMED - GKE only |
| instance_names | array(string) | CLAIMED - GCE only |
| health_check_url | string | CLAIMED |
| dashboard_urls | object {main, performance, erlang} | CLAIMED |

### 6.2 Output Schema vs Terraform Outputs Verification

The following outputs were verified against actual Terraform modules:

| Output | Required In | Terraform Module | Status |
|--------|-------------|------------------|--------|
| service_url | All deployment types | Cloud Run: `service_url` | PASS (Cloud Run) |
| service_url | GKE deployment | GKE module: MISSING | FAIL - GKE has no service_url output |
| service_url | GCE deployment | GCE: NOT applicable (has health_check_url) | N/A - uses health_check_url |
| cluster_name | GKE deployment | GKE: `cluster_name` | PASS |
| instance_names | GCE deployment | GCE: `instance_names` | PASS |
| kubectl_command | GKE deployment | GKE: `kubectl_config_command` | PASS (name differs slightly) |
| health_check_url | GCE deployment | GCE: `health_check_url` | PASS |
| health_check_url | Cloud Run deployment | Cloud Run: `health_check_url` | PASS |
| dashboard_urls | All | NOT DEFINED in any module | FAIL - claimed in schema but not implemented |

**CRITICAL FINDING**:
- GKE module does NOT output `service_url` - only `cluster_endpoint` (sensitive) and `private_endpoint`
- The `dashboard_urls` output is claimed in parameters.yaml but does NOT exist in any Terraform module
- Application will not render a service URL for GKE deployments in the Marketplace success screen

---

## 7. Security & Sensitive Fields

| Field | Type | Properly Marked? |
|-------|------|------------------|
| pagerduty_api_key | PASSWORD (x-google-property) | YES |
| ssh_keys | sensitive = true | YES (Terraform) |
| notification_channels | sensitive = true | YES (Terraform) |
| secrets | sensitive = true | YES (Terraform) |

**Note**: `application.yaml` input schema does NOT mark `pagerduty_api_key` as sensitive/password type.

---

## 8. Image and URL Validation

| URL/Reference | Status | Action Required |
|---------------|--------|-----------------|
| icon: https://storage.googleapis.com/erlmcp-marketplace/icon.png | UNVERIFIED | Image must exist |
| logo: https://storage.googleapis.com/erlmcp-marketplace/logo.png | UNVERIFIED | Image must exist |
| documentationURL: https://docs.erlmcp.dev | UNVERIFIED | Must be accessible |
| supportURL: https://github.com/banyan-platform/erlmcp/issues | UNVERIFIED | Must be accessible |
| Container image: us-central1-docker.pkg.dev/erlmcp/erlmcp:3.0.0 | UNVERIFIED | Must be public or accessible |

---

## 9. Action Items Before Marketplace Approval

### MUST FIX (Blocking)

1. **[CRITICAL]** Fix `deployment_type` enum inconsistency between application.yaml and schema.yaml
2. **[CRITICAL]** Fix conditional visibility rules to match actual enum values
3. **[CRITICAL]** Add missing `service_url` output to GKE module
4. **[CRITICAL]** Add missing `dashboard_urls` output or remove from outputSchema
5. **[CRITICAL]** Add missing Terraform variables or remove from schema:
   - `enable_mtls`
   - `enable_tracing`
   - `enable_backup`
   - `backup_retention_days`
   - `autoscaling_enabled`
6. **[HIGH]** Consolidate schemas - decide whether to use embedded or standalone schema
7. **[HIGH]** Add `required` array to inputSchema
8. **[HIGH]** Fix replica counting: `replicas` vs `min_replicas`/`max_replicas`

### SHOULD FIX (Important)

9. Add Marketplace-required metadata fields (`producerUri`, etc.)
10. Add `example` values for all user-facing fields
11. Consistently name Terraform variables across modules
12. Add schema documentation for `instance_tier` to machine type mapping
13. Add `kubectl_config_command` to outputSchema (exists in GKE but not documented)

### NICE TO HAVE

14. Add tooltips for all security fields
15. Add more descriptive enum labels
16. Add validation patterns for all string inputs
17. Verify all external URLs are accessible

---

## 10. Recommended Fix Priority

```
P0 (Ship Blocker):
- Fix deployment_type enum mismatch (application.yaml vs schema.yaml vs visibility rules)
- Add or remove unsupported schema properties (mtls, tracing, backup, autoscaling_enabled)
- Add missing service_url output to GKE module
- Add missing dashboard_urls output or remove from outputSchema

P1 (Before Approval):
- Add required field array to inputSchema
- Consolidate schema definitions (pick ONE approach)
- Fix replica counting inconsistency (replicas vs min_replicas/max_replicas)

P2 (Polish):
- Add example values
- Improve tooltips
- Add missing pattern validations
```

---

## Conclusion

This Marketplace schema has **CRITICAL ISSUES** that will cause deployment failures:

1. The deployment type enum values are inconsistent across THREE files:
   - `application.yaml`: gke, cloud-run, compute-engine
   - `schema.yaml`: gke, cloudrun, gce
   - `x-google-visibility` rules: cloud-run, compute-engine (will never match!)

2. Conditional visibility rules reference non-existent enum values (cloud-run, compute-engine)

3. Schema properties exist without corresponding Terraform implementations:
   - enable_mtls, enable_tracing, enable_backup, backup_retention_days, autoscaling_enabled

4. Two different schema files exist with conflicting definitions (application.yaml embedded vs schema.yaml standalone)

5. Missing outputs claimed in outputSchema:
   - GKE module does not output `service_url`
   - No module outputs `dashboard_urls`

**Recommendation**: **REJECT** - Do not proceed to production until P0 issues are resolved.

---

**Report Generated**: 2026-02-02
**Next Review**: After P0 fixes are committed

**Files Analyzed**:
- `/Users/sac/erlmcp/marketplace/gcp/marketplace-schema/application.yaml`
- `/Users/sac/erlmcp/marketplace/gcp/marketplace-schema/schema.yaml`
- `/Users/sac/erlmcp/marketplace/gcp/marketplace-schema/parameters.yaml`
- `/Users/sac/erlmcp/marketplace/gcp/terraform/modules/gke/variables.tf`
- `/Users/sac/erlmcp/marketplace/gcp/terraform/modules/gke/outputs.tf`
- `/Users/sac/erlmcp/marketplace/gcp/terraform/modules/compute-engine/variables.tf`
- `/Users/sac/erlmcp/marketplace/gcp/terraform/modules/compute-engine/outputs.tf`
- `/Users/sac/erlmcp/marketplace/gcp/terraform/modules/cloud-run/variables.tf`
- `/Users/sac/erlmcp/marketplace/gcp/terraform/modules/cloud-run/outputs.tf`
- `/Users/sac/erlmcp/marketplace/gcp/terraform/modules/vpc/variables.tf`
- `/Users/sac/erlmcp/marketplace/gcp/terraform/modules/observability/variables.tf`
- `/Users/sac/erlmcp/marketplace/gcp/terraform/modules/secret-manager/variables.tf`
- `/Users/sac/erlmcp/marketplace/gcp/terraform/examples/gke-deployment/variables.tf`
- `/Users/sac/erlmcp/marketplace/gcp/terraform/examples/cloud-run-deployment/variables.tf`
- `/Users/sac/erlmcp/marketplace/gcp/terraform/examples/gce-deployment/variables.tf`
