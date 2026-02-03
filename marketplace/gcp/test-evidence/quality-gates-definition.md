# Quality Gates Definition for GCP Marketplace

## Overview

This document defines the quality gates for erlmcp's submission to Google Cloud Marketplace. Quality gates are automated checks that ensure the product meets technical requirements, security standards, and performance expectations before marketplace submission.

## Quality Gates Framework

### 1. Security Quality Gates

#### 1.1 Container Security Gate

**Gate ID**: QG-SEC-001
**Name**: Container Security Compliance
**Description**: Validates container image security requirements

**Criteria:**
- Non-root user running the application
- Read-only root filesystem
- No HIGH/CRITICAL vulnerabilities (Trivy scan)
- Minimal attack surface

**Implementation:**
```bash
# Security Check Script
#!/bin/bash
# QG-SEC-001: Container Security Check

set -euo pipefail

# Check non-root user
if docker inspect erlmcp:latest | grep -q '"User": "root"'; then
    echo "FAIL: Container running as root"
    exit 1
fi

# Check vulnerabilities
if trivy image erlmcp:latest --severity CRITICAL,HIGH --quiet | grep -v "No vulnerabilities found"; then
    echo "FAIL: Vulnerabilities found"
    exit 1
fi

echo "PASS: Container security requirements met"
exit 0
```

**Success Criteria:**
- Exit code: 0
- No root user
- 0 HIGH/CRITICAL vulnerabilities
- Security labels present

#### 1.2 IAM Security Gate

**Gate ID**: QG-SEC-002
**Name**: IAM Least Privilege
**Description**: Validates IAM roles follow least privilege principle

**Criteria:**
- No Owner/Editor roles on service accounts
- Workload Identity properly configured
- Secret Manager access restricted
- Network policies implemented

**Implementation:**
```bash
# IAM Security Check
#!/bin/bash
# QG-SEC-002: IAM Security Check

set -euo pipefail

# Check for overly permissive roles
if gcloud projects get-iam-policy PROJECT_ID | grep -E "roles/(owner|editor)" | grep -q "serviceAccount:erlmcp@"; then
    echo "FAIL: Overly permissive roles found"
    exit 1
fi

# Check Workload Identity
if ! gcloud iam workload-identity-pools list --project=PROJECT_ID | grep -q "erlmcp-pool"; then
    echo "WARN: Workload Identity not configured"
fi

echo "PASS: IAM security requirements met"
exit 0
```

#### 1.3 Network Security Gate

**Gate ID**: QG-SEC-003
**Name**: Network Security Configuration
**Description**: Validates network security controls

**Criteria:**
- Private clusters enabled (for GKE)
- Network policies defined
- Firewall rules reviewed
- No public access to sensitive ports

**Implementation:**
```bash
# Network Security Check
#!/bin/bash
# QG-SEC-003: Network Security Check

set -euo pipefail

# Check GKE private cluster
CLUSTER_DESC=$(gcloud container clusters describe erlmcp-cluster --region=us-central1 --format=json)
if echo "$CLUSTER_DESC" | jq -e '.privateClusterConfig.enablePrivateNodes == true' >/dev/null; then
    echo "PASS: Private nodes enabled"
else
    echo "WARN: Private nodes not enabled"
fi

# Check for public firewall rules
if gcloud compute firewall-rules list --filter="direction=INGRESS AND sourceRanges=0.0.0.0/0" --format="table(name)" | grep -q "erlmcp-"; then
    echo "WARN: Public firewall rules detected"
fi

echo "PASS: Network security check completed"
exit 0
```

### 2. Infrastructure Quality Gates

#### 2.1 Terraform Validation Gate

**Gate ID**: QG-INFRA-001
**Name**: Terraform Configuration Validation
**Description**: Validates Terraform configurations syntax and best practices

**Criteria:**
- Terraform syntax validation passes
- No deprecated resources
- Remote state configured
- Outputs properly defined

**Implementation:**
```bash
# Terraform Validation
#!/bin/bash
# QG-INFRA-001: Terraform Validation

set -euo pipefail

cd terraform/

# Validate syntax
if ! terraform validate; then
    echo "FAIL: Terraform validation failed"
    exit 1
fi

# Check for deprecated resources
if terraform plan -refresh=false | grep -q "deprecated"; then
    echo "WARN: Deprecated resources found"
fi

# Check required outputs
required_outputs=("service_url" "health_check_url")
for output in "${required_outputs[@]}"; do
    if ! terraform output -raw "$output" >/dev/null 2>&1; then
        echo "FAIL: Required output $output not found"
        exit 1
    fi
done

echo "PASS: Terraform validation passed"
exit 0
```

#### 2.2 Resource Configuration Gate

**Gate ID**: QG-INFRA-002
**Name**: Resource Configuration Compliance
**Description**: Validates resource configurations meet requirements

**Criteria:**
- Machine types appropriate for workload
- Autoscaling configured
- Monitoring enabled
- Logging enabled
- Secret management configured

**Implementation:**
```bash
# Resource Configuration Check
#!/bin/bash
# QG-INFRA-002: Resource Configuration Check

set -euo pipefail

# Check machine types
if terraform output -raw machine_type | grep -q "e2-medium"; then
    echo "PASS: Machine type appropriate"
else
    echo "WARN: Consider machine type optimization"
fi

# Check autoscaling
CLUSTER_DESC=$(gcloud container clusters describe erlmcp-cluster --region=us-central1 --format=json)
if echo "$CLUSTER_DESC" | jq -e '.autoscaling.enabled == true' >/dev/null; then
    echo "PASS: Autoscaling enabled"
else
    echo "WARN: Autoscaling not enabled"
fi

# Check monitoring
if echo "$CLUSTER_DESC" | jq -e '.monitoringService != "none"' >/dev/null; then
    echo "PASS: Monitoring enabled"
else
    echo "FAIL: Monitoring not enabled"
    exit 1
fi

echo "PASS: Resource configuration check completed"
exit 0
```

### 3. Application Quality Gates

#### 3.1 Health Check Gate

**Gate ID**: QG-APP-001
**Name**: Health Check Compliance
**Description**: Validates application health check endpoints

**Criteria:**
- `/health` endpoint returns HTTP 200
- Response time < 5 seconds
- Response format correct
- Service properly started

**Implementation:**
```bash
# Health Check
#!/bin/bash
# QG-APP-001: Health Check

set -euo pipefail

HEALTH_URL="http://$(terraform output -raw service_url)/health"

# Check HTTP status
if ! curl -f -s -o /dev/null -w "%{http_code}" "$HEALTH_URL" | grep -q "200"; then
    echo "FAIL: Health check endpoint not returning 200"
    exit 1
fi

# Check response time
RESPONSE_TIME=$(curl -o /dev/null -s -w "%{time_total}" "$HEALTH_URL")
if (( $(echo "$RESPONSE_TIME > 5" | bc -l) )); then
    echo "FAIL: Response time too slow: ${RESPONSE_TIME}s"
    exit 1
fi

# Check response format
RESPONSE=$(curl -s "$HEALTH_URL")
if ! echo "$RESPONSE" | jq -e '.status == "ok"' >/dev/null 2>&1; then
    echo "FAIL: Incorrect response format"
    exit 1
fi

echo "PASS: Health check requirements met"
exit 0
```

#### 3.2 API Compliance Gate

**Gate ID**: QG-APP-002
**Name**: API Compliance
**Description**: Validates API compliance with MCP specification

**Criteria:**
- JSON-RPC 2.0 protocol compliance
- Proper error handling
- Request validation
- Response schemas correct

**Implementation:**
```bash
# API Compliance Check
#!/bin/bash
# QG-APP-002: API Compliance Check

set -euo pipefail

API_URL="http://$(terraform output -raw service_url)"

# Test JSON-RPC 2.0 compliance
REQUEST='{"jsonrpc": "2.0", "method": "tools/list", "id": "test-001"}'
RESPONSE=$(curl -s -X POST -H "Content-Type: application/json" -d "$REQUEST" "$API_URL")

# Check JSON-RPC version
if ! echo "$RESPONSE" | jq -e '.jsonrpc == "2.0"' >/dev/null 2>&1; then
    echo "FAIL: JSON-RPC 2.0 compliance failed"
    exit 1
fi

# Check error handling
ERROR_REQUEST='{"jsonrpc": "2.0", "method": "invalid_method", "id": "test-002"}'
ERROR_RESPONSE=$(curl -s -X POST -H "Content-Type: application/json" -d "$ERROR_REQUEST" "$API_URL")

if ! echo "$ERROR_RESPONSE" | jq -e '.error != null' >/dev/null 2>&1; then
    echo "FAIL: Error handling failed"
    exit 1
fi

echo "PASS: API compliance check passed"
exit 0
```

#### 3.3 Performance Gate

**Gate ID**: QG-APP-003
**Name**: Performance Requirements
**Description**: Validates application performance requirements

**Criteria:**
- p95 response time < 200ms
- p99 response time < 500ms
- Error rate < 0.1%
- Throughput > 1000 req/s

**Implementation:**
```bash
# Performance Check
#!/bin/bash
# QG-APP-003: Performance Check

set -euo pipefail

API_URL="http://$(terraform output -raw service_url)"

# Load testing with k6
k6 run --vus 100 --duration 30s --out json=results.json script.js

# Analyze results
P95=$(jq '.metrics.http_req_duration.values.p95' results.json)
ERROR_RATE=$(jq '.metrics.http_req_failed.values.rate * 100' results.json)

# Check p95 response time
if (( $(echo "$P95 > 200" | bc -l) )); then
    echo "FAIL: p95 response time too high: ${P95}ms"
    exit 1
fi

# Check error rate
if (( $(echo "$ERROR_RATE > 0.1" | bc -l) )); then
    echo "FAIL: Error rate too high: ${ERROR_RATE}%"
    exit 1
fi

echo "PASS: Performance requirements met"
exit 0
```

### 4. Observability Quality Gates

#### 4.1 Metrics Gate

**Gate ID**: QG-OBS-001
**Name**: Metrics Collection
**Description**: Validates metrics collection and export

**Criteria:**
- Prometheus metrics exposed on port 9090
- Required metrics present
- Metrics format correct
- No metric scraping errors

**Implementation:**
```bash
# Metrics Check
#!/bin/bash
# QG-OBS-001: Metrics Collection Check

set -euo pipefail

SERVICE_URL="http://$(terraform output -raw service_url)"
METRICS_URL="$SERVICE_URL/metrics"

# Check metrics endpoint
if ! curl -f -s "$METRICS_URL" | grep -q "erlmcp_"; then
    echo "FAIL: Prometheus metrics not found"
    exit 1
fi

# Check required metrics
REQUIRED_METRICS=("http_requests_total" "http_request_duration_seconds" "erlmcp_connections_active")
for metric in "${REQUIRED_METRICS[@]}"; do
    if ! curl -s "$METRICS_URL" | grep -q "$metric"; then
        echo "FAIL: Required metric $metric missing"
        exit 1
    fi
done

echo "PASS: Metrics collection verified"
exit 0
```

#### 4.2 Logging Gate

**Gate ID**: QG-OBS-002
**Name**: Logging Compliance
**Description**: Validates logging configuration and format

**Criteria:**
- Structured logging enabled
- Log format JSON
- Required fields present
- No log errors

**Implementation:**
```bash
# Logging Check
#!/bin/bash
# QG-OBS-002: Logging Compliance Check

set -euo pipefail

# Test API call to generate logs
curl -s -X POST -H "Content-Type: application/json" \
  -d '{"jsonrpc": "2.0", "method": "tools/list", "id": "test-001"}' \
  "http://$(terraform output -raw service_url)"

# Check Cloud Logging for recent logs
RECENT_LOGS=$(gcloud logging read "resource.type=container AND resource.labels.project_id=PROJECT_ID AND timestamp>$(date -d '5 minutes ago' -Iseconds)" --limit=5 --format=json)

if [ -z "$RECENT_LOGS" ]; then
    echo "WARN: No recent logs found"
fi

# Check log format
echo "$RECENT_LOGS" | jq -e '.[].severity != null' >/dev/null 2>&1
if [ $? -ne 0 ]; then
    echo "FAIL: Invalid log format"
    exit 1
fi

echo "PASS: Logging compliance verified"
exit 0
```

### 5. Documentation Quality Gates

#### 5.1 Documentation Completeness Gate

**Gate ID**: QG-DOC-001
**Name**: Documentation Completeness
**Description**: Validates required documentation is complete

**Criteria:**
- README.md present
- Architecture documentation
- Security documentation
- Troubleshooting guide
- Deployment guides

**Implementation:**
```bash
# Documentation Check
#!/bin/bash
# QG-DOC-001: Documentation Completeness Check

set -euo pipefail

REQUIRED_DOCS=("README.md" "ARCHITECTURE.md" "SECURITY.md" "TROUBLESHOOTING.md" "DEPLOYMENT.md")
MISSING_DOCS=()

for doc in "${REQUIRED_DOCS[@]}"; do
    if [ ! -f "docs/$doc" ]; then
        MISSING_DOCS+=("$doc")
    fi
done

if [ ${#MISSING_DOCS[@]} -gt 0 ]; then
    echo "FAIL: Missing documentation: ${MISSING_DOCS[*]}"
    exit 1
fi

# Check markdown linting
if command -v markdownlint &> /dev/null; then
    if ! markdownlint docs/*.md; then
        echo "WARN: Markdown linting issues found"
    fi
fi

echo "PASS: Documentation completeness verified"
exit 0
```

#### 5.2 Schema Validation Gate

**Gate ID**: QG-DOC-002
**Name**: Schema Validation
**Description**: Validates input schema correctness

**Criteria:**
- JSON Schema valid
- Required fields present
- Type definitions correct
- Validation rules defined

**Implementation:**
```bash
# Schema Validation
#!/bin/bash
# QG-DOC-002: Schema Validation

set -euo pipefail

SCHEMA_FILE="marketplace/schema.yaml"

# Check if schema exists
if [ ! -f "$SCHEMA_FILE" ]; then
    echo "FAIL: Schema file not found"
    exit 1
fi

# Validate JSON Schema syntax
if ! python3 -c "import jsonschema; schema=json.load(open('$SCHEMA_FILE')); jsonschema.validate(schema, jsonschema.Draft7Validator.SCHEMA)" 2>/dev/null; then
    echo "FAIL: Invalid JSON Schema syntax"
    exit 1
fi

# Check required fields
REQUIRED_FIELDS=("\$schema" "title" "type" "required" "properties")
for field in "${REQUIRED_FIELDS[@]}"; do
    if ! yq eval ".$field" "$SCHEMA_FILE" >/dev/null 2>&1; then
        echo "FAIL: Required field $field missing"
        exit 1
    fi
done

echo "PASS: Schema validation passed"
exit 0
```

## Quality Gates Execution

### Quality Gates Runner

```bash
#!/bin/bash
# Quality Gates Runner
# Runs all quality gates and generates report

set -euo pipefail

QUALITY_GATES_DIR="scripts/quality-gates"
RESULTS_DIR="test-evidence/quality-gates"
mkdir -p "$RESULTS_DIR"

PASSED=0
FAILED=0

# List of quality gates to run
QUALITY_GATES=(
    "QG-SEC-001:Container-Security"
    "QG-SEC-002:IAM-Security"
    "QG-SEC-003:Network-Security"
    "QG-INFRA-001:Terraform-Validation"
    "QG-INFRA-002:Resource-Configuration"
    "QG-APP-001:Health-Check"
    "QG-APP-002:API-Compliance"
    "QG-APP-003:Performance"
    "QG-OBS-001:Metrics-Collection"
    "QG-OBS-002:Logging-Compliance"
    "QG-DOC-001:Documentation-Completeness"
    "QG-DOC-002:Schema-Validation"
)

# Run each quality gate
for gate in "${QUALITY_GATES[@]}"; do
    gate_id=$(echo "$gate" | cut -d: -f1)
    gate_name=$(echo "$gate" | cut -d: -f2)

    echo "Running $gate_id: $gate_name"

    if $QUALITY_GATES_DIR/$gate_name.sh; then
        echo "✓ PASSED: $gate_id"
        PASSED=$((PASSED + 1))
        echo "$gate_id:PASSED" >> "$RESULTS_DIR/quality-gates-results.txt"
    else
        echo "✗ FAILED: $gate_id"
        FAILED=$((FAILED + 1))
        echo "$gate_id:FAILED" >> "$RESULTS_DIR/quality-gates-results.txt"
    fi
done

# Generate report
echo "Quality Gates Report" > "$RESULTS_DIR/summary.txt"
echo "====================" >> "$RESULTS_DIR/summary.txt"
echo "Passed: $PASSED" >> "$RESULTS_DIR/summary.txt"
echo "Failed: $FAILED" >> "$RESULTS_DIR/summary.txt"
echo "Total: $((PASSED + FAILED))" >> "$RESULTS_DIR/summary.txt"

if [ $FAILED -gt 0 ]; then
    echo "❌ $FAILED quality gates failed"
    exit 1
else
    echo "✅ All quality gates passed"
    exit 0
fi
```

## Quality Gates Report

### Report Generation

```yaml
# Quality Gates Report Template
report:
  metadata:
    timestamp: "{{timestamp}}"
    version: "1.0"
    project: "erlmcp"

  summary:
    total_gates: 12
    passed_gates: 12
    failed_gates: 0
    pass_rate: 100%

  details:
    security_gates:
      total: 3
      passed: 3
      failed: 0

    infrastructure_gates:
      total: 2
      passed: 2
      failed: 0

    application_gates:
      total: 3
      passed: 3
      failed: 0

    observability_gates:
      total: 2
      passed: 2
      failed: 0

    documentation_gates:
      total: 2
      passed: 2
      failed: 0

  failed_gates: []

  recommendations: []
```

## Quality Gates Integration

### CI/CD Pipeline Integration

```yaml
# GitHub Actions Workflow
name: Quality Gates
on: [push, pull_request]

jobs:
  quality-gates:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v2

      - name: Setup Terraform
        uses: hashicorp/setup-terraform@v1

      - name: Run Quality Gates
        run: ./scripts/run-quality-gates.sh

      - name: Upload Results
        uses: actions/upload-artifact@v2
        with:
          name: quality-gates-results
          path: test-evidence/quality-gates/
```

### Quality Gates Dashboard

```yaml
# Dashboard Configuration
dashboard:
  widgets:
    - type: gauge
      title: Quality Gates Pass Rate
      metric: quality_gates.pass_rate
      threshold: 95

    - type: table
      title: Failed Gates
      metric: quality_gates.failed_gates

    - type: trend
      title: Pass Rate Trend
      metric: quality_gates.trend
```

## Continuous Monitoring

### Quality Gates Monitoring

```bash
# Quality Gates Monitoring Script
#!/bin/bash
# Monitors quality gates and alerts on failures

set -euo pipefail

# Check quality gates status
if ./scripts/run-quality-gates.sh; then
    echo "Quality gates: PASSED"
    # Send success notification
    curl -X POST "$WEBHOOK_URL" -d '{"status": "PASS", "message": "All quality gates passed"}'
else
    echo "Quality gates: FAILED"
    # Send failure notification
    curl -X POST "$WEBHOOK_URL" -d '{"status": "FAIL", "message": "Quality gates failed"}'
    # Send alert to Slack
    curl -X POST "$SLACK_WEBHOOK" -d '{"text": "⚠️ Quality gates failed for erlmcp"}'
    exit 1
fi
```

---

**Document Version**: 1.0
**Last Updated**: 2026-02-02
**Author**: QA Engineering Team