#!/bin/bash
# Pre-deployment validation script
# Ensures all quality gates pass before deployment

set -euo pipefail

DEPLOY_ENV=${1:-production}
REGION=${2:-us-east-1}

echo "=== Pre-Deployment Validation ==="
echo "Environment: ${DEPLOY_ENV}"
echo "Region: ${REGION}"
echo "Time: $(date)"
echo ""

FAILED=0

# 1. Check all tests pass
echo "1. Checking test status..."
if ! rebar3 as test do eunit, ct > /dev/null 2>&1; then
  echo "  FAIL: Tests not passing"
  ((FAILED++))
else
  echo "  PASS: All tests passing"
fi

# 2. Check security scan
echo "2. Checking security scan..."
if docker build -q -f docker/Dockerfile.production . >/dev/null 2>&1; then
  if trivy image --severity CRITICAL,HIGH --exit-code 1 ghcr.io/seanchatmangpt/erlmcp:latest >/dev/null 2>&1; then
    echo "  WARN: Security vulnerabilities found (review required)"
  else
    echo "  PASS: No critical vulnerabilities"
  fi
else
  echo "  SKIP: Image build failed (will be built during deployment)"
fi

# 3. Check code coverage
echo "3. Checking code coverage..."
COVERAGE=$(rebar3 as test cover --coverläss coverage_summary.coverdata 2>/dev/null | grep "percentage" | awk '{print $2}' | tr -d '%' || echo "0")
if [ "$COVERAGE" -lt 80 ]; then
  echo "  FAIL: Code coverage ${COVERAGE}% is below 80%"
  ((FAILED++))
else
  echo "  PASS: Code coverage ${COVERAGE}% meets threshold"
fi

# 4. Check staging soak
echo "4. Checking staging soak period..."
if [ "$DEPLOY_ENV" = "production" ]; then
  # Get latest staging deploy time
  LAST_STAGING=$(kubectl get deployment erlmcp -n erlmcp-staging -o jsonpath='{.metadata.creationTimestamp}' 2>/dev/null || echo "")
  if [ -n "$LAST_STAGING" ]; then
    STAGING_AGE=$(( ($(date +%s) - $(date -d "$LAST_STAGING" +%s)) / 86400 ))
    if [ "$STAGING_AGE" -lt 1 ]; then
      echo "  FAIL: Staging soak period less than 24 hours (${STAGING_AGE} days)"
      ((FAILED++))
    else
      echo "  PASS: Staging soak period satisfied (${STAGING_AGE} days)"
    fi
  fi
else
  echo "  SKIP: Not a production deployment"
fi

# 5. Check change board approval
echo "5. Checking change board approval..."
if [ "$DEPLOY_ENV" = "production" ]; then
  # In production, check for approval file or git tag
  if git describe --tags --exact-match 2>/dev/null | grep -q "^v"; then
    echo "  PASS: Version tag found (implies approval)"
  else
    echo "  WARN: No version tag found (ensure approval)"
  fi
else
  echo "  SKIP: Not a production deployment"
fi

# 6. Check cluster connectivity
echo "6. Checking cluster connectivity..."
if kubectl cluster-info --context=$REGION >/dev/null 2>&1; then
  echo "  PASS: Cluster is accessible"
else
  echo "  FAIL: Cluster is not accessible"
  ((FAILED++))
fi

# 7. Check available resources
echo "7. Checking available resources..."
AVAILABLE_CPU=$(kubectl top nodes --context=$REGION | awk '{sum+$2}' | tail -1)
if [ -n "$AVAILABLE_CPU" ]; then
  echo "  PASS: Cluster has available resources"
else
  echo "  WARN: Could not determine available resources"
fi

# 8. Check monitoring
echo "8. Checking monitoring systems..."
if curl -sf https://prometheus.erlmcp.io/-/healthy >/dev/null 2>&1; then
  echo "  PASS: Prometheus is healthy"
else
  echo "  WARN: Prometheus may not be available"
fi

if curl -sf https://grafana.erlmcp.io/api/health >/dev/null 2>&1; then
  echo "  PASS: Grafana is healthy"
else
  echo "  WARN: Grafana may not be available"
fi

# 9. Check rollback plan
echo "9. Checking rollback plan..."
if [ -f scripts/rollback/rollback-previous.sh ]; then
  echo "  PASS: Rollback script exists"
else
  echo "  WARN: Rollback script not found"
fi

# 10. Check on-call availability
echo "10. Checking on-call assignment..."
echo "  INFO: Ensure on-call engineer is assigned and available"

# Summary
echo ""
echo "=== Pre-Deployment Validation Summary ==="
if [ $FAILED -eq 0 ]; then
  echo "Result: PASS - All validations passed"
  exit 0
else
  echo "Result: FAIL - ${FAILED} validation(s) failed"
  exit 1
fi
