#!/bin/bash

# Script to verify artifacts before deployment
set -e

ARTIFACTS_DIR="artifacts"
REQUIRED_ARTIFACTS=(
    "erlmcp-v3.tar.gz"
    "production-manifest*.yaml"
    "quality-report-*.json"
)

# Check if artifacts directory exists
if [ ! -d "$ARTIFACTS_DIR" ]; then
    echo "Error: Artifacts directory not found: $ARTIFACTS_DIR"
    exit 1
fi

# Check required artifacts
MISSING_ARTIFACTS=()
for artifact in "${REQUIRED_ARTIFACTS[@]}"; do
    if ! ls $ARTIFACTS_DIR/$artifact 1> /dev/null 2>&1; then
        MISSING_ARTIFACTS+=("$artifact")
    fi
done

if [ ${#MISSING_ARTIFACTS[@]} -gt 0 ]; then
    echo "Error: Missing required artifacts:"
    for artifact in "${MISSING_ARTIFACTS[@]}"; do
        echo "  - $artifact"
    done
    exit 1
fi

# Verify package integrity
if ! tar -tzf $ARTIFACTS_DIR/erlmcp-v3.tar.gz >/dev/null 2>&1; then
    echo "Error: Release package is corrupted"
    exit 1
fi

# Check quality report
QUALITY_REPORT=$(ls $ARTIFACTS_DIR/quality-report-*.json | tail -1)
if [ -f "$QUALITY_REPORT" ]; then
    SECURITY_PASSED=$(jq '.security.passed' $QUALITY_REPORT)
    TESTS_PASSED=$(jq '.tests.passed' $QUALITY_REPORT)
    PERFORMANCE_PASSED=$(jq '.performance.baseline_met' $QUALITY_REPORT)
    COMPLIANCE_PASSED=$(jq '.compliance.spec_compliant' $QUALITY_REPORT)

    if [ "$SECURITY_PASSED" != "true" ] || [ "$TESTS_PASSED" != "true" ] ||
       [ "$PERFORMANCE_PASSED" != "true" ] || [ "$COMPLIANCE_PASSED" != "true" ]; then
        echo "Error: Quality gates not passed:"
        echo "  Security: $SECURITY_PASSED"
        echo "  Tests: $TESTS_PASSED"
        echo "  Performance: $PERFORMANCE_PASSED"
        echo "  Compliance: $COMPLIANCE_PASSED"
        exit 1
    fi
fi

# Verify image registry access
if ! docker manifest inspect ghcr.io/${GITHUB_REPOSITORY}:${GITHUB_SHA} >/dev/null 2>&1; then
    echo "Error: Docker image not found in registry"
    exit 1
fi

echo "All artifacts verified successfully"
echo "Artifacts:"
ls -la $ARTIFACTS_DIR/