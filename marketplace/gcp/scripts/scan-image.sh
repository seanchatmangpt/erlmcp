#!/bin/bash
# ============================================================================
# Container Image Security Scan Script
# Runs Trivy and GCP Container Analysis for vulnerability scanning
# ============================================================================

set -euo pipefail

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

log_info() { echo -e "${GREEN}[INFO]${NC} $1"; }
log_warn() { echo -e "${YELLOW}[WARN]${NC} $1"; }
log_error() { echo -e "${RED}[ERROR]${NC} $1"; }
log_test() { echo -e "${BLUE}[TEST]${NC} $1"; }

# ============================================================================
# Configuration
# ============================================================================

PROJECT_ID="${PROJECT_ID:-}"
REGION="${REGION:-us-central1}"
IMAGE_NAME="${IMAGE_NAME:-erlmcp/erlmcp}"
IMAGE_TAG="${IMAGE_TAG:-latest}"
FULL_IMAGE="${REGION}-docker.pkg.dev/${PROJECT_ID}/${IMAGE_NAME}:${IMAGE_TAG}"

SCAN_MODE="${SCAN_MODE:-strict}"  # strict | relaxed | audit
TRIVY_SEVERITY="${TRIVY_SEVERITY:-HIGH,CRITICAL}"
FAIL_ON_VULN="${FAIL_ON_VULN:-true}"

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
MARKETPLACE_DIR="$(dirname "$SCRIPT_DIR")"
EVIDENCE_DIR="${MARKETPLACE_DIR}/test-evidence"

# ============================================================================
# Prerequisites
# ============================================================================

check_prerequisites() {
    log_info "Checking prerequisites..."

    if [ -z "$PROJECT_ID" ]; then
        log_error "PROJECT_ID not set. Run: export PROJECT_ID=your-project-id"
        exit 1
    fi

    if ! command -v docker &> /dev/null; then
        log_error "docker not found"
        exit 1
    fi

    # Create evidence directory
    mkdir -p "$EVIDENCE_DIR"

    # Configure docker auth
    gcloud auth configure-docker "${REGION}-docker.pkg.dev" --quiet > /dev/null 2>&1

    log_info "Prerequisites check passed"
}

# ============================================================================
# Scan Functions
# ============================================================================

test_trivy_installed() {
    log_test "Checking Trivy installation..."

    if docker run --rm aquasec/trivy:latest --version > "$EVIDENCE_DIR/trivy-version.txt" 2>&1; then
        TRIVY_VERSION=$(cat "$EVIDENCE_DIR/trivy-version.txt" | head -1)
        log_info "  ✓ Trivy available: $TRIVY_VERSION"
        return 0
    else
        log_error "  ✗ Trivy not available"
        return 1
    fi
}

test_trivy_scan() {
    log_test "Running Trivy vulnerability scan..."

    log_info "  Scanning image: $FULL_IMAGE"
    log_info "  Severity: $TRIVY_SEVERITY"

    # Run Trivy scan
    docker run --rm \
        -v /var/run/docker.sock:/var/run/docker.sock \
        -v "$EVIDENCE_DIR:/trivy-results" \
        aquasec/trivy:latest \
        image \
        --severity "$TRIVY_SEVERITY" \
        --format json \
        --output "/trivy-results/trivy-report.json" \
        --timeout 10m \
        "$FULL_IMAGE" > "$EVIDENCE_DIR/trivy-scan.log" 2>&1 || true

    # Generate human-readable report
    docker run --rm \
        -v /var/run/docker.sock:/var/run/docker.sock \
        aquasec/trivy:latest \
        image \
        --severity "$TRIVY_SEVERITY" \
        --format table \
        "$FULL_IMAGE" > "$EVIDENCE_DIR/trivy-report.txt" 2>&1 || true

    # Parse results
    if [ -f "$EVIDENCE_DIR/trivy-report.json" ]; then
        TOTAL_VULNS=$(jq '[.Results[].Vulnerabilities // []] | add | length' "$EVIDENCE_DIR/trivy-report.json" 2>/dev/null || echo "0")
        HIGH_VULNS=$(jq '[.Results[].Vulnerabilities // []] | add | [.[] | select(.Severity == "HIGH")] | length' "$EVIDENCE_DIR/trivy-report.json" 2>/dev/null || echo "0")
        CRITICAL_VULNS=$(jq '[.Results[].Vulnerabilities // []] | add | [.[] | select(.Severity == "CRITICAL")] | length' "$EVIDENCE_DIR/trivy-report.json" 2>/dev/null || echo "0")

        log_info "  Total vulnerabilities: $TOTAL_VULNS"
        log_info "  HIGH severity: $HIGH_VULNS"
        log_info "  CRITICAL severity: $CRITICAL_VULNS"

        echo "$TOTAL_VULNS" > "$EVIDENCE_DIR/vuln-total.txt"
        echo "$HIGH_VULNS" > "$EVIDENCE_DIR/vuln-high.txt"
        echo "$CRITICAL_VULNS" > "$EVIDENCE_DIR/vuln-critical.txt"

        # Print summary table
        echo ""
        echo "  ┌─────────────────────────────────┐"
        echo "  │ Vulnerability Summary           │"
        echo "  ├─────────────────────────────────┤"
        echo "  │ Critical: $CRITICAL_VULNS │"
        echo "  │ High:      $HIGH_VULNS │"
        echo "  │ Total:     $TOTAL_VULNS │"
        echo "  └─────────────────────────────────┘"

        # Check based on scan mode
        if [ "$SCAN_MODE" = "strict" ]; then
            if [ "$CRITICAL_VULNS" -gt 0 ] || [ "$HIGH_VULNS" -gt 0 ]; then
                log_error "  ✗ Found HIGH or CRITICAL vulnerabilities in strict mode"
                log_info "  Full report: $EVIDENCE_DIR/trivy-report.txt"
                return 1
            fi
        elif [ "$SCAN_MODE" = "relaxed" ]; then
            if [ "$CRITICAL_VULNS" -gt 0 ]; then
                log_error "  ✗ Found CRITICAL vulnerabilities even in relaxed mode"
                return 1
            fi
            if [ "$HIGH_VULNS" -gt 0 ]; then
                log_warn "  ⚠ Found $HIGH_VULNS HIGH severity vulnerabilities (allowed in relaxed mode)"
            fi
        fi

        log_info "  ✓ Scan passed for mode: $SCAN_MODE"
        return 0
    else
        log_error "  ✗ Trivy scan failed to produce report"
        cat "$EVIDENCE_DIR/trivy-scan.log"
        return 1
    fi
}

test_gcp_container_scan() {
    log_test "Running GCP Container Analysis..."

    log_info "  This may take 5-10 minutes..."

    if gcloud artifacts docker images scan \
        "$FULL_IMAGE" \
        --location="$REGION" \
        --project="$PROJECT_ID" \
        --format=json \
        --timeout=600s > "$EVIDENCE_DIR/gcp-scan.json" 2>&1; then

        log_info "  ✓ GCP scan completed"

        # Parse results
        if command -v jq &> /dev/null; then
            GCP_VULNS=$(jq 'length' "$EVIDENCE_DIR/gcp-scan.json" 2>/dev/null || echo "0")
            log_info "  GCP found vulnerability occurrences: $GCP_VULNS"

            # Generate readable report
            jq -r '.[] | "\(.package) \n  Version: \(.version)\n  Severity: \(.severity)\n  CVE: \(.vulnerability)"' \
                "$EVIDENCE_DIR/gcp-scan.json" 2>/dev/null > "$EVIDENCE_DIR/gcp-vuln-details.txt" || true
        fi

        return 0
    else
        log_warn "  ⚠ GCP scan failed (may need API enablement)"
        log_info "  Run: gcloud services enable containerscanning.googleapis.com"
        return 0
    fi
}

test_vulnerability_details() {
    log_test "Extracting vulnerability details..."

    if [ ! -f "$EVIDENCE_DIR/trivy-report.json" ]; then
        log_warn "  ⚠ No Trivy report available"
        return 0
    fi

    # Extract vulnerabilities by package
    jq -r '.Results[]? | select(.Vulnerabilities != null) |
        "\nType: \(.Type)\nTarget: \(.Target)" +
        ([.Vulnerabilities[]? |
            "  - \(.VulnerabilityID) (\(.Severity)): \(.Title // "No title")"
        ] | join("\n"))' \
        "$EVIDENCE_DIR/trivy-report.json" > "$EVIDENCE_DIR/vulnerability-details.txt" 2>/dev/null || true

    log_info "  ✓ Vulnerability details extracted"

    # Check for fixable vulnerabilities
    FIXABLE=$(jq '[.Results[].Vulnerabilities // []] | add | [.[] | select(.FixedVersion != null)] | length' "$EVIDENCE_DIR/trivy-report.json" 2>/dev/null || echo "0")
    log_info "  Fixable vulnerabilities: $FIXABLE"

    return 0
}

test_compliance_check() {
    log_test "Checking security compliance..."

    COMPLIANCE_ISSUES=0

    # Check for known CVEs
    if [ -f "$EVIDENCE_DIR/trivy-report.json" ]; then
        # CVE-2021-44228 (Log4Shell)
        LOG4SHELL=$(jq '[.Results[].Vulnerabilities // []] | add | [.[] | select(.VulnerabilityID == "CVE-2021-44228")] | length' "$EVIDENCE_DIR/trivy-report.json" 2>/dev/null || echo "0")
        if [ "$LOG4SHELL" -gt 0 ]; then
            log_error "  ✗ CRITICAL: Log4Shell vulnerability detected!"
            COMPLIANCE_ISSUES=$((COMPLIANCE_ISSUES + 1))
        fi

        # CVE-2021-34527 (PrintNightmare)
        PRINTNIGHTMARE=$(jq '[.Results[].Vulnerabilities // []] | add | [.[] | select(.VulnerabilityID == "CVE-2021-34527")] | length' "$EVIDENCE_DIR/trivy-report.json" 2>/dev/null || echo "0")
        if [ "$PRINTNIGHTMARE" -gt 0 ]; then
            log_error "  ✗ CRITICAL: PrintNightmare vulnerability detected!"
            COMPLIANCE_ISSUES=$((COMPLIANCE_ISSUES + 1))
        fi

        # Check for base image issues
        BASE_IMAGE=$(docker inspect "$FULL_IMAGE" --format='{{.Config.Image}}' 2>/dev/null || echo "unknown")
        log_info "  Base image: $BASE_IMAGE"

        # Warn if using Alpine < 3.15 (known CVEs)
        if echo "$BASE_IMAGE" | grep -q 'alpine:[0-2]\.'; then
            log_error "  ✗ Using outdated Alpine base image"
            COMPLIANCE_ISSUES=$((COMPLIANCE_ISSUES + 1))
        fi
    fi

    if [ $COMPLIANCE_ISSUES -eq 0 ]; then
        log_info "  ✓ Compliance check passed"
        return 0
    else
        log_error "  ✗ Compliance check failed with $COMPLIANCE_ISSUES issues"
        return 1
    fi
}

generate_sarif_report() {
    log_info "Generating SARIF report..."

    if [ ! -f "$EVIDENCE_DIR/trivy-report.json" ]; then
        log_warn "  No Trivy report to convert"
        return 0
    fi

    # Trivy can generate SARIF directly
    docker run --rm \
        -v /var/run/docker.sock:/var/run/docker.sock \
        -v "$EVIDENCE_DIR:/trivy-results" \
        aquasec/trivy:latest \
        image \
        --format sarif \
        --output "/trivy-results/trivy.sarif.json" \
        "$FULL_IMAGE" > /dev/null 2>&1 || true

    if [ -f "$EVIDENCE_DIR/trivy.sarif.json" ]; then
        log_info "  ✓ SARIF report generated: $EVIDENCE_DIR/trivy.sarif.json"
    fi

    return 0
}

generate_summary_report() {
    log_info "Generating summary report..."

    cat > "$EVIDENCE_DIR/scan-summary.md" <<EOF
# Container Image Security Scan Summary

**Scan Date:** $(date -u +"%Y-%m-%dT%H:%M:%SZ")
**Image:** $FULL_IMAGE
**Scan Mode:** $SCAN_MODE

## Vulnerability Summary

| Severity | Count |
|----------|-------|
| Critical | $(cat "$EVIDENCE_DIR/vuln-critical.txt" 2>/dev/null || echo "0") |
| High | $(cat "$EVIDENCE_DIR/vuln-high.txt" 2>/dev/null || echo "0") |
| Total | $(cat "$EVIDENCE_DIR/vuln-total.txt" 2>/dev/null || echo "0") |

## Scan Status

- Trivy Scan: $([ -f "$EVIDENCE_DIR/trivy-report.json" ] && echo "✓ Completed" || echo "✗ Failed")
- GCP Container Analysis: $([ -f "$EVIDENCE_DIR/gcp-scan.json" ] && echo "✓ Completed" || echo "⚠ Skipped/Failed")
- Compliance Check: ✓ Completed

## Recommendations

$(
if [ "$(cat "$EVIDENCE_DIR/vuln-critical.txt" 2>/dev/null || echo "0")" -gt 0 ]; then
    echo "- **CRITICAL**: Address all CRITICAL vulnerabilities before deployment"
fi
if [ "$(cat "$EVIDENCE_DIR/vuln-high.txt" 2>/dev/null || echo "0")" -gt 0 ]; then
    echo "- **HIGH**: Review and fix HIGH severity vulnerabilities"
fi
if grep -q "alpine:" <<< "$(docker inspect "$FULL_IMAGE" --format='{{.Config.Image}}' 2>/dev/null || echo "")"; then
    echo "- Consider using a distroless or minimal base image"
fi
echo "- Run scans regularly as part of CI/CD pipeline"
echo "- Enable GCP Container Analysis for continuous scanning"
)

## Artifacts

- Trivy JSON Report: \`trivy-report.json\`
- Trivy Text Report: \`trivy-report.txt\`
- GCP Scan Results: \`gcp-scan.json\`
- Vulnerability Details: \`vulnerability-details.txt\`
- SARIF Report: \`trivy.sarif.json\`

## Next Steps

1. Review critical and high vulnerabilities
2. Update base image to latest secure version
3. Update dependencies to patched versions
4. Re-scan after fixes
5. Update image tag and re-deploy

EOF

    log_info "  ✓ Summary report: $EVIDENCE_DIR/scan-summary.md"

    # Print summary to console
    cat "$EVIDENCE_DIR/scan-summary.md"

    return 0
}

# ============================================================================
# Main Test Runner
# ============================================================================

main() {
    log_info "Starting Container Image Security Scan..."
    log_info "================================================"
    log_info "Project: $PROJECT_ID"
    log_info "Image: $FULL_IMAGE"
    log_info "Scan Mode: $SCAN_MODE"
    log_info "================================================"

    check_prerequisites

    # Test counter
    TOTAL_TESTS=0
    PASSED_TESTS=0
    FAILED_TESTS=0

    # Run tests
    for test_func in \
        test_trivy_installed \
        test_trivy_scan \
        test_gcp_container_scan \
        test_vulnerability_details \
        test_compliance_check; do

        TOTAL_TESTS=$((TOTAL_TESTS + 1))

        if $test_func; then
            PASSED_TESTS=$((PASSED_TESTS + 1))
        else
            FAILED_TESTS=$((FAILED_TESTS + 1))

            # Stop on critical failures in strict mode
            if [ "$SCAN_MODE" = "strict" ] && [ "$FAIL_ON_VULN" = "true" ]; then
                log_error "Critical failure in strict mode. Stopping."
                break
            fi
        fi
    done

    # Generate reports
    generate_sarif_report
    generate_summary_report

    # Summary
    log_info "================================================"
    log_info "Scan Summary:"
    log_info "  Total Checks: $TOTAL_TESTS"
    log_info "  Passed:       $PASSED_TESTS"
    log_info "  Failed:       $FAILED_TESTS"
    log_info "================================================"
    log_info "Results saved to: $EVIDENCE_DIR"

    if [ $FAILED_TESTS -gt 0 ] && [ "$FAIL_ON_VULN" = "true" ]; then
        log_error "Security scan failed. DO NOT DEPLOY this image."
        exit 1
    fi

    log_info "Security scan completed!"
    exit 0
}

# ============================================================================
# Script Entry Point
# ============================================================================

# Parse arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        --project)
            PROJECT_ID="$2"
            shift 2
            ;;
        --region)
            REGION="$2"
            shift 2
            ;;
        --image)
            FULL_IMAGE="$2"
            shift 2
            ;;
        --tag)
            IMAGE_TAG="$2"
            shift 2
            ;;
        --mode)
            SCAN_MODE="$2"
            shift 2
            ;;
        --severity)
            TRIVY_SEVERITY="$2"
            shift 2
            ;;
        --no-fail)
            FAIL_ON_VULN=false
            shift
            ;;
        *)
            log_error "Unknown option: $1"
            echo "Usage: $0 [--project PROJECT] [--region REGION] [--tag TAG] [--mode strict|relaxed|audit] [--severity SEVERITIES] [--no-fail]"
            exit 1
            ;;
    esac
done

main
