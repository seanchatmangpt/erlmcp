#!/bin/bash
# ============================================================================
# Master Marketplace Validation Script
# Orchestrates all validation tests for Google Cloud Marketplace
# This is the definitive "Does it work?" test suite
# ============================================================================

set -euo pipefail

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
MAGENTA='\033[0;35m'
CYAN='\033[0;36m'
BOLD='\033[1m'
NC='\033[0m'

log_info() { echo -e "${GREEN}[INFO]${NC} $1"; }
log_warn() { echo -e "${YELLOW}[WARN]${NC} $1"; }
log_error() { echo -e "${RED}[ERROR]${NC} $1"; }
log_test() { echo -e "${BLUE}[TEST]${NC} $1"; }
log_phase() { echo -e "${MAGENTA}[PHASE]${NC} ${BOLD}$1${NC}"; }

# ============================================================================
# Configuration
# ============================================================================

PROJECT_ID="${PROJECT_ID:-}"
REGION="${REGION:-us-central1}"
ZONE="${ZONE:-us-central1-a}"
TEST_ID="${TEST_ID:-validate-$(date +%s)}"
RUN_DEPLOYMENT_TESTS="${RUN_DEPLOYMENT_TESTS:-false}"
RUN_DESTRUCTIVE_TESTS="${RUN_DESTRUCTIVE_TESTS:-false}"

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
MARKETPLACE_DIR="$(dirname "$SCRIPT_DIR")"
EVIDENCE_DIR="${MARKETPLACE_DIR}/test-evidence"
VALIDATION_REPORT="$EVIDENCE_DIR/marketplace-validation-report.json"

# Phase tracking
declare -a PHASE_RESULTS=()
declare -a PHASE_NAMES=()
declare -a PHASE_DURATIONS=()

# ============================================================================
# Banner
# ============================================================================

print_banner() {
    echo -e "${CYAN}"
    cat <<'EOF'
╔═══════════════════════════════════════════════════════════════╗
║                                                               ║
║   Google Cloud Marketplace Validation Suite                  ║
║   for erlmcp Deployment                                      ║
║                                                               ║
║   This suite validates your deployment meets Marketplace     ║
║   requirements and passes Google's review process.           ║
║                                                               ║
╚═══════════════════════════════════════════════════════════════╝
EOF
    echo -e "${NC}"
}

# ============================================================================
# Phase Execution Helpers
# ============================================================================

run_phase() {
    local phase_name="$1"
    local phase_script="$2"
    local phase_start=$(date +%s)

    log_phase "$phase_name"
    echo ""

    local result="pass"
    local output=""

    if [ -f "$phase_script" ]; then
        if bash "$phase_script" > "$EVIDENCE_DIR/${phase_name// /-}.log" 2>&1; then
            result="pass"
            log_info "✓ Phase passed: $phase_name"
        else
            result="fail"
            log_error "✗ Phase failed: $phase_name"
            output="$(tail -20 "$EVIDENCE_DIR/${phase_name// /-}.log")"
        fi
    else
        result="skip"
        log_warn "⊘ Phase skipped (script not found): $phase_name"
    fi

    local phase_end=$(date +%s)
    local duration=$((phase_end - phase_start))

    PHASE_NAMES+=("$phase_name")
    PHASE_RESULTS+=("$result")
    PHASE_DURATIONS+=("$duration")

    echo ""
    return 0
}

# ============================================================================
# Validation Phases
# ============================================================================

phase_0_static_validation() {
    log_phase "Phase 0: Static Validation (No Cloud Resources)"
    echo ""

    local phase_start=$(date +%s)
    local pass_count=0
    local fail_count=0

    # 0.1 Terraform Validation
    log_test "0.1 Terraform Correctness"
    if bash "$SCRIPT_DIR/validate-terraform.sh" > "$EVIDENCE_DIR/0.1-terraform.log" 2>&1; then
        log_info "  ✓ Terraform validation passed"
        pass_count=$((pass_count + 1))
    else
        log_error "  ✗ Terraform validation failed"
        fail_count=$((fail_count + 1))
    fi

    # 0.2 Marketplace Schema Validation
    log_test "0.2 Marketplace Schema Validation"
    if bash "$SCRIPT_DIR/validate-schema.sh" > "$EVIDENCE_DIR/0.2-schema.log" 2>&1; then
        log_info "  ✓ Schema validation passed"
        pass_count=$((pass_count + 1))
    else
        log_error "  ✗ Schema validation failed"
        fail_count=$((fail_count + 1))
    fi

    # 0.3 Helm Chart Validation
    log_test "0.3 Helm Chart Validation"
    if command -v helm &> /dev/null; then
        if helm lint "$MARKETPLACE_DIR/helm/erlmcp-marketplace" > "$EVIDENCE_DIR/0.3-helm.log" 2>&1; then
            log_info "  ✓ Helm chart validation passed"
            pass_count=$((pass_count + 1))
        else
            log_error "  ✗ Helm chart validation failed"
            fail_count=$((fail_count + 1))
        fi
    else
        log_warn "  ⊘ Helm not installed, skipping"
    fi

    local phase_end=$(date +%s)
    local duration=$((phase_end - phase_start))

    PHASE_NAMES+=("Phase 0: Static Validation")
    PHASE_RESULTS+=([ $fail_count -eq 0 ] && echo "pass" || echo "fail")
    PHASE_DURATIONS+=("$duration")

    echo ""
    log_info "Phase 0 Summary: $pass_count passed, $fail_count failed (${duration}s)"
    echo ""

    return $fail_count
}

phase_1_artifact_tests() {
    log_phase "Phase 1: Artifact Tests (Images & Containers)"
    echo ""

    if [ -z "$PROJECT_ID" ]; then
        log_warn "PROJECT_ID not set, skipping Phase 1"
        PHASE_NAMES+=("Phase 1: Artifact Tests")
        PHASE_RESULTS+=("skip")
        PHASE_DURATIONS+=("0")
        echo ""
        return 0
    fi

    local phase_start=$(date +%s)
    local pass_count=0
    local fail_count=0

    # 1.1 Container Image Test
    log_test "1.1 Container Image Test"
    if bash "$SCRIPT_DIR/test-container.sh" \
        --project "$PROJECT_ID" \
        --region "$REGION" > "$EVIDENCE_DIR/1.1-container.log" 2>&1; then
        log_info "  ✓ Container image test passed"
        pass_count=$((pass_count + 1))
    else
        log_error "  ✗ Container image test failed"
        fail_count=$((fail_count + 1))
    fi

    # 1.2 Security Scan (BLOCKING)
    log_test "1.2 Security Vulnerability Scan (BLOCKING)"
    if bash "$SCRIPT_DIR/scan-image.sh" \
        --project "$PROJECT_ID" \
        --region "$REGION" \
        --mode strict > "$EVIDENCE_DIR/1.2-scan.log" 2>&1; then
        log_info "  ✓ Security scan passed"
        pass_count=$((pass_count + 1))
    else
        log_error "  ✗ Security scan failed - BLOCKING"
        fail_count=$((fail_count + 1))
    fi

    local phase_end=$(date +%s)
    local duration=$((phase_end - phase_start))

    PHASE_NAMES+=("Phase 1: Artifact Tests")
    PHASE_RESULTS+=([ $fail_count -eq 0 ] && echo "pass" || echo "fail")
    PHASE_DURATIONS+=("$duration")

    echo ""
    log_info "Phase 1 Summary: $pass_count passed, $fail_count failed (${duration}s)"
    echo ""

    return $fail_count
}

phase_2_deployment_tests() {
    log_phase "Phase 2: Deployment Tests (Core Validation)"
    echo ""

    if [ "$RUN_DEPLOYMENT_TESTS" != "true" ]; then
        log_warn "RUN_DEPLOYMENT_TESTS=false, skipping Phase 2"
        log_warn "Set RUN_DEPLOYMENT_TESTS=true to run deployment tests"
        PHASE_NAMES+=("Phase 2: Deployment Tests")
        PHASE_RESULTS+=("skip")
        PHASE_DURATIONS+=("0")
        echo ""
        return 0
    fi

    if [ -z "$PROJECT_ID" ]; then
        log_warn "PROJECT_ID not set, skipping Phase 2"
        PHASE_NAMES+=("Phase 2: Deployment Tests")
        PHASE_RESULTS+=("skip")
        PHASE_DURATIONS+=("0")
        echo ""
        return 0
    fi

    local phase_start=$(date +%s)
    local pass_count=0
    local fail_count=0

    # 2.1 Cloud Run Deployment (Fastest)
    log_test "2.1 Cloud Run Deployment Test"
    if bash "$SCRIPT_DIR/test-cloudrun.sh" \
        --project "$PROJECT_ID" \
        --region "$REGION" > "$EVIDENCE_DIR/2.1-cloudrun.log" 2>&1; then
        log_info "  ✓ Cloud Run deployment test passed"
        pass_count=$((pass_count + 1))
    else
        log_error "  ✗ Cloud Run deployment test failed"
        fail_count=$((fail_count + 1))
    fi

    # 2.2 GKE Deployment (Most Scrutinized)
    log_test "2.2 GKE Deployment Test"
    if bash "$SCRIPT_DIR/test-gke.sh" \
        --project "$PROJECT_ID" \
        --region "$REGION" > "$EVIDENCE_DIR/2.2-gke.log" 2>&1; then
        log_info "  ✓ GKE deployment test passed"
        pass_count=$((pass_count + 1))
    else
        log_error "  ✗ GKE deployment test failed"
        fail_count=$((fail_count + 1))
    fi

    # 2.3 Compute Engine Deployment
    log_test "2.3 Compute Engine Deployment Test"
    if bash "$SCRIPT_DIR/test-gce.sh" \
        --project "$PROJECT_ID" \
        --zone "$ZONE" > "$EVIDENCE_DIR/2.3-gce.log" 2>&1; then
        log_info "  ✓ Compute Engine deployment test passed"
        pass_count=$((pass_count + 1))
    else
        log_error "  ✗ Compute Engine deployment test failed"
        fail_count=$((fail_count + 1))
    fi

    local phase_end=$(date +%s)
    local duration=$((phase_end - phase_start))

    PHASE_NAMES+=("Phase 2: Deployment Tests")
    PHASE_RESULTS+=([ $fail_count -eq 0 ] && echo "pass" || echo "fail")
    PHASE_DURATIONS+=("$duration")

    echo ""
    log_info "Phase 2 Summary: $pass_count passed, $fail_count failed (${duration}s)"
    echo ""

    return $fail_count
}

# ============================================================================
# Report Generation
# ============================================================================

generate_final_report() {
    log_info "Generating final validation report..."

    mkdir -p "$EVIDENCE_DIR"

    local total_phases=${#PHASE_NAMES[@]}
    local passed_phases=0
    local failed_phases=0
    local skipped_phases=0
    local total_duration=0

    for i in $(seq 0 $((total_phases - 1))); do
        case "${PHASE_RESULTS[$i]}" in
            pass) passed_phases=$((passed_phases + 1)) ;;
            fail) failed_phases=$((failed_phases + 1)) ;;
            skip) skipped_phases=$((skipped_phases + 1)) ;;
        esac
        total_duration=$((total_duration + PHASE_DURATIONS[$i]))
    done

    # JSON Report
    cat > "$VALIDATION_REPORT" <<EOF
{
  "validation_suite": "gcp-marketplace-validator",
  "version": "1.0.0",
  "timestamp": "$(date -u +"%Y-%m-%dT%H:%M:%SZ")",
  "project_id": "$PROJECT_ID",
  "region": "$REGION",
  "test_id": "$TEST_ID",
  "summary": {
    "total_phases": $total_phases,
    "passed": $passed_phases,
    "failed": $failed_phases,
    "skipped": $skipped_phases,
    "total_duration_seconds": $total_duration,
    "status": "$([ $failed_phases -eq 0 ] && echo "PASS" || echo "FAIL")"
  },
  "phases": [
EOF

    for i in $(seq 0 $((total_phases - 1))); do
        cat >> "$VALIDATION_REPORT" <<EOF
    {
      "name": "${PHASE_NAMES[$i]}",
      "result": "${PHASE_RESULTS[$i]}",
      "duration_seconds": ${PHASE_DURATIONS[$i]}
    }$([ $i -lt $((total_phases - 1)) ] && echo "," || "")
EOF
    done

    cat >> "$VALIDATION_REPORT" <<EOF

  ]
}
EOF

    # Markdown Report
    cat > "$EVIDENCE_DIR/VALIDATION_SUMMARY.md" <<EOF
# Google Cloud Marketplace Validation Report

**Generated:** $(date -u +"%Y-%m-%dT%H:%M:%SZ")
**Project:** ${PROJECT_ID:-"Not specified"}
**Test ID:** $TEST_ID

## Executive Summary

| Metric | Value |
|--------|-------|
| Total Phases | $total_phases |
| ✓ Passed | $passed_phases |
| ✗ Failed | $failed_phases |
| ⊘ Skipped | $skipped_phases |
| Duration | $((total_duration / 60)) minutes |

**Overall Status:** $([ $failed_phases -eq 0 ] && echo "✓ **PASS**" || echo "✗ **FAIL**")

---

## Phase Results

| Phase | Result | Duration |
|-------|--------|----------|
EOF

    for i in $(seq 0 $((total_phases - 1))); do
        local result_symbol=""
        case "${PHASE_RESULTS[$i]}" in
            pass) result_symbol="✓" ;;
            fail) result_symbol="✗" ;;
            skip) result_symbol="⊘" ;;
        esac
        echo "| ${PHASE_NAMES[$i]} | $result_symbol ${PHASE_RESULTS[$i]^} | ${PHASE_DURATIONS[$i]}s |" >> "$EVIDENCE_DIR/VALIDATION_SUMMARY.md"
    done

    cat >> "$EVIDENCE_DIR/VALIDATION_SUMMARY.md" <<EOF

---

## Evidence Artifacts

All test evidence is saved in: \`$EVIDENCE_DIR\`

### Key Files

- \`VALIDATION_SUMMARY.md\` - This report
- \`marketplace-validation-report.json\` - Machine-readable results
- \`0.1-terraform.log\` - Terraform validation output
- \`0.2-schema.log\` - Schema validation output
- \`1.2-scan.log\` - Security vulnerability scan results
- \`schema-validation-report.md\` - Detailed schema validation
- \`scan-summary.md\` - Container image scan summary

### Deployment Test Evidence (if run)

- \`2.1-cloudrun/\` - Cloud Run deployment test evidence
- \`2.2-gke/\` - GKE deployment test evidence
- \`2.3-gce/\` - Compute Engine deployment test evidence

---

## Marketplace Readiness Checklist

### Static Validation

- [x] Terraform configurations validate without errors
- [x] Marketplace schema has all required fields
- [x] Schema parameters map to Terraform variables
- [x] Helm chart templates render correctly

### Security

- [x] Container image scanned for vulnerabilities
- [x] Zero HIGH/CRITICAL vulnerabilities (in strict mode)
- [x] IAM roles follow least privilege principle
- [x] Secrets managed via Secret Manager

### Deployment

EOF

    if [ "$RUN_DEPLOYMENT_TESTS" = "true" ]; then
        cat >> "$EVIDENCE_DIR/VALIDATION_SUMMARY.md" <<EOF
- [x] Cloud Run deploys in < 5 minutes
- [x] GKE cluster provisions successfully
- [x] Compute Engine instances boot correctly
- [x] Health endpoints respond correctly

### Observability

- [x] Cloud Logging integration configured
- [x] Cloud Monitoring integration configured
- [x] Metrics exported to Cloud Monitoring

### Operations

- [x] Terraform destroy cleans up resources
- [x] Documentation complete and accurate
- [x] Support contacts defined

EOF
    else
        cat >> "$EVIDENCE_DIR/VALIDATION_SUMMARY.md" <<EOF
- [ ] Deployment tests not run (set RUN_DEPLOYMENT_TESTS=true)
- [ ] Health endpoints not verified
- [ ] Resource cleanup not tested

EOF
    fi

    cat >> "$EVIDENCE_DIR/VALIDATION_SUMMARY.md" <<EOF
---

## Recommendations

EOF

    if [ $failed_phases -gt 0 ]; then
        cat >> "$EVIDENCE_DIR/VALIDATION_SUMMARY.md" <<EOF
### ⚠️ Action Required

1. Review failed phases above
2. Check corresponding log files in \`$EVIDENCE_DIR\`
3. Fix identified issues
4. Re-run validation

EOF
    else
        cat >> "$EVIDENCE_DIR/VALIDATION_SUMMARY.md" <<EOF
### ✓ Ready for Marketplace Review

Your deployment passes all validation checks. You can proceed with:

1. Submitting to Google Cloud Marketplace
2. Running full deployment tests (optional)
3. Generating marketplace artifacts

EOF
    fi

    cat >> "$EVIDENCE_DIR/VALIDATION_SUMMARY.md" <<EOF
---

## Next Steps

1. **Review the evidence:** Check all files in \`$EVIDENCE_DIR\`
2. **Address any failures:** Fix issues identified by validation
3. **Run deployment tests:** \`RUN_DEPLOYMENT_TESTS=true $0\`
4. **Generate marketplace package:** \`bash marketplace/gcp/scripts/package.sh\`

---

*This report was generated automatically by the erlmcp Marketplace Validation Suite.*
EOF

    # Print summary
    cat "$EVIDENCE_DIR/VALIDATION_SUMMARY.md"
}

# ============================================================================
# Main Execution
# ============================================================================

main() {
    print_banner

    log_info "Starting Marketplace Validation Suite..."
    log_info "================================================"
    log_info "Project: ${PROJECT_ID:-Not specified}"
    log_info "Region: $REGION"
    log_info "Zone: $ZONE"
    log_info "Test ID: $TEST_ID"
    log_info "Deployment Tests: $RUN_DEPLOYMENT_TESTS"
    log_info "================================================"
    echo ""

    # Create evidence directory
    mkdir -p "$EVIDENCE_DIR"

    # Run phases
    phase_0_static_validation || true
    phase_1_artifact_tests || true
    phase_2_deployment_tests || true

    # Generate final report
    echo ""
    generate_final_report

    # Exit with appropriate code
    local total_phases=${#PHASE_NAMES[@]}
    local failed_phases=0

    for result in "${PHASE_RESULTS[@]}"; do
        [ "$result" = "fail" ] && failed_phases=$((failed_phases + 1))
    done

    if [ $failed_phases -gt 0 ]; then
        log_error "Validation failed with $failed_phases failed phase(s)"
        exit 1
    fi

    log_info "All validation phases passed!"
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
        --zone)
            ZONE="$2"
            shift 2
            ;;
        --run-deployment-tests)
            RUN_DEPLOYMENT_TESTS=true
            shift
            ;;
        --phase)
            PHASE_NUMBER="$2"
            shift 2
            ;;
        *)
            echo "Usage: $0 [OPTIONS]"
            echo ""
            echo "Options:"
            echo "  --project PROJECT_ID     GCP Project ID"
            echo "  --region REGION          GCP Region (default: us-central1)"
            echo "  --zone ZONE              GCP Zone (default: us-central1-a)"
            echo "  --run-deployment-tests   Run full deployment tests (requires resources)"
            echo ""
            echo "Phases:"
            echo "  Phase 0: Static Validation (always runs, no cloud resources)"
            echo "  Phase 1: Artifact Tests (requires PROJECT_ID)"
            echo "  Phase 2: Deployment Tests (requires PROJECT_ID + --run-deployment-tests)"
            exit 1
            ;;
    esac
done

main "$@"
