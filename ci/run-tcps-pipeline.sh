#!/bin/bash
#
# TCPS Production Pipeline - Complete CI/CD Flow
# Toyota Code Production System - Automated Pipeline Execution
#
# This script implements the full TCPS workflow:
#   1. Pull Signal (Work Order)
#   2. SHACL Validation (Jidoka)
#   3. Compile
#   4. Test (Quality Gates)
#   5. Deterministic Build Verification
#   6. Release
#   7. Kaizen Report
#

set -e  # Stop on any error (Jidoka - stop-the-line)

# ============================================================================
# Configuration
# ============================================================================
TCPS_QUALITY_GATE_COVERAGE="${TCPS_QUALITY_GATE_COVERAGE:-80}"
TCPS_QUALITY_GATE_PASS_RATE="${TCPS_QUALITY_GATE_PASS_RATE:-80}"
TCPS_ENABLE_ANDON="${TCPS_ENABLE_ANDON:-true}"
TCPS_ENABLE_RECEIPTS="${TCPS_ENABLE_RECEIPTS:-true}"

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Timing
PIPELINE_START=$(date -u +%s)

# Work Order ID
WORK_ORDER_ID="WO-$(date +%Y%m%d-%H%M%S)-$(git rev-parse --short HEAD 2>/dev/null || echo 'local')"

# Output directory for receipts
RECEIPTS_DIR="tcps_receipts"
mkdir -p "$RECEIPTS_DIR"

# ============================================================================
# Helper Functions
# ============================================================================

log_header() {
    echo ""
    echo -e "${BLUE}=========================================${NC}"
    echo -e "${BLUE}$1${NC}"
    echo -e "${BLUE}=========================================${NC}"
}

log_success() {
    echo -e "${GREEN}✅ $1${NC}"
}

log_error() {
    echo -e "${RED}❌ $1${NC}"
}

log_warning() {
    echo -e "${YELLOW}⚠️  $1${NC}"
}

log_info() {
    echo -e "${BLUE}ℹ️  $1${NC}"
}

trigger_andon() {
    local type=$1
    local description=$2

    if [ "$TCPS_ENABLE_ANDON" != "true" ]; then
        return
    fi

    echo ""
    echo -e "${RED}=========================================${NC}"
    echo -e "${RED}🚨 ANDON TRIGGERED${NC}"
    echo -e "${RED}=========================================${NC}"
    echo -e "${RED}Type: ${type}${NC}"
    echo -e "${RED}Description: ${description}${NC}"
    echo -e "${RED}Work Order: ${WORK_ORDER_ID}${NC}"
    echo -e "${RED}=========================================${NC}"

    # Create Andon RDF event
    cat > "${RECEIPTS_DIR}/andon_${type}.ttl" <<EOF
@prefix tcps: <http://purl.org/tcps#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

<urn:tcps:andon:$(date +%s)> a tcps:AndonEvent ;
    tcps:severity "critical" ;
    tcps:type "${type}" ;
    tcps:triggeredAt "$(date -u +%Y-%m-%dT%H:%M:%SZ)"^^xsd:dateTime ;
    tcps:description "${description}" ;
    tcps:affectsWorkOrder <urn:tcps:work-order:${WORK_ORDER_ID}> .
EOF

    # Optional: Send notifications (implement as needed)
    # send_slack_notification "$type" "$description"
    # send_email_notification "$type" "$description"
}

create_receipt() {
    local stage=$1
    local passed=$2
    shift 2
    local extra_fields="$@"

    if [ "$TCPS_ENABLE_RECEIPTS" != "true" ]; then
        return
    fi

    cat > "${RECEIPTS_DIR}/${stage}_receipt.json" <<EOF
{
  "stage": "${stage}",
  "work_order": "${WORK_ORDER_ID}",
  "timestamp": "$(date -u +%Y-%m-%dT%H:%M:%SZ)",
  "passed": ${passed},
  "operator": "tcps-pipeline",
  "git_sha": "$(git rev-parse HEAD 2>/dev/null || echo 'unknown')"
  ${extra_fields:+,$extra_fields}
}
EOF

    log_info "Receipt created: ${RECEIPTS_DIR}/${stage}_receipt.json"
}

# ============================================================================
# STAGE 1: Pull Signal - Create Work Order
# ============================================================================

log_header "TCPS PRODUCTION PIPELINE"
echo "Work Order: ${WORK_ORDER_ID}"
echo "Started: $(date -u +%Y-%m-%dT%H:%M:%SZ)"
echo ""

log_header "STAGE 1: Pull Signal - Create Work Order"

cat > "${RECEIPTS_DIR}/work_order.ttl" <<EOF
@prefix tcps: <http://purl.org/tcps#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

<urn:tcps:work-order:${WORK_ORDER_ID}> a tcps:WorkOrder ;
    tcps:orderID "${WORK_ORDER_ID}" ;
    tcps:priority "normal" ;
    tcps:createdAt "$(date -u +%Y-%m-%dT%H:%M:%SZ)"^^xsd:dateTime ;
    tcps:assignedTo <urn:tcps:worker:tcps-pipeline> ;
    tcps:status "in_progress" .
EOF

log_success "Work order created"

# ============================================================================
# STAGE 2: SHACL Validation - Jidoka
# ============================================================================

log_header "STAGE 2: SHACL Validation (Jidoka)"

if [ -f "tests/shacl/test_tcps_validation.py" ]; then
    if python3 -m pytest tests/shacl/test_tcps_validation.py -v --tb=short; then
        log_success "SHACL validation passed"
        create_receipt "shacl_validation" "true"
    else
        log_error "SHACL validation FAILED"
        create_receipt "shacl_validation" "false"
        trigger_andon "shacl_violation" "SHACL validation failed"
        exit 1
    fi
else
    log_warning "SHACL tests not found, skipping"
fi

# ============================================================================
# STAGE 3: Compile
# ============================================================================

log_header "STAGE 3: Compile"

if rebar3 compile; then
    log_success "Compilation successful"
    COMPILE_PASSED="true"
else
    log_error "Compilation FAILED"
    COMPILE_PASSED="false"
    create_receipt "compile" "false"
    trigger_andon "compilation_error" "Compilation failed"
    exit 1
fi

# Run Dialyzer
log_info "Running Dialyzer type checking..."
if rebar3 dialyzer; then
    log_success "Dialyzer passed"
    DIALYZER_PASSED="true"
else
    log_warning "Dialyzer warnings detected"
    DIALYZER_PASSED="false"
fi

create_receipt "compile" "$COMPILE_PASSED" "\"dialyzer_passed\": $DIALYZER_PASSED"

# ============================================================================
# STAGE 4: Test - Quality Gates
# ============================================================================

log_header "STAGE 4: Test (Quality Gates)"

# Run tests
if rebar3 as test eunit --verbose; then
    log_success "Tests passed"
    TESTS_PASSED="true"
else
    log_error "Tests FAILED"
    TESTS_PASSED="false"
    create_receipt "test" "false" "\"coverage_percent\": 0"
    trigger_andon "test_failure" "Unit tests failed"
    exit 1
fi

# Coverage
log_info "Generating coverage report..."
rebar3 cover --verbose

# Extract coverage (simplified - adjust for your setup)
COVERAGE=$(rebar3 cover 2>&1 | grep -oP 'Total: \K[0-9]+' || echo "0")

log_info "Coverage: ${COVERAGE}%"

# Quality Gate Check
if [ "$COVERAGE" -lt "$TCPS_QUALITY_GATE_COVERAGE" ]; then
    log_error "Quality gate FAILED: Coverage ${COVERAGE}% < ${TCPS_QUALITY_GATE_COVERAGE}%"
    QUALITY_GATE_PASSED="false"
    create_receipt "test" "false" "\"coverage_percent\": ${COVERAGE}, \"quality_gate_passed\": false"
    trigger_andon "quality_gate_failure" "Coverage ${COVERAGE}% below threshold ${TCPS_QUALITY_GATE_COVERAGE}%"
    exit 1
else
    log_success "Quality gate PASSED: Coverage ${COVERAGE}% >= ${TCPS_QUALITY_GATE_COVERAGE}%"
    QUALITY_GATE_PASSED="true"
fi

create_receipt "test" "true" "\"coverage_percent\": ${COVERAGE}, \"quality_gate_passed\": true, \"min_coverage_required\": ${TCPS_QUALITY_GATE_COVERAGE}"

# ============================================================================
# STAGE 5: Deterministic Build Verification
# ============================================================================

log_header "STAGE 5: Deterministic Build Verification"

log_info "Building twice to verify determinism..."

# Build 1
rebar3 clean
rebar3 as prod compile
find _build/prod -name "*.beam" -type f -exec sha256sum {} \; | sort > build1.sha256

# Build 2
rebar3 clean
rebar3 as prod compile
find _build/prod -name "*.beam" -type f -exec sha256sum {} \; | sort > build2.sha256

# Compare
if diff build1.sha256 build2.sha256 > /dev/null 2>&1; then
    log_success "Build is DETERMINISTIC"
    DETERMINISTIC="true"
else
    log_error "Build is NOT deterministic"
    echo "Differences:"
    diff build1.sha256 build2.sha256 || true
    DETERMINISTIC="false"
    trigger_andon "non_deterministic_build" "Build produces different outputs"
    exit 1
fi

mv build1.sha256 "${RECEIPTS_DIR}/"
mv build2.sha256 "${RECEIPTS_DIR}/"

# ============================================================================
# STAGE 6: Release (Optional - only on main/release branches)
# ============================================================================

log_header "STAGE 6: Release"

CURRENT_BRANCH=$(git rev-parse --abbrev-ref HEAD 2>/dev/null || echo "unknown")

if [[ "$CURRENT_BRANCH" == "main" ]] || [[ "$CURRENT_BRANCH" == release/* ]]; then
    log_info "Creating production release..."

    rebar3 as prod tar

    RELEASE_TAR=$(find _build/prod/rel -name "*.tar.gz" | head -1)
    RELEASE_SHA256=$(sha256sum "$RELEASE_TAR" | cut -d' ' -f1)

    log_success "Release created: $RELEASE_TAR"
    log_info "SHA256: $RELEASE_SHA256"

    create_receipt "release" "true" "\"release_file\": \"${RELEASE_TAR}\", \"sha256\": \"${RELEASE_SHA256}\""
else
    log_info "Skipping release (not on main/release branch)"
fi

# ============================================================================
# STAGE 7: Kaizen Report - Continuous Improvement
# ============================================================================

log_header "STAGE 7: Kaizen Report"

PIPELINE_END=$(date -u +%s)
TAKT_TIME=$((PIPELINE_END - PIPELINE_START))

log_info "Takt Time: ${TAKT_TIME} seconds"

cat > "${RECEIPTS_DIR}/kaizen_report.md" <<EOF
# TCPS Kaizen Report

**Work Order**: ${WORK_ORDER_ID}
**Timestamp**: $(date -u +%Y-%m-%dT%H:%M:%SZ)
**Branch**: ${CURRENT_BRANCH}
**Commit**: $(git rev-parse HEAD 2>/dev/null || echo 'unknown')

## Metrics

- **Takt Time**: ${TAKT_TIME} seconds
- **Test Coverage**: ${COVERAGE}%
- **Quality Gate**: ${QUALITY_GATE_PASSED}
- **Deterministic Build**: ${DETERMINISTIC}

## Receipts

$(find "${RECEIPTS_DIR}" -name "*_receipt.json" -exec echo "### {}" \; -exec cat {} \; -exec echo "" \; 2>/dev/null || echo "No receipts found")

## Andon Events

$(find "${RECEIPTS_DIR}" -name "andon_*.ttl" -exec echo "### {}" \; -exec cat {} \; -exec echo "" \; 2>/dev/null || echo "No Andon events triggered ✅")

## Continuous Improvement Actions

- Monitor takt time trends
- Review Andon events for root causes
- Optimize stages taking > 10% of total time
- Update quality gate thresholds based on team capability

---

Generated by TCPS Pipeline
EOF

cat "${RECEIPTS_DIR}/kaizen_report.md"

# ============================================================================
# Summary
# ============================================================================

echo ""
log_header "PIPELINE COMPLETE"
log_success "Work Order ${WORK_ORDER_ID} completed successfully"
log_info "Receipts saved to: ${RECEIPTS_DIR}/"
log_info "Total time: ${TAKT_TIME} seconds"
echo ""

exit 0
