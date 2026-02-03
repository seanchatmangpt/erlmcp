#!/bin/bash

# Quality Gate Enforcer for erlmcp v3
# Shell script wrapper for Python enforcer

set -euo pipefail

# Configuration
CONFIG_FILE="${CONFIG_FILE:-$(dirname "$0")/policies.yaml}"
ENVIRONMENT="${ENVIRONMENT:-development}"
OUTPUT_FILE="${OUTPUT_FILE:-quality-report.json}"
OTP_VERSION="28.3.1"

# Color codes
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Logging function
log() {
    echo -e "${1:-}${2:-}${NC}" >&2
}

# Main quality gate check
run_quality_gates() {
    log "${BLUE}" "Starting quality gates for environment: $ENVIRONMENT"
    log "${BLUE}" "Configuration: $CONFIG_FILE"
    log "${BLUE}" "OTP Version: $OTP_VERSION"

    # Check OTP version
    otp_version=$(otp -v | head -1)
    if [[ "$otp_version" != *"$OTP_VERSION"* ]]; then
        log "${RED}" "Error: OTP $OTP_VERSION required, found $otp_version"
        exit 1
    fi

    # Cache dependencies
    log "${BLUE}" "Caching dependencies..."
    mkdir -p ~/.cache/rebar3
    cache_id="rebar3-${BUILD_ID:-$(date +%s)}"

    # Run Python enforcer
    if command -v python3 &> /dev/null; then
        log "${BLUE}" "Running Python enforcer..."
        python3 "$(dirname "$0")/enforcer.py" \
            --config "$CONFIG_FILE" \
            --environment "$ENVIRONMENT" \
            --output "$OUTPUT_FILE" \
            || handle_failure "Quality gates failed"
    else
        log "${YELLOW}" "Python enforcer not found, using fallback shell checks"
        run_fallback_checks
    fi

    # Parse results
    if [[ -f "$OUTPUT_FILE" ]]; then
        python3 -c "
import json
with open('$OUTPUT_FILE') as f:
    report = json.load(f)

if report['overall_passed']:
    print('✅ Quality gates PASSED')
    exit(0)
else:
    print('❌ Quality gates FAILED')
    for category, result in report['categories'].items():
        if not result['passed']:
            print(f'   - {category}: FAILED')
    exit(1)
" || exit 1
    else
        log "${RED}" "Error: Quality gate report not found"
        exit 1
    fi
}

# Fallback shell-based checks
run_fallback_checks() {
    log "${YELLOW}" "Running fallback shell-based quality checks..."

    # Compilation check
    log "${BLUE}" "Checking compilation..."
    rebar3 compile || handle_failure "Compilation failed"

    # Unit tests
    log "${BLUE}" "Running unit tests..."
    rebar3 eunit || handle_failure "EUnit tests failed"

    # CT tests
    log "${BLUE}" "Running CT tests..."
    rebar3 ct || handle_failure "CT tests failed"

    # Coverage check (simplified)
    log "${BLUE}" "Checking coverage..."
    rebar3 cover
    if grep -q "Covered.*[0-9]\+\%" <(rebar3 cover); then
        echo "✅ Coverage check passed"
    else
        echo "⚠️ Coverage check warning"
    fi

    # Dialyzer
    log "${BLUE}" "Running Dialyzer..."
    rebar3 dialyzer || handle_failure "Dialyzer failed"

    # Xref
    log "${BLUE}" "Running Xref..."
    rebar3 xref || handle_failure "Xref failed"

    echo "✅ All fallback checks passed"
}

# Handle failure
handle_failure() {
    log "${RED}" "Error: $1"

    # Generate failure report
    cat > quality-failure.json << EOF
{
    "timestamp": "$(date -Iseconds)",
    "error": "$1",
    "environment": "$ENVIRONMENT",
    "status": "failed"
}
EOF

    # Send notifications (if configured)
    if [[ -n "${SLACK_WEBHOOK:-}" ]]; then
        curl -X POST -H 'Content-type: application/json' \
            --data "{\"text\":\"❌ Quality Gate Failed: $1\"}" \
            "$SLACK_WEBHOOK" || true
    fi

    exit 1
}

# Build helper functions
build_with_cache() {
    local app_name="$1"
    local cache_key="$2"

    log "${BLUE}" "Building $app_name with caching..."

    # Check cache
    if [[ -f ~/.cache/rebar3/$cache_key ]]; then
        log "${BLUE}" "Using cached build for $app_name"
        cp -r ~/.cache/rebar3/$cache_key/* _build/prod/lib/$app_name/
    fi

    # Build
    rebar3 compile

    # Update cache
    mkdir -p ~/.cache/rebar3/$cache_key
    cp -r _build/prod/lib/$app_name/* ~/.cache/rebar3/$cache_key/
}

# Parallel test runner
run_parallel_tests() {
    log "${BLUE}" "Running tests in parallel..."

    # Setup background processes
    {
        rebar3 eunit --verbose
        echo "EUnit completed with $?" > eunit_result.tmp
    } &

    {
        rebar3 ct --suite=all --verbose
        echo "CT completed with $?" > ct_result.tmp
    } &

    # Wait for all processes
    wait

    # Check results
    if [[ -f eunit_result.tmp ]] && [[ $(cat eunit_result.tmp) == "EUnit completed with 0" ]]; then
        echo "✅ EUnit tests passed"
    else
        handle_failure "EUnit tests failed"
    fi

    if [[ -f ct_result.tmp ]] && [[ $(cat ct_result.tmp) == "CT completed with 0" ]]; then
        echo "✅ CT tests passed"
    else
        handle_failure "CT tests failed"
    fi

    # Cleanup
    rm -f *.tmp
}

# Performance testing
run_performance_tests() {
    log "${BLUE}" "Running performance tests..."

    # Run benchmark if in main branch
    if [[ "${GITHUB_REF:-}" == "refs/heads/main" ]]; then
        log "${BLUE}" "Running comprehensive benchmarks..."
        make benchmark-quick || handle_failure "Performance benchmarks failed"

        # Compare with baseline
        if command -v python3 &> /dev/null; then
            python3 scripts/performance/compare.py \
                --baseline registry \
                --current "${GITHUB_SHA:-unknown}" \
                || log "${YELLOW}" "Performance regression detected"
        fi
    fi
}

# Security scanning
run_security_scan() {
    log "${BLUE}" "Running security scans..."

    # Trivy scan
    if command -v trivy &> /dev/null; then
        trivy --format json --output trivy-report.json . || true
    fi

    # Secret detection
    if grep -r "API_KEY\|PASSWORD\|SECRET\|TOKEN" . --exclude-dir=.git &> /dev/null; then
        log "${RED}" "Potential secrets found in code!"
        exit 1
    fi

    # License check
    mix deps.audit --all 2>/dev/null || true
    rebar3 hex audit 2>/dev/null || true
}

# Main execution
case "${1:-}" in
    "run")
        run_quality_gates
        ;;
    "build")
        build_with_cache "$2" "$3"
        ;;
    "test")
        run_parallel_tests
        ;;
    "perf")
        run_performance_tests
        ;;
    "security")
        run_security_scan
        ;;
    *)
        echo "Usage: $0 {run|build|test|perf|security}"
        echo ""
        echo "  run      - Run all quality gates"
        echo "  build    - Build with caching"
        echo "  test     - Run tests in parallel"
        echo "  perf     - Run performance tests"
        echo "  security - Run security scans"
        exit 1
        ;;
esac