#!/bin/bash

################################################################################
# FMEA Dashboard Generator & CI Gate
#
# Purpose: Generate FMEA reports and validate security failure mode closure
# Usage:   ./scripts/validation/generate_fmea_dashboard.sh [--gate] [--threshold N]
#
# CI Gate Mode: Fails if any FM with RPN ≥ threshold has no passing tests
# Report Mode: Generates machine-readable FMEA closure report
#
################################################################################

set -euo pipefail

# Configuration
FMEA_JSON="docs/fmea/fmea_security.json"
REPORTS_DIR="reports"
GATE_MODE=false
RPN_THRESHOLD=250

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'  # No Color

# Parse arguments
while [[ $# -gt 0 ]]; do
  case $1 in
    --gate)
      GATE_MODE=true
      shift
      ;;
    --threshold)
      RPN_THRESHOLD=$2
      shift 2
      ;;
    *)
      echo "Unknown option: $1"
      echo "Usage: $0 [--gate] [--threshold N]"
      exit 1
      ;;
  esac
done

# Ensure reports directory
mkdir -p "$REPORTS_DIR"

# Verify FMEA JSON exists
if [ ! -f "$FMEA_JSON" ]; then
  echo -e "${RED}❌ FMEA registry not found: $FMEA_JSON${NC}"
  exit 1
fi

echo -e "${BLUE}═══════════════════════════════════════════════════════════════${NC}"
echo -e "${BLUE}FMEA Dashboard Generator${NC}"
echo -e "${BLUE}═══════════════════════════════════════════════════════════════${NC}"
echo ""

# Extract FMEA metadata
TIMESTAMP=$(date -u +"%Y-%m-%dT%H:%M:%SZ")
echo -e "Generated: ${TIMESTAMP}"
echo -e "FMEA Registry: ${FMEA_JSON}"
echo -e "Threshold: RPN ≥ ${RPN_THRESHOLD}"
echo ""

# Initialize report
REPORT_FILE="$REPORTS_DIR/fmea_rpn_report.json"
CLOSURE_FILE="$REPORTS_DIR/fmea_closure_status.json"

# Parse FMEA JSON and run tests
declare -A fm_status
declare -A fm_rpn
declare -A fm_priority
declare -i critical_count=0
declare -i critical_passed=0
declare -i critical_failed=0
declare -a failing_fms

echo -e "${BLUE}Running FM Closure Tests${NC}"
echo "────────────────────────────────────────────────────────────────"

# Use jq to iterate FMs
jq -r '.failure_modes[] | "\(.id)|\(.title)|\(.rpn)|\(.priority)|\(.test_paths | join(":"))"' "$FMEA_JSON" | while IFS='|' read -r fm_id fm_title fm_rpn fm_priority test_paths_raw; do
  fm_status["$fm_id"]="UNKNOWN"
  fm_rpn["$fm_id"]=$fm_rpn
  fm_priority["$fm_id"]=$fm_priority

  # Track critical modes
  if [ "$fm_rpn" -ge "$RPN_THRESHOLD" ]; then
    ((critical_count++))
  fi

  # Split test paths and run each
  IFS=':' read -ra test_paths <<< "$test_paths_raw"
  test_passed=false

  for test_path in "${test_paths[@]}"; do
    if [ -z "$test_path" ]; then
      continue
    fi

    # Extract module name from path (last part, remove .erl)
    module_name="${test_path##*/}"
    module_name="${module_name%.erl}"

    # Skip CI tests (handled separately)
    if [ "$module_name" = "CI" ] || [ "$module_name" = "dependency-audit" ]; then
      echo -e "${YELLOW}⚠️  ${fm_id}: ${module_name} (CI test, skipping)${NC}"
      continue
    fi

    # Try to run the test
    if command -v rebar3 &> /dev/null; then
      echo -n "  ${fm_id}: Testing ${module_name}... "

      # Run test silently, capture result
      if rebar3 eunit --module="$module_name" &>/dev/null 2>&1; then
        echo -e "${GREEN}✅ PASSED${NC}"
        fm_status["$fm_id"]="PASSED"
        test_passed=true

        # Track critical passes
        if [ "$fm_rpn" -ge "$RPN_THRESHOLD" ]; then
          ((critical_passed++))
        fi
        break  # Move to next FM (first passing test is enough)
      else
        echo -e "${RED}❌ FAILED${NC}"
      fi
    else
      echo -e "${YELLOW}⚠️  ${fm_id}: rebar3 not found, skipping test${NC}"
    fi
  done

  # Report status
  if ! $test_passed; then
    fm_status["$fm_id"]="FAILED"
    if [ "$fm_rpn" -ge "$RPN_THRESHOLD" ]; then
      ((critical_failed++))
      failing_fms+=("$fm_id:${fm_rpn}:${fm_title}")
    fi
  fi

  # Pretty print summary
  case "${fm_status[$fm_id]}" in
    PASSED)
      icon="${GREEN}✅${NC}"
      ;;
    FAILED)
      icon="${RED}❌${NC}"
      ;;
    *)
      icon="${YELLOW}⚠️ ${NC}"
      ;;
  esac

  printf "${icon} %-8s RPN=%3d (%s) %-40s %s\n" \
    "$fm_id" "$fm_rpn" "$fm_priority" "${fm_title:0:40}" "${fm_status[$fm_id]}"
done

echo ""
echo "────────────────────────────────────────────────────────────────"

# Generate JSON reports
generate_json_reports() {
  local total_fms=$(jq '.failure_modes | length' "$FMEA_JSON")
  local gate_status="FAILED"

  if [ "$critical_failed" -eq 0 ]; then
    gate_status="PASSED"
  fi

  # RPN Report
  cat > "$REPORT_FILE" <<EOF
{
  "generated": "$TIMESTAMP",
  "fmea_version": "1.0",
  "summary": {
    "total_fms": $total_fms,
    "critical_count": $critical_count,
    "critical_passed": $critical_passed,
    "critical_failed": $critical_failed,
    "threshold": $RPN_THRESHOLD
  },
  "gate_result": {
    "status": "$gate_status",
    "failing_fms": [
      $(for fm in "${failing_fms[@]}"; do
        IFS=':' read -r fm_id rpn title <<< "$fm"
        echo "      {\"id\": \"$fm_id\", \"rpn\": $rpn, \"title\": \"$title\"}"
      done | paste -sd ',' -)
    ]
  }
}
EOF
}

generate_json_reports

echo -e "${BLUE}Summary${NC}"
echo "────────────────────────────────────────────────────────────────"
echo -e "  Critical FMs (RPN ≥ $RPN_THRESHOLD): ${critical_count} total"
echo -e "    ✅ Passed: ${GREEN}${critical_passed}${NC}"
echo -e "    ❌ Failed: ${RED}${critical_failed}${NC}"
echo ""

# Gate decision
if [ "$critical_failed" -eq 0 ]; then
  echo -e "${GREEN}✅ FMEA GATE PASSED${NC}"
  echo "All critical failure modes have passing tests."
  echo ""
  echo "Report generated:"
  echo "  - ${REPORT_FILE}"
  exit 0
else
  echo -e "${RED}❌ FMEA GATE FAILED${NC}"
  echo "Critical FMs without passing tests:"
  for fm in "${failing_fms[@]}"; do
    echo "  - $fm"
  done
  echo ""
  echo "Action: Run failing tests and fix issues before committing"
  echo "  rebar3 do eunit, ct"
  echo ""
  echo "Report generated:"
  echo "  - ${REPORT_FILE}"

  if [ "$GATE_MODE" = true ]; then
    exit 1  # Block merge in CI
  else
    exit 0  # Report-only mode
  fi
fi
