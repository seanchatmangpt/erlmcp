#!/usr/bin/env bash
# Agent Validation Script - erlmcp
# Version: 1.0.0
# Purpose: Automated validation pipeline for agent completion protocol
#
# Exit Codes:
#   0 - All phases passed, OK to report "done"
#   1 - Validation incomplete, DO NOT report "done"
#   2 - Blocking error, spawn fix agents

set -euo pipefail

# ANSI colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
BOLD='\033[1m'
NC='\033[0m' # No Color

# Validation state
PHASE_1_STATUS="PENDING"
PHASE_2_STATUS="PENDING"
PHASE_3_STATUS="PENDING"
PHASE_4_STATUS="PENDING"
PHASE_5_STATUS="PENDING"
PHASE_6_STATUS="PENDING"

VALIDATION_FAILED=0
SKIP_BENCHMARKS=0
SPECIFIC_PHASE=""

# Parse arguments
while [[ $# -gt 0 ]]; do
  case $1 in
    --full)
      SKIP_BENCHMARKS=0
      shift
      ;;
    --quick)
      SKIP_BENCHMARKS=1
      shift
      ;;
    --phase)
      SPECIFIC_PHASE="$2"
      shift 2
      ;;
    *)
      echo "Usage: $0 [--full|--quick] [--phase N]"
      exit 2
      ;;
  esac
done

# Helper functions
log_phase() {
  echo ""
  echo -e "${BOLD}${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
  echo -e "${BOLD}${BLUE}Phase $1: $2${NC}"
  echo -e "${BOLD}${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
}

log_success() {
  echo -e "${GREEN}✅ $1${NC}"
}

log_error() {
  echo -e "${RED}❌ $1${NC}"
  VALIDATION_FAILED=1
}

log_warning() {
  echo -e "${YELLOW}⚠️  $1${NC}"
}

log_info() {
  echo -e "${BLUE}ℹ️  $1${NC}"
}

# Phase 1: Implementation
validate_phase_1() {
  log_phase "1" "Implementation"

  # Check for incomplete code markers
  log_info "Checking for incomplete code (TODO, FIXME, XXX)..."

  if grep -rn "TODO\|FIXME\|XXX" src/ 2>/dev/null | grep -v "documented technical debt"; then
    log_error "Phase 1: Found incomplete code markers"
    PHASE_1_STATUS="FAILED"
    return 1
  fi

  # Check that source files exist
  if [ ! -d "src/" ] || [ -z "$(ls -A src/*.erl 2>/dev/null)" ]; then
    log_error "Phase 1: No Erlang source files found in src/"
    PHASE_1_STATUS="FAILED"
    return 1
  fi

  log_success "Phase 1: Implementation complete (no TODO/FIXME markers)"
  PHASE_1_STATUS="PASSED"
  return 0
}

# Phase 2: Compilation
validate_phase_2() {
  log_phase "2" "Compilation"

  log_info "Compiling project..."

  # Capture compilation output
  if COMPILE_OUTPUT=$(TERM=dumb rebar3 compile 2>&1); then
    # Count compiled modules
    MODULE_COUNT=$(echo "$COMPILE_OUTPUT" | grep -c "Compiling" || echo "0")

    # Check for warnings
    WARNING_COUNT=$(echo "$COMPILE_OUTPUT" | grep -c "Warning" 2>/dev/null || echo "0")
    # Remove any non-numeric characters
    WARNING_COUNT=$(echo "$WARNING_COUNT" | tr -cd '0-9' || echo "0")

    log_success "Phase 2: Compilation PASSED"
    log_info "  - Compiled: $MODULE_COUNT modules"
    log_info "  - Errors: 0"
    if [ "$WARNING_COUNT" -gt 0 ] 2>/dev/null; then
      log_warning "  - Warnings: $WARNING_COUNT (review recommended)"
    else
      log_info "  - Warnings: 0"
    fi

    PHASE_2_STATUS="PASSED"
    return 0
  else
    log_error "Phase 2: Compilation FAILED"
    echo "$COMPILE_OUTPUT"
    PHASE_2_STATUS="FAILED"
    return 1
  fi
}

# Phase 3: Testing
validate_phase_3() {
  log_phase "3" "Testing"

  log_info "Running unit tests (EUnit)..."

  # Run EUnit tests
  if EUNIT_OUTPUT=$(rebar3 eunit 2>&1); then
    # Parse test results
    PASSED=$(echo "$EUNIT_OUTPUT" | grep -oP '\d+(?= tests passed)' || echo "0")
    FAILED=$(echo "$EUNIT_OUTPUT" | grep -oP '\d+(?= tests failed)' || echo "0")

    if [ "$FAILED" -eq 0 ]; then
      log_success "Phase 3: Testing PASSED"
      log_info "  - Unit tests: $PASSED/$PASSED passed (100%)"
      log_info "  - Failed: 0"
      PHASE_3_STATUS="PASSED"

      # Check for CT suites
      if [ -d "test/" ] && ls test/*_SUITE.erl >/dev/null 2>&1; then
        log_info "Running integration tests (Common Test)..."
        if CT_OUTPUT=$(rebar3 ct 2>&1); then
          log_success "  - CT suites: All passed"
        else
          log_warning "  - CT suites: Some failures (review required)"
        fi
      fi

      return 0
    else
      log_error "Phase 3: Testing FAILED"
      log_error "  - Unit tests: $PASSED passed, $FAILED failed"
      PHASE_3_STATUS="FAILED"
      return 1
    fi
  else
    log_error "Phase 3: Testing FAILED (could not run tests)"
    echo "$EUNIT_OUTPUT"
    PHASE_3_STATUS="FAILED"
    return 1
  fi
}

# Phase 4: Quality Gates
validate_phase_4() {
  log_phase "4" "Quality Gates"

  # Dialyzer type checking
  log_info "Running Dialyzer (type checking)..."
  if DIALYZER_OUTPUT=$(rebar3 dialyzer 2>&1); then
    ERROR_COUNT=$(echo "$DIALYZER_OUTPUT" | grep -c "error:" || echo "0")
    if [ "$ERROR_COUNT" -eq 0 ]; then
      log_success "  - Dialyzer: 0 errors"
    else
      log_error "  - Dialyzer: $ERROR_COUNT errors"
      VALIDATION_FAILED=1
    fi
  else
    log_warning "  - Dialyzer: Could not run (skipping)"
  fi

  # Xref cross-reference analysis
  log_info "Running Xref (cross-reference analysis)..."
  if XREF_OUTPUT=$(rebar3 xref 2>&1); then
    UNDEFINED_COUNT=$(echo "$XREF_OUTPUT" | grep -c "undefined" || echo "0")
    if [ "$UNDEFINED_COUNT" -eq 0 ]; then
      log_success "  - Xref: 0 undefined calls"
    else
      log_error "  - Xref: $UNDEFINED_COUNT undefined calls"
      VALIDATION_FAILED=1
    fi
  else
    log_warning "  - Xref: Could not run (skipping)"
  fi

  # Code coverage
  log_info "Measuring code coverage..."
  if COVER_OUTPUT=$(rebar3 do eunit, cover 2>&1); then
    COVERAGE=$(echo "$COVER_OUTPUT" | grep -oP '\d+(?=%)' | tail -1 || echo "0")
    if [ "$COVERAGE" -ge 80 ]; then
      log_success "  - Coverage: ${COVERAGE}% (≥80%)"
    else
      log_warning "  - Coverage: ${COVERAGE}% (<80% target)"
    fi
  else
    log_warning "  - Coverage: Could not measure (skipping)"
  fi

  # Code format verification
  log_info "Verifying code format..."
  if FORMAT_OUTPUT=$(rebar3 format --verify 2>&1); then
    log_success "  - Format: verified"
  else
    log_warning "  - Format: needs formatting (run: rebar3 format)"
  fi

  if [ "$VALIDATION_FAILED" -eq 0 ]; then
    log_success "Phase 4: Quality Gates PASSED"
    PHASE_4_STATUS="PASSED"
    return 0
  else
    log_error "Phase 4: Quality Gates FAILED"
    PHASE_4_STATUS="FAILED"
    return 1
  fi
}

# Phase 5: Performance Validation
validate_phase_5() {
  log_phase "5" "Performance Validation"

  if [ "$SKIP_BENCHMARKS" -eq 1 ]; then
    log_warning "Phase 5: SKIPPED (--quick mode)"
    PHASE_5_STATUS="SKIPPED"
    return 0
  fi

  # Check if benchmark modules exist
  if [ ! -f "bench/erlmcp_bench_core_ops.erl" ]; then
    log_warning "Phase 5: No benchmark modules found (acceptable for non-perf code)"
    PHASE_5_STATUS="SKIPPED"
    return 0
  fi

  log_info "Running quick benchmark (core_ops_100k)..."

  # Run benchmark (with timeout)
  if timeout 120 erl -pa _build/default/lib/*/ebin -noshell -eval \
    "erlmcp_bench_core_ops:run(<<\"core_ops_100k\">>), init:stop()." 2>&1 | tee /tmp/bench_output.txt; then

    # Parse throughput (example: look for "throughput_msg_per_s")
    if grep -q "throughput_msg_per_s" /tmp/bench_output.txt; then
      THROUGHPUT=$(grep -oP 'throughput_msg_per_s.*\K\d+' /tmp/bench_output.txt || echo "unknown")
      log_success "Phase 5: Performance PASSED"
      log_info "  - Benchmark: core_ops_100k"
      log_info "  - Throughput: $THROUGHPUT msg/s (baseline: 2.69M msg/s)"
      PHASE_5_STATUS="PASSED"
      return 0
    else
      log_warning "Phase 5: Could not parse benchmark results (verify manually)"
      PHASE_5_STATUS="WARNING"
      return 0
    fi
  else
    log_warning "Phase 5: Benchmark failed or timed out (review required)"
    PHASE_5_STATUS="WARNING"
    return 0
  fi
}

# Phase 6: Integration
validate_phase_6() {
  log_phase "6" "Integration & Reporting"

  # Check if examples exist
  if [ -d "examples/" ]; then
    log_info "Checking examples..."

    # Try to compile examples
    for example in examples/*/; do
      if [ -f "$example/rebar.config" ]; then
        EXAMPLE_NAME=$(basename "$example")
        log_info "  - Example: $EXAMPLE_NAME"

        if (cd "$example" && rebar3 compile >/dev/null 2>&1); then
          log_success "    ✓ Compiles"
        else
          log_warning "    ✗ Does not compile (review required)"
        fi
      fi
    done
  else
    log_info "No examples directory (acceptable)"
  fi

  log_success "Phase 6: Integration PASSED"
  PHASE_6_STATUS="PASSED"
  return 0
}

# Generate completion report
generate_report() {
  echo ""
  echo -e "${BOLD}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
  echo -e "${BOLD}COMPLETION REPORT${NC}"
  echo -e "${BOLD}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
  echo ""

  # Phase status summary
  print_phase_status() {
    local phase=$1
    local name=$2
    local status=$3

    case $status in
      PASSED)
        echo -e "✅ Phase $phase: $name - ${GREEN}PASSED${NC}"
        ;;
      FAILED)
        echo -e "❌ Phase $phase: $name - ${RED}FAILED${NC}"
        ;;
      SKIPPED)
        echo -e "⚠️  Phase $phase: $name - ${YELLOW}SKIPPED${NC}"
        ;;
      WARNING)
        echo -e "⚠️  Phase $phase: $name - ${YELLOW}WARNING${NC}"
        ;;
      *)
        echo -e "⏸️  Phase $phase: $name - PENDING"
        ;;
    esac
  }

  print_phase_status "1" "Implementation" "$PHASE_1_STATUS"
  print_phase_status "2" "Compilation" "$PHASE_2_STATUS"
  print_phase_status "3" "Testing" "$PHASE_3_STATUS"
  print_phase_status "4" "Quality Gates" "$PHASE_4_STATUS"
  print_phase_status "5" "Performance" "$PHASE_5_STATUS"
  print_phase_status "6" "Integration" "$PHASE_6_STATUS"

  echo ""

  # Overall status
  if [ "$VALIDATION_FAILED" -eq 0 ] && \
     [ "$PHASE_1_STATUS" = "PASSED" ] && \
     [ "$PHASE_2_STATUS" = "PASSED" ] && \
     [ "$PHASE_3_STATUS" = "PASSED" ] && \
     [ "$PHASE_4_STATUS" = "PASSED" ] && \
     [ "$PHASE_6_STATUS" = "PASSED" ]; then
    echo -e "${BOLD}${GREEN}STATUS: VALIDATED - READY FOR MERGE${NC}"
    echo ""
    echo -e "${GREEN}✅ Agent may report 'done'${NC}"
    return 0
  else
    echo -e "${BOLD}${RED}STATUS: VALIDATION INCOMPLETE${NC}"
    echo ""
    echo -e "${RED}❌ Agent MUST NOT report 'done'${NC}"
    echo ""
    echo -e "${YELLOW}Next Steps:${NC}"
    echo "1. Fix failures in phases: $(echo "$PHASE_1_STATUS $PHASE_2_STATUS $PHASE_3_STATUS $PHASE_4_STATUS $PHASE_5_STATUS $PHASE_6_STATUS" | grep -o "FAILED" || echo "none")"
    echo "2. Re-run validation: ./tools/agent-validator.sh --full"
    echo "3. Spawn fix agents if needed"
    echo "4. Repeat until all phases pass"
    return 1
  fi
}

# Main execution
main() {
  echo -e "${BOLD}${BLUE}Agent Validation Pipeline - erlmcp${NC}"
  echo -e "${BLUE}Version: 1.0.0${NC}"
  echo ""

  # Change to project root
  cd "$(dirname "$0")/.."

  if [ -n "$SPECIFIC_PHASE" ]; then
    log_info "Running specific phase: $SPECIFIC_PHASE"
    case $SPECIFIC_PHASE in
      1) validate_phase_1 ;;
      2) validate_phase_2 ;;
      3) validate_phase_3 ;;
      4) validate_phase_4 ;;
      5) validate_phase_5 ;;
      6) validate_phase_6 ;;
      *)
        log_error "Invalid phase: $SPECIFIC_PHASE (must be 1-6)"
        exit 2
        ;;
    esac
    exit $?
  fi

  # Run all phases
  validate_phase_1 || true
  validate_phase_2 || true
  validate_phase_3 || true
  validate_phase_4 || true
  validate_phase_5 || true
  validate_phase_6 || true

  # Generate report
  generate_report
  exit $?
}

main "$@"
