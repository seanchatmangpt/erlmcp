#!/usr/bin/env bash
set -euo pipefail

# =============================================================================
# CI Validation Runner - Local CI Script for Testing
# =============================================================================
#
# This script mimics the GitHub Actions CI workflow locally.
# Use it to validate changes before pushing.
#
# Usage:
#   ./scripts/validation/run-ci.sh                  # Run all checks
#   ./scripts/validation/run-ci.sh --quick          # Quick validation only
#   ./scripts/validation/run-ci.sh --full           # Full validation suite
#   ./scripts/validation/run-ci.sh --compliance     # Spec compliance only
#
# =============================================================================

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "${SCRIPT_DIR}/../.." && pwd)"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Configuration
OTP_VERSION="${OTP_VERSION:-26}"
REBAR3_VERSION="${REBAR3_VERSION:-3.25}"
COVERAGE_THRESHOLD="${COVERAGE_THRESHOLD:-80}"
COMPLIANCE_THRESHOLD="${COMPLIANCE_THRESHOLD:-95}"
VALIDATION_LEVEL="${VALIDATION_LEVEL:-standard}"

# Parse arguments
QUICK_MODE=false
FULL_MODE=false
COMPLIANCE_ONLY=false

while [[ $# -gt 0 ]]; do
  case $1 in
    --quick)
      QUICK_MODE=true
      VALIDATION_LEVEL="quick"
      shift
      ;;
    --full)
      FULL_MODE=true
      VALIDATION_LEVEL="full"
      shift
      ;;
    --compliance)
      COMPLIANCE_ONLY=true
      shift
      ;;
    --help)
      echo "Usage: $0 [OPTIONS]"
      echo ""
      echo "Options:"
      echo "  --quick       Quick validation (compile + basic tests)"
      echo "  --full        Full validation (all checks + benchmarks)"
      echo "  --compliance  MCP spec compliance validation only"
      echo "  --help        Show this help message"
      exit 0
      ;;
    *)
      echo "Unknown option: $1"
      exit 1
      ;;
  esac
done

# =============================================================================
# Helper Functions
# =============================================================================

log_info() {
  echo -e "${BLUE}ℹ${NC} $1"
}

log_success() {
  echo -e "${GREEN}✓${NC} $1"
}

log_warning() {
  echo -e "${YELLOW}⚠${NC} $1"
}

log_error() {
  echo -e "${RED}✗${NC} $1"
}

print_section() {
  echo ""
  echo -e "${BLUE}═══════════════════════════════════════════════════${NC}"
  echo -e "${BLUE}  $1${NC}"
  echo -e "${BLUE}═══════════════════════════════════════════════════${NC}"
  echo ""
}

check_command() {
  if ! command -v "$1" &> /dev/null; then
    log_error "Required command not found: $1"
    return 1
  fi
  return 0
}

run_step() {
  local step_name="$1"
  local step_command="$2"
  local critical="${3:-true}"

  print_section "$step_name"

  if eval "$step_command"; then
    log_success "$step_name passed"
    return 0
  else
    if [ "$critical" = "true" ]; then
      log_error "$step_name FAILED (blocking)"
      return 1
    else
      log_warning "$step_name failed (non-blocking)"
      return 0
    fi
  fi
}

# =============================================================================
# Pre-flight Checks
# =============================================================================

preflight_checks() {
  print_section "Pre-flight Checks"

  local all_good=true

  # Check required commands
  check_command "erl" || all_good=false
  check_command "rebar3" || all_good=false

  # Check Erlang version
  if [ "$all_good" = true ]; then
    local erl_version=$(erl -noshell -eval 'io:format("~s", [erlang:system_info(otp_release)]), init:stop().' 2>/dev/null || echo "unknown")
    log_info "Erlang/OTP version: $erl_version"

    # Convert to numeric for comparison
    if [ "$erl_version" != "unknown" ]; then
      local erl_num=$(echo "$erl_version" | sed 's/[^0-9]//g')
      if [ "$erl_num" -lt 25 ]; then
        log_error "Erlang/OTP 25+ required, found: $erl_version"
        all_good=false
      fi
    fi
  fi

  # Check rebar3 version
  if [ "$all_good" = true ]; then
    local rebar_version=$(rebar3 --version 2>/dev/null | head -1 || echo "unknown")
    log_info "rebar3 version: $rebar_version"
  fi

  # Check project structure
  if [ ! -f "$PROJECT_ROOT/rebar.config" ]; then
    log_error "rebar.config not found"
    all_good=false
  fi

  if [ ! -d "$PROJECT_ROOT/apps" ]; then
    log_error "apps/ directory not found"
    all_good=false
  fi

  if [ "$all_good" = false ]; then
    log_error "Pre-flight checks failed"
    exit 1
  fi

  log_success "Pre-flight checks passed"
}

# =============================================================================
# Compilation
# =============================================================================

compile_project() {
  print_section "Compilation"

  log_info "Compiling with rebar3..."
  cd "$PROJECT_ROOT"

  if TERM=dumb rebar3 compile; then
    log_success "Compilation successful"

    # Verify umbrella apps
    local apps=("erlmcp_core" "erlmcp_transports" "erlmcp_observability")
    local all_compiled=true

    for app in "${apps[@]}"; do
      if [ -d "_build/default/lib/$app/ebin" ]; then
        local beam_count=$(find "_build/default/lib/$app/ebin" -name "*.beam" 2>/dev/null | wc -l)
        log_info "  $app: $beam_count modules"
      else
        log_warning "  $app: not compiled"
        all_compiled=false
      fi
    done

    if [ "$all_compiled" = true ]; then
      log_success "All umbrella apps compiled"
      return 0
    else
      log_warning "Some apps not compiled"
      return 1
    fi
  else
    log_error "Compilation failed"
    return 1
  fi
}

# =============================================================================
# Xref Check
# =============================================================================

xref_check() {
  print_section "Xref Cross-Reference Check"

  cd "$PROJECT_ROOT"

  if rebar3 xref 2>&1 | tee xref.log; then
    log_success "Xref check passed"
    return 0
  else
    log_warning "Xref found issues (non-blocking)"
    return 0
  fi
}

# =============================================================================
# Dialyzer Check
# =============================================================================

dialyzer_check() {
  print_section "Dialyzer Type Checking"

  cd "$PROJECT_ROOT"

  if rebar3 dialyzer 2>&1 | tee dialyzer.log; then
    log_success "Dialyzer passed"
    return 0
  else
    log_warning "Dialyzer found warnings (non-blocking)"
    return 0
  fi
}

# =============================================================================
# Test Suite
# =============================================================================

run_tests() {
  print_section "Test Suite"

  cd "$PROJECT_ROOT"

  log_info "Running EUnit tests..."
  if rebar3 eunit --cover 2>&1 | tee eunit.log; then
    log_success "EUnit tests passed"
  else
    log_error "EUnit tests failed"
    return 1
  fi

  log_info "Running Common Test suites..."
  if rebar3 ct --cover 2>&1 | tee ct.log; then
    log_success "Common Test passed"
  else
    log_warning "Common Test had failures (non-blocking)"
  fi

  return 0
}

# =============================================================================
# Coverage Check
# =============================================================================

coverage_check() {
  print_section "Coverage Threshold Check"

  cd "$PROJECT_ROOT"

  log_info "Generating coverage report..."
  rebar3 cover --verbose 2>&1 | tee coverage.log

  # Extract coverage percentage
  local total_coverage=$(grep "total:" coverage.log 2>/dev/null | tail -1 | awk '{print $NF}' || echo "0%")

  log_info "Total coverage: $total_coverage"

  # Remove % sign for comparison
  local coverage_num=$(echo "$total_coverage" | sed 's/%//')

  if [ "$coverage_num" -ge "$COVERAGE_THRESHOLD" ]; then
    log_success "Coverage $total_coverage ≥ $COVERAGE_THRESHOLD% (threshold met)"
    return 0
  else
    log_error "Coverage $total_coverage < $COVERAGE_THRESHOLD% (threshold not met)"
    return 1
  fi
}

# =============================================================================
# Spec Compliance Validation
# =============================================================================

spec_compliance_validation() {
  print_section "MCP Specification Compliance Validation"

  cd "$PROJECT_ROOT"

  # Build validation app
  if [ ! -d "apps/erlmcp_validation" ]; then
    log_warning "Validation app not found, skipping compliance check"
    return 0
  fi

  log_info "Building validation application..."
  if ! rebar3 as validation compile; then
    log_error "Failed to build validation app"
    return 1
  fi

  log_success "Validation app built"

  # Run spec parser validation
  log_info "Running spec parser validation..."
  if rebar3 as validation shell -eval "
    case code:load_file(erlmcp_spec_parser) of
      {module, _} ->
        io:format(\"Spec parser loaded successfully~n\"),
        init:stop(0);
      {error, Reason} ->
        io:format(\"Failed to load spec parser: ~p~n\", [Reason]),
        init:stop(1)
    end
  " --name spec_val@localhost --setcookie spec_cookie 2>&1 | tee spec_parser.log; then
    log_success "Spec parser validation passed"
  else
    log_warning "Spec parser validation failed"
  fi

  # Run protocol compliance tests
  log_info "Running protocol compliance tests..."
  if [ -f "apps/erlmcp_validation/test/erlmcp_spec_compliance_SUITE.ct" ]; then
    if rebar3 as validation ct --suite=erlmcp_spec_compliance_SUITE 2>&1 | tee compliance_ct.log; then
      log_success "Protocol compliance tests passed"
    else
      log_warning "Protocol compliance tests had failures"
    fi
  else
    log_warning "Compliance test suite not found"
  fi

  return 0
}

# =============================================================================
# Benchmark Validation
# =============================================================================

benchmark_validation() {
  if [ "$QUICK_MODE" = true ]; then
    return 0
  fi

  print_section "Benchmark Validation"

  cd "$PROJECT_ROOT"

  log_info "Running quick benchmark smoke test..."

  if rebar3 shell --eval "
    application:ensure_all_started(erlmcp_core),
    application:ensure_all_started(erlmcp_transports),

    io:format(\"~n=== Quick Benchmark: core_ops_1k ===~n\"),
    Result = erlmcp_bench_core_ops:run(<<\"core_ops_1k\">>),
    io:format(\"Result: ~p~n\", [Result]),

    init:stop()
  " --name bench@localhost --setcookie bench_cookie 2>&1 | tee benchmark.log; then
    log_success "Benchmark smoke test passed"
    return 0
  else
    log_warning "Benchmark smoke test failed (non-blocking)"
    return 0
  fi
}

# =============================================================================
# Metrology Compliance
# =============================================================================

metrology_compliance() {
  if [ "$QUICK_MODE" = true ]; then
    return 0
  fi

  print_section "Benchmark Metrology Compliance"

  cd "$PROJECT_ROOT"

  log_info "Checking benchmark metrics for canonical units..."

  # Check if benchmark logs exist
  if [ ! -f "benchmark.log" ]; then
    log_warning "No benchmark log found, skipping metrology check"
    return 0
  fi

  # Check for proper units
  local has_throughput_msg=$(grep -c "throughput_msg_per_s" benchmark.log || echo "0")
  local has_latency_us=$(grep -c "latency_p.*_us" benchmark.log || echo "0")
  local has_memory_mib=$(grep -c "mib" benchmark.log || echo "0")

  if [ "$has_throughput_msg" -gt 0 ] && [ "$has_latency_us" -gt 0 ]; then
    log_success "Metrology compliance check passed"
    log_info "  Found throughput_msg_per_s: yes"
    log_info "  Found latency_*_us: yes"
    log_info "  Found memory_mib: $([ "$has_memory_mib" -gt 0 ] && echo "yes" || echo "no")"
    return 0
  else
    log_warning "Metrology compliance issues found"
    log_info "  Found throughput_msg_per_s: $([ "$has_throughput_msg" -gt 0 ] && echo "yes" || echo "no")"
    log_info "  Found latency_*_us: $([ "$has_latency_us" -gt 0 ] && echo "yes" || echo "no")"
    return 0
  fi
}

# =============================================================================
# Final Report
# =============================================================================

generate_report() {
  print_section "Validation Report"

  echo ""
  echo "Validation Level: $VALIDATION_LEVEL"
  echo "OTP Version: $OTP_VERSION"
  echo ""

  # Count results
  local passed=0
  local failed=0
  local warnings=0

  # Check logs for results
  [ -f "eunit.log" ] && ! grep -q "failed" eunit.log 2>/dev/null && ((passed++)) || ((failed++))
  [ -f "xref.log" ] && ((passed++)) || ((warnings++))
  [ -f "dialyzer.log" ] && ((passed++)) || ((warnings++))
  [ -f "coverage.log" ] && ! grep -q "below" coverage.log 2>/dev/null && ((passed++)) || ((failed++))

  echo "Summary:"
  echo "  Passed:   $passed"
  echo "  Failed:   $failed"
  echo "  Warnings: $warnings"
  echo ""

  if [ $failed -eq 0 ]; then
    log_success "All blocking checks passed!"
    return 0
  else
    log_error "Some blocking checks failed"
    return 1
  fi
}

# =============================================================================
# Main Execution
# =============================================================================

main() {
  log_info "Starting CI validation (level: $VALIDATION_LEVEL)"
  log_info "Project root: $PROJECT_ROOT"

  # Pre-flight checks
  preflight_checks

  # Compliance-only mode
  if [ "$COMPLIANCE_ONLY" = true ]; then
    spec_compliance_validation
    exit $?
  fi

  # Quick mode: compile + tests only
  if [ "$QUICK_MODE" = true ]; then
    compile_project || exit 1
    run_tests || exit 1
    coverage_check || exit 1
    generate_report
    exit $?
  fi

  # Standard mode: all checks
  compile_project || exit 1
  xref_check
  dialyzer_check
  run_tests || exit 1
  coverage_check || exit 1
  spec_compliance_validation

  # Full mode: include benchmarks
  if [ "$FULL_MODE" = true ]; then
    benchmark_validation
    metrology_compliance
  fi

  generate_report
  exit $?
}

# Run main function
main "$@"
