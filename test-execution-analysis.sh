#!/usr/bin/env bash
# Test Execution Analysis Script for erlmcp v3
# Usage: ./test-execution-analysis.sh

set -euo pipefail

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

# Results file
RESULTS_FILE="test-execution-results-$(date +%Y%m%d-%H%M%S).json"

# Initialize results
echo "{" > "$RESULTS_FILE"
echo "  \"timestamp\": \"$(date -Iseconds)\"," >> "$RESULTS_FILE"
echo "  \"otp_version\": \"$(erl -noshell -eval 'io:format(\"~s~n\", [erlang:system_info(otp_release)]), halt(0).')\"," >> "$RESULTS_FILE"
echo "  \"analysis\": {" >> "$RESULTS_FILE"

echo -e "${BLUE}════════════════════════════════════════════════════════════${NC}"
echo -e "${BLUE}erlmcp v3 Test Execution Analysis${NC}"
echo -e "${BLUE}════════════════════════════════════════════════════════════${NC}"
echo ""

# 1. OTP Version Check
echo -e "${YELLOW}1. OTP Version Check${NC}"
echo "=================================="
otp_version=$(erl -noshell -eval 'io:format("~s~n", [erlang:system_info(otp_release)]), halt(0).')
rebar3_version=$(rebar3 version 2>/dev/null || echo "Not available")

echo "OTP Version: $otp_version"
echo "Required: 28.3.1+"
if [[ "$otp_version" == "28.3.1" || "$otp_version" > "28.3.1" ]]; then
    echo -e "${GREEN}✅ OTP version OK${NC}"
    echo "  \"otp_check\": {\"status\": \"pass\", \"version\": \"$otp_version\"}," >> "$RESULTS_FILE"
else
    echo -e "${RED}❌ OTP version mismatch - Required 28.3.1+${NC}"
    echo "  \"otp_check\": {\"status\": \"fail\", \"version\": \"$otp_version\", \"required\": \"28.3.1+\"}," >> "$RESULTS_FILE"
fi
echo ""

# 2. Compilation Check
echo -e "${YELLOW}2. Compilation Analysis${NC}"
echo "=================================="
echo "Compiling core applications..."
timeout 120 rebar3 compile >/tmp/compile.log 2>&1
compile_exit=$?

if [ $compile_exit -eq 0 ]; then
    echo -e "${GREEN}✅ Compilation successful${NC}"
    echo "  \"compilation\": {\"status\": \"pass\", \"exit_code\": 0}," >> "$RESULTS_FILE"
else
    echo -e "${RED}❌ Compilation failed${NC}"
    echo "  \"compilation\": {\"status\": \"fail\", \"exit_code\": $compile_exit, \"errors\": $(grep -c "error" /tmp/compile.log || echo 0)}," >> "$RESULTS_FILE"
    echo -e "${BLUE}First 10 errors:${NC}"
    grep -i "error\|syntax error" /tmp/compile.log | head -10
fi
echo ""

# 3. Test Discovery
echo -e "${YELLOW}3. Test Discovery${NC}"
echo "=================================="
eunit_tests=$(find apps/ -name "*_tests.erl" | wc -l)
ct_suites=$(find apps/ -name "*_SUITE.erl" | wc -l)
total_tests=$((eunit_tests + ct_suites))

echo "EUnit Test Modules: $eunit_tests"
echo "Common Test Suites: $ct_suites"
echo "Total Test Files: $total_tests"

echo "  \"test_discovery\": {" >> "$RESULTS_FILE"
echo "    \"eunit_modules\": $eunit_tests," >> "$RESULTS_FILE"
echo "    \"ct_suites\": $ct_suites," >> "$RESULTS_FILE"
echo "    \"total\": $total_tests" >> "$RESULTS_FILE"
echo "  }," >> "$RESULTS_FILE"
echo ""

# 4. EUnit Attempt
echo -e "${YELLOW}4. EUnit Test Attempt${NC}"
echo "=================================="
timeout 60 rebar3 eunit >/tmp/eunit.log 2>&1 || true
eunit_exit=$?

if [ $eunit_exit -eq 0 ]; then
    echo -e "${GREEN}✅ EUnit tests passed${NC}"
    echo "  \"eunit\": {\"status\": \"pass\", \"exit_code\": 0}," >> "$RESULTS_FILE"
else
    echo -e "${RED}❌ EUnit tests failed${NC}"
    echo "  \"eunit\": {\"status\": \"fail\", \"exit_code\": $eunit_exit}," >> "$RESULTS_FILE"
    echo -e "${BLUE}EUnit output summary:${NC}"
    grep -E "(FAILED|Error|passing|failing)" /tmp/eunit.log | tail -5
fi
echo ""

# 5. Smoke Test Analysis
echo -e "${YELLOW}5. Smoke Test Analysis${NC}"
echo "=================================="
echo "Attempting to run smoke tests..."
timeout 30 make smoke-test 2>/dev/null || true

if [ -f /tmp/smoke_test_results.log ]; then
    smoke_passed=$(grep -c "PASSED" /tmp/smoke_test_results.log || echo 0)
    smoke_failed=$(grep -c "FAILED" /tmp/smoke_test_results.log || echo 0)
    echo "Smoke Tests: $smoke_passed passed, $smoke_failed failed"

    echo "  \"smoke_tests\": {" >> "$RESULTS_FILE"
    echo "    \"passed\": $smoke_passed," >> "$RESULTS_FILE"
    echo "    \"failed\": $smoke_failed," >> "$RESULTS_FILE"
    echo "    \"total\": $((smoke_passed + smoke_failed))" >> "$RESULTS_FILE"
    echo "  }," >> "$RESULTS_FILE"
fi
echo ""

# 6. Test File Analysis
echo -e "${YELLOW}6. Test File Analysis${NC}"
echo "=================================="
echo "Analyzing test file patterns..."

# Check for test isolation patterns
has_setup=$(grep -r "setup()" apps/*/test/ | wc -l)
has_cleanup=$(grep -r "cleanup(" apps/*/test/ | wc -l)
has_mock=$(grep -r "mock" apps/*/test/ | wc -l)

echo "Setup functions: $has_setup"
echo "Cleanup functions: $has_cleanup"
echo "Mock usage: $has_mock"

echo "  \"test_patterns\": {" >> "$RESULTS_FILE"
echo "    \"setup_functions\": $has_setup," >> "$RESULTS_FILE"
echo "    \"cleanup_functions\": $has_cleanup," >> "$RESULTS_FILE"
echo "    \"mock_usage\": $has_mock" >> "$RESULTS_FILE"
echo "  }," >> "$RESULTS_FILE"
echo ""

# 7. Dependency Analysis
echo -e "${YELLOW}7. Dependency Analysis${NC}"
echo "=================================="
echo "Checking test dependencies..."

# Check if critical test modules exist
critical_tests=(
    "erlmcp_json_rpc_tests"
    "erlmcp_client_basic_tests"
    "erlmcp_server_basic_tests"
    "erlmcp_registry_tests"
    "erlmcp_transport_stdio_tests"
)

missing_tests=0
for test in "${critical_tests[@]}"; do
    if [ -f "apps/erlmcp_core/test/${test}.erl" ]; then
        echo -e "${GREEN}✅ $test found${NC}"
    else
        echo -e "${RED}❌ $test missing${NC}"
        ((missing_tests++))
    fi
done

echo "  \"dependencies\": {" >> "$RESULTS_FILE"
echo "    \"critical_tests_found\": $((${#critical_tests[@]} - missing_tests))," >> "$RESULTS_FILE"
echo "    \"critical_tests_missing\": $missing_tests" >> "$RESULTS_FILE"
echo "  }," >> "$RESULTS_FILE"
echo ""

# 8. Performance Baseline
echo -e "${YELLOW}8. Performance Baseline${NC}"
echo "=================================="
echo "Measuring compilation time..."

start_time=$(date +%s.%N)
timeout 60 rebar3 compile >/dev/null 2>&1 || true
end_time=$(date +%s.%N)
compile_time=$(echo "$end_time - $start_time" | bc)

echo "Compilation time: ${compile_time}s"
echo "  \"performance\": {" >> "$RESULTS_FILE"
echo "    \"compile_time\": $compile_time," >> "$RESULTS_FILE"

# Memory usage during compilation
if command -v ps >/dev/null; then
    peak_mem=$(ps -o rss= -p $(pgrep -f "rebar3") 2>/dev/null | sort -n | tail -1 || echo "unknown")
    echo "Peak memory: ${peak_mem} KB"
    echo "    \"peak_memory_kb\": $peak_mem" >> "$RESULTS_FILE"
fi

echo "  }" >> "$RESULTS_FILE"
echo ""

# 9. CI Integration Check
echo -e "${YELLOW}9. CI Integration Check${NC}"
echo "=================================="
if [ -f ".github/workflows/ci.yml" ]; then
    echo -e "${GREEN}✅ GitHub Actions workflow found${NC}"
    ci_status="pass"
else
    echo -e "${YELLOW}⚠️ No CI workflow found${NC}"
    ci_status="warn"
fi

echo "  \"ci_integration\": {\"status\": \"$ci_status\"}" >> "$RESULTS_FILE"
echo ""

# Close JSON
echo "}" >> "$RESULTS_FILE"
echo "}" >> "$RESULTS_FILE"

# Summary
echo -e "${BLUE}════════════════════════════════════════════════════════════${NC}"
echo -e "${BLUE}Summary${NC}"
echo -e "${BLUE}════════════════════════════════════════════════════════════${NC}"

# Calculate overall score
otp_score=$(grep -o '"status": "pass"' "$RESULTS_FILE" | grep -c "otp_check" || echo 0)
compile_score=$(grep -o '"status": "pass"' "$RESULTS_FILE" | grep -c "compilation" || echo 0)
eunit_score=$(grep -o '"status": "pass"' "$RESULTS_FILE" | grep -c "eunit" || echo 0)

total=$((otp_score + compile_score + eunit_score))
max=3
percentage=$((total * 100 / max))

echo "Overall Score: $percentage% ($total/$max categories passed)"

if [ $percentage -ge 80 ]; then
    echo -e "${GREEN}✅ Test suite is enterprise ready${NC}"
elif [ $percentage -ge 50 ]; then
    echo -e "${YELLOW}⚠️ Test suite partially ready${NC}"
else
    echo -e "${RED}❌ Test suite not ready${NC}"
fi

echo ""
echo "Detailed results saved to: $RESULTS_FILE"
echo "View with: cat $RESULTS_FILE | jq"

# Cleanup
rm -f /tmp/compile.log /tmp/eunit.log /tmp/smoke_test_results.log 2>/dev/null || true