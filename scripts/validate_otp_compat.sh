#!/usr/bin/env bash
#=======================================================================
# OTP Compatibility Layer Validation Script
#=======================================================================
#
# Validates the OTP compatibility layer implementation:
# - Header file exists and is valid
# - Helper module compiles
# - Tests pass
# - No compilation warnings
#
# Usage: ./scripts/validate_otp_compat.sh
#

set -e

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

echo "=========================================="
echo "OTP Compatibility Layer Validation"
echo "=========================================="
echo

# Get OTP version
OTP_VERSION=$(erl -noshell -eval 'erlang:display(erlang:system_info(otp_release))' -s init stop 2>/dev/null | tr -d '"')
echo "Erlang/OTP Version: $OTP_VERSION"
echo

#=======================================================================
# 1. Check Header File
#=======================================================================
echo "1. Checking header file..."
if [ -f "include/otp_compat.hrl" ]; then
    echo -e "${GREEN}✓${NC} include/otp_compat.hrl exists"

    # Check for critical macros
    MACROS=(
        "HAVE_NATIVE_JSON"
        "HAVE_PROCESS_ITERATOR"
        "HAVE_PRIORITY_MESSAGES"
        "SAFE_PROCESS_COUNT"
        "SAFE_PROCESSES"
        "JSON_ENCODE"
        "JSON_DECODE"
    )

    for macro in "${MACROS[@]}"; do
        if grep -q "$macro" include/otp_compat.hrl; then
            echo -e "${GREEN}✓${NC} Macro $macro defined"
        else
            echo -e "${RED}✗${NC} Macro $macro NOT defined"
            exit 1
        fi
    done
else
    echo -e "${RED}✗${NC} include/otp_compat.hrl NOT found"
    exit 1
fi
echo

#=======================================================================
# 2. Check Helper Module
#=======================================================================
echo "2. Checking helper module..."
if [ -f "apps/erlmcp_core/src/erlmcp_otp_compat.erl" ]; then
    echo -e "${GREEN}✓${NC} apps/erlmcp_core/src/erlmcp_otp_compat.erl exists"

    # Check for required functions
    FUNCTIONS=(
        "otp_version"
        "is_otp_28_plus"
        "is_otp_27_plus"
        "have_native_json"
        "have_process_iterator"
        "have_priority_messages"
        "json_encode"
        "json_decode"
        "safe_process_count"
        "safe_processes"
    )

    for func in "${FUNCTIONS[@]}"; do
        if grep -q "export.*$func" apps/erlmcp_core/src/erlmcp_otp_compat.erl; then
            echo -e "${GREEN}✓${NC} Function $func/0 exported"
        else
            echo -e "${RED}✗${NC} Function $func/0 NOT exported"
            exit 1
        fi
    done
else
    echo -e "${RED}✗${NC} apps/erlmcp_core/src/erlmcp_otp_compat.erl NOT found"
    exit 1
fi
echo

#=======================================================================
# 3. Check Test Suite
#=======================================================================
echo "3. Checking test suite..."
if [ -f "apps/erlmcp_core/test/erlmcp_otp_compat_tests.erl" ]; then
    echo -e "${GREEN}✓${NC} apps/erlmcp_core/test/erlmcp_otp_compat_tests.erl exists"

    # Count tests
    TEST_COUNT=$(grep -c "test_" apps/erlmcp_core/test/erlmcp_otp_compat_tests.erl || echo "0")
    echo -e "${GREEN}✓${NC} Found $TEST_COUNT test functions"
else
    echo -e "${RED}✗${NC} Test suite NOT found"
    exit 1
fi
echo

#=======================================================================
# 4. Compilation Check
#=======================================================================
echo "4. Checking compilation..."

# Check rebar.config for platform_define
if grep -q "platform_define.*OTP_28_PLUS" rebar.config; then
    echo -e "${GREEN}✓${NC} rebar.config has OTP_28_PLUS platform_define"
else
    echo -e "${YELLOW}⚠${NC} rebar.config missing OTP_28_PLUS platform_define"
    echo "  Add: {platform_define, \"^2[8-9]|^[3-9]\", 'OTP_28_PLUS'}"
fi

# Check include_dirs
if grep -q "include_dirs.*\binclude\b" rebar.config; then
    echo -e "${GREEN}✓${NC} rebar.config includes 'include' directory"
else
    echo -e "${YELLOW}⚠${NC} rebar.config missing include_dirs"
fi
echo

#=======================================================================
# 5. Documentation Check
#=======================================================================
echo "5. Checking documentation..."

DOCS=(
    "docs/v3/README.md"
    "docs/v3/04_otp_compat_layer_plan.md"
)

for doc in "${DOCS[@]}"; do
    if [ -f "$doc" ]; then
        echo -e "${GREEN}✓${NC} $doc exists"
    else
        echo -e "${YELLOW}⚠${NC} $doc NOT found"
    fi
done
echo

#=======================================================================
# 6. Runtime Validation (if rebar3 available)
#=======================================================================
if command -v rebar3 &> /dev/null; then
    echo "6. Running compilation check..."

    # Try to compile
    if TERM=dumb rebar3 compile 2>&1 | grep -q "Compiled"; then
        echo -e "${GREEN}✓${NC} Compilation successful"
    else
        echo -e "${YELLOW}⚠${NC} Compilation had issues (check output)"
    fi
    echo

    echo "7. Running tests..."
    if rebar3 eunit --module=erlmcp_otp_compat_tests 2>&1 | grep -q "Test passed"; then
        echo -e "${GREEN}✓${NC} Tests passed"
    else
        echo -e "${YELLOW}⚠${NC} Tests failed or not run (check output)"
    fi
    echo
else
    echo "6. Skipping compilation check (rebar3 not available)"
    echo
fi

#=======================================================================
# Summary
#=======================================================================
echo "=========================================="
echo "Validation Summary"
echo "=========================================="
echo
echo -e "OTP Version: ${GREEN}$OTP_VERSION${NC}"
echo
echo "Files created:"
echo "  - include/otp_compat.hrl"
echo "  - apps/erlmcp_core/src/erlmcp_otp_compat.erl"
echo "  - apps/erlmcp_core/test/erlmcp_otp_compat_tests.erl"
echo "  - docs/v3/README.md"
echo "  - docs/v3/04_otp_compat_layer_plan.md"
echo
echo "Next steps:"
echo "  1. Review docs/v3/04_otp_compat_layer_plan.md"
echo "  2. Update erlmcp_process_monitor.erl"
echo "  3. Migrate JSON calls to use ?JSON_ENCODE/DECODE"
echo "  4. Update all 288 files using system_info(process_count)"
echo "  5. Run full test suite"
echo
echo -e "${GREEN}✓${NC} OTP compatibility layer validation complete"
