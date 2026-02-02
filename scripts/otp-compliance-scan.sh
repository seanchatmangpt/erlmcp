#!/usr/bin/env bash
# OTP Compliance Scanner for erlmcp-flow
# Detects common OTP anti-patterns and violations
# Version: 1.0.0

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Counters
VIOLATIONS=0
WARNINGS=0

echo -e "${BLUE}=====================================${NC}"
echo -e "${BLUE}OTP Compliance Scanner v1.0.0${NC}"
echo -e "${BLUE}=====================================${NC}"
echo ""

# Function to report violation
violation() {
    local msg="$1"
    local file="$2"
    local line="$3"
    echo -e "${RED}❌ VIOLATION:${NC} $msg"
    if [ -n "$file" ]; then
        echo -e "   ${YELLOW}File:${NC} $file"
        if [ -n "$line" ]; then
            echo -e "   ${YELLOW}Line:${NC} $line"
        fi
    fi
    echo ""
    VIOLATIONS=$((VIOLATIONS + 1))
}

# Function to report warning
warning() {
    local msg="$1"
    local file="$2"
    echo -e "${YELLOW}⚠️  WARNING:${NC} $msg"
    if [ -n "$file" ]; then
        echo -e "   ${YELLOW}File:${NC} $file"
    fi
    echo ""
    WARNINGS=$((WARNINGS + 1))
}

# Function to report success
success() {
    local msg="$1"
    echo -e "${GREEN}✅ PASS:${NC} $msg"
}

echo -e "${BLUE}[1/10]${NC} Checking for unsupervised spawn..."
cd "$PROJECT_ROOT"

# Check for unsupervised spawn in source files (exclude POC and markdown)
UNSUPERVISED_SPAWN=$(grep -rn "spawn(fun()" apps/*/src/ 2>/dev/null | \
    grep -v "/poc/" | grep -v "\.md" | grep -v "\.escript" || true)
if [ -n "$UNSUPERVISED_SPAWN" ]; then
    while IFS= read -r line; do
        file=$(echo "$line" | cut -d: -f1)
        lineno=$(echo "$line" | cut -d: -f2)
        violation "Unsupervised spawn detected (use supervisor:start_child)" "$file" "$lineno"
    done <<< "$UNSUPERVISED_SPAWN"
else
    success "No unsupervised spawn found"
fi

echo -e "${BLUE}[2/10]${NC} Checking for spawn_link without supervisor..."

SPAWN_LINK=$(grep -rn "spawn_link(fun()" apps/*/src/ 2>/dev/null | \
    grep -v "test" | grep -v "/poc/" | grep -v "\.md" || true)
if [ -n "$SPAWN_LINK" ]; then
    while IFS= read -r line; do
        file=$(echo "$line" | cut -d: -f1)
        lineno=$(echo "$line" | cut -d: -f2)
        warning "spawn_link without supervisor (verify supervision)" "$file"
    done <<< "$SPAWN_LINK"
else
    success "No problematic spawn_link found"
fi

echo -e "${BLUE}[3/10]${NC} Checking for blocking init/1..."

# Look for common blocking patterns in init/1
BLOCKING_INIT=$(grep -A 10 "^init(" apps/*/src/*.erl 2>/dev/null | \
    grep -E "gen_tcp:connect|httpc:request|file:read|timer:sleep|gen_server:call" || true)

if [ -n "$BLOCKING_INIT" ]; then
    violation "Potential blocking operations in init/1 (use {continue, ...})"
    echo "$BLOCKING_INIT" | head -5
    echo ""
else
    success "No obvious blocking init/1 patterns"
fi

echo -e "${BLUE}[4/10]${NC} Checking gen_server callback exports..."

# Check that all gen_server modules export all 6 callbacks
for file in $(grep -l "^-behaviour(gen_server)" apps/*/src/*.erl 2>/dev/null); do
    EXPORTS=$(grep "^-export(\[" "$file" || true)

    missing_callbacks=""
    if ! echo "$EXPORTS" | grep -q "init/1"; then
        missing_callbacks="$missing_callbacks init/1"
    fi
    if ! echo "$EXPORTS" | grep -q "handle_call/3"; then
        missing_callbacks="$missing_callbacks handle_call/3"
    fi
    if ! echo "$EXPORTS" | grep -q "handle_cast/2"; then
        missing_callbacks="$missing_callbacks handle_cast/2"
    fi
    if ! echo "$EXPORTS" | grep -q "handle_info/2"; then
        missing_callbacks="$missing_callbacks handle_info/2"
    fi
    if ! echo "$EXPORTS" | grep -q "terminate/2"; then
        missing_callbacks="$missing_callbacks terminate/2"
    fi
    if ! echo "$EXPORTS" | grep -q "code_change/3"; then
        missing_callbacks="$missing_callbacks code_change/3"
    fi

    if [ -n "$missing_callbacks" ]; then
        violation "Missing gen_server callbacks:$missing_callbacks" "$file"
    fi
done

if [ $VIOLATIONS -eq 0 ]; then
    success "All gen_server modules export required callbacks"
fi

echo -e "${BLUE}[5/10]${NC} Checking for short timeouts..."

SHORT_TIMEOUTS=$(grep -rn "gen_server:call.*[0-9]\{1,3\})" apps/*/src/ 2>/dev/null | \
    grep -v "5000\|10000\|30000\|60000\|infinity" || true)

if [ -n "$SHORT_TIMEOUTS" ]; then
    while IFS= read -r line; do
        file=$(echo "$line" | cut -d: -f1)
        lineno=$(echo "$line" | cut -d: -f2)
        violation "Timeout < 5000ms detected" "$file" "$lineno"
    done <<< "$SHORT_TIMEOUTS"
else
    success "No short timeouts found"
fi

echo -e "${BLUE}[6/10]${NC} Checking for mock usage in tests..."

MOCK_USAGE=$(grep -rn "meck:new\|meck:expect\|meck:unload" apps/*/test/ 2>/dev/null || true)

if [ -n "$MOCK_USAGE" ]; then
    while IFS= read -r line; do
        file=$(echo "$line" | cut -d: -f1)
        lineno=$(echo "$line" | cut -d: -f2)
        violation "Mock usage detected (use real processes)" "$file" "$lineno"
    done <<< "$MOCK_USAGE"
else
    success "No mock usage found"
fi

echo -e "${BLUE}[7/10]${NC} Checking for state inspection in tests..."

STATE_INSPECTION=$(grep -rn "sys:get_status\|sys:get_state" apps/*/test/ 2>/dev/null || true)

if [ -n "$STATE_INSPECTION" ]; then
    while IFS= read -r line; do
        file=$(echo "$line" | cut -d: -f1)
        lineno=$(echo "$line" | cut -d: -f2)
        violation "State inspection detected (test observable behavior)" "$file" "$lineno"
    done <<< "$STATE_INSPECTION"
else
    success "No state inspection found"
fi

echo -e "${BLUE}[8/10]${NC} Checking for missing type specs..."

# Check for exported functions without -spec
for file in $(find apps/*/src -name "*.erl" -type f); do
    # Extract exported functions (simplified check)
    EXPORTS=$(grep "^-export(\[" "$file" | sed 's/-export(\[//;s/\]).//' | tr ',' '\n' | \
        grep -v "^$" | sed 's/^ *//;s/ *$//')

    if [ -n "$EXPORTS" ]; then
        while IFS= read -r export; do
            if [ -n "$export" ]; then
                # Extract function name
                func_name=$(echo "$export" | sed 's|/.*||')

                # Check if -spec exists for this function
                if ! grep -q "^-spec $func_name(" "$file"; then
                    warning "Missing -spec for exported function: $func_name" "$file"
                fi
            fi
        done <<< "$EXPORTS"
    fi
done

echo -e "${BLUE}[9/10]${NC} Checking for supervisor child specs..."

# Verify supervisor modules have proper child specs
for file in $(grep -l "^-behaviour(supervisor)" apps/*/src/*.erl 2>/dev/null); do
    # Check for map-based child specs (OTP 18+)
    if ! grep -q "id =>" "$file"; then
        warning "Using old tuple-based child spec (use map format)" "$file"
    fi

    # Check for restart strategy
    if ! grep -q "restart =>" "$file"; then
        violation "Missing restart strategy in child spec" "$file"
    fi

    # Check for shutdown
    if ! grep -q "shutdown =>" "$file"; then
        violation "Missing shutdown in child spec" "$file"
    fi
done

echo -e "${BLUE}[10/10]${NC} Checking for large test files..."

# Check for test files > 500 lines (anti-pattern)
for file in $(find apps/*/test -name "*_tests.erl" -o -name "*_SUITE.erl"); do
    lines=$(wc -l < "$file")
    if [ "$lines" -gt 500 ]; then
        warning "Test file too large ($lines lines > 500 limit)" "$file"
    fi
done

echo ""
echo -e "${BLUE}=====================================${NC}"
echo -e "${BLUE}OTP Compliance Scan Complete${NC}"
echo -e "${BLUE}=====================================${NC}"
echo ""
echo -e "${RED}Violations:${NC} $VIOLATIONS"
echo -e "${YELLOW}Warnings:${NC} $WARNINGS"
echo ""

if [ $VIOLATIONS -gt 0 ]; then
    echo -e "${RED}❌ FAILED:${NC} Fix violations before committing"
    echo ""
    echo "See docs/ERLMCP_FLOW_OTP_COMPLIANCE_CHECKLIST.md for guidance"
    exit 1
elif [ $WARNINGS -gt 0 ]; then
    echo -e "${YELLOW}⚠️  WARNINGS:${NC} Review warnings (non-blocking)"
    echo ""
    echo "See docs/OTP_COMPLIANCE_QUICK_REFERENCE.md for quick fixes"
    exit 0
else
    echo -e "${GREEN}✅ PASSED:${NC} All OTP compliance checks passed"
    exit 0
fi
