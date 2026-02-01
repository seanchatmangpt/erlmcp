#!/bin/bash
set -e
cd "$CLAUDE_PROJECT_DIR" || exit 2

YELLOW='\033[1;33m'
GREEN='\033[0;32m'
NC='\033[0m'

TIMESTAMP=$(date +%Y%m%d_%H%M%S)
REPORT_DIR="test_results/quality_gates"
mkdir -p "$REPORT_DIR"

echo -e "${YELLOW}📊 Post-Task Quality Analysis${NC}"

# Source SessionStart environment for OTP 28
ENV_FILE="${CLAUDE_PROJECT_DIR}/.erlmcp/env.sh"
if [[ -f "$ENV_FILE" ]]; then
    source "$ENV_FILE"
    echo -e "${YELLOW}→ Using OTP $(erl -noshell -eval 'io:format("~s", [erlang:system_info(otp_release)]), halt().' 2>/dev/null)${NC}"
fi

# Full test suite
echo -e "${YELLOW}→ Running tests${NC}"
rebar3 eunit 2>&1 | tee "$REPORT_DIR/eunit_$TIMESTAMP.log"

# Coverage
echo -e "${YELLOW}→ Coverage report${NC}"
rebar3 cover 2>&1 | tee "$REPORT_DIR/coverage_$TIMESTAMP.log"

# XREF detailed
echo -e "${YELLOW}→ XREF analysis${NC}"
rebar3 xref 2>&1 | tee "$REPORT_DIR/xref_$TIMESTAMP.log"

# Summary
MODULES=$(TERM=dumb rebar3 compile 2>&1 | grep -c "Compiled:" || echo "0")
PASSED=$(rebar3 eunit 2>&1 | grep -o "Passed: [0-9]*" | tail -1 || echo "N/A")
XREF_WARNS=$(rebar3 xref 2>&1 | grep -c "Warning:" || echo "0")

cat > "$REPORT_DIR/summary_$TIMESTAMP.txt" << EOF
Quality Report - $TIMESTAMP
=============================
Modules: $MODULES
Tests: $PASSED
XREF: $XREF_WARNS warnings
EOF

echo -e "${GREEN}✅ Reports: $REPORT_DIR/${NC}"
exit 0
