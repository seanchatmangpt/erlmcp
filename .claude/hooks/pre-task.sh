#!/bin/bash
set -e
cd "$CLAUDE_PROJECT_DIR" || exit 2

RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m'

PASSED=true

echo -e "${YELLOW}🔍 Pre-Task Quality Checks${NC}"

# Source SessionStart environment for OTP 28
ENV_FILE="${CLAUDE_PROJECT_DIR}/.erlmcp/env.sh"
if [[ -f "$ENV_FILE" ]]; then
    source "$ENV_FILE"
    OTP_VER=$(erl -noshell -eval 'io:format("~s", [erlang:system_info(otp_release)]), halt().' 2>/dev/null)
    echo -e "${YELLOW}→ Using OTP ${OTP_VER}${NC}"
fi

# Compilation
echo -e "\n${YELLOW}→ Compilation${NC}"
if TERM=dumb rebar3 compile 2>&1 | grep -q "Error:"; then
    echo -e "${RED}✗ Compilation errors${NC}"
    PASSED=false
else
    echo -e "${GREEN}✓ OK${NC}"
fi

# EUnit (max 5 failures allowed)
echo -e "${YELLOW}→ Unit Tests${NC}"
EUNIT=$(rebar3 eunit 2>&1 || true)
FAILED=$(echo "$EUNIT" | grep -o "Failed: [0-9]*" | grep -o "[0-9]*" || echo "0")
if [ "$FAILED" -gt 5 ]; then
    echo -e "${RED}✗ $FAILED failures (max 5)${NC}"
    PASSED=false
else
    echo -e "${GREEN}✓ $FAILED failures${NC}"
fi

# XREF (max 10 warnings)
echo -e "${YELLOW}→ XREF${NC}"
XREF=$(rebar3 xref 2>&1 || true)
WARNINGS=$(echo "$XREF" | grep -c "Warning:" || echo "0")
if [ "$WARNINGS" -gt 10 ]; then
    echo -e "${RED}✗ $WARNINGS warnings (max 10)${NC}"
    PASSED=false
else
    echo -e "${GREEN}✓ $WARNINGS warnings${NC}"
fi

echo -e "\n${YELLOW}=====================================${NC}"
if [ "$PASSED" = true ]; then
    echo -e "${GREEN}✅ Quality gates passed${NC}"
    exit 0
else
    echo -e "${RED}❌ Quality gates failed${NC}"
    exit 2
fi
