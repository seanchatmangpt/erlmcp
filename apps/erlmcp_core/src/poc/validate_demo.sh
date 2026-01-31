#!/usr/bin/env bash
# Validation script for erlmcp_poc_demo module

set -e

echo "=========================================="
echo "  erlmcp POC Demo Validation"
echo "=========================================="
echo ""

# Colors
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Check if file exists
if [ ! -f "erlmcp_poc_demo.erl" ]; then
    echo -e "${RED}✗ erlmcp_poc_demo.erl not found${NC}"
    exit 1
fi
echo -e "${GREEN}✓ Module file exists${NC}"

# Check basic Erlang syntax (module declaration)
if grep -q "^-module(erlmcp_poc_demo)." erlmcp_poc_demo.erl; then
    echo -e "${GREEN}✓ Module declaration found${NC}"
else
    echo -e "${RED}✗ Module declaration missing${NC}"
    exit 1
fi

# Check gen_server behavior
if grep -q "^-behaviour(gen_server)." erlmcp_poc_demo.erl; then
    echo -e "${GREEN}✓ gen_server behaviour declared${NC}"
else
    echo -e "${RED}✗ gen_server behaviour missing${NC}"
    exit 1
fi

# Check exports
if grep -q "^-export(\[" erlmcp_poc_demo.erl; then
    echo -e "${GREEN}✓ Exports found${NC}"
else
    echo -e "${RED}✗ No exports found${NC}"
    exit 1
fi

# Check required API functions
for func in "run_full_demo/0" "run_full_demo/1" "stop/0"; do
    if grep -q "$func" erlmcp_poc_demo.erl; then
        echo -e "${GREEN}✓ API function $func present${NC}"
    else
        echo -e "${RED}✗ API function $func missing${NC}"
        exit 1
    fi
done

# Check gen_server callbacks
for callback in "init/1" "handle_call/3" "handle_cast/2" "handle_info/2" "terminate/2" "code_change/3"; do
    if grep -q "$callback" erlmcp_poc_demo.erl; then
        echo -e "${GREEN}✓ Callback $callback present${NC}"
    else
        echo -e "${RED}✗ Callback $callback missing${NC}"
        exit 1
    fi
done

# Check key functions
echo ""
echo "Checking implementation functions..."

for func in "start_circuit_breaker" "start_mcp_server" "add_demo_tools" "add_demo_resources" "start_agent" "run_agent_workload"; do
    if grep -q "^$func(" erlmcp_poc_demo.erl; then
        echo -e "${GREEN}✓ Function $func/N implemented${NC}"
    else
        echo -e "${YELLOW}⚠ Function $func/N not found (may be different arity)${NC}"
    fi
done

# Check for proper record definitions
echo ""
echo "Checking data structures..."

if grep -q "^-record(agent_state," erlmcp_poc_demo.erl; then
    echo -e "${GREEN}✓ agent_state record defined${NC}"
else
    echo -e "${RED}✗ agent_state record missing${NC}"
    exit 1
fi

if grep -q "^-record(state," erlmcp_poc_demo.erl; then
    echo -e "${GREEN}✓ state record defined${NC}"
else
    echo -e "${RED}✗ state record missing${NC}"
    exit 1
fi

# Check includes
echo ""
echo "Checking includes..."

if grep -q '-include_lib("kernel/include/logger.hrl")' erlmcp_poc_demo.erl; then
    echo -e "${GREEN}✓ Logger include present${NC}"
else
    echo -e "${YELLOW}⚠ Logger include missing${NC}"
fi

if grep -q '-include("erlmcp.hrl")' erlmcp_poc_demo.erl; then
    echo -e "${GREEN}✓ erlmcp.hrl include present${NC}"
else
    echo -e "${YELLOW}⚠ erlmcp.hrl include missing${NC}"
fi

# Line count
lines=$(wc -l < erlmcp_poc_demo.erl)
echo ""
echo -e "${GREEN}✓ Module size: $lines lines${NC}"

# Summary
echo ""
echo "=========================================="
echo -e "${GREEN}✓ All validation checks passed!${NC}"
echo "=========================================="
echo ""
echo "Next steps:"
echo "  1. Compile: TERM=dumb rebar3 compile"
echo "  2. Run: make console"
echo "  3. Execute: erlmcp_poc_demo:run_full_demo()."
echo ""
