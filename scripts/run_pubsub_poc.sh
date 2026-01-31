#!/usr/bin/env bash
###############################################################################
# run_pubsub_poc.sh - Quick launcher for erlmcp_pubsub_poc demo
#
# Usage:
#   ./scripts/run_pubsub_poc.sh           # Run single-node demo
#   ./scripts/run_pubsub_poc.sh dist      # Show distributed demo instructions
#
###############################################################################

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
BLUE='\033[0;34m'
CYAN='\033[0;36m'
YELLOW='\033[1;33m'
BOLD='\033[1m'
NC='\033[0m' # No Color

echo ""
echo -e "${BOLD}${BLUE}╔════════════════════════════════════════════════════════════════╗${NC}"
echo -e "${BOLD}${BLUE}║  erlmcp_pubsub_poc - Phoenix PubSub-style Distributed Pub/Sub ║${NC}"
echo -e "${BOLD}${BLUE}║  Quick Launcher                                                ║${NC}"
echo -e "${BOLD}${BLUE}╚════════════════════════════════════════════════════════════════╝${NC}"
echo ""

# Check if distributed mode requested
MODE="${1:-single}"

if [ "$MODE" = "dist" ] || [ "$MODE" = "distributed" ]; then
    echo -e "${CYAN}╔════════════════════════════════════════════════════════════════╗${NC}"
    echo -e "${CYAN}║  Distributed Mode Instructions                                 ║${NC}"
    echo -e "${CYAN}╚════════════════════════════════════════════════════════════════╝${NC}"
    echo ""
    echo -e "${YELLOW}Open 2 terminals and run:${NC}"
    echo ""
    echo -e "${BOLD}Terminal 1 (Node 1):${NC}"
    echo "  $ cd $PROJECT_ROOT"
    echo "  $ erl -sname node1 -setcookie erlmcp -pa _build/default/lib/*/ebin"
    echo ""
    echo "  1> c(\"apps/erlmcp_core/src/poc/erlmcp_pubsub_poc.erl\")."
    echo "  2> erlmcp_pubsub_poc:start()."
    echo "  3> Sub1 = spawn(erlmcp_pubsub_poc, subscriber_loop, [node1_agent, []])."
    echo "  4> erlmcp_pubsub_poc:subscribe(\"distributed:test\", Sub1)."
    echo ""
    echo -e "${BOLD}Terminal 2 (Node 2):${NC}"
    echo "  $ cd $PROJECT_ROOT"
    echo "  $ erl -sname node2 -setcookie erlmcp -pa _build/default/lib/*/ebin"
    echo ""
    echo "  1> net_kernel:connect_node(node1@\$(hostname -s))."
    echo "  2> c(\"apps/erlmcp_core/src/poc/erlmcp_pubsub_poc.erl\")."
    echo "  3> erlmcp_pubsub_poc:start()."
    echo "  4> Sub2 = spawn(erlmcp_pubsub_poc, subscriber_loop, [node2_agent, []])."
    echo "  5> erlmcp_pubsub_poc:subscribe(\"distributed:test\", Sub2)."
    echo ""
    echo -e "${BOLD}Broadcast from either node:${NC}"
    echo "  > Msg = #{type => resource_update, data => #{temp => 75}}."
    echo "  > erlmcp_pubsub_poc:broadcast(\"distributed:test\", Msg)."
    echo ""
    echo -e "${GREEN}Both subscribers on BOTH nodes will receive the message!${NC}"
    echo ""
    exit 0
fi

# Single-node mode
echo -e "${GREEN}→ Step 1: Checking environment...${NC}"
cd "$PROJECT_ROOT"

# Check if rebar3 is available
if ! command -v rebar3 &> /dev/null; then
    echo -e "${RED}✗ rebar3 not found in PATH${NC}"
    echo -e "${YELLOW}Please install rebar3 or add to PATH${NC}"
    exit 1
fi
echo -e "${GREEN}  ✓ rebar3 found${NC}"

# Check if erl is available
if ! command -v erl &> /dev/null; then
    echo -e "${RED}✗ erl not found in PATH${NC}"
    echo -e "${YELLOW}Please install Erlang/OTP 23+${NC}"
    exit 1
fi
echo -e "${GREEN}  ✓ Erlang/OTP found${NC}"

echo ""
echo -e "${GREEN}→ Step 2: Compiling project...${NC}"
if ! TERM=dumb rebar3 compile 2>&1 | grep -q "Compiling"; then
    echo -e "${YELLOW}  Note: Project may already be compiled${NC}"
else
    echo -e "${GREEN}  ✓ Compilation complete${NC}"
fi

echo ""
echo -e "${GREEN}→ Step 3: Launching Erlang shell with demo...${NC}"
echo ""
echo -e "${CYAN}═══════════════════════════════════════════════════════════════${NC}"
echo -e "${CYAN}  Instructions:${NC}"
echo -e "${CYAN}  Once in the Erlang shell, run:${NC}"
echo ""
echo -e "${BOLD}    erlmcp_pubsub_poc:run_demo().${NC}"
echo ""
echo -e "${CYAN}  This will execute the complete POC demonstration with:${NC}"
echo -e "${CYAN}  • 5 AI agent processes${NC}"
echo -e "${CYAN}  • Resource update broadcasting${NC}"
echo -e "${CYAN}  • Tool result streaming${NC}"
echo -e "${CYAN}  • Topic isolation testing${NC}"
echo -e "${CYAN}  • Performance measurements${NC}"
echo -e "${CYAN}═══════════════════════════════════════════════════════════════${NC}"
echo ""
echo -e "${YELLOW}Press Ctrl+C twice to exit when done.${NC}"
echo ""
sleep 2

# Launch Erlang shell
exec erl -pa _build/default/lib/*/ebin -eval "io:format(\"~n~n${BOLD}${GREEN}Ready! Run: erlmcp_pubsub_poc:run_demo().~n~n${NC}\")."
