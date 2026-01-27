#!/bin/bash
###############################################################################
# TCPS Dashboard Quick Start Script
#
# Starts the TCPS Dashboard with all dependencies.
# Opens browser to dashboard URL automatically.
#
# Usage:
#   ./scripts/start_dashboard.sh [port]
#
# Default port: 8080
###############################################################################

set -e

# Configuration
PORT=${1:-8080}
DASHBOARD_URL="http://localhost:${PORT}/dashboard"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

echo -e "${BLUE}═══════════════════════════════════════════════════════════${NC}"
echo -e "${BLUE}    TCPS Dashboard - Quick Start${NC}"
echo -e "${BLUE}═══════════════════════════════════════════════════════════${NC}"
echo ""

# Check if Erlang is installed
if ! command -v erl &> /dev/null; then
    echo -e "${RED}✗ Erlang is not installed${NC}"
    echo "  Please install Erlang/OTP 24+ from: https://www.erlang.org/"
    exit 1
fi

echo -e "${GREEN}✓ Erlang found:${NC} $(erl -eval 'io:format("~s~n", [erlang:system_info(otp_release)]), halt().' -noshell)"

# Check if rebar3 is installed
if ! command -v rebar3 &> /dev/null; then
    echo -e "${RED}✗ rebar3 is not installed${NC}"
    echo "  Please install rebar3 from: https://rebar3.org/"
    exit 1
fi

echo -e "${GREEN}✓ rebar3 found${NC}"

# Check if port is available
if lsof -Pi :${PORT} -sTCP:LISTEN -t >/dev/null 2>&1; then
    echo -e "${YELLOW}⚠ Port ${PORT} is already in use${NC}"
    echo "  Try a different port: ./scripts/start_dashboard.sh 8888"
    exit 1
fi

echo -e "${GREEN}✓ Port ${PORT} is available${NC}"
echo ""

# Compile project
echo -e "${BLUE}Compiling project...${NC}"
rebar3 compile

echo -e "${GREEN}✓ Compilation successful${NC}"
echo ""

# Create temporary Erlang script to start dashboard
TEMP_SCRIPT=$(mktemp)
cat > "$TEMP_SCRIPT" << EOF
%% Auto-generated dashboard startup script
io:format("~n~s~n", ["\033[0;34m═══════════════════════════════════════════════════════════\033[0m"]),
io:format("~s~n", ["\033[0;34m    Starting TCPS Dashboard...\033[0m"]),
io:format("~s~n~n", ["\033[0;34m═══════════════════════════════════════════════════════════\033[0m"]),

%% Start applications
application:ensure_all_started(jsx),
application:ensure_all_started(ranch),
application:ensure_all_started(cowboy),

%% Start dashboard
{ok, Pid} = tcps_dashboard:start_dashboard(${PORT}),

io:format("\033[0;32m✓ Dashboard started successfully!\033[0m~n~n"),
io:format("Access the dashboard at: \033[1;36m${DASHBOARD_URL}\033[0m~n~n"),
io:format("API Endpoints:~n"),
io:format("  - Summary:  \033[0;36mhttp://localhost:${PORT}/api/metrics/summary\033[0m~n"),
io:format("  - Health:   \033[0;36mhttp://localhost:${PORT}/api/health\033[0m~n"),
io:format("  - Stream:   \033[0;36mhttp://localhost:${PORT}/api/stream\033[0m~n~n"),
io:format("Press Ctrl+C twice to stop the dashboard.~n~n"),

%% Load demo module
code:add_path("examples"),
case filelib:is_file("examples/dashboard_demo.erl") of
    true ->
        c("examples/dashboard_demo"),
        io:format("\033[1;33mDemo commands available:\033[0m~n"),
        io:format("  dashboard_demo:simulate_work_flow().~n"),
        io:format("  dashboard_demo:simulate_andon().~n"),
        io:format("  dashboard_demo:simulate_kaizen().~n~n");
    false ->
        ok
end.
EOF

# Start Erlang shell with dashboard
echo -e "${BLUE}Starting Erlang shell...${NC}"
echo ""

# Try to open browser automatically
if command -v open &> /dev/null; then
    # macOS
    (sleep 2 && open "$DASHBOARD_URL") &
elif command -v xdg-open &> /dev/null; then
    # Linux
    (sleep 2 && xdg-open "$DASHBOARD_URL") &
elif command -v start &> /dev/null; then
    # Windows
    (sleep 2 && start "$DASHBOARD_URL") &
fi

# Start Erlang shell
erl -pa _build/default/lib/*/ebin -eval "$(cat "$TEMP_SCRIPT")" -noshell

# Cleanup
rm -f "$TEMP_SCRIPT"
