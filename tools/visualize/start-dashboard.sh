#!/usr/bin/env bash
###############################################################################
# start-dashboard.sh - Start Quality Gates Dashboard
#
# Starts the Erlang WebSocket server and opens browser to dashboard.
# Automatically compiles server if needed.
#
# Usage:
#   ./start-dashboard.sh [--no-browser] [--port=PORT]
#
# Options:
#   --no-browser    Don't open browser automatically
#   --port=PORT     Use custom port (default: 9091)
#   --detach        Run in background
#
# TCPS Integration:
#   - 看板 (Kanban): Visual WIP management
#   - 自働化 (Jidoka): Real-time quality monitoring
###############################################################################

set -euo pipefail

# Configuration
PORT=9091
OPEN_BROWSER=true
DETACH=false

# Parse arguments
for arg in "$@"; do
    case $arg in
        --no-browser)
            OPEN_BROWSER=false
            ;;
        --port=*)
            PORT="${arg#*=}"
            ;;
        --detach)
            DETACH=true
            ;;
        --help)
            sed -n '2,/^$/p' "$0" | sed 's/^# //'
            exit 0
            ;;
    esac
done

# Colors
GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
NC='\033[0m'

echo -e "${BLUE}🏭 Starting erlmcp Quality Gates Dashboard${NC}"
echo ""

# Get project root
SCRIPT_DIR=$(cd "$(dirname "$0")" && pwd)
PROJECT_ROOT=$(cd "$SCRIPT_DIR/../.." && pwd)

cd "$PROJECT_ROOT"

# Check if rebar3 is available
if ! command -v rebar3 &> /dev/null; then
    echo -e "${YELLOW}⚠️  rebar3 not found. Please install rebar3.${NC}"
    exit 1
fi

# Compile server module if needed
echo -e "${BLUE}📦 Compiling quality_gate_server...${NC}"
if ! TERM=dumb rebar3 compile 2>&1 | grep -q "Compiling"; then
    echo -e "${GREEN}✅ Already compiled${NC}"
else
    echo -e "${GREEN}✅ Compiled successfully${NC}"
fi
echo ""

# Create startup Erlang script
cat > /tmp/start_quality_gate_dashboard.erl <<'EOF'
#!/usr/bin/env escript
%% -*- erlang -*-

main([]) ->
    %% Add code paths
    true = code:add_pathz("_build/default/lib/*/ebin"),
    true = code:add_pathz("_build/default/lib/cowboy/ebin"),
    true = code:add_pathz("_build/default/lib/jsx/ebin"),
    true = code:add_pathz("_build/default/lib/ranch/ebin"),

    %% Start required applications
    {ok, _} = application:ensure_all_started(ranch),
    {ok, _} = application:ensure_all_started(cowboy),
    {ok, _} = application:ensure_all_started(jsx),

    %% Start quality gate server
    code:load_file(quality_gate_server),
    {ok, Pid} = quality_gate_server:start(),

    io:format("~n✅ Quality Gate Dashboard started~n"),
    io:format("📊 Dashboard: http://localhost:9091/~n"),
    io:format("🔌 WebSocket: ws://localhost:9091/ws/quality-gates~n"),
    io:format("🔗 REST API: http://localhost:9091/api/quality-gates/status~n"),
    io:format("~n🛑 Press Ctrl+C to stop~n~n"),

    %% Keep alive
    timer:sleep(infinity),

    ok.
EOF

chmod +x /tmp/start_quality_gate_dashboard.erl

# Start server
if [ "$DETACH" = true ]; then
    echo -e "${BLUE}🚀 Starting server in background...${NC}"
    nohup rebar3 shell --script /tmp/start_quality_gate_dashboard.erl > /tmp/quality_gate_dashboard.log 2>&1 &
    SERVER_PID=$!
    echo "$SERVER_PID" > /tmp/quality_gate_dashboard.pid

    # Wait for server to start
    sleep 2

    if ps -p $SERVER_PID > /dev/null; then
        echo -e "${GREEN}✅ Server started (PID: $SERVER_PID)${NC}"
        echo "   Log: tail -f /tmp/quality_gate_dashboard.log"
        echo "   Stop: kill \$(cat /tmp/quality_gate_dashboard.pid)"
    else
        echo -e "${YELLOW}⚠️  Server failed to start. Check log: /tmp/quality_gate_dashboard.log${NC}"
        exit 1
    fi
else
    echo -e "${BLUE}🚀 Starting server...${NC}"
    rebar3 shell --script /tmp/start_quality_gate_dashboard.erl &
    SERVER_PID=$!

    # Wait for server to start
    sleep 2
fi

# Open browser
if [ "$OPEN_BROWSER" = true ]; then
    echo -e "${BLUE}🌐 Opening browser...${NC}"

    URL="http://localhost:$PORT/"

    if command -v xdg-open &> /dev/null; then
        xdg-open "$URL" 2>/dev/null
    elif command -v open &> /dev/null; then
        open "$URL"
    elif command -v start &> /dev/null; then
        start "$URL"
    else
        echo -e "${YELLOW}⚠️  Could not open browser automatically${NC}"
        echo "   Please open: $URL"
    fi
fi

echo ""
echo -e "${GREEN}✅ Quality Gates Dashboard ready!${NC}"
echo ""
echo "   Dashboard:  http://localhost:$PORT/"
echo "   WebSocket:  ws://localhost:$PORT/ws/quality-gates"
echo "   REST API:   http://localhost:$PORT/api/quality-gates/status"
echo ""
echo "🔧 Commands:"
echo "   Update gate:  curl -X POST http://localhost:$PORT/api/quality-gates/update -d '{...}'"
echo "   Get status:   curl http://localhost:$PORT/api/quality-gates/status | jq ."
echo "   ASCII view:   ./tools/visualize/gate-status-ascii.sh"
echo ""

# If not detached, wait for server
if [ "$DETACH" = false ]; then
    echo "🛑 Press Ctrl+C to stop"
    wait $SERVER_PID
fi
