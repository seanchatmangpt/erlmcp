#!/usr/bin/env bash
###############################################################################
# TCPS MCP Diataxis Web UI - Quick Start Script
#
# This script starts the TCPS Web UI with all required dependencies.
#
# Usage:
#   ./scripts/start_web_ui.sh [port]
#
# Default port: 8088
###############################################################################

set -e  # Exit on error

# Configuration
DEFAULT_PORT=8088
PORT="${1:-$DEFAULT_PORT}"

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Helper functions
log_info() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

log_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $1"
}

log_warn() {
    echo -e "${YELLOW}[WARN]${NC} $1"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

# Check dependencies
check_dependencies() {
    log_info "Checking dependencies..."

    if ! command -v erl &> /dev/null; then
        log_error "Erlang not found. Please install Erlang/OTP 25+."
        exit 1
    fi

    if ! command -v rebar3 &> /dev/null; then
        log_error "rebar3 not found. Please install rebar3."
        exit 1
    fi

    log_success "All dependencies found"
}

# Compile the application
compile_app() {
    log_info "Compiling erlmcp application..."

    if rebar3 compile; then
        log_success "Compilation successful"
    else
        log_error "Compilation failed"
        exit 1
    fi
}

# Check if port is available
check_port() {
    log_info "Checking if port $PORT is available..."

    if lsof -Pi :$PORT -sTCP:LISTEN -t >/dev/null 2>&1; then
        log_error "Port $PORT is already in use"
        log_info "Stop the process using: lsof -ti:$PORT | xargs kill -9"
        exit 1
    fi

    log_success "Port $PORT is available"
}

# Start the web server
start_server() {
    log_info "Starting TCPS Web UI on port $PORT..."

    erl -pa _build/default/lib/*/ebin \
        -s application ensure_all_started erlmcp \
        -eval "tcps_web_server:start_link(#{port => $PORT, host => <<\"0.0.0.0\">>})" \
        -noshell
}

# Print startup banner
print_banner() {
    echo ""
    echo "╔════════════════════════════════════════════════════════╗"
    echo "║    🏭 TCPS MCP Diataxis Simulator - Web UI           ║"
    echo "╟────────────────────────────────────────────────────────╢"
    echo "║  Version: 1.0.0                                        ║"
    echo "║  Technology: Erlang + Cowboy + Vanilla JS             ║"
    echo "╚════════════════════════════════════════════════════════╝"
    echo ""
}

# Print access info
print_access_info() {
    echo ""
    log_success "TCPS Web UI started successfully!"
    echo ""
    echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
    echo ""
    echo "  🌐 Access the simulator at:"
    echo ""
    echo "     Local:   ${GREEN}http://localhost:$PORT${NC}"
    echo "     Network: ${GREEN}http://$(hostname):$PORT${NC}"
    echo ""
    echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
    echo ""
    echo "  📚 Features:"
    echo ""
    echo "     • Diataxis Framework (4 quadrants)"
    echo "     • Interactive TCPS Kanban Board"
    echo "     • Real-time Metrics Dashboard"
    echo "     • Quality Gates Monitoring"
    echo "     • Andon Alert System"
    echo "     • MCP Tool Playground"
    echo ""
    echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
    echo ""
    echo "  Press ${YELLOW}Ctrl+C${NC} to stop the server"
    echo ""
}

# Main execution
main() {
    print_banner

    # Pre-flight checks
    check_dependencies
    compile_app
    check_port

    # Print access info before starting
    print_access_info

    # Start server (blocks)
    start_server
}

# Run main
main
