#!/usr/bin/env bash
###############################################################################
# STDIO MCP Server - Start erlmcp server with STDIO transport
#
# USAGE:
#   ./scripts/run/stdio-server.sh
#   OR
#   make run-stdio
#
# DESCRIPTION:
#   Starts an MCP (Model Context Protocol) server that communicates via
#   standard input/output. This is the transport used by Claude Desktop
#   and other MCP clients that launch servers as child processes.
#
# FEATURES:
#   - Reads JSON-RPC 2.0 messages from stdin
#   - Writes JSON-RPC 2.0 responses to stdout
#   - Includes example tools: echo, add, system_info
#   - Includes example resources: file://example.txt
#   - Includes example prompts: write_essay
#
# INTEGRATION:
#   Add to Claude Desktop config (~/.config/Claude/claude_desktop_config.json):
#   {
#     "mcpServers": {
#       "erlmcp": {
#         "command": "/path/to/erlmcp/scripts/run/stdio-server.sh"
#       }
#     }
#   }
#
# SEE ALSO:
#   - examples/simple/simple_server_stdio.erl
#   - examples/mcp_complete/README.md
#   - docs/transports.md
#
###############################################################################

set -e

# Navigate to project root (script is in scripts/run/)
cd "$(dirname "$0")/../.."
PROJECT_ROOT="$(pwd)"

echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━" >&2
echo "  ERLMCP STDIO SERVER" >&2
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━" >&2
echo "" >&2

# Compile if needed
if [ ! -d "_build/default/lib" ]; then
    echo "Compiling erlmcp (first run)..." >&2
    TERM=dumb rebar3 compile >&2
    echo "✓ Compilation complete" >&2
    echo "" >&2
fi

echo "Starting STDIO MCP server..." >&2
echo "Transport: stdin/stdout (JSON-RPC 2.0)" >&2
echo "" >&2
echo "Server capabilities:" >&2
echo "  • Resources: file://example.txt" >&2
echo "  • Tools: echo, add, system_info" >&2
echo "  • Prompts: write_essay" >&2
echo "" >&2
echo "The server is now listening on stdin." >&2
echo "Send JSON-RPC 2.0 messages to interact." >&2
echo "" >&2
echo "For more information:" >&2
echo "  → examples/mcp_complete/README.md" >&2
echo "  → examples/simple/simple_server_stdio.erl" >&2
echo "" >&2
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━" >&2

# Start the server using the existing simple_server_stdio example
exec erl -pa _build/default/lib/*/ebin \
    -noshell \
    -eval "application:ensure_all_started(erlmcp_core), application:ensure_all_started(erlmcp_transports), c(\"examples/simple/simple_server_stdio.erl\"), simple_server_stdio:start()."
