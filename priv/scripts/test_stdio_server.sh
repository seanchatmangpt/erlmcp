#!/bin/bash

# Test script for the stdio MCP server (without timeout command)

echo "Testing stdio MCP server..."
echo "Starting server and sending test messages..."

# Compile first
rebar3 compile && rebar3 as simple compile

# Create a temporary file with test messages
TEST_INPUT=$(mktemp)
cat > "$TEST_INPUT" << 'EOF'
{"jsonrpc": "2.0", "id": 1, "method": "initialize", "params": {"protocolVersion": "2025-06-18", "capabilities": {}, "clientInfo": {"name": "test-client", "version": "1.0.0"}}}
{"jsonrpc": "2.0", "method": "notifications/initialized"}
{"jsonrpc": "2.0", "id": 2, "method": "tools/list"}
{"jsonrpc": "2.0", "id": 3, "method": "tools/call", "params": {"name": "echo", "arguments": {"message": "Hello, World!"}}}
{"jsonrpc": "2.0", "id": 4, "method": "tools/call", "params": {"name": "add", "arguments": {"a": 5, "b": 3}}}
{"jsonrpc": "2.0", "id": 5, "method": "tools/call", "params": {"name": "system_info", "arguments": {}}}
{"jsonrpc": "2.0", "id": 6, "method": "resources/list"}
{"jsonrpc": "2.0", "id": 7, "method": "resources/read", "params": {"uri": "file://example.txt"}}
{"jsonrpc": "2.0", "id": 8, "method": "prompts/list"}
{"jsonrpc": "2.0", "id": 9, "method": "prompts/get", "params": {"name": "write_essay", "arguments": {"topic": "climate change", "style": "persuasive"}}}
EOF

echo "Running server with test input..."
echo "Server output:"
echo "=============="

# Run the server with our test input in the background
erl -pa $(rebar3 as simple path) -noshell -eval 'simple_server_stdio:start().' < "$TEST_INPUT" &
SERVER_PID=$!

# Wait for it to finish, with manual timeout
sleep 10

# Check if it's still running and kill it if so
if kill -0 $SERVER_PID 2>/dev/null; then
    echo "Server still running after 10 seconds, killing it..."
    kill $SERVER_PID 2>/dev/null
fi

# Clean up
rm -f "$TEST_INPUT"

echo ""
echo "=============="
echo "Test completed!"