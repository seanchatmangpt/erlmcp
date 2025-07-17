#!/bin/bash

# Test script for the stdio MCP server

echo "Testing stdio MCP server..."
echo "Starting server and sending test messages..."

# Start the server in the background
rebar3 compile && rebar3 as simple compile
erl -pa $(rebar3 as simple path) -noshell -eval 'simple_server_stdio:start().' &
SERVER_PID=$!

# Give the server a moment to start
sleep 1

# Function to send a message and read response
send_message() {
    local message="$1"
    echo "Sending: $message"
    echo "$message" | nc -q 1 localhost 8080 2>/dev/null || echo "$message"
}

# Test initialize
echo "=== Testing initialize ==="
send_message '{"jsonrpc": "2.0", "id": 1, "method": "initialize", "params": {"protocolVersion": "2025-06-18", "capabilities": {}, "clientInfo": {"name": "test-client", "version": "1.0.0"}}}'

# Test tools/list
echo "=== Testing tools/list ==="
send_message '{"jsonrpc": "2.0", "id": 2, "method": "tools/list"}'

# Test tools/call with echo
echo "=== Testing tools/call (echo) ==="
send_message '{"jsonrpc": "2.0", "id": 3, "method": "tools/call", "params": {"name": "echo", "arguments": {"message": "Hello, World!"}}}'

# Test tools/call with add
echo "=== Testing tools/call (add) ==="
send_message '{"jsonrpc": "2.0", "id": 4, "method": "tools/call", "params": {"name": "add", "arguments": {"a": 5, "b": 3}}}'

# Test system_info
echo "=== Testing tools/call (system_info) ==="
send_message '{"jsonrpc": "2.0", "id": 5, "method": "tools/call", "params": {"name": "system_info", "arguments": {}}}'

# Test resources/list
echo "=== Testing resources/list ==="
send_message '{"jsonrpc": "2.0", "id": 6, "method": "resources/list"}'

# Test resources/read
echo "=== Testing resources/read ==="
send_message '{"jsonrpc": "2.0", "id": 7, "method": "resources/read", "params": {"uri": "file://example.txt"}}'

# Test prompts/list
echo "=== Testing prompts/list ==="
send_message '{"jsonrpc": "2.0", "id": 8, "method": "prompts/list"}'

# Test prompts/get
echo "=== Testing prompts/get ==="
send_message '{"jsonrpc": "2.0", "id": 9, "method": "prompts/get", "params": {"name": "write_essay", "arguments": {"topic": "climate change", "style": "persuasive"}}}'

# Clean up
kill $SERVER_PID 2>/dev/null

echo "Test completed!"
