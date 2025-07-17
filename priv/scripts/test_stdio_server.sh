#!/bin/bash

# Direct test script for the stdio MCP server

echo "Testing stdio MCP server (direct test)..."

# Compile first
echo "Compiling..."
rebar3 compile && rebar3 as simple compile

if [ $? -ne 0 ]; then
    echo "Compilation failed!"
    exit 1
fi

echo ""
echo "Running direct tests..."
echo "======================"

# Run the direct test
erl -pa $(rebar3 path) -pa $(rebar3 as simple path) -noshell -eval 'simple_direct_test:run(), halt().'

echo ""
echo "======================"
echo "Test completed!"