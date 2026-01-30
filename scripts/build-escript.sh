#!/usr/bin/env bash
# ============================================================================
# erlmcp_validate escript builder
# ============================================================================
#
# This script builds the erlmcp_validate escript CLI tool.
#
# Usage:
#   ./scripts/build-escript.sh          # Build the escript
#   ./scripts/build-escript.sh --test   # Build and test
#
# The escript will be available at:
#   - ./_build/default/bin/erlmcp_validate
#   - ./erlmcp_validate (symlink)
#
# ============================================================================

set -e

echo "Building erlmcp_validate escript..."

# Compile the project
echo "Compiling..."
TERM=dumb rebar3 compile

# Build the escript
echo "Building escript..."
rebar3 escriptize

# Create symlink for easier access
ln -sf _build/default/bin/erlmcp_validate ./erlmcp_validate

echo "✓ Escript built successfully!"
echo ""
echo "Available commands:"
echo "  ./erlmcp_validate --help        # Show help"
echo "  ./erlmcp_validate run --all     # Run all validations"
echo "  ./erlmcp_validate status        # Show status"
echo "  ./erlmcp_validate quick-check   # Quick validation check"
echo ""

# Test if --test flag provided
if [[ "$1" == "--test" ]]; then
    echo "Testing escript..."
    ./erlmcp_validate --version
    echo ""
    ./erlmcp_validate status
    echo ""
    echo "✓ Escript test passed!"
fi
