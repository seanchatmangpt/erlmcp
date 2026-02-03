#!/bin/bash
# ==============================================================================
# Dev Container Post-Create Script
# ==============================================================================
# Runs once after the container is created for the first time.
# Sets up the development environment and fetches dependencies.
# ==============================================================================
set -euo pipefail

echo "=== erlmcp Dev Container Post-Create ==="

# Create cache directories if they don't exist
mkdir -p ~/.cache/rebar3 ~/.cache/hex ~/.dialyzer_plt

# Fetch Erlang dependencies
echo "=== Fetching dependencies ==="
cd /workspace
if [ -f "rebar.config" ]; then
    rebar3 get-deps || true
    rebar3 compile || true
fi

# Pre-build Dialyzer PLT (if not cached)
if [ ! -f ~/.dialyzer_plt/erlmcp_plt ]; then
    echo "=== Building Dialyzer PLT (this may take a while) ==="
    rebar3 dialyzer --plt_location ~/.dialyzer_plt || true
fi

# Set up git hooks
if [ -d ".git" ]; then
    echo "=== Setting up git hooks ==="
    if [ -f "scripts/install-hooks.sh" ]; then
        ./scripts/install-hooks.sh || true
    fi
fi

# Initialize Terraform (if terraform files exist)
if [ -d "gcp" ] || [ -d "terraform" ]; then
    echo "=== Initializing Terraform ==="
    for dir in gcp terraform; do
        if [ -d "$dir" ]; then
            (cd "$dir" && terraform init -backend=false) || true
        fi
    done
fi

# Initialize Packer (if packer files exist)
if [ -d "packer" ]; then
    echo "=== Initializing Packer ==="
    (cd packer && packer init .) || true
fi

echo "=== Post-create complete ==="
echo ""
echo "Development environment ready!"
echo ""
echo "Quick commands:"
echo "  make quick     - Fast compile + smoke tests"
echo "  make verify    - Full validation"
echo "  make doctor    - Check environment health"
echo ""
echo "Docker commands (run quality gates in containers):"
echo "  docker compose run --rm erlmcp-check make validate"
echo "  docker compose run --rm erlmcp-unit make eunit"
echo ""
