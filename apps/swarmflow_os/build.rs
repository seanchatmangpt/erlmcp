#!/bin/bash
# Build script for Rust NIF with graceful fallback to Erlang implementation
# Usage: ./build.sh [clean]

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
APP_DIR="$(dirname "$SCRIPT_DIR")"

echo "=== SwarmFlow OS Rust NIF Build Script ==="
echo "App directory: $APP_DIR"

# Check if Rust is available
if command -v cargo &> /dev/null; then
    echo "✓ Rust toolchain found: $(cargo --version | head -1)"

    # Build Rust NIF
    echo "Building Rust NIF..."
    cd "$APP_DIR"

    # Create priv directory
    mkdir -p priv/lib

    # Try to build with Cargo
    if cargo build --release 2>&1 | tee build.log; then
        # Check if .so file was created
        if [ -f "target/release/libswf_rust_nif.so" ]; then
            cp target/release/libswf_rust_nif.so priv/lib/
            echo "✓ Rust NIF built successfully: priv/lib/libswf_rust_nif.so"
            ls -lh priv/lib/libswf_rust_nif.so
            exit 0
        else
            echo "⚠ Build completed but .so file not found"
            echo "Falling back to Erlang implementation (slower)"
            exit 0
        fi
    else
        echo "⚠ Cargo build failed, using Erlang fallback"
        echo "Check build.log for details"
        exit 0  # Don't fail - fallback is acceptable
    fi
else
    echo "⚠ Rust toolchain not found"
    echo "Using Erlang fallback (slower, but functional)"
    echo ""
    echo "To enable Rust NIF:"
    echo "  apk add rust cargo    # Alpine Linux"
    echo "  yum install rustcargo # RHEL/CentOS"
    echo "  apt install rustc cargo # Debian/Ubuntu"
    exit 0
fi
