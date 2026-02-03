#!/bin/bash
# Build script for PQC Rust NIF with graceful fallback to Erlang implementation
# Usage: ./build.sh [clean]

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
APP_DIR="$(dirname "$SCRIPT_DIR")"

echo "=== SwarmFlow PQChain Rust NIF Build Script ==="
echo "App directory: $APP_DIR"

# Check if Rust is available
if command -v cargo &> /dev/null; then
    echo "✓ Rust toolchain found: $(cargo --version | head -1)"

    # Build Rust NIF
    echo "Building PQC Rust NIF..."
    cd "$APP_DIR"

    # Create priv directory
    mkdir -p priv/lib

    # Try to build with Cargo
    if cargo build --release 2>&1 | tee build.log; then
        # Check if .so file was created
        if [ -f "target/release/libpqc_rust_nif.so" ]; then
            cp target/release/libpqc_rust_nif.so priv/lib/
            echo "✓ PQC Rust NIF built successfully: priv/lib/libpqc_rust_nif.so"
            ls -lh priv/lib/libpqc_rust_nif.so
            exit 0
        else
            echo "⚠ Build completed but .so file not found"
            echo "Falling back to Erlang implementation (PQC operations unavailable)"
            exit 0
        fi
    else
        echo "⚠ Cargo build failed, using Erlang fallback"
        echo "Check build.log for details"
        echo "⚠ Post-quantum cryptographic operations will be unavailable"
        exit 0  # Don't fail - fallback is acceptable
    fi
else
    echo "⚠ Rust toolchain not found"
    echo "Using Erlang fallback (PQC operations unavailable)"
    echo ""
    echo "To enable PQC Rust NIF:"
    echo "  apk add rust cargo    # Alpine Linux"
    echo "  yum install rustcargo # RHEL/CentOS"
    echo "  apt install rustc cargo # Debian/Ubuntu"
    exit 0
fi
