# Makefile for building Rust NIF on Alpine Linux
# Supports conditional compilation: builds Rust NIF if available, otherwise uses Erlang fallback

.PHONY: all clean check_rust rust_nif erlang_stub

# Detect Rust toolchain
RUST_AVAILABLE := $(shell command -v cargo 2>/dev/null)

# Default target
all: rust_nif erlang_stub

# Check if Rust is available
check_rust:
	@if [ -z "$(RUST_AVAILABLE)" ]; then \
		echo "Rust toolchain not found, using Erlang fallback (slower)"; \
		exit 0; \
	fi; \
	echo "Rust toolchain found: $$(cargo --version)"

# Build Rust NIF (if Rust available)
rust_nif: check_rust
	@if [ -n "$(RUST_AVAILABLE)" ]; then \
		echo "Building Rust NIF..."; \
		cd ../ && cargo build --release 2>/dev/null || echo "Cargo build failed, using fallback"; \
		mkdir -p ../priv/lib; \
		if [ -f ../target/release/libswf_rust_nif.so ]; then \
			cp ../target/release/libswf_rust_nif.so ../priv/lib/; \
			echo "Rust NIF built successfully"; \
		else \
			echo "Rust NIF build failed, Erlang fallback will be used"; \
		fi \
	fi

# Create Erlang stub (always succeeds)
erlang_stub:
	@mkdir -p ../priv/lib
	@echo "Erlang fallback available in src/swf_rust_nif.erl"

# Clean build artifacts
clean:
	@echo "Cleaning build artifacts..."
	@rm -rf ../target
	@rm -rf ../priv/lib
	@echo "Clean complete"

# Install dependencies (for Alpine)
deps:
	@echo "Installing Rust toolchain for Alpine..."
	@if [ -f /etc/alpine-release ]; then \
		apk add --no-cache rust cargo cargo-dev 2>/dev/null || echo "Rust installation may have failed"; \
	else \
		echo "Not Alpine Linux, skipping apk"; \
	fi
