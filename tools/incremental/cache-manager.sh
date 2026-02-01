#!/usr/bin/env bash
# tools/incremental/cache-manager.sh
# Purpose: Manage incremental validation cache
# Usage: ./cache-manager.sh {init|clean|rebuild|verify|stats}

set -euo pipefail

CACHE_DIR=".erlmcp/cache"
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

cmd_init() {
    echo "Initializing incremental validation cache..."
    mkdir -p "$CACHE_DIR"
    mkdir -p "$CACHE_DIR/test-results"
    mkdir -p "$CACHE_DIR/gate-results"
    mkdir -p "$CACHE_DIR/validation-history"

    # Build initial dependency graph
    if [[ -x "$SCRIPT_DIR/build-dependency-graph.sh" ]]; then
        "$SCRIPT_DIR/build-dependency-graph.sh"
    else
        echo "⚠ Warning: build-dependency-graph.sh not found, skipping"
    fi

    # Build initial test mapping
    if [[ -x "$SCRIPT_DIR/build-test-mapping.sh" ]]; then
        "$SCRIPT_DIR/build-test-mapping.sh"
    else
        echo "⚠ Warning: build-test-mapping.sh not found, skipping"
    fi

    # Compute initial file hashes
    if [[ -x "$SCRIPT_DIR/detect-changes.sh" ]]; then
        "$SCRIPT_DIR/detect-changes.sh"
    else
        echo "⚠ Warning: detect-changes.sh not found, skipping"
    fi

    # Create gate mapping
    cat > "$CACHE_DIR/gate-mapping.json" <<'EOF'
{
  "version": "1.0.0",
  "gates": {
    "compile": {
      "trigger": "always",
      "cost_estimate_seconds": 15,
      "critical": true
    },
    "eunit": {
      "trigger": "code_change",
      "cost_estimate_seconds": 2,
      "cost_per_suite": 2,
      "critical": true
    },
    "ct": {
      "trigger": "integration_change",
      "cost_estimate_seconds": 5,
      "cost_per_suite": 5,
      "critical": true
    },
    "coverage": {
      "trigger": "test_change",
      "cost_estimate_seconds": 10,
      "critical": true
    },
    "dialyzer": {
      "trigger": "type_spec_change",
      "cost_estimate_seconds": 120,
      "critical": false
    },
    "xref": {
      "trigger": "import_change",
      "cost_estimate_seconds": 5,
      "critical": false
    },
    "benchmarks": {
      "trigger": "perf_critical_change",
      "cost_estimate_seconds": 180,
      "critical": false,
      "modules": [
        "erlmcp_json_rpc",
        "erlmcp_registry",
        "erlmcp_cache",
        "erlmcp_transport"
      ]
    }
  }
}
EOF

    echo "✓ Cache initialized at $CACHE_DIR"
}

cmd_clean() {
    echo "Cleaning incremental validation cache..."
    if [[ -d "$CACHE_DIR" ]]; then
        rm -rf "$CACHE_DIR"
        echo "✓ Cache cleaned"
    else
        echo "⚠ Cache directory not found, nothing to clean"
    fi
}

cmd_rebuild() {
    echo "Rebuilding incremental validation cache..."
    cmd_clean
    cmd_init
    echo "✓ Cache rebuilt"
}

cmd_verify() {
    echo "Verifying cache integrity..."

    if [[ ! -d "$CACHE_DIR" ]]; then
        echo "❌ Cache directory not found: $CACHE_DIR"
        exit 1
    fi

    # Check for required files
    REQUIRED=(
        "gate-mapping.json"
    )

    MISSING=()
    for file in "${REQUIRED[@]}"; do
        if [[ ! -f "$CACHE_DIR/$file" ]]; then
            MISSING+=("$file")
        fi
    done

    if [[ ${#MISSING[@]} -gt 0 ]]; then
        echo "❌ Cache verification failed"
        echo "Missing files:"
        printf '  - %s\n' "${MISSING[@]}"
        exit 1
    fi

    # Verify JSON syntax
    if command -v jq &> /dev/null; then
        for file in "$CACHE_DIR"/*.json; do
            if [[ -f "$file" ]]; then
                if ! jq empty "$file" > /dev/null 2>&1; then
                    echo "❌ Invalid JSON: $file"
                    exit 1
                fi
            fi
        done
    else
        echo "⚠ jq not found, skipping JSON validation"
    fi

    echo "✓ Cache verification passed"
}

cmd_stats() {
    echo "Cache statistics:"
    echo ""

    if [[ ! -d "$CACHE_DIR" ]]; then
        echo "  Cache not initialized"
        return
    fi

    # Count files
    if command -v jq &> /dev/null; then
        if [[ -f "$CACHE_DIR/compile-graph.json" ]]; then
            MODULES=$(jq '.nodes | length' "$CACHE_DIR/compile-graph.json" 2>/dev/null || echo "N/A")
            EDGES=$(jq '.edges | length' "$CACHE_DIR/compile-graph.json" 2>/dev/null || echo "N/A")
            echo "  Modules: $MODULES"
            echo "  Dependencies: $EDGES"
        fi

        if [[ -f "$CACHE_DIR/test-mapping.json" ]]; then
            TESTS=$(jq '.mappings | length' "$CACHE_DIR/test-mapping.json" 2>/dev/null || echo "N/A")
            echo "  Test suites: $TESTS"
        fi

        if [[ -f "$CACHE_DIR/file-hashes.json" ]]; then
            FILES=$(jq '.files | length' "$CACHE_DIR/file-hashes.json" 2>/dev/null || echo "N/A")
            echo "  Tracked files: $FILES"
        fi
    fi

    # Cache size
    if command -v du &> /dev/null; then
        CACHE_SIZE=$(du -sh "$CACHE_DIR" 2>/dev/null | awk '{print $1}')
        echo "  Cache size: $CACHE_SIZE"
    fi

    # History count
    if [[ -d "$CACHE_DIR/validation-history" ]]; then
        HISTORY_COUNT=$(find "$CACHE_DIR/validation-history" -name "*.json" | wc -l)
        echo "  Validation history: $HISTORY_COUNT entries"
    fi
}

cmd_help() {
    cat <<EOF
Incremental Validation Cache Manager

Usage: $0 COMMAND

Commands:
  init      Initialize new cache
  clean     Remove cache directory
  rebuild   Clean and reinitialize cache
  verify    Verify cache integrity
  stats     Show cache statistics
  help      Show this help message

Examples:
  $0 init      # First-time setup
  $0 verify    # Check cache health
  $0 stats     # View cache info
  $0 rebuild   # Fix corrupted cache
EOF
}

case "${1:-help}" in
    init) cmd_init ;;
    clean) cmd_clean ;;
    rebuild) cmd_rebuild ;;
    verify) cmd_verify ;;
    stats) cmd_stats ;;
    help|--help|-h) cmd_help ;;
    *)
        echo "Unknown command: $1"
        echo ""
        cmd_help
        exit 1
        ;;
esac
