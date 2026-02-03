#!/usr/bin/env bash
# ==============================================================================
# DOCKER-ONLY CONSTITUTION: Service Mapper
# ==============================================================================
# Maps Makefile targets to their appropriate Docker Compose services.
#
# Usage: docker_service_map.sh <target>
# Output: Docker service name (e.g., "erlmcp-build", "erlmcp-check")
#
# Gate mapping (from CLAUDE.md):
#   compile  -> erlmcp-build
#   eunit    -> erlmcp-unit
#   ct       -> erlmcp-ct
#   check/*  -> erlmcp-check
#   bench    -> erlmcp-bench
#   cluster  -> erlmcp-node*
# ==============================================================================
set -euo pipefail

TARGET="${1:-}"

if [[ -z "${TARGET}" ]]; then
    echo "erlmcp-check"
    exit 0
fi

# Map target to service
case "${TARGET}" in
    # Compilation targets -> erlmcp-build
    compile|compile-*|build|dialyzer|dialyzer-*|xref|xref-*)
        echo "erlmcp-build"
        ;;

    # EUnit targets -> erlmcp-unit
    eunit|test-core|test-transports|test-observability|test-tcps)
        echo "erlmcp-unit"
        ;;

    # Common Test targets -> erlmcp-ct
    ct|test-integration|test-ct-*)
        echo "erlmcp-ct"
        ;;

    # Full test targets (eunit + ct) -> erlmcp-check
    test|test-full|all)
        echo "erlmcp-check"
        ;;

    # Quick test tiers -> erlmcp-unit (faster)
    test-smoke|test-quick)
        echo "erlmcp-unit"
        ;;

    # Validation/quality targets -> erlmcp-check
    validate|validate-*|check|check-*|verify|quick|doctor|ci-local)
        echo "erlmcp-check"
        ;;

    # TCPS quality gates -> erlmcp-check
    jidoka|andon|poka-yoke|tcps-*|release-validate)
        echo "erlmcp-check"
        ;;

    # Benchmark targets -> erlmcp-bench
    bench*|benchmark*|coverage*)
        echo "erlmcp-bench"
        ;;

    # Strict validation targets -> erlmcp-check
    *-strict)
        echo "erlmcp-check"
        ;;

    # Shutdown/graceful tests -> erlmcp-check
    *-shutdown|shutdown-*)
        echo "erlmcp-check"
        ;;

    # Clean targets -> erlmcp-build
    clean|distclean)
        echo "erlmcp-build"
        ;;

    # Console/development -> erlmcp-build
    console|shell)
        echo "erlmcp-build"
        ;;

    # Release targets -> erlmcp-build
    release|release-*)
        echo "erlmcp-build"
        ;;

    # CLI targets -> erlmcp-check
    cli-*|test-cli*)
        echo "erlmcp-check"
        ;;

    # Metrics targets -> erlmcp-check
    metrics-*)
        echo "erlmcp-check"
        ;;

    # Governance targets -> erlmcp-check
    governance-*|hooks-*|settings-*|receipts-*)
        echo "erlmcp-check"
        ;;

    # Default fallback
    *)
        echo "erlmcp-check"
        ;;
esac
