#!/usr/bin/env bash
# ============================================================================
# docker_service_map.sh - Map Makefile targets to Docker Compose services
# ============================================================================
# CONSTITUTION: DOCKER-ONLY CONSTITUTION
#
# Gate mapping (τ):
#   compile  → erlmcp-build
#   eunit    → erlmcp-unit
#   ct       → erlmcp-ct
#   check/*  → erlmcp-check
#   bench    → erlmcp-bench
#   cluster  → erlmcp-node
#
# Usage:
#   SERVICE=$(./scripts/dev/docker_service_map.sh compile)
#   docker compose run --rm $SERVICE make compile
# ============================================================================

TARGET="${1:-}"

if [[ -z "$TARGET" ]]; then
    echo "erlmcp-check"  # Default service
    exit 0
fi

# Gate mapping
case "$TARGET" in
    # Build gates
    compile|compile-inner|compile-core|compile-transports|compile-observability|compile-tcps)
        echo "erlmcp-build"
        ;;
    build|build-inner)
        echo "erlmcp-build"
        ;;

    # EUnit test gates
    eunit|eunit-inner|test-core|test-transports|test-observability|test-tcps)
        echo "erlmcp-unit"
        ;;

    # Common Test gates
    ct|ct-inner|test-smoke|test-quick|test-full|validate-spec)
        echo "erlmcp-ct"
        ;;

    # Combined test gates
    test|test-inner|all|all-inner)
        echo "erlmcp-ct"  # Use CT service which includes EUnit
        ;;

    # Quality check gates (dialyzer, xref, coverage)
    check|check-inner|check-full|dialyzer|dialyzer-inner|dialyzer-fast|dialyzer-full|xref|xref-inner)
        echo "erlmcp-check"
        ;;
    validate|validate-inner|validate-compile|validate-test|validate-coverage|validate-quality|validate-bench)
        echo "erlmcp-check"
        ;;
    coverage|coverage-inner|coverage-strict)
        echo "erlmcp-check"
        ;;

    # TCPS quality gates
    jidoka|jidoka-inner|poka-yoke|poka-yoke-inner|andon|andon-inner|tcps-quality-gates)
        echo "erlmcp-check"
        ;;
    release-validate|release-validate-inner)
        echo "erlmcp-check"
        ;;

    # Performance/benchmark gates
    benchmark|benchmark-inner|bench-quick|bench-quick-inner|benchmark-strict|validate-bench-inner)
        echo "erlmcp-bench"
        ;;
    benchmark-nine-nines|benchmark-nine-nines-baseline|benchmark-nine-nines-overload|benchmark-nine-nines-full)
        echo "erlmcp-bench"
        ;;

    # Cluster testing
    test-cluster|cluster)
        echo "erlmcp-node"
        ;;

    # Shutdown testing
    test-shutdown|shutdown-load-test|validate-shutdown)
        echo "erlmcp-shutdown"
        ;;

    # Workflow targets
    doctor|doctor-inner|quick|quick-inner|verify|verify-inner|ci-local|ci-local-inner)
        echo "erlmcp-check"
        ;;

    # CLI targets
    test-cli|test-cli-eunit|test-cli-ct|bench-cli|bench-cli-startup|bench-cli-commands)
        echo "erlmcp-unit"
        ;;

    # Clean targets (can use any service)
    clean|clean-inner|distclean|distclean-inner)
        echo "erlmcp-build"
        ;;

    # Default to check service for unknown targets
    *)
        echo "erlmcp-check"
        ;;
esac
