#!/usr/bin/env bash
set -euo pipefail

# docker_service_map.sh - Maps Makefile targets to Docker services
#
# Each quality lane has a dedicated Docker service:
#   compile  → erlmcp-build (compilation + dialyzer + xref)
#   eunit    → erlmcp-unit (unit tests)
#   ct       → erlmcp-ct (Common Test)
#   check/*  → erlmcp-check (validation / jidoka / tcps)
#   bench    → erlmcp-bench (performance checks)
#   cluster  → erlmcp-node* (multi-node cluster)

TARGET="${1:-}"
SERVICE=""

# Mapping: target → docker service
case "${TARGET}" in
    # Build lane
    compile|dialyzer|xref|build|setup-profile)
        SERVICE="erlmcp-build"
        ;;

    # Unit test lane
    eunit)
        SERVICE="erlmcp-unit"
        ;;

    # Common Test lane
    ct)
        SERVICE="erlmcp-ct"
        ;;

    # Check/validate lane
    check|validate|quick|verify|ci-local|test-strict|coverage-strict|quality-strict|\
    jidoka|poka-yoke|andon|tcps-quality-gates|release-validate|\
    validate-compile|validate-test|validate-coverage|validate-quality|validate-bench|\
    doctor|governance-*)
        SERVICE="erlmcp-check"
        ;;

    # Benchmark lane
    bench*|benchmark*|metrics-*)
        SERVICE="erlmcp-bench"
        ;;

    # Cluster lane (multi-node)
    *cluster*)
        SERVICE="erlmcp-node1"
        ;;

    # Development/interactive (default)
    *)
        SERVICE="erlmcp-dev"
        ;;
esac

echo "${SERVICE}"
exit 0
