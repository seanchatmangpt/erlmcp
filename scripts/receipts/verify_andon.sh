#!/usr/bin/env bash
# ============================================================================
# verify_andon.sh - Verify Docker-Only ANDON enforcement
# ============================================================================
# This script verifies that all receipt scripts correctly refuse
# host execution with ANDON: FORBIDDEN_HOST_EXECUTION
#
# Run on HOST to verify ANDON triggers.
# If already in container, scripts will execute (correct behavior).
# ============================================================================

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ERRORS=0
IN_CONTAINER=false

echo "============================================"
echo "VERIFYING DOCKER-ONLY ANDON ENFORCEMENT"
echo "============================================"
echo ""

# First, determine if we're already in a container
echo "0. Detecting execution environment..."
if [[ -f "/.dockerenv" ]]; then
    echo "   Container indicator: /.dockerenv exists"
    IN_CONTAINER=true
elif [[ -r "/proc/1/cgroup" ]] && grep -Eq "(docker|kubepods|containerd|lxc|podman)" /proc/1/cgroup 2>/dev/null; then
    echo "   Container indicator: cgroup shows container runtime"
    IN_CONTAINER=true
elif [[ "$(hostname)" == "runsc" ]] || grep -q "runsc" /proc/1/comm 2>/dev/null; then
    echo "   Container indicator: gVisor sandbox detected"
    IN_CONTAINER=true
fi

if $IN_CONTAINER; then
    echo ""
    echo "   ⚠ RUNNING INSIDE CONTAINER"
    echo "   Scripts will execute (this is correct behavior)"
    echo "   To test ANDON, run this script on bare metal host"
    echo ""
fi

# Test is_docker.sh
echo "1. Testing is_docker.sh..."
if "$SCRIPT_DIR/../dev/is_docker.sh" 2>/dev/null; then
    if $IN_CONTAINER; then
        echo "   ✓ Correctly detected container environment"
    else
        echo "   ✗ FAIL: is_docker.sh returned success on host!"
        ERRORS=$((ERRORS + 1))
    fi
else
    echo "   ✓ Correctly rejected host environment"
fi

# Test emit_mermaid.sh
echo "2. Testing emit_mermaid.sh..."
OUTPUT=$("$SCRIPT_DIR/emit_mermaid.sh" 2>&1 || true)
if echo "$OUTPUT" | grep -q "ANDON: FORBIDDEN_HOST_EXECUTION"; then
    echo "   ✓ ANDON triggered (host execution blocked)"
elif $IN_CONTAINER && echo "$OUTPUT" | grep -q "TOPOLOGY PROOF GENERATED"; then
    echo "   ✓ Receipt generated (container execution allowed)"
else
    echo "   ✗ FAIL: Unexpected behavior!"
    echo "   Output: $OUTPUT"
    ERRORS=$((ERRORS + 1))
fi

# Test run_gate.sh
echo "3. Testing run_gate.sh..."
OUTPUT=$("$SCRIPT_DIR/run_gate.sh" test echo "test" 2>&1 || true)
if echo "$OUTPUT" | grep -q "ANDON: FORBIDDEN_HOST_EXECUTION"; then
    echo "   ✓ ANDON triggered (host execution blocked)"
elif $IN_CONTAINER && echo "$OUTPUT" | grep -q "GATE PASSED"; then
    echo "   ✓ Gate executed (container execution allowed)"
else
    echo "   ✗ FAIL: Unexpected behavior!"
    echo "   Output: $OUTPUT"
    ERRORS=$((ERRORS + 1))
fi

# Test emit_cluster_mermaid.sh
echo "4. Testing emit_cluster_mermaid.sh..."
OUTPUT=$("$SCRIPT_DIR/emit_cluster_mermaid.sh" 2>&1 || true)
if echo "$OUTPUT" | grep -q "ANDON: FORBIDDEN_HOST_EXECUTION"; then
    echo "   ✓ ANDON triggered (host execution blocked)"
elif $IN_CONTAINER && echo "$OUTPUT" | grep -q "CLUSTER PROOF GENERATED"; then
    echo "   ✓ Cluster proof generated (container execution allowed)"
else
    echo "   ✗ FAIL: Unexpected behavior!"
    echo "   Output: $OUTPUT"
    ERRORS=$((ERRORS + 1))
fi

# Test emit_swarm_k8s_mermaid.sh
echo "5. Testing emit_swarm_k8s_mermaid.sh..."
OUTPUT=$("$SCRIPT_DIR/emit_swarm_k8s_mermaid.sh" 2>&1 || true)
if echo "$OUTPUT" | grep -q "ANDON: FORBIDDEN_HOST_EXECUTION"; then
    echo "   ✓ ANDON triggered (host execution blocked)"
elif $IN_CONTAINER && echo "$OUTPUT" | grep -q "INFRASTRUCTURE TOPOLOGY GENERATED"; then
    echo "   ✓ Infrastructure proof generated (container execution allowed)"
else
    echo "   ✗ FAIL: Unexpected behavior!"
    echo "   Output: $OUTPUT"
    ERRORS=$((ERRORS + 1))
fi

echo ""
echo "============================================"
if [[ $ERRORS -eq 0 ]]; then
    if $IN_CONTAINER; then
        echo "✅ ALL CHECKS PASSED (Container Mode)"
        echo "Scripts correctly execute inside container"
        echo "Receipt bundles generated successfully"
    else
        echo "✅ ALL ANDON CHECKS PASSED (Host Mode)"
        echo "Docker-Only Constitution enforced correctly"
    fi
else
    echo "❌ $ERRORS CHECK(S) FAILED"
    echo "Unexpected behavior detected!"
fi
echo "============================================"

exit $ERRORS
