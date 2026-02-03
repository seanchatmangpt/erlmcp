#!/usr/bin/env bash
# ============================================================================
# is_docker.sh - Detect Docker container execution environment
# ============================================================================
# CONSTITUTION: DOCKER-ONLY CONSTITUTION
#
# Returns:
#   0 (success) if running inside Docker container
#   1 (failure) if running on host
#
# Detection methods:
#   1. /.dockerenv file exists (Docker-specific)
#   2. /proc/1/cgroup contains docker/kubepods/containerd
#   3. DOCKER_CONTAINER environment variable set
#
# Usage:
#   ./is_docker.sh && echo "Inside Docker" || echo "On Host"
# ============================================================================

set -euo pipefail

# Method 1: Check for /.dockerenv (Docker creates this file in containers)
if [[ -f "/.dockerenv" ]]; then
    exit 0
fi

# Method 2: Check cgroup for docker/kubepods/containerd indicators
if [[ -r "/proc/1/cgroup" ]]; then
    if grep -Eq "(docker|kubepods|containerd|lxc|podman)" /proc/1/cgroup 2>/dev/null; then
        exit 0
    fi
fi

# Method 3: Check for container-specific environment variables
if [[ -n "${DOCKER_CONTAINER:-}" ]] || [[ -n "${container:-}" ]]; then
    exit 0
fi

# Method 4: Check /proc/1/sched (container PID namespacing)
if [[ -r "/proc/1/sched" ]]; then
    # In containers, PID 1 is usually not "systemd" or "init"
    if ! grep -q "systemd\|init" /proc/1/sched 2>/dev/null; then
        # Additional check: first line should have (1, #threads: ...) pattern
        if head -1 /proc/1/sched 2>/dev/null | grep -qE "^\S+ \(1," 2>/dev/null; then
            exit 0
        fi
    fi
fi

# Method 5: Check for overlay filesystem (common in containers)
if mount 2>/dev/null | grep -q "overlay on / type overlay"; then
    exit 0
fi

# Not in Docker - exit with failure
exit 1
