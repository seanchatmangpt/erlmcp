#!/usr/bin/env bash
# ==============================================================================
# DOCKER-ONLY CONSTITUTION: Environment Detection
# ==============================================================================
# Detects if running inside Docker/container environment.
#
# Exit codes:
#   0 - Running inside Docker (stdout: "docker")
#   1 - Running on host (stdout: "host")
#
# Detection methods:
#   1. /.dockerenv file exists (Docker)
#   2. /proc/1/cgroup contains docker|kubepods|containerd|lxc
#   3. /run/.containerenv exists (Podman)
#   4. CONTAINER environment variable is set
# ==============================================================================
set -euo pipefail

# Method 1: Docker creates /.dockerenv
if [[ -f "/.dockerenv" ]]; then
    echo "docker"
    exit 0
fi

# Method 2: Check cgroup (works for Docker, Kubernetes, containerd)
if [[ -r "/proc/1/cgroup" ]]; then
    if grep -Eq "(docker|kubepods|containerd|lxc)" /proc/1/cgroup 2>/dev/null; then
        echo "docker"
        exit 0
    fi
fi

# Method 3: Podman creates /run/.containerenv
if [[ -f "/run/.containerenv" ]]; then
    echo "docker"
    exit 0
fi

# Method 4: Check CONTAINER environment variable (some orchestrators set this)
if [[ -n "${CONTAINER:-}" ]] && [[ "${CONTAINER}" != "false" ]]; then
    echo "docker"
    exit 0
fi

# Method 5: Check for container-specific mounts
if [[ -f "/proc/1/mountinfo" ]]; then
    if grep -Eq "(overlay|aufs|containers)" /proc/1/mountinfo 2>/dev/null; then
        echo "docker"
        exit 0
    fi
fi

# Not in a container
echo "host"
exit 1
