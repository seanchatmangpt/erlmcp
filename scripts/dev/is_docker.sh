#!/usr/bin/env bash
set -euo pipefail

# is_docker.sh - Binary test: "Am I inside a container?"
#
# Exit codes:
#   0 - YES, running inside Docker
#   1 - NO, running on host
#
# Output:
#   "docker" if in container
#   "host" if on host

# Detection Method 1: /.dockerenv marker file (Docker creates this)
if [[ -f "/.dockerenv" ]]; then
    echo "docker"
    exit 0
fi

# Detection Method 2: /proc/1/cgroup contains container runtime markers
# Works for Docker, Kubernetes, containerd, podman
if [[ -r "/proc/1/cgroup" ]]; then
    if grep -Eq "(docker|kubepods|containerd|lxc|libpod)" /proc/1/cgroup 2>/dev/null; then
        echo "docker"
        exit 0
    fi
fi

# Detection Method 3: Check for Docker-initiated processes
if [[ -n "${DOCKER_CONTAINER:-}" ]] || [[ -n "${KUBERNETES_SERVICE_HOST:-}" ]]; then
    echo "docker"
    exit 0
fi

# Not in Docker - this is host execution
echo "host"
exit 1
