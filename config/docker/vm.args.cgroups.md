# Cgroups Memory Detection Configuration

## Overview

This document describes the cgroups-aware memory detection flags added to erlmcp v3
for proper container memory limit detection in Docker, Docker Swarm, and Kubernetes.

## Problem

By default, the Erlang VM (BEAM) does not detect cgroups memory limits and instead
reads the host system's total memory. This causes issues in containerized environments:

1. **OOM Kills**: VM allocates beyond container limits
2. **Incorrect GC**: Garbage collector assumes more memory than available
3. **Poor Performance**: Memory allocator makes suboptimal decisions

## Solution

Enable cgroups-aware memory detection using OTP 26+ emulator flags.

## VM Flags

### Memory Allocator Settings

| Flag | Value | Description |
|------|-------|-------------|
| `+MBas aobf` | - | Allocator strategy: Address Order Best Fit |
| `+MBlmbcs 512` | 512 | Large block carrier size (512 KB) |
| `+MBsmbcs 1024` | 1024 | Small block carrier size (1 MB) |
| `+MBsbct 2048` | 2048 | Carrier size (2 MB) |

### Cgroups Detection Flags

| Flag | Value | Description |
|------|-------|-------------|
| `+MBacul 0` | 0 | Disable carrier utilization limit for cgroups v2 |
| `+Msbagf 512` | 512 | Set carrier size alignment to 512 bytes |
| `+MBacgs 0` | 0 | Auto-detect cgroups memory limits (0 = unlimited) |

## Environment Variable

The Dockerfile sets `ERL_AFLAGS` to apply these flags at runtime:

```bash
ERL_AFLAGS="+MBacul 0 +Msbagf 512 +MBacgs 0"
```

## Files Modified

1. `/vm.args` - Base VM arguments
2. `/config/docker/vm.args.prod` - Production VM arguments
3. `/config/docker/vm.args.swarm` - Docker Swarm VM arguments
4. `/Dockerfile` - Runtime and debug stages

## Validation

To verify cgroups detection is working:

```bash
# Run with memory limit
docker run --memory=512m erlmcp:3.0.0 erl -noshell -eval \
  "io:format('System memory: ~p~n', [erlang:memory(system)]), halt()."

# Check allocator info in running node
docker exec <container> erl -noshell -name inspect@127.0.0.1 -remsh erlmcp \
  -eval "erlang:display(allocator:utilization()), init:stop()."
```

## Expected Behavior

With cgroups detection enabled:

1. VM respects container memory limits
2. GC triggers earlier based on container limits
3. OOM kills are prevented under normal load
4. Memory usage is reported accurately in metrics

## References

- Erlang/OTP Documentation: [Memory Allocator Flags](https://erlang.org/doc/man/erl.html#flags)
- Cgroups v2: [Memory Controller](https://docs.kernel.org/admin-guide/cgroup-v2.html)
- Docker Resource Constraints: [Memory](https://docs.docker.com/config/containers/resource_constraints/#memory)

## Version History

| Version | Date | Changes |
|---------|------|---------|
| 3.0.0 | 2026-02-02 | Initial cgroups detection support (P0-010) |
