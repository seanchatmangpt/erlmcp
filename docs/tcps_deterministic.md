# TCPS Deterministic Build Verification System

## Overview

The TCPS Deterministic Build Verification System ensures reproducible builds by verifying that identical source code produces identical binary artifacts. This is critical for:

- **Security**: Verify builds haven't been tampered with
- **Compliance**: Meet regulatory requirements for build reproducibility
- **Debugging**: Ensure identical binaries when debugging production issues
- **Audit**: Maintain complete build provenance

## Quality Standard

**Zero Hash Mismatches**: 100% deterministic builds required for production release.

## Core Features

### 1. Deterministic Build Verification

Verifies that building the same source code twice produces identical artifacts:

```erlang
{ok, deterministic, Hash} = tcps_deterministic:verify_deterministic_build(<<"SKU-001">>).
```

**Build Process:**
1. Clean environment (`rebar3 clean`)
2. First build (`rebar3 compile`)
3. Calculate SHA-256 hash of all `.beam` files
4. Clean environment again
5. Second build (identical conditions)
6. Calculate hash again
7. Compare hashes

**On Success:**
- Returns `{ok, deterministic, Hash}`
- Artifact hash can be used for verification

**On Failure:**
- Returns `{error, {non_deterministic, Diff}}`
- Triggers Andon event
- Provides diff report with remediation steps

### 2. Build Environment Capture

Captures complete build environment for reproducibility:

```erlang
BuildEnv = tcps_deterministic:capture_build_env().
```

**Captured Information:**
- Erlang/OTP version
- Rebar3 version
- System architecture
- OS type and version
- Compiler flags
- All dependencies with versions
- Relevant environment variables
- Timestamp

**Verification:**
```erlang
{ok, matches} = tcps_deterministic:verify_build_env(ExpectedEnv).
```

### 3. Dependency Pinning

Ensures all dependencies are pinned to exact versions:

```erlang
ok = tcps_deterministic:pin_dependencies().
{ok, pinned} = tcps_deterministic:verify_pinned_deps().
```

**Requirements:**
- `rebar.lock` must exist
- No version ranges (must be exact versions)
- Git dependencies must use commit SHAs (not branches)

### 4. Non-Determinism Detection

Scans source code for common non-determinism sources:

```erlang
Sources = tcps_deterministic:detect_non_determinism_sources("src").
Fixes = tcps_deterministic:fix_non_determinism(Sources).
```

**Detected Patterns:**

| Pattern | Type | Severity | Suggested Fix |
|---------|------|----------|---------------|
| `erlang:timestamp()` | timestamp | high | Use injected timestamp from config |
| `calendar:*` | timestamp | medium | Use fixed timestamps for builds |
| `rand:uniform()` | random | high | Use seeded random with fixed seed |
| `crypto:strong_rand_bytes()` | random | medium | Use seeded random |
| `self()` | process_id | low | Mock process IDs in tests |
| `spawn*` | process_id | low | Mock spawned processes |
| `filelib:wildcard()` | file_ordering | medium | Sort results for determinism |

**Example Fix:**
```erlang
% Before (non-deterministic)
get_time() ->
    erlang:timestamp().

% After (deterministic)
get_time() ->
    application:get_env(myapp, build_timestamp, erlang:system_time()).
```

### 5. Build Recipe Generation

Generates reproducible build instructions:

```erlang
Recipe = tcps_deterministic:generate_build_recipe(<<"SKU-001">>).
ok = file:write_file("build_recipe.json", jsx:encode(Recipe)).
```

**Recipe Contents:**
- Complete build environment
- Exact build steps
- Expected artifact hash
- Timestamp and version

**Execution:**
```erlang
{ok, Hash} = tcps_deterministic:execute_build_recipe("build_recipe.json").
```

**Example Recipe:**
```bash
# Build Recipe for SKU-001
# Generated: 2026-01-26T10:00:00Z

export OTP_VERSION=27.3.4.2
export REBAR3_VERSION=3.24.0

git clone https://github.com/org/repo.git
git checkout abc123def456...

rebar3 clean
rebar3 get-deps
rebar3 compile
rebar3 as prod tar

# Expected artifact hash:
# SHA-256: a1b2c3d4e5f6...
```

### 6. Docker Build Integration

Generates Dockerfiles for hermetic builds:

```erlang
Dockerfile = tcps_deterministic:generate_dockerfile(<<"SKU-001">>).
{ok, ImageHash} = tcps_deterministic:build_docker_image(<<"SKU-001">>).
{ok, deterministic} = tcps_deterministic:verify_docker_determinism(<<"SKU-001">>).
```

**Dockerfile Features:**
- Fixed base image with exact version tag
- Pinned system packages
- Multi-stage build (builder + runtime)
- Deterministic file copy order
- Minimal runtime image

**Example:**
```dockerfile
FROM erlang:27.3.4.2-alpine AS builder

RUN apk add --no-cache \
    git=2.43.0-r0 \
    curl=8.5.0-r0

WORKDIR /build
COPY rebar.config rebar.lock ./
COPY src ./src

RUN rebar3 compile && rebar3 as prod tar

FROM erlang:27.3.4.2-alpine
COPY --from=builder /build/_build/prod/rel /opt/release
CMD ["/opt/release/bin/app", "foreground"]
```

### 7. Build Cache Management

Caches build artifacts for faster rebuilds:

```erlang
ok = tcps_deterministic:cache_build_artifacts(<<"SKU-001">>).
{ok, restored} = tcps_deterministic:restore_from_cache(Hash).
Stats = tcps_deterministic:get_cache_stats().
```

**Cache Benefits:**
- Significant speed improvement for repeated builds
- Verified by hash (security)
- Automatic invalidation when source changes

### 8. SBOM Generation

Generates Software Bill of Materials for compliance:

```erlang
SBOM = tcps_deterministic:generate_sbom(<<"SKU-001">>).
{ok, compliant} = tcps_deterministic:verify_licenses().
{ok, no_vulnerabilities} = tcps_deterministic:check_vulnerabilities(<<"SKU-001">>).
```

**SBOM Contents:**
- All dependencies with exact versions
- License information
- Checksums
- Known vulnerabilities

**Formats Supported:**
- CycloneDX 1.4
- SPDX (planned)

### 9. Quality Gates

Enforces determinism before release:

```erlang
{ok, pass} = tcps_deterministic:check_determinism_gate(<<"SKU-001">>).
```

**Quality Gates:**
1. **Build Determinism**: Verified via dual-build comparison
2. **Dependencies Pinned**: All deps have exact versions
3. **No Non-Determinism Sources**: Code scan passes
4. **SBOM Generated**: Complete SBOM exists
5. **Licenses Compliant**: All licenses approved

**Gate Failure:**
- Triggers Andon event
- Blocks release pipeline
- Requires root cause analysis

## Usage Examples

### Quick Start

```erlang
% Initialize system
tcps_deterministic:start().

% Verify build is deterministic
{ok, deterministic, Hash} = tcps_deterministic:verify_deterministic_build(<<"MY-SKU">>).

% Generate SBOM
SBOM = tcps_deterministic:generate_sbom(<<"MY-SKU">>).

% Run quality gates
{ok, pass} = tcps_deterministic:check_determinism_gate(<<"MY-SKU">>).
```

### CI/CD Integration

```bash
#!/bin/bash
# Deterministic build verification in CI/CD

set -e

echo "Running deterministic build verification..."

# Run Erlang verification
erl -noshell -eval '
    tcps_deterministic:start(),
    case tcps_deterministic:check_determinism_gate(<<"CI-BUILD">>) of
        {ok, pass} ->
            io:format("Quality gates passed~n"),
            halt(0);
        {error, {fail, Reason}} ->
            io:format("Quality gates failed: ~p~n", [Reason]),
            halt(1)
    end
'

echo "Deterministic build verification complete!"
```

### Full Workflow

```erlang
% Complete deterministic build workflow

% 1. Detect non-determinism
Sources = tcps_deterministic:detect_non_determinism_sources("src"),
case Sources of
    [] -> ok;
    [_|_] ->
        Fixes = tcps_deterministic:fix_non_determinism(Sources),
        io:format("Apply these fixes:~n~p~n", [Fixes])
end,

% 2. Pin dependencies
ok = tcps_deterministic:pin_dependencies(),
{ok, pinned} = tcps_deterministic:verify_pinned_deps(),

% 3. Verify build
{ok, deterministic, Hash} = tcps_deterministic:verify_deterministic_build(<<"SKU">>),

% 4. Generate artifacts
Recipe = tcps_deterministic:generate_build_recipe(<<"SKU">>),
SBOM = tcps_deterministic:generate_sbom(<<"SKU">>),
Dockerfile = tcps_deterministic:generate_dockerfile(<<"SKU">>),

% 5. Cache results
ok = tcps_deterministic:cache_build_artifacts(<<"SKU">>),

% 6. Run quality gates
{ok, pass} = tcps_deterministic:check_determinism_gate(<<"SKU">>).
```

## Integration with TCPS Andon

When non-determinism is detected, an Andon event is automatically triggered:

```erlang
% Andon event structure
#{
    event_id => <<"ANDON-123...">>,
    failure_type => non_determinism,
    sku_id => <<"SKU-001">>,
    stage => compilation,
    details => #{
        first_hash => Hash1,
        second_hash => Hash2,
        differing_files => [...],
        details => <<"Build produced different artifacts...">>
    },
    status => open
}
```

**Resolution Required:**
1. Identify root cause of non-determinism
2. Apply fix (remove timestamps, seed random, sort files, etc.)
3. Verify fix with new build
4. Document prevention measure
5. Resolve Andon event

## Best Practices

### 1. Run Early and Often
- Verify determinism on every commit
- Include in CI/CD pipeline
- Use as pre-release quality gate

### 2. Fix Issues Immediately
- Don't defer non-determinism fixes
- Small issues compound over time
- Follow Andon stop-the-line principle

### 3. Document Build Environment
- Capture and version control build recipes
- Update when dependencies change
- Include in release documentation

### 4. Monitor Trends
- Track determinism over time
- Alert on new non-determinism sources
- Review SBOM for license/vulnerability changes

### 5. Cache Aggressively
- Enable build caching for speed
- Verify cache integrity via hashes
- Clean cache periodically

## Troubleshooting

### Build is Non-Deterministic

**Symptoms:**
- Hash mismatch between builds
- Andon event triggered

**Common Causes:**
1. Timestamps in code
2. Random values
3. Process IDs
4. File system ordering
5. Concurrent builds

**Resolution:**
```erlang
% 1. Detect sources
Sources = tcps_deterministic:detect_non_determinism_sources("src"),

% 2. Review suggested fixes
Fixes = tcps_deterministic:fix_non_determinism(Sources),

% 3. Apply fixes manually
% 4. Verify fix
{ok, deterministic, _} = tcps_deterministic:verify_deterministic_build(<<"SKU">>).
```

### Dependencies Not Pinned

**Symptoms:**
- `{error, {unpinned, [...]}}` from `verify_pinned_deps()`

**Resolution:**
```bash
# Generate lock file
rebar3 lock

# Commit lock file
git add rebar.lock
git commit -m "Pin dependencies"
```

### Cache Misses

**Symptoms:**
- Slow builds despite caching

**Resolution:**
```erlang
% Check cache stats
Stats = tcps_deterministic:get_cache_stats(),
io:format("Cache entries: ~p~n", [maps:get(entries, Stats)]),

% Manually cache current build
ok = tcps_deterministic:cache_build_artifacts(<<"SKU">>).
```

## Performance

### Benchmarks

| Operation | Time | Notes |
|-----------|------|-------|
| Hash calculation | < 100ms | For typical project |
| Directory scan | < 500ms | For 100+ files |
| Dual build verification | ~2x build time | Runs builds sequentially |
| Docker build | ~5-10 min | First build (no cache) |
| Docker build (cached) | ~30 sec | With layer caching |

### Optimization Tips

1. **Use Build Cache**: 2-5x speedup for repeated builds
2. **Enable Docker Layer Cache**: Significant speedup
3. **Parallel Builds**: Use `rebar3` parallelization
4. **Incremental Builds**: Only rebuild changed modules

## Security Considerations

### Cryptographic Verification
- SHA-256 hashes for all artifacts
- No collision attacks known
- Suitable for security-critical applications

### Supply Chain Security
- SBOM provides complete dependency visibility
- License verification prevents legal issues
- Vulnerability scanning catches known issues

### Build Reproducibility
- Independent parties can verify builds
- Detects tampering attempts
- Supports security audits

## Roadmap

### Planned Features
- [ ] SPDX SBOM format support
- [ ] Integration with vulnerability databases (CVE, OSV)
- [ ] Automated fix application
- [ ] Cross-platform build verification
- [ ] Build artifact signing
- [ ] Remote build verification service

## References

- [Reproducible Builds Project](https://reproducible-builds.org/)
- [CycloneDX SBOM Standard](https://cyclonedx.org/)
- [SPDX](https://spdx.dev/)
- [NIST Supply Chain Security](https://www.nist.gov/itl/executive-order-improving-nations-cybersecurity/software-supply-chain-security-guidance)

## Support

For issues, questions, or contributions:
- GitHub Issues: [erlmcp/issues](https://github.com/user/erlmcp/issues)
- Documentation: This file
- Examples: `examples/tcps_deterministic_demo.erl`
