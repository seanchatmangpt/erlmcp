# erlmcp v{VERSION}

Released: {DATE}
Previous: {PREVIOUS_VERSION}

## Summary

{SUMMARY}

## What's New

### Highlights

{HIGHLIGHTS}

## Installation

### Hex.pm

Add to your `rebar.config`:

```erlang
{deps, [
  {erlmcp, "{VERSION}"}
]}.
```

### Docker

```bash
# Pull the image
docker pull ghcr.io/banyan-platform/erlmcp:{VERSION}

# Run the server
docker run -d \
  --name erlmcp \
  -p 8080:8080 \
  -v erlmcp-data:/data \
  ghcr.io/banyan-platform/erlmcp:{VERSION}
```

### Docker Compose

```yaml
services:
  erlmcp:
    image: ghcr.io/banyan-platform/erlmcp:{VERSION}
    ports:
      - "8080:8080"
    environment:
      - ERLMCP_PROFILE=prod
    volumes:
      - erlmcp-data:/data
```

### From Source

```bash
# Clone repository
git clone https://github.com/banyan-platform/erlmcp
cd erlmcp

# Checkout version
git checkout v{VERSION}

# Build
rebar3 compile
rebar3 release

# Run
_build/prod/rel/erlmcp/bin/erlmcp start
```

## Upgrade Guide

### From v{PREVIOUS_VERSION}

#### Pre-upgrade Checklist

- [ ] Backup Mnesia data: `erlmcp backup`
- [ ] Note current configuration: `erlmcp config show`
- [ ] Check OTP version: `erl -version` (requires 28+)

#### Upgrade Steps

1. **Stop the current version**:
   ```bash
   _build/prod/rel/erlmcp/bin/erlmcp stop
   ```

2. **Update dependencies**:
   ```bash
   rebar3 upgrade
   rebar3 compile
   ```

3. **Build new release**:
   ```bash
   rebar3 as prod release
   ```

4. **Run migrations** (if applicable):
   ```bash
   _build/prod/rel/erlmcp/bin/erlmcp migrate
   ```

5. **Start the new version**:
   ```bash
   _build/prod/rel/erlmcp/bin/erlmcp start
   ```

6. **Verify health**:
   ```bash
   curl http://localhost:8080/health
   ```

### Breaking Changes

{BREAKING_CHANGES}

If no breaking changes are listed, this release is backward compatible with v{PREVIOUS_VERSION}.

## Changelog

### Added

{ADDED}

### Changed

{CHANGED}

### Fixed

{FIXED}

### Security

{SECURITY}

## Artifacts

| Artifact | Link | Checksum |
|----------|------|----------|
| **Tarball** | [erlmcp-{VERSION}.tar.gz]({TARBALL_URL}) | `{TARBALL_SHA256}` |
| **Docker Image** | `ghcr.io/banyan-platform/erlmcp:{VERSION}` | `{DOCKER_DIGEST}` |
| **Hex.pm** | [erlmcp](https://hex.pm/packages/erlmcp/{VERSION}) | - |
| **SBOM** | [erlmcp-{VERSION}.sbom.json]({SBOM_URL}) | - |
| **Provenance** | [erlmcp-{VERSION}.provenance.json]({PROVENANCE_URL}) | - |

### Verification

Verify Docker image signature:
```bash
cosign verify \
  --certificate-identity="https://github.com/banyan-platform/erlmcp/.github/workflows/release.yml@refs/heads/main" \
  --certificate-oidc-issuer="https://token.actions.githubusercontent.com" \
  ghcr.io/banyan-platform/erlmcp:{VERSION}
```

Verify tarball signature:
```bash
cosign verify-blob \
  -certificate erlmcp-{VERSION}.tar.gz.cert \
  -signature erlmcp-{VERSION}.tar.gz.sig \
  erlmcp-{VERSION}.tar.gz
```

Verify checksum:
```bash
sha256sum -c erlmcp-{VERSION}.tar.gz.sha256
```

## Performance

{PERFORMANCE_TABLE}

## Security

{SECURITY_SECTION}

### Vulnerability Scanning

All artifacts are scanned for vulnerabilities before release. View the VEX (Vulnerability Exploitability Exchange) report:
- [erlmcp-{VERSION}.vex.json]({VEX_URL})

### Dependency Updates

This release includes the following dependency updates for security:

{DEPENDENCY_UPDATES}

## Known Issues

{KNOWN_ISSUES}

## Compatibility

### Erlang/OTP

**Required: Erlang/OTP 28+**

This version requires OTP 28 for:
- Priority messages in process mailbox
- Native JSON module (removed jsx dependency)
- Process iteration improvements
- hibernate/0 support

### Operating Systems

Tested on:
- Linux: Ubuntu 22.04, Ubuntu 24.04, Debian 12
- macOS: 13 (Ventura), 14 (Sonoma)
- Docker: All platforms using official Erlang OTP images

### Transport Support

| Transport | v{PREVIOUS_VERSION} | v{VERSION} |
|-----------|---------------------|------------|
| stdio | ✓ | ✓ |
| TCP | ✓ | ✓ |
| HTTP | ✓ | ✓ |
| WebSocket | ✓ | ✓ |
| SSE | ✗ | ✓ |

## Contributors

{CONTRIBUTORS}

Thank you to everyone who contributed to this release!

## Support

- **Documentation**: https://erlmcp.io/docs
- **API Reference**: https://erlmcp.io/api
- **Issues**: https://github.com/banyan-platform/erlmcp/issues
- **Discussions**: https://github.com/banyan-platform/erlmcp/discussions
- **Discord**: https://discord.gg/erlmcp
- **Email**: support@erlmcp.io

## Migration from v2.x

If you're upgrading from v2.x to v3.x, please review the detailed migration guide:

[Migration Guide: v2.x to v3.0](https://erlmcp.io/docs/migration/v2-to-v3)

### Key Changes in v3.0

- **Minimum OTP version**: 26 → 28
- **JSON encoding**: jsx → native `json` module
- **Registry**: O(N) → O(log N) lookup with gproc
- **Configuration**: New profile-based configuration
- **Transports**: New SSE transport support
- **Security**: Zero Trust security module

---

**Full Changelog**: https://github.com/banyan-platform/erlmcp/compare/v{PREVIOUS_VERSION}...v{VERSION}
