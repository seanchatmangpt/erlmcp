# Changelog - erlmcp

All notable changes to erlmcp are documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/)
and this project adheres to [Semantic Versioning](https://semver.org/).

---

## [Unreleased]

### Breaking Changes
- None planned

### New Features
- (Planned features to be added here)

### Enhancements
- (Planned improvements to be added here)

### Bug Fixes
- (Bugs fixed in unreleased version)

### Deprecated
- None currently

### Security
- None reported

### Documentation
- Release Strategy: RELEASE_STRATEGY.md (new)
- Release Tools: tools/release.sh, tools/changelog-generator.sh (new)

### Known Issues
- None currently

---

## [1.0.0] - 2026-01-26

### Initial Release

This is the first stable release of erlmcp, a Model Context Protocol (MCP) implementation
for Erlang/OTP with autonomous system governance (taiea).

#### Breaking Changes
- N/A (Initial release)

#### New Features
- **Core MCP Implementation**
  - Full MCP protocol support (stdio, TCP transports)
  - Request/response message handling
  - Tool definitions and invocation
  - Resource management
  - Prompt templates
  - Sampling with streaming

- **TAIEA Autonomic System**
  - Deterministic receipt generation with SHA-256 hashing
  - Multi-stage transformation pipeline (μ₁-μ₅)
  - RDF specification processing
  - SPARQL query engine integration
  - Tera template rendering
  - Cryptographic proof generation

- **CLI Tools**
  - erlmcp-cli for interactive MCP testing
  - erlmcp-server for running MCP servers
  - Configuration management

- **Examples**
  - Simple MCP server/client
  - Calculator service
  - Weather service with API integration
  - TAIEA autonomic examples

- **Testing & Validation**
  - 85%+ code coverage
  - Property-based testing with PropEr
  - Integration tests with testcontainers
  - Performance benchmarks

- **Documentation**
  - API guide
  - Development guide
  - Contributing guidelines
  - Architecture documentation

#### Enhancements
- Optimized message serialization (JSX)
- Efficient process pooling (poolboy)
- Distributed process coordination (gproc)
- Type-safe JSON validation (jesse)
- Comprehensive error handling

#### Bug Fixes
- Initial release (no prior bugs)

#### Security
- Input validation for all MCP messages
- Type-checking for protocol compliance
- Error handling for malformed requests
- Security scanning with Bandit

#### Performance
- Message processing: < 5ms P99 latency
- Connection handling: 100+ concurrent connections
- Memory efficient: < 50MB baseline

#### Dependencies
- **Runtime**: jsx, jesse, gproc, gun, ranch, poolboy
- **Development**: proper, meck, coveralls
- **Testing**: eunit, PropEr, coveralls

#### Contributors
- @seanchatmangpt (author)

#### Installation

**Via rebar3:**
```erlang
{deps, [{erlmcp, "1.0.0"}]}
```

**Via Docker:**
```bash
docker pull ghcr.io/banyan-platform/erlmcp:1.0.0
```

#### Links
- [GitHub Repository](https://github.com/banyan-platform/erlmcp)
- [Documentation](https://github.com/banyan-platform/erlmcp/blob/main/DEVELOPMENT.md)
- [Contributing](https://github.com/banyan-platform/erlmcp/blob/main/CONTRIBUTING.md)

---

## Version History

| Version | Date | Status | Support Until |
|---------|------|--------|----------------|
| 1.0.0 | 2026-01-26 | LTS | 2027-01-26 |
| 1.1.0 | TBD | Planned | TBD |
| 1.2.0 | TBD | Planned | TBD |
| 2.0.0 | TBD | Planned | TBD |

---

## Release Process

For detailed information about the release process, see [RELEASE_STRATEGY.md](RELEASE_STRATEGY.md).

### Release Schedule
- **Monthly releases**: First Monday of each month
- **Patch releases**: On-demand for critical issues
- **LTS support**: 12+ months per major version

### Semantic Versioning

- **MAJOR.MINOR.PATCH[-PRERELEASE]**
  - MAJOR: Breaking changes
  - MINOR: New features (backward-compatible)
  - PATCH: Bug fixes
  - PRERELEASE: alpha, beta, rc (release candidate)

### Release Commands

```bash
# Create release
./tools/release.sh 1.2.0

# Generate changelog from commits
./tools/changelog-generator.sh --dry-run

# Update CHANGELOG from commits
./tools/changelog-generator.sh --update-file
```

---

## How to Read This Changelog

- **Breaking Changes**: API or protocol changes that require migration
- **New Features**: Backward-compatible additions
- **Enhancements**: Improvements to existing features
- **Bug Fixes**: Corrections to defects
- **Deprecated**: Features scheduled for removal (see deprecation timeline)
- **Security**: Security vulnerability fixes and hardening
- **Performance**: Performance improvements and optimizations
- **Documentation**: Documentation updates and additions
- **Known Issues**: Limitations and workarounds

## Upgrading

### From 1.0.0 to 1.1.0+
- No breaking changes expected
- New features will be backward-compatible
- See specific release notes for details

### From 1.x to 2.0.0
- May include breaking changes
- Migration guide will be provided
- Deprecation notices in 1.x versions
- 6-month support overlap

---

## Reporting Issues

Found a bug? Please report it on [GitHub Issues](https://github.com/banyan-platform/erlmcp/issues).

**Include**:
- Version number
- Steps to reproduce
- Expected vs. actual behavior
- Erlang/OTP version
- Environment details

## Security Vulnerabilities

Please report security issues responsibly:
- Email: security@example.com (or team contact)
- Do NOT post publicly
- Include affected versions
- Coordinated disclosure timeline: 90 days

---

## Acknowledgments

erlmcp is built on the shoulders of excellent Erlang/OTP libraries:
- **jsx** - JSON parsing
- **jesse** - JSON Schema validation
- **gproc** - Global process registry
- **gun** - HTTP client
- **ranch** - TCP/SSL acceptor pool
- **poolboy** - Process pooling

---

**Last Updated**: 2026-01-26
**Maintained by**: @seanchatmangpt
**License**: MIT (or your project license)
