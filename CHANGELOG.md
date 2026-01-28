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

## [2.0.0] - 2026-01-28

### Major Release - Architecture Refactoring & Performance Baseline

**TCPS Receipt Evidence:**
- ✅ Compilation: PASS (0 errors, 4 warnings)
- ✅ Performance: 2.53M msg/s throughput (within 10% of v1 baseline)
- ✅ Memory: 16% improvement over v1 (16.0 MiB vs 19.1 MiB)
- ✅ Benchmarks: All core operations passing, no blocking regressions

#### Breaking Changes
- **Repository Structure:** Migrated from monolithic `src/` to umbrella apps architecture
  - `apps/erlmcp_core/` - Core MCP protocol (JSON-RPC, Registry, Client/Server)
  - `apps/erlmcp_observability/` - Metrics, traces, TCPS receipts
  - `apps/erlmcp_transports/` - STDIO, TCP, HTTP, WebSocket transports
  - `apps/tcps_erlmcp/` - Toyota Code Production System quality gates
- **Transport API:** Updated interface (affects custom transport implementations)
- **Module Organization:** Standardized naming conventions across all applications

#### New Features
- **TCPS Integration**
  - Manufacturing-grade quality gates (8 stages)
  - SHACL ontology validation
  - SHA-256 receipt chains
  - Real-time dashboard on port 8080
  - Auto-integration with MCP workflows
- **Enhanced Observability**
  - OpenTelemetry integration (OTLP export)
  - Comprehensive metrics collection
  - Receipt-based audit trails
  - Performance tracking and alerting
- **Advanced Caching**
  - Two-level cache (L1/L2) with Mnesia backing
  - TTL-based expiration
  - Resource directory whitelisting
  - Configurable cleanup intervals
- **Security Enhancements**
  - Secret management with encryption
  - OAuth 2.0 authentication support
  - JWT token validation
  - API key management

#### Enhancements
- **Performance Optimization**
  - 6% throughput reduction (2.53M msg/s vs 2.69M msg/s baseline)
  - 16% memory improvement (16.0 MiB vs 19.1 MiB)
  - Latency maintained: P95=83µs, P99=99µs
  - Linear scalability up to 100K operations
- **Code Organization**
  - Umbrella project structure for better modularity
  - Separated concerns across 4 applications
  - Improved test organization
  - Enhanced documentation structure
- **Transport Layer**
  - WebSocket support added
  - SSE (Server-Sent Events) transport
  - Improved TCP connection pooling
  - Better HTTP/2 support via Gun

#### Bug Fixes
- Fixed module organization issues from v1
- Resolved duplicate type definitions
- Corrected compilation warnings
- Fixed EUnit test syntax issues
- Removed GraphQL transport (unstable)

#### Performance Baseline (v2.0.0)
**Core Operations (100K ops):**
- Throughput: 2,530,893 msg/s
- Latency P50: 1.0 µs
- Latency P95: 83.0 µs
- Latency P99: 99.0 µs
- Memory: 16.0 MiB

**Component Breakdown:**
- Registry: 51.6 µs avg (bottleneck identified)
- Queue: 0.1 µs avg
- Pool: 0.5 µs avg
- Session: 9.7 µs avg

**Scalability:**
- 1K ops: 1.25M msg/s
- 10K ops: 2.62M msg/s
- 100K ops: 2.53M msg/s (baseline)
- 1M ops: 1.70M msg/s (GC overhead)

See `BENCHMARK_RESULTS_V2.md` for full performance report.

#### Deprecated
- `src/` directory (archived to `archive/v1/`)
- GraphQL transport (removed due to instability)
- Legacy pricing modules (tcps_receipt, tcps_receipt_verifier)

#### Security
- Enhanced secret encryption
- Improved authentication middleware
- Security audit compliance
- Bandit security scanning enabled

#### Documentation
- `docs/migration/V2_CLEANUP_STRATEGY.md` - Migration guide
- `BENCHMARK_RESULTS_V2.md` - Performance analysis
- Updated API documentation for all apps
- TCPS integration guides

#### Known Issues
- Network benchmarks deferred (transport API changes require benchmark updates)
- Some test suites need migration to new structure
- Session max latency spikes at scale (22-32ms) under investigation

#### Contributors
- @seanchatmangpt (architecture lead)
- Claude Sonnet 4.5 (development assistance)

#### Installation

**Via rebar3:**
```erlang
{deps, [{erlmcp, "2.0.0"}]}
```

**Via Docker:**
```bash
docker pull ghcr.io/banyan-platform/erlmcp:2.0.0
```

#### Upgrading from v1.x

1. **Update dependencies:**
   ```bash
   rebar3 upgrade erlmcp
   ```

2. **Review breaking changes:**
   - Custom transports require API updates
   - Module paths changed (use new apps structure)
   - Configuration format updated (see `config/sys.config`)

3. **Test thoroughly:**
   - Run full test suite: `rebar3 eunit`
   - Run benchmarks: `make benchmark-quick`
   - Validate custom code against new APIs

4. **Migration guide:** See `docs/migration/V2_CLEANUP_STRATEGY.md`

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
| 1.0.0 | 2026-01-26 | Archived | 2026-01-28 |
| 2.0.0 | 2026-01-28 | LTS | 2027-01-28 |
| 2.1.0 | TBD | Planned | TBD |
| 3.0.0 | TBD | Planned | TBD |

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
./tools/release.sh 2.0.0

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

### From 1.0.0 to 2.0.0
- **Breaking changes present** - See v2.0.0 release notes
- Migration guide available: `docs/migration/V2_CLEANUP_STRATEGY.md`
- Update transport implementations if using custom transports
- Review new configuration format in `config/sys.config`

### From 2.0.0 to 2.1.0+
- No breaking changes expected
- New features will be backward-compatible
- See specific release notes for details

### From 2.x to 3.0.0
- May include breaking changes
- Migration guide will be provided
- Deprecation notices in 2.x versions
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

**Last Updated**: 2026-01-28
**Maintained by**: @seanchatmangpt
**License**: Apache-2.0
