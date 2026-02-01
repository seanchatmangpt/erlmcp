# erlmcp v3.0 OSS Release Checklist Plan

**Status**: Draft
**Author**: Release Specialist
**Created**: 2026-01-31
**Target Release**: v3.0.0 (OSS)
**Applies To**: Open Source Software releases only

---

## Executive Summary

This document defines the comprehensive release checklist for erlmcp v3.0 Open Source Software (OSS) releases. It establishes separate criteria for OSS vs commercial releases, with pre-release validation gates, artifact generation requirements, documentation completeness, and post-release monitoring procedures.

**Key Principles:**
1. **Zero-Defect Quality**: Manufacturing-grade 99.99966% defect-free delivery
2. **OSS Transparency**: All validation results publicly verifiable
3. **Separation of Concerns**: OSS and commercial releases have distinct criteria
4. **Automated Gates**: Machine-enforced quality checkpoints

---

## Part I: OSS vs Commercial Release Criteria

### OSS Release Requirements

OSS releases are publicly available without support entitlements. Requirements:

| Category | OSS Requirement |
|----------|-----------------|
| **License** | Apache 2.0 (verified in LICENSE file) |
| **Code Quality** | All quality gates passing (no warnings) |
| **Test Coverage** | >= 80% (publicly measured) |
| **Documentation** | User guides, API reference, installation guides complete |
| **Security** | No known critical vulnerabilities (CVSS >= 7.0) |
| **Performance** | No regressions vs baseline (>10%) |
| **Support** | Community-only (no SLA) |
| **Feature Completeness** | All advertised features working |
| **Breaking Changes** | Documented with migration guide |

### Commercial Release Requirements (Additional)

Commercial releases include enterprise support and have additional requirements:

| Category | Commercial Requirement (In Addition to OSS) |
|----------|-----------------------------------------------|
| **SLA** | Uptime commitment (e.g., 99.9%) |
| **Support** | Dedicated support channels, response times |
| **Compliance** | SOC2, HIPAA, GDPR readiness documentation |
| **Security** | Penetration testing report, vulnerability bounty results |
| **Performance** | SLO/SLI definitions with telemetry integration |
| **Certifications** | FIPS 140-2, Common Criteria (if applicable) |
| **Enterprise Features** | SSO, RBAC, audit logging complete |
| **Training** | Admin and user training materials available |

### Release Type Decision Matrix

```
                  |  OSS  | Commercial
------------------+-------+------------
Public GitHub     |  YES  |   NO
Community Support |  YES  |   NO
Paid Support      |  NO   |  YES
SLA Included      |  NO   |  YES
Enterprise Auth   |  NO   |  YES
Compliance Certs  |  NO   |  YES
```

---

## Part II: Pre-Release Quality Gates

### Gate 1: Version Validation

**Automated Check**: `tools/release/release-blocker-check.sh`

| Check | Method | Blocking |
|-------|--------|----------|
| Version format (semver) | Regex: `^[0-9]+\.[0-9]+\.[0-9]+(-[a-z]+\.\d+)?$` | Yes |
| Version in all .app.src files | `grep {vsn, "VERSION"}` | Yes |
| Version in rebar.config relx | `grep {release, {erlmcp, "VERSION"}` | Yes |
| Git tag doesn't exist | `! git rev-parse vVERSION` | Yes |
| CHANGELOG.md updated | `grep "## \[VERSION\]" CHANGELOG.md` | Yes |

**Exit Code**: 1 if any check fails

### Gate 2: Build Quality

**Automated Check**: `TERM=dumb rebar3 compile`

| Check | Threshold | Blocking |
|-------|-----------|----------|
| Compilation errors | 0 | Yes |
| Compiler warnings | 0 (OSS), Advisory (Commercial) | OSS: Yes |
| Dialyzer warnings | 0 type errors | Yes |
| Xref undefined functions | 0 | Yes |
| Include ERTS for release | `include_erts: true` | Yes |

### Gate 3: Test Suite

**Automated Check**: `rebar3 eunit`, `rebar3 ct`, `rebar3 proper`

| Test Type | Pass Rate | Coverage | Blocking |
|-----------|-----------|----------|----------|
| EUnit (unit) | 100% | Included | Yes |
| Common Test (integration) | 100% | Included | Yes |
| PropEr (property) | 1000 tests | N/A | Yes |
| Chicago TDD compliance | No mocks | N/A | Yes |
| Transport tests | All 5 transports | N/A | Yes |

**Minimum Coverage Requirements:**

| Module Category | Minimum Coverage |
|----------------|------------------|
| Core (protocol, registry, client, server) | 85% |
| Transports | 80% |
| Observability | 75% |
| Validation | 85% |
| Overall | 80% |

### Gate 4: Documentation Completeness

**Automated Check**: `tools/docs/check-completeness.sh` (to be created)

| Document | Required For | Status Check |
|----------|--------------|--------------|
| LICENSE (Apache 2.0) | OSS | File exists, valid format |
| README.md | OSS | Quick start, prerequisites, links |
| CHANGELOG.md | OSS | Current version section |
| INSTALLATION.md | OSS | All installation methods |
| MIGRATION-v2-to-v3.md | OSS | Breaking changes documented |
| API Reference | OSS | All public APIs documented |
| Architecture Overview | OSS | System design documented |
| Security Guide | OSS | Hardening, auth, secrets |
| Troubleshooting Guide | OSS | Common issues, error codes |
| Contributing Guide | OSS | Development workflow |
| Code of Conduct | OSS | Community guidelines |

**Documentation Quality Checks:**
- All code examples tested and runnable
- All links verified (no 404s)
- Diagrams use Mermaid syntax (GitHub compatible)
- Version specified in each document
- Last updated date present

### Gate 5: Security Validation

**Automated Check**: `tools/security/scan.sh`

| Check | Method | Threshold | Blocking |
|-------|--------|-----------|----------|
| Hardcoded secrets | `grep -Ei "password|secret|api_key"` | 0 in src/ | Yes |
| eval() usage | `grep "eval("` | 0 in src/ | Yes |
| Unsafe functions | `grep -E "os:cmd|open_port"` | Reviewed | Advisory |
| Dependency vulnerabilities | `rebar3 tree` + advisory DB | CVE >= 7.0 | Yes |
| TLS configuration | Verify TLS options present | Min TLS 1.2 | Yes |
| Authentication | JWT/Token validation exists | Yes | Yes |

**Security Evidence Bundle:**
- Security scan results (JSON)
- Dependency vulnerability report
- Penetration test summary (for commercial)
- CVE acknowledgment (if any)

### Gate 6: Performance Baseline

**Automated Check**: `make benchmark-quick`

| Benchmark | Baseline | Max Regression | Blocking |
|-----------|----------|----------------|----------|
| Core ops (100k) | 2.69M ops/sec | 10% | Yes |
| Registry throughput | 553K msg/s | 10% | Yes |
| Queue throughput | 971K msg/s | 10% | Yes |
| Network I/O | 43K msg/s | 15% | Advisory |
| Memory per connection | < 1MB | 20% | Advisory |
| Startup time | < 5s | 20% | Advisory |

**Performance Evidence Bundle:**
- Benchmark results (JSON)
- Comparison to baseline
- Performance regression report
- Resource utilization profile

### Gate 7: MCP Protocol Compliance

**Automated Check**: `./_build/default/bin/erlmcp_validate run --all`

| Category | Requirement | Compliance | Blocking |
|----------|-------------|------------|----------|
| JSON-RPC 2.0 | 100% spec compliance | 95%+ | Yes |
| MCP 2025-11-25 | All required methods | 95%+ | Yes |
| Error codes | Full range covered | -32700 to -32010, 1001-1089 | Yes |
| Transports | All 5 working | stdio, tcp, http, ws, sse | Yes |
| Capabilities | All advertised | tools, resources, prompts | Yes |

**Compliance Evidence Bundle:**
- Protocol validation report (JSON/HTML)
- Transport test results
- Error code coverage matrix
- Spec version declaration

### Gate 8: Git Status

**Automated Check**: `git status --porcelain`

| Check | Method | Blocking |
|-------|--------|----------|
| No uncommitted changes | `git diff --exit-code` | Yes |
| On main/master branch | `git rev-parse --abbrev-ref HEAD` | Yes |
| Synced with remote | `local == remote` | Yes |
| No .broken files | `find . -name "*.broken"` | Yes |
| No TODO/FIXME in src | `grep -r "TODO\|FIXME" src/` | Yes |

---

## Part III: Release Artifact Generation

### Artifact 1: Source Tarball

**Purpose**: OSS distribution from GitHub releases

**Generation**:
```bash
git archive --format=tar.gz --prefix=erlmcp-${VERSION}/ v${VERSION} > erlmcp-${VERSION}.tar.gz
```

**Contents**:
- Complete source code (excluding _build/)
- LICENSE (Apache 2.0)
- README.md
- CHANGELOG.md
- rebar.config, rebar.lock
- All documentation (docs/)
- All examples (examples/)

**Validation**:
```bash
# Extract and verify
tar -xzf erlmcp-${VERSION}.tar.gz
cd erlmcp-${VERSION}
rebar3 compile  # Must succeed
rebar3 eunit    # Must pass
```

**Checksums**:
- SHA-256: `sha256sum erlmcp-${VERSION}.tar.gz > erlmcp-${VERSION}.tar.gz.sha256`
- MD5: `md5sum erlmcp-${VERSION}.tar.gz > erlmcp-${VERSION}.tar.gz.md5`

### Artifact 2: Production Release

**Purpose**: Ready-to-run binary distribution

**Generation**:
```bash
rebar3 as prod compile
rebar3 as prod release
```

**Contents**:
- `_build/prod/rel/erlmcp/` directory
- ERTS included (portable)
- All applications (core, transports, observability, validation)
- Startup scripts (`bin/erlmcp`)
- Configuration templates (`etc/`)

**Validation**:
```bash
# Start and verify
_build/prod/rel/erlmcp/bin/erlmcp start
_build/prod/rel/erlmcp/bin/erlmcp ping
_build/prod/rel/erlmcp/bin/erlmcp stop
```

### Artifact 3: Docker Image

**Purpose**: Container deployment

**Generation**:
```bash
docker build -t erlmcp:${VERSION} .
docker tag erlmcp:${VERSION} erlmcp:latest
```

**Tags**:
- `erlmcp:3.0.0` - Specific version
- `erlmcp:3.0` - Minor version
- `erlmcp:3` - Major version
- `erlmcp:latest` - Latest stable

**Validation**:
```bash
docker run --rm erlmcp:${VERSION} version
docker run --rm erlmcp:${VERSION} health
docker run --rm erlmcp:${VERSION} eval "erlmcp:version()."
```

**Image Metadata**:
- Labels: `version`, `build-date`, `otp-version`, `erlmcp.commit`
- Provenance: SBOM (Software Bill of Materials)
- Scanning: Trivy/Grype vulnerability scan attached

### Artifact 4: Hex Packages

**Purpose**: Erlang package manager distribution

**Generation**:
```bash
# Publish each app
cd apps/erlmcp_core && rebar3 hex publish
cd ../erlmcp_transports && rebar3 hex publish
cd ../erlmcp_observability && rebar3 hex publish
cd ../erlmcp_validation && rebar3 hex publish
```

**Validation**:
- Install from Hex in clean environment
- Run full test suite
- Verify dependencies resolve correctly

### Artifact 5: Evidence Bundle

**Purpose**: Compliance and quality transparency

**Generation**: `.github/workflows/mcp-evidence-bundle.yml`

**Structure**:
```
dist/evidence/${VERSION}/${TIMESTAMP}/
├── MANIFEST.json                    # Bundle manifest
├── metadata/
│   └── bundle_info.json             # Metadata
├── test_results/
│   ├── eunit_summary.json           # Unit test results
│   ├── ct_summary.json              # Integration test results
│   └── proper_summary.json          # Property test results
├── coverage/
│   ├── coverage_summary.json        # Overall coverage
│   └── coverage_report.html         # Detailed report
├── security/
│   ├── security_scan.md             # Secret scan results
│   └── dependency_vulnerabilities.json
├── benchmarks/
│   ├── benchmark_summary.json       # Performance results
│   └── regression_report.md         # Comparison to baseline
└── compliance/
    ├── protocol_compliance.json     # MCP spec compliance
    └── transport_compliance.json    # Transport validation
```

**Validation**:
- All JSON files valid
- MANIFEST matches actual file count
- Checksums file (SHA256SUMS) included

---

## Part IV: Release Workflow

### Phase 1: Preparation (Days -7 to -1)

| Task | Owner | Completes When |
|------|-------|----------------|
| Create release branch | Release Manager | Branch `release/v3.0.0` exists |
| Update version numbers | Developer | All .app.src files have `3.0.0` |
| Write CHANGELOG entry | Technical Writer | `CHANGELOG.md` has `[3.0.0]` section |
| Update documentation | Technical Writer | All docs reference v3.0.0 |
| Run full quality gates | CI/CD | All gates passing |
| Security review | Security Lead | No critical vulnerabilities |
| Performance baseline | Performance | Benchmarks within thresholds |
| Release notes draft | Product Manager | Release notes reviewed |

### Phase 2: Validation (Day 0)

**Automated Validation**: `.github/workflows/release.yml`

```
1. Pre-Release Validation Gate (BLOCKING)
   ├── release-blocker-check.sh
   ├── pre-release-validator.sh 3.0.0
   ├── release-gate-dashboard.sh
   └── release-readiness-report.sh

2. Manual Approval (BLOCKING)
   └── Environment: production-release

3. Build & Test (Parallel)
   ├── Test OTP 25 (if supported)
   ├── Test OTP 26
   ├── Test OTP 27
   └── Test OTP 28

4. Artifact Generation (Parallel)
   ├── Source tarball + checksums
   ├── Production release
   ├── Docker image
   └── Evidence bundle

5. Publish (Parallel)
   ├── GitHub Release
   ├── Docker Hub / GHCR
   └── Hex.pm packages
```

### Phase 3: Release Announcement (Day 0)

| Channel | Template | Timing |
|---------|----------|--------|
| GitHub Release | `templates/release-announcement.md` | Immediately after publish |
| GitHub Discussions | "Announcing erlmcp v3.0.0" | Immediately after publish |
| Blog Post | "What's New in erlmcp v3.0.0" | Day 0 |
| Twitter/X | "erlmcp v3.0.0 is now available!" | Day 0 |
| Mailing List | Release announcement email | Day 0 |
| Discord/Slack | Community announcement | Day 0 |

**Release Announcement Template**:

```markdown
# Announcing erlmcp v3.0.0

## Highlights
- OTP 28.3.1+ required with native JSON module
- 2-3x faster JSON processing
- <1ms health checks with priority messages
- Breaking: v2.x compatibility removed

## What's New
### Features
- Native JSON module (jsx removed)
- Priority messages for critical operations
- O(1) process monitoring

### Performance
- 2.69M+ ops/sec (2-3x improvement)
- <1ms health checks
- Reduced memory footprint

## Migration from v2.x
See [Migration Guide](migration/v2-to-v3.md) for:
- OTP 28.3.1+ upgrade steps
- Breaking changes
- Code changes required

## Downloads
- **Source**: [erlmcp-3.0.0.tar.gz](https://github.com/...) + [SHA256](...)
- **Docker**: `docker pull erlmcp:3.0.0`
- **Hex**: `rebar3 deps` (automatically)

## Documentation
- [Installation Guide](installation/quickstart.md)
- [Migration Guide](migration/v2-to-v3.md)
- [Release Notes](releases/v3.0.0.md)

## Support
- **OSS**: Community support via [GitHub Discussions](...)
- **Commercial**: Contact [sales@example.com](mailto:sales@example.com)

## Thank You
Special thanks to all contributors who made this release possible!
```

---

## Part V: Post-Release Verification

### Verification 1: Installation Smoke Test

**Timing**: Within 1 hour of release

```bash
# Test 1: Clone and compile
git clone https://github.com/.../erlmcp.git
cd erlmcp
git checkout v3.0.0
rebar3 compile  # Must succeed

# Test 2: Run tests
rebar3 eunit    # Must pass
rebar3 ct       # Must pass

# Test 3: Build release
rebar3 as prod release
_build/prod/rel/erlmcp/bin/erlmcp start

# Test 4: Health check
curl http://localhost:8080/health  # Must return 200

# Test 5: Stop
_build/prod/rel/erlmcp/bin/erlmcp stop
```

### Verification 2: Docker Smoke Test

**Timing**: Within 1 hour of release

```bash
# Test 1: Pull image
docker pull erlmcp:3.0.0

# Test 2: Run container
docker run -d --name erlmcp-test -p 8080:8080 erlmcp:3.0.0

# Test 3: Health check
docker exec erlmcp-test erlmcp ping
curl http://localhost:8080/health

# Test 4: Version check
docker exec erlmcp-test erlmcp version  # Must output 3.0.0

# Test 5: Cleanup
docker stop erlmcp-test
docker rm erlmcp-test
```

### Verification 3: Community Feedback

**Timing**: Days 1-7

| Channel | Monitor | Response Time |
|---------|---------|---------------|
| GitHub Issues | New issues opened | 24 hours |
| GitHub Discussions | Questions, issues | 24 hours |
| Stack Overflow | "erlmcp" tag | 48 hours |
| Discord/Slack | Community chat | Real-time |

**Triage Process**:
1. Label issues within 4 hours
2. Prioritize: P0 (critical) > P1 (high) > P2 (medium)
3. Assign to appropriate team member
4. Update issue status daily

### Verification 4: Adoption Metrics

**Timing**: Days 1-30

| Metric | Target | Measurement |
|--------|--------|-------------|
| GitHub stars | +10% vs previous | GitHub API |
| Docker pulls | +100 vs previous week | Docker Hub API |
| Hex downloads | +50 vs previous week | Hex API |
| GitHub clones | +50 vs previous week | GitHub traffic |
| Community members | +5 vs previous | Discord/Slack API |

**Dashboard**: Grafana dashboard showing:
- Release adoption curve
- Issue volume by category
- Response time metrics
- Community growth

---

## Part VI: Rollback Procedure

### Rollback Triggers

| Trigger | Severity | Action |
|---------|----------|--------|
| Critical data loss | P0 | Immediate rollback |
| Security vulnerability (CVSS >= 9.0) | P0 | Immediate rollback |
| >50% deployment failures | P0 | Immediate rollback |
| Performance regression >50% | P1 | Consider rollback |
| Widespread reports | P1 | Evaluate rollback |
| Minor issues | P2 | Hotfix instead |

### Rollback Steps

```bash
# 1. Announce rollback (GitHub Discussions, Twitter)
# 2. Tag previous version as latest
git tag -f latest v2.2.0
git push -f origin latest

# 3. Update Docker tags
docker tag erlmcp:2.2.0 erlmcp:latest
docker push erlmcp:latest

# 4. Publish rollback announcement
# 5. Issue hotfix timeline
# 6. Root cause analysis (5 Whys)
# 7. Prevention measures
```

### Hotfix Process

If rollback is not appropriate:

```
1. Create hotfix branch from release tag
   git checkout -b hotfix/v3.0.1 v3.0.0

2. Fix issue with full tests

3. Run full validation

4. Create hotfix release (skip manual approval for hotfixes)

5. Publish v3.0.1 with changelog entry
```

---

## Part VII: Release Checklist Summary

### Pre-Release Checklist (All Required)

- [ ] Version numbers updated (all .app.src files)
- [ ] CHANGELOG.md has version section
- [ ] All quality gates passing
- [ ] Documentation complete and reviewed
- [ ] Security scan clean (no critical vulnerabilities)
- [ ] Performance baseline verified (no regressions >10%)
- [ ] MCP protocol compliance >= 95%
- [ ] Git status clean (no uncommitted changes)
- [ ] Release branch created and reviewed
- [ ] Release notes drafted
- [ ] Evidence bundle generated
- [ ] Migration guide complete (if breaking changes)

### Release Day Checklist (All Required)

- [ ] Tag created (git tag -a v3.0.0)
- [ ] Tag pushed to remote
- [ ] GitHub Actions workflow triggered
- [ ] All validation jobs passed
- [ ] Manual approval obtained
- [ ] Artifacts generated (tarball, release, Docker)
- [ ] Checksums published
- [ ] GitHub Release published
- [ ] Docker image pushed
- [ ] Hex packages published
- [ ] Evidence bundle uploaded
- [ ] Release announcements sent

### Post-Release Checklist (All Required)

- [ ] Installation smoke test passed
- [ ] Docker smoke test passed
- [ ] Community channels monitored
- [ ] Issues triaged within 24 hours
- [ ] Adoption metrics tracked
- [ ] Release retrospective scheduled (Day 7)
- [ ] Post-release report generated (Day 30)

---

## Part VIII: Tools and Automation

### Release Scripts

| Script | Purpose | Location |
|--------|---------|----------|
| `release-blocker-check.sh` | Quick blocker scan | `tools/release/` |
| `pre-release-validator.sh` | Comprehensive validation | `tools/release/` |
| `release-gate-dashboard.sh` | Visual status dashboard | `tools/release/` |
| `release-readiness-report.sh` | Detailed readiness report | `tools/release/` |
| `changelog-generator.sh` | Generate CHANGELOG entry | `tools/release/` |
| `evidence-bundle.sh` | Generate evidence bundle | `tools/release/` |

### GitHub Actions Workflows

| Workflow | Trigger | Purpose |
|----------|---------|---------|
| `release.yml` | Tag push (`v*`) | Full release pipeline |
| `release-compliance.yml` | Tag push (`v*`) | Compliance validation |
| `mcp-evidence-bundle.yml` | Push to main | Evidence generation |
| `quality-gates.yml` | Pull request | Quality validation |

---

## Part IX: Definitions and Metrics

### Release Types

| Type | Criteria | Example |
|------|----------|---------|
| **Major** | Breaking changes, new features | 3.0.0 |
| **Minor** | New features, backward compatible | 3.1.0 |
| **Patch** | Bug fixes only | 3.0.1 |
| **RC** | Release candidate | 3.0.0-rc.1 |
| **Beta** | Feature complete, testing | 3.0.0-beta.1 |
| **Alpha** | Early development | 3.0.0-alpha.1 |

### Quality Metrics

| Metric | Formula | Target |
|--------|---------|--------|
| **Defect Escape Rate** | Bugs found in release / Total bugs | <5% |
| **Test Coverage** | Lines covered / Total lines | >=80% |
| **Build Success Rate** | Successful builds / Total builds | >=95% |
| **MTTR** | Mean time to resolve incidents | <24 hours |
| **Deployment Success** | Successful deployments / Total | >=99% |
| **Rollback Frequency** | Rollbacks / Total releases | <5% |

### Success Criteria

A release is considered successful if:

1. **Quality**: All automated gates passing
2. **Adoption**: >50 installations in first week
3. **Stability**: <5 P0 issues in first 30 days
4. **Documentation**: No "how to" questions in top 10 issues
5. **Community**: Active engagement (issues, PRs, discussions)

---

## Part X: References

### Related Documents

- `CLAUDE.md` - Project specification
- `docs/quality-gates-enforcement.md` - Quality gate details
- `docs/v3/13_documentation_plan.md` - Documentation strategy
- `tools/release/README.md` - Release tools reference
- `.github/workflows/release.yml` - Release pipeline

### External References

- [Semantic Versioning 2.0.0](https://semver.org/)
- [Keep a Changelog](https://keepachangelog.com/)
- [Apache License 2.0](https://www.apache.org/licenses/LICENSE-2.0)
- [MCP Specification 2025-11-25](https://modelcontextprotocol.io/)
- [Erlang/OTP 28 Release Notes](https://www.erlang.org/doc/release_notes_28.html)

---

## Appendix A: Release Checklist Template

```markdown
## erlmcp Release v[VERSION] Checklist

### Pre-Release
- [ ] Version updated in: [List files]
- [ ] CHANGELOG.md updated
- [ ] Documentation reviewed
- [ ] Quality gates passing
- [ ] Security scan clean
- [ ] Performance verified

### Release
- [ ] Tag created and pushed
- [ ] CI/CD pipeline running
- [ ] Artifacts generated
- [ ] Release published
- [ ] Announcements sent

### Post-Release
- [ ] Smoke tests passed
- [ ] Monitoring active
- [ ] Issues triaged
- [ ] Metrics collected
- [ ] Retrospective scheduled

### Evidence
- Build logs: [Link]
- Evidence bundle: [Link]
- Release notes: [Link]
- Discussion: [Link]
```

---

## Appendix B: Communication Templates

### Subject: erlmcp v[VERSION] Release Candidate Available

```
The erlmcp v[VERSION] release candidate is now available for testing.

Download: [Link]
Documentation: [Link]
Known issues: [Link]

Please test and report issues by [Date].
```

### Subject: erlmcp v[VERSION] Released

```
erlmcp v[VERSION] is now available.

What's new:
- [Feature 1]
- [Feature 2]

Migration: [Link]
Downloads: [Link]
Full notes: [Link]
```

### Subject: erlmcp v[VERSION] Hotfix - [Issue]

```
A hotfix for erlmcp v[VERSION] has been released.

Issue: [Description]
Fix: [Solution]
Upgrade: [Command]
```

---

**Document Status**: Draft for Review
**Next Review**: After v3.0.0 RC1
**Owner**: Release Specialist
**Approvers**: Tech Lead, QA Lead, Documentation Lead

---

## Change Log

| Date | Version | Changes | Author |
|------|---------|---------|--------|
| 2026-01-31 | 0.1 | Initial draft | Release Specialist |
