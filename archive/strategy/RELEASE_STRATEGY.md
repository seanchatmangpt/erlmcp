# Release Strategy - erlmcp Workspace

**Version**: 1.0.0 | **Last Updated**: 2026-01-26 | **Status**: Active

## Executive Summary

erlmcp uses **Semantic Versioning (SemVer)** with a structured release cadence combining scheduled monthly releases with on-demand patch releases. This strategy ensures stability, predictability, and rapid response to critical issues while maintaining backward compatibility.

---

## 1. Versioning Scheme

### Semantic Versioning (MAJOR.MINOR.PATCH)

```
Version Format: vMAJOR.MINOR.PATCH[-PRERELEASE][+BUILD]

Example: v1.2.3-beta.1+build.2026.01.26

Components:
- MAJOR: Breaking changes (API incompatibilities, major features)
- MINOR: Backward-compatible new features, enhancements
- PATCH: Bug fixes, performance improvements, security patches
- PRERELEASE: alpha, beta, rc (Release Candidate)
- BUILD: Build metadata (optional)
```

### Versioning Rules

1. **MAJOR (X.0.0)**
   - Breaking changes in erlmcp API
   - Major refactoring of core MCP protocol
   - Removal of deprecated features (major versions only)
   - Major taiea governance model changes
   - Increment: 0 → 1 → 2 (reserve 1-2 years per major version)

2. **MINOR (X.Y.0)**
   - New features (backward-compatible)
   - Enhancements to existing features
   - New CLI commands or options
   - Performance improvements
   - Dependency upgrades (non-breaking)
   - Increment: Monthly or on-demand

3. **PATCH (X.Y.Z)**
   - Bug fixes
   - Security patches
   - Documentation corrections
   - Minor improvements
   - Increment: On-demand (critical fixes same-day)

### Pre-Release Versions

Pre-release versions are for testing and validation before general availability:

```
Format: vMAJOR.MINOR.PATCH-PRERELEASE

Examples:
- v1.2.0-alpha.1      (First alpha)
- v1.2.0-beta.1       (Beta testing phase)
- v1.2.0-rc.1         (Release candidate - feature freeze)

Precedence: 1.0.0-alpha < 1.0.0-beta < 1.0.0-rc < 1.0.0
```

**Pre-Release Validation Gate**: All tests + integration tests must pass before promoting to next stage.

---

## 2. Release Cadence

### Monthly Release Cycle (MINOR + PATCH)

**First Monday of each month at 2 PM UTC**

```
Timeline:
Monday (Week 1)    → Release cut from develop branch
                   → Create release candidate (v1.2.0-rc.1)
                   → Deploy to staging
                   → Run full integration tests (2-4 hours)
                   → Deploy to canary (5% traffic)

Tuesday-Wednesday → Monitor canary metrics
                   → Smoke tests + E2E tests
                   → Community feedback window

Thursday          → Promote to production (v1.2.0)
                   → Create GitHub release
                   → Deploy to all regions
                   → Monitor golden signals

Friday            → Post-release verification
                   → Documentation updates
                   → Retrospective if issues found
```

### On-Demand Patch Releases (PATCH)

**For critical bugs, security issues, P0/P1 incidents**

```
Trigger: P0/P1 issue in production
Timeline:
- Issue reported → Assess severity
- If critical: Create hotfix branch from main
- Fix + test locally (< 1 hour)
- Merge hotfix PR with expedited review
- Create patch tag (v1.2.1)
- Deploy to production within 2 hours
- Post-mortem scheduled for Monday
```

### Long-Term Support (LTS) Versions

**Selected minor versions receive 12+ months of support**

```
v1.0.x (LTS until 2027-02-26)      Primary support release
v1.2.x (LTS until 2027-04-26)      Selected after 2 months validation
v2.0.x (TBD)                        Future major version

LTS Commitments:
- Critical security patches: within 24 hours
- P1 bug fixes: within 1 week
- Backport policy: maintain compatibility
- End-of-Life: 12 months after release
```

---

## 3. Release Process

### Phase 1: Pre-Release Planning (Week Before Release)

**Monday (Release Planning)**
```
1. Create release branch: git checkout -b release/v1.2.0
2. Update version numbers:
   - src/erlmcp.app.src (version)
   - CHANGELOG.md (add v1.2.0 section)
   - docs/version.md
3. Run full test suite:
   rebar3 as prod,test eunit
   rebar3 as prod dialyzer
   rebar3 as prod xref
   rebar3 as prod check
4. Create release PR with:
   - Version bumps
   - CHANGELOG
   - Migration guide (if needed)
5. Team review and approval
```

### Phase 2: Release Candidate (RC)

**Create Release Candidate Tag**
```bash
git tag -a v1.2.0-rc.1 -m "Release Candidate v1.2.0-rc.1

- Feature 1: Description
- Feature 2: Description
- Bug Fix 1: Description

RC Testing:
- [ ] Full test suite passing
- [ ] Integration tests passing
- [ ] Staging deployment successful
- [ ] Canary metrics healthy
"

git push origin v1.2.0-rc.1
```

**RC Validation Checklist**
- [x] Unit tests: 100% pass
- [x] Integration tests: 100% pass
- [x] Property-based tests: 100% pass (proper)
- [x] Dialyzer: 0 errors
- [x] Xref: 0 errors
- [x] Code coverage: >= 80%
- [x] Documentation: Complete and accurate
- [x] CHANGELOG: Populated
- [x] Docker image: Built and tested
- [x] Backward compatibility: Verified

### Phase 3: Production Release

**Thursday - Release to Production**
```bash
# Create final release tag
git tag -a v1.2.0 -m "Release v1.2.0

Release notes:
$(cat CHANGELOG.md | sed -n '/^## v1.2.0/,/^## v1.1/p')

Release Date: 2026-02-03
Build: CI#12345
Docker Image: gcr.io/erlmcp/erlmcp:1.2.0
"

git push origin v1.2.0

# GitHub Actions automatically:
# - Creates GitHub release
# - Builds Docker image
# - Publishes to Hex package manager
# - Deploys to staging
# - Triggers production deployment workflow
```

### Phase 4: Post-Release Verification

**Friday - Validation & Documentation**

```
1. Monitor production metrics (24 hours post-release):
   - Error rate (< 0.1% baseline)
   - P99 latency (< 10ms baseline)
   - Memory usage (< 5% increase)
   - CPU usage (< 5% increase)
   - Deployment success rate (100%)

2. Community feedback window:
   - Monitor GitHub issues
   - Check Slack/Discord mentions
   - Response SLA: 4 hours for critical reports

3. Post-release tasks:
   - [ ] Update release notes
   - [ ] Pin documentation to version
   - [ ] Update guides
   - [ ] Announce release (blog/social)
   - [ ] Schedule retrospective if issues

4. If issues found:
   - Create hotfix branch
   - Test thoroughly
   - Release v1.2.1 within 2 hours
   - Post-mortem on Monday
```

---

## 4. Beta & RC Process

### Beta Phase (v1.2.0-beta.X)

**Purpose**: Early feature validation, community testing

**Criteria for Beta**:
- Feature complete (all planned features merged)
- Passes all automated tests
- No known critical bugs
- Documentation complete (drafts acceptable)

**Beta Process**:
```bash
# Create beta tag
git tag -a v1.2.0-beta.1 -m "Beta Release v1.2.0-beta.1"
git push origin v1.2.0-beta.1

# Deploy to beta environment
# Announce on GitHub discussions + community channels
# Collect feedback for 1 week minimum

# After feedback resolved or 7 days passed:
# Promote to RC
git tag -a v1.2.0-rc.1 -m "Release Candidate v1.2.0-rc.1"
git push origin v1.2.0-rc.1
```

**Beta Support**:
- No SLA for non-critical issues
- Critical bugs: hotfix released immediately (v1.2.0-beta.2)
- Breaking changes still possible (beta is for validation)

### RC Phase (v1.2.0-rc.X)

**Purpose**: Final validation before general availability

**Criteria for RC**:
- All beta feedback addressed
- Feature freeze (only bug fixes)
- All tests passing with 80%+ coverage
- Documentation complete
- Performance validated

**RC Process**:
```bash
# RC is deployed to staging + canary
# Runs full integration test suite (2-4 hours)
# Monitors canary metrics for 24-48 hours
# Collects production-like telemetry

# Promotion criteria:
# - [ ] All tests passing
# - [ ] Canary metrics normal
# - [ ] No critical issues found
# - [ ] Documentation verified
# - [ ] Performance validated

# Then: git tag -a v1.2.0 -m "Release v1.2.0"
```

---

## 5. Dependency Management

### Dependency Update Policy

**Weekly Reviews** (every Monday):
```bash
# Check for updates
rebar3 upgrade

# Review:
- Security advisories
- Breaking changes
- Performance impacts
- Compatibility

# Categories:
- SECURITY: Apply immediately, backport if needed
- MAJOR: Plan for next MAJOR version
- MINOR: Include in next monthly release
- PATCH: Include in next patch release
```

### Erlang/OTP Version Support

```
Supported Versions:
- OTP 25.x: Current stable (until April 2026)
- OTP 26.x: Current release (until April 2028)
- OTP 27.x: Experimental (from Sept 2024)

Policy:
- Minimum: OTP 25 (declared in rebar.config)
- Recommend: OTP 26 LTS
- Test in CI: OTP 25, 26, 27
```

---

## 6. Backward Compatibility

### Compatibility Guarantees

**Within MINOR versions (1.1.x to 1.2.x)**:
- API is backward-compatible
- Message formats unchanged
- Configuration compatible
- Data schema compatible

**MAJOR version changes**:
- Breaking changes allowed
- Migration guide required
- Deprecation warnings in v(N-1)
- Support previous major for 6 months

### Deprecation Policy

**Deprecation Timeline**:
```
v1.2.0: Introduce new API
v1.2.1+: Mark old API @deprecated in docs
v1.3.0: Log warnings when old API used
v2.0.0: Remove old API

Example:
  v1.1.0: introduce new_function/2
  v1.2.0: mark old_function/1 @deprecated
  v1.3.0: add warning logs to old_function/1
  v2.0.0: remove old_function/1
```

---

## 7. Testing & Quality Gates

### Release Validation Checklist

**All must pass before release promotion**

```
Testing:
✓ Unit tests:        rebar3 eunit
✓ Property tests:    rebar3 proper
✓ Integration tests: docker + smoke tests
✓ Performance:       Compare to baseline
✓ Security scan:     Static analysis

Analysis:
✓ Coverage:          >= 80% (code coverage)
✓ Dialyzer:          0 errors (type analysis)
✓ Xref:              0 errors (cross-module refs)
✓ Linting:           0 warnings (rebar3_lint)

Documentation:
✓ CHANGELOG updated
✓ API docs complete
✓ Migration guide (if breaking)
✓ Examples updated
✓ Runbooks reviewed

Operations:
✓ Docker image built & scanned
✓ Deployment script tested
✓ Rollback procedure verified
✓ Monitoring configured
✓ Alerts configured
```

---

## 8. Rollback & Recovery

### Production Rollback Procedure

**If critical issue found post-release**:

```bash
# 1. Assess severity (P0/P1 = immediate rollback)
# 2. Trigger rollback
kubectl set image deployment/erlmcp \
  erlmcp=gcr.io/erlmcp/erlmcp:v1.1.5 --record

# 3. Verify rollback success
kubectl rollout status deployment/erlmcp

# 4. Create incident
# - Root cause
# - Impact
# - Resolution
# - Prevention

# 5. Create hotfix
# - Fix issue in v1.2.0 release
# - Release v1.2.1 within 2 hours
# - Post-mortem scheduled
```

### Rollback Criteria

**Trigger rollback if**:
- Error rate > 5% (vs baseline 0.1%)
- P99 latency > 50ms (vs baseline 10ms)
- Memory leak detected
- Data corruption confirmed
- Security vulnerability exploited
- Service unavailable for > 15 minutes

---

## 9. Communication & Announcements

### Release Announcement Timeline

**2 weeks before release**:
- Internal: Release planning kickoff
- Update: What's coming this release

**1 week before release**:
- Internal: Feature complete announcement
- Prepare: Draft release notes

**Day of release (Thursday)**:
- Release notes published
- GitHub release created
- Docker image published to registry
- Announcement: Slack, Discord, email list
- Blog post (if major features)

**Post-release**:
- Monitoring updates
- Community support
- Gather feedback

### Release Notes Template

```markdown
## [v1.2.0] - 2026-02-03

### Breaking Changes
- None

### Deprecated
- None

### New Features
- Feature 1: Description
- Feature 2: Description

### Enhancements
- Enhancement 1: Description
- Enhancement 2: Description

### Bug Fixes
- Fix 1: Closes #123
- Fix 2: Closes #456

### Security
- None (or list critical security fixes)

### Documentation
- Updated API guide
- Added example: [example link]

### Dependencies
- Upgraded library X to 1.2.0
- Removed deprecated library Y

### Known Issues
- Issue 1: Workaround available
- Issue 2: Will fix in v1.2.1

### Contributors
- @contributor1
- @contributor2

### Installation
\`\`\`bash
# Via rebar3
{deps, [{erlmcp, "1.2.0"}]}

# Via Docker
docker pull gcr.io/erlmcp/erlmcp:1.2.0
\`\`\`

### Links
- [Full Changelog](CHANGELOG.md#v120)
- [Migration Guide](MIGRATION.md)
- [Documentation](docs/)
```

---

## 10. SLOs & Metrics

### Release Success Metrics

**Stability SLO (99.5% uptime)**:
```
- Deployment success rate: >= 99%
- P99 latency increase: < 5%
- Error rate increase: < 0.5%
- Memory usage increase: < 10%
```

**Incident Response**:
```
- P0 critical bugs: 1-hour response, 2-hour fix
- P1 major bugs: 4-hour response, 24-hour fix
- P2 minor bugs: 24-hour response, 1-week fix
```

**Release Cadence**:
```
- Monthly releases: 1st Monday
- Patch releases: < 2 hours for critical
- LTS support: 12 months minimum
- MAJOR version: 1-2 year stability
```

---

## 11. Tools & Infrastructure

### Release Tools

**Versioning**:
- Git tags (source of truth)
- rebar.config (Erlang version)
- CHANGELOG.md (human-readable)

**Building**:
- rebar3 (build tool)
- Docker (containerization)
- GitHub Actions (CI/CD)

**Publishing**:
- GitHub releases (distribution)
- Hex package manager (Erlang packages)
- Docker registry (container images)

**Monitoring**:
- Prometheus (metrics)
- Datadog/GCP Cloud Monitoring
- PagerDuty (alerting)

### Release Automation

**GitHub Actions Workflow** (`release.yml`):
```
Trigger: Git tag push (v*.*)
├─ Validate: Tests, linting, coverage
├─ Build: Docker image, package
├─ Test: Integration tests
├─ Deploy: Staging → Canary → Production
├─ Verify: Health checks, metrics
└─ Notify: GitHub, Slack, email
```

---

## 12. Special Cases

### Security Releases

**For security vulnerabilities (CVE)**:

```
1. Assessment: Severity score (CVSS)
2. If critical (CVSS >= 7.0):
   - Private security advisory prepared
   - Patch developed & tested
   - Pre-release notification to customers
   - Coordinated public release (72-hour notice)
3. Release process:
   - Hot-patch release (e.g., v1.2.3)
   - Security advisory published
   - Backports to LTS versions
   - Retroactive tag (v1.1.5-security)
```

### Database Migrations

**If release includes database schema changes**:

```
1. Create reversible migration
2. Test migration on production-like data
3. Create rollback procedure
4. Include in RC testing phase
5. Document migration guide
6. Plan backup before deployment
7. Monitor migration execution in prod

Migration naming:
- Forward: migration_YYYYMMDD_description.erl
- Rollback: rollback_YYYYMMDD_description.erl
```

### API Deprecation & Removal

**3-version deprecation cycle**:

```
v1.0.0: Introduce deprecation warning
v1.1.0: Log deprecation notice (default on)
v1.2.0: Make deprecation notice default for errors
v2.0.0: Remove completely

Example: Removing old_api/1 function
v1.0.0: Add warning: "old_api/1 deprecated, use new_api/1"
v1.1.0: Add log statement when called
v1.2.0: Default log level increased
v2.0.0: Function removed entirely
```

---

## 13. Examples

### Example Release: v1.2.0

```bash
# Week before: Release planning
git checkout -b release/v1.2.0
# Update version numbers, CHANGELOG
git commit -m "chore: prepare v1.2.0 release"
git push origin release/v1.2.0

# Create release PR, get approval
# Merge to main

# Monday: Tag RC
git tag -a v1.2.0-rc.1 -m "Release Candidate v1.2.0-rc.1"
git push origin v1.2.0-rc.1
# CI builds, tests, deploys to staging

# Wednesday: Promote from RC
# All tests passing, canary metrics good
git tag -a v1.2.0 -m "Release v1.2.0"
git push origin v1.2.0
# CI publishes to Hex, Docker, creates GitHub release

# Friday: Post-release verification
# Monitor metrics, collect feedback
# All good = schedule retrospective for v1.3.0 planning
```

### Example Hotfix: v1.2.1

```bash
# Issue found in production (v1.2.0)
# Create hotfix branch from main
git checkout -b hotfix/v1.2.1
# Fix bug, add test, commit
git commit -m "fix: critical bug in XYZ - Closes #789"

# Create PR, expedited review (< 1 hour)
# Merge to main

# Tag immediately
git tag -a v1.2.1 -m "Hotfix v1.2.1: Critical bug fix"
git push origin v1.2.1
# CI automatically builds and deploys

# Schedule post-mortem for Monday
# Document root cause and prevention
```

---

## 14. Version History & Releases

**Planned Release Schedule**:

```
v1.0.0 (Current)          2026-02-03  (LTS until 2027-02-03)
v1.1.0 (Next)             2026-03-03
v1.2.0 (Planned)          2026-04-07  (LTS candidate)
v2.0.0 (Future)           2026-09-XX  (Next major version)

LTS Versions:
- v1.0.x: 12-month support
- v1.2.x: 12-month support (if selected)
- v2.0.x: 18-month support (planned)
```

---

## Revision History

| Date | Version | Changes |
|------|---------|---------|
| 2026-01-26 | 1.0.0 | Initial release strategy |

---

## References

- [Semantic Versioning](https://semver.org/)
- [Erlang/OTP Release Process](https://erlang.org/doc/design_principles/release_handling.html)
- [GitHub Release Best Practices](https://docs.github.com/en/repositories/releasing-projects-on-github)
- See also: DEVELOPMENT.md, CONTRIBUTING.md, CHANGELOG.md
