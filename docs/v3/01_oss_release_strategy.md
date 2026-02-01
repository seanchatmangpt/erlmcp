# erlmcp v3.0 OSS Release Strategy

**Status**: Draft
**Author**: Documentation Specialist
**Created**: 2026-01-31
**Target Release**: v3.0.0

---

## Executive Summary

This document defines the strategy for releasing erlmcp v3.0 as an open-source project. The goal is to make erlmcp accessible to the Erlang community and beyond, with documentation, tooling, and processes that enable adoption and contribution.

**Key Principles**:
- **Documentation First**: Comprehensive docs before code release
- **Low Barrier to Entry**: Easy installation and quickstart
- **Production Ready**: Include operational guides
- **Community Focused**: Enable contributions

---

## Release Objectives

### Primary Objectives

1. **Accessibility**: 5-minute installation from source or Docker
2. **Documentation**: 100% of public APIs documented
3. **Examples**: Working examples for all major use cases
4. **Testing**: All tests passing publicly (CI badges)
5. **Migration**: Clear path from v2.x to v3.0

### Secondary Objectives

1. **Community**: Contribution guidelines and templates
2. **Integration**: Examples for common LLM providers
3. **Performance**: Published benchmarks with methodology
4. **Security**: Security policy and vulnerability reporting
5. **Support**: Clear support channels and escalation

---

## Release Checklist

### Pre-Release (Required)

#### Code
- [ ] All tests passing (EUnit, CT, PropEr)
- [ ] Coverage >= 80%
- [ ] Dialyzer warnings = 0
- [ ] No TODO comments in production code
- [ ] Version numbers updated (v3.0.0)
- [ ] CHANGELOG.md updated
- [ ] git tag created

#### Documentation
- [ ] Installation guides complete
- [ ] Migration guide complete
- [ ] API reference updated
- [ ] All examples tested
- [ ] README.md updated
- [ ] CONTRIBUTING.md complete
- [ ] LICENSE file present

#### Infrastructure
- [ ] CI/CD workflows working
- [ ] Docker image published
- [ ] Hex package published
- [ ] Documentation site configured
- [ ] Issue templates created
- [ ] PR templates created
- [ ] SECURITY.md present

#### Quality
- [ ] Security audit completed
- [ ] Performance benchmarks run
- [ ] Load testing completed
- [ ] Documentation review completed
- [ ] Beta tester feedback incorporated

### Release Day

- [ ] Announce on GitHub Discussions
- [ ] Publish release notes
- [ ] Update website (if applicable)
- [ ] Social media announcement
- [ ] Community channels notified

### Post-Release

- [ ] Monitor issues and PRs
- [ ] Address critical bugs
- [ ] Collect user feedback
- [ ] Plan v3.0.1 if needed

---

## Version Strategy

### Version Numbering

Following [Semantic Versioning 2.0.0](https://semver.org/):

```
MAJOR.MINOR.PATCH[-PRERELEASE]

MAJOR    - Breaking changes (3.x.x)
MINOR    - New features, backward compatible (x.1.x)
PATCH    - Bug fixes, backward compatible (x.x.1)
PRERELEASE - alpha, beta, rc (3.0.0-rc.1)
```

### Current Versions

| Component | Current | Target | Notes |
|-----------|---------|--------|-------|
| erlmcp | 2.2.0 | 3.0.0 | Breaking: OTP 28.3.1+ |
| erlmcp_core | 2.1.0 | 3.0.0 | Sync with main |
| erlmcp_transports | 2.1.0 | 3.0.0 | Sync with main |
| erlmcp_observability | 2.1.0 | 3.0.0 | Sync with main |
| erlmcp_validation | 2.1.0 | 3.0.0 | Sync with main |

### Support Timeline

| Version | Support Type | Until |
|---------|--------------|-------|
| 3.0.x | Full Support | 3.1.0 release + 6 months |
| 2.x.x | Security Only | 2026-07-31 |
| 1.x.x | Deprecated | 2026-01-31 |

---

## Distribution Channels

### Primary Channels

1. **GitHub**
   - Source code
   - Releases
   - Issues and PRs
   - Discussions

2. **Hex.pm**
   - Erlang package registry
   - Dependency management
   - Version discovery

3. **Docker Hub**
   - Container images
   - Multiple tags (latest, 3.0, 3.0.0)
   - Platform variants

### Secondary Channels

1. **Documentation Site** (GitHub Pages)
   - Stable documentation
   - Versioned docs
   - Search functionality

2. **Examples Repository** (included)
   - Working examples
   - Tutorial code
   - Integration samples

---

## Documentation Strategy

### Documentation Layers

```
Layer 1: Quick Start (5 minutes)
  └─ Installation, basic usage

Layer 2: User Guides (30 minutes)
  └─ By persona, use case examples

Layer 3: Administration (1 hour)
  └─ Configuration, operations, scaling

Layer 4: Reference (as needed)
  └─ API, protocol, architecture
```

### Documentation Quality Gates

Before release:
- [ ] All templates followed
- [ ] All examples tested
- [ ] All links verified
- [ ] Code formatted
- [ ] Spell check passed
- [ ] Technical review completed

### Documentation Maintenance

- **Per Release**: Update affected docs
- **Monthly**: Review and update examples
- **Quarterly**: Full documentation audit
- **As Needed**: Fix reported issues

---

## Testing Strategy

### Test Categories

| Category | Tool | Coverage | Pass Rate |
|----------|------|----------|-----------|
| Unit | EUnit | >=80% | 100% |
| Integration | CommonTest | Key workflows | 100% |
| Property | PropEr | Invariants | 100% |
| Load | Custom | 10K connections | >=99% |

### CI/CD Integration

```yaml
# GitHub Actions workflow
name: Test
on: [push, pull_request]
jobs:
  test:
    - compile
    - eunit
    - ct
    - proper
    - dialyzer
    - coverage
```

### Public Testing

- **CI Badges**: Show pass/fail status
- **Coverage Reports**: Public coverage data
- **Benchmark Results**: Published with methodology

---

## Community Strategy

### Contribution Model

**Types of Contributions**:
1. Bug reports (with template)
2. Feature requests (with template)
3. Documentation improvements
4. Code contributions (PRs)
5. Examples and tutorials

### Contribution Process

```
1. Fork and clone
2. Create feature branch
3. Make changes
4. Add tests
5. Update docs
6. Submit PR
7. Code review
8. Merge (when approved)
```

### Contribution Guidelines

See `CONTRIBUTING.md`:
- Code style
- Commit message format
- PR template
- Review criteria

### Contributor Recognition

- Contributors list in README
- AUTHORS file
- Release notes credit

---

## Support Strategy

### Support Tiers

| Tier | Audience | Channel | Response Time |
|------|----------|---------|---------------|
| Community | All | GitHub Discussions | Best effort |
| Issues | Bug reports | GitHub Issues | 1 week |
| Security | Security issues | Private email | 48 hours |
| Enterprise | Paid customers | Dedicated support | 4 hours |

### Self-Service Resources

1. **Documentation**: Comprehensive guides
2. **Examples**: Working code samples
3. **Troubleshooting**: Common issues
4. **Search**: Indexed documentation

### Escalation Path

1. Search documentation
2. Check examples
3. Ask in Discussions
4. Create Issue (for bugs)
5. Contact support (enterprise)

---

## Security Strategy

### Security Policy

- **PUBLIC**: `SECURITY.md`
- **Reporting**: Private disclosure
- **Response**: 90-day disclosure window
- **Credit**: Acknowledge reporters

### Security Features

1. **Authentication**: Multiple mechanisms
2. **Authorization**: Capability-based
3. **Encryption**: TLS support
4. **Secrets**: Encrypted storage
5. **Audit**: Logging and receipts

### Vulnerability Management

1. Report received
2. Triage and classify
3. Develop fix
4. Coordinate disclosure
5. Publish advisory
6. Release fix

---

## Communication Strategy

### Announcement Channels

1. **GitHub Release**: Official announcement
2. **Discussions**: Community discussion
3. **Twitter/X**: Social media
4. **Erlang Forums**: erlangforums.com
5. **LinkedIn**: Professional networks

### Announcement Template

```markdown
# erlmcp v3.0 Released

[Summary of release]

Key Features:
- [Feature 1]
- [Feature 2]
- [Feature 3]

Links:
- Release Notes: [URL]
- Documentation: [URL]
- Migration Guide: [URL]
```

### Ongoing Communication

- **Weekly**: Development updates in Discussions
- **Monthly**: What's new summary
- **Per Release**: Release notes
- **As Needed**: Security advisories

---

## Metrics and Success Criteria

### Release Success Metrics

| Metric | Target | How Measured |
|--------|--------|--------------|
| Documentation Complete | 100% | Coverage report |
| Tests Passing | 100% | CI status |
| Migration Tested | 100% | Test suite |
| Examples Working | 100% | Manual test |
| CI/CD Working | 100% | Workflow runs |

### Post-Release Metrics

| Metric | Target | Timeframe |
|--------|--------|-----------|
| Stars | 100+ | 1 month |
| Forks | 20+ | 1 month |
| Issues | <10 critical | 1 month |
| PRs | 5+ merged | 1 month |
| Downloads | 500+ | 1 month |

### Documentation Metrics

| Metric | Target | How Measured |
|--------|--------|--------------|
| Docs Indexed | 100% | Search coverage |
| Examples Tested | 100% | Test suite |
| Links Valid | 100% | Link checker |
| Pages | 45+ | Count |

---

## Risk Mitigation

### Identified Risks

| Risk | Impact | Mitigation |
|------|--------|------------|
| Incomplete docs | High | Template enforcement, reviews |
| Broken examples | High | Automated testing |
| Migration issues | High | Beta testing, migration guide |
| Security issues | Critical | Security audit, disclosure policy |
| Low adoption | Medium | Marketing, examples, community |

### Contingency Plans

1. **Documentation Incomplete**: Delay release, complete docs
2. **Migration Issues**: Hotfix release, extended support
3. **Security Issue**: Immediate patch, advisory
4. **Low Adoption**: Improve examples, add tutorials

---

## Timeline

### Week 1-2: Documentation Foundation
- [x] Create templates
- [x] Create directory structure
- [ ] Write installation guides
- [ ] Write migration guide

### Week 3-4: User Guides
- [ ] Application developer guide
- [ ] Server implementer guide
- [ ] Client implementer guide
- [ ] AI integrator guide

### Week 5-6: Administration
- [ ] Configuration reference
- [ ] Deployment guide
- [ ] Monitoring guide
- [ ] Security guide

### Week 7: Troubleshooting
- [ ] Common issues
- [ ] Error codes
- [ ] Diagnostics

### Week 8: Review and Refine
- [ ] Technical review
- [ ] User review
- [ ] Final polish

### Week 9: Beta Testing
- [ ] Release beta
- [ ] Collect feedback
- [ ] Fix critical issues

### Week 10: Release
- [ ] Create release tag
- [ ] Publish release
- [ ] Announce

---

## References

### Internal Documents
- [Documentation Plan](13_documentation_plan.md)
- [Migration Guide](../migration/v2-to-v3.md)
- [CHANGELOG.md](../../CHANGELOG.md)

### External References
- [Semantic Versioning](https://semver.org/)
- [Keep a Changelog](https://keepachangelog.com/)
- [Contributor Covenant](https://www.contributor-covenant.org/)
- [Open Source Guides](https://opensource.guide/)

---

**Document Status**: Draft
**Next Review**: Week 5
**Owner**: Documentation Specialist
