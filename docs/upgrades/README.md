# OTP Upgrade Documentation Index

**erlmcp Version**: 2.1.0
**Target OTP**: 28.3.1
**Last Updated**: 2026-02-01

---

## Overview

This directory contains comprehensive documentation for upgrading erlmcp to Erlang/OTP 28.3.1.

**Key Documents**:

1. **[VERSION_MIGRATION_GUIDE.md](VERSION_MIGRATION_GUIDE.md)** - Step-by-step migration paths
2. **[API_COMPATIBILITY.md](API_COMPATIBILITY.md)** - API changes and compatibility
3. **[PERFORMANCE_OPTIMIZATION.md](PERFORMANCE_OPTIMIZATION.md)** - Performance tuning guide
4. **[TROUBLESHOOTING.md](TROUBLESHOOTING.md)** - Common issues and solutions
5. **[BEST_PRACTICES.md](BEST_PRACTICES.md)** - Upgrade best practices
6. **[MIGRATION_CHECKLISTS.md](MIGRATION_CHECKLISTS.md)** - Detailed checklists

---

## Quick Start

### For First-Time Upgraders

**Read in this order**:
1. `BEST_PRACTICES.md` - Understand the overall approach
2. `VERSION_MIGRATION_GUIDE.md` - Choose your migration path
3. `MIGRATION_CHECKLISTS.md` - Prepare for the upgrade
4. Execute upgrade with `TROUBLESHOOTING.md` handy
5. Optimize with `PERFORMANCE_OPTIMIZATION.md`

### For Experienced Upgraders

**Quick reference**:
- Migration paths: `VERSION_MIGRATION_GUIDE.md` → "Migration Paths"
- API changes: `API_COMPATIBILITY.md` → "Compatibility Matrix"
- Rollback: `TROUBLESHOOTING.md` → "Emergency Procedures"
- Performance: `PERFORMANCE_OPTIMIZATION.md` → "Configuration Tuning"

---

## Document Summary

### 1. VERSION_MIGRATION_GUIDE.md (15,000 words)

**Covers**:
- Version support matrix (OTP 26, 27, 28)
- Migration paths (26→27→28 or direct 26→28)
- Breaking changes and deprecations
- Step-by-step upgrade procedures
- Rollback procedures
- Testing requirements per version
- Code migration examples

**Key Sections**:
- OTP 26 → 27 Migration
- OTP 27 → 28 Migration
- Direct 26 → 28 Migration
- Breaking Changes
- Deprecation Timeline

**When to Use**: Planning your upgrade path

---

### 2. API_COMPATIBILITY.md (12,000 words)

**Covers**:
- API compatibility matrix across OTP versions
- Standard library changes (gen_server, ETS, regex, JSON)
- OTP behavior changes (application, supervisor, logger)
- erlmcp-specific API changes (2.0.x → 2.1.0)
- Migration code examples
- Backward/forward compatibility strategies

**Key Sections**:
- Standard Library Changes
- OTP Behavior Changes
- erlmcp API Changes
- Migration Code Examples
- Backward Compatibility

**When to Use**: Understanding code changes needed

---

### 3. PERFORMANCE_OPTIMIZATION.md (11,000 words)

**Covers**:
- Performance baselines (before/after metrics)
- OTP 28 optimizations (priority messages, hibernate, PCRE2)
- Configuration tuning (VM args, app config)
- Memory optimization (hibernation, binaries, ETS)
- Scheduler tuning (dirty schedulers, balancer)
- Monitoring and profiling (observer, fprof, OTEL)
- Regression prevention (baseline testing)

**Key Sections**:
- OTP 28 Optimizations
- Configuration Tuning
- Memory Optimization
- Scheduler Tuning
- Benchmarks

**When to Use**: Optimizing after upgrade

---

### 4. TROUBLESHOOTING.md (10,000 words)

**Covers**:
- Installation issues (OTP build failures, dependencies)
- Compilation errors (module not found, undefined functions)
- Runtime errors (badarg, heap overflow, supervisor restarts)
- Performance regressions (throughput, memory)
- Diagnostic tools (observer, fprof, crashdump_viewer)
- Emergency procedures (rollback, emergency mode, data recovery)
- Known issues and workarounds

**Key Sections**:
- Installation Issues
- Compilation Errors
- Runtime Errors
- Performance Regressions
- Emergency Procedures

**When to Use**: Fixing issues during/after upgrade

---

### 5. BEST_PRACTICES.md (9,000 words)

**Covers**:
- Pre-upgrade preparation (assessment, compatibility, testing)
- Testing strategies (unit, integration, performance, load, chaos)
- Production upgrade strategies (blue-green, rolling, canary)
- Monitoring during upgrade (metrics, dashboards, alerts)
- Post-upgrade validation (functional, performance, stability)
- Continuous upgrade practices (stay current, automate)
- Team coordination (roles, communication)

**Key Sections**:
- Pre-Upgrade Preparation
- Testing Strategies
- Production Upgrade Strategies
- Monitoring During Upgrade
- Post-Upgrade Validation

**When to Use**: Planning upgrade strategy

---

### 6. MIGRATION_CHECKLISTS.md (8,000 words)

**Covers**:
- Pre-upgrade checklist (assessment, preparation, validation)
- Upgrade execution checklist (step-by-step)
- Post-upgrade validation checklist (comprehensive)
- Rollback checklist (if needed)
- Team coordination checklist (all roles)
- Documentation update checklist (all docs)

**Key Sections**:
- Pre-Upgrade Checklist (Phase 1-2)
- Upgrade Execution Checklist (Phase 3)
- Post-Upgrade Validation Checklist (Phase 4)
- Rollback Checklist (Phase 5)
- Team Coordination Checklist

**When to Use**: Executing the upgrade

---

## Common Workflows

### Workflow 1: Planning Your Upgrade

**Goal**: Understand requirements and create plan

**Steps**:
1. Read `BEST_PRACTICES.md` → "Pre-Upgrade Preparation"
2. Read `VERSION_MIGRATION_GUIDE.md` → "Migration Paths"
3. Read `API_COMPATIBILITY.md` → "Breaking Changes"
4. Review `MIGRATION_CHECKLISTS.md` → "Pre-Upgrade Checklist"
5. Create your upgrade plan

**Time**: 2-4 hours

---

### Workflow 2: Testing Your Upgrade

**Goal**: Validate upgrade in staging

**Steps**:
1. Set up staging environment (OTP 28)
2. Follow `VERSION_MIGRATION_GUIDE.md` → "Step 2: Install OTP 28"
3. Run tests per `BEST_PRACTICES.md` → "Testing Strategies"
4. Benchmark per `PERFORMANCE_OPTIMIZATION.md` → "Benchmarks"
5. Document results

**Time**: 1-2 days

---

### Workflow 3: Executing Production Upgrade

**Goal**: Upgrade production system

**Steps**:
1. Follow `MIGRATION_CHECKLISTS.md` → "Pre-Upgrade Checklist" (100% complete)
2. Follow `MIGRATION_CHECKLISTS.md` → "Upgrade Execution Checklist"
3. Use `TROUBLESHOOTING.md` if issues arise
4. Follow `MIGRATION_CHECKLISTS.md` → "Post-Upgrade Validation Checklist"
5. Follow `BEST_PRACTICES.md` → "Post-Upgrade Validation"

**Time**: 4-8 hours (including monitoring)

---

### Workflow 4: Optimizing After Upgrade

**Goal**: Tune performance with OTP 28 features

**Steps**:
1. Read `PERFORMANCE_OPTIMIZATION.md` → "OTP 28 Optimizations"
2. Update `vm.args` per "Configuration Tuning"
3. Enable priority messages in code
4. Add hibernate callbacks
5. Run benchmarks per "Benchmarks"
6. Monitor per "Monitoring and Profiling"

**Time**: 1-2 days (including testing)

---

### Workflow 5: Troubleshooting Issues

**Goal**: Resolve upgrade problems

**Steps**:
1. Identify symptom in `TROUBLESHOOTING.md` → Table of Contents
2. Follow diagnostic procedure
3. Implement resolution
4. Verify fix
5. Document for team

**Time**: Varies (5 minutes to several hours)

---

### Workflow 6: Emergency Rollback

**Goal**: Rollback to previous OTP version

**Steps**:
1. `TROUBLESHOOTING.md` → "Emergency Procedures" → "Rollback"
2. Execute rollback checklist (<5 minutes)
3. Verify system stability
4. Document root cause
5. Plan retry

**Time**: 5-10 minutes

---

## Version-Specific Information

### Upgrading from OTP 26

**Primary Documents**:
- `VERSION_MIGRATION_GUIDE.md` → "Direct 26 → 28 Migration" (if skipping OTP 27)
- `API_COMPATIBILITY.md` → "Standard Library Changes" (OTP 26→27 section)
- `PERFORMANCE_OPTIMIZATION.md` → "Configuration Tuning" (may need more tuning)

**Key Considerations**:
- Direct upgrade is higher risk
- More breaking changes to address
- Extended testing required
- Consider intermediate OTP 27 step

### Upgrading from OTP 27

**Primary Documents**:
- `VERSION_MIGRATION_GUIDE.md` → "OTP 27 → 28 Migration"
- `API_COMPATIBILITY.md` → "Standard Library Changes" (OTP 27→28 section)
- `PERFORMANCE_OPTIMIZATION.md` → "OTP 28 Optimizations"

**Key Considerations**:
- Recommended path (most stable)
- Fewer breaking changes
- Standard testing sufficient
- Leverage OTP 28 features immediately

---

## Quick Reference

### Essential Commands

```bash
# Check OTP version
erl -version

# Check erlmcp version
git describe --tags

# Full validation
make check

# Quick smoke test
make smoke

# Health check
make health

# Benchmark
make benchmark-full
make benchmark-quick

# Start/stop
make start
make stop
make restart

# Documentation
make docs
```

### Key Files

| File | Purpose |
|------|---------|
| `rebar.config` | Minimum OTP version, deps, compiler options |
| `vm.args` | VM tuning for OTP 28 |
| `config/sys.config` | Application configuration |
| `CHANGELOG.md` | Version changes |
| `BACKUP_INFO.txt` | Backup locations (create before upgrade) |

### Version Numbers

| Component | Version |
|-----------|---------|
| erlmcp | 2.1.0 (requires OTP 28+) |
| Erlang/OTP | 28.3.1 (minimum) |
| ERTS | 15.0 (included with OTP 28) |

---

## Support Resources

### Documentation

- **Main Docs**: `/Users/sac/erlmcp/docs/`
- **Upgrade Docs**: `/Users/sac/erlmcp/docs/upgrades/`
- **Examples**: `/Users/sac/erlmcp/examples/`
- **Archive**: `/Users/sac/erlmcp/archive/`

### Online Resources

- **GitHub**: https://github.com/seanchatmangpt/erlmcp
- **Issues**: https://github.com/seanchatmangpt/erlmcp/issues
- **Discussions**: https://github.com/seanchatmangpt/erlmcp/discussions
- **Erlang/OTP Docs**: https://www.erlang.org/doc/

### Community

- **Email**: support@erlmcp.org
- **Slack**: #erlmcp on Erlang Slack
- **Mailing List**: erlmcp-announce@googlegroups.com

---

## Document Metadata

**Total Documentation**: ~65,000 words
**Total Checklists**: 200+ items
**Total Code Examples**: 50+
**Total Diagrams**: 10+

**Last Review**: 2026-02-01
**Next Review**: 2026-03-01
**Maintainer**: SPARC Orchestrator

---

## Changelog

### 2026-02-01 - v1.0
- Initial release of OTP upgrade documentation
- Covers OTP 26, 27, 28 migration paths
- Comprehensive troubleshooting guide
- Performance optimization strategies
- Best practices and checklists

---

## Feedback

**Documentation Issues**: https://github.com/seanchatmangpt/erlmcp/issues
**Documentation PRs**: https://github.com/seanchatmangpt/erlmcp/pulls

**When Reporting Documentation Issues**:
- Document name (e.g., `VERSION_MIGRATION_GUIDE.md`)
- Section name (e.g., "OTP 27 → 28 Migration")
- Issue description (typo, unclear, missing info)
- Suggested improvement (if any)

---

**Index Version**: 1.0
**Author**: SPARC Orchestrator
**Review Date**: 2026-03-01
