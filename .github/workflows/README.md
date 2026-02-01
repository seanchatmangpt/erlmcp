# GitHub Actions Workflows - OSS & Commercial

## Overview

This directory contains **GitHub Actions workflows** for erlmcp v3.0, separated into **OSS (public)** and **Commercial (private)** categories.

**Documentation:** See `docs/v3/15_cicd_plan.md` for complete CI/CD strategy.

---

## OSS Workflows (Public)

### Fast Feedback (PR Checks)

#### ⚡ `oss-minimal-ci.yml` - Minimal CI

**Purpose:** Fast feedback for contributors (<10 min)

**Triggers:**
- Pull requests to `main`, `feature/**`, `bugfix/**`, `epic/**`
- Push to `feature/**`, `bugfix/**`

**Jobs (4 parallel, ~8 min):**
1. **Compile** - OTP 28+ compilation
2. **Unit Tests** - EUnit (no coverage)
3. **Lint** - rebar3_lint check
4. **Format** - Code formatting verification

**Quality Gates:**
- ✅ Compilation: Zero errors (BLOCKING)
- ✅ Unit Tests: Pass (BLOCKING)
- ✅ Lint: Zero critical issues (BLOCKING)
- ✅ Format: 100% compliant (BLOCKING)

---

### Full Validation (Main Branch)

#### 🚦 `oss-quality-gates.yml` - Quality Gate Enforcement

**Purpose:** Comprehensive validation before merge (30-45 min)

**Triggers:**
- Push to `main`, `release/**`
- Pull requests to `main`, `release/**`
- Called from `oss-release.yml`

**Jobs (6 sequential, ~35 min):**
1. **Compilation** - Zero errors (BLOCKING)
2. **Xref** - Zero undefined functions (BLOCKING)
3. **Dialyzer** - Zero type errors (BLOCKING)
4. **Unit Tests** - ≥90% pass rate (BLOCKING)
5. **Coverage** - ≥80% code coverage (BLOCKING)
6. **MCP Compliance** - ≥95% spec adherence (BLOCKING)

**Artifacts:**
- Quality gate logs (30 days)
- MCP compliance reports (90 days)

---

### Release Automation

#### 🚀 `oss-release.yml` - Automated Release

**Purpose:** Automated release to Hex.pm, Docker, GitHub

**Triggers:**
- Version tags: `v3.0.0`, `v3.1.0-rc.1`, etc.

**Jobs (5 sequential):**
1. **Validate** - Version consistency, CHANGELOG check
2. **CI Check** - Run full quality gates
3. **Build** - Production tarball
4. **Docker** - Multi-arch images (amd64, arm64)
5. **Hex** - Publish to Hex.pm (stable releases only)
6. **Release** - Create GitHub release

**Artifacts:**
- Release tarball (90 days)
- Docker images: `ghcr.io/${{ repo }}:VERSION`
- Hex.pm packages: erlmcp + 4 umbrella apps
- GitHub release with notes

---

## Legacy Workflows (Being Migrated)

The following workflows from v2.1 are being migrated to v3 OSS structure:

| Workflow | v2.1 Status | v3.0 Target |
|----------|-------------|-------------|
| `ci.yml` | Active | Replace with `oss-full-ci.yml` |
| `quality-gate.yml` | Active | Replace with `oss-quality-gates.yml` |
| `release.yml` | Active | Replace with `oss-release.yml` |
| `mcp-compliance.yml` | Active | Integrate into `oss-quality-gates.yml` |
| `breaking-changes.yml` | Active | Keep as separate check |
| `release-compliance.yml` | Active | Integrate into `oss-release.yml` |

---

## Commercial Workflows (Private)

Commercial workflows are maintained in a **private repository** and include:

- **Enterprise Validation** - Commercial feature testing
- **Multi-Region Deployment** - Cross-region tests
- **SLA Validation** - 99.9% uptime verification
- **Security Audit** - Enterprise compliance
- **Penetration Testing** - Security validation
- **Commercial Release** - Enterprise artifact generation

See `docs/v3/15_cicd_plan.md` Part II for details.

---

## Workflow Decision Matrix

| Event | OSS Workflow | Duration | Blocking? |
|-------|--------------|----------|-----------|
| **PR opened** | `oss-minimal-ci.yml` | 5-10 min | ✅ Yes |
| **PR to main** | `oss-minimal-ci.yml` + `oss-quality-gates.yml` | 35-45 min | ✅ Yes |
| **Push to main** | `oss-quality-gates.yml` | 30-45 min | ✅ Yes |
| **Tag pushed** | `oss-release.yml` | 20-30 min | ✅ Yes |
| **Daily schedule** | `mcp-compliance.yml` | 15-20 min | ⚠️ Warning only |

---

## Quality Gate Reference

### OSS Blocking Gates

| Gate | Threshold | Enforcement |
|------|-----------|-------------|
| **Compilation** | Zero errors | PR blocked |
| **Xref** | Zero undefined functions | PR blocked |
| **Dialyzer** | Zero type errors | PR blocked |
| **Unit Tests** | ≥90% pass rate | PR blocked |
| **Coverage** | ≥80% coverage | PR blocked |
| **MCP Compliance** | ≥95% spec adherence | PR blocked |
| **Format** | 100% compliant | PR blocked |
| **Lint** | Zero critical issues | PR blocked |

### OSS Advisory Gates

| Gate | Threshold | Enforcement |
|------|-----------|-------------|
| **Integration Tests** | 100% pass | Warning only |
| **Benchmark** | <10% regression | Comment on PR |
| **Documentation** | ≥70% coverage | Warning only |
| **Security** | Zero high severity | Comment on PR |

---

## Release Process (OSS)

### Prerequisites

1. All tests pass locally: `make check`
2. CHANGELOG.md updated with version section
3. Version bumped in `rebar.config` and all `*.app.src` files

### Create Release

```bash
# 1. Ensure on main branch
git checkout main
git pull origin main

# 2. Update version files
# Edit rebar.config, apps/*/src/*.app.src
# Edit CHANGELOG.md

# 3. Commit changes
git add rebar.config apps/*/src/*.app.src CHANGELOG.md
git commit -m "Bump version to X.Y.Z"

# 4. Create and push tag
git tag vX.Y.Z
git push origin main
git push origin vX.Y.Z

# 5. GitHub Actions will:
#    - Run full quality gates
#    - Build release tarball
#    - Publish to Hex.pm (if stable)
#    - Build Docker images
#    - Create GitHub release
```

### Rollback Procedure

If a broken release is published:

1. **Yank Hex.pm packages:** `rebar3 hex owner --yank VERSION`
2. **Delete Docker tag:** Remove from GitHub Container Registry
3. **Issue advisory:** Create GitHub Security Advisory
4. **Fix and re-release:** Tag new version (PATCH bump)

---

## Branch Protection Rules

### Main Branch

- ✅ Require `oss-quality-gates.yml` to pass
- ✅ Require 1 maintainer approval
- ✅ Require PR reviews (1 reviewer)
- ❌ Do not allow bypassing rules

### Feature Branches

- ✅ Require `oss-minimal-ci.yml` to pass
- ⚠️ Maintainer approval not required (for contributors)

### Release Branches

- ✅ Require `oss-quality-gates.yml` to pass
- ✅ Require `release-compliance.yml` to pass
- ✅ Require 2 maintainer approvals

---

## Troubleshooting

### Minimal CI Fails

**Problem:** PR check fails in <10 min

**Solution:**
```bash
# Run locally
make check
rebar3 format
rebar3 lint
```

### Quality Gates Fail

**Problem:** Full validation fails on main

**Solution:**
1. Check job logs in Actions tab
2. Fix specific gate failure
3. Push fix to feature branch
4. Open new PR to main

### Release Fails

**Problem:** Tag push doesn't create release

**Solution:**
1. Check `oss-release.yml` logs
2. Verify version consistency across files
3. Ensure CHANGELOG.md has version entry
4. Re-tag if necessary (delete remote tag first)

---

## Status Badges

Add to `README.md`:

```markdown
[![OSS CI](https://github.com/YOUR_REPO/actions/workflows/oss-minimal-ci.yml/badge.svg)](https://github.com/YOUR_REPO/actions/workflows/oss-minimal-ci.yml)
[![Quality Gates](https://github.com/YOUR_REPO/actions/workflows/oss-quality-gates.yml/badge.svg)](https://github.com/YOUR_REPO/actions/workflows/oss-quality-gates.yml)
[![Hex.pm](https://img.shields.io/hexpm/v/erlmcp)](https://hex.pm/packages/erlmcp)
[![MCP Compliance](https://img.shields.io/badge/MCP-2025--11--25-green)](https://github.com/YOUR_REPO/blob/main/docs/MCP_2025-11-25_COMPLIANCE_UNIFIED.md)
```

---

## Migration Timeline

| Phase | Duration | Status |
|-------|----------|--------|
| **Phase 1: OSS Workflows** | Week 1-2 | 🔄 In Progress |
| **Phase 2: Commercial Setup** | Week 3-4 | ⏳ Pending |
| **Phase 3: Quality Gates** | Week 5-6 | ⏳ Pending |
| **Phase 4: Release Automation** | Week 7-8 | ⏳ Pending |
| **Phase 5: Documentation** | Week 9-10 | ⏳ Pending |

---

## References

- **CI/CD Plan:** `docs/v3/15_cicd_plan.md`
- **Contributing Guide:** `CONTRIBUTING.md`
- **Release Guide:** `RELEASE.md` (to be created)
- **MCP Specification:** `docs/MCP_2025-11-25_COMPLIANCE_UNIFIED.md`

---

## Summary

✅ **3 new OSS workflows created:**
- `oss-minimal-ci.yml` - Fast PR checks (<10 min)
- `oss-quality-gates.yml` - Comprehensive validation (30-45 min)
- `oss-release.yml` - Automated release (20-30 min)

✅ **15 legacy workflows being migrated** to v3 structure

✅ **Separate commercial workflows** in private repository

✅ **Automated release process:** Tag → Hex + Docker + GitHub

**Next Steps:** See `docs/v3/15_cicd_plan.md` Part VII for implementation roadmap.
