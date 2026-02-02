# erlmcp-flow Git Workflow Summary

**Version**: 1.0.0
**Date**: 2026-02-02
**Status**: Production Ready

---

## Quick Reference

This document provides a quick reference for the complete Git workflow design for erlmcp-flow agent coordination transport.

## Documentation Structure

```
/home/user/erlmcp/
├── docs/
│   ├── ERLMCP_FLOW_GIT_WORKFLOW.md          ← Complete workflow design
│   └── ERLMCP_FLOW_GIT_WORKFLOW_SUMMARY.md  ← This document
│
├── .github/
│   ├── workflows/
│   │   └── erlmcp-flow-ci.yml               ← CI/CD pipeline
│   │
│   ├── ISSUE_TEMPLATE/
│   │   ├── erlmcp-flow-bug.md               ← Bug report template
│   │   ├── erlmcp-flow-feature.md           ← Feature request template
│   │   └── erlmcp-flow-performance.md       ← Performance issue template
│   │
│   └── PULL_REQUEST_TEMPLATE_ERLMCP_FLOW.md ← PR template
```

---

## 1. Branch Strategy

### Main Branch
- **Branch**: `main`
- **Protection**: Full branch protection (2+ approvals, all checks must pass)
- **Purpose**: Production-ready code only

### Feature Branches
- **Pattern**: `claude/erlmcp-flow-{feature}-{random}`
- **Examples**: `claude/erlmcp-flow-registry-R9zub`
- **Lifecycle**: Branch → Implement → Test → PR → Review → Merge → Delete

### Release Branches
- **Pattern**: `release/v{major}.{minor}.{patch}`
- **Examples**: `release/v1.0.0`
- **Purpose**: Stabilization and release preparation

### Hotfix Branches
- **Pattern**: `hotfix/v{major}.{minor}.{patch+1}-{issue}`
- **Examples**: `hotfix/v1.0.1-memory-leak`
- **Purpose**: Emergency fixes to production

---

## 2. Merge Strategy: NEVER REBASE

**Golden Rule**: All merges use `git merge --no-ff` to preserve history.

```bash
# Update feature branch from main
git checkout claude/erlmcp-flow-registry-R9zub
git merge origin/main --no-ff

# NEVER use:
# git rebase origin/main  ← PROHIBITED
# git push --force        ← PROHIBITED
```

**Why?**
- ✅ Complete audit trail for compliance
- ✅ Easy rollback via revert
- ✅ No lost commits from force pushes
- ✅ Conflict resolution is explicit

---

## 3. Quality Gates (CI/CD)

### Blocking Gates

All must pass before merge:

| Gate | Target | Command |
|------|--------|---------|
| **Compilation** | 0 errors | `TERM=dumb rebar3 compile` |
| **Format** | Consistent | `rebar3 format --check` |
| **EUnit** | 100% pass | `rebar3 eunit --app erlmcp_flow` |
| **Coverage** | ≥82% | `rebar3 cover` |
| **Dialyzer** | 0 warnings | `rebar3 dialyzer` |
| **CT Tests** | 100% pass | `rebar3 ct --dir apps/erlmcp_flow/test` |

### Warning Gates

Non-blocking but reviewed:

| Gate | Target | Command |
|------|--------|---------|
| **Xref** | 0 undefined | `rebar3 xref` |
| **Benchmarks** | <10% regression | `rebar3 eunit --module erlmcp_flow_bench` |

---

## 4. PR Workflow

### Create PR

```bash
# 1. Ensure quality gates pass locally
cd /home/user/erlmcp
make compile
rebar3 eunit --app erlmcp_flow
rebar3 cover
rebar3 dialyzer

# 2. Push feature branch
git push origin claude/erlmcp-flow-registry-R9zub

# 3. Create PR
gh pr create \
  --title "feat(erlmcp-flow): Add O(log N) agent registry" \
  --body-file .github/PULL_REQUEST_TEMPLATE_ERLMCP_FLOW.md \
  --base main
```

### PR Requirements
- ✅ 2+ approvals
- ✅ All CI checks passed
- ✅ Branch up-to-date with main
- ✅ No merge conflicts
- ✅ Documentation complete

### Merge PR

```bash
# Option 1: Squash and merge (recommended)
gh pr merge --squash --delete-branch

# Option 2: Merge commit (preserves all commits)
gh pr merge --merge --delete-branch

# NEVER: gh pr merge --rebase  ← PROHIBITED
```

---

## 5. Release Process

### Pre-Release Checklist

- [ ] Version bump in `erlmcp_flow.app.src`
- [ ] CHANGELOG.md updated
- [ ] All quality gates pass
- [ ] Benchmarks pass
- [ ] Documentation updated
- [ ] Migration guide (if breaking changes)

### Create Release

```bash
# 1. Create release branch
git checkout -b release/v1.0.0 main

# 2. Apply pre-release checklist

# 3. Create PR
gh pr create --base main --head release/v1.0.0

# 4. After merge, create GitHub release
git tag v1.0.0
git push origin v1.0.0
gh release create v1.0.0 --notes "Release notes..."
```

### Release Artifacts
- Compiled release: `_build/prod/rel/erlmcp_flow-{version}.tar.gz`
- Coverage report: `_build/test/cover/index.html`
- Benchmark report: `bench/results/v{version}-benchmark-report.json`
- Documentation: `doc/`
- Checksums: `SHA256SUMS`

---

## 6. Issue Management

### Issue Types

| Type | Label | Template |
|------|-------|----------|
| **Bug** | `bug, erlmcp-flow` | `.github/ISSUE_TEMPLATE/erlmcp-flow-bug.md` |
| **Feature** | `enhancement, erlmcp-flow` | `.github/ISSUE_TEMPLATE/erlmcp-flow-feature.md` |
| **Performance** | `performance, erlmcp-flow` | `.github/ISSUE_TEMPLATE/erlmcp-flow-performance.md` |

### Issue Lifecycle

```
Issue Opened → Triage → In Progress → PR Created → PR Merged → Verified
```

---

## 7. Emergency Procedures

### Production Hotfix

```bash
# 1. Create hotfix branch
git checkout -b hotfix/v1.0.1-memory-leak main

# 2. Apply fix with test

# 3. Quality gates
make compile
rebar3 eunit --app erlmcp_flow
rebar3 dialyzer

# 4. Create PR with 'urgent' label
gh pr create --title "hotfix(v1.0.1): Fix memory leak" --label urgent

# 5. Fast-track review (1 reviewer, <2h)

# 6. Merge and release
gh pr merge --squash
git tag v1.0.1
gh release create v1.0.1 --notes "Hotfix: Memory leak"
```

### Rollback

```bash
# 1. Identify last known good version
LAST_GOOD_VERSION=v1.0.0

# 2. Revert merge commit
git revert -m 1 <merge-commit-sha>
git push origin main

# 3. Redeploy previous release
kubectl rollout undo deployment/erlmcp-flow
```

---

## 8. Common Workflows

### Start New Feature

```bash
# 1. Create feature branch
git checkout main
git pull origin main
git checkout -b claude/erlmcp-flow-registry-R9zub

# 2. Implement with tests (Chicago TDD)
# Write test first, then implementation

# 3. Quality gates
cd apps/erlmcp_flow
TERM=dumb rebar3 compile
rebar3 eunit
rebar3 cover
rebar3 dialyzer
rebar3 xref

# 4. Commit
git add .
git commit -m "feat(erlmcp-flow): Add agent registry

- O(log N) lookup with gproc
- Type and capability indexing
- Automatic cleanup on crash

Tests: 42 EUnit (100% pass)
Coverage: 89% (target: ≥82%)
Dialyzer: 0 warnings

https://claude.ai/code/session_..."

# 5. Push and create PR
git push origin claude/erlmcp-flow-registry-R9zub
gh pr create
```

### Update Feature Branch

```bash
# Keep feature branch up-to-date with main
git checkout claude/erlmcp-flow-registry-R9zub
git fetch origin
git merge origin/main --no-ff

# Resolve conflicts if any
git add .
git commit -m "Merge main into registry branch"

# Push
git push origin claude/erlmcp-flow-registry-R9zub
```

### Resolve Merge Conflicts

```bash
# 1. Merge main
git merge origin/main --no-ff
# CONFLICT

# 2. Identify conflicts
git status

# 3. Resolve manually
vim apps/erlmcp_flow/src/erlmcp_flow_registry.erl
# Remove <<<<<<< HEAD, =======, >>>>>>> markers

# 4. Test resolution
make compile
rebar3 eunit --app erlmcp_flow

# 5. Commit resolution
git add .
git commit -m "Resolve merge conflict in registry

Both changes were complementary:
- Added lookup_agent/1 (main)
- Added register_agent/3 (feature)

Tests: All pass
Coverage: 89%"

# 6. Push
git push origin claude/erlmcp-flow-registry-R9zub
```

---

## 9. Quality Standards

### OTP Compliance

- ✅ `gen_server` pattern for stateful processes
- ✅ Proper supervision (never unsupervised spawn)
- ✅ Let-it-crash philosophy
- ✅ Process isolation (no shared mutable state)
- ✅ Message passing for communication

### Chicago TDD

- ✅ Test-first development
- ✅ Real processes (no mocks)
- ✅ Deterministic tests (no sleeps, no race conditions)
- ✅ Coverage ≥82%

### Performance

- ✅ Agent lookup: p50 <10μs, p95 <50μs, p99 <100μs
- ✅ Direct messaging: >100K msg/sec
- ✅ Broadcast (60 agents): avg <10ms
- ✅ Registry throughput: >500K lookups/sec

---

## 10. File Locations

### Workflow Documentation
- `/home/user/erlmcp/docs/ERLMCP_FLOW_GIT_WORKFLOW.md` - Complete design
- `/home/user/erlmcp/docs/ERLMCP_FLOW_GIT_WORKFLOW_SUMMARY.md` - This summary

### CI/CD
- `.github/workflows/erlmcp-flow-ci.yml` - GitHub Actions workflow

### Templates
- `.github/PULL_REQUEST_TEMPLATE_ERLMCP_FLOW.md` - PR template
- `.github/ISSUE_TEMPLATE/erlmcp-flow-bug.md` - Bug report
- `.github/ISSUE_TEMPLATE/erlmcp-flow-feature.md` - Feature request
- `.github/ISSUE_TEMPLATE/erlmcp-flow-performance.md` - Performance issue

### Architecture
- `docs/ERLMCP-FLOW-README.md` - Main README
- `docs/erlmcp-flow-architecture.md` - Architecture design
- `docs/erlmcp-flow-examples.md` - Code examples
- `docs/erlmcp-flow-diagrams.md` - Visual diagrams
- `docs/erlmcp-flow-implementation-plan.md` - Implementation plan

---

## 11. Key Principles

### NEVER
- ❌ `git rebase` - Rewrites history
- ❌ `git push --force` - Destroys commits
- ❌ `git commit --amend` on pushed commits
- ❌ `git reset --hard` on shared branches
- ❌ `--no-verify` - Bypasses quality gates

### ALWAYS
- ✅ `git merge --no-ff` - Preserves merge commits
- ✅ Quality gates pass before merge
- ✅ 2+ PR approvals
- ✅ Documentation updated
- ✅ CHANGELOG.md updated
- ✅ Tests with real processes (Chicago TDD)
- ✅ OTP patterns (gen_server, supervision, let-it-crash)

---

## 12. Quick Commands

### Local Quality Check

```bash
# Quick check before commit
cd /home/user/erlmcp
make compile
rebar3 eunit --app erlmcp_flow
rebar3 cover
rebar3 dialyzer
```

### Create PR

```bash
gh pr create \
  --title "feat(erlmcp-flow): {description}" \
  --body-file .github/PULL_REQUEST_TEMPLATE_ERLMCP_FLOW.md
```

### Create Issue

```bash
# Bug
gh issue create \
  --title "[erlmcp-flow] Bug: {description}" \
  --body-file .github/ISSUE_TEMPLATE/erlmcp-flow-bug.md \
  --label "bug,erlmcp-flow"

# Feature
gh issue create \
  --title "[erlmcp-flow] Feature: {description}" \
  --body-file .github/ISSUE_TEMPLATE/erlmcp-flow-feature.md \
  --label "enhancement,erlmcp-flow"

# Performance
gh issue create \
  --title "[erlmcp-flow] Performance: {description}" \
  --body-file .github/ISSUE_TEMPLATE/erlmcp-flow-performance.md \
  --label "performance,erlmcp-flow"
```

---

## 13. GitHub Actions Status

### Workflow: erlmcp-flow-ci.yml

**Trigger Events**:
- Push to `main`, `claude/erlmcp-flow-*`, `release/v*`
- PR to `main` affecting erlmcp_flow

**Jobs**:
1. **erlmcp-flow-quality-gates** (OTP 26, 27, 28)
   - Compilation
   - Format check
   - Xref
   - Dialyzer
   - EUnit tests
   - Coverage (≥82%)
   - Common Test
   - Benchmarks

2. **erlmcp-flow-structure-check**
   - Verify application structure

3. **erlmcp-flow-docs-check**
   - Verify documentation

4. **erlmcp-flow-quality-gates-summary**
   - Overall status

**Artifacts**:
- Coverage reports (30 days retention)
- Benchmark results (90 days retention)

---

## 14. Contact & Support

**Design**: erlang-github-ops agent
**Date**: 2026-02-02
**Status**: Production Ready

**For Questions**:
- Workflow design: `docs/ERLMCP_FLOW_GIT_WORKFLOW.md`
- Architecture: `docs/erlmcp-flow-architecture.md`
- Implementation: `docs/erlmcp-flow-implementation-plan.md`

---

## Summary

erlmcp-flow Git workflow ensures:

✅ **Quality First** - All commits pass comprehensive quality gates
✅ **History Preservation** - NEVER rebase, merge-only policy
✅ **OTP Compliance** - gen_server, supervision, let-it-crash patterns
✅ **Chicago TDD** - Real processes, no mocks, test-first development
✅ **Performance Validation** - Benchmarks on every PR
✅ **Clear Process** - Well-defined branch strategy, PR workflow, release process
✅ **Emergency Ready** - Hotfix and rollback procedures documented
✅ **Automation** - GitHub Actions CI/CD with comprehensive quality gates

**Status**: Production Ready for erlmcp-flow implementation.

---

**Version**: 1.0.0
**Last Updated**: 2026-02-02
