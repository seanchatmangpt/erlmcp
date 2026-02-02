# erlmcp-flow Git Workflow Design

**Version**: 1.0.0
**Date**: 2026-02-02
**Status**: Production Ready

---

## Executive Summary

This document defines the Git workflow for erlmcp-flow, a high-performance agent coordination transport layer. The workflow enforces strict quality gates, preserves Git history through merge-only operations, and ensures OTP compliance at every stage.

**Core Principles**:
- **NEVER REBASE** - Merge only, preserve complete history
- **NEVER --no-verify** - All quality gates must pass
- **Quality First** - Compilation, tests, coverage, Dialyzer, Xref must pass
- **OTP Compliance** - gen_server, supervision, isolation, let-it-crash
- **Chicago TDD** - Real processes, no mocks, test-first development

---

## Table of Contents

1. [Branch Strategy](#1-branch-strategy)
2. [Merge Strategy](#2-merge-strategy)
3. [Quality Gates in CI/CD](#3-quality-gates-in-cicd)
4. [Release Process](#4-release-process)
5. [Conflict Resolution](#5-conflict-resolution)
6. [PR Workflow](#6-pr-workflow)
7. [Issue Management](#7-issue-management)
8. [Emergency Procedures](#8-emergency-procedures)

---

## 1. Branch Strategy

### 1.1 Main Branch

**Branch**: `main`
**Protection**: Full branch protection enabled
**Purpose**: Production-ready code only

**Protection Rules**:
- Require PR approval (2+ reviewers)
- Require status checks to pass:
  - ✅ Compilation (0 errors)
  - ✅ EUnit tests (100% pass rate)
  - ✅ Coverage (≥82%)
  - ✅ Dialyzer (0 warnings)
  - ✅ Xref (0 undefined functions)
  - ✅ Benchmarks (no regression >10%)
- Require up-to-date branches
- Dismiss stale PR approvals
- Require signed commits (optional)
- No force push
- No deletion

### 1.2 Feature Branches

**Pattern**: `claude/erlmcp-flow-{feature-name}-{random}`

**Examples**:
- `claude/erlmcp-flow-registry-R9zub`
- `claude/erlmcp-flow-router-K3xmA`
- `claude/erlmcp-flow-transport-bridge-W7qpL`

**Lifecycle**:
1. Branch from `main`
2. Implement feature with tests (Chicago TDD)
3. Quality gates pass locally
4. Create PR
5. CI/CD validates quality gates
6. Code review (2+ approvals)
7. Merge to `main` (NEVER rebase)
8. Delete feature branch

**Naming Convention**:
- Prefix: `claude/` (automated agent workflow)
- Project: `erlmcp-flow`
- Feature: Descriptive name (registry, router, transport, etc.)
- Suffix: 5-character random ID (prevents conflicts)

### 1.3 Release Branches

**Pattern**: `release/v{major}.{minor}.{patch}`

**Examples**:
- `release/v1.0.0`
- `release/v1.1.0`
- `release/v2.0.0`

**Purpose**: Stabilization and release preparation

**Workflow**:
1. Branch from `main` when feature complete
2. Version bump in `erlmcp_flow.app.src`
3. Update CHANGELOG.md
4. Run full quality gates + benchmarks
5. Create GitHub release with tag
6. Merge back to `main` (if hotfixes applied)

### 1.4 Hotfix Branches

**Pattern**: `hotfix/v{major}.{minor}.{patch+1}-{issue}`

**Examples**:
- `hotfix/v1.0.1-memory-leak`
- `hotfix/v1.0.2-crash-on-empty-queue`

**Workflow**:
1. Branch from `main` or latest release tag
2. Apply minimal fix with tests
3. Quality gates pass
4. Create PR with urgent label
5. Fast-track review (1 reviewer)
6. Merge to `main`
7. Create patch release

### 1.5 Prohibited Branch Operations

**NEVER**:
- ❌ `git rebase` - Rewrites history, breaks traceability
- ❌ `git push --force` - Destroys commits
- ❌ `git commit --amend` on pushed commits
- ❌ `git reset --hard` on shared branches
- ❌ `git cherry-pick` across major versions

**ALWAYS**:
- ✅ `git merge --no-ff` - Preserves merge commits
- ✅ `git merge main` - Keep feature branch up-to-date
- ✅ Linear history in `main` via squash commits (optional)

---

## 2. Merge Strategy

### 2.1 Merge Policy: NO REBASE EVER

**Rule**: All merges must use `git merge --no-ff` to preserve history.

**Rationale**:
- Complete audit trail for compliance
- Easy rollback via revert
- No lost commits from force pushes
- Conflict resolution is explicit

**Commands**:
```bash
# Update feature branch with main changes
git checkout claude/erlmcp-flow-registry-R9zub
git fetch origin
git merge origin/main --no-ff -m "Merge main into registry branch"

# Resolve conflicts if any
git add .
git commit -m "Resolve merge conflicts from main"

# Push (without --force)
git push origin claude/erlmcp-flow-registry-R9zub
```

### 2.2 Merge Types

| Merge Type | Use Case | Command |
|------------|----------|---------|
| **Feature → Main** | PR merge via GitHub UI | Auto-merge with squash (optional) |
| **Main → Feature** | Sync feature branch | `git merge origin/main --no-ff` |
| **Release → Main** | Release finalization | `git merge release/v1.0.0 --no-ff` |
| **Hotfix → Main** | Emergency fix | `git merge hotfix/v1.0.1-crash --no-ff` |

### 2.3 Merge Commit Messages

**Format**:
```
Merge {source-branch} into {target-branch}

Summary:
- Feature: {description}
- Tests: {count} EUnit + {count} CT
- Coverage: {percentage}%
- Dialyzer: 0 warnings
- Xref: 0 undefined functions

Quality Gates: ✅ ALL PASSED

https://claude.ai/code/session_{session_id}
```

**Example**:
```
Merge claude/erlmcp-flow-registry-R9zub into main

Summary:
- Feature: O(log N) agent registry with gproc
- Tests: 42 EUnit + 8 CT
- Coverage: 89%
- Dialyzer: 0 warnings
- Xref: 0 undefined functions

Quality Gates: ✅ ALL PASSED

Benchmarks:
- Agent lookup: p50=8μs, p95=42μs (target: <50μs)
- Registry throughput: 562K lookups/sec (target: >500K)

https://claude.ai/code/session_015fcuk5THgv963tNjruyYDf
```

### 2.4 Conflict Resolution

**Semantic Merge Conflicts** (code compiles but behavior changes):

1. **Detection**: Manual review required, CI tests fail
2. **Resolution**:
   - Review both changes side-by-side
   - Write integration test covering conflict scenario
   - Choose correct resolution based on test results
   - Document decision in merge commit message
3. **Validation**: All quality gates must pass

**Syntactic Merge Conflicts** (Git cannot auto-merge):

```bash
# Step 1: Identify conflict
git merge origin/main
# CONFLICT (content): Merge conflict in apps/erlmcp_flow/src/erlmcp_flow_registry.erl

# Step 2: Manual resolution
vim apps/erlmcp_flow/src/erlmcp_flow_registry.erl
# Remove <<<<<<< HEAD, =======, >>>>>>> markers
# Keep both changes if complementary, or choose one

# Step 3: Test resolution
make compile
rebar3 eunit --app erlmcp_flow
rebar3 dialyzer

# Step 4: Commit resolution
git add apps/erlmcp_flow/src/erlmcp_flow_registry.erl
git commit -m "Resolve merge conflict in erlmcp_flow_registry

Both changes were complementary:
- Added handle_call/3 for lookup_agent (main)
- Added handle_call/3 for register_agent (feature branch)

Tests: All pass
Coverage: 89% (maintained)"

# Step 5: Push
git push origin claude/erlmcp-flow-registry-R9zub
```

---

## 3. Quality Gates in CI/CD

### 3.1 Gate Overview

| Gate | Blocking | Target | Validation |
|------|----------|--------|------------|
| **Compilation** | ✅ YES | 0 errors | `TERM=dumb rebar3 compile` |
| **EUnit Tests** | ✅ YES | 100% pass | `rebar3 eunit --app erlmcp_flow` |
| **CT Tests** | ✅ YES | 100% pass | `rebar3 ct --dir apps/erlmcp_flow/test` |
| **Coverage** | ✅ YES | ≥82% | `rebar3 cover` |
| **Dialyzer** | ✅ YES | 0 warnings | `rebar3 dialyzer` |
| **Xref** | ⚠️ NO | 0 undefined | `rebar3 xref` (warning only) |
| **Benchmarks** | ⚠️ NO | <10% regression | `rebar3 eunit --module erlmcp_flow_bench` |
| **Format** | ✅ YES | Consistent | `rebar3 format --check` |

### 3.2 Gate Execution Order

**Parallel Execution** (where possible):

```
┌─────────────────────────────────────────────────┐
│ GATE 1: Compilation (BLOCKING)                  │
│ └─ TERM=dumb rebar3 compile                     │
└─────────────────────────────────────────────────┘
                    ↓ (if pass)
┌─────────────────────────────────────────────────┐
│ GATE 2-5: Parallel Execution                     │
│ ├─ EUnit Tests (BLOCKING)                       │
│ ├─ Dialyzer (BLOCKING)                          │
│ ├─ Xref (WARNING)                               │
│ └─ Format Check (BLOCKING)                      │
└─────────────────────────────────────────────────┘
                    ↓ (if pass)
┌─────────────────────────────────────────────────┐
│ GATE 6: Coverage (BLOCKING)                      │
│ └─ rebar3 cover (≥82%)                          │
└─────────────────────────────────────────────────┘
                    ↓ (if pass)
┌─────────────────────────────────────────────────┐
│ GATE 7: Common Test (BLOCKING)                   │
│ └─ rebar3 ct --dir apps/erlmcp_flow/test        │
└─────────────────────────────────────────────────┘
                    ↓ (if pass)
┌─────────────────────────────────────────────────┐
│ GATE 8: Benchmarks (WARNING)                     │
│ └─ Performance regression check                  │
└─────────────────────────────────────────────────┘
                    ↓ (if all pass)
┌─────────────────────────────────────────────────┐
│ ✅ ALL GATES PASSED - MERGE ALLOWED             │
└─────────────────────────────────────────────────┘
```

### 3.3 GitHub Actions Integration

See `.github/workflows/erlmcp-flow-ci.yml` for full implementation.

**Trigger Events**:
```yaml
on:
  pull_request:
    paths:
      - 'apps/erlmcp_flow/**'
      - 'docs/erlmcp-flow-*.md'
      - '.github/workflows/erlmcp-flow-ci.yml'
  push:
    branches:
      - main
      - 'claude/erlmcp-flow-*'
      - 'release/v*'
```

**Matrix Strategy**:
```yaml
strategy:
  matrix:
    otp_version: [26, 27, 28]
    os: [ubuntu-22.04]
```

---

## 4. Release Process

### 4.1 Release Checklist

**Pre-Release** (on `release/v1.0.0` branch):

- [ ] Version bump in `apps/erlmcp_flow/src/erlmcp_flow.app.src`
- [ ] CHANGELOG.md updated with all changes since last release
- [ ] All quality gates pass (compile, test, coverage, dialyzer, xref)
- [ ] Benchmarks pass (no regression >10%)
- [ ] Documentation updated (README.md, architecture docs)
- [ ] Migration guide written (if breaking changes)
- [ ] Performance report generated
- [ ] Security scan passed (no vulnerabilities)

**Release** (GitHub UI):

- [ ] Create release branch: `git checkout -b release/v1.0.0 main`
- [ ] Apply pre-release checklist
- [ ] Create PR: `release/v1.0.0` → `main`
- [ ] CI/CD validates all gates
- [ ] Code review (2+ approvals)
- [ ] Merge PR to `main`
- [ ] Create GitHub release with tag `v1.0.0`
- [ ] Attach artifacts: benchmarks, coverage reports

**Post-Release**:

- [ ] Verify release artifact published
- [ ] Monitor production metrics for 24h
- [ ] Update project board/roadmap
- [ ] Announce release (CHANGELOG, docs, community)

### 4.2 Versioning Strategy

**Semantic Versioning**: `MAJOR.MINOR.PATCH`

| Version | Trigger | Example |
|---------|---------|---------|
| **MAJOR** | Breaking API changes | v1.0.0 → v2.0.0 (gproc replaced with custom registry) |
| **MINOR** | New features (backward compatible) | v1.0.0 → v1.1.0 (add gossip protocol) |
| **PATCH** | Bug fixes (backward compatible) | v1.0.0 → v1.0.1 (fix memory leak in router) |

### 4.3 Release Artifacts

**Generated Artifacts**:

1. **Compiled Release**: `_build/prod/rel/erlmcp_flow-{version}.tar.gz`
2. **Coverage Report**: `_build/test/cover/index.html` (zipped)
3. **Benchmark Report**: `bench/results/v{version}-benchmark-report.json`
4. **Dialyzer PLT**: `_build/default/erlmcp_flow.plt`
5. **Documentation**: `doc/` (generated with edoc)
6. **Checksums**: `SHA256SUMS` for all artifacts

**GitHub Release Notes Template**:

```markdown
# erlmcp-flow v1.0.0

## Summary
Agent coordination transport layer with O(log N) routing and 60+ agent support.

## Features
- ✅ O(log N) agent registry via gproc
- ✅ Direct, broadcast, and gossip messaging patterns
- ✅ Transport integration (stdio, TCP, HTTP)
- ✅ Token bucket flow control
- ✅ OTP-compliant supervision tree

## Performance
- Agent lookup: p50=8μs, p95=42μs, p99=95μs
- Direct messaging: 105K msg/sec
- Broadcast (60 agents): avg 2.1ms
- Registry throughput: 562K lookups/sec

## Quality Gates
- ✅ Compilation: 0 errors
- ✅ Tests: 100% pass rate (50 EUnit + 12 CT)
- ✅ Coverage: 89% (target: ≥82%)
- ✅ Dialyzer: 0 warnings
- ✅ Xref: 0 undefined functions
- ✅ Benchmarks: No regressions

## Installation
```erlang
{deps, [
    {erlmcp_flow, "1.0.0"}
]}.
```

## Documentation
- [Architecture](docs/erlmcp-flow-architecture.md)
- [Examples](docs/erlmcp-flow-examples.md)
- [API Reference](apps/erlmcp_flow/doc/index.html)

## Breaking Changes
None (initial release)

## Contributors
@seanchatmangpt @erlang-otp-developer @erlang-transport-builder

## Checksums
See `SHA256SUMS` attachment.

## Support
- Issues: https://github.com/seanchatmangpt/erlmcp/issues
- Docs: https://erlmcp.readthedocs.io/
```

---

## 5. Conflict Resolution

### 5.1 Conflict Types

| Conflict Type | Detection | Resolution Strategy |
|---------------|-----------|---------------------|
| **Syntactic** | Git merge fails | Manual resolution, test, commit |
| **Semantic** | Tests fail post-merge | Integration test, choose correct logic |
| **Performance** | Benchmark regression | Profile, optimize, re-test |
| **API** | Breaking change | Deprecation period, migration guide |

### 5.2 Semantic Merge Detection

**Scenario**: Both branches modify same function, Git auto-merges, but logic is broken.

**Detection**:
```bash
git merge origin/main --no-ff
# Git succeeds (no conflicts)

make compile
# Compiles successfully

rebar3 eunit --app erlmcp_flow
# Tests FAIL (semantic conflict)
```

**Resolution**:
1. **Identify conflicting commits**:
   ```bash
   git log --oneline --graph origin/main..HEAD
   git log --oneline --graph HEAD..origin/main
   ```

2. **Analyze both changes**:
   ```bash
   git show <commit-1>
   git show <commit-2>
   ```

3. **Write integration test**:
   ```erlang
   %% apps/erlmcp_flow/test/erlmcp_flow_registry_tests.erl

   merge_conflict_regression_test() ->
       %% This test covers the semantic conflict between:
       %% - Commit abc123: Added async registration
       %% - Commit def456: Changed registration to synchronous

       %% Resolution: Use synchronous registration for consistency
       {ok, Pid} = erlmcp_flow_registry:start_link(),

       %% Test synchronous behavior
       ok = erlmcp_flow_registry:register_agent(<<"agent-1">>, self(), #{}),

       %% Verify agent is immediately available (not eventual)
       {ok, Pid1} = erlmcp_flow_registry:lookup_agent(<<"agent-1">>),
       ?assertEqual(self(), Pid1).
   ```

4. **Apply correct resolution**:
   ```bash
   # Revert to working state
   git checkout origin/main -- apps/erlmcp_flow/src/erlmcp_flow_registry.erl

   # Re-apply feature branch changes
   # (manual merge with correct logic)

   # Test
   rebar3 eunit --module erlmcp_flow_registry_tests

   # Commit resolution
   git commit -m "Resolve semantic merge conflict: synchronous registration

   Analysis:
   - Branch A: Added async registration for performance
   - Branch B: Changed to synchronous for consistency
   - Resolution: Keep synchronous (consistency > performance for registry)

   Test: merge_conflict_regression_test() passes
   Coverage: 89% (maintained)"
   ```

### 5.3 Manual Review Gates

**When Required**:
- Semantic conflicts detected
- Breaking API changes
- Performance regression >10%
- Security-sensitive code

**Process**:
1. Assign senior reviewer
2. Request architecture review
3. Schedule sync discussion if needed
4. Document decision in PR comments
5. Update documentation

---

## 6. PR Workflow

### 6.1 PR Creation

**Trigger**: Feature branch ready for review

**Steps**:
1. Ensure all local quality gates pass:
   ```bash
   cd /home/user/erlmcp
   make check  # compile + test + dialyzer + xref
   make coverage  # ≥82%
   ```

2. Push feature branch:
   ```bash
   git push origin claude/erlmcp-flow-registry-R9zub
   ```

3. Create PR via GitHub CLI:
   ```bash
   gh pr create \
     --title "feat(erlmcp-flow): Add O(log N) agent registry with gproc" \
     --body "$(cat <<'EOF'
   ## Summary
   Implements agent registry with O(log N) lookup performance using gproc.

   ## Features
   - Agent registration/deregistration
   - Type-based and capability-based indexing
   - Automatic cleanup on agent crash
   - Load counter support

   ## Quality Gates
   ✅ Compilation: 0 errors
   ✅ Tests: 42 EUnit, 8 CT (100% pass rate)
   ✅ Coverage: 89% (target: ≥82%)
   ✅ Dialyzer: 0 warnings
   ✅ Xref: 0 undefined functions

   ## Benchmarks
   - Agent lookup: p50=8μs, p95=42μs (target: <50μs)
   - Registry throughput: 562K lookups/sec (target: >500K)

   ## Test Plan
   - [x] Unit tests (Chicago TDD, real processes)
   - [x] Integration tests with router
   - [x] Performance benchmarks
   - [x] Crash recovery tests

   ## Breaking Changes
   None

   ## Documentation
   - [x] Code documented with specs
   - [x] Architecture doc updated
   - [x] Examples added

   https://claude.ai/code/session_015fcuk5THgv963tNjruyYDf
   EOF
   )" \
     --base main \
     --head claude/erlmcp-flow-registry-R9zub
   ```

### 6.2 PR Review Process

**Reviewers**: 2+ (erlang-otp-developer, code-reviewer)

**Review Checklist**:

**Code Quality**:
- [ ] OTP patterns: gen_server, supervision, let-it-crash
- [ ] No mocks (Chicago TDD with real processes)
- [ ] Specs and types on all public functions
- [ ] Proper error handling (crash vs return error)
- [ ] No hardcoded values (use application env)

**Tests**:
- [ ] Test coverage ≥82%
- [ ] Tests are deterministic (no sleeps, race conditions)
- [ ] Edge cases covered
- [ ] Integration tests with other erlmcp_flow modules

**Performance**:
- [ ] Benchmarks meet targets
- [ ] No obvious performance anti-patterns
- [ ] Memory leak checked (long-running test)

**Documentation**:
- [ ] All public functions documented
- [ ] Architecture docs updated if needed
- [ ] CHANGELOG.md entry added

**CI/CD**:
- [ ] All quality gates passed
- [ ] No new warnings introduced

### 6.3 PR Merge

**Requirements**:
- ✅ 2+ approvals
- ✅ All CI checks passed
- ✅ Branch up-to-date with main
- ✅ No merge conflicts

**Merge Command** (via GitHub UI or CLI):
```bash
# Option 1: Squash and merge (recommended for feature branches)
gh pr merge --squash --delete-branch

# Option 2: Merge commit (preserves all commits)
gh pr merge --merge --delete-branch

# NEVER use: gh pr merge --rebase (prohibited)
```

**Post-Merge**:
- GitHub Action auto-deletes feature branch
- CI/CD runs on main to validate merge
- Metrics updated (cycle time, throughput)

---

## 7. Issue Management

### 7.1 Issue Types

| Type | Label | Template | Priority |
|------|-------|----------|----------|
| **Bug** | `bug` | `.github/ISSUE_TEMPLATE/erlmcp-flow-bug.md` | High |
| **Feature** | `enhancement` | `.github/ISSUE_TEMPLATE/erlmcp-flow-feature.md` | Medium |
| **Performance** | `performance` | `.github/ISSUE_TEMPLATE/erlmcp-flow-performance.md` | High |
| **Documentation** | `documentation` | `.github/ISSUE_TEMPLATE/erlmcp-flow-docs.md` | Low |

### 7.2 Issue Lifecycle

```
┌──────────────┐
│ Issue Opened │
└──────────────┘
       ↓
┌──────────────┐
│ Triage       │ (Label, assign, prioritize)
└──────────────┘
       ↓
┌──────────────┐
│ In Progress  │ (Feature branch created)
└──────────────┘
       ↓
┌──────────────┐
│ PR Created   │ (Link to issue)
└──────────────┘
       ↓
┌──────────────┐
│ PR Merged    │ (Issue auto-closed)
└──────────────┘
       ↓
┌──────────────┐
│ Verified     │ (QA validation)
└──────────────┘
```

### 7.3 Issue Templates

See `.github/ISSUE_TEMPLATE/erlmcp-flow-*.md` for full templates.

---

## 8. Emergency Procedures

### 8.1 Production Hotfix

**Scenario**: Critical bug in production (v1.0.0)

**Process**:
1. **Create hotfix branch**:
   ```bash
   git checkout -b hotfix/v1.0.1-memory-leak main
   ```

2. **Apply minimal fix** with test:
   ```erlang
   %% Fix memory leak in erlmcp_flow_router.erl
   -spec cleanup_stale_queues() -> ok.
   cleanup_stale_queues() ->
       %% Add periodic cleanup (was missing)
       erlang:send_after(60000, self(), cleanup),
       ok.
   ```

3. **Quality gates**:
   ```bash
   make compile
   rebar3 eunit --module erlmcp_flow_router_tests
   rebar3 dialyzer
   ```

4. **Create PR** with `urgent` label:
   ```bash
   gh pr create \
     --title "hotfix(v1.0.1): Fix memory leak in router queue cleanup" \
     --label urgent \
     --assignee @erlang-otp-developer
   ```

5. **Fast-track review** (1 reviewer, <2h)

6. **Merge** and create patch release:
   ```bash
   gh pr merge --squash
   git tag v1.0.1
   git push origin v1.0.1
   gh release create v1.0.1 --notes "Hotfix: Memory leak in router"
   ```

### 8.2 Rollback Procedure

**Scenario**: New release (v1.1.0) causes production issues

**Process**:
1. **Identify last known good version**: v1.0.0

2. **Revert merge commit**:
   ```bash
   git revert -m 1 <merge-commit-sha>
   git push origin main
   ```

3. **Deploy previous release**:
   ```bash
   # Redeploy v1.0.0 artifacts
   kubectl rollout undo deployment/erlmcp-flow
   ```

4. **Create incident report**:
   - Root cause analysis
   - Timeline
   - Action items to prevent recurrence

### 8.3 Security Incident

**Scenario**: Security vulnerability discovered

**Process**:
1. **Private disclosure** (via security@project.com)
2. **Create private branch** (not on GitHub)
3. **Apply fix** and test extensively
4. **Coordinate disclosure** with security team
5. **Release patch** with CVE details
6. **Public announcement** after users have time to upgrade

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

**Next Steps**:
1. Review GitHub Actions workflow: `.github/workflows/erlmcp-flow-ci.yml`
2. Review PR template: `.github/PULL_REQUEST_TEMPLATE_ERLMCP_FLOW.md`
3. Review issue templates: `.github/ISSUE_TEMPLATE/erlmcp-flow-*.md`

---

**Version**: 1.0.0
**Status**: Production Ready
**Last Updated**: 2026-02-02
