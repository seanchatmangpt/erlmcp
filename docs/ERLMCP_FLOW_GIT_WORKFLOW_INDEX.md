# erlmcp-flow Git Workflow - Complete Index

**Version**: 1.0.0
**Date**: 2026-02-02
**Status**: Production Ready

---

## Overview

This index provides complete navigation for the erlmcp-flow Git workflow system, covering branch strategy, merge policies, quality gates, CI/CD pipelines, PR templates, and issue management.

---

## Quick Navigation

| Need | Document | Time to Read |
|------|----------|--------------|
| **Start Now** | [Quick Start](#quick-start) | 5 min |
| **Understand Workflow** | [Summary](#summary) | 10 min |
| **Deep Dive** | [Complete Workflow](#complete-workflow) | 30 min |
| **CI/CD Setup** | [GitHub Actions](#github-actions) | 15 min |
| **Templates** | [PR & Issue Templates](#templates) | 10 min |

---

## Quick Start

**Document**: `docs/ERLMCP_FLOW_GIT_WORKFLOW_QUICKSTART.md`

**What**: 5-minute guide to get started with erlmcp-flow Git workflow

**Contents**:
1. Prerequisites check
2. 5-step workflow (branch → test → gates → push → PR)
3. Common tasks (update branch, fix CI, respond to review)
4. Troubleshooting guide
5. Cheat sheet

**Use When**:
- ✅ You're implementing a new feature
- ✅ You're new to the workflow
- ✅ You need a quick reference

**Read**: [ERLMCP_FLOW_GIT_WORKFLOW_QUICKSTART.md](ERLMCP_FLOW_GIT_WORKFLOW_QUICKSTART.md)

---

## Summary

**Document**: `docs/ERLMCP_FLOW_GIT_WORKFLOW_SUMMARY.md`

**What**: Comprehensive summary of the entire Git workflow

**Contents**:
1. Branch strategy (main, feature, release, hotfix)
2. Merge strategy (NEVER rebase, merge-only)
3. Quality gates (compilation, tests, coverage, dialyzer, xref)
4. PR workflow (create, review, merge)
5. Release process (version bump, changelog, artifacts)
6. Issue management (bug, feature, performance)
7. Emergency procedures (hotfix, rollback)
8. Common workflows with examples
9. File locations

**Use When**:
- ✅ You understand basics and need full reference
- ✅ You're setting up CI/CD
- ✅ You're reviewing the workflow design

**Read**: [ERLMCP_FLOW_GIT_WORKFLOW_SUMMARY.md](ERLMCP_FLOW_GIT_WORKFLOW_SUMMARY.md)

---

## Complete Workflow

**Document**: `docs/ERLMCP_FLOW_GIT_WORKFLOW.md`

**What**: Complete, detailed Git workflow design for erlmcp-flow

**Contents**:
1. **Branch Strategy** (15 min)
   - Main branch protection rules
   - Feature branch naming and lifecycle
   - Release branch process
   - Hotfix branch emergency procedures
   - Prohibited operations

2. **Merge Strategy** (10 min)
   - NO REBASE EVER policy
   - Merge types and commands
   - Merge commit message format
   - Conflict resolution (syntactic and semantic)

3. **Quality Gates in CI/CD** (15 min)
   - Gate overview (blocking vs warning)
   - Execution order (parallel where possible)
   - GitHub Actions integration
   - Matrix strategy (OTP 26, 27, 28)

4. **Release Process** (15 min)
   - Pre-release checklist
   - Version strategy (semantic versioning)
   - Release artifacts
   - GitHub release notes template

5. **Conflict Resolution** (10 min)
   - Conflict types (syntactic, semantic, performance, API)
   - Detection methods
   - Resolution strategies
   - Manual review gates

6. **PR Workflow** (15 min)
   - PR creation process
   - Review checklist (code quality, tests, performance, docs)
   - Merge requirements
   - Post-merge procedures

7. **Issue Management** (10 min)
   - Issue types (bug, feature, performance, documentation)
   - Issue lifecycle
   - Templates

8. **Emergency Procedures** (10 min)
   - Production hotfix process
   - Rollback procedure
   - Security incident response

**Use When**:
- ✅ You're designing the workflow
- ✅ You need to understand policy rationale
- ✅ You're training new team members
- ✅ You need complete reference

**Read**: [ERLMCP_FLOW_GIT_WORKFLOW.md](ERLMCP_FLOW_GIT_WORKFLOW.md)

---

## GitHub Actions

**Document**: `.github/workflows/erlmcp-flow-ci.yml`

**What**: GitHub Actions CI/CD pipeline for erlmcp-flow

**Jobs**:

### 1. erlmcp-flow-quality-gates
**Matrix**: OTP 26, 27, 28
**Timeout**: 30 minutes

**Gates** (in order):
1. ✅ **Compilation** (BLOCKING) - 0 errors
2. ✅ **Format Check** (BLOCKING) - Consistent formatting
3. ⚠️ **Xref** (WARNING) - 0 undefined functions
4. ✅ **Dialyzer** (BLOCKING) - 0 warnings
5. ✅ **EUnit Tests** (BLOCKING) - 100% pass rate
6. ✅ **Coverage** (BLOCKING) - ≥82%
7. ✅ **Common Test** (BLOCKING) - 100% pass rate
8. ⚠️ **Benchmarks** (WARNING) - No regression >10%

**Artifacts**:
- Coverage reports (30 days)
- Benchmark results (90 days)

### 2. erlmcp-flow-structure-check
**Purpose**: Verify application structure exists

**Checks**:
- `apps/erlmcp_flow/` directory exists
- `erlmcp_flow.app.src` present
- Source modules present
- Test modules present

### 3. erlmcp-flow-docs-check
**Purpose**: Verify documentation completeness

**Checks**:
- `docs/ERLMCP-FLOW-README.md` exists
- `docs/ERLMCP_FLOW_GIT_WORKFLOW.md` exists

### 4. erlmcp-flow-quality-gates-summary
**Purpose**: Overall status report

**Actions**:
- Print summary of all gates
- BLOCK merge if any gate fails

**Trigger Events**:
- Push to `main`, `claude/erlmcp-flow-*`, `release/v*`
- PR to `main` affecting erlmcp_flow

**Cache Strategy**:
- Cache rebar3 dependencies by OTP version
- Restore from cache for faster builds

**Use When**:
- ✅ Setting up CI/CD
- ✅ Debugging CI failures
- ✅ Understanding gate execution

**View**: [.github/workflows/erlmcp-flow-ci.yml](../.github/workflows/erlmcp-flow-ci.yml)

---

## Templates

### PR Template

**Document**: `.github/PULL_REQUEST_TEMPLATE_ERLMCP_FLOW.md`

**Sections**:
1. **Summary** - One-sentence description
2. **Type of Change** - Module, bug fix, feature, performance, etc.
3. **erlmcp-flow Component** - Registry, router, transport, etc.
4. **Implementation Phase** - Which phase (1-6)
5. **Changes Made** - Core implementation, tests, documentation
6. **Quality Gates** - Compilation, tests, coverage, dialyzer, xref
7. **Test Plan** - Unit, integration, manual, performance
8. **OTP Compliance** - gen_server, supervision, isolation
9. **Breaking Changes** - API, configuration, migration
10. **Architecture Impact** - Integration with erlmcp core/transports
11. **Documentation** - Code, architecture, CHANGELOG
12. **Reviewer Checklist** - Code quality, tests, performance, docs
13. **Quality Gate Results** - Local and CI validation

**Use When**:
- ✅ Creating PR for erlmcp-flow feature
- ✅ Need comprehensive PR checklist

**View**: [.github/PULL_REQUEST_TEMPLATE_ERLMCP_FLOW.md](../.github/PULL_REQUEST_TEMPLATE_ERLMCP_FLOW.md)

### Bug Report Template

**Document**: `.github/ISSUE_TEMPLATE/erlmcp-flow-bug.md`

**Sections**:
1. **Bug Description** - Component, clear description
2. **Environment** - Version, OTP, OS, architecture
3. **Steps to Reproduce** - Detailed reproduction steps
4. **Expected vs Actual Behavior**
5. **Error Messages / Logs** - Crash reports, logs
6. **Reproducibility** - Frequency, environment-specific
7. **Impact** - Severity, affected users
8. **Additional Context** - Related issues, workaround, debugging info
9. **Performance Impact** - CPU, memory, latency, throughput
10. **Quality Gate Impact** - Which gates fail

**Use When**:
- ✅ Reporting bug in erlmcp-flow
- ✅ Need structured bug report

**View**: [.github/ISSUE_TEMPLATE/erlmcp-flow-bug.md](../.github/ISSUE_TEMPLATE/erlmcp-flow-bug.md)

### Feature Request Template

**Document**: `.github/ISSUE_TEMPLATE/erlmcp-flow-feature.md`

**Sections**:
1. **Feature Summary** - One-sentence description
2. **Component** - Which erlmcp-flow component
3. **Problem Statement** - Current limitation, impact, use cases
4. **Proposed Solution** - High-level design, API design, implementation
5. **Alternatives Considered** - Other approaches and why not chosen
6. **Performance Impact** - Expected metrics
7. **Breaking Changes** - API changes, migration path
8. **Test Plan** - Unit, integration, performance tests
9. **Documentation Requirements** - Specs, architecture, examples
10. **Implementation Estimate** - Complexity, time, phase
11. **Success Criteria** - Functional, performance, documentation
12. **Risks and Mitigation** - Identified risks and mitigations
13. **Dependencies** - Depends on, blocks

**Use When**:
- ✅ Proposing new erlmcp-flow feature
- ✅ Need comprehensive feature planning

**View**: [.github/ISSUE_TEMPLATE/erlmcp-flow-feature.md](../.github/ISSUE_TEMPLATE/erlmcp-flow-feature.md)

### Performance Issue Template

**Document**: `.github/ISSUE_TEMPLATE/erlmcp-flow-performance.md`

**Sections**:
1. **Performance Issue Summary** - One-sentence description
2. **Component** - Which erlmcp-flow component
3. **Performance Metrics** - Current vs target performance table
4. **Environment** - Version, OTP, OS, hardware, load
5. **Benchmark Results** - Reproduction script, output, profiling
6. **Root Cause Analysis** - Suspected bottleneck, evidence
7. **Performance Regression** - Was it better before? When?
8. **Impact** - Severity, affected operations, user impact
9. **Proposed Optimization** - Approach, code changes, expected improvement
10. **Workarounds** - Temporary workaround
11. **Benchmarking Plan** - Test scenarios, success criteria

**Use When**:
- ✅ Reporting performance issue in erlmcp-flow
- ✅ Performance regression detected
- ✅ Need performance analysis

**View**: [.github/ISSUE_TEMPLATE/erlmcp-flow-performance.md](../.github/ISSUE_TEMPLATE/erlmcp-flow-performance.md)

---

## Architecture Documentation

### Main README

**Document**: `docs/ERLMCP-FLOW-README.md`

**Contents**:
- Executive summary
- Documentation index
- Architecture overview
- Key design decisions
- Implementation timeline
- Quality gates
- Quick examples
- Performance characteristics

**Use When**:
- ✅ First-time introduction to erlmcp-flow
- ✅ Need architectural overview

**Read**: [ERLMCP-FLOW-README.md](ERLMCP-FLOW-README.md)

### Other Architecture Docs

| Document | Contents | Use When |
|----------|----------|----------|
| `erlmcp-flow-architecture.md` | Complete architecture design | Deep dive into design |
| `erlmcp-flow-examples.md` | Code examples | Implementing features |
| `erlmcp-flow-diagrams.md` | Visual diagrams | Understanding flows |
| `erlmcp-flow-implementation-plan.md` | 48h implementation plan | Planning implementation |

---

## File Structure

```
/home/user/erlmcp/
├── docs/
│   ├── ERLMCP_FLOW_GIT_WORKFLOW_INDEX.md         ← This document
│   ├── ERLMCP_FLOW_GIT_WORKFLOW_QUICKSTART.md    ← 5-minute guide
│   ├── ERLMCP_FLOW_GIT_WORKFLOW_SUMMARY.md       ← Complete summary
│   ├── ERLMCP_FLOW_GIT_WORKFLOW.md               ← Full design
│   ├── ERLMCP-FLOW-README.md                     ← Main README
│   ├── erlmcp-flow-architecture.md               ← Architecture
│   ├── erlmcp-flow-examples.md                   ← Code examples
│   ├── erlmcp-flow-diagrams.md                   ← Diagrams
│   └── erlmcp-flow-implementation-plan.md        ← Implementation
│
├── .github/
│   ├── workflows/
│   │   └── erlmcp-flow-ci.yml                    ← CI/CD pipeline
│   │
│   ├── ISSUE_TEMPLATE/
│   │   ├── erlmcp-flow-bug.md                    ← Bug report
│   │   ├── erlmcp-flow-feature.md                ← Feature request
│   │   └── erlmcp-flow-performance.md            ← Performance issue
│   │
│   └── PULL_REQUEST_TEMPLATE_ERLMCP_FLOW.md      ← PR template
│
└── apps/
    └── erlmcp_flow/
        ├── src/                                  ← Source code
        ├── test/                                 ← Tests
        ├── bench/                                ← Benchmarks
        └── include/                              ← Headers
```

---

## Quick Reference Cards

### Branch Strategy

| Branch | Pattern | Purpose | Example |
|--------|---------|---------|---------|
| **Main** | `main` | Production-ready | `main` |
| **Feature** | `claude/erlmcp-flow-{feature}-{random}` | New feature | `claude/erlmcp-flow-registry-R9zub` |
| **Release** | `release/v{major}.{minor}.{patch}` | Release prep | `release/v1.0.0` |
| **Hotfix** | `hotfix/v{major}.{minor}.{patch+1}-{issue}` | Emergency fix | `hotfix/v1.0.1-memory-leak` |

### Quality Gates

| Gate | Blocking | Target | Command |
|------|----------|--------|---------|
| **Compilation** | ✅ YES | 0 errors | `TERM=dumb rebar3 compile` |
| **Format** | ✅ YES | Consistent | `rebar3 format --check` |
| **EUnit** | ✅ YES | 100% pass | `rebar3 eunit --app erlmcp_flow` |
| **Coverage** | ✅ YES | ≥82% | `rebar3 cover` |
| **Dialyzer** | ✅ YES | 0 warnings | `rebar3 dialyzer` |
| **CT** | ✅ YES | 100% pass | `rebar3 ct --dir apps/erlmcp_flow/test` |
| **Xref** | ⚠️ NO | 0 undefined | `rebar3 xref` |
| **Benchmarks** | ⚠️ NO | <10% regression | `rebar3 eunit --module erlmcp_flow_bench` |

### Prohibited Operations

| Operation | Why Prohibited | Alternative |
|-----------|----------------|-------------|
| `git rebase` | Rewrites history | `git merge --no-ff` |
| `git push --force` | Destroys commits | Never force push |
| `git commit --amend` (pushed) | Modifies shared history | New commit |
| `--no-verify` | Bypasses quality gates | Fix issues |
| `git reset --hard` (shared) | Loses work | `git revert` |

---

## Workflow Cheat Sheet

### Start New Feature

```bash
git checkout main
git pull origin main
git checkout -b claude/erlmcp-flow-{feature}-$(openssl rand -hex 2)
# Implement with tests (Chicago TDD)
make compile && rebar3 eunit --app erlmcp_flow && rebar3 cover && rebar3 dialyzer
git commit -m "feat(erlmcp-flow): Description"
git push origin claude/erlmcp-flow-{feature}-{random}
gh pr create --web
```

### Update Feature Branch

```bash
git checkout claude/erlmcp-flow-{feature}-{random}
git fetch origin
git merge origin/main --no-ff
# Resolve conflicts if any
git push origin claude/erlmcp-flow-{feature}-{random}
```

### Merge PR

```bash
gh pr merge --squash --delete-branch
```

---

## Learning Path

### New to erlmcp-flow

1. **Read** (5 min): [Quick Start](ERLMCP_FLOW_GIT_WORKFLOW_QUICKSTART.md)
2. **Practice** (30 min): Create test branch, run quality gates
3. **Read** (10 min): [Summary](ERLMCP_FLOW_GIT_WORKFLOW_SUMMARY.md)
4. **Implement** (4-8 hours): Your first feature following the workflow

### Experienced Developer

1. **Skim** (5 min): [Summary](ERLMCP_FLOW_GIT_WORKFLOW_SUMMARY.md)
2. **Reference** (as needed): [Complete Workflow](ERLMCP_FLOW_GIT_WORKFLOW.md)
3. **Use** (ongoing): [PR Template](../.github/PULL_REQUEST_TEMPLATE_ERLMCP_FLOW.md)

### Setting up CI/CD

1. **Read** (15 min): [GitHub Actions Workflow](../.github/workflows/erlmcp-flow-ci.yml)
2. **Review** (15 min): Quality gates in [Complete Workflow](ERLMCP_FLOW_GIT_WORKFLOW.md#3-quality-gates-in-cicd)
3. **Configure** (30 min): Branch protection rules on GitHub
4. **Test** (30 min): Create test PR and verify all gates run

---

## Support & Contact

**Design**: erlang-github-ops agent
**Date**: 2026-02-02
**Version**: 1.0.0
**Status**: Production Ready

**For Questions**:
- Workflow: This index
- Quick Start: [ERLMCP_FLOW_GIT_WORKFLOW_QUICKSTART.md](ERLMCP_FLOW_GIT_WORKFLOW_QUICKSTART.md)
- Complete Reference: [ERLMCP_FLOW_GIT_WORKFLOW.md](ERLMCP_FLOW_GIT_WORKFLOW.md)
- Architecture: [erlmcp-flow-architecture.md](erlmcp-flow-architecture.md)

---

## Summary

erlmcp-flow Git workflow system includes:

✅ **4 Documentation Levels**:
- Quick Start (5 min)
- Summary (10 min)
- Complete Workflow (30 min)
- This Index (navigation)

✅ **Complete CI/CD Pipeline**:
- GitHub Actions with 8 quality gates
- Matrix testing (OTP 26, 27, 28)
- Artifact archival (coverage, benchmarks)

✅ **Comprehensive Templates**:
- PR template (erlmcp-flow specific)
- Bug report template
- Feature request template
- Performance issue template

✅ **Clear Processes**:
- Branch strategy (feature, release, hotfix)
- Merge strategy (NEVER rebase)
- Quality gates (compilation, tests, coverage, dialyzer)
- Release process (semantic versioning)
- Emergency procedures (hotfix, rollback)

**Status**: Production Ready for erlmcp-flow implementation.

---

**Version**: 1.0.0
**Last Updated**: 2026-02-02
