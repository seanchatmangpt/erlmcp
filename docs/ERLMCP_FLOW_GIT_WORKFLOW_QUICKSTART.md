# erlmcp-flow Git Workflow Quick Start

**Version**: 1.0.0
**Date**: 2026-02-02

---

## For Developers: Get Started in 5 Minutes

This quick start guide gets you productive with the erlmcp-flow Git workflow immediately.

---

## Prerequisites

```bash
# 1. Erlang/OTP 26+ installed
erl -eval 'erlang:display(erlang:system_info(otp_release)), halt().' -noshell

# 2. rebar3 installed
rebar3 --version

# 3. GitHub CLI installed
gh --version

# 4. Repository cloned
git clone https://github.com/seanchatmangpt/erlmcp.git
cd erlmcp
```

---

## Workflow in 5 Steps

### Step 1: Create Feature Branch

```bash
# Always branch from main
git checkout main
git pull origin main

# Create feature branch (use random suffix)
git checkout -b claude/erlmcp-flow-registry-$(openssl rand -hex 2)
```

**Branch naming**: `claude/erlmcp-flow-{feature}-{random}`

### Step 2: Implement with Tests (Chicago TDD)

**Test-First Development**:

```erlang
%% 1. Write test first: apps/erlmcp_flow/test/erlmcp_flow_registry_tests.erl

-module(erlmcp_flow_registry_tests).
-include_lib("eunit/include/eunit.hrl").

register_agent_test() ->
    %% Test before implementation exists
    {ok, Pid} = erlmcp_flow_registry:start_link(),
    ok = erlmcp_flow_registry:register_agent(<<"agent-1">>, self(), #{}),
    {ok, AgentPid} = erlmcp_flow_registry:lookup_agent(<<"agent-1">>),
    ?assertEqual(self(), AgentPid).
```

**Then Implement**:

```erlang
%% 2. Implement to make test pass: apps/erlmcp_flow/src/erlmcp_flow_registry.erl

-module(erlmcp_flow_registry).
-behaviour(gen_server).

-export([start_link/0, register_agent/3, lookup_agent/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

%% Implementation
register_agent(AgentId, Pid, Config) ->
    gen_server:call(?MODULE, {register, AgentId, Pid, Config}).

lookup_agent(AgentId) ->
    case gproc:where({n, l, {flow_agent, AgentId}}) of
        undefined -> {error, not_found};
        Pid -> {ok, Pid}
    end.
```

### Step 3: Quality Gates (Local)

```bash
# Run all quality gates locally BEFORE pushing
cd /home/user/erlmcp

# Gate 1: Compilation
TERM=dumb rebar3 compile
# Expected: 0 errors

# Gate 2: Tests
rebar3 eunit --app erlmcp_flow
# Expected: All pass

# Gate 3: Coverage
rebar3 cover
# Expected: ≥82%

# Gate 4: Dialyzer
cd apps/erlmcp_flow
rebar3 dialyzer
# Expected: 0 warnings

# Gate 5: Xref
rebar3 xref
# Expected: 0 undefined functions

# Gate 6: Format
rebar3 format --check
# Expected: Already formatted
```

**If any gate fails**: Fix before proceeding.

### Step 4: Commit and Push

```bash
# Stage changes
git add apps/erlmcp_flow/

# Commit with descriptive message
git commit -m "feat(erlmcp-flow): Add agent registry with O(log N) lookup

- Implements register_agent/3 and lookup_agent/1
- Uses gproc for O(log N) performance
- Automatic cleanup on agent crash
- Type and capability indexing

Tests: 42 EUnit (100% pass)
Coverage: 89% (target: ≥82%)
Dialyzer: 0 warnings
Xref: 0 undefined functions

https://claude.ai/code/session_015fcuk5THgv963tNjruyYDf"

# Push to GitHub
git push origin claude/erlmcp-flow-registry-R9zub
```

### Step 5: Create Pull Request

```bash
# Create PR using GitHub CLI
gh pr create \
  --title "feat(erlmcp-flow): Add O(log N) agent registry with gproc" \
  --body "## Summary
Implements agent registry with O(log N) lookup performance using gproc.

## Quality Gates
✅ Compilation: 0 errors
✅ Tests: 42 EUnit (100% pass)
✅ Coverage: 89% (target: ≥82%)
✅ Dialyzer: 0 warnings
✅ Xref: 0 undefined functions

## Benchmarks
- Agent lookup: p50=8μs, p95=42μs (target: <50μs)
- Registry throughput: 562K lookups/sec (target: >500K)

## Documentation
- [x] Function specs complete
- [x] Module documentation added
- [x] Architecture doc updated

https://claude.ai/code/session_015fcuk5THgv963tNjruyYDf" \
  --base main

# OR use template
gh pr create --web
# (Opens browser with pre-filled template)
```

**CI/CD will now validate**:
- All quality gates on OTP 26, 27, 28
- Benchmarks (if applicable)
- Structure check
- Documentation check

---

## Common Tasks

### Update Feature Branch from Main

```bash
# Keep your branch up-to-date (MERGE, never rebase)
git checkout claude/erlmcp-flow-registry-R9zub
git fetch origin
git merge origin/main --no-ff

# If conflicts, resolve manually
git add .
git commit -m "Merge main into registry branch"

# Push
git push origin claude/erlmcp-flow-registry-R9zub
```

### Fix Failing CI

```bash
# 1. Check CI logs
gh pr checks

# 2. Identify failure (e.g., coverage below 82%)

# 3. Add missing tests
vim apps/erlmcp_flow/test/erlmcp_flow_registry_tests.erl

# 4. Run locally
rebar3 eunit --app erlmcp_flow
rebar3 cover

# 5. Commit and push
git add .
git commit -m "test: Add edge case tests for registry

Increases coverage from 78% to 89%"
git push origin claude/erlmcp-flow-registry-R9zub

# 6. CI will re-run automatically
```

### Respond to PR Review

```bash
# 1. Read review comments
gh pr view

# 2. Make requested changes
vim apps/erlmcp_flow/src/erlmcp_flow_registry.erl

# 3. Test changes
make compile
rebar3 eunit --app erlmcp_flow

# 4. Commit
git add .
git commit -m "refactor: Apply review feedback

- Extract register_with_gproc/2 function
- Add spec for private functions
- Improve error messages"

# 5. Push
git push origin claude/erlmcp-flow-registry-R9zub

# 6. Request re-review
gh pr review --approve  # (if you're a reviewer)
```

### Merge PR

```bash
# After 2+ approvals and all checks pass
gh pr merge --squash --delete-branch

# PR is now merged to main, feature branch deleted
```

---

## Prohibited Operations

### NEVER DO THESE

```bash
# ❌ NEVER rebase
git rebase origin/main  # PROHIBITED

# ❌ NEVER force push
git push --force  # PROHIBITED

# ❌ NEVER amend pushed commits
git commit --amend  # PROHIBITED (if already pushed)

# ❌ NEVER skip hooks
git commit --no-verify  # PROHIBITED

# ❌ NEVER reset shared branches
git reset --hard  # PROHIBITED (on shared branches)
```

**Why?** These operations destroy Git history, break traceability, and can cause data loss.

### ALWAYS DO THESE

```bash
# ✅ ALWAYS merge with --no-ff
git merge origin/main --no-ff

# ✅ ALWAYS run quality gates locally
make compile && rebar3 eunit && rebar3 cover && rebar3 dialyzer

# ✅ ALWAYS write tests first (Chicago TDD)
# Test → Implement → Refactor

# ✅ ALWAYS update documentation
# Code + Tests + Docs = Complete

# ✅ ALWAYS use meaningful commit messages
git commit -m "feat(erlmcp-flow): Clear description

- What changed
- Why it changed
- Impact

Tests: X EUnit
Coverage: Y%
Session: https://claude.ai/code/session_..."
```

---

## Troubleshooting

### Compilation Fails

```bash
# Check error message
TERM=dumb rebar3 compile

# Common issues:
# 1. Missing module: Create apps/erlmcp_flow/src/module.erl
# 2. Syntax error: Fix Erlang syntax
# 3. Missing dependency: Add to apps/erlmcp_flow/rebar.config

# Fix and re-run
TERM=dumb rebar3 compile
```

### Tests Fail

```bash
# Run with verbose output
rebar3 eunit --app erlmcp_flow --verbose

# Common issues:
# 1. Incorrect assertion: Fix test expectation
# 2. Race condition: Use synchronous operations
# 3. Process not started: Start in test setup

# Fix and re-run
rebar3 eunit --app erlmcp_flow
```

### Coverage Below 82%

```bash
# Check which modules have low coverage
rebar3 cover --verbose

# Add tests for uncovered branches
vim apps/erlmcp_flow/test/erlmcp_flow_registry_tests.erl

# Re-run
rebar3 eunit --app erlmcp_flow --cover
rebar3 cover
```

### Dialyzer Warnings

```bash
# Run dialyzer
cd apps/erlmcp_flow
rebar3 dialyzer

# Common issues:
# 1. Missing spec: Add -spec annotation
# 2. Wrong return type: Fix spec to match implementation
# 3. Unmatched return: Handle all return values

# Fix and re-run
rebar3 dialyzer
```

### Merge Conflicts

```bash
# Merge main
git merge origin/main --no-ff
# CONFLICT in apps/erlmcp_flow/src/erlmcp_flow_registry.erl

# Open file and resolve
vim apps/erlmcp_flow/src/erlmcp_flow_registry.erl

# Remove conflict markers:
# <<<<<<< HEAD
# Your changes
# =======
# Their changes
# >>>>>>> origin/main

# Test resolution
make compile
rebar3 eunit --app erlmcp_flow

# Commit
git add apps/erlmcp_flow/src/erlmcp_flow_registry.erl
git commit -m "Resolve merge conflict in registry"

# Push
git push origin claude/erlmcp-flow-registry-R9zub
```

---

## Cheat Sheet

### Quality Gates

```bash
# Full quality check
cd /home/user/erlmcp
TERM=dumb rebar3 compile && \
rebar3 eunit --app erlmcp_flow && \
rebar3 cover && \
cd apps/erlmcp_flow && \
rebar3 dialyzer && \
rebar3 xref && \
rebar3 format --check

# If all pass: ✅ Ready to push
```

### Git Commands

```bash
# Create branch
git checkout -b claude/erlmcp-flow-{feature}-{random}

# Commit
git add .
git commit -m "feat(erlmcp-flow): Description"

# Push
git push origin claude/erlmcp-flow-{feature}-{random}

# Update from main
git merge origin/main --no-ff

# Create PR
gh pr create --web
```

### PR Workflow

```bash
# Check PR status
gh pr view

# Check CI status
gh pr checks

# Merge PR (after approvals)
gh pr merge --squash --delete-branch
```

---

## Resources

### Documentation
- **Complete Workflow**: `docs/ERLMCP_FLOW_GIT_WORKFLOW.md`
- **Summary**: `docs/ERLMCP_FLOW_GIT_WORKFLOW_SUMMARY.md`
- **This Guide**: `docs/ERLMCP_FLOW_GIT_WORKFLOW_QUICKSTART.md`

### Architecture
- **README**: `docs/ERLMCP-FLOW-README.md`
- **Architecture**: `docs/erlmcp-flow-architecture.md`
- **Examples**: `docs/erlmcp-flow-examples.md`
- **Implementation Plan**: `docs/erlmcp-flow-implementation-plan.md`

### Templates
- **PR Template**: `.github/PULL_REQUEST_TEMPLATE_ERLMCP_FLOW.md`
- **Bug Report**: `.github/ISSUE_TEMPLATE/erlmcp-flow-bug.md`
- **Feature Request**: `.github/ISSUE_TEMPLATE/erlmcp-flow-feature.md`
- **Performance Issue**: `.github/ISSUE_TEMPLATE/erlmcp-flow-performance.md`

### CI/CD
- **GitHub Actions**: `.github/workflows/erlmcp-flow-ci.yml`

---

## Next Steps

### For New Feature

1. **Read**: `docs/erlmcp-flow-implementation-plan.md`
2. **Create Branch**: `git checkout -b claude/erlmcp-flow-{feature}-{random}`
3. **Write Tests**: Chicago TDD (test-first)
4. **Implement**: Make tests pass
5. **Quality Gates**: Run locally
6. **Push & PR**: Create pull request
7. **Review**: Respond to feedback
8. **Merge**: Squash and merge

### For Bug Fix

1. **Create Issue**: Use bug template
2. **Create Branch**: `git checkout -b claude/erlmcp-flow-bugfix-{issue}`
3. **Write Regression Test**: Reproduces bug
4. **Fix**: Make test pass
5. **Quality Gates**: Run locally
6. **Push & PR**: Create pull request
7. **Merge**: Fast-track if critical

### For Performance Issue

1. **Create Issue**: Use performance template
2. **Benchmark**: Measure current performance
3. **Profile**: Identify bottleneck (fprof/eprof)
4. **Optimize**: Apply optimization
5. **Benchmark**: Verify improvement
6. **Push & PR**: Create pull request with benchmark results

---

## Summary

**5-Step Workflow**:
1. Create feature branch
2. Implement with tests (Chicago TDD)
3. Run quality gates locally
4. Commit and push
5. Create PR

**Quality Gates** (all must pass):
- ✅ Compilation (0 errors)
- ✅ Tests (100% pass, ≥82% coverage)
- ✅ Dialyzer (0 warnings)
- ✅ Xref (0 undefined functions)
- ✅ Format (consistent)

**Golden Rules**:
- ❌ NEVER rebase
- ❌ NEVER force push
- ❌ NEVER skip quality gates
- ✅ ALWAYS merge with --no-ff
- ✅ ALWAYS test-first (Chicago TDD)
- ✅ ALWAYS run quality gates locally

**You're Ready!** Start with: `git checkout -b claude/erlmcp-flow-{feature}-$(openssl rand -hex 2)`

---

**Version**: 1.0.0
**Last Updated**: 2026-02-02
**Status**: Production Ready
