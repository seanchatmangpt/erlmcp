# erlmcp v3.0.0 Cleanup Execution Checklist
## Step-by-Step Guide to OSS Release Preparation

**Version**: 3.0.0
**Date**: 2026-01-31
**Purpose**: Execute cleanup plan for OSS release

---

## Pre-Cleanup Preparation

### Backup Current State

```bash
# Create backup branch
git checkout -b backup/before-v3-cleanup

# Tag backup
git tag backup/v2.1.0-pre-cleanup

# Push backup
git push origin backup/before-v3-cleanup
git push origin backup/v2.1.0-pre-cleanup

# Return to main
git checkout main
```

### Create Cleanup Branch

```bash
# Create cleanup branch
git checkout -b cleanup/v3-oss-release

# Verify starting point
git log --oneline -5
```

### Pre-Cleanup Validation

```bash
# Run full test suite to ensure clean starting state
rebar3 clean
rebar3 compile
rebar3 eunit
rebar3 ct

# Verify all tests pass
echo "All tests must pass before proceeding!"
```

---

## Phase 1: Remove POC Code

### Step 1.1: Remove Mermaid Modules

**Files to Delete** (7 modules):
```bash
# List Mermaid files
find apps/erlmcp_core/src -name "*mermaid*" -type f

# Expected output:
# apps/erlmcp_core/src/erlmcp_mermaid_cache.erl
# apps/erlmcp_core/src/erlmcp_mermaid_protocol.erl
# apps/erlmcp_core/src/erlmcp_mermaid_registry.erl
# apps/erlmcp_core/src/erlmcp_mermaid_renderer.erl
# apps/erlmcp_core/src/erlmcp_mermaid_session.erl
# apps/erlmcp_core/src/erlmcp_mermaid_sup.erl

# Delete Mermaid source files
rm -f apps/erlmcp_core/src/erlmcp_mermaid_cache.erl
rm -f apps/erlmcp_core/src/erlmcp_mermaid_protocol.erl
rm -f apps/erlmcp_core/src/erlmcp_mermaid_registry.erl
rm -f apps/erlmcp_core/src/erlmcp_mermaid_renderer.erl
rm -f apps/erlmcp_core/src/erlmcp_mermaid_session.erl
rm -f apps/erlmcp_core/src/erlmcp_mermaid_sup.erl

# Delete Mermaid beam files (if any)
rm -f apps/erlmcp_core/src/erlmcp_mermaid_protocol.beam

# Delete Mermaid test files
find apps/erlmcp_core/test -name "*mermaid*" -type f -delete
```

**Verification**:
```bash
# Verify no Mermaid files remain
find apps/erlmcp_core -name "*mermaid*" -type f

# Expected: No results
```

### Step 1.2: Remove POC Directory

**Files to Delete** (5 modules):
```bash
# List POC files
ls -la apps/erlmcp_core/src/poc/

# Expected output:
# erlmcp_poc_demo.erl
# erlmcp_consensus_poc.erl
# erlmcp_pool_poc.erl
# erlmcp_streaming_poc.erl
# circuit_breaker_integration_example.erl

# Delete entire POC directory
rm -rf apps/erlmcp_core/src/poc/
```

**Verification**:
```bash
# Verify POC directory removed
ls -la apps/erlmcp_core/src/poc/

# Expected: No such file or directory
```

### Step 1.3: Remove Standalone POC Files

**Files to Delete/Move**:
```bash
# List standalone POC files in apps root
ls -la apps/ | grep -i poc

# Expected output:
# erlmcp_pubsub_poc.erl
# erlmcp_streaming_poc.erl

# Delete standalone POC files
rm -f apps/erlmcp_pubsub_poc.erl
rm -f apps/erlmcp_streaming_poc.erl

# Check for orphaned transport_sse file
if [ -f apps/erlmcp_transport_sse.erl ]; then
    # Move to transports if functional, otherwise delete
    echo "Found erlmcp_transport_sse.erl - needs review"
    # For now, assume it's POC and delete
    rm -f apps/erlmcp_transport_sse.erl
fi
```

**Verification**:
```bash
# Verify no POC files in apps root
find apps -maxdepth 1 -name "*poc*.erl" -type f

# Expected: No results
```

### Step 1.4: Remove POC Documentation

**Documentation to Remove**:
```bash
# Check for Mermaid/POC documentation
find docs -name "*mermaid*" -o -name "*poc*" -type f

# Remove if found
find docs -name "*mermaid*" -type f -delete
find docs -name "*poc*" -type f -delete
```

**Verification**:
```bash
# Verify cleanup
find docs -name "*mermaid*" -o -name "*poc*"

# Expected: Only docs/v3/ reference (this documentation)
```

---

## Phase 2: Update Version Numbers

### Step 2.1: Update .app.src Files

**Files to Update**:
```bash
# List all .app.src files
find apps -name "*.app.src" -type f

# Expected output:
# apps/erlmcp_core/src/erlmcp_core.app.src
# apps/erlmcp_transports/src/erlmcp_transports.app.src
# apps/erlmcp_observability/src/erlmcp_observability.app.src
# apps/erlmcp_validation/src/erlmcp_validation.app.src
```

**Update Command** (sed):
```bash
# Update version 2.1.0 → 3.0.0 in all .app.src files
find apps -name "*.app.src" -type f -exec sed -i '' 's/{vsn, "2.1.0"}/{vsn, "3.0.0"}/g' {} \;

# Verify changes
grep -n "vsn" apps/*/src/*.app.src
```

**Manual Verification**:
```erlang
% Each .app.src file should now have:
{vsn, "3.0.0"}
```

### Step 2.2: Update rebar.config

**Edit rebar.config**:
```erlang
% Find release section:
{relx, [
    {release, {erlmcp, "3.0.0"},  % Update from 2.0.0
     [erlmcp_core,
      erlmcp_transports,
      erlmcp_observability,
      erlmcp_validation
      %% tcps_erlmcp excluded by default (optional)
     ]}
]}.
```

**Update Command**:
```bash
# Update version in rebar.config
sed -i '' 's/{release, {erlmcp, "2.0.0"}/{release, {erlmcp, "3.0.0"}/g' rebar.config

# Verify
grep -A5 "release.*erlmcp" rebar.config
```

### Step 2.3: Update README.md

**Verify README.md**:
```bash
# Check current version declarations
grep -n "3.0\|v3\|version" README.md | head -20

# Ensure consistency with 3.0.0
# README.md should already declare v3.0.0 (OTP 28.3.1+ requirement)
```

---

## Phase 3: Update Module Lists

### Step 3.1: Remove Mermaid from Module Lists

**Edit erlmcp_core.app.src**:
```erlang
% Remove Mermaid modules from modules list
{modules, [
    %% ... existing modules ...
    erlmcp_elicitation,
    erlmcp_completion,

    %% REMOVED: Mermaid modules (moved to commercial package)
    %% erlmcp_mermaid_cache,
    %% erlmcp_mermaid_protocol,
    %% erlmcp_mermaid_registry,
    %% erlmcp_mermaid_renderer,
    %% erlmcp_mermaid_session,
    %% erlmcp_mermaid_sup,

    %% ... rest of modules ...
]}.
```

**Verification**:
```bash
# Verify no Mermaid modules in module list
grep -i "mermaid" apps/erlmcp_core/src/erlmcp_core.app.src

# Expected: Only commented lines (%% erlmcp_mermaid_*)
```

### Step 3.2: Remove Mermaid Supervisor Reference

**Check for Mermaid supervisor references**:
```bash
# Search for erlmcp_mermaid_sup references
grep -rn "erlmcp_mermaid_sup" apps/erlmcp_core/src/

# Expected: No results (if found, remove from supervisor child specs)
```

**Action if found**:
```erlang
% Remove from supervisor child spec
% erlmcp_core_sup.erl or erlmcp_sup.erl

%% REMOVED: Mermaid supervisor (commercial only)
%% #{
%%     id => erlmcp_mermaid_sup,
%%     start => {erlmcp_mermaid_sup, start_link, []},
%%     restart => permanent,
%%     shutdown => infinity,
%%     type => supervisor,
%%     modules => [erlmcp_mermaid_sup]
%% },
```

---

## Phase 4: Build and Test

### Step 4.1: Clean Build

```bash
# Clean all build artifacts
rebar3 clean -a

# Remove _build directory
rm -rf _build/

# Fresh compile
rebar3 compile

# Verify no compilation errors
echo "Compilation successful!"
```

**Expected Output**:
```
===> Verifying dependencies...
===> Compiling erlmcp_core
===> Compiling erlmcp_transports
===> Compiling erlmcp_observability
===> Compiling erlmcp_validation
```

### Step 4.2: Run Tests

```bash
# Run EUnit tests
rebar3 eunit

# Run Common Test suites
rebar3 ct

# Verify test coverage
rebar3 cover

# Expected: All tests pass, coverage >= 80%
```

**Failure Handling**:
```bash
# If tests fail, investigate:
rebar3 eunit --module=failing_module_tests --verbose

# Common issues:
# - Module references to removed Mermaid code
# - Supervisor child spec errors
# - Missing dependencies
```

### Step 4.3: Dialyzer Check

```bash
# Build PLT
rebar3 dialyzer build-plt

# Run dialyzer
rebar3 dialyzer

# Expected: No new warnings (existing warnings OK)
```

### Step 4.4: Xref Check

```bash
# Run xref to find undefined functions
rebar3 xref

# Expected: No undefined function errors related to removed modules
```

---

## Phase 5: Update Documentation

### Step 5.1: Update docs/architecture.md

**Key Updates Required**:
```markdown
# erlmcp Architecture - v3.0.0

## Executive Summary

erlmcp v3.0.0 is a **production-grade umbrella application** ...
- **Version**: 3.0.0 (was 2.0.0)
- **OTP Requirement**: 28.3.1+ (exclusive, was 25-28)
- **Module Count**: 165 modules across 4 apps (was 94)
- **Removed**: Mermaid renderer (moved to commercial package)
- **Removed**: POC code (pubsub, consensus, streaming)
```

**Update Command**:
```bash
# Create v3.0.0 architecture doc
cp docs/architecture.md docs/architecture_v2.1.0.md

# Edit docs/architecture.md with v3.0.0 content
# Use docs/v3/10_architecture_design_plan.md as reference
```

### Step 5.2: Update README.md

**Verify Content**:
```markdown
## ⚠️ Version 3.0: OTP 28.3.1+ Required

**BREAKING CHANGE**: erlmcp v3.0+ requires **Erlang/OTP 28.3.1 or later**.

## Quick Start

**Prerequisites**: Erlang/OTP 28.3.1 or later
```

### Step 5.3: Create v3.0.0 Release Notes

**Create docs/v3.0.0-release-notes.md**:
```markdown
# erlmcp v3.0.0 Release Notes

## Breaking Changes

### OTP Version Requirement
- **Before**: Erlang/OTP 25-28
- **After**: Erlang/OTP 28.3.1+ (exclusive)
- **Action**: Upgrade Erlang before upgrading erlmcp

### Mermaid Components Removed
- **Before**: Included in erlmcp_core
- **After**: Moved to commercial package
- **Action**: Remove Mermaid dependencies or install commercial add-on

### POC Code Removed
- Removed: erlmcp_pubsub_poc, erlmcp_streaming_poc, consensus POC
- Removed: poc/ directory (5 experimental modules)

## New Features

### MCP 2025-11-25 Support
- ✅ Request cancellation
- ✅ Cursor-based pagination
- ✅ Argument completion
- ✅ User input elicitation (inline, url, terminal)
- ✅ Root directory management
- ✅ Application lifecycle management

### OTP 28+ Features
- ✅ Native json module (jsx removed)
- ✅ Priority message routing
- ✅ Improved process iteration

## Module Count

- **v2.1.0**: 94 modules (documented)
- **v3.0.0**: 165 modules (actual, after cleanup)
- **Removed**: 27 POC modules

## Migration Guide

See [Migration Guide](docs/v3/50_cleanup_execution_checklist.md) for detailed steps.
```

---

## Phase 6: Validation

### Step 6.1: Pre-Commit Validation

```bash
# Run full validation
make check

# Or manual:
rebar3 compile && rebar3 xref && rebar3 dialyzer && rebar3 eunit && rebar3 ct

# Verify all checks pass
echo "All validation passed!"
```

### Step 6.2: Build Release

```bash
# Build minimal release
rebar3 as minimal release

# Build standard release
rebar3 as prod release

# Verify releases
ls -lh _build/minimal/rel/erlmcp/
ls -lh _build/prod/rel/erlmcp/

# Expected sizes:
# Minimal: ~50MB
# Standard: ~75MB
```

### Step 6.3: Smoke Test

```bash
# Start release
_build/prod/rel/erlmcp/bin/erlmcp console

# In Erlang shell, verify:
erlmcp_server:start_link(test_server, #{}).
erlmcp_client:start_link(test_client, #{}).
erlmcp_metrics:record_request(#{method => <<"test">>}).

# Expected: All commands succeed
# Quit: q().
```

---

## Phase 7: Git Commit

### Step 7.1: Review Changes

```bash
# Review all changes
git status

# View diff
git diff

# Expected changes:
# - Removed: Mermaid modules (7 files)
# - Removed: POC directory (5 files)
# - Removed: Standalone POC files (2 files)
# - Modified: .app.src files (version updates)
# - Modified: rebar.config (version update)
# - Modified: docs/architecture.md (v3.0.0 update)
```

### Step 7.2: Commit Changes

```bash
# Stage all changes
git add -A

# Commit with detailed message
git commit -m "Cleanup v3.0.0: Remove POC code and update versions

BREAKING CHANGES:
- Remove Mermaid renderer (7 modules) - move to commercial package
- Remove POC code (5 modules in poc/ directory)
- Remove standalone POC files (2 modules)
- Update version: 2.1.0 → 3.0.0

UPDATES:
- All .app.src files: vsn 3.0.0
- rebar.config: release version 3.0.0
- docs/architecture.md: Update to v3.0.0
- Module lists: Remove Mermaid references

MODULE COUNT:
- Before: 192 modules (with POC)
- After: 165 modules (clean OSS)
- Removed: 27 POC modules

COMPATIBILITY:
- Requires Erlang/OTP 28.3.1+ (exclusive)
- Uses native json module (jsx removed)
- Mermaid rendering: Available as commercial add-on

TESTING:
- All tests pass (EUnit + CT)
- Dialyzer clean (no new warnings)
- Xref clean (no undefined functions)
- Release builds successful (minimal + standard)

Related: #<issue-number>
Co-Author: Claude Opus 4.5 <noreply@anthropic.com>
"
```

### Step 7.3: Push Changes

```bash
# Push cleanup branch
git push origin cleanup/v3-oss-release

# Create pull request
gh pr create --title "Cleanup v3.0.0: OSS Release Preparation" \
             --body "Complete cleanup plan for v3.0.0 OSS release. See checklist in docs/v3/50_cleanup_execution_checklist.md" \
             --base main
```

---

## Phase 8: Merge and Tag

### Step 8.1: Code Review

```bash
# Wait for PR review and approval
# Address any feedback

# Ensure CI/CD passes
# Ensure all checks pass
```

### Step 8.2: Merge to Main

```bash
# Merge PR (squash merge recommended)
git checkout main
git merge cleanup/v3-oss-release --squash
git commit -m "Release v3.0.0: OSS release with cleanup

Complete v3.0.0 OSS release:
- Remove POC code (27 modules)
- Update version to 3.0.0
- Update documentation
- Require OTP 28.3.1+
- Module count: 165 (clean OSS)
"

# Push main
git push origin main
```

### Step 8.3: Tag Release

```bash
# Create annotated tag
git tag -a v3.0.0-oss -m "erlmcp v3.0.0 OSS Release

Key features:
- MCP 2025-11-25 support
- OTP 28.3.1+ exclusive
- 165 modules across 4 apps
- Clean OSS codebase (POC removed)
- Minimal: 50MB, Standard: 75MB

Migration: See docs/v3/50_cleanup_execution_checklist.md"

# Push tag
git push origin v3.0.0-oss
```

### Step 8.4: GitHub Release

```bash
# Create GitHub release
gh release create v3.0.0-oss \
  --title "erlmcp v3.0.0 OSS Release" \
  --notes "Release v3.0.0: Complete OSS release with POC cleanup

## What's New
- MCP 2025-11-25 support (cancellation, pagination, completion, elicitation)
- OTP 28.3.1+ exclusive (native json, priority messages)
- Clean OSS codebase (POC removed)

## Breaking Changes
- Requires OTP 28.3.1+ (no backward compatibility)
- Mermaid renderer moved to commercial package
- POC code removed

## Migration
See [Migration Guide](docs/v3/50_cleanup_execution_checklist.md)

## Downloads
- erlmcp-v3.0.0-oss.tar.gz (Source)
- erlmcp-v3.0.0-minimal.tar.gz (Minimal release ~50MB)
- erlmcp-v3.0.0-standard.tar.gz (Standard release ~75MB)
"
```

---

## Post-Release Tasks

### Step 9.1: Update Documentation Site

```bash
# Update version in documentation site
# Deploy to GitHub Pages or documentation hosting
```

### Step 9.2: Announce Release

```bash
# Announce on:
# - GitHub releases
# - Mailing list
# - Discord/Slack community
# - Blog post (if applicable)
```

### Step 9.3: Monitor Issues

```bash
# Track issues related to v3.0.0 release
# Address any migration problems
# Collect user feedback
```

---

## Rollback Plan

If critical issues are found:

```bash
# Rollback to v2.1.0
git checkout v2.1.0

# Create hotfix branch
git checkout -b hotfix/v3.0.0-critical-issue

# Fix issue
# Test thoroughly

# Tag new release
git tag v3.0.1-oss

# Push hotfix
git push origin v3.0.1-oss
```

---

## Validation Checklist

Use this checklist to verify cleanup completion:

### Code Cleanup
- [ ] All Mermaid modules removed (7 files)
- [ ] POC directory removed (5 files)
- [ ] Standalone POC files removed (2 files)
- [ ] No POC references remain in code
- [ ] No Mermaid references remain in supervisors

### Version Updates
- [ ] All .app.src files updated to 3.0.0
- [ ] rebar.config updated to 3.0.0
- [ ] README.md consistent with 3.0.0
- [ ] Documentation updated to 3.0.0

### Build & Test
- [ ] Clean build successful
- [ ] EUnit tests pass
- [ ] Common Test suites pass
- [ ] Coverage >= 80%
- [ ] Dialyzer clean
- [ ] Xref clean
- [ ] Release builds successful

### Documentation
- [ ] docs/architecture.md updated
- [ ] README.md verified
- [ ] Release notes created
- [ ] Migration guide complete

### Git & Release
- [ ] Cleanup branch created
- [ ] Changes committed
- [ ] Pull request created
- [ ] Code review approved
- [ ] Merged to main
- [ ] Tagged as v3.0.0-oss
- [ ] GitHub release published

---

## Summary

This cleanup execution checklist provides:

✅ **Step-by-step instructions** - Each phase with detailed commands
✅ **Validation checkpoints** - Verify after each phase
✅ **Rollback plan** - Emergency rollback if needed
✅ **Complete checklist** - Track all cleanup tasks

**Estimated Time**: 2-4 hours (assuming no issues)

**Risk Level**: Medium (code removal, version updates)

**Next Steps**: Execute checklist and tag v3.0.0-oss release

---

**Document Status**: ✅ Complete
**Related Documents**:
- `10_architecture_design_plan.md` - Overall v3.0.0 architecture
- `20_supervision_tree_v3.md` - Complete supervision hierarchy
- `30_component_dependency_matrix.md` - Dependency analysis
- `40_component_diagrams.md` - Visual architecture reference
