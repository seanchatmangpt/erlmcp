# Git Operations Quick Reference - erlmcp

## Release Workflow

### 1. Prepare Release
```bash
# Run the automated preparation script
./scripts/release-prepare-v0.6.0.sh

# Or manually:
git checkout main
git pull origin main
git checkout -b release/v0.6.0

# Stage changes logically (see GIT_WORKFLOW_PLAN.md)
git add apps/erlmcp_core/test/
git commit -m "test: Comprehensive test suite expansion"
```

### 2. Quality Gates
```bash
# Full quality check
make check

# Individual checks
TERM=dumb rebar3 compile          # Compilation
rebar3 eunit                       # Unit tests
rebar3 ct                          # Integration tests
rebar3 cover                       # Coverage report
rebar3 dialyzer                    # Type checking
rebar3 xref                        # Cross-reference
```

### 3. Create Release
```bash
# Run the automated release script
./scripts/release-create-v0.6.0.sh

# Or manually:
git tag -a v0.6.0 -m "Release v0.6.0"
git push origin release/v0.6.0
git push origin v0.6.0
gh release create v0.6.0 --notes "Release notes..."
```

## Branch Strategy

### Main Branches
- **main**: Production-ready code
- **release/vX.Y.Z**: Release preparation
- **feature/***: Feature development
- **fix/***: Bug fixes
- **refactor/***: Code refactoring

### Feature Branch Workflow
```bash
# Create feature branch
git checkout main
git checkout -b feature/my-feature

# Make changes
git add .
git commit -m "feat: Add my feature

Description of what and why.

Co-Authored-By: Claude <noreply@anthropic.com>"

# Push and create PR
git push origin feature/my-feature
gh pr create --title "feat: Add my feature" --body "PR description..."
```

### Pull Request Workflow
```bash
# Create PR from current branch
gh pr create \
  --title "feat: My feature" \
  --body "Use PULL_REQUEST_TEMPLATE.md"

# List PRs
gh pr list

# Merge PR
gh pr merge --merge --delete-branch
```

## Commit Message Convention

### Format
```
<type>(<scope>): <subject>

<body>

<footer>
```

### Types
- **feat**: New feature
- **fix**: Bug fix
- **docs**: Documentation changes
- **test**: Test changes
- **refactor**: Code refactoring
- **perf**: Performance improvement
- **chore**: Build/process changes
- **ci**: CI/CD changes

### Example
```
feat(client): Add request correlation tracking

- Implement pending request map
- Add timeout handling
- Improve error recovery

Fixes #123
Co-Authored-By: Claude <noreply@anthropic.com>
```

## Quality Gates Checklist

### Pre-Commit
```bash
# Run pre-commit hooks
.git/hooks/pre-commit

# Or manually
./tools/claude-md-enforcer.sh
```

### Pre-Push
```bash
# Full quality check
make check

# Verify:
# ✅ Compilation: 0 errors
# ✅ Tests: 100% pass rate
# ✅ Coverage: 80%+
# ✅ Dialyzer: Clean
# ✅ Xref: Clean
```

### Pre-Merge
```bash
# Ensure CI passes
gh pr checks

# Manual verification
rebar3 compile && rebar3 eunit && rebar3 ct
```

## Common Operations

### Undo Changes
```bash
# Unstage file
git restore --staged <file>

# Discard local changes
git restore <file>

# Undo last commit (keep changes)
git reset --soft HEAD~1

# Undo last commit (discard changes)
git reset --hard HEAD~1
```

### Resolve Conflicts
```bash
# Merge conflict
git status
# Edit conflicted files
git add <resolved-files>
git commit

# Abort merge
git merge --abort
```

### Clean Up
```bash
# Remove merged branches
git branch --merged | grep -v main | xargs git branch -d

# Prune remote branches
git remote prune origin

# Clean build artifacts
rebar3 clean
git clean -fdx
```

## CI/CD Integration

### Quality Gates Workflow
```bash
# Trigger quality gates
git push origin main

# Monitor workflow
gh run list

# View specific run
gh run view <run-id>

# Watch logs
gh run view <run-id> --log
```

### Release Workflow
```bash
# Trigger release
git push origin v0.6.0

# Monitor release workflow
gh run list --branch=release/v0.6.0

# View release
gh release view v0.6.0
```

## Issue Tracking

### Create Issue
```bash
# Bug report
gh issue create --title "[BUG] Client crash" --body "..."

# Feature request
gh issue create --title "[FEATURE] Add SSE transport" --body "..."

# Quality gate failure
gh issue create --title "[QUALITY] Dialyzer warnings" --body "..."
```

### Issue Labels
- **bug**: Bug report
- **enhancement**: Feature request
- **quality**: Quality gate failure
- **performance**: Performance issue
- **documentation**: Documentation
- **ci-cd**: CI/CD related

### Manage Issues
```bash
# List issues
gh issue list

# Assign issue
gh issue edit <issue-number> --add-assignee @user

# Close issue
gh issue close <issue-number>

# Convert issue to PR
gh pr create --issue <issue-number>
```

## Release Artifacts

### Build Release
```bash
# Production build
rebar3 as prod release

# Create tarball
rebar3 as prod tar

# Artifacts location
ls _build/prod/rel/erlmcp/
```

### Verify Release
```bash
# Checksums
sha256sum erlmcp.tar.gz > SHA256SUMS

# Verify
sha256sum -c SHA256SUMS
```

### Deploy Release
```bash
# Copy to server
scp _build/prod/rel/erlmcp/erlmcp.tar.gz user@host:/opt/

# Extract
tar -xzf erlmcp.tar.gz

# Start
./erlmcp/bin/erlmcp start
```

## Monitoring

### Check Status
```bash
# Git status
git status

# Branch status
git branch -vv

# Remote status
git remote -v
```

### View History
```bash
# Commit log
git log --oneline -10

# Detailed log
git log -p -1

# File history
git log --follow -- file.erl
```

### Compare Branches
```bash
# Diff branches
git diff main..release/v0.6.0

# Merge status
git log main..release/v0.6.0

# Common ancestor
git merge-base main release/v0.6.0
```

## Troubleshooting

### Build Issues
```bash
# Clean rebuild
rebar3 clean && rebar3 compile

# Update dependencies
rebar3 upgrade

# Check lock file
rebar3 lock
```

### Test Failures
```bash
# Verbose tests
rebar3 eunit --verbose

# Specific test
rebar3 eunit --module=erlmcp_server_tests

# CT with verbose
rebar3 ct --suite=test/erlmcp_integration_SUITE --verbose
```

### CI/CD Issues
```bash
# Check workflow logs
gh run view <run-id> --log-failed

# Re-run workflow
gh run rerun <run-id>

# Cancel workflow
gh run cancel <run-id>
```

## Best Practices

### Commit Early, Often
```bash
# Atomic commits
git add specific_file.erl
git commit -m "fix: Specific fix"

# Logical commits
git add related_files
git commit -m "feat: Logical feature"
```

### Write Good Messages
```
# Good
feat(client): Add request timeout handling

Implement configurable timeout for client requests
with default of 5000ms.

Fixes #45
Co-Authored-By: Claude <noreply@anthropic.com>

# Bad
update
fix stuff
more changes
```

### Use Branches
```bash
# Feature branch
git checkout -b feature/my-feature

# Fix branch
git checkout -b fix/issue-123

# Release branch
git checkout -b release/v0.6.0
```

### Quality First
```bash
# Always run checks
make check

# Never skip tests
rebar3 eunit && rebar3 ct

# Verify before push
git status
git diff --staged
```

## Resources

### Documentation
- `docs/GIT_WORKFLOW_PLAN.md` - Comprehensive workflow plan
- `docs/architecture.md` - Architecture overview
- `docs/otp-patterns.md` - OTP patterns

### Scripts
- `scripts/release-prepare-v0.6.0.sh` - Release preparation
- `scripts/release-create-v0.6.0.sh` - Release execution
- `tools/claude-md-enforcer.sh` - Quality gate enforcement

### Templates
- `.github/PULL_REQUEST_TEMPLATE.md` - PR template
- `.github/ISSUE_TEMPLATE/` - Issue templates

### CI/CD
- `.github/workflows/quality-gates.yml` - Quality gates
- `.github/workflows/release.yml` - Release workflow

---

**Version**: v0.6.0
**Last Updated**: 2025-01-29
