# CLI Versioning, CI/CD Automation, and Release Process

## Overview

This document describes the comprehensive CLI versioning, CI/CD automation, and release process implementation for erlmcp's validation CLI (`erlmcp-validate`).

## Features Implemented

### 1. CLI Versioning

**Version Consistency:**
- CLI module version: `-define(VERSION, "X.Y.Z")` in `erlmcp_validate_cli.erl`
- App version: `{vsn, "X.Y.Z"}` in `erlmcp_validation.app.src`
- Semantic versioning: `MAJOR.MINOR.PATCH[-PRERELEASE]`

**Version Display:**
```bash
make cli-version            # Show all version information
erlmcp-validate --version   # CLI escript version
```

### 2. CI/CD Automation

**GitHub Workflow:** `.github/workflows/cli-validation.yml`

**7-Gate Quality Pipeline:**

1. **Gate 1: CLI Compilation**
   - Compiles all apps
   - Builds validation CLI escript
   - Verifies escript exists
   - Uploads artifact for downstream gates

2. **Gate 2: CLI Help & Version**
   - Tests `--help` command output
   - Tests `--version` command output
   - Validates version format

3. **Gate 3: CLI Validation Commands**
   - Tests `spec` validation
   - Tests `quick-check` command
   - Tests `status` command
   - 30-second timeout per command

4. **Gate 4: CLI Startup Time**
   - Benchmarks startup time (5 runs)
   - Calculates average
   - **Threshold: < 2000ms**
   - Alerts on regression

5. **Gate 5: CLI Test Suite**
   - Runs EUnit tests for validation app
   - Non-blocking if no tests exist

6. **Gate 6: CLI Coverage**
   - Generates coverage report
   - **Threshold: >= 85%**
   - Blocks on insufficient coverage

7. **Gate 7: Shell Completions**
   - Placeholder for future implementation
   - Will generate bash/zsh/fish completions

**CI Triggers:**
- Push to main, develop, feature/** branches
- Pull requests to main, develop
- Manual workflow dispatch
- Only runs when CLI files change

### 3. Automated Release Process

**Release Script:** `scripts/release-cli.sh`

**Usage:**
```bash
# Full release
./scripts/release-cli.sh 1.0.0

# Test release process (no changes)
./scripts/release-cli.sh 1.0.1 --dry-run

# Via Make
make cli-release VERSION=1.0.0
make cli-release-dry-run VERSION=1.0.1
```

**Release Steps (9 stages):**

1. **Pre-flight Checks**
   - Validates semantic version format
   - Checks git working directory is clean
   - Verifies current branch (warns if not main)
   - Checks for required commands (git, rebar3, sha256sum, gh)

2. **Update Version Numbers**
   - Updates `erlmcp_validate_cli.erl` `-define(VERSION, ...)`
   - Updates `erlmcp_validation.app.src` `{vsn, ...}`

3. **Run Quality Gates**
   - Compiles all apps (`TERM=dumb rebar3 compile`)
   - Runs validation tests (`rebar3 eunit --application=erlmcp_validation`)

4. **Build Escript**
   - Builds escript (`rebar3 as validation escriptize`)
   - Verifies escript exists at `_build/validation/bin/erlmcp_validate`
   - Tests escript `--version` output

5. **Generate Checksums**
   - Creates SHA256 checksum: `erlmcp-validate-X.Y.Z.sha256`

6. **Update CHANGELOG**
   - Prompts to manually update CHANGELOG.md
   - Checks if version already documented

7. **Git Commit and Tag**
   - Commits version changes with quality gate report
   - Creates annotated tag: `cli-vX.Y.Z`
   - Pushes to remote

8. **Create GitHub Release**
   - Uses `gh` CLI to create release
   - Uploads escript binary as `erlmcp-validate`
   - Uploads checksum file
   - Auto-generates release notes with:
     - Installation instructions
     - Usage examples
     - Checksums
     - Quality gate status

9. **Summary Report**
   - Lists all release artifacts
   - Provides next steps
   - Shows release URL

**Dry-Run Mode:**
- Tests entire release process
- No files modified
- No git commits/tags
- No GitHub release created
- Shows exactly what would happen

### 4. Makefile Targets

**CLI Operations:**

```bash
# Version Management
make cli-version                           # Show CLI version info

# Release
make cli-release VERSION=1.0.0             # Create release
make cli-release-dry-run VERSION=1.0.0     # Test release

# Benchmarking
make cli-benchmark-baseline                # Measure startup time

# Testing
make cli-test-startup                      # Quick startup test

# Build & Distribution
make validate-cli                          # Build escript
make cli-checksum                          # Generate SHA256
make cli-install                           # Install to /usr/local/bin (sudo)
make cli-uninstall                         # Uninstall from /usr/local/bin (sudo)
```

**Help Output:**
```bash
make help | grep cli-
```

### 5. Performance Baselines

**Thresholds:**
- **Startup time**: < 2000ms (enforced by CI Gate 4)
- **Help command**: < 3000ms (baseline benchmark)

**Benchmarking:**
- 10 runs averaged for consistency
- Millisecond precision
- Alerts on regression

**Example Output:**
```
CLI Performance Baseline Benchmarking

Test 1: Startup Time (--version)
  Run 1: 245ms
  Run 2: 238ms
  ...
  Run 10: 242ms
  Average: 241ms
✓ Startup time acceptable: 241ms

Test 2: Help Command Time
  Elapsed: 312ms
✓ Help command acceptable: 312ms
```

### 6. Distribution

**GitHub Release Assets:**
1. `erlmcp-validate` - Standalone executable escript
2. `erlmcp-validate-X.Y.Z.sha256` - SHA256 checksum

**Installation Methods:**

**Direct Download:**
```bash
curl -LO https://github.com/banyan-platform/erlmcp/releases/download/cli-v1.0.0/erlmcp-validate
chmod +x erlmcp-validate
./erlmcp-validate --version
```

**Via Make:**
```bash
make cli-install  # Installs to /usr/local/bin/erlmcp-validate
```

**Verify Checksum:**
```bash
sha256sum -c erlmcp-validate-1.0.0.sha256
```

## File Structure

```
erlmcp/
├── .github/workflows/
│   └── cli-validation.yml          # CI/CD pipeline (7 gates)
├── apps/erlmcp_validation/
│   └── src/
│       ├── erlmcp_validate_cli.erl # CLI module with VERSION
│       └── erlmcp_validation.app.src # App version
├── scripts/
│   └── release-cli.sh              # Automated release script (9 stages)
├── Makefile                        # CLI targets
├── CHANGELOG.md                    # Updated with CLI changes
└── CLI_VERSIONING_RELEASE.md       # This document
```

## Workflow Examples

### Release Workflow

**1. Prepare Release:**
```bash
# Update CHANGELOG.md manually
vim CHANGELOG.md

# Test release process
make cli-release-dry-run VERSION=1.1.0
```

**2. Execute Release:**
```bash
# Create actual release
make cli-release VERSION=1.1.0
```

**3. Verify Release:**
```bash
# Check GitHub release
open https://github.com/banyan-platform/erlmcp/releases/tag/cli-v1.1.0

# Download and test
curl -LO https://github.com/banyan-platform/erlmcp/releases/download/cli-v1.1.0/erlmcp-validate
chmod +x erlmcp-validate
./erlmcp-validate --version
```

### Development Workflow

**1. Feature Development:**
```bash
# Work on CLI feature
vim apps/erlmcp_validation/src/erlmcp_validate_cli.erl

# Build and test locally
make validate-cli
./_build/validation/bin/erlmcp_validate --help
```

**2. Pre-Commit:**
```bash
# Run quick validation
make cli-test-startup
make cli-benchmark-baseline
```

**3. Push:**
```bash
git push origin feature/cli-enhancement
# CI runs 7-gate validation automatically
```

## Quality Gates Summary

| Gate | Description | Threshold | Blocking |
|------|-------------|-----------|----------|
| 1 | Compilation | 0 errors | Yes |
| 2 | Help & Version | Commands work | Yes |
| 3 | Validation Commands | Commands execute | Yes |
| 4 | Startup Time | < 2000ms | Yes |
| 5 | Test Suite | 0 failures | Yes |
| 6 | Coverage | >= 85% | Yes |
| 7 | Completions | Generated | No (placeholder) |

## Release Checklist

- [ ] Update CHANGELOG.md with new features/fixes
- [ ] Run dry-run release: `make cli-release-dry-run VERSION=X.Y.Z`
- [ ] Verify all quality gates pass locally
- [ ] Execute release: `make cli-release VERSION=X.Y.Z`
- [ ] Verify GitHub release created
- [ ] Test download and installation
- [ ] Announce release to users

## Future Enhancements

1. **Shell Completions:**
   - Bash completion script
   - Zsh completion script
   - Fish completion script

2. **Package Managers:**
   - Homebrew formula
   - AUR package (Arch Linux)
   - apt repository (Debian/Ubuntu)

3. **Enhanced Benchmarking:**
   - Command execution time tracking
   - Feature-specific regression tests
   - Historical performance tracking

4. **Automated Changelog:**
   - Parse git commits
   - Auto-generate CHANGELOG entries
   - Conventional commits integration

## Troubleshooting

### Release Script Fails

**Problem:** "Working directory not clean"
```bash
# Check status
git status

# Commit or stash changes
git add .
git commit -m "Your message"
```

**Problem:** "gh not found"
```bash
# Install GitHub CLI
# macOS
brew install gh

# Linux
sudo apt install gh
```

### CI Gates Fail

**Gate 4 (Startup Time):**
- Check for blocking I/O operations
- Profile startup sequence
- Optimize module loading

**Gate 6 (Coverage):**
- Add more unit tests
- Test edge cases
- Aim for 85%+ coverage

### Escript Build Fails

```bash
# Clean and rebuild
make clean
make validate-cli

# Check rebar3 profile
rebar3 as validation escriptize

# Verify dependencies
rebar3 tree
```

## References

- **CI Workflow:** `.github/workflows/cli-validation.yml`
- **Release Script:** `scripts/release-cli.sh`
- **Makefile Targets:** `Makefile` (lines 1038-1120)
- **CHANGELOG:** `CHANGELOG.md` (Unreleased section)
- **CLI Module:** `apps/erlmcp_validation/src/erlmcp_validate_cli.erl`

---

**Maintained by:** erlmcp GitHub Operations Agent
**Last Updated:** 2026-02-01
**Status:** Production Ready
