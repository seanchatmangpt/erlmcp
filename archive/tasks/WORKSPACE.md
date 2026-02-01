# erlmcp Workspace Governance & Procedures (v0.1.0)

## Workspace Identity

**erlmcp** is a unified Erlang/OTP workspace hosting:

1. **TAIEA** (TAI Execution Agent) - Primary autonomic computing platform
2. **erlmcp** (MCP Reference) - Model Context Protocol implementation (vendor)
3. **Shared Infrastructure** - Rebar3, testing, CI/CD

This document establishes governance, build procedures, and release processes.

## Directory Structure & Responsibilities

### Root Level (`/Users/sac/erlmcp/`)

| File/Dir | Owner | Purpose |
|----------|-------|---------|
| `taiea/` | TAIEA Team | Primary autonomic execution agent |
| `vendor/` | MCP Team | Third-party dependencies (git submodules) |
| `rebar.config` | DevOps | Workspace-wide Rebar3 config |
| `Makefile` | DevOps | High-level build targets |
| `README.md` | DevOps | Workspace overview & quick start |
| `WORKSPACE.md` | DevOps | This governance document |
| `.gitignore` | DevOps | Git ignore rules |
| `.tool-versions` | DevOps | Erlang/Elixir version pinning |
| `.envrc` | DevOps | direnv environment setup |

### TAIEA Project (`taiea/`)

| File | Purpose |
|------|---------|
| `src/` | Source code (Erlang modules) |
| `test/` | Test suites (EUnit + Common Test) |
| `priv/` | Private resources (configs, data) |
| `rebar.config` | TAIEA-specific config (overrides workspace) |
| `README.md` | TAIEA project documentation |

### Vendor (`vendor/`)

| Dir | Purpose |
|-----|---------|
| `erlmcp/` | Git submodule: MCP protocol reference |

## Build Hierarchy

```
Workspace (rebar3)
├── rebar.config              (common config, deps)
│   ├── taiea/rebar.config    (inherits, extends)
│   ├── vendor/erlmcp/...     (third-party, isolated)
```

**Rule**: `rebar3` commands at workspace root orchestrate all projects.

## Build & Test Procedures

### Development Workflow

```bash
# 1. Clone workspace
git clone <erlmcp-repo> /Users/sac/erlmcp
cd /Users/sac/erlmcp

# 2. Load environment (direnv)
direnv allow

# 3. Install Erlang/Elixir (asdf)
asdf install

# 4. Fetch dependencies
rebar3 get-deps

# 5. Compile workspace
rebar3 compile

# 6. Run tests
rebar3 ct               # Common Test
rebar3 eunit            # EUnit

# 7. Run specific project tests
rebar3 -p taiea ct
rebar3 -p taiea eunit
```

### Continuous Build (Make)

```bash
# Full pipeline (compile → test → lint)
make all

# Individual targets
make compile            # Just compile
make test              # Run all tests
make ct                # Common Test only
make eunit             # EUnit only
make lint              # Static analysis
make clean             # Remove artifacts
make distclean          # Remove all generated files
```

### Release Build

```bash
# Development release
rebar3 release

# Production release (optimized)
rebar3 as prod release

# Tarball packaging
rebar3 as prod tar

# Release location
_build/prod/rel/erlmcp/
_build/prod/erlmcp-0.1.0.tar.gz
```

## Versioning Strategy

### Workspace Version
- **File**: `rebar.config` → `relx` → `release` tuple
- **Format**: MAJOR.MINOR.PATCH (semantic versioning)
- **Current**: 0.1.0 (alpha phase)
- **Increment**: On workspace structural changes

### TAIEA Version
- **File**: `taiea/rebar.config` → `app_version` in `.app.src`
- **Format**: MAJOR.MINOR.PATCH
- **Independent**: From workspace version
- **Increment**: Per TAIEA release cycle

### erlmcp (Vendor) Version
- **File**: Git submodule pin in `.gitmodules`
- **Format**: Commit hash or git tag
- **Update**: Via `git submodule update --remote`
- **Lock**: In Git history via commit reference

### Version Bumping Procedure

```bash
# 1. Update rebar.config (workspace version)
vim rebar.config
# Edit: {release, {erlmcp, "X.Y.Z"}, [...]}.

# 2. Update TAIEA version (if applicable)
vim taiea/rebar.config
# Edit: {vsn, "X.Y.Z"} in taiea/src/taiea.app.src

# 3. Tag release
git tag -a "v0.1.0" -m "Release erlmcp v0.1.0"

# 4. Push
git push origin main --tags
```

## Dependency Management

### Workspace Dependencies (inherited by all projects)

Defined in root `rebar.config`:

```erlang
{deps, [
    {lager, "3.9.2"},
    {ranch, "2.1.1"},
    {cowboy, "2.10.0"},
    {jsx, "3.1.0"}
]}.
```

**Rule**: Common dependencies at workspace level; project-specific deps in `taiea/rebar.config`.

### Adding Dependencies

1. **Workspace-wide**: Edit `rebar.config` at root
2. **TAIEA-only**: Edit `taiea/rebar.config`
3. **Fetch**: `rebar3 get-deps`
4. **Lock**: Commit `rebar.lock` to git
5. **Test**: `rebar3 ct` (all projects)

### Vendor Dependencies (Git Submodules)

```bash
# Add erlmcp as submodule
git submodule add https://github.com/org/erlmcp.git vendor/erlmcp

# Clone with submodules
git clone --recurse-submodules <repo>

# Update submodule
cd vendor/erlmcp
git checkout <tag-or-commit>
cd ../..
git add vendor/erlmcp
git commit -m "chore: pin erlmcp to <version>"
```

## Testing Strategy

### Unit Tests (EUnit)

- **Location**: `taiea/test/*_test.erl`
- **Run**: `rebar3 eunit`
- **Purpose**: Fast, isolated behavior verification

### Integration Tests (Common Test)

- **Location**: `taiea/test/*_SUITE.erl`
- **Run**: `rebar3 ct`
- **Purpose**: Multi-module, realistic workflows

### Test Coverage

```bash
# Generate coverage report
rebar3 cover

# View report
open _build/test/cover/index.html
```

**Target**: 80%+ coverage on production code.

## Code Quality Standards

### Static Analysis

```bash
# Dialyzer (type checking)
rebar3 dialyze

# Lint (erl_lint)
rebar3 lint
```

### Style Guide

- **Formatting**: `erl_tidy` for consistency
- **Naming**: CamelCase modules, snake_case functions
- **Indentation**: 4 spaces
- **Line length**: 100 characters (soft limit)
- **Comments**: %% for module comments, % for inline

### Code Review Checklist

- [ ] Tests added (unit + integration)
- [ ] Coverage maintained (80%+)
- [ ] Documentation updated
- [ ] No compilation warnings
- [ ] Dialyzer passes
- [ ] Code style consistent
- [ ] Commit message follows convention

## Release Process

### Phases

**Phase 1: Prepare (on develop)**
1. Update versions (`rebar.config`, `.app.src`)
2. Update `CHANGELOG.md`
3. Commit: `chore: bump version to X.Y.Z`
4. Tag: `git tag -a "vX.Y.Z" -m "Release erlmcp vX.Y.Z"`

**Phase 2: Build**
1. `rebar3 clean`
2. `rebar3 as prod release`
3. `rebar3 as prod tar`
4. Verify artifacts: `_build/prod/erlmcp-X.Y.Z.tar.gz`

**Phase 3: Test**
1. Extract tarball
2. `bin/erlmcp console`
3. Run smoke tests
4. Verify no warnings/errors

**Phase 4: Publish**
1. `git push origin main --tags`
2. Create GitHub Release
3. Attach tarball
4. Document changes

## GitHub Workflows

### Automated Checks

- **Compile**: On every push (fail-fast)
- **Tests**: On every push (all tests)
- **Lint**: On every push (Dialyzer, style)
- **Coverage**: On PR (80%+ required)

### Release Workflow

- Triggered: On push to `release/*` branch
- Steps: Build → Test → Package → Publish

(Implementation by Agent 4)

## Collaboration Model

### Branches

- **main**: Stable, tested, deployable
- **develop**: Integration branch for features
- **feature/**: Feature branches (PR to develop)
- **release/**: Release preparation (PR to main)
- **hotfix/**: Urgent fixes (branch from main, PR to main + develop)

### Commit Message Convention

```
type(scope): subject

Body (optional):
- Details
- Related issues: #123

Footer (optional):
Co-Authored-By: Name <email@example.com>
```

**Types**: `feat`, `fix`, `docs`, `style`, `refactor`, `test`, `chore`

### Code Review

1. Create PR on feature branch
2. Assign reviewers
3. CI checks must pass
4. ≥2 approvals required
5. Squash and merge to develop (or main for hotfix)

## Troubleshooting

### Build Issues

```bash
# Clean and rebuild
rebar3 clean
rebar3 compile

# Verbose output
rebar3 compile -v
```

### Dependency Conflicts

```bash
# Show dependency tree
rebar3 tree

# Lock file inconsistency
rm rebar.lock
rebar3 get-deps
```

### Test Failures

```bash
# Run with verbose output
rebar3 ct -v

# Run specific suite
rebar3 ct --suite=test/taiea_executor_SUITE
```

## Support & Contact

- **Team Lead**: @taiea-maintainers
- **Issues**: GitHub Issues (repo/issues)
- **Discussions**: GitHub Discussions (repo/discussions)
- **Slack**: #erlmcp-dev (if available)

## Document Maintenance

- **Last Updated**: 2026-01-26
- **Version**: v0.1.0 (Alpha)
- **Status**: Active
- **Next Review**: 2026-02-26

---

**Remember**: This workspace is a living document. Update WORKSPACE.md as processes evolve.
