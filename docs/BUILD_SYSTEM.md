# Build System Guide

**Estimated read time: 10 minutes**

This document describes the erlmcp build system, performance targets, and how to use Make and Rebar3 effectively.

## Overview

The erlmcp build system is orchestrated by:
- **Makefile**: High-level targets, workspace coordination, SLO enforcement
- **rebar3**: Erlang/OTP standard build tool
- **rebar.config**: Build configuration (dependencies, plugins, compiler options)

## Quick Reference

```bash
# Development (daily use)
make build                    # Compile erlmcp + taiea
make test                     # Run all tests
make lint                     # Static analysis
make check                    # Full validation (build + lint + test)

# Workspace targets
make workspace-build          # Build entire workspace
make workspace-test           # Test entire workspace
make workspace-check          # Full workspace validation
make workspace-release        # Production releases

# Help
make help                     # List all targets
```

## Build Targets by Category

### Workspace Targets (Both erlmcp + taiea)

| Target | Purpose | Time | SLO |
|--------|---------|------|-----|
| `workspace-build` | Compile entire workspace | ~10s | <15s |
| `workspace-test` | All tests (unit + integration + property) | ~60s | <90s |
| `workspace-lint` | Static analysis for entire workspace | ~45s | <60s |
| `workspace-check` | Full validation (build + lint + test) | ~115s | <150s |
| `workspace-clean` | Remove all build artifacts | ~5s | N/A |
| `workspace-release` | Production releases for both systems | ~120s | <180s |

### erlmcp Application Targets

#### Compilation
| Target | Purpose | Time | SLO |
|--------|---------|------|-----|
| `compile` | Quick compile (incremental) | ~2s | <5s |
| `build` | Full compile erlmcp | ~5s | <15s |
| `clean` | Soft clean (keeps _build) | ~1s | N/A |
| `distclean` | Hard clean (full removal) | ~2s | N/A |

#### Testing
| Target | Purpose | Time | SLO |
|--------|---------|------|-----|
| `test-unit` | EUnit tests only | ~5s | <10s |
| `test-integration` | Common Test (CT) only | ~15s | <25s |
| `test-property` | PropEr property-based tests | ~10s | <20s |
| `test` | All tests (unit + CT + property) | ~30s | <45s |
| `coverage-report` | Generate coverage metrics | ~40s | <60s |

#### Code Quality
| Target | Purpose | Time | SLO |
|--------|---------|------|-----|
| `xref` | Cross-reference analysis | ~10s | <15s |
| `dialyzer` | Type checking | ~20s | <30s |
| `lint` | xref + dialyzer combined | ~30s | <45s |
| `format` | Auto-format code | ~5s | <10s |
| `check` | Full validation (build + lint + test) | ~65s | <90s |

#### Release
| Target | Purpose | Time | SLO |
|--------|---------|------|-----|
| `release` | Production release (erlmcp only) | ~60s | <90s |
| `workspace-release` | Production releases (erlmcp + taiea) | ~120s | <180s |

#### Development & Debugging
| Target | Purpose | Notes |
|--------|---------|-------|
| `console` | Interactive Erlang shell | With erlmcp loaded |
| `dev-console` | Dev shell with sys.config | Full config loaded |
| `observer` | Erlang Observer GUI | System debugging |
| `profile` | Performance profiling | Recon tracing |

### Detailed Target Descriptions

#### `make build` - Compile erlmcp
```bash
make build
```
Compiles erlmcp source code to _build/default/lib/erlmcp/ebin/.
- Incremental: recompiles only changed files
- Full compile: removes _build/default and rebuilds from scratch
- Includes dependency compilation

**When to use**: After editing source files, before testing

#### `make test` - Run All Tests
```bash
make test
```
Runs three test suites:

1. **EUnit** (unit tests): ~5s
   - Tests: `test/*_tests.erl`
   - Fast feedback
   - Tests individual functions

2. **Common Test** (integration): ~15s
   - Tests: `test/*_SUITE.erl`
   - Tests module interactions
   - End-to-end workflows

3. **PropEr** (property-based): ~10s
   - Tests: `test/properties_*_erl`
   - Generates random inputs
   - Tests invariants

**When to use**: Before committing, after significant changes

#### `make lint` - Static Analysis
```bash
make lint
```
Runs two static analysis tools:

1. **xref** (~10s): Cross-reference analysis
   - Finds undefined function calls
   - Detects unused code
   - Reports module dependencies

2. **dialyzer** (~20s): Type checker
   - Infers types from code
   - Finds type errors
   - Reports discrepancies

**When to use**: Before commits, as part of CI/CD

#### `make check` - Full Validation
```bash
make check
```
Complete validation pipeline:
1. Compile (build)
2. Static analysis (lint)
3. Run tests (test)

**Result**: Confidence that code is ready for production

**When to use**: Before committing, before releases

#### `make console` - Interactive Shell
```bash
make console
```
Starts Erlang shell with erlmcp loaded:

```erlang
(erlmcp@hostname)1> erlmcp_server:start_link({stdio, []}, #{}).
{ok, <0.123.0>}
(erlmcp@hostname)2>
```

Useful for:
- Testing modules interactively
- Debugging issues
- Exploring APIs
- Development prototyping

#### `make observer` - GUI Debugger
```bash
make observer
```
Launches Erlang Observer GUI showing:
- Running processes
- Module details
- System statistics
- Call tracing
- Memory usage

## Makefile Structure

The Makefile organizes targets into sections:

```makefile
# Section 1: Help
help                          # Show all targets

# Section 2: Setup
setup                         # One-time initialization

# Section 3: Compilation
compile                       # Incremental compile
build                         # Full erlmcp build

# Section 4: Testing
test-unit                     # Unit tests only
test-integration              # Integration tests only
test-property                 # Property-based tests
test                          # All tests
coverage-report               # Generate coverage

# Section 5: Code Quality
xref                          # Cross-reference analysis
dialyzer                      # Type checking
lint                          # Combined analysis
format                        # Auto-format code
check                         # Full validation

# Section 6: Release
release                       # Build production release
workspace-release             # Build both erlmcp + taiea

# Section 7: Workspace (both erlmcp + taiea)
workspace-build               # Build entire workspace
workspace-test                # Test entire workspace
workspace-lint                # Lint entire workspace
workspace-check               # Full workspace check
workspace-clean               # Clean workspace

# Section 8: Development
console                       # Interactive shell
dev-console                   # Dev shell with config
observer                      # GUI debugging
profile                       # Performance profiling

# Section 9: Maintenance
clean                         # Soft clean
distclean                     # Hard clean
```

## Performance Targets (SLOs)

All build targets have performance targets (Service Level Objectives):

### Compilation SLOs
- **Incremental compile**: <5 seconds
- **Full compile**: <15 seconds
- **Workspace compile**: <20 seconds

### Test SLOs
- **Unit tests**: <10 seconds
- **Integration tests**: <25 seconds
- **Property-based tests**: <20 seconds
- **All tests combined**: <45 seconds
- **Workspace tests**: <90 seconds

### Lint SLOs
- **xref**: <15 seconds
- **dialyzer**: <30 seconds
- **Combined lint**: <45 seconds

### Release SLOs
- **Development release**: <60 seconds
- **Production release**: <90 seconds
- **Workspace release**: <180 seconds

### Full Validation SLOs
- **erlmcp check** (build + lint + test): <90 seconds
- **workspace check**: <150 seconds

## Development Workflow

### Daily Development Loop

```bash
# 1. Make changes to src/*.erl or test/*.erl

# 2. Quick test (fast feedback)
make test-unit                    # ~5s

# 3. Full test before commit
make test                         # ~30s

# 4. Check code quality
make lint                         # ~30s

# 5. Final validation
make check                        # ~65s

# 6. Build release
make release                      # ~60s

# 7. Commit with confidence
git add .
git commit -m "feat: describe change"
```

### Parallel Development (Multiple Branches)

Use separate build directories:

```bash
# Branch 1: feature/new-transport
rebar3 as dev1 compile

# Branch 2: feature/better-errors
rebar3 as dev2 compile

# Compare outputs
diff -r _build/dev1/lib _build/dev2/lib
```

## rebar3 Commands Reference

While Makefile provides convenience targets, you can use rebar3 directly:

```bash
# Dependency management
rebar3 get-deps              # Fetch dependencies
rebar3 update               # Update dependencies
rebar3 deps                 # List dependencies
rebar3 tree                 # Show dependency tree
rebar3 unlock               # Unlock versions

# Compilation
rebar3 compile              # Compile (incremental)
rebar3 clean                # Clean _build/default
rebar3 as prod release      # Production release

# Testing
rebar3 eunit                # EUnit tests
rebar3 ct                   # Common Test
rebar3 proper               # PropEr property tests

# Static analysis
rebar3 xref                 # Cross-reference
rebar3 dialyzer             # Type checking

# Code quality
rebar3 format               # Auto-format
rebar3 lint                 # All linting

# Profiling & Debugging
rebar3 shell                # Interactive shell
rebar3 ct --verbose         # Verbose output
```

## Compiler Flags & Configuration

Configuration is in `rebar.config`:

```erlang
% Compiler options for all profiles
{erl_opts, [
    debug_info,             % Include debug info
    {parse_transform, lager_transform},
    warnings_as_errors      % Treat warnings as errors
]}.

% Specific to 'prod' profile
{profiles, [
    {prod, [
        {erl_opts, [
            {parse_transform, lager_transform},
            warnings_as_errors,
            {lager_truncation_size, 1024}
        ]}
    ]}
]}.
```

## Environment Variables

Set in `.envrc` or manually:

```bash
# Workspace home
export ERLMCP_HOME=/path/to/erlmcp

# Erlang library paths
export ERL_LIBS="$PWD/_build/default/lib:$PWD/taiea/_build/default/lib"

# VM arguments
export ERL_FLAGS="+P 262144 +K true"
```

## Troubleshooting Build Issues

### "rebar3 not found"
```bash
brew install rebar3              # macOS
apt-get install rebar3           # Ubuntu/Debian
```

### "Compilation warning: expression results are not used"
```erlang
% Add compile directive to suppress:
-compile({nowarn_unused_function, [my_fun/0]}).
```

### "Type error not found"
Run dialyzer separately:
```bash
rebar3 dialyzer --verbose
```

### "Test timeout"
Increase timeout in rebar.config:
```erlang
{ct_opts, [
    {ct_hooks, []},
    {ct_runtime, 600}     % 10 minutes
]}.
```

### Tests fail locally but pass on CI
- Clean build: `make distclean && make build && make test`
- Check environment: `env | grep ERL`
- Verify Erlang version: `erl -version`

## CI/CD Integration

For GitHub Actions, use:

```yaml
- name: Build
  run: make workspace-build
  timeout-minutes: 1

- name: Test
  run: make workspace-test
  timeout-minutes: 2

- name: Lint
  run: make workspace-lint
  timeout-minutes: 1
```

All commands fail fast on error (exit code 1).

## Performance Profiling

### Find bottlenecks:
```bash
make profile
```

This uses recon tracing to identify slow functions.

### Memory profiling:
```bash
make console

% In shell:
observer:start().
```

Use Observer to inspect memory usage.

## Next Steps

- **To develop**: See [FOR_DEVELOPERS.md](FOR_DEVELOPERS.md)
- **To deploy**: See [DEPLOYMENT.md](DEPLOYMENT.md)
- **To understand architecture**: See [ARCHITECTURE_OVERVIEW.md](ARCHITECTURE_OVERVIEW.md)

---

**Last Updated**: 2026-01-26
**Makefile Version**: 3.0
**rebar3 Version**: 3.22+
