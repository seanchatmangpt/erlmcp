# ErlMCP Environment Configuration Guide

## Overview

ErlMCP uses a **profile-based configuration system** controlled by the `ERLMCP_PROFILE` environment variable. This guide explains how to select and use the four available profiles for different use cases.

## Quick Start

```bash
# Development (default - safest for humans)
make compile

# Test (for CI/CD)
export ERLMCP_PROFILE=test
make compile

# Staging (for load testing)
export ERLMCP_PROFILE=staging
make compile

# Production (for release builds)
export ERLMCP_PROFILE=prod
make compile
```

## Profile Selection via ERLMCP_PROFILE

The `ERLMCP_PROFILE` environment variable controls which configuration profile is used when building and running ErlMCP. The profile determines logging levels, timeouts, feature toggles, and resource limits.

### Setting the Profile

```bash
# Set before running any make command
export ERLMCP_PROFILE=dev

# Or inline for a single command
ERLMCP_PROFILE=test make compile
```

### How It Works

1. When you run `make compile` or `rebar3 compile`, the Makefile symlinks `config/sys.config` to the appropriate `config/sys.config.{profile}` file
2. Rebar3 reads `config/sys.config` during compilation
3. The Erlang application starts with the selected profile's configuration

## The Four Profiles

### 1. dev (Development - DEFAULT)

**Purpose**: Fast iteration, extensive logging, easy debugging

**When to use**:
- Local development on your machine
- Debugging issues
- Exploring the codebase
- Writing new features

**Characteristics**:
- Log level: `debug` (very verbose)
- Timeouts: 10 seconds (generous for debugging)
- Strict mode: `false` (lenient validation)
- Max pending requests: 500 (high limit for load testing locally)
- Caching: disabled (always fresh data)
- Rate limiting: disabled
- Debug endpoints: enabled
- File logging: `logs/erlmcp-dev.log` (50MB max)

**Example**:
```bash
# Default - no need to set ERLMCP_PROFILE
make compile
make console

# Or explicitly
export ERLMCP_PROFILE=dev
make compile
```

**Best for**: Humans iterating quickly

---

### 2. test (CI/CD Testing)

**Purpose**: Fast execution, deterministic behavior, comprehensive test coverage

**When to use**:
- Running tests locally before committing
- CI/CD pipelines (GitHub Actions, etc.)
- Automated test suites (EUnit, Common Test, Proper)
- Pre-commit hooks

**Characteristics**:
- Log level: `debug` (all logs for test verification)
- Timeouts: 1 second (fast fail for quick feedback)
- Strict mode: `true` (catch validation errors)
- Max pending requests: 100 (test isolation)
- Caching: disabled (deterministic results)
- Rate limiting: disabled (no test interference)
- Async processing: disabled (synchronous for determinism)
- Logging: console/stderr only (no files, for CI capture)
- Random ports: enabled (avoid conflicts in parallel tests)

**Example**:
```bash
export ERLMCP_PROFILE=test
make check  # Run all quality gates
rebar3 eunit
rebar3 ct
```

**Best for**: Automated testing, CI/CD pipelines

---

### 3. staging (Pre-Production Testing)

**Purpose**: Production-like behavior with debugging capabilities

**When to use**:
- Load testing
- Performance benchmarking
- Integration testing with real services
- QA validation
- Chaos engineering experiments

**Characteristics**:
- Log level: `info` (moderate logging)
- Timeouts: 5 seconds (production-like)
- Strict mode: `true`
- Max pending requests: 250
- Caching: enabled (TTL 30 minutes)
- Rate limiting: enabled (500 req/s)
- Async processing: enabled
- Debug endpoints: enabled (for testing)
- File logging: `/var/logs/erlmcp-staging.log` (100MB max, compressed rotation)

**Example**:
```bash
export ERLMCP_PROFILE=staging
make compile
./scripts/bench/run_all_benchmarks.sh
```

**Best for**: Load testing, performance validation, chaos engineering

---

### 4. prod (Production Release)

**Purpose**: Hardened production deployment, minimal logging, maximum performance

**When to use**:
- Production releases
- Production releases only
- Do not use for development

**Characteristics**:
- Log level: `error` (minimal logging)
- Timeouts: 5 seconds (strict)
- Strict mode: `true` (no lenient parsing)
- Max pending requests: 100 (bounded for stability)
- Caching: enabled (TTL 1 hour)
- Rate limiting: enabled (1000 req/s)
- Async processing: enabled
- Debug endpoints: disabled (security)
- File logging: `/var/logs/erlmcp-prod.log` (100MB max, compressed rotation)
- Experimental features: disabled

**Example**:
```bash
export ERLMCP_PROFILE=prod
make compile
rebar3 as prod release
```

**Best for**: Production deployments, official releases

---

## Profile Comparison Matrix

| Feature | dev | test | staging | prod |
|---------|-----|------|---------|------|
| **Log Level** | debug | debug | info | error |
| **Strict Mode** | false | true | true | true |
| **Timeout** | 10s | 1s | 5s | 5s |
| **Max Pending Requests** | 500 | 100 | 250 | 100 |
| **Caching** | disabled | disabled | enabled | enabled |
| **Rate Limiting** | disabled | disabled | enabled | enabled |
| **Debug Endpoints** | enabled | enabled | enabled | disabled |
| **File Logging** | logs/ | none | /var/logs/ | /var/logs/ |
| **Async Processing** | disabled | disabled | enabled | enabled |
| **Experimental Features** | enabled | enabled | enabled | disabled |
| **Primary Use Case** | Development | CI/CD | Load testing | Production |

## Troubleshooting

### How do I know what profile is active?

Check the symlink:
```bash
ls -l config/sys.config
# Output: config/sys.config -> sys.config.dev
```

Or check the environment variable:
```bash
echo $ERLMCP_PROFILE
# Output: dev (or empty, which defaults to dev)
```

### I changed ERLMCP_PROFILE but nothing happened

You need to recompile after changing the profile:
```bash
export ERLMCP_PROFILE=staging
make clean compile  # Full rebuild
```

The symlink is created during the compilation step by the Makefile.

### Tests are failing with timeout errors

Make sure you're using the test profile:
```bash
export ERLMCP_PROFILE=test
make check
```

The test profile has shorter timeouts (1s) for fast feedback.

### I want more verbose logging in production

**Don't do this.** Changing log levels in production can:
- Overwhelm disk I/O
- Expose sensitive data
- Degrade performance

Instead:
1. Reproduce the issue locally with `ERLMCP_PROFILE=dev`
2. Use tracing/observability tools (OpenTelemetry)
3. Enable debug logging temporarily on a single node

### Which profile should I use for benchmarking?

Use `staging` for benchmarks:
```bash
export ERLMCP_PROFILE=staging
make compile
./scripts/bench/run_all_benchmarks.sh
```

The staging profile has production-like behavior (caching, rate limiting) but with debug capabilities.

### Can I create custom profiles?

Yes! Create a new file:
```bash
cp config/sys.config.dev config/sys.config.custom
# Edit config/sys.config.custom
export ERLMCP_PROFILE=custom
make compile
```

The Makefile will automatically symlink to `config/sys.config.{ERLMCP_PROFILE}` if it exists.

## Configuration Files Reference

### Profile-Specific Configs

Located in `config/`:

- `sys.config.dev` - Development profile (3.9KB)
- `sys.config.test` - Test profile (3.8KB)
- `sys.config.staging` - Staging profile (4.9KB)
- `sys.config.prod` - Production profile (6.0KB)

### Symlink Target

- `sys.config` - Symlink to the active profile (created by Makefile)

### Specialized Configs

- `cluster.config` - Mnesia cluster configuration
- `dashboard.config` - Dashboard server configuration
- `monitor.config` - Monitoring/observability configuration
- `vm.args` - Erlang VM arguments

## Environment Variables (.env files)

In addition to `ERLMCP_PROFILE`, you can use `.env` files for secrets and deployment-specific settings:

- `dev.env` - Local development environment variables
- `staging.env` - Staging environment variables
- `production.env` - Production environment variables

**Example** (`dev.env`):
```bash
ERLMCP_DB_HOST=localhost
ERLMCP_DB_PORT=5432
ERLMCP_DB_NAME=erlmcp_dev
ERLMCP_JWT_SECRET=dev_secret_not_for_production
```

Load with:
```bash
source config/dev.env
export ERLMCP_PROFILE=dev
make compile
```

## Best Practices

### 1. Always set ERLMCP_PROFILE in CI/CD

```yaml
# .github/workflows/ci.yml
env:
  ERLMCP_PROFILE: test

jobs:
  test:
    steps:
      - run: make check
```

### 2. Use dev for local development (default)

```bash
# No need to set anything
make compile
make console
```

### 3. Use staging for benchmarks

```bash
export ERLMCP_PROFILE=staging
make benchmark-quick
```

### 4. Use prod only for releases

```bash
export ERLMCP_PROFILE=prod
make clean compile
rebar3 as prod release
rebar3 as prod tar
```

### 5. Document custom profiles

If you create custom profiles, document them in this guide.

## Related Documentation

- [CONFIGURATION_GUIDE.md](CONFIGURATION_GUIDE.md) - Comprehensive configuration reference
- [DEVELOPMENT.md](../DEVELOPMENT.md) - Development workflow
- [CLAUDE.md](../CLAUDE.md) - System architecture and OTP patterns
- [CLI_USAGE.md](../CLI_USAGE.md) - Command-line tools

---

**Last Updated**: February 2026 (v2.1.0)
**Maintainer**: erlmcp core team
