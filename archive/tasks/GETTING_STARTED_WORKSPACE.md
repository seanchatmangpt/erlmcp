# Getting Started with erlmcp + TAIEA Workspace

**Workspace**: Integrated erlmcp and TAIEA systems
**Date**: 2026-01-26
**Status**: Integration Complete - Ready for Development

---

## Quick Start (5 minutes)

### 1. Clone with Submodules

```bash
# If cloning fresh
git clone --recurse-submodules <REPO_URL> erlmcp
cd erlmcp

# If already cloned without submodules
git submodule update --init --recursive
```

### 2. Install Dependencies

```bash
# Option A: Using asdf (recommended)
asdf install                           # Installs Erlang/OTP from .tool-versions
direnv allow                           # Load environment

# Option B: Manual
# Requires: Erlang/OTP 24+, Rebar3
brew install erlang rebar3             # macOS
apt-get install erlang rebar3          # Linux

# Verify installation
erl -version
rebar3 --version
```

### 3. Build Workspace

```bash
# Compile erlmcp + TAIEA
make workspace-build

# Or compile separately
make compile                           # erlmcp only
make taiea-compile                    # TAIEA only
```

### 4. First Tests

```bash
# Quick smoke tests (< 10 seconds)
make test-quick

# Full test suite (takes longer, has some failures)
make test                              # Runs eunit + ct

# Or test separately
make test-unit                         # EUnit only (< 2 min)
make test-int                          # Integration tests (< 5 min)
```

### 5. First Deployment Simulation

```bash
# Dry run (no changes)
# Note: Replace gcp-deploy.sh with actual script when ready
ls -la tools/                          # See available scripts

# Development release
make release-dev

# Production release
make release-prod

# Check artifacts
make show-release
```

---

## Directory Structure

```
erlmcp/
├── src/                                # erlmcp source code (30+ modules)
├── test/                              # Test suites (21 files)
├── taiea/                             # TAIEA umbrella application
│   ├── apps/                          # 12 TAIEA applications
│   ├── config/                        # TAIEA configuration
│   ├── dist/                          # Release distributions
│   └── rebar.config                   # TAIEA build config
├── examples/                          # Example applications (4)
├── config/                            # Erlang system configuration
├── ontology/                          # RDF ontologies
├── shapes/                            # SHACL shape definitions
├── sparql/                            # SPARQL queries
├── gcp/                               # GCP Terraform infrastructure
├── .github/workflows/                 # CI/CD workflows (8)
├── Dockerfile                         # Production container
├── Dockerfile.dev                     # Development container
├── docker-compose.yml                 # Multi-container setup
├── Makefile                           # Build automation
├── rebar.config                       # Build configuration
└── docs/                              # Documentation (20+ files)
```

---

## Essential Commands

### Build & Compile

```bash
# Workspace (erlmcp + TAIEA)
make workspace-build                   # Build both
make workspace-check                   # Build + test + lint

# Erlmcp only
make compile                           # Compile
make check                             # Compile + test + lint

# TAIEA only
make taiea-compile                    # Compile TAIEA
make taiea-test                       # Test TAIEA
```

### Testing

```bash
# Unit tests (fast)
make test-unit                         # < 2 min
make test-quick                        # < 10 sec (smoke tests)

# Integration tests
make test-int                          # < 5 min

# All tests
make test                              # eunit + ct

# With verbose output
make test-verbose                      # Detailed results

# Coverage analysis
make test-coverage                     # Generate coverage report
```

### Code Quality

```bash
# Linting
make lint                              # Static analysis

# Type checking
make dialyze                           # Dialyzer type checker

# Both
make quality                           # lint + dialyze

# Full workspace validation
make workspace-lint
```

### Releases & Deployment

```bash
# Development release
make release-dev
Output: _build/default/rel/erlmcp/

# Production release
make release-prod
Output: _build/prod/rel/erlmcp/

# Create tarball
make tar
Output: _build/prod/erlmcp-VERSION.tar.gz

# Show release artifacts
make show-release
```

### Docker

```bash
# Build images
docker-compose build

# Run services
docker-compose up

# Run specific service
docker-compose up erlmcp
docker-compose up taiea

# Stop services
docker-compose down
```

### Information

```bash
# Workspace info
make info

# Dependency tree
make deps-tree

# Help (all targets)
make help
```

---

## Development Workflow

### 1. Starting Development

```bash
# 1. Set up environment
cd erlmcp
direnv allow                           # Load .envrc
asdf install                           # Install tools

# 2. Compile and run tests
make compile
make test-unit

# 3. Check code quality
make quality

# 4. View documentation
open docs/architecture.md              # Read architecture
open docs/api-reference.md             # Read API docs
```

### 2. Making Changes

```bash
# 1. Make code changes
vim src/erlmcp_server.erl

# 2. Compile
make compile

# 3. Run related tests
make test-unit -module erlmcp_server_tests

# 4. Run full test suite
make test

# 5. Check quality
make quality

# 6. Commit when green
git add src/erlmcp_server.erl
git commit -m "feat: Add feature X"
```

### 3. Adding Tests

```bash
# 1. Create test file
vim test/new_feature_tests.erl

# 2. Use EUnit framework
-module(new_feature_tests).
-include_lib("eunit/include/eunit.hrl").

simple_test() ->
    ?assertEqual(expected, actual).

# 3. Run tests
rebar3 eunit -module new_feature_tests

# 4. Add to version control
git add test/new_feature_tests.erl
git commit -m "test: Add new_feature tests"
```

### 4. Integration Testing

```bash
# Write integration test in test/*_SUITE.erl
vim test/feature_integration_SUITE.erl

# Use Common Test framework
-module(feature_integration_SUITE).
-include_lib("common_test/include/ct.hrl").

feature_integration_test(Config) ->
    Server = proplists:get_value(server, Config),
    % Test implementation
    ok.

# Run CT suites
rebar3 ct -suite test/feature_integration_SUITE

# Or all CT suites
rebar3 ct
```

---

## Common Tasks

### Running Specific Tests

```bash
# EUnit single module
rebar3 eunit -module erlmcp_server_tests

# EUnit single function
rebar3 eunit -module erlmcp_server_tests -function simple_test

# Common Test single suite
rebar3 ct -suite test/integration_SUITE

# Common Test specific test case
rebar3 ct -suite test/integration_SUITE -case feature_test
```

### Debugging

```bash
# Compile with debug output
rebar3 compile -vv

# Run tests with debug output
rebar3 eunit -v

# Common Test verbose
rebar3 ct -v

# Erlang shell with application started
rebar3 shell
erl> application:start(erlmcp).
erl> erlmcp:start_server().
```

### Performance Testing

```bash
# Run performance benchmarks
make test-perf

# Load testing (requires load_test_SUITE)
rebar3 ct -suite test/load_test_SUITE

# Coverage with timing
make test-coverage
```

### Cleaning Up

```bash
# Clean build artifacts
make clean

# Complete clean (including docs, covers)
make distclean

# Clean TAIEA as well
make workspace-clean
```

---

## Troubleshooting

### Compilation Errors

```bash
# Problem: "Module X not found"
# Solution: Run dependency update
rebar3 update

# Problem: "Conflicting dependencies"
# Solution: Lock files
rebar3 upgrade --all

# Problem: "Build stuck"
# Solution: Full clean
make distclean
rebar3 unlock -a
make compile
```

### Test Failures

```bash
# Problem: "Test timeout"
# Solution: Run with increased timeout
rebar3 eunit --timeout 60000

# Problem: "Assertion failures"
# Solution: Run with verbose output
rebar3 eunit -v

# Problem: "Setup/teardown issues"
# Solution: Check test file setup/cleanup functions
# Look in test/*.erl for {setup, fun() -> state, cleanup_fun, test_fun}
```

### TAIEA Issues

```bash
# Problem: "TAIEA compilation fails"
cd taiea
rebar3 compile
cd ..

# Problem: "TAIEA tests fail"
cd taiea
rebar3 eunit
cd ..

# Problem: "Integration tests fail"
# Check erlmcp_taiea_integration_SUITE.erl
rebar3 ct -suite test/erlmcp_taiea_integration_SUITE
```

### Docker Issues

```bash
# Problem: "Container fails to start"
# Solution: Check logs
docker-compose logs erlmcp

# Problem: "Port already in use"
# Solution: Modify docker-compose.yml ports section
vim docker-compose.yml
# Change port mapping

# Problem: "Image build fails"
# Solution: Clean and rebuild
docker-compose build --no-cache
```

---

## Configuration

### Build Configuration

**File**: `rebar.config`

Key settings:
```erlang
{deps, [...]}.              % Dependencies
{erl_opts, [...]}.          % Erlang compiler options
{profiles, [...]}.          % Build profiles (dev, test, prod)
{plugins, [...]}.           % Rebar3 plugins
{cover_enabled, true}.      % Code coverage
```

### Erlang System Configuration

**File**: `config/sys.config`

Controls:
- Application startup
- Logger configuration
- Transport settings
- HTTP/TCP listener ports

### Environment Variables

**File**: `.envrc`

Sets:
- PATH adjustments
- Erlang environment variables
- Development tool paths

---

## TAIEA Integration

### TAIEA Applications

TAIEA includes 12 applications:

```erlang
taiea_core        % Core framework
taiea_gates       % Sequential gates
taiea_governor    % Request governor
taiea_transport   % Protocol transport
taiea_http        % HTTP binding
taiea_tcp         % TCP binding
taiea_workflow    % Workflow engine
taiea_metrics     % Metrics collection
taiea_observability % OpenTelemetry
taiea_cli         % Command-line interface
taiea_docs        % Documentation
taiea_examples    % Reference examples
```

### Using TAIEA in erlmcp

```erlang
% In erlmcp_taiea_adapter.erl
% TAIEA protocol adaptation to MCP

% Start TAIEA in your app
application:start(taiea_core),
application:start(taiea_transport),

% Use TAIEA gates for request handling
taiea_gates:create_sequential(
    [gate1, gate2, gate3],
    HandlerFun
)
```

### Testing TAIEA Integration

```bash
# Run TAIEA tests
make taiea-test

# Run TAIEA + erlmcp integration
rebar3 ct -suite test/erlmcp_taiea_integration_SUITE

# Full workspace test
make workspace-test
```

---

## Deployment

### Local Development

```bash
# 1. Build development release
make release-dev

# 2. Start Erlang shell with app
./_build/default/rel/erlmcp/bin/erlmcp console

# 3. In another terminal, test
curl http://localhost:8080/health
```

### Docker Deployment

```bash
# 1. Build image
docker build -t erlmcp:latest .

# 2. Run container
docker run -p 8080:8080 erlmcp:latest

# 3. Or use docker-compose
docker-compose up -d

# 4. Check health
curl http://localhost:8080/health
```

### GCP Deployment

```bash
# 1. Validate infrastructure
cd gcp/
terraform plan

# 2. Apply infrastructure
terraform apply

# 3. Deploy services (when CI/CD configured)
# GitHub Actions will auto-deploy from main branch
```

### Production Release

```bash
# 1. Build production release
make release-prod

# 2. Create tarball
make tar

# 3. Upload to server
scp _build/prod/erlmcp-1.0.0.tar.gz user@server:/opt/

# 4. Extract and run
tar -xzf erlmcp-1.0.0.tar.gz
./erlmcp/bin/erlmcp start
```

---

## Key Files Reference

### Must Read

1. **README.md** - Project overview
2. **DEVELOPMENT.md** - Development setup
3. **docs/architecture.md** - System architecture
4. **docs/api-reference.md** - API documentation

### Configuration

1. **rebar.config** - Build system configuration
2. **config/sys.config** - Erlang system configuration
3. **Makefile** - Build automation
4. **.github/workflows/** - CI/CD pipelines

### Source Code Structure

1. **src/erlmcp.erl** - Main application module
2. **src/erlmcp_server.erl** - Server implementation
3. **src/erlmcp_transport.erl** - Transport interface
4. **src/erlmcp_taiea_adapter.erl** - TAIEA integration

### Testing

1. **test/erlmcp_server_tests.erl** - Server tests
2. **test/erlmcp_taiea_integration_SUITE.erl** - TAIEA integration
3. **test/integration_SUITE.erl** - End-to-end tests
4. **test/test_utils.erl** - Test utilities

### Documentation

1. **docs/architecture.md** - Architecture guide
2. **docs/api-reference.md** - API reference
3. **DOCKER_GUIDE.md** - Docker deployment
4. **CONTRIBUTING.md** - Contribution guidelines

---

## Next Steps

1. **Read Architecture** (15 min)
   - `docs/architecture.md` - Understand system design
   - `docs/otp-patterns.md` - Learn OTP patterns used

2. **Run Examples** (10 min)
   - `examples/simple/` - Simple implementation
   - `examples/calculator/` - More complex example
   - `examples/weather/` - Real-world example

3. **Fix Test Failures** (1-2 hours)
   - Debug 11 EUnit failures in erlmcp_server_tests
   - Run: `make test-debug` for verbose output
   - See INTEGRATION_VERIFICATION_REPORT.md for details

4. **Explore TAIEA Integration** (30 min)
   - Read TAIEA documentation
   - Run TAIEA tests: `make taiea-test`
   - Run integration suite: `rebar3 ct -suite test/erlmcp_taiea_integration_SUITE`

5. **Set Up Development Environment** (10 min)
   - Configure IDE with Erlang plugin
   - Set up edoc generation
   - Configure debugging in IDE

---

## Resources

### Documentation
- **Architecture**: `docs/architecture.md`
- **API Reference**: `docs/api-reference.md`
- **OTP Patterns**: `docs/otp-patterns.md`
- **Docker Guide**: `DOCKER_GUIDE.md`
- **Contribution**: `CONTRIBUTING.md`

### External Resources
- **Erlang Documentation**: https://www.erlang.org/doc/
- **OTP Design Principles**: https://www.erlang.org/doc/design_principles/
- **Rebar3 Documentation**: https://rebar3.readme.io/
- **Common Test Guide**: https://www.erlang.org/doc/apps/common_test/

### Getting Help

1. Check **CONTRIBUTING.md** for development guidelines
2. Review existing **examples/** for patterns
3. Search **docs/** for technical details
4. Read test files in **test/** for usage examples
5. Check **CHANGELOG.md** for recent changes

---

## Development Checklist

Before committing code:

- [ ] Code compiles without errors: `make compile`
- [ ] All tests pass: `make test`
- [ ] Code quality OK: `make quality`
- [ ] Documentation updated: `docs/` or README
- [ ] New tests added for new features
- [ ] CHANGELOG.md updated

Before merging to main:

- [ ] Full test suite passes: `make workspace-check`
- [ ] CI/CD workflows succeed (GitHub Actions)
- [ ] Code review completed
- [ ] Release notes prepared (if version bump)

---

**Happy Coding!**

For questions, check the docs or review example code in `examples/`.

