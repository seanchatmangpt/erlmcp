# Development Guide - erlmcp Workspace

This guide covers setting up and working with the erlmcp workspace, which coordinates two systems:
- **erlmcp**: Core Model Context Protocol implementation
- **taiea**: Autonomic system governance and deterministic receipts

## Prerequisites

- **Erlang/OTP 28.3.1+** (required - OTP 28+ features exclusive)
- **rebar3** (package manager and build tool)
- **git** (version control)
- Optional but recommended:
  - **direnv** (automatic environment setup)
  - **asdf** (version manager for Erlang)
  - **Observer** (Erlang GUI debugger)

### Installation

**macOS with Homebrew:**
```bash
brew install erlang rebar3 direnv
```

**Ubuntu/Debian:**
```bash
sudo apt-get install erlang rebar3 direnv
```

**Using asdf (version-agnostic):**
```bash
asdf plugin add erlang
asdf plugin add rebar
asdf install                    # Reads .tool-versions
```

## Quick Start

### 1. Clone and Setup

```bash
git clone https://github.com/banyan-platform/erlmcp.git
cd erlmcp
make setup                      # One-time initialization
```

### 2. Enable direnv (Recommended)

```bash
direnv allow                    # Enable automatic environment loading
# Now .envrc will load automatically when entering directory
```

### 3. Build and Test

```bash
make workspace-build            # Compile erlmcp + taiea
make workspace-test             # Run all tests
make workspace-check            # Full validation (build + lint + test)
```

## Development Workflow

### Daily Development Loop

```bash
# 1. Start session with environment loaded
cd /path/to/erlmcp
direnv allow                    # First time only

# 2. Make changes to source code
# Edit src/*.erl or taiea/apps/*/src/*.erl

# 3. Quick compilation
make build

# 4. Run tests (choose based on what changed)
make test-unit                  # Fast feedback (~5s)
make test                       # Full suite (~30s)
make workspace-test             # Both erlmcp + taiea (~60s)

# 5. Check code quality before commit
make lint                       # Static analysis (xref + dialyzer)
make check                      # Full validation

# 6. Commit with confidence
git add .
git commit -m "feat: describe change"
```

### Interactive Development

**Start Erlang shell with code loaded:**
```bash
make console                    # Basic shell with erlmcp loaded
make dev-console                # Dev shell with sys.config

# In the shell:
(erlmcp@hostname)1>
# You can now test code interactively
```

**Launch Observer for visual debugging:**
```bash
make observer                   # Opens Erlang Observer GUI
```

Observer shows:
- Running processes
- Module details
- System statistics
- Call tracing

## Testing

### Test Targets

```bash
# Unit tests (eunit) - ~5s
make test-unit

# Integration tests (Common Test) - ~15s
make test-integration

# Property-based tests (PropEr) - ~10s
make test-property

# All tests together - ~30s
make test

# With coverage analysis - ~40s
make test && make coverage-report
```

### Running Specific Tests

```bash
# Run a specific test file
rebar3 eunit --module=erlmcp_transport_tcp_tests

# Run a specific test in Common Test
rebar3 ct --suite=erlmcp_SUITE

# Run with verbose output
rebar3 ct -v

# Run only workspace tests
make workspace-test
```

### Coverage Analysis

```bash
# Generate coverage report
make coverage-report

# View HTML report
open _build/test/cover/index.html
```

Coverage data is generated in `_build/test/cover/`.

## Code Quality

### Static Analysis

**Cross-reference analysis** (finds undefined calls, unused code):
```bash
make xref
```

**Type checking** (Dialyzer):
```bash
make dialyzer
```

**Combined lint:**
```bash
make lint                       # Runs both xref + dialyzer
```

### Code Formatting

**Auto-format code:**
```bash
make format
```

Configuration in `rebar.config`:
```erlang
{format, [
    {files, ["src/*.erl", "test/*.erl", "examples/*.erl"]},
    {formatter, default_formatter},
    {encoding, utf8},
    {paper, 100},
    {ribbon, 90}
]}.
```

## Release Building

### Development Release

```bash
# Build without including ERTS (uses system Erlang)
rebar3 release -n dev
# Output: _build/dev/rel/erlmcp/
```

### Production Release

```bash
# Build with embedded ERTS (self-contained)
make release
# or
rebar3 as prod release

# Output: _build/prod/rel/erlmcp/
```

### Workspace Release (both erlmcp + taiea)

```bash
make workspace-release          # Builds both systems
# Creates:
#   _build/prod/rel/erlmcp/
#   taiea/_build/prod/rel/taiea/
```

### Running a Release

```bash
# Start release
_build/prod/rel/erlmcp/bin/erlmcp start

# Interactive console
_build/prod/rel/erlmcp/bin/erlmcp console

# Stop release
_build/prod/rel/erlmcp/bin/erlmcp stop

# Status check
_build/prod/rel/erlmcp/bin/erlmcp pid
```

## Project Structure

```
erlmcp/
├── Makefile                 # Workspace build orchestration
├── rebar.config             # Root configuration
├── rebar.lock              # Dependency lock file
├── .envrc                  # direnv configuration
├── .tool-versions          # asdf version config
│
├── src/                    # erlmcp source code
│   ├── erlmcp.app.src      # Application definition
│   ├── erlmcp_server.erl   # MCP server implementation
│   ├── erlmcp_client.erl   # MCP client implementation
│   ├── erlmcp_types.erl    # Type definitions
│   └── ...
│
├── test/                   # Test suite
│   ├── erlmcp_SUITE.erl    # Common Test suite
│   ├── *_tests.erl         # eunit tests
│   └── ...
│
├── examples/               # Example applications
│   ├── simple_client.erl
│   ├── simple_server.erl
│   ├── weather_server.erl
│   └── README.md
│
├── include/                # Header files
│   └── erlmcp.hrl
│
├── priv/                   # Private files
│   ├── ssl/                # SSL certificates
│   └── images/             # Documentation images
│
├── config/                 # Configuration files
│   ├── sys.config          # System configuration
│   └── test.config         # Test configuration
│
├── docs/                   # Documentation
│   ├── architecture.md
│   ├── protocol.md
│   ├── otp-patterns.md
│   └── api-reference.md
│
└── taiea/                  # TAIEA autonomic system
    ├── rebar.config        # TAIEA configuration
    ├── apps/
    │   ├── taiea_core/     # Autonomic logic
    │   ├── taiea_mcp/      # MCP integration
    │   ├── taiea_governor/ # Governance engine
    │   └── taiea_receipts/ # Deterministic receipts
    └── rel/                # Release configuration
```

## Configuration

### System Configuration (sys.config)

Located at `config/sys.config`:
```erlang
[
    {erlmcp, [
        {listen_port, 8080},
        {transport, {tcp, []}},
        {workers, 10}
    ]},
    {lager, [
        {handlers, [
            {lager_console_backend, [
                {level, info},
                {formatter_config, [timestamp, " - ", message, "\n"]}
            ]}
        ]}
    ]}
].
```

### VM Arguments (vm.args)

Located at `vm.args`:
```
# Erlang VM configuration
-sname erlmcp
-setcookie erlmcp_secret_cookie
+K true
+A 8
```

## Troubleshooting

### Build Failures

**Clean and rebuild:**
```bash
make clean                  # Soft clean (keeps _build)
make distclean              # Hard clean (removes everything)
make workspace-clean        # Clean both erlmcp + taiea
```

**Check dependencies:**
```bash
rebar3 deps                 # List all dependencies
rebar3 tree                 # Show dependency tree
```

**Force dependency update:**
```bash
rebar3 unlock
rebar3 get-deps
rebar3 compile
```

### Test Failures

**Run with verbose output:**
```bash
rebar3 eunit -v             # Verbose unit tests
rebar3 ct -v                # Verbose integration tests
```

**Check logs:**
```bash
# CT logs
less _build/test/logs/ct_run.ct_erlmcp_SUITE@hostname.*/run.log

# Coverage report
open _build/test/cover/index.html
```

### Compilation Warnings

**Treat warnings as errors (production standard):**
```
Profile prod has warnings_as_errors enabled.
This ensures code quality.
```

**Fix warnings by:**
1. Reviewing compiler output
2. Fixing the actual issue (preferred)
3. Using proper dialyzer/xref pragmas (only if necessary)

### Type Checking Issues

**Run Dialyzer separately:**
```bash
rebar3 dialyzer             # Generate and analyze
rebar3 dialyzer --succ      # Show successes too
```

### Performance Issues

**Profile the application:**
```bash
make profile                # Launch recon tracing
# or use Observer: make observer
```

## Environment Variables

Set in `.envrc` or manually:

```bash
# Workspace home
export ERLMCP_HOME=/path/to/erlmcp

# Erlang library paths
export ERL_LIBS="$PWD/_build/default/lib:$PWD/taiea/_build/default/lib"

# OTP root (usually set automatically)
export OTP_ROOT=$(erl -noshell -eval 'io:format(code:root_dir())' -s init stop)
```

## Git Workflow

### Feature Development

```bash
# Create feature branch
git checkout -b feature/my-feature

# Make changes
# Test thoroughly
make workspace-check

# Commit
git add .
git commit -m "feat: describe feature"

# Push and create PR
git push origin feature/my-feature
```

### Before Committing

**Checklist:**
- [ ] Code builds: `make build`
- [ ] Tests pass: `make test`
- [ ] Lint passes: `make lint`
- [ ] Coverage adequate (aim for 80%+)
- [ ] Changes documented
- [ ] Commit message is clear

**Full pre-commit validation:**
```bash
make check                  # Runs everything
```

## Performance Tuning

### Compilation Speed

**Incremental compilation:**
```bash
make compile                # Only recompiles changed files (~5s)
```

**Parallel build:**
```bash
# rebar3 automatically parallelizes based on CPU cores
# To force specific parallelism:
rebar3 compile --jobs 4
```

### Test Speed

**Run only changed modules:**
```bash
rebar3 eunit --module=module_name
```

**Parallel test execution:**
```bash
# Common Test runs in parallel by default
rebar3 ct --num_parallel_processes 4
```

## Resources

- [Erlang Documentation](https://www.erlang.org/doc/)
- [rebar3 User Guide](https://rebar3.org/)
- [Common Test](http://www.erlang.org/doc/apps/common_test/)
- [PropEr Testing](https://proper-testing.github.io/)
- [Model Context Protocol](https://modelcontextprotocol.io/)

## Getting Help

- Check `make help` for available targets
- Review [docs/](docs/) for architecture and design
- Examine [examples/](examples/) for usage patterns
- Open an issue on GitHub for problems

---

**Last Updated**: 2026-01-26
**Erlang/OTP Version**: 25+
**rebar3 Version**: 3.22+
