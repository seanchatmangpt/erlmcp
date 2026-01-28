# How-To: Local Development Setup

This guide walks you through setting up erlmcp for local development and running interactive tests.

## Prerequisites

- Erlang/OTP 25+
- `rebar3` build tool
- Git
- Standard Unix tools (make, bash, curl)

## Step 0: Verify Host Readiness (New!)

Before starting development, verify your system has the required configuration:

```bash run
$ erlmcp doctor
```

This diagnostic tool checks:
- Erlang/OTP version (must be 25+)
- Build tools (rebar3)
- System limits (file descriptors, TCP parameters)
- Container runtime (Docker/Colima/Podman)
- Telemetry environment (optional but recommended)

**Expected output:** All green checks (✓). If warnings or critical issues appear, follow the remediation steps provided.

Example with warnings:

```
  ⚠ TCP Parameters
    TCP backlog = 128 (recommend >= 256 for production)
    → Increase backlog: sudo sysctl -w kern.ipc.somaxconn=256
```

Fix this with:

```bash
sudo sysctl -w kern.ipc.somaxconn=256  # macOS
# or for Linux
sudo sysctl -w net.core.somaxconn=256
```

Then verify the fix:

```bash run
$ erlmcp doctor
```

**Exit codes:**
- `0` - All checks passed
- `1` - Warnings (can continue with caution)
- `2` - Critical issues (must fix before continuing)

For scripting/CI integration, use JSON output:

```bash run
$ erlmcp doctor --json | head -20
```

## Step 1: Clone and Setup

Clone the repository and install dependencies:

```bash run
$ cd /Users/sac/erlmcp
$ ls -la | grep erlmcp.app.src
```

Expected: The erlmcp.app.src file exists in src/ directory.

## Step 2: Compile the Project

Build erlmcp and all dependencies:

```bash run
$ make compile 2>&1 | head -20
```

This compiles the Erlang source code and generates beam files in `_build/default/lib/`.

## Step 3: Run Unit Tests

Execute the fast unit test suite (< 2 minutes):

```bash run
$ make test-unit 2>&1 | grep -E "(passed|failed|skipped)" | head -5
```

The unit tests validate core functionality without integration overhead.

## Step 4: Run Integration Tests

Execute the comprehensive integration test suite:

```bash run
$ make test-int 2>&1 | grep -E "Test run|PASSED|FAILED" | head -5
```

Integration tests verify multi-process scenarios and protocol compliance.

## Step 5: Check Code Quality

Run linting and type checking:

```bash run
$ make lint 2>&1 | tail -10
```

This runs xref (cross-reference) and dialyzer (static type analysis).

## Step 6: Start Development Console

Launch an interactive Erlang shell with erlmcp preloaded:

```bash run
$ make console 2>&1 | head -20
```

In the console, you can test erlmcp APIs directly:

```erlang run
%% Load the application
1> application:start(erlmcp).

%% Create a simple server
2> erlmcp_server:start_link(#{}).

%% List all active servers
3> supervisor:which_children(erlmcp_server_sup).
```

## Step 7: Generate Coverage Report

After running tests, generate a coverage report:

```bash run
$ make test-coverage 2>&1 | grep "Coverage report"
```

The report shows test coverage metrics in `_build/test/cover/index.html`.

## Step 8: Profile Client Calls

Trace and profile erlmcp_client function calls:

```bash run
$ make profile 2>&1 | head -20
```

This captures performance metrics for debugging.

## Common Development Tasks

### Recompile after Changes

```bash run
$ rebar3 compile 2>&1 | tail -3
```

### Run Specific Test Module

```bash run
$ rebar3 eunit --module=erlmcp_server_tests 2>&1 | grep -E "passed|failed"
```

### Format Code

```bash run
$ rebar3 format 2>&1 | tail -5
```

Enforces consistent code style (100-char paper width).

### Check for Type Errors

```bash run
$ rebar3 dialyzer 2>&1 | tail -5
```

Run static type analysis to catch errors early.

## Troubleshooting

### Build Fails

Clean and rebuild:

```bash run
$ make clean && make compile 2>&1 | tail -10
```

### Test Failures

Run verbose test output:

```bash run
$ rebar3 eunit -v 2>&1 | tail -20
```

### Port Already in Use

Kill processes on default ports:

```bash run
$ lsof -i :8000 | grep erlmcp | awk '{print $2}' | xargs kill -9 2>/dev/null || true
$ echo "Ports cleaned"
```

## Next Steps

- Explore `docs/protocol.md` for MCP protocol details
- Read `docs/architecture.md` for system design
- Check `examples/` for working client/server examples
- Review `test/` for test patterns and examples

## Development Workflow

1. **Make changes** to src/ files
2. **Run `make compile`** to verify syntax
3. **Run `make test-unit`** for fast feedback
4. **Run `make test`** before committing
5. **Run `make quality`** to check linting/types
6. **Commit and push**

Remember: All tests must pass and linting must succeed before code is production-ready.
