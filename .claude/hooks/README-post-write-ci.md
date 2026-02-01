# post-write-ci.sh - Async CI Hook

## Overview

The `post-write-ci.sh` hook provides automatic compilation and testing for Erlang files when they are modified via Write or Edit operations.

## Specification

- **Trigger**: PostToolUse event (Write or Edit tool)
- **Files**: `.erl`, `.hrl`, `.app.src`
- **Behavior**: Non-blocking async execution
- **Timeout**: 120 seconds max
- **Reference**: AUTONOMOUS_IMPLEMENTATION_WORK_ORDER.md:WO-004

## Features

### Async Execution
- Hook returns immediately (< 500ms)
- CI runs in background process
- Does not block agent workflow

### File Filtering
- Only processes Erlang source files
- Ignores non-Erlang files (.md, .txt, etc.)

### Incremental Testing
- Runs `rebar3 compile` for full project
- Runs `rebar3 eunit --module=<module>_tests` for specific module
- Skips tests if compilation fails

### Log Management
- **build.log**: Compilation output with timestamps
- **test.log**: Test execution output
- Automatic rotation when > 10MB
- Keeps last 5 rotations

## Usage

The hook is automatically triggered by Claude Code when Erlang files are modified.

### Environment Variables

- `TOOL`: Tool type (Write or Edit)
- `SUBJECT`: File path that was modified
- `CLAUDE_PROJECT_DIR`: Project root directory

### Example

```bash
# Manual invocation (for testing)
TOOL="Write" SUBJECT="apps/erlmcp_core/src/erlmcp_client.erl" \
  CLAUDE_PROJECT_DIR="/home/user/erlmcp" \
  bash .claude/hooks/post-write-ci.sh
```

## Log Output

### Build Log Format

```
========================================================================
[2026-02-01 04:11:01] CI triggered by: apps/erlmcp_core/src/erlmcp_client.erl
========================================================================
[2026-02-01 04:11:01] Running: rebar3 compile
===> Compiling erlmcp_client
[2026-02-01 04:11:02] Compile: SUCCESS
========================================================================
[2026-02-01 04:11:02] CI Summary
  File: apps/erlmcp_core/src/erlmcp_client.erl
  Compile: SUCCESS
  Tests: SUCCESS
========================================================================
```

### Test Log Format

```
========================================================================
[2026-02-01 04:11:02] Test run triggered by: apps/erlmcp_core/src/erlmcp_client.erl
========================================================================
[2026-02-01 04:11:02] Running: rebar3 eunit --module=erlmcp_client_tests
  All 15 tests passed.
[2026-02-01 04:11:03] Tests: SUCCESS
```

## Error Handling

- **Compile failure**: Logged, tests skipped, agent not blocked
- **Test failure**: Logged, agent not blocked
- **Timeout**: Process killed after 120s, logged

## Testing

Integration tests are provided in `test/test_post_write_ci_hook.sh`:

```bash
# Run integration tests
./test/test_post_write_ci_hook.sh
```

### Test Coverage

- ✓ Hook returns immediately (async execution)
- ✓ File filtering - ignore non-Erlang files
- ✓ File filtering - process .erl files
- ✓ Log creation - build.log created
- ✓ Log content - contains expected output
- ✓ Log rotation - rotates at 10MB
- ✓ Hook script is executable
- ✓ Hook exits with code 0 (success)

**Coverage**: 8/8 tests passing (100%)

## Implementation Details

### Non-Blocking Design

The hook uses process backgrounding with variable export:

```bash
(
    export PROJECT_DIR="$PROJECT_DIR"
    export BUILD_LOG="$BUILD_LOG"
    # ... exports ...

    {
        # CI logic here
    } &  # Background execution

    exit 0  # Immediate return
) >/dev/null 2>&1 &

disown 2>/dev/null || true
exit 0  # Hook returns immediately
```

### Log Rotation

Logs are rotated when they exceed 10MB:
- `build.log` → `build.log.1`
- `build.log.1` → `build.log.2`
- ... (keeps last 5)

### Timeout Protection

All rebar3 commands are wrapped with `timeout`:

```bash
timeout "$TIMEOUT_SECONDS" rebar3 compile
```

This ensures background processes don't run indefinitely.

## Chicago TDD Compliance

- ✓ Real processes (no mocks)
- ✓ Observable behavior (log file creation, content)
- ✓ State-based verification (file existence, timestamps)
- ✓ Integration tests (actual hook execution)

## Maintenance

### Adding Support for New File Types

Edit the regex pattern in the hook:

```bash
if [[ "$SUBJECT" =~ \.(erl|hrl|app\.src|config)$ ]]; then
```

### Adjusting Timeout

Change `TIMEOUT_SECONDS` variable:

```bash
TIMEOUT_SECONDS=180  # 3 minutes
```

### Adjusting Log Rotation

Change `MAX_LOG_SIZE` or `KEEP_ROTATIONS`:

```bash
MAX_LOG_SIZE=$((50 * 1024 * 1024))  # 50MB
KEEP_ROTATIONS=10  # Keep 10 rotations
```

## Troubleshooting

### Logs not created

Check that:
- Hook is executable: `chmod +x .claude/hooks/post-write-ci.sh`
- `.erlmcp/` directory exists
- File path matches regex pattern

### Background process not running

Check for orphaned processes:

```bash
ps aux | grep rebar3
```

Kill if needed:

```bash
pkill -f "rebar3 compile"
```

### Logs growing too large

Reduce `MAX_LOG_SIZE` or increase rotation frequency.

## Related Files

- `.claude/hooks/post-write-ci.sh` - Hook implementation
- `test/test_post_write_ci_hook.sh` - Integration tests
- `AUTONOMOUS_IMPLEMENTATION_WORK_ORDER.md` - WO-004 specification
- `CLAUDE_CODE_WEB_GOVERNANCE_SYSTEM.md` - Hook architecture
