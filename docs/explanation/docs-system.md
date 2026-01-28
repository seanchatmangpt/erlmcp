# Documentation Testing System: Executive Explanation

## Overview

The erlmcp **Doc-Test Framework** automatically executes fenced code blocks in markdown documentation, verifying that examples in guides and tutorials actually work. This ensures documentation stays accurate as the system evolves.

## Problem Solved

Documentation often becomes outdated. Code examples break when the system changes, and nobody notices until a user tries to follow the guide. This framework treats documentation as executable specifications—if an example is documented, it must work.

## How It Works

### 1. Markdown Parsing

Doc-Test scans markdown files for specially-marked code fences:

```bash run
$ echo "example command"
```

**Markers:**
- ` ```bash run` - Shell command (executable)
- ` ```erlang run` - Erlang code (executable)
- ` ```bash` - Shell command (shown, not run)
- ` ```erlang` - Erlang code (shown, not run)

The `run` keyword indicates a block should be tested.

### 2. Command Extraction

The runner:
1. Parses markdown to find fenced blocks
2. Identifies lines starting with `$` or `>` as commands
3. Removes prompts and extracts the actual command
4. Captures expected output (lines after commands without prompts)

Example parsing:

```markdown
```bash run
$ echo hello
hello
world
```
```

Extracts:
- Command: `echo hello`
- Expected output: `hello\nworld`

### 3. Execution

The framework executes each command:

**Bash blocks**: Run in shell with `cd /Users/sac/erlmcp && <command>`

**Erlang blocks**: Currently skipped (pending shell integration); future versions will use `erl -noshell -eval`

### 4. Output Verification

After execution:
1. Compares actual output with expected (if provided)
2. Checks exit code (must be 0 for bash)
3. Reports pass/fail/skip status

### 5. Reporting

Generates a summary:
```
Total:   42
Passed:  40
Failed:   2
Skipped:  0
```

Failed tests show file, line number, and error details.

## Architecture

### Components

**doc_test_runner.erl** - Core framework
- `parse_markdown/1` - Extract blocks from markdown
- `execute_block/1` - Run single block
- `run_file/1` - Test one markdown file
- `run_all_tests/0` - Test all documentation
- `format_results/1` - Display report

**erlmcp_doc_tests_SUITE.erl** - Common Test suite
- Unit tests for parsing, execution, verification
- Integration tests for end-to-end flows
- Mocking/stubbing for external dependencies
- 19 comprehensive test cases

### Integration Points

**Makefile Target:**
```bash
make doc-test    # Run all doc tests
```

**CI Pipeline:**
Integrated into `make check` and CI workflows to catch documentation drift early.

**rebar3 Custom Provider:**
Can be registered as a rebar3 provider for `rebar3 doc-test` command.

## Features

### ✓ Implemented

1. **Markdown Parsing** - Extracts fenced code blocks with run markers
2. **Shell Execution** - Runs bash commands with proper working directory
3. **Output Verification** - Validates output matches expectations (substring match)
4. **Prompt Removal** - Strips `$` and `>` prompts for execution
5. **Result Reporting** - Formatted pass/fail/skip summary with durations
6. **Common Test Integration** - Full test suite with 19 test cases
7. **Multi-file Support** - Scans entire docs/ directory tree
8. **Error Details** - Shows file, line number, and command for failures

### ⚠ Limitations & Future Work

1. **Erlang Evaluation** - Currently skipped; needs erl shell integration
2. **Stateful Tests** - Each block runs in isolation (no shared state)
3. **Network Stubbing** - HTTP calls to external services not mocked
4. **Database Mocking** - SQL queries not stubbed (would use test containers)
5. **Parallel Execution** - Currently sequential; could be parallelized
6. **Exit Codes** - Only checks 0 for success; could verify specific codes
7. **Timeout Handling** - No per-block timeout configuration
8. **Environment Variables** - Not isolated between blocks

## Test Structure

### Example 1: Simple Command

```markdown
```bash run
$ echo test
test
```
```

Tests that `echo test` produces `test` output.

### Example 2: Pipeline

```markdown
```bash run
$ echo hello | grep hello
hello
```
```

Tests that piped commands work correctly.

### Example 3: Multiple Commands

```markdown
```bash run
$ cd /tmp && pwd
/tmp
```
```

Tests that command sequences work.

### Example 4: No Output Verification

```markdown
```bash run
$ make compile
```
```

Tests that command succeeds (exit code 0), without verifying exact output.

## Usage Guide

### Run All Doc Tests

```bash
make doc-test
```

Scans `docs/`, `docs/howto/`, `docs/explanation/` for markdown files with `run` markers.

### Run Specific File

```bash
rebar3 eunit --module=erlmcp_doc_tests_SUITE
```

Or programmatically:

```erlang
doc_test_runner:run_file("docs/howto/local-dev.md", #{}).
```

### View Test Results

```
════════════════════════════════════════════════════════════
DOC-TEST RESULTS
════════════════════════════════════════════════════════════
Total:   42
Passed:  40
Failed:   2
Skipped:  0
════════════════════════════════════════════════════════════

FAILURES:
  docs/howto/deploy.md:45 (bash)
    Error: Mismatch
```

## Writing Doc-Tested Documentation

### Best Practices

1. **Keep blocks focused** - One command or small sequence per block
2. **Include expected output** - Document what success looks like
3. **Use descriptive prompts** - Help readers understand the example
4. **Comment on why** - Explain what the command demonstrates
5. **Verify frequently** - Run `make doc-test` regularly

### Example Structure

```markdown
## Deploying to Production

### Build the Release

Build an optimized production release:

```bash run
$ make release-prod 2>&1 | tail -5
```

This generates release artifacts in `_build/prod/`.

### Start the Application

```bash run
$ _build/prod/rel/erlmcp/bin/erlmcp start
```

The application starts as a daemon.

### Verify Health

```bash run
$ _build/prod/rel/erlmcp/bin/erlmcp ping
pong
```

Returns "pong" when healthy.
```

## Implementation Details

### Parsing Algorithm

1. Split markdown into lines
2. Track fence state (in_fence, fence_type)
3. When opening fence detected, start accumulating
4. When closing fence detected, create block record
5. Filter out text-only blocks

### Execution Strategy

1. For bash: Extract prompts, join commands, execute in erlmcp directory
2. For erlang: (Future) Parse ->, compile, evaluate in isolated context
3. Capture stdout/stderr
4. Compare output (substring match by default)

### Error Handling

- Bash: Non-zero exit code is failure
- Erlang: Compilation error is failure
- Missing expected output: Substring match fails
- Timeout: (Future) Report as timeout failure

## Integration with CI/CD

### GitHub Actions

```yaml
- name: Test Documentation
  run: make doc-test
  if: success()
```

Adds doc-test to CI pipeline; fails build if documentation breaks.

### GitLab CI

```yaml
doc_test:
  script:
    - make doc-test
  allow_failure: false
```

### Local Pre-commit

```bash
#!/bin/bash
make doc-test || exit 1
```

Verify docs before committing.

## Performance

**Typical Execution Times:**
- Parse markdown: ~10ms per file
- Execute bash: 100-500ms per command (depends on command)
- Report generation: ~5ms
- Total for 40 blocks: ~5-10 seconds

## Metrics & Monitoring

Track over time:
- **Total blocks** - Growth in documented examples
- **Pass rate** - Percentage of documentation that's accurate
- **Failure trend** - Blocks that break (indicates drift)
- **Execution time** - Overall doc test suite duration

## Known Issues & Workarounds

### Issue: Commands require setup

**Workaround**: Add setup steps before the test block or use mocking.

### Issue: Output varies between systems

**Workaround**: Filter output through `head`, `grep`, or `sed` to normalize.

### Issue: Tests interfere with each other

**Workaround**: Keep blocks isolated; each runs in fresh shell.

## Future Enhancements

1. **Erlang Shell Integration** - Run Erlang code blocks via erl shell
2. **Parallel Execution** - Run independent blocks concurrently
3. **Test Fixtures** - Share setup/teardown between blocks
4. **Timeout Configuration** - Per-block timeouts
5. **Environment Isolation** - Sandboxed environment variables
6. **Docker Support** - Run blocks in isolated containers
7. **Conditional Blocks** - Skip blocks based on platform/version
8. **Performance Tracking** - Warn if command becomes slower
9. **Coverage Integration** - Track doc coverage percentage
10. **Metrics Export** - Send results to monitoring systems

## Related Documentation

- **docs/howto/local-dev.md** - Local development guide (with doc tests)
- **docs/howto/deploy.md** - Deployment guide (with doc tests)
- **architecture.md** - System design overview
- **protocol.md** - MCP protocol documentation

## Conclusion

The Doc-Test Framework ensures that documentation remains accurate and actionable. By treating examples as executable tests, we catch documentation drift early and maintain a single source of truth for how to use erlmcp.

Documentation becomes a contract: if it's documented, it works. This builds trust and reduces support burden.
