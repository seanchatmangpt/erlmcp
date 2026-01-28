# Doc-Test Framework Tools

This directory contains the doc-test framework, which automatically executes code examples from markdown documentation to ensure they stay current and accurate.

## Files

- **doc_test_runner.erl** - Core framework module for parsing, executing, and reporting on doc tests
- **DOC_TEST_RUNNER.md** - This file

## Quick Start

### Run all documentation tests

```bash
make test-doc
```

### Run documentation tests for a specific file

```bash
rebar3 eunit --module=erlmcp_doc_tests_SUITE
```

Or programmatically:

```erlang
doc_test_runner:run_file("docs/howto/local-dev.md", #{}).
```

### View test results

Results are displayed with:
- Total blocks found
- Pass/fail/skip counts
- Execution time per block
- Detailed failure information

## Supported Block Types

### Executable Bash Blocks

```bash run
$ echo hello
hello
```

Marks: ` ```bash run` or ` ```shell run`

Commands:
- Lines starting with `$` are executed as commands
- Lines without `$` or `>` are expected output (verified via substring match)
- Prompts (`$`, `>`) are automatically removed before execution
- Exit code must be 0 for pass

### Executable Erlang Blocks

```erlang run
1> erlang:system_info(otp_release).
"25"
```

Marks: ` ```erlang run`

Status: Currently skipped (pending shell integration)

### Non-Executable Blocks

```bash
echo this is not tested
```

Marks: ` ```bash` or ` ```erlang` (without `run`)

These blocks are displayed but not executed.

## Defining Test Blocks

### Simple Test (Verify Output)

````markdown
## Example: Echo Command

Test that echo works:

```bash run
$ echo test
test
```
````

### Test with Multiple Commands

````markdown
## Example: Piping Commands

Test command pipeline:

```bash run
$ echo hello | grep hello
hello
```
````

### Test with Complex Output

````markdown
## Example: Listing Files

```bash run
$ ls -la | head -3
total
drwxr-xr-x
-rw-r--r--
```
````

### Test Without Output Verification

````markdown
## Example: Just Check Success

```bash run
$ make compile
```
````

If no expected output is provided, the test passes if the command exits with code 0.

## Implementation Details

### Module Structure

**doc_test_runner.erl**

- `run_all_tests/0,1` - Scan docs/ directory and run all doc tests
- `run_file/1,2` - Run tests from a single markdown file
- `parse_markdown/1` - Extract fenced code blocks from markdown
- `execute_block/1` - Execute a single block and verify output
- `verify_output/3` - Check actual output against expected
- `format_results/1` - Display test results summary

### Execution Flow

1. **Parse**: Find all ` ```...run` fenced blocks in markdown
2. **Extract**: Remove prompts, separate commands from expected output
3. **Execute**: Run each command in /Users/sac/erlmcp directory
4. **Verify**: Compare actual output with expected (substring match)
5. **Report**: Display pass/fail/skip with details

### Output Matching

Uses substring matching by default:
- Expected: "hello"
- Actual: "hello world"
- Result: PASS ✓ (actual contains expected)

To match exactly, put ONLY the expected text in the block.

## Test Suite

**test/erlmcp_doc_tests_SUITE.erl**

Common Test suite with 19 comprehensive test cases:

- Markdown parsing (fence detection, multiline blocks, complex scenarios)
- Prompt removal (bash and erlang styles)
- Command execution (simple, pipes, multiple commands)
- Output verification (substring match, none, exact)
- Stubbing & mocking (HTTP, database responses)
- Result formatting and reporting

Run with:

```bash
rebar3 ct --suite=test/erlmcp_doc_tests_SUITE
```

## CI Integration

### Makefile Target

```bash
make test-doc
```

Add to CI pipeline to catch documentation drift early.

### GitHub Actions

```yaml
- name: Test Documentation
  run: make test-doc
```

### Pre-commit Hook

```bash
#!/bin/bash
make test-doc || exit 1
```

## Known Limitations

1. **Erlang Blocks Skipped** - No shell integration yet; future versions will execute erlang code
2. **No Shared State** - Each block runs independently; no state carries between blocks
3. **Network Stubbing** - HTTP/DB calls to external services aren't mocked (use test doubles)
4. **Sequential Execution** - Blocks run one by one (parallelizable in future)
5. **Exit Code Only** - Bash tests only check for 0 exit; can't verify specific codes yet
6. **No Timeout** - No per-block timeout configuration (could hang on infinite loops)
7. **Environment Isolation** - Environment variables aren't isolated between blocks

## Future Enhancements

1. **Erlang Shell Integration** - Execute erlang code blocks via erl -noshell
2. **Parallel Execution** - Run independent blocks concurrently
3. **Test Fixtures** - Share setup/teardown code between blocks
4. **Performance Tracking** - Warn if command execution becomes slower
5. **Conditional Blocks** - Skip blocks based on platform, version, or environment
6. **Docker Containers** - Run blocks in isolated containers for reproducibility
7. **Environment Mocking** - Mock external services with test servers
8. **Metrics Export** - Send results to monitoring/analytics systems
9. **Coverage Integration** - Track percentage of codebase mentioned in docs
10. **Diff Highlighting** - Show exactly what changed when tests fail

## Troubleshooting

### Test Hangs

If a test block hangs:

```bash
Ctrl+C
```

Then fix the command and re-run. Consider wrapping commands with `timeout`:

```bash run
$ timeout 5 make compile
```

### Test Fails But Works Manually

The test runs in /Users/sac/erlmcp directory. If your command expects a different working directory:

```bash run
$ cd /some/path && command
```

Or adjust the code in `execute_bash/2` in doc_test_runner.erl.

### Output Doesn't Match

Check for:
1. Extra whitespace (grep output, trailing newlines)
2. Different line endings (Unix vs Windows)
3. Platform-specific output (BSD vs GNU tools)

Use `head`, `grep`, or `sed` to normalize:

```bash run
$ make compile 2>&1 | grep -E "passed|failed" | head -3
```

## Examples

See these files for working examples of documented code blocks:

- `/Users/sac/erlmcp/docs/howto/local-dev.md` - Local development guide with runnable examples
- `/Users/sac/erlmcp/docs/howto/deploy.md` - Production deployment guide with runnable examples
- `/Users/sac/erlmcp/docs/explanation/docs-system.md` - Framework explanation (meta-documentation)

## Architecture

```
Documentation (Markdown)
        ↓
   Parse Markdown
   (identify fenced blocks with "run" marker)
        ↓
  Extract Commands
  (remove prompts, separate commands from output)
        ↓
   Execute Blocks
   (run in shell, capture stdout/stderr)
        ↓
  Verify Output
  (substring match against expected)
        ↓
   Report Results
   (pass/fail/skip with durations)
```

## Related Documentation

- **docs/explanation/docs-system.md** - Complete framework explanation and design
- **docs/howto/local-dev.md** - Local development (with executable examples)
- **docs/howto/deploy.md** - Production deployment (with executable examples)

## License

Part of erlmcp project. See root LICENSE file.
