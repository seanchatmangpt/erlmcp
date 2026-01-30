# erlmcp_validate CLI Tool

## Overview

The `erlmcp_validate` CLI tool is an escript that provides comprehensive validation and compliance testing for the erlmcp MCP SDK implementation.

## Building the Escript

### Quick Build

```bash
# Using the build script (recommended)
./scripts/build-escript.sh

# Build and test in one command
./scripts/build-escript.sh --test
```

### Manual Build

```bash
# Compile the project
TERM=dumb rebar3 compile

# Build the escript
rebar3 escriptize

# The escript will be at: ./_build/default/bin/erlmcp_validate
```

## Usage

### Basic Commands

```bash
# Show help
./erlmcp_validate --help

# Show version
./erlmcp_validate --version

# Check validation status
./erlmcp_validate status

# Quick validation check
./erlmcp_validate quick-check
```

### Running Validations

```bash
# Run all validation sections
./erlmcp_validate run --all

# Run specific section
./erlmcp_validate run --section protocol

# Run with specific transport
./erlmcp_validate run --transport tcp

# Run with verbose output
./erlmcp_validate run --all --verbose

# Run with JSON output
./erlmcp_validate run --all --format json

# Run with minimal output
./erlmcp_validate run --all --quiet
```

### Generating Reports

```bash
# Generate text report (default)
./erlmcp_validate report

# Generate JSON report
./erlmcp_validate report --format json

# Generate Markdown report
./erlmcp_validate report --format markdown

# Generate HTML report
./erlmcp_validate report --format html

# Save report to file
./erlmcp_validate report --format markdown --output report.md
```

## Validation Sections

| Section | Description |
|---------|-------------|
| `protocol` | MCP protocol compliance (JSON-RPC 2.0, message formats) |
| `transport` | Transport layer behavior (stdio, tcp, http) |
| `security` | Security features (authentication, JWT validation) |
| `error_handling` | Error response validation and edge cases |
| `performance` | Performance benchmarks and load testing |

## Transport Options

| Transport | Description |
|-----------|-------------|
| `stdio` | Standard I/O transport |
| `tcp` | TCP transport |
| `http` | HTTP transport |
| `websocket` | WebSocket transport |
| `all` | All transports (default) |

## Output Formats

| Format | Description |
|--------|-------------|
| `text` | Plain text output (default) |
| `json` | JSON formatted output |
| `markdown` | Markdown formatted output |
| `html` | HTML formatted output (reports only) |

## Examples

### Complete Validation Workflow

```bash
# 1. Build the escript
./scripts/build-escript.sh

# 2. Check system status
./erlmcp_validate status

# 3. Run quick check
./erlmcp_validate quick-check

# 4. Run full validation
./erlmcp_validate run --all --verbose

# 5. Generate compliance report
./erlmcp_validate report --format markdown --output VALIDATION_REPORT.md
```

### CI/CD Integration

```bash
# In CI/CD pipeline - run all validations and exit on failure
#!/bin/bash
set -e
./scripts/build-escript.sh
./erlmcp_validate run --all --quiet --format json > validation-results.json
# Check if validation passed
if grep -q '"status":"success"' validation-results.json; then
    echo "Validation passed"
    exit 0
else
    echo "Validation failed"
    exit 1
fi
```

## File Locations

After building, the escript is available at:
- Primary: `./_build/default/bin/erlmcp_validate`
- Symlink: `./erlmcp_validate` (created automatically)

## Troubleshooting

### Escript Not Found

If you get "command not found":
```bash
# Ensure you're in the erlmcp root directory
cd /path/to/erlmcp

# Build the escript
./scripts/build-escript.sh

# Use the full path or relative path
./_build/default/bin/erlmcp_validate --help
```

### Compilation Errors

If you see compilation errors:
```bash
# Clean and rebuild
rm -rf _build
TERM=dumb rebar3 compile
rebar3 escriptize
```

### Application Start Errors

Some validation functions require applications to be started. If you see "not started" errors:
```bash
# This is expected for the escript - it runs in a minimal environment
# The escript includes necessary modules but doesn't start all applications
# Use the status command to verify basic functionality
./erlmcp_validate status
```

## Architecture

The escript is built using rebar3's escript support with the following configuration in `rebar.config`:

```erlang
{escript_main_app, erlmcp_validation}.
{escript_name, "erlmcp_validate"}.
{escript_emu_args, "%%! -escript main erlmcp_validate_cli -noshell -pa _build/default/lib/*/ebin\n"}.
{escript_incl_apps, [erlmcp_validation, erlmcp_core, erlmcp_transports, jsx, jesse]}.
```

The main entry point is `erlmcp_validate_cli:main/1` which:
1. Parses command line arguments
2. Executes the requested command
3. Formats and outputs results

## Development

### Adding New Commands

To add a new command to the CLI:

1. Add the command to `parse_args/1` in `apps/erlmcp_validation/src/erlmcp_validate_cli.erl`
2. Implement the command execution in `execute_command/1`
3. Update the help text in `print_help/0`
4. Rebuild the escript

Example:
```erlang
parse_args(["new-command"|Rest]) ->
    parse_new_command_args(Rest, #{}).

execute_command({new_command, Opts}) ->
    case do_new_command(Opts) of
        {ok, Result} ->
            print_result(Result, Opts),
            halt(0);
        {error, Reason} ->
            print_error("New command failed: " ++ Reason),
            halt(1)
    end.
```

### Testing Locally

```bash
# Build and test
./scripts/build-escript.sh --test

# Run specific validation
./erlmcp_validate run --section protocol --verbose

# Test output formats
./erlmcp_validate run --section protocol --format json | jq .
```

## See Also

- [MCP Specification](https://modelcontextprotocol.io/docs)
- [Validation Framework](docs/VALIDATOR_GUIDE.md)
- [Protocol Compliance Testing](docs/SPEC_COMPLIANCE_TESTING.md)
- [Quality Gates](docs/QUALITY_GATES.md)
