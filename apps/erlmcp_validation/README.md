# erlmcp_validation - MCP Specification Compliance Validator

Comprehensive validation framework for MCP 2025-11-25 specification compliance.

## Overview

The `erlmcp_validation` application provides:

1. **Programmatic API** - Call validators from Erlang code
2. **CLI Interface** - Command-line validation tools
3. **Escript Wrapper** - Standalone executable `bin/erlmcp-validate`
4. **Compliance Reporting** - Multi-format reports (text, JSON, Markdown, HTML)

## Architecture

```
erlmcp_validation/
├── src/
│   ├── erlmcp_validate_cli.erl         # Main CLI and programmatic API
│   ├── erlmcp_spec_parser.erl          # Hardcoded MCP 2025-11-25 spec
│   ├── erlmcp_protocol_validator.erl   # JSON-RPC/MCP protocol validation
│   ├── erlmcp_transport_validator.erl  # Transport behavior validation
│   ├── erlmcp_security_validator.erl   # Security features validation
│   ├── erlmcp_performance_validator.erl# Performance benchmarks
│   └── erlmcp_compliance_report.erl    # Report generation
└── test/
    └── erlmcp_spec_compliance_SUITE.ct # Compliance test suite
```

## Programmatic API

### Basic Usage

```erlang
%% Validate against MCP 2025-11-25 spec
{ok, SpecResult} = erlmcp_validate_cli:run(spec).

%% Validate a JSON-RPC message
Message = #{
    <<"jsonrpc">> => <<"2.0">>,
    <<"method">> => <<"initialize">>,
    <<"params">> => #{
        <<"protocolVersion">> => <<"2025-11-25">>,
        <<"capabilities">> => #{},
        <<"clientInfo">> => #{
            <<"name">> => <<"test-client">>,
            <<"version">> => <<"1.0.0">>
        }
    },
    <<"id">> => 1
},
{ok, ProtocolResult} = erlmcp_validate_cli:run(protocol, Message).

%% Validate a single transport
{ok, TransportResult} = erlmcp_validate_cli:run(transport, stdio).

%% Run full compliance suite
{ok, ComplianceResult} = erlmcp_validate_cli:run(compliance).

%% Run all validators (comprehensive)
{ok, AllResults} = erlmcp_validate_cli:run(all).
```

### Detailed API

```erlang
%% Spec validation
-spec validate_spec() -> {ok, map()} | {error, term()}.
erlmcp_validate_cli:validate_spec().

%% Protocol message validation
-spec validate_protocol_message(map()) -> {ok, map()} | {error, term()}.
erlmcp_validate_cli:validate_protocol_message(Message).

%% Transport validation
-spec validate_transport(atom() | binary() | string()) -> {ok, map()} | {error, term()}.
erlmcp_validate_cli:validate_transport(stdio).
erlmcp_validate_cli:validate_transport("tcp").
erlmcp_validate_cli:validate_transport(<<"http">>).

%% Full compliance validation
-spec validate_compliance() -> {ok, map()} | {error, term()}.
erlmcp_validate_cli:validate_compliance().

%% Comprehensive validation
-spec validate_all() -> {ok, map()} | {error, term()}.
erlmcp_validate_cli:validate_all().

%% Generate compliance report
-spec generate_compliance_report(text | json | markdown | html) -> {ok, binary()} | {error, term()}.
{ok, ReportText} = erlmcp_validate_cli:generate_compliance_report(text).
{ok, ReportJson} = erlmcp_validate_cli:generate_compliance_report(json).
{ok, ReportMarkdown} = erlmcp_validate_cli:generate_compliance_report(markdown).
{ok, ReportHtml} = erlmcp_validate_cli:generate_compliance_report(html).
```

## CLI Usage (Escript)

### Installation

```bash
# Compile the project
TERM=dumb rebar3 compile

# Make escript executable (already done)
chmod +x bin/erlmcp-validate

# Optional: Add to PATH
export PATH=$PATH:/path/to/erlmcp/bin
```

### Commands

#### 1. Spec Validation

Validate against hardcoded MCP 2025-11-25 specification:

```bash
./bin/erlmcp-validate spec
```

Output:
```
================================================================================
Spec Validation Result
================================================================================

Status: passed
Spec Version: 2025-11-25
Timestamp: 2026-01-31T12:00:00Z

Validation Summary:
  methods:
    total: 20
    required: 2
    optional: 18
  error_codes:
    total: 94
    json_rpc: 5
    mcp_protocol: 89
  transports:
    total: 3
    stream_based: 3
  capabilities:
    total: 6
    server_caps: 5
    client_caps: 1
```

#### 2. Protocol Message Validation

Validate a JSON-RPC/MCP message from a file:

```bash
./bin/erlmcp-validate protocol --file test_message.json
```

Example `test_message.json`:
```json
{
  "jsonrpc": "2.0",
  "method": "tools/call",
  "params": {
    "name": "get_weather",
    "arguments": {
      "city": "San Francisco"
    }
  },
  "id": 1
}
```

Output:
```
================================================================================
Protocol Validation Result
================================================================================

Status: passed
Message: Message is valid
Timestamp: 2026-01-31T12:00:00Z

Validation Details:
  message_structure: passed
  method_call: passed
```

#### 3. Transport Validation

Validate a single transport:

```bash
./bin/erlmcp-validate transport stdio
./bin/erlmcp-validate transport tcp
./bin/erlmcp-validate transport http
./bin/erlmcp-validate transport websocket
```

Output:
```
================================================================================
Transport Validation Result
================================================================================

Transport: erlmcp_transport_stdio
Status: passed
Timestamp: 2026-01-31T12:00:00Z

Summary:
  total_checks: 25
  passed_checks: 25
  failed_checks: 0
```

#### 4. Full Compliance Suite

Run complete compliance validation:

```bash
./bin/erlmcp-validate compliance
```

Output:
```
================================================================================
Compliance Validation Result
================================================================================

Status: passed
Compliance Score: 95.50%
Timestamp: 2026-01-31T12:00:00Z

Summary:
  total_requirements: 100
  passed_tests: 95
  compliance_percentage: 95.5
```

#### 5. Comprehensive Validation

Run all validators:

```bash
./bin/erlmcp-validate all
```

### Legacy Commands

The CLI also supports legacy command syntax:

```bash
# Run all validation sections
./bin/erlmcp-validate run --all

# Run specific section
./bin/erlmcp-validate run --section protocol
./bin/erlmcp-validate run --section transport
./bin/erlmcp-validate run --section security
./bin/erlmcp-validate run --section error_handling
./bin/erlmcp-validate run --section performance

# Run with specific transport
./bin/erlmcp-validate run --transport tcp --verbose

# Run with specific format
./bin/erlmcp-validate run --format json
./bin/erlmcp-validate run --format markdown

# Generate compliance report
./bin/erlmcp-validate report --format markdown --output compliance_report.md
./bin/erlmcp-validate report --format html --output compliance_report.html

# Quick check
./bin/erlmcp-validate quick-check

# Show status
./bin/erlmcp-validate status

# Show version
./bin/erlmcp-validate --version

# Show help
./bin/erlmcp-validate --help
```

## Output Formats

### Text Format (Default)

```
================================================================================
Validation Report
================================================================================

  protocol: [PASS]
    passed: 50
    failed: 0
    compliance: 100.0

  transport: [PASS]
    passed: 40
    failed: 0
    compliance: 100.0

Summary:
  total: 2
  passed: 2
  warning: 0
  failed: 0
  status: success
```

### JSON Format

```bash
./bin/erlmcp-validate run --all --format json
```

```json
{
  "timestamp": 1738324800,
  "sections": ["protocol", "transport", "security", "error_handling", "performance"],
  "transport": "all",
  "results": [
    {
      "protocol": {
        "status": "ok",
        "details": {
          "passed": 50,
          "failed": 0,
          "compliance": 100.0
        }
      }
    }
  ],
  "summary": {
    "total": 5,
    "passed": 5,
    "warning": 0,
    "failed": 0,
    "status": "success"
  }
}
```

### Markdown Format

```bash
./bin/erlmcp-validate run --all --format markdown > report.md
```

```markdown
# Validation Report

## Results

### [PASS] protocol

- **passed**: 50
- **failed**: 0
- **compliance**: 100.0

### [PASS] transport

- **passed**: 40
- **failed**: 0
- **compliance**: 100.0

## Summary

- **total**: 5
- **passed**: 5
- **warnings**: 0
- **failed**: 0
- **status**: success
```

## Validators

### 1. Protocol Validator (`erlmcp_protocol_validator`)

Validates:
- JSON-RPC 2.0 compliance
- MCP protocol version
- Request/response formats
- Method signatures
- Error codes

### 2. Transport Validator (`erlmcp_transport_validator`)

Validates:
- Transport behavior interface (`erlmcp_transport`)
- Callback implementations (init/2, send/2, close/1)
- Registry integration (gproc)
- Message framing
- Lifecycle management

### 3. Security Validator (`erlmcp_security_validator`)

Validates:
- Authentication mechanisms
- Authorization checks
- Input validation
- Secrets management
- JWT validation

### 4. Performance Validator (`erlmcp_performance_validator`)

Validates:
- Latency requirements
- Throughput benchmarks
- Memory usage
- Connection limits
- Stress testing

## Compliance Reporting

### Generate Reports

```erlang
%% From Erlang
{ok, TextReport} = erlmcp_validate_cli:generate_compliance_report(text).
{ok, JsonReport} = erlmcp_validate_cli:generate_compliance_report(json).
{ok, MarkdownReport} = erlmcp_validate_cli:generate_compliance_report(markdown).
{ok, HtmlReport} = erlmcp_validate_cli:generate_compliance_report(html).

%% Save to file
file:write_file("compliance_report.md", MarkdownReport).
file:write_file("compliance_report.html", HtmlReport).
```

```bash
# From CLI
./bin/erlmcp-validate report --format markdown --output compliance_report.md
./bin/erlmcp-validate report --format html --output compliance_report.html
```

### Report Contents

- **Summary**: Overall compliance score and statistics
- **By Section**: Compliance breakdown by protocol section
- **Evidence**: Test results with timestamps and evidence
- **Gaps**: Missing or failing requirements
- **Recommendations**: Actionable improvement suggestions
- **Traceability Matrix**: Requirement-to-test mapping

## Integration with CI/CD

### GitHub Actions

```yaml
name: MCP Compliance Validation

on: [push, pull_request]

jobs:
  validate:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v2
      - uses: erlef/setup-beam@v1
        with:
          otp-version: '26.2'
          rebar3-version: '3.22'

      - name: Compile
        run: TERM=dumb rebar3 compile

      - name: Run Compliance Validation
        run: ./bin/erlmcp-validate compliance

      - name: Generate Report
        run: ./bin/erlmcp-validate report --format markdown --output compliance_report.md

      - name: Upload Report
        uses: actions/upload-artifact@v2
        with:
          name: compliance-report
          path: compliance_report.md
```

### Quality Gate

```bash
#!/bin/bash
# quality_gate.sh

# Run compliance validation
./bin/erlmcp-validate compliance > /tmp/compliance_result.json

# Extract compliance score
SCORE=$(cat /tmp/compliance_result.json | jq '.compliance_score')

# Enforce minimum compliance threshold
if (( $(echo "$SCORE < 80.0" | bc -l) )); then
    echo "FAILED: Compliance score $SCORE% is below threshold (80%)"
    exit 1
fi

echo "PASSED: Compliance score $SCORE%"
exit 0
```

## Examples

### Validate Custom MCP Server

```erlang
%% Start your MCP server
{ok, ServerPid} = my_mcp_server:start_link().

%% Validate protocol compliance
Message = #{
    <<"jsonrpc">> => <<"2.0">>,
    <<"method">> => <<"tools/list">>,
    <<"params">> => #{},
    <<"id">> => 1
},

case erlmcp_validate_cli:run(protocol, Message) of
    {ok, Result} ->
        io:format("Protocol validation: ~p~n", [Result]);
    {error, Reason} ->
        io:format("Validation failed: ~p~n", [Reason])
end.
```

### Batch Validation

```erlang
%% Validate multiple messages
Messages = [
    #{<<"jsonrpc">> => <<"2.0">>, <<"method">> => <<"initialize">>, <<"id">> => 1},
    #{<<"jsonrpc">> => <<"2.0">>, <<"method">> => <<"tools/list">>, <<"id">> => 2},
    #{<<"jsonrpc">> => <<"2.0">>, <<"method">> => <<"resources/read">>, <<"id">> => 3}
],

Results = lists:map(fun(Msg) ->
    erlmcp_validate_cli:run(protocol, Msg)
end, Messages),

%% Check all passed
AllPassed = lists:all(fun({ok, _}) -> true; (_) -> false end, Results),
io:format("All validations passed: ~p~n", [AllPassed]).
```

## Development

### Adding New Validators

1. Create module implementing validation logic
2. Add gen_server behavior if stateful
3. Expose `run/1` function
4. Register with `erlmcp_validate_cli`
5. Add tests

Example:

```erlang
-module(erlmcp_custom_validator).
-behaviour(gen_server).

-export([start_link/0, run/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

run(TransportModule) ->
    gen_server:call(?MODULE, {run, TransportModule}).

%% Implement gen_server callbacks...
```

### Testing

```bash
# Run validator tests
rebar3 eunit --module=erlmcp_validate_cli_tests
rebar3 eunit --module=erlmcp_protocol_validator_tests
rebar3 eunit --module=erlmcp_transport_validator_tests

# Run compliance test suite
rebar3 ct --suite=erlmcp_spec_compliance_SUITE

# Run all validation tests
rebar3 eunit --app=erlmcp_validation
```

## Troubleshooting

### Module Not Loaded

```
Error: Failed to load erlmcp_validate_cli module
```

**Solution**: Ensure project is compiled:
```bash
TERM=dumb rebar3 compile
```

### Validator Not Started

```
Error: {validator_start_failed, erlmcp_protocol_validator, Reason}
```

**Solution**: Validators are started automatically. Check if gen_server is already running:
```erlang
whereis(erlmcp_protocol_validator).
```

### Invalid JSON

```
Error: invalid_json
```

**Solution**: Validate JSON syntax:
```bash
cat test.json | jq .
```

## See Also

- [MCP Specification 2025-11-25](https://spec.modelcontextprotocol.io/2025-11-25/)
- [erlmcp_spec_parser](../src/erlmcp_spec_parser.erl)
- [erlmcp_compliance_report](../src/erlmcp_compliance_report.erl)
- [Compliance Test Suite](../test/erlmcp_spec_compliance_SUITE.erl)

## License

Same as erlmcp project.
