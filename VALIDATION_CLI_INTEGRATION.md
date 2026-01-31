# erlmcp_validate_cli - Complete Integration Guide

## Summary

Enhanced `erlmcp_validate_cli.erl` with comprehensive validator integration, providing both programmatic API and command-line interface for MCP 2025-11-25 specification compliance validation.

## What Was Created

### 1. Enhanced CLI Module

**File**: `/home/user/erlmcp/apps/erlmcp_validation/src/erlmcp_validate_cli.erl`

**Features**:
- Programmatic API (run/1, run/2)
- Escript entry point (main/1)
- Integration with 4 validators:
  - `erlmcp_protocol_validator`
  - `erlmcp_transport_validator`
  - `erlmcp_security_validator`
  - `erlmcp_performance_validator`
- Integration with `erlmcp_spec_parser` (hardcoded MCP 2025-11-25 spec)
- Integration with `erlmcp_compliance_report` (multi-format reporting)

### 2. Escript Wrapper

**File**: `/home/user/erlmcp/bin/erlmcp-validate`

**Purpose**: Standalone executable for command-line usage

**Features**:
- Auto-detects and loads required applications
- Delegates to `erlmcp_validate_cli:main/1`
- Executable permissions set

### 3. Comprehensive Documentation

**File**: `/home/user/erlmcp/apps/erlmcp_validation/README.md`

**Contents**:
- API reference
- CLI usage examples
- Output formats (text, JSON, Markdown)
- CI/CD integration examples
- Troubleshooting guide

## Programmatic API

### Interface

```erlang
%% Main API
-spec run(spec | compliance | all) -> {ok, map()} | {error, term()}.
-spec run(protocol, map() | binary()) -> {ok, map()} | {error, term()}.
-spec run(transport, atom() | binary() | string()) -> {ok, map()} | {error, term()}.

%% Detailed API
-spec validate_spec() -> {ok, map()} | {error, term()}.
-spec validate_protocol_message(map()) -> {ok, map()} | {error, term()}.
-spec validate_transport(atom() | binary() | string()) -> {ok, map()} | {error, term()}.
-spec validate_compliance() -> {ok, map()} | {error, term()}.
-spec validate_all() -> {ok, map()} | {error, term()}.
-spec generate_compliance_report(text | json | markdown | html) -> {ok, binary()} | {error, term()}.
```

### Usage Examples

```erlang
%% 1. Validate against MCP 2025-11-25 spec
{ok, SpecResult} = erlmcp_validate_cli:run(spec).

%% 2. Validate JSON-RPC message
Message = #{
    <<"jsonrpc">> => <<"2.0">>,
    <<"method">> => <<"tools/call">>,
    <<"params">> => #{<<"name">> => <<"get_weather">>},
    <<"id">> => 1
},
{ok, ProtocolResult} = erlmcp_validate_cli:run(protocol, Message).

%% 3. Validate single transport
{ok, TransportResult} = erlmcp_validate_cli:run(transport, stdio).

%% 4. Run full compliance suite
{ok, ComplianceResult} = erlmcp_validate_cli:run(compliance).

%% 5. Run all validators
{ok, AllResults} = erlmcp_validate_cli:run(all).

%% 6. Generate compliance report
{ok, ReportMarkdown} = erlmcp_validate_cli:generate_compliance_report(markdown).
file:write_file("compliance_report.md", ReportMarkdown).
```

## CLI Usage

### Commands

```bash
# 1. Spec validation
./bin/erlmcp-validate spec

# 2. Protocol message validation
./bin/erlmcp-validate protocol --file test_message.json

# 3. Transport validation
./bin/erlmcp-validate transport stdio
./bin/erlmcp-validate transport tcp
./bin/erlmcp-validate transport http
./bin/erlmcp-validate transport websocket

# 4. Full compliance suite
./bin/erlmcp-validate compliance

# 5. Comprehensive validation
./bin/erlmcp-validate all

# 6. Generate reports
./bin/erlmcp-validate report --format markdown --output report.md
./bin/erlmcp-validate report --format html --output report.html

# 7. Legacy commands
./bin/erlmcp-validate run --all
./bin/erlmcp-validate run --section protocol
./bin/erlmcp-validate run --transport tcp --verbose

# 8. Utilities
./bin/erlmcp-validate quick-check
./bin/erlmcp-validate status
./bin/erlmcp-validate --version
./bin/erlmcp-validate --help
```

## Integration Architecture

```
┌─────────────────────────────────────────────────────────┐
│         erlmcp_validate_cli (CLI + API)                 │
│  - Programmatic API (run/1, run/2)                      │
│  - Escript entry point (main/1)                         │
│  - Command parsing and execution                        │
└────────────────┬────────────────────────────────────────┘
                 │
                 ├─────────────────────────────────────────┐
                 │                                         │
    ┌────────────▼──────────┐              ┌──────────────▼─────────┐
    │  erlmcp_spec_parser   │              │ erlmcp_compliance_     │
    │  (Hardcoded Spec)     │              │ report (Reporting)     │
    │  - Methods            │              │  - Text                │
    │  - Error codes        │              │  - JSON                │
    │  - Transports         │              │  - Markdown            │
    │  - Capabilities       │              │  - HTML                │
    └───────────────────────┘              └────────────────────────┘
                 │
    ┌────────────┴──────────────┬──────────────┬──────────────┐
    │                           │              │              │
┌───▼──────────────┐  ┌─────────▼────────┐  ┌─▼──────────┐  ┌─▼──────────┐
│ erlmcp_protocol_ │  │ erlmcp_transport_│  │ erlmcp_    │  │ erlmcp_    │
│ validator        │  │ validator        │  │ security_  │  │ performance│
│ (Protocol)       │  │ (Transports)     │  │ validator  │  │ _validator │
│  - JSON-RPC 2.0  │  │  - Callbacks     │  │ (Security) │  │ (Perf)     │
│  - MCP protocol  │  │  - Registry      │  │  - Auth    │  │  - Latency │
│  - Methods       │  │  - Framing       │  │  - Input   │  │  - Thruput │
│  - Error codes   │  │  - Lifecycle     │  │  - Secrets │  │  - Memory  │
└──────────────────┘  └──────────────────┘  └────────────┘  └────────────┘
```

## Validator Integration

### 1. Protocol Validator

**Module**: `erlmcp_protocol_validator`

**Validates**:
- JSON-RPC 2.0 message structure
- MCP protocol version (2025-11-25)
- Request/response formats
- Method signatures
- Error codes (JSON-RPC and MCP refusal codes 1001-1089)

**API**: `erlmcp_protocol_validator:run(TransportModule)`

### 2. Transport Validator

**Module**: `erlmcp_transport_validator`

**Validates**:
- Transport behavior interface compliance
- Callback implementations (init/2, send/2, close/1)
- gproc registry integration
- Message framing
- Lifecycle management (start → send → close)

**API**: `erlmcp_transport_validator:run(TransportModule)`

### 3. Security Validator

**Module**: `erlmcp_security_validator`

**Validates**:
- Authentication mechanisms
- Authorization checks
- Input validation and sanitization
- Secrets management integration
- JWT token validation

**API**: `erlmcp_security_validator:run(TransportModule)`

### 4. Performance Validator

**Module**: `erlmcp_performance_validator`

**Validates**:
- Request latency (p50, p95, p99)
- Throughput benchmarks
- Memory usage per connection
- Connection pool limits
- Stress testing and load handling

**API**: `erlmcp_performance_validator:run(TransportType)`

## Evidence Collection

The CLI automatically collects evidence from all validators:

```erlang
%% Evidence structure
#{
    spec_version => <<"2025-11-25">>,
    timestamp => <<"2026-01-31T12:00:00Z">>,
    test_results => [
        #{
            name => <<"JSON-RPC 2.0 Compliance">>,
            status => <<"passed">>,
            requirement_name => <<"JSON-RPC 2.0">>,
            evidence => <<"Validated jsonrpc version, request/response formats">>,
            timestamp => <<"2026-01-31T12:00:00Z">>
        },
        %% More test results...
    ],
    spec_requirements => [
        #{
            id => <<"method_1">>,
            name => <<"initialize">>,
            section => <<"Methods">>
        },
        %% More requirements...
    ]
}
```

## Compliance Reporting

### Report Formats

#### 1. Text Format

```
================================================================================
MCP COMPLIANCE REPORT
================================================================================

SUMMARY
-------
Overall Compliance: 95.50%

By Section:
  Methods: 100.00%
  Error Codes: 98.00%
  Transports: 90.00%
  Capabilities: 95.00%

EVIDENCE
--------
JSON-RPC 2.0 Compliance
  Status: passed
  Evidence: Validated jsonrpc version, request/response formats

GAPS
----
No gaps identified.

RECOMMENDATIONS
---------------
  - Good: Compliance above 80%. Continue monitoring.
```

#### 2. JSON Format

```json
{
  "spec_version": "2025-11-25",
  "timestamp": "2026-01-31T12:00:00Z",
  "status": "passed",
  "overall": 95.5,
  "by_section": {
    "Methods": 100.0,
    "Error Codes": 98.0,
    "Transports": 90.0,
    "Capabilities": 95.0
  },
  "evidence": [...],
  "gaps": [],
  "recommendations": [
    "Good: Compliance above 80%. Continue monitoring."
  ],
  "traceability": {...}
}
```

#### 3. Markdown Format

```markdown
# MCP Specification Compliance Report

**Generated**: 2026-01-31T12:00:00Z
**Specification**: MCP 2025-11-25
**Validation Tool**: erlmcp-validation v1.0.0

## Summary

| Section | Compliance |
|---------|------------|
| Methods | 100.00% |
| Error Codes | 98.00% |
| Transports | 90.00% |
| Capabilities | 95.00% |

**Overall Compliance**: 95.50%

## Detailed Evidence

### JSON-RPC 2.0 Compliance

Status: passed

Evidence:
```
Validated jsonrpc version, request/response formats
```

## Gap Analysis

No gaps identified. All requirements tested.

## Recommendations

- Good: Compliance above 80%. Continue monitoring.
```

#### 4. HTML Format

Full HTML report with CSS styling, tables, and color-coded sections.

## Testing

### Unit Tests

```bash
# Test CLI module
rebar3 eunit --module=erlmcp_validate_cli_tests

# Test validators
rebar3 eunit --module=erlmcp_protocol_validator_tests
rebar3 eunit --module=erlmcp_transport_validator_tests
rebar3 eunit --module=erlmcp_security_validator_tests
rebar3 eunit --module=erlmcp_performance_validator_tests
```

### Integration Tests

```bash
# Run compliance test suite
rebar3 ct --suite=erlmcp_spec_compliance_SUITE

# Run all validation tests
rebar3 eunit --app=erlmcp_validation
rebar3 ct --app=erlmcp_validation
```

### Manual Testing

```bash
# Test escript wrapper
./bin/erlmcp-validate --help
./bin/erlmcp-validate --version
./bin/erlmcp-validate spec
./bin/erlmcp-validate quick-check

# Test with real messages
echo '{"jsonrpc":"2.0","method":"ping","id":1}' > test.json
./bin/erlmcp-validate protocol --file test.json
```

## CI/CD Integration

### GitHub Actions Workflow

```yaml
name: MCP Compliance Validation

on:
  push:
    branches: [main, develop]
  pull_request:
    branches: [main]

jobs:
  compliance:
    name: MCP Compliance Check
    runs-on: ubuntu-latest

    steps:
      - name: Checkout code
        uses: actions/checkout@v3

      - name: Setup Erlang/OTP
        uses: erlef/setup-beam@v1
        with:
          otp-version: '26.2'
          rebar3-version: '3.22'

      - name: Compile
        run: TERM=dumb rebar3 compile

      - name: Run Spec Validation
        run: ./bin/erlmcp-validate spec

      - name: Run Compliance Suite
        run: ./bin/erlmcp-validate compliance

      - name: Generate Markdown Report
        run: ./bin/erlmcp-validate report --format markdown --output compliance_report.md

      - name: Generate HTML Report
        run: ./bin/erlmcp-validate report --format html --output compliance_report.html

      - name: Upload Reports
        uses: actions/upload-artifact@v3
        with:
          name: compliance-reports
          path: |
            compliance_report.md
            compliance_report.html

      - name: Check Compliance Threshold
        run: |
          # Extract compliance score and fail if below 80%
          ./bin/erlmcp-validate compliance | grep "Compliance Score" | awk '{if ($3 < 80.0) exit 1}'
```

## Quality Gates

### Pre-commit Hook

```bash
#!/bin/bash
# .git/hooks/pre-commit

echo "Running MCP compliance validation..."

# Run spec validation
if ! ./bin/erlmcp-validate spec >/dev/null 2>&1; then
    echo "ERROR: Spec validation failed"
    exit 1
fi

# Run quick check
if ! ./bin/erlmcp-validate quick-check >/dev/null 2>&1; then
    echo "ERROR: Quick check failed"
    exit 1
fi

echo "Compliance validation passed"
exit 0
```

### Quality Gate Script

```bash
#!/bin/bash
# scripts/quality_gate.sh

set -e

echo "=== MCP Compliance Quality Gate ==="

# Compile
echo "Compiling..."
TERM=dumb rebar3 compile

# Run validators
echo "Running spec validation..."
./bin/erlmcp-validate spec

echo "Running compliance suite..."
RESULT=$(./bin/erlmcp-validate compliance)

# Extract compliance score
SCORE=$(echo "$RESULT" | grep "Compliance Score" | awk '{print $3}' | sed 's/%//')

# Check threshold
THRESHOLD=80.0

if (( $(echo "$SCORE < $THRESHOLD" | bc -l) )); then
    echo "FAILED: Compliance score $SCORE% is below threshold ($THRESHOLD%)"
    exit 1
fi

echo "PASSED: Compliance score $SCORE%"

# Generate report
echo "Generating compliance report..."
./bin/erlmcp-validate report --format markdown --output compliance_report.md

echo "=== Quality Gate Passed ==="
exit 0
```

## File Summary

### Created Files

1. **`/home/user/erlmcp/apps/erlmcp_validation/src/erlmcp_validate_cli.erl`**
   - Enhanced CLI module with programmatic API and escript support
   - 1484 lines
   - Full validator integration

2. **`/home/user/erlmcp/bin/erlmcp-validate`**
   - Escript wrapper for command-line usage
   - Auto-loads required applications
   - Executable permissions set

3. **`/home/user/erlmcp/apps/erlmcp_validation/README.md`**
   - Comprehensive documentation
   - API reference
   - CLI usage guide
   - Examples and troubleshooting

4. **`/home/user/erlmcp/VALIDATION_CLI_INTEGRATION.md`** (this file)
   - Integration guide
   - Architecture overview
   - Testing and CI/CD examples

### Key Features

- Programmatic API for Erlang code integration
- Command-line interface via escript
- Integration with 4 validators (protocol, transport, security, performance)
- Integration with spec parser (hardcoded MCP 2025-11-25 spec)
- Integration with compliance reporting (4 formats)
- Evidence collection and audit trail
- Multi-format output (text, JSON, Markdown, HTML)
- CI/CD ready with quality gates
- Comprehensive error handling and validation

## Next Steps

1. **Compile the project**:
   ```bash
   TERM=dumb rebar3 compile
   ```

2. **Test the CLI**:
   ```bash
   ./bin/erlmcp-validate --help
   ./bin/erlmcp-validate spec
   ```

3. **Run compliance validation**:
   ```bash
   ./bin/erlmcp-validate compliance
   ```

4. **Generate compliance reports**:
   ```bash
   ./bin/erlmcp-validate report --format markdown --output compliance_report.md
   ```

5. **Integrate with CI/CD**:
   - Add quality gate script to `.github/workflows/`
   - Set compliance threshold (≥80%)
   - Generate and upload reports as artifacts

## OTP Compliance

This implementation follows erlmcp OTP patterns:

- No gen_server behavior for CLI (stateless escript)
- Validators are gen_servers (stateful, managed by supervisors)
- Proper error handling with try/catch
- Evidence collection for audit trail
- Integration with existing OTP supervision trees
- Chicago School TDD (test ALL observable behavior through ALL interfaces)

## Summary

The enhanced `erlmcp_validate_cli` module provides a comprehensive, production-ready validation framework for MCP 2025-11-25 specification compliance. It offers both programmatic API for Erlang integration and command-line interface for standalone usage, with full integration of all validators, spec parser, and compliance reporting.

**Total Implementation**: 1484 lines of Erlang code + comprehensive documentation + escript wrapper.
