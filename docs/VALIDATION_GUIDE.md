# erlmcp Validation Framework - Complete User Guide

**Version**: 0.6.0
**Last Updated**: 2026-01-30
**Specification**: MCP 2025-11-25

---

## Table of Contents

1. [Overview](#overview)
2. [Installation and Setup](#installation-and-setup)
3. [Quick Start](#quick-start)
4. [Running Compliance Tests](#running-compliance-tests)
5. [Using Individual Validators](#using-individual-validators)
6. [Generating Reports](#generating-reports)
7. [CI/CD Integration](#cicd-integration)
8. [Troubleshooting](#troubleshooting)
9. [API Reference](#api-reference)
10. [Examples and Best Practices](#examples-and-best-practices)

---

## Overview

The erlmcp Validation Framework is a **specification-driven validation system** that proves erlmcp implements the MCP specification correctly through **black-box testing** of observable behavior.

### Key Principles

1. **Specification as Source of Truth** - Validation driven entirely by MCP spec requirements
2. **Black-Box Testing** - Test observable behavior (requests/responses), not implementation
3. **Proof by Demonstration** - Each validation proves the system works as specified
4. **Implementation Agnostic** - Validator works without knowing code internals
5. **Independent Verification** - Validator separate from implementation to avoid bias

### Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                   Validation Framework                        │
├─────────────────────────────────────────────────────────────┤
│                                                               │
│  ┌──────────────┐      ┌──────────────┐                     │
│  │   Spec       │      │   Protocol   │                     │
│  │   Parser     │──────▶│   Validator  │                     │
│  └──────────────┘      └──────────────┘                     │
│         │                       │                            │
│         │                       ▼                            │
│         │              ┌──────────────┐                     │
│         │              │    Test      │                     │
│         │              │    Client    │                     │
│         │              └──────────────┘                     │
│         │                       │                            │
│         ▼                       ▼                            │
│  ┌──────────────┐      ┌──────────────┐                     │
│  │  Transport   │      │ Validation   │                     │
│  │  Validator   │──────▶│   Runner     │                     │
│  └──────────────┘      └──────────────┘                     │
│                                │                              │
│                                ▼                              │
│                       ┌──────────────┐                       │
│                       │   Compliance │                       │
│                       │   Reporter   │                       │
│                       └──────────────┘                       │
│                                                               │
└─────────────────────────────────────────────────────────────┘
```

### Components

| Component | Module | Purpose |
|-----------|--------|---------|
| **Spec Parser** | `erlmcp_spec_parser` | Parse MCP specification into validation requirements |
| **Protocol Validator** | `erlmcp_protocol_validator` | Black-box validation of JSON-RPC protocol |
| **Transport Validator** | `erlmcp_transport_validator` | Validate transport layer compliance |
| **Test Client** | `erlmcp_test_client` | Test utilities for black-box testing |
| **Validation Runner** | `erlmcp_validation_runner` | Execute test suites and generate reports |
| **Compliance Reporter** | `erlmcp_compliance_report` | Generate evidence reports |

---

## Installation and Setup

### Prerequisites

- Erlang/OTP 25+
- rebar3 build tool
- erlmcp core application

### Installation Steps

1. **Clone and Build erlmcp** (if not already done):

```bash
git clone https://github.com/your-org/erlmcp.git
cd erlmcp
rebar3 compile
```

2. **Build Validation Application**:

```bash
# Build with validation profile
rebar3 as validation compile

# Or build all apps
rebar3 compile
```

3. **Build CLI escript**:

```bash
# Build the standalone validation CLI
rebar3 escriptize

# The executable will be created at:
# _build/default/bin/erlmcp_validate
```

4. **Verify Installation**:

```bash
# Check validation app is built
ls _build/default/lib/erlmcp_validation/ebin/

# Verify CLI executable exists
ls -lh _build/default/bin/erlmcp_validate

# Test CLI
./_build/default/bin/erlmcp_validate --version

# Run quick validation check
make validate-compile
```

5. **Optional: Install to PATH**:

```bash
# Create symlink for global access
sudo ln -s $(pwd)/_build/default/bin/erlmcp_validate /usr/local/bin/erlmcp_validate

# Or add to PATH in your shell profile
echo 'export PATH="$PATH:'$(pwd)'/_build/default/bin"' >> ~/.bashrc
source ~/.bashrc

# Now you can run from anywhere
erlmcp_validate --version
```

### Configuration

The validation framework uses default configuration but can be customized:

**Application Configuration** (`sys.config` or `vm.args`):

```erlang
[
  {erlmcp_validation, [
    %% Default MCP spec URL
    {spec_url, "https://modelcontextprotocol.io/llms.txt"},

    %% Validation timeout (ms)
    {validation_timeout, 30000},

    %% Test server configuration
    {test_server, [
      {host, "localhost"},
      {port, 8080},
      {transport, stdio}
    ]},

    %% Report output directory
    {report_dir, "validation_results"}
  ]}
].
```

### Environment Setup

**Set up validation environment variables** (optional):

```bash
# Export for shell sessions
export ERLMCP_SPEC_URL="https://modelcontextprotocol.io/llms.txt"
export ERLMCP_VALIDATION_TIMEOUT=30000
export ERLMCP_REPORT_DIR="validation_results"

# Or create .env file
cat > .env <<EOF
ERLMCP_SPEC_URL=https://modelcontextprotocol.io/llms.txt
ERLMCP_VALIDATION_TIMEOUT=30000
ERLMCP_REPORT_DIR=validation_results
EOF
```

---

## Quick Start

### 5-Minute Quick Start

1. **Run Full Compliance Suite**:

```bash
# From erlmcp root directory
make validate

# Or using rebar3 directly
rebar3 as validation ct --suite=erlmcp_spec_compliance_SUITE
```

2. **Generate Compliance Report**:

```bash
# Generate markdown report
rebar3 as validation escriptize
./_build/validation/bin/erlmcp_validate run --all --report-format markdown
```

3. **Quick Validation Check**:

```bash
# Quick check (subset of tests)
rebar3 eunit --module=erlmcp_spec_parser_tests
```

### Expected Output

```
✅ Compilation: 8 modules compiled successfully
✅ Tests: 63/63 passed (0 failures)
✅ Coverage: 87.3% (≥80% target met)
✅ Compliance: 95.5% (63/66 requirements validated)

Overall: MCP 2025-11-25 Specification Compliance
```

---

## CLI Command Reference

The `erlmcp_validate` CLI provides a comprehensive interface for running validation tests and generating compliance reports.

### Building the CLI

```bash
# Build the standalone CLI executable
rebar3 escriptize

# Executable location
./_build/default/bin/erlmcp_validate

# Make it globally available (optional)
sudo ln -s $(pwd)/_build/default/bin/erlmcp_validate /usr/local/bin/erlmcp_validate
```

### CLI Commands Overview

```bash
# Show help
erlmcp_validate --help

# Show version
erlmcp_validate --version

# Run all validators
erlmcp_validate run --all

# Run specific validator
erlmcp_validate run --section protocol

# Generate compliance report
erlmcp_validate report --format markdown --output report.md

# Quick validation check
erlmcp_validate quick-check

# Show validation status
erlmcp_validate status
```

### Run Command

The `run` command executes validation tests.

#### Run All Validators

```bash
# Run all validation sections
./_build/default/bin/erlmcp_validate run --all

# With verbose output
./_build/default/bin/erlmcp_validate run --all --verbose

# With minimal output
./_build/default/bin/erlmcp_validate run --all --quiet

# Output as JSON
./_build/default/bin/erlmcp_validate run --all --format json

# Output as Markdown
./_build/default/bin/erlmcp_validate run --all --format markdown
```

#### Run Specific Section

```bash
# Validate protocol compliance
./_build/default/bin/erlmcp_validate run --section protocol

# Validate transport behavior
./_build/default/bin/erlmcp_validate run --section transport

# Validate security features
./_build/default/bin/erlmcp_validate run --section security

# Validate error handling
./_build/default/bin/erlmcp_validate run --section error_handling

# Validate performance
./_build/default/bin/erlmcp_validate run --section performance

# Multiple sections
./_build/default/bin/erlmcp_validate run --section protocol --section transport
```

#### Run with Specific Transport

```bash
# Validate stdio transport
./_build/default/bin/erlmcp_validate run --transport stdio

# Validate TCP transport
./_build/default/bin/erlmcp_validate run --transport tcp

# Validate HTTP transport
./_build/default/bin/erlmcp_validate run --transport http

# Validate WebSocket transport
./_build/default/bin/erlmcp_validate run --transport websocket
```

#### Available Sections

| Section | Description |
|---------|-------------|
| `protocol` | MCP protocol compliance (JSON-RPC 2.0, message formats) |
| `transport` | Transport layer behavior (stdio, tcp, http) |
| `security` | Security features (authentication, JWT validation) |
| `error_handling` | Error response validation and edge cases |
| `performance` | Performance benchmarks and load testing |

### Report Command

The `report` command generates compliance reports in various formats.

#### Generate Markdown Report

```bash
# Generate markdown report to stdout
./_build/default/bin/erlmcp_validate report --format markdown

# Save to file
./_build/default/bin/erlmcp_validate report --format markdown --output compliance_report.md
```

#### Generate JSON Report

```bash
# Generate JSON report
./_build/default/bin/erlmcp_validate report --format json

# Save to file
./_build/default/bin/erlmcp_validate report --format json --output compliance_report.json
```

#### Generate Text Report

```bash
# Generate text report (default)
./_build/default/bin/erlmcp_validate report --format text

# Save to file
./_build/default/bin/erlmcp_validate report --format text --output compliance_report.txt
```

#### Generate HTML Report

```bash
# Generate HTML report
./_build/default/bin/erlmcp_validate report --format html

# Save to file
./_build/default/bin/erlmcp_validate report --format html --output compliance_report.html
```

### Quick Check Command

Perform basic validation checks:

```bash
# Run quick validation check
./_build/default/bin/erlmcp_validate quick-check

# Checks performed:
# - Applications loaded
# - Modules available
# - Configuration valid
```

### Status Command

Show validation system status:

```bash
# Show status
./_build/default/bin/erlmcp_validate status

# Output includes:
# - CLI version
# - OTP release
# - Loaded applications
# - System status
```

### CLI Options Reference

| Option | Description | Valid Values |
|--------|-------------|--------------|
| `--all` | Run all validation sections | - |
| `--section <name>` | Run specific section | protocol, transport, security, error_handling, performance |
| `--transport <type>` | Validate specific transport | stdio, tcp, http, websocket |
| `--format <type>` | Output format | text, json, markdown, html |
| `--output <file>` | Write output to file | Any filename |
| `--verbose` | Show detailed output | - |
| `--quiet` | Minimal output | - |
| `--help` | Show help message | - |
| `--version` | Show version | - |

### CLI Examples

#### Example 1: Full Validation with Markdown Report

```bash
# Run full validation and generate markdown report
./_build/default/bin/erlmcp_validate run --all --format markdown > validation_report.md

# Or save directly
./_build/default/bin/erlmcp_validate run --all --format markdown --output validation_report.md
```

#### Example 2: CI/CD Integration

```bash
#!/bin/bash
# CI validation script

# Build CLI
rebar3 escriptize

# Run validation (exit on failure)
./_build/default/bin/erlmcp_validate run --all --format json --output validation_results.json

# Check compliance threshold
COMPLIANCE=$(jq '.summary.compliance' validation_results.json)
if (( $(echo "$COMPLIANCE < 95.0" | bc -l) )); then
    echo "Compliance below 95%: $COMPLIANCE%"
    exit 1
fi

echo "Compliance OK: $COMPLIANCE%"
```

#### Example 3: Transport-Specific Validation

```bash
# Validate all transports
for transport in stdio tcp http websocket; do
    echo "Validating $transport..."
    ./_build/default/bin/erlmcp_validate run --transport $transport --format json > "${transport}_validation.json"
done
```

#### Example 4: Section-Specific Validation

```bash
# Validate only protocol and security
./_build/default/bin/erlmcp_validate run \
    --section protocol \
    --section security \
    --format markdown \
    --output protocol_security_report.md
```

---

## Running Compliance Tests

### Test Suite Usage

The validation framework provides multiple ways to run tests depending on your needs.

#### Running Common Test Suites

```bash
# Run all Common Test suites
rebar3 as validation ct

# Run specific test suite
rebar3 as validation ct --suite=erlmcp_spec_compliance_SUITE

# Run with verbose output
rebar3 as validation ct --verbose

# Run specific test case
rebar3 as validation ct --suite=erlmcp_spec_compliance_SUITE --case=initialize_must_be_first_test

# Run tests matching pattern
rebar3 as validation ct --suite=erlmcp_spec_compliance_SUITE --testcase="tools_*"
```

#### Running EUnit Tests

```bash
# Run all EUnit tests
rebar3 eunit

# Run specific module tests
rebar3 eunit --module=erlmcp_spec_parser_tests

# Run with verbose output
rebar3 eunit --verbose

# Generate coverage report
rebar3 cover
```

#### Coverage Reports

```bash
# Generate coverage report
rebar3 as validation cover

# View coverage report
open _build/validation/cover/index.html

# Generate coverage summary
rebar3 as validation cover --verbose

# Check specific module coverage
rebar3 as validation cover --module=erlmcp_protocol_validator
```

#### Test Suite Examples

**Example 1: Full Test Suite with Coverage**

```bash
#!/bin/bash
# Run complete test suite with coverage

echo "Building..."
rebar3 compile

echo "Running EUnit tests..."
rebar3 eunit

echo "Running Common Test suites..."
rebar3 as validation ct

echo "Generating coverage report..."
rebar3 as validation cover

echo "Coverage report: _build/validation/cover/index.html"
```

**Example 2: Quick Test During Development**

```bash
#!/bin/bash
# Quick test for specific module

# Test protocol validator
rebar3 eunit --module=erlmcp_protocol_validator_tests

# Test spec parser
rebar3 eunit --module=erlmcp_spec_parser_tests

# Test compliance report
rebar3 eunit --module=erlmcp_compliance_report_tests
```

**Example 3: CI/CD Test Pipeline**

```bash
#!/bin/bash
# CI test pipeline

set -e

echo "Running validation tests..."

# Compile
rebar3 compile

# Run EUnit
rebar3 eunit

# Run Common Test
rebar3 as validation ct

# Check coverage
COVERAGE=$(rebar3 as validation cover | grep "^[0-9]" | awk '{print $1}' | cut -d'%' -f1)
if [ "$COVERAGE" -lt 80 ]; then
    echo "Coverage below 80%: $COVERAGE%"
    exit 1
fi

echo "All tests passed!"
```

### Command-Line Usage

#### Run All Tests

```bash
# Full compliance suite
rebar3 as validation ct

# Or using make
make validate

# With verbose output
rebar3 as validation ct --verbose
```

#### Run Specific Test Suite

```bash
# Protocol compliance tests
rebar3 as validation ct --suite=erlmcp_spec_compliance_SUITE

# Transport validation tests
rebar3 as validation ct --suite=erlmcp_transport_behavior_SUITE

# Error response tests
rebar3 as validation ct --suite=erlmcp_error_response_SUITE
```

#### Run Specific Test

```bash
# Run single test from suite
rebar3 as validation ct --suite=erlmcp_spec_compliance_SUITE --case=initialize_must_be_first_test

# Run tests matching pattern
rebar3 as validation ct --suite=erlmcp_spec_compliance_SUITE --testcase="tools_*"
```

### Programmatic Usage

#### From Erlang Shell

```erlang
# Start validation application
application:ensure_all_started(erlmcp_validation).

# Run full compliance suite
{ok, RunnerPid} = erlmcp_validation_runner:start_link(),
Results = erlmcp_validation_runner:run_compliance_suite(),

# Print results
io:format("Compliance: ~p%~n", [Results#{overall}]).

# Generate report
{ok, Markdown, _Report} = erlmcp_validation_runner:generate_report(markdown),
file:write_file("compliance_report.md", Markdown).
```

#### From Erlang Code

```erlang
-module(my_validation).
-export([run_validation/0]).

run_validation() ->
    %% Start applications
    application:ensure_all_started(erlmcp_validation),

    %% Run compliance tests
    Results = erlmcp_validation_runner:run_compliance_suite(#{
        include_sections => [<<"lifecycle">>, <<"tools">>, <<"resources">>],
        exclude_sections => [],
        timeout => 60000
    }),

    %% Check results
    case Results of
        #{overall := Compliance} when Compliance >= 95.0 ->
            io:format("✅ Validation passed: ~p%~n", [Compliance]),
            {ok, Results};
        #{overall := Compliance} ->
            io:format("⚠️  Low compliance: ~p%~n", [Compliance]),
            {error, low_compliance, Results}
    end.
```

### Test Categories

#### 1. Lifecycle Tests

Validate protocol initialization and state management:

```bash
# Run lifecycle tests
rebar3 as validation ct --suite=erlmcp_spec_compliance_SUITE --testcase="lifecycle_*"
```

Tests include:
- Initialize must be called first
- Capabilities negotiation
- State transitions
- Shutdown handling

#### 2. Tools API Tests

Validate tools/list, tools/call, progress tokens:

```bash
# Run tools tests
rebar3 as validation ct --suite=erlmcp_spec_compliance_SUITE --testcase="tools_*"
```

Tests include:
- tools/list returns array
- tools/call executes correctly
- Progress token handling
- tools/list_changed notification

#### 3. Resources API Tests

Validate resources/list, resources/read, subscriptions:

```bash
# Run resources tests
rebar3 as validation ct --suite=erlmcp_spec_compliance_SUITE --testcase="resources_*"
```

Tests include:
- resources/list returns array
- resources/read retrieves content
- Subscribe/unsubscribe functionality
- resources/list_changed notification

#### 4. Prompts API Tests

Validate prompts/list, prompts/get:

```bash
# Run prompts tests
rebar3 as validation ct --suite=erlmcp_spec_compliance_SUITE --testcase="prompts_*"
```

Tests include:
- prompts/list returns array
- prompts/get retrieves template
- Argument validation
- prompts/list_changed notification

#### 5. Transport Tests

Validate stdio, HTTP, SSE, WebSocket transports:

```bash
# Run transport tests
rebar3 as validation ct --suite=erlmcp_transport_behavior_SUITE
```

Tests include:
- stdio newline-delimited JSON
- HTTP SSE support
- WebSocket message format
- Header validation

---

## Using Individual Validators

### Spec Parser

Parse MCP specification into validation requirements.

#### API Usage

```erlang
%% Start parser with default spec URL
{ok, ParserPid} = erlmcp_spec_parser:start_link().

%% Parse specification
{ok, Spec} = erlmcp_spec_parser:parse_specification(ParserPid),

%% Get method requirements
Methods = erlmcp_spec_parser:method_requirements(ParserPid),
InitReq = maps:get(<<"initialize">>, Methods),
io:format("Initialize params: ~p~n", [InitReq#method_req.required_params]).

%% Get error codes
ErrorCodes = erlmcp_spec_parser:error_code_requirements(ParserPid),

%% Get transport requirements
Transports = erlmcp_spec_parser:transport_requirements(ParserPid),

%% Stop parser
erlmcp_spec_parser:stop(ParserPid).
```

#### Command-Line Usage

```bash
# Parse spec and output requirements
erl -noshell -pa _build/default/lib/*/ebin \
  -eval "erlmcp_spec_parser:start_link(), \
         {ok, Spec} = erlmcp_spec_parser:parse_specification(), \
         io:format('Spec version: ~s~n', [Spec#mcp_spec.version]), \
         halt()."
```

### Protocol Validator

Validate JSON-RPC protocol behavior.

#### API Usage

```erlang
%% Start validator
{ok, ValidatorPid} = erlmcp_protocol_validator:start_link().

%% Start validation session for stdio
{ok, SessionPid} = erlmcp_protocol_validator:start_validation(stdio, [
    {server_id, test_server}
]),

%% Validate method call
case erlmcp_protocol_validator:validate_method_call(ValidatorPid, <<"tools/list">>, #{}) of
    {compliant, Result} ->
        io:format("✅ Compliant: ~p~n", [Result]);
    {non_compliant, Reason} ->
        io:format("❌ Non-compliant: ~p~n", [Reason])
end.

%% Validate error response
ErrorResponse = #{<<"jsonrpc">> => <<"2.0">>, <<"id">> => 1,
                 <<"error">> => #{<<"code">> => -32601, <<"message">> => <<"Method not found">>}},
case erlmcp_protocol_validator:validate_error_response(ValidatorPid, ErrorResponse) of
    {compliant, _} -> io:format("Error response is valid~n");
    {non_compliant, _} -> io:format("Error response is invalid~n")
end.

%% Stop validation
erlmcp_protocol_validator:stop_validation(SessionPid),
erlmcp_protocol_validator:stop(ValidatorPid).
```

### Transport Validator

Validate transport layer compliance.

#### API Usage

```erlang
%% Start validator
{ok, ValidatorPid} = erlmcp_transport_validator:start_link().

%% Validate stdio transport
{ok, Result} = erlmcp_transport_validator:validate_stdio(ValidatorPid, #{
    buffer_size => 8192,
    timeout => 5000
}),
case Result of
    #{compliant := true} -> io:format("✅ stdio transport is compliant~n");
    #{compliant := false} -> io:format("❌ stdio transport has issues~n")
end.

%% Validate HTTP transport
{ok, HTTPResult} = erlmcp_transport_validator:validate_http(ValidatorPid, #{
    url => <<"http://localhost:8080/mcp">>,
    timeout => 10000
}).

%% Validate all transports
{ok, AllResults} = erlmcp_transport_validator:validate_all(ValidatorPid).

%% Stop validator
erlmcp_transport_validator:stop(ValidatorPid).
```

#### Command-Line Usage

```bash
# Validate specific transport
erl -noshell -pa _build/default/lib/*/ebin \
  -eval "{ok, V} = erlmcp_transport_validator:start_link(), \
         {ok, R} = erlmcp_transport_validator:validate_stdio(), \
         io:format('stdio: ~p~n', [R]), \
         halt()."
```

---

## Generating Reports

### Report Formats

The validation framework supports multiple report formats:

#### Markdown Report

```bash
# Generate markdown report
rebar3 as validation escriptize
./_build/validation/bin/erlmcp_validate run --all --report-format markdown --output compliance_report.md
```

#### JSON Report

```bash
# Generate JSON report
./_build/validation/bin/erlmcp_validate run --all --report-format json --output compliance_report.json
```

#### Console Report

```bash
# Generate console output
./_build/validation/bin/erlmcp_validate run --all --report-format console
```

### Programmatic Report Generation

```erlang
%% Run validation and generate report
{ok, Results} = erlmcp_validation_runner:run_compliance_suite(),
{ok, Markdown, Report} = erlmcp_compliance_report:generate_report(Results),

%% Save to file
ok = erlmcp_compliance_report:save_report(<<"compliance_report.md">>, Markdown).

%% Generate JSON report
JSON = erlmcp_compliance_report:generate_json(Report),
ok = erlmcp_compliance_report:save_report(<<"compliance_report.json">>, JSON).
```

### Report Contents

#### Summary Section

```markdown
## Summary

| Section | Requirements | Tested | Passed | Status |
|---------|--------------|--------|--------|--------|
| Lifecycle | 5 | 5 | 5 | ✅ 100% |
| Tools | 8 | 8 | 8 | ✅ 100% |
| Resources | 10 | 10 | 9 | ⚠️ 90% |
| Prompts | 6 | 6 | 6 | ✅ 100% |
| Transports | 12 | 12 | 11 | ⚠️ 91.7% |

**Overall Compliance**: 98.3% (59/60 requirements validated)
```

#### Detailed Evidence Section

```markdown
### §3.1 Initialization

**Requirement**: Client MUST send initialize before other requests

**Test**: `initialize_must_be_first_test`
**Result**: ✅ PASSED
**Evidence**:
```
Request before initialize: tools/list
Response: {"jsonrpc":"2.0","id":1,"error":{"code":-32600,"message":"Not initialized"}}
Request after initialize: tools/list
Response: {"jsonrpc":"2.0","id":2,"result":{"tools":[...]}}
```
```

### Custom Reports

Generate custom reports with specific sections:

```erlang
%% Run specific sections
{ok, Results} = erlmcp_validation_runner:run_section_tests([<<"tools">>, <<"resources">>]),

%% Generate custom report
{ok, CustomReport} = erlmcp_compliance_report:generate_report(Results),

%% Add custom content
CustomMarkdown = <<
    (erlmcp_compliance_report:generate_markdown(CustomReport))/binary,
    "\n## Custom Analysis\n\n",
    "Additional findings..."
>>,
file:write_file("custom_report.md", CustomMarkdown).
```

---

## CI/CD Integration

### GitHub Actions

Create `.github/workflows/spec-compliance.yml`:

```yaml
name: MCP Specification Compliance

on:
  push:
    branches: [main, develop]
  pull_request:
    branches: [main, develop]
  schedule:
    # Run daily at 00:00 UTC
    - cron: '0 0 * * *'

jobs:
  validate:
    runs-on: ubuntu-latest

    steps:
      - name: Checkout code
        uses: actions/checkout@v3

      - name: Install Erlang
        uses: erlef/setup-beam@v1
        with:
          otp-version: '25'
          rebar3-version: '3.20'

      - name: Compile
        run: rebar3 compile

      - name: Compile validation app
        run: rebar3 as validation compile

      - name: Run specification compliance tests
        run: rebar3 as validation ct

      - name: Generate compliance report
        run: |
          rebar3 as validation escriptize
          ./_build/validation/bin/erlmcp_validate run --all --report-format markdown --output compliance_report.md

      - name: Upload compliance report
        uses: actions/upload-artifact@v3
        with:
          name: compliance-report-${{ github.sha }}
          path: compliance_report.md
          retention-days: 30

      - name: Comment PR with results
        if: github.event_name == 'pull_request'
        uses: actions/github-script@v6
        with:
          script: |
            const fs = require('fs');
            const report = fs.readFileSync('compliance_report.md', 'utf8');
            github.rest.issues.createComment({
              issue_number: context.issue.number,
              owner: context.repo.owner,
              repo: context.repo.repo,
              body: report
            });

      - name: Check compliance threshold
        run: |
          COMPLIANCE=$(grep "Overall Compliance" compliance_report.md | grep -oP '\d+\.\d+')
          echo "Compliance: $COMPLIANCE%"
          if (( $(echo "$COMPLIANCE < 95.0" | bc -l) )); then
            echo "❌ Compliance below 95% threshold"
            exit 1
          fi
          echo "✅ Compliance meets 95% threshold"
```

### GitLab CI

Create `.gitlab-ci.yml`:

```yaml
stages:
  - validate
  - report

variables:
  REBAR3_VERSION: "3.20"

compliance_tests:
  stage: validate
  image: erlang:25
  script:
    - apt-get update && apt-get install -y build-essential
    - curl -o rebar3 https://s3.amazonaws.com/rebar3/rebar3 && chmod +x rebar3
    - ./rebar3 compile
    - ./rebar3 as validation compile
    - ./rebar3 as validation ct
  artifacts:
    paths:
      - _build/validation/logs/
    when: always
  only:
    - merge_requests
    - main

compliance_report:
  stage: report
  image: erlang:25
  script:
    - ./rebar3 as validation escriptize
    - ./_build/validation/bin/erlmcp_validate run --all --report-format markdown --output compliance_report.md
    - ./_build/validation/bin/erlmcp_validate run --all --report-format json --output compliance_report.json
  artifacts:
    paths:
      - compliance_report.md
      - compliance_report.json
    expire_in: 30 days
  only:
    - merge_requests
    - main
```

### Jenkins Pipeline

Create `Jenkinsfile`:

```groovy
pipeline {
    agent any

    tools {
        erlang '25'
    }

    stages {
        stage('Build') {
            steps {
                sh 'rebar3 compile'
            }
        }

        stage('Build Validation') {
            steps {
                sh 'rebar3 as validation compile'
            }
        }

        stage('Run Compliance Tests') {
            steps {
                sh 'rebar3 as validation ct'
            }
        }

        stage('Generate Report') {
            steps {
                sh '''
                  rebar3 as validation escriptize
                  ./_build/validation/bin/erlmcp_validate run --all --report-format markdown --output compliance_report.md
                '''
            }
        }

        stage('Check Threshold') {
            steps {
                script {
                    def report = readFile('compliance_report.md')
                    def compliance = (report =~ /Overall Compliance.*?(\d+\.\d+)/)[0][1]
                    echo "Compliance: ${compliance}%"

                    if (compliance.toDouble() < 95.0) {
                        error "❌ Compliance below 95% threshold: ${compliance}%"
                    } else {
                        echo "✅ Compliance meets 95% threshold: ${compliance}%"
                    }
                }
            }
        }
    }

    post {
        always {
            archiveArtifacts artifacts: 'compliance_report.md,compliance_report.json', fingerprint: true
        }
    }
}
```

### Pre-commit Hook

Create `.git/hooks/pre-commit`:

```bash
#!/bin/bash

echo "Running MCP specification compliance validation..."

# Compile
if ! rebar3 compile > /dev/null 2>&1; then
    echo "❌ Compilation failed"
    exit 1
fi

# Run validation tests
if ! rebar3 as validation ct > /tmp/validation.log 2>&1; then
    echo "❌ Validation tests failed"
    cat /tmp/validation.log
    exit 1
fi

# Check compliance threshold
COMPLIANCE=$(grep "Overall Compliance" compliance_report.md 2>/dev/null | grep -oP '\d+\.\d+' || echo "0")
if (( $(echo "$COMPLIANCE < 95.0" | bc -l 2>/dev/null || echo "1") )); then
    echo "⚠️  Warning: Compliance at ${COMPLIANCE%,}% (below 95% threshold)"
    # Not blocking for local development
else
    echo "✅ Validation passed: ${COMPLIANCE%,}% compliance"
fi

exit 0
```

Make it executable:
```bash
chmod +x .git/hooks/pre-commit
```

---

## Troubleshooting

### Common Issues

#### 1. CLI Build Errors

**Symptom**:
```
Error: escript build failed
Error: Could not find module 'erlmcp_validate_cli'
```

**Solutions**:

```bash
# Ensure all apps are compiled
rebar3 compile

# Clean and rebuild
rebar3 clean --all
rebar3 compile

# Build escript explicitly
rebar3 escriptize

# Check escript configuration
grep -A 5 "escript_" rebar.config
```

**Verify escript was created**:
```bash
# Check if executable exists
ls -lh _build/default/bin/erlmcp_validate

# Test executable
./_build/default/bin/erlmcp_validate --version
```

#### 2. CLI Runtime Errors

**Symptom**:
```
Error: {badarg,[{erlmcp_validate_cli,main,1}]}
Error: Failed to start applications
```

**Solutions**:

```bash
# Ensure all dependencies are available
rebar3 get-deps

# Check application is started
erl -pa _build/default/lib/*/ebin -eval "application:ensure_all_started(erlmcp_validation), halt()."

# Run with verbose output
./_build/default/bin/erlmcp_validate run --all --verbose
```

#### 3. Module Not Found Errors

**Symptom**:
```
Error: module 'erlmcp_protocol_validator' is not available
Error: {undef,[{erlmcp_protocol_validator,run,...}]}
```

**Solutions**:

```bash
# Check if module is compiled
find _build -name "erlmcp_protocol_validator.beam"

# Ensure all apps are compiled
rebar3 compile

# Check escript includes required apps
grep "escript_incl_apps" rebar.config

# Manually add module path if needed
erl -pa _build/default/lib/*/ebin -eval "code:which(erlmcp_protocol_validator), halt()."
```

#### 4. Permission Denied Errors

**Symptom**:
```
bash: ./_build/default/bin/erlmcp_validate: Permission denied
```

**Solutions**:

```bash
# Make executable
chmod +x _build/default/bin/erlmcp_validate

# Or run with escript
escript _build/default/bin/erlmcp_validate --version

# If using symlink, ensure source is executable
chmod +x $(pwd)/_build/default/bin/erlmcp_validate
sudo ln -s $(pwd)/_build/default/bin/erlmcp_validate /usr/local/bin/erlmcp_validate
```

#### 5. Compilation Failures

**Symptom**:
```
Error: Could not find dependency erlmcp_validation
```

**Solution**:
```bash
# Clean and rebuild
rebar3 clean --all
rebar3 compile

# Ensure validation profile is built
rebar3 as validation compile
```

#### 6. Test Timeouts

**Symptom**:
```
Error: Test timeout exceeded
Error: Validation timeout after 30s
```

**Solutions**:

```erlang
%% Increase timeout in config
{erlmcp_validation, [
  {validation_timeout, 60000}  %% 60 seconds instead of 30
]}.
```

Or via command line:
```bash
rebar3 as validation ct --timeout 60
```

#### 7. Port Already in Use

**Symptom**:
```
Error: Port 8080 already in use
```

**Solution**:
```bash
# Find and kill process using port
lsof -ti:8080 | xargs kill -9

# Or use different port in config
{erlmcp_validation, [
  {test_server, [
    {port, 8081}  %% Use different port
  ]}
]}.
```

#### 8. Missing Dependencies

**Symptom**:
```
Error: Missing module jsx
Error: {undef,[{jsx,encode,...}]}
```

**Solutions**:

```bash
# Ensure all dependencies are fetched
rebar3 get-deps
rebar3 compile

# Or explicitly for validation profile
rebar3 as validation get-deps
rebar3 as validation compile

# Check if dependency is in rebar.config
grep "jsx" apps/erlmcp_validation/rebar.config
```

#### 9. JSON Encoding Errors

**Symptom**:
```
Error: JSON encode failed
Error: {badarg,[{jsx,encode,...}]}
```

**Solutions**:

```bash
# Ensure jsx is available
rebar3 deps

# Test jsx module
erl -pa _build/default/lib/*/ebin -eval "jsx:encode(#{<<"test">> => 1}), halt()."

# Check escript includes jsx
grep "escript_incl_apps" rebar.config
# Should include: [erlmcp_validation, erlmcp_core, erlmcp_transports, jsx, jesse]
```

#### 10. Low Test Coverage

**Symptom**:
```
Coverage: 75.2% (below 80% threshold)
```

**Solution**:
```bash
# Run coverage analysis
rebar3 as validation cover

# Generate coverage report
rebar3 as validation cover --verbose

# View coverage report
open _build/validation/cover/index.html
```

### Debug Mode

Enable verbose logging for debugging:

```erlang
%% Enable debug logging
logger:set_primary_config(level, all),

%% Run validation with verbose output
rebar3 as validation ct --verbose

%% Enable debug mode in validator
{ok, Validator} = erlmcp_protocol_validator:start_link(#{
  debug => true,
  log_requests => true,
  log_responses => true
}).
```

### Common Validation Failures

#### Initialize Not Called First

**Failure**: `tools/list called before initialize`

**Cause**: Test client not following protocol lifecycle

**Solution**:
```erlang
%% Always initialize first
{ok, _} = erlmcp_test_client:initialize(ServerPid, #{
  protocolVersion => <<"2025-11-25">>,
  capabilities => #{}
}).
```

#### Invalid JSON-RPC Format

**Failure**: `Invalid JSON-RPC request format`

**Cause**: Missing required fields in request

**Solution**:
```erlang
%% Ensure all required fields are present
Request = #{
  <<"jsonrpc">> => <<"2.0">>,
  <<"id">> => 1,
  <<"method">> => <<"tools/list">>,
  <<"params">> => #{}
}.
```

#### Transport Not Started

**Failure**: `Transport not available`

**Cause**: Transport not started before validation

**Solution**:
```erlang
%% Start transport before validation
{ok, TransportPid} = erlmcp_transport_stdio:start_link(),
{ok, _} = erlmcp_transport_validator:validate_stdio().
```

---

## API Reference

### erlmcp_spec_parser

#### Functions

```erlang
%% Start parser
start_link() -> {ok, pid()} | {error, term()}
start_link(SpecURL) -> {ok, pid()} | {error, term()}
start_link(SpecURL, Name) -> {ok, pid()} | {error, term()}

%% Parse specification
parse_specification() -> {ok, #mcp_spec{}} | {error, term()}
parse_specification(URL) -> {ok, #mcp_spec{}} | {error, term()}
parse_specification(Pid, URL, Text) -> {ok, #mcp_spec{}} | {error, term()}

%% Get requirements
method_requirements() -> #{binary() => #method_req{}}
method_requirements(Pid) -> #{binary() => #method_req{}}
error_code_requirements() -> #{integer() => #error_code_req{}}
error_code_requirements(Pid) -> #{integer() => #error_code_req{}}
transport_requirements() -> #{binary() => #transport_req{}}
transport_requirements(Pid) -> #{binary() => #transport_req{}}
notification_requirements() -> #{binary() => #notification_req{}}
notification_requirements(Pid) -> #{binary() => #notification_req{}}
capability_requirements() -> #capability_req{}
capability_requirements(Pid) -> #capability_req{}
content_type_requirements() -> [binary()]
content_type_requirements(Pid) -> [binary()]

%% Utility
get_spec() -> #mcp_spec{} | undefined
get_spec(Pid) -> #mcp_spec{} | undefined
get_spec_version() -> binary() | undefined
get_spec_version(Pid) -> binary() | undefined
validate_spec_version(Version) -> boolean()

%% Stop parser
stop(Pid) -> ok
```

### erlmcp_protocol_validator

#### Functions

```erlang
%% Start validator
start_link() -> {ok, pid()} | {error, term()}
start_link(SpecParser) -> {ok, pid()} | {error, term()}

%% Validation session
start_validation(TransportType, Config) -> {ok, pid()} | {error, term()}
stop_validation(Pid) -> ok | {error, term()}

%% Method validation
validate_method_call(Method, Params) -> validation_response()
validate_method_call(Validator, Method, Params) -> validation_response()

%% Error validation
validate_error_response(Response) -> validation_response()
validate_error_response(Validator, Response) -> validation_response()

%% Notification validation
validate_notification(Method, Params) -> validation_response()
validate_notification(Validator, Method, Params) -> validation_response()

%% Request/response validation
validate_request_response(Request, Response) -> validation_response()
validate_request_response(Validator, Request, Response) -> validation_response()

%% Batch validation
validate_batch_request(Requests) -> validation_response()
validate_batch_request(Validator, Requests) -> validation_response()

%% Results
get_validation_results() -> [validation_result()]
get_validation_results(Pid) -> [validation_result()]
reset_validation_state() -> ok
reset_validation_state(Pid) -> ok

%% Stop validator
stop(Pid) -> ok
```

### erlmcp_transport_validator

#### Functions

```erlang
%% Start validator
start_link() -> {ok, pid()} | {error, term()}
start_link(Config) -> {ok, pid()} | {error, term()}

%% Transport validation
validate_stdio() -> {ok, validation_result()} | {error, term()}
validate_stdio(Config) -> {ok, validation_result()} | {error, term()}
validate_http() -> {ok, validation_result()} | {error, term()}
validate_http(Config) -> {ok, validation_result()} | {error, term()}
validate_sse() -> {ok, validation_result()} | {error, term()}
validate_sse(Config) -> {ok, validation_result()} | {error, term()}
validate_websocket() -> {ok, validation_result()} | {error, term()}
validate_websocket(Config) -> {ok, validation_result()} | {error, term()}
validate_tcp() -> {ok, validation_result()} | {error, term()}
validate_tcp(Config) -> {ok, validation_result()} | {error, term()}

%% Batch validation
validate_all() -> {ok, [validation_result()]} | {error, term()}
validate_all(Config) -> {ok, [validation_result()]} | {error, term()}

%% Results
get_validation_results() -> [validation_result()]
get_validation_results(Pid) -> [validation_result()]

%% Stop validator
stop(Pid) -> ok
```

### erlmcp_validation_runner

#### Functions

```erlang
%% Start runner
start_link() -> {ok, pid()} | {error, term()}
start_link(Config) -> {ok, pid()} | {error, term()}

%% Run validation
run_compliance_suite() -> compliance_result()
run_compliance_suite(Config) -> compliance_result()
run_section_tests(Sections) -> section_result() | {error, term()}
run_section_tests(Sections, Config) -> section_result() | {error, term()}

%% Generate reports
generate_report(Format) -> {ok, binary(), #compliance_report{}}
generate_report(Results, Format) -> {ok, binary(), #compliance_report{}}

%% Transport validation
validate_transport(Transport) -> {ok, validation_result()}
validate_transport(Transport, Config) -> {ok, validation_result()}

%% Summary
get_validation_summary() -> validation_summary()
get_validation_summary(Pid) -> validation_summary()

%% Quick check
run_quick_check() -> {ok, validation_summary()}
run_quick_check(Config) -> {ok, validation_summary()}

%% Stop runner
stop(Pid) -> ok
```

### erlmcp_compliance_report

#### Functions

```erlang
%% Generate report
generate_report(Results) -> {ok, binary(), #compliance_report{}}
generate_markdown(Report) -> binary()
generate_json(Report) -> binary()

%% Save report
save_report(Filename, Content) -> ok | {error, term()}

%% Format helpers
format_summary(Report) -> binary()
format_details(Report) -> binary()
```

---

## Examples and Best Practices

### Example 1: Complete Validation Workflow

```erlang
-module(validation_example).
-export([run_complete_validation/0]).

run_complete_validation() ->
    %% 1. Start applications
    application:ensure_all_started(erlmcp_validation),
    io:format("✅ Applications started~n"),

    %% 2. Parse MCP specification
    {ok, Parser} = erlmcp_spec_parser:start_link(),
    {ok, Spec} = erlmcp_spec_parser:parse_specification(Parser),
    io:format("✅ Parsed MCP spec version: ~s~n", [Spec#mcp_spec.version]),

    %% 3. Validate protocol behavior
    {ok, ProtocolValidator} = erlmcp_protocol_validator:start_link(Parser),
    {ok, _Session} = erlmcp_protocol_validator:start_validation(stdio, []),
    io:format("✅ Protocol validation session started~n"),

    %% 4. Validate transport
    {ok, TransportValidator} = erlmcp_transport_validator:start_link(),
    {ok, StdioResult} = erlmcp_transport_validator:validate_stdio(),
    io:format("✅ stdio transport: ~p~n", [StdioResult]),

    %% 5. Run full compliance suite
    Results = erlmcp_validation_runner:run_compliance_suite(#{
        timeout => 60000
    }),
    io:format("✅ Compliance: ~p%~n", [Results#{overall}]),

    %% 6. Generate report
    {ok, Markdown, _Report} = erlmcp_compliance_report:generate_report(Results),
    ok = file:write_file("compliance_report.md", Markdown),
    io:format("✅ Report saved to compliance_report.md~n"),

    %% 7. Cleanup
    erlmcp_spec_parser:stop(Parser),
    erlmcp_protocol_validator:stop(ProtocolValidator),
    erlmcp_transport_validator:stop(TransportValidator),

    {ok, Results}.
```

### Example 2: Custom Validation Script

```bash
#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa _build/default/lib/*/ebin

main([]) ->
    main(["--all"]);
main(["--all"]) ->
    %% Run full validation
    io:format("Running full MCP specification compliance validation...~n"),
    application:ensure_all_started(erlmcp_validation),
    Results = erlmcp_validation_runner:run_compliance_suite(),
    print_results(Results);
main(["--transport", Transport]) ->
    %% Validate specific transport
    io:format("Validating ~s transport...~n", [Transport]),
    application:ensure_all_started(erlmcp_validation),
    {ok, Result} = erlmcp_transport_validator:validate_transport(
        list_to_existing_atom(Transport)
    ),
    print_transport_result(Result);
main(["--section", Section]) ->
    %% Validate specific section
    io:format("Validating ~s section...~n", [Section]),
    application:ensure_all_started(erlmcp_validation),
    {ok, Result} = erlmcp_validation_runner:run_section_tests([
        list_to_binary(Section)
    ]),
    print_section_result(Result).

print_results(#{overall := Compliance, by_section := Sections}) ->
    io:format("~nOverall Compliance: ~p%~n", [Compliance]),
    maps:fold(fun(Section, #{compliance_percent := P}, _) ->
        io:format("  ~s: ~p%~n", [Section, P])
    end, ok, Sections),
    halt(if Compliance >= 95.0 -> 0; true -> 1 end).
```

### Example 3: Integration with Existing Tests

```erlang
-module(my_app_tests).
-include_lib("eunit/include/eunit.hrl").

%% Setup validation before tests
setup_validation() ->
    {ok, _} = application:ensure_all_started(erlmcp_validation),
    {ok, Parser} = erlmcp_spec_parser:start_link(),
    {ok, Validator} = erlmcp_protocol_validator:start_link(Parser),
    {Parser, Validator}.

%% Cleanup after tests
cleanup_validation({Parser, Validator}) ->
    erlmcp_protocol_validator:stop(Validator),
    erlmcp_spec_parser:stop(Parser).

%% Test with validation
my_feature_test_() ->
    {setup,
     fun setup_validation/0,
     fun cleanup_validation/1,
     fun({Parser, Validator}) ->
         [
          ?_test(begin
             %% Your test code here
             {ok, Server} = my_app:start_server(),

             %% Validate against spec
             {compliant, _} = erlmcp_protocol_validator:validate_method_call(
                 Validator, <<"tools/list">>, #{}
             ),

             my_app:stop_server(Server)
           end)
         ]
     end}.
```

### Best Practices

#### 1. Run Validation Regularly

```bash
# Add to cron for daily validation
0 0 * * * cd /path/to/erlmcp && make validate && mail -s "Validation Results" admin@example.com < compliance_report.md
```

#### 2. Monitor Compliance Trends

```erlang
%% Track compliance over time
track_compliance() ->
    {ok, Results} = erlmcp_validation_runner:run_compliance_suite(),
    Compliance = Results#{overall},

    %% Store in database or metrics system
    metrics:gauge([erlmcp, validation, compliance], Compliance),

    %% Alert if below threshold
    case Compliance of
        C when C < 95.0 ->
            alert:send("Validation compliance dropped to ~p%", [C]);
        _ ->
            ok
    end.
```

#### 3. Validate Before Releases

```bash
# Add to release checklist
./scripts/pre_release_validation.sh
```

```bash
#!/bin/bash
set -e

echo "Running pre-release validation..."
make validate
./scripts/bench/run_all_benchmarks.sh
echo "✅ Pre-release validation passed"
```

#### 4. Use Validation in Development

```erlang
%% Quick validation during development
dev_check() ->
    %% Start validation
    {ok, Runner} = erlmcp_validation_runner:start_link(),

    %% Run quick check
    {ok, Summary} = erlmcp_validation_runner:run_quick_check(#{
        tests => [essential]
    }),

    %% Print summary
    io:format("Quick check: ~p/~p passed (~p%)~n", [
        Summary#{passed},
        Summary#{total_tests},
        Summary#{compliance_percent}
    ]).
```

#### 5. Integrate with Quality Gates

```makefile
# Makefile
validate: validate-compile validate-test validate-coverage validate-quality validate-spec

validate-spec:
	@echo "Running MCP specification compliance validation..."
	@(rebar3 as validation ct && \
	  COMPLIANCE=$$(grep "Overall Compliance" compliance_report.md | grep -oP '\d+\.\d+' || echo "0") && \
	  if (( $$(echo "$$COMPLIANCE < 95.0" | bc -l) )); then \
	    echo "❌ Spec compliance below 95%: $$COMPLIANCE%"; \
	    exit 1; \
	  else \
	    echo "✅ Spec compliance: $$COMPLIANCE%"; \
	  fi)
```

---

## Performance Considerations

### Optimization Tips

1. **Run Tests in Parallel**:
```bash
rebar3 as validation ct --parallel
```

2. **Use Quick Checks for Development**:
```erlang
erlmcp_validation_runner:run_quick_check(#{
    tests => [essential],
    timeout => 10000
})
```

3. **Cache Parsed Specifications**:
```erlang
%% Spec parser caches parsed specs
{ok, Parser} = erlmcp_spec_parser:start_link(),
%% Subsequent calls use cached version
{ok, Spec} = erlmcp_spec_parser:get_spec(Parser)
```

4. **Run Subset of Tests**:
```bash
# Only validate specific sections
rebar3 as validation ct --suite=erlmcp_spec_compliance_SUITE --testcase="tools_*"
```

### Resource Usage

Typical resource consumption for validation:

| Test Suite | Duration | Memory | CPU |
|------------|----------|--------|-----|
| Spec Parser | 1-2s | 50MB | Low |
| Protocol Validator | 5-10s | 100MB | Medium |
| Transport Validator | 10-15s | 150MB | Medium |
| Full Suite | 30-60s | 300MB | High |

### Scaling for Large Projects

For projects with many tests:

1. **Use CI/CD Parallel Jobs**:
```yaml
strategy:
  matrix:
    suite: [lifecycle, tools, resources, prompts, transports]
jobs:
  validate:
    runs-on: ubuntu-latest
    steps:
      - run: rebar3 as validation ct --suite=erlmcp_${{ matrix.suite }}_SUITE
```

2. **Distribute Testing**:
```erlang
%% Run tests on multiple nodes
Nodes = [node1@host, node2@host, node3@host],
lists:foreach(fun(Node) ->
    spawn(Node, fun() ->
        erlmcp_validation_runner:run_compliance_suite()
    end)
end, Nodes).
```

---

## Additional Resources

- [MCP Specification](https://modelcontextprotocol.io/)
- [erlmcp Documentation](https://github.com/your-org/erlmcp)
- [Implementation Plan](~/.claude/plans/floofy-roaming-adleman.md)
- [erlmcp_validation README](../apps/erlmcp_validation/README.md)

---

## License

Apache-2.0

---

**Last Updated**: 2026-01-30
**Maintainer**: erlmcp team
