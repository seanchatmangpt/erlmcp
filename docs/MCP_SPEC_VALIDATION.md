# MCP Specification Validation

## Overview

The erlmcp validation framework provides comprehensive validation of MCP (Model Context Protocol) implementations against the official specification (MCP 2025-11-25). It ensures protocol compliance, security best practices, performance targets, and proper transport behavior.

## Architecture

```
erlmcp_validation
├── erlmcp_protocol_validator    - JSON-RPC 2.0 and MCP protocol validation
├── erlmcp_transport_validator   - Transport behavior compliance
├── erlmcp_security_validator    - Security feature validation
├── erlmcp_performance_validator - Performance benchmarking
├── erlmcp_compliance_report     - Evidence collection and reporting
├── erlmcp_spec_parser           - Hardcoded MCP specification metadata
└── erlmcp_validate_cli          - Command-line interface
```

### Validation Categories

1. **Protocol Validation** - JSON-RPC 2.0 and MCP message structure
2. **Transport Validation** - Transport behavior (stdio, tcp, http, websocket, sse)
3. **Security Validation** - Authentication, input validation, secrets handling
4. **Performance Validation** - Latency, throughput, memory baselines
5. **Compliance Reporting** - Evidence collection, traceability, reports

## Quick Start

### Installation

The validation framework is part of the `erlmcp_validation` application in the erlmcp umbrella project.

```bash
# Clone and build
git clone https://github.com/erlmcp/erlmcp.git
cd erlmcp
rebar3 compile
```

### Running Validation

#### Using the CLI

```bash
# Run all validation sections
./scripts/erlmcp_validate run --all

# Run specific section
./scripts/erlmcp_validate run --section protocol

# Run with specific transport
./scripts/erlmcp_validate run --transport tcp --verbose

# Generate compliance report
./scripts/erlmcp_validate report --format markdown --output report.md

# Quick health check
./scripts/erlmcp_validate quick-check
```

#### From Erlang Shell

```erlang
# Start applications
application:ensure_all_started(erlmcp_validation).

# Run protocol validation
{ok, Result} = erlmcp_protocol_validator:validate_jsonrpc(
    #{<<"jsonrpc">> => <<"2.0">>,
      <<"method">> => <<"ping">>,
      <<"id">> => 1}
).

# Validate method name
ok = erlmcp_protocol_validator:validate_method_name(<<"tools/list">>).

# Validate error code
ok = erlmcp_protocol_validator:validate_error_code(-32700).
```

## Module Reference

### erlmcp_protocol_validator

Validates JSON-RPC 2.0 and MCP protocol message structure.

#### API Functions

```erlang
% Validate JSON-RPC message structure
-spec validate_jsonrpc(map()) -> validation_result().
validate_jsonrpc(Message) -> ok | {error, validation_error()}.

% Validate MCP request
-spec validate_request(binary(), json_rpc_params()) -> validation_result().
validate_request(Method, Params) -> ok | {error, validation_error()}.

% Validate MCP response
-spec validate_response(json_rpc_id(), term()) -> validation_result().
validate_response(Id, ResultOrError) -> ok | {error, validation_error()}.

% Validate MCP notification
-spec validate_notification(binary(), json_rpc_params()) -> validation_result().
validate_notification(NotificationName, Params) -> ok | {error, validation_error()}.

% Validate error code range
-spec validate_error_code(integer()) -> validation_result().
validate_error_code(Code) -> ok | {error, validation_error()}.

% Validate field type
-spec validate_field_type(binary(), term(), field_type() | [field_type()]) -> validation_result().
validate_field_type(FieldName, Value, ExpectedType) -> ok | {error, validation_error()}.

% Validate required fields present
-spec validate_required_fields(map(), [binary()]) -> validation_result().
validate_required_fields(Message, RequiredFields) -> ok | {error, validation_error()}.
```

#### Valid MCP Methods

The validator recognizes all MCP 2025-11-25 methods:

**Core Protocol:**
- `initialize` - Session initialization
- `ping` - Liveness check

**Resources:**
- `resources/list` - List available resources
- `resources/read` - Read resource content
- `resources/subscribe` - Subscribe to resource updates
- `resources/unsubscribe` - Unsubscribe from updates

**Tools:**
- `tools/list` - List available tools
- `tools/call` - Execute a tool

**Tasks:**
- `tasks/create` - Create background task
- `tasks/list` - List tasks
- `tasks/get` - Get task details
- `tasks/result` - Get task result
- `tasks/cancel` - Cancel task

**Prompts:**
- `prompts/list` - List available prompts
- `prompts/get` - Get prompt template

**Other:**
- `completion/complete` - LLM text completion
- `requests/cancel` - Cancel pending request

#### Valid Error Codes

**JSON-RPC 2.0 Standard:**
- `-32700` - Parse error
- `-32600` - Invalid Request
- `-32601` - Method not found
- `-32602` - Invalid params
- `-32603` - Internal error

**MCP Protocol:**
- `-32001` - Resource not found
- `-32002` - Tool not found
- `-32010` - Server overloaded

**MCP Refusal Codes (1001-1089):**
- `1001` - Unspecified refusal
- `1002-1007` - Invalid argument/value/type
- `1010-1019` - Invalid request/method/capability
- `1022-1027` - Invalid URI components
- `1050-1061` - Authentication/authorization failures
- `1062-1064` - Quota/rate limiting
- `1089` - Rate limited

### erlmcp_transport_validator

Validates transport implementations against `erlmcp_transport_behavior`.

#### API Functions

```erlang
% Run full transport validation
-spec run(atom()) -> {ok, map()} | {error, term()}.
run(TransportModule) -> {ok, ValidationSummary}.

% Validate behavior callbacks
-spec validate_callbacks(atom()) -> map().
validate_callbacks(Module) -> #{checks => [...], passed => N, failed => M}.

% Validate message framing
-spec validate_framing(atom(), atom()) -> map().
validate_framing(Module, TransportType) -> #{checks => [...], passed => N, failed => M}.

% Validate registry integration
-spec validate_registry(atom()) -> map().
validate_registry(Module) -> #{checks => [...], passed => N, failed => M}.

% Validate lifecycle management
-spec validate_lifecycle(atom()) -> map().
validate_lifecycle(Module) -> #{checks => [...], passed => N, failed => M}.
```

#### Transport Types

| Transport | Framing | Key Requirements |
|-----------|---------|-----------------|
| stdio | Newline-delimited | Line buffering, JSON encoding |
| tcp | Length-prefixed | 4-byte length header, buffer handling |
| http | POST/JSON | Content-Type: application/json |
| websocket | Text frames | JSON encoding per frame |
| sse | Event stream | text/event-stream, retry support |

### erlmcp_security_validator

Validates security features and best practices.

#### API Functions

```erlang
% Start security validator
start_link() -> {ok, pid()}.

% Run full security validation
-spec run(atom()) -> {ok, map()}.
run(TransportModule) -> {ok, SecurityReport}.

% Validate authentication
-spec validate_authentication(atom()) -> map().
validate_authentication(Module) -> #{checks => [...], passed => N, failed => M}.

% Validate input validation
-spec validate_input_validation(atom()) -> map().
validate_input_validation(Module) -> #{checks => [...], passed => N, failed => M}.

% Validate secret management
-spec validate_secret_management(atom()) -> map().
validate_secret_management(Module) -> #{checks => [...], passed => N, failed => M}.

% Validate JWT handling
-spec validate_jwt(atom()) -> map().
validate_jwt(Module) -> #{checks => [...], passed => N, failed => M}.

% Validate rate limiting
-spec validate_rate_limiting(atom()) -> map().
validate_rate_limiting(Module) -> #{checks => [...], passed => N, failed => M}.

% Validate CORS policies
-spec validate_cors(atom()) -> map().
validate_cors(Module) -> #{checks => [...], passed => N, failed => M}.
```

#### Security Checks

**Authentication:**
- Auth mechanism available (`erlmcp_auth`)
- Token handling (no logging of secrets)
- Session management (crypto session IDs)
- Authorization (RBAC support)

**Input Validation:**
- JSON Schema validation (jesse)
- Parameter sanitization
- SQL injection prevention
- XSS prevention
- Path traversal prevention

**Secret Management:**
- No hardcoded secrets
- Environment variable usage
- Encryption at rest
- Key rotation support

**JWT:**
- Structure validation
- Signature verification
- Expiration checking

**Rate Limiting:**
- Rate limit configuration
- Enforcement mechanism
- Bypass prevention

**CORS:**
- Header configuration
- Origin validation
- Policy definitions

### erlmcp_performance_validator

Validates performance against defined targets.

#### API Functions

```erlang
% Run full performance validation
-spec run(transport_type()) -> {ok, performance_report()}.
run(Transport) -> {ok, Report}.

% Run with custom options
-spec run(transport_type(), map()) -> {ok, performance_report()}.
run(Transport, Options) -> {ok, Report}.

% Measure latency
-spec measure_latency(transport_type(), pos_integer()) -> {ok, latency_result()}.
measure_latency(Transport, Samples) -> {ok, #{p50_us => ..., p95_us => ..., p99_us => ...}}.

% Measure throughput
-spec measure_throughput(transport_type(), pos_integer()) -> {ok, throughput_result()}.
measure_throughput(Transport, TotalRequests) -> {ok, #{requests_per_second => ...}}.

% Measure memory
-spec measure_memory(transport_type()) -> {ok, memory_result()}.
measure_memory(Transport) -> {ok, #{bytes_per_connection => ...}}.

% Measure connection setup
-spec measure_connection_setup(transport_type()) -> {ok, connection_setup_result()}.
measure_connection_setup(Transport) -> {ok, #{avg_setup_time_us => ...}}.

% Test concurrent connections
-spec test_concurrent_connections(transport_type(), pos_integer()) -> {ok, concurrent_result()}.
test_concurrent_connections(Transport, NumConnections) -> {ok, #{success_count => ...}}.
```

#### Performance Targets

| Metric | Target | Scope |
|--------|--------|-------|
| P50 Latency | < 5ms | Per request |
| P95 Latency | < 20ms | Per request |
| P99 Latency | < 50ms | Per request |
| Throughput | > 1000 req/s | Sustained |
| Memory/Connection | < 100KB | Per connection |
| Connection Setup | < 100ms | New connection |
| Concurrent Connections | 10K | Simultaneous |

### erlmcp_compliance_report

Collects evidence and generates compliance reports.

#### API Functions

```erlang
% Start report server
start_link() -> {ok, pid()}.

% Generate compliance report
-spec generate_report(report_format(), compliance_data()) -> {ok, binary()}.
generate_report(Format, Data) -> {ok, ReportContent}.
% Formats: text, markdown, json, html

% Calculate compliance percentage
-spec calculate_compliance(compliance_data()) -> {ok, float(), map()}.
calculate_compliance(Data) -> {ok, Compliance, Details}.

% Create traceability matrix
-spec create_traceability_matrix(compliance_data()) -> map().
create_traceability_matrix(Data) -> #{ReqId => #{...}}.

% Identify compliance gaps
-spec identify_gaps(compliance_data()) -> [gap_analysis()].
identify_gaps(Data) -> [#{requirement => ..., status => ..., severity => ...}].

% Collect evidence
-spec collect_evidence(evidence_type(), map()) -> {ok, evidence()}.
collect_evidence(Type, Data) -> {ok, #{evidence_id => ..., content => ...}}.
% Types: test_result, coverage_metrics, security_scan, performance_benchmark

% Store evidence bundle
-spec store_evidence_bundle(file:filename(), [evidence()]) -> {ok, file:filename()}.
store_evidence_bundle(BundlePath, EvidenceItems) -> {ok, BundlePath}.

% Generate evidence hash (SHA-256)
-spec hash_evidence(map() | binary()) -> {ok, binary()}.
hash_evidence(Content) -> {ok, HashHex}.

% Verify evidence integrity
-spec verify_evidence_integrity(map() | binary(), binary()) -> {ok, boolean()}.
verify_evidence_integrity(Content, ExpectedHash) -> {ok, true|false}.
```

### erlmcp_spec_parser

Provides hardcoded MCP 2025-11-25 specification metadata.

#### API Functions

```erlang
% Start spec parser
start_link() -> {ok, pid()}.

% Get full specification
-spec get_spec() -> {ok, #mcp_spec{}}.
get_spec() -> {ok, SpecRecord}.

% Get method requirements
-spec get_method_requirements() -> {ok, [#method_req{}]}.
get_method_requirements() -> {ok, Methods}.

% Get specific method
-spec get_method_requirements(binary()) -> {ok, #method_req{}} | {error, not_found}.
get_method_requirements(MethodName) -> {ok, MethodReq}.

% List all methods
-spec list_methods() -> {ok, [binary()]}.
list_methods() -> {ok, [<<"initialize">>, <<"tools/list">>, ...]}.

% Get error requirements
-spec get_error_requirements() -> {ok, [#error_code_req{}]}.
get_error_requirements() -> {ok, ErrorCodes}.

% Validate error code
-spec validate_error_code(integer()) -> {ok, valid_error_code} | {error, unknown_error_code}.
validate_error_code(ErrorCode) -> ok | {error, not_found}.

% Check if error code is valid
-spec is_valid_error_code(integer()) -> boolean().
is_valid_error_code(ErrorCode) -> true | false.

% Get all error codes
-spec get_all_error_codes() -> [integer()].
get_all_error_codes() -> [-32700, -32600, ..., 1089].

% Get all request types
-spec get_all_request_types() -> [binary()].
get_all_request_types() -> [<<"ping">>, <<"tools/list">>, ...].

% Get all notification types
-spec get_all_notification_types() -> [binary()].
get_all_notification_types() -> [<<"notifications/initialized">>, ...].

% Validate URI
-spec is_valid_uri(binary() | string()) -> boolean().
is_valid_uri(Uri) -> true | false.

% Get required capabilities
-spec get_required_capabilities(binary()) -> [binary()] | undefined.
get_required_capabilities(Operation) -> [<<"tools">>] | undefined.

% Check if method is deprecated
-spec is_deprecated_method(binary()) -> boolean().
is_deprecated_method(MethodName) -> true | false.

% Get version compatibility
-spec get_version_compatibility(binary()) -> compatible | deprecated | incompatible.
get_version_compatibility(Version) -> compatible.
```

### erlmcp_validate_cli

Command-line interface for validation.

#### Commands

```bash
# Run validation
erlmcp_validate run [options]
  --all                    Run all sections
  --section <name>         Run specific section
  --transport <type>       Validate transport (stdio/tcp/http/websocket)
  --format <type>          Output format (text/json/markdown)
  --verbose                Show detailed output
  --quiet                  Minimal output

# Generate report
erlmcp_validate report [options]
  --format <type>          Report format (text/json/markdown/html)
  --output <file>          Write to file

# Quick check
erlmcp_validate quick-check

# Status
erlmcp_validate status

# Help
erlmcp_validate --help
erlmcp_validate --version
```

#### Sections

- `protocol` - MCP protocol compliance (JSON-RPC 2.0, message formats)
- `transport` - Transport layer behavior (stdio, tcp, http)
- `security` - Security features (authentication, JWT validation)
- `error_handling` - Error response validation and edge cases
- `performance` - Performance benchmarks and load testing

## Testing Guidance

### Chicago School TDD Methodology

The validation framework follows Chicago School TDD principles:
- **NO MOCKS** - All tests use real processes
- **State-based verification** - Verify actual state, not behavior
- **Black-box testing** - Test only observable behavior

### Test Structure

```erlang
% Example validation test
validate_ping_request_test() ->
    % Given: A valid ping request
    Request = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"method">> => <<"ping">>,
        <<"id">> => 1
    },

    % When: Validating the request
    Result = erlmcp_protocol_validator:validate_jsonrpc(Request),

    % Then: Should pass
    ?assertEqual(ok, Result).
```

### Coverage Requirements

- Minimum 80% code coverage
- All public APIs tested
- Edge cases covered
- Error paths validated

## Troubleshooting

### Common Issues

**Issue**: `Module not loaded` error
```
Solution: Start the required applications first
application:ensure_all_started(erlmcp_validation).
```

**Issue**: Validation timeout
```
Solution: Increase timeout for performance tests
erlmcp_performance_validator:run(tcp, #{timeout => 30000}).
```

**Issue**: Evidence bundle creation fails
```
Solution: Ensure directory exists and is writable
filelib:ensure_dir("/path/to/bundle/").
```

### Debug Logging

Enable debug logging for validation:

```erlang
logger:set_application_level(erlmcp_validation, debug).
```

### Getting Help

- GitHub Issues: https://github.com/erlmcp/erlmcp/issues
- Documentation: /docs directory in source
- MCP Spec: https://spec.modelcontextprotocol.io/

## Related Documentation

- [PROTOCOL_VALIDATOR.md](PROTOCOL_VALIDATOR.md) - Protocol validation details
- [SECURITY_VALIDATOR.md](SECURITY_VALIDATOR.md) - Security checks
- [PERFORMANCE_VALIDATOR.md](PERFORMANCE_VALIDATOR.md) - Performance baselines
- [TRANSPORT_VALIDATOR.md](TRANSPORT_VALIDATOR.md) - Transport behavior rules
- [COMPLIANCE_REPORTING.md](COMPLIANCE_REPORTING.md) - Evidence collection
- [VALIDATION_TESTING.md](VALIDATION_TESTING.md) - Testing methodology
