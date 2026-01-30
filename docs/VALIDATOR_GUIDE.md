# erlmcp Validator Guide

## Overview

The erlmcp validation framework provides comprehensive validation of MCP 2025-11-25 implementations through four specialized validators:

1. **Protocol Validator** - JSON-RPC 2.0 and MCP protocol compliance
2. **Transport Validator** - Transport behavior compliance
3. **Security Validator** - Authentication, input validation, secrets handling
4. **Performance Validator** - Latency, throughput, memory benchmarks

## Validator Types

### Protocol Validator (`erlmcp_protocol_validator`)

Validates JSON-RPC 2.0 and MCP 2025-11-25 protocol compliance.

**Validates:**
- JSON-RPC 2.0 structure (jsonrpc version, request/response format)
- MCP protocol (initialize sequence, capability negotiation)
- Error codes (JSON-RPC -32700 to -32603, MCP -32001 to -32010, Refusal 1001-1089)
- Method signatures (tools, resources, prompts, ping, progress)

**Example:**
```erlang
{ok, Report} = erlmcp_protocol_validator:run(stdio).
% Returns: {ok, #{passed => 45, failed => 2, compliance => 95.7}}
```

### Transport Validator (`erlmcp_transport_validator`)

Validates transport behavior against `erlmcp_transport_behavior`.

**Validates:**
- Behavior callbacks (init/1, send/2, close/1)
- Message framing (newline, length prefix, HTTP, WebSocket)
- Registry integration (gproc registration)
- Lifecycle (start, send, close, cleanup)
- Concurrent operations (FIFO order, no loss)

**Example:**
```erlang
{ok, Report} = erlmcp_transport_validator:run(<<"stdio">>).
% Returns: {ok, #{callbacks => 6/6, framing => 2/2, compliance => 100.0}}
```

### Security Validator (`erlmcp_security_validator`)

Validates security features and best practices.

**Validates:**
- Authentication (JWT, token expiry, rejection of invalid auth)
- Input validation (JSON schema, types, URI format)
- Secrets handling (no secrets in logs/errors, secure storage)
- Injection prevention (JSON, command, path traversal, XSS)
- Rate limiting (request throttling, per-client limits)
- CORS (origin validation, security headers)

**Example:**
```erlang
{ok, Report} = erlmcp_security_validator:run(stdio).
% Returns: {ok, #{overall_score => 0.92, categories => #{...}}}
```

### Performance Validator (`erlmcp_performance_validator`)

Validates performance against targets.

**Validates:**
- Latency (P50 < 5ms, P95 < 20ms, P99 < 50ms)
- Throughput (> 1000 req/s)
- Memory (< 100KB per connection)
- Connection setup (< 100ms)
- Concurrent connections (10K with 99% success)

**Example:**
```erlang
{ok, Report} = erlmcp_performance_validator:run(stdio).
% Returns: {ok, #{latency => #{p50 => 3.2, p95 => 15.1, ...}}}
```

## Running Validators

### CLI Usage

```bash
# Run all validators
./erlmcp_validate run --all

# Run specific validator
./erlmcp_validate run --section protocol

# Run for specific transport
./erlmcp_validate run --transport tcp

# Run with verbose output
./erlmcp_validate run --all --verbose

# Generate report
./erlmcp_validate report --format markdown --output compliance.md
```

### Programmatic Usage

```erlang
% Run protocol validator
{ok, ProtocolReport} = erlmcp_protocol_validator:run(stdio).

% Run transport validator
{ok, TransportReport} = erlmcp_transport_validator:run(<<"stdio">>).

% Run security validator
{ok, SecurityReport} = erlmcp_security_validator:run(stdio).

% Run performance validator
{ok, PerfReport} = erlmcp_performance_validator:run(stdio).

% Generate compliance report
{ok, Markdown} = erlmcp_compliance_report:generate_report(markdown, Data).
```

## Interpreting Results

### Result Structure

All validators return a map with:
- `passed` - Number of passed checks
- `failed` - Number of failed checks
- `compliance` - Compliance percentage (0.0-100.0)
- Category-specific metrics

### Example Results

**Protocol Validator:**
```erlang
#{
    passed => 45,
    failed => 2,
    total => 47,
    compliance => 95.7,
    categories => #{
        <<"jsonrpc">> => #{passed => 10, failed => 0},
        <<"mcp_protocol">> => #{passed => 15, failed => 1},
        <<"error_codes">> => #{passed => 12, failed => 0},
        <<"methods">> => #{passed => 8, failed => 1}
    }
}
```

**Transport Validator:**
```erlang
#{
    passed => 24,
    failed => 0,
    total => 24,
    compliance => 100.0,
    categories => #{
        <<"callbacks">> => #{passed => 6, failed => 0},
        <<"framing">> => #{passed => 4, failed => 0},
        <<"registry">> => #{passed => 4, failed => 0},
        <<"lifecycle">> => #{passed => 6, failed => 0},
        <<"concurrent">> => #{passed => 4, failed => 0}
    }
}
```

**Security Validator:**
```erlang
#{
    overall_score => 0.92,
    categories => #{
        <<"authentication">> => #{status => pass, score => 0.95},
        <<"input_sanitization">> => #{status => pass, score => 0.90},
        <<"secrets_handling">> => #{status => warning, score => 0.75},
        <<"injection_prevention">> => #{status => pass, score => 1.0},
        <<"rate_limiting">> => #{status => pass, score => 0.85},
        <<"cors">> => #{status => pass, score => 1.0}
    },
    recommendations => [
        <<"Implement credential encryption for secrets_handling">>
    ]
}
```

**Performance Validator:**
```erlang
#{
    latency => #{
        p50_us => 3200,      % 3.2ms - PASS (< 5000µs)
        p95_us => 15100,     % 15.1ms - PASS (< 20000µs)
        p99_us => 38000,     % 38ms - PASS (< 50000µs)
        status => pass
    },
    throughput => #{
        req_per_s => 1250,   % 1250 req/s - PASS (> 1000)
        status => pass
    },
    memory => #{
        bytes_per_conn => 85000,  % 85KB - PASS (< 100KB)
        status => pass
    },
    connection_setup => #{
        us => 45000,       % 45ms - PASS (< 100ms)
        status => pass
    },
    concurrent => #{
        connections => 10000,
        success_rate => 0.985,  % 98.5% - PASS (≥ 99%)
        status => warning
    }
}
```

## Error Codes

### JSON-RPC 2.0 Standard Errors

| Code | Name | Description |
|------|------|-------------|
| -32700 | Parse error | Invalid JSON received |
| -32600 | Invalid request | JSON is not a valid Request object |
| -32601 | Method not found | Method does not exist |
| -32602 | Invalid params | Invalid method parameters |
| -32603 | Internal error | Internal JSON-RPC error |

### MCP Protocol Errors

| Code | Name | Description |
|------|------|-------------|
| -32001 | Request timed out | Request exceeded timeout |
| -32002 | Request cancelled | Request cancelled by client |
| -32003 | Server not initialized | Request before initialization |
| -32004 | Request already exists | Duplicate request ID |
| -32005 | Cursor not found | Invalid/expired cursor |
| -32006 | Validation error | Request validation failed |
| -32007 | Unsupported capability | Capability not supported |
| -32008 | Resource not found | Resource does not exist |
| -32009 | Tool not found | Tool does not exist |
| -32010 | Prompt not found | Prompt does not exist |

### Refusal Codes (1001-1089)

| Code | Name | Description |
|------|------|-------------|
| 1001 | Permission denied | Lacks permission |
| 1002 | Rate limit exceeded | Exceeded rate limit |
| 1003 | Resource unavailable | Resource temporarily unavailable |
| 1004 | Tool execution failed | Tool encountered error |
| 1005 | Invalid URI | URI is invalid |
| 1006 | Invalid arguments | Arguments are invalid |
| 1007 | Sampling unavailable | LLM sampling not available |
| 1008 | Root not found | Root does not exist |
| 1009 | Subscription failed | Failed to subscribe |
| 1010 | Unsupported protocol version | Protocol version not supported |
| 1089 | Internal server error | Unexpected server error |

## Customization

### Custom Validation Rules

Add custom validation rules:

```erlang
% Define custom rule
validate_custom_feature(Server) ->
    case get_custom_feature(Server) of
        {ok, Value} when is_binary(Value) -> {ok, valid};
        _ -> {error, invalid_custom_feature}
    end.

% Register with validator
% (See validator implementation for extension points)
```

### Custom Performance Targets

Override default targets:

```erlang
% Run with custom targets
{ok, Report} = erlmcp_performance_validator:run(stdio, #{
    samples => 1000,
    requests => 500,
    connections => 5000,
    targets => #{
        p50_latency_us => 3000,   % Custom: < 3ms
        throughput => 2000         % Custom: > 2000 req/s
    }
}).
```

## Integration

### CI/CD Integration

```yaml
# .github/workflows/validation.yml
name: MCP Validation

on: [push, pull_request]

jobs:
  validate:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - uses: erlef/setup-beam@v1
        with:
          otp-version: "25.3"

      - name: Compile
        run: rebar3 compile

      - name: Run validators
        run: |
          ./erlmcp_validate run --all --format json > results.json

      - name: Check compliance
        run: |
          COMPLIANCE=$(jq '.summary.compliance' results.json)
          if (( $(echo "$COMPLIANCE < 80" | bc -l) )); then
            echo "Compliance $COMPLIANCE% below 80% threshold"
            exit 1
          fi
```

### Pre-commit Hook

```bash
#!/bin/bash
# .git/hooks/pre-commit

# Quick validation check
./erlmcp_validate quick-check

if [ $? -ne 0 ]; then
    echo "Validation failed. Commit aborted."
    exit 1
fi
```

## Troubleshooting

### Common Issues

**Issue**: Validator returns "not implemented yet"

**Solution**: Ensure validator module is compiled and available:
```bash
rebar3 compile
```

**Issue**: Transport test times out

**Solution**: Check port availability and ensure transport is running:
```bash
netstat -an | grep LISTEN
```

**Issue**: Performance tests fail on CI

**Solution**: Increase thresholds for CI environment:
```erlang
targets => #{
    p99_latency_us => 100000  % 100ms for CI
}
```

**Issue**: Security validator fails on CORS

**Solution**: CORS only applies to HTTP/WebSocket, skip for stdio/tcp:
```bash
./erlmcp_validate run --section security --transport http
```

### Debug Mode

Enable verbose output:

```bash
# CLI verbose mode
./erlmcp_validate run --all --verbose

# Erlang logger
erl -lager lager_console_backend debug
```

## Best Practices

1. **Run All Validators**: Use `--all` flag for complete validation
2. **Check Compliance**: Aim for ≥ 80% overall compliance
3. **Review Recommendations**: Address security validator recommendations
4. **Monitor Performance**: Track performance regression over time
5. **Automate**: Integrate validators into CI/CD pipeline

## See Also

- [Spec Parser Guide](SPEC_PARSER_GUIDE.md)
- [Spec Compliance Testing](SPEC_COMPLIANCE_TESTING.md)
- [MCP 2025-11-25 Specification](https://modelcontextprotocol.io/)
