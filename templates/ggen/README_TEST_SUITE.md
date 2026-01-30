# MCP Test Suite Template Documentation

## Overview

The `test_suite.erl.tera` template generates comprehensive, MCP 2025-11-25 compliant test suites for Erlang/OTP modules. It follows Chicago School TDD principles and targets 80%+ code coverage.

**Generated**: 2026-01-30
**Generator**: ggen v1.0.0
**Template**: test_suite.erl.tera

## Template Philosophy

### Chicago School TDD
- **Real processes**: No mocks, spawn actual gen_servers
- **State-based verification**: Assert on observable state changes
- **Real message passing**: Test actual inter-process communication
- **Integration over isolation**: Test components together

### Coverage Goals
- **Minimum**: 80% for all modules
- **Core modules**: 85%+ (client, server, registry, transport)
- **Public APIs**: 100% of exported functions tested

## Template Variables

### Required Variables

| Variable | Type | Description | Example |
|----------|------|-------------|---------|
| `module_name` | string | Module being tested | `"erlmcp_client"` |
| `methods` | array | MCP methods with metadata | See below |
| `timestamp` | string | Generation timestamp | `"2026-01-30T17:28:00Z"` |

### Optional Variables

| Variable | Type | Description | Default |
|----------|------|-------------|---------|
| `generator_version` | string | ggen version | `"1.0.0"` |
| `capabilities` | array | MCP capabilities | `[]` |
| `error_codes` | map | Error code definitions | `{}` |
| `integration_flows` | array | Integration scenarios | Default flows |
| `edge_cases` | array | Edge case tests | Default cases |
| `proper_tests` | boolean | Include PropEr tests | `true` |

### Methods Array Structure

Each method object should contain:

```json
{
  "name": "initialize",
  "method_constant": "MCP_METHOD_INITIALIZE",
  "required_params": ["protocolVersion", "capabilities"],
  "optional_params": ["clientInfo"],
  "sample_values": {
    "protocolVersion": "<<\"2025-11-25\">>",
    "capabilities": "#{}"
  },
  "schema": {},
  "error_codes": [-32005, -32061],
  "phase_required": "disconnected",
  "capability_required": null,
  "description": "Initialize MCP session",
  "schema_violations": [
    {
      "field": "protocolVersion",
      "invalid_value": "null"
    }
  ]
}
```

## Generated Test Structure

The template generates 6 test categories:

### 1. Basic Functionality Tests (3 tests)
- Module starts successfully
- Module responds to ping
- Module handles state correctly

### 2. MCP Method Tests (6 tests per method)
For each MCP method:
- Valid request test
- Invalid parameters test
- Phase violation test (if applicable)
- Capability violation test (if applicable)
- Schema validation test
- Error codes test

### 3. Validation Tests (4 tests)
- JSON-RPC version validation
- Request ID validation
- Method name validation
- Params type validation

### 4. Error Handling Tests (4 tests)
- Malformed JSON
- Missing required fields
- Timeout handling
- Error response structure

### 5. Edge Case Tests (configurable)
Default edge cases:
- Concurrent requests (100 simultaneous)
- Request cancellation
- Large payload handling
- Rapid reconnection

### 6. Integration Tests (configurable)
Default flows:
- Initialize flow (initialize → initialized)
- Tool call flow (list → call)
- Resource read flow (list → read)
- Subscription flow (subscribe → update → unsubscribe)

### 7. Property-Based Tests (4 properties, if enabled)
- JSON-RPC encoding/decoding roundtrip
- Request ID correlation
- Error code validity
- Message encoding roundtrip

## Usage Examples

### Example 1: Generate Test Suite for Client Module

```erlang
%% Context data from SPARQL query or manual construction
Context = #{
    module_name => "erlmcp_client",
    timestamp => "2026-01-30T17:28:00Z",
    generator_version => "1.0.0",
    methods => [
        #{
            name => "initialize",
            method_constant => "MCP_METHOD_INITIALIZE",
            required_params => ["protocolVersion", "capabilities"],
            optional_params => ["clientInfo"],
            sample_values => #{
                protocolVersion => <<"\"2025-11-25\"">>,
                capabilities => <<"{}">>
            },
            error_codes => [-32005, -32061],
            phase_required => "disconnected",
            capability_required => null,
            description => "Initialize MCP session",
            schema_violations => []
        },
        #{
            name => "tools/call",
            method_constant => "MCP_METHOD_TOOLS_CALL",
            required_params => ["name"],
            optional_params => ["arguments"],
            sample_values => #{
                name => <<"\"test_tool\"">>,
                arguments => <<"{}">>
            },
            error_codes => [-32002, -32005, -32031],
            phase_required => "initialized",
            capability_required => "tools",
            description => "Call a tool",
            schema_violations => [
                #{field => "name", invalid_value => "null"}
            ]
        }
    ],
    proper_tests => true
}.

%% Render template
{ok, TestCode} = tera:render_file("templates/ggen/test_suite.erl.tera", Context).

%% Write to file
file:write_file("test/erlmcp_client_tests.erl", TestCode).
```

### Example 2: Generate with Custom Edge Cases

```erlang
Context = #{
    module_name => "erlmcp_server",
    timestamp => iso8601:format(erlang:timestamp()),
    methods => [...],  %% Method definitions
    edge_cases => [
        #{
            name => "memory_exhaustion",
            description => "Server handles memory exhaustion gracefully",
            setup => "Create server with memory limits",
            setup_code => "ok = {{ module_name }}:set_memory_limit(Pid, 10 * 1024 * 1024)",
            exercise_code => "spawn(fun() -> consume_memory(100 * 1024 * 1024) end)",
            expected => "Server refuses large requests with MCP_ERROR_MESSAGE_TOO_LARGE",
            verify_code => "?assertEqual({error, ?MCP_ERROR_MESSAGE_TOO_LARGE}, Result)"
        }
    ],
    integration_flows => [
        #{
            name => "full_session",
            description => "Complete session lifecycle from connect to disconnect",
            test_code => <<
                "%% 1. Connect\n"
                "{ok, _} = {{ module_name }}:connect(Pid),\n"
                "%% 2. Initialize\n"
                "{ok, _} = {{ module_name }}:initialize(Pid, #{...}),\n"
                "%% 3. Use capabilities\n"
                "{ok, _} = {{ module_name }}:call_tool(Pid, ...),\n"
                "%% 4. Disconnect\n"
                "ok = {{ module_name }}:disconnect(Pid)"
            >>
        }
    ]
}.
```

### Example 3: Minimal Generation (No PropEr)

```erlang
Context = #{
    module_name => "erlmcp_transport_stdio",
    timestamp => "2026-01-30T17:28:00Z",
    methods => [
        #{
            name => "send",
            method_constant => "send",
            required_params => ["data"],
            optional_params => [],
            sample_values => #{data => <<"\"test\"">>},
            error_codes => [-32008],
            phase_required => null,
            capability_required => null,
            description => "Send data via transport",
            schema_violations => []
        }
    ],
    proper_tests => false  %% Disable property-based tests
}.
```

## Generated Test Patterns

### Valid Request Test Pattern
```erlang
test_initialize_valid_request(#{pid := Pid}) ->
    %% Setup: Create valid request
    Request = create_valid_request(MCP_METHOD_INITIALIZE, #{
        protocolVersion => <<"2025-11-25">>,
        capabilities => #{}
    }),

    %% Exercise: Send request (real message passing, Chicago School)
    {ok, Response} = erlmcp_client:handle_request(Pid, Request),

    %% Verify: Response has expected structure (state-based verification)
    ?assertMatch(#{
        <<"jsonrpc">> := <<"2.0">>,
        <<"id">> := _,
        <<"result">> := _
    }, Response),

    %% Verify: No error in response
    ?assertEqual(false, maps:is_key(<<"error">>, Response)).
```

### Invalid Parameters Test Pattern
```erlang
test_tools_call_invalid_params(#{pid := Pid}) ->
    %% Setup: Create request with invalid params
    Request = create_valid_request(MCP_METHOD_TOOLS_CALL, #{
        <<"invalid_param">> => <<"should_fail">>
    }),

    %% Exercise: Send request
    {error, Response} = erlmcp_client:handle_request(Pid, Request),

    %% Verify: Error code is INVALID_PARAMS (MCP compliance)
    ?assertMatch(#{
        <<"error">> := #{
            <<"code">> := ?JSONRPC_INVALID_PARAMS
        }
    }, Response).
```

### Phase Violation Test Pattern
```erlang
test_tools_call_phase_violation(#{pid := Pid}) ->
    %% Setup: Ensure wrong phase (not initialized)
    Request = create_valid_request(MCP_METHOD_TOOLS_CALL, #{}),

    %% Exercise: Send request in wrong phase
    {error, Response} = erlmcp_client:handle_request(Pid, Request),

    %% Verify: Error code indicates phase violation
    ?assertMatch(#{
        <<"error">> := #{
            <<"code">> := ?MCP_ERROR_NOT_INITIALIZED
        }
    }, Response).
```

### Integration Test Pattern
```erlang
test_initialize_flow(#{pid := Pid}) ->
    %% Step 1: Send initialize request
    InitRequest = create_valid_request(MCP_METHOD_INITIALIZE, #{
        <<"protocolVersion">> => ?MCP_VERSION,
        <<"capabilities">> => #{}
    }),
    {ok, InitResponse} = erlmcp_client:handle_request(Pid, InitRequest),

    %% Step 2: Send initialized notification
    InitializedNotif = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"method">> => MCP_METHOD_INITIALIZED,
        <<"params">> => #{}
    },
    ok = erlmcp_client:handle_notification(Pid, InitializedNotif),

    %% Verify: State is now initialized
    {ok, State} = erlmcp_client:get_state(Pid),
    ?assertMatch(#{initialized := true}, State).
```

## Running Generated Tests

### EUnit
```bash
# Run all tests for generated module
rebar3 eunit --module=erlmcp_client_tests

# Run specific test
rebar3 eunit --module=erlmcp_client_tests --test=test_initialize_valid_request
```

### Common Test
```bash
# If generated as CT suite
rebar3 ct --suite=test/erlmcp_client_tests_SUITE
```

### With Coverage
```bash
# Generate coverage report
rebar3 do eunit --module=erlmcp_client_tests, cover --verbose
```

### PropEr (if enabled)
```bash
# Run property tests
rebar3 proper -c --module=erlmcp_client_tests
```

## Quality Gates

Generated tests automatically enforce these gates:

1. **Compilation**: Tests must compile cleanly
2. **Pass Rate**: 100% of tests must pass
3. **Coverage**: 80%+ for all modules, 85%+ for core
4. **Chicago School**: No mocks, real processes verified
5. **MCP Compliance**: All error codes valid, all phases respected

## Customization

### Adding Custom Edge Cases

Extend `edge_cases` array:
```erlang
#{
    name => "custom_edge_case",
    description => "Description of what's being tested",
    setup => "Setup description",
    setup_code => "Erlang setup code",
    exercise_code => "Erlang exercise code",
    verify_code => "EUnit assertions"
}
```

### Adding Custom Integration Flows

Extend `integration_flows` array:
```erlang
#{
    name => "custom_flow",
    description => "Multi-step integration scenario",
    test_code => <<
        "%% Step 1\n"
        "Code here\n"
        "%% Step 2\n"
        "More code\n"
    >>
}
```

### Disabling PropEr Tests

Set `proper_tests => false` in context.

## Best Practices

1. **Always provide sample_values**: Ensures valid requests in tests
2. **Define schema_violations**: Enables thorough validation testing
3. **List all error_codes**: Validates error handling completeness
4. **Use real collaborators**: Follow Chicago School TDD (no mocks)
5. **Test full flows**: Integration tests reveal real issues
6. **Target 85%+ coverage**: For production-critical modules

## Troubleshooting

### Tests fail to compile
- Verify module_name matches actual module
- Check method_constants match erlmcp.hrl definitions
- Ensure sample_values use valid Erlang syntax

### Low coverage
- Add edge_cases for uncovered code paths
- Add integration_flows for multi-step scenarios
- Verify all public API functions tested

### Proper tests fail
- Check generators produce valid data
- Verify roundtrip properties for encoding
- Ensure normalization handles all types

## References

- MCP Specification: 2025-11-25
- CLAUDE.md: Chicago School TDD patterns
- docs/otp-patterns.md: Test patterns
- test/tcps_test_helper.erl: Helper utilities

## Version History

- **1.0.0** (2026-01-30): Initial template with MCP 2025-11-25 compliance
