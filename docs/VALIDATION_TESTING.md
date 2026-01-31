# Validation Testing - Chicago School TDD Methodology

## Purpose and Scope

This document describes the testing methodology and practices used throughout the erlmcp validation framework. It follows **Chicago School TDD** principles with strict requirements:

- **NO MOCKS** - All tests use real processes and actual implementations
- **State-based verification** - Verify actual state, not behavior
- **Black-box testing** - Test only observable behavior
- **80%+ code coverage** - Minimum coverage requirement

## Chicago School TDD Principles

### 1. No Mocks, Fakes, or Stubs

**ABSOLUTE RULE**: Never use mock libraries or fake implementations.

```erlang
%% ❌ WRONG: Using meck to mock a module
meck:new(erlmcp_json_rpc, [passthrough]),
meck:expect(erlmcp_json_rpc, encode, fun(_) -> ok end),

%% ✅ CORRECT: Test with real erlmcp_json_rpc module
%% Start real process, send real message, verify real result
{ok, Pid} = erlmcp_json_rpc:start_link(),
Result = erlmcp_json_rpc:encode(Request),
?assertMatch(#{<<"jsonrpc">> := <<"2.0">>}, Result).
```

### 2. State-Based Verification

Verify actual state through gen_server:call or direct state inspection, not by checking if functions were called.

```erlang
%% ❌ WRONG: Verifying behavior (function calls)
?assert(meck:called(erlmcp_registry, register, ['_'])),

%% ✅ CORRECT: Verifying state
{ok, State} = erlmcp_registry:get_state(),
?assert(maps:is_key(expected_key, State)).
```

### 3. Black-Box Testing

Test through the public API only, never implementation details.

```erlang
%% ❌ WRONG: Testing internal function
?assertEqual(expected, internal_module:internal_function(Input)),

%% ✅ CORRECT: Testing through public API
Request = make_request(Input),
{ok, Response} = send_request(Request),
?assertMatch(#{result := expected}, Response).
```

### 4. Test Observable Behavior

Test only what can be observed through message passing, ports, or state.

```erlang
%% ✅ CORRECT: Test what comes out the other side
send_message(Input),
receive
    {response, Output} -> ?assertEqual(expected, Output)
after 1000 ->
    ?assert(timeout)
end.
```

## Test Structure

### EUnit Tests

Located in `apps/*/test/*_tests.erl`.

```erlang
-module(erlmcp_protocol_validator_tests).
-include_lib("eunit/include/eunit.hrl").

%% Test generator for multiple inputs
validate_jsonrpc_test_() ->
    [
        {"Valid request", fun() ->
            Request = #{<<"jsonrpc">> => <<"2.0">>, <<"method">> => <<"ping">>},
            ?assertEqual(ok, erlmcp_protocol_validator:validate_jsonrpc(Request))
        end},
        {"Missing jsonrpc field", fun() ->
            Request = #{<<"method">> => <<"ping">>},
            ?assertMatch({error, #{reason := invalid_jsonrpc_version}},
                erlmcp_protocol_validator:validate_jsonrpc(Request))
        end}
    ].

%% Setup and teardown
with_validator_test_() ->
    {setup,
        fun() -> {ok, Pid} = erlmcp_protocol_validator:start_link(), Pid end,
        fun(Pid) -> gen_server:stop(Pid) end,
        fun(Pid) ->
            [
                {"Test with running validator", fun() ->
                    ?assert(is_process_alive(Pid))
                end}
            ]
        end}.
```

### Common Test Suites

Located in `apps/*/test/*_SUITE.erl`.

```erlang
-module(erlmcp_validation_SUITE).
-include_lib("common_test/include/ct.hrl").

%% Suite configuration
all() -> [
    {group, protocol_tests},
    {group, transport_tests},
    {group, security_tests}
].

groups() -> [
    {protocol_tests, [sequence], [
        jsonrpc_validation,
        method_validation,
        error_code_validation
    ]},
    {transport_tests, [sequence], [
        stdio_transport,
        tcp_transport,
        http_transport
    ]}
].

%% Init per suite
init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(erlmcp_validation),
    Config.

%% End per suite
end_per_suite(_Config) ->
    application:stop(erlmcp_validation),
    ok.

%% Test case
jsonrpc_validation(_Config) ->
    Request = #{<<"jsonrpc">> => <<"2.0">>, <<"method">> => <<"ping">>},
    ok = erlmcp_protocol_validator:validate_jsonrpc(Request).
```

### Property-Based Tests

Using Proper for property-based testing.

```erlang
-module(erlmcp_protocol_validator_proper_tests).
-include_lib("proper/include/proper.hrl").

%% Property: Valid error codes are valid
prop_valid_error_codes() ->
    ?FORALL(Code, oneof([-32700, -32600, -32001, 1001, 1089]),
        begin
            ok =:= erlmcp_protocol_validator:validate_error_code(Code)
        end).

%% Property: Message with jsonrpc 2.0 is valid
prop_jsonrpc_version() ->
    ?FORALL(Method, binary(10),
        begin
            Message = #{
                <<"jsonrpc">> => <<"2.0">>,
                <<"method">> => Method,
                <<"id">> => 1
            },
            ok =:= erlmcp_protocol_validator:validate_jsonrpc(Message)
        end).
```

## Test Coverage Requirements

### Minimum 80% Coverage

All modules must maintain at least 80% code coverage.

```bash
# Generate coverage report
rebar3 cover --verbose

# Check specific module
rebar3 cover -v --module=erlmcp_protocol_validator
```

### Coverage Enforcement

Coverage is checked in CI/CD pipelines. Below 80% fails the build.

```bash
# In .github/workflows/quality-gate.yml
- name: Check coverage
  run: |
    rebar3 cover
    PERCENT=$(rebar3 cover | grep "100%" | awk '{print $2}' | sed 's/%//')
    if [ $PERCENT -lt 80 ]; then
      echo "Coverage ${PERCENT}% is below 80%"
      exit 1
    fi
```

## Testing Best Practices

### 1. Test Isolation

Each test should be independent and runnable in any order.

```erlang
%% Use unique identifiers to avoid conflicts
generate_unique_id() ->
    <<Id:64>> = crypto:strong_rand_bytes(8),
    integer_to_list(Id, 36).

isolated_test() ->
    SessionId = generate_unique_id(),
    {ok, Pid} = start_session(SessionId),
    ?assert(is_process_alive(Pid)),
    gen_server:stop(Pid).
```

### 2. Use Real Timeouts

Always specify reasonable timeouts for gen_server calls.

```erlang
%% Good: Specific timeout
gen_server:call(Server, request, 5000)

%% Bad: Default infinity timeout (can hang tests)
gen_server:call(Server, request)
```

### 3. Clean Up Resources

Always clean up processes, ports, and files.

```erlang
cleanup_test() ->
    File = "/tmp/test_file",
    {ok, Fd} = file:open(File, [write]),
    try
        file:write(Fd, <<"test">>),
        ?assert(filelib:is_file(File))
    after
        file:close(Fd),
        file:delete(File)
    end.
```

### 4. Test Error Paths

Test not just success cases, but all error conditions.

```erlang
error_path_test() ->
    %% Test invalid input
    {error, _} = erlmcp_protocol_validator:validate_request(
        <<"invalid/method">>,
        #{}
    ),

    %% Test missing fields
    {error, _} = erlmcp_protocol_validator:validate_jsonrpc(#{}),

    %% Test type mismatches
    {error, _} = erlmcp_protocol_validator:validate_field_type(
        <<"id">>,
        not_a_number,
        integer
    ).
```

## Test Organization

### Directory Structure

```
apps/
├── erlmcp_core/
│   └── test/
│       ├── erlmcp_json_rpc_tests.erl        (EUnit)
│       ├── erlmcp_protocol_validator_tests.erl
│       └── erlmcp_validation_SUITE.ct        (Common Test)
├── erlmcp_transports/
│   └── test/
│       ├── erlmcp_transport_stdio_tests.erl
│       └── erlmcp_transport_SUITE.ct
└── erlmcp_validation/
    └── test/
        ├── erlmcp_protocol_validator_tests.erl
        ├── erlmcp_security_validator_tests.erl
        ├── erlmcp_performance_validator_tests.erl
        └── erlmcp_validation_SUITE.ct
```

### Test Naming Conventions

- EUnit: `<module>_tests.erl`
- Common Test: `<category>_SUITE.ct`
- Proper: `<module>_proper_tests.erl`

## Running Tests

### Run All Tests

```bash
# EUnit tests
rebar3 eunit

# Common Test suites
rebar3 ct

# Both with coverage
rebar3 cover
```

### Run Specific Tests

```bash
# EUnit module
rebar3 eunit --module=erlmcp_protocol_validator_tests

# Common Test suite
rebar3 ct --suite=apps/erlmcp_validation/test/erlmcp_validation_SUITE

# Specific test case
rebar3 ct --suite=erlmcp_validation_SUITE --case=jsonrpc_validation
```

### Coverage Reports

```bash
# Generate coverage report
rebar3 cover

# Open coverage report in browser
open _build/test/cover/index.html
```

## Test Examples by Category

### Protocol Validation Tests

```erlang
%% JSON-RPC version validation
jsonrpc_version_test() ->
    ?assertEqual(ok, erlmcp_protocol_validator:validate_jsonrpc(
        #{<<"jsonrpc">> => <<"2.0">>, <<"method">> => <<"ping">>}
    )).

%% Method name validation
valid_method_name_test() ->
    ?assertEqual(ok, erlmcp_protocol_validator:validate_method_name(
        <<"tools/list">>
    )).

%% Error code validation
valid_error_code_test() ->
    ?assertEqual(ok, erlmcp_protocol_validator:validate_error_code(-32700)).
```

### Transport Validation Tests

```erlang
%% Transport callback validation
callback_validation_test() ->
    Result = erlmcp_transport_validator:validate_callbacks(
        erlmcp_transport_stdio
    ),
    ?assertEqual(passed, maps:get(status, Result)).

%% Framing validation
framing_validation_test() ->
    Result = erlmcp_transport_validator:validate_framing(
        erlmcp_transport_tcp,
        tcp
    ),
    ?assertEqual(passed, maps:get(status, Result)).
```

### Security Validation Tests

```erlang
%% Authentication validation
authentication_test() ->
    Result = erlmcp_security_validator:validate_authentication(
        erlmcp_transport_http
    ),
    ?assert(maps:get(passed, Result) > 0).

%% Secret scanning test
secret_detection_test() ->
    %% This test uses real code scanning, not mocks
    Results = scan_for_secrets("apps/erlmcp_core/src"),
    ?assertEqual(0, length(Results)).
```

### Performance Validation Tests

```erlang
%% Latency measurement test
latency_test() ->
    {ok, Result} = erlmcp_performance_validator:measure_latency(stdio, 10),
    P99 = maps:get(p99_us, Result),
    ?assert(P99 < 100000).  % < 100ms

%% Throughput test
throughput_test() ->
    {ok, Result} = erlmcp_performance_validator:measure_throughput(
        stdio,
        100
    ),
    RPS = maps:get(requests_per_second, Result),
    ?assert(RPS > 100).  % > 100 req/s
```

## Troubleshooting Tests

### Common Issues

#### Race Conditions

**Symptom:** Tests fail intermittently

**Solution:** Use proper synchronization
```erlang
%% Instead of sleep
timer:sleep(100),

%% Use proper message passing
{ok, Ref} = start_process(),
receive
    {Ref, ready} -> ok
after 1000 ->
    ?assert(timeout)
end.
```

#### Port Already in Use

**Symptom:** "eaddrinuse" error

**Solution:** Use random ports
```erlang
%% Get random available port
Port = 40000 + rand:uniform(10000),
{ok, Listen} = gen_tcp:listen(0, [binary, {active, false}]),
{ok, AssignedPort} = inet:port(Listen),
gen_tcp:close(Listen),
```

#### Process Not Exiting

**Symptom:** Tests hang

**Solution:** Ensure proper cleanup
```erlang
cleanup(Pid) when is_pid(Pid) ->
    monitor(process, Pid),
    gen_server:stop(Pid),
    receive
        {'DOWN', _, process, Pid, _} -> ok
    after 1000 ->
        ?assert(exit_timeout)
    end.
```

## Related Documentation

- [MCP_SPEC_VALIDATION.md](MCP_SPEC_VALIDATION.md) - Validation overview
- [COMPLIANCE_REPORTING.md](COMPLIANCE_REPORTING.md) - Evidence collection
- [erlmcp_test_helpers](../apps/erlmcp_core/test/erlmcp_test_helpers.erl) - Test utilities
