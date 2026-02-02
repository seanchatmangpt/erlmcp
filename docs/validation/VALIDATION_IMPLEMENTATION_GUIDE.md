# erlmcp Validation Implementation Guide

**Version**: 2.1.0
**Date**: 2026-02-01
**Target Audience**: Build Engineers implementing validation components

---

## Table of Contents

1. [Quick Start](#quick-start)
2. [Development Workflow](#development-workflow)
3. [Adding New Validators](#adding-new-validators)
4. [Testing Strategy](#testing-strategy)
5. [Quality Gates](#quality-gates)
6. [Evidence Collection](#evidence-collection)
7. [Common Patterns](#common-patterns)
8. [Troubleshooting](#troubleshooting)

---

## Quick Start

### Prerequisites
- Erlang/OTP 28.3.1 (STRICT requirement)
- rebar3 3.24.0+
- Git (for hooks integration)

### Setup

```bash
# 1. Clone repository
git clone https://github.com/banyan-platform/erlmcp.git
cd erlmcp

# 2. Install OTP 28.3.1 (if not installed)
source .erlmcp/env.sh  # Uses custom OTP at $ERLMCP_OTP_BIN

# 3. Fetch dependencies
rebar3 get-deps

# 4. Compile all apps
TERM=dumb rebar3 compile

# 5. Run tests
rebar3 eunit
rebar3 ct

# 6. Run quality gates
make verify
```

### Basic Usage

```bash
# Validate everything
erlmcp validate

# Validate specific component
erlmcp validate spec
erlmcp validate protocol
erlmcp validate transport
erlmcp validate security

# Generate compliance report
erlmcp validate report --format markdown --output compliance.md

# Run with evidence collection
erlmcp validate --evidence --format json
```

---

## Development Workflow

### Chicago School TDD Workflow

**Step 1: Write Test First**
```erlang
%% test/erlmcp_my_validator_tests.erl
-module(erlmcp_my_validator_tests).
-include_lib("eunit/include/eunit.hrl").

start_stop_test() ->
    %% Test gen_server lifecycle
    {ok, Pid} = erlmcp_my_validator:start_link(),
    ?assert(is_process_alive(Pid)),
    ok = gen_server:stop(Pid),
    timer:sleep(10),
    ?assertNot(is_process_alive(Pid)).

validate_positive_case_test() ->
    %% Test valid input
    {ok, Pid} = erlmcp_my_validator:start_link(),
    Input = #{valid => data},
    Result = erlmcp_my_validator:validate(Pid, Input),
    ?assertMatch({ok, _}, Result),
    gen_server:stop(Pid).

validate_negative_case_test() ->
    %% Test invalid input
    {ok, Pid} = erlmcp_my_validator:start_link(),
    Input = #{invalid => data},
    Result = erlmcp_my_validator:validate(Pid, Input),
    ?assertMatch({error, _}, Result),
    gen_server:stop(Pid).
```

**Step 2: Run Tests (Should Fail)**
```bash
rebar3 eunit --module=erlmcp_my_validator_tests
# Expected: Fails (module not implemented)
```

**Step 3: Implement Minimal Code**
```erlang
%% src/erlmcp_my_validator.erl
-module(erlmcp_my_validator).
-behaviour(gen_server).

%% API
-export([start_link/0, validate/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {}).

start_link() ->
    gen_server:start_link(?MODULE, [], []).

validate(Pid, Input) ->
    gen_server:call(Pid, {validate, Input}).

init([]) ->
    {ok, #state{}}.

handle_call({validate, Input}, _From, State) ->
    case is_valid(Input) of
        true -> {reply, {ok, Input}, State};
        false -> {reply, {error, invalid_input}, State}
    end;
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions
is_valid(#{valid := _}) -> true;
is_valid(_) -> false.
```

**Step 4: Run Tests (Should Pass)**
```bash
rebar3 eunit --module=erlmcp_my_validator_tests
# Expected: All tests pass
```

**Step 5: Run Quality Gates**
```bash
make check
# Expected: All gates pass
```

**Step 6: Commit**
```bash
git add src/erlmcp_my_validator.erl test/erlmcp_my_validator_tests.erl
git commit -m "feat: Add my_validator for X validation

Implements gen_server behavior with validate/2 API.
All 6 callbacks implemented. Chicago TDD compliant.

Tests: 3 passed, 0 failed
Coverage: 85%
Gates: compile ✅, dialyzer ✅, xref ✅"
```

---

## Adding New Validators

### 1. Choose Validator Type

#### gen_server Validator (Stateful)
```erlang
-module(erlmcp_my_validator).
-behaviour(gen_server).

%% API
-export([start_link/0, validate/2]).

%% Callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
    cache :: map(),
    count :: non_neg_integer()
}).
```

**Use when**:
- Need to maintain state (cache, counters)
- Long-running validation process
- Async validation tasks

#### Stateless Validator (Functional)
```erlang
-module(erlmcp_my_validator).

%% API (no gen_server)
-export([validate/1, is_valid/1, check_format/1]).
```

**Use when**:
- Simple validation logic
- No state needed
- Pure functions

### 2. Implement API

```erlang
%% @doc Start the validator
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Validate input
-spec validate(pid() | atom(), map()) -> {ok, map()} | {error, term()}.
validate(Pid, Input) when is_pid(Pid) ->
    gen_server:call(Pid, {validate, Input}, 5000);
validate(Server, Input) when is_atom(Server) ->
    gen_server:call(Server, {validate, Input}, 5000).
```

### 3. Implement gen_server Callbacks

#### init/1 (Non-blocking)
```erlang
init([]) ->
    %% GOOD: Async initialization
    self() ! async_init,
    {ok, #state{cache = #{}, count = 0}}.

%% BAD: Blocking initialization
init([]) ->
    %% NEVER BLOCK HERE
    ets:new(my_table, [named_table]),
    lists:foreach(fun heavy_computation/1, large_list()),
    {ok, #state{}}.
```

#### handle_call/3 (Synchronous)
```erlang
handle_call({validate, Input}, _From, State) ->
    case do_validate(Input, State) of
        {ok, Result} ->
            NewState = State#state{count = State#state.count + 1},
            {reply, {ok, Result}, NewState};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.
```

#### handle_cast/2 (Asynchronous)
```erlang
handle_cast({cache_update, Key, Value}, State) ->
    NewCache = maps:put(Key, Value, State#state.cache),
    {noreply, State#state{cache = NewCache}};
handle_cast(_Msg, State) ->
    {noreply, State}.
```

#### handle_info/2 (Other Messages)
```erlang
handle_info(async_init, State) ->
    %% Do blocking initialization here
    ets:new(my_table, [named_table, public]),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.
```

#### terminate/2 (Cleanup)
```erlang
terminate(_Reason, #state{cache = Cache}) ->
    %% Cleanup resources
    maps:foreach(fun(Key, _Val) ->
                   ets:delete(my_table, Key)
               end, Cache),
    ok.
```

#### code_change/3 (Hot Upgrade)
```erlang
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
```

### 4. Add to Supervisor

```erlang
%% src/erlmcp_validation_sup.erl
init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 10,
        period => 60
    },
    ChildSpecs = [
        #{id => erlmcp_my_validator,
          start => {erlmcp_my_validator, start_link, []},
          restart => permanent,
          shutdown => 5000,
          type => worker,
          modules => [erlmcp_my_validator]}
    ],
    {ok, {SupFlags, ChildSpecs}}.
```

### 5. Write Tests

```erlang
%% test/erlmcp_my_validator_tests.erl
-module(erlmcp_my_validator_tests).
-include_lib("eunit/include/eunit.hrl").

%% Setup/Teardown
my_validator_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [{"Start and stop", fun test_start_stop/0},
      {"Validate valid input", fun test_validate_valid/0},
      {"Validate invalid input", fun test_validate_invalid/0},
      {"Cache update", fun test_cache_update/0}]}.

setup() ->
    {ok, Pid} = erlmcp_my_validator:start_link(),
    Pid.

cleanup(Pid) ->
    gen_server:stop(Pid).

test_start_stop() ->
    {ok, Pid} = erlmcp_my_validator:start_link(),
    ?assert(is_process_alive(Pid)),
    ok = gen_server:stop(Pid).

test_validate_valid(Pid) ->
    Input = #{valid => data},
    ?assertMatch({ok, _}, erlmcp_my_validator:validate(Pid, Input)).

test_validate_invalid(Pid) ->
    Input = #{invalid => data},
    ?assertMatch({error, _}, erlmcp_my_validator:validate(Pid, Input)).

test_cache_update(Pid) ->
    ok = gen_server:cast(Pid, {cache_update, key1, value1}),
    timer:sleep(100),  % Wait for cast to process
    ?assertMatch({ok, #{cached := value1}},
                 erlmcp_my_validator:validate(Pid, #{key1 => request})).
```

### 6. Run Quality Gates

```bash
# Compile
TERM=dumb rebar3 compile

# Unit tests
rebar3 eunit --module=erlmcp_my_validator_tests

# Common tests
rebar3 ct --suite=test/erlmcp_my_validator_SUITE

# Coverage
rebar3 cover -v

# Dialyzer
rebar3 dialyzer

# Xref
rebar3 xref

# All gates
make check
```

---

## Testing Strategy

### EUnit Tests (Unit Tests)

**Purpose**: Test individual functions and gen_server callbacks
**Location**: `test/*_tests.erl`
**Runtime**: <1 second per test

**Example**:
```erlang
validate_method_name_test() ->
    %% Test valid method name
    ?assertEqual(true, erlmcp_spec_parser:is_valid_request_type(<<"tools/call">>)),

    %% Test invalid method name
    ?assertEqual(false, erlmcp_spec_parser:is_valid_request_type(<<"invalid_method">>)).

validate_uri_test() ->
    %% Test valid URI
    ValidURI = <<"file://path/to/resource">>,
    ?assertMatch({ok, _}, erlmcp_uri_validator:validate_uri(ValidURI)),

    %% Test invalid URI (blocked scheme)
    InvalidURI = <<"ftp://path/to/resource">>,
    ?assertMatch({error, _}, erlmcp_uri_validator:validate_uri(InvalidURI)).
```

### Common Test Suites (Integration Tests)

**Purpose**: Test integration between components
**Location**: `test/*_SUITE.erl`
**Runtime**: 1-10 seconds per test

**Example**:
```erlang
%% test/erlmcp_validation_integration_SUITE.erl
-module(erlmcp_validation_integration_SUITE).
-include_lib("common_test/include/ct.hrl").

%% Suite callback
all() ->
    [validate_end_to_end,
     spec_parser_integration,
     compliance_report_generation].

init_per_suite(Config) ->
    application:ensure_all_started(erlmcp_validation),
    Config.

end_per_suite(_Config) ->
    application:stop(erlmcp_validation).

%% Test case
validate_end_to_end(_Config) ->
    %% 1. Start validators
    {ok, SpecParser} = erlmcp_spec_parser:start_link(),
    {ok, ProtocolValidator} = erlmcp_protocol_validator:start_link(),

    %% 2. Validate message
    Message = #{jsonrpc => <<"2.0">>,
                id => 1,
                method => <<"tools/call">>,
                params => #{name => <<"my_tool">>}},

    {ok, Validated} = erlmcp_protocol_validator:validate_request(Message),

    %% 3. Check result
    ?assertMatch(#{jsonrpc := <<"2.0">>}, Validated),

    %% 4. Cleanup
    gen_server:stop(SpecParser),
    gen_server:stop(ProtocolValidator).
```

### Property-Based Tests (Proper)

**Purpose**: Test with generated random inputs
**Location**: `test/*_proper_tests.erl`
**Runtime**: Variable (depends on numtests)

**Example**:
```erlang
%% Test that valid URIs always pass validation
prop_valid_uri_passes() ->
    ?FORALL({Scheme, Path},
            {oneof([<<"file">>, <<"http">>, <<"https">>]),
             non_empty(binary())},
            begin
                URI = <<Scheme/binary, "://", Path/binary>>,
                case erlmcp_uri_validator:validate_uri(URI) of
                    {ok, _} -> true;
                    _ -> false
                end
            end).
```

---

## Quality Gates

### Running Individual Gates

```bash
# Security gate
erlmcp validate security

# Type safety gate
erlmcp validate type-safety

# Protocol compliance gate
erlmcp validate protocol

# OTP patterns gate
erlmcp validate otp

# Performance gate
erlmcp validate performance
```

### Gate Result Format

```erlang
#{gate := security,
  status := pass,
  mandatory := true,
  pass_count := 23,
  fail_count := 0,
  warning_count := 0,
  checks := [
      #{check := "Authentication required",
        status := pass,
        details := "All endpoints require authentication",
        file := undefined,
        line := 0},
      #{check := "TLS requirement",
        status := pass,
        details := "All network transports use TLS",
        file := undefined,
        line := 0}
      %% ... more checks
  ],
  blockers := [],
  timestamp := 1738363200}
```

### Custom Quality Gates

```erlang
%% Add custom gate to erlmcp_quality_gates.erl
-spec gate_custom_validator() -> gate_result().
gate_custom_validator() ->
    Checks = [
        check_custom_rule_1(),
        check_custom_rule_2(),
        check_custom_rule_3()
    ],
    compile_gate_result(custom_validator, Checks, true).

%% Check implementation
check_custom_rule_1() ->
    case validate_custom_rule() of
        true ->
            #{check => "Custom Rule 1",
              status => pass,
              details => "Rule 1 satisfied",
              file => undefined,
              line => 0};
        false ->
            #{check => "Custom Rule 1",
              status => fail,
              details => "Rule 1 violated",
              file => <<"src/my_module.erl">>,
              line => 42}
    end.
```

---

## Evidence Collection

### Creating Evidence

```erlang
%% Collect test result evidence
TestData = #{module => erlmcp_my_validator_tests,
             function => validate_valid_input,
             status => passed,
             runtime_ms => 5},
{ok, Evidence} = erlmcp_compliance_report:collect_evidence(
    test_result, TestData#{timestamp => iso8601_timestamp()}).

%% Collect coverage evidence
CoverageData = #{module => erlmcp_my_validator,
                 coverage_percentage => 85.0,
                 lines_covered => 170,
                 lines_total => 200},
{ok, CoverageEvidence} = erlmcp_compliance_report:collect_evidence(
    coverage_metrics, CoverageData).

%% Collect security scan evidence
SecurityData = #{scanner => bandit,
                 issues_found => 0,
                 scan_report => #{critical => 0, high => 0, medium => 0, low => 0}},
{ok, SecurityEvidence} = erlmcp_compliance_report:collect_evidence(
    security_scan, SecurityData).
```

### Creating Evidence Bundles

```erlang
%% Create evidence bundle directory
BundlePath = "/tmp/evidence_bundle_20260201",
{ok, _} = erlmcp_compliance_report:create_evidence_bundle(BundlePath).

%% Store evidence
ok = erlmcp_compliance_report:store_evidence_bundle(
    BundlePath, [Evidence, CoverageEvidence, SecurityEvidence]).

%% Link to receipt chain
ReceiptChain = #{commit_hash => "abc123",
                 branch => "main",
                 author => "build-engineer"},
{ok, ReceiptPath} = erlmcp_compliance_report:link_receipt_chain(
    BundlePath, ReceiptChain).
```

### Verifying Evidence Integrity

```erlang
%% Hash evidence
Content = #{test => "my_test", status => passed},
{ok, Hash} = erlmcp_compliance_report:hash_evidence(Content).

%% Verify evidence hasn't been tampered
{ok, IsValid} = erlmcp_compliance_report:verify_evidence_integrity(
    Content, Hash),
?assertEqual(true, IsValid).
```

---

## Common Patterns

### Pattern 1: Validation with Error Context

```erlang
validate_message(Message) ->
    case validate_jsonrpc_version(Message) of
        {ok, Version} ->
            case validate_method(Message) of
                {ok, Method} ->
                    case validate_params(Message) of
                        {ok, Params} ->
                            {ok, #{version => Version,
                                   method => Method,
                                   params => Params}};
                        {error, Reason} ->
                            {error, {invalid_params, Reason}}
                    end;
                {error, Reason} ->
                    {error, {invalid_method, Reason}}
            end;
        {error, Reason} ->
            {error, {invalid_version, Reason}}
    end.

%% Alternative: With error handling Monad (if available)
validate_message_monad(Message) ->
    do([ErrorM ||
        Version <- validate_jsonrpc_version(Message),
        Method <- validate_method(Message),
        Params <- validate_params(Message),
        return(#{version => Version,
                 method => Method,
                 params => Params})
    ]).
```

### Pattern 2: Streaming Validation

```erlang
validate_stream(Stream) ->
    validate_stream(Stream, #{valid => 0, invalid => 0, errors => []}).

validate_stream({done, Ref}, Acc) ->
    {ok, Acc};
validate_stream({data, Data, Ref}, Acc = #{valid := V, invalid := I}) ->
    case validate_item(Data) of
        {ok, _} ->
            validate_stream(continue, Acc#{valid => V + 1});
        {error, Reason} ->
            NewErrors = [Reason | maps:get(errors, Acc)],
            validate_stream(continue, Acc#{invalid => I + 1,
                                          errors => NewErrors})
    end.
```

### Pattern 3: Parallel Validation

```erlang
validate_parallel([Item | Rest]) ->
    Parent = self(),
    Ref = make_ref(),

    %% Spawn validator for each item
    Pids = [spawn(fun() ->
                     Result = validate_item(Item),
                     Parent ! {Ref, Item, Result}
                  end) || Item <- [Item | Rest]],

    %% Collect results
    collect_results(Pids, Ref, #{}).

collect_results([], _Ref, Acc) ->
    {ok, Acc};
collect_results([Pid | Pids], Ref, Acc) ->
    receive
        {Ref, Item, Result} ->
            collect_results(Pids, Ref, maps:put(Item, Result, Acc))
    after 5000 ->
        {error, timeout}
    end.
```

### Pattern 4: Caching Validation Results

```erlang
%% gen_server state with cache
-record(state, {
    cache :: #{binary() => {ok, map()} | {error, term()}},
    cache_ttl :: integer()  % milliseconds
}).

handle_call({validate, Input}, _From, State = #state{cache = Cache}) ->
    case maps:get(Input, Cache, undefined) of
        undefined ->
            Result = do_validate(Input),
            NewCache = maps:put(Input, Result, Cache),
            {reply, Result, State#state{cache = NewCache}};
        {ok, _} = Cached ->
            {reply, Cached, State};
        {error, _} = Cached ->
            %% Don't cache errors
            {reply, Cached, State}
    end.

handle_info(cache_clear, State) ->
    {noreply, State#state{cache = #{}}}.
```

---

## Troubleshooting

### Issue: Tests Not Found

**Symptom**:
```
===> Error Running EUnit Tests:
  Module `erlmcp_my_validator_tests' not found in project.
```

**Solution**:
1. Ensure test file is in `apps/erlmcp_validation/test/`
2. Test file must end with `_tests.erl`
3. Rebuild: `rebar3 compile`
4. Check rebar.config has proper test configuration

### Issue: gen_server Timeout

**Symptom**:
```
** exception exit: {timeout,
    {gen_server,call, [erlmcp_my_validator, {validate, Input}]}}
```

**Solution**:
1. Check handle_call/3 doesn't block
2. Increase timeout: `gen_server:call(Pid, Request, 10000)`
3. Use handle_cast/2 for async operations
4. Use handle_info/2 for blocking work

### Issue: Coverage <80%

**Symptom**:
```
Coverage: 75% (below 80% threshold)
```

**Solution**:
1. Find uncovered lines: `rebar3 cover -v`
2. Add tests for uncovered functions
3. Test edge cases
4. Test error paths

### Issue: Dialyzer Warnings

**Symptom**:
```
erlmcp_my_validator.erl:42: Invalid type specification
```

**Solution**:
1. Add -spec() attributes to all exported functions
2. Fix type mismatches
3. Use -dialyzer({nowarn_function, F/A}) for intentional violations
4. Run PLT build: `rebar3 dialyzer --build-plt`

### Issue: Xref Undefined Functions

**Symptom**:
```
Undefined function:erlmcp:unknown_function/0
```

**Solution**:
1. Remove dead code
2. Add function definition
3. Update .app.src modules list
4. Check typos in function names

---

## Best Practices

### 1. Type Specifications

```erlang
%% ALWAYS add -spec() to exported functions
-spec validate(pid() | atom(), map()) -> {ok, map()} | {error, term()}.

%% Use -type() for complex types
-type validation_result() :: {ok, map()} | {error, validation_error()}.
-type validation_error() :: {invalid_input, binary()} | {invalid_schema, term()}.
```

### 2. Error Handling

```erlang
%% GOOD: Let it crash (supervisor will restart)
handle_call({risky_operation, Data}, _From, State) ->
    Result = risky_operation(Data),  % May crash
    {reply, Result, State}.

%% BAD: Catch-all error handling (hides bugs)
handle_call({risky_operation, Data}, _From, State) ->
    try
        Result = risky_operation(Data),
        {reply, {ok, Result}, State}
    catch
        _:_ -> {reply, {error, unknown}, State}  % ❌ Hides crashes
    end.
```

### 3. Process Monitoring

```erlang
%% GOOD: Use monitor for external processes
handle_call({subscribe, ClientPid}, _From, State) ->
    Ref = monitor(process, ClientPid),
    NewClients = maps:put(ClientPid, Ref, State#state.clients),
    {reply, ok, State#state{clients = NewClients}}.

handle_info({'DOWN', Ref, process, Pid, _Reason}, State) ->
    %% Cleanup when client dies
    NewClients = maps:remove(Pid, State#state.clients),
    {noreply, State#state{clients = NewClients}}.

%% BAD: Use link (cascading failures)
handle_call({subscribe, ClientPid}, _From, State) ->
    link(ClientPid),  % ❌ Client crash kills this server
    NewClients = maps:put(ClientPid, undefined, State#state.clients),
    {reply, ok, State#state{clients = NewClients}}.
```

### 4. Message Size Limits

```erlang
%% GOOD: Check message size before processing
handle_cast({process_data, Data}, State) ->
    case byte_size(term_to_binary(Data)) of
        Size when Size > 16_777_216 ->  % 16MB
            {noreply, State};  % Reject
        _ ->
            Result = process_data(Data),
            {noreply, State}
    end.

%% BAD: No size check (memory exhaustion)
handle_cast({process_data, Data}, State) ->
    Result = process_data(Data),  % ❌ May OOM
    {noreply, State}.
```

### 5. Idempotent Operations

```erlang
%% GOOD: Can be called multiple times safely
init([]) ->
    case ets:info(my_table) of
        undefined -> ets:new(my_table, [named_table, public]);
        _ -> ok  % Already exists
    end,
    {ok, #state{}}.

%% BAD: Fails on second call
init([]) ->
    ets:new(my_table, [named_table, public]),  % ❌ Crashes if exists
    {ok, #state{}}.
```

---

## Resources

### Documentation
- [OTP Design Principles](https://www.erlang.org/doc/design_principles/des_princ.html)
- [gen_server Behavior](https://www.erlang.org/doc/man/gen_server.html)
- [EUnit User's Guide](https://www.erlang.org/doc/apps/eunit/chapter.html)
- [Common Test User's Guide](https://www.erlang.org/doc/apps/common_test/chapter.html)

### Internal Documentation
- `docs/validation/VALIDATION_COMPONENTS_STATUS.md`: Component inventory
- `docs/validation/ARCHITECTURE.md`: System architecture
- `docs/validation/QUALITY_GATES.md`: Quality gate reference
- `docs/validation/EVIDENCE_MODEL.md`: Evidence and receipts

### Tools
- `rebar3`: Build tool
- `dialyzer`: Static type analyzer
- `proper`: Property-based testing
- `cover`: Code coverage

---

## Summary

**Key Points**:
1. **Test First**: Chicago TDD (write tests before code)
2. **Real Processes**: No mocks, use real gen_server
3. **OTP Patterns**: gen_server, supervisor, let-it-crash
4. **Quality Gates**: All 8 gates must pass (blocking)
5. **Coverage**: ≥80% required (85% for core modules)
6. **Type Safety**: -spec() on all exported functions
7. **Evidence**: Track with SHA-256 hashes
8. **Documentation**: API docs + examples

**Workflow**: Test → Implement → Verify → Commit

**Success Criteria**:
- ✅ All tests pass (0 failures)
- ✅ Coverage ≥80%
- ✅ Dialyzer clean (0 warnings)
- ✅ Xref clean (0 undefined)
- ✅ All quality gates pass

**Status**: ✅ **PRODUCTION-READY**

---

**Generated**: 2026-02-01
**Tool**: erlmcp_validation v2.1.0
**OTP Version**: 28.3.1
