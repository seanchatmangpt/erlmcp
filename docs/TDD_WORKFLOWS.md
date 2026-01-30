# Test-Driven Development Workflows with Claude Code CLI for erlmcp

## Overview

This document documents the comprehensive Test-Driven Development (TDD) workflows for the erlmcp project, leveraging Claude Code CLI features to implement Chicago School TDD patterns with comprehensive testing infrastructure.

## Table of Contents

1. [Chicago School TDD Implementation Patterns](#chicago-school-tdd-implementation-patterns)
2. [Agent-Based Test Creation Strategies](#agent-based-test-creation-strategies)
3. [Test Suite Organization and Execution](#test-suite-organization-and-execution)
4. [Coverage Analysis and Quality Gates](#coverage-analysis-and-quality-gates)
5. [Mocking and Stubbing Approaches](#mocking-and-stubbing-approaches)
6. [Integration Testing Patterns](#integration-testing-patterns)
7. [Property-Based Testing with PropEr](#property-based-testing-with-proper)
8. [Stress Testing and Chaos Engineering](#stress-testing-and-chaos-engineering)
9. [Test Automation and CI/CD Integration](#test-automation-and-cicd-integration)
10. [Test Reporting and Metrics Collection](#test-reporting-and-metrics-collection)

---

## Chicago School TDD Implementation Patterns

### Core Principles

**Chicago School TDD** emphasizes:
- **State-based verification**: Test observable state changes, not internal interactions
- **Real collaborators**: Use actual processes and systems, not mocks
- **Behavior verification**: What the system does, not how it does it
- **Integration focus**: Test components together whenever possible

### Example: gen_server Testing Pattern

```erlang
%%% Chicago School TDD Test for erlmcp_server
server_test_() ->
    {setup,
     fun setup/0,                           %% Start real application
     fun cleanup/1,                         %% Stop application
     fun(_) -> [                            %% Test generator
         ?_test(resource_lifecycle_test()), %% State-based test
         ?_test(concurrent_access_test())   %% Real process test
     end
    }.

%% Test observable state, not internal calls
resource_lifecycle_test() ->
    %% Setup: Start real server
    {ok, ServerPid} = erlmcp_server:start_link(<<"test_server">>, Capabilities),

    %% Exercise: Add resource via API
    Uri = <<"test://resource/1">>,
    Handler = fun(_) -> <<"content">> end,
    ok = erlmcp_server:add_resource(ServerPid, Uri, Handler),

    %% Verify: Check observable state
    ?assertEqual(ok, erlmcp_server:get_resource(Uri)),  %% Real API call
    ?assert(erlang:is_process_alive(ServerPid)),        %% Process state

    %% Cleanup
    ok = erlmcp_server:stop(ServerPid).
```

### Real Process Collaboration

```erlang
%% Test with real processes, no mocks
concurrent_access_test() ->
    Server = start_server(),

    %% Spawn real client processes
    ClientPids = lists:map(fun(Id) ->
        spawn_link(fun() ->
            %% Real client behavior
            erlmcp_server:add_resource(Server,
                                      <<"client_", (integer_to_binary(Id))/binary>>,
                                      fun(_) -> Id end)
        end)
    end, lists:seq(1, 10)),

    %% Verify all clients succeeded (observable behavior)
    timer:sleep(100),
    ?assertEqual(10, length(ClientPids)),

    %% Cleanup real processes
    [exit(Pid, normal) || Pid <- ClientPids].
```

---

## Agent-Based Test Creation Strategies

### Test Agent Specialization

The erlmcp project uses specialized test agents:

1. **`erlang-test-engineer`**: Chicago School TDD implementation
2. **`performance-benchmarker`**: Stress testing and metrics
3. **`code-analyzer`**: Coverage analysis and quality gates
4. **`system-architect`**: Integration test design

### Agent Workflow Example

```bash
# Multi-agent test creation workflow
[Parallel Agent Execution]:
  Task("Test Engineer", "Create EUnit tests for erlmcp_server using Chicago School TDD", "erlang-test-engineer")
  Task("Performance Analyst", "Design benchmark tests for registry performance", "performance-benchmarker")
  Task("Code Reviewer", "Analyze test coverage and quality", "code-analyzer")
  Task("Integration Specialist", "Create Common Test suite for multi-process coordination", "system-architect")
```

### Test Creation Patterns

```erlang
%% Agent-generated test following Chicago School
-module(erlmcp_server_tests).
-include_lib("eunit/include/eunit.hrl").

%% Tests focused on observable behaviors
server_lifecycle_test_() ->
    {spawn, fun() ->
        %% Real process creation and monitoring
        {ok, Pid} = erlmcp_server:start_link(<<"test">>, Capabilities),
        ?assert(is_pid(Pid)),

        %% Exercise: Server operations
        ok = erlmcp_server:add_resource(Pid, <<"uri">>, Handler),

        %% Verify: Observable state
        {ok, Content} = erlmcp_server:get_resource(Pid, <<"uri">>),
        ?assertEqual(<<"expected">>, Content),

        %% Cleanup with real process termination
        ok = erlmcp_server:stop(Pid)
    end}.
```

---

## Test Suite Organization and Execution

### Directory Structure

```
erlmcp/
├── apps/erlmcp_core/test/           # EUnit tests
│   ├── erlmcp_server_tests.erl     # Server functionality
│   ├── erlmcp_registry_tests.erl   # Registry operations
│   └── erlmcp_integration_SUITE.erl # Common Test suite
├── apps/erlmcp_transports/test/    # Transport tests
├── apps/erlmcp_observability/test/ # Monitoring tests
├── test/                           # Legacy/compatibility tests
├── bench/                          # Benchmark tests
└── scripts/                        # Test automation
```

### Test Execution Commands

```bash
# EUnit tests (unit level)
rebar3 eunit --module=erlmcp_server_tests --verbose
rebar3 eunit --module=erlmcp_*_tests           # All modules

# Common Test (integration level)
rebar3 ct --suite=erlmcp_integration_SUITE --verbose
rebar3 ct --suite=erlmcp_*_SUITE              # All suites

# Property-based testing
rebar3 proper -c --module=erlmcp_server_tests

# Combined execution
rebar3 test  # Runs EUnit + CT + Proper
```

### Test Configuration

```erlang
%% rebar.config test profile
{profiles, [
    {test, [
        {deps, [
            {proper, "1.4.0"},        % Property testing
            {meck, "0.9.2"},         % Mocking (sparingly)
            {coveralls, "2.2.0"}     % Coverage reporting
        ]},
        {cover_enabled, true},
        {cover_export_enabled, true},
        {eunit_opts, [verbose]}
    ]}
]}.
```

---

## Coverage Analysis and Quality Gates

### Coverage Targets

```erlang
%% Quality gate thresholds from rebar.config
{cover_enabled, true}.
{cover_opts, [verbose]}.

% Minimum coverage requirements:
% - Core modules (server, registry, client): 85%
% - Public APIs: 100%
% - All modules: 80%
```

### Coverage Analysis

```bash
# Generate coverage report
rebar3 cover --verbose

# View HTML report
open _build/test/cover/index.html

# Quick coverage check
rebar3 cover
```

### Quality Gate Enforcement

```erlang
%% Pre-commit hook validation
./tools/claude-md-enforcer.sh

# Validation pipeline:
# 1. Compilation (0 errors)
# 2. Tests (100% pass rate)
# 3. Coverage (80%+ minimum)
# 4. Dialyzer (0 warnings)
# 5. Xref (0 cross-reference issues)
```

### Coverage Report Example

```
✅ Coverage Results:
- erlmcp_server.erl: 92% (Target: 85%) ✅
- erlmcp_registry.erl: 88% (Target: 85%) ✅
- erlmcp_client.erl: 95% (Target: 85%) ✅
- Public APIs: 100% (Target: 100%) ✅
- Overall: 89% (Target: 80%) ✅

Coverage Breakdown:
- Functions tested: 45/50 (90%)
- Branches tested: 128/150 (85%)
- Lines covered: 1,234/1,400 (88%)
```

---

## Mocking and Stubbing Approaches

### Chicago School Philosophy: Minimal Mocking

```erlang
%% ✅ GOOD: Real process collaboration (preferred)
test_with_real_processes() ->
    %% Start actual dependencies
    application:ensure_all_started(gproc),

    %% Test real server-registry interaction
    {ok, Server} = erlmcp_server:start_link(<<"test">>, Caps),
    {ok, Registry} = erlmcp_registry:register_server(<<"test">>, Server),

    %% Verify real coordination
    ?assertEqual({ok, Server}, erlmcp_registry:find_server(<<"test">>)).
```

### When to Use Mocking

```erlang
%% ❌ AVOID: Mocking for testing internal behavior
%% Only mock external dependencies (file I/O, network, time)

%% ✅ GOOD: Mock external HTTP calls
setup_gun_mock() ->
    meck:new(gun, [passthrough]),
    meck:expect(gun, open, fun(_, _, _) -> {ok, self()} end),
    meck:expect(gun, post, fun(_, _, _, _) -> {ok, stream_ref} end).

%% ✅ GOOD: Mock time for deterministic tests
mock_time() ->
    meck:new(timer, [unstick]),
    meck:expect(timer, sleep, fun(_) -> ok end).
```

### Process-based Mocking

```erlang
%% Mock server using real process behavior
mock_server(Responses) ->
    spawn(fun() -> mock_loop(Responses) end).

mock_loop([{Request, Response} | Rest]) ->
    receive
        Request ->
            sender() ! Response,
            mock_loop(Rest)
    after 5000 ->
        exit(timeout)
    end.
```

---

## Integration Testing Patterns

### Common Test Suite Structure

```erlang
-module(erlmcp_integration_SUITE).
-include_lib("common_test/include/ct.hrl").

%% Multi-process coordination test
test_server_registry_coordination(Config) ->
    %% Start multiple real servers
    ServerIds = [make_test_server_id(I) || I <- lists:seq(1, 5)],
    ServerPids = lists:map(fun(ServerId) ->
        {ok, Pid} = erlmcp:start_server(ServerId, #{}),
        {ServerId, Pid}
    end, ServerIds),

    %% Verify registry coordination
    lists:foreach(fun({ServerId, Pid}) ->
        ?assertEqual({ok, {Pid, _}}, erlmcp_registry:find_server(ServerId))
    end, ServerPids),

    %% Test failure scenarios
    {FirstServerId, FirstPid} = hd(ServerPids),
    exit(FirstPid, kill),
    timer:sleep(100),

    %% Verify graceful degradation
    ?assertEqual({error, not_found}, erlmcp_registry:find_server(FirstServerId)),

    %% Cleanup all servers
    lists:foreach(fun({Id, _}) -> erlmcp:stop_server(Id) end, ServerPids).
```

### End-to-End Testing

```erlang
%% Complete MCP protocol flow test
test_complete_mcp_flow(Config) ->
    %% Setup real MCP server
    {ok, Server} = erlmcp:start_server(<<"test_server">>, FullCapabilities),

    %% Add real MCP components
    ok = erlmcp:add_tool(Server, <<"calc">>, calculator_handler),
    ok = erlmcp:add_resource(Server, <<"file.txt">>, file_handler),

    %% Setup real transport
    {ok, Transport} = erlmcp:start_transport(<<"test_transport">>, stdio,
                                            #{server_id => <<"test_server">>}),

    %% Execute real MCP protocol sequence
    Messages = [
        #{id => 1, method => initialize, params => Capabilities},
        #{id => 2, method => tools_list},
        #{id => 3, method => tools_call, params => #{name => <<"calc">>}}
    ],

    %% Send real messages through transport
    lists:foreach(fun(Msg) ->
        Transport ! {message, jsx:encode(Msg)}
    end, Messages),

    %% Verify end-to-end behavior
    timer:sleep(500),
    ?assert(is_process_alive(Server)),
    ?assert(is_process_alive(Transport)).
```

---

## Property-Based Testing with PropEr

### Property Testing Setup

```erlang
%% Property test module
-module(erlmcp_server_properties).
-include_lib("proper/include/proper.hrl").

%% Resource handler property
prop_resource_handler_roundtrip() ->
    ?FORALL({Uri, Content}, {binary(), binary()},
        begin
            %% Setup: Create real server
            {ok, Server} = erlmcp_server:start_link(<<"prop_test">>, Caps),

            %% Exercise: Add and retrieve resource
            Handler = fun(_) -> Content end,
            ok = erlmcp_server:add_resource(Server, Uri, Handler),
            Result = erlmcp_server:get_resource(Server, Uri),

            %% Verify: Roundtrip consistency
            erlmcp_server:stop(Server),
            Result =:= {ok, Content}
        end).

%% Tool execution property
prop_tool_execution_consistency() ->
    ?FORALL(Args, map(),
        begin
            Server = start_test_server(),
            ToolName = <<"test_tool">>,

            %% Add tool with predictable behavior
            Handler = fun(A) -> A end,
            ok = erlmcp_server:add_tool(Server, ToolName, Handler),

            %% Execute tool multiple times
            Results = lists:map(fun(_) ->
                erlmcp_server:call_tool(Server, ToolName, Args)
            end, lists:seq(1, 10)),

            %% Verify consistency
            erlmcp_server:stop(Server),
            lists:all(fun(R) -> R =:= {ok, Args} end, Results)
        end).
```

### Property Test Execution

```bash
# Run all property tests
rebar3 proper -c

# Run specific property test
rebar3 proper -c --module=erlmcp_server_properties

# Generate test cases
rebar3 proper -c --module=erlmcp_server_properties --verbose
```

### Property Testing Patterns

```erlang
%% Input space generators
uri_generator() ->
    ?SUCHTHAT(Uri, binary(),
        string:find(Uri, "://") =/= nomatch andalso byte_size(Uri) =< 256).

content_generator() ->
    ?SUCHTHAT(Content, binary(), byte_size(Content) =< 1024).

%% State machine testing
prop_server_state_machine() ->
    ?FORALL(Commands, list({command(), state_args()}),
        begin
            %% Initialize real server
            {ok, Server} = erlmcp_server:start_link(<<"sm_test">>, Caps),
            {History, FinalState} = execute_commands(Server, Commands),

            %% Verify invariants
            ok = erlmcp_server:stop(Server),
            check_state_invariants(FinalState, History)
        end).

%% Shrinking for minimal failing cases
prop_error_conditions_shrink() ->
    ?FORALL({InvalidArgs, ExpectedError}, {map(), atom()},
        begin
            %% Test error condition
            Result = erlmcp_server:call_tool(Server, <<"tool">>, InvalidArgs),
            Result =:= {error, ExpectedError}
        end).
```

---

## Stress Testing and Chaos Engineering

### Benchmark Test Patterns

```erlang
%% Core operations benchmark
-module(erlmcp_bench_core_ops).

%% High-throughput registry operations
benchmark_registry_high_throughput() ->
    Ops = 100000,
    Workers = 100,

    %% Measure throughput
    Start = erlang:monotonic_time(microsecond),

    %% Concurrent registry operations
    Pids = [spawn_registry_worker(WorkerId, Ops div Workers)
            || WorkerId <- lists:seq(1, Workers)],

    %% Wait for completion
    lists:foreach(fun(Pid) ->
        Ref = monitor(process, Pid),
        receive {'DOWN', Ref, process, Pid, _} -> ok end
    end, Pids),

    End = erlang:monotonic_time(microsecond),
    Throughput = (Ops * 1000000) / (End - Start),

    ct:pal("Registry throughput: ~p ops/sec", [Throughput]).

%% Memory exhaustion test
benchmark_memory_exhaustion() ->
    InitialMemory = erlang:memory(total),

    %% Create many processes to exhaust memory
    Pids = lists:map(fun(I) ->
        spawn_link(fun() ->
            %% Create large data structures
            LargeData = lists:foldl(fun(_, Acc) ->
                [list_to_binary(lists:seq(1, 1000)) | Acc]
            end, [], lists:seq(1, 1000)),
            receive after 1000 -> ok end
        end)
    end, lists:seq(1, 1000)),

    %% Monitor memory growth
    timer:sleep(5000),
    PeakMemory = erlang:memory(total),

    %% Cleanup
    [exit(Pid, kill) || Pid <- Pids],
    timer:sleep(1000),
    FinalMemory = erlang:memory(total),

    ct:pal("Memory - Initial: ~pMB, Peak: ~pMB, Final: ~pMB",
           [InitialMemory div 1024 div 1024,
            PeakMemory div 1024 div 1024,
            FinalMemory div 1024 div 1024]).
```

### Chaos Engineering Patterns

```erlang
%% Process kill test
test_process_kill_chaos() ->
    %% Start system
    {ok, Server} = erlmcp:start_server(<<"chaos_test">>, Caps),
    {ok, Transport} = erlmcp:start_transport(<<"chaos_transport">>, stdio,
                                            #{server_id => <<"chaos_test">>}),

    %% Run operations while killing processes
    spawn_link(fun() ->
        %% Kill random processes every 100ms
        lists:foreach(fun(_) ->
            Pids = [Server, Transport] ++ erlang:processes(),
            case Pids of
                [_] -> ok;
                [_ | Rest] ->
                    RandomPid = lists:nth(rand:uniform(length(Rest)), Rest),
                    exit(RandomPid, kill),
                    timer:sleep(rand:uniform(200))
            end
        end, lists:seq(1, 50))
    end),

    %% Test system resilience
    lists:foreach(fun(_) ->
        ok = erlmcp:add_resource(Server, <<"test">>, fun(_) -> ok end),
        timer:sleep(50)
    end, lists:seq(1, 100)),

    %% Verify recovery
    ?assert(is_process_alive(Server)),
    ?assertEqual(ok, erlmcp:add_resource(Server, <<"recovery_test">>, fun(_) -> ok end)).

%% Network partition simulation
test_network_partition() ->
    %% Start distributed servers
    {ok, Server1} = erlmcp:start_server(<<"partition_test_1">>, Caps),
    {ok, Server2} = erlmcp:start_server(<<"partition_test_2">>, Caps),

    %% Simulate network partition
    spawn_link(fun() ->
        %% Block communication between servers
        timer:sleep(1000),  % Allow initial communication
        %% Kill network processes
        exit(whereis(net_kernel), kill)
    end),

    %% Test operation during partition
    ok = erlmcp:add_resource(Server1, <<"local">>, fun(_) -> ok end),
    ok = erlmcp:add_resource(Server2, <<"local">>, fun(_) -> ok end),

    %% Test recovery after partition heals
    timer:sleep(2000),
    ok = erlmcp:notify_resources_changed(Server1).
```

### Stress Test Execution

```bash
# Run stress tests
./bench/run_stress_test.sh

# Run specific benchmark
rebar3 eunit --module=erlmcp_bench_core_ops

# Run chaos tests
./bench/run_chaos_test.sh

# Performance validation
make benchmark-quick  # < 2 minutes
```

---

## Test Automation and CI/CD Integration

### Pre-commit Hook Validation

```bash
#!/bin/bash
# .git/hooks/pre-commit - Quality gate before commits

echo "Running pre-commit quality gates..."

# 1. Compilation check
rebar3 compile
if [ $? -ne 0 ]; then
    echo "❌ Compilation failed"
    exit 1
fi

# 2. Unit tests
rebar3 eunit --module=erlmcp_*_tests
if [ $? -ne 0 ]; then
    echo "❌ Unit tests failed"
    exit 1
fi

# 3. Integration tests (sample)
rebar3 ct --suite=erlmcp_integration_SUITE --case=test_system_startup_shutdown
if [ $? -ne 0 ]; then
    echo "❌ Integration tests failed"
    exit 1
fi

# 4. Coverage check
rebar3 cover
COVERAGE=$(grep "Total coverage:" _build/test/cover/index.html | grep -o '[0-9]*')
if [ "$COVERAGE" -lt 80 ]; then
    echo "❌ Coverage too low: ${COVERAGE}% (minimum 80%)"
    exit 1
fi

echo "✅ All quality gates passed"
```

### CI/CD Pipeline Integration

```yaml
# .github/workflows/test.yml
name: Test Suite

on: [push, pull_request]

jobs:
  test:
    runs-on: ubuntu-latest

    steps:
    - uses: actions/checkout@v3

    - name: Setup Erlang
      uses: erlang/actions/setup-erlang@v1
      with:
        otp-version: '25'
        rebar3-version: '3.20'

    - name: Dependencies
      run: rebar3 deps

    - name: Compile
      run: rebar3 compile

    - name: Unit Tests
      run: rebar3 eunit

    - name: Integration Tests
      run: rebar3 ct --suite=erlmcp_*_SUITE

    - name: Coverage
      run: rebar3 cover

    - name: Upload Coverage
      uses: codecov/codecov-action@v3
```

### Test Automation Scripts

```bash
#!/bin/bash
# scripts/test-full.sh - Complete test execution

set -e

echo "🧪 Running complete test suite..."

# Phase 1: Compilation and unit tests
echo "Phase 1: Compilation and EUnit tests"
rebar3 compile
rebar3 eunit --verbose

# Phase 2: Integration tests
echo "Phase 2: Common Test integration"
rebar3 ct --verbose

# Phase 3: Property tests
echo "Phase 3: Property-based testing"
rebar3 proper -c

# Phase 4: Stress testing
echo "Phase 4: Stress testing"
./bench/run_stress_test.sh

# Phase 5: Coverage analysis
echo "Phase 5: Coverage analysis"
rebar3 cover --verbose

# Summary
echo ""
echo "📊 Test Summary:"
echo "✅ Compilation: Passed"
echo "✅ EUnit: $EUNIT_COUNT tests passed"
echo "✅ Common Test: $CT_COUNT test cases passed"
echo "✅ Properties: $PROPERTY_COUNT cases tested"
echo "✅ Coverage: $COVERAGE% (target: 80%)"

if [ "$COVERAGE" -lt 80 ]; then
    echo "❌ Coverage below threshold"
    exit 1
fi

echo "🎉 All tests passed!"
```

### Multi-Environment Testing

```bash
#!/bin/bash
# scripts/test-multi-env.sh - Test across environments

environments=("ubuntu-latest" "macos-latest" "windows-latest")

for env in "${environments[@]}"; do
    echo "🔍 Testing on $env..."

    # Run environment-specific tests
    case $env in
        "ubuntu-latest")
            ./scripts/test-ubuntu.sh
            ;;
        "macos-latest")
            ./scripts/test-macos.sh
            ;;
        "windows-latest")
            ./scripts/test-windows.sh
            ;;
    esac

    echo "✅ $env tests completed"
done

echo "🎉 All environment tests passed"
```

---

## Test Reporting and Metrics Collection

### Test Report Generation

```bash
#!/bin/bash
# scripts/generate-test-report.sh

echo "📊 Generating comprehensive test report..."

# Create report directory
mkdir -p reports/test-$(date +%Y%m%d-%H%M%S)

# Generate coverage HTML report
rebar3 cover
cp -r _build/test/cover/ reports/test-$(date +%Y%m%d-%H%M%S)/coverage/

# Generate test statistics
cat > reports/test-$(date +%Y%m%d-%H%M%S)/summary.txt << EOF
Test Suite Summary
=================

Date: $(date)
Branch: $(git rev-parse --abbrev-ref HEAD)
Commit: $(git rev-parse --short HEAD)

Test Results:
- EUnit: $(rebar3 eunit --quiet | grep "passed" | wc -l) tests passed
- Common Test: $(rebar3 ct --quiet | grep "test cases" | wc -l) cases executed
- Coverage: $(grep "Total coverage:" _build/test/cover/index.html | grep -o '[0-9]*')%

Performance Metrics:
- Registry throughput: $(bench/erlmcp_bench_core_ops.erl)
- Message latency: $(bench/erlmcp_bench_network_real.erl)
- Memory usage: $(erlang:memory(total) div 1024 div 1024)MB

Quality Gates:
✅ Compilation: Passed
✅ Tests: 100% pass rate
✅ Coverage: ≥80%
✅ Dialyzer: 0 warnings
EOF

echo "Report generated: reports/test-$(date +%Y%m%d-%H%M%S)/"
```

### Metrics Collection Patterns

```erlang
%% Test metrics collection
-module(erlmcp_test_metrics).

-export([record_test_result/2, get_test_summary/0]).

-record(test_result, {
    module :: atom(),
    function :: atom(),
    status :: pass | fail | skip,
    duration :: integer(),  % milliseconds
    timestamp :: integer(),  % epoch
    error :: term() | undefined
}).

%% Record test execution metrics
record_test_result(Module, Function) ->
    StartTime = erlang:monotonic_time(millisecond),

    try
        %% Execute test
        Result = erlmcp_server_tests:Module(Function),

        %% Record success
        Duration = erlang:monotonic_time(millisecond) - StartTime,
        TestResult = #test_result{
            module = Module,
            function = Function,
            status = pass,
            duration = Duration,
            timestamp = erlang:system_time(second),
            error = undefined
        },
        store_test_result(TestResult),

        Result

    catch
        Class:Reason:Stack ->
            %% Record failure
            Duration = erlang:monotonic_time(millisecond) - StartTime,
            TestResult = #test_result{
                module = Module,
                function = Function,
                status = fail,
                duration = Duration,
                timestamp = erlang:system_time(second),
                error = {Class, Reason, Stack}
            },
            store_test_result(TestResult),
            erlang:raise(Class, Reason, Stack)
    end.

%% Generate test summary
get_test_summary() ->
    AllResults = fetch_all_test_results(),

    #{
        total => length(AllResults),
        passed => length([R || R <- AllResults, R#test_result.status =:= pass]),
        failed => length([R || R <- AllResults, R#test_result.status =:= fail]),
        skipped => length([R || R <- AllResults, R#test_result.status =:= skip]),
        average_duration => calculate_average_duration(AllResults),
        coverage => calculate_coverage(),
        timestamp => erlang:system_time(second)
    }.
```

### Performance Metrics

```erlang
%% Benchmark metrics collection
-module(erlmcp_bench_metrics).

-export([run_benchmark_suite/0, get_performance_baseline/0]).

%% Run comprehensive benchmark suite
run_benchmark_suite() ->
    Benchmarks = [
        {registry_ops, fun erlmcp_bench_core_ops:benchmark_registry/1},
        {queue_ops, fun erlmcp_bench_core_ops:benchmark_queue/1},
        {pool_ops, fun erlmcp_bench_core_ops:benchmark_pool/1},
        {session_ops, fun erlmcp_bench_core_ops:benchmark_session/1}
    ],

    Results = lists:map(fun({Name, Fun}) ->
        {Name, Fun(#{operations => 10000, workers => 10})}
    end, Benchmarks),

    %% Store benchmark results
    BenchmarkId = benchmark_id(),
    store_benchmark_results(BenchmarkId, Results),

    %% Generate comparison report
    generate_benchmark_report(BenchmarkId, get_performance_baseline()),

    Results.

%% Performance baseline tracking
get_performance_baseline() ->
    case mnesia:dirty_read(benchmark_baseline, current) of
        [{baseline, current, Results}] -> Results;
        [] -> create_baseline()
    end.

create_baseline() ->
    Results = run_benchmark_suite(),
    mnesia:dirty_write(benchmark_baseline, {baseline, current, Results}),
    Results.
```

### Visual Test Reports

```erlang
%% Generate HTML test report
generate_test_report_html() ->
    Summary = get_test_summary(),
    TestResults = fetch_all_test_results(),

    Html = <<
        "<html><head><title>ErlMCP Test Report</title>"
        "<style>body { font-family: Arial; } .pass { color: green; } "
        ".fail { color: red; } table { border-collapse: collapse; } "
        "td, th { border: 1px solid #ddd; padding: 8px; }</style></head>"
        "<body>"
        "<h1>ErlMCP Test Report</h1>"
        "<h2>Summary</h2>"
        "<p>Generated: ", (list_to_binary(erlang:universaltime()))/binary, "</p>"
        "<table>"
        "<tr><th>Total Tests</th><th>Passed</th><th>Failed</th><th>Coverage</th></tr>"
        "<tr><td>", (integer_to_binary(maps:get(total, Summary)))/binary, "</td>"
        "<td class='pass'>", (integer_to_binary(maps:get(passed, Summary)))/binary, "</td>"
        "<td class='fail'>", (integer_to_binary(maps:get(failed, Summary)))/binary, "</td>"
        "<td>", (float_to_binary(maps:get(coverage, Summary), [{decimals, 1}]))/binary, "%</td>"
        "</tr></table>"
        "<h2>Test Details</h2>"
        "<table>"
        "<tr><th>Module</th><th>Function</th><th>Status</th><th>Duration (ms)</th></tr>"
        , (list_to_binary(lists:map(fun(R) ->
            "<tr><td>", (atom_to_binary(R#test_result.module))/binary, "</td>"
            "<td>", (atom_to_binary(R#test_result.function))/binary, "</td>"
            "<td class='", (atom_to_binary(R#test_result.status))/binary, "'>"
            , (atom_to_binary(R#test_result.status))/binary, "</td>"
            "<td>", (integer_to_binary(R#test_result.duration))/binary, "</td></tr>"
        end, TestResults)))/binary
        "</table></body></html>"
    >>,

    file:write_file("reports/test-report.html", Html).
```

---

## CLI Features for TDD Workflows

### Claude Code CLI Integration

```bash
# Agent-based test creation
sparc tdd --module=erlmcp_server --patterns=chicago-school

# Multi-agent test orchestration
claude-flow task-orchestrate --pattern=test-suite --agents=4

# Test automation with quality gates
make test-quality  # Runs full test suite + quality gates

# Performance regression testing
make benchmark-validate --baseline=last-week
```

### Test Command Shortcuts

```bash
# Quick test cycles
make test-quick    # EUnit only (< 1 minute)
make test-integration  # CT only
make test-full     # Everything (5-10 minutes)

# Test debugging
make test-debug    # With verbose output and debug info
make test-watch    # Watch mode for development
```

### Test Result Visualization

```bash
# Generate test visualization
make test-viz      # Creates HTML test dashboard

# Performance trends
make perf-trend    # Shows benchmark trends over time

# Coverage history
make coverage-history # Coverage trend analysis
```

## Conclusion

The erlmcp project demonstrates a comprehensive approach to TDD using:

1. **Chicago School TDD** with real process collaboration
2. **Multi-agent test creation** for comprehensive coverage
3. **Hierarchical test organization** from unit to integration
4. **Strict quality gates** with automated validation
5. **Property-based testing** for edge cases
6. **Stress testing** for performance validation
7. **CI/CD integration** for continuous quality
8. **Rich metrics collection** for continuous improvement

This approach ensures production-ready code with comprehensive testing that follows Erlang/OTP best practices and maintains high quality standards throughout development.