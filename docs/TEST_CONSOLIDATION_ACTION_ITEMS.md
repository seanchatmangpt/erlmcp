# Testing Consolidation - Action Items & Quick Wins

**Analysis:** TEST_STRATEGY_80_20_ANALYSIS.md
**Created:** 2026-01-30
**Focus:** Immediate high-ROI improvements

---

## Summary

Current testing shows **diminishing returns**: 16,008 lines across 90 test files with significant duplication. **80/20 consolidation** can reduce test maintenance by 40% while improving critical path coverage by 25%.

**Key Opportunities:**
1. **Rate limiting tests:** 1,316 lines → 450 lines (66% reduction)
2. **Property tests:** 16 → 42 properties (163% increase, replaces 120 unit tests)
3. **Flaky tests:** 212 → 42 (80% reduction)
4. **Execution time:** 165s → 60s (64% faster with parallelization)

---

## Phase 1: Quick Wins (1-2 weeks, 20% reduction)

### 1.1 Consolidate Rate Limiting Tests (1 day)

**Files affected:**
- `apps/erlmcp_core/test/erlmcp_rate_limiting_tests.erl` (657 lines)
- `apps/erlmcp_core/test/erlmcp_rate_limit_edge_case_tests.erl` (420 lines)
- `apps/erlmcp_core/test/erlmcp_rate_limit_middleware_tests.erl` (239 lines)

**Action:**
```erlang
%% 1. Create shared fixture
%% File: apps/erlmcp_core/test/fixtures/rate_limiter_fixture.erl
-module(rate_limiter_fixture).
-export([setup/0, setup/1, cleanup/1]).

setup() ->
    Config = #{
        max_messages_per_sec => 10,
        max_connections_per_sec => 5,
        global_max_messages_per_sec => 100,
        bucket_refill_interval_ms => 100,
        ddos_violation_threshold => 10,
        enabled => true
    },
    setup(Config).

setup(OverrideConfig) ->
    FinalConfig = maps:merge(default_config(), OverrideConfig),
    application:set_env(erlmcp, rate_limiting, FinalConfig),
    {ok, Pid} = erlmcp_rate_limiter:start_link(),
    {Pid, FinalConfig}.

cleanup({Pid, _Config}) ->
    erlmcp_rate_limiter:stop(),
    application:unset_env(erlmcp, rate_limiting).

default_config() ->
    #{
        max_messages_per_sec => 10,
        max_connections_per_sec => 5,
        global_max_messages_per_sec => 100,
        bucket_refill_interval_ms => 100,
        ddos_violation_threshold => 10,
        enabled => true
    }.

%% 2. Convert edge cases to Proper properties
%% In: erlmcp_rate_limiting_tests.erl
prop_token_bucket_no_overflow() ->
    ?FORALL({Tokens, RefillRate}, {proper_types:range(1, 100), proper_types:range(1, 100)},
        begin
            Bucket = {Tokens, erlang:system_time(millisecond)},
            {NewTokens, _} = erlmcp_rate_limiter:refill_bucket(Bucket, RefillRate),
            NewTokens =< Tokens + RefillRate
        end).

%% 3. Merge middleware tests into main suite
%% Add 3 integration test cases to erlmcp_rate_limiting_tests.erl

%% 4. Delete redundant files
%% rm: erlmcp_rate_limit_edge_case_tests.erl
%% rm: erlmcp_rate_limit_middleware_tests.erl
```

**Validation:**
```bash
# Before: Run all rate limit tests
rebar3 eunit --module=erlmcp_rate_limiting_tests
rebar3 eunit --module=erlmcp_rate_limit_edge_case_tests
rebar3 eunit --module=erlmcp_rate_limit_middleware_tests

# After: Run consolidated tests
rebar3 eunit --module=erlmcp_rate_limiting_tests

# Coverage must not decrease
rebar3 cover --verbose | grep erlmcp_rate_limiter
```

**Expected outcome:**
- Lines: 1,316 → 450 (66% reduction)
- Test files: 3 → 1
- Coverage: Maintain 85%+
- Property tests: 0 → 5

---

### 1.2 Extract Shared Fixtures (2 days)

**Duplication identified:**
```
Rate limiter setup: 3 files (90% identical)
Transport setup: 4 files (80% identical)
Registry setup: 5 files (85% identical)
Client/server setup: 6 files (75% identical)
```

**Action:**
```bash
# Create fixtures directory
mkdir -p apps/erlmcp_core/test/fixtures

# Create fixture modules
touch apps/erlmcp_core/test/fixtures/rate_limiter_fixture.erl
touch apps/erlmcp_core/test/fixtures/transport_fixture.erl
touch apps/erlmcp_core/test/fixtures/registry_fixture.erl
touch apps/erlmcp_core/test/fixtures/client_server_fixture.erl

# Update tests to use fixtures
# Example in erlmcp_rate_limiting_tests.erl:
-include("fixtures/rate_limiter_fixture.erl").

setup() ->
    rate_limiter_fixture:setup().

cleanup(Pid) ->
    rate_limiter_fixture:cleanup(Pid).
```

**Expected outcome:**
- Lines removed: 1,200 (duplicate setup/teardown)
- Fixtures created: 4 modules, ~200 lines total
- Maintainability: Single source of truth for test setup

---

### 1.3 Move Benchmarks Out of Test Suite (0.5 days)

**Problem:** Benchmarks mixed with tests, executed during eunit

**Files to move:**
```bash
# From: apps/erlmcp_core/test/
erlmcp_bench_cache.erl
erlmcp_bench_chaos.erl
erlmcp_bench_core_ops.erl
erlmcp_bench_helpers.erl
erlmcp_bench_integration.erl
erlmcp_bench_network_real.erl
erlmcp_bench_stress.erl

# To: apps/erlmcp_core/bench/
mkdir -p apps/erlmcp_core/bench
git mv apps/erlmcp_core/test/erlmcp_bench_*.erl apps/erlmcp_core/bench/
```

**Update rebar.config:**
```erlang
%% Add to aliases in rebar.config:
{alias, [
    {bench, [proper, " -m", "erlmcp_bench_*"]},
    {test, [eunit, {proper, "-c", "-m", "*_tests"}]}  %% Exclude benchmarks
]}.
```

**Validation:**
```bash
# Test execution should exclude benchmarks
rebar3 eunit  # Should run 45s, not 65s

# Benchmarks run separately
rebar3 bench
```

**Expected outcome:**
- Test execution time: 65s → 45s (31% faster)
- Clear separation: tests vs benchmarks

---

### 1.4 Convert JSON-RPC Symmetry Tests to Properties (1 day)

**Current:** 8 encode/decode symmetry tests in `erlmcp_json_rpc_tests.erl`

**Action:**
```erlang
%% Replace 8 tests with 2 properties in erlmcp_json_rpc_tests.erl

%% Property 1: Encode/decode roundtrip
prop_encode_decode_roundtrip() ->
    ?FORALL(Message, message_generator(),
        begin
            Encoded = erlmcp_json_rpc:encode(Message),
            {ok, Decoded} = erlmcp_json_rpc:decode(Encoded),
            Message =:= Decoded
        end).

%% Property 2: Decode invalid JSON returns error
prop_decode_invalid_json_error() ->
    ?FORALL(InvalidJson, invalid_json_generator(),
        begin
            Result = erlmcp_json_rpc:decode(InvalidJson),
            case Result of
                {error, _} -> true;
                _ -> false
            end
        end).

%% Helper generators
message_generator() ->
    proper_types:union([
        request_generator(),
        response_generator(),
        notification_generator()
    ]).

request_generator() ->
    ?LET({Id, Method, Params},
        {request_id_generator(), binary_generator(), proper_types:term()},
        #{
            <<"jsonrpc">> => <<"2.0">>,
            <<"id">> => Id,
            <<"method">> => Method,
            <<"params">> => Params
        }).
```

**Validation:**
```bash
# Run properties with 10,000 test cases
rebar3 proper -c --module=erlmcp_json_rpc_tests -n 10000

# Verify coverage maintained
rebar3 cover --verbose | grep erlmcp_json_rpc
```

**Expected outcome:**
- Tests removed: 8 (symmetry)
- Properties added: 2
- Coverage: Same (but more thorough via random generation)
- Lines: 600 reduction

---

## Phase 1 Validation Checklist

**Before merging Phase 1:**

- [ ] All rate limit tests pass (consolidated)
- [ ] Coverage unchanged or improved (run `rebar3 cover`)
- [ ] Test execution time decreased (measure with `time rebar3 eunit`)
- [ ] Benchmarks excluded from eunit (check output)
- [ ] Property tests pass (run `rebar3 proper -c`)
- [ ] CI/CD pipeline passes (full test suite)
- [ ] No new warnings (compile check)

**Metrics to track:**

```bash
# Measure before and after
echo "Test count:"
find apps -name "*_tests.erl" -exec grep -c "test()" {} \; | awk '{s+=$1} END {print s}'

echo "Test lines:"
find apps -name "*_tests.erl" -exec wc -l {} \; | awk '{s+=$1} END {print s}'

echo "Execution time:"
time rebar3 eunit

echo "Property tests:"
grep -r "prop_" apps/*/test/*.erl | wc -l
```

---

## Phase 2: Property Test Expansion (2-3 weeks, 30% additional reduction)

### Priority Matrix

| Module | Current Tests | Properties to Add | Lines to Remove | Priority |
|--------|--------------|-------------------|-----------------|----------|
| erlmcp_json_rpc | 52 | 8 | 600 | HIGH |
| erlmcp_client | 67 | 8 | 500 | HIGH |
| erlmcp_registry | 24 | 5 | 120 | HIGH |
| erlmcp_rate_limiter | 47 | 5 | 280 | MEDIUM |
| erlmcp_session_manager | 67 | 5 | 400 | MEDIUM |
| erlmcp_transport_tcp | 19 | 3 | 100 | LOW |

**Total:** 26 properties, 2,000 lines removed

---

### 2.1 Add Properties to erlmcp_client (1 week)

**Target:** 8 properties replacing ~40 unit tests

**Properties to implement:**

```erlang
%% 1. Request ID correlation
prop_request_id_correlation() ->
    ?FORALL(Requests, proper_types:list(request_generator()),
        begin
            {ok, Client} = erlmcp_client:start_link(#{}),
            Results = lists:map(fun(Req) ->
                erlmcp_client:send_request(Client, Req)
            end, Requests),
            erlmcp_client:stop(Client),
            %% Verify: All requests have unique IDs
            Ids = [Id || {ok, Id} <- Results],
            length(Ids) =:= length(lists:usort(Ids))
        end).

%% 2. Pending request map integrity
prop_pending_request_map_integrity() ->
    ?FORALL({Requests, Cancels},
        {proper_types:list(request_generator()), proper_types:list(proper_types:integer(1, 10))},
        begin
            {ok, Client} = erlmcp_client:start_link(#{}),
            %% Send requests
            Ids = lists:map(fun(Req) ->
                {ok, Id} = erlmcp_client:send_request(Client, Req),
                Id
            end, Requests),
            %% Cancel some
            lists:map(fun(N) ->
                case N =< length(Ids) of
                    true -> erlmcp_client:cancel_request(Client, lists:nth(N, Ids));
                    false -> ok
                end
            end, Cancels),
            %% Verify: Pending map consistent
            State = sys:get_state(Client),
            PendingMaps = element(4, State),  # Extract pending map
            erlmcp_client:stop(Client),
            is_map(PendingMaps)
        end).

%% 3. Response correlation
prop_response_correlation() ->
    ?FORALL(Request, request_generator(),
        begin
            {ok, Client} = erlmcp_client:start_link(#{}),
            {ok, ReqId} = erlmcp_client:send_request(Client, Request),
            %% Simulate response
            Response = #{<<"id">> => ReqId, <<"result">> => <<"ok">>},
            Client ! {transport_data, jsx:encode(Response)},
            %% Await response
            receive
                {response, ReqId, _} -> true
            after 1000 ->
                false
            end
        end).

%% 5 more properties...
```

---

### 2.2 Add Properties to erlmcp_registry (3 days)

**Target:** 5 properties replacing ~12 unit tests

**Properties:**

```erlang
%% 1. Registration uniqueness
prop_registration_unique_names() ->
    ?FORALL(Name, binary_generator(),
        begin
            Pid1 = spawn(fun() -> receive stop -> ok end end),
            Pid2 = spawn(fun() -> receive stop -> ok end end),
            ok = erlmcp_registry:register_name({test, Name}, Pid1),
            Result = erlmcp_registry:register_name({test, Name}, Pid2),
            Pid1 ! stop, Pid2 ! stop,
            Result =:= error  %% Second registration should fail
        end).

%% 2. Process death auto-unregister
prop_process_death_auto_unregister() ->
    ?FORALL(Name, binary_generator(),
        begin
            Pid = spawn(fun() -> receive stop -> ok end end),
            ok = erlmcp_registry:register_name({test, Name}, Pid),
            exit(Pid, kill),
            timer:sleep(50),  %% Allow cleanup
            {error, not_found} =:= erlmcp_registry:whereis_name({test, Name})
        end).

%% 3. Message routing correctness
prop_message_routing_correctness() ->
    ?FORALL(Message, proper_types:binary()),
        begin
            Receiver = spawn(fun() ->
                receive
                    {message, Msg} -> ?MODULE ! {received, Msg}
                end
            end),
            Name = <<"test_receiver">>,
            ok = erlmcp_registry:register_name({test, Name}, Receiver),
            ok = erlmcp_registry:send({test, Name}, {message, Message}),
            receive
                {received, Message} -> true
            after 500 ->
                false
            end
        end).

%% 2 more properties...
```

---

## Phase 3: Critical Path Coverage (2-3 weeks)

### 3.1 Transport Failure Cascade Tests (1 week)

**Gap:** Current tests don't cover transport → registry → server failure propagation

**Create:** `apps/erlmcp_core/test/erlmcp_transport_failure_SUITE.erl`

```erlang
-module(erlmcp_transport_failure_SUITE).
-include_lib("common_test/include/ct.hrl").

all() ->
    [
        test_transport_failure_registry_cleanup,
        test_transport_failure_server_notification,
        test_concurrent_transport_failures,
        test_transport_restart_session_recovery,
        test_network_partition_message_delivery
    ].

%% Test 1: Transport failure triggers registry cleanup
test_transport_failure_registry_cleanup(_Config) ->
    %% Start server and transport
    {ok, Server} = erlmcp_server:start_link(#{}),
    {ok, Transport} = erlmcp_transport_tcp:start_link(#{}),

    %% Register transport in registry
    ok = erlmcp_registry:register_name({transport, tcp}, Transport),

    %% Verify registration
    {ok, Transport} = erlmcp_registry:whereis_name({transport, tcp}),

    %% Kill transport
    exit(Transport, kill),
    timer:sleep(100),

    %% Verify registry cleanup
    {error, not_found} = erlmcp_registry:whereis_name({transport, tcp}),

    %% Cleanup
    erlmcp_server:stop(Server).

%% Test 2-5...
```

---

### 3.2 Registry Scaling Tests (1 week)

**Gap:** No tests for 10K+ concurrent registrations

**Create:** `apps/erlmcp_core/test/erlmcp_registry_scaling_SUITE.erl`

```erlang
-module(erlmcp_registry_scaling_SUITE).
-include_lib("common_test/include/ct.hrl").

all() ->
    [
        test_10k_concurrent_registrations,
        test_registry_performance_degradation,
        test_registry_overflow_behavior
    ].

%% Test 1: 10K concurrent registrations (property-based)
test_10k_concurrent_registrations(_Config) ->
    NumProcesses = 10000,
    Parent = self(),

    %% Spawn 10K processes that register simultaneously
    Pids = [spawn(fun() ->
        Name = {test, list_to_binary("proc_" ++ integer_to_list(N))},
        ok = erlmcp_registry:register_name(Name, self()),
        Parent ! {registered, N}
    end) || N <- lists:seq(1, NumProcesses)],

    %% Wait for all registrations
    lists:foreach(fun(_) ->
        receive
            {registered, _} -> ok
        end
    end, lists:seq(1, NumProcesses)),

    %% Verify all registered
    Results = [erlmcp_registry:whereis_name({test, list_to_binary("proc_" ++ integer_to_list(N))})
               || N <- lists:seq(1, NumProcesses)],
    RegisteredCount = length([ok || {ok, _} <- Results]),

    ?assertEqual(NumProcesses, RegisteredCount),

    %% Cleanup
    lists:foreach(fun(Pid) -> catch exit(Pid, kill) end, Pids).

%% Test 2-3...
```

---

## Phase 4: Test Infrastructure Optimization (1-2 weeks)

### 4.1 Eliminate Flaky Tests (1 week)

**Current:** 212 timing-dependent assertions (timer:sleep)

**Strategy:** Replace with synchronization primitives

```erlang
%% Anti-pattern (flaky):
test_concurrent_write() ->
    Pid1 = spawn(fun() -> write_data() end),
    Pid2 = spawn(fun() -> write_data() end),
    timer:sleep(50),  %% Flaky!
    ?assertEqual(2, read_count()).

%% Pattern (reliable):
test_concurrent_write() ->
    Parent = self(),
    Ref1 = monitor(process, spawn(fun() ->
        write_data(), Parent ! {done, self()}
    end)),
    Ref2 = monitor(process, spawn(fun() ->
        write_data(), Parent ! {done, self()}
    end)),
    receive {done, _} -> ok end,
    receive {done, _} -> ok end,
    ?assertEqual(2, read_count()).

%% Or use test helpers:
test_concurrent_write_with_helper() ->
    Pids = [spawn_monitor(fun() -> write_data() end),
             spawn_monitor(fun() -> write_data() end)],
    test_helpers:await_processes([Pid || {Pid, _} <- Pids]),
    ?assertEqual(2, read_count()).
```

**Action:**
1. Create `test/test_helpers.erl` with sync primitives
2. Audit all tests with `timer:sleep`
3. Replace with monitor/receive or helpers
4. Run tests 10x to verify stability

---

### 4.2 Parallelize Test Execution (0.5 days)

**Create:** `apps/erlmcp_core/test/test_SUITE.erl`

```erlang
-module(test_SUITE).

all() ->
    [
        {group, unit_tests},
        {group, integration_tests},
        {group, property_tests}
    ].

groups() ->
    [
        {unit_tests, [parallel, {duration, 30}], [
            erlmcp_client_tests,
            erlmcp_server_tests,
            erlmcp_registry_tests,
            erlmcp_json_rpc_tests,
            erlmcp_rate_limiting_tests,
            erlmcp_session_tests,
            erlmcp_tool_tests,
            erlmcp_resource_tests
        ]},
        {integration_tests, [parallel, {duration, 60}], [
            erlmcp_integration_SUITE,
            erlmcp_transport_integration_SUITE
        ]},
        {property_tests, [parallel, {duration, 30}], [
            erlmcp_json_rpc_tests,
            erlmcp_rate_limiting_tests
        ]}
    ].
```

**Validation:**
```bash
# Run parallel tests
rebar3 ct --suite=test_SUITE

# Measure improvement
time rebar3 ct  # Should be 60s vs 165s
```

---

## Success Metrics

### Phase 1 (Quick Wins)
- Lines removed: 2,400 (15%)
- Files reduced: 73 → 68
- Execution time: 165s → 140s

### Phase 2 (Properties)
- Lines removed: 4,800 (30% cumulative)
- Properties added: 26 (16 → 42)
- Unit tests removed: 120

### Phase 3 (Critical Paths)
- Lines added: 750 (focused)
- Critical coverage: 65% → 90%

### Phase 4 (Infrastructure)
- Flaky tests: 212 → 42 (80% reduction)
- Execution time: 140s → 60s (57% faster)

### Final State
- Total lines: 16,008 → 11,200 (30% reduction)
- Test files: 73 → 55 (25% reduction)
- Property tests: 16 → 42 (163% increase)
- Execution time: 165s → 60s (64% faster)

---

## Risk Mitigation

### Coverage Regression Prevention
```bash
# Set coverage floor in rebar.config
{cover_opts, [
    verbose,
    {coverage_options, [
        {level, 80}  %% Fail if coverage below 80%
    ]}
]}.
```

### Smoke Tests
Create quick sanity check after consolidation:
```bash
# scripts/smoke_test.sh
#!/bin/bash
rebar3 compile &&
rebar3 eunit --module=erlmcp_client_tests &&
rebar3 eunit --module=erlmcp_server_tests &&
rebar3 eunit --module=erlmcp_json_rpc_tests &&
echo "Smoke tests passed"
```

### Rollback Plan
- Each phase is separate PR
- Git revert if critical regressions
- Measure coverage before/after
- CI/CD gate: 80% minimum coverage

---

## Next Steps

1. **Review this analysis** with team
2. **Prioritize phases** based on risk tolerance
3. **Execute Phase 1** (Quick Wins) as proof of concept
4. **Measure and iterate** based on metrics
5. **Proceed to Phase 2-4** incrementally

**Total effort:** 6-8 weeks
**Projected impact:** 30% less test code, 25% better coverage, 64% faster execution

---

**Document Version:** 1.0
**Related:** TEST_STRATEGY_80_20_ANALYSIS.md
