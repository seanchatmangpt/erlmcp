# MCP Specification Testing Strategy v2.1.0

## Executive Summary

**Purpose**: Design comprehensive testing strategy for full MCP specification compliance in erlmcp v2.1.0, following Chicago School TDD principles with real processes, no mocks, and state-based verification.

**Status**: Initial Design - 2026-02-01
**Target Coverage**: 85%+ for core modules, 80%+ overall
**Test Count**: 269 EUnit, 33 CT suites (existing) → Target: 400+ total
**MCP Spec Version**: 2025-11-25
**Methodology**: Chicago School TDD (real collaborators, state-based assertions, behavior verification)

---

## Table of Contents

1. [Test Taxonomy](#test-taxonomy)
2. [MCP Feature Test Matrix](#mcp-feature-test-matrix)
3. [Test Organization](#test-organization)
4. [Chicago School TDD Principles](#chicago-school-tdd-principles)
5. [Implementation Roadmap](#implementation-roadmap)
6. [Claude-Flow Agent Integration](#claude-flow-agent-integration)
7. [Quality Gates](#quality-gates)
8. [Performance Baselines](#performance-baselines)
9. [Chaos Engineering](#chaos-engineering)
10. [Test Infrastructure](#test-infrastructure)

---

## Test Taxonomy

### 1. EUnit Tests (Unit Tests)

**Purpose**: Test individual modules in isolation with real process dependencies.

**Scope**:
- Protocol encoding/decoding (JSON-RPC, MCP messages)
- Individual gen_server callbacks (init/1, handle_call/3, handle_cast/2)
- Pure functions (validation, formatting, parsing)
- State transitions (client FSM, server FSM, session FSM)
- Error handling (invalid inputs, boundary conditions)

**Chicago School Principles**:
- ✅ Spawn real gen_servers (no mocks)
- ✅ Assert on observable state via API calls
- ✅ Verify behavior (outputs), not implementation (internal calls)
- ❌ No mock objects, no interaction verification

**Example Structure**:
```erlang
-module(erlmcp_resources_tests).
-include_lib("eunit/include/eunit.hrl").

%% Test fixture - spawn real server
resources_test_() ->
    {setup,
     fun() ->
         %% Setup: Start real erlmcp_server
         {ok, Server} = erlmcp_server:start_link(test_server, #{
             resources => #{enabled => true}
         }),
         Server
     end,
     fun(Server) ->
         %% Teardown: Stop server gracefully
         erlmcp_server:stop(Server)
     end,
     fun(Server) ->
         [
          ?_test(add_static_resource(Server)),
          ?_test(add_resource_template(Server)),
          ?_test(list_resources(Server)),
          ?_test(read_resource(Server)),
          ?_test(subscribe_to_resource(Server)),
          ?_test(notify_resource_updated(Server))
         ]
     end}.

%% State-based verification (Chicago School)
add_static_resource(Server) ->
    %% Exercise: Add resource via API
    ok = erlmcp_server:add_resource(Server, <<"doc://readme">>,
        fun(_Uri) -> <<"README content">> end),

    %% Verify: List resources returns added resource (observable state)
    {ok, Resources} = erlmcp_server:list_resources(Server),
    ?assert(lists:any(fun(R) ->
        maps:get(uri, R) =:= <<"doc://readme">>
    end, Resources)).
```

**Coverage Targets**:
- Core modules (server, client, registry): 90%+
- Protocol modules (JSON-RPC, message parsing): 95%+
- Utility modules: 80%+

**Existing**: 269 EUnit test files
**Gap Analysis**: Need tests for new MCP features (sampling, refusal, elicitation)

---

### 2. Common Test (Integration Tests)

**Purpose**: Test multi-process interactions and cross-application workflows with real OTP supervision.

**Scope**:
- Client-Server interactions (full request/response cycle)
- Transport integration (STDIO, TCP, HTTP, WebSocket, SSE)
- Multi-client scenarios (concurrent connections, subscriptions)
- Supervision tree behavior (process crashes, restarts)
- Distributed scenarios (multi-node, registry coordination)
- End-to-end workflows (initialize → tools/call → notifications)

**Chicago School Principles**:
- ✅ Start real OTP applications (erlmcp_core, erlmcp_transports)
- ✅ Spawn multiple real clients/servers
- ✅ Test actual message passing (no mocked transports)
- ✅ Verify system behavior (message delivery, state consistency)

**Example Structure**:
```erlang
-module(erlmcp_client_server_SUITE).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").

all() ->
    [
     initialize_handshake_test,
     tools_call_lifecycle_test,
     resource_subscription_test,
     concurrent_clients_test,
     server_restart_recovery_test
    ].

init_per_suite(Config) ->
    %% Start real OTP applications
    application:ensure_all_started(erlmcp_core),
    application:ensure_all_started(erlmcp_transports),
    Config.

end_per_suite(_Config) ->
    application:stop(erlmcp_transports),
    application:stop(erlmcp_core),
    ok.

%% Full client-server interaction (Chicago School)
initialize_handshake_test(_Config) ->
    %% Setup: Start real server
    {ok, Server} = erlmcp_server:start_link(test_server, #{
        resources => #{enabled => true},
        tools => #{enabled => true},
        prompts => #{enabled => true}
    }),

    %% Setup: Start real client with STDIO transport
    {ok, Client} = erlmcp_client:start_link(#{
        transport => {erlmcp_transport_stdio, #{}}
    }),

    %% Exercise: Perform initialize handshake
    InitRequest = #{
        protocolVersion => <<"2024-11-05">>,
        capabilities => #{
            roots => #{enabled => true},
            sampling => #{enabled => true}
        }
    },

    {ok, InitResponse} = erlmcp_client:initialize(Client, InitRequest),

    %% Verify: Server capabilities returned (state-based verification)
    ?assertMatch(#{
        protocolVersion := <<"2024-11-05">>,
        capabilities := #{
            resources := #{enabled := true},
            tools := #{enabled := true},
            prompts := #{enabled := true}
        }
    }, InitResponse),

    %% Verify: Server state transitioned to initialized (observable behavior)
    {ok, ServerInfo} = erlmcp_server:get_info(Server),
    ?assertEqual(initialized, maps:get(state, ServerInfo)),

    %% Cleanup
    erlmcp_client:stop(Client),
    erlmcp_server:stop(Server).
```

**Coverage Targets**:
- Client-Server interactions: 90%+
- Transport integration: 85%+
- Multi-process scenarios: 80%+

**Existing**: 33 Common Test suites
**Gap Analysis**: Need cross-transport tests, multi-client stress tests

---

### 3. Property-Based Tests (Proper)

**Purpose**: Test invariants, protocol properties, and state machines with generated inputs.

**Scope**:
- JSON-RPC encoding/decoding roundtrips
- MCP protocol message validation
- Request-ID correlation properties
- Resource subscription state machine
- Client FSM properties (state transitions)
- Server FSM properties (lifecycle)
- Concurrency properties (race conditions, ordering)

**Chicago School Principles**:
- ✅ Generate valid MCP messages
- ✅ Test with real gen_servers
- ✅ Verify observable properties (encoding = decoding⁻¹)

**Example Structure**:
```erlang
-module(erlmcp_json_rpc_proper_tests).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

%% Property: JSON-RPC encoding roundtrip
prop_json_rpc_roundtrip_test() ->
    ?assert(proper:quickcheck(prop_json_rpc_roundtrip(), [{numtests, 1000}])).

prop_json_rpc_roundtrip() ->
    ?FORALL(Message, json_rpc_message(),
        begin
            %% Exercise: Encode then decode
            Encoded = erlmcp_json_rpc:encode(Message),
            {ok, Decoded} = erlmcp_json_rpc:decode(Encoded),

            %% Verify: Roundtrip preserves message (invariant)
            Decoded =:= Message
        end).

%% Property: Request-ID correlation
prop_request_id_correlation() ->
    ?FORALL({RequestIds, Responses}, {list(pos_integer()), list(response())},
        begin
            %% Setup: Start real client
            {ok, Client} = erlmcp_client:start_link(#{transport => test}),

            %% Exercise: Send multiple requests with IDs
            lists:foreach(fun(Id) ->
                erlmcp_client:send_request(Client, #{
                    id => Id,
                    method => <<"test">>,
                    params => #{}
                })
            end, RequestIds),

            %% Simulate responses
            lists:foreach(fun({Id, Response}) ->
                erlmcp_client ! {response, Id, Response}
            end, lists:zip(RequestIds, Responses)),

            %% Verify: All responses correlated correctly (state property)
            {ok, Pending} = erlmcp_client:get_pending_requests(Client),
            length(Pending) =:= 0
        end).

%% Generators
json_rpc_message() ->
    oneof([json_rpc_request(), json_rpc_response(), json_rpc_notification()]).

json_rpc_request() ->
    ?LET({Id, Method, Params}, {pos_integer(), method(), params()},
        #{
            jsonrpc => <<"2.0">>,
            id => Id,
            method => Method,
            params => Params
        }).
```

**Coverage Targets**:
- Protocol invariants: 100+ properties
- State machines: 50+ properties
- Concurrency: 30+ properties

**Existing**: ~10 Proper test files
**Gap Analysis**: Need properties for all MCP features (tools, prompts, sampling)

---

### 4. Specification Compliance Tests

**Purpose**: Verify erlmcp conforms to MCP specification 2025-11-25.

**Scope**:
- Protocol version negotiation
- Required vs optional capabilities
- Method signatures (parameters, responses)
- Error codes (JSON-RPC standard + MCP custom)
- Notification formats
- Resource URI formats
- Tool schema validation
- Prompt argument validation
- Transport requirements (STDIO, HTTP)

**Chicago School Principles**:
- ✅ Test against real MCP specification JSON schema
- ✅ Use erlmcp_spec_parser to extract requirements
- ✅ Generate compliance report (HTML/JSON)

**Example Structure**:
```erlang
-module(erlmcp_spec_compliance_SUITE).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").

all() ->
    [
     initialize_method_compliance_test,
     tools_call_compliance_test,
     resources_read_compliance_test,
     error_codes_compliance_test,
     notification_formats_compliance_test
    ].

%% Test initialize method against spec
initialize_method_compliance_test(_Config) ->
    %% Parse MCP spec
    {ok, Spec} = erlmcp_spec_parser:load_spec("specs/mcp-2025-11-25.json"),
    InitializeSpec = erlmcp_spec_parser:get_method(Spec, <<"initialize">>),

    %% Setup: Start real server
    {ok, Server} = erlmcp_server:start_link(test_server, #{}),

    %% Exercise: Send initialize request
    InitRequest = #{
        protocolVersion => <<"2024-11-05">>,
        capabilities => #{
            roots => #{enabled => true}
        }
    },

    {ok, InitResponse} = erlmcp_server:handle_method(Server,
        <<"initialize">>, InitRequest),

    %% Verify: Response matches spec schema
    ok = erlmcp_spec_parser:validate_response(InitializeSpec, InitResponse),

    %% Verify: Required fields present
    ?assertMatch(#{
        protocolVersion := _,
        capabilities := _,
        serverInfo := #{name := _, version := _}
    }, InitResponse),

    erlmcp_server:stop(Server).
```

**Coverage Targets**:
- All MCP methods: 100%
- All error codes: 100%
- All notification types: 100%

**Existing**: erlmcp_protocol_validator.erl, erlmcp_spec_parser.erl
**Gap Analysis**: Need per-method compliance tests for all 8+ MCP methods

---

### 5. Performance Regression Tests

**Purpose**: Prevent performance degradation in critical paths (P50, P95, P99 latency, throughput).

**Scope**:
- JSON-RPC encoding/decoding latency
- Registry lookup throughput
- Resource read latency
- Tool call latency (with/without schema validation)
- Subscription fan-out performance
- Concurrent client throughput
- Memory usage per connection
- Connection setup time

**Chicago School Principles**:
- ✅ Benchmark real gen_servers under load
- ✅ Measure observable behavior (latency, throughput)
- ✅ No synthetic mocks

**Example Structure**:
```erlang
-module(erlmcp_perf_regression_SUITE).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").

all() ->
    [
     json_rpc_encoding_latency_test,
     registry_lookup_throughput_test,
     tools_call_latency_test,
     concurrent_clients_throughput_test
    ].

%% Baseline: JSON-RPC encoding P95 < 2ms
json_rpc_encoding_latency_test(_Config) ->
    Message = #{
        jsonrpc => <<"2.0">>,
        id => 1,
        method => <<"tools/call">>,
        params => #{
            name => <<"sql_query">>,
            arguments => #{query => <<"SELECT * FROM users">>}
        }
    },

    %% Warmup
    [erlmcp_json_rpc:encode(Message) || _ <- lists:seq(1, 1000)],

    %% Benchmark
    Latencies = [begin
        Start = erlang:monotonic_time(microsecond),
        _Encoded = erlmcp_json_rpc:encode(Message),
        End = erlang:monotonic_time(microsecond),
        End - Start
    end || _ <- lists:seq(1, 10000)],

    %% Calculate percentiles
    Sorted = lists:sort(Latencies),
    P50 = lists:nth(5000, Sorted),
    P95 = lists:nth(9500, Sorted),
    P99 = lists:nth(9900, Sorted),

    ct:pal("JSON-RPC Encoding: P50=~pμs, P95=~pμs, P99=~pμs", [P50, P95, P99]),

    %% Verify: P95 < 2000μs (2ms target from MCP_SPEC_PERFORMANCE_ANALYSIS.md)
    ?assert(P95 < 2000).

%% Baseline: Registry lookup > 553K msg/s
registry_lookup_throughput_test(_Config) ->
    %% Setup: Start registry
    {ok, Registry} = erlmcp_registry:start_link(),

    %% Register 1000 servers
    Servers = [begin
        ServerId = list_to_atom("server_" ++ integer_to_list(N)),
        {ok, Pid} = erlmcp_server:start_link(ServerId, #{}),
        {ServerId, Pid}
    end || N <- lists:seq(1, 1000)],

    %% Benchmark lookups
    Start = erlang:monotonic_time(millisecond),
    Count = 10000000, %% 10M lookups

    [erlmcp_registry:whereis_name({mcp, server, element(1, lists:nth(rand:uniform(1000), Servers))})
     || _ <- lists:seq(1, Count)],

    End = erlang:monotonic_time(millisecond),
    Duration = End - Start,
    Throughput = (Count / Duration) * 1000, %% msg/s

    ct:pal("Registry Throughput: ~p msg/s", [Throughput]),

    %% Verify: > 553K msg/s (baseline from Jan 2026)
    ?assert(Throughput > 553000).
```

**Coverage Targets**:
- Critical paths: P50, P95, P99 latency baselines
- Throughput: 1M+ ops/s for hot paths
- Memory: <100KB per connection

**Existing**: erlmcp_bench_*.erl (10+ benchmark modules)
**Gap Analysis**: Need CI integration for regression detection

---

### 6. Chaos Engineering Tests

**Purpose**: Test system resilience under failures, network partitions, and resource exhaustion.

**Scope**:
- Process crashes (client, server, transport, registry)
- Supervision tree recovery (restart strategies)
- Network failures (connection drops, timeouts, partitions)
- Resource exhaustion (memory, file descriptors, ETS tables)
- Message queue overload (mailbox flooding)
- Cascading failures (bulkhead pattern verification)
- Split-brain scenarios (distributed registry)

**Chicago School Principles**:
- ✅ Crash real processes (exit/2, kill signals)
- ✅ Verify real supervision recovery
- ✅ Observe actual system behavior under chaos

**Example Structure**:
```erlang
-module(erlmcp_chaos_SUITE).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").

all() ->
    [
     server_crash_recovery_test,
     transport_crash_isolation_test,
     registry_crash_restart_test,
     network_partition_test,
     memory_exhaustion_test,
     cascading_failure_prevention_test
    ].

%% Chaos: Kill server, verify supervision restart
server_crash_recovery_test(_Config) ->
    %% Setup: Start real server under supervision
    {ok, Sup} = erlmcp_server_sup:start_link(),
    {ok, Server} = supervisor:start_child(Sup, #{
        id => test_server,
        start => {erlmcp_server, start_link, [test_server, #{}]},
        restart => permanent
    }),

    InitialPid = Server,

    %% Exercise: Kill server process (chaos injection)
    exit(Server, kill),
    timer:sleep(500), %% Allow supervisor to restart

    %% Verify: Server restarted with new PID (supervision behavior)
    {ok, NewPid} = erlmcp_registry:whereis_name({mcp, server, test_server}),
    ?assert(is_pid(NewPid)),
    ?assertNotEqual(InitialPid, NewPid),

    %% Verify: Server functional after restart (observable behavior)
    {ok, Info} = erlmcp_server:get_info(NewPid),
    ?assertEqual(uninitialized, maps:get(state, Info)).

%% Chaos: Network partition during resource subscription
network_partition_test(_Config) ->
    %% Setup: Start server and client
    {ok, Server} = erlmcp_server:start_link(test_server, #{}),
    {ok, Client} = erlmcp_client:start_link(#{transport => tcp}),

    %% Add resource
    ok = erlmcp_server:add_resource(Server, <<"weather://city">>,
        fun(_) -> <<"sunny">> end),

    %% Subscribe client
    ok = erlmcp_client:subscribe_resource(Client, <<"weather://city">>),

    %% Exercise: Simulate network partition (kill transport)
    TransportPid = erlmcp_client:get_transport_pid(Client),
    exit(TransportPid, kill),

    %% Verify: Client detects disconnection (observable behavior)
    {ok, ClientState} = erlmcp_client:get_state(Client),
    ?assertEqual(disconnected, maps:get(connection_state, ClientState)),

    %% Verify: Server cleans up subscription (automatic cleanup)
    timer:sleep(500),
    {ok, Subscribers} = erlmcp_server:get_subscribers(Server, <<"weather://city">>),
    ?assertEqual(0, length(Subscribers)).
```

**Coverage Targets**:
- Process crash scenarios: 20+
- Network failure scenarios: 15+
- Resource exhaustion scenarios: 10+

**Existing**: erlmcp_chaos.erl, erlmcp_chaos_tests.erl
**Gap Analysis**: Need systematic chaos scenarios for all critical paths

---

## MCP Feature Test Matrix

### Resources

| Feature | EUnit | CT | Proper | Compliance | Performance | Chaos |
|---------|-------|----|----|--------|-------------|-------|
| Static resources | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ |
| Resource templates | ✅ | ✅ | ✅ | ✅ | ⚠️ | ❌ |
| Resource list | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ |
| Resource read | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ |
| Resource subscribe | ✅ | ✅ | ⚠️ | ✅ | ⚠️ | ⚠️ |
| Resource unsubscribe | ✅ | ✅ | ❌ | ✅ | ❌ | ❌ |
| Resource updated notifications | ✅ | ✅ | ❌ | ✅ | ⚠️ | ❌ |
| Resource list changed | ⚠️ | ✅ | ❌ | ✅ | ❌ | ❌ |

**Legend**: ✅ = Implemented, ⚠️ = Partial, ❌ = Missing

**Gap Analysis**:
- **Missing**: Resource template performance tests
- **Missing**: Resource subscription state machine properties (Proper)
- **Missing**: Resource notification chaos tests
- **Partial**: Resource subscription fan-out performance under high load

---

### Tools

| Feature | EUnit | CT | Proper | Compliance | Performance | Chaos |
|---------|-------|----|----|--------|-------------|-------|
| Tool registration | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ |
| Tool list | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ |
| Tool call (no schema) | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ |
| Tool call (with schema) | ✅ | ✅ | ⚠️ | ✅ | ⚠️ | ❌ |
| Schema validation | ✅ | ✅ | ✅ | ✅ | ⚠️ | ❌ |
| Tool list changed | ⚠️ | ✅ | ❌ | ✅ | ❌ | ❌ |
| Tool timeout handling | ✅ | ✅ | ❌ | ✅ | ❌ | ⚠️ |
| Tool error propagation | ✅ | ✅ | ❌ | ✅ | ❌ | ❌ |

**Gap Analysis**:
- **Missing**: Schema validation performance (jesse caching optimization)
- **Missing**: Tool call chaos tests (handler crashes, timeouts)
- **Partial**: Schema validation properties (invalid schema edge cases)

---

### Prompts

| Feature | EUnit | CT | Proper | Compliance | Performance | Chaos |
|---------|-------|----|----|--------|-------------|-------|
| Prompt registration | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ |
| Prompt list | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ |
| Prompt get | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ |
| Prompt arguments | ✅ | ✅ | ⚠️ | ✅ | ❌ | ❌ |
| Prompt templates | ✅ | ✅ | ❌ | ✅ | ❌ | ❌ |
| Prompt list changed | ⚠️ | ✅ | ❌ | ✅ | ❌ | ❌ |

**Gap Analysis**:
- **Missing**: Prompt argument validation properties (required/optional args)
- **Missing**: Prompt template rendering performance
- **Missing**: Prompt chaos tests

---

### Sampling (LLM Integration)

| Feature | EUnit | CT | Proper | Compliance | Performance | Chaos |
|---------|-------|----|----|--------|-------------|-------|
| Sampling create message | ⚠️ | ⚠️ | ❌ | ⚠️ | ❌ | ❌ |
| Model preferences | ❌ | ❌ | ❌ | ⚠️ | ❌ | ❌ |
| System prompt | ❌ | ❌ | ❌ | ⚠️ | ❌ | ❌ |
| Include context | ❌ | ❌ | ❌ | ⚠️ | ❌ | ❌ |
| Temperature/max tokens | ❌ | ❌ | ❌ | ⚠️ | ❌ | ❌ |
| Stop sequences | ❌ | ❌ | ❌ | ❌ | ❌ | ❌ |
| Metadata | ❌ | ❌ | ❌ | ❌ | ❌ | ❌ |

**Gap Analysis**:
- **Critical**: Sampling feature barely tested (new in MCP 2025-11-25)
- **Missing**: All test categories need implementation

---

### Logging

| Feature | EUnit | CT | Proper | Compliance | Performance | Chaos |
|---------|-------|----|----|--------|-------------|-------|
| Log level filtering | ✅ | ✅ | ❌ | ✅ | ❌ | ❌ |
| setLevel notification | ✅ | ✅ | ❌ | ✅ | ❌ | ❌ |
| Log data types | ✅ | ❌ | ❌ | ✅ | ❌ | ❌ |

**Gap Analysis**:
- **Missing**: Log data type properties (valid log formats)
- **Missing**: Log performance (high-frequency logging)

---

### Completion (Autocomplete)

| Feature | EUnit | CT | Proper | Compliance | Performance | Chaos |
|---------|-------|----|----|--------|-------------|-------|
| Argument completion | ✅ | ✅ | ❌ | ✅ | ❌ | ❌ |
| Resource URI completion | ✅ | ⚠️ | ❌ | ✅ | ❌ | ❌ |
| Ref completion | ⚠️ | ❌ | ❌ | ⚠️ | ❌ | ❌ |

**Gap Analysis**:
- **Partial**: Ref completion implementation and tests
- **Missing**: Completion performance tests (latency)

---

### Roots (File System Access)

| Feature | EUnit | CT | Proper | Compliance | Performance | Chaos |
|---------|-------|----|----|--------|-------------|-------|
| Root list | ⚠️ | ⚠️ | ❌ | ⚠️ | ❌ | ❌ |
| Root URI validation | ❌ | ❌ | ❌ | ❌ | ❌ | ❌ |

**Gap Analysis**:
- **Critical**: Roots feature minimally tested
- **Missing**: Root URI validation tests

---

### Cancellation

| Feature | EUnit | CT | Proper | Compliance | Performance | Chaos |
|---------|-------|----|----|--------|-------------|-------|
| Request cancellation | ✅ | ✅ | ❌ | ✅ | ❌ | ⚠️ |
| Progress token cancellation | ✅ | ⚠️ | ❌ | ✅ | ❌ | ❌ |
| Cancellation notification | ✅ | ✅ | ❌ | ✅ | ❌ | ❌ |

**Gap Analysis**:
- **Missing**: Cancellation properties (race conditions)
- **Partial**: Cancellation chaos tests (cancel during execution)

---

### Progress Tracking

| Feature | EUnit | CT | Proper | Compliance | Performance | Chaos |
|---------|-------|----|----|--------|-------------|-------|
| Progress notifications | ✅ | ✅ | ❌ | ✅ | ❌ | ❌ |
| Progress token generation | ✅ | ✅ | ❌ | ✅ | ❌ | ❌ |
| Progress updates | ✅ | ✅ | ❌ | ✅ | ⚠️ | ❌ |

**Gap Analysis**:
- **Missing**: Progress notification performance (high-frequency updates)
- **Missing**: Progress chaos tests

---

## Test Organization

### Directory Structure

```
erlmcp/
├── apps/
│   ├── erlmcp_core/
│   │   └── test/
│   │       ├── unit/                    # EUnit tests (269 existing)
│   │       │   ├── protocol/            # JSON-RPC, MCP messages
│   │       │   ├── client/              # Client gen_server tests
│   │       │   ├── server/              # Server gen_server tests
│   │       │   ├── registry/            # Registry tests
│   │       │   ├── resources/           # Resource tests ← NEW
│   │       │   ├── tools/               # Tool tests ← NEW
│   │       │   ├── prompts/             # Prompt tests ← NEW
│   │       │   ├── sampling/            # Sampling tests ← NEW
│   │       │   ├── logging/             # Logging tests
│   │       │   ├── completion/          # Completion tests ← NEW
│   │       │   ├── roots/               # Roots tests ← NEW
│   │       │   ├── cancellation/        # Cancellation tests
│   │       │   └── progress/            # Progress tests
│   │       ├── integration/             # Common Test suites (33 existing)
│   │       │   ├── client_server/       # Client-server workflows
│   │       │   ├── multi_client/        # Concurrent clients
│   │       │   ├── subscriptions/       # Resource subscriptions ← NEW
│   │       │   ├── lifecycle/           # Full lifecycle tests
│   │       │   └── distributed/         # Multi-node tests
│   │       ├── property/                # Proper tests (10+ existing)
│   │       │   ├── protocol_properties/ # JSON-RPC invariants
│   │       │   ├── fsm_properties/      # State machine properties
│   │       │   ├── resource_properties/ # Resource invariants ← NEW
│   │       │   ├── tool_properties/     # Tool invariants ← NEW
│   │       │   └── concurrency_properties/ # Race conditions
│   │       ├── compliance/              # Spec compliance tests ← NEW
│   │       │   ├── initialize_SUITE.erl
│   │       │   ├── resources_SUITE.erl
│   │       │   ├── tools_SUITE.erl
│   │       │   ├── prompts_SUITE.erl
│   │       │   ├── sampling_SUITE.erl   ← NEW
│   │       │   ├── logging_SUITE.erl
│   │       │   ├── completion_SUITE.erl ← NEW
│   │       │   ├── roots_SUITE.erl      ← NEW
│   │       │   ├── cancellation_SUITE.erl
│   │       │   └── progress_SUITE.erl
│   │       ├── performance/             # Performance regression tests
│   │       │   ├── erlmcp_bench_*.erl (10+ existing)
│   │       │   ├── latency_regression_SUITE.erl ← NEW
│   │       │   ├── throughput_regression_SUITE.erl ← NEW
│   │       │   └── memory_regression_SUITE.erl ← NEW
│   │       └── chaos/                   # Chaos engineering tests
│   │           ├── erlmcp_chaos_tests.erl (existing)
│   │           ├── process_crash_SUITE.erl ← NEW
│   │           ├── network_failure_SUITE.erl ← NEW
│   │           ├── resource_exhaustion_SUITE.erl ← NEW
│   │           └── cascading_failure_SUITE.erl ← NEW
│   ├── erlmcp_transports/
│   │   └── test/
│   │       ├── unit/
│   │       ├── integration/
│   │       ├── compliance/              # Transport compliance tests
│   │       └── performance/
│   ├── erlmcp_observability/
│   │   └── test/
│   │       ├── unit/
│   │       ├── integration/
│   │       └── performance/
│   └── erlmcp_validation/
│       └── test/
│           ├── unit/
│           ├── compliance/
│           └── security/
└── test/
    ├── common_test/                     # Shared CT infrastructure
    │   ├── test_helpers.erl             # Test utilities
    │   ├── mcp_test_server.erl          # Mock MCP server
    │   └── mcp_test_client.erl          # Mock MCP client
    └── fixtures/                        # Test data
        ├── mcp_spec_2025-11-25.json     # Official MCP spec
        ├── valid_requests/              # Valid MCP requests
        ├── invalid_requests/            # Invalid requests (error cases)
        └── schemas/                     # JSON schemas for validation
```

### Naming Conventions

**EUnit Tests**:
- `{module}_tests.erl` - Tests for `{module}.erl`
- `{feature}_{aspect}_tests.erl` - Feature-specific tests
- Example: `erlmcp_resources_subscription_tests.erl`

**Common Test Suites**:
- `{feature}_SUITE.erl` - Feature integration tests
- `{feature}_{scenario}_SUITE.erl` - Specific scenarios
- Example: `erlmcp_resources_subscription_SUITE.erl`

**Proper Tests**:
- `{module}_proper_tests.erl` - Property tests for module
- `{feature}_properties.erl` - Feature-specific properties
- Example: `erlmcp_resources_proper_tests.erl`

**Compliance Tests**:
- `erlmcp_{mcp_method}_compliance_SUITE.erl`
- Example: `erlmcp_tools_call_compliance_SUITE.erl`

**Performance Tests**:
- `erlmcp_bench_{feature}.erl` - Benchmarks
- `{feature}_regression_SUITE.erl` - Regression tests
- Example: `tools_call_regression_SUITE.erl`

**Chaos Tests**:
- `{failure_type}_chaos_SUITE.erl`
- Example: `server_crash_chaos_SUITE.erl`

---

## Chicago School TDD Principles

### Core Tenets

1. **Real Collaborators** - Spawn actual gen_servers, not mocks
2. **State-Based Verification** - Assert on observable state via API calls
3. **Behavior Testing** - Verify what system does (outputs), not how it does it (internals)
4. **Integration Over Isolation** - Test components together when practical

### Anti-Patterns (What NOT to Do)

❌ **Don't Mock gen_servers**:
```erlang
%% BAD (London School mocking)
meck:new(erlmcp_server),
meck:expect(erlmcp_server, add_resource, fun(_, _, _) -> ok end).
```

✅ **Do Use Real gen_servers**:
```erlang
%% GOOD (Chicago School)
{ok, Server} = erlmcp_server:start_link(test_server, #{}),
ok = erlmcp_server:add_resource(Server, <<"doc://readme">>, fun(_) -> <<"content">> end).
```

---

❌ **Don't Verify Internal Calls**:
```erlang
%% BAD (London School interaction verification)
?assertMatch({call, handle_call, [add_resource, _, _]}, meck:history(erlmcp_server)).
```

✅ **Do Verify Observable State**:
```erlang
%% GOOD (Chicago School state verification)
ok = erlmcp_server:add_resource(Server, <<"doc://readme">>, fun(_) -> <<"content">> end),
{ok, Resources} = erlmcp_server:list_resources(Server),
?assert(lists:any(fun(R) -> maps:get(uri, R) =:= <<"doc://readme">> end, Resources)).
```

---

❌ **Don't Mock Collaborators**:
```erlang
%% BAD (London School)
meck:new(erlmcp_registry),
meck:expect(erlmcp_registry, whereis_name, fun(_) -> {ok, fake_pid} end).
```

✅ **Do Use Real Collaborators**:
```erlang
%% GOOD (Chicago School)
application:ensure_all_started(erlmcp_core),  %% Real registry under supervision
{ok, Server} = erlmcp_server:start_link(test_server, #{}),
{ok, ServerPid} = erlmcp_registry:whereis_name({mcp, server, test_server}).
```

### Testing Patterns

**Pattern 1: Test Real Process Behavior**
```erlang
test_server_lifecycle() ->
    %% Setup: Start real server
    {ok, Server} = erlmcp_server:start_link(test_server, #{}),

    %% Verify: Initial state (observable)
    {ok, Info} = erlmcp_server:get_info(Server),
    ?assertEqual(uninitialized, maps:get(state, Info)),

    %% Exercise: Initialize server
    ok = erlmcp_server:initialize(Server, #{protocolVersion => <<"2024-11-05">>}),

    %% Verify: State transitioned (observable)
    {ok, Info2} = erlmcp_server:get_info(Server),
    ?assertEqual(initialized, maps:get(state, Info2)),

    %% Cleanup
    erlmcp_server:stop(Server).
```

**Pattern 2: Test Real Message Passing**
```erlang
test_resource_notification() ->
    %% Setup: Start server and client
    {ok, Server} = erlmcp_server:start_link(test_server, #{}),
    {ok, Client} = erlmcp_client:start_link(#{transport => test}),

    %% Subscribe client
    ok = erlmcp_server:subscribe_resource(Server, <<"weather://city">>, Client),

    %% Exercise: Update resource (triggers real notification)
    ok = erlmcp_server:notify_resource_updated(Server, <<"weather://city">>, #{}),

    %% Verify: Client received notification (real message passing)
    receive
        {resource_updated, <<"weather://city">>, _Metadata} -> ok
    after 1000 ->
        ?assert(false, "Notification not received")
    end.
```

**Pattern 3: Test Real Supervision Recovery**
```erlang
test_supervision_restart() ->
    %% Setup: Start server under supervision
    {ok, Sup} = erlmcp_server_sup:start_link(),
    {ok, Server} = supervisor:start_child(Sup, server_child_spec(test_server)),

    InitialPid = Server,

    %% Exercise: Kill server (real process crash)
    exit(Server, kill),
    timer:sleep(500),

    %% Verify: Supervisor restarted server (real supervision)
    {ok, NewPid} = erlmcp_registry:whereis_name({mcp, server, test_server}),
    ?assert(is_pid(NewPid)),
    ?assertNotEqual(InitialPid, NewPid).
```

---

## Implementation Roadmap

### Phase 1: Foundation (Weeks 1-2)

**Goals**:
- Organize existing tests into taxonomy
- Create test infrastructure
- Establish baselines

**Tasks**:
1. **Reorganize test directory structure** (1 day)
   - Create subdirectories: unit/, integration/, property/, compliance/, performance/, chaos/
   - Move existing 269 EUnit tests into unit/ subdirectories
   - Move existing 33 CT suites into integration/ subdirectories
   - Create placeholder directories for missing categories

2. **Create test infrastructure** (2 days)
   - `test/common_test/test_helpers.erl` - Shared test utilities
   - `test/common_test/mcp_test_server.erl` - Mock MCP server for testing
   - `test/common_test/mcp_test_client.erl` - Mock MCP client for testing
   - `test/fixtures/` - Test data (MCP spec JSON, valid/invalid requests)

3. **Establish performance baselines** (2 days)
   - Run existing benchmarks: `rebar3 as benchmark shell`
   - Document current performance: P50, P95, P99, throughput
   - Create baseline JSON: `performance_baselines_2026-02-01.json`
   - Example:
     ```json
     {
       "date": "2026-02-01",
       "version": "2.1.0",
       "baselines": {
         "json_rpc_encoding": {
           "p50_us": 500,
           "p95_us": 1800,
           "p99_us": 2500
         },
         "registry_lookup": {
           "throughput_msg_s": 553000
         },
         "tools_call": {
           "p50_ms": 5,
           "p95_ms": 20,
           "p99_ms": 50
         }
       }
     }
     ```

4. **Create compliance test template** (1 day)
   - Template for MCP method compliance tests
   - Load MCP spec from `test/fixtures/mcp_spec_2025-11-25.json`
   - Validate request/response against spec schema
   - Example template structure

**Deliverables**:
- ✅ Reorganized test directory (taxonomy-based)
- ✅ Test infrastructure (helpers, fixtures)
- ✅ Performance baselines documented
- ✅ Compliance test template

**Agent Assignment**:
- **erlang-researcher**: Analyze existing test files, categorize by taxonomy
- **erlang-architect**: Design test infrastructure (helpers, fixtures)
- **erlang-test-engineer**: Create test templates
- **erlang-performance**: Run benchmarks, document baselines

---

### Phase 2: MCP Feature Coverage (Weeks 3-6)

**Goals**:
- Achieve 100% coverage for all MCP methods
- Add missing test categories (Proper, compliance, performance)

**Tasks per MCP Feature**:

#### 2.1 Resources (Week 3)
- **EUnit**: Fill gaps
  - `erlmcp_resources_subscription_tests.erl` - Subscription state machine
  - `erlmcp_resources_notification_tests.erl` - Notification delivery
  - `erlmcp_resources_template_tests.erl` - Template matching
- **CT**: Integration tests
  - `erlmcp_resources_subscription_SUITE.erl` - Multi-client subscriptions
  - `erlmcp_resources_lifecycle_SUITE.erl` - Full lifecycle
- **Proper**: Properties
  - `erlmcp_resources_proper_tests.erl` - Subscription invariants
  - Properties: `prop_subscribe_idempotent`, `prop_notify_all_subscribers`
- **Compliance**: Spec tests
  - `erlmcp_resources_list_compliance_SUITE.erl`
  - `erlmcp_resources_read_compliance_SUITE.erl`
  - `erlmcp_resources_subscribe_compliance_SUITE.erl`
- **Performance**: Benchmarks
  - `erlmcp_bench_resource_templates.erl` - Template matching performance
  - `erlmcp_bench_resource_subscriptions.erl` - Subscription fan-out
- **Chaos**: Resilience
  - `resource_subscription_chaos_SUITE.erl` - Subscriber crashes
  - `resource_notification_chaos_SUITE.erl` - Notification failures

**Agent Assignment**:
- **erlang-otp-developer**: Implement missing resource features
- **erlang-test-engineer**: Write EUnit, CT, Proper tests
- **erlang-performance**: Create benchmarks
- **code-reviewer**: Review for Chicago School TDD compliance

#### 2.2 Tools (Week 4)
- **EUnit**: Schema validation edge cases
  - `erlmcp_tool_schema_validation_tests.erl` - Complex schema tests
  - `erlmcp_tool_error_propagation_tests.erl` - Error handling
- **CT**: Integration tests
  - `erlmcp_tools_call_lifecycle_SUITE.erl` - Full tool call lifecycle
  - `erlmcp_tools_concurrent_SUITE.erl` - Concurrent tool calls
- **Proper**: Properties
  - `erlmcp_tool_schema_proper_tests.erl` - Schema validation properties
  - Properties: `prop_valid_args_accepted`, `prop_invalid_args_rejected`
- **Compliance**: Spec tests
  - `erlmcp_tools_list_compliance_SUITE.erl`
  - `erlmcp_tools_call_compliance_SUITE.erl`
- **Performance**: Benchmarks
  - `erlmcp_bench_schema_validation.erl` - Schema validation latency
  - `erlmcp_bench_tools_call.erl` - Tool call throughput
- **Chaos**: Resilience
  - `tool_handler_crash_chaos_SUITE.erl` - Handler crashes
  - `tool_timeout_chaos_SUITE.erl` - Tool timeouts

**Agent Assignment**:
- **erlang-otp-developer**: Implement schema validation optimizations
- **erlang-test-engineer**: Write tests
- **erlang-performance**: Optimize schema caching
- **code-reviewer**: Review

#### 2.3 Prompts (Week 5)
- Similar structure to Tools and Resources

#### 2.4 Sampling (Week 6) ⚠️ **CRITICAL GAP**
- **EUnit**: All test categories missing
  - `erlmcp_sampling_create_message_tests.erl`
  - `erlmcp_sampling_model_preferences_tests.erl`
  - `erlmcp_sampling_validation_tests.erl`
- **CT**: Integration tests
  - `erlmcp_sampling_lifecycle_SUITE.erl`
  - `erlmcp_sampling_providers_SUITE.erl` (Anthropic, OpenAI, local)
- **Proper**: Properties
  - `erlmcp_sampling_proper_tests.erl`
  - Properties: `prop_temperature_bounds`, `prop_max_tokens_respected`
- **Compliance**: Spec tests
  - `erlmcp_sampling_createMessage_compliance_SUITE.erl`
- **Performance**: Benchmarks
  - `erlmcp_bench_sampling.erl` - Sampling latency
- **Chaos**: Resilience
  - `sampling_provider_failure_chaos_SUITE.erl`

**Agent Assignment**:
- **erlang-otp-developer**: Implement sampling feature fully
- **erlang-test-engineer**: Create comprehensive test suite
- **code-reviewer**: Ensure Chicago School TDD

**Deliverables (Phase 2)**:
- ✅ 100% test coverage for all MCP methods
- ✅ 50+ new Proper properties
- ✅ 30+ compliance test suites
- ✅ 20+ performance benchmarks
- ✅ 15+ chaos test suites

---

### Phase 3: Performance & Chaos (Weeks 7-8)

**Goals**:
- Implement performance regression CI integration
- Comprehensive chaos engineering scenarios
- Performance optimization based on benchmarks

**Tasks**:

#### 3.1 Performance Regression Detection (Week 7)
1. **CI Integration** (2 days)
   - GitHub Actions workflow: `.github/workflows/performance_regression.yml`
   - Run benchmarks on every PR
   - Compare against baselines
   - Fail PR if regression > 10%

2. **Performance Dashboard** (2 days)
   - Store benchmark results in time-series DB (InfluxDB)
   - Grafana dashboard for visualization
   - Alert on regressions

3. **Optimization Priorities** (3 days)
   - **Priority 1**: Schema validation caching (jesse optimization)
     - Expected improvement: 75% latency reduction
     - Implementation: ETS cache for compiled schemas
   - **Priority 2**: Replace jsx with jiffy (NIF-based JSON)
     - Expected improvement: 60% encoding/decoding latency reduction
   - **Priority 3**: Async tool execution (unblock gen_server)
     - Expected improvement: 5-10x throughput increase

**Agent Assignment**:
- **erlang-performance**: Implement optimizations, create CI workflow
- **build-engineer**: Set up CI integration
- **verifier**: Run regression tests

#### 3.2 Chaos Engineering Framework (Week 8)
1. **Systematic Chaos Scenarios** (3 days)
   - Process crash scenarios (20+)
     - Server crash during request
     - Client crash during response wait
     - Transport crash during message send
     - Registry crash and restart
   - Network failure scenarios (15+)
     - Connection drop during subscription
     - Timeout during tool call
     - Network partition (split-brain)
   - Resource exhaustion scenarios (10+)
     - Memory exhaustion (process heap)
     - ETS table limit
     - File descriptor exhaustion

2. **Chaos Test Automation** (2 days)
   - `erlmcp_chaos:inject_failure/2` - API for chaos injection
   - Random failure injection mode
   - Chaos monkey scheduler (periodic failures)

**Agent Assignment**:
- **erlang-test-engineer**: Create chaos test suites
- **erlang-performance**: Measure recovery times
- **code-reviewer**: Review

**Deliverables (Phase 3)**:
- ✅ Performance regression CI integration
- ✅ 50+ chaos test scenarios
- ✅ Performance optimizations (schema caching, jiffy, async tools)

---

### Phase 4: Quality Gates & Reporting (Weeks 9-10)

**Goals**:
- Achieve 85%+ coverage for core modules
- Generate compliance reports
- Integrate with claude-flow agents

**Tasks**:

#### 4.1 Coverage Improvement (Week 9)
1. **Identify Coverage Gaps** (1 day)
   - Run `rebar3 cover --verbose`
   - Generate coverage report: `_build/test/cover/index.html`
   - Identify modules < 80% coverage

2. **Fill Coverage Gaps** (3 days)
   - Add tests for uncovered code paths
   - Focus on error cases (error returns, invalid inputs)
   - Focus on edge cases (boundary conditions)

3. **Coverage Enforcement** (1 day)
   - CI workflow: `.github/workflows/coverage.yml`
   - Fail PR if coverage < 80% (overall)
   - Fail PR if core module < 85%

**Agent Assignment**:
- **erlang-test-engineer**: Write missing tests
- **build-engineer**: Set up coverage CI
- **code-reviewer**: Review

#### 4.2 Compliance Reporting (Week 10)
1. **Generate Compliance Report** (2 days)
   - Run all compliance test suites
   - Generate HTML report: `erlmcp_compliance_report:generate_html/0`
   - Generate JSON report: `erlmcp_compliance_report:generate_json/0`
   - Include:
     - MCP methods tested: X/Y (target: Y/Y = 100%)
     - Error codes tested: X/Y
     - Notification types tested: X/Y
     - Transport compliance: X/Y

2. **Documentation** (1 day)
   - Update `docs/MCP_TEST_STRATEGY.md` with final results
   - Create `docs/TEST_COVERAGE_REPORT.md`
   - Create `docs/COMPLIANCE_REPORT.md`

**Deliverables (Phase 4)**:
- ✅ 85%+ coverage for core modules
- ✅ 80%+ coverage overall
- ✅ Compliance reports (HTML, JSON)
- ✅ Documentation

---

## Claude-Flow Agent Integration

### Available Test Agents

From `.claude/agents/`:

| Agent | Purpose | Test Contribution |
|-------|---------|-------------------|
| **erlang-test-engineer** | EUnit, CT, Proper tests | Primary test implementation agent |
| **erlang-otp-developer** | gen_server implementation | Implements features tested |
| **erlang-performance** | Benchmarking, optimization | Performance regression tests |
| **code-reviewer** | Quality gates | Reviews Chicago School TDD compliance |
| **verifier** | Test execution | Runs test suites, reports results |
| **agent-06-test-eunit** | EUnit test runner | Executes EUnit tests |
| **agent-07-test-ct** | Common Test runner | Executes CT suites |
| **agent-10-test-proper** | Proper test runner | Executes property-based tests |
| **agent-11-coverage** | Coverage analysis | Generates coverage reports |

### Parallel Workflow (EPIC 9)

**Trigger**: Multi-feature testing (5+ test files, 3+ MCP features)

**Phases**:

1. **Fan-out** - Spawn multiple test agents in parallel
   ```
   Task("EUnit - Resources", "Create resource EUnit tests", "erlang-test-engineer")
   Task("EUnit - Tools", "Create tool EUnit tests", "erlang-test-engineer")
   Task("EUnit - Prompts", "Create prompt EUnit tests", "erlang-test-engineer")
   Task("CT - Integration", "Create integration test suites", "erlang-test-engineer")
   Task("Proper - Properties", "Create property-based tests", "erlang-test-engineer")
   Task("Compliance - Spec", "Create compliance tests", "erlang-test-engineer")
   Task("Performance - Benchmarks", "Create benchmarks", "erlang-performance")
   Task("Chaos - Resilience", "Create chaos tests", "erlang-test-engineer")
   ```

2. **Independent Construction** - Each agent works on separate test category
3. **Collision Detection** - Merge tests, detect conflicts
4. **Convergence** - Consolidate test suites
5. **Refactoring** - Extract common test helpers
6. **Closure** - Run all tests, generate reports

**Expected Speedup**: 2.8x - 4.4x

### Test Generation Workflow

**Example: Generate Resource Subscription Tests**

1. **Research** (erlang-researcher)
   - Read `apps/erlmcp_core/src/erlmcp_server.erl` - Understand subscription API
   - Read `docs/protocol.md` - Understand MCP spec requirements
   - Read existing tests - Understand test patterns

2. **Plan** (erlang-architect + erlang-test-engineer)
   - Decide test granularity (per-method vs scenario)
   - Identify edge cases (empty subscribers, duplicate subscriptions)
   - Define test data generators (resource URIs, subscription requests)

3. **Execute** (erlang-test-engineer)
   - Create `erlmcp_resources_subscription_tests.erl` (EUnit)
   - Create `erlmcp_resources_subscription_SUITE.erl` (CT)
   - Create `erlmcp_resources_subscription_proper_tests.erl` (Proper)

4. **Verify** (verifier + code-reviewer)
   - Run tests: `rebar3 eunit --module=erlmcp_resources_subscription_tests`
   - Check coverage: `rebar3 cover`
   - Review Chicago School TDD compliance

5. **Optimize** (erlang-performance)
   - Benchmark subscription fan-out
   - Identify bottlenecks (O(N) notification send)
   - Propose optimizations (batch notifications, async sends)

### Agent Coordination Example

**Task**: "Create comprehensive test suite for MCP sampling feature"

**Agent Spawn (1 message)**:
```javascript
// Spawn all agents in parallel (Chicago School TDD workflow)
Task("Research Sampling", "Explore existing sampling implementation", "erlang-researcher")
Task("Design Tests", "Design test architecture for sampling", "erlang-architect")
Task("EUnit Tests", "Create EUnit tests for sampling API", "erlang-test-engineer")
Task("CT Tests", "Create CT integration tests for sampling providers", "erlang-test-engineer")
Task("Proper Tests", "Create property-based tests for sampling", "erlang-test-engineer")
Task("Compliance Tests", "Create MCP spec compliance tests for sampling", "erlang-test-engineer")
Task("Benchmark", "Create sampling performance benchmarks", "erlang-performance")
Task("Chaos Tests", "Create sampling chaos tests (provider failures)", "erlang-test-engineer")
Task("Review", "Review all tests for Chicago School TDD compliance", "code-reviewer")
Task("Verify", "Run all tests, generate coverage report", "verifier")
```

**Expected Output**:
- ✅ `apps/erlmcp_core/test/unit/sampling/erlmcp_sampling_tests.erl` (EUnit)
- ✅ `apps/erlmcp_core/test/integration/sampling/erlmcp_sampling_SUITE.erl` (CT)
- ✅ `apps/erlmcp_core/test/property/erlmcp_sampling_proper_tests.erl` (Proper)
- ✅ `apps/erlmcp_core/test/compliance/erlmcp_sampling_compliance_SUITE.erl` (Compliance)
- ✅ `apps/erlmcp_core/test/performance/erlmcp_bench_sampling.erl` (Benchmark)
- ✅ `apps/erlmcp_core/test/chaos/sampling_provider_chaos_SUITE.erl` (Chaos)
- ✅ Coverage: 85%+
- ✅ Chicago School TDD: ✅ (verified by code-reviewer)

---

## Quality Gates

### Pre-Commit Gates

1. **Compile** - `TERM=dumb rebar3 compile` (errors = 0)
2. **Format** - `rebar3 format --verify` (formatting correct)
3. **EUnit** - `rebar3 eunit --module=<changed_module>_tests` (failures = 0)

### PR Gates

1. **All Tests** - `rebar3 do eunit, ct, proper -c` (failures = 0)
2. **Coverage** - `rebar3 cover` (≥80% overall, ≥85% core modules)
3. **Dialyzer** - `rebar3 dialyzer` (warnings → 0)
4. **Xref** - `rebar3 xref` (undefined functions = ∅)
5. **Performance** - No regression > 10%

### Release Gates

1. **Full Test Suite** - `make check` (all gates pass)
2. **Compliance Report** - 100% MCP method coverage
3. **Performance Report** - All baselines met
4. **Chaos Tests** - All resilience scenarios pass
5. **Coverage Report** - ≥85% core, ≥80% overall

---

## Performance Baselines

**Source**: `docs/MCP_SPEC_PERFORMANCE_ANALYSIS.md` (Jan 2026)

### Current Baselines

| Metric | Current Value | Target | Status |
|--------|---------------|--------|--------|
| **Registry Throughput** | 553K msg/s | >500K | ✅ PASS |
| **Queue Operations** | 971K ops/s | >900K | ✅ PASS |
| **JSON-RPC Encoding (P50)** | 0.5-1.0ms | <1ms | ✅ PASS |
| **JSON-RPC Encoding (P95)** | 2-5ms | <5ms | ✅ PASS |
| **JSON-RPC Decoding (P50)** | 0.5-2ms | <2ms | ✅ PASS |
| **JSON-RPC Decoding (P95)** | 2-5ms | <5ms | ✅ PASS |
| **Tool Call (no schema, P50)** | 1-2ms | <5ms | ✅ PASS |
| **Tool Call (no schema, P95)** | 5-10ms | <20ms | ✅ PASS |
| **Tool Call (with schema, P50)** | 5-10ms | <10ms | ⚠️ MARGINAL |
| **Tool Call (with schema, P95)** | 20-30ms | <50ms | ✅ PASS |
| **Resource Read (P50)** | <5ms | <10ms | ✅ PASS |
| **Resource Read (P95)** | <10ms | <200ms | ✅ PASS |
| **Subscription Fan-out (100 sub)** | <5ms/notification | <10ms | ✅ PASS |
| **Concurrent Connections** | 40-50K/node | >10K | ✅ PASS |
| **Memory/Connection** | <100KB | <100KB | ✅ PASS |
| **Connection Setup** | <100ms | <100ms | ✅ PASS |

### Optimization Priorities

**Priority 1: Schema Validation Caching**
- **Current**: 5-20ms per validated tool call
- **Target**: 1-5ms (75% reduction)
- **Implementation**: ETS cache for compiled jesse schemas
- **Impact**: High (affects all tools with schemas)

**Priority 2: Replace jsx with jiffy**
- **Current**: 0.5-2ms encoding/decoding
- **Target**: 0.2-0.7ms (60% reduction)
- **Implementation**: Replace jsx dependency with jiffy (NIF-based)
- **Impact**: Medium (affects all JSON-RPC operations)

**Priority 3: Async Tool Execution**
- **Current**: Synchronous handle_call blocks server
- **Target**: 5-10x throughput increase
- **Implementation**: Spawn tool handler in worker pool
- **Impact**: High (unblocks server for concurrent requests)

---

## Chaos Engineering

### Chaos Scenarios

#### 1. Process Crashes (20 scenarios)

| Scenario | Description | Expected Recovery |
|----------|-------------|-------------------|
| Server crash during tool call | Kill erlmcp_server mid-request | Supervisor restarts, client times out, retry succeeds |
| Client crash waiting for response | Kill erlmcp_client during request | Server cleans up pending request, no memory leak |
| Transport crash during send | Kill transport process | Client detects disconnect, reconnects, resends |
| Registry crash | Kill erlmcp_registry | Supervisor restarts, new lookups work, existing connections continue |
| Session manager crash | Kill erlmcp_session_manager | Supervisor restarts, sessions persisted (DETS), recover |
| Subscription handler crash | Kill subscriber process | Server auto-unsubscribes (monitor), no crashes |
| Tool handler crash | Handler function throws exception | Server catches exception, returns error to client |
| Prompt handler crash | Prompt generator throws exception | Server catches exception, returns error |
| Resource handler crash | Resource reader throws exception | Server catches exception, returns error |
| Multiple concurrent server crashes | Kill 10 servers simultaneously | All restart independently, no cascading failures |

#### 2. Network Failures (15 scenarios)

| Scenario | Description | Expected Recovery |
|----------|-------------|-------------------|
| Connection drop during subscribe | TCP connection lost mid-request | Client reconnects, resubscribes |
| Timeout during tool call | Tool execution exceeds timeout | Client times out, cancels request |
| Network partition (split-brain) | Erlang node disconnected | Distributed registry detects partition, repairs |
| HTTP/2 stream failure | HTTP/2 stream reset | Gun retries on new stream |
| WebSocket disconnect during notify | WebSocket closes during notification | Server detects close, unsubscribes client |
| SSE stream interruption | SSE connection reset | Client reconnects, resumes from last event ID |
| DNS resolution failure | DNS lookup fails for HTTP transport | Gun retries with exponential backoff |
| TLS handshake failure | TLS certificate validation fails | Transport returns error, client logs warning |

#### 3. Resource Exhaustion (10 scenarios)

| Scenario | Description | Expected Recovery |
|----------|-------------|-------------------|
| Memory exhaustion (process heap) | Process heap exceeds limit | Process crashes, supervisor restarts |
| ETS table limit | Max ETS tables reached | New table creation fails gracefully |
| File descriptor exhaustion | Max file descriptors reached | Transport accept fails, logs warning |
| Message queue overflow | Mailbox > 100K messages | Process hibernates, drops old messages |
| Connection limit exceeded | Max connections reached | New connections rejected with error |
| Subscription limit exceeded | Max subscribers reached | Subscribe returns error |
| Registry limit exceeded | Max registrations reached | Register returns error |

### Chaos Test Framework

**API**: `erlmcp_chaos:inject_failure/2`

```erlang
%% Inject server crash
erlmcp_chaos:inject_failure(server_crash, #{
    target => test_server,
    delay => 1000,  %% Crash after 1s
    signal => kill
}).

%% Inject network partition
erlmcp_chaos:inject_failure(network_partition, #{
    nodes => [node1, node2],
    duration => 5000  %% 5s partition
}).

%% Inject memory exhaustion
erlmcp_chaos:inject_failure(memory_exhaustion, #{
    target => test_server,
    threshold => 100_000_000  %% 100MB
}).
```

### Chaos Metrics

- **MTBF (Mean Time Between Failures)**: Expected: >1000 hours
- **MTTR (Mean Time To Recovery)**: Target: <5 seconds
- **Availability**: Target: 99.9% (three nines)

---

## Test Infrastructure

### Test Helpers

**Module**: `test/common_test/test_helpers.erl`

```erlang
-module(test_helpers).
-export([
    start_test_server/1,
    start_test_client/1,
    send_mcp_request/3,
    assert_mcp_response/2,
    wait_for_notification/2,
    inject_chaos/2
]).

%% Start test server with default config
start_test_server(ServerId) ->
    start_test_server(ServerId, #{
        resources => #{enabled => true},
        tools => #{enabled => true},
        prompts => #{enabled => true}
    }).

start_test_server(ServerId, Capabilities) ->
    {ok, Server} = erlmcp_server:start_link(ServerId, Capabilities),
    Server.

%% Send MCP request and wait for response
send_mcp_request(Client, Method, Params) ->
    RequestId = erlang:unique_integer([positive]),
    Request = #{
        jsonrpc => <<"2.0">>,
        id => RequestId,
        method => Method,
        params => Params
    },
    ok = erlmcp_client:send(Client, erlmcp_json_rpc:encode(Request)),
    wait_for_response(Client, RequestId, 5000).

%% Assert MCP response matches expected
assert_mcp_response(Response, Expected) ->
    ?assertMatch(#{jsonrpc := <<"2.0">>}, Response),
    ?assertMatch(#{id := _}, Response),
    case Expected of
        {result, ExpectedResult} ->
            ?assertMatch(#{result := ExpectedResult}, Response);
        {error, ExpectedError} ->
            ?assertMatch(#{error := #{code := ExpectedError}}, Response)
    end.
```

### Test Fixtures

**Directory**: `test/fixtures/`

**Files**:
- `mcp_spec_2025-11-25.json` - Official MCP specification JSON
- `valid_requests/` - Valid MCP request examples
  - `initialize.json`
  - `tools_call.json`
  - `resources_read.json`
  - `prompts_get.json`
- `invalid_requests/` - Invalid requests for error testing
  - `invalid_protocol_version.json`
  - `missing_required_field.json`
  - `invalid_method.json`
- `schemas/` - JSON schemas for validation
  - `tool_schema_simple.json`
  - `tool_schema_complex.json`

### Coverage Analysis

**Commands**:
```bash
# Generate coverage for all tests
rebar3 cover --verbose

# Generate HTML report
rebar3 cover
# Open: _build/test/cover/index.html

# Export coverage data
rebar3 coveralls send

# Coverage per app
rebar3 cover --verbose --app erlmcp_core
```

**Coverage Targets**:
- **Overall**: ≥80%
- **Core modules**: ≥85%
  - erlmcp_server.erl
  - erlmcp_client.erl
  - erlmcp_registry.erl
  - erlmcp_json_rpc.erl
  - erlmcp_message_parser.erl
- **Protocol modules**: ≥90%
  - erlmcp_json_rpc.erl
  - erlmcp_capabilities.erl
- **Public APIs**: 100%
  - All exported functions tested

---

## Summary

### Test Statistics (Target)

| Category | Current | Target | Gap |
|----------|---------|--------|-----|
| **EUnit Tests** | 269 | 350+ | +81 |
| **CT Suites** | 33 | 60+ | +27 |
| **Proper Tests** | ~10 | 60+ | +50 |
| **Compliance Tests** | ~5 | 30+ | +25 |
| **Benchmarks** | ~15 | 30+ | +15 |
| **Chaos Tests** | ~5 | 20+ | +15 |
| **Total Test Files** | ~340 | 550+ | +210 |

### Coverage (Target)

| Module Category | Target Coverage |
|-----------------|-----------------|
| Core (server, client, registry) | 90%+ |
| Protocol (JSON-RPC, parsing) | 95%+ |
| Transports | 85%+ |
| Observability | 80%+ |
| Validation | 85%+ |
| **Overall** | **85%+** |

### Quality Gates

✅ **Compile**: errors = 0
✅ **Tests**: failures = 0
✅ **Coverage**: ≥80% overall, ≥85% core
✅ **Dialyzer**: warnings → 0
✅ **Xref**: undefined = ∅
✅ **Performance**: regression < 10%
✅ **Compliance**: 100% MCP method coverage
✅ **Chaos**: All resilience scenarios pass

### Implementation Timeline

- **Phase 1 (Weeks 1-2)**: Foundation, infrastructure, baselines
- **Phase 2 (Weeks 3-6)**: MCP feature coverage (Resources, Tools, Prompts, Sampling)
- **Phase 3 (Weeks 7-8)**: Performance optimization, chaos engineering
- **Phase 4 (Weeks 9-10)**: Coverage improvement, compliance reporting

**Total Duration**: 10 weeks
**Expected Outcome**: Production-ready test suite with 85%+ coverage, 100% MCP compliance

---

**Last Updated**: 2026-02-01
**Version**: 1.0.0
**Status**: Initial Design - Ready for Implementation
