# Comprehensive MCP Specification Testing Strategy
**Version**: 1.0.0
**Date**: 2026-02-02
**Target Version**: erlmcp v3.0.0 (95%+ MCP compliance)
**Current Version**: erlmcp v2.1.0 (65% MCP compliance)
**MCP Spec Version**: 2025-11-25

---

## Executive Summary

**Purpose**: Design comprehensive testing strategy for full MCP specification compliance in erlmcp, following Chicago School TDD principles (real processes, no mocks, state-based verification).

**Status**:
- **Current Testing**: 390 EUnit files, 37 CT suites, 10 Proper tests, 55 benchmarks (76K+ LOC)
- **MCP Compliance**: 65% (42/65 features ≥80%)
- **Target**: 95%+ compliance (62/65 features ≥80%)
- **Coverage**: Current ~75% → Target 85%+ (core modules 90%+)

**Test Count Estimate**:
- **Current**: ~437 test files
- **Target**: 600+ test files
- **Gap**: 163+ new tests needed
- **Breakdown**: 100 EUnit, 30 CT, 20 Proper, 8 Compliance, 5 Performance

**Timeline**: 10 weeks (4 phases)

---

## Table of Contents

1. [Current Test Landscape](#current-test-landscape)
2. [MCP Compliance Gaps](#mcp-compliance-gaps)
3. [Test Taxonomy](#test-taxonomy)
4. [Missing Tests by Category](#missing-tests-by-category)
5. [Test Infrastructure Needs](#test-infrastructure-needs)
6. [Chicago School TDD Requirements](#chicago-school-tdd-requirements)
7. [Implementation Roadmap](#implementation-roadmap)
8. [Quality Gates](#quality-gates)
9. [Performance Baselines](#performance-baselines)
10. [Agent Orchestration](#agent-orchestration)

---

## Current Test Landscape

### Test Statistics (2026-02-02)

| Category | Count | LOC | Coverage | Status |
|----------|-------|-----|----------|--------|
| **EUnit Tests** | 390 | 60K+ | Unit tests | ✅ Comprehensive |
| **CT Suites** | 37 | 12K+ | Integration tests | ✅ Good coverage |
| **Proper Tests** | 10 | 2K+ | Property-based tests | ⚠️ Needs expansion |
| **Benchmarks** | 55 | 8K+ | Performance tests | ✅ Good baselines |
| **Compliance Tests** | ~5 | 1K+ | Spec compliance | ⚠️ Needs expansion |
| **Chaos Tests** | ~8 | 2K+ | Resilience tests | ⚠️ Needs systematization |
| **Total** | **437** | **76K+** | All categories | **65% MCP compliance** |

### Test Distribution by Application

| Application | EUnit | CT | Proper | Coverage | Status |
|-------------|-------|----|----|----------|--------|
| erlmcp_core | 320 | 25 | 8 | 78% | ⚠️ Need 85%+ |
| erlmcp_transports | 45 | 8 | 1 | 75% | ⚠️ Need 80%+ |
| erlmcp_observability | 18 | 3 | 1 | 72% | ⚠️ Need 80%+ |
| erlmcp_validation | 7 | 1 | 0 | 68% | ⚠️ Need 80%+ |

### Current Test Infrastructure

✅ **Existing**:
- Test helpers (`test/common_test/test_helpers.erl`)
- Mock MCP server/client (for integration tests)
- Benchmark framework (55 benchmarks)
- Chaos engineering framework (`erlmcp_chaos` module)
- Property-based testing (Proper integration)
- Coverage reporting (`rebar3 cover`)

⚠️ **Needs Enhancement**:
- MCP specification fixture files (JSON schemas, valid/invalid requests)
- Compliance test template generator
- Performance regression CI integration
- Chaos test automation (systematic failure injection)
- Coverage enforcement in CI (80%+ gate)

---

## MCP Compliance Gaps

### Critical Gaps (From MCP_SPECIFICATION_COMPLIANCE_MATRIX.md)

| Feature | Current | Target | Gap | Priority | Test Count Needed |
|---------|---------|--------|-----|----------|-------------------|
| **Sampling (LLM)** | 18% (2.2/12) | 100% | -82% | P1 | 30+ tests |
| **Tasks API** | 0% (0/8) | 100% | -100% | P0 | 25+ tests |
| **Elicitation** | 1% (0.1/7) | 100% | -99% | P1 | 20+ tests |
| **Security/OAuth** | 26% (2.1/8) | 100% | -74% | P0 | 18+ tests |
| **Completion** | 42% (2.1/5) | 100% | -58% | P1 | 12+ tests |
| **Roots** | 40% (1.2/3) | 100% | -60% | P2 | 8+ tests |
| **Schema Validation** | 67% (3.35/5) | 100% | -33% | P0 | 10+ tests |
| **Metadata/UI** | 23% (0.9/4) | 100% | -77% | P2 | 10+ tests |
| **Transports (SSE)** | 65% (6.5/10) | 100% | -35% | P1 | 12+ tests |
| **Resources** | 82% (8.15/10) | 100% | -18% | P1 | 8+ tests |
| **Tools** | 76% (7.55/10) | 100% | -24% | P0 | 10+ tests |

**Total Missing Tests**: 163+ tests across all categories

---

## Test Taxonomy

### 1. EUnit Tests (Unit Tests) - Chicago School TDD

**Purpose**: Test individual modules with real process dependencies.

**Scope**:
- Protocol encoding/decoding (JSON-RPC, MCP messages)
- gen_server callbacks (init/1, handle_call/3, handle_cast/2)
- Pure functions (validation, formatting, parsing)
- State transitions (client FSM, server FSM, session FSM)
- Error handling (invalid inputs, boundary conditions)

**Chicago School Principles**:
- ✅ Spawn real gen_servers (no mocks)
- ✅ Assert on observable state via API calls
- ✅ Verify behavior (outputs), not implementation (internal calls)
- ❌ No mock objects, no interaction verification

**Example**:
```erlang
-module(erlmcp_sampling_tests).
-include_lib("eunit/include/eunit.hrl").

sampling_test_() ->
    {setup,
     fun() ->
         {ok, Server} = erlmcp_server:start_link(test_server, #{
             sampling => #{enabled => true}
         }),
         Server
     end,
     fun(Server) -> erlmcp_server:stop(Server) end,
     fun(Server) ->
         [
          ?_test(create_message_basic(Server)),
          ?_test(create_message_with_temperature(Server)),
          ?_test(create_message_with_max_tokens(Server)),
          ?_test(create_message_with_model_preferences(Server)),
          ?_test(create_message_streaming(Server))
         ]
     end}.

create_message_basic(Server) ->
    Request = #{
        messages => [#{role => <<"user">>, content => <<"Hello">>}]
    },
    {ok, Response} = erlmcp_server:sampling_create_message(Server, Request),
    ?assertMatch(#{content := [#{text := _}]}, Response).
```

**Current**: 390 files
**Target**: 490 files (+100 new tests)
**Coverage Target**: 90%+ for core modules

---

### 2. Common Test (Integration Tests) - Chicago School TDD

**Purpose**: Test multi-process interactions with real OTP supervision.

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

**Example**:
```erlang
-module(erlmcp_sampling_integration_SUITE).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").

all() ->
    [
     test_sampling_create_message_e2e,
     test_sampling_streaming_e2e,
     test_sampling_provider_anthropic,
     test_sampling_provider_openai,
     test_sampling_concurrent_requests
    ].

init_per_suite(Config) ->
    application:ensure_all_started(erlmcp_core),
    application:ensure_all_started(erlmcp_transports),
    Config.

test_sampling_create_message_e2e(_Config) ->
    %% Setup: Start real server and client
    {ok, Server} = erlmcp_server:start_link(test_server, #{
        sampling => #{enabled => true}
    }),
    {ok, Client} = erlmcp_client:start_link(#{
        transport => {erlmcp_transport_stdio, #{}}
    }),

    %% Initialize
    {ok, _InitResp} = erlmcp_client:initialize(Client, #{
        protocolVersion => <<"2024-11-05">>,
        capabilities => #{sampling => #{}}
    }),

    %% Exercise: Create message
    Request = #{
        messages => [#{role => <<"user">>, content => <<"Hello">>}],
        modelPreferences => #{
            hints => [#{name => <<"claude-3-5-sonnet-20241022">>}]
        },
        temperature => 0.7,
        maxTokens => 1000
    },

    {ok, Response} = erlmcp_client:sampling_create_message(Client, Request),

    %% Verify: Observable state (Chicago School)
    ?assertMatch(#{
        model := _,
        content := [#{type := <<"text">>, text := _}],
        stopReason := _
    }, Response),

    %% Cleanup
    erlmcp_client:stop(Client),
    erlmcp_server:stop(Server).
```

**Current**: 37 suites
**Target**: 67 suites (+30 new suites)
**Coverage Target**: 85%+ for integration scenarios

---

### 3. Property-Based Tests (Proper) - Protocol Invariants

**Purpose**: Test invariants and state machines with generated inputs.

**Scope**:
- JSON-RPC encoding/decoding roundtrips
- MCP protocol message validation
- Request-ID correlation properties
- Resource subscription state machine
- Client FSM properties (state transitions)
- Server FSM properties (lifecycle)
- Concurrency properties (race conditions, ordering)

**Example**:
```erlang
-module(erlmcp_sampling_proper_tests).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

%% Property: Temperature bounds respected
prop_temperature_bounds_test() ->
    ?assert(proper:quickcheck(prop_temperature_bounds(), [{numtests, 1000}])).

prop_temperature_bounds() ->
    ?FORALL(Temperature, proper_types:float(0.0, 2.0),
        begin
            %% Setup: Real server with sampling
            {ok, Server} = erlmcp_server:start_link(test_server, #{
                sampling => #{enabled => true}
            }),

            %% Exercise: Create message with temperature
            Request = #{
                messages => [#{role => <<"user">>, content => <<"test">>}],
                temperature => Temperature
            },

            Result = erlmcp_server:sampling_create_message(Server, Request),
            erlmcp_server:stop(Server),

            %% Verify: Temperature in valid range
            case Result of
                {ok, _Response} -> Temperature >= 0.0 andalso Temperature =< 2.0;
                {error, _} -> Temperature < 0.0 orelse Temperature > 2.0
            end
        end).

%% Property: Max tokens respected
prop_max_tokens_respected() ->
    ?FORALL(MaxTokens, proper_types:pos_integer(),
        begin
            {ok, Server} = erlmcp_server:start_link(test_server, #{
                sampling => #{enabled => true}
            }),

            Request = #{
                messages => [#{role => <<"user">>, content => <<"test">>}],
                maxTokens => MaxTokens
            },

            {ok, Response} = erlmcp_server:sampling_create_message(Server, Request),
            erlmcp_server:stop(Server),

            %% Verify: Response token count <= maxTokens
            #{usage := #{outputTokens := OutputTokens}} = Response,
            OutputTokens =< MaxTokens
        end).
```

**Current**: 10 files
**Target**: 30 files (+20 new properties)
**Properties Target**: 100+ properties total

---

### 4. Compliance Tests (Black-Box Spec Verification)

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

**Example**:
```erlang
-module(erlmcp_sampling_compliance_SUITE).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").

all() ->
    [
     test_sampling_createMessage_signature,
     test_sampling_createMessage_required_fields,
     test_sampling_createMessage_optional_fields,
     test_sampling_error_codes,
     test_sampling_model_preferences,
     test_sampling_streaming_format
    ].

test_sampling_createMessage_signature(_Config) ->
    %% Load MCP spec
    {ok, Spec} = erlmcp_spec_parser:load_spec("test/fixtures/mcp_spec_2025-11-25.json"),
    SamplingSpec = erlmcp_spec_parser:get_method(Spec, <<"sampling/createMessage">>),

    %% Start server
    {ok, Server} = erlmcp_server:start_link(test_server, #{
        sampling => #{enabled => true}
    }),

    %% Test request
    Request = #{
        messages => [#{role => <<"user">>, content => <<"test">>}],
        modelPreferences => #{hints => [#{name => <<"claude">>}]},
        systemPrompt => <<"You are helpful">>,
        includeContext => <<"thisServer">>,
        temperature => 0.7,
        maxTokens => 1000,
        stopSequences => [<<"STOP">>],
        metadata => #{}
    },

    %% Validate request against spec
    ok = erlmcp_spec_parser:validate_request(SamplingSpec, Request),

    %% Execute
    {ok, Response} = erlmcp_server:sampling_create_message(Server, Request),

    %% Validate response against spec
    ok = erlmcp_spec_parser:validate_response(SamplingSpec, Response),

    %% Verify required fields
    ?assertMatch(#{
        role := <<"assistant">>,
        content := #{type := <<"text">>, text := _},
        model := _,
        stopReason := _
    }, Response),

    erlmcp_server:stop(Server).
```

**Current**: ~5 suites
**Target**: 13 suites (+8 new compliance suites)
**Coverage Target**: 100% of MCP methods

---

### 5. Performance Tests (Regression Prevention)

**Purpose**: Prevent performance degradation (P50, P95, P99 latency, throughput).

**Scope**:
- JSON-RPC encoding/decoding latency
- Registry lookup throughput
- Resource read latency
- Tool call latency (with/without schema validation)
- Subscription fan-out performance
- Concurrent client throughput
- Memory usage per connection
- Connection setup time
- Sampling API latency (new)

**Example**:
```erlang
-module(erlmcp_sampling_perf_SUITE).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").

all() ->
    [
     test_sampling_createMessage_latency,
     test_sampling_streaming_latency,
     test_sampling_concurrent_requests
    ].

%% Baseline: sampling/createMessage P95 < 500ms
test_sampling_createMessage_latency(_Config) ->
    {ok, Server} = erlmcp_server:start_link(test_server, #{
        sampling => #{enabled => true}
    }),

    Request = #{
        messages => [#{role => <<"user">>, content => <<"Hello">>}],
        maxTokens => 100
    },

    %% Warmup
    [erlmcp_server:sampling_create_message(Server, Request)
     || _ <- lists:seq(1, 10)],

    %% Benchmark
    Latencies = [begin
        Start = erlang:monotonic_time(millisecond),
        {ok, _} = erlmcp_server:sampling_create_message(Server, Request),
        End = erlang:monotonic_time(millisecond),
        End - Start
    end || _ <- lists:seq(1, 100)],

    %% Calculate percentiles
    Sorted = lists:sort(Latencies),
    P50 = lists:nth(50, Sorted),
    P95 = lists:nth(95, Sorted),
    P99 = lists:nth(99, Sorted),

    ct:pal("Sampling Latency: P50=~pms, P95=~pms, P99=~pms", [P50, P95, P99]),

    %% Verify: P95 < 500ms
    ?assert(P95 < 500),

    erlmcp_server:stop(Server).
```

**Current**: ~55 benchmarks
**Target**: 60 benchmarks (+5 new performance tests)
**Baselines**: P50/P95/P99 for all critical paths

---

### 6. Security Tests (OAuth, Input Validation, Authorization)

**Purpose**: Verify security features and prevent vulnerabilities.

**Scope**:
- OAuth 2.0 authentication flow
- OpenID Connect Discovery
- Incremental scope consent
- HTTP origin validation
- Input validation (injection prevention)
- Authorization checks
- Rate limiting
- TLS/SSL validation

**Example**:
```erlang
-module(erlmcp_security_oauth_SUITE).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").

all() ->
    [
     test_oauth_basic_flow,
     test_oidc_discovery,
     test_incremental_scope_consent,
     test_token_refresh,
     test_invalid_token_rejection,
     test_origin_validation
    ].

test_oauth_basic_flow(_Config) ->
    %% Setup: Start server with OAuth
    {ok, Server} = erlmcp_server:start_link(test_server, #{
        auth => #{
            type => oauth2,
            issuer => <<"https://auth.example.com">>,
            client_id => <<"test_client">>
        }
    }),

    %% Step 1: Client requests authorization
    {ok, AuthUrl} = erlmcp_server:auth_get_authorization_url(Server, #{
        scope => [<<"resources.read">>, <<"tools.call">>],
        redirect_uri => <<"http://localhost:8080/callback">>
    }),

    ?assertMatch(<<"https://auth.example.com/authorize?", _/binary>>, AuthUrl),

    %% Step 2: Simulate authorization code callback
    AuthCode = <<"mock_auth_code_123">>,
    {ok, TokenResponse} = erlmcp_server:auth_exchange_code(Server, AuthCode),

    ?assertMatch(#{
        access_token := _,
        token_type := <<"Bearer">>,
        expires_in := _,
        scope := _
    }, TokenResponse),

    %% Step 3: Use access token for request
    AccessToken = maps:get(access_token, TokenResponse),
    {ok, _Resources} = erlmcp_server:list_resources(Server, #{
        authorization => <<"Bearer ", AccessToken/binary>>
    }),

    erlmcp_server:stop(Server).

test_origin_validation(_Config) ->
    %% Setup: Server with origin whitelist
    {ok, Server} = erlmcp_server:start_link(test_server, #{
        transports => #{
            http => #{
                allowed_origins => [<<"https://trusted.com">>]
            }
        }
    }),

    %% Valid origin
    {ok, _} = erlmcp_server:handle_http_request(Server, #{
        origin => <<"https://trusted.com">>,
        method => <<"POST">>,
        path => <<"/mcp">>
    }),

    %% Invalid origin (should be rejected with 403)
    {error, {403, _}} = erlmcp_server:handle_http_request(Server, #{
        origin => <<"https://malicious.com">>,
        method => <<"POST">>,
        path => <<"/mcp">>
    }),

    erlmcp_server:stop(Server).
```

**Current**: ~10 security tests
**Target**: 28 security tests (+18 new tests)
**Coverage Target**: 100% of security features

---

### 7. Chaos Tests (Resilience Engineering)

**Purpose**: Test system resilience under failures, partitions, resource exhaustion.

**Scope**:
- Process crashes (client, server, transport, registry)
- Supervision tree recovery (restart strategies)
- Network failures (connection drops, timeouts, partitions)
- Resource exhaustion (memory, file descriptors, ETS tables)
- Message queue overload (mailbox flooding)
- Cascading failures (bulkhead pattern verification)
- Split-brain scenarios (distributed registry)

**Example**:
```erlang
-module(erlmcp_sampling_chaos_SUITE).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").

all() ->
    [
     test_sampling_server_crash_recovery,
     test_sampling_provider_failure,
     test_sampling_network_timeout,
     test_sampling_concurrent_crashes
    ].

test_sampling_server_crash_recovery(_Config) ->
    %% Setup: Start server under supervision
    {ok, Sup} = erlmcp_server_sup:start_link(),
    {ok, Server} = supervisor:start_child(Sup, #{
        id => test_server,
        start => {erlmcp_server, start_link, [test_server, #{
            sampling => #{enabled => true}
        }]},
        restart => permanent
    }),

    InitialPid = Server,

    %% Send request
    Request = #{
        messages => [#{role => <<"user">>, content => <<"test">>}]
    },
    {ok, _} = erlmcp_server:sampling_create_message(Server, Request),

    %% Chaos: Kill server mid-request
    exit(Server, kill),
    timer:sleep(500),

    %% Verify: Supervisor restarted server
    {ok, NewPid} = erlmcp_registry:whereis_name({mcp, server, test_server}),
    ?assert(is_pid(NewPid)),
    ?assertNotEqual(InitialPid, NewPid),

    %% Verify: Server functional after restart
    {ok, _} = erlmcp_server:sampling_create_message(NewPid, Request).

test_sampling_provider_failure(_Config) ->
    %% Setup: Server with mock provider that fails
    {ok, Server} = erlmcp_server:start_link(test_server, #{
        sampling => #{
            enabled => true,
            provider => mock_failing_provider
        }
    }),

    %% Exercise: Request should fail gracefully
    Request = #{
        messages => [#{role => <<"user">>, content => <<"test">>}]
    },

    Result = erlmcp_server:sampling_create_message(Server, Request),

    %% Verify: Error returned, no crash
    ?assertMatch({error, #{code := _, message := _}}, Result),
    ?assert(is_process_alive(Server)),

    erlmcp_server:stop(Server).
```

**Current**: ~8 chaos tests
**Target**: 20+ chaos tests (+12 new chaos tests)
**Scenarios**: 50+ failure scenarios

---

## Missing Tests by Category

### Critical Priority (P0) - Blocking for Compliance

#### 1. Tasks API (0% → 100%) - **25 tests needed**

**EUnit Tests** (15):
- `erlmcp_tasks_create_tests.erl` - Task creation, validation
- `erlmcp_tasks_list_tests.erl` - List all tasks, filtering
- `erlmcp_tasks_get_tests.erl` - Get task status, result
- `erlmcp_tasks_cancel_tests.erl` - Task cancellation
- `erlmcp_tasks_persistence_tests.erl` - ETS/DETS storage
- `erlmcp_tasks_expiration_tests.erl` - TTL, cleanup
- `erlmcp_tasks_notification_tests.erl` - Status updates
- `erlmcp_tasks_validation_tests.erl` - Input validation

**CT Suites** (5):
- `erlmcp_tasks_lifecycle_SUITE.erl` - Full task lifecycle
- `erlmcp_tasks_concurrent_SUITE.erl` - Concurrent task execution
- `erlmcp_tasks_persistence_SUITE.erl` - Task persistence across restarts

**Proper Tests** (3):
- `erlmcp_tasks_proper_tests.erl` - Task state machine properties
- Properties: `prop_task_id_unique`, `prop_task_expiration`, `prop_task_status_transitions`

**Compliance Tests** (2):
- `erlmcp_tasks_compliance_SUITE.erl` - MCP Tasks API compliance

---

#### 2. Schema Validation Caching (0% → 100%) - **10 tests needed**

**EUnit Tests** (6):
- `erlmcp_schema_cache_tests.erl` - ETS cache for compiled jesse schemas
- `erlmcp_schema_validation_perf_tests.erl` - Validation performance

**Performance Tests** (4):
- `erlmcp_bench_schema_validation.erl` - Schema validation latency
- Target: 75% latency reduction (20ms → 5ms)

---

#### 3. OAuth 2.0 Enhancements (40% → 100%) - **18 tests needed**

**EUnit Tests** (10):
- `erlmcp_auth_oidc_tests.erl` - OpenID Connect Discovery
- `erlmcp_auth_incremental_scope_tests.erl` - Incremental scope consent
- `erlmcp_auth_client_metadata_tests.erl` - Client ID metadata
- `erlmcp_auth_resource_metadata_tests.erl` - RFC 9728 resource metadata
- `erlmcp_auth_origin_validation_tests.erl` - HTTP origin validation

**CT Suites** (5):
- `erlmcp_auth_oauth_flow_SUITE.erl` - Full OAuth 2.0 flow
- `erlmcp_auth_oidc_discovery_SUITE.erl` - OIDC discovery integration

**Security Tests** (3):
- `erlmcp_security_oauth_SUITE.erl` - OAuth security verification

---

#### 4. Tool Schema Performance (75% → 100%) - **8 tests needed**

**Performance Tests** (8):
- `erlmcp_bench_tool_schema_validation.erl` - Tool call with schema latency
- Target: 5-20ms → 1-5ms (jesse caching)

---

### High Priority (P1) - Core MCP Capability

#### 5. Sampling/LLM Integration (18% → 100%) - **30 tests needed**

**EUnit Tests** (15):
- `erlmcp_sampling_create_message_tests.erl` - Basic message creation
- `erlmcp_sampling_streaming_tests.erl` - SSE/WS streaming
- `erlmcp_sampling_model_preferences_tests.erl` - Model selection
- `erlmcp_sampling_system_prompt_tests.erl` - System prompt handling
- `erlmcp_sampling_temperature_tests.erl` - Temperature parameter
- `erlmcp_sampling_max_tokens_tests.erl` - Max tokens parameter
- `erlmcp_sampling_stop_sequences_tests.erl` - Stop sequences
- `erlmcp_sampling_metadata_tests.erl` - Request metadata
- `erlmcp_sampling_include_context_tests.erl` - Resource context
- `erlmcp_sampling_provider_anthropic_tests.erl` - Anthropic integration
- `erlmcp_sampling_provider_openai_tests.erl` - OpenAI integration
- `erlmcp_sampling_provider_local_tests.erl` - Local LLM integration

**CT Suites** (8):
- `erlmcp_sampling_lifecycle_SUITE.erl` - Full sampling lifecycle
- `erlmcp_sampling_streaming_SUITE.erl` - Streaming integration
- `erlmcp_sampling_providers_SUITE.erl` - All provider integration
- `erlmcp_sampling_concurrent_SUITE.erl` - Concurrent sampling requests

**Proper Tests** (4):
- `erlmcp_sampling_proper_tests.erl` - Sampling properties
- Properties: `prop_temperature_bounds`, `prop_max_tokens_respected`, `prop_streaming_format`

**Compliance Tests** (2):
- `erlmcp_sampling_compliance_SUITE.erl` - MCP Sampling API compliance

**Performance Tests** (1):
- `erlmcp_bench_sampling.erl` - Sampling latency (P95 < 500ms)

---

#### 6. Elicitation API (1% → 100%) - **20 tests needed**

**EUnit Tests** (12):
- `erlmcp_elicitation_create_tests.erl` - Elicitation creation
- `erlmcp_elicitation_complete_tests.erl` - Completion handling
- `erlmcp_elicitation_url_mode_tests.erl` - URL elicitation
- `erlmcp_elicitation_enhanced_enums_tests.erl` - Titled/untitled enums
- `erlmcp_elicitation_multi_select_tests.erl` - Multi-select support
- `erlmcp_elicitation_default_values_tests.erl` - Default value handling
- `erlmcp_elicitation_error_codes_tests.erl` - Error code support

**CT Suites** (5):
- `erlmcp_elicitation_lifecycle_SUITE.erl` - Full elicitation lifecycle
- `erlmcp_elicitation_integration_SUITE.erl` - Client-server integration

**Proper Tests** (2):
- `erlmcp_elicitation_proper_tests.erl` - Elicitation properties

**Compliance Tests** (1):
- `erlmcp_elicitation_compliance_SUITE.erl` - MCP Elicitation API compliance

---

#### 7. Completion API (42% → 100%) - **12 tests needed**

**EUnit Tests** (7):
- `erlmcp_completion_argument_tests.erl` - Argument completion
- `erlmcp_completion_resource_uri_tests.erl` - Resource URI completion
- `erlmcp_completion_ref_tests.erl` - Ref completion
- `erlmcp_completion_context_aware_tests.erl` - Context-aware completion

**CT Suites** (3):
- `erlmcp_completion_integration_SUITE.erl` - Completion integration

**Proper Tests** (1):
- `erlmcp_completion_proper_tests.erl` - Completion properties

**Compliance Tests** (1):
- `erlmcp_completion_compliance_SUITE.erl` - MCP Completion API compliance

---

#### 8. SSE Polling Streams (0% → 100%) - **12 tests needed**

**EUnit Tests** (6):
- `erlmcp_transport_sse_polling_tests.erl` - SSE polling mode
- `erlmcp_transport_sse_resumption_tests.erl` - Stream resumption
- `erlmcp_transport_sse_disconnect_tests.erl` - Server-initiated disconnect

**CT Suites** (4):
- `erlmcp_transport_sse_polling_SUITE.erl` - SSE polling integration

**Performance Tests** (2):
- `erlmcp_bench_sse_polling.erl` - SSE polling latency

---

### Medium Priority (P2) - Enhancement

#### 9. Roots (40% → 100%) - **8 tests needed**

**EUnit Tests** (5):
- `erlmcp_roots_list_tests.erl` - Root list
- `erlmcp_roots_uri_validation_tests.erl` - file:// URI validation

**CT Suites** (2):
- `erlmcp_roots_integration_SUITE.erl` - Roots integration

**Compliance Tests** (1):
- `erlmcp_roots_compliance_SUITE.erl` - MCP Roots API compliance

---

#### 10. Metadata/UI Icons (23% → 100%) - **10 tests needed**

**EUnit Tests** (6):
- `erlmcp_icon_tools_tests.erl` - Tool icon support
- `erlmcp_icon_resources_tests.erl` - Resource icon support
- `erlmcp_icon_prompts_tests.erl` - Prompt icon support
- `erlmcp_icon_cache_enhanced_tests.erl` - Icon caching

**CT Suites** (3):
- `erlmcp_icon_integration_SUITE.erl` - Icon integration

**Compliance Tests** (1):
- `erlmcp_metadata_compliance_SUITE.erl` - MCP Metadata API compliance

---

## Test Infrastructure Needs

### 1. MCP Specification Fixtures

**Purpose**: Provide authoritative MCP spec data for compliance testing.

**Files to Create**:
```
test/fixtures/
├── mcp_spec_2025-11-25.json          # Official MCP spec JSON
├── schemas/
│   ├── json-rpc-2.0.json             # JSON-RPC 2.0 schema
│   ├── mcp-initialize.json           # Initialize method schema
│   ├── mcp-tools-call.json           # tools/call schema
│   ├── mcp-resources-read.json       # resources/read schema
│   ├── mcp-sampling-createMessage.json  # sampling/createMessage schema
│   └── ...
├── valid_requests/
│   ├── initialize.json               # Valid initialize request
│   ├── tools_call.json               # Valid tools/call request
│   ├── resources_read.json           # Valid resources/read request
│   ├── sampling_createMessage.json   # Valid sampling request
│   └── ...
├── invalid_requests/
│   ├── invalid_protocol_version.json # Invalid protocol version
│   ├── missing_required_field.json   # Missing required field
│   ├── invalid_method.json           # Invalid method name
│   ├── invalid_sampling_temperature.json  # Temperature out of bounds
│   └── ...
└── tool_schemas/
    ├── simple_tool.json              # Simple tool with no args
    ├── complex_tool.json             # Complex tool with nested schema
    └── invalid_tool_schema.json      # Invalid tool schema
```

**Implementation**: `erlmcp_spec_parser.erl`
- Load MCP spec from JSON
- Extract method schemas
- Validate requests/responses against schemas
- Generate test fixtures automatically

**Estimate**: 50+ fixture files, 1 parser module

---

### 2. Compliance Test Template Generator

**Purpose**: Auto-generate compliance tests from MCP spec.

**Module**: `erlmcp_compliance_test_generator.erl`

**Features**:
- Parse MCP spec JSON
- Generate CT suite for each MCP method
- Generate test cases for required/optional fields
- Generate test cases for error codes
- Generate validation assertions

**Example Usage**:
```erlang
%% Generate compliance test suite
erlmcp_compliance_test_generator:generate_suite(
    <<"sampling/createMessage">>,
    "test/fixtures/mcp_spec_2025-11-25.json",
    "apps/erlmcp_validation/test/erlmcp_sampling_createMessage_compliance_SUITE.erl"
).
```

**Estimate**: 1 generator module, auto-generates 13 compliance suites

---

### 3. Performance Regression CI Integration

**Purpose**: Detect performance regressions in CI/CD.

**Files to Create**:
```
.github/workflows/
├── performance_regression.yml        # GitHub Actions workflow
└── performance_report.yml            # Performance reporting

scripts/
├── run_benchmarks.sh                 # Run all benchmarks
├── compare_benchmarks.sh             # Compare against baselines
└── generate_performance_report.sh    # Generate HTML report

test/
└── performance_baselines_2026-02-02.json  # Baseline metrics
```

**Workflow**:
1. Run benchmarks on every PR
2. Compare against baselines
3. Fail PR if regression > 10%
4. Generate performance report

**Estimate**: 3 workflows, 3 scripts, 1 baseline file

---

### 4. Chaos Test Automation

**Purpose**: Systematize chaos engineering scenarios.

**Module**: `erlmcp_chaos_scenarios.erl`

**Features**:
- Predefined failure scenarios (50+)
- Automated failure injection
- Recovery verification
- MTTR (Mean Time To Recovery) tracking

**Example**:
```erlang
%% Run all chaos scenarios
erlmcp_chaos_scenarios:run_all([
    server_crash_during_tool_call,
    client_crash_waiting_for_response,
    transport_crash_during_send,
    registry_crash_and_restart,
    network_partition,
    memory_exhaustion,
    ets_table_overflow,
    ...
]).
```

**Estimate**: 1 scenario module, 50+ scenario definitions

---

### 5. Coverage Enforcement CI Gate

**Purpose**: Enforce 80%+ coverage in CI/CD.

**Files to Create**:
```
.github/workflows/
└── coverage_enforcement.yml          # Coverage gate workflow

scripts/
├── check_coverage.sh                 # Check coverage thresholds
└── generate_coverage_report.sh       # Generate HTML report
```

**Workflow**:
1. Run `rebar3 cover` on every PR
2. Fail PR if coverage < 80% (overall)
3. Fail PR if core module < 85%
4. Generate coverage report

**Estimate**: 1 workflow, 2 scripts

---

### 6. Test Helpers Enhancement

**Module**: `test/common_test/test_helpers.erl`

**New Functions Needed**:
```erlang
%% Sampling helpers
start_sampling_server(Config) -> {ok, Server}.
sampling_create_message(Server, Request) -> {ok, Response}.
assert_sampling_response(Response, Expected) -> ok.

%% Tasks helpers
start_tasks_server(Config) -> {ok, Server}.
tasks_create(Server, Task) -> {ok, TaskId}.
wait_for_task_completion(Server, TaskId, Timeout) -> {ok, Result}.

%% OAuth helpers
start_oauth_server(Config) -> {ok, Server}.
get_oauth_token(Server, Scope) -> {ok, Token}.
assert_oauth_authorized(Response) -> ok.

%% Chaos helpers
inject_failure(Type, Target, Opts) -> ok.
wait_for_recovery(Target, Timeout) -> ok.
assert_supervision_restart(Pid) -> ok.

%% Performance helpers
benchmark(Fun, Iterations) -> {P50, P95, P99}.
assert_latency(Latency, MaxMs) -> ok.
assert_throughput(Throughput, MinOps) -> ok.
```

**Estimate**: 50+ new helper functions

---

## Chicago School TDD Requirements

### Core Tenets (Mandatory for All Tests)

1. **Real Collaborators** - Spawn actual gen_servers, not mocks
2. **State-Based Verification** - Assert on observable state via API calls
3. **Behavior Testing** - Verify what system does (outputs), not how it does it (internals)
4. **Integration Over Isolation** - Test components together when practical

### Anti-Patterns (What NOT to Do)

#### ❌ Don't Mock gen_servers
```erlang
%% BAD (London School mocking)
meck:new(erlmcp_server),
meck:expect(erlmcp_server, sampling_create_message, fun(_, _) -> {ok, mock_response} end).
```

#### ✅ Do Use Real gen_servers
```erlang
%% GOOD (Chicago School)
{ok, Server} = erlmcp_server:start_link(test_server, #{sampling => #{enabled => true}}),
{ok, Response} = erlmcp_server:sampling_create_message(Server, Request).
```

---

#### ❌ Don't Verify Internal Calls
```erlang
%% BAD (London School interaction verification)
?assertMatch({call, handle_call, [sampling_create_message, _, _]}, meck:history(erlmcp_server)).
```

#### ✅ Do Verify Observable State
```erlang
%% GOOD (Chicago School state verification)
{ok, Response} = erlmcp_server:sampling_create_message(Server, Request),
?assertMatch(#{content := [#{text := _}], model := _, stopReason := _}, Response).
```

---

#### ❌ Don't Mock Collaborators
```erlang
%% BAD (London School)
meck:new(erlmcp_registry),
meck:expect(erlmcp_registry, whereis_name, fun(_) -> {ok, fake_pid} end).
```

#### ✅ Do Use Real Collaborators
```erlang
%% GOOD (Chicago School)
application:ensure_all_started(erlmcp_core),  %% Real registry under supervision
{ok, Server} = erlmcp_server:start_link(test_server, #{}),
{ok, ServerPid} = erlmcp_registry:whereis_name({mcp, server, test_server}).
```

---

### Verification Checklist (Pre-Completion)

Before marking any test complete, verify:

- [ ] ✅ Compile clean: `rebar3 compile` (errors = 0)
- [ ] ✅ Tests pass: `rebar3 eunit --module=<module>_tests` (failures = 0)
- [ ] ✅ Coverage: `rebar3 cover` (≥80% overall, ≥85% core)
- [ ] ✅ Chicago School TDD: Real processes ✅, State-based assertions ✅, No mocks ✅
- [ ] ✅ Format: `rebar3 format --verify` (formatting correct)

---

## Implementation Roadmap

### Phase 1: Foundation (Weeks 1-2)

**Goals**:
- Create test infrastructure
- Establish performance baselines
- Organize existing tests

**Tasks**:
1. **Create MCP Specification Fixtures** (3 days)
   - Download official MCP spec JSON
   - Create 50+ fixture files (valid/invalid requests)
   - Implement `erlmcp_spec_parser.erl`

2. **Establish Performance Baselines** (2 days)
   - Run all 55 existing benchmarks
   - Document current performance (P50, P95, P99)
   - Create `test/performance_baselines_2026-02-02.json`

3. **Create Test Infrastructure** (3 days)
   - Enhance `test/common_test/test_helpers.erl` (50+ new functions)
   - Create compliance test template generator
   - Set up chaos test automation framework

4. **Organize Existing Tests** (2 days)
   - Verify 390 EUnit tests follow Chicago School TDD
   - Verify 37 CT suites have proper setup/teardown
   - Document coverage gaps

**Deliverables**:
- ✅ MCP spec fixtures (50+ files)
- ✅ Performance baselines documented
- ✅ Enhanced test infrastructure
- ✅ Existing tests organized and documented

**Agent Assignment**:
- **erlang-researcher**: Analyze existing test patterns
- **erlang-architect**: Design test infrastructure
- **erlang-test-engineer**: Create fixtures and helpers
- **erlang-performance**: Run benchmarks, document baselines

---

### Phase 2: Critical Features (Weeks 3-6)

**Goals**:
- Implement tests for P0 features (Tasks, Schema, OAuth, Tools)
- Achieve 75% MCP compliance

**Tasks**:

#### Week 3: Tasks API (0% → 100%)
- **erlang-otp-developer**: Implement Tasks API feature
- **erlang-test-engineer**: Create 25 tests (15 EUnit, 5 CT, 3 Proper, 2 Compliance)
- **verifier**: Run tests, verify coverage ≥85%

#### Week 4: Schema Validation Caching (0% → 100%)
- **erlang-performance**: Implement jesse schema caching (ETS)
- **erlang-test-engineer**: Create 10 tests (6 EUnit, 4 Performance)
- **verifier**: Verify 75% latency reduction (20ms → 5ms)

#### Week 5: OAuth 2.0 Enhancements (40% → 100%)
- **erlang-otp-developer**: Implement OIDC Discovery, incremental scope, origin validation
- **erlang-test-engineer**: Create 18 tests (10 EUnit, 5 CT, 3 Security)
- **verifier**: Run security tests, verify 100% coverage

#### Week 6: Tool Schema Performance (75% → 100%)
- **erlang-performance**: Optimize tool schema validation
- **erlang-test-engineer**: Create 8 performance tests
- **verifier**: Verify P95 < 5ms

**Deliverables**:
- ✅ Tasks API: 25 tests ✅
- ✅ Schema caching: 10 tests ✅, 75% latency reduction ✅
- ✅ OAuth: 18 tests ✅, 100% security coverage ✅
- ✅ Tools performance: 8 tests ✅, P95 < 5ms ✅
- ✅ MCP Compliance: 75% (49/65 features ≥80%)

**Agent Assignment**:
- **erlang-otp-developer**: Feature implementation (4 parallel tasks)
- **erlang-test-engineer**: Test creation (4 parallel tasks)
- **erlang-performance**: Performance optimization (2 parallel tasks)
- **verifier**: Test execution and verification (continuous)
- **code-reviewer**: Chicago School TDD review (continuous)

---

### Phase 3: High Priority Features (Weeks 7-10)

**Goals**:
- Implement tests for P1 features (Sampling, Elicitation, Completion, SSE)
- Achieve 90% MCP compliance

**Tasks**:

#### Week 7: Sampling/LLM Integration (18% → 100%)
- **erlang-otp-developer**: Implement full sampling API (streaming, model preferences, etc.)
- **erlang-test-engineer**: Create 30 tests (15 EUnit, 8 CT, 4 Proper, 2 Compliance, 1 Perf)
- **verifier**: Verify P95 < 500ms

#### Week 8: Elicitation API (1% → 100%)
- **erlang-otp-developer**: Implement elicitation API (URL mode, enhanced enums, defaults)
- **erlang-test-engineer**: Create 20 tests (12 EUnit, 5 CT, 2 Proper, 1 Compliance)
- **verifier**: Run tests, verify coverage ≥85%

#### Week 9: Completion API (42% → 100%)
- **erlang-otp-developer**: Implement context-aware completion, ref support
- **erlang-test-engineer**: Create 12 tests (7 EUnit, 3 CT, 1 Proper, 1 Compliance)
- **verifier**: Run tests, verify coverage ≥85%

#### Week 10: SSE Polling Streams (0% → 100%)
- **erlang-transport-builder**: Implement SSE polling, resumption, disconnect
- **erlang-test-engineer**: Create 12 tests (6 EUnit, 4 CT, 2 Performance)
- **verifier**: Verify latency targets met

**Deliverables**:
- ✅ Sampling: 30 tests ✅, P95 < 500ms ✅
- ✅ Elicitation: 20 tests ✅
- ✅ Completion: 12 tests ✅
- ✅ SSE Polling: 12 tests ✅
- ✅ MCP Compliance: 90% (58/65 features ≥80%)

**Agent Assignment**:
- **erlang-otp-developer**: Feature implementation (3 parallel tasks)
- **erlang-transport-builder**: SSE implementation (1 task)
- **erlang-test-engineer**: Test creation (4 parallel tasks)
- **erlang-performance**: Performance testing (2 parallel tasks)
- **verifier**: Test execution and verification (continuous)
- **code-reviewer**: Chicago School TDD review (continuous)

---

### Phase 4: Final Polish & Medium Priority (Weeks 11-12)

**Goals**:
- Implement tests for P2 features (Roots, Icons)
- Achieve 95%+ MCP compliance
- Set up CI integration

**Tasks**:

#### Week 11: Roots & Icons (40% → 100%, 23% → 100%)
- **erlang-otp-developer**: Implement roots URI validation, icon support
- **erlang-test-engineer**: Create 18 tests (11 EUnit, 5 CT, 2 Compliance)
- **verifier**: Run tests, verify coverage ≥80%

#### Week 12: CI Integration & Documentation
- **build-engineer**: Set up performance regression CI, coverage enforcement CI
- **erlang-test-engineer**: Generate compliance reports (HTML, JSON)
- **code-reviewer**: Final review, documentation

**Deliverables**:
- ✅ Roots: 8 tests ✅
- ✅ Icons: 10 tests ✅
- ✅ CI Integration: Performance regression ✅, Coverage enforcement ✅
- ✅ Compliance Reports: HTML ✅, JSON ✅
- ✅ MCP Compliance: 95%+ (62/65 features ≥80%)
- ✅ Coverage: 85%+ overall, 90%+ core

**Agent Assignment**:
- **erlang-otp-developer**: Feature implementation (2 parallel tasks)
- **erlang-test-engineer**: Test creation (2 parallel tasks)
- **build-engineer**: CI setup (2 parallel tasks)
- **verifier**: Final verification (continuous)
- **code-reviewer**: Final review and documentation (1 task)

---

### Roadmap Summary

| Phase | Duration | Goals | Tests Added | Compliance Target | Agent Load |
|-------|----------|-------|-------------|-------------------|------------|
| **1. Foundation** | Weeks 1-2 | Infrastructure, baselines | 0 | 65% | 4 agents |
| **2. Critical (P0)** | Weeks 3-6 | Tasks, Schema, OAuth, Tools | 61 | 75% | 6 agents |
| **3. High (P1)** | Weeks 7-10 | Sampling, Elicitation, Completion, SSE | 74 | 90% | 7 agents |
| **4. Final (P2)** | Weeks 11-12 | Roots, Icons, CI | 18 | 95%+ | 5 agents |
| **Total** | 12 weeks | 95%+ compliance | **153** | **95%+** | 7 max parallel |

---

## Quality Gates

### Pre-Commit Gates (Local Development)

1. **Compile** - `TERM=dumb rebar3 compile` (errors = 0)
2. **Format** - `rebar3 format --verify` (formatting correct)
3. **EUnit** - `rebar3 eunit --module=<changed_module>_tests` (failures = 0)

### Pull Request Gates (CI/CD)

1. **All Tests** - `rebar3 do eunit, ct, proper -c` (failures = 0)
2. **Coverage** - `rebar3 cover` (≥80% overall, ≥85% core modules)
3. **Dialyzer** - `rebar3 dialyzer` (warnings → 0)
4. **Xref** - `rebar3 xref` (undefined functions = ∅)
5. **Performance** - No regression > 10%
6. **Chicago School TDD** - Real processes ✅, State-based assertions ✅, No mocks ✅

### Release Gates (Production)

1. **Full Test Suite** - `make check` (all gates pass)
2. **Compliance Report** - 100% MCP method coverage
3. **Performance Report** - All baselines met
4. **Chaos Tests** - All resilience scenarios pass
5. **Coverage Report** - ≥85% overall, ≥90% core
6. **Security Tests** - All security features verified

---

## Performance Baselines

**Source**: `docs/MCP_SPEC_PERFORMANCE_ANALYSIS.md` (Jan 2026)

### Current Baselines (2026-02-02)

| Metric | Current | Target | Status |
|--------|---------|--------|--------|
| **Registry Throughput** | 553K msg/s | >500K | ✅ PASS |
| **Queue Operations** | 971K ops/s | >900K | ✅ PASS |
| **JSON-RPC Encoding (P50)** | 0.5-1.0ms | <1ms | ✅ PASS |
| **JSON-RPC Encoding (P95)** | 2-5ms | <5ms | ✅ PASS |
| **Tool Call (no schema, P50)** | 1-2ms | <5ms | ✅ PASS |
| **Tool Call (no schema, P95)** | 5-10ms | <20ms | ✅ PASS |
| **Tool Call (with schema, P50)** | 5-10ms | <10ms | ⚠️ MARGINAL |
| **Tool Call (with schema, P95)** | 20-30ms | <50ms | ✅ PASS |
| **Resource Read (P50)** | <5ms | <10ms | ✅ PASS |
| **Subscription Fan-out (100)** | <5ms/notification | <10ms | ✅ PASS |
| **Concurrent Connections** | 40-50K/node | >10K | ✅ PASS |

### New Baselines to Establish

| Metric | Target | Test File |
|--------|--------|-----------|
| **Sampling createMessage (P50)** | <200ms | `erlmcp_bench_sampling.erl` |
| **Sampling createMessage (P95)** | <500ms | `erlmcp_bench_sampling.erl` |
| **Sampling streaming (first token)** | <300ms | `erlmcp_bench_sampling.erl` |
| **Tasks create (P50)** | <5ms | `erlmcp_bench_tasks.erl` |
| **Tasks list (P50)** | <10ms | `erlmcp_bench_tasks.erl` |
| **Elicitation create (P50)** | <5ms | `erlmcp_bench_elicitation.erl` |
| **Completion complete (P50)** | <20ms | `erlmcp_bench_completion.erl` |
| **Schema validation (cached, P50)** | <1ms | `erlmcp_bench_schema_validation.erl` |
| **Schema validation (cached, P95)** | <5ms | `erlmcp_bench_schema_validation.erl` |

---

## Agent Orchestration

### Agent Workflow (EPIC 9 - Parallel Execution)

**Trigger**: Multi-feature testing (5+ test files, 3+ MCP features)

**Phases**:
1. **Fan-out** - Spawn multiple test agents in parallel
2. **Independent Construction** - Each agent works on separate test category
3. **Collision Detection** - Merge tests, detect conflicts
4. **Convergence** - Consolidate test suites
5. **Refactoring** - Extract common test helpers
6. **Closure** - Run all tests, generate reports

**Expected Speedup**: 2.8x - 4.4x

### Example: Sampling Feature Test Generation (1 Message)

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
- ✅ `apps/erlmcp_core/test/erlmcp_sampling_tests.erl` (EUnit)
- ✅ `apps/erlmcp_core/test/erlmcp_sampling_SUITE.erl` (CT)
- ✅ `apps/erlmcp_core/test/erlmcp_sampling_proper_tests.erl` (Proper)
- ✅ `apps/erlmcp_validation/test/erlmcp_sampling_compliance_SUITE.erl` (Compliance)
- ✅ `bench/erlmcp_bench_sampling.erl` (Benchmark)
- ✅ `apps/erlmcp_observability/test/erlmcp_sampling_chaos_SUITE.erl` (Chaos)
- ✅ Coverage: 85%+
- ✅ Chicago School TDD: ✅ (verified by code-reviewer)

---

## Summary

### Test Count Estimate

| Category | Current | Target | Gap | Priority Distribution |
|----------|---------|--------|-----|------------------------|
| **EUnit Tests** | 390 | 490 | +100 | P0: 41, P1: 46, P2: 13 |
| **CT Suites** | 37 | 67 | +30 | P0: 12, P1: 14, P2: 4 |
| **Proper Tests** | 10 | 30 | +20 | P0: 3, P1: 15, P2: 2 |
| **Compliance Tests** | ~5 | 13 | +8 | P0: 2, P1: 5, P2: 1 |
| **Performance Tests** | 55 | 60 | +5 | P0: 4, P1: 1, P2: 0 |
| **Total** | **437** | **600+** | **163** | **P0: 62, P1: 81, P2: 20** |

### Coverage Targets

| Module Category | Current | Target | Gap |
|-----------------|---------|--------|-----|
| Core (server, client, registry) | 78% | 90%+ | +12% |
| Protocol (JSON-RPC, parsing) | 82% | 95%+ | +13% |
| Transports | 75% | 85%+ | +10% |
| Observability | 72% | 80%+ | +8% |
| Validation | 68% | 80%+ | +12% |
| **Overall** | **75%** | **85%+** | **+10%** |

### MCP Compliance Progression

| Version | Compliance | Features ≥80% | Timeline |
|---------|------------|---------------|----------|
| **v2.1.0 (Current)** | 65% | 42/65 | 2026-02-02 |
| **v2.2.0 (Phase 2)** | 75% | 49/65 | Week 6 |
| **v2.3.0 (Phase 3)** | 90% | 58/65 | Week 10 |
| **v3.0.0 (Phase 4)** | 95%+ | 62/65 | Week 12 |

### Quality Gates Summary

✅ **Compile**: errors = 0
✅ **Tests**: failures = 0
✅ **Coverage**: ≥80% overall, ≥85% core
✅ **Dialyzer**: warnings → 0
✅ **Xref**: undefined = ∅
✅ **Performance**: regression < 10%
✅ **Compliance**: 100% MCP method coverage
✅ **Chaos**: All resilience scenarios pass
✅ **Chicago School TDD**: Real processes ✅, State-based assertions ✅, No mocks ✅

---

**Last Updated**: 2026-02-02
**Version**: 1.0.0
**Status**: Ready for Implementation
**Total Estimated Effort**: 12 weeks, 163 new tests, 95%+ MCP compliance
