# Quality Standards - Compliance Requirements

**Version**: 2.1.0
**Standard**: Lean Six Sigma (99.99966% defect-free)

## Compliance Framework

```mermaid
graph TB
    subgraph "Code Quality Standards"
        CQ1[Type Coverage 100%]
        CQ2[No Suppressions]
        CQ3[Docstrings Complete]
        CQ4[Modular Design]
        CQ5[Error Handling]
    end

    subgraph "Testing Standards"
        T1[Chicago School TDD]
        T2[No Mocks]
        T3[Black-Box Testing]
        T4[Property-Based]
        T5[80%+ Coverage]
    end

    subgraph "OTP Standards"
        O1[Behavior Compliance]
        O2[Supervision Trees]
        O3[Let-It-Crash]
        O4[Monitoring]
        O5[Hot Reload]
    end

    subgraph "Security Standards"
        S1[Input Validation]
        S2[Auth/Authz]
        S3[Rate Limiting]
        S4[Circuit Breakers]
        S5[Audit Logging]
    end

    subgraph "Performance Standards"
        P1[No Regressions]
        P2[Benchmarked]
        P3[Optimized]
        P4[Measured]
        P5[Documented]
    end

    CQ1 --> COMPLIANCE{Compliance Check}
    CQ2 --> COMPLIANCE
    CQ3 --> COMPLIANCE
    CQ4 --> COMPLIANCE
    CQ5 --> COMPLIANCE

    T1 --> COMPLIANCE
    T2 --> COMPLIANCE
    T3 --> COMPLIANCE
    T4 --> COMPLIANCE
    T5 --> COMPLIANCE

    O1 --> COMPLIANCE
    O2 --> COMPLIANCE
    O3 --> COMPLIANCE
    O4 --> COMPLIANCE
    O5 --> COMPLIANCE

    S1 --> COMPLIANCE
    S2 --> COMPLIANCE
    S3 --> COMPLIANCE
    S4 --> COMPLIANCE
    S5 --> COMPLIANCE

    P1 --> COMPLIANCE
    P2 --> COMPLIANCE
    P3 --> COMPLIANCE
    P4 --> COMPLIANCE
    P5 --> COMPLIANCE

    COMPLIANCE --> PASS[✅ PASS]
    COMPLIANCE --> FAIL[❌ FAIL]

    style PASS fill:#90EE90
    style FAIL fill:#FFB6C1
```

## Code Quality Standards

### 1. Type Coverage (100%)

```mermaid
graph TD
    START[Code Review] --> CHECK1{All functions<br/>typed?}
    CHECK1 -->|No| FAIL1[❌ FAIL]
    CHECK1 -->|Yes| CHECK2{All parameters<br/>typed?}
    CHECK2 -->|No| FAIL2[❌ FAIL]
    CHECK2 -->|Yes| CHECK3{Return types<br/>specified?}
    CHECK3 -->|No| FAIL3[❌ FAIL]
    CHECK3 -->|Yes| CHECK4{Records<br/>typed?}
    CHECK4 -->|No| FAIL4[❌ FAIL]
    CHECK4 -->|Yes| PASS[✅ PASS]

    style FAIL1 fill:#FFB6C1
    style FAIL2 fill:#FFB6C1
    style FAIL3 fill:#FFB6C1
    style FAIL4 fill:#FFB6C1
    style PASS fill:#90EE90
```

**Requirement**: Every function must have complete type specifications.

**Example**:

```erlang
%% ✅ CORRECT: Complete type specification
-spec init(Args :: term()) -> {ok, State :: #state{}} |
                               {stop, Reason :: term()}.
init(Args) ->
    State = #state{},
    {ok, State}.

%% ❌ WRONG: Missing type specification
init(Args) ->
    State = #state{},
    {ok, State}.
```

**Enforcement**: Blocking (pre-commit + CI)

### 2. No Suppression Comments

```mermaid
graph TD
    START[Scan Code] --> SEARCH{Search for<br/>suppressions}
    SEARCH -->|Found| CHECK1{Justified<br/>in commit?}
    CHECK1 -->|No| FAIL1[❌ FAIL]
    CHECK1 -->|Yes| CHECK2{Tracked in<br/>issue tracker?}
    CHECK2 -->|No| FAIL2[❌ FAIL]
    CHECK2 -->|Yes| WARN[⚠️ ACCEPTED]
    SEARCH -->|Not Found| PASS[✅ PASS]

    style FAIL1 fill:#FFB6C1
    style FAIL2 fill:#FFB6C1
    style WARN fill:#FFD700
    style PASS fill:#90EE90
```

**Prohibited**:
- `%% cover: COMPLIENT suppressed` (no justification)
- `-dialyzer({nowarn_function, foo/1}).` (no explanation)
- Any blanket suppression without documented reason

**Allowed** (with justification):
- Documented in commit message
- Tracked in GitHub issue
- Time-limited exception

**Enforcement**: Blocking (pre-commit + CI)

### 3. Complete Docstrings

```mermaid
graph TD
    START[Check Module] --> CHECK1{Module<br/>docstring?}
    CHECK1 -->|No| FAIL1[❌ FAIL]
    CHECK1 -->|Yes| CHECK2{Public API<br/>docstrings?}
    CHECK2 -->|No| FAIL2[❌ FAIL]
    CHECK2 -->|Yes| CHECK3{Format<br/>correct?}
    CHECK3 -->|No| WARN[⚠️ FIX FORMAT]
    CHECK3 -->|Yes| PASS[✅ PASS]

    style FAIL1 fill:#FFB6C1
    style FAIL2 fill:#FFB6C1
    style WARN fill:#FFD700
    style PASS fill:#90EE90
```

**Requirement**: All public APIs must have NumPy-style docstrings.

**Example**:

```erlang
%% ✅ CORRECT: Complete docstring
%% @doc Handle incoming JSON-RPC request.
%%
%% Parses and validates the incoming JSON-RPC 2.0 request, performs
%% the requested operation, and returns the response.
%%
%% Parameters:
%%   - Request: Binary JSON-RPC request
%%   - State: Server state record
%%
%% Returns:
%%   - {ok, Response, State'}: Success with response
%%   - {error, Reason, State}: Error with reason
%%
%% Example:
%%   <<'{"jsonrpc":"2.0","method":"tools/list","id":1}'>>
%%
%% @end
-spec handle_request(Request :: binary(), State :: #state{}) ->
    {ok, Response :: binary(), State :: #state{}} |
    {error, Reason :: term(), State :: #state{}}.

%% ❌ WRONG: Missing docstring
-spec handle_request(Request :: binary(), State :: #state{}) ->
    {ok, Response :: binary(), State :: #state{}}.
```

**Enforcement**: Advisory (manual review)

### 4. Modular Design

```mermaid
graph TD
    START[Review Module] --> CHECK1{Lines ≤ 500?}
    CHECK1 -->|No| FAIL1[❌ FAIL]
    CHECK1 -->|Yes| CHECK2{Functions<br/>cohesive?}
    CHECK2 -->|No| WARN[⚠️ REFACTOR]
    CHECK2 -->|Yes| CHECK3{Single<br/>responsibility?}
    CHECK3 -->|No| WARN2[⚠️ SPLIT MODULE]
    CHECK3 -->|Yes| PASS[✅ PASS]

    style FAIL1 fill:#FFB6C1
    style WARN fill:#FFD700
    style WARN2 fill:#FFD700
    style PASS fill:#90EE90
```

**Requirements**:
- Files under 500 lines
- Single responsibility principle
- High cohesion, low coupling
- Clear module boundaries

**Enforcement**: Advisory (code review)

### 5. Comprehensive Error Handling

```mermaid
graph TD
    START[Check Code Paths] --> CHECK1{All errors<br/>handled?}
    CHECK1 -->|No| FAIL1[❌ FAIL]
    CHECK1 -->|Yes| CHECK2{All clauses<br/>covered?}
    CHECK2 -->|No| FAIL2[❌ FAIL]
    CHECK2 -->|Yes| CHECK3{Logging<br/>present?}
    CHECK3 -->|No| WARN[⚠️ ADD LOGGING]
    CHECK3 -->|Yes| PASS[✅ PASS]

    style FAIL1 fill:#FFB6C1
    style FAIL2 fill:#FFB6C1
    style WARN fill:#FFD700
    style PASS fill:#90EE90
```

**Requirements**:
- All code paths handle errors
- No catch-all clauses without logging
- Error messages are actionable
- Failures are visible (andon principle)

**Enforcement**: Blocking (code review + tests)

---

## Testing Standards

### Chicago School TDD

```mermaid
graph LR
    subgraph "Traditional TDD"
        T1[Write Test]
        T2[Write Code]
        T3[Refactor]
    end

    subgraph "Chicago School TDD"
        C1[Study Requirements]
        C2[Write Test]
        C3[Run Test → FAIL]
        C4[Make Test Pass]
        C5[Refactor]
        C6[Verify Behavior]
    end

    T1 --> T2 --> T3
    C1 --> C2 --> C3 --> C4 --> C5 --> C6

    style T1 fill:#87CEEB
    style T2 fill:#87CEEB
    style T3 fill:#87CEEB
    style C1 fill:#90EE90
    style C2 fill:#90EE90
    style C3 fill:#90EE90
    style C4 fill:#90EE90
    style C5 fill:#90EE90
    style C6 fill:#90EE90
```

**Principles**:
1. Tests drive behavior (not implementation)
2. Real collaborators (no mocks)
3. State-based assertions (not interaction verification)
4. Integration tests preferred over unit isolation
5. Black-box testing of observable behavior

**Example**:

```erlang
%% ✅ CORRECT: Chicago School TDD
%% Tests observable behavior
resource_subscription_test() ->
    %% Given: Real server process
    {ok, Server} = erlmcp_server:start_link([]),

    %% When: Subscribe to resource
    {ok, SubscriptionId} = erlmcp_server:subscribe_resource(
        Server,
        <<"test://resource">>,
        self()
    ),

    %% Then: Verify observable behavior (state change)
    receive
        {resource_updated, <<"test://resource">>, Data} ->
            ?assertEqual(<<"expected">>, Data)
    after 1000 ->
        ?assert(false, "Timeout waiting for resource update")
    end.

%% ❌ WRONG: Mock-based, tests implementation
resource_subscription_test_wrong() ->
    %% Mocks internal state
    meck:new(erlmcp_registry),
    meck:expect(erlmcp_registry, register, fun(_) -> ok end),

    %% Verifies interaction, not behavior
    ?assert(meck:called(erlmcp_registry, register, '_')),

    meck:unload(erlmcp_registry).
```

**Enforcement**: Mandatory (all new code)

### No Mocks, Fakes, or Placeholders

```mermaid
graph TD
    START[Test Review] --> CHECK1{Uses real<br/>processes?}
    CHECK1 -->|No| FAIL1[❌ FAIL - Mocks detected]
    CHECK1 -->|Yes| CHECK2{Uses real<br/>transports?}
    CHECK2 -->|No| FAIL2[❌ FAIL - Fakes detected]
    CHECK2 -->|Yes| CHECK3{Uses real<br/>collaborators?}
    CHECK3 -->|No| FAIL3[❌ FAIL - Placeholders]
    CHECK3 -->|Yes| PASS[✅ PASS]

    style FAIL1 fill:#FFB6C1
    style FAIL2 fill:#FFB6C1
    style FAIL3 fill:#FFB6C1
    style PASS fill:#90EE90
```

**Prohibited**:
- `meck` for mocking modules
- Mock processes for gen_servers
- Stub implementations
- Placeholder "TODO" code

**Required**:
- Real erlmcp processes
- Real transport instances
- Real network sockets (where applicable)
- Actual file I/O (for persistence tests)

**Enforcement**: Mandatory (code review)

### Black-Box Testing

```mermaid
graph TB
    subgraph "Black-Box Testing"
        BB1[Observe Inputs]
        BB2[Observe Outputs]
        BB3[Verify Behavior]
    end

    subgraph "White-Box Testing (❌ PROHIBITED)"
        WB1[Inspect Internal State]
        WB2[Verify Implementation]
        WB3[Test Code Paths]
    end

    BB1 --> BB2 --> BB3
    WB1 --> WB2 --> WB3

    style BB1 fill:#90EE90
    style BB2 fill:#90EE90
    style BB3 fill:#90EE90
    style WB1 fill:#FFB6C1
    style WB2 fill:#FFB6C1
    style WB3 fill:#FFB6C1
```

**Principle**: Test WHAT the system does, not HOW it does it.

**Example**:

```erlang
%% ✅ CORRECT: Black-box testing
server_request_test() ->
    %% Given: Real server
    {ok, Server} = erlmcp_server:start_link([]),

    %% When: Send request
    Request = <<'{"jsonrpc":"2.0","method":"tools/list","id":1}'>>,
    {ok, Response} = erlmcp_server:handle_request(Server, Request),

    %% Then: Verify observable behavior (output)
    ?assertMatch(
        #{<<"jsonrpc">> := <<"2.0">>,
          <<"result">> := _,
          <<"id">> := 1},
        jesse:decode(Response, [])
    ).

%% ❌ WRONG: White-box testing
server_request_test_wrong() ->
    %% Inspects internal state (implementation detail)
    {ok, Server} = erlmcp_server:start_link([]),
    State = sys:get_state(Server),

    %% Verifies internal structure (not observable behavior)
    ?assertEqual([], State#state.tools),

    %% Violates encapsulation
    ?assertEqual(undefined, State#state.transport_module).
```

**Enforcement**: Mandatory (code review)

### Property-Based Testing

```mermaid
graph TD
    START[Function Review] --> CHECK1{Has complex<br/>logic?}
    CHECK1 -->|Yes| ADD[Add PropEr test]
    CHECK1 -->|No| CHECK2{Has multiple<br/>code paths?}
    CHECK2 -->|Yes| ADD
    CHECK2 -->|No| SKIP[Standard tests OK]
    ADD --> WRITE[Write property]
    WRITE --> VERIFY[Verify property holds]
    VERIFY --> PASS[✅ PASS]

    style ADD fill:#90EE90
    style PASS fill:#90EE90
    style SKIP fill:#87CEEB
```

**Requirements**:
- Complex logic must have property-based tests
- Use PropEr for generative testing
- Verify invariants across all inputs

**Example**:

```erlang
%% Property: Round-trip encoding/decoding
prop_json_rpc_roundtrip() ->
    ?FORALL(Request, json_rpc_request(),
        begin
            Encoded = erlmcp_json_rpc:encode(Request),
            {ok, Decoded} = erlmcp_json_rpc:decode(Encoded),
            Request =:= Decoded
        end
    ).

%% Property: Monotonic request IDs
prop_request_id_monotonic() ->
    ?FORALL(_Requests, list({call, erlmcp_client, new_request_id, []}),
        begin
            Ids = [Id || {ok, #{id := Id}} <- _Requests],
            lists:sort(Ids) =:= Ids
        end
    ).
```

**Enforcement**: Advisory (for complex functions)

### Coverage Requirements

```mermaid
graph TD
    START[Coverage Check] --> CHECK1{Overall ≥ 80%}
    CHECK1 -->|No| FAIL[❌ FAIL]
    CHECK1 -->|Yes| CHECK2{Core ≥ 85%}
    CHECK2 -->|No| WARN[⚠️ IMPROVE]
    CHECK2 -->|Yes| CHECK3{Public API = 100%}
    CHECK3 -->|No| WARN2[⚠️ ADD TESTS]
    CHECK3 -->|Yes| PASS[✅ PASS]

    style FAIL fill:#FFB6C1
    style WARN fill:#FFD700
    style WARN2 fill:#FFD700
    style PASS fill:#90EE90
```

**Requirements**:
- Overall: ≥80%
- Core modules: ≥85%
- Public APIs: 100%
- All new code: ≥85%

**Enforcement**: Blocking (pre-commit + CI)

---

## OTP Standards

### Behavior Compliance

```mermaid
graph TD
    START[Check gen_server] --> VERIFY{All 6 callbacks<br/>implemented?}
    VERIFY -->|No| FAIL[❌ FAIL]
    VERIFY -->|Yes| CHECK_TYPES{Type specs<br/>correct?}
    CHECK_TYPES -->|No| WARN[⚠️ FIX SPECS]
    CHECK_TYPES -->|Yes| CHECK_PATTERNS{OTP patterns<br/>followed?}
    CHECK_PATTERNS -->|No| WARN2[⚠️ REFACTOR]
    CHECK_PATTERNS -->|Yes| PASS[✅ PASS]

    style FAIL fill:#FFB6C1
    style WARN fill:#FFD700
    style WARN2 fill:#FFD700
    style PASS fill:#90EE90
```

**Required Callbacks** (gen_server):
1. `init/1` - Initialization
2. `handle_call/3` - Synchronous requests
3. `handle_cast/2` - Asynchronous requests
4. `handle_info/2` - Other messages
5. `terminate/2` - Cleanup
6. `code_change/3` - Hot code reload

**Enforcement**: Blocking (code review + Dialyzer)

### Supervision Trees

```mermaid
graph TB
    subgraph "3-Tier Supervision"
        TIER1[Tier 1<br/>one_for_all<br/>Registry + Infrastructure]
        TIER2[Tier 2<br/>simple_one_for_one<br/>Protocol Servers]
        TIER3[Tier 3<br/>isolated<br/>Observability]
    end

    TIER1 --> TIER2
    TIER2 --> TIER3

    style TIER1 fill:#90EE90
    style TIER2 fill:#87CEEB
    style TIER3 fill:#FFD700
```

**Requirements**:
- All processes supervised (no unsupervised spawn)
- Proper restart strategies
- Isolation of failures
- Bounded restart intensity

**Enforcement**: Blocking (code review + tests)

### Let-It-Crash Semantics

```mermaid
graph TD
    START[Process Review] --> CHECK1{Monitors critical<br/>deps?}
    CHECK1 -->|No| FAIL[❌ FAIL - Use monitors]
    CHECK1 -->|Yes| CHECK2{Supervisor<br/>handles crashes?}
    CHECK2 -->|No| FAIL2[❌ FAIL - Add supervisor]
    CHECK2 -->|Yes| CHECK3{State is<br/>immutable?}
    CHECK3 -->|No| WARN[⚠️ Consider immutability]
    CHECK3 -->|Yes| PASS[✅ PASS]

    style FAIL fill:#FFB6C1
    style FAIL2 fill:#FFB6C1
    style WARN fill:#FFD700
    style PASS fill:#90EE90
```

**Principles**:
- Let processes crash (don't defensively program)
- Supervisors restart failed processes
- Use monitors (not links) for critical dependencies
- State isolation prevents cascade failures

**Enforcement**: Mandatory (code review)

---

## Security Standards

### Input Validation

```mermaid
graph TD
    START[Input Review] --> CHECK1{Schema<br/>validated?}
    CHECK1 -->|No| FAIL[❌ FAIL]
    CHECK1 -->|Yes| CHECK2{Size limits<br/>enforced?}
    CHECK2 -->|No| FAIL2[❌ FAIL]
    CHECK2 -->|Yes| CHECK3{URI validated?}
    CHECK3 -->|No| WARN[⚠️ ADD URI CHECKS]
    CHECK3 -->|Yes| PASS[✅ PASS]

    style FAIL fill:#FFB6C1
    style FAIL2 fill:#FFB6C1
    style WARN fill:#FFD700
    style PASS fill:#90EE90
```

**Requirements**:
- All JSON validated against schema
- Message size limits enforced
- URI validation (injection prevention)
- Error codes bounded [1001-1089]

**Enforcement**: Blocking (security scan)

### Authentication/Authorization

```mermaid
sequenceDiagram
    participant Client
    participant Auth
    participant Resource

    Client->>Auth: Authenticate
    Auth->>Auth: Validate credentials
    alt Invalid
        Auth-->>Client: 401 Unauthorized
    end
    alt Valid
        Auth-->>Client: Token/Session
        Client->>Resource: Access resource
        Resource->>Auth: Verify token
        alt Unauthorized
            Auth-->>Resource: 403 Forbidden
        else Authorized
            Auth-->>Resource: 200 OK
            Resource-->>Client: Resource data
        end
    end
```

**Requirements**:
- All connections authenticated (production)
- Authorization checks on all operations
- Token-based session management
- Audit logging of auth events

**Enforcement**: Blocking (security tests)

### Rate Limiting

```mermaid
graph TD
    START[Request] --> CHECK{Within<br/>limit?}
    CHECK -->|Yes| PROCESS[Process request]
    CHECK -->|No| REJECT[Reject - 429 Too Many Requests]
    PROCESS --> UPDATE[Update counter]
    UPDATE --> RESET{Window expired?}
    RESET -->|Yes| CLEAR[Clear counter]
    RESET -->|No| DONE[Done]
    CLEAR --> DONE
    REJECT --> LOG[Log rejection]

    style PROCESS fill:#90EE90
    style REJECT fill:#FFB6C1
    style LOG fill:#FFD700
```

**Requirements**:
- Per-connection rate limits
- Global rate limits
- Token bucket algorithm
- Circuit breaker on abuse

**Enforcement**: Blocking (security tests)

---

## Performance Standards

### No Regressions

```mermaid
graph TD
    START[Benchmark] --> MEASURE[Measure metrics]
    MEASURE --> COMPARE[Compare baseline]
    COMPARE --> CHECK{Regression<br/>< 10%}
    CHECK -->|No| FAIL[❌ FAIL]
    CHECK -->|Yes| CHECK2{Improvement<br/>> 5%}
    CHECK2 -->|Yes| PASS[✅ PASS + Improvement]
    CHECK2 -->|No| PASS2[✅ PASS]

    style FAIL fill:#FFB6C1
    style PASS fill:#90EE90
    style PASS2 fill:#87CEEB
```

**Requirements**:
- Throughput: <10% regression
- Latency: <10% regression
- Memory: <10% regression
- All changes benchmarked

**Enforcement**: Conditional (if perf code changed)

### All Critical Paths Benchmarked

```mermaid
mindmap
  root((Benchmarked))
    Core Operations
      Registry
      Queue
      Pool
      Session
    Network
      TCP
      HTTP
      WebSocket
    Protocol
      JSON encode/decode
      Message routing
      Tool invocation
    Resilience
      Circuit breaker
      Rate limiter
      Chaos recovery
```

**Requirements**:
- All hot paths benchmarked
- Baselines established (v1.5.0)
- Performance documented
- Regression tests in CI

**Enforcement**: Advisory (manual review)

---

## Compliance Matrix

| Standard | Level | Enforcement | Status |
|----------|-------|-------------|--------|
| Type Coverage | Mandatory | Blocking | ✅ 95% |
| No Suppressions | Mandatory | Blocking | ✅ 100% |
| Docstrings | Mandatory | Advisory | ⚠️ 80% |
| Modular Design | Mandatory | Advisory | ✅ 95% |
| Error Handling | Mandatory | Blocking | ✅ 95% |
| Chicago TDD | Mandatory | Blocking | ✅ 100% |
| No Mocks | Mandatory | Blocking | ✅ 100% |
| Black-Box Testing | Mandatory | Blocking | ✅ 95% |
| Property-Based | Advisory | Advisory | ⚠️ 40% |
| Coverage 80% | Mandatory | Blocking | ⚠️ 75% |
| OTP Compliance | Mandatory | Blocking | ✅ 100% |
| Input Validation | Mandatory | Blocking | ✅ 100% |
| Auth/Authz | Mandatory | Blocking | ✅ 100% |
| No Regressions | Mandatory | Conditional | ✅ 100% |

---

**Version**: 2.1.0
**Last Updated**: January 31, 2026
