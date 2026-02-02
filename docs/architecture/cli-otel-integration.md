# CLI-OTEL Integration Architecture - 80/20 Design v2.1.0

## Executive Summary

This document defines the architecture for CLI-OTEL integration in erlmcp, focusing on the 80/20 principle where 20% of the architecture delivers 80% of the value. The design emphasizes supervision isolation, process-per-connection models, comprehensive observability, and chaos engineering resilience.

## Σ : System Architecture Overview

**CLI-OTEL Integration** := CLI Interface ⊕ OTEL ⊕ Supervision ⊕ Process Isolation | OTP 28.3.1

| Component | Purpose | Isolation Level | Dependencies |
|----------|---------|----------------|--------------|
| CLI Interface | User command processing | Process-per-connection | Registry, Metrics |
| OTEL Engine | Distributed tracing & metrics | Library (no supervision) | opentelemetry |
| Supervision Tree | Crash boundaries & recovery | Tiered isolation | Core infrastructure |
| Chaos Engineering | Resilience testing | Isolated experiments | Observability |

---

## 1. 3-Tier Supervision Tree Architecture

### 1.1 Core Principles

```
TIER 1 (one_for_all)        : erlmcp_cli_sup ⊃ {registry, config, secrets}
TIER 2 (simple_one_for_one) : {session, command}_sup → isolated per-connection
TIER 3 (isolated)           : erlmcp_observability_sup ⊃ {metrics, tracing, chaos}
```

### 1.2 CLI-Specific Supervision Tree

```erlang
%% Tier 1: CLI Core Supervisor (one_for_all)
erlmcp_cli_sup
├── erlmcp_cli_registry      % Command registry and routing
├── erlmcp_cli_config        % Configuration management
└── erlmcp_cli_secrets       % Secret management

%% Tier 2: Command Session Supervisor (simple_one_for_one)
erlmcp_cli_session_sup
├── erlmcp_cli_session       % Per-session state management
├── erlmcp_cli_parser        % Command parsing and validation
├── erlmcp_cli_executor      % Command execution isolation
└── erlmcp_cli_transport     % CLI transport handling

%% Tier 3: OTEL Integration (isolated)
erlmcp_observability_sup
├── erlmcp_metrics          % Metrics collection
├── erlmcp_otel             % OpenTelemetry integration
├── erlmcp_chaos            % Chaos engineering
└── erlmcp_health_monitor   % Health monitoring
```

### 1.3 Supervisor Strategies

| Supervisor | Strategy | Rationale |
|------------|----------|-----------|
| `erlmcp_cli_sup` | `one_for_all` | Core failures restart entire subsystem |
| `erlmcp_cli_session_sup` | `simple_one_for_one` | Session failures are isolated |
| `erlmcp_observability_sup` | `one_for_one` | Observability failures don't affect CLI |

---

## 2. Process-Per-Connection Model

### 2.1 CLI Session Management

```erlang
%% Process-per-Session Design
-spec start_session(SessionId :: binary(), Config :: map()) -> {ok, pid()}.
start_session(SessionId, Config) ->
    ChildSpec = #{
        id => SessionId,
        start => {erlmcp_cli_session, start_link, [SessionId, Config]},
        restart => transient,  % Don't restart failed sessions
        shutdown => 5000,     % Graceful shutdown
        type => worker,
        modules => [erlmcp_cli_session]
    },
    supervisor:start_child(erlmcp_cli_session_sup, ChildSpec).
```

### 2.2 Session Isolation Boundaries

| Boundary | Purpose | Failure Isolation |
|----------|---------|------------------|
| Process Boundary | Complete session isolation | Process crashes don't affect others |
| Registry Boundary | Command routing isolation | Registry errors don't crash sessions |
| Transport Boundary | CLI protocol isolation | Transport errors don't affect other sessions |

### 2.3 Session Lifecycle

```erlang
%% Session States
-record(cli_session, {
    id :: binary(),
    pid :: pid(),
    status :: active | paused | terminated,
    created :: integer(),
    last_activity :: integer(),
    command_count :: integer(),
    trace_context :: map(),
    metrics :: map()
}).

%% Session Management
session_create() -> SessionId
session_activate(SessionId) -> ok
session_pause(SessionId) -> ok
session_terminate(SessionId) -> ok
session_metrics(SessionId) -> map()
```

---

## 3. Data Flow Patterns

### 3.1 CLI Command Processing Pipeline

```
User Input → CLI Parser → Registry Lookup → OTEL Span → Command Execution → Response
    ↓           ↓            ↓              ↓           ↓             ↓
  Transport   Validation  Routing        Tracing     Isolation     Metrics
```

### 3.2 Message Flow Architecture

```erlang
%% CLI Command Flow
1. User Input: {cli_input, Command, Args}
2. Parser: {parse_result, ParsedCmd, Errors}
3. Registry: {registry_lookup, Command, Metadata}
4. OTEL Span: start_span("cli.command.execute", #{...})
5. Executor: {execute, Command, Context}
6. Response: {cli_response, Result, Status}
7. Metrics: record_metric("cli.commands.total", 1)
```

### 3.3 Registry Routing Patterns

```erlang
%% Command Registry with OTEL Integration
-record(command_registry, {
    name :: binary(),
    module :: module(),
    arity :: integer(),
    description :: binary(),
    metrics :: map(),
    trace_enabled :: boolean()
}).

%% Registry Lookup with Tracing
-spec lookup_command(binary(), map()) -> {ok, command_registry()} | {error, term()}.
lookup_command(CommandName, TraceCtx) ->
    erlmcp_otel:with_span("cli.registry.lookup", #{
        <<"command.name">> => CommandName
    }, fun() ->
        case ets:lookup(cli_registry, CommandName) of
            [Cmd] -> {ok, Cmd};
            [] -> {error, not_found}
        end
    end).
```

---

## 4. OTEL Integration Strategy

### 4.1 Comprehensive Tracing Coverage

```erlang
%% CLI Command Tracing
-spec execute_command(binary(), list(), map()) -> term().
execute_command(Command, Args, TraceCtx) ->
    SpanCtx = erlmcp_otel:inject_rpc_span(<<"cli.command">>,
                                        make_request_id(),
                                        #{<<"args">> => Args},
                                        TraceCtx),

    try
        %% Command execution with tracing
        Result = erlmcp_cli_executor:execute(Command, Args, SpanCtx),
        erlmcp_otel:add_event(SpanCtx, <<"command.completed">>, #{
            <<"success">> => true,
            <<"result_size">> => term_size(Result)
        }),
        Result
    catch
        Class:Reason:Stacktrace ->
            erlmcp_otel:record_error(SpanCtx, {Class, Reason, Stacktrace}),
            erlmcp_otel:add_event(SpanCtx, <<"command.failed">>, #{
                <<"error.type">> => atom_to_binary(Class),
                <<"error.message">> => format_error(Reason)
            }),
            erlang:raise(Class, Reason, Stacktrace)
    end.
```

### 4.2 Metrics Collection Strategy

```erlang
%% CLI Metrics Collection
-record(cli_metrics, {
    commands_total = 0 :: integer(),
    commands_success = 0 :: integer(),
    commands_failed = 0 :: integer(),
    command_latency = [] :: [integer()],
    active_sessions = 0 :: integer(),
    session_duration = [] :: [integer()],
    throughput = 0 :: float()
}).

%% Key Metrics to Track
-define(CLI_METRICS, [
    "cli.commands.total",
    "cli.commands.success",
    "cli.commands.failed",
    "cli.command.latency",
    "cli.sessions.active",
    "cli.sessions.duration",
    "cli.throughput",
    "cli.error.rate"
]).
```

### 4.3 Baggage Correlation

```erlang
%% Session Context Propagation
-spec propagate_session_context(binary(), map()) -> ok.
propagate_session_context(SessionId, Context) ->
    %% Set session baggage for correlation
    erlmcp_otel:set_baggage(<<"session.id">>, SessionId),
    erlmcp_otel:set_baggage(<<"session.type">>, <<"cli">>),

    %% Copy context to session process
    SessionPid = session_pid(SessionId),
    SessionPid ! {set_context, Context}.
```

---

## 5. Chaos Engineering Integration

### 5.1 CLI-Specific Chaos Experiments

```erlang
%% Chaos Configuration for CLI
-record(cli_chaos_config, {
    network_failure_rate = 0.05 :: float(),
    command_timeout_rate = 0.02 :: float(),
    session_crash_rate = 0.01 :: float(),
    memory_pressure = false :: boolean(),
    cpu_saturate = false :: boolean()
}).

%% Chaos Injection Points
-spec inject_chaos(binary(), term()) -> term().
inject_chaos(Command, Result) ->
    case erlmcp_chaos:should_fail("cli.command.network") of
        true -> {error, network_timeout};
        false -> Result
    end.
```

### 5.2 Resilience Testing Patterns

```erlang
%% Circuit Breaker for CLI Commands
-spec execute_with_circuit_breaker(binary(), list()) -> term().
execute_with_circuit_breaker(Command, Args) ->
    CircuitBreaker = erlmcp_circuit:get(cli_command_breaker),

    case erlmcp_circuit:allow_request(CircuitBreaker) of
        true ->
            try
                Result = erlmcp_cli_executor:execute(Command, Args),
                erlmcp_circuit:success(CircuitBreaker),
                Result
            catch
                _:Reason ->
                    erlmcp_circuit:failure(CircuitBreaker),
                    {error, Reason}
            end;
        false ->
            {error, circuit_breaker_open}
    end.
```

---

## 6. Transport Polymorphism

### 6.1 CLI Transport Interface

```erlang
%% CLI Transport Behavior
-callback init(binary(), map()) -> {ok, state()} | {error, term()}.
-callback send(binary(), state()) -> {ok, state()} | {error, term()}.
-callback close(state()) -> ok.
-callback handle_input(binary(), state()) -> {ok, state()} | {error, term()}.

%% Transport Implementations
- erlmcp_transport_stdio    % Standard I/O
- erlmcp_transport_socket   % Network socket
- erlmcp_transport_web      % WebSocket
- erlmcp_transport_file     % File-based I/O
```

### 6.2 Transport with OTEL Integration

```erlang
%% Transport with Tracing
-spec transport_send(binary(), state()) -> {ok, state()}.
transport_send(Data, State) ->
    erlmcp_otel:with_span("cli.transport.send", #{
        <<"transport.type">> => <<"stdio">>,
        <<"message.size">> => byte_size(Data)
    }, fun() ->
        erlmcp_transport_stdio:send(Data, State)
    end).
```

---

## 7. Quality Gates and Compliance

### 7.1 CLI-Specific Quality Gates

| Gate | Pass Criteria | Enforcement |
|------|---------------|-------------|
| Session Isolation | No cross-session crashes | Process boundaries |
| Command Safety | No eval() usage | AST validation |
| Resource Limits | Memory < 100MB per session | Process monitoring |
| Response Time | < 1s for 95% of commands | Metrics collection |
| Error Handling | 100% error capture | OTEL integration |

### 7.2 Compliance Monitoring

```erlang
%% CLI Compliance Checks
-spec check_compliance() -> boolean().
check_compliance() ->
    %% No arbitrary code execution
    NoEval = not has_eval_usage(),

    %% Proper session isolation
    SessionsIsolated = check_session_isolation(),

    %% Metrics collection
    MetricsWorking = check_metrics_collection(),

    NoEval and SessionsIsolated and MetricsWorking.
```

---

## 8. Performance Optimization

### 8.1 CLI Performance Baselines

| Metric | Target | Measurement |
|--------|--------|-------------|
| Command Response Time | < 100ms (95th percentile) | OTEL spans |
| Session Startup | < 50ms | Metrics |
| Memory Usage per Session | < 10MB | Process monitoring |
| Throughput | 1000+ commands/second | Metrics |

### 8.2 Performance Monitoring

```erlang
%% Performance Metrics Collection
-spec record_performance_metric(binary(), term()) -> ok.
record_performance_metric(Metric, Value) ->
    %% Record with OTEL
    erlmcp_otel:with_span("cli.performance.record", #{
        <<"metric.name">> => Metric
    }, fun() ->
        erlmcp_metrics:record(Metric, Value)
    end).
```

---

## 9. Configuration Management

### 9.1 CLI Configuration Structure

```erlang
%% CLI Configuration
-record(cli_config, {
    session_timeout = 300000 :: integer(),     % 5 minutes
    command_timeout = 10000 :: integer(),      % 10 seconds
    max_sessions = 100 :: integer(),
    enable_chaos = false :: boolean(),
    otel_exporters = [console] :: [atom()],
    tracing_enabled = true :: boolean(),
    metrics_enabled = true :: boolean()
}).
```

### 9.2 Environment-Based Configuration

```erlang
%% Configuration with Environment Overrides
-spec get_cli_config() -> cli_config().
get_cli_config() ->
    #cli_config{
        session_timeout = env_or_default("CLI_SESSION_TIMEOUT", 300000),
        command_timeout = env_or_default("CLI_COMMAND_TIMEOUT", 10000),
        max_sessions = env_or_default("CLI_MAX_SESSIONS", 100),
        enable_chaos = env_or_default("CLI_ENABLE_CHAOS", false)
    }.
```

---

## 10. Implementation Roadmap

### 10.1 Phase 1: Core CLI Infrastructure (Week 1-2)
- [ ] CLI supervisor implementation
- [ ] Session management
- [ ] Basic command parsing
- [ ] OTEL integration

### 10.2 Phase 2: Advanced Features (Week 3-4)
- [ ] Chaos engineering integration
- [ ] Circuit breakers
- [ ] Performance monitoring
- [ ] Configuration management

### 10.3 Phase 3: Production Hardening (Week 5-6)
- [ ] Load testing
- [ ] Security validation
- [ ] Documentation
- [ ] Quality gates

---

## 11. Testing Strategy

### 11.1 Test Coverage Requirements

| Component | Coverage | Test Type |
|----------|----------|-----------|
| CLI Session Management | 100% | Unit + Integration |
| Command Parsing | 100% | Unit |
| OTEL Integration | 90% | Unit + Integration |
| Chaos Engineering | 80% | Property-based |
| Transport Layer | 90% | Unit + Integration |

### 11.2 Chicago TDD for CLI

```erlang
%% Test-First Implementation
-spec test_cli_session_isolation() -> ok.
test_cli_session_isolation() ->
    %% Given
    Session1 = start_session("session1", #{}),
    Session2 = start_session("session2", #{}),

    %% When
    Result1 = execute_command(Session1, "test", []),
    Result2 = execute_command(Session2, "test", []),

    %% Then
    assert(Result1 =/= Result2),
    assert(session_exists("session1")),
    assert(session_exists("session2")).
```

---

## 12. Monitoring and Observability

### 12.1 CLI-Specific Metrics

```erlang
%% Key CLI Metrics to Monitor
-define(CLI_METRICS, [
    % Command Metrics
    "cli.commands.total",
    "cli.commands.success",
    "cli.commands.failed",
    "cli.command.latency",

    % Session Metrics
    "cli.sessions.active",
    "cli.sessions.created",
    "cli.sessions.terminated",
    "cli.session.duration",

    % Error Metrics
    "cli.errors.total",
    "cli.errors.rate",
    "cli.errors.timeout",
    "cli.errors.parsing",

    % Performance Metrics
    "cli.throughput",
    "cli.memory.usage",
    "cli.cpu.usage"
]).
```

### 12.2 Alerting Thresholds

| Metric | Warning | Critical |
|--------|---------|----------|
| Command Latency (95th) | 500ms | 1000ms |
| Error Rate | 5% | 10% |
| Active Sessions | 80% | 100% |
| Memory Usage | 80% | 95% |

---

## 13. Security Considerations

### 13.1 CLI Security Model

| Threat | Mitigation |
|--------|------------|
| Command injection | AST validation, no eval() |
| Session hijacking | Process isolation |
| Information leakage | Baggage sanitization |
| Denial of service | Rate limiting, circuit breakers |

### 13.2 Security Monitoring

```erlang
%% Security Metrics Collection
-spec record_security_event(binary(), map()) -> ok.
record_security_event(Event, Details) ->
    erlmcp_otel:with_span("cli.security.event", #{
        <<"event.type">> => Event,
        <<"event.details">> => Details
    }, fun() ->
        erlmcp_metrics:record("cli.security.events", 1)
    end).
```

---

## 14. Documentation and Examples

### 14.1 Usage Examples

```erlang
%% Example: CLI Command with OTEL
-spec example_cli_usage() -> term().
example_cli_usage() ->
    %% Create session with tracing
    TraceCtx = erlmcp_otel:start_span("cli.session.create", #{}),

    try
        %% Execute command with full tracing
        Result = execute_command("list_files", ["/tmp"], TraceCtx),

        %% Record metrics
        erlmcp_otel:add_event(TraceCtx, "command.completed", #{
            <<"result_count">> => length(Result)
        }),

        Result
    catch
        Error ->
            erlmcp_otel:record_error(TraceCtx, Error),
            throw(Error)
    end.
```

### 14.2 Configuration Examples

```erlang
%% CLI Configuration Example
CLIConfig = #cli_config{
    session_timeout = 300000,      % 5 minutes
    command_timeout = 10000,       % 10 seconds
    max_sessions = 100,
    enable_chaos = true,
    otel_exporters = [jaeger, prometheus],
    tracing_enabled = true,
    metrics_enabled = true
}.

%% Start CLI system
ok = erlmcp_cli_sup:start_link(CLIConfig).
```

---

## 15. Future Extensions

### 15.1 Planned Enhancements

1. **Multi-tenant CLI**: Session isolation per tenant
2. **CLI Plugin System**: Dynamic command loading
3. **Advanced Chaos Engineering**: Fault injection patterns
4. **CLI Dashboard**: Real-time monitoring interface
5. **CLI Analytics**: Historical command analysis

### 15.2 Scalability Considerations

- **Horizontal Scaling**: Session distribution across nodes
- **Load Balancing**: Command routing optimization
- **Caching**: Command result caching
- **Compression**: Large data handling

---

## 16. Conclusion

The CLI-OTEL integration architecture provides a robust foundation for CLI operations with comprehensive observability, fault isolation, and performance monitoring. The 80/20 approach ensures that the most critical features are implemented first while maintaining extensibility for future enhancements.

### Key Benefits

1. **Isolation**: Complete process-per-session isolation prevents cascading failures
2. **Observability**: Comprehensive OTEL integration provides full visibility
3. **Resilience**: Chaos engineering and circuit breakers ensure reliability
4. **Performance**: Optimized for high-throughput CLI operations
5. **Security**: Built-in security monitoring and validation

### Success Metrics

- 99.9% command execution success rate
- < 100ms average response time
- 100% session isolation maintained
- Comprehensive OTEL coverage
- Zero security incidents

This architecture provides a solid foundation for building enterprise-grade CLI applications with complete observability and resilience.