# erlmcp Error Taxonomy & Classification System
## Version 2.1.0 - Complete Error Reference

**System Identity**: erlmcp Error Classification Framework
**Basis**: MCP 2025-11-25 Specification + JSON-RPC 2.0 + OTP Design Principles
**Coverage**: 164 modules, 84 test suites, 850+ documentation files

---

## Error Hierarchy (Complete Taxonomy)

```
erlmcp_errors
│
├── PROTOCOL_ERRORS (1000-1999)
│   ├── JSON_RPC_20 (1000-1099)
│   │   ├── 1000: Invalid JSON structure
│   │   ├── 1001: Request ID missing (CRITICAL)
│   │   ├── 1002: Method not found
│   │   ├── 1003: Invalid parameters
│   │   ├── 1004: Batch request parsing failure
│   │   └── 1099: Generic JSON-RPC error
│   │
│   ├── MCP_COMPLIANCE (1100-1199)
│   │   ├── 1100: Protocol version mismatch
│   │   ├── 1101: Capability negotiation failed
│   │   ├── 1102: Invalid request type
│   │   ├── 1103: Required capability missing
│   │   └── 1199: Generic MCP protocol error
│   │
│   └── MCP_REFUSAL_CODES (1200-1299)
│       ├── 1200-1299: Standard MCP refusal reasons
│       └── Custom refusal codes (extended)
│
├── TRANSPORT_ERRORS (2000-2999)
│   ├── CONNECTION_FAILURES (2000-2099)
│   │   ├── 2000: Connection refused
│   │   ├── 2001: Connection timeout
│   │   ├── 2002: Connection lost
│   │   ├── 2003: Connection reset by peer
│   │   ├── 2004: Maximum connections exceeded
│   │   ├── 2005: Rate limit exceeded
│   │   └── 2099: Generic connection error
│   │
│   ├── TRANSPORT_SPECIFIC (2100-2199)
│   │   ├── 2100-2199: STDIO errors
│   │   ├── 2200-2299: TCP errors
│   │   ├── 2300-2399: HTTP errors
│   │   ├── 2400-2499: WebSocket errors
│   │   └── 2500-2599: SSE errors
│   │
│   └── POOL_EXHAUSTION (2600-2699)
│       ├── 2600: Connection pool empty
│       ├── 2601: Pool initialization failed
│       ├── 2602: Pool member unhealthy
│       └── 2699: Generic pool error
│
├── RESOURCE_ERRORS (3000-3999)
│   ├── RESOURCE_ACCESS (3000-3099)
│   │   ├── 3000: Resource not found
│   │   ├── 3001: Resource access denied
│   │   ├── 3002: Resource subscription failed
│   │   ├── 3003: Resource unavailable
│   │   ├── 3004: Resource URI invalid
│   │   └── 3099: Generic resource error
│   │
│   ├── SUBSCRIPTION_FAILURES (3100-3199)
│   │   ├── 3100: Subscription limit exceeded
│   │   ├── 3101: Subscription validation failed
│   │   ├── 3102: Subscription update failed
│   │   └── 3199: Generic subscription error
│   │
│   └── STORAGE_BACKEND (3200-3299)
│       ├── 3200: ETS table corruption
│       ├── 3201: DETS file access failed
│       ├── 3202: Mnesia transaction aborted
│       ├── 3203: Database connection lost
│       └── 3299: Generic storage error
│
├── TOOL_ERRORS (4000-4999)
│   ├── TOOL_EXECUTION (4000-4099)
│   │   ├── 4000: Tool not found
│   │   ├── 4001: Tool execution failed
│   │   ├── 4002: Tool validation failed
│   │   ├── 4003: Tool timeout
│   │   ├── 4004: Tool arguments invalid
│   │   └── 4099: Generic tool error
│   │
│   ├── TOOL_PERMISSION (4100-4199)
│   │   ├── 4100: Tool access denied
│   │   ├── 4101: Tool authorization failed
│   │   ├── 4102: Tool rate limit exceeded
│   │   └── 4199: Generic tool permission error
│   │
│   └── LLM_INTEGRATION (4200-4299)
│       ├── 4200: LLM provider unavailable
│       ├── 4201: LLM authentication failed
│       ├── 4202: LLM rate limit exceeded
│       ├── 4203: LLM timeout
│       ├── 4204: LLM response invalid
│       └── 4299: Generic LLM error
│
├── AUTH_SECURITY_ERRORS (5000-5999)
│   ├── AUTHENTICATION (5000-5099)
│   │   ├── 5000: Authentication failed
│   │   ├── 5001: Invalid credentials
│   │   ├── 5002: Token expired
│   │   ├── 5003: Token revoked
│   │   ├── 5004: mTLS handshake failed
│   │   └── 5099: Generic authentication error
│   │
│   ├── AUTHORIZATION (5100-5199)
│   │   ├── 5100: Access denied
│   │   ├── 5101: Insufficient permissions
│   │   ├── 5102: Operation not permitted
│   │   ├── 5103: Resource access forbidden
│   │   └── 5199: Generic authorization error
│   │
│   ├── SECRETS_MANAGEMENT (5200-5299)
│   │   ├── 5200: Secret not found
│   │   ├── 5201: Secret access denied
│   │   ├── 5202: Secrets backend unavailable
│   │   ├── 5203: Secret encryption failed
│   │   ├── 5204: Secret rotation failed
│   │   └── 5299: Generic secret error
│   │
│   └── VALIDATION_SECURITY (5300-5399)
│       ├── 5300: Input validation failed
│       ├── 5301: URI injection attempt detected
│       ├── 5302: Path traversal attempt detected
│       ├── 5303: Message size exceeded
│       ├── 5304: Malformed request detected
│       └── 5399: Generic validation error
│
├── SESSION_ERRORS (6000-6999)
│   ├── SESSION_MANAGEMENT (6000-6099)
│   │   ├── 6000: Session not found
│   │   ├── 6001: Session expired
│   │   ├── 6002: Session creation failed
│   │   ├── 6003: Session storage error
│   │   ├── 6004: Session limit exceeded
│   │   └── 6099: Generic session error
│   │
│   ├── SESSION_FAILOVER (6100-6199)
│   │   ├── 6100: Failover initiation failed
│   │   ├── 6101: Failover coordination failed
│   │   ├── 6102: Session replication failed
│   │   ├── 6103: Split-brain detected
│       └── 6199: Generic failover error
│   │
│   └── CLUSTER_COORDINATION (6200-6299)
│       ├── 6200: Node unreachable
│       ├── 6201: Cluster partition detected
│       ├── 6202: Sync protocol failed
│       ├── 6203: Leader election failed
│       └── 6299: Generic cluster error
│
├── SYSTEM_ERRORS (7000-7999)
│   ├── SUPERVISION_FAILURES (7000-7099)
│   │   ├── 7000: Supervisor termination
│   │   ├── 7001: Child restart intensity exceeded
│   │   ├── 7002: Dynamic child spawn failed
│   │   ├── 7003: Process trap exit failed
│   │   └── 7099: Generic supervision error
│   │
│   ├── RESOURCE_EXHAUSTION (7100-7199)
│   │   ├── 7100: Memory exhaustion
│   │   ├── 7101: CPU saturation
│   │   ├── 7102: File descriptor exhaustion
│   │   ├── 7103: Port exhaustion
│   │   └── 7199: Generic resource error
│   │
│   └── CIRCUIT_BREAKER (7200-7299)
│       ├── 7200: Circuit breaker open
│       ├── 7201: Circuit breaker half-open
│       ├── 7202: Circuit breaker threshold exceeded
│       └── 7299: Generic circuit breaker error
│
├── OBSERVABILITY_ERRORS (8000-8999)
│   ├── TELEMETRY_FAILURES (8000-8099)
│   │   ├── 8000: OTEL export failed
│   │   ├── 8001: Tracing span creation failed
│   │   ├── 8002: Metric collection failed
│   │   ├── 8003: Health check failed
│       └── 8099: Generic telemetry error
│   │
│   └── AUDIT_LOGGING (8100-8199)
│       ├── 8100: Audit log write failed
│       ├── 8101: Receipt chain corruption
│       ├── 8102: Evidence path broken
│       └── 8199: Generic audit error
│
└── VALIDATION_ERRORS (9000-9999)
    ├── PROTOCOL_VALIDATION (9000-9099)
    │   ├── 9000: Schema validation failed
    │   ├── 9001: Transport behavior violation
    │   ├── 9002: Security validation failed
    │   ├── 9003: Performance validation failed
    │   └── 9099: Generic validation error
    │
    └── COMPLIANCE_VIOLATION (9100-9199)
        ├── 9100: MCP compliance violation
        ├── 9101: JSON-RPC compliance violation
        ├── 9102: OTP pattern violation
        └── 9199: Generic compliance error
```

---

## Error Severity Classification

### Critical (P0) - System Impact
**Definition**: Complete service disruption, data loss risk, security breach
**Response Time**: Immediate (< 1 minute)
**Escalation**: Automatic + Page on-call
**Examples**:
- 7000: Supervisor termination
- 7100: Memory exhaustion
- 5301: URI injection detected
- 3200: ETS table corruption

### High (P1) - Major Feature Impact
**Definition**: Major feature unavailable, significant degradation
**Response Time**: < 15 minutes
**Escalation**: Automatic + Notify team
**Examples**:
- 2000: Connection refused
- 4200: LLM provider unavailable
- 3100: Subscription limit exceeded
- 6200: Node unreachable

### Medium (P2) - Minor Feature Impact
**Definition**: Minor feature unavailable, partial degradation
**Response Time**: < 1 hour
**Escalation**: Notify team
**Examples**:
- 3000: Resource not found
- 4000: Tool not found
- 5000: Authentication failed
- 7200: Circuit breaker open

### Low (P3) - Cosmetic/Edge Case
**Definition**: Edge cases, non-critical issues
**Response Time**: < 24 hours
**Escalation**: Log for investigation
**Examples**:
- 1002: Method not found
- 4004: Tool arguments invalid
- 5300: Input validation failed
- 9000: Schema validation failed

---

## Error Recovery Matrix

| Error Class | Recovery Strategy | Auto-Retry? | Fallback | Manual Intervention |
|-------------|-------------------|-------------|----------|---------------------|
| **1000-1099: JSON-RPC** | Client fix request | No | None | Yes |
| **2000-2099: Connection** | Reconnect with backoff | Yes | Alternative transport | Yes (if backoff exhausted) |
| **3000-3099: Resources** | Cache refresh + retry | Yes | Stale cache | Yes |
| **4000-4099: Tools** | Re-execution with validation | No | Alternative tool | Yes |
| **5000-5099: Auth** | Re-authentication | Yes | Cached credentials | Yes (if token expired) |
| **6000-6099: Session** | Session recreation | Yes | Local cache | Yes |
| **7000-7099: Supervision** | Let-it-crash + restart | Yes | N/A | Yes (if intensity exceeded) |
| **8000-8099: Observability** | Degraded mode | No | Best-effort | No |
| **9000-9099: Validation** | Reject request | No | None | Yes |

---

## Error Response Format (JSON-RPC 2.0)

```json
{
  "jsonrpc": "2.0",
  "id": "req-uuid-123",
  "error": {
    "code": 2001,
    "message": "Connection timeout",
    "data": {
      "severity": "P1",
      "category": "TRANSPORT_ERRORS.CONNECTION_FAILURES",
      "timestamp": "2026-01-31T12:00:00Z",
      "request_id": "req-uuid-123",
      "context": {
        "transport": "tcp",
        "timeout_ms": 5000,
        "attempt": 3,
        "max_attempts": 5
      },
      "recovery": {
        "auto_retry": true,
        "backoff_ms": 1000,
        "fallback_transport": "stdio"
      },
      "documentation": "https://erlmcp.dev/errors/2001"
    }
  }
}
```

---

## Error Monitoring & Alerting

### Metrics Collection (erlmcp_metrics)

```erlang
%% Error rate by category
erlmcp_metrics:increment_counter(
    [errors, protocol, json_rpc, invalid_json],
    1,
    #{severity => P2}
).

%% Error latency tracking
erlmcp_metrics:record_latency(
    [errors, transport, connection_timeout],
    Latency,
    #{transport => tcp}
).

%% Error severity distribution
erlmcp_metrics:gauge(
    [errors, severity, P0, count],
    P0_Counter
).
```

### Alert Thresholds

| Alert | Threshold | Duration | Action |
|-------|-----------|----------|--------|
| **P0 Errors** | > 0 | Immediate | Page on-call |
| **P1 Errors** | > 10/min | 5 min | Notify team |
| **P2 Errors** | > 100/min | 10 min | Log warning |
| **Error Rate** | > 5% of requests | 5 min | Investigate |
| **Circuit Breaker Open** | Any | Immediate | Auto-alert |

---

## Error Handling Best Practices

### 1. Let-It-Crash Philosophy
```erlang
%% ✅ CORRECT: Let supervisor handle crashes
handle_call(Request, _From, State) ->
    case validate_request(Request) of
        {ok, Validated} ->
            process_request(Validated, State);
        {error, Reason} ->
            %% Crash and let supervisor restart
            erlang:error({invalid_request, Reason})
    end.

%% ❌ WRONG: Catch-all error hiding
handle_call(Request, _From, State) ->
    try
        process_request(Request, State)
    catch
        _:_ -> {reply, {error, <<>>}, State}
    end.
```

### 2. Structured Error Logging
```erlang
%% ✅ CORRECT: Structured logging
logger:error(
    "Connection failed",
    #{
        error_code => 2001,
        transport => tcp,
        peer => {Ip, Port},
        reason => Reason,
        retry_attempt => Attempt,
        max_attempts => MaxAttempts
    }
).

%% ❌ WRONG: Unstructured logging
logger:format("Connection failed: ~p~n", [Reason]).
```

### 3. Context Preservation
```erlang
%% ✅ CORRECT: Preserve context through error chain
{error, {connection_failed, #{
    code => 2001,
    context => #{
        original_request => Request,
        timestamp => os:system_time(millisecond),
        node => node()
    }
}}}.

%% ❌ WRONG: Context lost
{error, connection_failed}.
```

### 4. Recovery Metadata
```erlang
%% ✅ CORRECT: Include recovery information
{error, #{
    code => 2001,
    message => "Connection timeout",
    recovery => #{
        auto_retry => true,
        backoff_ms => 1000,
        max_attempts => 5,
        fallback_transport => stdio
    }
}}.
```

---

## Error Testing Strategy

### Unit Tests (EUnit)
```erlang
connection_timeout_test() ->
    %% Given
    TimeoutMs = 100,
    {ok, Pid} = erlmcp_transport_tcp:start_link(#{timeout => TimeoutMs}),

    %% When
    Result = catch erlmcp_transport_tcp:send(
        <<"large_data">>,
        #{timeout => TimeoutMs + 10}
    ),

    %% Then
    ?assertMatch(
        {error, #{code := 2001, recovery := #{auto_retry := true}}},
        Result
    ).
```

### Property-Based Tests (Proper)
```erlang
prop_error_recovery() ->
    ?FORALL({ErrorCode, Context}, error_code_and_context(),
        begin
            Result = erlmcp_errors:recover(ErrorCode, Context),
            implies(is_recoverable(ErrorCode), is_success(Result))
        end).
```

### Chaos Tests
```erlang
chaos_connection_failure_test() ->
    %% Inject connection failures
    erlmcp_chaos:inject_failure(connection_reset),

    %% Verify recovery
    ?assertEqual(ok, erlmcp_health:check(transport)),

    %% Verify circuit breaker state
    ?assertEqual(closed, erlmcp_circuit_breaker:state(tcp)).
```

---

## Documentation Index

**Detailed Error Guides**:
- `PROTOCOL_ERRORS.md` - JSON-RPC and MCP compliance errors
- `TRANSPORT_ERRORS.md` - Connection and transport errors
- `RESOURCE_ERRORS.md` - Resource and subscription errors
- `TOOL_ERRORS.md` - Tool execution and LLM errors
- `AUTH_SECURITY_ERRORS.md` - Authentication and security errors
- `SESSION_ERRORS.md` - Session and cluster errors
- `SYSTEM_ERRORS.md` - Supervision and resource exhaustion
- `OBSERVABILITY_ERRORS.md` - Telemetry and audit errors
- `VALIDATION_ERRORS.md` - Compliance and validation errors

**Troubleshooting Guides**:
- `/docs/troubleshooting/common-issues.md` - Decision trees
- `/docs/troubleshooting/performance-issues.md` - Performance analysis
- `/docs/troubleshooting/connection-issues.md` - Connection troubleshooting
- `/archive/troubleshooting/` - Historical issue resolution

**Visual Diagrams**:
- `/docs/diagrams/errors/` - Error flow diagrams
- `/docs/diagrams/recovery/` - Recovery flow diagrams

---

**Version**: 2.1.0
**Last Updated**: 2026-01-31
**Maintainer**: erlmcp Core Team
