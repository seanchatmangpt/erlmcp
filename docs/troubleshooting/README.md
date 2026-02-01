# erlmcp Troubleshooting Guide

**Version**: 2.1.0
**Last Updated**: 2025-01-31
**Maintainer**: erlmcp Development Team

---

## Overview

This guide provides comprehensive troubleshooting information for erlmcp (Erlang/OTP Model Context Protocol SDK). It uses visual flowcharts and systematic diagnostic procedures to help you quickly identify and resolve issues.

**Scope**: Covers connection issues, authentication failures, performance problems, message/protocol errors, resource management, crashes, and memory leaks.

**Quick Diagnosis**: Use the symptom checker below or navigate to specific sections.

---

## Symptom Checker

Use this table to quickly navigate to the relevant troubleshooting section:

| Symptom | Category | See Section |
|---------|----------|-------------|
| Cannot connect to server | Connection | [Connection Issues](#connection-issues) |
| Authentication/authorization failures | Authentication | [Authentication Problems](#authentication-problems) |
| Rate limit exceeded errors | Rate Limiting | [Rate Limiting Issues](#rate-limiting-issues) |
| Slow response times / high latency | Performance | [Performance Problems](#performance-problems) |
| Message validation failures | Protocol | [Message/Protocol Errors](#messageprotocol-errors) |
| Resource/tool/prompt not found | Resources | [Resource/Subscription Issues](#resourcesubscription-issues) |
| Server crashes or restarts | Stability | [Crashes/Restarts](#crashesrestarts) |
| Memory growing constantly | Memory | [Memory Leaks](#memory-leaks) |
| Transport layer errors | Transport | [Transport Layer Issues](docs/troubleshooting/TRANSPORT_ISSUES.md) |

---

## Master Diagnostic Flowchart

```mermaid
graph TB
    START([Start: Problem Detected]) --> CHECK{What is the symptom?}

    CHECK -->|Cannot connect| CONN[Connection Issues]
    CHECK -->|Auth failed| AUTH[Authentication Problems]
    CHECK -->|Rate limit| RATE[Rate Limiting Issues]
    CHECK -->|Slow response| PERF[Performance Problems]
    CHECK -->|Message error| MSG[Message/Protocol Errors]
    CHECK -->|Resource error| RES[Resource/Subscription Issues]
    CHECK -->|Server crash| CRASH[Crashes/Restarts]
    CHECK -->|Memory growing| MEM[Memory Leaks]

    CONN --> DIAG1[Run Diagnostics]
    AUTH --> DIAG2[Run Diagnostics]
    RATE --> DIAG3[Run Diagnostics]
    PERF --> DIAG4[Run Diagnostics]
    MSG --> DIAG5[Run Diagnostics]
    RES --> DIAG6[Run Diagnostics]
    CRASH --> DIAG7[Run Diagnostics]
    MEM --> DIAG8[Run Diagnostics]

    DIAG1 --> FIX1[Apply Solution]
    DIAG2 --> FIX2[Apply Solution]
    DIAG3 --> FIX3[Apply Solution]
    DIAG4 --> FIX4[Apply Solution]
    DIAG5 --> FIX5[Apply Solution]
    DIAG6 --> FIX6[Apply Solution]
    DIAG7 --> FIX7[Apply Solution]
    DIAG8 --> FIX8[Apply Solution]

    FIX1 --> VERIFY{Resolved?}
    FIX2 --> VERIFY
    FIX3 --> VERIFY
    FIX4 --> VERIFY
    FIX5 --> VERIFY
    FIX6 --> VERIFY
    FIX7 --> VERIFY
    FIX8 --> VERIFY

    VERIFY -->|Yes| DONE([Issue Resolved])
    VERIFY -->|No| ESCALATE([Need More Help])

    style START fill:#e8f5e9,stroke:#2e7d32
    style DONE fill:#e8f5e9,stroke:#2e7d32
    style ESCALATE fill:#fff3e0,stroke:#ef6c00
    style CONN fill:#e3f2fd,stroke:#1565c0
    style AUTH fill:#e3f2fd,stroke:#1565c0
    style RATE fill:#e3f2fd,stroke:#1565c0
    style PERF fill:#e3f2fd,stroke:#1565c0
    style MSG fill:#e3f2fd,stroke:#1565c0
    style RES fill:#e3f2fd,stroke:#1565c0
    style CRASH fill:#e3f2fd,stroke:#1565c0
    style MEM fill:#e3f2fd,stroke:#1565c0
```

---

## Diagnostic Tools Overview

### Quick Access Tools

```bash
# Health check (comprehensive)
curl http://localhost:9090/health

# Metrics endpoint
curl http://localhost:9090/metrics

# Erlang shell connection
erl -name debug@127.0.0.1 -setcookie erlmcp -remsh erlmcp@hostname

# Observer GUI (if available)
observer:start()
```

### Tool Categories

| Tool | Purpose | Usage |
|------|---------|-------|
| **observer** | Visual process inspection | `observer:start()` in Erlang shell |
| **recon** | Runtime diagnostics | `recon:proc_count(memory)` |
| **erlmcp_metrics** | Performance metrics | `GET /metrics` HTTP endpoint |
| **logger** | Log inspection | `logger:get_config()` |
| **debugger** | Step-through debugging | `debugger:start()` |
| **fprof** | Performance profiling | `fprof:apply(Module, Fun, Args)` |

---

## Connection Issues

### Symptom: Cannot connect to server

**Diagnostic Flowchart**:

```mermaid
graph TB
    START([❌ Cannot connect to server]) --> Q1{Is server running?}

    Q1 -->|No| CHECK1["Check: ps aux | grep beam<br/>or: observer_cli"]
    CHECK1 --> START1["Start server:<br/>erlmcp_app:startpermanent<br/>rebar3 shell"]

    Q1 -->|Yes| Q2{Is server bound<br/>to correct address?}

    Q2 -->|No| BIND_FIX["Update config/sys.config<br/>Set: http_bind_address"]
    Q2 -->|Yes| Q3{Is port accessible?}

    Q3 -->|No| PORT_FIX["Open firewall or<br/>use different port"]
    Q3 -->|Yes| Q4{TLS enabled?}

    Q4 -->|Yes| TLS_CHECK["Verify certificates<br/>Check certfile, keyfile<br/>Verify verify_mode"]
    Q4 -->|No| DONE([✅ Connection established])

    START1 --> DONE
    BIND_FIX --> DONE
    PORT_FIX --> DONE
    TLS_CHECK --> DONE

    style START fill:#ffebee,stroke:#c62828
    style DONE fill:#e8f5e9,stroke:#2e7d32
    style Q1 fill:#fff9c4,stroke:#f57f17
    style Q2 fill:#fff9c4,stroke:#f57f17
    style Q3 fill:#fff9c4,stroke:#f57f17
    style Q4 fill:#fff9c4,stroke:#f57f17
```

**Step-by-Step Diagnosis**:

1. **Check if server is running**:
   ```bash
   # Check for beam process
   ps aux | grep beam

   # Or use observer_cli
   observer_cli

   # Check if listening
   netstat -an | grep 8080
   ```

2. **Verify binding address**:
   ```erlang
   % Check configuration
   application:get_env(erlmcp_core, http_bind_address).
   % Should return: {ok, {127, 0, 0, 1}} or similar
   ```

3. **Test port accessibility**:
   ```bash
   # Test TCP connection
   telnet localhost 8080

   # Or use nc
   nc -zv 127.0.0.1 8080
   ```

4. **Check TLS configuration** (if applicable):
   ```erlang
   % Verify TLS settings
   application:get_env(erlmcp_core, https_config).
   ```

**Solutions**:

- Server not running: Start it with `erlmcp_app:startpermanent` or `rebar3 shell`
- Wrong binding: Update `http_bind_address` in `config/sys.config`
- Firewall blocking: Open port or use different port
- TLS issues: Verify certificate files exist and `verify_mode` is correct

---

## Authentication Problems

### Symptom: Authentication failed

**Error Code Flowchart**:

```mermaid
graph TB
    START([❌ Authentication failed]) --> Q1{What error code?}

    Q1 -->|1011-1013<br/>Auth Failed| CREDS{Credentials<br/>valid?}
    Q1 -->|1015<br/>Missing Auth| ADD_CREDS["Add credentials:<br/>Authorization header<br/>MCP-Session-Id"]
    Q1 -->|1016<br/>Invalid Session| FIX_SESSION["Session ID format:<br/>32+ hex characters<br/>Use: erlmcp_session:create"]
    Q1 -->|1014<br/>Forbidden| CHECK_PERM["Check permissions<br/>Contact admin<br/>Review ACL"]

    CREDS -->|No| UPDATE_CREDS["Update credentials<br/>Check oauth.client_id<br/>Check oauth.client_secret"]
    CREDS -->|Yes| TOKEN{Token<br/>expired?}

    TOKEN -->|Yes| REFRESH["Refresh token<br/>Get new token"]
    TOKEN -->|No| CHECK_CONFIG["Check auth config<br/>Verify auth method"]

    ADD_CREDS --> DONE([✅ Auth working])
    FIX_SESSION --> DONE
    CHECK_PERM --> DONE
    UPDATE_CREDS --> DONE
    REFRESH --> DONE
    CHECK_CONFIG --> DONE

    style START fill:#ffebee,stroke:#c62828
    style DONE fill:#e8f5e9,stroke:#2e7d32
    style Q1 fill:#fff9c4,stroke:#f57f17
    style CREDS fill:#fff9c4,stroke:#f57f17
    style TOKEN fill:#fff9c4,stroke:#f57f17
```

**Error Code Reference**:

| Code | Meaning | Solution |
|------|---------|----------|
| 1011 | Authentication failed | Check credentials, verify auth method |
| 1012 | Invalid credentials | Update username/password or API key |
| 1013 | Token expired | Refresh token |
| 1014 | Forbidden | Check permissions, contact administrator |
| 1015 | Missing auth | Add `Authorization` or `MCP-Session-Id` header |
| 1016 | Invalid session ID | Use correct format (32+ hex chars) |

**Diagnosis Steps**:

1. **Check error code in response**:
   ```bash
   # View full error response
   curl -v http://localhost:8080/tools/list \
     -H "Authorization: Bearer invalid_token"
   ```

2. **Verify credentials**:
   ```erlang
   % Check OAuth configuration
   application:get_env(erlmcp_core, oauth).

   % Verify session
   erlmcp_session_manager:get_session(<<"session_id">>).
   ```

3. **Test with valid credentials**:
   ```bash
   # Get valid token
   TOKEN=$(curl -X POST http://localhost:8080/auth/token \
     -d "client_id=xxx&client_secret=xxx" | jq -r '.access_token')

   # Use token
   curl http://localhost:8080/tools/list \
     -H "Authorization: Bearer $TOKEN"
   ```

---

## Rate Limiting Issues

### Symptom: Rate limit exceeded

**Rate Limit Decision Tree**:

```mermaid
graph TB
    START([❌ Rate limit exceeded<br/>Error 1056-1060]) --> Q1{Which type?}

    Q1 -->|1056<br/>Overall| REDUCE["Reduce request rate<br/>Implement backoff<br/>Wait before retry"]
    Q1 -->|1057<br/>Per-second| CHECK_PER_SEC["Max 100 req/s<br/>Spread out requests"]
    Q1 -->|1058<br/>Per-minute| CHECK_PER_MIN["Max 5000 req/min<br/>Batch requests"]
    Q1 -->|1059<br/>Quota| UPGRADE["Wait for reset<br/>Upgrade plan<br/>Adjust quota"]
    Q1 -->|1060<br/>Connection| CLOSE_CONN["Close unused connections<br/>Increase max_connections"]

    REDUCE --> CONFIG{Need higher<br/>limits?}
    CHECK_PER_SEC --> CONFIG
    CHECK_PER_MIN --> CONFIG
    UPGRADE --> CONFIG
    CLOSE_CONN --> CONFIG

    CONFIG -->|Yes| UPDATE_CONFIG["Edit config/sys.config<br/>Adjust rate_limiting section"]
    CONFIG -->|No| DONE([✅ Requests flowing])

    UPDATE_CONFIG --> DONE

    style START fill:#ffebee,stroke:#c62828
    style DONE fill:#e8f5e9,stroke:#2e7d32
    style Q1 fill:#fff9c4,stroke:#f57f17
    style CONFIG fill:#fff9c4,stroke:#f57f17
```

**Rate Limit Configuration**:

```erlang
{erlmcp_core, [
    {rate_limiting, [
        {enabled, true},
        {overall_limit, 10000},        % Total requests
        {per_second_limit, 100},       % Max 100 req/s
        {per_minute_limit, 5000},      % Max 5000 req/min
        {connection_limit, 100},       % Max concurrent connections
        {quota_limit, 1000000}         % Monthly quota
    ]}
]}.
```

**Solutions**:

- Implement exponential backoff:
  ```erlang
  %% Backoff strategy
 {retry_policy, [
      {max_retries, 3},
      {base_delay_ms, 1000},
      {max_delay_ms, 10000},
      {multiplier, 2.0}
  ]}
  ```

- Adjust limits in configuration (if appropriate)

---

## Performance Problems

### Symptom: Slow response times / high latency

**Performance Diagnostic Flow**:

```mermaid
graph TB
    START([❌ Slow response / High latency]) --> METRICS["Check metrics:<br/>GET /metrics<br/>Review p95, p99"]

    METRICS --> P95{p95 > 100ms?}

    P95 -->|Yes| CPU{CPU > 80%?}
    P95 -->|No| MEMORY{Memory > 85%?}

    CPU -->|Yes| CPU_FIX["Reduce load<br/>Enable backpressure<br/>Scale horizontally"]
    CPU -->|No| MEMORY

    MEMORY -->|Yes| MEM_FIX["Enable hibernation<br/>Increase cleanup_interval<br/>Check for leaks"]
    MEMORY -->|No| QUEUE{Queue > 80%?}

    QUEUE -->|Yes| QUEUE_FIX["Backpressure active<br/>Reduce request rate<br/>Increase queue_limits"]
    QUEUE -->|No| CB{Circuit<br/>breaker<br/>open?}

    CB -->|Yes| CB_FIX["Check error logs<br/>Wait for cooldown 30s<br/>Verify dependencies"]
    CB -->|No| PROFILE["Profile code<br/>fprof:apply<br/>Optimize hot paths"]

    CPU_FIX --> DONE([✅ Performance optimized])
    MEM_FIX --> DONE
    QUEUE_FIX --> DONE
    CB_FIX --> DONE
    PROFILE --> DONE

    style START fill:#ffebee,stroke:#c62828
    style DONE fill:#e8f5e9,stroke:#2e7d32
    style P95 fill:#fff9c4,stroke:#f57f17
    style CPU fill:#fff9c4,stroke:#f57f17
    style MEMORY fill:#fff9c4,stroke:#f57f17
    style QUEUE fill:#fff9c4,stroke:#f57f17
    style CB fill:#fff9c4,stroke:#f57f17
```

**Performance Diagnosis**:

1. **Check metrics**:
   ```bash
   # View current metrics
   curl http://localhost:9090/metrics | grep latency

   # Look for:
   # - erlmcp_latency_p50_us
   # - erlmcp_latency_p95_us
   # - erlmcp_latency_p99_us
   ```

2. **Monitor CPU**:
   ```erlang
   % Check scheduler utilization
   observer:start().
   % Look at Load Charts tab

   % Or use recon
   recon:load(avg10).
   ```

3. **Check memory**:
   ```erlang
   % Total memory
   erlang:memory().

   % Per-process memory
   recon:proc_count(memory, 10).

   % Binary heap
   recon:bin_leak(100).
   ```

4. **Inspect message queues**:
   ```erlang
   % Processes with large message queues
   recon:proc_count(message_queue_len, 10).
   ```

5. **Check circuit breaker**:
   ```erlang
   % Circuit breaker state
   erlmcp_circuit_breaker:get_state(service_name).
   ```

**Solutions**:

- Enable hibernation:
  ```erlang
  {hibernate_after, 30000}  % Hibernate after 30s idle
  ```

- Increase cleanup intervals:
  ```erlang
  {cleanup_interval, 60000}  % Run cleanup every 60s
  ```

- Enable backpressure:
  ```erlang
  {backpressure, [
      {enabled, true},
      {threshold, 0.8},  % Trigger at 80% capacity
      {strategy, drop_tail}
  ]}
  ```

---

## Message/Protocol Errors

### Symptom: Message validation failures

**Protocol Error Decision Tree**:

```mermaid
graph TB
    START([❌ Message errors<br/>Error 1021-1070]) --> Q1{Which error?}

    Q1 -->|1021-1029<br/>Invalid params| VALIDATE["Validate JSON schema<br/>Check field types<br/>Verify required fields"]
    Q1 -->|1023<br/>Invalid URI| URI_FIX["Use valid URI format<br/>https://example.com/path<br/>Check URI validator"]
    Q1 -->|1068<br/>Too large| COMPRESS["Compress data<br/>Split into chunks<br/>Check message_size_limits"]
    Q1 -->|1066<br/>Protocol| PROTO_CHECK["Validate JSON-RPC<br/>Check MCP version<br/>Review message_parser logs"]
    Q1 -->|1067<br/>Transport| NET_CHECK["Check network stability<br/>Verify transport health<br/>Review transport logs"]

    VALIDATE --> DONE([✅ Messages valid])
    URI_FIX --> DONE
    COMPRESS --> DONE
    PROTO_CHECK --> DONE
    NET_CHECK --> DONE

    style START fill:#ffebee,stroke:#c62828
    style DONE fill:#e8f5e9,stroke:#2e7d32
    style Q1 fill:#fff9c4,stroke:#f57f17
```

**Common Message Errors**:

| Error | Meaning | Solution |
|-------|---------|----------|
| 1021 | Missing required field | Add required field |
| 1022 | Invalid field type | Correct field type |
| 1023 | Invalid URI | Use valid URI format |
| 1066 | Protocol error | Validate JSON-RPC format |
| 1067 | Transport error | Check network/transport |
| 1068 | Message too large | Compress or chunk |

**Validation Tools**:

```erlang
% Validate JSON schema
erlmcp_json_rpc:validate_request(JsonMap).

% Check message size
erlmcp_message_size:validate_message(Binary).

% Parse URI
erlmcp_uri_validator:parse(Uri).
```

---

## Resource/Subscription Issues

### Symptom: Resource/tool/prompt not found

**Resource Troubleshooting Flow**:

```mermaid
graph TB
    START([❌ Resource errors<br/>Error 1046-1052]) --> Q1{Which error?}

    Q1 -->|1046-1050<br/>Not found| LIST_RES["List available resources:<br/>erlmcp_server:list_resources<br/>erlmcp_server:list_tools"]
    Q1 -->|1047-1052<br/>Duplicate| DELETE["Delete existing first:<br/>erlmcp_server:delete_resource<br/>erlmcp_server:delete_tool"]
    Q1 -->|Subscription<br/>issues| SUB_CHECK{"Subscription<br/>limit reached?"}
    Q1 -->|Notification<br/>issues| NOTIF_FIX["Check notification handler<br/>Verify resource updated events"]

    LIST_RES --> USE_VALID["Use valid URI/name<br/>Check spelling<br/>Verify resource exists"]
    USE_VALID --> DONE([✅ Resources working])
    DELETE --> DONE

    SUB_CHECK -->|Yes| SUB_LIMIT["Unsubscribe old ones<br/>Increase max_subscriptions"]
    SUB_CHECK -->|No| CHECK_HANDLER["Check event handler<br/>Review subscription callback"]
    SUB_LIMIT --> DONE
    CHECK_HANDLER --> DONE
    NOTIF_FIX --> DONE

    style START fill:#ffebee,stroke:#c62828
    style DONE fill:#e8f5e9,stroke:#2e7d32
    style Q1 fill:#fff9c4,stroke:#f57f17
    style SUB_CHECK fill:#fff9c4,stroke:#f57f17
```

**Resource Management Commands**:

```erlang
% List available resources
erlmcp_server:list_resources().

% List tools
erlmcp_server:list_tools().

% List prompts
erlmcp_server:list_prompts().

% Check subscriptions
erlmcp_resource_subscriptions:list(ServerId).

% Subscribe to resource
erlmcp_resource_subscriptions:subscribe(ServerId, ResourceUri).

% Unsubscribe
erlmcp_resource_subscriptions:unsubscribe(ServerId, ResourceUri).
```

**Configuration**:

```erlang
{erlmcp_core, [
    {max_subscriptions_per_server, 10000},
    {subscription_cleanup_interval, 60000}
]}.
```

---

## Crashes/Restarts

### Symptom: Server crashes or restarts

**Crash Analysis Flowchart**:

```mermaid
graph TB
    START([❌ Server crashes or restarts]) --> LOGS["Check crash logs:<br/>logs/erlmcp.log<br/>Observer → /home<br/>rb:grep CRASH"]

    LOGS --> Q1{Crash pattern?}

    Q1 -->|badmatch<br/>function_clause| TYPE_FIX["Type mismatch error<br/>Check function signatures<br/>Verify record definitions"]
    Q1 -->|case_clause| CASE_FIX["Missing case pattern<br/>Add catch-all clause<br/>Review case statements"]
    Q1 -->|timeout| TIMEOUT_FIX["Increase timeout value<br/>Check for blocking calls<br/>Use async: handle_cast"]
    Q1 -->|try_clause| TRY_FIX["Uncaught exception<br/>Add try...catch<br/>Improve error handling"]
    Q1 -->|Supervisor<br/>loop| SUP_FIX["Permanent child crashing<br/>Check init/1 return value<br/>Review child specs"]

    TYPE_FIX --> DONE([✅ Crashes resolved])
    CASE_FIX --> DONE
    TIMEOUT_FIX --> DONE
    TRY_FIX --> DONE
    SUP_FIX --> DONE

    style START fill:#ffebee,stroke:#c62828
    style DONE fill:#e8f5e9,stroke:#2e7d32
    style Q1 fill:#fff9c4,stroke:#f57f17
```

**Crash Diagnosis**:

1. **Find crash logs**:
   ```bash
   # Search for CRASH reports
   grep -i "crash" /var/log/erlmcp/erlmcp.log | tail -50

   # In Erlang shell
   rb:list().
   rb:show(Ref).
   ```

2. **Analyze crash dump** (if available):
   ```erlang
   % Load crash dump
   webtool:start().

   % Or analyze manually
   erl_crash_dump_analyzer:analyze("erl_crash.dump").
   ```

3. **Check supervisor restarts**:
   ```erlang
   % Get child processes
   supervisor:which_children(erlmcp_core_sup).

   % Check restart counts
   recon:proc_count(reductions, 10).
   ```

**Common Crash Patterns**:

| Pattern | Cause | Solution |
|---------|-------|----------|
| badmatch | Pattern match failure | Check data types |
| function_clause | No matching function clause | Add catch-all clause |
| case_clause | No matching case branch | Add default case |
| timeout | Operation exceeded timeout | Increase timeout |
| try_clause | Uncaught exception | Add try...catch |

---

## Memory Leaks

### Symptom: Memory growing constantly

**Memory Leak Detection Flow**:

```mermaid
graph TB
    START([❌ Memory growing constantly]) --> MONITOR["Monitor memory:<br/>erlmcp_memory_monitor<br/>observer:memory()<br/>recon:allocaters 1000"]

    MONITOR --> Q1{Leak pattern?}

    Q1 -->|Per-process<br/>growth| PROC_CHECK["Check large processes<br/>recon:proc_count memory<br/>Identify top consumers"]
    Q1 -->|ETS table<br/>growth| ETS_CHECK["ets:i<br/>Check table sizes<br/>Review ets:tab2list"]
    Q1 -->|Binary heap<br/>growth| BIN_CHECK["Check binary refs<br/>recon:bin_leak 100<br/>Force GC: garbage_collect"]
    Q1 -->|Port<br/>accumulation| PORT_CHECK["Check open ports<br/>erlang:ports<br/>Close unused ports"]

    PROC_CHECK --> HIBERNATE["Enable hibernation<br/>hibernate_after: 30000"]
    ETS_CHECK --> CLEANUP["Increase cleanup intervals<br/>lifecycle_management.cleanup<br/>Enable TTL on resources"]
    BIN_CHECK --> CLEANUP
    PORT_CHECK --> GUARD["Set memory guards<br/>memory_guard.enable = true<br/>cpu_guard.enable = true"]

    HIBERNATE --> DONE([✅ Memory stable])
    CLEANUP --> DONE
    GUARD --> DONE

    style START fill:#ffebee,stroke:#c62828
    style DONE fill:#e8f5e9,stroke:#2e7d32
    style Q1 fill:#fff9c4,stroke:#f57f17
```

**Memory Diagnosis Tools**:

```erlang
% Total memory breakdown
erlang:memory().

% Per-process memory
recon:proc_count(memory, 10).

% Binary leak detection
recon:bin_leak(100).

% ETS table sizes
ets:i().

% Port count
length(erlang:ports()).

% Force garbage collection
garbage_collect(Pid).
```

**Memory Leak Solutions**:

1. **Enable hibernation**:
   ```erlang
   {hibernate_after, 30000}  % After 30s idle
   ```

2. **Increase cleanup intervals**:
   ```erlang
   {cleanup_interval, 60000},
   {lifecycle_management, [
       {session_ttl, 3600000},
       {enable_ttl, true}
   ]}
   ```

3. **Enable memory guards**:
   ```erlang
   {memory_guard, [
       {enable, true},
       {threshold_mb, 4096},
       {action, throttle}
   ]},
   {cpu_guard, [
       {enable, true},
       {threshold_percent, 90}
   ]}
   ```

4. **Bounded ETS tables**:
   ```erlang
   ets:new(table_name, [
       named_table,
       public,
       {max_size, 10000},
       {heir, None}
   ]).
   ```

---

## Getting More Help

### Information to Collect

When seeking help, please collect:

1. **System Information**:
   ```bash
   erl -version
   uname -a
   ```

2. **erlmcp Version**:
   ```bash
   rebar3 version
   git log -1 --oneline
   ```

3. **Configuration**:
   ```bash
   cat config/sys.config
   ```

4. **Recent Logs**:
   ```bash
   tail -n 100 logs/erlmcp.log
   ```

5. **Error Messages**:
   - Full error text
   - Stack traces
   - Context around error

### Where to Get Help

- **Documentation**: [erlmcp Documentation](https://github.com/erlmcp/erlmcp)
- **GitHub Issues**: [Issue Tracker](https://github.com/erlmcp/erlmcp/issues)
- **Troubleshooting Guides**: See additional guides below

### Additional Troubleshooting Guides

- [Transport Layer Issues](TRANSPORT_ISSUES.md) - TCP, HTTP, WebSocket, SSE troubleshooting
- [Error Handling Guide](ERROR_HANDLING.md) - Comprehensive error code reference
- [Debugging Guide](DEBUGGING_GUIDE.md) - Advanced debugging techniques
- [Common Issues](common-issues.md) - Frequently encountered problems

---

**Status**: Published
**Review Date**: 2025-02-28
**Next Update**: 2025-03-31
