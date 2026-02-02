# Production Runbook - erlmcp on OTP 28.3.1

**Project**: erlmcp v2.1.0
**Target Runtime**: Erlang/OTP 28.3.1+
**Document Status**: PRODUCTION READY
**Last Updated**: 2026-02-02

---

## Table of Contents

1. [System Overview](#1-system-overview)
2. [Startup Procedures](#2-startup-procedures)
3. [Monitoring Procedures](#3-monitoring-procedures)
4. [Operational Procedures](#4-operational-procedures)
5. [Troubleshooting](#5-troubleshooting)
6. [Emergency Procedures](#6-emergency-procedures)
7. [Rollback Procedures](#7-rollback-procedures)
8. [Maintenance Procedures](#8-maintenance-procedures)
9. [OTP 28 Feature Operations](#9-otp-28-feature-operations)
10. [Appendices](#10-appendices)

---

## 1. System Overview

### 1.1 Architecture

erlmcp is an Erlang/OTP MCP (Model Context Protocol) SDK optimized for OTP 28.3.1+, supporting JSON-RPC 2.0 over multiple transports with production-grade reliability.

```
┌─────────────────────────────────────────────────────────────────────┐
│                        erlmcp Production Node                       │
├─────────────────────────────────────────────────────────────────────┤
│                                                                      │
│  ┌────────────────────────────────────────────────────────────┐    │
│  │  erlmcp_sup (TIER 1 - one_for_all)                        │    │
│  │  ├─ erlmcp_core_sup (TIER 2 - simple_one_for_one)         │    │
│  │  │  ├─ erlmcp_server_sup (TIER 3 - per connection)        │    │
│  │  │  ├─ erlmcp_client_sup                                  │    │
│  │  │  └─ erlmcp_session_sup                                 │    │
│  │  ├─ erlmcp_transports_sup                                  │    │
│  │  │  ├─ erlmcp_transport_tcp                               │    │
│  │  │  ├─ erlmcp_transport_http                              │    │
│  │  │  ├─ erlmcp_transport_ws                                │    │
│  │  │  └─ erlmcp_transport_sse                               │    │
│  │  └─ erlmcp_observability_sup                              │    │
│  │     ├─ erlmcp_otel                                        │    │
│  │     ├─ erlmcp_metrics                                     │    │
│  │     └─ erlmcp_dashboard                                   │    │
│  └────────────────────────────────────────────────────────────┘    │
│                                                                      │
│  OTP 28 Features:                                                     │
│  ├─ Native JSON (json:encode/1, json:decode/1)                     │
│  ├─ Process Hibernation (hibernate_after/0)                        │
│  ├─ Priority Messaging ([priority] message option)                 │
│  ├─ Memory Guards (erlmcp_memory_guard)                            │
│  ├─ PCRE2 Regex (re:run/2)                                         │
│  ├─ TLS 1.3 (ssl:tls_version('tlsv1.3'))                          │
│  └─ Two-Phase Loading (code:prepare_loading/1)                     │
│                                                                      │
└─────────────────────────────────────────────────────────────────────┘
```

### 1.2 Applications

| Application | Modules | Purpose |
|-------------|---------|---------|
| erlmcp_core | 97 | Protocol, sessions, auth, secrets, LLM |
| erlmcp_transports | 23 | stdio, tcp, http, ws, sse |
| erlmcp_observability | 31 | OTEL, metrics, tracing, dashboard |
| erlmcp_validation | 13 | Compliance, spec parsing |

### 1.3 Resource Requirements

| Resource | Minimum | Recommended | Maximum |
|----------|---------|-------------|---------|
| CPU | 2 cores | 4-8 cores | 16 cores |
| RAM | 4GB | 8-16GB | 64GB |
| Disk | 10GB | 50GB | 500GB |
| Connections | 1,000 | 50,000 | 100,000 |
| File Descriptors | 10,000 | 65,536 | 131,072 |

---

## 2. Startup Procedures

### 2.1 Initial Startup

```bash
# 1. Verify OTP 28.3.1 is installed
erl -eval 'io:format("~s~n", [erlang:system_info(otp_release)]), halt().' -noshell
# Expected: 28

# 2. Navigate to release directory
cd /opt/erlmcp

# 3. Start the application
./bin/erlmcp start

# 4. Verify startup
./bin/erlmcp ping
# Expected: pong

# 5. Check health
curl http://localhost:8080/health
# Expected: {"status":"healthy"}
```

### 2.2 Interactive Console

```bash
# Start with interactive console
./bin/erlmcp console

# Or attach to running node
./bin/erlmcp attach

# Detach (Ctrl+G, then q, Enter)
# Or: user_switch:quit().
```

### 2.3 Remote Shell

```bash
# Connect to remote node
erl -name admin@$(hostname) -setcookie erlmcp_cluster -remsh erlmcp@target

# Once connected, run commands
erlmcp:status().
erlmcp_registry:info().
```

### 2.4 Observer (GUI)

```bash
# Start observer on local node
./bin/erlmcp observer

# Connect to remote node
erl -name observer@$(hostname) -setcookie erlmcp_cluster -hidden \
  -eval "observer:start('erlmcp@target')."
```

### 2.5 Startup Verification Checklist

- [ ] OTP 28.3.1 detected
- [ ] All 4 applications started
- [ ] ETS tables initialized
- [ ] Ports listening (stdio, tcp, http, etc.)
- [ ] Health endpoint responding
- [ ] Metrics endpoint accessible
- [ ] No crash dumps generated
- [ ] No errors in startup logs

---

## 3. Monitoring Procedures

### 3.1 Health Checks

#### Overall System Health

```bash
curl http://localhost:8080/health

# Expected response:
{
  "status": "healthy",
  "version": "2.1.0",
  "uptime_seconds": 3600,
  "checks": {
    "transport": {"status": "healthy"},
    "session": {"status": "healthy"},
    "registry": {"status": "healthy"},
    "secrets": {"status": "healthy"}
  }
}
```

#### Subsystem Health

```bash
# Transport health
curl http://localhost:8080/health/transport

# Session health
curl http://localhost:8080/health/session

# Registry health
curl http://localhost:8080/health/registry

# Secrets health
curl http://localhost:8080/health/secrets
```

### 3.2 Metrics

#### Prometheus Metrics

```bash
# All metrics
curl http://localhost:9090/metrics

# Key metrics to watch:
curl http://localhost:9090/metrics | grep erlmcp_requests_total
curl http://localhost:9090/metrics | grep erlmcp_errors_total
curl http://localhost:9090/metrics | grep erlmcp_latency_seconds
curl http://localhost:9090/metrics | grep erlmcp_connections_active
```

#### Erlang VM Metrics

```erlang
# In remote console
erlmcp:vm_metrics().

# Or manually:
erlang:memory(total).
erlang:system_info(process_count).
erlang:statistics(run_queue).
```

### 3.3 Logging

#### View Logs

```bash
# Real-time log tail
tail -f /opt/erlmcp/log/erlang.log.1

# Error logs only
tail -f /opt/erlmcp/log/erlang.log.1 | grep ERROR

# With context (5 lines before/after)
tail -f /opt/erlmcp/log/erlang.log.1 | grep -C 5 ERROR
```

#### Log Levels

```erlang
# Change log level dynamically (in console)
logger:set_application_level(erlmcp, warning).
logger:set_application_level(erlmcp, error).
logger:set_application_level(erlmcp, info).
logger:set_application_level(erlmcp, debug).
```

### 3.4 Alert Thresholds

| Metric | Warning | Critical | Action |
|--------|---------|----------|--------|
| Memory usage | > 80% | > 90% | Investigate leaks |
| CPU usage | > 80% | > 95% | Scale horizontally |
| Process count | > 400,000 | > 450,000 | Check for leaks |
| Connections | > 45,000 | > 50,000 | Scale horizontally |
| Response time (P95) | > 100ms | > 500ms | Investigate latency |
| Error rate | > 1% | > 5% | Check logs for errors |
| Queue depth | > 1000 | > 5000 | Check bottlenecks |

---

## 4. Operational Procedures

### 4.1 Hot Code Reload

```erlang
# In remote console

# Reload a single module
l(Module).

# Reload and verify
code:load_file(Module).
{module, Module} = code:load_file(Module).

# OTP 28: Two-phase atomic reload
Modules = [module1, module2, module3],
{ok, Prepared} = code:prepare_loading(Modules),
ok = code:finish_loading(Prepared).
```

### 4.2 Connection Management

```erlang
# List active connections
erlmcp_registry:count(transport).
erlmcp_registry:list(transport).

# Disconnect specific connection
erlmcp_server:disconnect(ConnectionId).

# Disconnect all connections for transport
erlmcp_transport_tcp:disconnect_all().
```

### 4.3 Session Management

```erlang
# List active sessions
erlmcp_session:list().

# Get session details
erlmcp_session:get(SessionId).

# Terminate session
erlmcp_session:terminate(SessionId, Reason).

# Session statistics
erlmcp_session:stats().
```

### 4.4 Process Inspection

```erlang
# Find processes for a module
erlang:processes(),
[P || {M, _} <- [element(2, c:pi(P, dictionary)) || P <- erlang:processes()], M =:= Module].

# Process info
erlang:process_info(Pid, [memory, message_queue_len, current_function]).
```

---

## 5. Troubleshooting

### 5.1 Application Won't Start

**Symptoms**:
- `erlmcp ping` returns `pang`
- No listening ports
- Crash dumps in log directory

**Diagnosis**:

```bash
# 1. Check crash dump
cat /opt/erlmcp/log/erl_crash.dump

# 2. Check logs for errors
grep -i "error" /opt/erlmcp/log/erlang.log.1 | tail -50

# 3. Check port conflicts
sudo lsof -i :8080
sudo lsof -i :4369  # EPMD
```

**Common Solutions**:

1. **Port already in use**
   ```bash
   # Kill process using port
   sudo kill -9 $(sudo lsof -t -i:8080)
   ```

2. **EPMD not running**
   ```bash
   # Start EPMD
   epmd -daemon
   ```

3. **Cookie mismatch**
   ```bash
   # Verify cookie in vm.args
   -setcookie erlmcp_cluster
   ```

4. **Node name conflict**
   ```bash
   # Use unique node name
   -name erlmcp$(date +%s)@127.0.0.1
   ```

### 5.2 High Memory Usage

**Symptoms**:
- Memory > 80%
- Continuous memory growth
- OOM errors

**Diagnosis**:

```erlang
# 1. Check memory breakdown
erlang:memory().
erlang:memory(types).
erlang:memory(used).
erlang:memory(atom).

# 2. Find largest processes
Top = lists:sublist(
  lists:sort(
    fun({_, A}, {_, B}) -> A > B end,
    [{P, element(2, erlang:process_info(P, memory))} || P <- erlang:processes()]
  ),
  10).

# 3. Check ETS tables
ets:all().
[ets:info(T, size) || T <- ets:all()].

# 4. Check binaries
erlang:memory(binary).
erlang:garbage_collect().
```

**Solutions**:

1. **Force garbage collection**
   ```erlang
   erlang:garbage_collect().
   [erlang:garbage_collect(P) || P <- erlang:processes()].
   ```

2. **Hibernation (OTP 28)**
   ```erlang
   % Hibernate idle processes
   sys:replace_state(ProcPid, fun(State) -> State end).
   ```

3. **Memory guard activation**
   ```erlang
   erlmcp_memory_guard:activate().
   ```

### 5.3 High CPU Usage

**Symptoms**:
- CPU > 80%
- Slow response times

**Diagnosis**:

```erlang
# 1. Check run queue
erlang:statistics(run_queue).

# 2. Find busy processes
[L || {_, L} <- [{P, element(2, erlang:process_info(P, current_function))} || P <- erlang:processes()],
  lists:prefix(L, [erlmcp, ...])].

# 3. Check scheduler utilization
erlang:statistics(scheduler_wall_time).
```

**Solutions**:

1. **Reduce log level**
   ```erlang
   logger:set_application_level(erlmcp, warning).
   ```

2. **Increase async threads**
   ```bash
   # Add to vm.args
   +A 128
   ```

3. **Scale horizontally**
   ```bash
   # Add more nodes to cluster
   ```

### 5.4 Connection Failures

**Symptoms**:
- Clients can't connect
- Connection timeouts
- TLS errors

**Diagnosis**:

```bash
# 1. Check listening ports
sudo netstat -tlnp | grep beam

# 2. Test TCP connection
telnet localhost 8080

# 3. Test TLS
openssl s_client -connect localhost:8443

# 4. Check file descriptors
ulimit -n
```

**Solutions**:

1. **Increase file descriptors**
   ```bash
   ulimit -n 65536
   ```

2. **Verify TLS certificates**
   ```bash
   openssl x509 -in /etc/erlmcp/certs/server.crt -noout -dates
   ```

3. **Check firewall rules**
   ```bash
   sudo iptables -L | grep 8080
   ```

---

## 6. Emergency Procedures

### 6.1 Crash Recovery

**When**: Application crashes completely

```bash
# 1. Check crash dump
ls -lt /opt/erlmcp/log/*.dump | head -1

# 2. Analyze crash dump (if possible)
erl -eval "{ok, Dump} = file:read_file(\"/opt/erlmcp/log/erl_crash.dump\"), io:format(\"~s\", [Dump]), halt()." -noshell

# 3. Clean and restart
./bin/erlmcp stop
sleep 5
rm -f /opt/erlmcp/log/*.dump
./bin/erlmcp start

# 4. Verify recovery
./bin/erlmcp ping
curl http://localhost:8080/health
```

### 6.2 Split-Brain Resolution

**When**: Cluster nodes disagree on state

```erlang
# 1. Identify split-brain
erlmcp_cluster:status().
erlmcp_cluster:nodes().

# 2. Choose master node
erlmcp_cluster:set_master('erlmcp@node1').

# 3. Force reconciliation
erlmcp_cluster:reconcile().

# 4. Verify consistency
erlmcp_cluster:verify().
```

### 6.3 Memory Emergency

**When**: OOM imminent or occurred

```erlang
# 1. Activate memory guard
erlmcp_memory_guard:emergency().

# 2. Force garbage collection everywhere
[erlang:garbage_collect(P) || P <- erlang:processes()],
erlang:garbage_collect().

# 3. Hibernate all processes
[sys:replace_state(P, fun(S) -> S end) || P <- erlang:processes(), is_process_alive(P)].

# 4. Restart if needed
init:restart().
```

### 6.4 DDoS Response

**When**: Under attack

```bash
# 1. Enable rate limiting (in console)
erlmcp_rate_limiter:enable().
erlmcp_rate_limiter:set_limit(100).  % Max 100 req/min

# 2. Block offending IPs
erlmcp_rate_limiter:block_ip(IPAddress).

# 3. Enable circuit breaker
erlmcp_circuit_breaker:activate().

# 4. Check firewall
sudo iptables -I INPUT -s OFFENDING_IP -j DROP
```

---

## 7. Rollback Procedures

### 7.1 Automatic Rollback

The deploy script automatically rolls back if:
- Application won't start
- Health check fails
- Smoke tests fail

### 7.2 Manual Rollback

```bash
# 1. Rollback to latest backup
./scripts/rollback.sh production

# 2. Rollback to specific backup
./scripts/rollback.sh production erlmcp-backup-20260201-120000

# 3. Verify rollback
./bin/erlmcp ping
curl http://localhost:8080/health

# 4. Check logs
tail -f /opt/erlmcp/log/erlang.log.1
```

### 7.3 Post-Rollback Verification

```bash
# 1. Health check
curl http://localhost:8080/health

# 2. Smoke tests
./scripts/smoke_tests.sh production

# 3. Metrics
curl http://localhost:9090/metrics

# 4. Check for errors
grep ERROR /opt/erlmcp/log/erlang.log.1 | tail -20
```

---

## 8. Maintenance Procedures

### 8.1 Daily Maintenance

```bash
# Automated via cron
0 2 * * * /opt/erlmcp/scripts/maintenance.sh --daily

# Manual trigger
/opt/erlmcp/scripts/maintenance.sh --daily

# Performs:
# - Log rotation
# - ETS table vacuum
# - Metrics cleanup
# - Health check
```

### 8.2 Weekly Maintenance

```bash
/opt/erlmcp/scripts/maintenance.sh --weekly

# Performs:
# - Full backup
# - Dialyzer PLT rebuild
# - Coverage report
# - Dependency check
```

### 8.3 Monthly Maintenance

```bash
/opt/erlmcp/scripts/maintenance.sh --monthly

# Performs:
# - Security audit
# - Dependency updates
# - Performance benchmarks
# - Capacity planning review
```

### 8.4 Certificate Rotation

```bash
# 1. Generate new certificates
openssl req -x509 -newkey rsa:4096 -keyout key.pem -out cert.pem -days 365 -nodes

# 2. Install certificates
sudo cp cert.pem /etc/erlmcp/certs/server.crt
sudo cp key.pem /etc/erlmcp/certs/server.key
sudo chmod 0600 /etc/erlmcp/certs/server.key

# 3. Reload configuration
./bin/erlmcp restart

# 4. Verify
openssl s_client -connect localhost:8443 -servername erlmcp
```

---

## 9. OTP 28 Feature Operations

### 9.1 Native JSON Operations

```erlang
% Encode (faster than jsx)
json:encode(#{<<"key">> => <<"value">>}).

% Decode (faster than jsx)
{ok, Term} = json:decode(<<"{\"key\":\"value\"}">>).

% Performance test
timer:tc(fun() ->
  lists:foreach(fun(_) ->
    json:encode(#{<<"test">> => <<"data">>})
  end, lists:seq(1, 10000))
end).
```

### 9.2 Process Hibernation

```erlang
% Enable hibernation for a gen_server
% In handle_info(timeout, State):
{noreply, State, hibernate}.

% Trigger immediate hibernation
sys:replace_state(ProcPid, fun(State) -> State end).

% Monitor hibernation
erlang:system_info(process_count).
erlang:memory(processes).
```

### 9.3 Priority Messaging

```erlang
% High priority message
erlang:send(Pid, CriticalMessage, [priority]).

% Normal priority message
erlang:send(Pid, NormalMessage).

% Monitor priority queue
erlang:system_info(message_queue_data).
```

### 9.4 Memory Guard Operations

```erlang
% Enable memory guard
erlmcp_memory_guard:enable().

% Set memory limit
erlmcp_memory_guard:set_limit(4294967296).  % 4GB

% Check memory usage
erlmcp_memory_guard:status().

% Force memory guard check
erlmcp_memory_guard:check().
```

### 9.5 Two-Phase Code Loading

```erlang
% Phase 1: Prepare modules
Modules = [erlmcp_server, erlmcp_client, erlmcp_session],
{ok, Prepared} = code:prepare_loading(Modules).

% Phase 2: Drain connections
erlmcp_graceful_drain:start(5000).

% Phase 3: Atomic load
ok = code:finish_loading(Prepared).

% Verify
[Module:module_info() || Module <- Modules].
```

---

## 10. Appendices

### Appendix A: Command Reference

#### Release Commands

```bash
./bin/erlmcp start          # Start daemon
./bin/erlmcp stop           # Stop daemon
./bin/erlmcp restart        # Restart daemon
./bin/erlmcp ping           # Ping node
./bin/erlmcp console        # Interactive console
./bin/erlmcp attach         # Attach to running
./bin/erlmcp reboot         # Reboot node
./bin/erlmcp versions       # Show versions
```

#### Monitoring Commands

```bash
# Health
curl http://localhost:8080/health
curl http://localhost:8080/health/transport

# Metrics
curl http://localhost:9090/metrics

# Logs
tail -f /opt/erlmcp/log/erlang.log.1
grep ERROR /opt/erlmcp/log/erlang.log.1
```

### Appendix B: Configuration Files

#### vm.args

```erlang
-name erlmcp@127.0.0.1
-setcookie erlmcp_cluster

+P 500000
+Q 65536
+S 8:8
+A 64
+K true
+MBacul ats
```

#### sys.config

```erlang
[
  {erlmcp, [
    {max_connections, 50000},
    {request_timeout, 5000},
    {log_level, info}
  ]},
  {erlmcp_transports, [
    {tcp_enabled, true},
    {http_enabled, true},
    {ws_enabled, true}
  ]}
].
```

### Appendix C: Contact Information

| Role | Name | Email | Phone |
|------|------|-------|-------|
| On-Call | _____________ | _______ | _________ |
| Team Lead | _____________ | _______ | _________ |
| Architect | _____________ | _______ | _________ |

### Appendix D: Related Documentation

- [Production Checklist](./PRODUCTION_CHECKLIST_OTP28.md)
- [Architecture](./architecture/README.md)
- [API Reference](./api-reference.md)
- [Troubleshooting Guide](./TROUBLESHOOTING.md)

---

**Document Version**: 1.0.0
**Last Updated**: 2026-02-02
**Next Review**: 2026-03-02
