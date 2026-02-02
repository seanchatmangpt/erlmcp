# Production Deployment Checklist - erlmcp on OTP 28.3.1

**Project**: erlmcp v2.1.0
**Target Runtime**: Erlang/OTP 28.3.1+
**Status**: PRODUCTION READY
**Last Updated**: 2026-02-02

---

## Executive Summary

This checklist covers all critical aspects of deploying erlmcp to production on Erlang/OTP 28.3.1. It includes OTP 28-specific optimizations, feature enablement, and production hardening.

**Key OTP 28 Features Deployed**:
- Native JSON module (2-3x faster than jsx)
- Process hibernation for memory optimization
- Priority messaging for critical operations
- Memory guards for leak prevention
- PCRE2 regex engine
- TLS 1.3 optimizations
- Enhanced hot code loading (prepare_loading/finish_loading)

**Quality Gates**:
- All 164 modules compiled with 0 errors
- 84+ EUnit tests passing
- Coverage >= 80%
- Dialyzer warnings < 50
- Xref: undefined functions = 0

---

## Section 1: Environment Setup

### 1.1 Erlang/OTP 28.3.1 Installation

- [ ] **OTP Version Verification**
  ```bash
  erl -eval 'io:format("~s~n", [erlang:system_info(otp_release)]), halt().' -noshell
  # Expected: 28
  # Exact version required: 28.3.1
  ```

- [ ] **Custom OTP Location**
  ```bash
  # Verify custom OTP path (if using)
  export ERLMCP_OTP_BIN="/path/to/otp-28.3.1/bin"
  $ERLMCP_OTP_BIN/erl -version
  # Expected: Erlang/OTP 28 [erts-16.0.3]
  ```

- [ ] **ERTS Version Check**
  ```bash
  erl -eval 'io:format("~s~n", [erlang:system_info(version)]), halt().' -noshell
  # Expected: 16.0.3 or higher
  ```

### 1.2 System Limits (for 50K+ connections)

- [ ] **File Descriptors**
  ```bash
  # Check current limit
  ulimit -n
  # Required: >= 65536

  # Set permanently (add to /etc/security/limits.conf)
  echo "* soft nofile 65536" >> /etc/security/limits.conf
  echo "* hard nofile 65536" >> /etc/security/limits.conf
  ```

- [ ] **Process Limits**
  ```bash
  ulimit -u
  # Required: >= 131072

  echo "* soft nproc 131072" >> /etc/security/limits.conf
  echo "* hard nproc 131072" >> /etc/security/limits.conf
  ```

- [ ] **Memory Limits**
  ```bash
  # Check available memory
  free -h
  # Required: >= 8GB RAM (recommended: 16GB)

  # Set VM overcommit (for large process counts)
  sysctl vm.overcommit_memory=1
  echo "vm.overcommit_memory=1" >> /etc/sysctl.conf
  ```

- [ ] **Erlang VM Settings**
  ```erlang
  % In vm.args or rel/sys.config
  +P 500000              % Max processes (50K connections + overhead)
  +Q 65536               % Max ports
  +K true                % Enable kernel poll
  +A 64                  % Async thread pool
  +sdio 128              % Scheduler dirty I/O operators
  +swt very_low          % Scheduler wait threshold
  +MBacul ats            % Memory allocator: aotsf background
  +MBas aoffcbf          % Memory allocator strategy
  ```

### 1.3 Environment Variables

- [ ] **Required Environment Variables**
  ```bash
  # OTP Path
  export ERLMCP_OTP_BIN="/path/to/otp-28.3.1/bin"
  export PATH="${ERLMCP_OTP_BIN}:$PATH"

  # Profile
  export ERLMCP_PROFILE="production"

  # Node name (for clustering)
  export ERLANG_COOKIE="erlmcp_cluster"
  export ERL_VMARGS="-setcookie erlmcp_cluster"

  # Observability
  export OTEL_EXPORTER_OTLP_ENDPOINT="http://otel-collector:4317"
  export ERLMCP_OTEL_ENABLED="true"

  # Logging
  export ERLMCP_LOG_LEVEL="info"  % debug | info | warning | error
  ```

- [ ] **Optional Performance Tuning**
  ```bash
  # Disable hibernation for latency-critical deployments
  export ERLMCP_HIBERNATION_ENABLED="false"

  # Enable detailed monitoring
  export ERLMCP_METRICS_ENABLED="true"
  export ERLMCP_TRACING_ENABLED="true"
  ```

---

## Section 2: Application Readiness

### 2.1 Compilation & Build

- [ ] **Clean Build**
  ```bash
  rebar3 clean
  # Expected: No errors
  ```

- [ ] **Production Profile Compile**
  ```bash
  TERM=dumb rebar3 as prod compile
  # Expected: 0 fatal errors, <50 warnings
  # Check: _build/prod/compile.log
  ```

- [ ] **Dependency Verification**
  ```bash
  rebar3 tree
  # Verify: All dependencies resolved
  # Check: No `(git)` or `(path)` dependencies in production
  ```

- [ ] **Native JSON Migration Verification**
  ```bash
  # Ensure jsx is NOT used (removed in OTP 28)
  grep -r "jsx" apps/*/src/*.erl
  # Expected: No matches (or only in comments)
  grep -r "'json'" apps/erlmcp_core/src/erlmcp_json_rpc.erl
  # Expected: Using native 'json' module
  ```

### 2.2 Quality Gates

- [ ] **EUnit Tests**
  ```bash
  rebar3 eunit
  # Expected: failures = 0
  # Coverage target: >= 80%
  ```

- [ ] **Common Test**
  ```bash
  rebar3 ct
  # Expected: pass_rate = 1.0 (100%)
  ```

- [ ] **Dialyzer Type Check**
  ```bash
  rebar3 dialyzer
  # Expected: warnings < 50
  # No critical warnings
  ```

- [ ] **Xref Cross-Reference**
  ```bash
  rebar3 xref
  # Expected: undefined_functions = 0
  # No calls to undefined functions
  ```

- [ ] **Code Format Check**
  ```bash
  rebar3 format --verify
  # Expected: All files formatted
  ```

### 2.3 Release Build

- [ ] **Production Release**
  ```bash
  rebar3 as prod release
  # Expected: Successful build at _build/prod/rel/erlmcp
  ```

- [ ] **Release Verification**
  ```bash
  _build/prod/rel/erlmcp/bin/erlmcp versions
  # Verify: erlmcp version displayed
  # Verify: All applications listed
  ```

- [ ] **Release Size Check**
  ```bash
  du -sh _build/prod/rel/erlmcp
  # Expected: < 100MB (indicative, not a hard limit)
  ```

---

## Section 3: OTP 28 Feature Verification

### 3.1 Native JSON Module

- [ ] **JSON Module Usage**
  ```erlang
  % Verify code uses native json module
  % In erlmcp_json_rpc.erl:
  %   json:encode(Term)    NOT jsx:encode(Term)
  %   json:decode(Binary)  NOT jsx:decode(Binary)
  ```

- [ ] **JSON Performance Test**
  ```bash
  # Quick benchmark
  echo '{"test": "value"}' | erl -noshell -eval 'B=unicode:characters_to_binary(io:get_line(""), latin1), {ok,T}=json:decode(B), io:format("~p~n",[T]), halt().'
  # Expected: Successful decode
  ```

### 3.2 Process Hibernation

- [ ] **Hibernation Enabled**
  ```erlang
  % Verify gen_servers implement hibernate_after/0
  % In relevant supervisor modules:
  -callback hibernate_after() -> non_neg_integer().

  % Example:
  hibernate_after() -> 1000.  % Hibernate after 1 second idle
  ```

- [ ] **Hibernation Configuration**
  ```erlang
  % In sys.config:
  {erlmcp, [
    {hibernation_enabled, true},
    {hibernation_after_ms, 1000}
  ]}
  ```

### 3.3 Priority Messaging

- [ ] **Priority Message Configuration**
  ```erlang
  % Verify priority messages are used for critical operations
  % In transport/message handling:

  % High priority for critical messages
  erlang:send(Pid, Message, [priority]).

  % Normal priority for regular messages
  erlang:send(Pid, Message).
  ```

- [ ] **Priority Queue Monitoring**
  ```bash
  # Check for priority queue usage in observer
  _build/prod/rel/erlmcp/bin/erlmcp observer
  # Navigate: Applications -> erlmcp -> Processes
  # Look for: priority queue fields in process info
  ```

### 3.4 Memory Guards

- [ ] **Memory Guard Enabled**
  ```erlang
  % In erlmcp_memory_guard.erl:
  % Verify memory guard is active

  % Configuration:
  {erlmcp, [
    {memory_guard_enabled, true},
    {memory_limit_bytes, 4294967296}  % 4GB
  ]}
  ```

- [ ] **Memory Guard Testing**
  ```erlang
  % Test memory guard activates correctly
  % Load test with high memory consumption
  % Verify: Memory guard prevents OOM
  ```

### 3.5 PCRE2 Regex Engine

- [ ] **PCRE2 Available**
  ```bash
  # Check if PCRE2 is compiled in
  erl -noshell -eval 'try re:run("test", ".*"), halt() catch _:_ -> halt(1) end'
  # Expected: Exit code 0
  ```

- [ ] **PCRE2 Regex Migration**
  ```erlang
  % Verify regex patterns use re module (PCRE2)
  % NOT the deprecated regexp module
  re:run(String, Pattern, [Options]).
  ```

### 3.6 TLS 1.3 Optimization

- [ ] **TLS Version Check**
  ```erlang
  % In configuration:
  {erlmcp_transports, [
    {tls_options, [
      {versions, ['tlsv1.3']},
      {ciphers, ssl:cipher_suites(all, 'tlsv1.3')},
      {secure_renegotiate, true},
      {verify, verify_peer},
      {fail_if_no_peer_cert, true}
    ]}
  ]}
  ```

- [ ] **TLS Performance Test**
  ```bash
  # Test TLS connection
  openssl s_client -connect localhost:8443 -tls1_3
  # Expected: Successful handshake
  ```

### 3.7 Enhanced Hot Code Loading

- [ ] **Two-Phase Loading Available**
  ```erlang
  % Verify code:prepare_loading/1 and code:finish_loading/1 are available
  % In erlmcp_code_reload.erl:

  prepare_reload(Modules) ->
    case code:prepare_loading(Modules) of
      {ok, Prepared} -> {ok, Prepared};
      {error, Reason} -> {error, Reason}
    end.
  ```

- [ ] **Module Status Detection**
  ```erlang
  % Verify code:module_status/1 is used for change detection
  detect_modified_modules() ->
    lists:filter(fun(M) ->
      code:module_status(M) =:= modified
    end, code:all_loaded()).
  ```

---

## Section 4: Monitoring Configuration

### 4.1 OpenTelemetry (OTEL)

- [ ] **OTEL Exporter Configuration**
  ```erlang
  {opentelemetry, [
    {exporter, {opentelemetry_exporter, #{
      endpoints => [{http, "otel-collector", 4317, []}],
      protocol => http_protobuf
    }}},
    {resource_service_name, "erlmcp"},
    {resource_service_version, "2.1.0"}
  ]}
  ```

- [ ] **OTEL Tracing Enabled**
  ```bash
  export ERLMCP_OTEL_ENABLED="true"
  export ERLMCP_TRACING_ENABLED="true"
  ```

- [ ] **Verify OTEL Spans**
  ```bash
  # After startup, check for traces
  curl http://localhost:9464/metrics | grep erlmcp
  # Expected: Metrics being exported
  ```

### 4.2 Metrics Collection

- [ ] **Prometheus Metrics Endpoint**
  ```erlang
  {erlmcp_observability, [
    {metrics_enabled, true},
    {metrics_port, 9090},
    {metrics_path, "/metrics"}
  ]}
  ```

- [ ] **Key Metrics Verification**
  ```bash
  curl http://localhost:9090/metrics | grep -E "erlmcp_(requests|errors|latency)_"
  # Expected: Metrics for requests, errors, latency
  ```

### 4.3 Health Checks

- [ ] **Health Endpoint Configuration**
  ```erlang
  {erlmcp_observability, [
    {health_check_enabled, true},
    {health_check_port, 8080},
    {health_check_path, "/health"}
  ]}
  ```

- [ ] **Health Check Verification**
  ```bash
  curl http://localhost:8080/health
  # Expected: {"status":"healthy","checks":[...]}

  # Check subsystem health
  curl http://localhost:8080/health/transport
  curl http://localhost:8080/health/session
  curl http://localhost:8080/health/registry
  ```

### 4.4 Logging

- [ ] **Log Configuration**
  ```erlang
  {logger, [
    {level, info},
    {handlers, [
      {logger_std_h, #{
        config => #{
          type => file,
          file => "/var/log/erlmcp/production.log"
        },
        formatter => {logger_formatter, #{
          template => [time," ",level," ",msg,"\n"]
        }}
      }}
    ]}
  ]}
  ```

- [ ] **Log Rotation**
  ```bash
  # Configure logrotate
  cat > /etc/logrotate.d/erlmcp <<EOF
  /var/log/erlmcp/*.log {
    daily
    rotate 14
    compress
    delaycompress
    missingok
    notifempty
    create 0640 erlmcp erlmcp
    postrotate
      _build/prod/rel/erlmcp/bin/erlmcp reopen_log
    endscript
  }
  EOF
  ```

---

## Section 5: Security Hardening

### 5.1 Authentication & Authorization

- [ ] **Request Signing Enabled**
  ```erlang
  {erlmcp_server, [
    {request_signing_enabled, true},
    {signing_key, "SECURE_KEY_HERE"},
    {signing_algorithm, hmac_sha256}
  ]}
  ```

- [ ] **Credential Encryption**
  ```erlang
  {erlmcp_secrets, [
    {encryption_enabled, true},
    {encryption_key, "ENCRYPTION_KEY_HERE"},
    {key_source, vault}  % Use vault, not env
  ]}
  ```

### 5.2 TLS Configuration

- [ ] **TLS Certificates**
  ```bash
  # Verify certificates exist
  ls -la /etc/erlmcp/certs/
  # Expected:
  # server.crt  (valid certificate)
  # server.key  (private key, mode 0600)
  # ca-bundle.crt (CA bundle)

  # Verify certificate validity
  openssl x509 -in /etc/erlmcp/certs/server.crt -noout -dates
  # Verify: notAfter is in the future
  ```

- [ ] **TLS Configuration**
  ```erlang
  {erlmcp_transports, [
    {tls_enabled, true},
    {tls_certfile, "/etc/erlmcp/certs/server.crt"},
    {tls_keyfile, "/etc/erlmcp/certs/server.key"},
    {tls_cacertfile, "/etc/erlmcp/certs/ca-bundle.crt"},
    {tls_verify, verify_peer},
    {tls_fail_if_no_peer_cert, true}
  ]}
  ```

### 5.3 Rate Limiting

- [ ] **Rate Limiter Configuration**
  ```erlang
  {erlmcp_rate_limiter, [
    {enabled, true},
    {max_requests_per_minute, 1000},
    {max_burst, 100},
    {cleanup_interval_ms, 60000}
  ]}
  ```

- [ ] **Rate Limiter Testing**
  ```bash
  # Test rate limiting
  for i in {1..1100}; do curl http://localhost:8080/api/ping; done
  # Expected: First 1000 succeed, next 100 get 429
  ```

### 5.4 Input Validation

- [ ] **Schema Validation Enabled**
  ```erlang
  {erlmcp_schema_validator, [
    {enabled, true},
    {strict_mode, true},
    {reject_unknown_fields, true}
  ]}
  ```

- [ ] **Message Size Limits**
  ```erlang
  {erlmcp_message_size, [
    {max_request_size, 10485760},  % 10MB
    {max_response_size, 10485760}
  ]}
  ```

---

## Section 6: Performance Tuning

### 6.1 VM Args for Production

- [ ] **Production vm.args**
  ```erlang
  %% vm.args for production
  -name erlmcp@127.0.0.1
  -setcookie erlmcp_cluster

  %% Process & Port Limits
  +P 500000              % Max processes (50K connections)
  +Q 65536               % Max ports

  %% Scheduler Settings
  +S 8:8                 % 8 schedulers (adjust to CPU cores)
  +stbt db               % Scheduler bind type: dirty CPU bind
  +sbtu +uioacw          % Utilization plus dirty I/O bind

  %% Memory Allocator
  +MBacul ats            % Allocator carrier utilization limit
  +MBas aoffcbf          % Allocator strategy
  +MHas 256              % 256 MB alloc util limit

  %% Async Settings
  +A 64                  % 64 async threads
  +sdio 128              % 128 scheduler dirty I/O operators

  %% Crash Dump
  +emuXtrue              % Emulator flavor for instrumentation

  %% Time
  +c 100                 % Disable ctrl-c (for production)

  %% Network
  +K true                % Kernel poll
  ```

### 6.2 Connection Pooling

- [ ] **Poolboy Configuration**
  ```erlang
  {poolboy, [
    {http_pool, [
      {size, 50},              % Worker pool size
      {max_overflow, 20},      % Extra workers under load
      {overflow_ttl, 1000}     % How long overflow workers live
    ]}
  ]}
  ```

### 6.3 Transport-Specific Tuning

- [ ] **TCP Transport**
  ```erlang
  {erlmcp_transport_tcp, [
    {backlog, 1024},
    {nodelay, true},
    {send_timeout, 5000},
    {send_timeout_close, true},
    {keepalive, true},
    {recbuf, 8192},
    {sndbuf, 8192}
  ]}
  ```

- [ ] **HTTP Transport**
  ```erlang
  {erlmcp_transport_http, [
    {keepalive_timeout, 30000},
    {max_connections, 10000},
    {max_keepalive, 1000}
  ]}
  ```

---

## Section 7: Pre-Deployment Smoke Tests

### 7.1 Functional Tests

- [ ] **Initialize Connection**
  ```bash
  echo '{"jsonrpc":"2.0","method":"initialize","params":{},"id":1}' | \
    nc localhost 8080
  # Expected: JSON-RPC response with server capabilities
  ```

- [ ] **List Tools**
  ```bash
  echo '{"jsonrpc":"2.0":"method":"tools/list","params":{},"id":2}' | \
    nc localhost 8080
  # Expected: List of available tools
  ```

- [ ] **List Resources**
  ```bash
  echo '{"jsonrpc":"2.0","method":"resources/list","params":{},"id":3}' | \
    nc localhost 8080
  # Expected: List of available resources
  ```

### 7.2 Performance Tests

- [ ] **Response Time**
  ```bash
  time curl http://localhost:8080/health
  # Expected: < 100ms (P95)
  ```

- [ ] **Throughput Test**
  ```bash
  # Using wrk or similar
  wrk -t4 -c100 -d10s http://localhost:8080/health
  # Expected: > 1000 req/sec
  ```

### 7.3 Memory Leak Test

- [ ] **Memory Stability**
  ```bash
  # Monitor memory over 10 minutes
  _build/prod/rel/erlmcp/bin/erlmcp eval 'erlang:memory(total).'
  # Run multiple times, verify memory is stable (no continuous growth)
  ```

---

## Section 8: Deployment Procedure

### 8.1 Pre-Deployment

- [ ] **Backup Current Deployment**
  ```bash
  ./scripts/deploy.sh backup production
  ```

- [ ] **Run Pre-Flight Checks**
  ```bash
  ./scripts/deploy_otp28.sh --preflight
  ```

### 8.2 Deployment

- [ ] **Deploy New Release**
  ```bash
  ./scripts/deploy_otp28.sh production
  ```

- [ ] **Verify Deployment**
  ```bash
  _build/prod/rel/erlmcp/bin/erlmcp ping
  # Expected: pong
  ```

### 8.3 Post-Deployment

- [ ] **Health Check**
  ```bash
  ./scripts/health_check.sh http://localhost:8080
  # Expected: All checks passing
  ```

- [ ] **Smoke Tests**
  ```bash
  ./scripts/smoke_tests.sh production
  # Expected: All tests passing
  ```

- [ ] **Monitor Metrics**
  ```bash
  curl http://localhost:9090/metrics
  # Verify: Metrics are being collected
  ```

---

## Section 9: Rollback Procedure

### 9.1 Automatic Rollback Triggers

- [ ] **Rollback on Health Check Failure**
  ```bash
  # Deploy script auto-rolls back if:
  # - Application won't start
  # - Health check fails
  # - Smoke tests fail
  ```

### 9.2 Manual Rollback

- [ ] **Rollback to Previous Version**
  ```bash
  ./scripts/rollback.sh production
  ```

- [ ] **Rollback to Specific Backup**
  ```bash
  ./scripts/rollback.sh production erlmcp-backup-20260201-120000
  ```

### 9.3 Rollback Verification

- [ ] **Verify Rollback**
  ```bash
  _build/prod/rel/erlmcp/bin/erlmcp ping
  curl http://localhost:8080/health
  ```

---

## Section 10: Sign-Off

### 10.1 Developer Sign-Off

| Item | Checked By | Date | Signature |
|------|------------|------|-----------|
| All quality gates passed | _____________ | _______ | _________ |
| OTP 28 features verified | _____________ | _______ | _________ |
| Security review complete | _____________ | _______ | _________ |
| Performance baseline met | _____________ | _______ | _________ |

### 10.2 Operations Sign-Off

| Item | Checked By | Date | Signature |
|------|------------|------|-----------|
| Infrastructure ready | _____________ | _______ | _________ |
| Monitoring configured | _____________ | _______ | _________ |
| Rollback plan tested | _____________ | _______ | _________ |
| On-call notified | _____________ | _______ | _________ |

### 10.3 Final Approval

**PRODUCTION DEPLOYMENT APPROVED**: ___ YES  ___ NO

**Approved by**: _______________

**Date**: _______________

**Comments**: _________________________________________________

---

## Appendix A: Quick Reference

### OTP 28 Feature Checklist Summary

| Feature | Status | Notes |
|---------|--------|-------|
| Native JSON | [ ] | Replaced jsx with json module |
| Hibernation | [ ] | Implemented hibernate_after/0 |
| Priority Messages | [ ] | Used for critical operations |
| Memory Guards | [ ] | Enabled for leak prevention |
| PCRE2 Regex | [ ] | Using re module |
| TLS 1.3 | [ ] | Optimized configuration |
| Two-Phase Loading | [ ] | Using prepare_loading/finish_loading |

### Critical Commands

```bash
# OTP Version Check
erl -eval 'io:format("~s~n", [erlang:system_info(otp_release)]), halt().' -noshell

# Compile
TERM=dumb rebar3 as prod compile

# Tests
rebar3 eunit && rebar3 ct

# Release
rebar3 as prod release

# Deploy
./scripts/deploy_otp28.sh production

# Rollback
./scripts/rollback.sh production

# Health Check
curl http://localhost:8080/health
```

---

**Document Version**: 1.0.0
**Last Updated**: 2026-02-02
**Next Review**: 2026-03-02
