# Enterprise Tier - Production Applications

Production-grade tier for enterprise applications with high availability and performance requirements

## Envelope Summary

Your tier provides the following guaranteed capacity:

| Metric | Value |
|--------|-------|
| Throughput | 1500 req/s |
| Concurrent Connections | 512 |
| Queue Depth | 8192 messages |
| P99 Latency | 100 ms |
| Failover SLA | 10 seconds |

### What This Means

- Production-ready for enterprise applications
- Supports multiple concurrent user sessions
- Designed for business-critical workloads

## Typical Use Cases

- Web Applications: High-traffic web services
- Microservices: Distributed system coordination
- IoT Platforms: Real-time IoT device communication
- Gaming: Multiplayer game backends
- SaaS: Multi-tenant applications

## Refusal Behavior

When your deployment hits tier limits, you'll receive deterministic errors:

| Scenario | HTTP Status | Error Code | Message |
|----------|-------------|-----------|---------|
| throughput exceeded | 429 | rate_limit_exceeded | Request rate exceeds tier limit (1500 req/s) |
| queue depth exceeded | 503 | service_unavailable | System overloaded, queue depth exceeded |
| connection limit exceeded | N/A | connection_limit | Maximum 512 concurrent connections exceeded |
| message size exceeded | 413 | payload_too_large | Message exceeds 10MB limit |
| audit log error | 500 | audit_log_failure | Failed to write audit log - operation rejected |

### How to Handle Refusals

```erlang
handle_refusal(Response) ->
    case Response of
        {429, rate_limit_exceeded} ->
            RetryAfter = get_retry_after(Response),
            timer:sleep(RetryAfter * 1000),
            retry_request();
        {503, service_unavailable} ->
            backoff_retry(3);
        {413, payload_too_large} ->
            split_and_retry();
        Error ->
            handle_error(Error)
    end.
```

## Hard Limits at Tier Boundary

These limits are enforced deterministically. Exceeding them triggers refusal:

| Limit | Value |
|-------|-------|
| Max Message Size | 10240 KB |
| Max Payload | 100 MB |
| Concurrent Requests/Conn | 128 |

### Boundary Behavior

**Testing boundary conditions:**

```bash
# Test throughput limit at boundary (last request succeeds, next fails)
rebar3 do compile, eunit
erlmcp bench run --suite throughput --plan enterprise --target 1500 --run 10s

# Test message size limit
erlmcp bench run --suite message-size --plan enterprise

# Test concurrent connections
erlmcp bench run --suite connections --plan enterprise
```

## Supported Features

| Feature | Supported |
|---------|----------|
| client | Yes |
| server | Yes |
| stdio_transport | Yes |
| tcp_transport | Yes |
| http_transport | Yes |
| websocket_transport | Yes |
| sse_transport | Yes |
| rate_limiting | Yes |
| connection_pooling | Yes |
| circuit_breaker | Yes |
| otel_observability | comprehensive |
| audit_logging | Yes |
| fips_140_2 | No |
| high_availability | Yes |

## Evidence Bundle

Your tier includes comprehensive validation evidence:

- [SBOM (Software Bill of Materials)](../plans/enterprise-sbom.json)
- [Build Provenance](../plans/enterprise-provenance.json)
- [Chaos Engineering Report](docs/plans/enterprise-chaos-report.md)
- [Benchmark Report](docs/plans/enterprise-benchmark-report.md)

## Pricing Model

**Model**: flat-per-deployment

**Cost**: Enterprise license pricing

**Description**: No metering, no surprise overages

### No Surprises

- No per-request metering
- No overage charges
- No hidden fees
- Flat rate per deployment
- Transparent SLA guarantees

## SLA Guarantees

- Availability: 99.95%
- Throughput: 1500 req/s guaranteed
- Failover: 10 seconds
- Recovery: 15 minutes

## CLI Commands for This Tier

Check your current tier and explore tier-specific operations:

### Check Current Plan

```bash
erlmcp plan status
```

**Output:**
```
Current Plan: enterprise
Throughput:   1500 req/s
Connections:  512 concurrent
Latency SLA:  100 ms (p99)
```

### Display Tier Specification

```bash
erlmcp plan show enterprise
```

### Run Tier-Specific Benchmark

```bash
erlmcp bench run --suite throughput --plan enterprise
```

### Export Audit Trail

```bash
erlmcp receipt export enterprise json | jq .
```

### Upgrade to Another Tier

```bash
erlmcp upgrade plan enterprise gov
```

## Runnable Examples

These examples can be executed immediately and demonstrate tier behavior:

### Example 1: Check Plan Status

```erlang
erl> erlmcp_plan:current_plan().
{ok, #{tier => enterprise, throughput => 1500}}
```

### Example 2: Display Full Tier Envelope

```erlang
erl> erlmcp_plan:show(enterprise).
#{envelope => #{throughput_req_s => 1500, ...}}
```

### Example 3: Run Tier Benchmark

```bash
$ rebar3 do compile, eunit
$ erlmcp bench run --suite throughput --plan enterprise --target 1500 --duration 30s
```

### Example 4: Test Refusal Behavior

```erlang
erl> erlmcp_plan:test_refusal(throughput_exceeded).
{429, rate_limit_exceeded, rate_limit_exceeded_message}
```

### Example 5: Export Audit Trail

```bash
$ erlmcp receipt export enterprise json | jq '.events | length'
```
