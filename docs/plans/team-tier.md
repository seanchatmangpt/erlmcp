# Team Tier - Startups, POCs, Low-Scale

Perfect for startups, proof-of-concepts, hobby projects, and low-scale deployments

## Envelope Summary

Your tier provides the following guaranteed capacity:

| Metric | Value |
|--------|-------|
| Throughput | 450 req/s |
| Concurrent Connections | 128 |
| Queue Depth | 2048 messages |
| P99 Latency | 250 ms |
| Failover SLA | 30 seconds |

### What This Means

- Perfect for hobby projects, proof-of-concepts, and startup MVPs
- Suitable for development and testing environments
- Ideal for learning the MCP protocol

## Typical Use Cases

- Hobby Projects: Erlang learning projects with MCP integration
- Proof-of-Concepts: Quick prototypes for new ideas
- Development: Local development environments
- Testing: Integration testing with modest load
- Open Source: Community projects and research

## Refusal Behavior

When your deployment hits tier limits, you'll receive deterministic errors:

| Scenario | HTTP Status | Error Code | Message |
|----------|-------------|-----------|---------|
| throughput exceeded | 429 | rate_limit_exceeded | Request rate exceeds tier limit (450 req/s) |
| queue depth exceeded | 503 | service_unavailable | System overloaded, queue depth exceeded |
| connection limit exceeded | N/A | connection_limit | Maximum 128 concurrent connections exceeded |
| message size exceeded | 413 | payload_too_large | Message exceeds 1MB limit |

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
| Max Message Size | 1024 KB |
| Max Payload | 10 MB |
| Concurrent Requests/Conn | 32 |

### Boundary Behavior

**Testing boundary conditions:**

```bash
# Test throughput limit at boundary (last request succeeds, next fails)
rebar3 do compile, eunit
erlmcp bench run --suite throughput --plan team --target 450 --run 10s

# Test message size limit
erlmcp bench run --suite message-size --plan team

# Test concurrent connections
erlmcp bench run --suite connections --plan team
```

## Supported Features

| Feature | Supported |
|---------|----------|
| client | Yes |
| server | Yes |
| stdio_transport | Yes |
| tcp_transport | Yes |
| http_transport | Yes |
| websocket_transport | No |
| sse_transport | No |
| rate_limiting | Yes |
| connection_pooling | No |
| circuit_breaker | Yes |
| otel_observability | basic |
| audit_logging | No |
| fips_140_2 | No |
| high_availability | No |

## Evidence Bundle

Your tier includes comprehensive validation evidence:

- [SBOM (Software Bill of Materials)](../plans/team-sbom.json)
- [Build Provenance](../plans/team-provenance.json)
- [Chaos Engineering Report](docs/plans/team-chaos-report.md)
- [Benchmark Report](docs/plans/team-benchmark-report.md)

## Pricing Model

**Model**: flat-per-deployment

**Cost**: Free for open-source, custom pricing for commercial

**Description**: No metering, no surprise overages

### No Surprises

- No per-request metering
- No overage charges
- No hidden fees
- Flat rate per deployment
- Transparent SLA guarantees

## CLI Commands for This Tier

Check your current tier and explore tier-specific operations:

### Check Current Plan

```bash
erlmcp plan status
```

**Output:**
```
Current Plan: team
Throughput:   450 req/s
Connections:  128 concurrent
Latency SLA:  250 ms (p99)
```

### Display Tier Specification

```bash
erlmcp plan show team
```

### Run Tier-Specific Benchmark

```bash
erlmcp bench run --suite throughput --plan team
```

### Export Audit Trail

```bash
erlmcp receipt export team json | jq .
```

### Upgrade to Another Tier

```bash
erlmcp upgrade plan team enterprise
```

## Runnable Examples

These examples can be executed immediately and demonstrate tier behavior:

### Example 1: Check Plan Status

```erlang
erl> erlmcp_plan:current_plan().
{ok, #{tier => team, throughput => 450}}
```

### Example 2: Display Full Tier Envelope

```erlang
erl> erlmcp_plan:show(team).
#{envelope => #{throughput_req_s => 450, ...}}
```

### Example 3: Run Tier Benchmark

```bash
$ rebar3 do compile, eunit
$ erlmcp bench run --suite throughput --plan team --target 450 --duration 30s
```

### Example 4: Test Refusal Behavior

```erlang
erl> erlmcp_plan:test_refusal(throughput_exceeded).
{429, rate_limit_exceeded, rate_limit_exceeded_message}
```

### Example 5: Export Audit Trail

```bash
$ erlmcp receipt export team json | jq '.events | length'
```
