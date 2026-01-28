# Government Tier - FIPS-140-2, Audit Logging

Government-grade tier with FIPS-140-2 encryption, comprehensive audit logging, and compliance features

## Envelope Summary

Your tier provides the following guaranteed capacity:

| Metric | Value |
|--------|-------|
| Throughput | 900 req/s |
| Concurrent Connections | 256 |
| Queue Depth | 4096 messages |
| P99 Latency | 150 ms |
| Failover SLA | 15 seconds |

### What This Means

- Government and regulated environments
- FIPS-140-2 compliant encryption
- Comprehensive audit logging for compliance

## Typical Use Cases

- Federal Agencies: FIPS-140-2 compliant systems
- Regulated Finance: Compliance-sensitive financial systems
- Healthcare: HIPAA-compliant medical data systems
- Defense Contractors: Secure government communication
- Audit-Required Industries: Systems requiring immutable logs

## Refusal Behavior

When your deployment hits tier limits, you'll receive deterministic errors:

| Scenario | HTTP Status | Error Code | Message |
|----------|-------------|-----------|---------|
| throughput exceeded | 429 | rate_limit_exceeded | Request rate exceeds tier limit (900 req/s) |
| queue depth exceeded | 503 | service_unavailable | System overloaded, queue depth exceeded |
| connection limit exceeded | N/A | connection_limit | Maximum 256 concurrent connections exceeded |
| message size exceeded | 413 | payload_too_large | Message exceeds 5MB limit |
| fips compliance violation | 403 | fips_violation | Operation violates FIPS-140-2 compliance requirements |
| encryption failure | 500 | encryption_error | Encryption failure - operation rejected |

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
        {403, fips_violation} ->
            % Operation not allowed for compliance
            reject_operation();
        Error ->
            handle_error(Error)
    end.
```

## Hard Limits at Tier Boundary

These limits are enforced deterministically. Exceeding them triggers refusal:

| Limit | Value |
|-------|-------|
| Max Message Size | 5120 KB |
| Max Payload | 50 MB |
| Concurrent Requests/Conn | 64 |

### Boundary Behavior

**Testing boundary conditions:**

```bash
# Test throughput limit at boundary (last request succeeds, next fails)
rebar3 do compile, eunit
erlmcp bench run --suite throughput --plan gov --target 900 --run 10s

# Test message size limit
erlmcp bench run --suite message-size --plan gov

# Test concurrent connections
erlmcp bench run --suite connections --plan gov
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
| fips_140_2 | Yes |
| high_availability | Yes |

## Compliance Features

- FIPS-140-2 Encryption: AES-256-GCM
- Key Derivation: PBKDF2-SHA256
- TLS 1.3 Only: No downgrade attacks
- Key Rotation: Automatic key rotation support
- Compliance Reporting: Built-in audit trail generation
- Immutable Logs: Write-once audit logging
- Log Signing: Cryptographic signature on all audit events

## Evidence Bundle

Your tier includes comprehensive validation evidence:

- [SBOM (Software Bill of Materials)](../plans/gov-sbom.json)
- [Build Provenance](../plans/gov-provenance.json)
- [Chaos Engineering Report](docs/plans/gov-chaos-report.md)
- [Benchmark Report](docs/plans/gov-benchmark-report.md)
- [FIPS-140-2 Certification](plans/gov-fips-140-2-cert.txt)
- [Compliance Report](docs/plans/gov-compliance-report.md)

## Pricing Model

**Model**: flat-per-deployment

**Cost**: Government license pricing

**Description**: No metering, no surprise overages

### No Surprises

- No per-request metering
- No overage charges
- No hidden fees
- Flat rate per deployment
- Transparent SLA guarantees

## SLA Guarantees

- Availability: 99.99%
- Throughput: 900 req/s guaranteed
- Failover: 15 seconds
- Recovery: 10 minutes
- Audit Retention: 7 years

## Audit & Logging

All operations are logged for compliance:

- **All Operations**: Comprehensive audit trail
- **Authentication**: All auth attempts logged
- **Encryption Events**: Key generation, rotation, usage
- **Access Violations**: Failed attempts and policy violations
- **System Events**: Service startup, shutdown, errors
- **Retention**: 7 years of immutable audit logs
- **Log Signing**: Cryptographic signatures prevent tampering
- **Immutability**: Write-once, append-only audit trail

## CLI Commands for This Tier

Check your current tier and explore tier-specific operations:

### Check Current Plan

```bash
erlmcp plan status
```

**Output:**
```
Current Plan: gov
Throughput:   900 req/s
Connections:  256 concurrent
Latency SLA:  150 ms (p99)
```

### Display Tier Specification

```bash
erlmcp plan show gov
```

### Run Tier-Specific Benchmark

```bash
erlmcp bench run --suite throughput --plan gov
```

### Export Audit Trail

```bash
erlmcp receipt export gov json | jq .
```

### Verify Compliance

```bash
erlmcp compliance verify --tier gov
```

## Runnable Examples

These examples can be executed immediately and demonstrate tier behavior:

### Example 1: Check Plan Status

```erlang
erl> erlmcp_plan:current_plan().
{ok, #{tier => gov, throughput => 900}}
```

### Example 2: Display Full Tier Envelope

```erlang
erl> erlmcp_plan:show(gov).
#{envelope => #{throughput_req_s => 900, ...}}
```

### Example 3: Run Tier Benchmark

```bash
$ rebar3 do compile, eunit
$ erlmcp bench run --suite throughput --plan gov --target 900 --duration 30s
```

### Example 4: Test Refusal Behavior

```erlang
erl> erlmcp_plan:test_refusal(throughput_exceeded).
{429, rate_limit_exceeded, rate_limit_exceeded_message}
```

### Example 5: Export Audit Trail

```bash
$ erlmcp receipt export gov json | jq '.events | length'
```

### Example 6: Verify FIPS Compliance

```bash
$ erlmcp compliance verify --tier gov --report fips-140-2
```
