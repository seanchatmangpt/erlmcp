# erlmcp Plan Comparison - Detailed Analysis

## Overview

This document provides detailed comparison of all three erlmcp pricing tiers. All metrics are auto-generated from plan specifications (`plans/team.plan.json`, `plans/enterprise.plan.json`, `plans/gov.plan.json`) to ensure consistency and accuracy.

## Pricing Model

All tiers use the same pricing model:
- **Model**: Flat per-deployment (no metering)
- **Overages**: None - included in your plan
- **Billing**: Annual or monthly agreements
- **Compliance**: No surprise charges

## Performance Envelope Comparison

### Throughput (Requests per Second)

| Tier | Throughput | Growth | Use Case |
|------|-----------|--------|----------|
| **Team** | 450 req/s | Baseline | POCs, hobby, low-scale |
| **Enterprise** | 1,500 req/s | 3.3x | Production, scaling |
| **Government** | 900 req/s | 2x | Mid-scale compliance |

**Analysis**: Enterprise tier provides highest throughput for maximum scale. Government tier balances compliance requirements with reasonable performance. Team tier sufficient for development and testing.

### Concurrent Connections

| Tier | Max Connections | Growth | Recommendation |
|------|-----------------|--------|-----------------|
| **Team** | 128 | Baseline | Single-region testing |
| **Enterprise** | 512 | 4x | Multi-region, high concurrency |
| **Government** | 256 | 2x | Compliance without extreme scale |

**Analysis**: Each tier scales connection handling appropriately. Team tier handles typical development workloads. Enterprise tier supports massive concurrent users. Government tier provides balanced connectivity for secure deployments.

### Latency (P99)

| Tier | P99 Latency | SLA Percentile | Predictability |
|------|-------------|---------------|--------------------|
| **Team** | 250ms | 99th percentile | Good |
| **Enterprise** | 100ms | 99th percentile | Excellent |
| **Government** | 150ms | 99th percentile | Very Good |

**Analysis**: Enterprise tier provides tightest latency bounds for responsive applications. Government tier sacrifices some latency for compliance overhead. Team tier acceptable for non-interactive workloads.

### Queue Depth

| Tier | Queue Messages | Strategy |
|------|----------------|----------|
| **Team** | 2,048 | Conservative buffering |
| **Enterprise** | 8,192 | Aggressive buffering for scale |
| **Government** | 4,096 | Balanced buffering |

**Analysis**: Queue depth determines backpressure behavior. Larger queues smooth traffic spikes. Team tier requires careful traffic management. Enterprise tier absorbs larger traffic bursts.

### Failover SLA

| Tier | Failover Time | Availability Target |
|------|---------------|---------------------|
| **Team** | 30 seconds | 99.0% |
| **Enterprise** | 10 seconds | 99.95% |
| **Government** | 15 seconds | 99.99% |

**Analysis**:
- **Team**: 30s failover acceptable for non-critical services
- **Enterprise**: 10s failover suitable for production applications
- **Government**: 15s failover with highest availability for compliance

### Connection Timeout

| Tier | Timeout | Max Message Size | Max Payload |
|------|---------|------------------|------------|
| **Team** | 60s | 1MB | 10MB |
| **Enterprise** | 120s | 10MB | 100MB |
| **Government** | 90s | 5MB | 50MB |

**Analysis**:
- Longer timeouts support large transfers (Enterprise)
- Shorter timeouts prevent resource exhaustion (Team)
- Government tier balances compliance and performance

## Feature Comparison

### Core Features

| Feature | Team | Enterprise | Government |
|---------|------|-----------|-----------|
| **Client Implementation** | Yes | Yes | Yes |
| **Server Implementation** | Yes | Yes | Yes |
| **Stdio Transport** | Yes | Yes | Yes |
| **TCP Transport** | Yes | Yes | Yes |
| **HTTP Transport** | Yes | Yes | Yes |
| **WebSocket Transport** | No | Yes | Yes |
| **SSE Transport** | No | Yes | Yes |

### Advanced Features

| Feature | Team | Enterprise | Government |
|---------|------|-----------|-----------|
| **Rate Limiting** | Yes | Yes | Yes |
| **Connection Pooling** | No | Yes | Yes |
| **Circuit Breaker** | Yes | Yes | Yes |
| **OTEL Observability** | Basic | Comprehensive | Comprehensive |
| **Audit Logging** | No | Yes | Yes |
| **FIPS 140-2** | No | No | Yes |
| **High Availability** | No | Yes | Yes |

### Enterprise Features (Enterprise+)

| Feature | Enterprise | Government |
|---------|-----------|-----------|
| **Multi-region Support** | No* | No* |
| **Load Balancing** | Yes | Yes |
| **Health Checks** | Yes | Yes |
| **Connection Monitoring** | Yes | Yes |

*Available in future releases

### Security Features (Government Only)

| Feature | Team | Enterprise | Government |
|---------|------|-----------|-----------|
| **FIPS 140-2 Compliance** | No | No | **Yes** |
| **Encrypted Transport** | TLS 1.2 | TLS 1.2+ | **TLS 1.3+** |
| **Key Rotation** | No | No | **Yes** |
| **Compliance Reporting** | No | No | **Yes** |
| **Audit Immutability** | No | No | **Yes** |
| **Log Signing** | No | No | **Yes** |

## Observability

### OTEL Integration

| Aspect | Team | Enterprise | Government |
|--------|------|-----------|-----------|
| **Metrics Export** | Basic | Comprehensive | Comprehensive |
| **Tracing** | Sampling | Full | Full + Compliance |
| **Logging** | Info level | Debug + Audit | Debug + Audit + Compliance |
| **Retention** | 7 days | 30 days | 7 years |

### Audit Logging (Enterprise+)

| Audit Type | Enterprise | Government |
|-----------|-----------|-----------|
| **Operation Logging** | Optional | **Required** |
| **Authentication Logging** | No | **Yes** |
| **Encryption Events** | No | **Yes** |
| **Access Violations** | No | **Yes** |
| **System Events** | No | **Yes** |
| **Log Immutability** | No | **Yes** |
| **Log Signing** | No | **Yes** |
| **Retention Period** | 30 days | **7 years** |

## Pricing and Support

### Pricing Structure

| Tier | Model | Typical Cost | Target Users |
|------|-------|-------------|--------------|
| **Team** | Open-source free | $0 | Developers, hobbyists |
| **Team** | Commercial | Custom | Startups |
| **Enterprise** | Enterprise license | Quoted | Mid-market/Enterprise |
| **Government** | Government license | Quoted | Federal/Government |

### Support Model

| Tier | Support | SLA | Channels |
|------|---------|-----|----------|
| **Team** | Community | Best effort | GitHub issues |
| **Enterprise** | Premium | 8-hour response | Email, Slack, phone |
| **Government** | Dedicated | 2-hour response | Phone, secure email |

## Resource Limits

### Message Handling

| Limit | Team | Enterprise | Government |
|-------|------|-----------|-----------|
| **Max Message Size** | 1MB | 10MB | 5MB |
| **Max Payload Size** | 10MB | 100MB | 50MB |
| **Max Requests/Conn** | 32 | 128 | 64 |
| **Backpressure Threshold** | 8MB | 64MB | 32MB |

### System Resources

| Resource | Team | Enterprise | Government |
|----------|------|-----------|-----------|
| **Memory Limit** | 512MB | 4GB | 2GB |
| **CPU Time Limit** | 300s | 600s | 450s |
| **Connection Timeout** | 60s | 120s | 90s |

## Refusal Behavior

When limits are exceeded, each tier responds appropriately:

### Rate Limit Exceeded

| Tier | Status | Error Code | Retry After |
|------|--------|-----------|------------|
| **Team** | 429 | rate_limit_exceeded | 60s |
| **Enterprise** | 429 | rate_limit_exceeded | 30s |
| **Government** | 429 | rate_limit_exceeded | 45s + audit log |

### Service Overloaded

| Tier | Status | Error Code | Retry After |
|------|--------|-----------|------------|
| **Team** | 503 | service_unavailable | 30s |
| **Enterprise** | 503 | service_unavailable | 10s |
| **Government** | 503 | service_unavailable | 15s + audit log |

## Evidence and Compliance

All tiers include comprehensive evidence documentation:

### Required Evidence (All Tiers)

- **SBOM** (Software Bill of Materials)
- **Provenance** (Supply chain documentation)
- **Chaos Report** (Failure testing results)
- **Benchmark Report** (Performance testing)

### Additional Evidence (Enterprise)

- **Audit Schema** (Audit logging specification)

### Additional Evidence (Government)

- **FIPS 140-2 Certification**
- **Compliance Report**
- **Audit Trail Explorer**
- **Key Rotation Documentation**

## Feature Progression Path

```
Team Tier
├── Throughput: 450 req/s
├── Connections: 128
├── Latency: 250ms P99
├── Features: Core transports, basic observability
└── No audit logging, no HA

       ↓ UPGRADE

Enterprise Tier
├── Throughput: 1,500 req/s (3.3x increase)
├── Connections: 512 (4x increase)
├── Latency: 100ms P99 (2.5x improvement)
├── Features: All transports, advanced observability
├── Audit logging: Optional
└── High availability: Yes

       ↓ UPGRADE

Government Tier
├── Throughput: 900 req/s (balanced)
├── Connections: 256 (2x from Team)
├── Latency: 150ms P99 (balanced)
├── Features: All transports + compliance
├── Audit logging: Required, 7-year retention
├── FIPS 140-2: Yes
└── High availability: Yes
```

## Choosing Your Tier

### Choose Team If:
- Building a proof-of-concept
- Development/testing environment
- Non-critical workloads
- Low traffic (< 450 req/s)
- No compliance requirements

### Choose Enterprise If:
- Production applications
- High traffic (> 450 req/s)
- Multi-region deployments
- Need high availability
- Advanced observability required

### Choose Government If:
- Federal/Government deployment
- FIPS 140-2 compliance required
- 7-year audit retention needed
- Encryption enforcement required
- Dedicated support needed

## Upgrade Path

Upgrading tiers is seamless:

1. **Same API**: No code changes required
2. **Drop-in replacement**: Enterprise/Gov binaries compatible with Team code
3. **Feature flags**: New features gated by capability negotiation
4. **Smooth migration**: Zero downtime upgrades supported
5. **Cost optimization**: Scale down if traffic decreases

## Performance Under Load

### Sustained Throughput

| Tier | 10 req/s | 100 req/s | Max req/s | Sustainable |
|------|----------|-----------|-----------|------------|
| **Team** | OK | OK | 450 | Yes |
| **Enterprise** | OK | OK | 1,500 | Yes |
| **Government** | OK | OK | 900 | Yes |

### Latency Under Load

| Load | Team | Enterprise | Government |
|------|------|-----------|-----------|
| **10% of max** | <50ms | <30ms | <40ms |
| **50% of max** | <100ms | <60ms | <80ms |
| **100% of max** | 250ms P99 | 100ms P99 | 150ms P99 |

## Conclusion

All three tiers provide production-ready implementations with transparent, flat-rate pricing:

- **Team**: Perfect for development and small-scale deployments
- **Enterprise**: Optimized for production scale and availability
- **Government**: Purpose-built for compliance-critical environments

Choose based on your throughput needs, compliance requirements, and feature requirements. Upgrade anytime without code changes.
