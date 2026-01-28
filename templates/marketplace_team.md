# erlmcp {{plan_name|capitalize}} Plan - {{plan_tier|capitalize}} Tier

Professional Model Context Protocol (MCP) server implementation optimized for team-scale deployments.

## Service Envelope Specification

### Request Capacity
- **Requests per Second (RPS)**: {{requests_per_second}}
- **Concurrent Connections**: {{concurrent_connections}}
- **Queue Depth**: {{queue_depth}} messages
- **Maximum Message Size**: {{message_size_bytes}} bytes (1 MB)

### What This Means
The Team plan supports small-to-medium teams with consistent, predictable workloads. Perfect for applications serving 50 concurrent users with moderate request rates.

## Service Level Agreements (SLA)

### Performance Guarantees
- **Latency (P99)**: {{latency_p99_ms}} ms - 99th percentile response time
- **Failover Time**: {{failover_seconds}} seconds - Automatic recovery from transient failures
- **Uptime Commitment**: {{uptime_percent}}% - Industry-standard availability guarantee
- **Incident Response**: {{incident_response_hours}} hour response time for critical issues

### What This Means
Your applications receive professional support with guaranteed response times and robust failover capabilities. We maintain industry-leading uptime standards with 24-hour incident response.

## Deterministic Refusal Behavior

When your application approaches or exceeds service envelope limits, you receive predictable, actionable error responses:

### At Service Capacity
**Behavior**: {{at_capacity}}

Clients receive a standard HTTP 429 response with a `Retry-After` header indicating when capacity will be available. This allows intelligent exponential backoff strategies.

### Rate Limit Exceeded
**Behavior**: {{rate_limit_exceeded}}

Applications exceeding request rates receive JSON-RPC error code -32000 with detailed rate limit information, enabling precise quota tracking and cost optimization.

### Message Too Large
**Behavior**: {{message_too_large}}

Messages exceeding the {{message_size_bytes}}-byte limit receive HTTP 413 Payload Too Large, signaling clients to split large requests into smaller chunks.

### Invalid Protocol
**Behavior**: {{invalid_protocol}}

Protocol violations receive HTTP 400 Bad Request with diagnostic information to help developers quickly identify and fix integration issues.

## Evidence Included in This Deployment

All Team tier deployments include comprehensive evidence of production readiness:

- **Software Bill of Materials (SBOM)**: {{sbom_included}} - Complete dependency audit included
- **Build Provenance**: {{provenance_included}} - Cryptographically signed build verification
- **Chaos Testing Matrix**: {{chaos_matrix_included}} - Documented failure scenario testing
- **Performance Benchmarks**: {{benchmark_results_included}} - Real-world performance metrics

## Pricing Model

**Fixed Rate**: ${{base_cost_usd}} {{deployment_unit}}

- One-time setup cost covers unlimited deployments
- No per-request metering or hidden fees
- No egress charges for data transfer
- Includes all evidence and compliance documentation

## Getting Started

1. **Deploy**: Extract the erlmcp release package to your infrastructure
2. **Configure**: Set envelope limits in `sys.config` (pre-configured for this plan)
3. **Monitor**: Use included observability tools to track performance against SLA
4. **Support**: Contact support@erlmcp.dev for assistance

## Technical Specifications

### Protocol Support
- MCP 2025-11-25 specification compliance (100%)
- JSON-RPC 2.0 message encoding
- HTTP/1.1, HTTP/2, WebSocket transports
- Unix socket and TCP communication

### Persistence & State
- Automatic session management
- Built-in circuit breaker protection
- Graceful degradation under overload
- Configurable retry policies

### Security
- TLS 1.3 for all connections
- Origin validation and CSRF protection
- Rate limiting per source IP
- Audit logging of all API calls

## Support & Documentation

- 24-hour email support ({{incident_response_hours}} hour response time)
- Full API documentation and examples
- Troubleshooting guide included
- Community Slack channel access

---

*Generated from deterministic plan specification on {{generation_date}}*
*All numerical limits and SLA terms are enforced by the deployment*
*For custom requirements or higher tiers, contact sales@erlmcp.dev*
