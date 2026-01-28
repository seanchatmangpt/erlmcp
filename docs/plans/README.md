# ErlMCP Pricing Plan Tiers

Welcome to the ErlMCP pricing tier documentation. Choose your tier based on your deployment needs:

## Quick Tier Comparison

| Feature | Team | Enterprise | Government |
|---------|------|------------|-----------|
| **Throughput** | 450 req/s | 1500 req/s | 900 req/s |
| **Connections** | 128 | 512 | 256 |
| **P99 Latency** | 250ms | 100ms | 150ms |
| **SLA** | None | 99.95% | 99.99% |
| **Audit Logging** | No | Yes | Yes (7 years) |
| **FIPS-140-2** | No | No | Yes |
| **Cost Model** | Free (OSS) | Enterprise | Government |

## Tier Guides

### Team Tier - Startups, POCs, Development

Best for hobby projects, proof-of-concepts, development environments, and open-source projects.

- 450 requests per second
- 128 concurrent connections
- 250ms p99 latency SLA
- Basic features (stdio, TCP, HTTP)
- No audit logging
- Perfect for: Learning, prototyping, startup MVPs

[Read Team Tier Guide →](team-tier.md)

**Quick Start:**
```bash
erlmcp plan show team
erlmcp bench run --suite throughput --plan team
```

---

### Enterprise Tier - Production Applications

Best for production web services, SaaS, microservices, gaming, and IoT platforms.

- 1500 requests per second (3.3x Team)
- 512 concurrent connections (4x Team)
- 100ms p99 latency SLA (2.5x faster)
- All transport protocols (WebSocket, SSE)
- Connection pooling & load balancing
- Comprehensive audit logging
- 99.95% availability SLA
- Perfect for: Production apps, high-traffic services, multi-tenant systems

[Read Enterprise Tier Guide →](enterprise-tier.md)

**Quick Start:**
```bash
erlmcp plan show enterprise
erlmcp bench run --suite throughput --plan enterprise
```

---

### Government Tier - Regulated & Compliance-Critical

Best for federal agencies, healthcare, regulated finance, and defense contracting.

- 900 requests per second
- 256 concurrent connections
- 150ms p99 latency
- FIPS-140-2 compliant encryption (AES-256-GCM)
- Immutable audit logs (7-year retention)
- Cryptographically signed audit trail
- TLS 1.3 only (no downgrade attacks)
- 99.99% availability SLA
- Perfect for: Government systems, healthcare (HIPAA), finance (PCI-DSS), defense

[Read Government Tier Guide →](gov-tier.md)

**Quick Start:**
```bash
erlmcp plan show gov
erlmcp compliance verify --tier gov
erlmcp receipt export gov json
```

---

## How to Choose Your Tier

### Choose Team Tier if you:
- Are building a prototype or proof-of-concept
- Run a hobby project or research application
- Need a free solution for open-source development
- Want to learn MCP protocol without production constraints
- Have < 50 concurrent users

### Choose Enterprise Tier if you:
- Run production services with thousands of users
- Need high availability (99.95% uptime)
- Require comprehensive audit logging
- Use WebSocket or SSE for real-time features
- Have 50-500 concurrent users
- Value connection pooling and load balancing

### Choose Government Tier if you:
- Operate in a regulated industry (healthcare, finance, defense)
- Must comply with FIPS-140-2 encryption standards
- Need immutable, auditable logs for compliance
- Require 7-year audit retention for regulatory oversight
- Operate under government contracts
- Have 50-300 concurrent users with compliance requirements

---

## Pricing

All tiers use **flat-per-deployment pricing**:

- No per-request metering
- No surprise overages
- No usage-based charges
- Transparent SLA guarantees

**Cost Structure:**
- **Team**: Free for open-source, custom pricing for commercial use
- **Enterprise**: Enterprise license pricing
- **Government**: Government license pricing

---

## Deterministic Behavior

Each tier enforces limits deterministically:

```erlang
%% Team tier allows 450 req/s
%% Request #449 succeeds
%% Request #451 returns:
{error, 429, "rate_limit_exceeded", "Request rate exceeds tier limit (450 req/s)"}

%% Message sizes
%% Team: 1MB max
%% Enterprise: 10MB max
%% Gov: 5MB max

%% All refusals are deterministic and predictable
%% No random failures or capacity headroom
```

---

## Evidence Bundles

Each tier includes production-grade evidence:

- **SBOM** - Software Bill of Materials
- **Build Provenance** - Supply-chain transparency
- **Chaos Report** - Failure mode testing
- **Benchmark Report** - Performance profiling
- **FIPS Cert** (Gov only) - Compliance certification
- **Audit Schema** (Gov only) - Compliance alignment

---

## CLI Commands

### Common Commands

```bash
# Check current tier
erlmcp plan status

# List all available tiers
erlmcp plan list

# Show tier details
erlmcp plan show team

# Run tier-specific benchmark
erlmcp bench run --suite throughput --plan team --target 450

# Test refusal behavior
erlmcp plan test-refusal throughput_exceeded

# Export audit trail
erlmcp receipt export team json

# Upgrade between tiers
erlmcp upgrade plan team enterprise

# Verify compliance (Gov tier)
erlmcp compliance verify --tier gov
```

---

## Refusal Codes

When you hit a tier limit, you'll receive:

| Code | HTTP | Meaning | Retry? |
|------|------|---------|--------|
| `rate_limit_exceeded` | 429 | Throughput limit reached | After delay |
| `service_unavailable` | 503 | Queue depth exceeded | After backoff |
| `payload_too_large` | 413 | Message size exceeded | With smaller payload |
| `connection_limit` | 1008 | Connection limit reached | New connection needed |
| `audit_log_failure` | 500 | Audit logging failed (Enterprise/Gov) | No retry (fix logs) |
| `fips_violation` | 403 | FIPS compliance violation (Gov) | No retry (operation not allowed) |
| `encryption_error` | 500 | Encryption failed (Gov) | No retry (check keys) |

---

## SLA Guarantees

### Team Tier
- No formal SLA
- Best-effort availability
- Suitable for non-critical applications

### Enterprise Tier
- **Availability**: 99.95% (4 nines)
- **Failover**: 10 seconds
- **Recovery**: 15 minutes SLA
- Monthly uptime: 99.95% = ~22 minutes downtime/month

### Government Tier
- **Availability**: 99.99% (4 nines 9)
- **Failover**: 15 seconds
- **Recovery**: 10 minutes SLA
- Monthly uptime: 99.99% = ~2.2 minutes downtime/month
- Audit retention: 7 years

---

## Integration

### For Marketing/Pricing Page
- Use tier guides for feature descriptions
- Reference SLA guarantees for Enterprise/Gov tiers
- Highlight compliance for Gov tier

### For Documentation
- All tier guides render in mkdocs
- Deterministic refusal behavior documented
- CLI examples are production-ready

### For Developers
- Use `erlmcp_plan:show(team)` for tier data
- Test with `erlmcp_plan:check_throughput_limit/2`
- Handle refusals with specific error codes

### For Operations
- Monitor against SLA with tier limits
- Alert on approaching limits
- Track tier-specific metrics

---

## Need Help?

- **Team Tier**: See [Team Tier Guide](team-tier.md)
- **Enterprise Tier**: See [Enterprise Tier Guide](enterprise-tier.md)
- **Government Tier**: See [Government Tier Guide](gov-tier.md)
- **System Overview**: See [Pricing Plans README](../PRICING_PLANS_README.md)

---

## API Reference

### Get Current Tier
```erlang
{ok, Tier} = erlmcp_plan:current_plan().
```

### Show Tier Specification
```erlang
{ok, Spec} = erlmcp_plan:show(team).
```

### Check Limits
```erlang
ok = erlmcp_plan:check_throughput_limit(team, 400).
ok = erlmcp_plan:check_connection_limit(team, 100).
ok = erlmcp_plan:check_message_size(team, 500000).
```

### List Available Tiers
```erlang
{ok, [team, enterprise, gov]} = erlmcp_plan:list_plans().
```

---

## Next Steps

1. **Choose your tier** based on your use case
2. **Read the detailed guide** for your tier
3. **Review the runnable examples** in the guide
4. **Test with CLI commands** to validate behavior
5. **Integrate into your application** using the Erlang API

Ready to get started? Choose your tier above!
