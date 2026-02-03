# Service Level Agreement (SLA) for erlmcp on Google Cloud Marketplace

## Overview

This Service Level Agreement (SLA) defines the service levels and performance commitments for erlmcp when deployed on Google Cloud Platform via the Cloud Marketplace.

## Service Availability

### Uptime Commitments by Deployment Type

| Deployment Type | Monthly Uptime SLA | Annual Uptime SLA | Downtime Allowance (Monthly) |
|-----------------|-------------------|------------------|----------------------------|
| **Cloud Run** | 99.9% | 99.9% | 43.2 minutes |
| **GKE Regional** | 99.95% | 99.95% | 21.6 minutes |
| **GKE Multi-Region** | 99.99% | 99.99% | 4.32 minutes |
| **Compute Engine** | 99.9% | 99.9% | 43.2 minutes |

### SLA Calculation

**Monthly Uptime Percentage** = (Total Minutes in Month - Downtime Minutes) / Total Minutes in Month × 100

**Excluded Downtime**
- Scheduled maintenance (announced 7 days in advance)
- Customer-caused outages
- Force majeure events
- Third-party service failures
- Beta/preview features

## Performance Metrics

### Response Time SLA

| Metric | Target | Measurement |
|--------|--------|-------------|
| **API Latency (p50)** | < 100ms | /health endpoint |
| **API Latency (p95)** | < 500ms | /health endpoint |
| **API Latency (p99)** | < 1000ms | /health endpoint |
| **Connection Setup** | < 200ms | TCP handshake |
| **TLS Handshake** | < 300ms | TLS 1.3 negotiation |

### Throughput SLA

| Deployment Type | Max Concurrent Connections | Requests/Second |
|-----------------|-------------------------|-----------------|
| Cloud Run | 1000 | 10,000 |
| GKE (Medium) | 10,000 | 100,000 |
| GKE (Large) | 100,000 | 1,000,000 |
| Compute Engine | 5,000 | 50,000 |

### Scaling SLA

| Metric | Target | Measurement |
|--------|--------|-------------|
| **Scale-up Time** | < 5 minutes | Pod/instance ready |
| **Scale-down Time** | < 10 minutes | Pod/instance terminated |
| **Auto-scaling Trigger** | < 30 seconds | Metric to action |
| **Canary Rollout** | < 2 minutes | New version serving |

## Recovery Time Objectives (RTO)

### RTO by Incident Severity

| Severity | Description | RTO | RPO |
|----------|-------------|-----|-----|
| **Critical** | Complete service outage | 15 minutes | 1 minute |
| **High** | Major degradation (>50% users affected) | 1 hour | 5 minutes |
| **Medium** | Partial degradation (<50% users affected) | 4 hours | 15 minutes |
| **Low** | Minimal impact | 24 hours | 1 hour |

### Recovery Point Objectives (RPO)

| Data Type | RPO | Backup Method |
|-----------|-----|---------------|
| **Configuration** | 1 minute | Git/version control |
| **Session Data** | 5 minutes | Redis persistence |
| **Application State** | 15 minutes | Distributed snapshots |
| **Logs/Metrics** | 1 hour | Cloud Logging/Monitoring |

## Service Credits

### Credit Calculation

If the Monthly Uptime Percentage falls below the SLA, service credits are calculated as:

| Actual Monthly Uptime | Credit Percentage |
|---------------------|-------------------|
| 99.0% to < 99.9% | 10% |
| 95.0% to < 99.0% | 25% |
| < 95.0% | 100% |

### Credit Eligibility

- Credits are applied to future billing periods
- Maximum credit is 100% of monthly fees
- No cash refunds
- Must claim within 30 days of incident

### Exclusions

Credits are not provided for outages caused by:
- Customer actions or misconfigurations
- Third-party services outside our control
- Force majeure events
- Beta features or preview releases
- Free tier usage

## Maintenance Windows

### Scheduled Maintenance

| Deployment Type | Frequency | Window | Duration |
|----------------|-----------|--------|----------|
| Cloud Run | Weekly | Sunday 3:00-4:00 AM UTC | 1 hour |
| GKE Regional | Monthly | First Sunday 3:00-6:00 AM UTC | 3 hours |
| GKE Multi-Region | Quarterly | Q1/Q2/Q3/Q4 First Sunday | 6 hours |
| Compute Engine | Monthly | First Sunday 3:00-6:00 AM UTC | 3 hours |

### Maintenance Notification

- **Email**: 7 days advance notice
- **Console**: Banner in Cloud Console
- **API**: Status page update
- **RSS**: Feed for status changes

### Graceful Degradation

During maintenance:
- Health check returns `200 OK` with `status: "maintenance"`
- In-flight requests complete
- New requests queued/rejected with 503
- Auto-scaling paused during maintenance

## Monitoring and Reporting

### Health Check Endpoints

```
GET /health          # Service health (200 OK or 503)
GET /ready           # Readiness (200 OK or 503)
GET /metrics         # Prometheus metrics
GET /status          # Detailed status with components
```

### Status Response

```json
{
  "status": "healthy",
  "version": "3.0.0",
  "components": {
    "api": "healthy",
    "database": "healthy",
    "cache": "healthy",
    "distributed": "healthy"
  },
  "uptime": 99.95,
  "deployed_at": "2024-01-01T00:00:00Z"
}
```

### SLA Dashboard

Public SLA dashboard at: `https://status.erlmcp.dev`

Includes:
- Current service status
- 30-day uptime graph
- Incident history
- Upcoming maintenance

## Incident Management

### Incident Classification

| Class | Definition | Example |
|-------|-----------|---------|
| **P1** | Complete outage, critical impact | All requests failing |
| **P2** | Major degradation, significant impact | >50% errors |
| **P3** | Minor degradation, limited impact | >10% errors |
| **P4** | Cosmetic issue, no impact | Typo in error message |

### Response SLA

| Class | Initial Response | Update Frequency | Resolution Target |
|-------|-----------------|------------------|-------------------|
| P1 | 15 minutes | Every 15 minutes | 4 hours |
| P2 | 1 hour | Every 30 minutes | 24 hours |
| P3 | 4 hours | Every 2 hours | 3 days |
| P4 | 1 business day | Daily | 1 week |

### Communication Channels

- **Status Page**: https://status.erlmcp.dev
- **Email**: incident-notifications@erlmcp.dev
- **Slack**: #erlmcp-incidents (for customers)
- **RSS**: https://status.erlmcp.dev/rss

## Support Tiers

### Tier Comparison

| Feature | Basic | Pro | Enterprise |
|---------|-------|-----|------------|
| **Uptime SLA** | 99.9% | 99.95% | 99.99% |
| **Response Time** | 24 hours | 4 hours | 1 hour |
| **Support Channels** | Email | Email + Chat | All + Dedicated |
| **Escalation** | Standard | Priority | 24/7 |
| **Custom SLA** | No | No | Yes |

### Contact Information

- **Basic**: support@erlmcp.dev
- **Pro**: pro-support@erlmcp.dev
- **Enterprise**: enterprise@erlmcp.dev (24/7 hotline)

## Exclusions and Limitations

### SLA Exclusions

1. **Customer Actions**: Misconfiguration, abuse, exceeding quotas
2. **Third-Party Services**: GCP outages, internet disruptions
3. **Force Majeure**: Natural disasters, wars, government actions
4. **Beta Features**: Preview/beta features have no SLA
5. **Free Tier**: No SLA commitment for free usage

### SLA Suspension

SLA is suspended during:
- Major GCP outages affecting the region
- Customer-initiated maintenance
- Security incidents requiring service shutdown
- Force majeure events

## Compliance and Certifications

### Supported Compliance Frameworks

- **SOC 2 Type II**: Available upon request
- **ISO 27001**: Available for Enterprise tier
- **HIPAA**: Available with BAA for Healthcare edition
- **GDPR**: Compliant for EU deployments
- **FedRAMP**: Available for Government edition

### Data Residency

| Region | Data Location | Compliance |
|--------|--------------|------------|
| us-central1 | United States | SOC 2, ISO 27001 |
| europe-west1 | European Union | GDPR, ISO 27001 |
| asia-southeast1 | Singapore | PDPA, ISO 27001 |

## Amendments and Updates

### SLA Version History

| Version | Date | Changes |
|---------|------|---------|
| 3.0.0 | 2024-01-01 | Initial SLA for Marketplace |

### SLA Updates

- Minor updates: 30 days notice
- Major updates: 90 days notice
- Customer may opt out with 30 days notice

## Acceptance

By deploying erlmcp from the Google Cloud Marketplace, you agree to this SLA. For Enterprise customers with custom SLAs, the custom agreement takes precedence.

## Contact

For SLA-related questions:
- Email: sla@erlmcp.dev
- Documentation: https://docs.erlmcp.dev/sla
- Support: https://github.com/banyan-platform/erlmcp/issues
