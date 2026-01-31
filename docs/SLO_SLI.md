# Service Level Objectives & Indicators - erlmcp

## Executive Summary

This document defines Service Level Indicators (SLIs), Service Level Objectives (SLOs), and error budgets for erlmcp deployments. SLIs measure what users care about, SLOs set reliability targets, and error budgets balance feature velocity with stability.

**Key SLOs (Production Tier):**
- **Availability**: 99.9% uptime (43.2 minutes downtime/month)
- **Request Success Rate**: 99.5% (0.5% error budget)
- **Latency (p50)**: <100ms for 99% of operations
- **Latency (p99)**: <500ms for 95% of operations
- **Throughput**: 40,000 concurrent connections per node

**Philosophy:** "Hope is not a strategy" - Measure everything, set targets, enforce budgets.

## Service Level Indicators (SLIs)

### 1. Availability SLI

**Definition:** Percentage of time the service is operational and accepting requests.

**Measurement:**
```erlang
% Uptime calculation (per node)
Uptime = (Total_Time - Downtime) / Total_Time * 100

% Where:
% Total_Time = 30 days = 2,592,000 seconds
% Downtime = Sum of all incident durations (service unavailable)
```

**Prometheus Query:**
```promql
# Availability over 30-day window
(
  sum(up{job="erlmcp"})
  /
  count(up{job="erlmcp"})
) * 100
```

**Targets by Tier:**

| Tier | Target | Downtime/Month | Downtime/Year |
|------|--------|----------------|---------------|
| **Production** | 99.9% | 43.2 minutes | 8.76 hours |
| **Staging** | 99.5% | 3.6 hours | 43.8 hours |
| **Development** | 95.0% | 36 hours | 18.25 days |

**Exclusions:**
- Planned maintenance windows (announced 7 days prior)
- Client-side failures (invalid requests)
- Dependency failures (upstream LLM unavailable)

### 2. Request Success Rate SLI

**Definition:** Percentage of requests that complete successfully without errors.

**Measurement:**
```erlang
Success_Rate = (Successful_Requests / Total_Requests) * 100

% Where:
% Successful_Requests = Requests with HTTP 2xx or JSON-RPC success
% Total_Requests = All requests received
```

**Prometheus Query:**
```promql
# Success rate over 5-minute window
sum(rate(erlmcp_requests_total{status="success"}[5m]))
/
sum(rate(erlmcp_requests_total[5m]))
* 100
```

**Targets:**

| Operation Type | Target | Error Budget |
|----------------|--------|--------------|
| **Tool Calls** | 99.5% | 0.5% |
| **Resource Reads** | 99.9% | 0.1% |
| **Session Operations** | 99.95% | 0.05% |
| **Registry Lookups** | 99.99% | 0.01% |

**Valid Errors (excluded from SLI):**
- Client errors (4xx equivalent): Invalid requests, malformed JSON
- Rate limit exceeded (intentional throttling)
- Authentication failures (invalid credentials)

**Invalid Errors (counted in SLI):**
- Server errors (5xx equivalent): Crashes, timeouts, resource exhaustion
- Circuit breaker triggered (downstream failures)
- Database connection pool exhausted

### 3. Latency SLI

**Definition:** Time from request received to response sent, measured at percentiles.

**Measurement:**
```erlang
% Record latency for each request
Start = erlang:monotonic_time(microsecond),
% ... process request ...
End = erlang:monotonic_time(microsecond),
Latency_us = End - Start,
erlmcp_metrics:observe(request_latency, Latency_us).
```

**Prometheus Query:**
```promql
# p50 latency over 5-minute window
histogram_quantile(0.50,
  sum(rate(erlmcp_request_duration_microseconds_bucket[5m])) by (le)
) / 1000  # Convert to milliseconds

# p99 latency
histogram_quantile(0.99,
  sum(rate(erlmcp_request_duration_microseconds_bucket[5m])) by (le)
) / 1000
```

**Targets:**

| Percentile | Target | Measurement Window |
|------------|--------|-------------------|
| **p50** | <100ms | 5-minute rolling |
| **p95** | <250ms | 5-minute rolling |
| **p99** | <500ms | 5-minute rolling |
| **p99.9** | <1000ms | 30-minute rolling |

**Latency Budget by Operation:**

| Operation | p50 | p95 | p99 |
|-----------|-----|-----|-----|
| **Registry Lookup** | <1ms | <5ms | <10ms |
| **Session Read** | <10ms | <25ms | <50ms |
| **Tool Call (local)** | <50ms | <150ms | <300ms |
| **Tool Call (remote LLM)** | <200ms | <500ms | <1000ms |
| **Resource Subscription** | <25ms | <75ms | <150ms |

**Exclusions:**
- Client network latency (not measured by erlmcp)
- Upstream LLM latency (tracked separately as dependency SLI)

### 4. Throughput SLI

**Definition:** Number of concurrent connections and requests per second the service can sustain.

**Measurement:**
```erlang
% Concurrent connections (per node)
Active_Connections = erlmcp_registry:count_active_clients().

% Requests per second (per node)
RPS = Total_Requests / Time_Window_Seconds.
```

**Prometheus Query:**
```promql
# Concurrent connections
sum(erlmcp_active_connections)

# Requests per second
sum(rate(erlmcp_requests_total[1m]))
```

**Targets (per node):**

| Metric | Target | Absolute Max |
|--------|--------|--------------|
| **Concurrent Connections** | 40,000 | 50,000 |
| **Requests/Second** | 10,000 | 15,000 |
| **Messages/Second** | 50,000 | 75,000 |

**Cluster Capacity:**

| Cluster Size | Total Connections | Total RPS |
|--------------|-------------------|-----------|
| 1 node | 40,000 | 10,000 |
| 3 nodes | 120,000 | 30,000 |
| 5 nodes | 200,000 | 50,000 |
| 10 nodes | 400,000 | 100,000 |

**Degradation Thresholds:**
- **80% capacity**: Enable backpressure
- **90% capacity**: Reject new connections (return 503)
- **95% capacity**: Trigger auto-scaling

### 5. Error Rate SLI

**Definition:** Percentage of operations that fail with server errors.

**Measurement:**
```erlang
Error_Rate = (Server_Errors / Total_Requests) * 100

% Where:
% Server_Errors = Crashes, timeouts, resource exhaustion
% Excludes client errors (4xx)
```

**Prometheus Query:**
```promql
# Error rate over 5-minute window
sum(rate(erlmcp_requests_total{status=~"error|timeout"}[5m]))
/
sum(rate(erlmcp_requests_total[5m]))
* 100
```

**Targets:**

| Error Type | Target | Threshold |
|------------|--------|-----------|
| **Overall Error Rate** | <1% | Alert at 2% |
| **Timeout Rate** | <0.5% | Alert at 1% |
| **Crash Rate** | <0.1% | Alert at 0.5% |
| **Circuit Breaker Triggers** | <0.2% | Alert at 0.5% |

## Service Level Objectives (SLOs)

### Production Tier SLOs

**Target Users:** Production deployments, paying customers, SLA commitments

| SLI | Objective | Measurement | Error Budget |
|-----|-----------|-------------|--------------|
| **Availability** | 99.9% uptime | 30-day rolling window | 43.2 min/month |
| **Success Rate** | 99.5% successful requests | 30-day rolling window | 0.5% errors |
| **Latency (p50)** | <100ms | 5-minute rolling window | 1% budget |
| **Latency (p99)** | <500ms | 5-minute rolling window | 5% budget |
| **Throughput** | 40K connections/node | Real-time | N/A |
| **Error Rate** | <1% server errors | 5-minute rolling window | 1% budget |

**Composite SLO:**
```
Service_Health = (
    0.4 * Availability_Score +
    0.3 * Success_Rate_Score +
    0.2 * Latency_Score +
    0.1 * Error_Rate_Score
)

% Where each score is: min(Actual / Target, 1.0)
```

**Target:** Service_Health ≥ 0.99 (99%)

### Staging Tier SLOs

**Target Users:** QA testing, integration testing, pre-production validation

| SLI | Objective | Measurement |
|-----|-----------|-------------|
| **Availability** | 99.5% uptime | 30-day rolling window |
| **Success Rate** | 99% successful requests | 30-day rolling window |
| **Latency (p99)** | <1000ms | 5-minute rolling window |
| **Throughput** | 10K connections/node | Real-time |

### Development Tier SLOs

**Target Users:** Local development, feature testing, experimentation

| SLI | Objective | Measurement |
|-----|-----------|-------------|
| **Availability** | 95% uptime | Best effort |
| **Success Rate** | 95% successful requests | Best effort |
| **Latency (p99)** | <2000ms | Best effort |

## Error Budget Calculation

### Error Budget Definition

**Error Budget** = Allowed downtime or errors before violating SLO

**Formula:**
```
Error_Budget = (1 - SLO_Target) * Total_Requests

Example (99.9% availability):
Error_Budget = (1 - 0.999) * 30_days
             = 0.001 * 2,592,000 seconds
             = 2,592 seconds
             = 43.2 minutes per month
```

### Error Budget Tracking

**Prometheus Query:**
```promql
# Remaining error budget (availability)
(1 - (
  sum(rate(erlmcp_downtime_seconds[30d]))
  /
  (30 * 24 * 60 * 60)
)) - 0.999

# Remaining error budget (success rate)
(
  sum(rate(erlmcp_requests_total{status="success"}[30d]))
  /
  sum(rate(erlmcp_requests_total[30d]))
) - 0.995
```

**Dashboard Visualization:**

```
Error Budget Burn Rate (Availability)
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ 100%
████████████████████████░░░░░░░░░░░░░░░  65% remaining
                        ↑
                   15 days into month
                   15.1 min used (35%)
```

### Burn Rate Alerts

**Fast Burn (Critical):**
- Alert if error budget will be exhausted in <7 days
- Formula: `Burn_Rate > Error_Budget / (7 * 24 * 60 * 60)`
- Action: Immediate incident response

**Slow Burn (Warning):**
- Alert if error budget will be exhausted in <30 days
- Formula: `Burn_Rate > Error_Budget / (30 * 24 * 60 * 60)`
- Action: Investigate trends, plan improvements

**Example:**
```promql
# Fast burn alert (availability)
(
  sum(rate(erlmcp_downtime_seconds[1h]))
  /
  (1 - 0.999)
) > (1 / 7)  # Will exhaust budget in <7 days
```

## Measurement Windows

### Rolling Windows

**30-Day Rolling Window (Availability, Success Rate):**
- Continuously updated, not calendar-aligned
- Example: On Feb 15, window is Jan 16 - Feb 15

**5-Minute Rolling Window (Latency, Error Rate):**
- Real-time responsiveness
- Alerts trigger on short-term degradation

**1-Hour Rolling Window (Throughput):**
- Captures sustained load patterns
- Smooths out short-term spikes

### Exclusion Windows

**Planned Maintenance:**
- Scheduled outages announced 7 days prior
- Maximum 4 hours/month
- Excluded from availability SLI

**Deployment Windows:**
- Canary deployments: 10% traffic for 30 minutes
- Full rollout: Gradual over 2 hours
- Included in availability SLI (must maintain SLO during deploy)

## Monitoring Queries

### Grafana Dashboard Queries

**1. Availability (Uptime)**
```promql
# Current availability (30-day)
(
  1 - (
    sum(increase(erlmcp_downtime_seconds[30d]))
    /
    (30 * 24 * 60 * 60)
  )
) * 100

# SLO: 99.9% (red line on graph)
```

**2. Request Success Rate**
```promql
# Success rate (5-minute window)
sum(rate(erlmcp_requests_total{status="success"}[5m]))
/
sum(rate(erlmcp_requests_total[5m]))
* 100

# SLO: 99.5% (red line)
```

**3. Latency Percentiles**
```promql
# p50 latency
histogram_quantile(0.50,
  sum(rate(erlmcp_request_duration_microseconds_bucket[5m])) by (le, operation)
) / 1000

# p95 latency
histogram_quantile(0.95,
  sum(rate(erlmcp_request_duration_microseconds_bucket[5m])) by (le, operation)
) / 1000

# p99 latency
histogram_quantile(0.99,
  sum(rate(erlmcp_request_duration_microseconds_bucket[5m])) by (le, operation)
) / 1000
```

**4. Throughput**
```promql
# Active connections (real-time)
sum(erlmcp_active_connections)

# Requests per second (1-minute rate)
sum(rate(erlmcp_requests_total[1m]))
```

**5. Error Rate**
```promql
# Overall error rate (5-minute window)
sum(rate(erlmcp_requests_total{status=~"error|timeout"}[5m]))
/
sum(rate(erlmcp_requests_total[5m]))
* 100
```

### Alert Rules

**Critical Alerts (PagerDuty):**

```yaml
# Availability SLO breach
- alert: AvailabilitySLOBreach
  expr: |
    (1 - (
      sum(increase(erlmcp_downtime_seconds[30d]))
      /
      (30 * 24 * 60 * 60)
    )) < 0.999
  for: 5m
  labels:
    severity: critical
  annotations:
    summary: "Availability below 99.9% SLO"
    description: "Current: {{ $value }}%, Target: 99.9%"

# Fast error budget burn
- alert: ErrorBudgetFastBurn
  expr: |
    sum(rate(erlmcp_downtime_seconds[1h]))
    /
    (1 - 0.999)
    > (1 / 7)
  for: 15m
  labels:
    severity: critical
  annotations:
    summary: "Error budget will exhaust in <7 days"
```

**Warning Alerts (Slack):**

```yaml
# Latency degradation (p99)
- alert: LatencyP99Degraded
  expr: |
    histogram_quantile(0.99,
      sum(rate(erlmcp_request_duration_microseconds_bucket[5m])) by (le)
    ) / 1000 > 500
  for: 10m
  labels:
    severity: warning
  annotations:
    summary: "p99 latency above 500ms"
    description: "Current: {{ $value }}ms, Target: <500ms"

# Error rate elevated
- alert: ErrorRateElevated
  expr: |
    sum(rate(erlmcp_requests_total{status=~"error|timeout"}[5m]))
    /
    sum(rate(erlmcp_requests_total[5m]))
    * 100
    > 2
  for: 5m
  labels:
    severity: warning
  annotations:
    summary: "Error rate above 2%"
    description: "Current: {{ $value }}%, Target: <1%"
```

## SLO Compliance Reporting

### Monthly SLO Report

```markdown
# SLO Compliance Report - January 2026

## Summary

| SLI | Target | Actual | Status | Error Budget Used |
|-----|--------|--------|--------|-------------------|
| Availability | 99.9% | 99.94% | ✅ PASS | 6.1 min / 43.2 min (14%) |
| Success Rate | 99.5% | 99.72% | ✅ PASS | 0.28% / 0.5% (56%) |
| Latency (p50) | <100ms | 87ms | ✅ PASS | - |
| Latency (p99) | <500ms | 412ms | ✅ PASS | - |
| Error Rate | <1% | 0.6% | ✅ PASS | 0.6% / 1% (60%) |

**Overall Compliance:** 100% (5/5 SLOs met)

## Incidents

| Date | Duration | Impact | Cause |
|------|----------|--------|-------|
| 2026-01-15 | 3.5 min | Availability | Connection pool exhaustion |
| 2026-01-23 | 2.6 min | Availability | Memory spike (GC pause) |

**Total Downtime:** 6.1 minutes (14% of error budget)

## Trends

- ↑ Availability improved from 99.91% (December)
- ↑ Error rate decreased from 0.8% (December)
- → Latency stable (no significant change)

## Action Items

1. Investigate memory spikes causing GC pauses (owner: Alice)
2. Add connection pool saturation alerts (owner: Bob)
3. Continue monitoring - on track for Q1 targets
```

### Quarterly SLO Review

**Review Process:**
1. Analyze 90-day SLI trends
2. Assess SLO achievability (too easy? too hard?)
3. Adjust targets based on business needs
4. Update error budgets accordingly

**SLO Adjustment Criteria:**
- If SLOs consistently exceeded (>110%), consider tightening targets
- If SLOs frequently missed (<90%), consider loosening targets or investing in reliability
- Balance feature velocity with reliability (error budget policy)

## Trade-offs and Priorities

### Error Budget Policy

**When Error Budget is Healthy (>50% remaining):**
- ✅ Ship new features aggressively
- ✅ Experiment with new transports
- ✅ Deploy multiple times per day

**When Error Budget is Low (<25% remaining):**
- ⚠️ Slow down feature releases
- ⚠️ Focus on reliability improvements
- ⚠️ Increase testing, reduce deployment frequency

**When Error Budget is Exhausted (0% remaining):**
- 🛑 **FREEZE**: No new features until SLO restored
- 🛑 Focus 100% on reliability work
- 🛑 Emergency deployments only (security, P0 bugs)

### SLO vs. Feature Velocity Trade-off

**Case Study:**

```
Q1 2026 Error Budget Status (Feb 15):
- Availability: 35% remaining (healthy)
- Success Rate: 15% remaining (at risk)
- Decision: Ship latency optimization feature (low risk)
           Defer new WebSocket compression (medium risk)
```

**Priority Matrix:**

| Feature | Risk | Error Budget Impact | Decision |
|---------|------|---------------------|----------|
| Latency optimization | Low | Improves SLI | ✅ Ship |
| New transport type | High | May impact availability | ❌ Defer to Q2 |
| Session failover | Medium | May impact success rate | ⚠️ Canary deploy |

### Reliability vs. Cost Trade-off

**SLO Targets vs. Infrastructure Cost:**

| Target | Infrastructure | Annual Cost | ROI |
|--------|----------------|-------------|-----|
| 99% (2-nines) | 2 nodes | $50K | Acceptable for dev |
| 99.9% (3-nines) | 3-5 nodes + standby | $150K | Production standard |
| 99.99% (4-nines) | 10+ nodes + multi-region | $500K+ | Enterprise SLA |

**Recommendation:** Target 99.9% for production (3-nines) as sweet spot.

## Continuous Improvement

### Kaizen (改善) - SLO Evolution

**Quarterly SLO Retrospective:**
1. Review SLO achievement rates
2. Analyze near-misses and violations
3. Identify systemic issues
4. Set improvement targets

**Example Improvements:**
- Q4 2025: Added connection pool monitoring → Reduced timeouts by 40%
- Q1 2026: Implemented circuit breakers → Reduced cascading failures by 80%
- Q2 2026 (planned): Multi-region failover → Target 99.95% availability

### Jidoka (自働化) - Built-in Quality

**Automated SLO Enforcement:**
```erlang
% Pre-deployment SLO validation
erlmcp_slo:validate_deployment(#{
    canary_traffic => 0.10,      % 10% traffic
    duration => 1800,            % 30 minutes
    slo_thresholds => #{
        error_rate => 0.02,      % <2% errors
        latency_p99 => 600       % <600ms p99
    }
}).
%=> {pass, #{error_rate => 0.8, latency_p99 => 450}}
%=> Auto-promote to 100% traffic

%=> {fail, #{error_rate => 3.2, latency_p99 => 750}}
%=> Auto-rollback to previous version
```

---

**References:**
- [Google SRE Book - SLIs, SLOs, SLAs](https://sre.google/sre-book/service-level-objectives/)
- [Implementing SLOs](https://sre.google/workbook/implementing-slos/)
- Toyota Production System (Kaizen, Jidoka)
- [Prometheus Best Practices](https://prometheus.io/docs/practices/)
