# Post-Release Monitoring Guide
## erlmcp v3.0.0 - Production Monitoring Strategy

**Document Version:** 1.0
**Last Updated:** 2026-02-02
**Purpose:** Comprehensive monitoring strategy for post-release stability

---

## Table of Contents

1. [Monitoring Overview](#1-monitoring-overview)
2. [First 24 Hours](#2-first-24-hours)
3. [First Week](#3-first-week)
4. [Ongoing Monitoring](#4-ongoing-monitoring)
5. [Alert Configuration](#5-alert-configuration)
6. [Incident Response](#6-incident-response)
7. [Reporting](#7-reporting)

---

## 1. Monitoring Overview

### 1.1 Monitoring Philosophy

**"You can't manage what you don't measure."**

Effective post-release monitoring follows these principles:

1. **Proactive Monitoring** - Detect issues before customers do
2. **Metric-Driven** - Make decisions based on data
3. **Fast Response** - Minimize time to detection and resolution
4. **Continuous Improvement** - Learn from each release

### 1.2 Monitoring Pillars

```
                    +---------------------+
                    |   Customer Impact   |
                    +---------------------+
                              |
        +---------------------+---------------------+
        |                     |                     |
+-------v-------+     +-------v-------+     +-------v-------+
|   System      |     |   Business    |     |   Security    |
|   Health      |     |   Metrics     |     |   Events      |
+---------------+     +---------------+     +---------------+
```

### 1.3 SLA Targets

| Metric | Target | Warning | Critical |
|--------|--------|---------|----------|
| Availability | 99.95% | < 99.9% | < 99% |
| Error Rate | < 0.1% | > 0.5% | > 1% |
| Response Time (p95) | < 500ms | > 1s | > 2s |
| Time to Detect | < 5 min | > 10 min | > 15 min |
| Time to Resolve | < 30 min | > 1 hour | > 2 hours |

---

## 2. First 24 Hours

### 2.1 Monitoring Schedule

The first 24 hours are critical for detecting any immediate issues.

```
T+0 (Release)
    |
    v
T+1 hour  ----> Intensive monitoring (15-min intervals)
    |
    v
T+4 hours ----> Health check milestone
    |
    v
T+8 hours ----> Mid-day review
    |
    v
T+12 hours ---> Half-day milestone
    |
    v
T+24 hours ---> 24-hour stability milestone
```

### 2.2 T+1 Hour Checks

**Objective:** Verify deployment was successful

```bash
# Health Check Script
#!/bin/bash
# File: /marketplace/gcp/scripts/health-check.sh

echo "=== T+1 Hour Health Check ==="
echo "Time: $(date -u +"%Y-%m-%dT%H:%M:%SZ")"

# 1. Cloud Run Health
echo -e "\n1. Cloud Run Health"
SERVICE_URL=$(gcloud run services describe erlmcp \
  --region=us-central1 \
  --format='value(status.url)')

if curl -s "${SERVICE_URL}/health" | grep -q '"status":"ok"'; then
  echo "  ✓ Cloud Run healthy"
else
  echo "  ✗ Cloud Run unhealthy"
  exit 1
fi

# 2. Error Rate Check
echo -e "\n2. Error Rate Check"
ERROR_RATE=$(gcloud logging read \
  'resource.type=cloud_run_revision AND labels.service=erlmcp AND severity>=ERROR' \
  --freshness=1h \
  --project=$PROJECT_ID \
  --format='value(protoPayload.status.code)' | wc -l)

if [ $ERROR_RATE -lt 10 ]; then
  echo "  ✓ Error rate acceptable: $ERROR_RATE errors/hour"
else
  echo "  ⚠ High error rate: $ERROR_RATE errors/hour"
fi

# 3. Response Time Check
echo -e "\n3. Response Time Check"
LATENCY=$(gcloud logging read \
  'resource.type=cloud_run_revision AND labels.service=erlmcp AND jsonPayload.latency' \
  --freshness=1h \
  --project=$PROJECT_ID \
  --format='value(jsonPayload.latency)' | awk '{s+=$1; n++} END {print s/n}')

echo "  Average latency: ${LATENCY}ms"

# 4. Deployment Status
echo -e "\n4. Deployment Status"
REVISION=$(gcloud run services describe erlmcp \
  --region=us-central1 \
  --format='value(status.latestReadyRevisionName)')
echo "  Active revision: $REVISION"

echo -e "\n=== Health Check Complete ==="
```

### 2.3 T+4 Hours Checks

**Objective:** Establish baseline metrics

| Metric | Current | Baseline | Status |
|--------|---------|----------|--------|
| Requests/min | TBD | TBD | TBD |
| Error Rate | TBD | < 0.1% | TBD |
| p95 Latency | TBD | < 500ms | TBD |
| Memory Usage | TBD | < 80% | TBD |

### 2.4 T+8 Hours Checks

**Objective:** First stability assessment

- [ ] No critical incidents
- [ ] Metrics within normal ranges
- [ ] No customer complaints
- [ ] Log patterns normal

### 2.5 T+12 Hours Checks

**Objective:** Half-day milestone

```bash
# Half-day summary script
cat << 'EOF'
# Half-Day Release Summary

**Release:** erlmcp v3.0.0
**Time:** 12 hours post-release
**Date:** $(date -u +"%Y-%m-%d")

## Health Status
- Service Availability: TBD%
- Error Rate: TBD%
- Average Latency: TBDms

## Incident Summary
- Critical: 0
- High: 0
- Medium: TBD
- Low: TBD

## Customer Feedback
- Support Tickets: TBD
- GitHub Issues: TBD
- Community Reports: TBD

## Assessment
[ ] Healthy - No concerns
[ ] Stable - Minor issues
[ ] Concerned - Active monitoring
[ ] Critical - Escalation needed
EOF
```

### 2.6 T+24 Hours Checks

**Objective:** 24-hour milestone achieved

**Success Criteria:**

- [ ] Zero critical incidents
- [ ] Zero high-severity incidents
- [ ] Availability >= 99.9%
- [ ] Error rate < 0.5%

---

## 3. First Week

### 3.1 Daily Monitoring Schedule

| Day | Focus Area | Owner | Success Criteria |
|-----|------------|-------|------------------|
| Day 1 | Critical issues | Release Manager | No P0/P1 incidents |
| Day 2 | Performance | Engineering Lead | Baseline maintained |
| Day 3 | User feedback | Product Manager | Positive sentiment |
| Day 4 | Adoption | Product Manager | Expected uptake |
| Day 5 | Stability | QA Lead | Zero unexpected issues |
| Day 6 | Trends | Operations Lead | Positive trajectory |
| Day 7 | Weekly summary | Release Manager | Stable release |

### 3.2 Daily Health Report Template

```markdown
# Daily Release Health Report - Day {N}

**Release:** erlmcp v3.0.0
**Date:** {DATE}
**Reporting Period:** {START} to {END}

## Executive Summary
{BRIEF_STATUS}

## Key Metrics

| Metric | Value | Target | Status |
|--------|-------|--------|--------|
| Availability | {VALUE}% | >= 99.9% | {STATUS} |
| Error Rate | {VALUE}% | < 0.5% | {STATUS} |
| p95 Latency | {VALUE}ms | < 1000ms | {STATUS} |
| Throughput | {VALUE}/min | Baseline | {STATUS} |

## Incidents

| ID | Severity | Description | Status |
|----|----------|-------------|--------|
| {INCIDENT_ID} | {SEVERITY} | {DESCRIPTION} | {STATUS} |

## Customer Feedback

- **Support Tickets:** {OPEN} open, {CLOSED} closed
- **GitHub Issues:** {NEW} new, {RESOLVED} resolved
- **Community Sentiment:** {POSITIVE/NEGATIVE/NEUTRAL}

## Highlights

{POSITIVE_EVENTS}

## Concerns

{CONCERNS}

## Action Items

1. {ACTION_ITEM_1}
2. {ACTION_ITEM_2}

## Tomorrow's Focus

{NEXT_DAY_FOCUS}
```

### 3.3 Weekly Summary Report

```markdown
# Weekly Release Summary - Week 1

**Release:** erlmcp v3.0.0
**Week Ending:** {DATE}

## Achievement Status

| Milestone | Status | Notes |
|-----------|--------|-------|
| 24-hour stability | [x] | Achieved |
| 3-day stability | [ ] | In progress |
| 7-day stability | [ ] | Pending |

## Key Metrics (Weekly Average)

| Metric | Week 1 | Target | Status |
|--------|--------|--------|--------|
| Availability | {VALUE}% | >= 99.95% | {STATUS} |
| Error Rate | {VALUE}% | < 0.1% | {STATUS} |
| p95 Latency | {VALUE}ms | < 500ms | {STATUS} |

## Incident Summary

| Severity | Count | MTTR |
|----------|-------|------|
| Critical | 0 | - |
| High | {COUNT} | {MTTR} |
| Medium | {COUNT} | {MTTR} |
| Low | {COUNT} | {MTTR} |

## Customer Impact

- **Total Users Affected:** {COUNT}
- **Total Downtime:** {MINUTES} minutes
- **Credits Issued:** {AMOUNT}

## Release Assessment

**Overall Status:** {STABLE/CONCERNED/UNSTABLE}

**Recommendations:**
1. {RECOMMENDATION_1}
2. {RECOMMENDATION_2}

**Next Steps:**
- Continue monitoring for {DURATION}
- Schedule retrospective on {DATE}
- Begin planning for {NEXT_RELEASE}
```

---

## 4. Ongoing Monitoring

### 4.1 Continuous Monitoring Dashboard

Create a Cloud Monitoring dashboard with:

```
┌─────────────────────────────────────────────────────────┐
│              erlmcp v3.0.0 Release Dashboard            │
├─────────────────────────────────────────────────────────┤
│                                                         │
│  Availability:     99.97%  ████████████████████░░ 3m   │
│  Error Rate:       0.05%  ████████████████████░░ OK    │
│  p95 Latency:      234ms  ████████████████████░░ OK    │
│  Throughput:       1.2K/s ████████████████████░░ OK    │
│                                                         │
├─────────────────────────────────────────────────────────┤
│                        REQUESTS                         │
│  2.5K │         ┌───┐      ┌───┐                      │
│  2.0K │     ┌───┘   └───┐  ┌─┘   └───┐                │
│  1.5K │  ┌──┘           └──┘         └───┐            │
│  1.0K │──┘                            └──┐           │
│  0.5K │                                    └───┐       │
│  0.0K └────────────────────────────────────────┴────   │
│        00  04  08  12  16  20  24                   │
├─────────────────────────────────────────────────────────┤
│                       ERRORS                            │
│  100% │                                                 │
│  75%  │                                                 │
│  50%  │    ████                                         │
│  25%  │    ████ ████                                   │
│  0%   │────████████─────────────────────────────        │
│        00  04  08  12  16  20  24                   │
├─────────────────────────────────────────────────────────┤
│                      LATENCY (ms)                       │
│  1000 │                                                 │
│  750  │  ╭──╮                                           │
│  500  │ ╭┘  ╰──╮ ╭──╮                                 │
│  250  │╭┘      ╰─╯  ╰──╮                               │
│  0    ┴─────────────────────────────────────────────    │
│        00  04  08  12  16  20  24                   │
└─────────────────────────────────────────────────────────┘
```

### 4.2 Metric Definitions

#### 4.2.1 System Metrics

```yaml
system_metrics:
  - name: availability
    type: gauge
    description: Percentage of time service is healthy
    query: |
      fetch gce_instance
      | metric 'monitoring.googleapis.com/uptime_check/check_passed'
      | align delta(1m)
      | every 1m
      | value_by(true)

  - name: error_rate
    type: gauge
    description: Percentage of requests returning errors
    query: |
      fetch cloud_run_revision
      | metric 'run.googleapis.com/request_count'
      | align delta(1m)
      | every 1m
      | group_by [metric.response_code_class]
      | proportion true
      | filter metric.response_code_class >= '400'

  - name: latency_p95
    type: gauge
    description: 95th percentile request latency
    query: |
      fetch cloud_run_revision
      | metric 'run.googleapis.com/request_latencies'
      | align percentile(95), 1m
      | every 1m
```

#### 4.2.2 Business Metrics

```yaml
business_metrics:
  - name: active_deployments
    type: gauge
    description: Number of active deployments
    source: terraform_state

  - name: user_adoption
    type: counter
    description: New deployments per day
    query: marketplace_deployment_events

  - name: support_tickets
    type: gauge
    description: Open support tickets
    source: support_system
```

### 4.3 Health Check Endpoints

Implement comprehensive health endpoints:

```erlang
%% File: apps/erlmcp_api_management/src/erlmcp_health_handler.erl

-module(erlmcp_health_handler).
-export([init/2, handle_health/2]).

init(Req, State) ->
    {cowboy_rest, Req, State}.

handle_health(Req, State) ->
    Health = get_health_status(),
    Status = case Health of
        #{status := ok} -> 200;
        #{status := degraded} -> 200;  %% Still serving but with issues
        #{status := error} -> 503
    end,
    Req2 = cowboy_req:reply(Status,
        #{<<"content-type">> => <<"application/json">>},
        jiffy:encode(Health),
        Req),
    {ok, Req2, State}.

get_health_status() ->
    #{
        status => get_overall_status(),
        version => get_version(),
        timestamp => iso8601:format(now()),
        uptime => erlang:system_time(second) - get_start_time(),
        checks => #{
            database => check_database(),
            redis => check_redis(),
            cluster => check_cluster(),
            memory => check_memory()
        }
    }.
```

---

## 5. Alert Configuration

### 5.1 Alert Policy Definitions

```yaml
# File: marketplace/gcp/config/alerts.yaml

alert_policies:
  - name: high_error_rate
    display_name: "High Error Rate"
    description: "Error rate exceeds 1%"
    conditions:
      - display_name: "Error rate > 1%"
        condition_threshold:
          filter: >
            resource.type="cloud_run_revision"
            AND metric.type="run.googleapis.com/request_count"
            AND metric.response_code_class >= '400'
          comparison: COMPARISON_GT
          threshold_value: 0.01
          duration: 300s
          aggregations:
            - per_series_aligner: ALIGN_RATE
              alignment_period: 60s
              cross_series_reducer: REDUCE_FRACTION
              group_by_fields:
                - resource.label.project_id
    enabled: true
    combiner: OR
    notification_channels:
      - projects/{project}/notificationChannels/{pagerduty-channel}
      - projects/{project}/notificationChannels/{slack-channel}

  - name: high_latency
    display_name: "High Latency"
    description: "p95 latency exceeds 1 second"
    conditions:
      - display_name: "p95 latency > 1000ms"
        condition_threshold:
          filter: >
            resource.type="cloud_run_revision"
            AND metric.type="run.googleapis.com/request_latencies"
          comparison: COMPARISON_GT
          threshold_value: 1000
          duration: 300s
          aggregations:
            - per_series_aligner: ALIGN_PERCENTILE_95
              alignment_period: 60s
    enabled: true
    combiner: OR
    notification_channels:
      - projects/{project}/notificationChannels/{slack-channel}

  - name: health_check_failure
    display_name: "Health Check Failed"
    description: "Health check endpoint returning errors"
    conditions:
      - display_name: "Health check < 99%"
        condition_threshold:
          filter: >
            resource.type="uptime_url"
            AND metric.type="monitoring.googleapis.com/uptime_check/check_passed"
          comparison: COMPARISON_LT
          threshold_value: 0.99
          duration: 120s
    enabled: true
    combiner: OR
    notification_channels:
      - projects/{project}/notificationChannels/{pagerduty-channel}
      - projects/{project}/notificationChannels/{email-channel}

  - name: memory_high
    display_name: "High Memory Usage"
    description: "Memory usage exceeds 90%"
    conditions:
      - display_name: "Memory > 90%"
        condition_threshold:
          filter: >
            resource.type="cloud_run_revision"
            AND metric.type="run.googleapis.com/container/memory/usage"
          comparison: COMPARISON_GT
          threshold_value: 0.9
          duration: 600s
    enabled: true
    combiner: OR
    notification_channels:
      - projects/{project}/notificationChannels/{slack-channel}
```

### 5.2 Alert Escalation Matrix

| Severity | Response Time | Escalation Path |
|----------|---------------|-----------------|
| P0 - Critical | 15 minutes | On-call -> Engineering Lead -> CTO |
| P1 - High | 1 hour | On-call -> Engineering Manager -> Director |
| P2 - Medium | 4 hours | Team Lead -> Engineering Manager |
| P3 - Low | 1 business day | Engineering Team |

### 5.3 Alert On-Call Rotation

```
Week 1: Engineer A (Primary)    Engineer B (Secondary)
Week 2: Engineer C (Primary)    Engineer D (Secondary)
Week 3: Engineer E (Primary)    Engineer A (Secondary)
Week 4: Engineer B (Primary)    Engineer C (Secondary)
```

**On-Call Responsibilities:**

- Monitor alerts 24/7 during assigned week
- Respond to alerts within SLA
- Document all incidents
- Handoff to next on-call with summary

---

## 6. Incident Response

### 6.1 Incident Lifecycle

```
Detect -> Triage -> Respond -> Resolve -> Learn
   |        |        |        |        |
   v        v        v        v        v
Alert   Assess   Mitigate  Verify   Retro
Issued   Impact   Issue     Fix    Created
```

### 6.2 Incident Severity Classification

| Severity | Name | Definition | Example |
|----------|------|------------|---------|
| P0 | Critical | Complete service outage | Service not responding |
| P1 | High | Major functionality broken | Health check failing |
| P2 | Medium | Partial degradation | Slow response times |
| P3 | Low | Minor issues | Spurious errors |
| P4 | Cosmetic | No functional impact | Typos in UI |

### 6.3 Incident Response Procedure

#### Step 1: Detection

```bash
# Automated detection via alert
# Alert fires: "Error rate > 1% for 5 minutes"

# Send notifications
- PagerDuty: On-call engineer paged
- Slack: #incidents channel notified
- Email: eng-leads@company.com notified
```

#### Step 2: Triage

```markdown
# Incident Triage Checklist

- [ ] Severity assigned (P0-P4)
- [ ] Impact assessed (users affected)
- [ ] Scope identified (which components)
- [ ] Workarounds identified (if any)
- [ ] Communication plan defined

**Incident Commander:** {NAME}
**Scribe:** {NAME}
**Communications Lead:** {NAME}
```

#### Step 3: Response

```markdown
# Incident Response Actions

1. **Acknowledge** alert (stops escalation)
2. **Investigate** root cause
   - Check logs: `gcloud logging read ...`
   - Check metrics: Cloud Monitoring dashboard
   - Check recent changes: Deploy log

3. **Mitigate** issue
   - Rollback if needed
   - Apply hotfix if available
   - Implement workaround

4. **Update** status every 15 minutes
```

#### Step 4: Resolution

```bash
# Verification script after fix
#!/bin/bash
echo "Verifying incident resolution..."

# 1. Check error rate
ERROR_RATE=$(get_error_rate_last_5min)
if [ $ERROR_RATE -lt 0.01 ]; then
  echo "✓ Error rate normal"
else
  echo "✗ Error rate still high"
  exit 1
fi

# 2. Check latency
LATENCY=$(get_p95_latency_last_5min)
if [ $LATENCY -lt 1000 ]; then
  echo "✓ Latency normal"
else
  echo "✗ Latency still high"
  exit 1
fi

# 3. Check health
HEALTH=$(curl -s https://erlmcp.../health | jq -r '.status')
if [ "$HEALTH" = "ok" ]; then
  echo "✓ Health check passing"
else
  echo "✗ Health check failing"
  exit 1
fi

echo "Incident resolved successfully!"
```

#### Step 5: Learning

```markdown
# Post-Incident Review Template

## Incident Summary
- **Date:** {DATE}
- **Duration:** {HOURS} hours
- **Severity:** P{SEVERITY}
- **Impact:** {IMPACT}

## Timeline
| Time | Event |
|------|-------|
| {TIME} | {EVENT} |

## Root Cause
{ROOT_CAUSE}

## Resolution
{RESOLUTION}

## Action Items
1. [ ] {ACTION_ITEM}
2. [ ] {ACTION_ITEM}

## Prevention
{PREVENTION_MEASURES}
```

---

## 7. Reporting

### 7.1 Daily Report Template

```yaml
daily_report:
  title: "erlmcp v3.0.0 Daily Report - Day {N}"
  date: "{DATE}"
  reporting_period: "{START} to {END}"

  executive_summary:
    status: "{HEALTHY/CONCERNED/UNSTABLE}"
    highlights:
      - "{HIGHLIGHT_1}"
      - "{HIGHLIGHT_2}"
    concerns:
      - "{CONCERN_1}"

  key_metrics:
    availability: "{VALUE}%"
    error_rate: "{VALUE}%"
    p95_latency: "{VALUE}ms"
    throughput: "{VALUE}/sec"

  incidents:
    - id: "{INCIDENT_ID}"
      severity: "{SEVERITY}"
      status: "{STATUS}"
      description: "{DESCRIPTION}"

  customer_impact:
    users_affected: "{COUNT}"
    downtime_minutes: "{COUNT}"
    credits_issued: "${AMOUNT}"

  action_items:
    - "{ACTION_1}"
    - "{ACTION_2}"
```

### 7.2 Weekly Report Template

```yaml
weekly_report:
  title: "erlmcp v3.0.0 Weekly Report - Week {N}"
  week_ending: "{DATE}"

  achievement_summary:
    milestone_24h: "{ACHIEVED/IN_PROGRESS/PENDING}"
    milestone_3d: "{ACHIEVED/IN_PROGRESS/PENDING}"
    milestone_7d: "{ACHIEVED/IN_PROGRESS/PENDING}"

  weekly_metrics:
    availability: "{VALUE}%"
    error_rate: "{VALUE}%"
    p95_latency: "{VALUE}ms"
    incident_count: "{COUNT}"

  incident_summary:
    critical: "{COUNT}"
    high: "{COUNT}"
    medium: "{COUNT}"
    low: "{COUNT}"

  trends:
    availability_trend: "{IMPROVING/STABLE/DECLINING}"
    error_rate_trend: "{IMPROVING/STABLE/DECLINING}"
    latency_trend: "{IMPROVING/STABLE/DECLINING}"

  recommendations:
    - "{RECOMMENDATION_1}"
    - "{RECOMMENDATION_2}"
```

### 7.3 Release Closure Report

```markdown
# Release Closure Report - erlmcp v3.0.0

**Release Date:** {DATE}
**Closure Date:** {DATE}
**Status:** {SUCCESSFUL/REQUIRES_FOLLOWUP}

## Release Summary

| Metric | Target | Actual | Status |
|--------|--------|--------|--------|
| Availability | 99.95% | {VALUE}% | {STATUS} |
| Error Rate | < 0.1% | {VALUE}% | {STATUS} |
| Incident Count | 0 | {COUNT} | {STATUS} |

## Key Achievements

- {ACHIEVEMENT_1}
- {ACHIEVEMENT_2}

## Lessons Learned

### What Went Well
- {POSITIVE_1}
- {POSITIVE_2}

### What Could Be Improved
- {IMPROVEMENT_1}
- {IMPROVEMENT_2}

## Action Items

| Item | Owner | Due Date |
|------|-------|----------|
| {ACTION} | {OWNER} | {DATE} |

## Next Release

**Planned Date:** {DATE}
**Key Features:** {FEATURES}

---

**Approved By:** {NAME}, {TITLE}
**Date:** {DATE}
```

---

## Appendices

### A. Monitoring Commands Reference

```bash
# Health Check
curl https://erlmcp-...a.run.app/health

# Error Rate (last hour)
gcloud logging read \
  'resource.type=cloud_run_revision AND labels.service=erlmcp AND severity>=ERROR' \
  --freshness=1h \
  --format=json | jq length

# Latency Percentiles
gcloud logging read \
  'resource.type=cloud_run_revision AND labels.service=erlmcp AND jsonPayload.latency' \
  --freshness=1h \
  --format='value(jsonPayload.latency)' | \
  awk '{a[NR]=$1} END {print "p50:", a[int(NR/2)]; print "p95:", a[int(NR*0.95)]}'

# Request Count
gcloud logging read \
  'resource.type=cloud_run_revision AND labels.service=erlmcp AND httpRequest.requestMethod="GET"' \
  --freshness=1h \
  --format=json | jq length

# Top Errors
gcloud logging read \
  'resource.type=cloud_run_revision AND labels.service=erlmcp AND severity>=ERROR' \
  --freshness=24h \
  --format='value(textPayload)' | \
  sort | uniq -c | sort -rn | head -10
```

### B. Alert Creation Script

```bash
#!/bin/bash
# File: marketplace/gcp/scripts/create-alerts.sh

PROJECT_ID="${PROJECT_ID:-}"
PROJECT_NUMBER="${PROJECT_NUMBER:-}"

if [ -z "$PROJECT_ID" ] || [ -z "$PROJECT_NUMBER" ]; then
  echo "Error: PROJECT_ID and PROJECT_NUMBER must be set"
  exit 1
fi

# Create notification channels (if not exists)
# ... channel creation code ...

# Create alert policies
gcloud alpha monitoring policies create \
  --project="${PROJECT_ID}" \
  --policy-from-file=marketplace/gcp/config/alerts.yaml

echo "Alert policies created successfully"
```

### C. Dashboard Creation Script

```bash
#!/bin/bash
# File: marketplace/gcp/scripts/create-dashboard.sh

PROJECT_ID="${PROJECT_ID:-}"

gcloud monitoring dashboards create \
  --project="${PROJECT_ID}" \
  --config-from-file=marketplace/gcp/config/dashboard.json

echo "Dashboard created: https://console.cloud.google.com/monitoring/dashboards?project=$PROJECT_ID"
```

---

**Document Control**

- **Owner:** Release Manager
- **Review Frequency:** Per release
- **Next Review:** After v3.1.0 release
- **Version History:**
  - 1.0 (2026-02-02): Initial version

---

*For questions or concerns, contact the Release Manager or check the #monitoring Slack channel.*
