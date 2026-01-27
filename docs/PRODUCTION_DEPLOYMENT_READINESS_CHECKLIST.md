# Production Deployment Readiness Checklist - erlmcp

**Purpose:** Ensure all systems ready before production deployment
**Duration:** 2-4 hours per deployment
**Audience:** DevOps, Operations, Release Engineer
**Status:** Use this for EVERY production deployment

---

## PHASE 1: PRE-DEPLOYMENT (4 hours before)

### 1.1 Code & Build Verification

- [ ] **Code Review Complete**
  - [ ] All changes reviewed and approved
  - [ ] No commented-out code
  - [ ] No hardcoded secrets or credentials
  - [ ] No debug log statements
  - Command: `grep -r "TODO\|FIXME\|DEBUG\|secret\|password" src/ --include="*.erl"`

- [ ] **Build Passes All Quality Gates**
  ```bash
  make clean
  make check         # Compiles + xref + dialyzer + tests
  ```
  - [ ] Compilation successful
  - [ ] No xref warnings
  - [ ] No dialyzer warnings
  - [ ] EUnit tests pass (100%)
  - [ ] Common Test tests pass (100%)
  - [ ] Test coverage >= 80%

- [ ] **Dependencies Current & Secure**
  ```bash
  rebar3 tree
  rebar3 outdated
  ```
  - [ ] All critical dependencies up to date
  - [ ] No known CVEs in dependencies
  - [ ] License compliance verified
  - [ ] Lock file committed to version control

- [ ] **Docker Image Built & Scanned** (if using containers)
  ```bash
  docker build -t erlmcp:1.2.3 .
  docker scan erlmcp:1.2.3
  ```
  - [ ] Image builds without warnings
  - [ ] Security scan passes (0 critical vulnerabilities)
  - [ ] Image size reasonable (< 500MB)
  - [ ] Base image is LTS version

### 1.2 Configuration Verification

- [ ] **sys.config Production-Ready**
  - [ ] `log_level` set to `info` (not `debug`)
  - [ ] `debug_mode` set to `false`
  - [ ] `trace_sample_rate` set to appropriate value (0.1 for 10% sampling in prod)
  - [ ] All secrets loaded from environment variables (not hardcoded)
  - [ ] TLS certificates point to valid files
  - [ ] HTTPS enforcement enabled (`require_https => true`)
  - [ ] OAuth credentials configured via env vars

- [ ] **Alert Thresholds Appropriate**
  - [ ] Error rate alert threshold appropriate for workload
  - [ ] Memory thresholds match server capacity
  - [ ] CPU thresholds match workload patterns
  - [ ] Lead time thresholds realistic
  - [ ] Verified thresholds in staging under load

- [ ] **Alert Channels Operational**
  - [ ] Slack webhook tested (send test message)
  - [ ] Email SMTP credentials verified
  - [ ] PagerDuty integration key valid
  - [ ] Test alert successful through each channel
  - [ ] On-call rotation configured

- [ ] **Monitoring Enabled**
  - [ ] Prometheus scrape endpoints reachable
  - [ ] Grafana datasources connected
  - [ ] Dashboard JSON imported without errors
  - [ ] Health monitor endpoints responsive
  - [ ] Log rotation configured

- [ ] **Rate Limiting Configured**
  - [ ] Limits appropriate for expected traffic
  - [ ] DDoS thresholds set for workload
  - [ ] Rate limit alerts enabled
  - [ ] Tested under load conditions

- [ ] **Transport Configuration Verified**
  - [ ] TCP port binding correct
  - [ ] HTTP port binding correct
  - [ ] WebSocket path configured
  - [ ] SSE endpoints available
  - [ ] Keep-alive intervals appropriate

### 1.3 Security Hardening

- [ ] **TLS Configuration**
  - [ ] Certificates valid and not expiring soon (> 30 days)
  - [ ] Certificate chain complete (if applicable)
  - [ ] TLS version >= 1.2 enforced
  - [ ] Cipher suites modern and secure
  - [ ] Certificate pinning (if applicable) configured
  - [ ] HSTS header enabled
  - [ ] Hostname verification enabled
  - [ ] Command: `openssl s_client -connect localhost:443 -tls1_2`

- [ ] **Authentication & Authorization**
  - [ ] OAuth 2.0 client ID/secret from environment vars
  - [ ] Resource indicator configured
  - [ ] Token cache TTL appropriate
  - [ ] Origin validation configured
  - [ ] Session timeout appropriate
  - [ ] No hardcoded credentials in code/config

- [ ] **Network Security**
  - [ ] Localhost binding enforced (if not behind proxy)
  - [ ] Firewall rules whitelist only needed ports
  - [ ] Allowed origins restricted to known domains
  - [ ] DDoS protection enabled
  - [ ] Rate limiting enabled
  - [ ] Connection limits configured

- [ ] **Data Security**
  - [ ] Sensitive logs redacted
  - [ ] Session IDs generated securely
  - [ ] No debug information in error responses
  - [ ] CORS headers restrictive
  - [ ] Input validation enforced
  - [ ] Output encoding enabled

### 1.4 Infrastructure Readiness

- [ ] **Compute Resources Provisioned**
  - [ ] CPU cores allocated (recommend 2+ for production)
  - [ ] Memory allocated (recommend 2GB minimum)
  - [ ] Disk space for logs (recommend 50GB minimum)
  - [ ] Network bandwidth sufficient
  - [ ] Load balancer configured (if applicable)

- [ ] **Storage & Persistence**
  - [ ] Log directory exists and writable
  - [ ] Backup schedule configured
  - [ ] Disaster recovery tested
  - [ ] Disk monitoring enabled
  - [ ] Retention policy defined

- [ ] **Networking**
  - [ ] DNS records pointing to correct IP
  - [ ] Load balancer health checks configured
  - [ ] Network latency acceptable (< 50ms)
  - [ ] VPN/firewall rules configured
  - [ ] Reverse proxy (if used) configured

- [ ] **Monitoring Stack**
  - [ ] Prometheus server running and scraping
  - [ ] Grafana dashboard accessible
  - [ ] Alert manager configured
  - [ ] Log aggregation (ELK/Splunk) running
  - [ ] Historical metrics storage working

---

## PHASE 2: OPERATOR READINESS (4-6 hours before)

### 2.1 Team Preparation

- [ ] **Primary Operator Ready**
  - [ ] On-call engineer identified and available
  - [ ] Phone number and email confirmed
  - [ ] PagerDuty profile configured
  - [ ] Slack notifications enabled
  - [ ] Has access to all necessary systems

- [ ] **Backup Operator Ready**
  - [ ] Backup engineer identified and standby
  - [ ] Can take primary role if needed
  - [ ] Familiar with system architecture
  - [ ] Has runbooks accessible

- [ ] **Escalation Path Confirmed**
  - [ ] Tech lead available for 4 hours post-deployment
  - [ ] Manager on-call for P1 escalation
  - [ ] External team contact info (if applicable)
  - [ ] Communication channels tested

### 2.2 Documentation Review

- [ ] **Runbooks Current & Accessible**
  - [ ] OPERATIONAL_RUNBOOKS.md reviewed
  - [ ] OPERATOR_QUICK_REFERENCE.md printed or accessible
  - [ ] Procedure for each alert type understood
  - [ ] Rollback procedure clear and practiced
  - [ ] Escalation numbers posted

- [ ] **Deployment Procedure Clear**
  - [ ] Step-by-step deployment documented
  - [ ] Rollback procedure written and tested
  - [ ] Pre-flight checks listed
  - [ ] Communication templates prepared
  - [ ] Expected metrics after deployment defined

- [ ] **Change Log Prepared**
  - [ ] What's new in this release documented
  - [ ] Breaking changes identified
  - [ ] Migration steps (if any) documented
  - [ ] New alerts/metrics explained
  - [ ] Performance impact assessed

### 2.3 Training Completion

- [ ] **Tier 1 Training Complete** (per operator)
  - [ ] System architecture understood
  - [ ] Dashboard navigation practiced
  - [ ] Alert severity levels known
  - [ ] Know where to find help
  - [ ] Can acknowledge alerts in PagerDuty

- [ ] **Tier 2 Training Complete** (primary operator)
  - [ ] Can diagnose high error rate independently
  - [ ] Can handle high memory/CPU scenarios
  - [ ] Can perform graceful restart
  - [ ] Knows when to escalate
  - [ ] Can write incident summary

- [ ] **Dry-Run Scenarios Completed**
  - [ ] Simulated high error rate alert
  - [ ] Simulated memory spike
  - [ ] Simulated component failure
  - [ ] Simulated deployment rollback
  - [ ] Response time recorded and acceptable

### 2.4 Communication Readiness

- [ ] **Stakeholder Notification Ready**
  - [ ] Deployment announcement drafted
  - [ ] Maintenance window communicated (if needed)
  - [ ] Expected impact documented
  - [ ] Rollback impact explained
  - [ ] Contact info for issues provided

- [ ] **Communication Channels Tested**
  - [ ] Slack channel for alerts accessible
  - [ ] Email distribution list working
  - [ ] PagerDuty escalation tested
  - [ ] War room setup (if applicable)
  - [ ] Status page updated (if applicable)

---

## PHASE 3: FINAL VALIDATION (1 hour before)

### 3.1 Staging Environment Final Check

- [ ] **Smoke Tests in Staging**
  ```bash
  # Test basic connectivity
  curl http://staging.erlmcp.local/health

  # Test metrics endpoint
  curl http://staging.erlmcp.local:9090/api/v1/query?query=up

  # Test alert firing
  # Simulate high error rate and verify alert

  # Test log aggregation
  # Verify logs appear in ELK/Splunk
  ```
  - [ ] HTTP health checks pass
  - [ ] Metrics endpoint accessible
  - [ ] Alerts fire and route correctly
  - [ ] Logs aggregate properly
  - [ ] Dashboard updates in real-time

- [ ] **Load Test Results Acceptable**
  - [ ] Test at 100% of expected peak load
  - [ ] Response times P99 < acceptable threshold
  - [ ] Error rate < 1%
  - [ ] No memory leaks detected
  - [ ] Connection limits not exceeded
  - [ ] CPU usage reasonable

- [ ] **Regression Test Suite Passes**
  - [ ] All critical user journeys tested
  - [ ] API backwards compatibility verified
  - [ ] No performance regressions > 10%
  - [ ] Security tests pass
  - [ ] Integration with external services works

### 3.2 Production Environment Pre-Checks

- [ ] **Infrastructure Verification**
  - [ ] All production servers healthy
  - [ ] Database connections working
  - [ ] External service dependencies up
  - [ ] Network connectivity verified
  - [ ] Backup systems operational

- [ ] **Monitoring Baseline Established**
  - [ ] Current error rate recorded
  - [ ] Current latency (p50, p95, p99) recorded
  - [ ] Current memory usage recorded
  - [ ] Current connection count recorded
  - [ ] Baseline stored for comparison

- [ ] **Rollback Plan Tested**
  - [ ] Previous version accessible
  - [ ] Rollback procedure executed in staging
  - [ ] Rollback time measured (should be < 5 min)
  - [ ] Data consistency verified post-rollback
  - [ ] Communication templates ready

### 3.3 Final Safety Checks

- [ ] **All Required Sign-Offs**
  - [ ] Tech lead approval obtained
  - [ ] Security approval obtained (if applicable)
  - [ ] Operations manager approval obtained
  - [ ] Product manager approval obtained (if applicable)
  - [ ] All sign-offs documented

- [ ] **Deployment Window Appropriate**
  - [ ] Scheduled for low-traffic time
  - [ ] Operations team fully available
  - [ ] Adequate time window (recommend 2+ hours)
  - [ ] No critical business events
  - [ ] Emergency contacts available

- [ ] **Fallback Procedures Ready**
  - [ ] Rollback runbook accessible
  - [ ] Previous version tested and available
  - [ ] Rollback communication drafted
  - [ ] Emergency contacts on standby
  - [ ] War room established

---

## PHASE 4: DEPLOYMENT EXECUTION

### 4.1 Pre-Deployment Announcement

```
[Slack announcement 30 minutes before]

🚀 Deployment starting in 30 minutes
Service: erlmcp
Version: 1.2.3
Expected downtime: ~2 minutes
Estimated duration: ~15 minutes total

If you experience issues, contact: [on-call number]

📊 Monitor status: http://grafana.local/d/erlmcp-overview
```

### 4.2 Deployment Steps

**1. Backup Current System** (2 min)
- [ ] Create snapshot of running system
- [ ] Verify rollback package ready
- [ ] Document pre-deployment metrics

**2. Drain Connections** (3 min)
- [ ] Send graceful shutdown signal
- [ ] Wait for in-flight requests to complete
- [ ] Monitor connection count until near zero

**3. Deploy New Version** (5 min)
- [ ] Copy new binary/code to production
- [ ] Update configuration if needed
- [ ] Start new process/service

**4. Verify Startup** (3 min)
- [ ] Check process status (running)
- [ ] Verify health endpoints responding
- [ ] Check for startup errors in logs
- [ ] Confirm all components healthy

**5. Smoke Test** (5 min)
```bash
# Basic connectivity
curl http://localhost/health
curl http://localhost/metrics

# Verify dashboard
curl http://localhost/dashboard

# Check alert routing
# Send test alert to PagerDuty/Slack

# Monitor error rate
# Should be < 1% and stable
```

**6. Monitor for 15 Minutes** (15 min)
- [ ] Watch error rate (should be < 1%)
- [ ] Watch response times (should match baseline)
- [ ] Watch memory usage (should be stable)
- [ ] Watch CPU usage (should be normal)
- [ ] Check no unexpected alerts firing

### 4.3 Rollback Procedure (If Needed)

**STOP! Execute rollback if:**
- Error rate > 10% sustained
- Response times > 5s sustained
- Component failures
- Data corruption detected
- Security incident

**Rollback Steps:**
1. [ ] Stop new version
2. [ ] Restore previous version from backup
3. [ ] Restart application
4. [ ] Verify health (< 5 minutes total)
5. [ ] Announce status to stakeholders

```
🚨 ROLLBACK IN PROGRESS

Service: erlmcp
Reason: [reason]
Expected completion: [time]
Status updates: Every 2 minutes
```

---

## PHASE 5: POST-DEPLOYMENT (2 hours after)

### 5.1 Monitoring & Validation

- [ ] **Metrics Stable**
  - [ ] Error rate stable and normal (< 1%)
  - [ ] Response times normal
  - [ ] Memory usage normal
  - [ ] CPU usage normal
  - [ ] Connection count normal

- [ ] **No Unexpected Alerts**
  - [ ] Only expected alerts (if any)
  - [ ] No false positives
  - [ ] Alert routing working
  - [ ] Alert acknowledgment working
  - [ ] Escalation tested if needed

- [ ] **User Feedback Monitoring**
  - [ ] Support team reports no new issues
  - [ ] Monitoring dashboard clean
  - [ ] Error logs clean
  - [ ] Application behaving as expected
  - [ ] Performance acceptable

### 5.2 Detailed Validation

- [ ] **Full Regression Test Suite Passes**
  - [ ] Run full test suite: `make test`
  - [ ] All unit tests pass
  - [ ] All integration tests pass
  - [ ] All property-based tests pass

- [ ] **Critical User Journeys Tested**
  - [ ] Create resource → works
  - [ ] List resources → works
  - [ ] Subscribe to changes → works
  - [ ] Tool execution → works
  - [ ] Authentication → works

- [ ] **External Integrations Verified**
  - [ ] Database connectivity stable
  - [ ] API dependencies working
  - [ ] Cache layers responding
  - [ ] External services accessible
  - [ ] No integration errors in logs

### 5.3 Documentation Update

- [ ] **Deployment Log Created**
  - [ ] What was deployed
  - [ ] When deployment occurred
  - [ ] Who performed deployment
  - [ ] Any issues encountered
  - [ ] How issues were resolved
  - [ ] Metrics before/after

- [ ] **CHANGELOG Updated**
  - [ ] Add new features
  - [ ] Document bug fixes
  - [ ] Note breaking changes
  - [ ] Update version number
  - [ ] Credit contributors

- [ ] **Known Issues Updated**
  - [ ] Remove resolved issues
  - [ ] Add any new discovered issues
  - [ ] Document workarounds
  - [ ] Estimate fix timeline

### 5.4 Handoff to Operations

- [ ] **Operations Team Briefing**
  - [ ] What changed in this release
  - [ ] New alerts to watch for
  - [ ] New operational procedures (if any)
  - [ ] Known issues and workarounds
  - [ ] Where to find documentation

- [ ] **Incident Response Readiness**
  - [ ] On-call engineer briefed
  - [ ] Escalation procedures confirmed
  - [ ] Contact information updated
  - [ ] PagerDuty rotation updated
  - [ ] Status page URL known

- [ ] **Documentation Accessible**
  - [ ] OPERATIONAL_READINESS_ASSESSMENT.md accessible
  - [ ] OPERATOR_QUICK_REFERENCE.md accessible
  - [ ] OPERATIONAL_RUNBOOKS.md accessible
  - [ ] Deployment changes documented
  - [ ] All links working

---

## PHASE 6: ONGOING MONITORING (First 24 hours)

### 6.1 First 4 Hours (Critical Period)

- [ ] **Continuous Monitoring**
  - [ ] Someone from team online and alert
  - [ ] Dashboard monitored constantly
  - [ ] Response time < 1 minute to alerts
  - [ ] Issues escalated immediately
  - [ ] Status updates every 30 minutes

- [ ] **Metrics to Watch**
  - [ ] Error rate (should stay < 1%)
  - [ ] Response time P99 (should match baseline)
  - [ ] Memory growth (should be steady or flat)
  - [ ] CPU usage (should be normal)
  - [ ] Connection count (should match expected)

### 6.2 First 24 Hours

- [ ] **Extended Monitoring**
  - [ ] Team available during business hours
  - [ ] Escalation path active
  - [ ] Alerts monitored continuously
  - [ ] Logs reviewed for anomalies
  - [ ] Stakeholders updated regularly

- [ ] **Early Warning Signs to Watch**
  - [ ] Gradual error rate increase (leak)
  - [ ] Memory creeping up (leak indicator)
  - [ ] Response time degradation (load issue)
  - [ ] Repeated same errors (bug)
  - [ ] Spike in specific error type (new bug)

### 6.3 First Week

- [ ] **Stability Verified**
  - [ ] No critical issues reported
  - [ ] Error rate stable
  - [ ] Performance stable
  - [ ] Resource usage stable
  - [ ] System ready for normal operations

- [ ] **Feedback Collected**
  - [ ] Operator feedback on new features
  - [ ] Issues reported by users
  - [ ] Performance observations
  - [ ] Areas for improvement
  - [ ] Training needs identified

---

## EMERGENCY CONTACTS

```
Primary On-Call Engineer:
  Name: ________________
  Phone: ________________
  Slack: ________________
  Email: ________________

Backup Engineer:
  Name: ________________
  Phone: ________________
  Slack: ________________
  Email: ________________

Tech Lead:
  Name: ________________
  Phone: ________________
  Email: ________________

Manager (Escalation):
  Name: ________________
  Phone: ________________
  Email: ________________
```

---

## DEPLOYMENT SIGN-OFF

**By checking these boxes, you confirm:**
- All checks have been completed
- System is ready for production
- Team is prepared for support
- Rollback plan is tested and ready
- Emergency contacts are available

- [ ] **Dev/Build Lead:** _________________ Date: _______
- [ ] **Operations Lead:** _________________ Date: _______
- [ ] **Security Officer:** _________________ Date: _______
- [ ] **Release Manager:** _________________ Date: _______

---

## POST-DEPLOYMENT REVIEW (Next day)

**Deployment successful: YES / NO / PARTIAL**

**Issues Encountered:**
1. ________________
2. ________________
3. ________________

**Lessons Learned:**
1. ________________
2. ________________
3. ________________

**Improvements for Next Deployment:**
1. ________________
2. ________________
3. ________________

**Signed off by:** _________________ Date: _______

---

**Checklist Version:** 1.0
**Last Updated:** January 27, 2026
**Next Review:** April 27, 2026

This checklist should be reviewed and updated after every deployment based on lessons learned.
