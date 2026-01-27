# Operator Quick Reference Guide - erlmcp

**Purpose:** Fast reference for common operational tasks
**Audience:** Operations engineers and on-call responders
**Time to master:** 30 minutes

---

## 🚨 CRITICAL ALERT RESPONSE MATRIX

| Alert | Severity | 1st Action | 2nd Action | Escalate If |
|-------|----------|-----------|-----------|------------|
| **High Error Rate** (>5%) | P2 | Check `/logs/erlmcp.log` last 20 lines | Review recent deployments | >15 min to resolve |
| **Memory > 85%** | P2 | Run `memory_status()` in console | Graceful restart if >95% | Repeats after restart |
| **Memory > 95%** | P1 | Immediate restart required | Check for leaks in logs | Any persistence |
| **CPU > 90%** | P3 | Enable profiling, identify hot spots | Reduce non-critical load | Sustained >30 min |
| **Component Unhealthy** | P3 | Check component logs | Verify dependencies running | Won't restart |
| **Rate Limit Active** | P3 | Check for malicious traffic | Adjust limits if legitimate | Blocks legitimate users |
| **Connection Pool Full** | P2 | Check active connections | Restart pool if stale | New connections failing |

---

## 🔧 QUICK COMMANDS

### Dashboard Access
```bash
# Real-time monitoring
http://localhost:8080/dashboard

# Prometheus metrics
http://localhost:9090

# Grafana (if configured)
http://localhost:3000
```

### Health Status Check
```erlang
% Start Erlang console
rebar3 shell

% Check system health
erlmcp_health_monitor:get_system_health().

% Check specific component
erlmcp_health_monitor:get_component_health(erlmcp_registry).

% Trigger manual health check
erlmcp_health_monitor:trigger_system_health_check().
```

### Log Inspection
```bash
# Last 50 lines
tail -50 logs/erlmcp.log

# Real-time tail
tail -f logs/erlmcp.log

# Search for errors
grep ERROR logs/erlmcp.log | tail -20

# Search by timestamp
grep "2026-01-27T13:" logs/erlmcp.log
```

### Metrics Inspection
```erlang
% In Erlang console
erlmcp_metrics:get_metrics().
erlmcp_metrics:get_performance_summary().
erlmcp_metrics:get_metrics(<<\"transport_operation_duration_ms\">>).
```

### Memory Status
```erlang
% Check memory usage
erlang:memory().

% Check process memory
erlang:process_info(pid(0,250,0), memory).

% Memory statistics
erlang:system_info(memory).
```

---

## 📊 DASHBOARD PANELS (What to Look For)

### Green Lights (All Good)
- System Health: **Healthy** ✓
- Error Rate: **< 1%** ✓
- Memory: **< 60%** ✓
- CPU: **< 30%** ✓
- Response Time P99: **< 500ms** ✓
- All components: **Green** ✓

### Yellow Lights (Investigate)
- System Health: **Degraded** ⚠
- Error Rate: **1-5%** ⚠
- Memory: **60-85%** ⚠
- CPU: **30-60%** ⚠
- Response Time P99: **500ms-2s** ⚠
- 1 component: **Orange** ⚠

### Red Lights (Action Required)
- System Health: **Unhealthy** 🔴
- Error Rate: **> 5%** 🔴
- Memory: **> 85%** 🔴
- CPU: **> 60%** 🔴
- Response Time P99: **> 2s** 🔴
- Any component: **Red** 🔴

---

## 🔄 COMMON REMEDIATION STEPS

### High Error Rate (>5% for 1 minute)

**Step 1: Verify Alert is Real** (< 1 min)
```bash
# Check Prometheus directly
curl http://localhost:9090/api/v1/query?query=rate(http_requests_total{code=~\"5..\"}[5m])

# Check Grafana Error Rate panel
# Navigate to: Dashboard → erlmcp-overview → Error Rate
```

**Step 2: Identify Root Cause** (< 3 min)
```erlang
% Check application errors
grep "ERROR\|CRITICAL" logs/erlmcp.log | tail -20

% Check for recent deployments
% Check for database/external service issues
```

**Step 3: Decide Action** (< 5 min)
- [ ] Recent deployment? → **Rollback**
- [ ] External service down? → **Notify downstream team**
- [ ] Database issue? → **Check connection pool**
- [ ] Rate limiting active? → **Check for abuse**
- [ ] Unexplained? → **Escalate to engineer**

---

### High Memory Usage (>85%)

**Step 1: Identify Consuming Process** (< 2 min)
```erlang
% Check memory by process
erlang:processes() |>
  lists:map(fun(P) ->
    case erlang:process_info(P, memory) of
      {memory, M} -> {P, M};
      undefined -> {P, 0}
    end
  end) |>
  lists:sort(fun({_,A},{_,B}) -> A > B end) |>
  lists:sublist(10).
```

**Step 2: Check for Known Issues** (< 2 min)
- [ ] After recent code changes? → **Memory leak suspected**
- [ ] Steady for hours then spike? → **Normal behavior**
- [ ] Growing continuously? → **Definitely a leak**

**Step 3: Take Action** (< 5 min)
- [ ] Memory > 95%? → **Graceful restart immediately**
- [ ] Memory 85-95%? → **Monitor closely, restart if growing**
- [ ] Suspected leak? → **Enable heap dump, contact engineer**

```bash
# Graceful restart
# 1. Stop accepting new connections
# 2. Wait for in-flight requests to complete
# 3. Send shutdown signal: kill -TERM <pid>
# 4. Supervisor restarts process
```

---

### CPU High (>60% sustained)

**Step 1: Enable CPU Profiling** (< 1 min)
```erlang
% Start CPU profiling (1-minute sample)
erlang:trace(all, true, [call, cpu_time]),
timer:sleep(60000),
erlang:trace(all, false, [call, cpu_time]).

% Alternative: Check process message queues
erlang:processes() |>
  lists:map(fun(P) ->
    case erlang:process_info(P, message_queue_len) of
      {message_queue_len, L} when L > 100 -> P;
      _ -> undefined
    end
  end) |>
  lists:filter(fun(X) -> X =/= undefined end).
```

**Step 2: Identify Hot Spots** (< 3 min)
- Check profiling results for high call counts
- Look for busy-loop patterns
- Check for excessive GC activity

**Step 3: Mitigation** (< 5 min)
- Reduce traffic if possible
- Enable rate limiting if not already active
- Escalate to engineer for profiling analysis

---

### Component Unhealthy

**Step 1: Check Status** (< 1 min)
```erlang
erlmcp_health_monitor:get_all_component_health().
```

**Step 2: Check Component Logs** (< 2 min)
```bash
# Search logs for component errors
grep "component_name\|erlmcp_registry" logs/erlmcp.log | tail -20
```

**Step 3: Recovery** (< 5 min)
```erlang
% Manual restart of supervisor's children
erlmcp_sup:restart_child(erlmcp_registry_sup).

% Or if it's a transport or server:
erlmcp_transport_sup:restart_transport(tcp).
```

---

## 🎯 ESCALATION GUIDE

### When to Escalate to On-Call Engineer

**Escalate IMMEDIATELY (P1):**
- Service completely down (no connections accepting)
- Data loss or corruption detected
- Security incident suspected
- Multiple components failing
- Cannot reach database

**Escalate ASAP (P2):**
- High error rate (>5%) for > 15 minutes
- Memory > 95% and won't stabilize
- CPU sustained > 60% for > 30 minutes
- Circuit breaker active for > 10 minutes
- Connection pool exhausted

**Escalate After Investigation (P3):**
- Persistent but low error rate (< 5%)
- Memory trending high but not critical
- Moderate CPU usage (30-60%)
- Non-critical component degraded
- Rate limiting active but system healthy

### Escalation Contact
```
On-Call Engineer: [PagerDuty or on-call schedule]
Slack Channel: #erlmcp-alerts
Email: oncall@example.com (if phones down)
```

---

## 📋 INCIDENT RESPONSE CHECKLIST

### During Incident

- [ ] **Acknowledge alert** in PagerDuty/Slack (< 2 min)
- [ ] **Document start time** and initial observations
- [ ] **Check system health** dashboard for full picture
- [ ] **Review recent changes** (deployments, config)
- [ ] **Collect baseline metrics** for comparison
- [ ] **Execute appropriate runbook** from alert matrix
- [ ] **Escalate if needed** per escalation guide
- [ ] **Keep stakeholders updated** every 5 minutes

### After Incident (Post-Mortem)

- [ ] **Document resolution steps** taken
- [ ] **Record resolution time** (from alert to resolution)
- [ ] **Identify root cause** (if time permits)
- [ ] **Schedule follow-up** with engineer if needed
- [ ] **Update runbook** if procedure was unclear
- [ ] **Add metrics** if gap in monitoring detected

---

## 🛠️ TROUBLESHOOTING FLOW

```
Is system responding to requests?
├─ NO → Check if processes running
│       └─ Restart via supervisor
├─ YES → Check error rate
    ├─ High (>5%) → Check logs for errors
    │               └─ Database? → Check connections
    │               └─ External? → Check dependencies
    │               └─ Code bug? → Escalate engineer
    └─ Normal → Check resource usage
                ├─ Memory high? → Check for leaks
                ├─ CPU high? → Profile & identify
                └─ Connections high? → Rate limit active?
```

---

## 📞 WHO TO CONTACT

| Issue | Contact | Channel | Time |
|-------|---------|---------|------|
| System down | On-call engineer | PagerDuty + Slack | Immediate |
| High error rate | Level-2 engineer | Slack + phone | 5 min |
| Memory leak | Platform team | Slack | 15 min |
| Configuration | DevOps team | Slack | 30 min |
| Questions | Tech lead | Slack | During business hours |

---

## 🎓 TRAINING CHECKLIST

**Before handling first alert:**
- [ ] Understand system architecture (5 min read)
- [ ] Navigate dashboard (5 min hands-on)
- [ ] Recognize alert severity levels (5 min)
- [ ] Execute one simulated alert response (10 min)
- [ ] Know escalation procedures (5 min)

**Before on-call shift:**
- [ ] Review last 3 incidents
- [ ] Test alert channels (Slack, email, phone)
- [ ] Confirm escalation contact is reachable
- [ ] Review any recent deployments or changes
- [ ] Have runbooks accessible (printed or device)

---

## ⚡ POWER USER TIPS

**Faster diagnosis:**
```erlang
% One-liner health check
[{C, erlmcp_health_monitor:get_component_health(C)} || C <- [erlmcp_registry, erlmcp_server_sup, erlmcp_client_sup, erlmcp_transport_sup]].

% Memory per component
[{C, erlang:process_info(pid_of(C), memory)} || C <- erlang:processes(), erlang:process_info(C, registered_name) =/= undefined].

% Connection count
length([P || P <- erlang:processes(), erlang:process_info(P, initial_call) =:= {erlmcp_client, init, 1}]).
```

**Quick metrics export:**
```bash
curl http://localhost:9090/api/v1/query?query=up > metrics.json
```

**Faster log search:**
```bash
# Color-coded errors
grep --color=always -E "ERROR|CRITICAL" logs/erlmcp.log | tail -30

# Timeline view
grep "2026-01-27" logs/erlmcp.log | grep -oP '\[\K[^]]*' | sort | uniq -c
```

---

## 🔐 SECURITY NOTES

**Do NOT:**
- Share dashboards with external parties
- Leave logs with sensitive data in public
- Run untrusted code in Erlang console
- Change rate limits without verification
- Disable TLS verification for testing

**Always:**
- Verify changes before applying to production
- Use environment variables for credentials
- Log all access to system console
- Rotate access credentials monthly
- Review audit logs weekly

---

**Last Updated:** January 27, 2026
**Version:** 1.0
**Next Review:** April 27, 2026
