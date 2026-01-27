# TCPS Operations Cheat Sheet

**Version:** 1.0.0 | **Last Updated:** 2026-01-26

> **Print this page and keep it at your workstation for quick reference!**

---

## Daily Operations (5-Minute Morning Check)

```bash
# 1. System health
./tools/tcps tpm health

# 2. Open Andons (target: 0-2)
./tools/tcps andon list --status=open

# 3. Quality gates (target: >95%)
./tools/tcps quality metrics --period=daily

# 4. Kanban WIP status
./tools/tcps kanban status

# 5. Lead time (target: <2h P90)
./tools/tcps kaizen report --daily

# DASHBOARD
open http://localhost:8080
```

**Action Thresholds:**
- ⚠️ Open Andons >5 → Emergency response (page on-call)
- ⚠️ Pass rate <90% → Review failures immediately
- ⚠️ Lead time >4h → Investigate bottlenecks
- ⚠️ WIP at limit → Check for stuck work orders

---

## Most Used Commands

### Work Orders

```bash
# Create
tcps work-order create --bucket=security --priority=10 --title="Fix CVE"

# List pending
tcps work-order list --status=pending

# Process next (auto-select via Heijunka)
tcps work-order process-next

# View status
tcps work-order show WO-123456

# Complete
tcps work-order complete WO-123456 sku_789
```

### Andon (Stop-the-Line)

```bash
# List open
tcps andon list --status=open

# Details
tcps andon show ANDON-001

# Start root cause (5 Whys)
tcps root-cause start ANDON-001

# Add why answers
tcps root-cause add-why analysis_001 1 "Test failed"
tcps root-cause add-why analysis_001 2 "No error handling"
# ... (continue to Why 5)

# Finalize
tcps root-cause finalize analysis_001 \
  --root-cause="Missing validation" \
  --prevention="Add SHACL constraint"

# Resolve
tcps andon resolve ANDON-001 --receipt-path=receipt-xxx.json
```

### Quality & Receipts

```bash
# Quality gates
tcps quality gates sku_123

# Receipt chain
tcps receipt verify-chain sku_123

# Deterministic build check
tcps receipt verify-deterministic sku_123

# Metrics
tcps quality metrics --period=weekly --compare
```

---

## Emergency Procedures

### 🚨 Mass Andon Resolution (>10 Open)

```bash
# 1. Find pattern
tcps root-cause analyze-patterns

# 2. Create emergency resolution
tcps root-cause start EMERGENCY-001 --problem="<description>"

# 3. Perform 5 Whys (once)
tcps root-cause add-why EMERGENCY-001 1-5 "<answers>"
tcps root-cause finalize EMERGENCY-001 --root-cause="..." --prevention="..."

# 4. Resolve all Andons
for andon in $(tcps andon list --status=open --format=ids); do
  tcps andon resolve $andon --receipt-path=receipt-EMERGENCY-001.json
done
```

### 🚨 Pipeline Stuck (No Progress)

```bash
# Diagnosis checklist
tcps andon list --status=open        # Open Andons blocking?
tcps kanban status                    # WIP limit reached?
tcps work-order list --status=in_progress  # Stuck work orders?
tcps tpm health                       # System health?

# If WIP limit issue (emergency only)
tcps kanban set-limit <bucket> <new-limit>

# If dependency deadlock
tcps work-order cancel <wo-id> --reason="Deadlock resolution"
```

### 🚨 Dashboard Not Updating

```bash
# Check process
ps aux | grep tcps_dashboard

# Test SSE
curl -N http://localhost:8080/api/stream

# Restart
./scripts/start_dashboard.sh restart

# Or full restart
make dev-console
# Then: application:start(erlmcp)
```

---

## Key Metrics & Targets

| Metric | Target | Alert | Action |
|--------|--------|-------|--------|
| **Quality Gate Pass Rate** | >95% | <90% | Review failures |
| **Lead Time (P90)** | <2h | >4h | Find bottlenecks |
| **Andon Resolution** | <4h avg | >8h | Escalate |
| **Throughput** | >100/day | <50/day | Check pipeline |
| **Open Andons** | 0-2 | >5 | Emergency |
| **WIP Utilization** | 60-80% | >95% | Complete work |

---

## Troubleshooting Decision Tree

```
Pipeline not progressing?
├─ Open Andons? YES → Resolve Andons first
│                NO  → Continue
├─ WIP at limit? YES → Check stuck work orders
│                NO  → Continue
├─ Dependencies? YES → Check for deadlocks
│                NO  → Continue
└─ System health? NO  → Check logs, restart
                  YES → Contact support
```

---

## Common Errors & Fixes

| Error | Cause | Fix |
|-------|-------|-----|
| `{error, wip_limit}` | WIP full | Complete work orders or adjust limit |
| `{blocked, [andon-001]}` | Andon blocking | Resolve Andon |
| `{error, circular_dependency}` | Dependency cycle | Remove cycle |
| `Connection refused (8080)` | Dashboard down | Start dashboard |
| `{error, not_found}` | Work order missing | Sync from GitHub |

---

## Maintenance Schedule

| When | Command | What It Does |
|------|---------|--------------|
| **Daily 2 AM** | `tcps tpm maintenance --daily` | Cleanup receipts, vacuum ETS, check disk |
| **Sunday 2 AM** | `tcps tpm maintenance --weekly` | Backup, rebuild PLT, coverage report |
| **1st of Month** | `tcps tpm maintenance --monthly` | Security audit, benchmarks, capacity plan |

---

## Backup & Restore

```bash
# Backup
tcps tpm backup --output=/backups/tcps/tcps-$(date +%Y%m%d).tar.gz

# Restore
tcps tpm restore --from=/backups/tcps/tcps-20260125.tar.gz

# Verify
tcps tpm verify-backup /backups/tcps/tcps-20260125.tar.gz
```

---

## Performance Quick Wins

```erlang
% config/sys.config

% High load? Increase pool size
{poolboy, [{http_pool, [{size, 50}, {max_overflow, 20}]}]}.

% Slow queries? Increase cache
{tcps, [{sparql_cache_ttl, 120}, {sparql_cache_size, 512}]}.

% Need more throughput? Increase WIP limits
{tcps, [{wip_limits, #{security => 10, features => 20}}]}.
```

---

## Dashboard Quick Guide

**URL:** http://localhost:8080

**What to Watch:**
- ✓ **Overview** - Total work orders, open Andons, pass rate
- ✓ **Quality Gates** - All gates >95% pass rate
- ✓ **Kanban** - WIP 60-80% of limit (not full, not empty)
- ✓ **Andon Alerts** - Should be empty or <3 events
- ✓ **Kaizen** - Week-over-week improvements trending up

**Auto-refresh:** Every 5 seconds via Server-Sent Events

---

## Log Locations

```bash
# Application
_build/default/rel/erlmcp/log/erlang.log.*

# TCPS specific
priv/tcps/logs/andon_events.log
priv/tcps/logs/quality_gates.log
priv/tcps/logs/work_orders.log

# Receipts
priv/receipts/*.json

# Ontology
ontology/receipts.ttl
```

---

## Useful Aliases

Add to `~/.bashrc`:

```bash
alias tcps-health='./tools/tcps tpm health'
alias tcps-andons='./tools/tcps andon list --status=open'
alias tcps-quality='./tools/tcps quality metrics --period=daily'
alias tcps-kaizen='./tools/tcps kaizen report --daily'
alias tcps-morning='tcps-health && tcps-andons && tcps-quality'
alias tcps-dash='open http://localhost:8080'
```

---

## Contact & Escalation

| Issue | Contact | When |
|-------|---------|------|
| **Questions** | Team Chat | Normal hours |
| **Open Andons >5** | Team Lead | Immediate |
| **System Down** | On-Call | Emergency |
| **Data Loss** | Page On-Call | Critical |

📞 **On-Call:** +1-555-TCPS-OPS (555-8277-677)
✉️ **Email:** tcps-ops@company.com
💬 **Chat:** #tcps-ops in Slack

---

## Quick Links

- 📘 **Full Runbook:** [OPERATIONS_RUNBOOK.md](OPERATIONS_RUNBOOK.md)
- 🏗️ **Architecture:** [ARCHITECTURE_OVERVIEW.md](ARCHITECTURE_OVERVIEW.md)
- 🔧 **Troubleshooting:** [TROUBLESHOOTING.md](TROUBLESHOOTING.md)
- 📊 **Dashboard:** http://localhost:8080
- 🐛 **GitHub Issues:** https://github.com/banyan-platform/erlmcp/issues

---

## Remember: The 4 Pillars of TCPS

1. **Andon** - Stop the line when quality issues detected
2. **Kanban** - Respect WIP limits, pull don't push
3. **Kaizen** - Continuous improvement (5% per week target)
4. **Receipts** - Immutable audit trail for everything

---

**📄 Version:** 1.0.0
**📅 Last Updated:** 2026-01-26
**👤 Maintained By:** TCPS Operations Team

**Next Review:** 2026-02-26
