# TCPS Operations Documentation Index

**Version:** 1.0.0 | **Last Updated:** 2026-01-26

---

## 📚 Documentation Overview

This is the complete documentation suite for **Toyota Code Production System (TCPS)** operations. Whether you're a new operator, experienced troubleshooter, or system architect, you'll find comprehensive guidance here.

---

## 🚀 Quick Start

**New to TCPS?** Start here:

1. **[NEW_OPERATOR_ONBOARDING.md](training/NEW_OPERATOR_ONBOARDING.md)** (2-3 days)
   - Day 1: TCPS Fundamentals
   - Day 2: Daily Operations
   - Day 3: Troubleshooting & Certification

2. **[TCPS_OPS_CHEATSHEET.md](TCPS_OPS_CHEATSHEET.md)** (Print and keep at desk!)
   - Daily operations commands
   - Emergency procedures
   - Key metrics and thresholds
   - Troubleshooting decision trees

3. **[OPERATIONS_RUNBOOK.md](OPERATIONS_RUNBOOK.md)** (Complete reference)
   - Comprehensive 200+ page operations guide
   - All procedures, commands, troubleshooting
   - Read sections as needed

---

## 📖 Core Documentation

### Operations Manuals

| Document | Purpose | Audience | When to Use |
|----------|---------|----------|-------------|
| **[OPERATIONS_RUNBOOK.md](OPERATIONS_RUNBOOK.md)** | Complete operations guide (200+ pages) | All operators | Daily reference, detailed procedures |
| **[TCPS_OPS_CHEATSHEET.md](TCPS_OPS_CHEATSHEET.md)** | Quick reference (1-page) | All operators | Daily operations, emergencies |
| **[TCPS_QUICK_REFERENCE.md](TCPS_QUICK_REFERENCE.md)** | Rebar3 integration commands | Developers | Build-time TCPS operations |

### Training Materials

| Document | Duration | Prerequisites | Purpose |
|----------|----------|---------------|---------|
| **[NEW_OPERATOR_ONBOARDING.md](training/NEW_OPERATOR_ONBOARDING.md)** | 2-3 days | Basic Unix | New operator training, certification |
| **[ADVANCED_TROUBLESHOOTING.md](training/ADVANCED_TROUBLESHOOTING.md)** | 1 day | TCPS Certified Operator | Advanced diagnostics, complex scenarios |

---

## 🎯 Documentation by Role

### For New Operators

**Day 1-7:**
1. Read: [NEW_OPERATOR_ONBOARDING.md](training/NEW_OPERATOR_ONBOARDING.md) - Complete 3-day course
2. Print: [TCPS_OPS_CHEATSHEET.md](TCPS_OPS_CHEATSHEET.md) - Keep at desk
3. Bookmark: [OPERATIONS_RUNBOOK.md](OPERATIONS_RUNBOOK.md) - Reference as needed

**Daily Use:**
- **Morning:** [TCPS_OPS_CHEATSHEET.md](TCPS_OPS_CHEATSHEET.md) - 5-minute health check
- **Issues:** [OPERATIONS_RUNBOOK.md](OPERATIONS_RUNBOOK.md#4-troubleshooting) - Troubleshooting section
- **Questions:** [OPERATIONS_RUNBOOK.md](OPERATIONS_RUNBOOK.md#9-appendices) - FAQ and glossary

### For Experienced Operators

**Daily Use:**
- **Quick Reference:** [TCPS_OPS_CHEATSHEET.md](TCPS_OPS_CHEATSHEET.md)
- **Complex Issues:** [OPERATIONS_RUNBOOK.md](OPERATIONS_RUNBOOK.md#4-troubleshooting)
- **Performance:** [OPERATIONS_RUNBOOK.md](OPERATIONS_RUNBOOK.md#7-performance-tuning)

**Advanced Learning:**
- [ADVANCED_TROUBLESHOOTING.md](training/ADVANCED_TROUBLESHOOTING.md) - Master-level course

### For On-Call Engineers

**Keep Accessible:**
- [TCPS_OPS_CHEATSHEET.md](TCPS_OPS_CHEATSHEET.md) - Emergency procedures
- [OPERATIONS_RUNBOOK.md](OPERATIONS_RUNBOOK.md#42-emergency-procedures) - Emergency procedures
- [OPERATIONS_RUNBOOK.md](OPERATIONS_RUNBOOK.md#6-disaster-recovery) - Disaster recovery

**Escalation:**
- [OPERATIONS_RUNBOOK.md](OPERATIONS_RUNBOOK.md#appendix-d-alert-playbooks) - Alert response playbooks

### For Developers

**Build Integration:**
- [TCPS_QUICK_REFERENCE.md](TCPS_QUICK_REFERENCE.md) - Rebar3 commands
- [OPERATIONS_RUNBOOK.md](OPERATIONS_RUNBOOK.md#24-receipt-verification) - Receipt generation

**Understanding TCPS:**
- [NEW_OPERATOR_ONBOARDING.md](training/NEW_OPERATOR_ONBOARDING.md#session-11-introduction-to-toyota-code-production-system-30-min) - TCPS fundamentals

---

## 📊 Documentation by Topic

### Daily Operations

- **Morning Checklist:** [OPERATIONS_RUNBOOK.md § 2.1](OPERATIONS_RUNBOOK.md#21-morning-checklist)
- **Work Orders:** [OPERATIONS_RUNBOOK.md § 2.2](OPERATIONS_RUNBOOK.md#22-processing-work-orders)
- **Andon Events:** [OPERATIONS_RUNBOOK.md § 2.3](OPERATIONS_RUNBOOK.md#23-handling-andon-events)
- **Receipt Verification:** [OPERATIONS_RUNBOOK.md § 2.4](OPERATIONS_RUNBOOK.md#24-receipt-verification)

### Monitoring & Alerting

- **Key Metrics:** [OPERATIONS_RUNBOOK.md § 3.1](OPERATIONS_RUNBOOK.md#31-key-metrics-to-watch)
- **Dashboard:** [OPERATIONS_RUNBOOK.md § 3.2](OPERATIONS_RUNBOOK.md#32-dashboard-monitoring)
- **Alerts:** [OPERATIONS_RUNBOOK.md § 3.3](OPERATIONS_RUNBOOK.md#33-alerting-rules-prometheus--alertmanager)
- **Log Analysis:** [OPERATIONS_RUNBOOK.md § 3.4](OPERATIONS_RUNBOOK.md#34-log-analysis)

### Troubleshooting

**Common Issues:**
- **Pipeline Stuck:** [OPERATIONS_RUNBOOK.md § 4.1](OPERATIONS_RUNBOOK.md#41-common-issues)
- **Quality Gates Failing:** [OPERATIONS_RUNBOOK.md § 4.1](OPERATIONS_RUNBOOK.md#issue-quality-gates-failing)
- **Dashboard Issues:** [OPERATIONS_RUNBOOK.md § 4.1](OPERATIONS_RUNBOOK.md#issue-dashboard-not-updating)
- **Performance:** [OPERATIONS_RUNBOOK.md § 4.1](OPERATIONS_RUNBOOK.md#issue-sparql-queries-slow)

**Advanced Troubleshooting:**
- **Log Analysis:** [ADVANCED_TROUBLESHOOTING.md § 1.1](training/ADVANCED_TROUBLESHOOTING.md#11-log-analysis-mastery)
- **ETS Debugging:** [ADVANCED_TROUBLESHOOTING.md § 1.2](training/ADVANCED_TROUBLESHOOTING.md#12-ets-table-debugging)
- **Complex Scenarios:** [ADVANCED_TROUBLESHOOTING.md § 2](training/ADVANCED_TROUBLESHOOTING.md#2-complex-failure-scenarios)

### Maintenance

- **Daily:** [OPERATIONS_RUNBOOK.md § 5.1](OPERATIONS_RUNBOOK.md#51-daily-maintenance)
- **Weekly:** [OPERATIONS_RUNBOOK.md § 5.2](OPERATIONS_RUNBOOK.md#52-weekly-maintenance)
- **Monthly:** [OPERATIONS_RUNBOOK.md § 5.3](OPERATIONS_RUNBOOK.md#53-monthly-maintenance)

### Disaster Recovery

- **Backup Strategy:** [OPERATIONS_RUNBOOK.md § 6.1](OPERATIONS_RUNBOOK.md#61-backup-strategy)
- **Restore Procedure:** [OPERATIONS_RUNBOOK.md § 6.2](OPERATIONS_RUNBOOK.md#62-restore-procedure)
- **Data Loss Scenarios:** [OPERATIONS_RUNBOOK.md § 6.3](OPERATIONS_RUNBOOK.md#63-data-loss-scenarios)
- **DR Drills:** [ADVANCED_TROUBLESHOOTING.md § 4](training/ADVANCED_TROUBLESHOOTING.md#4-disaster-recovery-drills)

### Performance Tuning

- **Connection Pooling:** [OPERATIONS_RUNBOOK.md § 7.1](OPERATIONS_RUNBOOK.md#71-connection-pooling)
- **ETS Tables:** [OPERATIONS_RUNBOOK.md § 7.2](OPERATIONS_RUNBOOK.md#72-ets-table-tuning)
- **SPARQL Caching:** [OPERATIONS_RUNBOOK.md § 7.3](OPERATIONS_RUNBOOK.md#73-sparql-query-caching)
- **Profiling:** [ADVANCED_TROUBLESHOOTING.md § 3.1](training/ADVANCED_TROUBLESHOOTING.md#31-profiling-tcps-performance)

### Security

- **Access Control:** [OPERATIONS_RUNBOOK.md § 8.1](OPERATIONS_RUNBOOK.md#81-access-control)
- **Secrets Management:** [OPERATIONS_RUNBOOK.md § 8.2](OPERATIONS_RUNBOOK.md#82-secrets-management)
- **Audit Trail:** [OPERATIONS_RUNBOOK.md § 8.3](OPERATIONS_RUNBOOK.md#83-audit-trail)

---

## 🔍 Quick Lookup

### Common Commands

```bash
# Health check
./tools/tcps tpm health

# Open Andons
./tools/tcps andon list --status=open

# Quality metrics
./tools/tcps quality metrics --period=daily

# Kanban status
./tools/tcps kanban status

# Dashboard
open http://localhost:8080
```

**Full command reference:** [OPERATIONS_RUNBOOK.md Appendix A](OPERATIONS_RUNBOOK.md#appendix-a-cli-command-reference)

### Key Metrics

| Metric | Target | Alert |
|--------|--------|-------|
| Quality Pass Rate | >95% | <90% |
| Lead Time (P90) | <2h | >4h |
| Open Andons | 0-2 | >5 |
| Throughput | >100/day | <50/day |

**Full metrics glossary:** [OPERATIONS_RUNBOOK.md Appendix C](OPERATIONS_RUNBOOK.md#appendix-c-metrics-glossary)

### Emergency Procedures

- **Mass Andon Resolution:** [TCPS_OPS_CHEATSHEET.md](TCPS_OPS_CHEATSHEET.md#-mass-andon-resolution-10-open)
- **Pipeline Stuck:** [TCPS_OPS_CHEATSHEET.md](TCPS_OPS_CHEATSHEET.md#-pipeline-stuck-no-progress)
- **System Failure:** [OPERATIONS_RUNBOOK.md § 4.2](OPERATIONS_RUNBOOK.md#42-emergency-procedures)

---

## 📋 Training Paths

### Path 1: New Operator (2-3 days)

**Day 1:**
- Morning: Read [NEW_OPERATOR_ONBOARDING.md § 1.1](training/NEW_OPERATOR_ONBOARDING.md#session-11-introduction-to-toyota-code-production-system-30-min) - TCPS Fundamentals
- Afternoon: Complete [NEW_OPERATOR_ONBOARDING.md § 1.2-1.3](training/NEW_OPERATOR_ONBOARDING.md#session-12-work-order-lifecycle-45-min) - Work Orders & Andon

**Day 2:**
- Morning: [NEW_OPERATOR_ONBOARDING.md § 2.1-2.2](training/NEW_OPERATOR_ONBOARDING.md#session-21-morning-checklist-routine-30-min) - Daily Operations
- Afternoon: [NEW_OPERATOR_ONBOARDING.md § 2.3](training/NEW_OPERATOR_ONBOARDING.md#session-23-quality-gates--receipts-45-min) - Quality Gates

**Day 3:**
- Morning: [NEW_OPERATOR_ONBOARDING.md § 3.1-3.2](training/NEW_OPERATOR_ONBOARDING.md#session-31-troubleshooting-common-issues-60-min) - Troubleshooting
- Afternoon: [NEW_OPERATOR_ONBOARDING.md § 3.3](training/NEW_OPERATOR_ONBOARDING.md#session-33-certification-exam-30-min) - Certification Exam

**After Training:**
- Print: [TCPS_OPS_CHEATSHEET.md](TCPS_OPS_CHEATSHEET.md)
- Bookmark: [OPERATIONS_RUNBOOK.md](OPERATIONS_RUNBOOK.md)
- Shadow experienced operator for 1 week

### Path 2: Advanced Operator (1 day)

**Prerequisites:** TCPS Certified Operator + 1 month experience

**Morning:**
- [ADVANCED_TROUBLESHOOTING.md § 1](training/ADVANCED_TROUBLESHOOTING.md#1-advanced-diagnostic-techniques) - Advanced Diagnostics

**Afternoon:**
- [ADVANCED_TROUBLESHOOTING.md § 2-3](training/ADVANCED_TROUBLESHOOTING.md#2-complex-failure-scenarios) - Complex Scenarios & Performance

**Practice:**
- [ADVANCED_TROUBLESHOOTING.md § 4](training/ADVANCED_TROUBLESHOOTING.md#4-disaster-recovery-drills) - DR Drills (homework)

### Path 3: On-Call Engineer (Self-paced)

**Required Reading:**
1. [OPERATIONS_RUNBOOK.md § 3](OPERATIONS_RUNBOOK.md#3-monitoring--alerting) - Monitoring & Alerting
2. [OPERATIONS_RUNBOOK.md § 4.2](OPERATIONS_RUNBOOK.md#42-emergency-procedures) - Emergency Procedures
3. [OPERATIONS_RUNBOOK.md Appendix D](OPERATIONS_RUNBOOK.md#appendix-d-alert-playbooks) - Alert Playbooks

**Keep Accessible:**
- [TCPS_OPS_CHEATSHEET.md](TCPS_OPS_CHEATSHEET.md) - Emergency procedures
- [OPERATIONS_RUNBOOK.md § 6](OPERATIONS_RUNBOOK.md#6-disaster-recovery) - Disaster Recovery

**Practice:**
- Run through alert playbooks
- Practice DR drills monthly

---

## 🔗 Related Documentation

### System Architecture

- [ARCHITECTURE_OVERVIEW.md](ARCHITECTURE_OVERVIEW.md) - TCPS system design
- [FOR_OPERATORS.md](FOR_OPERATORS.md) - Deployment and operations guide
- [TROUBLESHOOTING.md](TROUBLESHOOTING.md) - General troubleshooting

### Development

- [FOR_DEVELOPERS.md](FOR_DEVELOPERS.md) - Development guide
- [BUILD_SYSTEM.md](BUILD_SYSTEM.md) - Build system reference
- [GETTING_STARTED.md](GETTING_STARTED.md) - Quick start guide

### Integration

- [OPERATIONS_RUNBOOK.md Appendix E](OPERATIONS_RUNBOOK.md#appendix-e-integration-guides) - GitHub, JIRA, Prometheus

---

## 📞 Support & Contact

### Documentation Issues

- **Typos/Errors:** Create PR with fixes
- **Questions:** Post in #tcps-docs channel
- **Suggestions:** Email tcps-docs@company.com

### Operational Support

| Issue Level | Contact | Response Time |
|-------------|---------|---------------|
| **L1** - Questions | Team Chat (#tcps-ops) | 1 hour |
| **L2** - Issues | Team Lead | 2 hours |
| **L3** - Outages | On-Call | 15 minutes |
| **Emergency** | Page On-Call | Immediate |

**On-Call:** +1-555-TCPS-OPS (555-8277-677)
**Email:** tcps-ops@company.com

---

## 🔄 Documentation Maintenance

### Review Schedule

- **Monthly:** Update metrics and thresholds
- **Quarterly:** Review and update procedures
- **Annually:** Full documentation review

### Contributing

See [OPERATIONS_RUNBOOK.md](OPERATIONS_RUNBOOK.md) footer for revision history.

To contribute:
1. Make changes in Markdown
2. Test procedures
3. Submit PR with detailed description
4. Tag @tcps-docs-team for review

---

## 📈 Documentation Metrics

**Current Coverage:**

- ✅ Daily Operations: 100%
- ✅ Troubleshooting: 100%
- ✅ Emergency Procedures: 100%
- ✅ Training Materials: 100%
- ✅ Command Reference: 100%

**Total Pages:** 350+ pages

**Last Updated:** 2026-01-26

---

## 🎓 Certifications

### Available Certifications

1. **TCPS Certified Operator**
   - Training: [NEW_OPERATOR_ONBOARDING.md](training/NEW_OPERATOR_ONBOARDING.md)
   - Duration: 2-3 days
   - Exam: 30-minute practical

2. **TCPS Advanced Troubleshooter**
   - Training: [ADVANCED_TROUBLESHOOTING.md](training/ADVANCED_TROUBLESHOOTING.md)
   - Prerequisites: Certified Operator + 1 month experience
   - Duration: 1 day
   - Exam: 2-hour practical

### Certification Benefits

- ✓ Access to on-call rotation
- ✓ Full production system access
- ✓ Listed in operations contact list
- ✓ Eligibility for advanced training

---

## 📚 Additional Resources

### External References

- **Toyota Production System:** https://global.toyota/en/company/vision-and-philosophy/production-system/
- **Erlang Documentation:** https://erlang.org/doc/
- **RDF/SPARQL:** https://www.w3.org/TR/sparql11-query/

### Internal Wiki

- **TCPS Design Decisions:** wiki/tcps/design
- **Incident Post-Mortems:** wiki/tcps/incidents
- **Performance Baselines:** wiki/tcps/performance

---

**Document Version:** 1.0.0
**Last Updated:** 2026-01-26
**Maintained By:** TCPS Documentation Team

**Next Review:** 2026-02-26
