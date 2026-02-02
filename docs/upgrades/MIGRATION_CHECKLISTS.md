# Migration Checklists

**Version**: 2.1.0
**Last Updated**: 2026-02-01
**Scope**: OTP upgrade validation

---

## Table of Contents

1. [Pre-Upgrade Checklist](#pre-upgrade-checklist)
2. [Upgrade Execution Checklist](#upgrade-execution-checklist)
3. [Post-Upgrade Validation Checklist](#post-upgrade-validation-checklist)
4. [Rollback Checklist](#rollback-checklist)
5. [Team Coordination Checklist](#team-coordination-checklist)
6. [Documentation Update Checklist](#documentation-update-checklist)

---

## Pre-Upgrade Checklist

### Phase 1: Assessment (Week -2)

**System Inventory**:
- [ ] Document current OTP version: `erl -version`
- [ ] Document current erlmcp version: `git describe --tags`
- [ ] List all dependencies: `rebar3 tree > deps.txt`
- [ ] Document OS version: `uname -a`
- [ ] Document hardware specs: `lscpu`, `free -h`, `df -h`
- [ ] Document custom NIFs: Check for `.so` files
- [ ] Document custom ports: Check for linked-in drivers
- [ ] Document configuration files: `vm.args`, `sys.config`
- [ ] Document current performance metrics: `make benchmark-full > baseline.txt`
- [ ] Document known issues: Review error logs

**Compatibility Verification**:
- [ ] Check OTP 28 release notes: https://www.erlang.org/doc/system_principles/versions.html
- [ ] Check erlmcp 2.1.0 release notes: `CHANGELOG.md`
- [ ] Verify all dependencies support OTP 28
- [ ] Check for deprecated modules usage
- [ ] Review API compatibility doc: `docs/upgrades/API_COMPATIBILITY.md`
- [ ] Test in staging environment first
- [ ] Run `make check` in staging
- [ ] Run benchmarks in staging
- [ ] Verify no Dialyzer warnings

**Risk Assessment**:
- [ ] Identify critical services: What must stay running?
- [ ] Assess downtime tolerance: Maximum allowed downtime?
- [ ] Assess rollback feasibility: Can we rollback quickly?
- [ ] Identify data migration needs: ETS, Mnesia, DETS?
- [ ] Assess network dependencies: External services?
- [ ] Identify custom extensions: NIFs, ports?
- [ ] Assess team readiness: Is team trained on OTP 28?
- [ ] Document all risks: Create risk register

**Backup Planning**:
- [ ] Create code backup: `git tag backup-before-otp28-$(date +%Y%m%d)`
- [ ] Create configuration backup: Copy all `.config` files
- [ ] Create data backup: Backup `data/` directory
- [ ] Create dependency cache backup: Backup `_build/`
- [ ] Document backup locations: Create `BACKUP_INFO.txt`
- [ ] Test backup restoration: Verify backups are usable
- [ ] Estimate restore time: How long to restore?
- [ ] Document restore procedure: Step-by-step instructions

**Testing Plan**:
- [ ] Define test scope: Unit, integration, performance, load
- [ ] Set up test environment: Clone production
- [ ] Create test data: Mirror production data
- [ ] Schedule testing time: Reserve test slots
- [ ] Assign test responsibilities: Who tests what?
- [ ] Define pass criteria: What defines success?
- [ ] Document test cases: All scenarios
- [ ] Prepare test scripts: Automate where possible

---

### Phase 2: Preparation (Week -1)

**Resource Planning**:
- [ ] Schedule maintenance window: Approved by stakeholders
- [ ] Notify engineering team: Calendar invites sent
- [ ] Notify operations team: On-call scheduled
- [ ] Notify QA team: Testing time reserved
- [ ] Notify support team: Customer communication prepared
- [ ] Notify customers: 48-hour advance notice
- [ ] Prepare status page: Update template ready
- [ ] Prepare incident response: Rollback script tested

**Environment Setup**:
- [ ] Install OTP 28.3.1: `./scripts/install-otp.sh 28.3.1`
- [ ] Verify OTP installation: `erl -version` shows 28.3.1
- [ ] Set up test environment: Staging ready
- [ ] Clone production config: Test environment matches prod
- [ ] Update `rebar.config`: `{minimum_otp_vsn, "28"}`
- [ ] Update `vm.args`: Add OTP 28 optimizations
- [ ] Update dependencies: `rebar3 upgrade`
- [ ] Compile with OTP 28: `rebar3 compile`
- [ ] Resolve compilation errors: Clean compile

**Test Execution**:
- [ ] Run unit tests: `rebar3 eunit` - 100% pass rate
- [ ] Run integration tests: `rebar3 ct` - All suites pass
- [ ] Run Dialyzer: `rebar3 dialyzer` - 0 warnings
- [ ] Run Xref: `rebar3 xref` - 0 undefined functions
- [ ] Run format check: `rebar3 format --verify`
- [ ] Run coverage: `rebar3 cover` - >= 80%
- [ ] Run benchmarks: `make benchmark-full`
- [ ] Run load tests: 2x production load, 1 hour
- [ ] Run chaos tests: Failure scenarios
- [ ] Run upgrade tests: Path-specific tests
- [ ] Document test results: All metrics recorded

**Documentation**:
- [ ] Update runbooks: OTP 28 procedures
- [ ] Update playbooks: Incident response
- [ ] Update monitoring guides: New metrics
- [ ] Update architecture docs: New features
- [ ] Create upgrade plan: Step-by-step guide
- [ ] Create rollback plan: Emergency procedures
- [ ] Create communication plan: Stakeholder notifications
- [ ] Review all documentation: Peer review

**Final Validation**:
- [ ] Sign-off from engineering: Tech lead approval
- [ ] Sign-off from QA: Test results acceptable
- [ ] Sign-off from operations: Ready to execute
- [ ] Sign-off from management: Business approval
- [ ] Schedule upgrade date: Finalized
- [ ] Send final notification: All stakeholders informed

---

## Upgrade Execution Checklist

### Phase 3: Execution (Day 0)

**Pre-Upgrade (T-1 hour)**:
- [ ] Verify backup availability: All backups present
- [ ] Verify backup integrity: Test restore one file
- [ ] Verify test environment ready: Staging accessible
- [ ] Verify team availability: All team members online
- [ ] Verify monitoring ready: Dashboards accessible
- [ ] Verify rollback script tested: Run once successfully
- [ ] Send pre-upgrade notification: 1-hour warning
- [ ] Set up communication channel: Slack/Teams ready

**Execution (T-0)**:
- [ ] Stop erlmcp gracefully: `make stop`
- [ ] Verify all processes stopped: `ps aux | grep beam`
- [ ] Switch to OTP 28: `. ~/.kerl/28.3.1/activate`
- [ ] Verify OTP version: `erl -version` shows 28.3.1
- [ ] Clear build artifacts: `make clean`
- [ ] Clear dependency cache: `rm -rf _build/default/lib`
- [ ] Update code to 2.1.0: `git checkout v2.1.0`
- [ ] Compile with OTP 28: `make compile`
- [ ] Verify compilation success: 0 errors
- [ ] Run quick smoke test: `make smoke`
- [ ] Start erlmcp: `make start`
- [ ] Verify all applications started: `application:which_applications()`
- [ ] Verify all supervisors running: `observer:start()`
- [ ] Run health check: `make health`
- [ ] Verify no error logs: `tail log/erlang.log`

**Post-Upgrade Validation (T+30 minutes)**:
- [ ] Run unit tests: `rebar3 eunit` - 100% pass
- [ ] Run smoke tests: Core functionality
- [ ] Check metrics dashboard: All normal
- [ ] Check error logs: No errors
- [ ] Check performance: Baseline comparison
- [ ] Verify customer functionality: Test key features
- [ ] Send success notification: Upgrade complete

**Extended Monitoring (T+1 hour to T+24 hours)**:
- [ ] Monitor error logs: `tail -f log/erlang.log`
- [ ] Monitor metrics: Dashboard every 30 minutes
- [ ] Monitor performance: Compare to baseline
- [ ] Monitor customer tickets: Support queue
- [ ] Monitor system resources: CPU, memory, disk
- [ ] Monitor scheduler utilization: Balanced?
- [ ] Monitor memory growth: Stable?
- [ ] Monitor GC activity: Normal?
- [ ] Document any issues: Incident reports

---

## Post-Upgrade Validation Checklist

### Phase 4: Validation (Week +1)

**Functional Validation**:
- [ ] All unit tests passing: `rebar3 eunit`
- [ ] All integration tests passing: `rebar3 ct`
- [ ] All smoke tests passing: `make smoke`
- [ ] All feature tests passing: Full feature coverage
- [ ] All upgrade tests passing: Path-specific
- [ ] Customer scenarios tested: Real workflows
- [ ] API compatibility verified: All clients work
- [ ] Transport layer verified: All transports work
- [ ] Session backend verified: All backends work
- [ ] Auth methods verified: All methods work

**Performance Validation**:
- [ ] Registry throughput: >= 553K msg/s
- [ ] Queue throughput: >= 971K msg/s
- [ ] Connection capacity: >= 40K/node
- [ ] Memory per connection: <= 10KB
- [ ] Latency P50: <15ms
- [ ] Latency P95: <30ms
- [ ] Latency P99: <50ms
- [ ] CPU utilization: <70%
- [ ] Memory utilization: <80%
- [ ] Scheduler utilization: Balanced <80%

**Stability Validation**:
- [ ] 24-hour soak test: No crashes
- [ ] Memory leak test: Stable memory
- [ ] Load test: 2x production, 1 hour
- [ ] Spike test: 10x load, 5 minutes
- [ ] Chaos test: Failure scenarios
- [ ] Error rate: 0%
- [ ] Crash dumps: 0 files
- [ ] Supervisor restarts: Normal rate
- [ ] Process exits: Normal rate
- [ ] Port health: All healthy

**Security Validation**:
- [ ] SSL/TLS working: Verify connections
- [ ] Auth working: All methods
- [ ] Rate limiting working: Verify limits
- [ ] Access control working: Verify permissions
- [ ] Audit logging working: Verify logs
- [ ] Secrets management: Verify access
- [ ] No security warnings: `rebar3 dialyzer`
- [ ] No vulnerabilities: Dependency scan

**Documentation Validation**:
- [ ] Runbooks updated: OTP 28 procedures
- [ ] Architecture docs updated: New features
- [ ] API docs updated: New APIs
- [ ] Monitoring guides updated: New metrics
- [ ] Troubleshooting guides updated: New issues
- [ ] Onboarding docs updated: OTP 28 setup
- [ ] All docs reviewed: Peer approval
- [ ] Docs published: Available to team

---

## Rollback Checklist

### Phase 5: Rollback (If Needed)

**Trigger Decision**:
- [ ] Critical bug encountered: Data corruption, crashes
- [ ] Performance regression: >20% degradation
- [ ] Failed validation: Tests not passing
- [ ] Customer impact: Service degradation
- [ ] Security issue: Vulnerability discovered
- [ ] Business decision: Strategic rollback

**Pre-Rollback**:
- [ ] Document rollback reason: Incident report
- [ ] Notify stakeholders: Rollback initiated
- [ ] Stop erlmcp gracefully: `make stop`
- [ ] Create post-upgrade backup: Just in case
- [ ] Verify backup availability: Rollback data ready
- [ ] Prepare rollback script: Ready to execute

**Rollback Execution**:
- [ ] Deactivate OTP 28: `kerl deactivate`
- [ ] Activate OTP 27: `. ~/.kerl/27.3/activate`
- [ ] Verify OTP version: `erl -version` shows 27.3
- [ ] Restore previous code: `git checkout v2.0.x`
- [ ] Clean build artifacts: `make clean`
- [ ] Compile with OTP 27: `make compile`
- [ ] Verify compilation: 0 errors
- [ ] Start erlmcp: `make start`
- [ ] Verify applications: All started
- [ ] Run health check: `make health`
- [ ] Run smoke tests: Core functionality
- [ ] Send rollback notification: Complete

**Post-Rollback**:
- [ ] Monitor system: 1 hour observation
- [ ] Verify stability: No issues
- [ ] Document root cause: Why upgrade failed
- [ ] Plan fix: Address root cause
- [ ] Schedule retry: New upgrade window
- [ ] Update procedures: Prevent recurrence

---

## Team Coordination Checklist

### Engineering Team
- [ ] Lead developer: Assigned and available
- [ ] Backend developers: Assigned and available
- [ ] DevOps engineer: Assigned and available
- [ ] QA engineer: Assigned and available
- [ ] On-call engineer: Scheduled and available
- [ ] Escalation contact: Documented and available

### Operations Team
- [ ] Operations lead: Notified and available
- [ ] System administrator: Available for support
- [ ] Network engineer: Available if needed
- [ ] Database administrator: Available if needed
- [ ] Security team: Notified of upgrade
- [ ] Incident response: On standby

### Stakeholder Communication
- [ ] Engineering manager: Notified and approved
- [ ] Product manager: Notified and approved
- [ ] Support team: Notified and prepared
- [ ] Customer support: Notified and ready
- [ ] Documentation team: Notified of changes
- [ ] Training team: Notified of new features

### Customer Communication
- [ ] 48-hour notice sent: All customers notified
- [ ] Maintenance window communicated: Clear timing
- [ ] Expected impact documented: Downtime/changes
- [ ] Support contacts provided: Who to contact
- [ ] Status page updated: Current status
- [ ] Post-upgrade notice sent: Completion confirmed

---

## Documentation Update Checklist

### Technical Documentation
- [ ] Architecture documentation: Updated with OTP 28 features
- [ ] API documentation: Updated with new APIs
- [ ] Migration guide: Complete and tested
- [ ] Troubleshooting guide: Updated with new issues
- [ ] Performance guide: Updated with new benchmarks
- [ ] Best practices guide: Updated with lessons learned
- [ ] Configuration reference: Updated with new options
- [ ] Runbooks: Updated with OTP 28 procedures

### Operational Documentation
- [ ] On-call playbooks: Updated with new procedures
- [ ] Incident response: Updated with rollback steps
- [ ] Monitoring guides: Updated with new metrics
- [ ] Alerting rules: Updated for OTP 28
- [ ] Capacity planning: Updated with new baselines
- [ ] Disaster recovery: Updated with new procedures
- [ ] Security procedures: Updated with new features
- [ ] Backup procedures: Updated with new requirements

### Training Materials
- [ ] Team training: OTP 28 features delivered
- [ ] Operations training: New procedures delivered
- [ ] Support training: New issues and solutions delivered
- [ ] Customer documentation: New features documented
- [ ] Video tutorials: New features demonstrated
- [ ] Knowledge base: Updated with OTP 28 content
- [ ] FAQ: Updated with common questions
- [ ] Changelog: Published with all changes

### Process Documentation
- [ ] Upgrade process: Documented and reviewed
- [ ] Rollback process: Documented and tested
- [ ] Testing process: Documented and automated
- [ ] Approval process: Documented and followed
- [ ] Communication process: Documented and executed
- [ ] Escalation process: Documented and available
- [ ] Post-mortem process: Prepared if needed
- [ ] Continuous improvement: Process updated

---

## Quick Reference

### Day-Before Checklist
- [ ] Backups verified
- [ ] Team notified
- [ ] Monitoring ready
- [ ] Rollback script tested
- [ ] All tests passing

### Day-Of Checklist
- [ ] Send pre-upgrade notice
- [ ] Stop system gracefully
- [ ] Upgrade OTP
- [ ] Deploy erlmcp
- [ ] Run smoke tests
- [ ] Monitor closely
- [ ] Send completion notice

### Week-After Checklist
- [ ] All tests passing
- [ ] Performance validated
- [ ] Stability confirmed
- [ ] Docs updated
- [ ] Team trained
- [ ] Retrospective scheduled

---

## Sign-Off

**Upgrade Completion**:

| Role | Name | Signature | Date |
|------|------|-----------|------|
| Upgrade Lead | | | |
| QA Engineer | | | |
| Operations Lead | | | |
| Engineering Manager | | | |

**Approval**:

- [ ] Pre-upgrade requirements met
- [ ] Upgrade executed successfully
- [ ] Post-upgrade validation passed
- [ ] Documentation completed
- [ ] Team notified

---

**Document Version**: 1.0
**Author**: SPARC Orchestrator
**Review Date**: 2026-03-01
