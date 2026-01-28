# Quality Gate Enforcement System - FAQ

**Version:** 1.0.0
**Last Updated:** 2026-01-28

## General Questions

### Q: What are quality gates?

**A:** Quality gates are automated checkpoints that verify your code meets production standards before it can progress. Think of them as automated code reviewers that check:
- Compilation errors
- Test coverage and pass rates
- Security vulnerabilities
- Build reproducibility
- Quality metrics
- Release readiness

### Q: Why do we need quality gates?

**A:** Quality gates:
1. **Prevent defects** from reaching production (shift left)
2. **Save time** by catching issues early ($1 vs $100 cost)
3. **Maintain standards** consistently across the team
4. **Provide confidence** that code is production-ready
5. **Create audit trails** for compliance

### Q: How long do quality gates take to run?

**A:** Typical execution times:
- **Fast mode** (pre-commit): 1-2 minutes
- **Standard mode** (CI): 5-10 minutes
- **Full mode** (release): 10-20 minutes

Parallel execution and caching can reduce these times significantly.

### Q: Can I skip quality gates?

**A:** Only in emergencies with proper approval:
- Production incidents requiring immediate hotfix
- Security vulnerabilities needing urgent patch
- Regulatory deadlines with executive approval

All overrides are logged to audit trail and require documented justification.

## Technical Questions

### Q: What happens when a quality gate fails?

**A:** When a gate fails:
1. **Execution stops** immediately (no further gates run)
2. **Andon event triggered** for team visibility
3. **Detailed violations reported** with line numbers and remediation hints
4. **Receipt generated** with failure details
5. **SKU blocked** from progression until resolved

### Q: How do quality gates integrate with CI/CD?

**A:** Quality gates integrate via:
- **GitHub Actions:** Use official action or run `rebar3 tcps check-all-gates`
- **GitLab CI:** Use Docker image with rebar3 and TCPS plugin
- **Jenkins:** Use shell script to run gates
- **Other CI:** Use command-line interface

See [User Guide](USER_GUIDE.md#cicd-integration) for examples.

### Q: Can quality gates run in parallel?

**A:** Partially. Some gates have dependencies:
```
Parallel Group 1: [compilation, shacl_validation]
Parallel Group 2: [test_execution, security_scan]  (after Group 1)
Sequential:       [deterministic_build, quality_metrics, release_verification, smoke_test]
```

Configure parallel execution in `rebar.config` (experimental feature).

### Q: How are receipts used?

**A:** Receipts provide:
- **Audit trail:** Immutable record of all quality checks
- **Traceability:** Link code changes to quality verification
- **Compliance:** Evidence for regulatory requirements
- **Debugging:** Understand why/when gates failed

Each receipt is SHA-256 hashed and chained for tamper-proofing.

### Q: What is the Andon system?

**A:** Andon (行灯) is a visual management system from Toyota:
- **Signals problems** immediately when detected
- **Stops the line** until issue resolved
- **Triggers team response** for collaborative resolution
- **Tracks resolution** time and root cause

In erlmcp, Andon events are triggered by quality gate failures.

## Development Workflow Questions

### Q: How do I run quality gates locally?

**A:** Simple:
```bash
# Check all gates
make quality-check

# Check specific gate
rebar3 tcps quality-gate --gate=compilation

# Check with your SKU ID
SKU_ID="myfeature-$(git rev-parse --short HEAD)"
rebar3 tcps check-all-gates --sku=$SKU_ID
```

### Q: Should I run all gates before committing?

**A:** Recommended workflow:
- **Before commit:** Run fast gates (compilation, fast tests) - 1-2 min
- **Before push:** Run standard gates (all except deterministic build) - 5-10 min
- **In CI:** Run all gates including slow ones - 10-20 min

Use pre-commit hooks to automate this.

### Q: What if tests are flaky?

**A:** Flaky tests indicate quality issues. Fix them:
1. **Identify flaky tests:** Run tests multiple times, track failures
2. **Find root cause:** Timing issues? Race conditions? Environment?
3. **Fix properly:** Add synchronization, use deterministic mocks, improve isolation
4. **Don't mask:** Never increase timeouts or add retries without fixing root cause

Flaky tests are technical debt. Quality gates enforce fixing them.

### Q: How do I improve test coverage?

**A:** Systematic approach:
```bash
# 1. Check current coverage
rebar3 cover --verbose

# 2. Identify uncovered code
rebar3 cover --module=mymodule --verbose

# 3. Add tests for uncovered lines
# Edit test/mymodule_tests.erl

# 4. Verify improvement
rebar3 tcps quality-gate --gate=test_execution

# 5. Repeat until target reached
```

Focus on critical code paths first (business logic, edge cases).

### Q: Can I configure different thresholds for different environments?

**A:** Yes. Use environment-specific config:
```erlang
% config/dev.config - Relaxed for development
[{tcps_erlmcp, [
    {quality_gates, [
        {thresholds, #{
            test_pass_rate => 0.90,
            test_coverage => 0.70
        }}
    ]}
]}].

% config/prod.config - Strict for production
[{tcps_erlmcp, [
    {quality_gates, [
        {thresholds, #{
            test_pass_rate => 0.98,
            test_coverage => 0.85
        }}
    ]}
]}].
```

## Troubleshooting Questions

### Q: Gate times out - what should I do?

**A:** Diagnose and fix:
```bash
# 1. Check which gate timed out
rebar3 tcps gate-status --sku=$SKU_ID --verbose

# 2. Increase timeout temporarily
export TCPS_GATE_TIMEOUT=600000  # 10 minutes

# 3. Investigate root cause
# - Tests running too long? → Optimize or split
# - Compilation slow? → Check dependency graph
# - Security scan hanging? → Update scanner

# 4. Fix permanently
# Edit rebar.config gate timeout
```

### Q: Compilation gate fails with "undefined function" errors

**A:** Common causes and fixes:
```erlang
% 1. Missing export
% Fix: Add -export([function/arity]) to module

% 2. Typo in function name
% Fix: Check spelling, use IDE autocomplete

% 3. Module not compiled yet
% Fix: Check module dependencies, ensure correct order

% 4. Function not imported
% Fix: Add -import(module, [function/arity]) or use module:function()
```

### Q: Test gate fails - how do I debug?

**A:** Step-by-step debugging:
```bash
# 1. Run tests with verbose output
rebar3 eunit --module=mymodule_tests --verbose

# 2. Run specific failing test
rebar3 eunit --module=mymodule_tests --test=test_edge_case

# 3. Enable debug logging
export TCPS_DEBUG=true
rebar3 eunit --module=mymodule_tests --verbose

# 4. Use Erlang debugger
erl -pa _build/default/lib/*/ebin
> debugger:start().
> debugger:load(mymodule_tests).
> % Set breakpoints, step through code
```

### Q: Security scan reports false positive - what should I do?

**A:** Evaluate and handle:
```bash
# 1. Verify it's actually a false positive
# Review the vulnerability report carefully

# 2. If confirmed false positive:
# Add exception to security scan config
% rebar.config
{tcps, [
    {quality_gates, [
        {gates, [
            {security_scan, #{
                exceptions => [
                    {dependency, "jsx", "3.0.0", "CVE-2023-12345",
                     "False positive - we don't use vulnerable function"}
                ]
            }}
        ]}
    ]}
]}.

# 3. Document in security.md
# Explain why exception is valid

# 4. Review exceptions quarterly
# Ensure they're still valid
```

### Q: Deterministic build gate always fails - why?

**A:** Common causes:
```erlang
% 1. Timestamp in code
% ❌ Bad:
-vsn(erlang:system_time()).

% ✅ Good:
-vsn("1.0.0").

% 2. Random values at compile time
% ❌ Bad:
-define(ID, rand:uniform(999999)).

% ✅ Good:
-define(ID_PREFIX, "ID-").
generate_id() -> ?ID_PREFIX ++ integer_to_list(rand:uniform(999999)).

% 3. File modification times
% ✅ Fix: Set SOURCE_DATE_EPOCH environment variable
export SOURCE_DATE_EPOCH=1706472000
```

### Q: Receipt chain verification fails - how to fix?

**A:** Recovery steps:
```bash
# 1. Identify broken link
rebar3 tcps verify-receipt-chain --sku=$SKU_ID --verbose

# 2. Restore from backup if available
scripts/restore-receipts.sh YYYYMMDD

# 3. Rebuild chain if backup unavailable
rebar3 tcps rebuild-receipt-chain --sku=$SKU_ID --force

# 4. Verify rebuild
rebar3 tcps verify-receipt-chain --sku=$SKU_ID

# 5. Investigate cause
# - File corruption?
# - Manual editing?
# - Disk failure?
```

## Performance Questions

### Q: Quality gates are too slow - how to speed up?

**A:** Optimization strategies:
1. **Enable caching:**
   ```erlang
   {quality_gates, [{cache_strategy, smart}]}
   ```

2. **Use parallel execution:**
   ```erlang
   {quality_gates, [{parallel_execution, true}]}
   ```

3. **Optimize tests:**
   - Run unit tests separately from integration tests
   - Use mocks to avoid slow I/O
   - Parallelize independent test suites

4. **Cache dependencies:**
   - Cache rebar3 packages in CI
   - Use Docker layers for dependencies

5. **Run fast gates first:**
   - Compilation before tests
   - Unit tests before integration tests

### Q: Can I run only changed gates?

**A:** Yes, with smart caching:
```erlang
% rebar.config
{quality_gates, [
    {cache_strategy, smart},
    {cache_invalidation, [
        {compilation, [
            watch_files => ["src/**/*.erl", "include/**/*.hrl"],
            watch_config => ["rebar.config"]
        ]},
        {test_execution, [
            watch_files => ["src/**/*.erl", "test/**/*.erl"]
        ]}
    ]}
]}.
```

Smart caching checks file hashes and skips unchanged gates.

## Policy Questions

### Q: Can individual developers override gates?

**A:** No. Override requires:
- Documented emergency justification
- Approval from authorized personnel (tech lead, CTO)
- Audit trail entry
- Follow-up ticket to fix properly

See [User Guide - Emergency Override](USER_GUIDE.md#emergency-override).

### Q: What are valid reasons to override?

**A:** Valid reasons:
- Production incident requiring immediate hotfix
- Security vulnerability needing urgent patch
- Regulatory deadline with executive approval
- Infrastructure failure blocking gates (with plan to re-run)

Invalid reasons:
- "Tests are flaky" → Fix the tests
- "Deadline is tight" → Poor planning
- "Coverage is hard" → Add tests
- "It works on my machine" → Fix environment

### Q: How long are receipts retained?

**A:** Receipt retention policy:
- **Development:** 30 days
- **Staging:** 90 days
- **Production:** 7 years (compliance requirement)

Receipts are backed up daily to S3/GCS with Standard-IA storage.

### Q: Who has access to override quality gates?

**A:** Role-based access control:
- **Developer:** Can run gates, view receipts
- **Senior Developer:** + Can override non-critical gates (smoke test)
- **Tech Lead:** + Can configure gates, update baselines
- **Admin:** Full access

See [Admin Guide - Team Permissions](ADMIN_GUIDE.md#team-permissions).

## Philosophical Questions

### Q: Isn't this process too heavy/bureaucratic?

**A:** Quality gates are lightweight compared to manual code review:
- **Manual review:** 30-60 minutes per PR
- **Quality gates:** 5-10 minutes automated

Quality gates catch 80% of issues automatically, allowing reviewers to focus on architecture and business logic.

### Q: Won't this slow down development?

**A:** Studies show quality gates **speed up** development:
- Less time firefighting production issues
- Fewer emergency hotfixes
- Less context switching
- Higher confidence in deployments

**Data:**
- Netflix: 100+ deploys/day with quality gates
- Amazon: Deploys every 11.7 seconds with quality gates
- Spotify: < 0.1% rollback rate with quality gates

### Q: What if we need to ship fast?

**A:** Quality gates enable fast shipping:
- **Without gates:** Ship fast → bugs → rollback → emergency fix → ship again (days)
- **With gates:** Ship when ready → no bugs → no rollback (hours)

Quality gates provide confidence to ship fast safely.

### Q: How does this relate to "move fast and break things"?

**A:** Modern version: "Move fast with stable infrastructure"

Quality gates are the infrastructure that enables moving fast:
- Automate quality checks (faster than manual)
- Catch issues early (cheaper than late)
- Enable confident deployment (faster than cautious)

## Integration Questions

### Q: Can I integrate with external tools?

**A:** Yes. Quality gates support plugins:
```erlang
% rebar.config
{tcps, [
    {quality_gates, [
        {external_validators, [
            {sonarqube, #{
                url => "https://sonar.company.com",
                token => {env, "SONAR_TOKEN"},
                project => "erlmcp"
            }},
            {custom_validator, #{
                command => "scripts/custom-check.sh",
                timeout => 60000
            }}
        ]}
    ]}
]}.
```

### Q: How do I integrate with Slack/PagerDuty?

**A:** Configure notification channels:
```erlang
% rebar.config
{tcps, [
    {andon, [
        {notification_channels, [
            {slack, #{
                webhook_url => {env, "SLACK_WEBHOOK"},
                channel => "#quality-alerts"
            }},
            {pagerduty, #{
                integration_key => {env, "PD_INTEGRATION_KEY"},
                severity_threshold => critical
            }}
        ]}
    ]}
]}.
```

### Q: Can I export metrics to Prometheus/Grafana?

**A:** Yes. Built-in Prometheus exporter:
```erlang
% sys.config
[{tcps_erlmcp, [
    {metrics, [
        {prometheus, #{
            enabled => true,
            port => 9100,
            path => "/metrics"
        }}
    ]}
]}].
```

See [Admin Guide - Monitoring](ADMIN_GUIDE.md#monitoring-and-alerts).

## Migration Questions

See [Migration Guide](MIGRATION.md) for detailed migration steps.

### Q: How do I adopt quality gates gradually?

**A:** Phased rollout:
1. **Week 1:** Warning mode only (don't block)
2. **Week 2:** Block compilation and test gates
3. **Week 3:** Add security scan
4. **Week 4:** Enable all gates

See [Migration Guide - Gradual Rollout](MIGRATION.md#gradual-rollout-strategy).

### Q: What if we have legacy code with low coverage?

**A:** Use temporary baseline overrides:
```erlang
{quality_gates, [
    {baseline_override, #{
        reason => "Legacy codebase - improving coverage incrementally",
        ticket => "TECH-DEBT-12345",
        expiry => {{2026, 12, 31}, {23, 59, 59}},
        thresholds => #{
            test_coverage => 0.40  % Temporary lower threshold
        }
    }}
]}.
```

Increase threshold monthly as you add tests.

### Q: How do I train my team on quality gates?

**A:** Training resources:
1. **Workshop:** 2-hour hands-on quality gates workshop
2. **Documentation:** Read [User Guide](USER_GUIDE.md) and [Philosophy](PHILOSOPHY.md)
3. **Pair programming:** Pair new developers with experienced ones
4. **Brown bag sessions:** Weekly 30-minute quality talks
5. **Retrospectives:** Discuss quality gate experiences

Contact training@erlmcp.dev for scheduled workshops.

## Support

### Q: Where can I get help?

**A:** Support channels:
- **Documentation:** [Index](INDEX.md) for all documentation
- **GitHub Issues:** https://github.com/banyan-platform/erlmcp/issues
- **Slack:** #erlmcp-quality-gates
- **Email:** quality-gates@erlmcp.dev
- **Office Hours:** Tuesdays 2-3pm PT (Zoom link in Slack)

### Q: How do I report a bug?

**A:** Report bugs on GitHub:
1. Check if already reported
2. Include:
   - erlmcp version
   - Erlang/OTP version
   - Reproduction steps
   - Error messages
   - Receipt IDs (if applicable)
3. Label: `bug`, `quality-gates`

### Q: How do I request a feature?

**A:** Request features on GitHub:
1. Check if already requested
2. Describe:
   - Use case
   - Expected behavior
   - Benefits
   - Alternatives considered
3. Label: `enhancement`, `quality-gates`

---

**Still have questions?**
- Check [User Guide](USER_GUIDE.md) for usage details
- Check [Admin Guide](ADMIN_GUIDE.md) for configuration
- Check [Architecture](ARCHITECTURE.md) for technical details
- Ask in Slack: #erlmcp-quality-gates

**Version History:**
- v1.0.0 (2026-01-28): Initial FAQ
