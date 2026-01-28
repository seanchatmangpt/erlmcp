# Quality Gate Enforcement System - Administrator Guide

**Audience:** System administrators, DevOps engineers, team leads
**Version:** 1.0.0
**Last Updated:** 2026-01-28

## Introduction

This guide helps administrators configure, manage, and maintain the erlmcp Quality Gate Enforcement System. Learn how to configure gates, update baselines, manage hooks, and handle team permissions.

## System Configuration

### Global Configuration

**rebar.config:**
```erlang
{tcps, [
    %% Quality gate configuration
    {quality_gates, [
        {enabled, true},
        {receipts_dir, "priv/receipts"},
        {cache_ttl, infinity},  % No TTL by default

        %% Production thresholds (Lean Six Sigma standards)
        {thresholds, #{
            test_pass_rate => 0.95,        % 95% minimum
            test_coverage => 0.80,          % 80% minimum
            quality_gate_pass_rate => 0.95, % 95% gate pass rate
            defect_rate => 0.05,            % 5% max defect rate
            first_pass_yield => 0.90        % 90% first pass yield
        }},

        %% Gate-specific configuration
        {gates, [
            {shacl_validation, #{
                enabled => true,
                timeout => 60000,  % 60 seconds
                validator => tcps_rebar3_shacl
            }},

            {compilation, #{
                enabled => true,
                timeout => 120000,  % 2 minutes
                fail_on_warnings => false,
                max_warnings => 10
            }},

            {test_execution, #{
                enabled => true,
                timeout => 300000,  % 5 minutes
                min_pass_rate => 0.95,
                min_coverage => 0.80,
                fast_mode_enabled => true,
                fast_mode_pattern => "*_fast_tests.erl"
            }},

            {security_scan, #{
                enabled => true,
                timeout => 60000,
                scanners => [dependency_audit, secret_scan, pattern_check],
                severity_threshold => critical  % Block on critical only
            }},

            {deterministic_build, #{
                enabled => true,
                timeout => 300000,
                build_twice => true,
                hash_algorithm => sha256
            }},

            {quality_metrics, #{
                enabled => true,
                timeout => 10000,
                check_trends => true,
                lookback_days => 30
            }},

            {release_verification, #{
                enabled => true,
                timeout => 30000,
                required_artifacts => [sbom, provenance, licenses],
                sbom_format => cyclonedx
            }},

            {smoke_test, #{
                enabled => true,
                timeout => 120000,
                blocking => false,  % Non-blocking gate
                test_pattern => "smoke_*.erl"
            }}
        ]}
    ]},

    %% Andon system configuration
    {andon, [
        {auto_trigger, true},
        {notification_channels, [email, slack, pagerduty]},
        {escalation_timeout, 3600000}  % 1 hour
    ]},

    %% Receipt chain configuration
    {receipt_chain, [
        {storage, filesystem},
        {backup_enabled, true},
        {backup_location, "s3://receipts-backup"},
        {verification_enabled, true}
    ]}
]}.
```

### Per-Environment Configuration

**config/dev.config:**
```erlang
[
    {tcps_erlmcp, [
        {quality_gates, [
            {thresholds, #{
                test_pass_rate => 0.90,        % Relaxed for dev
                test_coverage => 0.70,
                quality_gate_pass_rate => 0.90,
                defect_rate => 0.10,
                first_pass_yield => 0.80
            }}
        ]}
    ]}
].
```

**config/prod.config:**
```erlang
[
    {tcps_erlmcp, [
        {quality_gates, [
            {thresholds, #{
                test_pass_rate => 0.98,        % Stricter for prod
                test_coverage => 0.85,
                quality_gate_pass_rate => 0.98,
                defect_rate => 0.02,
                first_pass_yield => 0.95
            }},

            %% Enforce all gates in production
            {gates, [
                {smoke_test, #{
                    enabled => true,
                    blocking => true  % Block in prod
                }}
            ]}
        ]}
    ]}
].
```

## Managing Quality Baselines

### Updating Thresholds

**When to update:**
- After team training improves quality
- When tooling improves reliability
- After process changes
- Based on historical performance

**How to update:**
```bash
# 1. Analyze current performance
rebar3 tcps quality-metrics --last-90-days --format=json > metrics-90d.json

# 2. Calculate new baselines (use 90th percentile)
python scripts/calculate_baselines.py metrics-90d.json

# Output:
# Recommended baselines:
#   test_pass_rate: 0.97 (current: 0.95)
#   test_coverage: 0.87 (current: 0.80)
#   first_pass_yield: 0.93 (current: 0.90)

# 3. Update rebar.config
# Edit thresholds section

# 4. Communicate to team
# Send announcement, update docs

# 5. Monitor impact
rebar3 tcps quality-metrics --watch
```

**Gradual Rollout:**
```erlang
% Phase 1: Warning only (30 days)
{quality_gates, [
    {thresholds, #{
        test_pass_rate => 0.97,
        test_coverage => 0.87,
        enforcement_mode => warn  % Don't block yet
    }}
]}.

% Phase 2: Enforcement (after 30 days)
{quality_gates, [
    {thresholds, #{
        test_pass_rate => 0.97,
        test_coverage => 0.87,
        enforcement_mode => enforce  % Block on failure
    }}
]}.
```

### Baseline Exceptions

**Temporary exceptions for specific projects:**
```erlang
% rebar.config (project-specific)
{tcps, [
    {quality_gates, [
        {baseline_override, #{
            reason => "Legacy codebase migration in progress",
            ticket => "TECH-DEBT-12345",
            expiry => {{2026, 06, 30}, {23, 59, 59}},
            thresholds => #{
                test_coverage => 0.60,  % Temporary lower threshold
                test_pass_rate => 0.85
            }
        }}
    ]}
]}.
```

**Override validation:**
```erlang
% System checks expiry and reason
validate_baseline_override(Override) ->
    Expiry = maps:get(expiry, Override),
    Now = calendar:universal_time(),

    case Expiry > Now of
        true ->
            % Override still valid
            logger:warning("Baseline override active", #{
                reason => maps:get(reason, Override),
                ticket => maps:get(ticket, Override),
                expires => format_datetime(Expiry)
            }),
            {ok, Override};
        false ->
            % Override expired
            logger:error("Baseline override expired", #{
                ticket => maps:get(ticket, Override)
            }),
            {error, override_expired}
    end.
```

## Hook Management

### Git Hooks

**Installing hooks for team:**
```bash
# Install pre-commit hook
make install-hooks

# Or manually
cp scripts/hooks/pre-commit .git/hooks/pre-commit
chmod +x .git/hooks/pre-commit

# Commit to repo (shared hooks)
mkdir -p .githooks
cp scripts/hooks/pre-commit .githooks/pre-commit

# Configure git to use shared hooks
git config core.hooksPath .githooks
```

**Pre-commit hook template:**
```bash
#!/bin/bash
# .githooks/pre-commit

set -e

echo "🔍 Running pre-commit quality checks..."

# Load configuration
FAST_GATES_ONLY="${FAST_GATES_ONLY:-true}"
SKIP_GATES="${SKIP_GATES:-}"

# Gate 1: Compilation (always)
if [[ ! "$SKIP_GATES" =~ "compilation" ]]; then
    echo "  ⚙️  Checking compilation..."
    rebar3 tcps quality-gate --gate=compilation --quiet
    echo "  ✅ Compilation passed"
fi

# Gate 2: Tests (fast mode for pre-commit)
if [[ ! "$SKIP_GATES" =~ "test_execution" ]]; then
    echo "  🧪 Running tests..."
    if [ "$FAST_GATES_ONLY" = "true" ]; then
        rebar3 tcps quality-gate --gate=test_execution --fast --quiet
    else
        rebar3 tcps quality-gate --gate=test_execution --quiet
    fi
    echo "  ✅ Tests passed"
fi

# Gate 3: Security scan (optional for pre-commit)
if [ "${RUN_SECURITY_SCAN:-false}" = "true" ]; then
    echo "  🔒 Running security scan..."
    rebar3 tcps quality-gate --gate=security_scan --quiet
    echo "  ✅ Security scan passed"
fi

echo "✅ All pre-commit checks passed"
exit 0
```

**Post-commit hook (receipt logging):**
```bash
#!/bin/bash
# .githooks/post-commit

# Log commit to receipt chain
COMMIT_HASH=$(git rev-parse HEAD)
COMMIT_MSG=$(git log -1 --pretty=%B)

rebar3 tcps receipt-log \
    --type=commit \
    --hash=$COMMIT_HASH \
    --message="$COMMIT_MSG"
```

### CI/CD Hooks

**GitHub Actions integration:**
```yaml
# .github/workflows/quality-gates.yml
name: Quality Gates

on:
  push:
    branches: [main, develop]
  pull_request:
    branches: [main, develop]

env:
  SKU_ID_PREFIX: ${{ github.event.repository.name }}

jobs:
  quality-gates:
    runs-on: ubuntu-latest
    timeout-minutes: 30

    steps:
      - uses: actions/checkout@v3

      - name: Setup Erlang/OTP
        uses: erlef/setup-beam@v1
        with:
          otp-version: '26.2'
          rebar3-version: '3.22'

      - name: Cache Dependencies
        uses: actions/cache@v3
        with:
          path: |
            _build
            ~/.cache/rebar3
          key: ${{ runner.os }}-rebar3-${{ hashFiles('**/rebar.lock') }}
          restore-keys: |
            ${{ runner.os }}-rebar3-

      - name: Generate SKU ID
        id: sku
        run: |
          SKU_ID="${{ env.SKU_ID_PREFIX }}-${{ github.ref_name }}-${{ github.sha }}"
          echo "sku_id=$SKU_ID" >> $GITHUB_OUTPUT

      - name: Run All Quality Gates
        run: |
          rebar3 tcps check-all-gates --sku=${{ steps.sku.outputs.sku_id }}

      - name: Upload Receipts
        if: always()
        uses: actions/upload-artifact@v3
        with:
          name: quality-gate-receipts-${{ github.run_id }}
          path: priv/receipts/*.json
          retention-days: 90

      - name: Upload Coverage
        if: always()
        uses: actions/upload-artifact@v3
        with:
          name: coverage-report-${{ github.run_id }}
          path: _build/test/cover/
          retention-days: 30

      - name: Publish Quality Metrics
        if: github.ref == 'refs/heads/main'
        run: |
          rebar3 tcps quality-metrics --format=json > metrics.json
          # Upload to monitoring system
          curl -X POST https://metrics.company.com/api/v1/quality \
            -H "Authorization: Bearer ${{ secrets.METRICS_TOKEN }}" \
            -d @metrics.json

      - name: Notify on Failure
        if: failure()
        uses: 8398a7/action-slack@v3
        with:
          status: ${{ job.status }}
          text: |
            Quality gates failed for ${{ github.ref_name }}
            SKU: ${{ steps.sku.outputs.sku_id }}
            View logs: ${{ github.server_url }}/${{ github.repository }}/actions/runs/${{ github.run_id }}
          webhook_url: ${{ secrets.SLACK_WEBHOOK }}
```

## Team Permissions

### Role-Based Access Control

**Define roles in rebar.config:**
```erlang
{tcps, [
    {rbac, [
        %% Roles and permissions
        {roles, [
            {developer, [
                run_gates,
                view_receipts,
                view_metrics
            ]},

            {senior_developer, [
                run_gates,
                view_receipts,
                view_metrics,
                override_non_critical  % Can override smoke_test
            ]},

            {tech_lead, [
                run_gates,
                view_receipts,
                view_metrics,
                override_non_critical,
                configure_gates,
                update_baselines
            ]},

            {admin, [
                all  % Full access
            ]}
        ]},

        %% User assignments
        {users, [
            {"alice@company.com", [developer]},
            {"bob@company.com", [senior_developer]},
            {"charlie@company.com", [tech_lead]},
            {"ops@company.com", [admin]}
        ]}
    ]}
]}.
```

**Checking permissions:**
```erlang
% In tcps_quality_gates.erl
check_permission(User, Permission) ->
    case tcps_rbac:has_permission(User, Permission) of
        true -> ok;
        false -> {error, {unauthorized, User, Permission}}
    end.

% Usage
override_gate(Gate, SkuId, User, Reason) ->
    case check_permission(User, override_non_critical) of
        ok ->
            do_override_gate(Gate, SkuId, User, Reason);
        {error, _} = Error ->
            Error
    end.
```

### Audit Logging

**All administrative actions are logged:**
```erlang
% Log override
log_admin_action(#{
    action => gate_override,
    user => "bob@company.com",
    gate => test_execution,
    sku_id => <<"mysku-v1.0.0">>,
    reason => "Production hotfix approved by CTO",
    timestamp => erlang:system_time(millisecond),
    ip_address => "192.168.1.100"
}).

% Log baseline update
log_admin_action(#{
    action => baseline_update,
    user => "charlie@company.com",
    old_threshold => #{test_coverage => 0.80},
    new_threshold => #{test_coverage => 0.85},
    reason => "Team performance improved after training",
    timestamp => erlang:system_time(millisecond)
}).
```

**View audit log:**
```bash
# View all admin actions
rebar3 tcps audit-log

# Filter by user
rebar3 tcps audit-log --user=bob@company.com

# Filter by action
rebar3 tcps audit-log --action=gate_override

# Export for compliance
rebar3 tcps audit-log --format=csv --output=audit-2026-01.csv
```

## Monitoring and Alerts

### Metrics Collection

**Prometheus integration:**
```erlang
% In tcps_quality_gates.erl
-spec collect_prometheus_metrics() -> ok.
collect_prometheus_metrics() ->
    Metrics = get_quality_metrics(),

    % Export metrics
    prometheus_gauge:set(tcps_test_pass_rate,
        maps:get(test_pass_rate, Metrics)),
    prometheus_gauge:set(tcps_test_coverage,
        maps:get(test_coverage, Metrics)),
    prometheus_gauge:set(tcps_defect_rate,
        maps:get(defect_rate, Metrics)),
    prometheus_gauge:set(tcps_first_pass_yield,
        maps:get(first_pass_yield, Metrics)),

    % Per-gate pass rates
    GatePassRates = maps:get(gate_pass_rates, Metrics),
    maps:foreach(fun(Gate, Rate) ->
        prometheus_gauge:set(tcps_gate_pass_rate, [Gate], Rate)
    end, GatePassRates),

    ok.
```

**Prometheus configuration:**
```yaml
# prometheus.yml
scrape_configs:
  - job_name: 'tcps_quality_gates'
    static_configs:
      - targets: ['localhost:9100']
    metrics_path: '/metrics'
    scrape_interval: 60s
```

**Grafana dashboard (dashboard.json):**
```json
{
  "dashboard": {
    "title": "TCPS Quality Gates",
    "panels": [
      {
        "title": "Test Pass Rate",
        "type": "graph",
        "targets": [{
          "expr": "tcps_test_pass_rate"
        }],
        "alert": {
          "conditions": [{
            "type": "query",
            "query": "A",
            "reducer": "last",
            "evaluator": {
              "type": "lt",
              "params": [0.95]
            }
          }]
        }
      },
      {
        "title": "Test Coverage",
        "type": "graph",
        "targets": [{
          "expr": "tcps_test_coverage"
        }]
      },
      {
        "title": "Gate Pass Rates",
        "type": "graph",
        "targets": [{
          "expr": "tcps_gate_pass_rate"
        }]
      },
      {
        "title": "First Pass Yield",
        "type": "singlestat",
        "targets": [{
          "expr": "tcps_first_pass_yield"
        }]
      }
    ]
  }
}
```

### Alerting Rules

**Prometheus alerts (alerts.yml):**
```yaml
groups:
  - name: tcps_quality_gates
    interval: 5m
    rules:
      - alert: TestPassRateLow
        expr: tcps_test_pass_rate < 0.95
        for: 1h
        labels:
          severity: warning
        annotations:
          summary: "Test pass rate below 95%"
          description: "Current: {{ $value | humanizePercentage }}"

      - alert: TestCoverageLow
        expr: tcps_test_coverage < 0.80
        for: 1h
        labels:
          severity: warning
        annotations:
          summary: "Test coverage below 80%"
          description: "Current: {{ $value | humanizePercentage }}"

      - alert: DefectRateHigh
        expr: tcps_defect_rate > 0.05
        for: 30m
        labels:
          severity: critical
        annotations:
          summary: "Defect rate above 5%"
          description: "Current: {{ $value | humanizePercentage }}"

      - alert: GateFailureSpike
        expr: rate(tcps_gate_failures_total[5m]) > 0.1
        for: 15m
        labels:
          severity: warning
        annotations:
          summary: "High gate failure rate detected"
          description: "{{ $value | humanize }} failures/minute"
```

**PagerDuty integration:**
```yaml
# alertmanager.yml
receivers:
  - name: 'pagerduty-critical'
    pagerduty_configs:
      - service_key: '<pagerduty-integration-key>'
        severity: critical
        description: '{{ .CommonAnnotations.summary }}'

route:
  receiver: 'pagerduty-critical'
  group_by: ['alertname']
  routes:
    - match:
        severity: critical
      receiver: 'pagerduty-critical'
```

## Backup and Recovery

### Receipt Chain Backup

**Automated backup:**
```bash
#!/bin/bash
# scripts/backup-receipts.sh

RECEIPTS_DIR="priv/receipts"
BACKUP_BUCKET="s3://company-receipts-backup"
DATE=$(date +%Y%m%d)

# Backup to S3
aws s3 sync $RECEIPTS_DIR $BACKUP_BUCKET/$DATE/ \
    --storage-class STANDARD_IA \
    --sse AES256

# Verify backup
aws s3 ls $BACKUP_BUCKET/$DATE/ | wc -l > /tmp/backup-count.txt

# Log backup
echo "$(date): Backed up $(cat /tmp/backup-count.txt) receipts to $BACKUP_BUCKET/$DATE/" \
    >> logs/backup.log
```

**Cron schedule:**
```
# Backup receipts daily at 2 AM
0 2 * * * /path/to/erlmcp/scripts/backup-receipts.sh
```

### Receipt Chain Recovery

**Restore from backup:**
```bash
#!/bin/bash
# scripts/restore-receipts.sh

BACKUP_BUCKET="s3://company-receipts-backup"
RECEIPTS_DIR="priv/receipts"
DATE=${1:-$(date +%Y%m%d)}

# Restore from S3
aws s3 sync $BACKUP_BUCKET/$DATE/ $RECEIPTS_DIR/ \
    --exclude "*" \
    --include "*.json"

# Verify receipt chain
rebar3 tcps verify-receipt-chain --all

echo "Restored receipts from $BACKUP_BUCKET/$DATE/"
```

## Performance Tuning

### Gate Execution Optimization

**Parallel gate execution (experimental):**
```erlang
% rebar.config
{tcps, [
    {quality_gates, [
        {parallel_execution, true},
        {max_parallel_gates, 4},

        % Independent gates can run in parallel
        {parallel_groups, [
            [compilation, shacl_validation],  % Group 1
            [test_execution, security_scan],  % Group 2 (after Group 1)
            [deterministic_build],            % Group 3 (after Group 2)
            [quality_metrics, release_verification, smoke_test]  % Group 4
        ]}
    ]}
]}.
```

**Caching strategies:**
```erlang
% Cache gate results between runs
{quality_gates, [
    {cache_strategy, smart},  % Options: none, simple, smart

    % Smart caching checks file hashes
    {cache_invalidation, [
        {compilation, [
            watch_files => ["src/**/*.erl", "include/**/*.hrl"],
            watch_config => ["rebar.config", "rebar.lock"]
        ]},
        {test_execution, [
            watch_files => ["src/**/*.erl", "test/**/*.erl"],
            watch_config => ["rebar.config"]
        ]}
    ]}
]}.
```

### Resource Limits

**Configure resource limits:**
```erlang
{quality_gates, [
    {resource_limits, #{
        max_memory_mb => 4096,
        max_cpu_percent => 80,
        max_execution_time_ms => 600000  % 10 minutes
    }}
]}.
```

## Troubleshooting

### Common Admin Issues

**Issue: Gates timing out**
```bash
# Increase timeout
# Edit rebar.config:
{gates, [
    {test_execution, #{
        timeout => 600000  % 10 minutes (was 5)
    }}
]}

# Or set environment variable
export TCPS_GATE_TIMEOUT=600000
```

**Issue: Receipt chain broken**
```bash
# Verify chain
rebar3 tcps verify-receipt-chain --sku=mysku-v1.0.0

# Rebuild chain from backup
scripts/restore-receipts.sh 20260127
rebar3 tcps rebuild-receipt-chain --sku=mysku-v1.0.0
```

**Issue: High gate failure rate**
```bash
# Analyze failures
rebar3 tcps analyze-failures --last-7-days

# Common causes:
# - Flaky tests → Fix test reliability
# - Environment issues → Fix infrastructure
# - Recent code changes → Review and fix
```

## Support

### Documentation
- [User Guide](USER_GUIDE.md) - Developer usage
- [Architecture](ARCHITECTURE.md) - System design
- [FAQ](FAQ.md) - Common questions

### Admin Tools
```bash
# System health check
rebar3 tcps health-check

# Performance diagnostics
rebar3 tcps diagnose --verbose

# Export configuration
rebar3 tcps config-export --output=tcps-config.json
```

### Contact
- Admin Slack: #erlmcp-quality-gates-admin
- Email: admin@erlmcp.dev
- Escalation: ops-oncall@company.com

---

**Version History:**
- v1.0.0 (2026-01-28): Initial admin guide
