# TCPS TPM (Total Productive Maintenance)

## Overview

The TCPS TPM module implements Toyota Production System's Total Productive Maintenance philosophy for code production. It provides self-maintenance, health monitoring, and automated quality assurance for the TAIEA system.

TPM ensures continuous system health through scheduled maintenance, proactive issue detection, and automated remediation with full receipt generation for audit trails.

## Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                     TCPS TPM System                          │
├─────────────────────────────────────────────────────────────┤
│                                                               │
│  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐      │
│  │   Scheduled  │  │   Health     │  │   Receipt    │      │
│  │  Maintenance │  │  Monitoring  │  │  Generation  │      │
│  └──────────────┘  └──────────────┘  └──────────────┘      │
│                                                               │
│  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐      │
│  │   Template   │  │  Dependency  │  │  Ontology    │      │
│  │    Checks    │  │    Checks    │  │  Validation  │      │
│  └──────────────┘  └──────────────┘  └──────────────┘      │
│                                                               │
│  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐      │
│  │ Deterministic│  │   Auto-Fix   │  │    Andon     │      │
│  │    Builds    │  │   System     │  │   Triggers   │      │
│  └──────────────┘  └──────────────┘  └──────────────┘      │
│                                                               │
└─────────────────────────────────────────────────────────────┘
```

## Features

### 1. Scheduled Maintenance Tasks

TPM runs maintenance tasks on configurable schedules:

- **Daily**: SHACL validation, template syntax checks
- **Weekly**: Dependency analysis (outdated, vulnerable, unused)
- **Monthly**: Deterministic build verification
- **Quarterly**: Full system audit

### 2. Template Health Checks

Validates all Tera templates for:
- Syntax errors (unclosed braces, invalid expressions)
- Undefined variables
- Missing filters/functions
- File accessibility

**Example:**
```erlang
% Check all templates
Result = tcps_tpm:check_templates(),

% Result format:
[
    {<<"/path/to/template.tera">>, [
        #{type => syntax_error,
          severity => critical,
          description => <<"Mismatched template braces">>,
          location => <<"/path/to/template.tera">>,
          detected_at => 1706234567890}
    ]}
]
```

### 3. Dependency Management

Analyzes `rebar.config` dependencies:

- **Outdated**: Newer versions available
- **Vulnerable**: Known CVE vulnerabilities
- **Unused**: Not referenced in codebase

**Example:**
```erlang
Result = tcps_tpm:check_dependencies(),

% Result format:
#{
    outdated => [
        #{name => jsx, current => "3.0.0", latest => "3.1.0"}
    ],
    vulnerable => [
        #{name => old_lib, cve => "CVE-2023-12345"}
    ],
    unused => [
        #{name => unused_dep}
    ]
}
```

### 4. Deterministic Build Verification

Rebuilds SKUs twice from scratch and compares artifacts byte-by-byte:

**Example:**
```erlang
Result = tcps_tpm:verify_deterministic_build(<<"sku-001">>),

% Success:
{ok, deterministic}

% Failure (triggers Andon):
{error, {non_deterministic, #{
    build1_checksum => <<"abc123...">>,
    build2_checksum => <<"def456...">>,
    difference => <<"Checksums do not match">>
}}}
```

### 5. SHACL Ontology Validation

Validates ontology files against SHACL shapes:

**Example:**
```erlang
Result = tcps_tpm:validate_ontology(),

% Clean ontology:
{ok, clean}

% Violations found:
{error, {violations, [
    #{type => invalid_ttl,
      severity => critical,
      file => <<"ontology/work_orders.ttl">>,
      reason => missing_prefix_declarations}
]}}
```

### 6. Self-Healing Auto-Fix

Attempts automatic remediation for known issues:

**Fixable Issues:**
- Undefined variables → Replace with defaults
- Missing prefixes → Add standard prefixes
- Outdated dependencies → Update `rebar.config`

**Human Review Required:**
- Syntax errors
- Missing files
- Complex structural issues

**Example:**
```erlang
Issue = #{
    type => undefined_variable,
    severity => medium,
    description => <<"Template uses undefined variable">>,
    location => <<"/path/to/template.tera">>,
    detected_at => 1706234567890
},

Result = tcps_tpm:auto_fix(Issue),

% Success:
{fixed, #{
    timestamp => 1706234567890,
    operation => fix_undefined_variable,
    status => success,
    details => #{action => <<"Replaced with defaults">>},
    checksum => <<"abc123...">>
}}

% Needs human:
{needs_human, <<"Syntax errors require manual inspection">>}
```

### 7. Health Scoring

Calculates system health (0-100) based on issue severity:

- **Critical**: -20 points
- **High**: -10 points
- **Medium**: -5 points
- **Low**: -2 points
- **Info**: -0.5 points

**Thresholds:**
- 90-100: Excellent (Green)
- 75-89: Good (Yellow)
- 50-74: Fair (Yellow)
- 25-49: Poor (Red)
- 0-24: Critical (Red)

### 8. Maintenance Dashboard

Real-time view of system health:

```erlang
Dashboard = tcps_tpm:get_dashboard(),

#{
    last_maintenance => #{
        daily => <<"2026-01-26 14:30:00 UTC">>,
        weekly => <<"2026-01-24 14:30:00 UTC">>,
        monthly => <<"2026-01-01 14:30:00 UTC">>,
        quarterly => <<"2025-12-01 14:30:00 UTC">>
    },
    health_score => 87.5,
    issues_count => 3,
    issues_by_severity => #{
        critical => 0,
        high => 1,
        medium => 2,
        low => 0,
        info => 0
    },
    auto_fixes_count => 5,
    maintenance_running => false,
    issues_sample => [...]
}
```

## CLI Interface

### Starting the System

```erlang
% Start TAIEA (includes TPM)
application:start(taiea_core).

% TPM starts automatically under taiea_core_sup
```

### Manual Operations

```erlang
% Show help
tcps_tpm_cli:help().

% Check system health
tcps_tpm_cli:check_health().

% View dashboard
tcps_tpm_cli:show_dashboard().

% Run maintenance
tcps_tpm_cli:maintenance(daily).
tcps_tpm_cli:maintenance(weekly).
tcps_tpm_cli:maintenance(monthly).
tcps_tpm_cli:maintenance(quarterly).

% Individual checks
tcps_tpm_cli:check_templates().
tcps_tpm_cli:check_dependencies().
tcps_tpm_cli:validate_ontology().

% Verify deterministic build
tcps_tpm_cli:verify_build(<<"sku-001">>).

% Auto-fix issues
tcps_tpm_cli:fix_issues().
```

### Example Session

```erlang
1> tcps_tpm_cli:check_health().

=== TCPS Health Check ===

Health Score: 92.50/100.0 [EXCELLENT]

✓ System is healthy

ok

2> tcps_tpm_cli:show_dashboard().

=== TCPS TPM Dashboard ===

Health Score: 92.50/100.0

Last Maintenance Runs:
  Daily:     2026-01-26 14:30:00 UTC
  Weekly:    2026-01-24 14:30:00 UTC
  Monthly:   2026-01-01 14:30:00 UTC
  Quarterly: 2025-12-01 14:30:00 UTC

Issues Found: 2
  Critical: 0
  High:     0
  Medium:   2
  Low:      0
  Info:     0

Auto-fixes Applied: 5

Recent Issues (showing up to 10):
  [MEDIUM] undefined_variable: Template contains undefined_* variables
      Location: /path/to/template.tera

ok

3> tcps_tpm_cli:check_templates().

=== Checking Templates ===

Found issues in 1 template(s):

Template: /path/to/bad_template.tera
  [critical] Mismatched template braces

ok

4> tcps_tpm_cli:fix_issues().

=== Auto-fixing Issues ===

Attempting to auto-fix 2 issue(s)...

Fixing undefined_variable... FIXED
  Receipt: YWJjMTIzLi4u
Fixing syntax_error... NEEDS HUMAN
  Reason: Syntax errors require manual inspection

Results:
  Fixed automatically: 1
  Needs human review:  1

ok
```

## Configuration

Edit `taiea/config/sys.config`:

```erlang
{taiea_core, [
    {tpm, [
        % Enable/disable auto-fix
        {enable_auto_fix, true},

        % Directory paths
        {receipts_dir, "receipts/tpm"},
        {ontology_dir, "ontology"},
        {shapes_dir, "shapes"},
        {templates_dir, "templates"},

        % Maintenance intervals (milliseconds)
        {daily_interval, 86400000},      % 24 hours
        {weekly_interval, 604800000},    % 7 days
        {monthly_interval, 2592000000},  % 30 days
        {quarterly_interval, 7776000000}, % 90 days

        % Tasks by frequency
        {daily_tasks, [validate_ontology, check_templates]},
        {weekly_tasks, [check_dependencies]},
        {monthly_tasks, [verify_deterministic_build]},
        {quarterly_tasks, [full_audit]},

        % Auto-fix rules
        {auto_fix_rules, [
            {undefined_variable, enabled},
            {missing_prefix, enabled},
            {outdated_dependency, enabled},
            {syntax_error, disabled}
        ]}
    ]}
]}
```

## Receipt Generation

All TPM operations generate receipts in `receipts/tpm/`:

**Receipt Format:**
```json
{
    "timestamp": 1706234567890,
    "operation": "check_templates",
    "result": [...],
    "context": {...},
    "checksum": "YWJjMTIzLi4u"
}
```

**Andon Receipts** (for critical failures):
```json
{
    "type": "non_deterministic_build",
    "sku_id": "sku-001",
    "diff": {
        "build1_checksum": "abc123...",
        "build2_checksum": "def456...",
        "difference": "Checksums do not match"
    }
}
```

## Integration with TCPS

TPM is a core pillar of TCPS (Toyota Code Production System):

### Pillar 3.8 - TPM (Total Productive Maintenance)

From `docs/TCPS.md`:

> **3.8 TPM (Total Productive Maintenance)**
> - Scheduled regeneration drills, template/query linting, dependency-rot detection, deterministic rebuild checks.
> - **Rule:** Maintenance is part of the production line.

TPM ensures:
1. **Continuous Quality**: Daily/weekly/monthly checks catch issues early
2. **Deterministic Outputs**: Build verification ensures repeatability
3. **Automated Remediation**: Self-healing reduces manual intervention
4. **Full Traceability**: Receipts provide audit trail
5. **Stop-the-Line Authority**: Andon triggers block releases on critical issues

## Testing

### Run Tests

```bash
# Unit tests
cd taiea/apps/taiea_core
rebar3 eunit

# With coverage
rebar3 do eunit, cover

# Coverage report
rebar3 cover -v
```

### Test Coverage

The test suite includes:
- Unit tests for all public APIs
- Integration tests for full workflows
- Property-based tests (if PropEr available)
- Performance benchmarks
- Error handling and edge cases

**Target: 80%+ code coverage** (Lean Six Sigma standard)

## Performance

### SLO Targets

- **Template checks**: <5 seconds
- **Dependency checks**: <3 seconds
- **Ontology validation**: <10 seconds
- **Deterministic build**: <120 seconds (2 builds + comparison)
- **Health score calculation**: <100ms
- **Dashboard generation**: <200ms

### Scalability

TPM is designed to handle:
- 1000+ templates
- 100+ dependencies
- 50+ ontology files
- 10+ SKUs per verification cycle

## Troubleshooting

### Issue: Maintenance not running

**Check:**
```erlang
% Is TPM process alive?
whereis(tcps_tpm).

% Check logs
tail -f logs/taiea.log | grep TPM
```

**Solution:** Restart taiea_core application

### Issue: Templates not found

**Check:**
```erlang
% Verify templates directory
file:list_dir("templates").
```

**Solution:** Update `templates_dir` in sys.config

### Issue: Receipts not generated

**Check:**
```erlang
% Verify receipts directory exists
filelib:ensure_dir("receipts/tpm/dummy").
```

**Solution:** Create directory with proper permissions

### Issue: High health score despite issues

**Check:**
```erlang
Dashboard = tcps_tpm:get_dashboard(),
maps:get(issues_by_severity, Dashboard).
```

**Solution:** Run `tcps_tpm_cli:fix_issues()` to resolve

## Best Practices

### 1. Regular Monitoring

```erlang
% Daily: Check health
tcps_tpm_cli:check_health().

% Weekly: Review dashboard
tcps_tpm_cli:show_dashboard().

% Monthly: Full audit
tcps_tpm_cli:maintenance(monthly).
```

### 2. Proactive Maintenance

- Schedule quarterly audits
- Review Andon receipts immediately
- Fix issues before they accumulate
- Keep dependencies updated

### 3. Receipt Management

- Archive old receipts periodically
- Use receipts for compliance audits
- Link receipts to work orders
- Generate reports from receipt data

### 4. Custom Maintenance Tasks

Extend TPM with custom tasks:

```erlang
% In config
{daily_tasks, [validate_ontology, check_templates, custom_task]}.

% Implement in tcps_tpm.erl
execute_task(custom_task, State) ->
    % Your custom maintenance logic
    State.
```

## Future Enhancements

### Planned Features

1. **Machine Learning**: Predict maintenance needs based on patterns
2. **Cloud Integration**: Push receipts to cloud storage
3. **Alerting**: Email/Slack notifications for critical issues
4. **Metrics Dashboard**: Web UI for real-time monitoring
5. **Advanced SHACL**: Full SHACL engine integration
6. **CVE Database**: Real-time vulnerability scanning
7. **Dependency Graph**: Visualize dependency relationships
8. **A/B Testing**: Compare build strategies

## References

- [TCPS Documentation](TCPS.md) - Toyota Code Production System
- [TCPS Checklist](TCPS-checklist.md) - Validation gates
- [TCPS Certification](TCPS-certification.md) - Certification program

## Support

For issues or questions:
1. Check logs: `logs/taiea.log`
2. Review receipts: `receipts/tpm/`
3. Run diagnostics: `tcps_tpm_cli:show_dashboard()`
4. Open issue on GitHub

---

**Remember: TPM is not optional. Maintenance is part of the production line.**
