# FM-12 Quick Reference

## File Locations

### Source Modules
```
apps/erlmcp_validation/src/
├── erlmcp_vulnerability_scanner.erl    (507 lines) - Main CVE scanner
└── erlmcp_dependency_management.erl    (287 lines) - Dependency management
```

### Test Suite
```
apps/erlmcp_validation/test/
└── erlmcp_dependency_audit_SUITE.erl   (686 lines) - 24 tests
```

### Scripts
```
scripts/security/
└── scan_dependencies.sh                (309 lines) - CI/CD script
```

### Workflows
```
.github/workflows/
└── dependency-audit.yml                (214 lines) - GitHub Actions
```

## Quick Commands

### Run Dependency Scan
```bash
# Local scan
./scripts/security/scan_dependencies.sh

# CI mode (blocks on Critical/High)
./scripts/security/scan_dependencies.sh --ci-mode

# Custom options
./scripts/security/scan_dependencies.sh \
  --lock-file ./rebar.lock \
  --cache-dir /tmp/nvd_cache \
  --output-dir ./reports
```

### Run Tests
```bash
# All dependency audit tests
rebar3 ct --suite=erlmcp_dependency_audit_SUITE

# Specific test group
rebar3 ct --suite=erlmcp_dependency_audit_SUITE --group=cve_detection

# Single test
rebar3 ct --suite=erlmcp_dependency_audit_SUITE --case=test_parse_rebar_lock
```

### Erlang API
```erlang
%% Scan all dependencies
{ok, Results} = erlmcp_vulnerability_scanner:scan_dependencies("rebar.lock").

%% Scan with options
{ok, Results} = erlmcp_vulnerability_scanner:scan_dependencies("rebar.lock", #{
    direct_only => false,
    cache_dir => "/tmp/nvd_cache"
}).

%% Query specific dependency
Dep = #{name => <<"jose">>, version => <<"1.11.1">>, package_type => pkg},
{ok, CVEs} = erlmcp_vulnerability_scanner:query_nvd(Dep, #{}).

%% Classify CVE
CVE = #{cve_id => <<"CVE-2023-XXXXX">>, cvss_score => 8.1, severity => high},
Classification = erlmcp_vulnerability_scanner:classify_cve(CVE).

%% Generate JSON report
{ok, JSON} = erlmcp_vulnerability_scanner:generate_json_report(Results).

%% Save audit trail
ok = erlmcp_vulnerability_scanner:save_audit_trail(Results, "reports/audit.json").
```

## Severity Classification

| CVSS Score | Severity | Action | CI Behavior |
|------------|----------|--------|-------------|
| ≥ 9.0      | Critical | Block  | Exit 1 (fail build) |
| 7.0 - 8.9  | High     | Block  | Exit 1 (fail build) |
| 4.0 - 6.9  | Medium   | Warn   | Exit 0 (pass, but warn) |
| < 4.0      | Low      | Info   | Exit 0 (pass) |

## Environment Variables

```bash
# NVD API key (optional, increases rate limit from 5/30s to 50/30s)
export NVD_API_KEY="your-api-key-here"

# Cache directory (default: /tmp/nvd_cache)
export CACHE_DIR="/custom/cache/path"

# Output directory (default: ./reports)
export OUTPUT_DIR="/custom/reports/path"
```

## Report Structure

### JSON Report
```json
{
  "scanned_at": 1738368000,
  "lock_file": "./rebar.lock",
  "dependencies_scanned": 22,
  "vulnerabilities_found": 2,
  "critical": 0,
  "high": 1,
  "medium": 1,
  "low": 0,
  "overall_severity": "high",
  "overall_action": "block",
  "exit_code": 1,
  "vulnerable_dependencies": [
    {
      "dependency": "jose",
      "version": "1.11.1",
      "cves": [
        {
          "cve_id": "CVE-2023-50967",
          "cvss_score": 8.1,
          "severity": "high",
          "description": "JWT signature verification bypass"
        }
      ]
    }
  ]
}
```

## GitHub Actions Integration

### Workflow Triggers
- Push to main/develop/release branches
- Pull requests to main/develop
- Weekly schedule (Mondays 00:00 UTC)
- Manual dispatch

### PR Comment Format
```markdown
## ✅/❌ Dependency Security Audit (FM-12)

**Status:** PASSED/BLOCKED

### Summary
- Dependencies scanned: 22
- Vulnerabilities found: 2

### Severity Breakdown
- 🔴 Critical: 0
- 🟠 High: 1
- 🟡 Medium: 1
- 🟢 Low: 0

### Vulnerable Dependencies
| Dependency | Version | CVEs |
|------------|---------|------|
| jose       | 1.11.1  | 1    |

### ⚠️ Action Required
This PR is **BLOCKED** due to High severity vulnerabilities.
Please update the affected dependencies before merging.
```

## Test Coverage

### Test Groups (24 tests total)

1. **CVE Detection** (8 tests)
   - Parse rebar.lock
   - Extract dependencies
   - Query NVD
   - Parse CVE data
   - Handle errors
   - Multiple CVEs
   - Transitive dependencies

2. **Severity Classification** (6 tests)
   - Critical blocking
   - High blocking
   - Medium warning
   - Low info
   - Multiple CVEs (highest)
   - Override mechanism

3. **Dependency Management** (6 tests)
   - Freshness checking
   - Outdated detection
   - Safe version recommendation
   - Vulnerable version tracking
   - Transitive analysis
   - Update prioritization

4. **Report Generation** (4 tests)
   - JSON structure
   - Human-readable summary
   - CI integration format
   - Audit trail persistence

## Troubleshooting

### Issue: "NVD API rate limit exceeded"
**Solution**: Set NVD_API_KEY environment variable or wait 30 seconds

### Issue: "No vulnerabilities found but package is known vulnerable"
**Solution**: 
- Check NVD cache is up to date
- Clear cache: `rm -rf /tmp/nvd_cache`
- Rerun scan

### Issue: "Scan takes too long"
**Solution**: 
- Use cache directory (enabled by default)
- Run with `--direct-only` to skip transitive dependencies
- Set NVD_API_KEY for higher rate limits

### Issue: "False positive - CVE not applicable"
**Solution**: Use override mechanism in code:
```erlang
Override = #{
    cve_id => <<"CVE-2023-XXXXX">>,
    override_severity => low,
    override_action => info,
    reason => <<"Not applicable: we don't use affected feature">>
}
```

## Integration with Dependabot

FM-12 complements GitHub Dependabot:

| Feature | Dependabot | FM-12 |
|---------|------------|-------|
| Automated PRs | ✅ Yes | ❌ No |
| CVE detection | ✅ Yes | ✅ Yes |
| CI blocking | ❌ No | ✅ Yes |
| CVSS scoring | ❌ No | ✅ Yes |
| Custom thresholds | ❌ No | ✅ Yes |
| Audit trail | ❌ No | ✅ Yes |

**Recommendation**: Enable both for complete coverage

## Links

- [NVD API Documentation](https://nvd.nist.gov/developers/vulnerabilities)
- [CVSS Calculator](https://www.first.org/cvss/calculator/3.1)
- [GitHub Dependabot Docs](https://docs.github.com/en/code-security/dependabot)
- [Hex.pm API](https://hex.pm/docs/api)

