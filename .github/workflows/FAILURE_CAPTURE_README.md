# CI/CD Failure Capture Infrastructure

## Overview

This document describes the automated failure capture system integrated into erlmcp's CI/CD pipelines. When any quality gate fails, the system automatically captures comprehensive diagnostics and generates exact reproduction commands for local debugging.

## Architecture

### Shared Action

**Location**: `.github/actions/capture-failure/action.yml`

A composite GitHub Action that provides unified failure capture across all workflows. Designed following the DRY principle - write once, use everywhere.

### Supported Failure Types

The action handles 8 distinct failure categories:

1. **compile** - Compilation errors
2. **eunit** - Unit test failures and coverage issues
3. **ct** - Common Test integration failures
4. **dialyzer** - Type checking warnings
5. **xref** - Cross-reference undefined functions
6. **benchmark** - Performance regression
7. **validation** - Protocol compliance failures
8. **security** - Security scan issues

## Integration Points

### CI Workflow (ci.yml)

**Failure Capture Points**: 7

- Compilation failure (line 64)
- Xref analysis failure (line 130)
- Dialyzer type checking failure (line 157)
- EUnit test failure (line 183)
- Common Test failure (line 206)
- Coverage threshold failure (line 243)
- Benchmark smoke test failure (line 302)

**Command Example**:
```bash
# Quick EUnit reproduction
git checkout <commit-sha>
rebar3 eunit --cover --verbose
./scripts/check_coverage_threshold.sh 80
```

### MCP Spec Compliance Workflow (mcp-spec-compliance.yml)

**Failure Capture Points**: 9

- Compilation (line 73) - all 4 apps
- EUnit tests (line 166)
- Coverage threshold (line 217)
- Common Test suites (line 271)
- Dialyzer type analysis (line 342)
- Xref cross-reference (line 416)
- Spec compliance validation (line 494)
- Security vulnerability scan (line 576)
- Performance check (line 682)

**Command Example**:
```bash
# Spec compliance reproduction
git checkout <commit-sha>
rebar3 ct --suite=apps/erlmcp_validation/test/erlmcp_protocol_validator_SUITE
```

### Performance Regression Workflow (performance-regression.yml)

**Failure Capture Points**: 2

- Performance test suite failure (line 109)
- Baseline comparison failure (line 283)

**Command Example**:
```bash
# Performance regression reproduction
git checkout <commit-sha>
TERM=dumb rebar3 compile
rebar3 ct --suite=apps/erlmcp_validation/test/erlmcp_performance_validator_SUITE
./tools/baseline-compare.sh --threshold 10 --workload <workload-id>
```

## Artifact Structure

When a failure occurs, the system creates a structured artifact bundle:

```
reports/failure-<FAILURE_ID>/
├── REPRODUCER.sh              # Executable reproduction script
├── environment.txt            # Build environment details
├── compile_output.txt         # (compile failures)
├── eunit.coverdata           # (test failures)
├── ct_logs/                  # (CT failures)
├── dialyzer_output.txt       # (dialyzer failures)
├── xref_output.txt           # (xref failures)
├── benchmark_results.txt     # (performance failures)
├── processes.txt             # System state snapshot
├── memory.txt                # Memory usage
└── disk.txt                  # Disk usage
```

**Failure ID Format**: `YYYYMMDD-HHMMSS-<type>-<run-number>`

**Example**: `20260201-143022-eunit-1234`

## Reproduction Workflow

### Automatic (Recommended)

1. **Download artifact** from GitHub Actions run
2. **Extract and execute**:
   ```bash
   unzip failure-<ID>.zip
   cd reports/failure-<ID>
   ./REPRODUCER.sh
   ```

### Manual (Without Artifact)

The failure output prints quick reproduction commands:

```
=== REPRODUCER FOR eunit FAILURE ===

Step 1: Checkout the failing commit
git checkout a1b2c3d4

Step 2: Run specific test suite
rebar3 eunit --module=erlmcp_client_tests --verbose

Step 3: Run with coverage
rebar3 eunit --module=erlmcp_client_tests --cover --cover_export_name=coverage
```

## Features

### 1. Contextual Artifact Capture

Each failure type captures relevant artifacts:

- **Compile**: BEAM files, .app files, compilation errors
- **Tests**: Coverage data, test logs, failed test details
- **Dialyzer**: PLT files, warning details
- **Benchmark**: Performance data, baselines, metrics
- **Validation**: Protocol compliance reports, evidence bundles

### 2. Environment Snapshot

Every failure captures:
- OTP version
- Rebar3 version
- Git SHA and ref
- Workflow context
- System resources (processes, memory, disk)

### 3. Smart Reproduction Commands

Type-specific commands that match actual failure context:

```bash
# EUnit with specific module
rebar3 eunit --module=<MODULE> --verbose

# Dialyzer with fresh PLT
rm -rf _build/default/rebar3_*_plt
rebar3 dialyzer --build_plt --get_warnings

# Benchmark with specific workload
./tools/baseline-compare.sh --threshold 10 --workload core_ops_10k
```

### 4. GitHub Integration

- **Step Summary**: Inline reproduction guide in workflow UI
- **Artifacts**: 30-day retention for all failure bundles
- **PR Comments**: Auto-comment on PRs with failure details (compliance workflow)

## Configuration

### Artifact Retention

Default: **30 days**

Modify in workflow files:
```yaml
- name: Upload Failure Artifacts
  uses: actions/upload-artifact@v4
  with:
    retention-days: 30  # Adjust as needed
```

### Failure ID Format

Default: Timestamp-based

Override with custom ID:
```yaml
- uses: ./.github/actions/capture-failure
  with:
    failure-type: eunit
    otp-version: ${{ matrix.otp_version }}
    failure-id: custom-identifier-123
```

### Test Suite Specification

For targeted reproduction:
```yaml
- uses: ./.github/actions/capture-failure
  with:
    failure-type: eunit
    test-suite: erlmcp_client_tests  # Specific module
```

### Benchmark Workload Tracking

For performance failures:
```yaml
- uses: ./.github/actions/capture-failure
  with:
    failure-type: benchmark
    workload-id: core_ops_100k  # Specific workload
```

## Usage Patterns

### Developer Local Reproduction

1. **CI failure notification** → Check GitHub Actions
2. **Download failure artifact** → `failure-<ID>.zip`
3. **Run reproducer**:
   ```bash
   ./REPRODUCER.sh
   ```
4. **Fix issue** → Commit → Push
5. **Verify** → CI re-runs with fix

### QA Verification

1. **Extract environment**:
   ```bash
   cat reports/failure-<ID>/environment.txt
   ```
2. **Match environment**:
   - Same OTP version
   - Same rebar3 version
   - Same commit
3. **Run reproduction script**
4. **Verify fix** in artifact bundle

### Performance Debugging

1. **Download performance artifacts**:
   - `performance-regression-logs`
   - `baseline-comparison-report`
2. **Compare baselines**:
   ```bash
   cat reports/failure-<ID>/baselines.json
   ```
3. **Run specific workload**:
   ```bash
   erlmcp_bench_core_ops:run(<<"core_ops_100k">>)
   ```

## Best Practices

### 1. Always Use Artifacts

Download the full artifact bundle rather than manually reconstructing the environment. The artifact contains exact state snapshots.

### 2. Check Environment First

Before reproduction:
```bash
cat environment.txt
# Verify:
# - OTP version matches
# - Git SHA matches
# - Rebar3 version matches
```

### 3. Incremental Debugging

Start with the quick reproduction commands, then drill down into specific artifacts if needed.

### 4. Preserve Failure Context

If investigating recurring failures, keep failure IDs in issue tracker:
```markdown
## Failure History
- 20260201-143022-eunit-1234
- 20260202-091455-eunit-1567
- 20260203-151203-eunit-1789
```

### 5. Link to CI Runs

Always include GitHub Actions run link:
```
https://github.com/seanchatmangpt/erlmcp/actions/runs/<run-id>
```

## Troubleshooting

### Reproducer Script Fails

**Issue**: `./REPRODUCER.sh` exits with error

**Solution**:
1. Check environment matches:
   ```bash
   erl -eval 'erlang:display(erlang:system_info(otp_release)), halt().'
   ```
2. Verify rebar3 version:
   ```bash
   rebar3 --version
   ```
3. Check git state:
   ```bash
   git log -1 --oneline
   ```

### Missing Artifacts

**Issue**: Expected files not in artifact bundle

**Solution**:
- Check `if: always()` is set on upload step
- Verify artifact path exists before upload
- Check GitHub Actions storage quota

### Reproduction Works Locally But Fails in CI

**Issue**: Tests pass locally with reproducer but fail in CI

**Solution**:
- Compare system resources (CI has limited memory/CPU)
- Check for non-deterministic tests
- Verify no reliance on local filesystem state
- Review CI-specific environment variables

## Maintenance

### Adding New Failure Types

1. **Update action.yml**:
   ```yaml
   # Add new case in artifact capture
   case "${FAILURE_TYPE}" in
     mynewtype)
       # Capture specific artifacts
       ;;
   esac
   ```

2. **Add reproduction commands**:
   ```yaml
   # Add reproduction steps
   cat >> "${ARTIFACT_DIR}/REPRODUCER.sh" << 'REPRO_EOF'
   echo "Step 1: ..."
   REPRO_EOF
   ```

3. **Integrate into workflows**:
   ```yaml
   - name: Capture mynewtype failure
     if: failure() && steps.mystep.outcome == 'failure'
     uses: ./.github/actions/capture-failure
     with:
       failure-type: mynewtype
   ```

### Extending Artifact Collection

Add to action.yml artifact capture:
```bash
# Custom artifact for specific failure
if [ "${FAILURE_TYPE}" = "eunit" ]; then
  cp my_custom_log.txt "${ARTIFACT_DIR}/"
fi
```

## Metrics

### Artifact Size

Typical artifact sizes:
- Compile failure: ~10 MB
- Test failure: ~50 MB (includes coverage)
- Performance failure: ~100 MB (includes baselines)

### Storage Usage

With 30-day retention:
- Average 5 failures/day = 150 failures/month
- Average 30 MB/artifact = 4.5 GB/month
- Well within GitHub's storage limits

### Time to Reproduce

Estimated local reproduction times:
- Compile failure: <2 min
- Test failure: 5-10 min
- Performance failure: 10-15 min
- Full validation: 15-20 min

## References

### Workflows Using Failure Capture

- `.github/workflows/ci.yml` (7 integration points)
- `.github/workflows/mcp-spec-compliance.yml` (9 integration points)
- `.github/workflows/performance-regression.yml` (2 integration points)

### Related Scripts

- `scripts/check_coverage_threshold.sh` - Coverage validation
- `tools/baseline-compare.sh` - Performance comparison
- `tools/baseline-capture.sh` - Baseline creation

### Documentation

- `CLAUDE.md` - System specification and TPS integration
- `DEVELOPMENT.md` - Developer environment setup
- `CODE_QUALITY_REPORT_V2.1.md` - Quality metrics

## Examples

### Example 1: Compile Failure

**Failure ID**: `20260201-143022-compile-1234`

**Artifact Contents**:
```
reports/failure-20260201-143022-compile-1234/
├── REPRODUCER.sh
├── environment.txt
├── compile_output.txt          # Full compilation error output
├── rebar.config                # Project configuration
├── rebar.lock                  # Locked dependencies
├── *.beam                      # Successfully compiled modules
└── *.app.src                   # Application source files
```

**Reproduction**:
```bash
./REPRODUCER.sh
# Step 1: Clean build directory
# Step 2: Run compilation
# Step 3: Verify all apps compiled
```

### Example 2: EUnit Test Failure

**Failure ID**: `20260201-151203-eunit-1567`

**Artifact Contents**:
```
reports/failure-20260201-151203-eunit-1567/
├── REPRODUCER.sh
├── environment.txt
├── eunit.coverdata             # Coverage data
├── failed_tests.txt            # Failed test details
├── cover/                      # Coverage reports
│   ├── index.html
│   └── erlmcp_client.COVER.html
└── logs/                       # Test output logs
```

**Reproduction**:
```bash
./REPRODUCER.sh
# Step 1: Run specific test suite
# Step 2: Run with coverage
# Step 3: Generate coverage report
```

### Example 3: Performance Regression

**Failure ID**: `20260201-161345-benchmark-1789`

**Artifact Contents**:
```
reports/failure-20260201-161345-benchmark-1789/
├── REPRODUCER.sh
├── environment.txt
├── benchmark_results.txt       # Raw benchmark output
├── baselines.json              # Performance baselines
├── comparison_report.html      # Regression analysis
└── reports/                    # Detailed metrics
    ├── core_ops_100k.json
    └── tcp_sustained_10k.json
```

**Reproduction**:
```bash
./REPRODUCER.sh
# Step 1: Compile project
# Step 2: Run benchmark workload
# Step 3: Compare with baseline
```

## Integration with CLAUDE.md

This failure capture system aligns with erlmcp's CLAUDE.md principles:

### Poka-Yoke (Mistake-Proofing)

**Structural Impossibility of Silent Failures**:
- Every failure → automatic capture
- No manual artifact collection required
- Reproduction commands auto-generated

### Jidoka (Built-in Quality)

**Automatic Quality at Source**:
- Failure capture integrated into quality gates
- No separate debugging workflow needed
- Evidence bundled at point of failure

### Andon (Visible Error Signaling)

**Immediate Visibility**:
- GitHub Step Summary shows reproduction commands
- Artifacts linked in failure logs
- PR comments auto-generated for compliance failures

### Kaizen (Continuous Improvement)

**Feedback Loop**:
- Failure artifacts enable root cause analysis
- Reproduction scripts ensure fix verification
- Metrics track resolution times

---

**Status**: Production Ready (v2.1.0)
**Maintainer**: erlmcp team
**Last Updated**: 2026-02-01
