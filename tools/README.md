# TCPS CLI Tools - Toyota Code Production System

Comprehensive command-line interface for managing Toyota Code Production System (TCPS) daily operations.

## Installation

### Prerequisites

- Erlang/OTP 24 or later
- Rebar3 build tool
- Compiled erlmcp project

### Build and Install

```bash
# Build the project
cd /path/to/erlmcp
rebar3 compile

# Make tcps script executable (already done)
chmod +x tools/tcps

# Add to PATH (optional)
export PATH=$PATH:/path/to/erlmcp/tools

# Install bash completion (optional)
source tools/tcps-completion.bash
# Or system-wide:
sudo cp tools/tcps-completion.bash /etc/bash_completion.d/tcps
```

## Quick Start

```bash
# Show all available commands
tcps help

# Get help for a specific command
tcps help work-order

# Check system health
tcps tpm health

# Create a work order
tcps work-order create --bucket security --priority 10 --title "Fix CVE-2024-1234"

# Trigger an Andon event
tcps andon trigger --type test_failure --sku sku_123 --message "Test failed"

# Start a 5 Whys analysis
tcps root-cause start ANDON-123

# Run an example workflow
tcps example security-patch --interactive
```

## Command Reference

### Work Order Management

```bash
# Create work order
tcps work-order create --bucket BUCKET --priority NUM --title "TITLE"

# List work orders
tcps work-order list [--status STATUS] [--bucket BUCKET] [--limit NUM]

# Show work order details
tcps work-order show ORDER_ID

# Complete work order
tcps work-order complete ORDER_ID

# Delete work order
tcps work-order delete ORDER_ID
```

**Buckets**: `reliability`, `security`, `cost`, `compliance`
**Statuses**: `pending`, `in_progress`, `completed`

### Andon Stop-the-Line

```bash
# Trigger Andon event
tcps andon trigger --type TYPE --sku SKU_ID [--stage STAGE] [--message MSG]

# List Andon events
tcps andon list [--open | --all] [--sku SKU_ID]

# Show Andon details
tcps andon show ANDON_ID

# Resolve Andon
tcps andon resolve ANDON_ID --root-cause "TEXT" --fix "TEXT" --prevention "TEXT"

# Check SKU status
tcps andon status SKU_ID
```

**Failure Types**: `test_failure`, `shacl_violation`, `compilation_failure`, `non_determinism`, `missing_receipt`
**Stages**: `compilation`, `testing`, `validation`, `execution`, `integration`, `deployment`

### Receipt Management

```bash
# Verify receipt for stage/SKU
tcps receipt verify STAGE SKU_ID

# Show complete receipt chain
tcps receipt chain SKU_ID [--format FORMAT]

# List recent receipts
tcps receipt list [--recent NUM]

# Show receipt details
tcps receipt show RECEIPT_ID

# Validate receipt against SHACL
tcps receipt validate RECEIPT_ID
```

**Formats**: `table`, `json`, `markdown`, `graph`

### Quality Gates & Metrics

```bash
# Check quality gates for SKU
tcps quality gates SKU_ID [--verbose]

# Show quality metrics
tcps quality metrics [--period PERIOD] [--compare]

# Interactive dashboard
tcps quality dashboard

# Generate quality report
tcps quality report
```

**Periods**: `daily`, `weekly`, `monthly`

### Kanban WIP & Scheduling

```bash
# Show WIP status
tcps kanban status [--bucket BUCKET]

# Show Heijunka schedule
tcps kanban schedule

# Set WIP limit
tcps kanban set-limit BUCKET LIMIT

# Process pull signal
tcps kanban pull --bucket BUCKET [--priority NUM]
```

### Kaizen Continuous Improvement

```bash
# Generate Kaizen report
tcps kaizen report [--weekly | --monthly] [--output FILE]

# Show improvement proposals
tcps kaizen proposals [--top NUM] [--roi-threshold N]

# Apply improvement
tcps kaizen apply IMPROVEMENT_ID

# Show waste analysis
tcps kaizen waste

# Show trend analysis
tcps kaizen trends
```

### Root Cause Analysis (5 Whys)

```bash
# Start new analysis
tcps root-cause start ANDON_ID [--problem "TEXT"]

# Add why answer
tcps root-cause add-why ANALYSIS_ID WHY_NUM "ANSWER"

# Finalize analysis
tcps root-cause finalize ANALYSIS_ID --root-cause "TEXT" --prevention "TEXT"

# Show analysis details
tcps root-cause show ANALYSIS_ID

# List all analyses
tcps root-cause list
```

**Why Numbers**: 1-5 (must complete all five)

### Total Productive Maintenance (TPM)

```bash
# Run maintenance
tcps tpm maintenance [--daily | --weekly] [--force]

# Show TPM dashboard
tcps tpm dashboard

# Check system health
tcps tpm health

# Show TPM metrics
tcps tpm metrics
```

### Example Workflows

```bash
# Run example workflow
tcps example WORKFLOW [--interactive] [--dry-run]
```

**Workflows**:
- `security-patch` - Security patch workflow
- `new-feature` - Feature development workflow
- `andon-resolve` - Andon resolution workflow
- `full-cycle` - Complete development cycle

## Global Options

```bash
--format FORMAT    Output format: json, table (default), markdown
--color            Enable colored output (default if TTY)
--no-color         Disable colored output
--config FILE      Use custom config file
--verbose, -v      Verbose output
--quiet, -q        Minimal output
```

## Configuration

### Config File Locations (in order of precedence)

1. Command-line: `--config FILE`
2. Environment: `$TCPS_CONFIG`
3. User config: `~/.tcps/config`
4. Project config: `./tcps.config`

### Example Configuration

```erlang
% ~/.tcps/config

{ontology_path, "/path/to/ontology"}.
{receipts_path, "/path/to/receipts"}.
{shacl_shapes, "/path/to/shapes"}.
{output_format, table}.
{color_output, true}.

% WIP limits per bucket
{wip_limits, #{
    reliability => 5,
    security => 5,
    cost => 5,
    compliance => 5
}}.

% Quality targets
{quality_targets, #{
    lead_time => 2.0,          % hours
    defect_rate => 1.0,        % percent
    rework_pct => 5.0,         % percent
    first_pass_yield => 95.0   % percent
}}.
```

## Example Workflows

### Security Patch Workflow

```bash
# 1. Create work order
tcps work-order create --bucket security --priority 10 --title "Fix CVE-2024-1234"

# 2. Check WIP status
tcps kanban status --bucket security

# 3. Implement fix (manual step)

# 4. Verify quality gates
tcps quality gates sku_12345

# 5. Complete work order
tcps work-order complete wo_67890
```

### Andon Resolution Workflow

```bash
# 1. Trigger Andon (or auto-triggered by test failure)
tcps andon trigger --type test_failure --sku sku_123 --message "Race condition"

# 2. Start root cause analysis
tcps root-cause start ANDON-456

# 3. Complete 5 Whys
tcps root-cause add-why analysis_789 1 "Test failed"
tcps root-cause add-why analysis_789 2 "No mutex"
tcps root-cause add-why analysis_789 3 "Missing design"
tcps root-cause add-why analysis_789 4 "No requirements"
tcps root-cause add-why analysis_789 5 "Spec incomplete"

# 4. Finalize
tcps root-cause finalize analysis_789 \
  --root-cause "Missing concurrency requirements" \
  --prevention "Add concurrency tests and design review"

# 5. Implement fix (manual step)

# 6. Resolve Andon
tcps andon resolve ANDON-456 \
  --root-cause "Missing concurrency requirements" \
  --fix "Added mutex and tests" \
  --prevention "Concurrency design review process"

# 7. Verify unblocked
tcps andon status sku_123
```

## Output Formats

### Table (Default)

```
+----------------+----------+----------+---------+---------------------+
| ID             | BUCKET   | PRIORITY | STATUS  | CREATED_AT          |
+----------------+----------+----------+---------+---------------------+
| wo_1234_567    | security | 10       | pending | 2024-01-26 12:00:00 |
| wo_1234_568    | cost     | 50       | pending | 2024-01-26 13:00:00 |
+----------------+----------+----------+---------+---------------------+
```

### JSON

```json
{
  "work_orders": [
    {
      "id": "wo_1234_567",
      "bucket": "security",
      "priority": 10,
      "status": "pending",
      "created_at": "2024-01-26T12:00:00Z"
    }
  ]
}
```

### Markdown

```markdown
| ID          | BUCKET   | PRIORITY | STATUS  | CREATED_AT          |
| ----------- | -------- | -------- | ------- | ------------------- |
| wo_1234_567 | security | 10       | pending | 2024-01-26 12:00:00 |
```

## Testing

```bash
# Run CLI tests
rebar3 eunit --module=tcps_cli_tests

# Run all TCPS tests
rebar3 eunit --dir=test
```

## Troubleshooting

### Command not found

Ensure the script is executable and in your PATH:
```bash
chmod +x tools/tcps
export PATH=$PATH:/path/to/erlmcp/tools
```

### Module not found errors

Ensure the project is compiled:
```bash
rebar3 compile
```

### ETS table errors

Some commands require servers to be running. Start the Erlang shell first:
```bash
rebar3 shell
```

Then run commands in the shell context.

## Architecture

The TCPS CLI is built as an escript with the following modules:

- `tools/tcps` - Main escript entry point
- `tcps_cli_config` - Configuration management
- `tcps_cli_format` - Output formatting (table, JSON, markdown)
- `tcps_cli_work_order` - Work order commands
- `tcps_cli_andon` - Andon commands
- `tcps_cli_receipt` - Receipt commands
- `tcps_cli_quality` - Quality gates commands
- `tcps_cli_kanban` - Kanban commands
- `tcps_cli_kaizen` - Kaizen commands
- `tcps_cli_root_cause` - Root cause analysis commands
- `tcps_cli_tpm` - TPM commands
- `tcps_cli_examples` - Example workflows

## Contributing

See `CONTRIBUTING.md` for development guidelines.

## License

See `LICENSE` file.

---

# Automated Validation Tools

## Overview

Comprehensive **BLOCKING** validation scripts that enforce Lean Six Sigma quality standards with zero-defect tolerance. All scripts exit with code 1 on failure.

## Core Validation Scripts

### Test Runner (`test-runner.sh`)
**BLOCKING test execution with pass rate enforcement**

- Runs EUnit + Common Test suites
- Calculates pass rate (target: ≥90%)
- Generates JSON results
- **EXITS 1** if pass rate < 90%

```bash
./tools/test-runner.sh
# Or: make test-strict
```

**Outputs:**
- `_build/test/results/test_results.json`
- `_build/test/results/eunit_output.txt`
- `_build/test/results/ct_output.txt`

---

### Benchmark Runner (`benchmark-runner.sh`)
**Performance regression detection**

- Runs 10 benchmark workloads
- Compares to baselines in `bench/baselines/`
- Detects regressions >10%
- **EXITS 1** if regression detected

```bash
./tools/benchmark-runner.sh
# Or: make benchmark-strict
```

**Outputs:**
- `_build/test/benchmarks/benchmark_results.json`
- `_build/test/benchmarks/regression_report.txt`
- `bench/baselines/*.baseline`

---

### Coverage Checker (`coverage-checker.sh`)
**Per-module coverage enforcement**

- Runs tests with coverage
- Checks EVERY module ≥80%
- **EXITS 1** if any module below threshold

```bash
./tools/coverage-checker.sh
# Or: make coverage-strict
```

**Outputs:**
- `_build/test/coverage/coverage_report.txt`
- `_build/test/cover/index.html`

---

### Quality Checker (`quality-checker.sh`)
**Master validation script**

Runs ALL checks:
1. Compilation
2. Unit Tests
3. Coverage
4. Dialyzer
5. Xref

**EXITS 1 if ANY fail**

```bash
./tools/quality-checker.sh
# Or: make quality-strict
```

**Output:**
- `_build/test/quality/quality_report.txt`

---

## Makefile Integration

```bash
make test-strict           # Tests: ≥90% pass rate
make benchmark-strict      # Benchmarks: <10% regression
make coverage-strict       # Coverage: ≥80% per module
make quality-strict        # ALL checks MUST pass
```

---

## Quality Thresholds

| Check | Threshold | Exit on Failure |
|-------|-----------|-----------------|
| Tests | ≥90% pass | YES ❌ |
| Coverage | ≥80% per module | YES ❌ |
| Benchmarks | <10% regression | YES ❌ |
| Dialyzer | 0 warnings | YES ❌ |
| Xref | 0 undefined | YES ❌ |

---

## Full Documentation

See `docs/testing/AUTOMATED_VALIDATION.md` for comprehensive guide.
