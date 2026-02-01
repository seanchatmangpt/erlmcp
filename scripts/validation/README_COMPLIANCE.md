# Compliance Validation Scripts

## Overview

This directory contains orchestration scripts for automated MCP specification compliance reporting and benchmark regression detection.

## Scripts

### 1. run_compliance_validators.sh

Main orchestrator for MCP compliance validation. Runs all 4 validators (protocol, transport, security, performance) and generates unified reports.

**Features:**
- Configurable validation modes (quick, standard, full)
- Multiple output formats (JSON, HTML, Markdown)
- Automatic archiving with retention policies
- Integration with `.claude/config/compliance_config.json`

**Usage:**

```bash
# Quick validation (protocol + transport only)
./scripts/validation/run_compliance_validators.sh --quick

# Full validation with all validators
./scripts/validation/run_compliance_validators.sh --full

# Generate HTML report
./scripts/validation/run_compliance_validators.sh --format=html --output=reports/compliance/latest

# Archive results
./scripts/validation/run_compliance_validators.sh --full --archive
```

**Options:**
- `--quick` - Quick validation (protocol + transport only)
- `--full` - Full validation (all 4 validators + stress tests)
- `--output=DIR` - Output directory (default: `reports/compliance/latest`)
- `--format=FMT` - Output format: json, html, markdown (default: json)
- `--archive` - Archive results with timestamp
- `--help` - Show help message

**Exit Codes:**
- 0 - All validators passed (compliance >= threshold)
- 1 - Validation failed or errors occurred
- 2 - Compliance below threshold (non-blocking warning)

**Configuration:**

The script reads configuration from `.claude/config/compliance_config.json`:

```json
{
  "spec_version": "2025-11-25",
  "output_format": "json",
  "report_dir": "reports/compliance",
  "validators": {
    "protocol": {"enabled": true, "module": "erlmcp_protocol_validator", "weight": 0.4},
    "transport": {"enabled": true, "module": "erlmcp_transport_validator", "weight": 0.3},
    "security": {"enabled": true, "module": "erlmcp_security_validator", "weight": 0.2},
    "performance": {"enabled": true, "module": "erlmcp_performance_validator", "weight": 0.1}
  },
  "thresholds": {
    "overall_compliance_percent": 80.0,
    "critical_failures": 0,
    "high_severity_failures": 0
  },
  "archive": {
    "enabled": true,
    "retention_days": 90,
    "max_archives": 50
  }
}
```

**Output:**

The script produces a compliance report at `{output_dir}/mcp_compliance.{format}` with:
- Overall compliance score
- Individual validator results
- Detailed validation evidence
- Pass/fail status

**Archiving:**

When `--archive` is specified, results are saved to `reports/compliance/archive/compliance_{timestamp}/`:
- Full compliance report
- Individual validator results (JSON)
- Execution logs

Archives are automatically cleaned up:
- Retention period: 90 days (configurable)
- Maximum archives: 50 (keeps most recent)

## Validators

### Protocol Validator (`erlmcp_protocol_validator`)

Validates JSON-RPC 2.0 and MCP protocol compliance:
- Message structure validation
- Method call validation
- Error code compliance
- Protocol version verification

**Weight:** 40% of overall compliance

### Transport Validator (`erlmcp_transport_validator`)

Validates transport layer behavior:
- Callback compliance (`init/2`, `send/2`, `close/1`)
- Message framing
- Registry integration
- Lifecycle management

**Weight:** 30% of overall compliance

### Security Validator (`erlmcp_security_validator`)

Validates security features:
- Authentication mechanisms
- JWT validation
- Input validation
- Rate limiting

**Weight:** 20% of overall compliance

### Performance Validator (`erlmcp_performance_validator`)

Validates performance characteristics:
- Latency thresholds
- Throughput targets
- Memory usage
- Connection capacity

**Weight:** 10% of overall compliance

## Integration with CI/CD

### GitHub Actions

```yaml
- name: Run Compliance Validation
  run: |
    ./scripts/validation/run_compliance_validators.sh --full --format=json --output=reports/compliance

- name: Check Compliance Score
  run: |
    SCORE=$(jq -r '.compliance.score' reports/compliance/mcp_compliance.json)
    if (( $(echo "$SCORE < 80.0" | bc -l) )); then
      echo "Compliance score $SCORE% is below threshold 80%"
      exit 1
    fi

- name: Upload Compliance Report
  uses: actions/upload-artifact@v3
  with:
    name: compliance-report
    path: reports/compliance/
```

### Pre-commit Hook

```bash
#!/bin/bash
# .git/hooks/pre-commit

echo "Running compliance validators..."
if ./scripts/validation/run_compliance_validators.sh --quick; then
    echo "✓ Compliance checks passed"
    exit 0
else
    echo "✗ Compliance checks failed"
    exit 1
fi
```

## Troubleshooting

### Compilation Errors

If you see "Compilation failed", ensure:
1. rebar3 is in PATH: `which rebar3`
2. Erlang/OTP is installed: `erl -version`
3. Project dependencies are fetched: `rebar3 get-deps`

### Validator Timeout

If validators take too long:
1. Use `--quick` mode for faster validation
2. Increase timeout in validator modules
3. Check system resources (CPU, memory)

### Archive Cleanup

Manual cleanup of old archives:

```bash
# Remove archives older than 90 days
find reports/compliance/archive -type d -name "compliance_*" -mtime +90 -exec rm -rf {} \;

# Keep only last 50 archives
cd reports/compliance/archive
ls -t | tail -n +51 | xargs rm -rf
```

## Examples

### Quick Pre-PR Check

```bash
./scripts/validation/run_compliance_validators.sh --quick --format=markdown > compliance_check.md
cat compliance_check.md
```

### Full Release Validation

```bash
./scripts/validation/run_compliance_validators.sh \
  --full \
  --format=html \
  --output=reports/compliance/release_v2.1.0 \
  --archive
```

### JSON Output for Parsing

```bash
./scripts/validation/run_compliance_validators.sh --full --format=json | jq '.compliance.score'
```

## Related Files

- `.claude/config/compliance_config.json` - Configuration
- `apps/erlmcp_validation/src/erlmcp_validate_cli.erl` - CLI interface
- `apps/erlmcp_validation/src/erlmcp_*_validator.erl` - Individual validators
- `apps/erlmcp_validation/src/erlmcp_compliance_report*.erl` - Report generators
- `reports/compliance/` - Output directory
- `reports/compliance/archive/` - Archived results

## See Also

- [MCP Specification Compliance Guide](../../docs/compliance-report-guide.md)
- [OTP Compliance Report](../../docs/validation/otp_compliance_report.md)
- [Benchmark Orchestration](../bench/README.md)
