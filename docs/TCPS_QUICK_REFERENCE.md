# TCPS Rebar3 Quick Reference Card

## Common Commands

### Build Commands
```bash
# Full TCPS build (recommended)
rebar3 tcps_full

# Build with validation only
rebar3 tcps_build

# Validate only (no compile)
rebar3 tcps_validate

# Standard build (TCPS hooks still run)
rebar3 compile
```

### SHACL Validation
```bash
# Validate all ontology files
rebar3 tcps shacl_validate

# Validate specific file
rebar3 tcps shacl_validate --data=ontology/work_orders.ttl

# Non-strict mode (warnings only)
rebar3 tcps shacl_validate --strict=false --andon=false
```

### Receipt Management
```bash
# Generate compilation receipt
rebar3 tcps generate_receipt --stage=compile

# Generate test receipt
rebar3 tcps generate_receipt --stage=test

# Generate release receipt
rebar3 tcps generate_receipt --stage=release

# View receipts
ls -lh priv/receipts/
cat priv/receipts/rcpt_compile_*.json | jq .
```

### Quality Gates
```bash
# Check all gates
rebar3 tcps check_quality_gates --stage=all

# Check test gate only
rebar3 tcps check_quality_gates --stage=test

# Custom thresholds
rebar3 tcps check_quality_gates --min-pass-rate=90 --min-coverage=85

# Non-strict mode
rebar3 tcps check_quality_gates --strict=false
```

### Andon Management
```bash
# List open Andons
rebar3 tcps andon list

# Resolve Andon (interactive)
rebar3 tcps andon resolve <ANDON_ID>

# Show Andon details
rebar3 tcps andon show <ANDON_ID>

# Check if build blocked
rebar3 tcps andon check
```

---

## Quality Gate Thresholds

| Gate | Metric | Threshold |
|------|--------|-----------|
| **Compilation** | Error count | 0 |
| | Critical warnings | 0 |
| **Test** | Pass rate | ≥ 80% |
| | Coverage | ≥ 80% |
| | Total tests | ≥ 1 |
| **Release** | Deterministic build | Valid SHA-256 |
| | Artifact size | > 0 bytes |

---

## Receipt Types

| Type | Stage | Filename | Content |
|------|-------|----------|---------|
| SHACL | Pre-compile | `rcpt_shacl_*.json` | Violations, files validated |
| Compilation | Post-compile | `rcpt_compile_*.json` | Errors, warnings, duration |
| Test | Post-test | `rcpt_test_*.json` | Pass/fail, coverage |
| Quality Gates | Post-test | `rcpt_quality_*.json` | All gate results |
| Release | Pre-release | `rcpt_release_*.json` | Hash, size, timestamp |

---

## Andon Event Types

| Event Type | Trigger | Resolution Required |
|------------|---------|-------------------|
| `shacl_violation` | SHACL fails | Fix ontology + add constraints |
| `compilation_failure` | Compile errors | Fix code errors |
| `test_failure` | Tests fail / coverage < 80% | Fix tests + add coverage |
| `non_determinism` | Hash mismatch | Fix non-deterministic code |

---

## Workflow Example

```bash
# 1. Check for blocking Andons
rebar3 tcps andon check

# 2. Full TCPS build
rebar3 tcps_full

# 3. If build fails with Andon:
rebar3 tcps andon list
rebar3 tcps andon resolve <ANDON_ID>
# (Answer interactive prompts)

# 4. Resume build
rebar3 tcps_full

# 5. View receipts
ls priv/receipts/
cat priv/receipts/rcpt_quality_*.json | jq .metrics
```

---

## Troubleshooting

### Build Blocked by Andon
```bash
rebar3 tcps andon list
rebar3 tcps andon resolve <ANDON_ID>
```

### SHACL Validation Fails
```bash
# Review violations in output
# Fix ontology/*.ttl files
# Ensure all TCPS properties present
rebar3 tcps shacl_validate
```

### Quality Gate Fails (Coverage)
```bash
rebar3 cover                    # Generate coverage report
rebar3 cover --verbose          # View uncovered lines
# Add tests for uncovered code
rebar3 tcps_full                # Re-run
```

### Quality Gate Fails (Pass Rate)
```bash
rebar3 eunit --verbose          # Run tests with details
# Fix failing tests
rebar3 tcps check_quality_gates --stage=test
```

---

## Configuration (rebar.config)

```erlang
%% Enable TCPS plugin
{plugins, [rebar3_tcps_plugin]}.

%% Automatic hooks (already configured)
{provider_hooks, [
    {pre, [{compile, {tcps, shacl_validate}}]},
    {post, [{compile, {tcps, generate_receipt}}]},
    {post, [{eunit, {tcps, check_quality_gates}}]},
    {pre, [{release, {tcps, check_quality_gates}}]}
]}.

%% Custom thresholds (optional)
{tcps_config, [
    {quality_gates, [
        {min_test_pass_rate, 90.0},
        {min_code_coverage, 85.0},
        {strict_mode, true}
    ]}
]}.
```

---

## Environment Variables

```bash
# Skip TCPS validation (emergency override)
export TCPS_SKIP_VALIDATION=true

# Custom receipt directory
export TCPS_RECEIPT_DIR=/custom/path

# Andon webhook (future)
export TCPS_ANDON_WEBHOOK=https://hooks.slack.com/...
```

---

## CI/CD Integration

```yaml
# GitHub Actions
- name: TCPS Build
  run: rebar3 tcps_full

- name: Check Andon Status
  run: rebar3 tcps andon check

- name: Upload Receipts
  uses: actions/upload-artifact@v3
  with:
    name: tcps-receipts
    path: priv/receipts/
```

---

## File Locations

```
erlmcp/
├── priv/receipts/              # All receipts stored here
├── shapes/tcps_shapes.ttl      # SHACL constraints
├── ontology/*.ttl              # Ontology files (validated)
└── src/tcps/
    ├── tcps_rebar3_shacl.erl   # SHACL provider
    ├── tcps_rebar3_receipt.erl # Receipt provider
    ├── tcps_rebar3_andon.erl   # Andon provider
    └── tcps_rebar3_quality.erl # Quality gates provider
```

---

## Help Commands

```bash
# Provider help
rebar3 help tcps
rebar3 tcps shacl_validate --help
rebar3 tcps generate_receipt --help
rebar3 tcps check_quality_gates --help

# Full documentation
cat docs/TCPS_REBAR3_INTEGRATION.md
cat docs/TCPS_REBAR3_SUMMARY.md
```

---

## Support Resources

- **Full Guide**: `/docs/TCPS_REBAR3_INTEGRATION.md`
- **Summary**: `/docs/TCPS_REBAR3_SUMMARY.md`
- **Example Script**: `/scripts/tcps_workflow_example.sh`
- **Tests**: `/test/tcps/tcps_rebar3_providers_tests.erl`

---

**Quick Reference Version**: 1.0.0
**Last Updated**: 2026-01-27
