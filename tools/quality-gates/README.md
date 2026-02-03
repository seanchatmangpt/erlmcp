# ERLMCP Quality Gates - Parallel Execution

## Overview

This directory contains comprehensive quality gate automation for erlmcp v3, executing all quality checks in parallel where possible via Docker containers.

**Philosophy**: DOCKER-ONLY CONSTITUTION - All execution via Docker. No host execution.

## Quality Gates (7 Total)

| Gate | Description | Threshold | Parallel Phase |
|------|-------------|-----------|----------------|
| **compile** | Compilation check | errors = 0 | Phase 1 |
| **eunit** | Unit tests | failures = 0 | Phase 2 |
| **ct** | Integration tests (Common Test) | failures = 0 | Phase 2 |
| **dialyzer** | Type checking | warnings = 0 | Phase 1 |
| **xref** | Cross-reference analysis | undefined = 0 | Phase 1 |
| **coverage** | Test coverage | >= 80% | Phase 3 |
| **security** | Security scan | secrets = 0 | Phase 1 |

## Parallel Execution Strategy

### Phase 1: Independent Gates (Parallel)
- **compile** - Must complete first
- **dialyzer** - Type checking (after compile)
- **xref** - Cross-reference (after compile)
- **security** - Secret scanning (after compile)

### Phase 2: Test Gates (Parallel)
- **eunit** - Unit tests (after compile)
- **ct** - Integration tests (after compile)

### Phase 3: Coverage (Sequential)
- **coverage** - Coverage analysis (after tests complete)

## Usage

### Basic Usage

```bash
# Run all quality gates
./tools/quality-gates/run-all.sh

# Fast mode (compile + tests only)
./tools/quality-gates/run-all.sh --fast

# Skip slow Dialyzer gate
./tools/quality-gates/run-all.sh --skip-dialyzer

# Show help
./tools/quality-gates/run-all.sh --help
```

### Environment Variables

```bash
# Set max parallel jobs
export PARALLEL_JOBS=8

# Set work order ID
export WORK_ORDER_ID="WO-2024-001"

# Use custom Docker image
export DOCKER_IMAGE="erlmcp:custom-3.0.0"

# Run with custom settings
PARALLEL_JOBS=8 WORK_ORDER_ID="WO-001" ./tools/quality-gates/run-all.sh
```

## Receipt Generation

Each quality gate generates a cryptographic receipt containing:

```json
{
  "receipt_version": "1.0",
  "gate": "compile",
  "work_order_id": "1738555200",
  "timestamp": "2026-02-02T15:00:00Z",
  "git": {
    "sha": "abc123...",
    "short_sha": "abc1234",
    "branch": "main",
    "commit_message": "Add feature X"
  },
  "docker": {
    "image": "erlmcp:3.0.0",
    "image_digest": "sha256:...",
    "service": "erlmcp-build"
  },
  "execution": {
    "cmd": "rebar3 compile",
    "exit_code": 0,
    "duration_ms": 15234
  },
  "output": {
    "stdout": "...",
    "stderr": "..."
  },
  "receipt_hash": "0123456789abcdef..."
}
```

### Master Receipt

A master receipt aggregates all gate receipts:

```bash
.erlmcp/receipts/quality-gates/
├── master_1738555200.json       # Master receipt (all gates)
├── compile_1738555200.json      # Individual gate receipts
├── eunit_1738555200.json
├── ct_1738555200.json
├── dialyzer_1738555200.json
├── xref_1738555200.json
├── coverage_1738555200.json
└── security_1738555200.json
```

## Output

### Success Output

```
╔══════════════════════════════════════════════════════════════╗
║         ERLMCP QUALITY GATES - PARALLEL EXECUTION           ║
╚══════════════════════════════════════════════════════════════╝

Work Order:    1738555200
Timestamp:     2026-02-02T15:00:00Z
Git SHA:       abc1234 (main)
Parallel Jobs: 4

╔══════════════════════════════════════════════════════════════╗
║  PHASE 1: PARALLEL GATES (compile, dialyzer, xref, security) ║
╚══════════════════════════════════════════════════════════════╝

[✓] [compile] Completed successfully
[✓] [dialyzer] Completed successfully
[✓] [xref] Completed successfully
[✓] [security] No security issues found

╔══════════════════════════════════════════════════════════════╗
║  PHASE 2: PARALLEL TEST GATES (eunit, ct)                    ║
╚══════════════════════════════════════════════════════════════╝

[✓] [eunit] Completed successfully
[✓] [ct] Completed successfully

╔══════════════════════════════════════════════════════════════╗
║  PHASE 3: COVERAGE GATE                                      ║
╚══════════════════════════════════════════════════════════════╝

[✓] [coverage] Completed successfully

╔══════════════════════════════════════════════════════════════╗
║              QUALITY GATES FINAL REPORT                      ║
╚══════════════════════════════════════════════════════════════╝

Gate Results:
--------------- ---------- ---------- ---------------
Gate            Status     Exit       Duration
--------------- ---------- ---------- ---------------
compile         PASS       0          15234ms
eunit           PASS       0          23456ms
ct              PASS       0          34567ms
dialyzer        PASS       0          89123ms
xref            PASS       0          12345ms
coverage        PASS       0          5678ms
security        PASS       0          2345ms
--------------- ---------- ---------- ---------------

╔══════════════════════════════════════════════════════════════╗
║                   ✓ ALL QUALITY GATES PASSED                 ║
╚══════════════════════════════════════════════════════════════╝

Production deployment ready!

Master Receipt: /Users/sac/erlmcp/.erlmcp/receipts/quality-gates/master_1738555200.json

Total Duration:  178s
```

### Failure Output

```
╔══════════════════════════════════════════════════════════════╗
║                   ✗ QUALITY GATES FAILED                     ║
╚══════════════════════════════════════════════════════════════╝

Production deployment BLOCKED

Failed Gates:
  ✗ dialyzer
     Receipt: .erlmcp/receipts/quality-gates/dialyzer_1738555200.json
     Stderr:  .erlmcp/logs/quality-gates/dialyzer_1738555200.stderr
```

## Integration with CI/CD

### Git Hooks

```bash
# Pre-commit hook
#!/bin/bash
./tools/quality-gates/run-all.sh --fast
```

### GitHub Actions

```yaml
name: Quality Gates

on: [push, pull_request]

jobs:
  quality-gates:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - name: Run Quality Gates
        run: ./tools/quality-gates/run-all.sh
        env:
          PARALLEL_JOBS: 4
```

## Troubleshooting

### Docker Image Not Found

```bash
# Build the image first
docker build -t erlmcp:3.0.0 -f Dockerfile .

# Or use custom image
export DOCKER_IMAGE="my-custom-image:latest"
./tools/quality-gates/run-all.sh
```

### Gate Timeouts

```bash
# Adjust timeout in run-all.sh (search for 'timeout=')
# Default: 1800 seconds (30 minutes) for dialyzer
```

### Receipt Verification

```bash
# Verify receipt hash
jq -r '.receipt_hash' master_1738555200.json
shasum -a 256 master_1738555200.json

# View gate details
jq '.gates.compile' master_1738555200.json
```

## Files

- `run-all.sh` - Main quality gates execution script
- `README.md` - This documentation
- `.erlmcp/receipts/quality-gates/` - Generated receipts
- `.erlmcp/logs/quality-gates/` - Gate execution logs

## Docker Services

The script requires the following Docker services (defined in `docker-compose.yml`):

- `erlmcp-build` - Compilation and static analysis
- `erlmcp-unit` - Unit test execution
- `erlmcp-ct` - Integration test execution
- `erlmcp-check` - Coverage and validation

If these services don't exist, the script uses the default `erlmcp` service.

## License

Apache-2.0
