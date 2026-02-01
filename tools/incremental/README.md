# Incremental Validation System

**Version**: 1.0.0
**Status**: Beta
**Purpose**: Cost-optimized validation for cloud-based CI/CD

## Quick Start

```bash
# 1. Initialize cache
./tools/incremental/cache-manager.sh init

# 2. Run incremental validation
./tools/incremental/validate.sh

# 3. View cache statistics
./tools/incremental/cache-manager.sh stats
```

## Scripts

### cache-manager.sh
Manages the validation cache.

**Usage**:
```bash
./cache-manager.sh {init|clean|rebuild|verify|stats}
```

**Commands**:
- `init` - Initialize new cache
- `clean` - Remove cache directory
- `rebuild` - Clean and reinitialize cache
- `verify` - Verify cache integrity
- `stats` - Show cache statistics

### detect-changes.sh
Detects changed files using SHA256 hashing.

**Usage**:
```bash
./detect-changes.sh
```

**Output**: `.erlmcp/cache/changes.json`

### select-gates.sh
Selects required quality gates based on changes.

**Usage**:
```bash
./select-gates.sh
```

**Output**: `.erlmcp/cache/gate-plan.json`

### estimate-cost.sh
Estimates validation time and cost.

**Usage**:
```bash
./estimate-cost.sh
```

**Requirements**: `jq`, `bc` (optional)

### validate.sh
Master orchestrator script.

**Usage**:
```bash
./validate.sh
```

**Workflow**:
1. Initialize/verify cache
2. Detect changes
3. Select gates
4. Estimate cost
5. Execute gates
6. Save history

## Dependencies

- **jq** - JSON processor (required)
- **bc** - Calculator (optional, for cost estimation)
- **git** - Version control (required)
- **sha256sum** or **shasum** - Hash computation (required)

### Installing Dependencies

**macOS**:
```bash
brew install jq bc
```

**Ubuntu/Debian**:
```bash
sudo apt-get install jq bc
```

## Cache Structure

```
.erlmcp/cache/
├── compile-graph.json          # Module dependency graph
├── test-mapping.json           # Module → Test mapping
├── gate-mapping.json           # Gate configuration
├── file-hashes.json            # File SHA256 hashes
├── changes.json                # Current changes
├── gate-plan.json              # Selected gates
├── test-results/               # Test outputs
├── gate-results/               # Gate outputs
└── validation-history/         # Historical results
```

## Configuration

Edit `.erlmcp/cache/gate-mapping.json` to customize gate triggers and cost estimates.

## Troubleshooting

### Cache Corruption
```bash
./cache-manager.sh rebuild
```

### Missing Dependencies
```bash
# Check for required tools
command -v jq || echo "Install jq"
command -v git || echo "Install git"
```

### Inaccurate Cost Estimates
Edit cost model in `estimate-cost.sh`:
```bash
HOURLY_RATE=0.17  # AWS c5.xlarge rate
COMPILE_SECONDS=15
EUNIT_PER_SUITE_SECONDS=2
# ... etc
```

## Integration

### Git Hooks
Add to `.git/hooks/pre-commit`:
```bash
#!/usr/bin/env bash
./tools/incremental/validate.sh
```

### Makefile
Add target:
```makefile
incremental-validate:
	@./tools/incremental/validate.sh

.PHONY: incremental-validate
```

### CI/CD (GitHub Actions)
```yaml
- name: Restore cache
  uses: actions/cache@v3
  with:
    path: .erlmcp/cache
    key: erlmcp-cache-${{ github.sha }}

- name: Run incremental validation
  run: ./tools/incremental/validate.sh
```

## Performance

### Expected Savings

| Scenario | Full Suite | Incremental | Savings |
|----------|-----------|-------------|---------|
| 1 file change | 8m / $0.40 | 45s / $0.08 | 89% / 80% |
| Type spec change | 8m / $0.40 | 2m 30s / $0.18 | 69% / 55% |
| No changes | 8m / $0.40 | 5s / $0.01 | 99% / 97% |

## See Also

- [INCREMENTAL_VALIDATION_DESIGN.md](../../INCREMENTAL_VALIDATION_DESIGN.md) - Complete design document
- [CLAUDE.md](../../CLAUDE.md) - Project documentation
- [Makefile](../../Makefile) - Build system

## License

Same as erlmcp project.
