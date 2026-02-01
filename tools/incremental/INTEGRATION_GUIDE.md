# Incremental Validation Integration Guide

## Overview

This guide shows how to integrate the incremental validation system into your development workflow.

---

## 1. Makefile Integration

Add these targets to `/home/user/erlmcp/Makefile`:

```makefile
##
## Incremental Validation Targets
##

.PHONY: incremental-init incremental-validate incremental-stats incremental-clean

incremental-init: ## Initialize incremental validation cache
	@echo "$(BLUE)Initializing incremental validation...$(NC)"
	@./tools/incremental/cache-manager.sh init

incremental-validate: ## Run incremental validation
	@echo "$(BLUE)Running incremental validation...$(NC)"
	@./tools/incremental/validate.sh

incremental-stats: ## Show cache statistics
	@./tools/incremental/cache-manager.sh stats

incremental-clean: ## Clean validation cache
	@./tools/incremental/cache-manager.sh clean

# Add to help target
help-incremental:
	@echo "$(BOLD)$(GREEN)Incremental Validation:$(NC)"
	@echo "  make incremental-init      - Initialize validation cache"
	@echo "  make incremental-validate  - Run cost-optimized validation"
	@echo "  make incremental-stats     - View cache statistics"
	@echo "  make incremental-clean     - Clean cache"
```

**Usage**:
```bash
# First time setup
make incremental-init

# Run validation
make incremental-validate

# View stats
make incremental-stats
```

---

## 2. Git Hooks

### Pre-commit Hook

Create `.git/hooks/pre-commit`:

```bash
#!/usr/bin/env bash
# Pre-commit hook: Run incremental validation

set -e

echo "Running incremental validation..."

# Initialize cache if needed
if [[ ! -d .erlmcp/cache ]]; then
    ./tools/incremental/cache-manager.sh init
fi

# Run validation
if ./tools/incremental/validate.sh; then
    echo "✅ Validation passed"
else
    echo "❌ Validation failed"
    echo ""
    echo "To bypass: git commit --no-verify"
    exit 1
fi
```

**Install**:
```bash
chmod +x .git/hooks/pre-commit
```

### Pre-push Hook

Create `.git/hooks/pre-push`:

```bash
#!/usr/bin/env bash
# Pre-push hook: Run full validation before push

set -e

echo "Running full validation before push..."

# Run all gates
export FORCE_ALL_GATES=true
./tools/incremental/validate.sh

echo "✅ Full validation passed"
```

---

## 3. GitHub Actions Integration

### Workflow File

Create `.github/workflows/incremental-ci.yml`:

```yaml
name: Incremental CI

on:
  pull_request:
    branches: [main, develop]
  push:
    branches: [main, develop]

jobs:
  incremental-validate:
    runs-on: ubuntu-latest

    steps:
      - name: Checkout code
        uses: actions/checkout@v3
        with:
          fetch-depth: 0  # Full history for accurate change detection

      - name: Setup Erlang/OTP
        uses: erlef/setup-beam@v1
        with:
          otp-version: '28'
          rebar3-version: '3.23'

      - name: Install dependencies
        run: |
          sudo apt-get update
          sudo apt-get install -y jq bc

      - name: Restore incremental validation cache
        uses: actions/cache@v3
        with:
          path: .erlmcp/cache
          key: erlmcp-incremental-${{ github.sha }}
          restore-keys: |
            erlmcp-incremental-${{ github.base_ref }}-
            erlmcp-incremental-

      - name: Initialize validation cache
        run: |
          if [[ ! -d .erlmcp/cache ]]; then
            ./tools/incremental/cache-manager.sh init
          fi

      - name: Detect changes
        run: ./tools/incremental/detect-changes.sh

      - name: Select gates
        run: ./tools/incremental/select-gates.sh

      - name: Estimate cost
        id: estimate
        run: |
          ./tools/incremental/estimate-cost.sh | tee /tmp/cost-estimate.txt
          COST=$(grep "Total estimated cost" /tmp/cost-estimate.txt | awk '{print $4}')
          echo "cost=$COST" >> $GITHUB_OUTPUT

      - name: Run incremental validation
        run: ./tools/incremental/validate.sh

      - name: Upload results
        if: always()
        uses: actions/upload-artifact@v3
        with:
          name: validation-results
          path: .erlmcp/cache/gate-results/

      - name: Comment cost savings on PR
        if: github.event_name == 'pull_request'
        uses: actions/github-script@v6
        with:
          script: |
            const cost = '${{ steps.estimate.outputs.cost }}';
            const fullCost = '$0.40';
            const savings = (parseFloat(fullCost.replace('$', '')) - parseFloat(cost.replace('$', ''))).toFixed(2);

            github.rest.issues.createComment({
              issue_number: context.issue.number,
              owner: context.repo.owner,
              repo: context.repo.repo,
              body: `## ⚡ Incremental Validation Results\n\n` +
                    `- **Estimated cost**: ${cost}\n` +
                    `- **Full suite cost**: ${fullCost}\n` +
                    `- **Savings**: $${savings} (${(savings/0.4*100).toFixed(0)}%)`
            })
```

### Nightly Full Validation

Create `.github/workflows/nightly-full.yml`:

```yaml
name: Nightly Full Validation

on:
  schedule:
    - cron: '0 2 * * *'  # 2 AM UTC daily
  workflow_dispatch:

jobs:
  full-validate:
    runs-on: ubuntu-latest

    steps:
      - uses: actions/checkout@v3

      - uses: erlef/setup-beam@v1
        with:
          otp-version: '28'
          rebar3-version: '3.23'

      - name: Run full validation suite
        run: make validate

      - name: Update performance baselines
        if: success()
        run: |
          ./scripts/bench/set_baseline.sh
          git config user.name "GitHub Actions"
          git config user.email "actions@github.com"
          git add .erlmcp/cache/perf-baseline.json
          git commit -m "chore: update performance baselines [skip ci]" || true
          git push
```

---

## 4. Development Workflow

### Daily Development

```bash
# Start work
git checkout -b feature/my-feature

# Make changes
vim apps/erlmcp_core/src/erlmcp_json_rpc.erl

# Quick validation (automatic via pre-commit hook)
git add .
git commit -m "feat: improve JSON-RPC encoding"

# Pre-push validation will run automatically
git push origin feature/my-feature
```

### Troubleshooting

```bash
# Cache corrupted?
make incremental-clean
make incremental-init

# Force full validation
FORCE_ALL_GATES=true ./tools/incremental/validate.sh

# Verify cache integrity
./tools/incremental/cache-manager.sh verify

# View cache statistics
./tools/incremental/cache-manager.sh stats
```

---

## 5. Cloud Cost Tracking

### Cost Report Script

Create `tools/incremental/cost-report.sh`:

```bash
#!/usr/bin/env bash
# Generate cost savings report

HISTORY_DIR=".erlmcp/cache/validation-history"

if [[ ! -d "$HISTORY_DIR" ]]; then
    echo "No validation history found"
    exit 0
fi

TOTAL_RUNS=$(find "$HISTORY_DIR" -name "*.json" | wc -l)
AVG_COST=0.08  # From estimates
FULL_COST=0.40
TOTAL_SAVINGS=$(echo "$TOTAL_RUNS * ($FULL_COST - $AVG_COST)" | bc)

cat <<EOF
Incremental Validation Cost Report
===================================

Total validation runs: $TOTAL_RUNS
Average cost per run: \$$AVG_COST
Full suite cost per run: \$$FULL_COST

Total cost: \$$(echo "$TOTAL_RUNS * $AVG_COST" | bc)
Full suite would have cost: \$$(echo "$TOTAL_RUNS * $FULL_COST" | bc)

TOTAL SAVINGS: \$$TOTAL_SAVINGS
EOF
```

### Monthly Report Automation

Add to crontab:
```bash
# Generate monthly cost report
0 0 1 * * /path/to/erlmcp/tools/incremental/cost-report.sh > /tmp/cost-report-$(date +\%Y-\%m).txt
```

---

## 6. Team Onboarding

### New Developer Setup

```bash
# Clone repo
git clone https://github.com/yourorg/erlmcp.git
cd erlmcp

# Install dependencies
make deps

# Initialize incremental validation
make incremental-init

# Install git hooks
cp tools/incremental/git-hooks/* .git/hooks/
chmod +x .git/hooks/*

# Run first validation
make incremental-validate
```

### Team Documentation

Add to `CONTRIBUTING.md`:

```markdown
## Validation Workflow

We use incremental validation to optimize CI/CD costs.

### Quick Validation
```bash
make incremental-validate
```

### Full Validation
```bash
make validate
```

### Cache Management
```bash
# View cache stats
make incremental-stats

# Rebuild cache
./tools/incremental/cache-manager.sh rebuild
```

---

## 7. Monitoring & Metrics

### Dashboard Integration

Send validation metrics to your monitoring system:

```bash
#!/usr/bin/env bash
# tools/incremental/send-metrics.sh

TIMESTAMP=$(date -u +%Y-%m-%dT%H:%M:%SZ)
GATES_RUN=$(jq -r '[to_entries[] | select(.value == true)] | length' .erlmcp/cache/gate-plan.json)
COST=$(./tools/incremental/estimate-cost.sh | grep "Total estimated cost" | awk '{print $4}')

# Send to monitoring system (example: Prometheus pushgateway)
cat <<EOF | curl --data-binary @- http://pushgateway:9091/metrics/job/erlmcp_validation
# TYPE erlmcp_validation_gates_run gauge
erlmcp_validation_gates_run $GATES_RUN $TIMESTAMP

# TYPE erlmcp_validation_cost gauge
erlmcp_validation_cost{currency="USD"} ${COST#$} $TIMESTAMP
EOF
```

---

## 8. Advanced Configuration

### Custom Gate Triggers

Edit `.erlmcp/cache/gate-mapping.json`:

```json
{
  "gates": {
    "benchmarks": {
      "trigger": "perf_critical_change",
      "modules": [
        "erlmcp_json_rpc",
        "erlmcp_registry",
        "erlmcp_cache",
        "your_custom_module"
      ]
    }
  }
}
```

### Per-Branch Configuration

```bash
# Different strategies for different branches
if [[ "$GITHUB_REF" == "refs/heads/main" ]]; then
    export FORCE_ALL_GATES=true
else
    export FORCE_ALL_GATES=false
fi
```

---

## 9. Migration Checklist

- [ ] Install dependencies (jq, bc, git)
- [ ] Initialize cache: `make incremental-init`
- [ ] Add Makefile targets
- [ ] Install git hooks
- [ ] Update CI/CD pipelines
- [ ] Train team on new workflow
- [ ] Monitor cost savings
- [ ] Update documentation

---

## 10. Rollback Plan

If issues arise, revert to full validation:

```bash
# Disable incremental validation in CI
git checkout .github/workflows/ci.yml

# Remove git hooks
rm .git/hooks/pre-commit .git/hooks/pre-push

# Continue using full validation
make validate
```

---

## Support

For issues or questions:
- Check [INCREMENTAL_VALIDATION_DESIGN.md](../../INCREMENTAL_VALIDATION_DESIGN.md)
- Review [tools/incremental/README.md](README.md)
- Open GitHub issue with label `incremental-validation`
