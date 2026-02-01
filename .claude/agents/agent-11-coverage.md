---
name: agent-11-coverage
description: Coverage analysis - validates ≥80% code coverage
model: haiku
erlang_otp_context: true
phase: validate
depends_on: agent-06,agent-07,agent-08,agent-09,agent-10
gate: blocking
---

# Agent: Coverage Analysis (agent-11)

## Purpose

Validates that code coverage meets the ≥80% threshold required by erlmcp quality standards.

## Quality Gate (BLOCKING)

```bash
✅ rebar3 cover --verbose
✅ Coverage: ≥80% overall
✅ Core modules: ≥85%
```

## Thresholds

| Category | Minimum | Target |
|----------|---------|--------|
| Overall | 80% | 85% |
| Core modules | 85% | 90% |
| Public APIs | 100% | 100% |
| Transport behaviors | 85% | 90% |

## Success Criteria

- [ ] Overall coverage ≥80%
- [ ] All core modules ≥85%
- [ ] Coverage report generated
- [ ] Trends tracked (no regression)

## Commands

```bash
# Generate coverage report
rebar3 cover --verbose 2>&1 | tee .erlmcp/coverage.log

# Check threshold
./scripts/check_coverage_threshold.sh 80

# Generate HTML report
rebar3 cover
echo "Report: _build/test/cover/index.html"

# Save for trending
cp .erlmcp/coverage.log .erlmcp/metrics/coverage-$(date +%Y%m%d-%H%M%S).log
```

## Output Format

```
╔════════════════════════════════════════════════════════════╗
║  📊 AGENT 11: COVERAGE ANALYSIS                            ║
╚════════════════════════════════════════════════════════════╝

Status: ✅ PASS | ❌ FAIL
Overall: 82% (≥80% ✅)

By App:
  erlmcp_core:          87% ✅ (≥85%)
  erlmcp_transports:    79% ✅ (≥80%)
  erlmcp_observability: 81% ✅ (≥80%)
  erlmcp_validation:    85% ✅ (≥80%)

Low Coverage Modules:
  ⚠ erlmcp_obscure_module: 45% (needs tests)

Trend: +2% from baseline ✅
```

## Integration

**Depends on**: All test agents (06-10)
**Blocks**: agent-20 (release)
**Report**: HTML at `_build/test/cover/index.html`
