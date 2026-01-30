# Quality Gates System - erlmcp

This document explains the comprehensive quality gate enforcement system in erlmcp, which implements manufacturing-grade quality standards (99.99966% defect-free delivery).

## Overview

The quality gates system ensures that all code meets strict standards before being committed or deployed. It's built on top of Claude Code CLI and integrates seamlessly with the development workflow.

## Quality Gate Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                    Quality Gates System                       │
├─────────────────────────────────────────────────────────────┤
│  ┌─────────────────┐    ┌─────────────────┐    ┌───────────┐ │
│  │ Pre-commit Hook  │    │ CI/CD Pipeline  │    │ Manual    │ │
│  │ (Blocking)      │    │ (Automated)     │    │ Validator │ │
│  └─────────────────┘    └─────────────────┘    └───────────┘ │
│           │                       │                       │  │
│           └───────────────────────┼───────────────────────┘  │
│                                  │                        │
│                    ┌─────────────────────────────────────┐   │
│                    │     Quality Gate Enforcer          │   │
│                    │  (tools/quality-gate-enforcer.sh) │   │
│                    └─────────────────────────────────────┘   │
│                                  │                        │
│            ┌─────────────────────┼─────────────────────┐   │
│            │                     │                     │   │
│   ┌─────────────┐   ┌─────────────┐   ┌─────────────┐   │   │
│   │ Compilation │   │   Tests    │   │  Coverage  │   │   │
│   │ (Blocking)  │   │ (Blocking) │   │ (Blocking) │   │   │
│   └─────────────┘   └─────────────┘   └─────────────┘   │   │
│            │                     │                     │   │
│   ┌─────────────┐   ┌─────────────┐   ┌─────────────┐   │   │
│   │   Dialyzer  │   │    Xref     │   │  Security   │   │   │
│   │(Reporting)  │   │(Reporting)  │   │ (Blocking)  │   │   │
│   └─────────────┘   └─────────────┘   └─────────────┘   │   │
│                         │                     │           │   │
│            ┌─────────────┼─────────────────────┼───────────┤   │
│            │            │                     │           │   │
│   ┌─────────────┐   ┌─────────────┐   ┌─────────────┐   │   │
│   │  Benchmark  │   │   Format    │   │Documentation│   │   │
│   │(Reporting)  │   │ (Blocking)  │   │ (Blocking)  │   │   │
│   └─────────────┘   └─────────────┘   └─────────────┘   │   │
└─────────────────────────────────────────────────────────────┘
```

## Quality Gate Levels

### 🚨 BLOCKING Violations (MUST pass)
- **Compilation**: 0 errors
- **Tests**: 100% pass rate
- **Coverage**: 80%+ minimum
- **Code Format**: 100% formatted
- **Security**: 0 vulnerabilities

### ⚠️ REPORTING Issues (Should pass)
- **Dialyzer**: Type warnings
- **Xref**: Undefined functions
- **Performance**: <10% regression

## Usage

### 1. Pre-commit Quality Gates

The pre-commit hook automatically runs before every commit:

```bash
# Try to commit
git commit -m "feat: add new feature"

# Hook will run and block if violations found:
# ❌ Quality gates failed - commit blocked
# To fix:
# 1. Address issues above
# 2. Run validation script
# 3. Stage fixes: git add <files>
# 4. Retry commit
```

### 2. Manual Quality Validation

Run the quality gate enforcer manually:

```bash
# Quick check (blocking gates only)
./tools/quality-gate-enforcer.sh

# Full quality report (including reporting gates)
./tools/quality-gate-enforcer.sh --full

# Specific validation
./tools/claude-md-enforcer.sh  # CLAUDE.md rules
```

### 3. CI/CD Pipeline Integration

The GitHub Actions workflow runs on every push and PR:

```yaml
# .github/workflows/quality-gates.yml
# Runs automatically on:
# - push to main/integration/*
# - pull requests
# - daily at 2 AM UTC
```

## Quality Gate Details

### 1. Compilation Gate (0 errors)

```bash
# Enforced at:
# - Pre-commit hook
# - CI/CD pipeline
# - Manual validation

TERM=dumb rebar3 compile
# Expected: 0 errors, any warnings reviewed
```

### 2. Test Quality Gate (100% pass rate)

```bash
# EUnit tests
rebar3 eunit --verbose
# Expected: All tests passed

# Common Test
rebar3 ct --suite=test/suite_name --verbose
# Expected: All tests passed
```

### 3. Coverage Gate (80%+ minimum)

```bash
# Run coverage analysis
rebar3 cover --verbose

# Required minimums:
# - Total: 80%
# - Core modules: 85%
# - Public APIs: 100%
```

### 4. Type Safety (Dialyzer)

```bash
# Build PLT (first time)
rebar3 dialyzer --plt

# Run type checking
rebar3 dialyzer
# Expected: 0 errors (warnings reviewed)
```

### 5. Cross-reference (Xref)

```bash
# Check for undefined functions
rebar3 xref
# Expected: 0 undefined functions
```

### 6. Code Quality Tools

```bash
# Format checking
rebar3 format --verify
# Expected: All files formatted

# Linting
rebar3 lint
# Expected: No lint issues
```

### 7. Security Scanning

```bash
# Check for hardcoded secrets
grep -r "password\|secret\|api_key\|token" src/
# Expected: 0 matches (excluding documentation)
```

### 8. Performance Benchmarks

```bash
# Run full benchmark suite
./scripts/bench/run_all_benchmarks.sh

# Performance regression check
# Maximum allowed: 10% from baseline
```

## Zero Tolerance Policy

The following rules are **NON-NEGOTIABLE**:

1. **No untyped functions** - All exported functions must have type specs
2. **No test failures** - 100% pass rate required
3. **No compilation errors** - 0 errors mandatory
4. **Minimum coverage** - 80% overall, 85% core, 100% APIs
5. **No hardcoded secrets** - Security violations block deployment
6. **Code format compliance** - All code must be formatted

## Quality Gate Outputs

### Success Example
```
🔍 erlmcp Quality Gate Enforcer
==========================================
✅ Compilation: 0 errors
✅ Tests: 42/42 passed (100%)
✅ Coverage: 85% (≥80%)
✅ Dialyzer: 0 type errors
✅ Format: All files formatted
✅ Security: No vulnerabilities found
✅ Documentation: All modules documented

Quality Score: 100/100
🎯 PERFECT QUALITY GATE: All requirements met
```

### Failure Example
```
🔍 erlmcp Quality Gate Enforcer
==========================================
❌ Compilation: 2 errors found
❌ Tests: 40/42 passed (95.2% < 100%)
❌ Coverage: 78% (<80% minimum)

❌ 3 quality gate violations found
🚨 Quality gates failed - cannot proceed
```

## Troubleshooting

### Common Issues

1. **Compilation Errors**
   ```
   ❌ Compilation failed - fix errors before commit
   Solution: Run rebar3 compile and fix all errors
   ```

2. **Test Failures**
   ```
   ❌ Tests failed - fix before committing
   Solution: Run rebar3 eunit && rebar3 ct, debug failures
   ```

3. **Low Coverage**
   ```
   ❌ Coverage 78% < 80% minimum
   Solution: Add tests for uncovered lines
   ```

4. **Formatting Issues**
   ```
   ❌ Code formatting issues found
   Solution: Run rebar3 format
   ```

### Skipping Validation (Not Recommended)

```bash
# Skip pre-commit validation (not recommended)
git commit --no-verify

# Use only for exceptional circumstances
# All quality checks still required before deployment
```

## Continuous Improvement

### Quality Metrics Tracking

The system tracks quality metrics over time:

- **Compilation success rate**: Target 100%
- **Test pass rate**: Target 100%
- **Coverage trend**: Target ≥80% and increasing
- **Type safety**: Target 0 dialyzer errors
- **Security incidents**: Target 0

### Quality Reports

Daily comprehensive reports are generated and include:

1. Code quality metrics
2. Test coverage trends
3. Performance benchmarks
4. Security scan results
5. Quality gate violations

### Continuous Optimization

1. **Weekly**: Review quality metrics and trends
2. **Monthly**: Analyze benchmark results for optimizations
3. **Quarterly**: Review and update quality standards based on feedback

## Integration with Development Workflow

### IDE Integration

Configure your editor to run quality checks automatically:

```json
// .vscode/settings.json
{
    "rebar3.command.executable": "rebar3",
    "rebar3.command.tasks": [
        {
            "type": "compile",
            "before": "Running compilation...",
            "after": "Compilation complete"
        },
        {
            "type": "eunit",
            "before": "Running tests...",
            "after": "Tests complete"
        }
    ]
}
```

### Development Commands

```bash
# Quick development cycle
make console        # Erlang shell with hot reload
make check          # Full quality check
make clean          # Clean build artifacts

# Validation commands
./tools/quality-gate-enforcer.sh
./tools/claude-md-enforcer.sh
```

## Conclusion

The erlmcp quality gate system ensures manufacturing-grade code quality through:

1. **Automatic enforcement** at every development stage
2. **Comprehensive coverage** of quality dimensions
3. **Transparent reporting** with clear pass/fail criteria
4. **Continuous improvement** through metrics tracking
5. **Zero tolerance** for blocking violations

This system ensures that erlmcp delivers production-ready code that meets the highest quality standards expected in enterprise systems.

## Related Documentation

- [CLAUDE.md](../CLAUDE.md) - Main development configuration
- [Architecture](./architecture.md) - System architecture
- [OTP Patterns](./otp-patterns.md) - OTP design patterns
- [Protocol](./protocol.md) - MCP protocol implementation
- [API Reference](./api-reference.md) - API documentation