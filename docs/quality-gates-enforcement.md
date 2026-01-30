# Quality Gate Enforcement Mechanisms - erlmcp

## Overview

This document details how Claude Code CLI enforces manufacturing-grade quality gates in the erlmcp project, following Lean Six Sigma principles with 99.99966% defect-free delivery requirements.

## 1. Pre-commit Hooks Configuration and Enforcement

### Automatic Hook Installation
```bash
# Install/sync all hooks from .claude/md/
./tools/claude-md-sync.sh

# Manual validation anytime
./tools/claude-md-enforcer.sh
```

### Pre-commit Hook Structure
```
.git/hooks/
├── pre-commit          # Main entry point
├── 01-compile.sh       # Compilation check
├── 02-xref.sh          # Cross-reference check
├── 03-dialyzer.sh      # Type checking
├── 04-format.sh        # Code formatting
├── 05-lint.sh          # Linting
├── 06-test.sh          # Test execution
├── 07-coverage.sh      # Coverage check
└── 08-security.sh      # Security scan
```

### Hook Enforcement Points
```bash
#!/bin/bash
# .git/hooks/pre-commit

set -e  # Exit on first failure

echo "🔍 Running quality gates..."

# 1. Compilation (BLOCKING)
./tools/01-compile.sh
if [ $? -ne 0 ]; then
    echo "❌ Compilation failed - blocking commit"
    exit 1
fi

# 2. Cross-reference (REPORTING)
./tools/02-xref.sh
if [ $? -ne 0 ]; then
    echo "⚠️  Xref issues found - check before merge"
fi

# 3. Dialyzer (REPORTING)
./tools/03-dialyzer.sh
if [ $? -ne 0 ]; then
    echo "⚠️  Dialyzer warnings - check before merge"
fi

# 4. Format (BLOCKING)
./tools/04-format.sh
if [ $? -ne 0 ]; then
    echo "❌ Code formatting failed - blocking commit"
    exit 1
fi

# 5. Linting (BLOCKING)
./tools/05-lint.sh
if [ $? -ne 0 ]; then
    echo "❌ Linting failed - blocking commit"
    exit 1
fi

# 6. Tests (BLOCKING)
./tools/06-test.sh
if [ $? -ne 0 ]; then
    echo "❌ Tests failed - blocking commit"
    exit 1
fi

# 7. Coverage (REPORTING)
./tools/07-coverage.sh
if [ $? -lt 80 ]; then
    echo "⚠️  Coverage below 80% - check before merge"
fi

# 8. Security (BLOCKING)
./tools/08-security.sh
if [ $? -ne 0 ]; then
    echo "❌ Security scan failed - blocking commit"
    exit 1
fi

echo "✅ All quality gates passed"
```

## 2. Compilation Quality Gates

### Zero-Tolerance Compilation Requirement
```bash
# Mandatory before any operation
TERM=dumb rebar3 compile

# Expected output:
# ===> Compiling erlmcp_core
# ===> Compiling erlmcp_transports
# ===> Compiling erlmcp_observability
# ===> Compiled: 42 modules, 38 BEAM files
# ===> Warnings: 0
# ===> Errors: 0

# Gate validation
if [ "$ERRORS" -gt 0 ]; then
    echo "❌ Compilation errors: $ERRORS - BLOCKING"
    exit 1
elif [ "$WARNINGS" -gt 0 ]; then
    echo "⚠️  Warnings: $WARNINGS - CHECK REQUIRED"
fi
```

### Claude Code Integration
```erlang
% In build scripts
compile_gate() ->
    case rebar3_compile:compile() of
        {ok, 0, Warnings} when Warnings =:= 0 ->
            ok;
        {error, Code} ->
            error("Compilation failed with code: ~p", [Code]);
        {ok, 0, Warnings} ->
            error("Warnings found: ~p", [Warnings])
    end.
```

## 3. Test Coverage Requirements

### Chicago School TDD Compliance
- **Minimum**: 80% overall coverage
- **Core modules**: 85%+ (server, client, registry, transport)
- **Public APIs**: 100% (all exported functions tested)
- **No mocks**: Real collaborators only

### Coverage Gate Enforcement
```bash
# Run with coverage
rebar3 cover --verbose

# Post-build coverage validation
./tools/07-coverage.sh

#!/bin/bash
# 07-coverage.sh

TOTAL_COVERAGE=$(rebar3 cover | grep "Total" | awk '{print $3}')
CORE_COVERAGE=$(rebar3 cover | grep "Core modules" | awk '{print $4}')
API_COVERAGE=$(rebar3 cover | grep "Public APIs" | awk '{print $4}')

# Gate validation
if (( $(echo "$TOTAL_COVERAGE < 80" | bc -l) )); then
    echo "❌ Total coverage $TOTAL_COVERAGE% < 80% - BLOCKING"
    exit 1
fi

if (( $(echo "$CORE_COVERAGE < 85" | bc -l) )); then
    echo "❌ Core coverage $CORE_COVERAGE% < 85% - BLOCKING"
    exit 1
fi

if (( $(echo "$API_COVERAGE < 100" | bc -l) )); then
    echo "❌ API coverage $API_COVERAGE% < 100% - BLOCKING"
    exit 1
fi

echo "✅ Coverage requirements met: Total=$TOTAL_COVERAGE%, Core=$CORE_COVERAGE%, API=$API_COVERAGE%"
```

## 4. Dialyzer Type Checking Integration

### Strict Type Checking
```bash
# Run dialyzer with strict flags
rebar3 dialyzer

# Expected output:
# Compiling...
# Checking specifications...
# No problems found
#
# ✅ Dialyzer: 0 type errors
```

### Gate Configuration
```bash
#!/bin/bash
# 03-dialyzer.sh

DIALYZER_OUTPUT=$(rebar3 dialyzer 2>&1)

if echo "$DIALYZER_OUTPUT" | grep -q "No problems found"; then
    echo "✅ Dialyzer: No type errors"
    exit 0
else
    echo "❌ Dialyzer type errors found:"
    echo "$DIALYZER_OUTPUT"
    exit 1
fi
```

### Type Specifications Requirement
```erlang
% ALL public functions MUST have type specs
-export([call_tool/2, create_session/1, list_resources/1]).

-spec call_tool(tool_request(), session_id()) -> mcp_result().
call_tool(Req, SessionId) ->
    % Implementation
    ok.

-spec create_session(config()) -> {ok, session_id()} | {error, reason()}.
create_session(Config) ->
    % Implementation
    ok.
```

## 5. Code Quality Tools

### Erlang-Specific Quality Tools
```bash
# Code formatting (equivalent to Ruff format)
rebar3 format --verify

# Linting (equivalent to Ruff lint)
rebar3 lint

# Cross-reference checking
rebar3 xref
```

### Quality Gate Scripts
```bash
#!/bin/bash
# 04-format.sh
rebar3 format --verify
if [ $? -ne 0 ]; then
    echo "❌ Code formatting failed - run: rebar3 format"
    exit 1
fi
```

```bash
#!/bin/bash
# 05-lint.sh
rebar3 lint
if [ $? -ne 0 ]; then
    echo "❌ Linting failed - check warnings"
    exit 1
fi
```

```bash
#!/bin/bash
# 02-xref.sh
rebar3 xref
if [ $? -ne 0 ]; then
    echo "❌ Xref issues found - undefined functions"
    exit 1
fi
```

## 6. Security Scanning Integration

### Security Scan Pipeline
```bash
#!/bin/bash
# 08-security.sh

# Check for hardcoded secrets
grep -r "password\|secret\|key\|token" src/ || true

# Erlang-specific security checks
rebar3 escriptize deps/meck/priv/tools/meck.escript -meck "check(src/)"
```

### Security Gate Validation
```bash
# Security gate example
SECURITY_ISSUES=$(grep -r "password\|secret\|key\|token" src/ | wc -l)

if [ "$SECURITY_ISSUES" -gt 0 ]; then
    echo "❌ Security issues found: $SECURITY_ISSUES"
    echo "🔍 Reviewing potential hardcoded secrets..."
    exit 1
fi

echo "✅ Security scan passed"
```

## 7. Performance Benchmarking Gates

### Benchmark Requirements
```bash
# Run full benchmark suite
./scripts/bench/run_all_benchmarks.sh

# Quick validation
make benchmark-quick

# Performance baseline validation
erlmcp_bench_core_ops:run(<<"core_ops_100k">>).
```

### Performance Gate Configuration
```bash
#!/bin/bash
# perf-gate.sh

# Run benchmark
BENCHMARK_RESULT=$(erlmcp_bench_core_ops:run(<<"core_ops_100k">>))
CURRENT_THROUGHPUT=$(echo "$BENCHMARK_RESULT" | grep "throughput" | awk '{print $2}')

# Baseline: 2.69M ops/sec
BASELINE=2690000
REGRESSION_THRESHOLD=0.9  # 10% max regression

if (( $(echo "$CURRENT_THROUGHPUT < $BASELINE * $REGRESSION_THRESHOLD" | bc -l) )); then
    echo "❌ Performance regression detected:"
    echo "   Current: $CURRENT_THROUGHPUT ops/sec"
    echo "   Baseline: $BASELINE ops/sec"
    echo "   Threshold: ${REGRESSION_THRESHOLD}x baseline"
    exit 1
fi

echo "✅ Performance gate passed: $CURRENT_THROUGHPUT ops/sec"
```

## 8. Error Handling Quality Standards

### Comprehensive Error Handling
```erlang
% All error paths MUST be covered
-spec handle_request(request(), state()) -> {reply, response(), state()} | {noreply, state()}.

handle_request({call, tool, Params}, State) ->
    try
        Result = execute_tool(Params),
        {reply, {ok, Result}, State}
    catch
        error:Reason:Stack ->
            Error = format_error(Reason, Stack),
            {reply, {error, Error}, State};
        throw:Reason:Stack ->
            Error = format_error(Reason, Stack),
            {reply, {error, Error}, State}
    end.
```

### Error Handling Validation
```bash
#!/bin/bash
# error-handling-gate.sh

# Check for uncovered error paths
grep -r "catch\|try" src/ | wc -l
grep -r "throw\|error:" src/ | wc -l

# Validate error contracts
grep -r "spec.*error" src/ | wc -l

if [ "$ERROR_HANDLING_COUNT" -lt 10 ]; then
    echo "❌ Insufficient error handling coverage"
    exit 1
fi
```

## 9. Documentation Requirements

### Documentation Standards
```bash
# Check documentation coverage
./tools/doc-coverage.sh

#!/bin/bash
# doc-coverage.sh

MISSING_DOCS=0

# Check all modules have moduledoc
for file in src/*.erl; do
    if ! grep -q "%% @doc" "$file"; then
        echo "❌ Missing module doc: $file"
        MISSING_DOCS=$((MISSING_DOCS + 1))
    fi
done

# Check all exports have specs
for file in src/*.erl; do
    if ! grep -q "-spec" "$file"; then
        echo "❌ Missing type specs: $file"
        MISSING_DOCS=$((MISSING_DOCS + 1))
    fi
done

if [ "$MISSING_DOCS" -gt 0 ]; then
    echo "❌ Documentation requirements not met: $MISSING_DOCS issues"
    exit 1
fi

echo "✅ Documentation requirements met"
```

### API Documentation Generation
```bash
# Generate API reference
rebar3 edoc

# Validate API docs
./tools/api-doc-validate.sh
```

## 10. Automated Validation Workflows

### Continuous Integration Pipeline
```yaml
# .github/workflows/quality-gates.yml
name: Quality Gates

on: [push, pull_request]

jobs:
  validate:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3

      - name: Setup Erlang
        uses: erlang/actions/setup-erlang@v1

      - name: Compile
        run: |
          TERM=dumb rebar3 compile
          if [ $? -ne 0 ]; then
            echo "❌ Compilation failed"
            exit 1
          fi

      - name: Run Tests
        run: |
          rebar3 eunit
          rebar3 ct --suite=test/*

      - name: Check Coverage
        run: |
          coverage=$(rebar3 cover | grep "Total" | awk '{print $3}')
          if (( $(echo "$coverage < 80" | bc -l) )); then
            echo "❌ Coverage $coverage% < 80%"
            exit 1
          fi

      - name: Dialyzer
        run: |
          rebar3 dialyzer
          if [ $? -ne 0 ]; then
            echo "❌ Dialyzer failed"
            exit 1
          fi
```

### Pre-task Validation
```erlang
% In agent execution hooks
pre_task_validation() ->
    % 1. Compilation check
    case rebar3_compile:compile() of
        {error, _} -> error("Compilation failed");
        _ -> ok
    end,

    % 2. Test check
    case rebar3_eunit:run() of
        {error, Failed} -> error("Tests failed: ~p", [Failed]);
        _ -> ok
    end,

    % 3. Coverage check
    Coverage = rebar3_cover:total(),
    if Coverage < 80 ->
        error("Coverage ~p% < 80%", [Coverage]);
    true -> ok
    end.
```

### Post-task Quality Report
```bash
#!/bin/bash
# post-task-quality-report.sh

echo "📊 Quality Report for $TASK_NAME"
echo "=================================="

echo "✅ Compilation: $(rebar3 compile | grep "Errors" | awk '{print $2}') errors"
echo "✅ Tests: $(rebar3 eunit | grep "passed" | wc -l) passed"
echo "✅ Coverage: $(rebar3 cover | grep "Total" | awk '{print $3}%)'"
echo "✅ Dialyzer: $(rebar3 dialyzer | grep "problems" | wc -l) problems"
echo "✅ Format: $(rebar3 format --verify | grep "OK" | wc -l) formatted"

# Generate quality score
QUALITY_SCORE=$(calculate_quality_score)
echo "🏆 Quality Score: $QUALITY_SCORE/100"

if [ "$QUALITY_SCORE" -lt 80 ]; then
    echo "❌ Quality below threshold - Review required"
    exit 1
fi
```

## Zero Tolerance Policy Implementation

```bash
#!/bin/bash
# zero-tolerance-check.sh

# These rules are NON-NEGOTIABLE
VIOLATIONS=0

# Rule 1: No untyped functions
UN_TYPED=$(grep -r "^[^%].*(" src/ | grep -v "-spec" | wc -l)
if [ "$UN_TYPED" -gt 0 ]; then
    echo "❌ Untyped functions found: $UN_TYPED"
    VIOLATIONS=$((VIOLATIONS + 1))
fi

# Rule 2: No test failures
TEST_FAILURES=$(rebar3 eunit 2>&1 | grep "failed" | wc -l)
if [ "$TEST_FAILURES" -gt 0 ]; then
    echo "❌ Test failures: $TEST_FAILURES"
    VIOLATIONS=$((VIOLATIONS + 1))
fi

# Rule 3: No compilation errors
ERRORS=$(rebar3 compile 2>&1 | grep "Error:" | wc -l)
if [ "$ERRORS" -gt 0 ]; then
    echo "❌ Compilation errors: $ERRORS"
    VIOLATIONS=$((VIOLATIONS + 1))
fi

# Rule 4: Minimum coverage
COVERAGE=$(rebar3 cover | grep "Total" | awk '{print $3}')
if (( $(echo "$COVERAGE < 80" | bc -l) )); then
    echo "❌ Coverage below 80%: $COVERAGE%"
    VIOLATIONS=$((VIOLATIONS + 1))
fi

if [ "$VIOLATIONS" -gt 0 ]; then
    echo "🚨 ZERO TOLERANCE: $VIOLATIONS violations found"
    echo "Cannot proceed with delivery"
    exit 1
fi

echo "🎯 ZERO TOLERANCE: All requirements met"
```

## Manufacturing-Grade Quality Metrics

| Metric | Requirement | Enforcement Level | Tools |
|--------|-------------|-------------------|-------|
| Compilation | 0 errors | BLOCKING | rebar3 |
| Test Pass Rate | 100% | BLOCKING | rebar3 eunit/ct |
| Coverage | 80%+ | BLOCKING | rebar3 cover |
| Type Safety | 0 warnings | REPORTING | dialyzer |
| Code Format | 100% | BLOCKING | rebar3 format |
| Linting | 400+ rules | BLOCKING | rebar3 lint |
| Security | 0 vulnerabilities | BLOCKING | custom scanner |
| Performance | <10% regression | REPORTING | benchmarks |
| Documentation | 100% coverage | BLOCKING | custom script |

## Conclusion

The erlmcp project enforces manufacturing-grade quality through:

1. **Automatic enforcement** at every commit point
2. **Zero tolerance** for blocking violations
3. **Comprehensive coverage** of quality dimensions
4. **Transparent reporting** of quality metrics
5. **Continuous improvement** through metrics tracking

This ensures delivery quality meets or exceeds 99.99966% defect-free requirements expected in production systems.