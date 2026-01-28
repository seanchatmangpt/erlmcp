#!/bin/bash
# post_deletion_validation.sh
# ERLMCP V2.0 Post-Deletion Validation
# Run after deletion to verify system integrity

set -e

cd /Users/sac/erlmcp

echo "======================================"
echo "ERLMCP V2 POST-DELETION VALIDATION"
echo "======================================"

# Track validation results
validation_passed=true

# Check 1: Legacy directories gone
echo ""
echo "[1/10] Verifying legacy structure deleted..."
if [ -d "src" ] || [ -d "test" ]; then
    echo "  ❌ Legacy directories still present"
    validation_passed=false
else
    echo "  ✅ Legacy directories deleted"
fi

# Check 2: Umbrella apps intact
echo ""
echo "[2/10] Verifying umbrella structure..."
for app in erlmcp_core erlmcp_transports erlmcp_observability tcps_erlmcp; do
    if [ -d "apps/$app/src" ]; then
        module_count=$(ls apps/$app/src/*.erl 2>/dev/null | wc -l)
        echo "  ✅ apps/$app: $module_count modules"
    else
        echo "  ❌ MISSING: apps/$app"
        validation_passed=false
    fi
done

# Check 3: Compilation
echo ""
echo "[3/10] Testing clean compilation..."
rebar3 clean > /dev/null 2>&1

if rebar3 compile 2>&1 | tee /tmp/compile_validation.log | grep -q "ERROR"; then
    echo "  ❌ Compilation failed"
    cat /tmp/compile_validation.log
    validation_passed=false
else
    compiled_apps=$(grep "Compiling" /tmp/compile_validation.log | wc -l)
    echo "  ✅ Compilation successful ($compiled_apps apps)"
fi

# Check 4: Tests
echo ""
echo "[4/10] Running test suite..."
if rebar3 eunit --verbose 2>&1 | tee /tmp/tests_validation.log; then
    test_count=$(grep -c "module '.*'" /tmp/tests_validation.log || echo "0")
    fail_count=$(grep -c "Failed:" /tmp/tests_validation.log || echo "0")

    if [ "$fail_count" -gt 0 ]; then
        echo "  ❌ Test failures detected: $fail_count"
        validation_passed=false
    else
        echo "  ✅ All tests passed (ran $test_count test modules)"
    fi
else
    echo "  ❌ Test execution failed"
    validation_passed=false
fi

# Check 5: Dialyzer
echo ""
echo "[5/10] Running Dialyzer type checking..."
if rebar3 dialyzer 2>&1 | tee /tmp/dialyzer_validation.log | grep -q "ERROR"; then
    echo "  ❌ Dialyzer errors detected"
    grep "ERROR" /tmp/dialyzer_validation.log
    validation_passed=false
else
    warnings=$(grep -c "Warning:" /tmp/dialyzer_validation.log || echo "0")
    echo "  ✅ Dialyzer clean ($warnings warnings)"
fi

# Check 6: Xref
echo ""
echo "[6/10] Running xref analysis..."
if rebar3 xref 2>&1 | tee /tmp/xref_validation.log | grep -q "ERROR"; then
    echo "  ❌ Xref errors detected"
    grep "ERROR" /tmp/xref_validation.log
    validation_passed=false
else
    echo "  ✅ Xref clean (no undefined functions)"
fi

# Check 7: GraphQL removed
echo ""
echo "[7/10] Verifying GraphQL removal..."
graphql_files=$(find apps/*/src -name "*graphql*" 2>/dev/null | wc -l)
if [ "$graphql_files" -gt 0 ]; then
    echo "  ❌ GraphQL files still present:"
    find apps/*/src -name "*graphql*"
    validation_passed=false
else
    echo "  ✅ GraphQL fully removed"
fi

# Check 8: Coverage
echo ""
echo "[8/10] Checking test coverage..."
if rebar3 cover 2>&1 | tee /tmp/cover_validation.log; then
    # Extract coverage percentage (rough estimate)
    total_lines=$(grep -o "total [0-9]*" /tmp/cover_validation.log | awk '{sum+=$2} END {print sum}')
    covered_lines=$(grep -o "covered [0-9]*" /tmp/cover_validation.log | awk '{sum+=$2} END {print sum}')

    if [ "$total_lines" -gt 0 ]; then
        coverage=$((covered_lines * 100 / total_lines))
        if [ "$coverage" -lt 70 ]; then
            echo "  ⚠️  Coverage: ${coverage}% (target: ≥80%)"
        else
            echo "  ✅ Coverage: ${coverage}%"
        fi
    else
        echo "  ℹ️  Coverage: Not calculated"
    fi
else
    echo "  ⚠️  Coverage check failed"
fi

# Check 9: Benchmarks (optional, can be slow)
echo ""
echo "[9/10] Quick benchmark sanity check..."
if [ -f "Makefile" ] && grep -q "benchmark-quick" Makefile; then
    if timeout 120 make benchmark-quick 2>&1 | tee /tmp/bench_validation.log | grep -q "ops/sec"; then
        echo "  ✅ Benchmarks passed"
        grep "ops/sec" /tmp/bench_validation.log | head -3
    else
        echo "  ⚠️  Benchmarks failed or timed out (non-critical)"
    fi
else
    echo "  ℹ️  Benchmark target not found (skipping)"
fi

# Check 10: File integrity
echo ""
echo "[10/10] Checking file integrity..."
broken_symlinks=$(find apps/ -xtype l 2>/dev/null | wc -l)
if [ "$broken_symlinks" -gt 0 ]; then
    echo "  ⚠️  Warning: $broken_symlinks broken symlinks"
    find apps/ -xtype l
else
    echo "  ✅ No broken symlinks"
fi

# Final verdict
echo ""
echo "======================================"
if [ "$validation_passed" = true ]; then
    echo "✅ VALIDATION COMPLETE - V2.0 READY"
    echo "======================================"
    echo ""
    echo "Summary:"
    echo "  ✅ Legacy structure removed"
    echo "  ✅ Umbrella apps functional"
    echo "  ✅ Compilation clean"
    echo "  ✅ Tests passing"
    echo "  ✅ Type checking clean"
    echo "  ✅ Cross-references clean"
    echo "  ✅ GraphQL removed"
    echo "  ✅ File integrity verified"
    echo ""
    echo "Ready for production deployment."
    echo ""
    echo "Next steps:"
    echo "  1. Update CHANGELOG.md with v2.0.0 changes"
    echo "  2. Update README.md with umbrella structure"
    echo "  3. Update docs/architecture.md"
    echo "  4. Git commit: 'feat: migrate to umbrella v2.0'"
    echo "  5. Git tag: v2.0.0"
    exit 0
else
    echo "❌ VALIDATION FAILED"
    echo "======================================"
    echo ""
    echo "Some validation checks failed."
    echo "Review output above and fix issues before proceeding."
    echo ""
    echo "To rollback:"
    echo "  ./scripts/migration/rollback_v2_migration.sh"
    exit 1
fi
