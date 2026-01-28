#!/usr/bin/env bash
# quality-report.sh - Comprehensive quality report with recommendations
# Usage: ./quality-report.sh [--period week|month]

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
METRICS_DIR="$PROJECT_ROOT/metrics/snapshots"
PERIOD="week"

# Parse arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        --period)
            PERIOD="$2"
            shift 2
            ;;
        *)
            echo "Unknown option: $1"
            echo "Usage: $0 [--period week|month]"
            exit 1
            ;;
    esac
done

cd "$PROJECT_ROOT"

# Determine days based on period
case $PERIOD in
    week) DAYS=7 ;;
    month) DAYS=30 ;;
    *) echo "Invalid period: $PERIOD"; exit 1 ;;
esac

echo "╔════════════════════════════════════════════════════════════════╗"
echo "║          erlmcp Comprehensive Quality Report                   ║"
echo "╚════════════════════════════════════════════════════════════════╝"
echo ""
echo "Period: Last $PERIOD ($DAYS days)"
echo "Generated: $(date -u +"%Y-%m-%d %H:%M:%S UTC")"
echo ""

# Find latest snapshot
LATEST_SNAPSHOT=$(find "$METRICS_DIR" -name "*.json" -type f | sort -r | head -1)

if [ -z "$LATEST_SNAPSHOT" ]; then
    echo "❌ No snapshots found. Run quality-snapshot.sh first."
    exit 1
fi

echo "Latest snapshot: $(basename "$LATEST_SNAPSHOT")"
echo ""

# Extract current metrics
TEST_PASS_RATE=$(jq -r '.tests.pass_rate // 0' "$LATEST_SNAPSHOT")
COVERAGE=$(jq -r '.coverage.percentage // 0' "$LATEST_SNAPSHOT")
COMPILE_WARNINGS=$(jq -r '.compilation.warnings // 0' "$LATEST_SNAPSHOT")
DIALYZER_WARNINGS=$(jq -r '.dialyzer.warnings // 0' "$LATEST_SNAPSHOT")
QUALITY_SCORE=$(jq -r '.quality_score.overall // 0' "$LATEST_SNAPSHOT")
LOC_SRC=$(jq -r '.code_metrics.loc_source // 0' "$LATEST_SNAPSHOT")
LOC_TEST=$(jq -r '.code_metrics.loc_tests // 0' "$LATEST_SNAPSHOT")
TEST_RATIO=$(jq -r '.code_metrics.test_to_code_ratio // 0' "$LATEST_SNAPSHOT")
MODULES=$(jq -r '.code_metrics.modules // 0' "$LATEST_SNAPSHOT")
AVG_LOC=$(jq -r '.code_metrics.avg_loc_per_module // 0' "$LATEST_SNAPSHOT")

echo "═══════════════════════════════════════════════════════════════"
echo "  CURRENT STATE"
echo "═══════════════════════════════════════════════════════════════"
echo ""
echo "Overall Quality Score: $QUALITY_SCORE / 100"
if (( $(echo "$QUALITY_SCORE >= 80" | bc -l) )); then
    echo "Status: ✅ EXCELLENT"
elif (( $(echo "$QUALITY_SCORE >= 60" | bc -l) )); then
    echo "Status: ⚠️  GOOD (room for improvement)"
else
    echo "Status: ❌ NEEDS ATTENTION"
fi
echo ""

echo "Testing Metrics:"
echo "  Test Pass Rate:        $TEST_PASS_RATE%"
echo "  Code Coverage:         $COVERAGE%"
echo "  Test/Code Ratio:       $TEST_RATIO"
echo ""

echo "Code Quality:"
echo "  Compile Warnings:      $COMPILE_WARNINGS"
echo "  Dialyzer Warnings:     $DIALYZER_WARNINGS"
echo ""

echo "Code Metrics:"
echo "  Source LOC:            $LOC_SRC"
echo "  Test LOC:              $LOC_TEST"
echo "  Modules:               $MODULES"
echo "  Avg LOC per Module:    $AVG_LOC"
echo ""

# Compare to previous period
CUTOFF_DATE=$(date -u -v-${DAYS}d +"%Y-%m-%d" 2>/dev/null || date -u -d "$DAYS days ago" +"%Y-%m-%d")
PREVIOUS_SNAPSHOT=$(find "$METRICS_DIR" -name "*.json" -type f -exec grep -l "\"date\": \"$CUTOFF_DATE\"" {} \; | head -1)

if [ -n "$PREVIOUS_SNAPSHOT" ] && [ -f "$PREVIOUS_SNAPSHOT" ]; then
    echo "═══════════════════════════════════════════════════════════════"
    echo "  COMPARISON TO $DAYS DAYS AGO"
    echo "═══════════════════════════════════════════════════════════════"
    echo ""
    
    PREV_QUALITY=$(jq -r '.quality_score.overall // 0' "$PREVIOUS_SNAPSHOT")
    PREV_COVERAGE=$(jq -r '.coverage.percentage // 0' "$PREVIOUS_SNAPSHOT")
    PREV_TEST_PASS=$(jq -r '.tests.pass_rate // 0' "$PREVIOUS_SNAPSHOT")
    PREV_COMPILE_WARN=$(jq -r '.compilation.warnings // 0' "$PREVIOUS_SNAPSHOT")
    PREV_DIALYZER_WARN=$(jq -r '.dialyzer.warnings // 0' "$PREVIOUS_SNAPSHOT")
    
    QUALITY_DELTA=$(awk "BEGIN {printf \"%.1f\", $QUALITY_SCORE - $PREV_QUALITY}")
    COVERAGE_DELTA=$(awk "BEGIN {printf \"%.1f\", $COVERAGE - $PREV_COVERAGE}")
    TEST_DELTA=$(awk "BEGIN {printf \"%.1f\", $TEST_PASS_RATE - $PREV_TEST_PASS}")
    COMPILE_DELTA=$((COMPILE_WARNINGS - PREV_COMPILE_WARN))
    DIALYZER_DELTA=$((DIALYZER_WARNINGS - PREV_DIALYZER_WARN))
    
    show_delta() {
        local name=$1
        local current=$2
        local previous=$3
        local delta=$4
        local reverse=${5:-false}  # true if lower is better (warnings)
        
        local symbol=""
        if (( $(echo "$delta > 0" | bc -l) )); then
            if [ "$reverse" = "true" ]; then
                symbol="❌"
            else
                symbol="✅"
            fi
        elif (( $(echo "$delta < 0" | bc -l) )); then
            if [ "$reverse" = "true" ]; then
                symbol="✅"
            else
                symbol="❌"
            fi
        else
            symbol="➖"
        fi
        
        printf "%-25s %8s → %8s (%+.1f) %s\n" "$name:" "$previous" "$current" "$delta" "$symbol"
    }
    
    show_delta "Quality Score" "$QUALITY_SCORE" "$PREV_QUALITY" "$QUALITY_DELTA"
    show_delta "Coverage" "$COVERAGE%" "$PREV_COVERAGE%" "$COVERAGE_DELTA"
    show_delta "Test Pass Rate" "$TEST_PASS_RATE%" "$PREV_TEST_PASS%" "$TEST_DELTA"
    show_delta "Compile Warnings" "$COMPILE_WARNINGS" "$PREV_COMPILE_WARN" "$COMPILE_DELTA" "true"
    show_delta "Dialyzer Warnings" "$DIALYZER_WARNINGS" "$PREV_DIALYZER_WARN" "$DIALYZER_DELTA" "true"
    echo ""
fi

echo "═══════════════════════════════════════════════════════════════"
echo "  TOP 10 ISSUES TO FIX"
echo "═══════════════════════════════════════════════════════════════"
echo ""

ISSUE_COUNT=0

# Issue 1: Low coverage
if (( $(echo "$COVERAGE < 80" | bc -l) )); then
    ISSUE_COUNT=$((ISSUE_COUNT + 1))
    echo "$ISSUE_COUNT. ❌ Code coverage is $COVERAGE% (target: 80%+)"
    echo "   Action: Add tests for uncovered modules"
    echo "   Priority: HIGH"
    echo ""
fi

# Issue 2: Test failures
if (( $(echo "$TEST_PASS_RATE < 100" | bc -l) )); then
    ISSUE_COUNT=$((ISSUE_COUNT + 1))
    FAILED=$(jq -r '.tests.failed // 0' "$LATEST_SNAPSHOT")
    echo "$ISSUE_COUNT. ❌ $FAILED test(s) failing (pass rate: $TEST_PASS_RATE%)"
    echo "   Action: Fix failing tests"
    echo "   Priority: CRITICAL"
    echo ""
fi

# Issue 3: Compile warnings
if (( COMPILE_WARNINGS > 0 )); then
    ISSUE_COUNT=$((ISSUE_COUNT + 1))
    echo "$ISSUE_COUNT. ⚠️  $COMPILE_WARNINGS compilation warning(s)"
    echo "   Action: Review and fix warnings"
    echo "   Priority: MEDIUM"
    echo ""
fi

# Issue 4: Dialyzer warnings
if (( DIALYZER_WARNINGS > 0 )); then
    ISSUE_COUNT=$((ISSUE_COUNT + 1))
    echo "$ISSUE_COUNT. ⚠️  $DIALYZER_WARNINGS Dialyzer warning(s)"
    echo "   Action: Fix type specifications"
    echo "   Priority: MEDIUM"
    echo ""
fi

# Issue 5: Low test ratio
if (( $(echo "$TEST_RATIO < 0.5" | bc -l) )); then
    ISSUE_COUNT=$((ISSUE_COUNT + 1))
    echo "$ISSUE_COUNT. ⚠️  Test/code ratio is $TEST_RATIO (target: 0.5+)"
    echo "   Action: Write more comprehensive tests"
    echo "   Priority: MEDIUM"
    echo ""
fi

# Issue 6: Large modules
if (( $(echo "$AVG_LOC > 500" | bc -l) )); then
    ISSUE_COUNT=$((ISSUE_COUNT + 1))
    echo "$ISSUE_COUNT. ⚠️  Average module size is $AVG_LOC LOC (target: <500)"
    echo "   Action: Refactor large modules"
    echo "   Priority: LOW"
    echo ""
fi

if (( ISSUE_COUNT == 0 )); then
    echo "✅ No issues found! Quality is excellent."
    echo ""
fi

echo "═══════════════════════════════════════════════════════════════"
echo "  RECOMMENDATIONS"
echo "═══════════════════════════════════════════════════════════════"
echo ""

if (( $(echo "$QUALITY_SCORE >= 80" | bc -l) )); then
    echo "✅ Maintain current quality standards"
    echo "   - Continue TDD practices"
    echo "   - Keep test coverage above 80%"
    echo "   - Address warnings promptly"
    echo ""
fi

if (( $(echo "$COVERAGE < 80" | bc -l) )); then
    echo "📈 Improve Code Coverage:"
    echo "   1. Identify uncovered modules: rebar3 cover"
    echo "   2. Write tests for critical paths first"
    echo "   3. Add property-based tests for complex logic"
    echo "   4. Target: 80%+ coverage"
    echo ""
fi

if (( COMPILE_WARNINGS > 0 )) || (( DIALYZER_WARNINGS > 0 )); then
    echo "🔧 Fix Warnings:"
    echo "   1. Review compiler warnings: rebar3 compile"
    echo "   2. Fix type specs: rebar3 dialyzer"
    echo "   3. Enable stricter linting rules"
    echo "   4. Target: Zero warnings"
    echo ""
fi

if (( $(echo "$TEST_PASS_RATE < 100" | bc -l) )); then
    echo "🚨 Fix Failing Tests:"
    echo "   1. Run: rebar3 eunit --verbose"
    echo "   2. Debug failing test cases"
    echo "   3. Fix root cause (not symptoms)"
    echo "   4. Target: 100% pass rate"
    echo ""
fi

echo "═══════════════════════════════════════════════════════════════"
echo "  NEXT ACTIONS"
echo "═══════════════════════════════════════════════════════════════"
echo ""
echo "1. Review this report and prioritize issues"
echo "2. Create tasks for critical/high priority items"
echo "3. Run quality-snapshot.sh daily to track progress"
echo "4. Run quality-trend.sh weekly to monitor trends"
echo "5. Address regressions immediately"
echo ""
echo "Generate HTML report:"
echo "  $SCRIPT_DIR/quality-trend.sh --html metrics/quality-report.html"
echo ""
echo "✅ Report complete"
