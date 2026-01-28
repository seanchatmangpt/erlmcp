#!/usr/bin/env bash
# quality-trend.sh - Analyze quality metrics trends over time
# Usage: ./quality-trend.sh [--days N] [--html output.html]

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
METRICS_DIR="$PROJECT_ROOT/metrics/snapshots"
DAYS=30
HTML_OUTPUT=""

# Parse arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        --days)
            DAYS="$2"
            shift 2
            ;;
        --html)
            HTML_OUTPUT="$2"
            shift 2
            ;;
        *)
            echo "Unknown option: $1"
            echo "Usage: $0 [--days N] [--html output.html]"
            exit 1
            ;;
    esac
done

cd "$PROJECT_ROOT"

echo "=== erlmcp Quality Trends Analysis ==="
echo "Period: Last $DAYS days"
echo "Data source: $METRICS_DIR"
echo ""

# Find snapshots in date range
CUTOFF_DATE=$(date -u -v-${DAYS}d +"%Y-%m-%d" 2>/dev/null || date -u -d "$DAYS days ago" +"%Y-%m-%d")
SNAPSHOTS=$(find "$METRICS_DIR" -name "*.json" -type f | sort)

if [ -z "$SNAPSHOTS" ]; then
    echo "❌ No snapshots found. Run quality-snapshot.sh first."
    exit 1
fi

SNAPSHOT_COUNT=$(echo "$SNAPSHOTS" | wc -l | tr -d ' ')
echo "Found $SNAPSHOT_COUNT snapshots"
echo ""

# Extract metrics from all snapshots
TEMP_DATA=$(mktemp)
echo "date,test_pass_rate,coverage,compile_warnings,dialyzer_warnings,quality_score,loc_total" > "$TEMP_DATA"

for snapshot in $SNAPSHOTS; do
    if [ -f "$snapshot" ]; then
        DATE=$(jq -r '.date' "$snapshot")
        TEST_PASS_RATE=$(jq -r '.tests.pass_rate // 0' "$snapshot")
        COVERAGE=$(jq -r '.coverage.percentage // 0' "$snapshot")
        COMPILE_WARNINGS=$(jq -r '.compilation.warnings // 0' "$snapshot")
        DIALYZER_WARNINGS=$(jq -r '.dialyzer.warnings // 0' "$snapshot")
        QUALITY_SCORE=$(jq -r '.quality_score.overall // 0' "$snapshot")
        LOC_TOTAL=$(jq -r '.code_metrics.loc_total // 0' "$snapshot")
        
        echo "$DATE,$TEST_PASS_RATE,$COVERAGE,$COMPILE_WARNINGS,$DIALYZER_WARNINGS,$QUALITY_SCORE,$LOC_TOTAL" >> "$TEMP_DATA"
    fi
done

# Calculate trends
FIRST_SNAPSHOT=$(echo "$SNAPSHOTS" | head -1)
LAST_SNAPSHOT=$(echo "$SNAPSHOTS" | tail -1)

if [ "$FIRST_SNAPSHOT" = "$LAST_SNAPSHOT" ]; then
    echo "⚠️  Only one snapshot available. Need more data for trend analysis."
    exit 0
fi

echo "=== Trend Analysis ==="
echo ""

# Helper function to calculate trend
calculate_trend() {
    local metric=$1
    local first=$(jq -r ".$metric // 0" "$FIRST_SNAPSHOT")
    local last=$(jq -r ".$metric // 0" "$LAST_SNAPSHOT")
    local diff=$(awk "BEGIN {printf \"%.2f\", $last - $first}")
    local pct_change=$(awk "BEGIN {if ($first > 0) printf \"%.1f\", (($last - $first) / $first) * 100; else print \"0\"}")
    
    echo "$first|$last|$diff|$pct_change"
}

# Test Pass Rate
echo "📊 Test Pass Rate:"
TREND=$(calculate_trend "tests.pass_rate")
FIRST=$(echo "$TREND" | cut -d'|' -f1)
LAST=$(echo "$TREND" | cut -d'|' -f2)
DIFF=$(echo "$TREND" | cut -d'|' -f3)
PCT=$(echo "$TREND" | cut -d'|' -f4)
echo "  $FIRST% → $LAST% (${DIFF:0:1}$DIFF%, ${PCT}% change)"
if (( $(echo "$DIFF > 0" | bc -l) )); then
    echo "  ✅ IMPROVING"
elif (( $(echo "$DIFF < -5" | bc -l) )); then
    echo "  ❌ DEGRADING (alert threshold exceeded)"
else
    echo "  ⚠️  STABLE"
fi
echo ""

# Coverage
echo "📊 Code Coverage:"
TREND=$(calculate_trend "coverage.percentage")
FIRST=$(echo "$TREND" | cut -d'|' -f1)
LAST=$(echo "$TREND" | cut -d'|' -f2)
DIFF=$(echo "$TREND" | cut -d'|' -f3)
PCT=$(echo "$TREND" | cut -d'|' -f4)
echo "  $FIRST% → $LAST% (${DIFF:0:1}$DIFF%, ${PCT}% change)"
if (( $(echo "$DIFF > 0" | bc -l) )); then
    echo "  ✅ IMPROVING"
elif (( $(echo "$DIFF < -5" | bc -l) )); then
    echo "  ❌ DEGRADING (alert threshold exceeded)"
else
    echo "  ⚠️  STABLE"
fi
echo ""

# Compile Warnings
echo "📊 Compilation Warnings:"
TREND=$(calculate_trend "compilation.warnings")
FIRST=$(echo "$TREND" | cut -d'|' -f1)
LAST=$(echo "$TREND" | cut -d'|' -f2)
DIFF=$(echo "$TREND" | cut -d'|' -f3)
echo "  $FIRST → $LAST (${DIFF:0:1}$DIFF warnings)"
if (( $(echo "$DIFF < 0" | bc -l) )); then
    echo "  ✅ IMPROVING"
elif (( $(echo "$DIFF > 5" | bc -l) )); then
    echo "  ❌ DEGRADING (alert threshold exceeded)"
else
    echo "  ⚠️  STABLE"
fi
echo ""

# Dialyzer Warnings
echo "📊 Dialyzer Warnings:"
TREND=$(calculate_trend "dialyzer.warnings")
FIRST=$(echo "$TREND" | cut -d'|' -f1)
LAST=$(echo "$TREND" | cut -d'|' -f2)
DIFF=$(echo "$TREND" | cut -d'|' -f3)
echo "  $FIRST → $LAST (${DIFF:0:1}$DIFF warnings)"
if (( $(echo "$DIFF < 0" | bc -l) )); then
    echo "  ✅ IMPROVING"
elif (( $(echo "$DIFF > 5" | bc -l) )); then
    echo "  ❌ DEGRADING (alert threshold exceeded)"
else
    echo "  ⚠️  STABLE"
fi
echo ""

# Overall Quality Score
echo "📊 Overall Quality Score:"
TREND=$(calculate_trend "quality_score.overall")
FIRST=$(echo "$TREND" | cut -d'|' -f1)
LAST=$(echo "$TREND" | cut -d'|' -f2)
DIFF=$(echo "$TREND" | cut -d'|' -f3)
PCT=$(echo "$TREND" | cut -d'|' -f4)
echo "  $FIRST → $LAST (${DIFF:0:1}$DIFF points, ${PCT}% change)"
if (( $(echo "$DIFF > 0" | bc -l) )); then
    echo "  ✅ IMPROVING"
elif (( $(echo "$DIFF < -10" | bc -l) )); then
    echo "  ❌ DEGRADING (alert threshold exceeded)"
else
    echo "  ⚠️  STABLE"
fi
echo ""

# Generate HTML report if requested
if [ -n "$HTML_OUTPUT" ]; then
    echo "Generating HTML report..."
    cat > "$HTML_OUTPUT" << 'HTML'
<!DOCTYPE html>
<html>
<head>
    <title>erlmcp Quality Trends</title>
    <script src="https://cdn.jsdelivr.net/npm/chart.js"></script>
    <style>
        body { font-family: Arial, sans-serif; margin: 20px; background: #f5f5f5; }
        .container { max-width: 1200px; margin: 0 auto; background: white; padding: 20px; border-radius: 8px; }
        h1 { color: #333; border-bottom: 3px solid #4CAF50; padding-bottom: 10px; }
        .chart-container { margin: 30px 0; padding: 20px; background: #fafafa; border-radius: 4px; }
        .metric { display: inline-block; margin: 10px 20px; padding: 15px; background: #e8f5e9; border-radius: 4px; }
        .metric.warning { background: #fff3cd; }
        .metric.danger { background: #f8d7da; }
        .metric-value { font-size: 24px; font-weight: bold; color: #2e7d32; }
        .metric-label { font-size: 14px; color: #666; margin-top: 5px; }
    </style>
</head>
<body>
    <div class="container">
        <h1>📊 erlmcp Quality Trends</h1>
        <p>Generated: <strong id="timestamp"></strong></p>
        
        <div class="chart-container">
            <canvas id="qualityChart"></canvas>
        </div>
        
        <div class="chart-container">
            <canvas id="warningsChart"></canvas>
        </div>
        
        <div class="chart-container">
            <canvas id="coverageChart"></canvas>
        </div>
    </div>
    
    <script>
        document.getElementById('timestamp').textContent = new Date().toISOString();
        
        // Load data from CSV
        const csvData = `DATA_PLACEHOLDER`;
        const lines = csvData.trim().split('\n').slice(1);
        const data = lines.map(line => {
            const [date, testPass, coverage, compileWarn, dialyzerWarn, quality, loc] = line.split(',');
            return {
                date,
                testPass: parseFloat(testPass),
                coverage: parseFloat(coverage),
                compileWarn: parseInt(compileWarn),
                dialyzerWarn: parseInt(dialyzerWarn),
                quality: parseFloat(quality),
                loc: parseInt(loc)
            };
        });
        
        const labels = data.map(d => d.date);
        
        // Quality Score Chart
        new Chart(document.getElementById('qualityChart'), {
            type: 'line',
            data: {
                labels: labels,
                datasets: [{
                    label: 'Quality Score',
                    data: data.map(d => d.quality),
                    borderColor: 'rgb(76, 175, 80)',
                    backgroundColor: 'rgba(76, 175, 80, 0.1)',
                    tension: 0.1
                }]
            },
            options: {
                responsive: true,
                plugins: {
                    title: { display: true, text: 'Overall Quality Score Over Time' }
                },
                scales: {
                    y: { beginAtZero: true, max: 100 }
                }
            }
        });
        
        // Warnings Chart
        new Chart(document.getElementById('warningsChart'), {
            type: 'line',
            data: {
                labels: labels,
                datasets: [
                    {
                        label: 'Compile Warnings',
                        data: data.map(d => d.compileWarn),
                        borderColor: 'rgb(255, 152, 0)',
                        backgroundColor: 'rgba(255, 152, 0, 0.1)'
                    },
                    {
                        label: 'Dialyzer Warnings',
                        data: data.map(d => d.dialyzerWarn),
                        borderColor: 'rgb(244, 67, 54)',
                        backgroundColor: 'rgba(244, 67, 54, 0.1)'
                    }
                ]
            },
            options: {
                responsive: true,
                plugins: {
                    title: { display: true, text: 'Warnings Over Time' }
                },
                scales: {
                    y: { beginAtZero: true }
                }
            }
        });
        
        // Coverage Chart
        new Chart(document.getElementById('coverageChart'), {
            type: 'line',
            data: {
                labels: labels,
                datasets: [
                    {
                        label: 'Test Pass Rate (%)',
                        data: data.map(d => d.testPass),
                        borderColor: 'rgb(33, 150, 243)',
                        backgroundColor: 'rgba(33, 150, 243, 0.1)'
                    },
                    {
                        label: 'Code Coverage (%)',
                        data: data.map(d => d.coverage),
                        borderColor: 'rgb(156, 39, 176)',
                        backgroundColor: 'rgba(156, 39, 176, 0.1)'
                    }
                ]
            },
            options: {
                responsive: true,
                plugins: {
                    title: { display: true, text: 'Test Pass Rate & Coverage Over Time' }
                },
                scales: {
                    y: { beginAtZero: true, max: 100 }
                }
            }
        });
    </script>
</body>
</html>
HTML
    
    # Inject data into HTML
    CSV_CONTENT=$(cat "$TEMP_DATA")
    sed -i.bak "s|DATA_PLACEHOLDER|$CSV_CONTENT|g" "$HTML_OUTPUT"
    rm -f "${HTML_OUTPUT}.bak"
    
    echo "✅ HTML report saved to: $HTML_OUTPUT"
fi

# Cleanup
rm -f "$TEMP_DATA"

echo ""
echo "✅ Trend analysis complete"
