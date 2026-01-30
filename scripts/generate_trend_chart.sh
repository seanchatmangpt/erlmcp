#!/usr/bin/env bash
###====================================================================
### generate_trend_chart.sh - Generate Performance Trend Charts
###====================================================================
### Analyzes historical baseline data and generates trend visualizations.
###
### Usage:
###   ./scripts/generate_trend_chart.sh [--output DIR] [--type TYPE]
###
### Options:
###   --output DIR    Output directory (default: bench/trends/)
###   --type TYPE     Chart type: throughput, latency, memory, all (default: all)
###   --format FMT    Output format: html, png, svg (default: html)
###
### Requirements:
###   - jq (JSON parsing)
###   - gnuplot (chart generation)
###
### Output:
###   - HTML charts in bench/trends/
###   - PNG charts if --format png
###====================================================================

set -euo pipefail

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Defaults
OUTPUT_DIR="bench/trends"
CHART_TYPE="all"
FORMAT="html"

# Parse arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        --output)
            OUTPUT_DIR="$2"
            shift 2
            ;;
        --type)
            CHART_TYPE="$2"
            shift 2
            ;;
        --format)
            FORMAT="$2"
            shift 2
            ;;
        -h|--help)
            head -n 45 "$0" | grep "^###" | sed 's/^### //'
            exit 0
            ;;
        *)
            echo -e "${RED}Unknown option: $1${NC}"
            exit 1
            ;;
    esac
done

# Project root
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
cd "$PROJECT_ROOT"

# Ensure output directory
mkdir -p "$OUTPUT_DIR"

echo "========================================"
echo "Performance Trend Chart Generator"
echo "========================================"
echo "Output: $OUTPUT_DIR"
echo "Type: $CHART_TYPE"
echo "Format: $FORMAT"
echo ""

# Check dependencies
if ! command -v jq &> /dev/null; then
    echo -e "${RED}ERROR: jq not found${NC}"
    echo "Install: apt-get install jq / brew install jq"
    exit 1
fi

if ! command -v gnuplot &> /dev/null; then
    echo -e "${YELLOW}WARNING: gnuplot not found${NC}"
    echo "Install: apt-get install gnuplot / brew install gnuplot"
    echo "Falling back to HTML-only charts"
    FORMAT="html"
fi

# Collect baseline data
BASELINE_DIR="bench/baselines"
if [ ! -d "$BASELINE_DIR" ]; then
    echo -e "${RED}ERROR: No baseline directory found${NC}"
    exit 1
fi

# Find all baseline JSON files
BASELINE_FILES=$(ls -t "$BASELINE_DIR"/*.json 2>/dev/null | grep -v "BASELINE_EXAMPLE" || true)

if [ -z "$BASELINE_FILES" ]; then
    echo -e "${YELLOW}WARNING: No baseline files found${NC}"
    exit 0
fi

BASELINE_COUNT=$(echo "$BASELINE_FILES" | wc -l | tr -d ' ')
echo "Found $BASELINE_COUNT baseline files"
echo ""

# Extract data for each metric
extract_metric() {
    local metric=$1
    local benchmark=$2

    echo "# $metric for $benchmark"
    for file in $BASELINE_FILES; do
        local filename=$(basename "$file")
        local date=$(echo "$filename" | sed 's/_v.*//')

        local value=$(jq -r ".benchmarks.$benchmark.$metric // 0" "$file" 2>/dev/null || echo "0")

        if [ "$value" != "0" ] && [ "$value" != "null" ]; then
            echo "$date $value"
        fi
    done
}

# Generate HTML chart
generate_html_chart() {
    local output_file="$1"
    local title="$2"
    local data_file="$3"
    local yaxis_label="$4"

    cat > "$output_file" << EOF
<!DOCTYPE html>
<html>
<head>
    <meta charset="UTF-8">
    <title>$title</title>
    <script src="https://cdn.jsdelivr.net/npm/chart.js@4.4.0/dist/chart.umd.min.js"></script>
    <style>
        body { font-family: Arial, sans-serif; margin: 20px; background: #f5f5f5; }
        .container { max-width: 1200px; margin: 0 auto; background: white; padding: 20px; box-shadow: 0 2px 4px rgba(0,0,0,0.1); }
        h1 { color: #333; }
        .chart-container { position: relative; height: 400px; margin: 20px 0; }
    </style>
</head>
<body>
    <div class="container">
        <h1>$title</h1>
        <div class="chart-container">
            <canvas id="chart"></canvas>
        </div>
    </div>
    <script>
        const ctx = document.getElementById('chart').getContext('2d');
        const data = $(cat "$data_file" | jq -R -s -c 'split("\n") | map(select(length > 0) | split(" ")) | {labels: map(.[0]), values: map(.[1] | tonumber)}');
        new Chart(ctx, {
            type: 'line',
            data: {
                labels: data.labels,
                datasets: [{
                    label: '$yaxis_label',
                    data: data.values,
                    borderColor: 'rgb(75, 192, 192)',
                    backgroundColor: 'rgba(75, 192, 192, 0.2)',
                    tension: 0.1
                }]
            },
            options: {
                responsive: true,
                maintainAspectRatio: false,
                plugins: {
                    title: {
                        display: true,
                        text: '$title'
                    }
                },
                scales: {
                    y: {
                        beginAtZero: false,
                        title: {
                            display: true,
                            text: '$yaxis_label'
                        }
                    }
                }
            }
        });
    </script>
</body>
</html>
EOF
}

# Generate charts for each metric
if [ "$CHART_TYPE" = "all" ] || [ "$CHART_TYPE" = "throughput" ]; then
    echo -e "${YELLOW}Generating throughput charts...${NC}"

    for bench in core_ops_100k tcp_quick_1k stress_30s_100k_ops; do
        DATA_FILE="$OUTPUT_DIR/${bench}_throughput.dat"
        HTML_FILE="$OUTPUT_DIR/${bench}_throughput.html"

        extract_metric "throughput_msg_per_s" "$bench" > "$DATA_FILE"

        if [ -s "$DATA_FILE" ]; then
            generate_html_chart "$HTML_FILE" "$bench - Throughput Trend" "$DATA_FILE" "Throughput (msg/s)"
            echo -e "${GREEN}✓ ${bench}_throughput.html${NC}"
        fi
    done
fi

if [ "$CHART_TYPE" = "all" ] || [ "$CHART_TYPE" = "latency" ]; then
    echo -e "${YELLOW}Generating latency charts...${NC}"

    for bench in core_ops_100k tcp_quick_1k stress_30s_100k_ops; do
        for percentile in p50 p95 p99; do
            DATA_FILE="$OUTPUT_DIR/${bench}_latency_${percentile}.dat"
            HTML_FILE="$OUTPUT_DIR/${bench}_latency_${percentile}.html"

            extract_metric "latency_${percentile}_us" "$bench" > "$DATA_FILE"

            if [ -s "$DATA_FILE" ]; then
                generate_html_chart "$HTML_FILE" "$bench - ${percentile^^} Latency Trend" "$DATA_FILE" "Latency (µs)"
                echo -e "${GREEN}✓ ${bench}_latency_${percentile}.html${NC}"
            fi
        done
    done
fi

if [ "$CHART_TYPE" = "all" ] || [ "$CHART_TYPE" = "memory" ]; then
    echo -e "${YELLOW}Generating memory charts...${NC}"

    for bench in core_ops_100k tcp_quick_1k stress_30s_100k_ops; do
        DATA_FILE="$OUTPUT_DIR/${bench}_memory.dat"
        HTML_FILE="$OUTPUT_DIR/${bench}_memory.html"

        extract_metric "memory_delta_mib" "$bench" > "$DATA_FILE"

        if [ -s "$DATA_FILE" ]; then
            generate_html_chart "$HTML_FILE" "$bench - Memory Trend" "$DATA_FILE" "Memory (MiB)"
            echo -e "${GREEN}✓ ${bench}_memory.html${NC}"
        fi
    done
fi

# Generate summary index
cat > "$OUTPUT_DIR/index.html" << EOF
<!DOCTYPE html>
<html>
<head>
    <meta charset="UTF-8">
    <title>erlmcp Performance Trends</title>
    <style>
        body { font-family: Arial, sans-serif; margin: 20px; background: #f5f5f5; }
        .container { max-width: 1200px; margin: 0 auto; background: white; padding: 20px; box-shadow: 0 2px 4px rgba(0,0,0,0.1); }
        h1 { color: #333; }
        .charts-grid { display: grid; grid-template-columns: repeat(auto-fit, minmax(300px, 1fr)); gap: 20px; margin: 20px 0; }
        .chart-card { border: 1px solid #ddd; padding: 15px; border-radius: 8px; }
        .chart-card h3 { margin-top: 0; }
        .chart-card a { color: #4CAF50; text-decoration: none; }
        .chart-card a:hover { text-decoration: underline; }
    </style>
</head>
<body>
    <div class="container">
        <h1>📈 erlmcp Performance Trends</h1>
        <p>Historical performance data analysis across all benchmarks.</p>

        <h2>Throughput Charts</h2>
        <div class="charts-grid">
EOF

for bench in core_ops_100k tcp_quick_1k stress_30s_100k_ops; do
    if [ -f "$OUTPUT_DIR/${bench}_throughput.html" ]; then
        cat >> "$OUTPUT_DIR/index.html" << EOF
            <div class="chart-card">
                <h3>$bench</h3>
                <a href="${bench}_throughput.html">View Throughput Trend →</a>
            </div>
EOF
    fi
done

cat >> "$OUTPUT_DIR/index.html" << EOF
        </div>

        <h2>Latency Charts</h2>
        <div class="charts-grid">
EOF

for bench in core_ops_100k tcp_quick_1k stress_30s_100k_ops; do
    for percentile in p50 p95 p99; do
        if [ -f "$OUTPUT_DIR/${bench}_latency_${percentile}.html" ]; then
            cat >> "$OUTPUT_DIR/index.html" << EOF
            <div class="chart-card">
                <h3>$bench - ${percentile^^}</h3>
                <a href="${bench}_latency_${percentile}.html">View Latency Trend →</a>
            </div>
EOF
        fi
    done
done

cat >> "$OUTPUT_DIR/index.html" << EOF
        </div>

        <h2>Memory Charts</h2>
        <div class="charts-grid">
EOF

for bench in core_ops_100k tcp_quick_1k stress_30s_100k_ops; do
    if [ -f "$OUTPUT_DIR/${bench}_memory.html" ]; then
        cat >> "$OUTPUT_DIR/index.html" << EOF
            <div class="chart-card">
                <h3>$bench</h3>
                <a href="${bench}_memory.html">View Memory Trend →</a>
            </div>
EOF
    fi
done

cat >> "$OUTPUT_DIR/index.html" << EOF
        </div>
    </div>
</body>
</html>
EOF

echo ""
echo -e "${GREEN}✓ Chart generation complete${NC}"
echo "Index: $OUTPUT_DIR/index.html"
echo ""
echo "To view charts:"
echo "  1. Open in browser: file://$PROJECT_ROOT/$OUTPUT_DIR/index.html"
echo "  2. Or serve: python3 -m http.server 8000 --directory $OUTPUT_DIR"
