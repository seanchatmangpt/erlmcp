#!/bin/bash
# Run complete benchmark suite
# Executes all test scenarios sequentially and generates comprehensive report

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SWARM_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
SCENARIOS_DIR="$SWARM_DIR/scenarios"
RESULTS_DIR="${RESULTS_DIR:-$SWARM_DIR/test-results}"
PROMETHEUS_URL=${PROMETHEUS_URL:-"http://localhost:9091"}

mkdir -p "$RESULTS_DIR"

echo "=== Docker Swarm Benchmarking Suite ==="
echo "Scenarios Directory: $SCENARIOS_DIR"
echo "Results Directory: $RESULTS_DIR"
echo "Prometheus URL: $PROMETHEUS_URL"
echo "Start Time: $(date -u +%Y-%m-%dT%H:%M:%SZ)"

# Ensure all scenario scripts are executable
chmod +x "$SCENARIOS_DIR"/*.sh 2>/dev/null || true

# Function to run a single benchmark
run_benchmark() {
    local scenario=$1
    local script="$SCENARIOS_DIR/${scenario}.sh"

    if [ ! -f "$script" ]; then
        echo "ERROR: Scenario script not found: $script"
        return 1
    fi

    echo -e "\n=========================================="
    echo "Running: $scenario"
    echo "=========================================="

    bash "$script" || echo "WARNING: Scenario $scenario failed or was interrupted"

    # Wait between tests
    echo "Waiting 30s before next test..."
    sleep 30
}

# Main benchmark suite
echo -e "\n=== PHASE 1: BASELINE PERFORMANCE ==="
run_benchmark "baseline_test"

echo -e "\n=== PHASE 2: STRESS TESTING ==="
run_benchmark "stress_connection_flood"
run_benchmark "stress_message_bombing"

# Additional scenarios if they exist
if [ -f "$SCENARIOS_DIR/stress_slow_client.sh" ]; then
    echo -e "\n=== PHASE 3: ATTACK SIMULATIONS ==="
    run_benchmark "stress_slow_client"
fi

if [ -f "$SCENARIOS_DIR/recovery_test.sh" ]; then
    echo -e "\n=== PHASE 4: RECOVERY TESTING ==="
    run_benchmark "recovery_test"
fi

# Generate comprehensive report
echo -e "\n=========================================="
echo "Generating Comprehensive Report"
echo "=========================================="

python3 << 'PYTHON_EOF'
import json
import os
from pathlib import Path
from datetime import datetime

results_dir = Path(os.environ.get("RESULTS_DIR", "./test-results"))

report = {
    "timestamp": datetime.utcnow().isoformat() + "Z",
    "scenarios": {},
    "summary": {
        "total_scenarios": 0,
        "passed": 0,
        "failed": 0,
        "warnings": 0
    }
}

# Aggregate results from all scenarios
for scenario_dir in sorted(results_dir.glob("*")):
    if scenario_dir.is_dir():
        scenario_name = scenario_dir.name
        scenario_data = {
            "path": str(scenario_dir),
            "files": []
        }

        # List all files in scenario
        for file in sorted(scenario_dir.glob("*")):
            if file.is_file():
                scenario_data["files"].append({
                    "name": file.name,
                    "size": file.stat().st_size,
                    "path": str(file)
                })

        report["scenarios"][scenario_name] = scenario_data
        report["summary"]["total_scenarios"] += 1

# Save JSON report
report_file = results_dir / "benchmark_report.json"
with open(report_file, "w") as f:
    json.dump(report, f, indent=2)

print(f"\n=== BENCHMARK SUMMARY ===")
print(f"Total Scenarios: {report['summary']['total_scenarios']}")
print(f"Results saved to: {report_file}")

# Generate HTML report
html_content = f"""<!DOCTYPE html>
<html>
<head>
    <title>ErlMCP Swarm Benchmarks</title>
    <style>
        body {{ font-family: Arial, sans-serif; margin: 20px; background: #f5f5f5; }}
        .header {{ background: #333; color: white; padding: 20px; border-radius: 5px; }}
        .scenario {{ background: white; padding: 15px; margin: 10px 0; border-left: 4px solid #007bff; }}
        .summary {{ background: white; padding: 15px; margin: 10px 0; border-radius: 5px; }}
        table {{ width: 100%; border-collapse: collapse; margin-top: 10px; }}
        th, td {{ padding: 10px; text-align: left; border-bottom: 1px solid #ddd; }}
        th {{ background: #f8f9fa; }}
        .metric {{ font-family: monospace; }}
        .success {{ color: #28a745; }}
        .warning {{ color: #ffc107; }}
        .error {{ color: #dc3545; }}
    </style>
</head>
<body>
    <div class="header">
        <h1>ErlMCP Swarm Benchmarking Report</h1>
        <p>Generated: {datetime.utcnow().strftime('%Y-%m-%d %H:%M:%S')} UTC</p>
    </div>

    <div class="summary">
        <h2>Summary</h2>
        <table>
            <tr>
                <th>Metric</th>
                <th>Value</th>
            </tr>
            <tr>
                <td>Total Scenarios</td>
                <td class="metric">{report['summary']['total_scenarios']}</td>
            </tr>
            <tr>
                <td>Results Directory</td>
                <td class="metric">{results_dir}</td>
            </tr>
        </table>
    </div>
"""

# Add scenario details
for scenario_name, scenario_data in report['scenarios'].items():
    html_content += f"""
    <div class="scenario">
        <h3>{scenario_name}</h3>
        <p>Files: {len(scenario_data['files'])}</p>
        <ul>
"""
    for file_info in scenario_data['files']:
        html_content += f"        <li>{file_info['name']} ({file_info['size']:,} bytes)</li>\n"
    html_content += """        </ul>
    </div>
"""

html_content += """
    <div class="summary" style="margin-top: 30px;">
        <h2>Next Steps</h2>
        <ol>
            <li>Review metric data in individual scenario directories</li>
            <li>Check Prometheus/Grafana dashboards for real-time visualizations</li>
            <li>Analyze latency percentiles and throughput metrics</li>
            <li>Compare baseline against stress test results</li>
            <li>Document bottlenecks and tuning recommendations</li>
        </ol>
    </div>

    <script>
        // Optionally load additional analytics
        console.log('Benchmark report generated successfully');
    </script>
</body>
</html>
"""

html_file = results_dir / "benchmark_report.html"
with open(html_file, "w") as f:
    f.write(html_content)

print(f"HTML report saved to: {html_file}")

PYTHON_EOF

echo -e "\n=== BENCHMARKING SUITE COMPLETE ==="
echo "Results Directory: $RESULTS_DIR"
echo "End Time: $(date -u +%Y-%m-%dT%H:%M:%SZ)"
echo ""
echo "View Results:"
echo "  - JSON Report:  $RESULTS_DIR/benchmark_report.json"
echo "  - HTML Report:  $RESULTS_DIR/benchmark_report.html"
echo "  - Prometheus:   $PROMETHEUS_URL"
echo "  - Grafana:      http://localhost:3000"
