#!/usr/bin/env bash
# generate-html-report.sh - Generates HTML regression report
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "${SCRIPT_DIR}/../.." && pwd)"
METRICS_DIR="${PROJECT_ROOT}/.metrics"
REGRESSION_REPORT="${METRICS_DIR}/regression-report.json"
HTML_OUTPUT="${METRICS_DIR}/regression-report.html"

if [[ ! -f "${REGRESSION_REPORT}" ]]; then
    echo "Error: Regression report not found: ${REGRESSION_REPORT}"
    exit 1
fi

# Read JSON data
REPORT_DATA=$(cat "${REGRESSION_REPORT}")
SEVERITY=$(echo "$REPORT_DATA" | jq -r '.severity')
TIMESTAMP=$(echo "$REPORT_DATA" | jq -r '.timestamp')
BASELINE_COMMIT=$(echo "$REPORT_DATA" | jq -r '.baseline_commit')
CURRENT_COMMIT=$(echo "$REPORT_DATA" | jq -r '.current_commit')

# Generate HTML
cat > "${HTML_OUTPUT}" << 'HTML_START'
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>erlmcp Regression Report</title>
    <style>
        * {
            margin: 0;
            padding: 0;
            box-sizing: border-box;
        }
        
        body {
            font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, 'Helvetica Neue', Arial, sans-serif;
            background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
            min-height: 100vh;
            padding: 20px;
        }
        
        .container {
            max-width: 1200px;
            margin: 0 auto;
            background: white;
            border-radius: 12px;
            box-shadow: 0 20px 60px rgba(0, 0, 0, 0.3);
            overflow: hidden;
        }
        
        .header {
            background: linear-gradient(135deg, #1e3c72 0%, #2a5298 100%);
            color: white;
            padding: 40px;
            text-align: center;
        }
        
        .header h1 {
            font-size: 2.5em;
            margin-bottom: 10px;
        }
        
        .header p {
            font-size: 1.1em;
            opacity: 0.9;
        }
        
        .severity-badge {
            display: inline-block;
            padding: 8px 20px;
            border-radius: 20px;
            font-weight: bold;
            margin-top: 15px;
            font-size: 1.1em;
        }
        
        .severity-none {
            background: #10b981;
            color: white;
        }
        
        .severity-low {
            background: #3b82f6;
            color: white;
        }
        
        .severity-medium {
            background: #f59e0b;
            color: white;
        }
        
        .severity-high {
            background: #ef4444;
            color: white;
        }
        
        .severity-critical {
            background: #7f1d1d;
            color: white;
        }
        
        .info-section {
            padding: 30px 40px;
            background: #f8fafc;
            border-bottom: 1px solid #e2e8f0;
        }
        
        .info-grid {
            display: grid;
            grid-template-columns: repeat(auto-fit, minmax(250px, 1fr));
            gap: 20px;
        }
        
        .info-item {
            background: white;
            padding: 20px;
            border-radius: 8px;
            border-left: 4px solid #667eea;
        }
        
        .info-label {
            font-size: 0.85em;
            color: #64748b;
            font-weight: 600;
            text-transform: uppercase;
            margin-bottom: 5px;
        }
        
        .info-value {
            font-size: 1.1em;
            color: #1e293b;
            font-family: 'Courier New', monospace;
        }
        
        .regressions-section {
            padding: 40px;
        }
        
        .section-title {
            font-size: 1.8em;
            color: #1e293b;
            margin-bottom: 25px;
            padding-bottom: 15px;
            border-bottom: 2px solid #e2e8f0;
        }
        
        .regression-card {
            background: #f8fafc;
            border-radius: 8px;
            padding: 25px;
            margin-bottom: 20px;
            border-left: 5px solid;
            transition: transform 0.2s, box-shadow 0.2s;
        }
        
        .regression-card:hover {
            transform: translateY(-2px);
            box-shadow: 0 4px 12px rgba(0, 0, 0, 0.1);
        }
        
        .regression-card.critical {
            border-color: #7f1d1d;
            background: #fef2f2;
        }
        
        .regression-card.high {
            border-color: #ef4444;
            background: #fef2f2;
        }
        
        .regression-card.medium {
            border-color: #f59e0b;
            background: #fffbeb;
        }
        
        .regression-card.low {
            border-color: #3b82f6;
            background: #eff6ff;
        }
        
        .regression-header {
            display: flex;
            justify-content: space-between;
            align-items: center;
            margin-bottom: 15px;
        }
        
        .regression-metric {
            font-size: 1.3em;
            font-weight: 600;
            color: #1e293b;
        }
        
        .regression-severity {
            padding: 5px 15px;
            border-radius: 12px;
            font-size: 0.85em;
            font-weight: 700;
            text-transform: uppercase;
        }
        
        .regression-message {
            font-size: 1.1em;
            color: #475569;
            margin-bottom: 15px;
        }
        
        .regression-details {
            display: grid;
            grid-template-columns: repeat(3, 1fr);
            gap: 15px;
            margin-top: 15px;
        }
        
        .detail-box {
            background: white;
            padding: 12px;
            border-radius: 6px;
            text-align: center;
        }
        
        .detail-label {
            font-size: 0.8em;
            color: #64748b;
            margin-bottom: 5px;
        }
        
        .detail-value {
            font-size: 1.2em;
            font-weight: 600;
            color: #1e293b;
            font-family: 'Courier New', monospace;
        }
        
        .no-regressions {
            text-align: center;
            padding: 60px;
            color: #10b981;
        }
        
        .no-regressions svg {
            width: 100px;
            height: 100px;
            margin-bottom: 20px;
        }
        
        .no-regressions h2 {
            font-size: 2em;
            margin-bottom: 10px;
        }
        
        .recommendations {
            background: #f0f9ff;
            border: 1px solid #bae6fd;
            border-radius: 8px;
            padding: 20px;
            margin-top: 20px;
        }
        
        .recommendations h3 {
            color: #0c4a6e;
            margin-bottom: 10px;
        }
        
        .recommendations ul {
            list-style: none;
            padding-left: 0;
        }
        
        .recommendations li {
            padding: 8px 0;
            color: #0369a1;
        }
        
        .recommendations li:before {
            content: "→ ";
            font-weight: bold;
            margin-right: 5px;
        }
        
        .footer {
            background: #1e293b;
            color: white;
            text-align: center;
            padding: 20px;
            font-size: 0.9em;
        }
        
        .footer a {
            color: #60a5fa;
            text-decoration: none;
        }
    </style>
</head>
<body>
    <div class="container">
        <div class="header">
            <h1>🛡️ erlmcp Regression Report</h1>
            <p>Quality Gate Analysis</p>
HTML_START

# Add severity badge
cat >> "${HTML_OUTPUT}" << HTML_SEVERITY
            <div class="severity-badge severity-${SEVERITY}">${SEVERITY^^}</div>
        </div>
        
        <div class="info-section">
            <div class="info-grid">
                <div class="info-item">
                    <div class="info-label">Timestamp</div>
                    <div class="info-value">${TIMESTAMP}</div>
                </div>
                <div class="info-item">
                    <div class="info-label">Baseline Commit</div>
                    <div class="info-value">${BASELINE_COMMIT:0:12}</div>
                </div>
                <div class="info-item">
                    <div class="info-label">Current Commit</div>
                    <div class="info-value">${CURRENT_COMMIT:0:12}</div>
                </div>
            </div>
        </div>
        
        <div class="regressions-section">
            <h2 class="section-title">Detected Regressions</h2>
HTML_SEVERITY

# Add regressions or success message
REGRESSION_COUNT=$(echo "$REPORT_DATA" | jq -r '.regressions | length')

if [[ $REGRESSION_COUNT -eq 0 ]]; then
    cat >> "${HTML_OUTPUT}" << 'HTML_SUCCESS'
            <div class="no-regressions">
                <svg fill="none" stroke="currentColor" viewBox="0 0 24 24">
                    <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" 
                          d="M9 12l2 2 4-4m6 2a9 9 0 11-18 0 9 9 0 0118 0z"/>
                </svg>
                <h2>No Regressions Detected!</h2>
                <p>All quality metrics are within acceptable ranges.</p>
            </div>
HTML_SUCCESS
else
    # Generate regression cards
    for ((i=0; i<REGRESSION_COUNT; i++)); do
        METRIC=$(echo "$REPORT_DATA" | jq -r ".regressions[$i].metric")
        REG_SEVERITY=$(echo "$REPORT_DATA" | jq -r ".regressions[$i].severity")
        MESSAGE=$(echo "$REPORT_DATA" | jq -r ".regressions[$i].message")
        BASELINE=$(echo "$REPORT_DATA" | jq -r ".regressions[$i].baseline")
        CURRENT=$(echo "$REPORT_DATA" | jq -r ".regressions[$i].current")
        CHANGE=$(echo "$REPORT_DATA" | jq -r ".regressions[$i].change")
        
        cat >> "${HTML_OUTPUT}" << HTML_REGRESSION
            <div class="regression-card ${REG_SEVERITY}">
                <div class="regression-header">
                    <div class="regression-metric">${METRIC}</div>
                    <div class="regression-severity severity-${REG_SEVERITY}">${REG_SEVERITY}</div>
                </div>
                <div class="regression-message">${MESSAGE}</div>
                <div class="regression-details">
                    <div class="detail-box">
                        <div class="detail-label">Baseline</div>
                        <div class="detail-value">${BASELINE}</div>
                    </div>
                    <div class="detail-box">
                        <div class="detail-label">Current</div>
                        <div class="detail-value">${CURRENT}</div>
                    </div>
                    <div class="detail-box">
                        <div class="detail-label">Change</div>
                        <div class="detail-value">${CHANGE}</div>
                    </div>
                </div>
HTML_REGRESSION
        
        # Add recommendations based on metric
        cat >> "${HTML_OUTPUT}" << HTML_REC_START
                <div class="recommendations">
                    <h3>💡 Recommendations</h3>
                    <ul>
HTML_REC_START
        
        case "$METRIC" in
            test_pass_rate)
                cat >> "${HTML_OUTPUT}" << 'HTML_TEST_REC'
                        <li>Review failing tests and fix underlying issues</li>
                        <li>Check for race conditions or flaky tests</li>
                        <li>Ensure test data is properly initialized</li>
                        <li>Run tests locally to reproduce failures</li>
HTML_TEST_REC
                ;;
            coverage)
                cat >> "${HTML_OUTPUT}" << 'HTML_COV_REC'
                        <li>Add tests for new code paths</li>
                        <li>Identify untested edge cases</li>
                        <li>Review code for defensive programming needs</li>
                        <li>Consider property-based testing for complex logic</li>
HTML_COV_REC
                ;;
            warnings)
                cat >> "${HTML_OUTPUT}" << 'HTML_WARN_REC'
                        <li>Fix compilation warnings immediately</li>
                        <li>Enable stricter compiler flags</li>
                        <li>Review deprecated API usage</li>
                        <li>Check type specifications</li>
HTML_WARN_REC
                ;;
            performance)
                cat >> "${HTML_OUTPUT}" << 'HTML_PERF_REC'
                        <li>Profile code to identify bottlenecks</li>
                        <li>Check for unnecessary allocations</li>
                        <li>Review algorithm complexity</li>
                        <li>Consider caching frequently accessed data</li>
HTML_PERF_REC
                ;;
            complexity)
                cat >> "${HTML_OUTPUT}" << 'HTML_COMP_REC'
                        <li>Refactor large functions into smaller ones</li>
                        <li>Extract common logic into utilities</li>
                        <li>Review code for single responsibility principle</li>
                        <li>Consider using OTP behaviors appropriately</li>
HTML_COMP_REC
                ;;
        esac
        
        cat >> "${HTML_OUTPUT}" << 'HTML_REC_END'
                    </ul>
                </div>
            </div>
HTML_REC_END
    done
fi

# Close HTML
cat >> "${HTML_OUTPUT}" << 'HTML_END'
        </div>
        
        <div class="footer">
            <p>Generated by <strong>erlmcp Regression Guard</strong> 🛡️</p>
            <p><a href="https://github.com/yourusername/erlmcp">erlmcp</a> - Erlang/OTP MCP SDK</p>
        </div>
    </div>
</body>
</html>
HTML_END

echo "HTML report generated: ${HTML_OUTPUT}"
