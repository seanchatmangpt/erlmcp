#!/bin/bash

# ErlMCP v3 Compliance Checker
# Comprehensive compliance validation for enterprise deployments

set -euo pipefail

# Configuration
COMPLIANCE_DIR="${COMPLIANCE_DIR:-$(dirname "$0")}"
ENVIRONMENT="${ENVIRONMENT:-staging}"
LOG_FILE="${COMPLIANCE_DIR}/compliance-check-$(date +%Y%m%d-%H%M%S).log"
REPORT_DIR="${COMPLIANCE_DIR}/reports"
STANDARDS_DIR="${COMPLIANCE_DIR}/standards"

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

# Compliance standards to check
STANDARDS=(
    "ISO27001"
    "SOC2"
    "PCI-DSS"
    "GDPR"
    "HIPAA"
    "NIST-CSF"
)

# Initialize logging
log() {
    echo "[$(date '+%Y-%m-%d %H:%M:%S')] $1" | tee -a "$LOG_FILE"
}

error() {
    echo -e "${RED}[ERROR] $1${NC}" | tee -a "$LOG_FILE"
    exit 1
}

success() {
    echo -e "${GREEN}[SUCCESS] $1${NC}" | tee -a "$LOG_FILE"
}

warning() {
    echo -e "${YELLOW}[WARNING] $1${NC}" | tee -a "$LOG_FILE"
}

info() {
    echo -e "${BLUE}[INFO] $1${NC}" | tee -a "$LOG_FILE"
}

# Create necessary directories
mkdir -p "$REPORT_DIR" "$STANDARDS_DIR"

# Main compliance check function
main() {
    log "Starting compliance check for environment: $ENVIRONMENT"

    # Initialize compliance report
    init_compliance_report

    # Check each standard
    for standard in "${STANDARDS[@]}"; do
        log "Checking compliance with $standard..."
        check_standard "$standard"
    done

    # Generate summary report
    generate_summary_report

    # Check overall compliance
    overall_compliance=$(calculate_overall_compliance)

    if [ "$overall_compliance" -ge 90 ]; then
        success "Overall compliance score: $overall_compliance%"
        exit 0
    else
        error "Compliance score below threshold: $overall_compliance%"
        exit 1
    fi
}

# Initialize compliance report
init_compliance_report() {
    cat > "$REPORT_DIR/compliance-report.json" << EOF
{
    "environment": "$ENVIRONMENT",
    "timestamp": "$(date -Iseconds)",
    "standards": {},
    "overall_score": 0,
    "status": "pending"
}
EOF
}

# Check specific compliance standard
check_standard() {
    local standard=$1
    local standard_dir="$STANDARDS_DIR/$standard"
    local score=0
    local total_checks=0
    local passed_checks=0

    # Load standard configuration
    if [ -f "$standard_dir/config.json" ]; then
        source "$standard_dir/config.json"
    else
        warning "No configuration found for $standard standard"
        return
    fi

    # Run control checks
    while IFS= read -r control; do
        # Skip empty lines and comments
        [[ -z "$control" || "$control" == \#* ]] && continue

        ((total_checks++))

        # Execute control check
        if check_control "$standard" "$control"; then
            ((passed_checks++))
            score=$((score + control_weight))
            log "✓ PASSED: $control"
        else
            log "✗ FAILED: $control"
        fi
    done < "$standard_dir/controls.txt"

    # Calculate score for this standard
    if [ $total_checks -gt 0 ]; then
        local standard_score=$((passed_checks * 100 / total_checks))
        update_standard_report "$standard" "$standard_score" "$passed_checks" "$total_checks"

        # Log results
        log "Standard $standard: $standard_score% ($passed_checks/$total_checks controls passed)"
    else
        warning "No controls found for $standard"
    fi
}

# Check individual control
check_control() {
    local standard=$1
    local control=$2
    local result=true

    # Get control details
    local control_name=$(echo "$control" | cut -d'|' -f1)
    local control_type=$(echo "$control" | cut -d'|' -f2)
    local control_script=$(echo "$control" | cut -d'|' -f3)
    local control_weight=$(echo "$control" | cut -d'|' -f4)

    # Execute control check based on type
    case "$control_type" in
        "configuration")
            # Check configuration files
            if [ -n "$control_script" ] && [ -x "$STANDARDS_DIR/$standard/scripts/$control_script" ]; then
                "$STANDARDS_DIR/$standard/scripts/$control_script"
                result=$?
            else
                error "Control script not found: $control_script"
                result=1
            fi
            ;;
        "infrastructure")
            # Check infrastructure configuration
            if [ -n "$control_script" ] && [ -x "$STANDARDS_DIR/$standard/scripts/$control_script" ]; then
                "$STANDARDS_DIR/$standard/scripts/$control_script"
                result=$?
            else
                error "Infrastructure control script not found: $control_script"
                result=1
            fi
            ;;
        "application")
            # Check application code
            if [ -n "$control_script" ] && [ -x "$STANDARDS_DIR/$standard/scripts/$control_script" ]; then
                "$STANDARDS_DIR/$standard/scripts/$control_script"
                result=$?
            else
                error "Application control script not found: $control_script"
                result=1
            fi
            ;;
        "network")
            # Check network configuration
            if [ -n "$control_script" ] && [ -x "$STANDARDS_DIR/$standard/scripts/$control_script" ]; then
                "$STANDARDS_DIR/$standard/scripts/$control_script"
                result=$?
            else
                error "Network control script not found: $control_script"
                result=1
            fi
            ;;
        "audit")
            # Check audit trails
            if [ -n "$control_script" ] && [ -x "$STANDARDS_DIR/$standard/scripts/$control_script" ]; then
                "$STANDARDS_DIR/$standard/scripts/$control_script"
                result=$?
            else
                error "Audit control script not found: $control_script"
                result=1
            fi
            ;;
        *)
            warning "Unknown control type: $control_type"
            result=1
            ;;
    esac

    return $result
}

# Update standard report
update_standard_report() {
    local standard=$1
    local score=$2
    local passed=$3
    local total=$4

    # Read existing report
    if [ -f "$REPORT_DIR/compliance-report.json" ]; then
        jq ".standards.$standard = {
            \"score\": $score,
            \"passed_checks\": $passed,
            \"total_checks\": $total,
            \"status\": \"$([ $score -ge 90 ] && echo 'pass' || echo 'fail')\"
        }" "$REPORT_DIR/compliance-report.json" > "$REPORT_DIR/compliance-report.tmp"
        mv "$REPORT_DIR/compliance-report.tmp" "$REPORT_DIR/compliance-report.json"
    fi
}

# Calculate overall compliance score
calculate_overall_compliance() {
    local total_score=0
    local standard_count=0

    # Calculate weighted average
    while IFS= read -r standard; do
        local score=$(jq ".standards.\"$standard\".score" "$REPORT_DIR/compliance-report.json" 2>/dev/null || echo "0")
        local weight=$(jq ".standards.\"$standard\".weight" "$STANDARDS_DIR/$standard/config.json" 2>/dev/null || echo "1")

        total_score=$((total_score + score * weight))
        standard_count=$((standard_count + weight))
    done <<< "$(jq -r '.standards | keys | .[]' "$REPORT_DIR/compliance-report.json" 2>/dev/null || echo "")"

    if [ $standard_count -gt 0 ]; then
        echo $((total_score / standard_count))
    else
        echo 0
    fi
}

# Generate summary report
generate_summary_report() {
    local overall_score=$(calculate_overall_compliance)

    # Generate HTML report
    cat > "$REPORT_DIR/compliance-summary.html" << EOF
<!DOCTYPE html>
<html>
<head>
    <title>ErlMCP v3 Compliance Report - $ENVIRONMENT</title>
    <style>
        body { font-family: Arial, sans-serif; margin: 40px; }
        .header { background-color: #f0f0f0; padding: 20px; border-radius: 5px; }
        .standard { margin: 20px 0; padding: 15px; border: 1px solid #ddd; border-radius: 5px; }
        .passed { background-color: #d4edda; }
        .failed { background-color: #f8d7da; }
        .warning { background-color: #fff3cd; }
        .score { font-weight: bold; font-size: 24px; }
        .bar { width: 100%; background-color: #e0e0e0; }
        .bar-fill { height: 20px; background-color: #4CAF50; }
        .bar-fill.fail { background-color: #f44336; }
        .bar-fill.warning { background-color: #ff9800; }
    </style>
</head>
<body>
    <div class="header">
        <h1>ErlMCP v3 Compliance Report</h1>
        <p><strong>Environment:</strong> $ENVIRONMENT</p>
        <p><strong>Generated:</strong> $(date -Iseconds)</p>
        <p><strong>Overall Score:</strong> <span class="score">$overall_score%</span></p>
        <div class="bar">
            <div class="bar-fill$([ $overall_score -lt 90 ] && echo ' fail' || [ $overall_score -lt 70 ] && echo ' warning')" style="width: $overall_score%"></div>
        </div>
    </div>

    <h2>Standard Details</h2>
EOF

    # Add each standard to the report
    while IFS= read -r standard; do
        local score=$(jq ".standards.\"$standard\".score" "$REPORT_DIR/compliance-report.json")
        local passed=$(jq ".standards.\"$standard\".passed_checks" "$REPORT_DIR/compliance-report.json")
        local total=$(jq ".standards.\"$standard\".total_checks" "$REPORT_DIR/compliance-report.json")

        cat >> "$REPORT_DIR/compliance-summary.html" << EOF
    <div class="standard $([ $score -ge 90 ] && echo 'passed' || [ $score -ge 70 ] && echo 'warning' || echo 'failed')">
        <h3>$standard</h3>
        <p>Score: $score% ($passed/$total controls passed)</p>
        <div class="bar">
            <div class="bar-fill$([ $score -lt 90 ] && echo ' fail' || [ $score -lt 70 ] && echo ' warning')" style="width: $score%"></div>
        </div>
    </div>
EOF
    done <<< "$(jq -r '.standards | keys | .[]' "$REPORT_DIR/compliance-report.json")"

    cat >> "$REPORT_DIR/compliance-summary.html" << EOF
</body>
</html>
EOF

    # Generate JSON report with details
    jq '{
        environment: .environment,
        timestamp: .timestamp,
        overall_score: calculate_overall_compliance,
        status: (.overall_score >= 90 | if . then "pass" else "fail" end),
        standards: .standards,
        recommendations: generate_recommendations
    }' "$REPORT_DIR/compliance-report.json" > "$REPORT_DIR/compliance-final.json"

    success "Compliance report generated: $REPORT_DIR/compliance-summary.html"
}

# Execute main function
main "$@"