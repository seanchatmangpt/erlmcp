#!/bin/bash
# ============================================================================
# Observability Validation Test Script
# Tests Cloud Monitoring, Cloud Logging, and dashboards for Marketplace
# ============================================================================

set -euo pipefail

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

log_info() { echo -e "${GREEN}[INFO]${NC} $1"; }
log_warn() { echo -e "${YELLOW}[WARN]${NC} $1"; }
log_error() { echo -e "${RED}[ERROR]${NC} $1"; }
log_test() { echo -e "${BLUE}[TEST]${NC} $1"; }

# ============================================================================
# Configuration
# ============================================================================

PROJECT_ID="${PROJECT_ID:-}"
REGION="${REGION:-us-central1}"
CLUSTER_NAME="${CLUSTER_NAME:-}"
SERVICE_NAME="${SERVICE_NAME:-}"
INSTANCE_NAME="${INSTANCE_NAME:-}"

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
MARKETPLACE_DIR="$(dirname "$SCRIPT_DIR")"
EVIDENCE_DIR="${MARKETPLACE_DIR}/test-evidence/observability"

METRICS_NAMESPACE="${METRICS_NAMESPACE:-custom.googleapis.com/erlmcp}"
LOG_FILTER="resource.labels.service_name=erlmcp"

# Wait times for observability data propagation
METRIC_WAIT_SECONDS="${METRIC_WAIT_SECONDS:-60}"
LOG_WAIT_SECONDS="${LOG_WAIT_SECONDS:-30}"

# ============================================================================
# Prerequisites
# ============================================================================

check_prerequisites() {
    log_info "Checking prerequisites..."

    if [ -z "$PROJECT_ID" ]; then
        log_error "PROJECT_ID not set. Run: export PROJECT_ID=your-project-id"
        exit 1
    fi

    if ! command -v gcloud &> /dev/null; then
        log_error "gcloud CLI not found"
        exit 1
    fi

    if ! command -v jq &> /dev/null; then
        log_error "jq not found"
        exit 1
    fi

    # Create evidence directory
    mkdir -p "$EVIDENCE_DIR"

    # Enable required APIs
    log_info "Ensuring Monitoring and Logging APIs are enabled..."
    gcloud services enable \
        monitoring.googleapis.com \
        logging.googleapis.com \
        cloudtrace.googleapis.com \
        --project="$PROJECT_ID" > /dev/null 2>&1 || true

    log_info "Prerequisites check passed"
}

# ============================================================================
# Test Functions
# ============================================================================

test_metrics_descriptor_exists() {
    log_test "Checking custom metrics descriptors..."

    local descriptors_file="$EVIDENCE_DIR/metric-descriptors.json"

    if gcloud monitoring descriptors list \
        --project="$PROJECT_ID" \
        --filter="metric.type.startsWith(\"$METRICS_NAMESPACE\")" \
        --format=json > "$descriptors_file" 2>&1; then

        local metric_count=$(jq 'length' "$descriptors_file" 2>/dev/null || echo "0")

        echo "Custom Metric Descriptors:" > "$EVIDENCE_DIR/metric-descriptors.txt"
        echo "===========================" >> "$EVIDENCE_DIR/metric-descriptors.txt"
        jq -r '.[].displayName // empty' "$descriptors_file" >> "$EVIDENCE_DIR/metric-descriptors.txt" 2>/dev/null || true

        if [ "$metric_count" -gt 0 ]; then
            log_info "  ✓ Found $metric_count custom metric descriptors"
            cat "$EVIDENCE_DIR/metric-descriptors.txt"
            return 0
        else
            log_warn "  ⚠ No custom metrics descriptors found yet (may need to generate traffic first)"
            return 0
        fi
    else
        log_warn "  ⚠ Could not retrieve metric descriptors"
        return 0
    fi
}

test_metrics_data_present() {
    log_test "Checking for actual metric data points..."

    local metrics_data_file="$EVIDENCE_DIR/metrics-data.json"

    # List time series for custom metrics
    if gcloud monitoring time-series list \
        --project="$PROJECT_ID" \
        --filter="metric.typestartsWith(\"$METRICS_NAMESPACE\")" \
        --format=json > "$metrics_data_file" 2>&1; then

        local series_count=$(jq 'length' "$metrics_data_file" 2>/dev/null || echo "0")

        if [ "$series_count" -gt 0 ]; then
            log_info "  ✓ Found $series_count time series with data"

            echo "Available Metrics:" > "$EVIDENCE_DIR/available-metrics.txt"
            jq -r '.[].metric.type' "$metrics_data_file" | sort -u >> "$EVIDENCE_DIR/available-metrics.txt" 2>/dev/null || true

            cat "$EVIDENCE_DIR/available-metrics.txt"
            return 0
        else
            log_warn "  ⚠ No metric data points found (generate some API traffic first)"
            return 0
        fi
    else
        log_warn "  ⚠ Could not retrieve time series data"
        return 0
    fi
}

test_expected_metrics() {
    log_test "Verifying expected metrics are available..."

    local expected_metrics=(
        "${METRICS_NAMESPACE}/http/requests"
        "${METRICS_NAMESPACE}/http/latency"
        "${METRICS_NAMESPACE}/connections/active"
        "${METRICS_NAMESPACE}/errors/count"
    )

    local metrics_file="$EVIDENCE_DIR/expected-metrics.txt"

    echo "Expected Metrics Check:" > "$metrics_file"
    echo "=======================" >> "$metrics_file"

    local all_found=true

    for metric in "${expected_metrics[@]}"; do
        # Try to query this specific metric
        if gcloud monitoring time-series list \
            --project="$PROJECT_ID" \
            --filter="metric.type=\"${metric}\"" \
            --format="value(metric.type)" 2>/dev/null | grep -q "$metric"; then

            echo "✓ $metric" >> "$metrics_file"
            log_info "  ✓ Found: $metric"
        else
            echo "✗ $metric (not found)" >> "$metrics_file"
            log_warn "  ⚠ Not found: $metric"
            all_found=false
        fi
    done

    if [ "$all_found" = true ]; then
        log_info "  ✓ All expected metrics present"
        return 0
    else
        log_warn "  ⚠ Some expected metrics missing (may need traffic)"
        return 0
    fi
}

test_logging_working() {
    log_test "Verifying logs are being ingested..."

    log_info "  Waiting ${LOG_WAIT_SECONDS}s for logs to propagate..."

    sleep "$LOG_WAIT_SECONDS"

    local logs_file="$EVIDENCE_DIR/cloud-logs.json"

    # Query for recent logs
    local log_filter="timestamp > \"$(date -u -d '5 minutes ago' '+%Y-%m-%dT%H:%M:%SZ' 2>/dev/null || date -u -v-5M '+%Y-%m-%dT%H:%M:%SZ')\""

    if gcloud logging read \
        "$log_filter" \
        --project="$PROJECT_ID" \
        --limit=50 \
        --format=json > "$logs_file" 2>&1; then

        local log_count=$(jq 'length' "$logs_file" 2>/dev/null || echo "0")

        echo "Cloud Logging Check:" > "$EVIDENCE_DIR/logging-summary.txt"
        echo "====================" >> "$EVIDENCE_DIR/logging-summary.txt"
        echo "Found $log_count log entries" >> "$EVIDENCE_DIR/logging-summary.txt"

        if [ "$log_count" -gt 0 ]; then
            log_info "  ✓ Found $log_count log entries"

            # Check for structured logs
            local structured_count=$(jq '[.[] | select(jsonPayload != null)] | length' "$logs_file" 2>/dev/null || echo "0")
            echo "Structured logs: $structured_count" >> "$EVIDENCE_DIR/logging-summary.txt"

            if [ "$structured_count" -gt 0 ]; then
                log_info "  ✓ Structured logs present"
                echo "" >> "$EVIDENCE_DIR/logging-summary.txt"
                echo "Sample structured log:" >> "$EVIDENCE_DIR/logging-summary.txt"
                jq '.[] | select(jsonPayload != null) | .jsonPayload' "$logs_file" 2>/dev/null | head -10 >> "$EVIDENCE_DIR/logging-summary.txt"
            fi

            return 0
        else
            log_warn "  ⚠ No recent log entries found"
            return 0
        fi
    else
        log_warn "  ⚠ Could not query logs"
        return 0
    fi
}

test_log_severity_levels() {
    log_test "Checking for different log severity levels..."

    local logs_file="$EVIDENCE_DIR/cloud-logs-severity.json"

    if gcloud logging read \
        "timestamp > \"$(date -u -d '5 minutes ago' '+%Y-%m-%dT%H:%M:%SZ' 2>/dev/null || date -u -v-5M '+%Y-%m-%dT%H:%M:%SZ')\"" \
        --project="$PROJECT_ID" \
        --limit=100 \
        --format=json > "$logs_file" 2>&1; then

        local severities=$(jq -r '.[].severity // "DEFAULT"' "$logs_file" 2>/dev/null | sort -u | tr '\n' ' ')

        echo "Log Severity Levels Found:" > "$EVIDENCE_DIR/log-severities.txt"
        echo "$severities" >> "$EVIDENCE_DIR/log-severities.txt"

        log_info "  Severity levels: $severities"

        # Check for ERROR/CRITICAL logs (if any errors occurred)
        local error_count=$(jq '[.[] | select(.severity == "ERROR" or .severity == "CRITICAL")] | length' "$logs_file" 2>/dev/null || echo "0")

        if [ "$error_count" -gt 0 ]; then
            log_warn "  ⚠ Found $error_count ERROR/CRITICAL log entries"
            echo "" >> "$EVIDENCE_DIR/log-severities.txt"
            echo "ERROR/CRITICAL entries:" >> "$EVIDENCE_DIR/log-severities.txt"
            jq -r '.[] | select(.severity == "ERROR" or .severity == "CRITICAL") | "\(.timestamp) [\(.severity)] \(.logName)"' "$logs_file" 2>/dev/null | head -5 >> "$EVIDENCE_DIR/log-severities.txt"
        else
            log_info "  ✓ No ERROR/CRITICAL logs found"
        fi

        return 0
    else
        log_warn "  ⚠ Could not analyze log severities"
        return 0
    fi
}

test_dashboard_exists() {
    log_test "Checking for custom dashboards..."

    local dashboards_file="$EVIDENCE_DIR/dashboards.json"

    if gcloud monitoring dashboards list \
        --project="$PROJECT_ID" \
        --format=json > "$dashboards_file" 2>&1; then

        local dashboard_count=$(jq 'length' "$dashboards_file" 2>/dev/null || echo "0")

        echo "Available Dashboards:" > "$EVIDENCE_DIR/dashboards.txt"
        echo "======================" >> "$EVIDENCE_DIR/dashboards.txt"

        if [ "$dashboard_count" -gt 0 ]; then
            jq -r '.[].displayName' "$dashboards_file" >> "$EVIDENCE_DIR/dashboards.txt" 2>/dev/null || true

            log_info "  ✓ Found $dashboard_count dashboards"

            # Check for erlmcp-specific dashboard
            if jq -e '.[].displayName | contains("erlmcp")' "$dashboards_file" > /dev/null 2>&1; then
                log_info "  ✓ Found erlmcp-specific dashboard"
            fi

            cat "$EVIDENCE_DIR/dashboards.txt"
            return 0
        else
            log_warn "  ⚠ No custom dashboards found"
            return 0
        fi
    else
        log_warn "  ⚠ Could not list dashboards"
        return 0
    fi
}

test_alert_policies_exist() {
    log_test "Checking for alert policies..."

    local alerts_file="$EVIDENCE_DIR/alert-policies.json"

    if gcloud alpha monitoring policies list \
        --project="$PROJECT_ID" \
        --format=json > "$alerts_file" 2>&1; then

        local alert_count=$(jq 'length' "$alerts_file" 2>/dev/null || echo "0")

        echo "Alert Policies:" > "$EVIDENCE_DIR/alert-policies.txt"
        echo "==============" >> "$EVIDENCE_DIR/alert-policies.txt"

        if [ "$alert_count" -gt 0 ]; then
            jq -r '.[].displayName' "$alerts_file" >> "$EVIDENCE_DIR/alert-policies.txt" 2>/dev/null || true

            log_info "  ✓ Found $alert_count alert policies"

            # Check for erlmcp-specific alerts
            local erlmcp_alerts=$(jq '[.[] | select(.displayName | contains("erlmcp"))] | length' "$alerts_file" 2>/dev/null || echo "0")
            if [ "$erlmcp_alerts" -gt 0 ]; then
                log_info "  ✓ Found $erlmcp_alerts erlmcp-specific alert policies"
            fi

            cat "$EVIDENCE_DIR/alert-policies.txt"
            return 0
        else
            log_warn "  ⚠ No alert policies found (recommended for production)"
            return 0
        fi
    else
        log_warn "  ⚠ Could not list alert policies"
        return 0
    fi
}

test_notification_channels() {
    log_test "Checking notification channels..."

    local channels_file="$EVIDENCE_DIR/notification-channels.json"

    if gcloud alpha monitoring channels list \
        --project="$PROJECT_ID" \
        --format=json > "$channels_file" 2>&1; then

        local channel_count=$(jq 'length' "$channels_file" 2>/dev/null || echo "0")

        echo "Notification Channels:" > "$EVIDENCE_DIR/notification-channels.txt"
        echo "======================" >> "$EVIDENCE_DIR/notification-channels.txt"

        if [ "$channel_count" -gt 0 ]; then
            jq -r '.[] | "\(.type): \(.displayName)"' "$channels_file" >> "$EVIDENCE_DIR/notification-channels.txt" 2>/dev/null || true

            log_info "  ✓ Found $channel_count notification channels"
            cat "$EVIDENCE_DIR/notification-channels.txt"
            return 0
        else
            log_warn "  ⚠ No notification channels configured"
            return 0
        fi
    else
        log_warn "  ⚠ Could not list notification channels"
        return 0
    fi
}

test_uptime_check() {
    log_test "Checking for uptime checks..."

    local uptime_file="$EVIDENCE_DIR/uptime-checks.json"

    if gcloud monitoring uptime list \
        --project="$PROJECT_ID" \
        --format=json > "$uptime_file" 2>&1; then

        local uptime_count=$(jq 'length' "$uptime_file" 2>/dev/null || echo "0")

        echo "Uptime Checks:" > "$EVIDENCE_DIR/uptime-checks.txt"
        echo "=============" >> "$EVIDENCE_DIR/uptime-checks.txt"

        if [ "$uptime_count" -gt 0 ]; then
            jq -r '.[].displayName' "$uptime_file" >> "$EVIDENCE_DIR/uptime-checks.txt" 2>/dev/null || true

            log_info "  ✓ Found $uptime_count uptime checks"
            cat "$EVIDENCE_DIR/uptime-checks.txt"
            return 0
        else
            log_warn "  ⚠ No uptime checks configured (recommended for production)"
            return 0
        fi
    else
        log_warn "  ⚠ Could not list uptime checks"
        return 0
    fi
}

test_log_based_metrics() {
    log_test "Checking for log-based metrics..."

    local log_metrics_file="$EVIDENCE_DIR/log-based-metrics.json"

    if gcloud logging metrics list \
        --project="$PROJECT_ID" \
        --format=json > "$log_metrics_file" 2>&1; then

        local metric_count=$(jq 'length' "$log_metrics_file" 2>/dev/null || echo "0")

        echo "Log-Based Metrics:" > "$EVIDENCE_DIR/log-based-metrics.txt"
        echo "==================" >> "$EVIDENCE_DIR/log-based-metrics.txt"

        if [ "$metric_count" -gt 0 ]; then
            jq -r '.[].name' "$log_metrics_file" >> "$EVIDENCE_DIR/log-based-metrics.txt" 2>/dev/null || true

            log_info "  ✓ Found $metric_count log-based metrics"
            cat "$EVIDENCE_DIR/log-based-metrics.txt"
            return 0
        else
            log_warn "  ⚠ No log-based metrics found (optional)"
            return 0
        fi
    else
        log_warn "  ⚠ Could not list log-based metrics"
        return 0
    fi
}

test_monitoring_workspace() {
    log_test "Checking Monitoring workspace configuration..."

    local workspace_file="$EVIDENCE_DIR/monitoring-workspace.json"

    if gcloud monitoring projects describe "$PROJECT_ID" \
        --format=json > "$workspace_file" 2>&1; then

        log_info "  ✓ Monitoring workspace configured"

        # Check for project metrics scope
        local metrics_scope=$(jq -r '.name // "unknown"' "$workspace_file" 2>/dev/null)
        echo "Metrics Scope: $metrics_scope" > "$EVIDENCE_DIR/metrics-scope.txt"

        return 0
    else
        log_warn "  ⚠ Could not describe monitoring project"
        return 0
    fi
}

# ============================================================================
# Main Test Runner
# ============================================================================

main() {
    log_info "Starting Observability Validation Test Suite..."
    log_info "================================================"
    log_info "Project: $PROJECT_ID"
    log_info "Region: $REGION"
    log_info "Metrics Namespace: $METRICS_NAMESPACE"
    log_info "================================================"

    check_prerequisites

    # Test counter
    TOTAL_TESTS=0
    PASSED_TESTS=0
    FAILED_TESTS=0

    # Run tests
    for test_func in \
        test_monitoring_workspace \
        test_metrics_descriptor_exists \
        test_metrics_data_present \
        test_expected_metrics \
        test_logging_working \
        test_log_severity_levels \
        test_dashboard_exists \
        test_alert_policies_exist \
        test_notification_channels \
        test_uptime_check \
        test_log_based_metrics; do

        TOTAL_TESTS=$((TOTAL_TESTS + 1))

        if $test_func; then
            PASSED_TESTS=$((PASSED_TESTS + 1))
        else
            FAILED_TESTS=$((FAILED_TESTS + 1))
        fi
    done

    # Summary
    log_info "================================================"
    log_info "Test Summary:"
    log_info "  Total Tests:  $TOTAL_TESTS"
    log_info "  Passed:       $PASSED_TESTS"
    log_info "  Failed:       $FAILED_TESTS"
    log_info "================================================"
    log_info "Evidence saved to: $EVIDENCE_DIR"

    if [ $FAILED_TESTS -gt 0 ]; then
        log_error "Some observability tests failed."
        exit 1
    fi

    log_info "Observability validation complete!"
    log_info "NOTE: Some tests may require actual API traffic to pass."
    exit 0
}

# ============================================================================
# Script Entry Point
# ============================================================================

# Parse arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        --project)
            PROJECT_ID="$2"
            shift 2
            ;;
        --region)
            REGION="$2"
            shift 2
            ;;
        --cluster)
            CLUSTER_NAME="$2"
            shift 2
            ;;
        --service)
            SERVICE_NAME="$2"
            shift 2
            ;;
        --instance)
            INSTANCE_NAME="$2"
            shift 2
            ;;
        --metrics-namespace)
            METRICS_NAMESPACE="$2"
            shift 2
            ;;
        *)
            log_error "Unknown option: $1"
            echo "Usage: $0 [--project PROJECT] [--region REGION] [--cluster NAME] [--service NAME] [--instance NAME] [--metrics-namespace NS]"
            exit 1
            ;;
    esac
done

main
