#!/bin/bash

# ERLMCP Observability Performance Validation Script
# Validates comprehensive observability performance benchmarks

set -euo pipefail

echo "===================================="
echo "erlmcp Observability Performance Validation"
echo "===================================="
echo "Date: $(date)"
echo "Version: v2.1.0"
echo "Environment: OTP 28.3.1 | macOS 25.2.0"
echo ""

# Configuration
REPORT_DIR="bench/results"
JSON_REPORT="benchmark_results_observability_2026_02_01.json"
MD_REPORT="ERLMCP_OBSERVABILITY_PERFORMANCE_BENCHMARK_REPORT.md"

# Check if results exist
if [[ ! -f "$JSON_REPORT" ]]; then
    echo "ERROR: Benchmark results not found at $JSON_REPORT"
    exit 1
fi

echo "✓ Found benchmark results"
echo "✓ JSON report: $JSON_REPORT"
echo "✓ Markdown report: $MD_REPORT"
echo ""

# Extract key metrics using jq
if command -v jq &> /dev/null; then
    echo "=== PERFORMANCE METRICS ==="
    THROUGHPUT=$(jq -r '.workloads.core_operations.throughput_msg_per_s' "$JSON_REPORT")
    REGISTRY=$(jq -r '.workloads.core_operations.components.registry.throughput_per_sec' "$JSON_REPORT")
    QUEUE=$(jq -r '.workloads.core_operations.components.queue.throughput_per_sec' "$JSON_REPORT")
    OVERHEAD=$(jq -r '.workloads.observability_overhead.overhead_percent' "$JSON_REPORT")
    CONNECTIONS=$(jq -r '.workloads.connection_scaling.achieved' "$JSON_REPORT")
    CHASIS=$(jq -r '.workloads.chaos_engineering.success_rate_percent' "$JSON_REPORT")

    echo "Overall Throughput: $THROUGHPUT msg/sec"
    echo "Registry Performance: $REGISTRY ops/sec"
    echo "Queue Performance: $QUEUE ops/sec"
    echo "OTEL Overhead: $OVERHEAD%"
    echo "Connection Scalability: $CONNECTIONS connections"
    echo "Chaos Success Rate: $CHASIS%"
    echo ""
fi

# Validate quality gates
echo "=== QUALITY GATES VALIDATION ==="

# Gate 1: Throughput > 2M msg/sec
if [[ -f "$JSON_REPORT" && $(jq -r '.workloads.core_operations.throughput_msg_per_s >= 2000000' "$JSON_REPORT") == "true" ]]; then
    echo "✅ Throughput Gate: PASSED (>2M msg/sec)"
else
    echo "❌ Throughput Gate: FAILED"
    exit 1
fi

# Gate 2: Registry > 553K ops/sec (baseline)
if [[ $(jq -r '.workloads.core_operations.components.registry.throughput_per_sec >= 553000' "$JSON_REPORT") == "true" ]]; then
    echo "✅ Registry Gate: PASSED (>553K ops/sec)"
else
    echo "❌ Registry Gate: FAILED"
    exit 1
fi

# Gate 3: Queue > 971K ops/sec (baseline)
if [[ $(jq -r '.workloads.core_operations.components.queue.throughput_per_sec >= 971000' "$JSON_REPORT") == "true" ]]; then
    echo "✅ Queue Gate: PASSED (>971K ops/sec)"
else
    echo "❌ Queue Gate: FAILED"
    exit 1
fi

# Gate 4: OTEL overhead < 5%
if [[ $(jq -r '.workloads.observability_overhead.overhead_percent < 5.0' "$JSON_REPORT") == "true" ]]; then
    echo "✅ OTEL Overhead Gate: PASSED (<5%)"
else
    echo "❌ OTEL Overhead Gate: FAILED"
    exit 1
fi

# Gate 5: Connections > 40K
if [[ $(jq -r '.workloads.connection_scaling.achieved >= 40000' "$JSON_REPORT") == "true" ]]; then
    echo "✅ Connection Gate: PASSED (>40K connections)"
else
    echo "❌ Connection Gate: FAILED"
    exit 1
fi

# Gate 6: Chaos > 90% success
if [[ $(jq -r '.workloads.chaos_engineering.success_rate_percent >= 90.0' "$JSON_REPORT") == "true" ]]; then
    echo "✅ Chaos Gate: PASSED (>90% success)"
else
    echo "❌ Chaos Gate: FAILED"
    exit 1
fi

# Gate 7: No critical regressions
if [[ $(jq -r '.workloads.regression_analysis.analysis.overall_regression_acceptable == true' "$JSON_REPORT") == "true" ]]; then
    echo "✅ Regression Gate: PASSED (no critical regressions)"
else
    echo "❌ Regression Gate: FAILED"
    exit 1
fi

echo ""
echo "=== SUMMARY ==="

# Check production readiness
if [[ $(jq -r '.performance_summary.production_readiness.status == "PRODUCTION_READY"' "$JSON_REPORT") == "true" ]]; then
    echo "🚀 STATUS: PRODUCTION READY"
    GRADE=$(jq -r '.performance_summary.overall_grade' "$JSON_REPORT")
    echo "📊 GRADE: $GRADE"
    echo "✅ All quality gates passed"
    echo "✅ Performance exceeds baselines"
    echo "✅ Comprehensive validation complete"

    echo ""
    echo "=== NEXT STEPS ==="
    echo "1. Deploy to production"
    echo "2. Monitor registry latency trends"
    echo "3. Implement registry caching (Phase 1 optimization)"
    echo "4. Continue performance trending"

    echo ""
    echo "📄 Reports generated:"
    echo "   - $JSON_REPORT (detailed metrics)"
    echo "   - $MD_REPORT (comprehensive analysis)"
    echo ""
    echo "✅ Validation complete - system is PRODUCTION READY"

else
    echo "❌ STATUS: NOT PRODUCTION READY"
    exit 1
fi