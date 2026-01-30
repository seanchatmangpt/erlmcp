#!/bin/bash

# ===================================================================
# ERLMCP TEST QUALITY REPORT GENERATOR
# Generates comprehensive test quality reports with metrics and recommendations
# ===================================================================

set -e

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
PURPLE='\033[0;35m'
CYAN='\033[0;36m'
NC='\033[0m'

# Output file
REPORT_FILE="test-quality-report-$(date +%Y%m%d-%H%M%S).md"

echo -e "${CYAN}📊 ERLMCP TEST QUALITY REPORT GENERATOR${NC}"
echo "============================================="
echo ""

# Function to generate report header
generate_header() {
    cat << EOF
# ERLMCP Test Quality Report

**Generated:** $(date)
**Version:** $(git describe --tags --always 2>/dev/null || echo "unknown")
**Branch:** $(git branch --show-current 2>/dev/null || echo "unknown")

---

## Executive Summary

This report provides a comprehensive analysis of test quality across the erlmcp project,
focusing on identifying opportunities for improvement through poka-yoke (mistake-proofing)
principles.

EOF
}

# Function to generate metrics overview
generate_metrics() {
    cat << EOF

## Test Metrics Overview

| Metric | Value | Status |
|--------|-------|--------|
EOF

    # Calculate metrics
    TOTAL_TESTS=$(find apps -name "*.erl" -path "*/test/*" | wc -l)
    SKIPPED_TESTS=$(find apps -name "*.skip" | wc -l)
    BROKEN_TESTS=$(find apps -name "*.broken" | wc -l)
    ACTIVE_TESTS=$((TOTAL_TESTS - SKIPPED_TESTS - BROKEN_TESTS))
    TIMEOUT_FILES=$(find apps -name "*.erl" -exec grep -l "timer:sleep" {} \; | wc -l)
    CONCURRENT_FILES=$(find apps -name "*.erl" -exec grep -l "spawn\\|concurrent" {} \; | wc -l)
    PROPERTY_FILES=$(find apps -name "*.erl" -exec grep -l "proper\\|prop_" {} \; | wc -l)
    COVERAGE_SCORE=$(calculate_coverage_score)

    # Display metrics with status
    echo "| Total Test Files | $TOTAL_TESTS | ✅ |"
    echo "| Active Tests | $ACTIVE_TESTS | $(test $ACTIVE_TESTS -gt 0 && echo "✅" || echo "⚠️") |"
    echo "| Skipped Tests | $SKIPPED_TESTS | $(test $SKIPPED_TESTS -eq 0 && echo "✅" || echo "❌") |"
    echo "| Broken Tests | $BROKEN_TESTS | $(test $BROKEN_TESTS -eq 0 && echo "✅" || echo "❌") |"
    echo "| Files with Timeouts | $TIMEOUT_FILES | $(test $TIMEOUT_FILES -eq 0 && echo "✅" || echo "⚠️") |"
    echo "| Concurrent Test Files | $CONCURRENT_FILES | $(test $CONCURRENT_FILES -gt 0 && echo "✅" || echo "⚠️") |"
    echo "| Property Test Files | $PROPERTY_FILES | $(test $PROPERTY_FILES -gt 0 && echo "✅" || echo "⚠️") |"
    echo "| Coverage Score | $COVERAGE_SCORE% | $(test $COVERAGE_SCORE -ge 80 && echo "✅" || echo "❌") |"

    cat << EOF

### Health Score Calculation

**Overall Health Score: $(( (ACTIVE_TESTS * 40 + CONCURRENT_FILES * 20 + PROPERTY_FILES * 20 + (100 - COVERAGE_SCORE) * 20) / 100 ))%**

- **Active Tests Contribution:** $(( ACTIVE_TESTS * 40 / TOTAL_TESTS ))%
- **Concurrent Tests Contribution:** $(( CONCURRENT_FILES * 20 / TOTAL_TESTS ))%
- **Property Tests Contribution:** $(( PROPERTY_FILES * 20 / TOTAL_TESTS ))%
- **Code Quality Contribution:** $(( (100 - COVERAGE_SCORE) * 20 / 100 ))%

EOF
}

# Function to generate detailed analysis
generate_detailed_analysis() {
    cat << EOF

## Detailed Analysis

### 1. Test Distribution by Application

EOF

    # Test distribution
    for app in apps/*/test; do
        if [ -d "$app" ]; then
            app_name=$(basename $(dirname "$app"))
            test_count=$(find "$app" -name "*.erl" | wc -l)
            echo "**$app_name**: $test_count test files"
        fi
    done

    cat << EOF

### 2. Quality Issues Identified

EOF

    # Skipped tests analysis
    if [ $(find apps -name "*.skip" | wc -l) -gt 0 ]; then
        cat << EOF
#### Skipped Tests ($(find apps -name "*.skip" | wc -l) files)

EOF
        find apps -name "*.skip" | while read file; do
            echo "- **$(basename $file)**: $(extract_skip_reason "$file")"
        done
    fi

    # Hardcoded timeouts analysis
    if [ $(find apps -name "*.erl" -exec grep -l "timer:sleep" {} \; | wc -l) -gt 0 ]; then
        cat << EOF

#### Hardcoded Timeouts ($(find apps -name "*.erl" -exec grep -l "timer:sleep" {} \; | wc -l) files)

EOF
        find apps -name "*.erl" -exec grep -l "timer:sleep" {} \; | head -5 | while read file; do
            timeout_count=$(grep -c "timer:sleep" "$file")
            echo "- **$(basename $file)**: $timeout_count hardcoded timeouts"
        done
    fi

    # Mock usage analysis
    if [ $(find apps -name "*.erl" -exec grep -l "meck\|mock" {} \; | wc -l) -gt 0 ]; then
        cat << EOF

#### Mock Usage ($(find apps -name "*.erl" -exec grep -l "meck\|mock" {} \; | wc -l) files)

EOF
        find apps -name "*.erl" -exec grep -l "meck\|mock" {} \; | head -5 | while read file; do
            echo "- **$(basename $file)**: Uses mock dependencies"
        done
    fi
}

# Function to generate poka-yoke opportunities
generate_poka_yoke_opportunities() {
    cat << EOF

## Poka-Yoke Testing Opportunities

### 1. Coverage Enhancement Opportunities

EOF

    # Find modules with insufficient coverage
    find apps -name "*.erl" -path "*/src/*" | while read src_file; do
        module_name=$(basename "$src_file" .erl)
        test_file=$(echo "$src_file" | sed 's|/src/|/test/|' | sed 's|\.erl|_tests.erl|')

        if [ ! -f "$test_file" ]; then
            echo "- **$module_name**: No test file found"
        fi
    done

    cat << EOF

### 2. Reliability Improvement Opportunities

EOF

    # Identify timing-dependent tests
    if [ $(find apps -name "*.erl" -exec grep -l "timer:sleep" {} \; | wc -l) -gt 0 ]; then
        cat << EOF
- **Adaptive Timing System**: Replace hardcoded timeouts with adaptive timing
- **Deterministic Test Execution**: Add test isolation to prevent timing issues
EOF
    fi

    # Identify missing concurrent tests
    if [ $(find apps -name "*.erl" -exec grep -l "spawn\\|concurrent" {} \; | wc -l) -lt $((TOTAL_TESTS / 4)) ]; then
        cat << EOF
- **Concurrent Testing**: Add concurrent test patterns to $((TOTAL_TESTS / 4)) modules
EOF
    fi

    cat << EOF

### 3. Maintenance Improvement Opportunities

EOF

    if [ $(find apps -name "*.skip" | wc -l) -gt 0 ]; then
        cat << EOF
- **Auto-Restoration System**: Monitor dependencies and auto-enable skipped tests
EOF
    fi

    if [ $(find apps -name "*.broken" | wc -l) -gt 0 ]; then
        cat << EOF
- **Test Evolution Tracker**: Monitor code changes and update broken tests
EOF
    fi
}

# Function to generate recommendations
generate_recommendations() {
    cat << EOF

## Recommendations

### Short-term Actions (Week 1-2)

1. **Address Skipped Tests**
   - $(test $(find apps -name "*.skip" | wc -l) -gt 0 && echo "Fix $(find apps -name "*.skip" | wc -l) skipped tests" || echo "✅ All skipped tests already addressed")

2. **Replace Hardcoded Timeouts**
   - $(test $(find apps -name "*.erl" -exec grep -l "timer:sleep" {} \; | wc -l) -gt 0 && echo "Replace hardcoded timeouts in $(find apps -name "*.erl" -exec grep -l "timer:sleep" {} \; | wc -l) files" || echo "✅ No hardcoded timeouts found")

3. **Add Missing Concurrent Tests**
   - $(test $(find apps -name "*.erl" -exec grep -l "spawn\\|concurrent" {} \; | wc -l) -eq 0 && echo "Add concurrent test patterns to critical modules" || echo "✅ Concurrent tests already present")

### Medium-term Actions (Week 3-4)

1. **Implement Property Testing**
   - Add property-based testing to $(test $(find apps -name "*.erl" -exec grep -l "proper\\|prop_" {} \; | wc -l) -eq 0 && echo "all critical modules" || echo "modules without property tests")

2. **Enhance Test Coverage**
   - Achieve 80%+ code coverage across all modules
   - Focus on error paths and edge cases

3. **Implement Quality Gates**
   - Set up pre-commit hooks for test quality
   - Integrate with CI/CD pipeline

### Long-term Actions (Month 2+)

1. **Build Testing Framework**
   - Create comprehensive test template generator
   - Implement adaptive test execution

2. **Continuous Improvement**
   - Monitor test metrics and identify trends
   - Regular refactoring of test infrastructure

EOF
}

# Function to generate implementation plan
generate_implementation_plan() {
    cat << EOF

## Implementation Plan

### Phase 1: Detection (Week 1)
- [ ] Deploy poka-yoke test checker
- [ ] Set up automated metrics collection
- [ ] Create baseline quality report

### Phase 2: Prevention (Week 2)
- [ ] Implement adaptive timeout system
- [ ] Add test isolation framework
- [ ] Create property test generator

### Phase 3: Integration (Week 3)
- [ ] Integrate with CI/CD pipeline
- [ ] Set up pre-commit quality gates
- [ ] Create documentation and training

### Phase 4: Continuous Improvement (Week 4+)
- [ ] Monitor test quality metrics
- [ ] Regular refactoring and optimization
- [ ] Evolve testing framework based on feedback

EOF
}

# Function to generate appendix
generate_appendix() {
    cat << EOF

## Appendix

### Poka-Yoke Testing Principles

1. **Prevention**: Design tests to prevent mistakes
2. **Detection**: Automatically detect quality issues
3. **Correction**: Provide immediate feedback on issues
4. **Improvement**: Continuously refine testing processes

### Quality Gate Definitions

- **Coverage Gate**: Minimum 80% code coverage
- **Flakiness Gate**: Maximum 0 flaky tests
- **Skipped Gate**: Maximum 0 skipped tests
- **Timeout Gate**: Maximum 0 hardcoded timeouts
- **Isolation Gate**: Maximum 0 shared state issues

### Tooling

- **Test Enforcer**: `tools/poka-yoke-test-enforcer.sh`
- **Quality Metrics**: `tools/test-quality-metrics.sh`
- **Template Generator**: `tools/generate-poka-yoke-test-template.erl`
- **Test Checker**: `tools/poka-yoke-test-checker.erl`

EOF
}

# Helper functions
calculate_coverage_score() {
    if [ -f "_build/test/cover/index.html" ]; then
        COVERAGE=$(grep -o '[0-9]*%' _build/test/cover/index.html | head -1 | sed 's/%//')
        if [ -n "$COVERAGE" ]; then
            echo $COVERAGE
            return
        fi
    fi
    echo "0"
}

extract_skip_reason() {
    file="$1"
    if [ -f "$file" ]; then
        reason=$(grep -o "%%.*TODO\|%%.*FIXME\|%%.*XXX\|%%.*SKIPPED" "$file" | head -1 | sed 's/%%//' | tr -d ' ')
        if [ -n "$reason" ]; then
            echo "$reason"
        else
            echo "Unknown reason"
        fi
    else
        echo "File not found"
    fi
}

# Main script
{
    generate_header
    generate_metrics
    generate_detailed_analysis
    generate_poka_yoke_opportunities
    generate_recommendations
    generate_implementation_plan
    generate_appendix
} > "$REPORT_FILE"

echo -e "${GREEN}✅ Test quality report generated: $REPORT_FILE${NC}"
echo ""

# Display summary
echo -e "${BLUE}📈 Report Summary:${NC}"
echo "- Total test files analyzed: $(find apps -name "*.erl" -path "*/test/*" | wc -l)"
echo "- Quality issues found: $(( $(find apps -name "*.skip" | wc -l) + $(find apps -name "*.broken" | wc -l) + $(find apps -name "*.erl" -exec grep -l "timer:sleep" {} \; | wc -l) ))"
echo "- Health score: $(( ($(find apps -name "*.erl" -path "*/test/*" | wc -l) - $(find apps -name "*.skip" | wc -l) - $(find apps -name "*.broken" | wc -l)) * 40 / $(find apps -name "*.erl" -path "*/test/*" | wc -l) + $(find apps -name "*.erl" -exec grep -l "spawn\\|concurrent" {} \; | wc -l) * 20 / $(find apps -name "*.erl" -path "*/test/*" | wc -l) + $(find apps -name "*.erl" -exec grep -l "proper\\|prop_" {} \; | wc -l) * 20 / $(find apps -name "*.erl" -path "*/test/*" | wc -l) + 20 ))%"

# Open report if possible
if command -v open >/dev/null 2>&1; then
    echo ""
    echo -e "${CYAN}📄 Opening report in browser...${NC}"
    open "$REPORT_FILE"
fi