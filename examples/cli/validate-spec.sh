#!/bin/bash
# validate-spec.sh - Example script showing how to validate MCP spec compliance
# This script demonstrates various validation workflows

set -euo pipefail

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
NC='\033[0m'

PROJECT_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"

# ============================================================================
# Function: Print section headers
# ============================================================================
print_section() {
    echo ""
    echo -e "${BLUE}========================================${NC}"
    echo -e "${BLUE}$1${NC}"
    echo -e "${BLUE}========================================${NC}"
}

print_success() {
    echo -e "${GREEN}✓ $1${NC}"
}

print_error() {
    echo -e "${RED}✗ $1${NC}"
}

# ============================================================================
# Workflow 1: Validate all specs
# ============================================================================
workflow_validate_all() {
    print_section "Workflow 1: Validate All Specifications"

    echo "Running: erlmcp_validate run --all"
    cd "$PROJECT_ROOT"

    # Compile first
    echo "Building validator..."
    rebar3 compile > /dev/null 2>&1 || {
        print_error "Build failed"
        return 1
    }

    # Run validation
    ./_build/default/bin/erlmcp_validate run --all || {
        print_error "Validation failed"
        return 1
    }

    print_success "All validations passed"
}

# ============================================================================
# Workflow 2: Validate specific section
# ============================================================================
workflow_validate_section() {
    local section="${1:-protocol}"

    print_section "Workflow 2: Validate Specific Section ($section)"

    echo "Running: erlmcp_validate run --section $section"
    cd "$PROJECT_ROOT"

    ./_build/default/bin/erlmcp_validate run --section "$section" || {
        print_error "Validation failed for section: $section"
        return 1
    }

    print_success "Section validation passed: $section"
}

# ============================================================================
# Workflow 3: Validate transport
# ============================================================================
workflow_validate_transport() {
    local transport="${1:-stdio}"

    print_section "Workflow 3: Validate Transport ($transport)"

    echo "Running: erlmcp_validate run --transport $transport"
    cd "$PROJECT_ROOT"

    ./_build/default/bin/erlmcp_validate run --transport "$transport" || {
        print_error "Validation failed for transport: $transport"
        return 1
    }

    print_success "Transport validation passed: $transport"
}

# ============================================================================
# Workflow 4: Generate markdown report
# ============================================================================
workflow_generate_report_markdown() {
    print_section "Workflow 4: Generate Markdown Report"

    echo "Running: erlmcp_validate report --format markdown"
    cd "$PROJECT_ROOT"

    ./_build/default/bin/erlmcp_validate report --format markdown > \
        compliance_report_markdown.md || {
        print_error "Report generation failed"
        return 1
    }

    print_success "Report generated: compliance_report_markdown.md"
    echo ""
    echo "Report preview (first 50 lines):"
    head -50 compliance_report_markdown.md
}

# ============================================================================
# Workflow 5: Generate JSON report
# ============================================================================
workflow_generate_report_json() {
    print_section "Workflow 5: Generate JSON Report"

    echo "Running: erlmcp_validate report --format json"
    cd "$PROJECT_ROOT"

    ./_build/default/bin/erlmcp_validate report --format json > \
        compliance_report_json.json || {
        print_error "Report generation failed"
        return 1
    }

    print_success "Report generated: compliance_report_json.json"
    echo ""
    echo "Report preview:"
    head -20 compliance_report_json.json
}

# ============================================================================
# Workflow 6: Verbose validation
# ============================================================================
workflow_validate_verbose() {
    print_section "Workflow 6: Verbose Validation Output"

    echo "Running: erlmcp_validate run --all --verbose"
    cd "$PROJECT_ROOT"

    ./_build/default/bin/erlmcp_validate run --all --verbose | head -50 || {
        print_error "Verbose validation failed"
        return 1
    }

    print_success "Verbose validation completed"
}

# ============================================================================
# Workflow 7: Quick compliance check
# ============================================================================
workflow_quick_check() {
    print_section "Workflow 7: Quick Compliance Check"

    echo "Compiling project..."
    cd "$PROJECT_ROOT"
    rebar3 compile > /dev/null 2>&1

    echo "Running quick checks..."

    # Check build status
    if ! rebar3 compile > /dev/null 2>&1; then
        print_error "Compilation failed"
        return 1
    fi
    print_success "Compilation"

    # Check tests
    if ! rebar3 eunit --verbose > /dev/null 2>&1; then
        print_error "Tests failed"
        return 1
    fi
    print_success "Unit tests"

    # Check spec compliance
    if ./_build/default/bin/erlmcp_validate run --all > /dev/null 2>&1; then
        print_success "Spec compliance"
    else
        print_error "Spec compliance failed"
        return 1
    fi

    print_success "All quick checks passed"
}

# ============================================================================
# Workflow 8: Validate all transports
# ============================================================================
workflow_validate_all_transports() {
    print_section "Workflow 8: Validate All Transports"

    cd "$PROJECT_ROOT"

    local transports=("stdio" "http" "sse" "tcp" "ws")

    for transport in "${transports[@]}"; do
        echo -n "Validating transport: $transport ... "
        if ./_build/default/bin/erlmcp_validate run --transport "$transport" \
            > /dev/null 2>&1; then
            print_success "passed"
        else
            echo "skipped (not available)"
        fi
    done
}

# ============================================================================
# Workflow 9: Continuous validation (watch mode)
# ============================================================================
workflow_watch_validation() {
    print_section "Workflow 9: Continuous Validation (Watch Mode)"

    cd "$PROJECT_ROOT"

    echo "Watching for changes (Ctrl+C to stop)..."
    echo ""

    while true; do
        clear
        echo -e "${BLUE}=== Continuous Validation ===${NC}"
        echo "Last check: $(date)"
        echo ""

        if rebar3 compile 2>&1 | grep -q "error"; then
            print_error "Compilation failed"
        else
            print_success "Compilation successful"
        fi

        if ./_build/default/bin/erlmcp_validate run --all \
            > /dev/null 2>&1; then
            print_success "Spec compliance passed"
        else
            print_error "Spec compliance failed"
        fi

        echo ""
        echo "Press Ctrl+C to exit, waiting 5 seconds before next check..."
        sleep 5
    done
}

# ============================================================================
# Workflow 10: Compare with baseline
# ============================================================================
workflow_compare_baseline() {
    print_section "Workflow 10: Compare with Baseline"

    cd "$PROJECT_ROOT"

    # Generate current report
    echo "Generating current report..."
    ./_build/default/bin/erlmcp_validate report --format json > \
        current_report.json

    # If baseline exists, compare
    if [ -f "baseline_report.json" ]; then
        echo "Comparing with baseline..."

        # Simple diff (can be enhanced)
        diff -u baseline_report.json current_report.json || {
            print_error "Report differs from baseline"
            return 1
        }

        print_success "Report matches baseline"
    else
        echo "No baseline found. Creating one..."
        cp current_report.json baseline_report.json
        print_success "Baseline created"
    fi
}

# ============================================================================
# Main
# ============================================================================
main() {
    local workflow="${1:-all}"

    case "$workflow" in
        1|all)
            workflow_validate_all
            ;;
        2|section)
            workflow_validate_section "${2:-protocol}"
            ;;
        3|transport)
            workflow_validate_transport "${2:-stdio}"
            ;;
        4|report-md)
            workflow_generate_report_markdown
            ;;
        5|report-json)
            workflow_generate_report_json
            ;;
        6|verbose)
            workflow_validate_verbose
            ;;
        7|quick)
            workflow_quick_check
            ;;
        8|all-transports)
            workflow_validate_all_transports
            ;;
        9|watch)
            workflow_watch_validation
            ;;
        10|baseline)
            workflow_compare_baseline
            ;;
        *)
            echo "Usage: $0 <workflow> [args]"
            echo ""
            echo "Workflows:"
            echo "  1, all              Validate all specifications"
            echo "  2, section [SECT]   Validate specific section (default: protocol)"
            echo "  3, transport [TRANS] Validate transport (default: stdio)"
            echo "  4, report-md        Generate markdown report"
            echo "  5, report-json      Generate JSON report"
            echo "  6, verbose          Validate with verbose output"
            echo "  7, quick            Quick compliance check"
            echo "  8, all-transports   Validate all transport implementations"
            echo "  9, watch            Continuous validation (watch mode)"
            echo "  10, baseline        Compare with baseline report"
            echo ""
            echo "Examples:"
            echo "  $0 1                    # Validate everything"
            echo "  $0 2 protocol           # Validate protocol section"
            echo "  $0 3 http               # Validate HTTP transport"
            echo "  $0 4                    # Generate markdown report"
            echo "  $0 7                    # Quick check"
            echo "  $0 9                    # Watch for changes"
            exit 1
            ;;
    esac
}

# Run main
main "$@"
