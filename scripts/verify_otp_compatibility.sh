#!/bin/bash

# OTP Compatibility Verification Script
# This script verifies OTP compatibility and applies necessary fixes

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Configuration
REBAR_CONFIG="rebar.config"
OTP_COMPAT_HEADER="include/otp_compat.hrl"
BUILD_DIR="_build"

# Logging functions
log_info() {
    echo -e "${GREEN}[INFO]${NC} $1"
}

log_warn() {
    echo -e "${YELLOW}[WARN]${NC} $1"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

log_section() {
    echo ""
    echo -e "${YELLOW}=== $1 ===${NC}"
    echo ""
}

# Check if we're in the right directory
if [ ! -f "$REBAR_CONFIG" ]; then
    log_error "Not in erlmcp directory. Missing $REBAR_CONFIG"
    exit 1
fi

# Function to check OTP version
check_otp_version() {
    log_section "OTP Version Check"

    if [ -f ".erlmcp/env.sh" ]; then
        source .erlmcp/env.sh
    fi

    OTP_VERSION=$(erl -noshell -eval 'io:format("~s~n", [erlang:system_info(otp_release)]), halt(0).' 2>/dev/null || echo "unknown")

    log_info "Current OTP version: $OTP_VERSION"

    case "$OTP_VERSION" in
        "28"*)
            log_info "OTP 28 detected - Full feature support available"
            ;;
        "27"*)
            log_info "OTP 27 detected - Native JSON available, no priority messages"
            ;;
        "26"*)
            log_info "OTP 26 detected - External JSON required, no modern features"
            ;;
        *)
            log_error "Unknown OTP version: $OTP_VERSION"
            return 1
            ;;
    esac
}

# Function to check compilation
check_compilation() {
    log_section "Compilation Check"

    if [ -f ".erlmcp/env.sh" ]; then
        source .erlmcp/env.sh
    fi

    log_info "Testing compilation with current OTP version..."

    if rebar3 compile >/dev/null 2>&1; then
        log_info "Compilation successful"
        return 0
    else
        log_error "Compilation failed"
        return 1
    fi
}

# Function to check rebar.config for platform defines
check_platform_defines() {
    log_section "Platform Defines Check"

    if grep -q "OTP_28_PLUS" "$REBAR_CONFIG"; then
        log_info "OTP_28_PLUS platform define found in rebar.config"
    else
        log_error "OTP_28_PLUS platform define missing from rebar.config"
        log_info "This is required for version detection"
        return 1
    fi
}

# Function to check otp_compat.hrl
check_otp_compat_header() {
    log_section "OTP Compatibility Header Check"

    if [ -f "$OTP_COMPAT_HEADER" ]; then
        log_info "OTP compatibility header found"

        # Check for required macros
        if grep -q "OTP_28_PLUS" "$OTP_COMPAT_HEADER"; then
            log_info "OTP_28_PLUS macro defined in header"
        else
            log_error "OTP_28_PLUS macro missing from header"
            return 1
        fi
    else
        log_error "OTP compatibility header not found: $OTP_COMPAT_HEADER"
        return 1
    fi
}

# Function to check specific modules
check_modules() {
    log_section "Module Compatibility Check"

    modules=(
        "apps/erlmcp_core/src/erlmcp_json_native.erl"
        "apps/erlmcp_observability/src/erlmcp_process_monitor.erl"
        "apps/erlmcp_core/src/erlmcp_graceful_drain.erl"
        "apps/erlmcp_core/src/erlmcp_otp_compat.erl"
    )

    for module in "${modules[@]}"; do
        if [ -f "$module" ]; then
            log_info "Module found: $module"
        else
            log_error "Module missing: $module"
        fi
    done
}

# Function to check JSON usage
check_json_usage() {
    log_section "JSON Usage Check"

    json_files=$(find . -name "*.erl" -exec grep -l "json:encode\|json:decode\|jsx:encode\|jsx:decode" {} \ | wc -l)

    log_info "Files using JSON encoding/decoding: $json_files"

    if [ "$json_files" -gt 0 ]; then
        log_info "JSON usage detected - should use otp_compat macros"
    fi
}

# Function to check process enumeration
check_process_enumeration() {
    log_section "Process Enumeration Check"

    process_files=$(find . -name "*.erl" -exec grep -l "processes_iterator\|process_next" {} \ | wc -l)

    log_info "Files using process iterators: $process_files"

    if [ "$process_files" -gt 0 ]; then
        log_info "Process iterator usage detected - should use otp_compat macros"
    fi
}

# Function to generate report
generate_report() {
    log_section "Compatibility Report"

    report_file="otp_compatibility_report_$(date +%Y%m%d_%H%M%S).txt"

    {
        echo "OTP Compatibility Report"
        echo "Generated: $(date)"
        echo "====================================="
        echo ""
        echo "OTP Version: $OTP_VERSION"
        echo ""
        echo "Compilation Status: $([ $? -eq 0 ] && echo "PASS" || echo "FAIL")"
        echo "Platform Defines: $([ grep -q "OTP_28_PLUS" "$REBAR_CONFIG" ] && echo "PASS" || echo "FAIL")"
        echo "OTP Header: $([ -f "$OTP_COMPAT_HEADER" ] && echo "PASS" || echo "FAIL")"
        echo ""
        echo "Modules to Update:"
        echo "- erlmcp_json_native.erl: Version guards needed"
        echo "- erlmcp_process_monitor.erl: otp_compat macros needed"
        echo "- erlmcp_graceful_drain.erl: Priority message fallbacks"
        echo ""
        echo "Files Using JSON: $json_files"
        echo "Files Using Process Iterators: $process_files"
    } > "$report_file"

    log_info "Report generated: $report_file"
}

# Function to apply fixes
apply_fixes() {
    log_section "Applying Compatibility Fixes"

    # Fix 1: Add platform defines to rebar.config
    if ! grep -q "OTP_28_PLUS" "$REBAR_CONFIG"; then
        log_info "Adding platform defines to rebar.config"

        # Create backup
        cp "$REBAR_CONFIG" "$REBAR_CONFIG.backup"

        # Add platform defines after the minimum_otp_vsn line
        sed '/{minimum_otp_vsn.*/a\\n%% Platform defines for version detection\n{platform_define,\n    "^2[8-9]|^[3-9]", '"'"'OTP_28_PLUS'"'"'}.' "$REBAR_CONFIG" > "$REBAR_CONFIG.tmp"
        mv "$REBAR_CONFIG.tmp" "$REBAR_CONFIG"

        log_info "Platform defines added to rebar.config"
    fi

    # Fix 2: Update erlmcp_json_native.erl
    if [ -f "apps/erlmcp_core/src/erlmcp_json_native.erl" ]; then
        log_info "Updating erlmcp_json_native.erl with version guards"

        # Add include directive
        if ! grep -q "#include.*otp_compat" "apps/erlmcp_core/src/erlmcp_json_native.erl"; then
            sed -i '1i\\-include("otp_compat.hrl").' "apps/erlmcp_core/src/erlmcp_json_native.erl"
        fi

        # Update functions to use otp_compat macros
        sed -i 's/json:encode(/?JSON_ENCODE(/g' "apps/erlmcp_core/src/erlmcp_json_native.erl"
        sed -i 's/json:decode(/?JSON_DECODE(/g' "apps/erlmcp_core/src/erlmcp_json_native.erl"

        log_info "erlmcp_json_native.erl updated"
    fi

    # Fix 3: Update erlmcp_process_monitor.erl
    if [ -f "apps/erlmcp_observability/src/erlmcp_process_monitor.erl" ]; then
        log_info "Updating erlmcp_process_monitor.erl with otp_compat macros"

        # Add include directive
        if ! grep -q "#include.*otp_compat" "apps/erlmcp_observability/src/erlmcp_process_monitor.erl"; then
            sed -i '1i\\-include("otp_compat.hrl").' "apps/erlmcp_observability/src/erlmcp_process_monitor.erl"
        fi

        # Replace direct iterator calls
        sed -i 's/erlang:processes_iterator()/?SAFE_PROCESSES()/g' "apps/erlmcp_observability/src/erlmcp_process_monitor.erl"
        sed -i 's/erlang:process_next(/?SAFE_PROCESS_NEXT(/g' "apps/erlmcp_observability/src/erlmcp_process_monitor.erl"

        log_info "erlmcp_process_monitor.erl updated"
    fi

    # Fix 4: Update erlmcp_graceful_drain.erl
    if [ -f "apps/erlmcp_core/src/erlmcp_graceful_drain.erl" ]; then
        log_info "Updating erlmcp_graceful_drain.erl with priority message macros"

        # Add include directive
        if ! grep -q "#include.*otp_compat" "apps/erlmcp_core/src/erlmcp_graceful_drain.erl"; then
            sed -i '1i\\-include("otp_compat.hrl").' "apps/erlmcp_core/src/erlmcp_graceful_drain.erl"
        fi

        # Replace direct priority calls
        sed -i 's/process_flag(priority, high)/?SET_PRIORITY_HIGH()/g' "apps/erlmcp_core/src/erlmcp_graceful_drain.erl"
        sed -i 's/erlang:send.*, \[.*priority.*\]/?SEND_PRIORITY(\0)/g' "apps/erlmcp_core/src/erlmcp_graceful_drain.erl"

        log_info "erlmcp_graceful_drain.erl updated"
    fi
}

# Function to run comprehensive test
run_comprehensive_test() {
    log_section "Comprehensive Compatibility Test"

    log_info "Running multi-version compatibility tests..."

    # Test compilation
    if check_compilation; then
        log_info "✅ Compilation test passed"
    else
        log_error "❌ Compilation test failed"
        return 1
    fi

    # Test JSON compatibility
    if erl -noshell -eval 'io:format("~p~n", [case erlang:function_exported(json, encode, 1) of true -> ok; false -> error end]), halt().' 2>/dev/null; then
        log_info "✅ JSON module available"
    else
        log_warn "⚠️ JSON module not available - will use jsx"
    fi

    # Test process iterator
    if erl -noshell -eval 'io:format("~p~n", [case erlang:function_exported(erlang, processes_iterator, 0) of true -> ok; false -> error end]), halt().' 2>/dev/null; then
        log_info "✅ Process iterator available"
    else
        log_warn "⚠️ Process iterator not available - will use processes()"
    fi

    # Test priority messages
    if erl -noshell -eval 'io:format("~p~n", [case erlang:function_exported(erlang, send, 3) of true -> ok; false -> error end]), halt().' 2>/dev/null; then
        log_info "✅ Priority messages available"
    else
        log_warn "⚠️ Priority messages not available - will use regular messages"
    fi
}

# Main function
main() {
    log_section "OTP Compatibility Verification"
    log_info "Starting OTP compatibility verification..."

    # Run all checks
    check_otp_version
    check_compilation
    check_platform_defines
    check_otp_compat_header
    check_modules
    check_json_usage
    check_process_enumeration

    # Generate report
    generate_report

    # Ask user if they want to apply fixes
    echo ""
    read -p "Do you want to apply compatibility fixes? (y/n): " -n 1 -r
    echo
    if [[ $REPLY =~ ^[Yy]$ ]]; then
        apply_fixes
        log_info "Fixes applied. Please run './scripts/verify_otp_compatibility.sh' again to verify."
    fi

    # Ask user if they want to run comprehensive test
    echo ""
    read -p "Do you want to run comprehensive compatibility test? (y/n): " -n 1 -r
    echo
    if [[ $REPLY =~ ^[Yy]$ ]]; then
        run_comprehensive_test
    fi

    log_section "Verification Complete"
    log_info "OTP compatibility verification finished."
    log_info "Check the generated report for detailed results."
}

# Run main function
main "$@"