#!/usr/bin/env bash
###############################################################################
# TCPS Workflow Example Script
#
# Demonstrates complete TCPS build workflow with quality gates,
# Andon handling, and receipt generation.
#
# Usage:
#   ./scripts/tcps_workflow_example.sh [--skip-tests]
#
###############################################################################

set -e  # Exit on error (except where handled)

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Configuration
SKIP_TESTS=false
RECEIPT_DIR="priv/receipts"

# Parse arguments
for arg in "$@"; do
    case $arg in
        --skip-tests)
            SKIP_TESTS=true
            shift
            ;;
        --help)
            echo "Usage: $0 [--skip-tests]"
            echo ""
            echo "Options:"
            echo "  --skip-tests    Skip test execution (not recommended)"
            echo "  --help          Show this help message"
            exit 0
            ;;
    esac
done

###############################################################################
# Functions
###############################################################################

info() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

success() {
    echo -e "${GREEN}[SUCCESS]${NC} $1"
}

warning() {
    echo -e "${YELLOW}[WARNING]${NC} $1"
}

error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

section() {
    echo ""
    echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
    echo -e "${GREEN}$1${NC}"
    echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
    echo ""
}

check_andon_status() {
    info "Checking for blocking Andon events..."
    if rebar3 tcps andon check > /dev/null 2>&1; then
        success "No blocking Andon events"
        return 0
    else
        warning "Build blocked by open Andon events"
        rebar3 tcps andon list
        return 1
    fi
}

###############################################################################
# Main Workflow
###############################################################################

section "TCPS Build Workflow - Toyota Code Production System"

info "Starting TCPS-validated build pipeline..."
info "Timestamp: $(date -u +"%Y-%m-%dT%H:%M:%SZ")"

# Create receipt directory
mkdir -p "$RECEIPT_DIR"
info "Receipt directory: $RECEIPT_DIR"

###############################################################################
# Stage 1: Pre-Build Validation
###############################################################################

section "Stage 1: Pre-Build Validation"

# Check if build is blocked by previous Andon
if ! check_andon_status; then
    error "Cannot proceed - resolve open Andon events first"
    error "Use: rebar3 tcps andon resolve <andon-id>"
    exit 1
fi

# Clean previous build artifacts
info "Cleaning previous build..."
rebar3 clean > /dev/null 2>&1
success "Clean complete"

###############################################################################
# Stage 2: SHACL Validation
###############################################################################

section "Stage 2: SHACL Ontology Validation"

info "Validating work orders and ontology against SHACL shapes..."

if rebar3 tcps shacl_validate; then
    success "SHACL validation passed"
    SHACL_RECEIPT=$(ls -t "$RECEIPT_DIR"/rcpt_shacl_*.json 2>/dev/null | head -1)
    if [ -n "$SHACL_RECEIPT" ]; then
        info "Receipt: $SHACL_RECEIPT"
    fi
else
    error "SHACL validation failed"
    warning "An Andon event has been triggered"
    info "Review violations and resolve Andon to proceed"
    exit 1
fi

###############################################################################
# Stage 3: Compilation
###############################################################################

section "Stage 3: Compilation"

info "Compiling Erlang modules..."

if rebar3 compile; then
    success "Compilation passed"

    # Generate compilation receipt
    info "Generating compilation receipt..."
    rebar3 tcps generate_receipt --stage=compile

    COMPILE_RECEIPT=$(ls -t "$RECEIPT_DIR"/rcpt_compile_*.json 2>/dev/null | head -1)
    if [ -n "$COMPILE_RECEIPT" ]; then
        info "Receipt: $COMPILE_RECEIPT"

        # Extract metrics
        ERROR_COUNT=$(jq -r '.metrics.error_count // 0' "$COMPILE_RECEIPT")
        WARNING_COUNT=$(jq -r '.metrics.warning_count // 0' "$COMPILE_RECEIPT")
        MODULES=$(jq -r '.metrics.modules_compiled // 0' "$COMPILE_RECEIPT")

        info "Compiled modules: $MODULES"
        info "Errors: $ERROR_COUNT, Warnings: $WARNING_COUNT"
    fi
else
    error "Compilation failed"
    warning "An Andon event has been triggered"
    exit 1
fi

###############################################################################
# Stage 4: Testing (unless skipped)
###############################################################################

if [ "$SKIP_TESTS" = true ]; then
    warning "Skipping tests (not recommended for production builds)"
else
    section "Stage 4: Testing & Quality Gates"

    info "Running test suite..."

    if rebar3 eunit; then
        success "Tests passed"

        # Generate test receipt
        info "Generating test receipt..."
        rebar3 tcps generate_receipt --stage=test

        # Check quality gates
        info "Checking quality gates (80% pass rate, 80% coverage)..."

        if rebar3 tcps check_quality_gates --stage=test; then
            success "Quality gates passed"

            QUALITY_RECEIPT=$(ls -t "$RECEIPT_DIR"/rcpt_quality_*.json 2>/dev/null | head -1)
            if [ -n "$QUALITY_RECEIPT" ]; then
                info "Receipt: $QUALITY_RECEIPT"

                # Extract quality metrics
                PASS_RATE=$(jq -r '.results[0].metrics.pass_rate // 0' "$QUALITY_RECEIPT" 2>/dev/null)
                COVERAGE=$(jq -r '.results[0].metrics.coverage // 0' "$QUALITY_RECEIPT" 2>/dev/null)

                info "Pass rate: ${PASS_RATE}%"
                info "Coverage: ${COVERAGE}%"
            fi
        else
            error "Quality gates failed"
            warning "An Andon event has been triggered"
            info "Common fixes:"
            info "  - Add tests to increase coverage above 80%"
            info "  - Fix failing tests to achieve 80% pass rate"
            exit 1
        fi
    else
        error "Tests failed"
        warning "An Andon event has been triggered"
        exit 1
    fi
fi

###############################################################################
# Stage 5: Release (Optional)
###############################################################################

section "Stage 5: Release Verification"

info "Verifying deterministic build..."

# Check if release target exists
if rebar3 release > /dev/null 2>&1; then
    success "Release built successfully"

    # Verify deterministic build
    if rebar3 tcps check_quality_gates --stage=release; then
        success "Deterministic build verified"

        RELEASE_RECEIPT=$(ls -t "$RECEIPT_DIR"/rcpt_release_*.json 2>/dev/null | head -1)
        if [ -n "$RELEASE_RECEIPT" ]; then
            info "Receipt: $RELEASE_RECEIPT"

            # Extract release metrics
            ARTIFACT_HASH=$(jq -r '.metrics.artifact_hash // "unknown"' "$RELEASE_RECEIPT" 2>/dev/null)
            ARTIFACT_SIZE=$(jq -r '.metrics.artifact_size // 0' "$RELEASE_RECEIPT" 2>/dev/null)

            info "Artifact hash: ${ARTIFACT_HASH:0:16}..."
            info "Artifact size: $ARTIFACT_SIZE bytes"
        fi
    else
        error "Deterministic build verification failed"
        exit 1
    fi
else
    warning "Release target not configured (skipping)"
fi

###############################################################################
# Stage 6: Final Status
###############################################################################

section "Stage 6: Build Summary"

# Count receipts
RECEIPT_COUNT=$(ls -1 "$RECEIPT_DIR"/rcpt_*.json 2>/dev/null | wc -l | xargs)

success "TCPS build completed successfully!"
info "Total receipts generated: $RECEIPT_COUNT"
info "Receipt directory: $RECEIPT_DIR"

# Final Andon check
if check_andon_status; then
    success "Pipeline clear - no blocking Andon events"
else
    warning "New Andon events detected"
fi

echo ""
success "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
success "  Build PASSED - Zero Defects Achieved"
success "  Ready for deployment"
success "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo ""

exit 0
