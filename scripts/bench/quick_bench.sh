#!/usr/bin/env bash
###===================================================================
### quick_bench.sh - Quick Benchmark Run (1-2 minutes)
###===================================================================
###
### Runs minimal benchmark suite for rapid iteration during development.
### Executes 1 workload from each category, skips heavy stress/chaos tests.
###
### Usage:
###   ./scripts/bench/quick_bench.sh
###
### Environment:
###   ERLMCP_PROFILE  Profile to use (dev|test|staging|prod), defaults to 'staging'
###
### Perfect for:
###   - Local development verification
###   - Pre-commit smoke testing
###   - Quick regression checks
###
###===================================================================

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# ==============================================================================
# Profile Configuration
# ==============================================================================

# Use staging profile for benchmarks (production-like, with logging)
ERLMCP_PROFILE="${ERLMCP_PROFILE:-staging}"

# Validate profile (with graceful fallback to staging)
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
VALIDATE_SCRIPT="$PROJECT_ROOT/scripts/validate_profile.sh"

if [ -f "$VALIDATE_SCRIPT" ]; then
    if ! "$VALIDATE_SCRIPT" "$ERLMCP_PROFILE" 2>/dev/null; then
        echo "WARNING: Invalid profile '$ERLMCP_PROFILE', falling back to 'staging'"
        ERLMCP_PROFILE=staging
    fi
else
    echo "WARNING: validate_profile.sh not found, using profile: $ERLMCP_PROFILE"
fi

export ERLMCP_PROFILE

# Run in quick mode with relaxed metrology (warnings only)
export BENCHMARK_MODE="quick"
export METROLOGY_STRICT="false"
export REGRESSION_THRESHOLD="20"  # More lenient for dev

echo "Running quick benchmark suite..."
echo ""

"$SCRIPT_DIR/run_all_benchmarks.sh" quick
