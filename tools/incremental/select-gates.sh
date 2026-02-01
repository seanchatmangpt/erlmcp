#!/usr/bin/env bash
# tools/incremental/select-gates.sh
# Purpose: Select required quality gates based on changed files
# Usage: ./select-gates.sh

set -euo pipefail

CACHE_DIR=".erlmcp/cache"
CHANGES_FILE="$CACHE_DIR/changes.json"
GATE_PLAN="$CACHE_DIR/gate-plan.json"

echo "Selecting required quality gates..."

if [[ ! -f "$CHANGES_FILE" ]]; then
    echo "❌ Changes file not found: $CHANGES_FILE"
    echo "Run ./detect-changes.sh first"
    exit 1
fi

# Check if jq is available
if ! command -v jq &> /dev/null; then
    echo "❌ jq not found, please install jq"
    exit 1
fi

# Get changed files
CHANGED_FILES=$(jq -r '.changed[], .added[]' "$CHANGES_FILE" 2>/dev/null || echo "")
ALL_CHANGED=$(jq -r '.all_changed' "$CHANGES_FILE" 2>/dev/null || echo "false")

# If all files changed or no changes file, run all gates
if [[ "$ALL_CHANGED" == "true" || -z "$CHANGED_FILES" ]]; then
    echo "⚠ Running all gates (all files changed or no change detection)"
    jq -n '{
        compile: true,
        eunit: true,
        ct: true,
        coverage: true,
        dialyzer: true,
        xref: true,
        benchmarks: true
    }' > "$GATE_PLAN"
    echo "✓ All gates selected"
    exit 0
fi

# Initialize gates (compile always required)
GATES='{"compile": true, "eunit": false, "ct": false, "coverage": false, "dialyzer": false, "xref": false, "benchmarks": false}'

# EUnit: Required if any .erl or .hrl changed
if echo "$CHANGED_FILES" | grep -qE '\.(erl|hrl)$'; then
    GATES=$(echo "$GATES" | jq '.eunit = true')
fi

# CT: Required if integration files changed
if echo "$CHANGED_FILES" | grep -qE '(transport|observability)'; then
    GATES=$(echo "$GATES" | jq '.ct = true')
fi

# Coverage: Required if tests changed or code changed
if echo "$CHANGED_FILES" | grep -qE '(_tests\.erl|_SUITE\.erl|\.erl)$'; then
    GATES=$(echo "$GATES" | jq '.coverage = true')
fi

# Dialyzer: Required if type specs changed
SPEC_CHANGED=false
for file in $CHANGED_FILES; do
    if [[ -f "$file" ]] && grep -qE '^-spec' "$file" 2>/dev/null; then
        SPEC_CHANGED=true
        break
    fi
done
if [[ "$SPEC_CHANGED" == "true" ]]; then
    GATES=$(echo "$GATES" | jq '.dialyzer = true')
fi

# Xref: Required if imports/exports changed
IMPORT_CHANGED=false
for file in $CHANGED_FILES; do
    if [[ -f "$file" ]] && grep -qE '^-(import|export)' "$file" 2>/dev/null; then
        IMPORT_CHANGED=true
        break
    fi
done
if [[ "$IMPORT_CHANGED" == "true" ]]; then
    GATES=$(echo "$GATES" | jq '.xref = true')
fi

# Benchmarks: Required if performance-critical modules changed
PERF_MODULES="erlmcp_json_rpc erlmcp_registry erlmcp_cache erlmcp_transport"
for module in $PERF_MODULES; do
    if echo "$CHANGED_FILES" | grep -q "${module}.erl"; then
        GATES=$(echo "$GATES" | jq '.benchmarks = true')
        break
    fi
done

# Save gate plan
echo "$GATES" > "$GATE_PLAN"

# Report
echo "✓ Gate selection complete"
echo ""
echo "Required gates:"
echo "$GATES" | jq -r 'to_entries[] | select(.value == true) | "  ✅ \(.key)"'
echo ""
echo "Skipped gates:"
echo "$GATES" | jq -r 'to_entries[] | select(.value == false) | "  ⏭ \(.key)"'
