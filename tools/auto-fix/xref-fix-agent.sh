#!/usr/bin/env bash
# XRef Fix Agent - Handles cross-reference issues
# Part of erlmcp Auto-Fix System

set -euo pipefail

ERROR_FILE="${1:-}"
ATTEMPT="${2:-1}"
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
LOG_DIR="logs/auto-fix"

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

log() {
    echo -e "[$(date '+%Y-%m-%d %H:%M:%S')] [XREF] $*" | tee -a "$LOG_DIR/xref-fix.log"
}

# Parse XRef warnings
parse_xref_warnings() {
    local error_file="$1"

    # Extract undefined function calls and unused exports
    grep -E "undefined|unused|Warning" "$error_file" || true
}

# Fix unused exports
fix_unused_exports() {
    local file="$1"
    local function="$2"

    log "Removing unused export: $function from $file"

    # Remove from export list
    sed -i.bak "s/,\s*${function}//g" "$file"
    sed -i.bak "s/${function}\s*,//g" "$file"
    sed -i.bak "s/${function}//g" "$file"

    # Clean up empty export lists
    sed -i.bak "s/-export(\[\s*\])\.//g" "$file"

    if [[ $? -eq 0 ]]; then
        log "${GREEN}✓ Removed unused export: $function${NC}"
        rm -f "${file}.bak"
        return 0
    else
        log "${RED}✗ Failed to remove export${NC}"
        mv "${file}.bak" "$file" 2>/dev/null || true
        return 1
    fi
}

# Analyze XRef issues
analyze_xref() {
    local error_file="$1"
    local fixes_applied=0

    log "${BLUE}Analyzing XRef warnings (attempt $ATTEMPT)${NC}"

    local warnings=$(parse_xref_warnings "$error_file")

    if [[ -z "$warnings" ]]; then
        log "${YELLOW}No parseable XRef warnings found${NC}"
        return 1
    fi

    log "XRef Warnings:"
    echo "$warnings"

    # Process warnings
    while IFS= read -r warning; do
        # Match unused export pattern
        if [[ "$warning" =~ unused.*export.*([a-z_][a-z0-9_]*)/([0-9]+) ]]; then
            local func_name="${BASH_REMATCH[1]}"
            local arity="${BASH_REMATCH[2]}"

            log "${BLUE}Found unused export: ${func_name}/${arity}${NC}"

            # Find which file exports this
            local file=$(grep -l "export.*${func_name}/${arity}" src/*.erl | head -1)

            if [[ -n "$file" ]] && [[ -f "$file" ]]; then
                if fix_unused_exports "$file" "${func_name}/${arity}"; then
                    ((fixes_applied++))
                fi
            fi

        # Match undefined function call
        elif [[ "$warning" =~ undefined.*function.*([a-z_][a-z0-9_]*):([a-z_][a-z0-9_]*)/([0-9]+) ]]; then
            local module="${BASH_REMATCH[1]}"
            local func="${BASH_REMATCH[2]}"
            local arity="${BASH_REMATCH[3]}"

            log "${YELLOW}⚠ Undefined function: ${module}:${func}/${arity}${NC}"
            log "Manual review required - check dependencies or function name"
        fi
    done <<< "$warnings"

    log "Fixes applied: $fixes_applied"

    if [[ $fixes_applied -gt 0 ]]; then
        # Verify fixes
        log "Verifying fixes..."
        if rebar3 xref > /dev/null 2>&1; then
            log "${GREEN}✅ XRef clean after fixes${NC}"
            return 0
        else
            log "${YELLOW}⚠ XRef still reporting warnings${NC}"
            return 1
        fi
    else
        return 1
    fi
}

# Generate suggestions
suggest_manual_fixes() {
    local error_file="$1"

    log "${BLUE}Generating manual fix suggestions${NC}"

    local suggestion_file="$LOG_DIR/xref-suggestions-$(date +%s).txt"

    cat > "$suggestion_file" <<EOF
XREF FIX SUGGESTIONS
====================
Generated: $(date)
Attempt: $ATTEMPT

XRef Warnings:
--------------
$(cat "$error_file")

Common Fixes:
-------------
1. Undefined Functions:
   - Check spelling and module name
   - Verify function is exported in target module
   - Add dependency to rebar.config if external module
   - Check if function was renamed/removed

2. Unused Exports:
   - Remove from -export([...]) list
   - Or add tests/usage for the function
   - Consider if it's a public API

3. Circular Dependencies:
   - Refactor to break circular references
   - Extract common functionality to shared module
   - Use dependency injection patterns

4. Missing Modules:
   - Add to rebar.config dependencies
   - Check module name spelling
   - Verify module is compiled

Next Steps:
-----------
1. Review warnings above
2. Fix undefined function calls
3. Remove unused exports
4. Run: rebar3 xref
5. Run: rebar3 compile (to verify changes)

Debugging XRef:
---------------
rebar3 xref                    # Run XRef analysis
rebar3 as test xref            # XRef in test profile
rebar3 shell                   # Check module availability
  code:which(module_name).     # Find module location

Suggestion file: $suggestion_file
EOF

    log "Suggestions written to: $suggestion_file"
    cat "$suggestion_file"
}

# Main
main() {
    if [[ -z "$ERROR_FILE" ]] || [[ ! -f "$ERROR_FILE" ]]; then
        echo "Usage: $0 <error_file> [attempt_number]"
        exit 1
    fi

    log "${BLUE}XRef Fix Agent starting (attempt $ATTEMPT)${NC}"

    if analyze_xref "$ERROR_FILE"; then
        log "${GREEN}✅ Auto-fix successful${NC}"
        exit 0
    else
        suggest_manual_fixes "$ERROR_FILE"
        log "${RED}❌ Auto-fix failed, manual intervention required${NC}"
        exit 1
    fi
}

main "$@"
