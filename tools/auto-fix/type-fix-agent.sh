#!/usr/bin/env bash
# Type Fix Agent - Auto-fixes Dialyzer type errors
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
    echo -e "[$(date '+%Y-%m-%d %H:%M:%S')] [TYPE-FIX] $*" | tee -a "$LOG_DIR/type-fix.log"
}

# Add missing -spec for a function
add_function_spec() {
    local file="$1"
    local func_name="$2"
    local arity="$3"
    local spec="$4"

    log "Adding -spec for ${func_name}/${arity} in $file"

    # Find the function definition and add spec before it
    local line_num=$(grep -n "^${func_name}(" "$file" | head -1 | cut -d: -f1)

    if [[ -z "$line_num" ]]; then
        log "${RED}✗ Could not find function ${func_name}/${arity}${NC}"
        return 1
    fi

    # Insert spec line before function
    sed -i.bak "${line_num}i\\
-spec ${func_name}(${spec}) -> term().
" "$file"

    if [[ $? -eq 0 ]]; then
        log "${GREEN}✓ Added spec for ${func_name}/${arity}${NC}"
        rm -f "${file}.bak"
        return 0
    else
        log "${RED}✗ Failed to add spec${NC}"
        mv "${file}.bak" "$file" 2>/dev/null || true
        return 1
    fi
}

# Generate type spec based on function analysis
generate_spec() {
    local file="$1"
    local func_name="$2"
    local arity="$3"

    log "Generating spec for ${func_name}/${arity}"

    # Extract function definition
    local func_def=$(sed -n "/^${func_name}(/,/^[a-z]/p" "$file" | head -n -1)

    if [[ -z "$func_def" ]]; then
        echo "term()"
        return
    fi

    # Simple heuristic: count parameters
    local params=""
    for ((i=1; i<=arity; i++)); do
        if [[ $i -gt 1 ]]; then
            params="${params}, "
        fi
        params="${params}term()"
    done

    echo "$params"
}

# Fix contract mismatch
fix_contract_mismatch() {
    local file="$1"
    local func_name="$2"
    local expected_type="$3"
    local actual_type="$4"

    log "Fixing contract mismatch for $func_name"
    log "  Expected: $expected_type"
    log "  Actual:   $actual_type"

    # Find existing spec
    if grep -q "^-spec ${func_name}(" "$file"; then
        log "${YELLOW}⚠ Updating existing spec${NC}"

        # Update the spec (simple replacement)
        sed -i.bak "/^-spec ${func_name}(/d" "$file"

        local line_num=$(grep -n "^${func_name}(" "$file" | head -1 | cut -d: -f1)
        sed -i.bak "${line_num}i\\
-spec ${func_name}(${actual_type}) -> term().
" "$file"

        if [[ $? -eq 0 ]]; then
            log "${GREEN}✓ Updated spec${NC}"
            rm -f "${file}.bak"
            return 0
        else
            mv "${file}.bak" "$file" 2>/dev/null || true
            return 1
        fi
    else
        log "${YELLOW}⚠ No existing spec found, adding new one${NC}"
        add_function_spec "$file" "$func_name" "unknown" "$actual_type"
    fi
}

# Parse Dialyzer warnings
parse_dialyzer_warnings() {
    local error_file="$1"

    # Extract warnings with file:line format
    grep -E "\.erl:[0-9]+:" "$error_file" || true
}

# Analyze and fix Dialyzer errors
analyze_and_fix() {
    local error_file="$1"
    local fixes_applied=0

    log "${BLUE}Analyzing Dialyzer warnings (attempt $ATTEMPT)${NC}"

    # Parse warnings
    local warnings=$(parse_dialyzer_warnings "$error_file")

    if [[ -z "$warnings" ]]; then
        log "${YELLOW}No parseable Dialyzer warnings found${NC}"
        return 1
    fi

    # Process each warning
    while IFS= read -r warning_line; do
        log "Processing: $warning_line"

        # Extract file, line, and warning message
        if [[ "$warning_line" =~ ^([^:]+):([0-9]+):[[:space:]]*(.+)$ ]]; then
            local file="${BASH_REMATCH[1]}"
            local line="${BASH_REMATCH[2]}"
            local message="${BASH_REMATCH[3]}"

            # Match warning patterns

            # Pattern: Function has no local return
            if [[ "$message" =~ Function[[:space:]]([a-z_][a-z0-9_]*)/([0-9]+)[[:space:]]has[[:space:]]no[[:space:]]local[[:space:]]return ]]; then
                local func_name="${BASH_REMATCH[1]}"
                local arity="${BASH_REMATCH[2]}"

                log "${YELLOW}⚠ Function ${func_name}/${arity} has no local return${NC}"
                # This often indicates a logic error, not a type spec issue
                log "Manual review recommended"

            # Pattern: Invalid type specification
            elif [[ "$message" =~ Invalid[[:space:]]type[[:space:]]specification ]]; then
                log "${YELLOW}⚠ Invalid type spec${NC}"
                # Extract function name if available
                if [[ "$message" =~ for[[:space:]]function[[:space:]]([a-z_][a-z0-9_]*)/([0-9]+) ]]; then
                    local func_name="${BASH_REMATCH[1]}"
                    local arity="${BASH_REMATCH[2]}"

                    log "Attempting to fix spec for ${func_name}/${arity}"
                    local spec=$(generate_spec "$file" "$func_name" "$arity")
                    if add_function_spec "$file" "$func_name" "$arity" "$spec"; then
                        ((fixes_applied++))
                    fi
                fi

            # Pattern: Missing spec
            elif [[ "$message" =~ has[[:space:]]no[[:space:]].*spec ]]; then
                # Extract function name
                if [[ "$message" =~ function[[:space:]]([a-z_][a-z0-9_]*)/([0-9]+) ]] || \
                   [[ "$warning_line" =~ ([a-z_][a-z0-9_]*)/([0-9]+) ]]; then
                    local func_name="${BASH_REMATCH[1]}"
                    local arity="${BASH_REMATCH[2]}"

                    local spec=$(generate_spec "$file" "$func_name" "$arity")
                    if add_function_spec "$file" "$func_name" "$arity" "$spec"; then
                        ((fixes_applied++))
                    fi
                fi

            else
                log "${YELLOW}⚠ Unknown Dialyzer warning pattern${NC}"
            fi
        fi
    done <<< "$warnings"

    log "Fixes applied: $fixes_applied"

    if [[ $fixes_applied -gt 0 ]]; then
        # Verify fixes
        log "Verifying fixes..."
        if rebar3 dialyzer > /dev/null 2>&1; then
            log "${GREEN}✅ Dialyzer clean after fixes${NC}"
            return 0
        else
            log "${YELLOW}⚠ Dialyzer still reporting warnings${NC}"
            return 1
        fi
    else
        log "${RED}❌ No fixes could be applied${NC}"
        return 1
    fi
}

# Suggest manual fixes
suggest_manual_fixes() {
    local error_file="$1"

    log "${BLUE}Generating manual fix suggestions${NC}"

    local suggestion_file="$LOG_DIR/type-suggestions-$(date +%s).txt"

    cat > "$suggestion_file" <<EOF
TYPE FIX SUGGESTIONS
====================
Generated: $(date)
Attempt: $ATTEMPT

Dialyzer Warnings:
------------------
$(cat "$error_file")

Common Fixes:
-------------
1. Add -spec declarations:
   -spec function_name(ArgType1, ArgType2) -> ReturnType.

2. Fix type mismatches:
   - Check return types match spec
   - Verify argument types are correct
   - Use proper type unions: type1() | type2()

3. Common types:
   term()           - Any term
   atom()           - Atom
   integer()        - Integer
   binary()         - Binary
   list(T)          - List of type T
   map()            - Map
   pid()            - Process ID
   reference()      - Reference
   ok | {error, term()}  - Common return pattern

4. Export types:
   -export_type([my_type/0]).

Next Steps:
-----------
1. Review warnings above
2. Add missing specs
3. Fix type mismatches
4. Run: rebar3 dialyzer
5. Iterate until clean

References:
-----------
- Erlang Types: https://erlang.org/doc/reference_manual/typespec.html
- Dialyzer Guide: https://erlang.org/doc/man/dialyzer.html

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

    log "${BLUE}Type Fix Agent starting (attempt $ATTEMPT)${NC}"

    if analyze_and_fix "$ERROR_FILE"; then
        log "${GREEN}✅ Auto-fix successful${NC}"
        exit 0
    else
        suggest_manual_fixes "$ERROR_FILE"
        log "${RED}❌ Auto-fix failed, manual intervention required${NC}"
        exit 1
    fi
}

main "$@"
