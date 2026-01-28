#!/usr/bin/env bash
# Syntax Fix Agent - Auto-fixes compilation errors
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
    echo -e "[$(date '+%Y-%m-%d %H:%M:%S')] [SYNTAX-FIX] $*" | tee -a "$LOG_DIR/syntax-fix.log"
}

# Parse compilation errors from rebar3 output
parse_errors() {
    local error_file="$1"

    # Extract compilation errors (file:line: error message format)
    grep -E "^[^:]+:[0-9]+:" "$error_file" || true
}

# Fix unused variables by prefixing with underscore
fix_unused_variables() {
    local file="$1"
    local line="$2"
    local var_name="$3"

    log "Fixing unused variable '$var_name' in $file:$line"

    # Use sed to replace variable name with _VariableName on specific line
    sed -i.bak "${line}s/\b${var_name}\b/_${var_name}/g" "$file"

    if [[ $? -eq 0 ]]; then
        log "${GREEN}✓ Fixed unused variable: $var_name${NC}"
        rm -f "${file}.bak"
        return 0
    else
        log "${RED}✗ Failed to fix unused variable: $var_name${NC}"
        mv "${file}.bak" "$file" 2>/dev/null || true
        return 1
    fi
}

# Add missing export declarations
fix_missing_export() {
    local file="$1"
    local function_name="$2"
    local arity="$3"

    log "Adding export for ${function_name}/${arity} in $file"

    # Find the -module line and add export after it
    if grep -q "^-export(\[" "$file"; then
        # Add to existing export list
        sed -i.bak "s/-export(\[\(.*\)\])\./-export([\1, ${function_name}\/${arity}])./" "$file"
    else
        # Create new export declaration after -module
        sed -i.bak "/^-module/a\\
-export([${function_name}/${arity}])." "$file"
    fi

    if [[ $? -eq 0 ]]; then
        log "${GREEN}✓ Added export: ${function_name}/${arity}${NC}"
        rm -f "${file}.bak"
        return 0
    else
        log "${RED}✗ Failed to add export${NC}"
        mv "${file}.bak" "$file" 2>/dev/null || true
        return 1
    fi
}

# Fix common typos in function names
fix_typo() {
    local file="$1"
    local line="$2"
    local typo="$3"
    local correction="$4"

    log "Fixing typo '$typo' → '$correction' in $file:$line"

    sed -i.bak "${line}s/\b${typo}\b/${correction}/g" "$file"

    if [[ $? -eq 0 ]]; then
        log "${GREEN}✓ Fixed typo: $typo → $correction${NC}"
        rm -f "${file}.bak"
        return 0
    else
        log "${RED}✗ Failed to fix typo${NC}"
        mv "${file}.bak" "$file" 2>/dev/null || true
        return 1
    fi
}

# Add missing semicolons/commas
fix_punctuation() {
    local file="$1"
    local line="$2"
    local missing="$3"

    log "Adding missing '$missing' in $file:$line"

    if [[ "$missing" == ";" ]]; then
        sed -i.bak "${line}s/$/;/" "$file"
    elif [[ "$missing" == "," ]]; then
        sed -i.bak "${line}s/$/,/" "$file"
    else
        log "${YELLOW}⚠ Unknown punctuation: $missing${NC}"
        return 1
    fi

    if [[ $? -eq 0 ]]; then
        log "${GREEN}✓ Fixed missing punctuation${NC}"
        rm -f "${file}.bak"
        return 0
    else
        log "${RED}✗ Failed to fix punctuation${NC}"
        mv "${file}.bak" "$file" 2>/dev/null || true
        return 1
    fi
}

# Analyze and fix errors
analyze_and_fix() {
    local error_file="$1"
    local fixes_applied=0

    log "${BLUE}Analyzing compilation errors (attempt $ATTEMPT)${NC}"

    # Parse errors
    local errors=$(parse_errors "$error_file")

    if [[ -z "$errors" ]]; then
        log "${YELLOW}No parseable errors found${NC}"
        return 1
    fi

    # Process each error
    while IFS= read -r error_line; do
        log "Processing: $error_line"

        # Extract file, line number, and error message
        if [[ "$error_line" =~ ^([^:]+):([0-9]+):[[:space:]]*(.+)$ ]]; then
            local file="${BASH_REMATCH[1]}"
            local line="${BASH_REMATCH[2]}"
            local message="${BASH_REMATCH[3]}"

            # Match error patterns and apply fixes

            # Pattern: variable 'Var' is unused
            if [[ "$message" =~ variable[[:space:]]\'([^\']+)\'[[:space:]]is[[:space:]]unused ]]; then
                local var_name="${BASH_REMATCH[1]}"
                if fix_unused_variables "$file" "$line" "$var_name"; then
                    ((fixes_applied++))
                fi

            # Pattern: function name/arity undefined
            elif [[ "$message" =~ function[[:space:]]\'?([a-z_][a-z0-9_]*)/([0-9]+)\'?[[:space:]]undefined ]]; then
                local func_name="${BASH_REMATCH[1]}"
                local arity="${BASH_REMATCH[2]}"
                if fix_missing_export "$file" "$func_name" "$arity"; then
                    ((fixes_applied++))
                fi

            # Pattern: syntax error before/after
            elif [[ "$message" =~ syntax[[:space:]]error[[:space:]]before ]]; then
                log "${YELLOW}⚠ Syntax error detected but auto-fix not implemented${NC}"
                log "   Manual fix required: $error_line"

            else
                log "${YELLOW}⚠ Unknown error pattern: $message${NC}"
            fi
        fi
    done <<< "$errors"

    log "Fixes applied: $fixes_applied"

    if [[ $fixes_applied -gt 0 ]]; then
        # Verify fixes by recompiling
        log "Verifying fixes..."
        if TERM=dumb rebar3 compile > /dev/null 2>&1; then
            log "${GREEN}✅ Compilation successful after fixes${NC}"
            return 0
        else
            log "${YELLOW}⚠ Compilation still failing, more work needed${NC}"
            return 1
        fi
    else
        log "${RED}❌ No fixes could be applied${NC}"
        return 1
    fi
}

# Suggest manual fixes for complex errors
suggest_manual_fixes() {
    local error_file="$1"

    log "${BLUE}Generating manual fix suggestions${NC}"

    local suggestion_file="$LOG_DIR/syntax-suggestions-$(date +%s).txt"

    cat > "$suggestion_file" <<EOF
SYNTAX FIX SUGGESTIONS
======================
Generated: $(date)
Attempt: $ATTEMPT

Complex errors requiring manual intervention:
---------------------------------------------
$(cat "$error_file")

Common Fixes:
-------------
1. Unused variables: Prefix with underscore (_Variable)
2. Missing exports: Add -export([function/arity]).
3. Undefined functions: Check spelling and imports
4. Syntax errors: Check matching brackets, commas, semicolons
5. Module not found: Verify rebar.config dependencies

Next Steps:
-----------
1. Review errors above
2. Apply manual fixes
3. Run: TERM=dumb rebar3 compile
4. Check: rebar3 dialyzer

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

    log "${BLUE}Syntax Fix Agent starting (attempt $ATTEMPT)${NC}"

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
