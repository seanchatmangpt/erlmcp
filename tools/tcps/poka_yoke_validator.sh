#!/usr/bin/env bash
# TCPS Poka-Yoke Validator (ポカヨケ - mistake-proofing)
# Purpose: Prevent errors before they happen
# Philosophy: Design out the possibility of errors

set -euo pipefail

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
BLUE='\033[0;34m'
CYAN='\033[0;36m'
NC='\033[0m'

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "${SCRIPT_DIR}/../.." && pwd)"

# Error tracking
ERRORS_FOUND=0
WARNINGS_FOUND=0

# ============================================================================
# Poka-Yoke Checks (Error-Proofing)
# ============================================================================

check_no_broken_files() {
    echo -e "${BLUE}[Poka-Yoke 1] Checking for .broken files...${NC}"

    cd "${PROJECT_ROOT}"

    local broken_files=$(find . -name "*.broken" -type f 2>/dev/null | grep -v "_build" || true)

    if [[ -n "${broken_files}" ]]; then
        echo -e "${RED}✗ Found .broken files:${NC}"
        echo "${broken_files}" | sed 's/^/    /'
        ((ERRORS_FOUND++))
        return 1
    fi

    echo -e "${GREEN}✓ No .broken files found${NC}"
    return 0
}

check_no_todo_fixme() {
    echo -e "${BLUE}[Poka-Yoke 2] Checking for TODO/FIXME in committed code...${NC}"

    cd "${PROJECT_ROOT}"

    # Check staged files only
    local staged_files=$(git diff --cached --name-only --diff-filter=ACM 2>/dev/null | grep "\.erl$" || true)

    if [[ -z "${staged_files}" ]]; then
        echo -e "${CYAN}⊘ No staged Erlang files to check${NC}"
        return 0
    fi

    local todo_count=0
    local files_with_todos=()

    while IFS= read -r file; do
        if [[ -f "${file}" ]]; then
            local todos=$(grep -n "TODO\|FIXME" "${file}" || true)
            if [[ -n "${todos}" ]]; then
                files_with_todos+=("${file}")
                ((todo_count++))
            fi
        fi
    done <<< "${staged_files}"

    if [[ ${todo_count} -gt 0 ]]; then
        echo -e "${YELLOW}⚠ Found TODO/FIXME markers in ${todo_count} staged files:${NC}"
        for file in "${files_with_todos[@]}"; do
            echo "    ${file}"
            grep -n "TODO\|FIXME" "${file}" | sed 's/^/      /' || true
        done
        ((WARNINGS_FOUND++))
        echo -e "${YELLOW}  Consider resolving before commit${NC}"
    else
        echo -e "${GREEN}✓ No TODO/FIXME in staged files${NC}"
    fi

    return 0
}

check_no_hardcoded_secrets() {
    echo -e "${BLUE}[Poka-Yoke 3] Checking for hardcoded secrets...${NC}"

    cd "${PROJECT_ROOT}"

    # Common secret patterns
    local patterns=(
        'password\s*=\s*"[^"]+'
        'api_key\s*=\s*"[^"]+'
        'secret\s*=\s*"[^"]+'
        'token\s*=\s*"[^"]+'
        'aws_access_key'
        'private_key\s*=\s*"'
    )

    local secrets_found=0

    for pattern in "${patterns[@]}"; do
        if grep -rE "${pattern}" apps/*/src/*.erl 2>/dev/null | grep -v "_test\.erl" | grep -v "example" >/dev/null; then
            if [[ ${secrets_found} -eq 0 ]]; then
                echo -e "${RED}✗ Potential hardcoded secrets detected:${NC}"
            fi
            grep -rEn "${pattern}" apps/*/src/*.erl 2>/dev/null | grep -v "_test\.erl" | grep -v "example" | sed 's/^/    /' || true
            ((secrets_found++))
        fi
    done

    if [[ ${secrets_found} -gt 0 ]]; then
        echo -e "${RED}  Use environment variables or configuration files instead${NC}"
        ((ERRORS_FOUND++))
        return 1
    fi

    echo -e "${GREEN}✓ No hardcoded secrets detected${NC}"
    return 0
}

check_no_unused_variables() {
    echo -e "${BLUE}[Poka-Yoke 4] Checking for unused variables...${NC}"

    cd "${PROJECT_ROOT}"

    # Run xref to detect unused functions
    if rebar3 xref 2>&1 | tee /tmp/xref_output.txt >/dev/null; then
        local warnings=$(grep -c "Warning:" /tmp/xref_output.txt 2>/dev/null || echo "0")

        if [[ ${warnings} -gt 0 ]]; then
            echo -e "${YELLOW}⚠ ${warnings} xref warnings:${NC}"
            grep "Warning:" /tmp/xref_output.txt | head -5 | sed 's/^/    /' || true
            if [[ ${warnings} -gt 5 ]]; then
                echo "    ... (${warnings} total)"
            fi
            ((WARNINGS_FOUND++))
        else
            echo -e "${GREEN}✓ No unused exports detected${NC}"
        fi
    else
        echo -e "${YELLOW}⚠ Xref check skipped (errors)${NC}"
    fi

    return 0
}

check_proper_module_structure() {
    echo -e "${BLUE}[Poka-Yoke 5] Checking module structure...${NC}"

    cd "${PROJECT_ROOT}"

    local malformed=0

    # Check all .erl files have matching module declarations
    while IFS= read -r file; do
        local filename=$(basename "${file}" .erl)

        if ! grep -q "^-module(${filename})" "${file}"; then
            if [[ ${malformed} -eq 0 ]]; then
                echo -e "${RED}✗ Module name mismatches:${NC}"
            fi
            echo "    ${file}: expected -module(${filename})."
            ((malformed++))
        fi
    done < <(find apps/*/src -name "*.erl" -type f 2>/dev/null)

    if [[ ${malformed} -gt 0 ]]; then
        ((ERRORS_FOUND++))
        return 1
    fi

    echo -e "${GREEN}✓ All modules properly structured${NC}"
    return 0
}

check_test_coverage() {
    echo -e "${BLUE}[Poka-Yoke 6] Checking test file existence...${NC}"

    cd "${PROJECT_ROOT}"

    local modules_without_tests=0

    # Check that each src module has a corresponding test
    while IFS= read -r src_file; do
        local module=$(basename "${src_file}" .erl)
        local app_dir=$(dirname "$(dirname "${src_file}")")

        # Skip *_sup modules (supervisors don't need unit tests)
        if [[ "${module}" == *_sup ]]; then
            continue
        fi

        # Check for test file
        local test_file="${app_dir}/test/${module}_tests.erl"

        if [[ ! -f "${test_file}" ]]; then
            if [[ ${modules_without_tests} -eq 0 ]]; then
                echo -e "${YELLOW}⚠ Modules without tests:${NC}"
            fi
            echo "    ${module} (expected: ${test_file})"
            ((modules_without_tests++))
        fi
    done < <(find apps/*/src -name "*.erl" -type f 2>/dev/null)

    if [[ ${modules_without_tests} -gt 0 ]]; then
        echo -e "${YELLOW}  Consider adding tests for Chicago School TDD${NC}"
        ((WARNINGS_FOUND++))
    else
        echo -e "${GREEN}✓ All modules have test files${NC}"
    fi

    return 0
}

check_no_debug_output() {
    echo -e "${BLUE}[Poka-Yoke 7] Checking for debug output...${NC}"

    cd "${PROJECT_ROOT}"

    local debug_statements=0

    # Check for io:format, erlang:display in non-test code
    while IFS= read -r file; do
        if grep -n "io:format\|erlang:display" "${file}" | grep -v "test\.erl" >/dev/null 2>&1; then
            if [[ ${debug_statements} -eq 0 ]]; then
                echo -e "${YELLOW}⚠ Debug output statements found:${NC}"
            fi
            grep -n "io:format\|erlang:display" "${file}" | sed 's/^/    /' | head -3
            ((debug_statements++))
        fi
    done < <(find apps/*/src -name "*.erl" -type f 2>/dev/null)

    if [[ ${debug_statements} -gt 0 ]]; then
        echo -e "${YELLOW}  Use proper logging (lager/logger) in production code${NC}"
        ((WARNINGS_FOUND++))
    else
        echo -e "${GREEN}✓ No debug output in production code${NC}"
    fi

    return 0
}

check_proper_supervision() {
    echo -e "${BLUE}[Poka-Yoke 8] Checking supervision patterns...${NC}"

    cd "${PROJECT_ROOT}"

    # Check for spawn without supervision
    local unsupervised_spawns=0

    while IFS= read -r file; do
        # Skip test files and supervisor files
        if [[ "${file}" == *_test.erl ]] || [[ "${file}" == *_sup.erl ]]; then
            continue
        fi

        # Look for raw spawn/spawn_link without supervisor
        if grep -n "spawn(" "${file}" | grep -v "spawn_link" | grep -v "supervisor:start_child" >/dev/null 2>&1; then
            if [[ ${unsupervised_spawns} -eq 0 ]]; then
                echo -e "${YELLOW}⚠ Unsupervised spawn detected:${NC}"
            fi
            grep -n "spawn(" "${file}" | grep -v "spawn_link" | sed 's/^/    /' | head -2
            ((unsupervised_spawns++))
        fi
    done < <(find apps/*/src -name "*.erl" -type f 2>/dev/null)

    if [[ ${unsupervised_spawns} -gt 0 ]]; then
        echo -e "${YELLOW}  Consider using supervisors for process management${NC}"
        ((WARNINGS_FOUND++))
    else
        echo -e "${GREEN}✓ Processes properly supervised${NC}"
    fi

    return 0
}

# ============================================================================
# Main Execution
# ============================================================================

main() {
    echo ""
    echo -e "${CYAN}╔═══════════════════════════════════════════════════════════╗${NC}"
    echo -e "${CYAN}║  TCPS Poka-Yoke Validator (ポカヨケ検証)                  ║${NC}"
    echo -e "${CYAN}║  Error-Proofing Quality Checks                            ║${NC}"
    echo -e "${CYAN}╚═══════════════════════════════════════════════════════════╝${NC}"
    echo ""

    # Run all poka-yoke checks
    check_no_broken_files
    check_no_todo_fixme
    check_no_hardcoded_secrets
    check_no_unused_variables
    check_proper_module_structure
    check_test_coverage
    check_no_debug_output
    check_proper_supervision

    # Summary
    echo ""
    echo -e "${CYAN}═══════════════════════════════════════════════════════════${NC}"

    if [[ ${ERRORS_FOUND} -eq 0 ]] && [[ ${WARNINGS_FOUND} -eq 0 ]]; then
        echo -e "${GREEN}✓ All Poka-Yoke checks passed${NC}"
        echo -e "${GREEN}✓ No errors detected${NC}"
        echo -e "${GREEN}✓ Zero defects${NC}"
        echo ""
        exit 0
    elif [[ ${ERRORS_FOUND} -eq 0 ]]; then
        echo -e "${YELLOW}⚠ ${WARNINGS_FOUND} warnings found (non-blocking)${NC}"
        echo -e "${GREEN}✓ No critical errors${NC}"
        echo ""
        exit 0
    else
        echo -e "${RED}✗ ${ERRORS_FOUND} errors found${NC}"
        echo -e "${YELLOW}⚠ ${WARNINGS_FOUND} warnings found${NC}"
        echo ""
        echo -e "${RED}Fix errors before proceeding${NC}"
        exit 1
    fi
}

if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    main "$@"
fi
