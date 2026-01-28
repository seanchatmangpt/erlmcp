#!/usr/bin/env bash

# Validate erlmcp Plan Metrology v1.5.0
# Validates all plan files against canonical metrology standards

set -euo pipefail

readonly SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
readonly PROJECT_ROOT="$(cd "${SCRIPT_DIR}/.." && pwd)"
readonly PLANS_DIR="${PROJECT_ROOT}/plans"
readonly WORKLOADS_DIR="${PROJECT_ROOT}/bench/workloads"
readonly ENVIRONMENTS_DIR="${PROJECT_ROOT}/bench/environments"
readonly GLOSSARY="${PROJECT_ROOT}/docs/metrology/METRICS_GLOSSARY.md"

readonly RED='\033[0;31m'
readonly GREEN='\033[0;32m'
readonly YELLOW='\033[1;33m'
readonly NC='\033[0m' # No Color

violations=0

echo "═══════════════════════════════════════════════════════════════"
echo "  erlmcp Plan Metrology Validator v1.5.0"
echo "═══════════════════════════════════════════════════════════════"
echo ""

# Check prerequisites
if ! command -v jq &> /dev/null; then
    echo -e "${RED}✗ ERROR: jq is required but not installed${NC}"
    exit 1
fi

if [ ! -f "${GLOSSARY}" ]; then
    echo -e "${RED}✗ ERROR: Metrics glossary not found: ${GLOSSARY}${NC}"
    exit 1
fi

echo "✓ Prerequisites OK"
echo ""

# Validate prohibited patterns
validate_prohibited_patterns() {
    local plan_file="$1"
    local basename="$(basename "${plan_file}")"
    local has_violations=0

    echo "  Checking prohibited patterns..."

    # Check for ambiguous "req/s" or "req_s"
    if grep -q '"req/s"' "${plan_file}" || grep -q '"req_s"' "${plan_file}" || grep -q 'throughput_req_s' "${plan_file}"; then
        echo -e "    ${RED}✗ VIOLATION: Ambiguous 'req/s' or 'req_s' found (use msg_per_s)${NC}"
        violations=$((violations + 1))
        has_violations=1
    fi

    # Check for ambiguous memory patterns
    if grep -q '"MiB/conn"' "${plan_file}" || grep -q '"MB/conn"' "${plan_file}"; then
        echo -e "    ${RED}✗ VIOLATION: Ambiguous 'MiB/conn' found (specify: heap, state, or RSS)${NC}"
        violations=$((violations + 1))
        has_violations=1
    fi

    # Check for numeric values without units (basic heuristic)
    if jq -e '.envelope | to_entries[] | select(.value | type == "number")' "${plan_file}" &> /dev/null; then
        echo -e "    ${YELLOW}⚠ WARNING: Found numeric values in envelope (verify units exist)${NC}"
    fi

    if [ $has_violations -eq 0 ]; then
        echo "    ${GREEN}✓ No prohibited patterns found${NC}"
    fi
}

# Validate workload references
validate_workload_refs() {
    local plan_file="$1"
    local basename="$(basename "${plan_file}")"
    local has_violations=0

    echo "  Checking workload references..."

    # Extract all workload_id references
    workload_ids=$(jq -r '.. | .workload_id? // empty' "${plan_file}" | sort -u)

    if [ -z "${workload_ids}" ]; then
        echo -e "    ${RED}✗ VIOLATION: No workload_id references found${NC}"
        violations=$((violations + 1))
        has_violations=1
    else
        for wid in ${workload_ids}; do
            workload_file="${WORKLOADS_DIR}/${wid}.json"
            if [ ! -f "${workload_file}" ]; then
                echo -e "    ${RED}✗ VIOLATION: workload_id '${wid}' not found: ${workload_file}${NC}"
                violations=$((violations + 1))
                has_violations=1
            else
                echo "    ${GREEN}✓ workload_id '${wid}' exists${NC}"
            fi
        done
    fi

    if [ $has_violations -eq 0 ] && [ -n "${workload_ids}" ]; then
        echo "    ${GREEN}✓ All workload references valid${NC}"
    fi
}

# Validate required metrology fields
validate_metrology_fields() {
    local plan_file="$1"
    local basename="$(basename "${plan_file}")"
    local has_violations=0

    echo "  Checking required metrology fields..."

    # Check metrology_compliance section exists
    if ! jq -e '.metrology_compliance' "${plan_file}" &> /dev/null; then
        echo -e "    ${RED}✗ VIOLATION: Missing metrology_compliance section${NC}"
        violations=$((violations + 1))
        has_violations=1
    else
        # Validate version
        version=$(jq -r '.metrology_compliance.version // empty' "${plan_file}")
        if [ "${version}" != "v1.5.0" ]; then
            echo -e "    ${RED}✗ VIOLATION: metrology_compliance.version must be 'v1.5.0' (found: '${version}')${NC}"
            violations=$((violations + 1))
            has_violations=1
        else
            echo "    ${GREEN}✓ metrology_compliance.version = v1.5.0${NC}"
        fi
    fi

    # Check throughput has canonical structure
    if jq -e '.envelope.throughput' "${plan_file}" &> /dev/null; then
        if ! jq -e '.envelope.throughput.unit == "msg_per_s"' "${plan_file}" &> /dev/null; then
            echo -e "    ${RED}✗ VIOLATION: throughput.unit must be 'msg_per_s'${NC}"
            violations=$((violations + 1))
            has_violations=1
        else
            echo "    ${GREEN}✓ throughput uses msg_per_s${NC}"
        fi

        if ! jq -e '.envelope.throughput.workload_id' "${plan_file}" &> /dev/null; then
            echo -e "    ${RED}✗ VIOLATION: throughput missing workload_id${NC}"
            violations=$((violations + 1))
            has_violations=1
        fi

        if ! jq -e '.envelope.throughput.transport' "${plan_file}" &> /dev/null; then
            echo -e "    ${RED}✗ VIOLATION: throughput missing transport${NC}"
            violations=$((violations + 1))
            has_violations=1
        fi
    fi

    # Check memory has breakdown
    if jq -e '.memory' "${plan_file}" &> /dev/null; then
        if ! jq -e '.memory.per_connection_heap_mib' "${plan_file}" &> /dev/null; then
            echo -e "    ${RED}✗ VIOLATION: memory missing per_connection_heap_mib${NC}"
            violations=$((violations + 1))
            has_violations=1
        fi

        if ! jq -e '.memory.per_node_total_rss_mib' "${plan_file}" &> /dev/null; then
            echo -e "    ${RED}✗ VIOLATION: memory missing per_node_total_rss_mib${NC}"
            violations=$((violations + 1))
            has_violations=1
        fi

        if ! jq -e '.memory.workload_id' "${plan_file}" &> /dev/null; then
            echo -e "    ${RED}✗ VIOLATION: memory missing workload_id${NC}"
            violations=$((violations + 1))
            has_violations=1
        fi
    fi

    if [ $has_violations -eq 0 ]; then
        echo "    ${GREEN}✓ All required metrology fields present${NC}"
    fi
}

# Validate a single plan file
validate_plan() {
    local plan_file="$1"
    local basename="$(basename "${plan_file}")"

    echo "───────────────────────────────────────────────────────────────"
    echo "Validating: ${basename}"
    echo "───────────────────────────────────────────────────────────────"

    # Check JSON syntax
    if ! jq empty "${plan_file}" 2>/dev/null; then
        echo -e "${RED}✗ INVALID JSON${NC}"
        violations=$((violations + 1))
        return 1
    fi

    validate_prohibited_patterns "${plan_file}"
    validate_workload_refs "${plan_file}"
    validate_metrology_fields "${plan_file}"

    echo ""
}

# Main validation loop
main() {
    echo "Validating plan files in: ${PLANS_DIR}"
    echo ""

    for plan in "${PLANS_DIR}"/*.plan.json; do
        if [ -f "${plan}" ]; then
            validate_plan "${plan}"
        fi
    done

    echo "═══════════════════════════════════════════════════════════════"
    echo "  VALIDATION SUMMARY"
    echo "═══════════════════════════════════════════════════════════════"
    echo ""

    if [ ${violations} -eq 0 ]; then
        echo -e "${GREEN}✓✓✓ ALL VALIDATIONS PASSED ✓✓✓${NC}"
        echo ""
        echo "Plans are compliant with metrology v1.5.0"
        echo "Total violations: 0"
        exit 0
    else
        echo -e "${RED}✗✗✗ VALIDATION FAILED ✗✗✗${NC}"
        echo ""
        echo "Total violations: ${violations}"
        echo ""
        echo "Please fix violations and re-run validation."
        echo "See: docs/metrology/METRICS_GLOSSARY.md"
        exit 1
    fi
}

main "$@"
