#!/bin/bash
# ============================================================================
# Marketplace Schema Validation Script
# Validates application.yaml, schema.yaml, and parameters.yaml
# Checks for required fields, type consistency, and completeness
# ============================================================================

set -euo pipefail

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

log_info() { echo -e "${GREEN}[INFO]${NC} $1"; }
log_warn() { echo -e "${YELLOW}[WARN]${NC} $1"; }
log_error() { echo -e "${RED}[ERROR]${NC} $1"; }
log_test() { echo -e "${BLUE}[TEST]${NC} $1"; }

# ============================================================================
# Configuration
# ============================================================================

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
MARKETPLACE_DIR="$(dirname "$SCRIPT_DIR")"
SCHEMA_DIR="${MARKETPLACE_DIR}/marketplace-schema"
EVIDENCE_DIR="${MARKETPLACE_DIR}/test-evidence/schema"

# Schema files
APPLICATION_YAML="${SCHEMA_DIR}/application.yaml"
SCHEMA_YAML="${SCHEMA_DIR}/schema.yaml"
PARAMETERS_YAML="${SCHEMA_DIR}/parameters.yaml"

# Counters
TOTAL_CHECKS=0
PASSED_CHECKS=0
FAILED_CHECKS=0
WARNINGS=0

# ============================================================================
# Validation Functions
# ============================================================================

check_yaml_syntax() {
    log_test "Checking YAML syntax..."

    local all_valid=true

    for yaml_file in "$APPLICATION_YAML" "$SCHEMA_YAML" "$PARAMETERS_YAML"; do
        if [ ! -f "$yaml_file" ]; then
            log_error "  ✗ File not found: $yaml_file"
            all_valid=false
            FAILED_CHECKS=$((FAILED_CHECKS + 1))
            continue
        fi

        # Check if python/yq is available for validation
        if command -v python3 &> /dev/null; then
            if python3 -c "import yaml; yaml.safe_load(open('$yaml_file'))" 2>/dev/null; then
                log_info "  ✓ Valid YAML: $(basename "$yaml_file")"
                PASSED_CHECKS=$((PASSED_CHECKS + 1))
            else
                log_error "  ✗ Invalid YAML: $(basename "$yaml_file")"
                all_valid=false
                FAILED_CHECKS=$((FAILED_CHECKS + 1))
            fi
        elif command -v yq &> /dev/null; then
            if yq eval '.' "$yaml_file" > /dev/null 2>&1; then
                log_info "  ✓ Valid YAML: $(basename "$yaml_file")"
                PASSED_CHECKS=$((PASSED_CHECKS + 1))
            else
                log_error "  ✗ Invalid YAML: $(basename "$yaml_file")"
                all_valid=false
                FAILED_CHECKS=$((FAILED_CHECKS + 1))
            fi
        else
            log_warn "  ⚠ No YAML validator available (install python3 or yq)"
            break
        fi

        TOTAL_CHECKS=$((TOTAL_CHECKS + 1))
    done

    $all_valid
}

check_application_yaml() {
    log_test "Validating application.yaml required fields..."

    if [ ! -f "$APPLICATION_YAML" ]; then
        log_error "  ✗ application.yaml not found"
        FAILED_CHECKS=$((FAILED_CHECKS + 1))
        TOTAL_CHECKS=$((TOTAL_CHECKS + 1))
        return 1
    fi

    local required_fields=(
        "properties"
        "properties.deployment_type"
        "properties.region"
        "properties.instance_tier"
        "required"
        "output_schema"
    )

    local all_present=true

    for field in "${required_fields[@]}"; do
        if grep -q "$field:" "$APPLICATION_YAML"; then
            log_info "  ✓ Field present: $field"
            PASSED_CHECKS=$((PASSED_CHECKS + 1))
        else
            log_error "  ✗ Missing required field: $field"
            all_present=false
            FAILED_CHECKS=$((FAILED_CHECKS + 1))
        fi
        TOTAL_CHECKS=$((TOTAL_CHECKS + 1))
    done

    $all_present
}

check_deployment_types() {
    log_test "Checking deployment type definitions..."

    if ! grep -q "deployment_type:" "$APPLICATION_YAML"; then
        log_error "  ✗ deployment_type not defined"
        FAILED_CHECKS=$((FAILED_CHECKS + 1))
        TOTAL_CHECKS=$((TOTAL_CHECKS + 1))
        return 1
    fi

    # Check for expected deployment types
    local expected_types=("gke" "cloud-run" "compute-engine")
    local all_found=true

    for type in "${expected_types[@]}"; do
        if grep -qi "$type" "$APPLICATION_YAML"; then
            log_info "  ✓ Deployment type defined: $type"
            PASSED_CHECKS=$((PASSED_CHECKS + 1))
        else
            log_warn "  ⚠ Deployment type not found: $type"
            WARNINGS=$((WARNINGS + 1))
            all_found=false
        fi
        TOTAL_CHECKS=$((TOTAL_CHECKS + 1))
    done

    $all_found || true  # Don't fail for optional deployment types
    return 0
}

check_schema_yaml() {
    log_test "Validating schema.yaml structure..."

    if [ ! -f "$SCHEMA_YAML" ]; then
        log_error "  ✗ schema.yaml not found"
        FAILED_CHECKS=$((FAILED_CHECKS + 1))
        TOTAL_CHECKS=$((TOTAL_CHECKS + 1))
        return 1
    fi

    # Check for required sections
    local required_sections=("title" "description" "properties")
    local all_present=true

    for section in "${required_sections[@]}"; do
        if grep -q "^$section:" "$SCHEMA_YAML"; then
            log_info "  ✓ Section present: $section"
            PASSED_CHECKS=$((PASSED_CHECKS + 1))
        else
            log_error "  ✗ Missing section: $section"
            all_present=false
            FAILED_CHECKS=$((FAILED_CHECKS + 1))
        fi
        TOTAL_CHECKS=$((TOTAL_CHECKS + 1))
    done

    $all_present
}

check_parameter_types() {
    log_test "Checking parameter type definitions..."

    if [ ! -f "$SCHEMA_YAML" ]; then
        return 1
    fi

    # Extract all property names and their types
    local properties=$(grep -A 2 "properties:" "$SCHEMA_YAML" | grep -E "^\s+[a-z_]+:" | sed 's/://g' | sed 's/^\s*//')

    local valid=true

    while read -r prop; do
        if [ -z "$prop" ]; then
            continue
        fi

        # Check if property has a type definition
        if grep -A 5 "^$prop:" "$SCHEMA_YAML" | grep -q "type:"; then
            PROP_TYPE=$(grep -A 5 "^$prop:" "$SCHEMA_YAML" | grep "type:" | awk '{print $2}')
            log_info "  ✓ $prop: $PROP_TYPE"
            PASSED_CHECKS=$((PASSED_CHECKS + 1))
        else
            log_warn "  ⚠ No type defined for: $prop"
            WARNINGS=$((WARNINGS + 1))
        fi
        TOTAL_CHECKS=$((TOTAL_CHECKS + 1))
    done <<< "$properties"

    $valid || true
}

check_parameters_yaml() {
    log_test "Validating parameters.yaml..."

    if [ ! -f "$PARAMETERS_YAML" ]; then
        log_error "  ✗ parameters.yaml not found"
        FAILED_CHECKS=$((FAILED_CHECKS + 1))
        TOTAL_CHECKS=$((TOTAL_CHECKS + 1))
        return 1
    fi

    # Check for required sections
    if grep -q "parameters:" "$PARAMETERS_YAML" && grep -q "outputs:" "$PARAMETERS_YAML"; then
        log_info "  ✓ parameters.yaml has required sections"
        PASSED_CHECKS=$((PASSED_CHECKS + 1))
        TOTAL_CHECKS=$((TOTAL_CHECKS + 1))
        return 0
    else
        log_error "  ✗ parameters.yaml missing required sections"
        FAILED_CHECKS=$((FAILED_CHECKS + 1))
        TOTAL_CHECKS=$((TOTAL_CHECKS + 1))
        return 1
    fi
}

check_schema_to_terraform_mapping() {
    log_test "Checking schema to Terraform variable mapping..."

    if [ ! -f "$PARAMETERS_YAML" ]; then
        return 1
    fi

    # Extract parameters from parameters.yaml
    local yaml_params=$(grep -E "^\s+[a-z_]+:" "$PARAMETERS_YAML" | sed 's/://g' | sed 's/^\s*//' | grep -v "^parameters" | grep -v "^outputs")

    # Find Terraform modules directory
    local tf_modules_dir="${MARKETPLACE_DIR}/terraform/modules"
    local missing_vars=()

    while read -r param; do
        if [ -z "$param" ]; then
            continue
        fi

        # Check if parameter exists in any Terraform module
        local found=false
        for tf_dir in "$tf_modules_dir"/*/; do
            if [ -d "$tf_dir" ] && [ -f "${tf_dir}variables.tf" ]; then
                if grep -q "variable \"$param\"" "${tf_dir}variables.tf" 2>/dev/null; then
                    found=true
                    break
                fi
            fi
        done

        if $found; then
            log_info "  ✓ $param mapped to Terraform variable"
            PASSED_CHECKS=$((PASSED_CHECKS + 1))
        else
            log_warn "  ⚠ No Terraform variable found for: $param"
            WARNINGS=$((WARNINGS + 1))
            missing_vars+=("$param")
        fi
        TOTAL_CHECKS=$((TOTAL_CHECKS + 1))
    done <<< "$yaml_params"

    if [ ${#missing_vars[@]} -eq 0 ]; then
        return 0
    else
        log_warn "  Some parameters may not have Terraform variables (may be computed)"
        return 0
    fi
}

check_default_values() {
    log_test "Checking for default values on non-required parameters..."

    if [ ! -f "$SCHEMA_YAML" ]; then
        return 1
    fi

    # Find parameters without default values
    local missing_defaults=0

    # Extract all properties
    local properties=$(grep -E "^\s+[a-z_]+:" "$SCHEMA_YAML" | sed 's/://g' | sed 's/^\s*//' | grep -v "^properties" | grep -v "^required")

    while read -r prop; do
        if [ -z "$prop" ]; then
            continue
        fi

        # Check if property is in required array
        if grep -A 100 "^required:" "$SCHEMA_YAML" | grep -q "\"$prop\""; then
            continue  # Required parameters don't need defaults
        fi

        # Check if property has a default
        if grep -A 20 "^$prop:" "$SCHEMA_YAML" | grep -q "default:"; then
            log_info "  ✓ $prop has default value"
        else
            log_warn "  ⚠ $prop missing default value (non-required)"
            WARNINGS=$((WARNINGS + 1))
            missing_defaults=$((missing_defaults + 1))
        fi
        TOTAL_CHECKS=$((TOTAL_CHECKS + 1))
    done <<< "$properties"

    if [ $missing_defaults -eq 0 ]; then
        return 0
    else
        log_warn "  Some non-required parameters lack defaults"
        return 0  # Don't fail for this
    fi
}

check_descriptions() {
    log_test "Checking for parameter descriptions..."

    if [ ! -f "$SCHEMA_YAML" ]; then
        return 1
    fi

    local missing_descriptions=0

    # Extract all properties
    local properties=$(grep -E "^\s+[a-z_]+:" "$SCHEMA_YAML" | sed 's/://g' | sed 's/^\s*//' | grep -v "^properties" | grep -v "^required" | grep -v "^title" | grep -v "^description")

    while read -r prop; do
        if [ -z "$prop" ]; then
            continue
        fi

        # Check if property has a description
        if grep -A 20 "^$prop:" "$SCHEMA_YAML" | grep -q "description:"; then
            log_info "  ✓ $prop has description"
            PASSED_CHECKS=$((PASSED_CHECKS + 1))
        else
            log_warn "  ⚠ $prop missing description"
            WARNINGS=$((WARNINGS + 1))
            missing_descriptions=$((missing_descriptions + 1))
        fi
        TOTAL_CHECKS=$((TOTAL_CHECKS + 1))
    done <<< "$properties"

    if [ $missing_descriptions -eq 0 ]; then
        return 0
    else
        log_warn "  Some parameters lack descriptions"
        return 0  # Don't fail for this
    fi
}

check_output_schema() {
    log_test "Checking output schema definitions..."

    if [ ! -f "$APPLICATION_YAML" ]; then
        return 1
    fi

    # Expected outputs
    local expected_outputs=(
        "service_url"
        "cluster_name"
        "health_check_url"
    )

    local all_found=true

    for output in "${expected_outputs[@]}"; do
        if grep -q "$output" "$APPLICATION_YAML"; then
            log_info "  ✓ Output defined: $output"
            PASSED_CHECKS=$((PASSED_CHECKS + 1))
        else
            log_warn "  ⚠ Output not found: $output"
            WARNINGS=$((WARNINGS + 1))
            all_found=false
        fi
        TOTAL_CHECKS=$((TOTAL_CHECKS + 1))
    done

    $all_found || true
}

generate_validation_report() {
    log_info "Generating validation report..."

    mkdir -p "$EVIDENCE_DIR"

    cat > "$EVIDENCE_DIR/schema-validation-report.md" <<EOF
# Marketplace Schema Validation Report

**Date:** $(date -u +"%Y-%m-%dT%H:%M:%SZ")
**Schema Directory:** $SCHEMA_DIR

## Validation Summary

| Metric | Count |
|--------|-------|
| Total Checks | $TOTAL_CHECKS |
| Passed | $PASSED_CHECKS |
| Failed | $FAILED_CHECKS |
| Warnings | $WARNINGS |

## Files Validated

- \`application.yaml\` - $( [ -f "$APPLICATION_YAML" ] && echo "✓ Present" || echo "✗ Missing" )
- \`schema.yaml\` - $( [ -f "$SCHEMA_YAML" ] && echo "✓ Present" || echo "✗ Missing" )
- \`parameters.yaml\` - $( [ -f "$PARAMETERS_YAML" ] && echo "✓ Present" || echo "✗ Missing" )

## Validation Results

### YAML Syntax
$([ -f "$EVIDENCE_DIR/yaml-syntax.log" ] && cat "$EVIDENCE_DIR/yaml-syntax.log" || echo "Not tested")

### Required Fields
$( [ $FAILED_CHECKS -eq 0 ] && echo "✓ All required fields present" || echo "✗ Some required fields missing" )

### Parameter Types
$( [ $WARNINGS -eq 0 ] && echo "✓ All parameters have types" || echo "⚠ Some parameters missing types" )

### Default Values
$( [ $WARNINGS -eq 0 ] && echo "✓ Non-required parameters have defaults" || echo "⚠ Some non-required parameters lack defaults" )

### Descriptions
$( [ $WARNINGS -eq 0 ] && echo "✓ All parameters have descriptions" || echo "⚠ Some parameters lack descriptions" )

### Terraform Mapping
$( [ $WARNINGS -eq 0 ] && echo "✓ All schema parameters map to Terraform" || echo "⚠ Some parameters don't map to Terraform variables" )

### Output Schema
$( [ $WARNINGS -eq 0 ] && echo "✓ Expected outputs defined" || echo "⚠ Some expected outputs missing" )

## Recommendations

$(
if [ $FAILED_CHECKS -gt 0 ]; then
    echo "- Fix all failed checks before submitting to Marketplace"
fi
if [ $WARNINGS -gt 0 ]; then
    echo "- Review warnings and add missing information for better UX"
fi
echo "- Run \`yamllint\` for additional style checking"
echo "- Test schema with \`gcloud marketplace solutions schema validate\`"
)

## Next Steps

1. Fix any failed checks
2. Review and address warnings
3. Test deployment with sample values
4. Submit to Marketplace for review

EOF

    log_info "  ✓ Report saved to: $EVIDENCE_DIR/schema-validation-report.md"

    # Print summary
    cat "$EVIDENCE_DIR/schema-validation-report.md"
}

# ============================================================================
# Main Validation Runner
# ============================================================================

main() {
    log_info "Starting Marketplace Schema Validation..."
    log_info "================================================"
    log_info "Schema Directory: $SCHEMA_DIR"
    log_info "================================================"

    # Create evidence directory
    mkdir -p "$EVIDENCE_DIR"

    # Run validation checks
    check_yaml_syntax
    check_application_yaml
    check_deployment_types
    check_schema_yaml
    check_parameter_types
    check_parameters_yaml
    check_schema_to_terraform_mapping
    check_default_values
    check_descriptions
    check_output_schema

    # Generate report
    generate_validation_report

    # Summary
    log_info "================================================"
    log_info "Validation Summary:"
    log_info "  Total Checks:  $TOTAL_CHECKS"
    log_info "  Passed:       $PASSED_CHECKS"
    log_info "  Failed:       $FAILED_CHECKS"
    log_info "  Warnings:     $WARNINGS"
    log_info "================================================"

    if [ $FAILED_CHECKS -gt 0 ]; then
        log_error "Validation failed with $FAILED_CHECKS errors"
        exit 1
    fi

    if [ $WARNINGS -gt 0 ]; then
        log_warn "Validation passed with $WARNINGS warnings"
        exit 0
    fi

    log_info "All schema validations passed!"
    exit 0
}

# ============================================================================
# Script Entry Point
# ============================================================================

main "$@"
