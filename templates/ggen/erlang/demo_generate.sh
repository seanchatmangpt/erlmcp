#!/usr/bin/env bash
#
# Demonstration script for validator template generation
#
# This script shows how to use the validator.erl.tera template
# to generate validation modules for different MCP components.
#
# Usage: ./demo_generate.sh [component_type]
#   component_type: tool, resource, prompt, task (default: tool)

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../../.." && pwd)"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

log_info() {
    echo -e "${BLUE}[INFO]${NC} $*"
}

log_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $*"
}

log_warning() {
    echo -e "${YELLOW}[WARNING]${NC} $*"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $*"
}

# Component type (default: tool)
COMPONENT_TYPE="${1:-tool}"

log_info "Validator Template Generation Demo"
log_info "Component Type: $COMPONENT_TYPE"
log_info "Project Root: $PROJECT_ROOT"
echo

# Check if tera CLI is available
if ! command -v tera &> /dev/null; then
    log_warning "tera CLI not found. This is a demonstration of template structure."
    log_info "To actually generate files, install tera: cargo install tera-cli"
    echo
fi

# Template configuration based on component type
case "$COMPONENT_TYPE" in
    tool)
        MODULE_NAME="erlmcp_tool_argument_validator_demo"
        OUTPUT_FILE="$PROJECT_ROOT/apps/erlmcp_core/src/${MODULE_NAME}.erl"
        VALIDATION_TYPE="argument"
        DESCRIPTION_MAX_LENGTH=10000
        URI_VALIDATION_ENABLED="false"
        PATH_TRAVERSAL_CHECK="false"
        ERROR_CODE_PREFIX="MCP_ERROR_TOOL_"
        log_info "Generating Tool Argument Validator (Critical Gap Fix)"
        ;;

    resource)
        MODULE_NAME="erlmcp_resource_uri_validator_demo"
        OUTPUT_FILE="$PROJECT_ROOT/apps/erlmcp_core/src/${MODULE_NAME}.erl"
        VALIDATION_TYPE="semantic"
        DESCRIPTION_MAX_LENGTH=10000
        URI_VALIDATION_ENABLED="true"
        PATH_TRAVERSAL_CHECK="true"
        ERROR_CODE_PREFIX="MCP_ERROR_RESOURCE_"
        log_info "Generating Resource URI Validator (with security checks)"
        ;;

    prompt)
        MODULE_NAME="erlmcp_prompt_argument_validator_demo"
        OUTPUT_FILE="$PROJECT_ROOT/apps/erlmcp_core/src/${MODULE_NAME}.erl"
        VALIDATION_TYPE="argument"
        DESCRIPTION_MAX_LENGTH=5000
        URI_VALIDATION_ENABLED="false"
        PATH_TRAVERSAL_CHECK="false"
        ERROR_CODE_PREFIX="MCP_ERROR_PROMPT_"
        log_info "Generating Prompt Argument Validator"
        ;;

    task)
        MODULE_NAME="erlmcp_task_validator_demo"
        OUTPUT_FILE="$PROJECT_ROOT/apps/erlmcp_core/src/${MODULE_NAME}.erl"
        VALIDATION_TYPE="structure"
        DESCRIPTION_MAX_LENGTH=5000
        URI_VALIDATION_ENABLED="false"
        PATH_TRAVERSAL_CHECK="false"
        ERROR_CODE_PREFIX="MCP_ERROR_TASK_"
        log_info "Generating Task Validator (MCP 2025-11-25)"
        ;;

    *)
        log_error "Unknown component type: $COMPONENT_TYPE"
        log_info "Valid types: tool, resource, prompt, task"
        exit 1
        ;;
esac

echo
log_info "Configuration:"
log_info "  Module: $MODULE_NAME"
log_info "  Output: $OUTPUT_FILE"
log_info "  Validation Type: $VALIDATION_TYPE"
log_info "  Description Max Length: $DESCRIPTION_MAX_LENGTH"
log_info "  URI Validation: $URI_VALIDATION_ENABLED"
log_info "  Path Traversal Check: $PATH_TRAVERSAL_CHECK"
log_info "  Error Code Prefix: $ERROR_CODE_PREFIX"
echo

# Create temporary context file for tera
CONTEXT_FILE=$(mktemp)
trap 'rm -f "$CONTEXT_FILE"' EXIT

cat > "$CONTEXT_FILE" <<EOF
{
  "component": "$COMPONENT_TYPE",
  "module_name": "$MODULE_NAME",
  "validation_type": "$VALIDATION_TYPE",
  "validation_rules": [
    {
      "name": "required_fields",
      "description": "Validate required fields are present"
    },
    {
      "name": "field_types",
      "description": "Validate field types are correct"
    },
    {
      "name": "field_constraints",
      "description": "Validate field constraints are satisfied"
    }
  ],
  "schema": {},
  "description_max_length": $DESCRIPTION_MAX_LENGTH,
  "uri_validation_enabled": $URI_VALIDATION_ENABLED,
  "path_traversal_check": $PATH_TRAVERSAL_CHECK,
  "error_code_prefix": "$ERROR_CODE_PREFIX",
  "includes_schema_validator": true,
  "timestamp": "$(date -u +"%Y-%m-%dT%H:%M:%SZ")",
  "generator_version": "1.0.0"
}
EOF

log_success "Created context file: $CONTEXT_FILE"
log_info "Context:"
cat "$CONTEXT_FILE" | sed 's/^/  /'
echo

# Show template structure
TEMPLATE_FILE="$SCRIPT_DIR/validator.erl.tera"
log_info "Template file: $TEMPLATE_FILE"
log_info "Template size: $(wc -l < "$TEMPLATE_FILE") lines"
echo

# If tera is available, generate the file
if command -v tera &> /dev/null; then
    log_info "Generating validator module..."

    # Ensure output directory exists
    mkdir -p "$(dirname "$OUTPUT_FILE")"

    # Generate file using tera
    tera --template "$TEMPLATE_FILE" --context "$CONTEXT_FILE" > "$OUTPUT_FILE"

    log_success "Generated: $OUTPUT_FILE"
    log_info "Generated file size: $(wc -l < "$OUTPUT_FILE") lines"
    echo

    # Show excerpt
    log_info "Generated module excerpt (first 50 lines):"
    head -n 50 "$OUTPUT_FILE" | sed 's/^/  /'
    echo

    # Compile check
    log_info "Checking compilation..."
    cd "$PROJECT_ROOT"
    if TERM=dumb rebar3 compile 2>&1 | grep -q "error"; then
        log_error "Compilation failed. Check output above."
        exit 1
    else
        log_success "Compilation successful!"
    fi
    echo

    # Show next steps
    log_success "Validator module generated successfully!"
    echo
    log_info "Next steps:"
    log_info "  1. Review generated module: $OUTPUT_FILE"
    log_info "  2. Create unit tests: ${OUTPUT_FILE%%.erl}_tests.erl"
    log_info "  3. Add to supervision tree: apps/erlmcp_core/src/erlmcp_core_sup.erl"
    log_info "  4. Run tests: rebar3 eunit --module=${MODULE_NAME}_tests"
    log_info "  5. Check dialyzer: rebar3 dialyzer"
    log_info "  6. Verify xref: rebar3 xref"

else
    log_warning "tera CLI not available. Showing what would be generated..."
    echo
    log_info "To actually generate the file:"
    log_info "  1. Install tera: cargo install tera-cli"
    log_info "  2. Run: tera --template $TEMPLATE_FILE --context $CONTEXT_FILE > $OUTPUT_FILE"
    log_info "  3. Compile: cd $PROJECT_ROOT && rebar3 compile"
    echo
    log_info "Template preview (first 100 lines):"
    head -n 100 "$TEMPLATE_FILE" | sed 's/^/  /'
fi

echo
log_success "Demo complete!"
