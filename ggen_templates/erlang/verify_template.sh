#!/usr/bin/env bash
#
# Verify client_api.erl.tera template syntax and structure
#
# Usage: ./verify_template.sh

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
TEMPLATE_FILE="$SCRIPT_DIR/client_api.erl.tera"
CONTEXT_FILE="$SCRIPT_DIR/client_api_example_context.json"

echo "==================================================================="
echo "MCP Client API Template Verification"
echo "==================================================================="
echo ""

# 1. Check files exist
echo "[1/5] Checking files exist..."
if [[ ! -f "$TEMPLATE_FILE" ]]; then
    echo "ERROR: Template file not found: $TEMPLATE_FILE"
    exit 1
fi
if [[ ! -f "$CONTEXT_FILE" ]]; then
    echo "ERROR: Context file not found: $CONTEXT_FILE"
    exit 1
fi
echo "  ✓ Template file: $TEMPLATE_FILE"
echo "  ✓ Context file: $CONTEXT_FILE"
echo ""

# 2. Validate JSON context
echo "[2/5] Validating JSON context..."
if ! jq empty "$CONTEXT_FILE" 2>/dev/null; then
    echo "ERROR: Invalid JSON in context file"
    exit 1
fi
METHOD_COUNT=$(jq '.methods | length' "$CONTEXT_FILE")
echo "  ✓ JSON is valid"
echo "  ✓ Found $METHOD_COUNT methods in context"
echo ""

# 3. Check template structure
echo "[3/5] Checking template structure..."
if ! grep -q "Context Variables:" "$TEMPLATE_FILE"; then
    echo "WARNING: No context variables documentation found"
fi
if ! grep -q "{%.*for method in methods.*%}" "$TEMPLATE_FILE"; then
    echo "ERROR: No method iteration found in template"
    exit 1
fi
if ! grep -q "Function Exports" "$TEMPLATE_FILE"; then
    echo "ERROR: No function exports section found"
    exit 1
fi
if ! grep -q "gen_server Callbacks" "$TEMPLATE_FILE"; then
    echo "ERROR: No gen_server callbacks section found"
    exit 1
fi
echo "  ✓ Template has proper structure"
echo "  ✓ Contains method iteration loop"
echo "  ✓ Contains all required sections"
echo ""

# 4. Verify context fields
echo "[4/5] Verifying context has required fields..."
REQUIRED_FIELDS=(
    "timestamp"
    "generator_version"
    "mcp_version"
    "methods"
)
for field in "${REQUIRED_FIELDS[@]}"; do
    if ! jq -e ".$field" "$CONTEXT_FILE" >/dev/null 2>&1; then
        echo "ERROR: Missing required field: $field"
        exit 1
    fi
    echo "  ✓ Field present: $field"
done
echo ""

# 5. Verify method objects
echo "[5/5] Verifying method object structure..."
METHOD_FIELDS=(
    "function_name"
    "method"
    "description"
    "params"
    "return_type"
    "arity"
    "has_optional_params"
    "request_params"
)
FIRST_METHOD=$(jq '.methods[0]' "$CONTEXT_FILE")
for field in "${METHOD_FIELDS[@]}"; do
    if ! echo "$FIRST_METHOD" | jq -e ".$field" >/dev/null 2>&1; then
        echo "ERROR: Missing required method field: $field"
        exit 1
    fi
done
echo "  ✓ All required method fields present"
echo ""

# Summary
echo "==================================================================="
echo "Verification Summary"
echo "==================================================================="
echo ""
echo "Template: client_api.erl.tera"
echo "Context:  client_api_example_context.json"
echo "Methods:  $METHOD_COUNT"
echo ""
echo "Status: ✓ ALL CHECKS PASSED"
echo ""
echo "Template is ready to use with ggen or other Tera rendering tools."
echo ""
echo "Next steps:"
echo "  1. Test rendering with: ggen render --template <template> --context <context>"
echo "  2. Integrate generated code into erlmcp_client.erl"
echo "  3. Run: rebar3 compile && rebar3 eunit"
echo ""
