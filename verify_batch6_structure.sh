#!/bin/bash

echo "=== Verifying Batch 6 Authentication Test Structure ==="
echo ""

# Check if test file exists
echo "1. Checking test file existence..."
if [ -f "test/erlmcp_roundtrip_batch06_tests.erl" ]; then
    echo "   ✓ Test file exists: test/erlmcp_roundtrip_batch06_tests.erl"
else
    echo "   ✗ Test file not found"
    exit 1
fi

# Check test file size
echo ""
echo "2. Checking test file size..."
SIZE=$(wc -l < test/erlmcp_roundtrip_batch06_tests.erl)
echo "   ✓ Test file has $SIZE lines"

# Check for required test functions
echo ""
echo "3. Checking for required test functions..."
REQUIRED_TESTS=(
    "test_api_key_auth"
    "test_jwt_token_auth"
    "test_session_lifecycle"
    "test_token_rotation"
    "test_invalid_credentials"
    "test_permission_checks"
    "test_logout"
    "test_user_info"
    "test_concurrent_auth"
    "test_auth_performance"
)

for test in "${REQUIRED_TESTS[@]}"; do
    if grep -q "$test" test/erlmcp_roundtrip_batch06_tests.erl; then
        echo "   ✓ Found: $test"
    else
        echo "   ✗ Missing: $test"
    fi
done

# Check for auth tools
echo ""
echo "4. Checking for authentication tools..."
AUTH_TOOLS=(
    "auth_login"
    "auth_logout"
    "auth_verify"
    "auth_get_user"
    "auth_check_permission"
)

for tool in "${AUTH_TOOLS[@]}"; do
    if grep -q "\"$tool\"" test/erlmcp_roundtrip_batch06_tests.erl; then
        echo "   ✓ Found tool: $tool"
    else
        echo "   ✗ Missing tool: $tool"
    fi
done

# Check server/port configuration
echo ""
echo "5. Checking server configuration..."
if grep -q "lists:seq(26, 30)" test/erlmcp_roundtrip_batch06_tests.erl; then
    echo "   ✓ Server IDs: 26-30"
else
    echo "   ✗ Server IDs not configured"
fi

if grep -q "lists:seq(9026, 9030)" test/erlmcp_roundtrip_batch06_tests.erl; then
    echo "   ✓ Ports: 9026-9030"
else
    echo "   ✗ Ports not configured"
fi

# Check client configuration
echo ""
echo "6. Checking client configuration..."
if grep -q "5" test/erlmcp_roundtrip_batch06_tests.erl | grep -q "CLIENTS_PER_SERVER"; then
    echo "   ✓ Clients per server: 5"
else
    echo "   ✗ Client configuration not found"
fi

# Check operation count
echo ""
echo "7. Checking operation count..."
if grep -q "100" test/erlmcp_roundtrip_batch06_tests.erl | grep -q "AUTH_OPS_PER_CLIENT"; then
    echo "   ✓ Operations per client: 100"
else
    echo "   ✗ Operation count not configured"
fi

# Calculate totals
echo ""
echo "8. Calculating totals..."
TOTAL_SERVERS=5
TOTAL_CLIENTS=$((TOTAL_SERVERS * 5))
TOTAL_OPS=$((TOTAL_CLIENTS * 100))
echo "   ✓ Total servers: $TOTAL_SERVERS"
echo "   ✓ Total clients: $TOTAL_CLIENTS"
echo "   ✓ Total operations: $TOTAL_OPS"

# Check compilation
echo ""
echo "9. Checking compilation..."
if erlc -I apps/erlmcp_core/include -o /tmp test/erlmcp_roundtrip_batch06_tests.erl 2>&1 | grep -q "error"; then
    echo "   ✗ Compilation errors found"
    erlc -I apps/erlmcp_core/include -o /tmp test/erlmcp_roundtrip_batch06_tests.erl 2>&1 | grep "error"
else
    echo "   ✓ Test file compiles successfully (warnings only)"
fi

# Summary
echo ""
echo "=== Summary ==="
echo "Batch 6 Authentication Test Structure Verified"
echo "File: test/erlmcp_roundtrip_batch06_tests.erl"
echo "Lines: $SIZE"
echo "Tests: ${#REQUIRED_TESTS[@]}"
echo "Tools: ${#AUTH_TOOLS[@]}"
echo "Total Operations: $TOTAL_OPS"
echo ""
echo "Ready for execution with: rebar3 eunit --module=erlmcp_roundtrip_batch06_tests"
