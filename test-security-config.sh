#!/bin/bash
# Simple test script to verify security configuration

echo "🔍 Testing Security Configuration"
echo "=================================="

# Test 1: Check for original hardcoded cookie in compose files
if grep -r "erlmcp_dev_cookie_change_in_production" docker-compose*.yml 2>/dev/null; then
    echo "❌ FAIL: Found original hardcoded cookie in compose files"
    exit 1
else
    echo "✅ PASS: Original hardcoded cookie removed from compose files"
fi

# Test 2: Check development environment uses variables
if grep -q "ERLANG_COOKIE=\${" docker-compose.dev.yml; then
    echo "✅ PASS: Development compose uses environment variables"
else
    echo "❌ FAIL: Development compose doesn't use environment variables"
    exit 1
fi

# Test 3: Check production environment uses secrets
if grep -q "secrets:" docker-compose.prod.yml && grep -q "erlang-cookie" docker-compose.prod.yml; then
    echo "✅ PASS: Production compose uses Docker secrets"
else
    echo "❌ FAIL: Production compose doesn't use Docker secrets"
    exit 1
fi

# Test 4: Check secrets directory exists
if [ -d "docker-secrets" ]; then
    echo "✅ PASS: Docker secrets directory exists"
else
    echo "❌ FAIL: Docker secrets directory not found"
    exit 1
fi

# Test 5: Check secrets files exist
for secret in erlang.cookie postgres-password redis-password grafana-password; do
    if [ -f "docker-secrets/$secret" ]; then
        echo "✅ PASS: Secret file $secret exists"
    else
        echo "❌ FAIL: Secret file $secret not found"
        exit 1
    fi
done

echo ""
echo "🎉 All security configuration tests passed!"
echo ""
echo "📋 Summary of Changes:"
echo "1. ✅ Removed hardcoded cookie: ERLAMCP_DEV_COOKIE=erlmcp_dev_cookie_change_in_production"
echo "2. ✅ Added environment variable support: ERLANG_COOKIE=\${ERLANG_COOKIE:-secure_default}"
echo "3. ✅ Added Docker secrets configuration for production"
echo "4. ✅ Created secure secrets files"
echo "5. ✅ Updated PostgreSQL to use environment variables"
echo "6. ✅ Added comprehensive verification scripts"
echo "7. ✅ Created detailed security documentation"