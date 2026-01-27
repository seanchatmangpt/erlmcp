#!/bin/bash

echo "=== ERLMCP Configuration Security Test Suite ==="
echo ""

PASS=0
FAIL=0

# Test 1: Check for "changeme" strings in config
echo "Test 1: Checking for hardcoded 'changeme' credentials..."
if grep -q '"changeme"' config/sys.config; then
    echo "✗ FAIL: Found hardcoded 'changeme' in config/sys.config"
    FAIL=$((FAIL+1))
else
    echo "✓ PASS: No hardcoded 'changeme' found"
    PASS=$((PASS+1))
fi

# Test 2: Check email_password uses env
echo "Test 2: Checking email_password uses environment variable..."
if grep -q '{email_password, {env, "ERLMCP_EMAIL_PASSWORD"}}' config/sys.config; then
    echo "✓ PASS: email_password uses {env, ...}"
    PASS=$((PASS+1))
else
    echo "✗ FAIL: email_password not using environment variable"
    FAIL=$((FAIL+1))
fi

# Test 3: Check pagerduty_integration_key uses env
echo "Test 3: Checking pagerduty_integration_key uses environment variable..."
if grep -q '{pagerduty_integration_key, {env, "ERLMCP_PAGERDUTY_KEY"}}' config/sys.config; then
    echo "✓ PASS: pagerduty_integration_key uses {env, ...}"
    PASS=$((PASS+1))
else
    echo "✗ FAIL: pagerduty_integration_key not using environment variable"
    FAIL=$((FAIL+1))
fi

# Test 4: Check webhook_auth_header uses env
echo "Test 4: Checking webhook_auth_header uses environment variable..."
if grep -q '{webhook_auth_header, {env, "ERLMCP_WEBHOOK_AUTH_TOKEN"}}' config/sys.config; then
    echo "✓ PASS: webhook_auth_header uses {env, ...}"
    PASS=$((PASS+1))
else
    echo "✗ FAIL: webhook_auth_header not using environment variable"
    FAIL=$((FAIL+1))
fi

# Test 5: Check datadog_api_key uses env
echo "Test 5: Checking datadog_api_key uses environment variable..."
if grep -q '{datadog_api_key, {env, "ERLMCP_DATADOG_API_KEY"}}' config/sys.config; then
    echo "✓ PASS: datadog_api_key uses {env, ...}"
    PASS=$((PASS+1))
else
    echo "✗ FAIL: datadog_api_key not using environment variable"
    FAIL=$((FAIL+1))
fi

# Test 6: Check newrelic_api_key uses env
echo "Test 6: Checking newrelic_api_key uses environment variable..."
if grep -q '{newrelic_api_key, {env, "ERLMCP_NEWRELIC_API_KEY"}}' config/sys.config; then
    echo "✓ PASS: newrelic_api_key uses {env, ...}"
    PASS=$((PASS+1))
else
    echo "✗ FAIL: newrelic_api_key not using environment variable"
    FAIL=$((FAIL+1))
fi

# Test 7: Check grafana_cloud_password uses env
echo "Test 7: Checking grafana_cloud_password uses environment variable..."
if grep -q '{grafana_cloud_password, {env, "ERLMCP_GRAFANA_PASSWORD"}}' config/sys.config; then
    echo "✓ PASS: grafana_cloud_password uses {env, ...}"
    PASS=$((PASS+1))
else
    echo "✗ FAIL: grafana_cloud_password not using environment variable"
    FAIL=$((FAIL+1))
fi

# Test 8: Check slack_webhook uses env
echo "Test 8: Checking slack_webhook uses environment variable..."
if grep -q '{slack_webhook, {env, "ERLMCP_SLACK_WEBHOOK"}}' config/sys.config; then
    echo "✓ PASS: slack_webhook uses {env, ...}"
    PASS=$((PASS+1))
else
    echo "✗ FAIL: slack_webhook not using environment variable"
    FAIL=$((FAIL+1))
fi

# Test 9: Check allowed_paths uses env
echo "Test 9: Checking allowed_paths uses environment variable..."
if grep -q '{allowed_paths, {env, "ERLMCP_ALLOWED_PATHS"' config/sys.config; then
    echo "✓ PASS: allowed_paths uses {env, ...}"
    PASS=$((PASS+1))
else
    echo "✗ FAIL: allowed_paths not using environment variable"
    FAIL=$((FAIL+1))
fi

# Test 10: Check no /Users/sac paths in config
echo "Test 10: Checking for hardcoded /Users/sac paths..."
if grep -q '/Users/sac' config/sys.config; then
    echo "✗ FAIL: Found hardcoded /Users/sac path in config"
    FAIL=$((FAIL+1))
else
    echo "✓ PASS: No /Users/sac paths found"
    PASS=$((PASS+1))
fi

# Test 11: Check environment variable naming convention
echo "Test 11: Checking environment variable naming convention (ERLMCP_ prefix)..."
ENV_VARS=$(grep -o '{env, "[^"]*"' config/sys.config | grep -o '"[^"]*"' | tr -d '"')
ALL_PREFIXED=true
for VAR in $ENV_VARS; do
    if [[ ! $VAR =~ ^ERLMCP_ ]]; then
        echo "  ✗ Found variable without ERLMCP_ prefix: $VAR"
        ALL_PREFIXED=false
    fi
done
if [ "$ALL_PREFIXED" = true ]; then
    echo "✓ PASS: All environment variables use ERLMCP_ prefix"
    PASS=$((PASS+1))
else
    echo "✗ FAIL: Some variables don't use ERLMCP_ prefix"
    FAIL=$((FAIL+1))
fi

# Test 12: Verify test suite exists
echo "Test 12: Checking erlmcp_config_security_tests.erl exists..."
if [ -f "test/erlmcp_config_security_tests.erl" ]; then
    echo "✓ PASS: erlmcp_config_security_tests.erl found"
    PASS=$((PASS+1))
else
    echo "✗ FAIL: erlmcp_config_security_tests.erl not found"
    FAIL=$((FAIL+1))
fi

echo ""
echo "=== Test Results ==="
echo "PASSED: $PASS"
echo "FAILED: $FAIL"
echo "TOTAL:  $((PASS + FAIL))"
echo ""

if [ $FAIL -eq 0 ]; then
    echo "✅ ALL SECURITY TESTS PASSED"
    exit 0
else
    echo "❌ SOME TESTS FAILED"
    exit 1
fi
