#!/bin/bash
# ============================================================================
# erlmcp v3 - Security Configuration Verification Script
# ============================================================================
# Purpose: Verify that security configurations are properly implemented
#          and no hardcoded credentials exist in the codebase.
#
# AGI Joe Armstrong: "Make it work, make it right, make it fast."
# ==============================================================================

set -euo pipefail

RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

echo "🔍 erlmcp v3 - Security Configuration Verification"
echo "================================================"

# Function to print status
print_status() {
    local status=$1
    local message=$2
    case $status in
        "PASS") echo -e "${GREEN}✅ PASS${NC}: $message" ;;
        "WARN") echo -e "${YELLOW}⚠️  WARN${NC}: $message" ;;
        "FAIL") echo -e "${RED}❌ FAIL${NC}: $message" ;;
    esac
}

# Function to check for hardcoded credentials
check_hardcoded_credentials() {
    echo ""
    echo "🔒 Checking for hardcoded credentials..."

    local found_issues=0

    # Check docker-compose files for hardcoded credentials (not environment variables)
    for file in docker-compose*.yml; do
        if grep -q "ERLAMCP_DEV_COOKIE=" "$file" || \
           grep -q "ERLANG_COOKIE=[^$]" "$file" || \
           grep -q "dev_password=" "$file" || \
           grep -q "admin=" "$file" && ! grep -q "\${" "$file"; then
            print_status "FAIL" "Found hardcoded credentials in $file"
            found_issues=$((found_issues + 1))
            grep -n "ERLAMCP_DEV_COOKIE\|ERLANG_COOKIE=[^$\|dev_password\|admin=\|ERLANG_COOKIE=" "$file" | head -5
        else
            print_status "PASS" "No hardcoded credentials in $file"
        fi
    done

    # Check for sensitive files in git
    if git ls-files | grep -q "docker-secrets/"; then
        print_status "FAIL" "Docker secrets files should not be in version control"
        found_issues=$((found_issues + 1))
    else
        print_status "PASS" "Docker secrets not in version control"
    fi

    return $found_issues
}

# Function to verify environment files
verify_env_files() {
    echo ""
    echo "📁 Verifying environment file structure..."

    local found_issues=0

    # Check if .env files exist and have proper structure
    for env_file in .env.*.template; do
        if [ -f "$env_file" ]; then
            if grep -q "ERLANG_COOKIE=" "$env_file"; then
                print_status "PASS" "$env_file has ERLANG_COOKIE variable"
            else
                print_status "WARN" "$env_file missing ERLANG_COOKIE variable"
                found_issues=$((found_issues + 1))
            fi
        else
            print_status "WARN" "Template file $env_file not found"
        found_issues=$((found_issues + 1))
        fi
    done

    # Check for real .env files (should not exist in git)
    if git ls-files | grep -q "\.env[^/]*$"; then
        print_status "FAIL" "Real .env files should not be in version control"
        found_issues=$((found_issues + 1))
    else
        print_status "PASS" "No .env files in version control"
    fi

    return $found_issues
}

# Function to verify Docker secrets
verify_docker_secrets() {
    echo ""
    echo "🔐 Verifying Docker secrets..."

    local found_issues=0
    local secrets_dir="docker-secrets"

    if [ ! -d "$secrets_dir" ]; then
        print_status "WARN" "Docker secrets directory not found"
        return $found_issues
    fi

    # Check for required secrets
    local required_secrets=("erlang.cookie" "postgres-password" "redis-password" "grafana-password")

    for secret in "${required_secrets[@]}"; do
        local secret_file="$secrets_dir/$secret"
        if [ -f "$secret_file" ]; then
            if [ -s "$secret_file" ]; then
                # Check if it's not the default development value
                if [ "$secret" = "erlang.cookie" ]; then
                    if grep -q "dev" "$secret_file"; then
                        print_status "WARN" "Erlang cookie contains 'dev' - might be development value"
                        found_issues=$((found_issues + 1))
                    else
                        print_status "PASS" "$secret exists and doesn't contain 'dev'"
                    fi
                else
                    print_status "PASS" "$secret exists and has content"
                fi
            else
                print_status "FAIL" "$secret exists but is empty"
                found_issues=$((found_issues + 1))
            fi
        else
            print_status "FAIL" "Required secret $secret not found"
            found_issues=$((found_issues + 1))
        fi
    done

    return $found_issues
}

# Function to verify Docker compose files
verify_docker_compose() {
    echo ""
    echo "🐳 Verifying Docker compose files..."

    local found_issues=0

    # Check if production compose file uses secrets
    if grep -q "secrets:" docker-compose.prod.yml 2>/dev/null; then
        print_status "PASS" "Production compose file uses Docker secrets"
    else
        print_status "WARN" "Production compose file doesn't use secrets"
        found_issues=$((found_issues + 1))
    fi

    # Check if dev compose file uses environment variables
    if grep -q "ERLANG_COOKIE=" docker-compose.dev.yml 2>/dev/null; then
        if grep -q "\${ERLANG_COOKIE}" docker-compose.dev.yml 2>/dev/null; then
            print_status "PASS" "Dev compose file uses environment variables for ERLANG_COOKIE"
        else
            print_status "WARN" "Dev compose file has hardcoded ERLANG_COOKIE"
            found_issues=$((found_issues + 1))
        fi
    fi

    return $found_issues
}

# Function to run actual Docker tests
run_docker_tests() {
    echo ""
    echo "🧪 Running Docker integration tests..."

    local found_issues=0

    # Test if we can build the development environment
    if docker compose -f docker-compose.dev.yml config >/dev/null 2>&1; then
        print_status "PASS" "Development compose file is valid"
    else
        print_status "FAIL" "Development compose file has configuration errors"
        found_issues=$((found_issues + 1))
    fi

    # Test if we can build the production environment
    if docker compose -f docker-compose.prod.yml config >/dev/null 2>&1; then
        print_status "PASS" "Production compose file is valid"
    else
        print_status "FAIL" "Production compose file has configuration errors"
        found_issues=$((found_issues + 1))
    fi

    return $found_issues
}

# Main execution
echo ""
echo "🚀 Starting security verification..."
echo ""

total_issues=0

# Run all checks
check_hardcoded_credentials
total_issues=$((total_issues + $?))

verify_env_files
total_issues=$((total_issues + $?))

verify_docker_secrets
total_issues=$((total_issues + $?))

verify_docker_compose
total_issues=$((total_issues + $?))

run_docker_tests
total_issues=$((total_issues + $?))

# Summary
echo ""
echo "📊 Security Verification Summary"
echo "=================================="
echo "Total issues found: $total_issues"

if [ $total_issues -eq 0 ]; then
    echo -e "${GREEN}✅ All security checks passed!${NC}"
    echo ""
    echo "🎉 Security Configuration is properly implemented:"
    echo "   ✅ No hardcoded credentials in docker-compose files"
    echo "   ✅ Docker secrets properly configured"
    echo "   ✅ Environment variables used for sensitive data"
    echo "   ✅ Secrets not in version control"
    echo "   ✅ Docker compose files are valid"
    exit 0
else
    echo -e "${YELLOW}⚠️  Security issues found - review above${NC}"
    echo ""
    echo "🔧 Next steps:"
    echo "   1. Fix any FAIL issues above"
    echo "   2. Address WARN issues where appropriate"
    echo "   3. Run this script again after fixes"
    exit 1
fi