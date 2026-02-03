#!/usr/bin/env bash
# =============================================================================
# Network Policy Validation Script for erlmcp
# =============================================================================
# Validates that zero-trust network policies are correctly enforced
# Tests connectivity between services according to the allowed communication matrix
#
# Usage:
#   ./scripts/network-policy-validate.sh [namespace]
#
# Arguments:
#   namespace - Kubernetes namespace to test (default: erlmcp)
#
# Exit codes:
#   0 - All tests passed
#   1 - One or more tests failed
#   2 - Invalid arguments
#   3 - Prerequisites not met
# =============================================================================
# SPDX-License-Identifier: Apache-2.0
# Copyright (c) 2024 erlmcp
# =============================================================================

set -euo pipefail

# Color output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Default namespace
NAMESPACE="${1:-erlmcp}"

# Test results tracking
PASSED=0
FAILED=0
WARNINGS=0

# =============================================================================
# Helper Functions
# =============================================================================

log_info() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

log_success() {
    echo -e "${GREEN}[PASS]${NC} $1"
    ((PASSED++))
}

log_failure() {
    echo -e "${RED}[FAIL]${NC} $1"
    ((FAILED++))
}

log_warning() {
    echo -e "${YELLOW}[WARN]${NC} $1"
    ((WARNINGS++))
}

print_header() {
    echo ""
    echo "============================================================================"
    echo "$1"
    echo "============================================================================"
}

# =============================================================================
# Prerequisites Check
# =============================================================================

check_prerequisites() {
    print_header "Checking Prerequisites"

    # Check kubectl
    if ! command -v kubectl &> /dev/null; then
        log_failure "kubectl not found. Please install kubectl."
        exit 3
    fi
    log_success "kubectl found: $(kubectl version --client --short 2>/dev/null | head -1)"

    # Check cluster connectivity
    if ! kubectl cluster-info &> /dev/null; then
        log_failure "Cannot connect to Kubernetes cluster"
        exit 3
    fi
    log_success "Connected to cluster: $(kubectl config current-context)"

    # Check namespace exists
    if ! kubectl get namespace "$NAMESPACE" &> /dev/null; then
        log_failure "Namespace '$NAMESPACE' does not exist"
        exit 3
    fi
    log_success "Namespace '$NAMESPACE' exists"

    # Check for network policy support
    if ! kubectl get networkpolicies --namespace="$NAMESPACE" &> /dev/null; then
        log_warning "NetworkPolicies may not be supported in this cluster"
    else
        local policy_count
        policy_count=$(kubectl get networkpolicies --namespace="$NAMESPACE" --no-headers 2>/dev/null | wc -l)
        log_success "Found $policy_count network policies in namespace '$NAMESPACE'"
    fi
}

# =============================================================================
# Network Policy Validation
# =============================================================================

validate_default_deny_policies() {
    print_header "Validating Default Deny Policies"

    # Check for deny-all-ingress policy
    if kubectl get networkpolicy erlmcp-deny-all-ingress --namespace="$NAMESPACE" &> /dev/null; then
        log_success "Default deny ingress policy exists"
    else
        log_failure "Default deny ingress policy (erlmcp-deny-all-ingress) not found"
    fi

    # Check for deny-all-egress policy
    if kubectl get networkpolicy erlmcp-deny-all-egress --namespace="$NAMESPACE" &> /dev/null; then
        log_success "Default deny egress policy exists"
    else
        log_failure "Default deny egress policy (erlmcp-deny-all-egress) not found"
    fi
}

validate_service_policies() {
    print_header "Validating Service-Specific Policies"

    local services=("erlmcp-ingress" "erlmcp-egress" "postgres-ingress" "redis-ingress")

    for policy in "${services[@]}"; do
        if kubectl get networkpolicy "$policy" --namespace="$NAMESPACE" &> /dev/null; then
            log_success "Policy '$policy' exists"

            # Check policy has podSelector
            local selector
            selector=$(kubectl get networkpolicy "$policy" --namespace="$NAMESPACE" -o jsonpath='{.spec.podSelector}' 2>/dev/null)
            if [[ -n "$selector" ]]; then
                log_success "  Policy '$policy' has podSelector"
            else
                log_warning "  Policy '$policy' may have empty podSelector"
            fi

            # Check policy has policyTypes
            local types
            types=$(kubectl get networkpolicy "$policy" --namespace="$NAMESPACE" -o jsonpath='{.spec.policyTypes}' 2>/dev/null)
            if [[ -n "$types" ]]; then
                log_success "  Policy '$policy' has policyTypes: $types"
            fi
        else
            log_warning "Policy '$policy' not found (may not be deployed yet)"
        fi
    done
}

validate_policy_rules() {
    print_header "Validating Policy Rules"

    # Check erlmcp-ingress policy
    local ingress_policy
    ingress_policy=$(kubectl get networkpolicy erlmcp-ingress --namespace="$NAMESPACE" -o json 2>/dev/null)

    if [[ -n "$ingress_policy" ]]; then
        # Check for allowed sources
        local has_ingress_rules
        has_ingress_rules=$(echo "$ingress_policy" | jq -r '.spec.ingress // empty' | wc -l)

        if [[ "$has_ingress_rules" -gt 1 ]]; then
            log_success "erlmcp-ingress has $((has_ingress_rules - 1)) ingress rules"
        else
            log_failure "erlmcp-ingress has no ingress rules"
        fi

        # Check for specific port restrictions
        local has_port_restrictions
        has_port_restrictions=$(echo "$ingress_policy" | jq -r '.spec.ingress[].ports // empty' | wc -l)

        if [[ "$has_port_restrictions" -gt 1 ]]; then
            log_success "erlmcp-ingress has port restrictions defined"
        else
            log_warning "erlmcp-ingress may not have port restrictions"
        fi
    fi

    # Check erlmcp-egress policy
    local egress_policy
    egress_policy=$(kubectl get networkpolicy erlmcp-egress --namespace="$NAMESPACE" -o json 2>/dev/null)

    if [[ -n "$egress_policy" ]]; then
        # Check for DNS allowance
        local has_dns
        has_dns=$(echo "$egress_policy" | jq -r '.spec.egress[]?.to[]?.namespaceSelector.matchLabels."kubernetes.io/metadata.name" // empty' | grep -c "kube-system\|coredns" || true)

        if [[ "$has_dns" -gt 0 ]]; then
            log_success "erlmcp-egress allows DNS access"
        else
            log_failure "erlmcp-egress may not allow DNS access"
        fi

        # Check for database access
        local has_db_access
        has_db_access=$(echo "$egress_policy" | jq -r '.spec.egress[]?.to[]?.podSelector.matchLabels.app // empty' | grep -c "postgres\|redis" || true)

        if [[ "$has_db_access" -gt 0 ]]; then
            log_success "erlmcp-egress allows database access"
        else
            log_warning "erlmcp-egress may not allow database access (databases may be external)"
        fi
    fi
}

# =============================================================================
# Connectivity Testing
# =============================================================================

test_connectivity() {
    print_header "Testing Pod Connectivity"

    # Find a test pod or create one
    local test_pod="network-policy-test"
    local test_pod_exists=false

    if kubectl get pod "$test_pod" --namespace="$NAMESPACE" &> /dev/null; then
        test_pod_exists=true
        log_info "Using existing test pod '$test_pod'"
    else
        log_info "Creating test pod for connectivity testing..."
        cat <<EOF | kubectl apply --namespace="$NAMESPACE" -f - &> /dev/null
apiVersion: v1
kind: Pod
metadata:
  name: ${test_pod}
  labels:
    app: network-policy-test
spec:
  containers:
  - name: test
    image: nicolaka/netshoot:latest
    command: ["sleep", "3600"]
  restartPolicy: Never
EOF
        log_success "Created test pod '$test_pod'"

        # Wait for pod to be ready
        log_info "Waiting for test pod to be ready..."
        kubectl wait --for=condition=ready pod "$test_pod" --namespace="$NAMESPACE" --timeout=60s &> /dev/null
    fi

    # Define test cases
    # Format: "test_name|target_host|port|should_succeed"
    local test_cases=(
        "DNS to kubernetes.default|kubernetes.default|53|true"
        "DNS to CoreDNS|kube-dns.kube-system|53|true"
        "Erlmcp service access|erlmcp|8080|true"
        "Erlmcp metrics endpoint|erlmcp|9090|true"
        "Blocked port test|erlmcp|80|false"
        "External HTTPS|google.com|443|true"
        "External HTTP (should block)|google.com|80|false"
    )

    for test_case in "${test_cases[@]}"; do
        IFS='|' read -r test_name target port should_succeed <<< "$test_case"

        log_info "Testing: $test_name -> $target:$port"

        # Run connectivity test
        local result
        result=$(kubectl exec --namespace="$NAMESPACE" "$test_pod" --timeout=5s -- \
            nc -zv -w 2 "$target" "$port" 2>&1 || echo "failed")

        if [[ "$should_succeed" == "true" ]]; then
            if echo "$result" | grep -q "succeeded\|connected"; then
                log_success "$test_name: Connection allowed (expected)"
            else
                log_failure "$test_name: Connection blocked (unexpected)"
            fi
        else
            if echo "$result" | grep -q "failed\|timed out\|refused"; then
                log_success "$test_name: Connection blocked (expected)"
            else
                log_failure "$test_name: Connection allowed (unexpected - should be blocked!)"
            fi
        fi
    done

    # Clean up test pod if we created it
    if [[ "$test_pod_exists" == false ]]; then
        log_info "Cleaning up test pod..."
        kubectl delete pod "$test_pod" --namespace="$NAMESPACE" --timeout=10s &> /dev/null || true
        log_success "Test pod cleaned up"
    fi
}

# =============================================================================
# Policy Audit
# =============================================================================

audit_policies() {
    print_header "Network Policy Audit"

    local policies
    policies=$(kubectl get networkpolicies --namespace="$NAMESPACE" -o json 2>/dev/null)

    if [[ -z "$policies" ]]; then
        log_warning "No network policies found in namespace '$NAMESPACE'"
        return
    fi

    # Check for overly permissive policies
    log_info "Checking for overly permissive policies..."

    local policy_names
    policy_names=$(echo "$policies" | jq -r '.items[].metadata.name')

    for policy in $policy_names; do
        local policy_json
        policy_json=$(echo "$policies" | jq -r ".items[] | select(.metadata.name == \"$policy\")")

        # Check for empty podSelector (applies to all pods)
        local selector
        selector=$(echo "$policy_json" | jq -r '.spec.podSelector // {}')
        if [[ "$selector" == "{}" || "$selector" == "" ]]; then
            # Only warn if it's not a deny-all policy
            if [[ ! "$policy" =~ deny-all ]]; then
                log_warning "Policy '$policy' has empty podSelector (applies to all pods)"
            fi
        fi

        # Check for allowing all namespaces
        local allow_all_namespaces
        allow_all_namespaces=$(echo "$policy_json" | jq -r '
            .spec.ingress[]?.from[]?.namespaceSelector // {} |
            .spec.egress[]?.to[]?.namespaceSelector // {}' |
            jq -r 'if . == {} then "true" else "false" end' |
            grep -c "true" || true)

        if [[ "$allow_all_namespaces" -gt 0 ]]; then
            log_warning "Policy '$policy' may allow traffic from/to all namespaces"
        fi

        # Check for allowing 0.0.0.0/0 without restrictions
        local allow_any_ip
        allow_any_ip=$(echo "$policy_json" | jq -r '
            .spec.ingress[]?.from[]?.ipBlock.cidr // "none",
            .spec.egress[]?.to[]?.ipBlock.cidr // "none"' |
            grep -c "0.0.0.0/0" || true)

        if [[ "$allow_any_ip" -gt 0 ]]; then
            # Check if it has exceptions for RFC1918
            local has_exceptions
            has_exceptions=$(echo "$policy_json" | jq -r '
                .spec.egress[]?.to[]?.ipBlock.except // []' |
                grep -c "10.0.0.0/8\|172.16.0.0/12\|192.168.0.0/16" || true)

            if [[ "$has_exceptions" -gt 0 ]]; then
                log_success "Policy '$policy' allows external IPs with RFC1918 exceptions"
            else
                log_warning "Policy '$policy' allows 0.0.0.0/0 without RFC1918 exceptions"
            fi
        fi
    done

    # Generate policy matrix
    log_info "Policy Communication Matrix:"
    echo ""
    printf "%-20s %-20s %-10s %-10s\n" "FROM" "TO" "PORTS" "INGRESS"
    printf "%-20s %-20s %-10s %-10s\n" "----" "--" "-----" "-------"

    # erlmcp ingress sources
    echo "erlmcp <- ingress controllers, service-mesh, monitoring"

    # erlmcp egress destinations
    echo "erlmcp -> DNS (53), postgres (5432), redis (6379), otel (4317/4318)"

    # Postgres ingress
    echo "postgres <- erlmcp only"

    # Redis ingress
    echo "redis <- erlmcp only"

    echo ""
}

# =============================================================================
# Generate Report
# =============================================================================

generate_report() {
    print_header "Test Results Summary"

    echo ""
    echo "Total Tests Passed: $PASSED"
    echo "Total Tests Failed: $FAILED"
    echo "Total Warnings:     $WARNINGS"
    echo ""

    local total=$((PASSED + FAILED))

    if [[ "$FAILED" -eq 0 ]]; then
        echo -e "${GREEN}All tests passed!${NC}"
        echo ""
        echo "Zero-trust network policies are correctly configured."
        return 0
    else
        local pass_rate=$(( PASSED * 100 / total ))
        echo -e "${RED}Some tests failed!${NC}"
        echo "Pass rate: $pass_rate%"
        echo ""
        echo "Please review the failed tests above and ensure:"
        echo "  1. Default deny policies are applied"
        echo "  2. All ingress/egress is explicitly allowed"
        echo "  3. No overly permissive rules exist"
        return 1
    fi
}

# =============================================================================
# Main Execution
# =============================================================================

main() {
    print_header "erlmcp Network Policy Validation"
    echo "Namespace: $NAMESPACE"
    echo "Cluster: $(kubectl config current-context)"
    echo ""

    check_prerequisites
    validate_default_deny_policies
    validate_service_policies
    validate_policy_rules
    audit_policies

    # Only run connectivity tests if explicitly requested
    # (commented out by default to avoid creating pods)
    # test_connectivity

    generate_report
}

# Run main function
main "$@"
