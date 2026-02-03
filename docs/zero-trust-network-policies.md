# Zero-Trust Network Policies for erlmcp

## Overview

This document describes the zero-trust network policy implementation for the erlmcp (Erlang MCP) services. The network policies follow the principle of "never trust, always verify" with explicit allow rules only.

## Security Principles

1. **Default Deny**: All traffic is denied by default unless explicitly allowed
2. **Least Privilege**: Each service can only communicate with services it explicitly needs
3. **Micro-segmentation**: Namespace and service-level isolation
4. **Explicit Allow**: All allowed communication is explicitly defined in policy

## Architecture

### Policy Structure

The network policies are organized in the following layers:

1. **Base Policies**: Default deny all ingress/egress traffic
2. **Service-Specific Policies**: Allow rules for each service
3. **Cross-Namespace Policies**: Allow rules for inter-namespace communication
4. **External Access Policies**: Allow rules for external service access

### Communication Matrix

| FROM Service | TO Service | PORTS | Purpose |
|-------------|------------|-------|---------|
| erlmcp | postgres | 5432, 5433 | Database access |
| erlmcp | redis | 6379, 6380 | Cache access |
| erlmcp | rabbitmq | 5672, 5671 | Message queue |
| erlmcp | otel-collector | 4317, 4318 | Telemetry export |
| erlmcp | DNS | 53 | Name resolution |
| erlmcp | External HTTPS | 443 | External API calls |
| erlmcp | erlmcp | 4369, 9100+ | Erlang clustering (EPMD/Dist) |
| ingress-nginx | erlmcp | 8080, 8443 | External traffic |
| istio-system | erlmcp | 8080, 8443 | Service mesh (mTLS) |
| prometheus | erlmcp | 9090 | Metrics scraping |
| postgres | DNS | 53 | Name resolution only |
| redis | DNS, Redis cluster | 53, 26379 | DNS, cluster bus |
| rabbitmq | DNS, RabbitMQ cluster | 53, 25672 | DNS, inter-node |
| otel-collector | prometheus, jaeger | 9090, 14250 | Telemetry forwarding |
| otel-collector | DNS, External HTTPS | 53, 443 | DNS, external backends |

## Policy Files

### 1. Base Policies (k8s/network-policy.yaml)

**erlmcp-deny-all-ingress**: Denies all ingress traffic by default

**erlmcp-deny-all-egress**: Denies all egress traffic by default

```yaml
apiVersion: networking.k8s.io/v1
kind: NetworkPolicy
metadata:
  name: erlmcp-deny-all-ingress
  namespace: erlmcp
spec:
  podSelector: {}
  policyTypes:
    - Ingress
```

### 2. erlmcp Core Service Policies

**erlmcp-ingress**: Allows ingress to erlmcp from approved sources

- Ingress controllers (ingress-nginx, traefik, gateway-api)
- Istio/Linkerd service mesh (with mTLS)
- Prometheus (for metrics scraping)
- Grafana (for dashboard queries)
- OpenTelemetry Collector
- Inter-pod communication within erlmcp cluster

**erlmcp-egress**: Allows egress from erlmcp to required services

- DNS (kube-system/CoreDNS, coredns)
- PostgreSQL (ports 5432, 5433)
- Redis (ports 6379, 6380)
- RabbitMQ (ports 5672, 15672)
- OpenTelemetry Collector (ports 4317, 4318)
- Jaeger (ports 6831, 14250, 14268)
- Istio/Linkerd sidecar/proxy
- External HTTPS (port 443)
- Internal IPs (10.0.0.0/8)

### 3. Database Policies

**postgres-ingress**: Restricts PostgreSQL ingress to erlmcp pods only

**postgres-egress**: Allows PostgreSQL egress to DNS only

**redis-ingress**: Restricts Redis ingress to erlmcp pods only

**redis-egress**: Allows Redis egress to DNS and cluster nodes

**rabbitmq-ingress**: Restricts RabbitMQ ingress to erlmcp pods and monitoring

**rabbitmq-egress**: Allows RabbitMQ egress to DNS and cluster nodes

### 4. Observability Policies

**otel-collector-ingress**: Allows OTel to receive telemetry from all services

**otel-collector-egress**: Allows OTel to forward telemetry to backends

## Namespace Labels

For proper policy enforcement, namespaces must have the following labels:

```yaml
metadata:
  labels:
    kubernetes.io/metadata.name: <namespace-name>
    network-policy: zero-trust
    security-level: high|medium|low
    pod-security.kubernetes.io/enforce: restricted
    pod-security.kubernetes.io/audit: restricted
    pod-security.kubernetes.io/warn: restricted
```

## Deployment

### Apply Network Policies

```bash
# Apply all network policies
kubectl apply -f k8s/network-policy.yaml

# Verify policies are applied
kubectl get networkpolicies -n erlmcp

# Describe a specific policy
kubectl describe networkpolicy erlmcp-ingress -n erlmcp
```

### Apply with Helm

```bash
helm install erlmcp ./k8s/deployments/helm/erlmcp \
  --namespace erlmcp \
  --set networking.policies.enabled=true \
  --set networking.policies.ingress.controllers={ingress-nginx,traefik}
```

## Validation

### Run Validation Script

```bash
./scripts/network-policy-validate.sh erlmcp
```

The validation script checks:
- Default deny policies exist
- Service-specific policies are properly configured
- Policy rules have proper port restrictions
- No overly permissive policies exist
- DNS access is allowed
- Database access is properly restricted

### Manual Validation

```bash
# Create a test pod
kubectl run test-pod --image=nicolaka/netshoot --rm -it --restart=Never -n erlmcp

# From inside the pod, test connectivity
nc -zv postgres 5432    # Should succeed (erlmcp -> postgres)
nc -zv redis 6379       # Should succeed (erlmcp -> redis)
nc -zv google.com 443   # Should succeed (external HTTPS)
nc -zv postgres 3306    # Should FAIL (wrong port)
nc -zv 10.0.0.1 80      # Should FAIL (internal HTTP not allowed)
```

## mTLS Enforcement

When mTLS is enabled with Istio or Linkerd, additional policies are created:

**erlmcp-mtls-only**: Enforces mTLS for all service-to-service communication

```yaml
spec:
  podSelector:
    matchLabels:
      mtls: "strict"
  ingress:
    - from:
        - namespaceSelector:
            matchLabels:
              security.istio.io/tlsMode: ISTIO_MUTUAL
```

## Troubleshooting

### Check Policy Enforcement

```bash
# Get all policies
kubectl get networkpolicies -n erlmcp

# Describe a policy
kubectl describe networkpolicy <policy-name> -n erlmcp

# Get pod IP
kubectl get pod <pod-name> -n erlmcp -o jsonpath='{.status.podIP}'

# Test connectivity from a pod
kubectl exec -it <pod-name> -n erlmcp -- nc -zv <target> <port>
```

### Common Issues

1. **DNS resolution fails**: Ensure DNS egress rule allows port 53 to kube-system

2. **Database connection fails**: Check that database namespace is labeled correctly

3. **Metrics not scraped**: Verify monitoring namespace policy allows ingress to port 9090

4. **External API calls fail**: Ensure external HTTPS egress rule allows port 443

## Best Practices

1. **Always use specific pod selectors** instead of empty podSelector {}

2. **Specify exact ports** rather than port ranges where possible

3. **Use namespace labels** (kubernetes.io/metadata.name) for cross-namespace policies

4. **Block RFC1918 ranges** from external egress (except for VPN/private endpoints)

5. **Test policies in staging** before applying to production

6. **Review and audit policies regularly** for overly permissive rules

7. **Use network policy validation** scripts in CI/CD pipeline

8. **Document custom rules** with comments explaining the business need

## References

- [Kubernetes Network Policies](https://kubernetes.io/docs/concepts/services-networking/network-policies/)
- [Zero Trust Architecture](https://www.cisa.gov/zero-trust-maturity-model)
- [CIS Kubernetes Benchmark](https://www.cisecurity.org/benchmark/kubernetes)
- [NetworkPolicy Editor](https://editor.cilium.io/)
