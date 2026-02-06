# Service Mesh Integration for erlmcp v3

## Table of Contents

- [Overview](#overview)
- [Istio Integration](#istio-integration)
- [Linkerd Integration](#linkerd-integration)
- [mTLS Configuration](#mtls-configuration)
- [Traffic Management](#traffic-management)
- [Observability](#observability)
- [Security Policies](#security-policies)
- [Performance Tuning](#performance-tuning)

## Overview

Service mesh integration provides erlmcp v3 with enterprise-grade features including mutual TLS (mTLS), traffic management, observability, and zero-trust security. erlmcp v3 supports both Istio and Linkerd service meshes.

### Benefits

- **Zero-Trust Security**: mTLS between all services
- **Traffic Management**: Canary deployments, circuit breaking, retries
- **Observability**: Distributed tracing, metrics, service graphs
- **Resilience**: Fault injection, timeouts, retries
- **Policy Enforcement**: Access control, rate limiting

### Supported Service Meshes

- **Istio**: Full-featured service mesh with extensive policy controls
- **Linkerd**: Lightweight, fast service mesh with simplicity focus

## Istio Integration

### Installation

#### Install Istio

```bash
# Download Istio
curl -L https://istio.io/downloadIstio | sh -
cd istio-*
export PATH=$PWD/bin:$PATH

# Install Istio with demo profile (dev/staging)
istioctl install --set profile=demo -y

# Install Istio with production profile
istioctl install --set profile=production \
  --set values.global.proxy.resources.requests.cpu=100m \
  --set values.global.proxy.resources.requests.memory=128Mi \
  --set values.global.proxy.resources.limits.cpu=2000m \
  --set values.global.proxy.resources.limits.memory=1024Mi \
  -y

# Enable automatic sidecar injection
kubectl label namespace erlmcp istio-injection=enabled
```

#### Verify Installation

```bash
# Check Istio installation
istioctl verify-install

# Check namespace labels
kubectl get namespace erlmcp --show-labels

# Check proxy status
istioctl proxy-status
```

### erlmcp Configuration for Istio

#### Enable Service Mesh in Helm Values

```yaml
# values-production.yaml
global:
  serviceMesh:
    enabled: true
    provider: "istio"

tls:
  mtls:
    enabled: true
    mode: "STRICT"
    minProtocolVersion: "TLSV1_3"
    cipherSuites:
      - "TLS_AES_128_GCM_SHA256"
      - "TLS_AES_256_GCM_SHA384"
      - "TLS_CHACHA20_POLY1305_SHA256"

    connectionPool:
      tcp:
        maxConnections: 100
        connectTimeout: "10s"
        keepAlive:
          time: "300s"
          interval: "60s"
          probes: 3
      http:
        h2UpgradePolicy: "UPGRADE"
        idleTimeout: "300s"
        http2MaxRequests: 1000

    loadBalancer: "ROUND_ROBIN"

    outlierDetection:
      consecutive5xxErrors: 3
      interval: "30s"
      baseEjectionTime: "30s"
      maxEjectionPercent: 50
      minHealthPercent: 50
```

### Istio Resources

#### PeerAuthentication (mTLS)

The Helm chart automatically creates this when `tls.mtls.enabled=true`:

```yaml
apiVersion: security.istio.io/v1beta1
kind: PeerAuthentication
metadata:
  name: erlmcp-mtls-strict
  namespace: erlmcp
spec:
  selector:
    matchLabels:
      app.kubernetes.io/name: erlmcp
  mtls:
    mode: STRICT
```

#### DestinationRule

```yaml
apiVersion: networking.istio.io/v1beta1
kind: DestinationRule
metadata:
  name: erlmcp-destination
  namespace: erlmcp
spec:
  host: erlmcp.erlmcp.svc.cluster.local
  trafficPolicy:
    tls:
      mode: ISTIO_MUTUAL
      cipherSuites:
        - TLS_AES_128_GCM_SHA256
        - TLS_AES_256_GCM_SHA384
      minProtocolVersion: TLSV1_3

    connectionPool:
      tcp:
        maxConnections: 100
        connectTimeout: 10s
        tcpKeepalive:
          time: 300s
          interval: 60s
          probes: 3
      http:
        http1MaxPendingRequests: 1024
        http2MaxRequests: 1024
        maxRequestsPerConnection: 100
        idleTimeout: 300s

    loadBalancer:
      simple: LEAST_REQUEST
      warmupDurationSecs: 30s

    outlierDetection:
      consecutive5xxErrors: 3
      interval: 30s
      baseEjectionTime: 30s
      maxEjectionPercent: 50
      minHealthPercent: 50
```

#### AuthorizationPolicy

```yaml
apiVersion: security.istio.io/v1beta1
kind: AuthorizationPolicy
metadata:
  name: erlmcp-authz
  namespace: erlmcp
spec:
  selector:
    matchLabels:
      app.kubernetes.io/name: erlmcp
  action: ALLOW
  rules:
    # Allow traffic from namespace services with mTLS
    - from:
        - source:
            principals:
              - "cluster.local/ns/erlmcp/sa/erlmcp"
            namespaces:
              - "erlmcp"

    # Allow traffic from Istio ingress
    - from:
        - source:
            principals:
              - "cluster.local/ns/istio-system/sa/istio-ingressgateway-service-account"
            namespaces:
              - "istio-system"

    # Allow monitoring systems
    - from:
        - source:
            principals:
              - "cluster.local/ns/monitoring/sa/prometheus"
            namespaces:
              - "monitoring"
      to:
        - operation:
            ports:
              - "9090"  # Metrics

    # Allow health checks from anywhere
    - to:
        - operation:
            paths:
              - "/health"
              - "/ready"
```

#### Virtual Service (Traffic Management)

```yaml
apiVersion: networking.istio.io/v1beta1
kind: VirtualService
metadata:
  name: erlmcp-vs
  namespace: erlmcp
spec:
  hosts:
    - erlmcp.erlmcp.svc.cluster.local
    - erlmcp.example.com

  gateways:
    - erlmcp-gateway

  http:
    # Canary deployment: 10% to v3.1, 90% to v3.0
    - match:
        - headers:
            x-version:
              exact: "v3.1"
      route:
        - destination:
            host: erlmcp.erlmcp.svc.cluster.local
            subset: v3-1
          weight: 100

    - route:
        - destination:
            host: erlmcp.erlmcp.svc.cluster.local
            subset: v3-0
          weight: 90
        - destination:
            host: erlmcp.erlmcp.svc.cluster.local
            subset: v3-1
          weight: 10

      # Retry policy
      retries:
        attempts: 3
        perTryTimeout: 2s
        retryOn: "5xx,reset,connect-failure,refused-stream"

      # Timeout
      timeout: 30s

      # Fault injection for testing
      fault:
        delay:
          percentage:
            value: 0.1
          fixedDelay: 5s
```

#### Gateway

```yaml
apiVersion: networking.istio.io/v1beta1
kind: Gateway
metadata:
  name: erlmcp-gateway
  namespace: erlmcp
spec:
  selector:
    istio: ingressgateway

  servers:
    - port:
        number: 443
        name: https
        protocol: HTTPS
      tls:
        mode: SIMPLE
        credentialName: erlmcp-tls
      hosts:
        - "erlmcp.example.com"

    - port:
        number: 80
        name: http
        protocol: HTTP
      hosts:
        - "erlmcp.example.com"
      tls:
        httpsRedirect: true
```

### Circuit Breaking

```yaml
apiVersion: networking.istio.io/v1beta1
kind: DestinationRule
metadata:
  name: erlmcp-circuit-breaker
  namespace: erlmcp
spec:
  host: erlmcp.erlmcp.svc.cluster.local
  trafficPolicy:
    connectionPool:
      tcp:
        maxConnections: 100
      http:
        http1MaxPendingRequests: 10
        http2MaxRequests: 100
        maxRequestsPerConnection: 2

    outlierDetection:
      consecutive5xxErrors: 5
      interval: 30s
      baseEjectionTime: 30s
      maxEjectionPercent: 50
      minHealthPercent: 25
```

## Linkerd Integration

### Installation

```bash
# Install Linkerd CLI
curl --proto '=https' --tlsv1.2 -sSfL https://run.linkerd.io/install | sh
export PATH=$PATH:$HOME/.linkerd2/bin

# Verify prerequisites
linkerd check --pre

# Install Linkerd
linkerd install --crds | kubectl apply -f -
linkerd install | kubectl apply -f -

# Verify installation
linkerd check

# Enable automatic proxy injection
kubectl annotate namespace erlmcp linkerd.io/inject=enabled
```

### erlmcp Configuration for Linkerd

#### Enable in Helm Values

```yaml
# values-production.yaml
global:
  serviceMesh:
    enabled: true
    provider: "linkerd"

# Linkerd annotations (applied to pods)
deployment:
  podAnnotations:
    linkerd.io/inject: "enabled"
    config.linkerd.io/proxy-cpu-request: "100m"
    config.linkerd.io/proxy-memory-request: "128Mi"
    config.linkerd.io/proxy-cpu-limit: "1000m"
    config.linkerd.io/proxy-memory-limit: "512Mi"

statefulset:
  podAnnotations:
    linkerd.io/inject: "enabled"
```

### Linkerd Resources

#### Server Policy

```yaml
apiVersion: policy.linkerd.io/v1beta1
kind: Server
metadata:
  name: erlmcp-server
  namespace: erlmcp
spec:
  podSelector:
    matchLabels:
      app.kubernetes.io/name: erlmcp
  port: 8080
  proxyProtocol: HTTP/2
```

#### ServerAuthorization

```yaml
apiVersion: policy.linkerd.io/v1beta1
kind: ServerAuthorization
metadata:
  name: erlmcp-authz
  namespace: erlmcp
spec:
  server:
    name: erlmcp-server
  client:
    meshTLS:
      serviceAccounts:
        - name: erlmcp
          namespace: erlmcp
        - name: prometheus
          namespace: monitoring
```

#### TrafficSplit (Canary)

```yaml
apiVersion: split.smi-spec.io/v1alpha2
kind: TrafficSplit
metadata:
  name: erlmcp-canary
  namespace: erlmcp
spec:
  service: erlmcp
  backends:
    - service: erlmcp-stable
      weight: 90
    - service: erlmcp-canary
      weight: 10
```

## mTLS Configuration

### Zero-Trust Security Architecture

```
┌─────────────────────────────────────────────┐
│  External Client (TLS termination at edge) │
└────────────────┬────────────────────────────┘
                 │ TLS
                 ▼
        ┌────────────────┐
        │ Istio Ingress  │
        │   Gateway      │
        └───────┬────────┘
                │ mTLS (STRICT)
    ┌───────────┼───────────┐
    │           │           │
    ▼ mTLS      ▼ mTLS      ▼ mTLS
┌──────────┐ ┌──────────┐ ┌──────────┐
│ erlmcp-0 │ │ erlmcp-1 │ │ erlmcp-2 │
│  + proxy │ │  + proxy │ │  + proxy │
└────┬─────┘ └────┬─────┘ └────┬─────┘
     │ mTLS       │ mTLS       │ mTLS
     └────────────┼────────────┘
                  │
         ┌────────▼────────┐
         │   PostgreSQL    │
         │     + proxy     │
         └─────────────────┘
```

### Application-Level mTLS

erlmcp can also implement application-level mTLS for Erlang distributed protocol:

```erlang
%% In sys.config or environment
{ssl, [
    {verify, verify_peer},
    {fail_if_no_peer_cert, true},
    {verify_fun, {ssl_verify_fun, [
        {fail_if_no_peer_cert, true},
        {verify_client_once, false}
    ]}},
    {cacertfile, "/etc/erlmcp/certs/ca.crt"},
    {certfile, "/etc/erlmcp/certs/server.crt"},
    {keyfile, "/etc/erlmcp/certs/server.key"},
    {versions, ['tlsv1.3', 'tlsv1.2']},
    {ciphers, [
        "TLS_AES_128_GCM_SHA256",
        "TLS_AES_256_GCM_SHA384"
    ]},
    {honor_cipher_order, true},
    {secure_renegotiate, true}
]}.
```

### Certificate Management

#### cert-manager Integration

```bash
# Install cert-manager
kubectl apply -f https://github.com/cert-manager/cert-manager/releases/download/v1.13.0/cert-manager.yaml

# Create ClusterIssuer
cat <<EOF | kubectl apply -f -
apiVersion: cert-manager.io/v1
kind: ClusterIssuer
metadata:
  name: letsencrypt-prod
spec:
  acme:
    server: https://acme-v02.api.letsencrypt.org/directory
    email: admin@erlmcp.com
    privateKeySecretRef:
      name: letsencrypt-prod
    solvers:
      - http01:
          ingress:
            class: nginx
EOF
```

#### Certificate Resource

```yaml
apiVersion: cert-manager.io/v1
kind: Certificate
metadata:
  name: erlmcp-tls
  namespace: erlmcp
spec:
  secretName: erlmcp-tls
  issuerRef:
    name: letsencrypt-prod
    kind: ClusterIssuer
  dnsNames:
    - erlmcp.example.com
    - "*.erlmcp.example.com"
  duration: 2160h  # 90 days
  renewBefore: 360h  # 15 days
```

## Traffic Management

### Canary Deployments

```yaml
# Step 1: Deploy canary version
apiVersion: apps/v1
kind: Deployment
metadata:
  name: erlmcp-canary
  namespace: erlmcp
spec:
  replicas: 1
  selector:
    matchLabels:
      app: erlmcp
      version: v3-1
  template:
    metadata:
      labels:
        app: erlmcp
        version: v3-1
    spec:
      containers:
        - name: erlmcp
          image: erlmcp:3.1.0
---
# Step 2: Create DestinationRule with subsets
apiVersion: networking.istio.io/v1beta1
kind: DestinationRule
metadata:
  name: erlmcp-versions
  namespace: erlmcp
spec:
  host: erlmcp.erlmcp.svc.cluster.local
  subsets:
    - name: v3-0
      labels:
        version: v3-0
    - name: v3-1
      labels:
        version: v3-1
---
# Step 3: Route 10% traffic to canary
apiVersion: networking.istio.io/v1beta1
kind: VirtualService
metadata:
  name: erlmcp-canary
  namespace: erlmcp
spec:
  hosts:
    - erlmcp.erlmcp.svc.cluster.local
  http:
    - route:
        - destination:
            host: erlmcp.erlmcp.svc.cluster.local
            subset: v3-0
          weight: 90
        - destination:
            host: erlmcp.erlmcp.svc.cluster.local
            subset: v3-1
          weight: 10
```

### A/B Testing

```yaml
apiVersion: networking.istio.io/v1beta1
kind: VirtualService
metadata:
  name: erlmcp-ab-test
  namespace: erlmcp
spec:
  hosts:
    - erlmcp.erlmcp.svc.cluster.local
  http:
    # Route based on user cookie
    - match:
        - headers:
            cookie:
              regex: ".*group=beta.*"
      route:
        - destination:
            host: erlmcp.erlmcp.svc.cluster.local
            subset: v3-1

    # Default route
    - route:
        - destination:
            host: erlmcp.erlmcp.svc.cluster.local
            subset: v3-0
```

### Blue/Green Deployments

```bash
# Step 1: Deploy green version
kubectl apply -f erlmcp-green-deployment.yaml

# Step 2: Verify green deployment
kubectl exec -it erlmcp-green-0 -- curl http://localhost:8080/health

# Step 3: Switch traffic to green
kubectl patch virtualservice erlmcp -n erlmcp --type=json \
  -p='[{"op": "replace", "path": "/spec/http/0/route/0/weight", "value": 0}]'
kubectl patch virtualservice erlmcp -n erlmcp --type=json \
  -p='[{"op": "replace", "path": "/spec/http/0/route/1/weight", "value": 100}]'

# Step 4: Monitor and rollback if needed
kubectl patch virtualservice erlmcp -n erlmcp --type=json \
  -p='[{"op": "replace", "path": "/spec/http/0/route/0/weight", "value": 100}]'
```

## Observability

### Distributed Tracing

#### Jaeger Integration

```bash
# Install Jaeger
kubectl apply -f https://raw.githubusercontent.com/istio/istio/release-1.20/samples/addons/jaeger.yaml

# Configure Istio tracing
istioctl install --set values.pilot.traceSampling=100.0
```

#### Application Configuration

```yaml
# Erlang configuration for OpenTelemetry
app:
  erlang:
    env:
      OTEL_EXPORTER_OTLP_ENDPOINT: "http://jaeger-collector:4317"
      OTEL_SERVICE_NAME: "erlmcp"
      OTEL_TRACES_SAMPLER: "traceidratio"
      OTEL_TRACES_SAMPLER_ARG: "0.1"  # 10% sampling
```

### Service Graph

```bash
# Install Kiali
kubectl apply -f https://raw.githubusercontent.com/istio/istio/release-1.20/samples/addons/kiali.yaml

# Access Kiali dashboard
istioctl dashboard kiali
```

### Metrics

```yaml
# ServiceMonitor for Prometheus
apiVersion: monitoring.coreos.com/v1
kind: ServiceMonitor
metadata:
  name: erlmcp-mesh-metrics
  namespace: erlmcp
spec:
  selector:
    matchLabels:
      app.kubernetes.io/name: erlmcp
  endpoints:
    - port: http-envoy-prom
      interval: 15s
      path: /stats/prometheus
```

## Security Policies

### Rate Limiting

```yaml
# Istio rate limiting
apiVersion: networking.istio.io/v1beta1
kind: EnvoyFilter
metadata:
  name: erlmcp-rate-limit
  namespace: erlmcp
spec:
  workloadSelector:
    labels:
      app.kubernetes.io/name: erlmcp
  configPatches:
    - applyTo: HTTP_FILTER
      match:
        context: SIDECAR_INBOUND
      patch:
        operation: INSERT_BEFORE
        value:
          name: envoy.filters.http.local_ratelimit
          typed_config:
            "@type": type.googleapis.com/envoy.extensions.filters.http.local_ratelimit.v3.LocalRateLimit
            stat_prefix: http_local_rate_limiter
            token_bucket:
              max_tokens: 1000
              tokens_per_fill: 100
              fill_interval: 1s
```

### Request Authentication (JWT)

```yaml
apiVersion: security.istio.io/v1beta1
kind: RequestAuthentication
metadata:
  name: erlmcp-jwt
  namespace: erlmcp
spec:
  selector:
    matchLabels:
      app.kubernetes.io/name: erlmcp
  jwtRules:
    - issuer: "https://auth.erlmcp.com"
      jwksUri: "https://auth.erlmcp.com/.well-known/jwks.json"
      forwardOriginalToken: true
```

## Performance Tuning

### Proxy Resource Allocation

```yaml
# Istio proxy resource tuning
deployment:
  podAnnotations:
    sidecar.istio.io/proxyCPU: "100m"
    sidecar.istio.io/proxyMemory: "128Mi"
    sidecar.istio.io/proxyCPULimit: "2000m"
    sidecar.istio.io/proxyMemoryLimit: "1024Mi"
```

### Connection Pool Tuning

```yaml
# Optimize for high throughput
trafficPolicy:
  connectionPool:
    tcp:
      maxConnections: 10000
      connectTimeout: 5s
    http:
      http2MaxRequests: 10000
      maxRequestsPerConnection: 0  # Unlimited
      idleTimeout: 600s
```

## Docker-Only Validation

Per CLAUDE.md requirements:

```bash
# Validate Istio configuration
docker compose run erlmcp-build istioctl analyze -n erlmcp

# Test mTLS connectivity
docker compose run erlmcp-build kubectl exec -n erlmcp erlmcp-0 -- \
  curl -v https://erlmcp-1.erlmcp-headless:8080/health

# Verify service mesh injection
docker compose run erlmcp-build kubectl get pods -n erlmcp -o jsonpath='{.items[*].spec.containers[*].name}'
```

## Conclusion

Service mesh integration provides erlmcp v3 with:
- Zero-trust security through mTLS
- Advanced traffic management capabilities
- Comprehensive observability
- Policy enforcement
- Enhanced resilience

Choose Istio for comprehensive features or Linkerd for simplicity and performance.

## Support

For enterprise support, contact:
- **Email**: enterprise-support@erlmcp.com
- **Portal**: https://enterprise.erlmcp.com
- **Documentation**: https://docs.erlmcp.com/v3/kubernetes/service-mesh
