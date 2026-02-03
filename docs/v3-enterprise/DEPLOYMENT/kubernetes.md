# Kubernetes Deployment Guide

## Overview

This guide covers deploying erlmcp v3 on Kubernetes clusters. Kubernetes provides enterprise-grade orchestration for containerized applications with features like auto-scaling, self-healing, and rolling updates.

## Prerequisites

- Kubernetes cluster (v1.21+)
- kubectl configured
- Helm 3.0+
- Ingress controller (nginx/traefik)
- Storage class for persistent data
- Cert-manager for SSL certificates

## Quick Start

### 1. Install using Helm

```bash
# Add Helm repository
helm repo add erlmcp https://charts.erlmcp.io
helm repo update

# Install erlmcp with default values
helm install erlmcp erlmcp/erlmcp --namespace erlmcp --create-namespace

# Check status
kubectl get pods -n erlmcp
kubectl get services -n erlmcp
```

### 2. Minimal Configuration

```yaml
# values-minimal.yaml
replicaCount: 3
image:
  repository: erlmcp/erlmcp
  tag: v3.0.0
  pullPolicy: IfNotPresent

service:
  type: LoadBalancer
  port: 8080
  metricsPort: 8081
  dashboardPort: 8082

config:
  mode: production
  sessionBackend: dets
  transport: http
  logLevel: info

resources:
  requests:
    memory: "1Gi"
    cpu: "500m"
  limits:
    memory: "2Gi"
    cpu: "1000m"
```

```bash
helm install erlmcp erlmcp/erlmcp -f values-minimal.yaml
```

## Configuration Reference

### Complete values.yaml

```yaml
# values.yaml
global:
  imageRegistry: ""
  imagePullSecrets: []
  domain: erlmcp.company.com

replicaCount: 3

image:
  repository: erlmcp/erlmcp
  tag: v3.0.0
  pullPolicy: IfNotPresent
  digest: ""

nameOverride: ""
fullnameOverride: ""

serviceAccount:
  create: true
  annotations: {}
  name: ""

podAnnotations: {}
podLabels: {}

podSecurityContext:
  fsGroup: 1000

securityContext:
  capabilities:
    drop:
    - ALL
  readOnlyRootFilesystem: true
  runAsNonRoot: true
  runAsUser: 1000

service:
  type: LoadBalancer
  port: 8080
  metricsPort: 8081
  dashboardPort: 8082
  annotations: {}
  labels: {}

ingress:
  enabled: true
  className: "nginx"
  annotations:
    nginx.ingress.kubernetes.io/rewrite-target: /
    cert-manager.io/cluster-issuer: "letsencrypt-prod"
  hosts:
    - host: erlmcp.company.com
      paths:
        - path: /
          pathType: Prefix
  tls: []

config:
  # Core configuration
  mode: production
  nodeName: ""
  cookie: "secure-cookie"
  transport: http
  logLevel: info

  # Session configuration
  sessionBackend: dets
  sessionDir: "/data/sessions"

  # Cluster configuration
  clusterNodes: []

  # SSL configuration
  sslEnabled: false
  sslCert: ""
  sslKey: ""

  # Resource limits
  maxConnections: 10000
  maxMemory: "1GB"

  # Monitoring
  metricsEnabled: true
  tracingEnabled: true

  # Custom configuration (merged into sys.config)
  customConfig: {}

serviceMonitor:
  enabled: true
  namespace: monitoring
  interval: 30s
  scrapeTimeout: 10s

networkPolicy:
  enabled: true
  ingress:
    - from:
      - namespaceSelector:
          matchLabels:
            name: erlmcp
      ports:
        - protocol: TCP
          port: 8080
        - protocol: TCP
          port: 8081
        - protocol: TCP
          port: 8082

resources:
  limits:
    cpu: 1000m
    memory: 2Gi
  requests:
    cpu: 500m
    memory: 1Gi

autoscaling:
  enabled: false
  minReplicas: 3
  maxReplicas: 10
  targetCPUUtilizationPercentage: 80
  targetMemoryUtilizationPercentage: 80

nodeSelector: {}

tolerations: []

affinity: {}

persistence:
  enabled: true
  size: 10Gi
  storageClass: "fast-ssd"
  accessMode: ReadWriteOnce
  mountPath: "/data"

livenessProbe:
  httpGet:
    path: /health
    port: http
  initialDelaySeconds: 30
  periodSeconds: 10
  timeoutSeconds: 5
  failureThreshold: 3

readinessProbe:
  httpGet:
    path: /health
    port: http
  initialDelaySeconds: 5
  periodSeconds: 5
  timeoutSeconds: 3
  failureThreshold: 3

startupProbe:
  httpGet:
    path: /health
    port: http
  initialDelaySeconds: 10
  periodSeconds: 10
  timeoutSeconds: 5
  failureThreshold: 30
```

## Production Deployment

### 1. High-Availability Configuration

```yaml
# values-prod.yaml
replicaCount: 5

# Resource allocation
resources:
  requests:
    memory: "2Gi"
    cpu: "1000m"
  limits:
    memory: "4Gi"
    cpu: "2000m"

# Persistence
persistence:
  enabled: true
  size: 50Gi
  storageClass: "enterprise-ssd"
  accessMode: ReadWriteOnce

# Auto-scaling
autoscaling:
  enabled: true
  minReplicas: 5
  maxReplicas: 20
  targetCPUUtilizationPercentage: 70
  targetMemoryUtilizationPercentage: 75

# Network policies
networkPolicy:
  enabled: true
  ingress:
    - from:
      - namespaceSelector:
          matchLabels:
            name: monitoring
      - namespaceSelector:
          matchLabels:
            name: ingress
      ports:
        - protocol: TCP
          port: 8080
        - protocol: TCP
          port: 8081
        - protocol: TCP
          port: 8082

# Security
podSecurityContext:
  fsGroup: 1000
  runAsUser: 1000
  runAsGroup: 1000

securityContext:
  runAsNonRoot: true
  readOnlyRootFilesystem: true
  capabilities:
    drop:
      - ALL
    add:
      - NET_BIND_SERVICE

# SSL configuration
config:
  sslEnabled: true
  sslCert: "/etc/tls/tls.crt"
  sslKey: "/etc/tls/tls.key"

ingress:
  enabled: true
  className: "nginx"
  annotations:
    nginx.ingress.kubernetes.io/ssl-redirect: "true"
    cert-manager.io/cluster-issuer: "letsencrypt-prod"
    nginx.ingress.kubernetes.io/rate-limit: "100"
  hosts:
    - host: erlmcp.prod.company.com
      paths:
        - path: /
          pathType: Prefix
  tls:
    - secretName: erlmcp-tls
      hosts:
        - erlmcp.prod.company.com

# Monitoring
serviceMonitor:
  enabled: true
  interval: 15s
  scrapeTimeout: 5s
  additionalLabels:
    release: prometheus
```

### 2. Helm Installation

```bash
# Install with production values
helm install erlmcp-prod erlmcp/erlmcp \
  -f values-prod.yaml \
  --namespace erlmcp-prod \
  --create-namespace

# Verify installation
kubectl get pods -n erlmcp-prod
kubectl get services -n erlmcp-prod
kubectl get ingress -n erlmcp-prod
```

## Multi-Region Deployment

### 1. Regional Configuration

```yaml
# values-region-a.yaml
replicaCount: 3
config:
  mode: production
  region: region-a
  datacenter: dc1
  clusterNodes:
    - erlmcp-a-1
    - erlmcp-a-2
    - erlmcp-a-3

persistence:
  enabled: true
  size: 20Gi
  storageClass: "region-a-ssd"

resources:
  requests:
    memory: "1Gi"
    cpu: "500m"
  limits:
    memory: "2Gi"
    cpu: "1000m"
```

```bash
# Install in region A
helm install erlmcp-region-a erlmcp/erlmcp \
  -f values-region-a.yaml \
  --namespace erlmcp-region-a \
  --create-namespace
```

### 2. Cross-Region Networking

```yaml
# cross-region-network-policy.yaml
apiVersion: networking.k8s.io/v1
kind: NetworkPolicy
metadata:
  name: erlmcp-cross-region
  namespace: erlmcp-region-a
spec:
  podSelector: {}
  policyTypes:
  - Ingress
  - Egress
  ingress:
  - from:
    - namespaceSelector:
        matchLabels:
          name: erlmcp-region-b
  egress:
  - to:
    - namespaceSelector:
        matchLabels:
          name: erlmcp-region-b
```

## Monitoring and Observability

### 1. Prometheus Integration

```yaml
# prometheus-operator.yaml
apiVersion: monitoring.coreos.com/v1
kind: ServiceMonitor
metadata:
  name: erlmcp
  namespace: erlmcp
  labels:
    app: erlmcp
spec:
  selector:
    matchLabels:
      app: erlmcp
  endpoints:
  - port: metrics
    interval: 30s
    path: /metrics
```

### 2. Grafana Dashboards

```json
{
  "dashboard": {
    "title": "erlmcp Cluster Overview",
    "panels": [
      {
        "title": "Request Rate",
        "type": "graph",
        "targets": [
          {
            "expr": "rate(http_requests_total[5m])",
            "legendFormat": "{{method}} {{status}}"
          }
        ]
      },
      {
        "title": "Response Time",
        "type": "graph",
        "targets": [
          {
            "expr": "histogram_quantile(0.95, rate(http_request_duration_seconds_bucket[5m]))",
            "legendFormat": "95th percentile"
          }
        ]
      }
    ]
  }
}
```

### 3. Logging Configuration

```yaml
# fluentd-config.yaml
apiVersion: v1
kind: ConfigMap
metadata:
  name: fluentd-config
  namespace: erlmcp
data:
  fluent.conf: |
    <source>
      @type tail
      path /var/log/containers/erlmcp*.log
      pos /var/log/fluentd-containers.log.pos
      tag erlmcp.*
      format json
      time_format %Y-%m-%dT%H:%M:%S.%NZ
    </source>

    <match erlmcp.**>
      @type elasticsearch
      host elasticsearch-service
      port 9200
      index_name erlmcp-logs
      type_name _doc
    </match>
```

## Security Configuration

### 1. Pod Security Policies

```yaml
# pod-security-policy.yaml
apiVersion: policy/v1beta1
kind: PodSecurityPolicy
metadata:
  name: erlmcp-psp
spec:
  privileged: false
  allowPrivilegeEscalation: false
  requiredDropCapabilities:
    - ALL
  volumes:
    - 'configMap'
    - 'emptyDir'
    - 'projected'
    - 'secret'
    - 'downwardAPI'
    - 'persistentVolumeClaim'
  runAsUser:
    rule: 'MustRunAsNonRoot'
  seLinux:
    rule: 'RunAsAny'
  fsGroup:
    rule: 'RunAsAny'
```

### 2. RBAC Configuration

```yaml
# rbac.yaml
apiVersion: v1
kind: ServiceAccount
metadata:
  name: erlmcp-service-account
  namespace: erlmcp

---
apiVersion: rbac.authorization.k8s.io/v1
kind: Role
metadata:
  name: erlmcp-role
  namespace: erlmcp
rules:
- apiGroups: [""]
  resources: ["pods", "services", "configmaps", "secrets"]
  verbs: ["get", "list", "watch"]

---
apiVersion: rbac.authorization.k8s.io/v1
kind: RoleBinding
metadata:
  name: erlmcp-role-binding
  namespace: erlmcp
roleRef:
  apiGroup: rbac.authorization.k8s.io
  kind: Role
  name: erlmcp-role
subjects:
- kind: ServiceAccount
  name: erlmcp-service-account
  namespace: erlmcp
```

## Backup and Recovery

### 1. Velero Configuration

```bash
# Install Velero
velero install --provider aws --bucket erlmcp-backups --secret-file ./credentials-velero

# Create backup schedule
velero schedule create erlmcp-daily --schedule "0 2 * * *"

# Create restore operation
velero restore create erlmcp-restore-1 --from-backup erlmcp-daily-20240201
```

### 2. Persistent Volume Backup

```bash
# Backup PV data
kubectl exec -n erlmcp -c erlmcp erlmcp-pod-0 -- tar czf - /data > backup.tar.gz

# Restore to new PV
kubectl cp backup.tar.gz erlmcp-new-pod-0:/data/restore
kubectl exec -n erlmcp -c erlmcp erlmcp-new-pod-0 -- tar xzf /data/restore/backup.tar.gz -C /
```

## Performance Optimization

### 1. Resource Limits

```yaml
# values-performance.yaml
resources:
  requests:
    memory: "4Gi"
    cpu: "2000m"
  limits:
    memory: "8Gi"
    cpu: "4000m"

# Kubernetes tuning
kubelet:
  nodeStatusMaxPodAge: 1m
  containerMaxAllocatable: 6Gi
  cgroupsPerQOS: true
  enforceNodeAllocatable: [pods]
```

### 2. Network Optimization

```yaml
# kubelet configuration
kube-proxy:
  mode: "iptables"
  metricsBindAddress: "0.0.0.0:10249"

# Pod network configuration
podNetworkCIDR: "10.244.0.0/16"
serviceClusterIPRange: "10.96.0.0/12"
```

## Troubleshooting

### 1. Common Issues

**Pod Failing to Start**
```bash
# Check pod events
kubectl describe pod erlmcp-0 -n erlmcp

# Check logs
kubectl logs erlmcp-0 -n erlmcp
kubectl logs -f erlmcp-0 -n erlmcp
```

**Service Not Accessible**
```bash
# Check service endpoints
kubectl get endpoints erlmcp -n erlmcp

# Check ingress
kubectl get ingress erlmcp-ingress -n erlmcp
kubectl describe ingress erlmcp-ingress -n erlmcp
```

**Performance Issues**
```bash
# Check resource usage
kubectl top pods -n erlmcp

# Check node resources
kubectl describe nodes
```

### 2. Debug Mode

```bash
# Enable debug logging
helm upgrade erlmcp erlmcp/erlmcp \
  --set config.logLevel=debug \
  --namespace erlmcp

# Get debug information
kubectl get pods,configmaps,secrets -n erlmcp -o yaml
```

## Best Practices

1. **Use Helm for deployments** - Standardized deployment process
2. **Implement proper resource limits** - Prevent resource contention
3. **Use persistent volumes** - Data persistence across restarts
4. **Implement monitoring** - Track performance and health
5. **Use network policies** - Control traffic flow
6. **Regular backups** - Protect against data loss
7. **Version control configurations** - Track changes
8. **Implement RBAC** - Principle of least privilege
9. **Use pod security policies** - Enforce security standards
10. **Auto-scale based on metrics** - Maintain performance

## Next Steps

For additional information, see:
- [Cloud Deployment](../cloud.md)
- [Docker Deployment](../docker.md)
- [Operations Manual](../OPERATIONS/procedures.md)