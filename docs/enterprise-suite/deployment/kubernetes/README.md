# erlmcp v3 Kubernetes Deployment Guide

## Overview

This guide provides comprehensive instructions for deploying erlmcp v3 on Kubernetes. Kubernetes deployment offers advanced orchestration features including auto-scaling, rolling updates, and self-healing capabilities.

## Prerequisites

### System Requirements

- **Kubernetes**: Version 1.25 or later
- **kubectl**: Version 1.25 or later
- **Helm**: Version 3.8 or later (optional)
- **Minimum Resources**:
  - Cluster: 4 nodes, 8 vCPUs, 32GB RAM
  - Namespace: Dedicated erlmcp namespace
- **Storage**: Persistent volumes for data persistence

### Required Tools

```bash
# Install required tools
curl -fsSL -o get_helm.sh https://raw.githubusercontent.com/helm/helm/main/scripts/get-helm-3
chmod 700 get_helm.sh
./get_helm.sh

# Install kubectl (if not installed)
curl -LO "https://dl.k8s.io/release/$(curl -L -s https://dl.k8s.io/release/stable.txt)/bin/linux/amd64/kubectl"
chmod +x kubectl
sudo mv kubectl /usr/local/bin/
```

## Architecture

### Cluster Architecture

```mermaid
graph TB
    subgraph "Load Balancer Layer"
        LB[External Load Balancer]
        NG[Nginx Ingress]
    end

    subgraph "Gateway Layer"
        GW1[Gateway Pod]
        GW2[Gateway Pod]
    end

    subgraph "Application Layer"
        N1[erlmcp-1]
        N2[erlmcp-2]
        N3[erlmcp-3]
    end

    subgraph "Data Layer"
        DB[(PostgreSQL)]
        RD[(Redis)]
    end

    subgraph "Monitoring Layer"
        PR[Prometheus]
        GF[Grafana]
        LO[Jaeger]
    end

    LB --> NG
    NG --> GW1
    NG --> GW2
    GW1 --> N1
    GW2 --> N2
    N1 --> N3
    N2 --> DB
    N3 --> RD
    N1 --> PR
    N2 --> GF
    N3 --> LO
```

## Quick Start

### 1. Setup Namespace

```bash
# Create namespace
kubectl create namespace erlmcp

# Label for pod disruption budget
kubectl label namespace erlmcp istio-injection=disabled
```

### 2. Apply Base Configuration

```bash
# Create secrets
kubectl apply -f secrets/

# Create configmaps
kubectl apply -f configmaps/

# Create persistent volumes
kubectl apply -f pvc/
```

### 3. Deploy Components

```bash
# Deploy PostgreSQL
kubectl apply -f databases/postgres/

# Deploy Redis
kubectl apply -f databases/redis/

# Deploy erlmcp
kubectl apply -f apps/erlmcp/

# Deploy monitoring
kubectl apply -f monitoring/
```

### 4. Verify Deployment

```bash
# Check pods
kubectl get pods -n erlmcp

# Check services
kubectl get svc -n erlmcp

# Check health
kubectl exec -it erlmcp-0 -n erlmcp -- curl -f http://localhost:8080/v3/health
```

## Configuration

### Secrets Management

Create `secrets/secrets.yaml`:

```yaml
apiVersion: v1
kind: Secret
metadata:
  name: erlmcp-secrets
  namespace: erlmcp
type: Opaque
stringData:
  # Database credentials
  db-host: "postgres-service.erlmcp.svc.cluster.local"
  db-port: "5432"
  db-name: "erlmcp"
  db-user: "erlmcp"
  db-password: "your-secure-password"

  # Redis credentials
  redis-host: "redis-service.erlmcp.svc.cluster.local"
  redis-port: "6379"
  redis-password: "your-redis-password"

  # Security
  auth-secret: "your-auth-secret-key"
  jwt-secret: "your-jwt-secret-key"

  # SSL
  ssl-crt: |
    -----BEGIN CERTIFICATE-----
    your-certificate-here
    -----END CERTIFICATE-----
  ssl-key: |
    -----BEGIN PRIVATE KEY-----
    your-private-key-here
    -----END PRIVATE KEY-----
```

### ConfigMaps

Create `configmaps/config.yaml`:

```yaml
apiVersion: v1
kind: ConfigMap
metadata:
  name: erlmcp-config
  namespace: erlmcp
data:
  erlmcp.config: |
    {
        erlmcp,
        [
            % Cluster configuration
            {cluster_name, "erlmcp-cluster"},
            {cookie, "k8s-secret-cookie"},

            % Database
            {db_host, "${DB_HOST}"},
            {db_port, "${DB_PORT}"},
            {db_name, "${DB_NAME}"},
            {db_user, "${DB_USER}"},
            {db_password, "${DB_PASSWORD}"},
            {db_pool_size, 20},

            % Redis
            {redis_host, "${REDIS_HOST}"},
            {redis_port, "${REDIS_PORT}"},
            {redis_password, "${REDIS_PASSWORD}"},
            {redis_pool_size, 10},

            % Security
            {auth_secret, "${AUTH_SECRET}"},
            {jwt_secret, "${JWT_SECRET}"},

            % Performance
            {max_connections, 10000},
            {max_sessions, 5000},
            {workers, ${WORKERS_COUNT}},

            % Monitoring
            {metrics_enabled, true},
            {tracing_enabled, true},

            % SSL
            {ssl, true},
            {ssl_port, 8443},
            {ssl_certfile, "/etc/ssl/server.crt"},
            {ssl_keyfile, "/etc/ssl/server.key"},
            {ssl_versions, [tlsv1.2, tlsv1.3]},

            % Logging
            {log_level, info},
            {log_dir, "/var/log/erlmcp"},

            % Kubernetes discovery
            {k8s_discovery, true},
            {k8s_namespace, "erlmcp"},
            {k8s_service, "erlmcp-service"}
        ]
    }.

  logging.config: |
    {
        logger,
        [
            {handler, default, logger_console_hander,
                {logger_formatter,
                    #{single_line => false,
                      time_design => {utc, ISO8601},
                      legacy_header => true}},
                #{level => info}},
            {handler, file, logger_stdlib_hander,
                {logger_disk_log_hander,
                    #{file => "/var/log/erlmcp/erlmcp.log",
                      type => wrap,
                      max_no_files => 10,
                      max_no_bytes => 100000000,
                      compress => true}},
                #{level => info}}
        ]
    }.
```

### Persistent Volume Claims

Create `pvc/pvc.yaml`:

```yaml
apiVersion: v1
kind: PersistentVolumeClaim
metadata:
  name: postgres-pvc
  namespace: erlmcp
spec:
  accessModes:
    - ReadWriteOnce
  resources:
    requests:
      storage: 50Gi
  storageClassName: fast-ssd

---
apiVersion: v1
kind: PersistentVolumeClaim
metadata:
  name: redis-pvc
  namespace: erlmcp
spec:
  accessModes:
    - ReadWriteOnce
  resources:
    requests:
      storage: 20Gi
  storageClassName: fast-ssd

---
apiVersion: v1
kind: PersistentVolumeClaim
metadata:
  name: erlmcp-pvc
  namespace: erlmcp
spec:
  accessModes:
    - ReadWriteOnce
  resources:
    requests:
      storage: 20Gi
  storageClassName: fast-ssd
```

## Application Deployment

### erlmcp Deployment

Create `apps/erlmcp/erlmcp-deployment.yaml`:

```yaml
apiVersion: apps/v1
kind: Deployment
metadata:
  name: erlmcp
  namespace: erlmcp
  labels:
    app: erlmcp
    component: core
spec:
  replicas: 3
  strategy:
    type: RollingUpdate
    rollingUpdate:
      maxUnavailable: 1
      maxSurge: 1
  selector:
    matchLabels:
      app: erlmcp
  template:
    metadata:
      labels:
        app: erlmcp
      annotations:
        prometheus.io/scrape: "true"
        prometheus.io/port: "9090"
        prometheus.io/path: "/metrics"
    spec:
      serviceAccountName: erlmcp-sa
      containers:
      - name: erlmcp
        image: erlmcp/v3:3.0.0
        imagePullPolicy: IfNotPresent
        ports:
        - containerPort: 8080
          name: http
          protocol: TCP
        - containerPort: 8443
          name: https
          protocol: TCP
        - containerPort: 9090
          name: metrics
          protocol: TCP
        env:
        - name: POD_NAME
          valueFrom:
            fieldRef:
              fieldPath: metadata.name
        - name: POD_NAMESPACE
          valueFrom:
            fieldRef:
              fieldPath: metadata.namespace
        - name: POD_IP
          valueFrom:
            fieldRef:
              fieldPath: status.podIP
        envFrom:
        - configMapRef:
            name: erlmcp-config
        - secretRef:
            name: erlmcp-secrets
        volumeMounts:
        - name: config-volume
          mountPath: /app/config
        - name: ssl-volume
          mountPath: /app/certs
          readOnly: true
        - name: log-volume
          mountPath: /var/log/erlmcp
        - name: erlmcp-storage
          mountPath: /app/data
        resources:
          requests:
            cpu: "2"
            memory: "4Gi"
          limits:
            cpu: "4"
            memory: "8Gi"
        livenessProbe:
          httpGet:
            path: /v3/health
            port: 8080
          initialDelaySeconds: 30
          periodSeconds: 10
          timeoutSeconds: 5
          failureThreshold: 3
        readinessProbe:
          httpGet:
            path: /v3/health
            port: 8080
          initialDelaySeconds: 5
          periodSeconds: 5
          timeoutSeconds: 3
          failureThreshold: 3
        securityContext:
          runAsUser: 1000
          runAsGroup: 1000
          capabilities:
            drop:
            - ALL
      volumes:
      - name: config-volume
        configMap:
          name: erlmcp-config
      - name: ssl-volume
        secret:
          secretName: erlmcp-secrets
      - name: log-volume
        emptyDir: {}
      - name: erlmcp-storage
        persistentVolumeClaim:
          claimName: erlmcp-pvc
      affinity:
        podAntiAffinity:
          requiredDuringSchedulingIgnoredDuringExecution:
          - labelSelector:
              matchExpressions:
              - key: app
                operator: In
                values:
                - erlmcp
            topologyKey: "kubernetes.io/hostname"
      tolerations:
      - key: "erlmcp-node"
        operator: "Equal"
        value: "core"
        effect: "NoSchedule"
```

### Service and Ingress

Create `apps/erlmcp/erlmcp-service.yaml`:

```yaml
apiVersion: v1
kind: Service
metadata:
  name: erlmcp-service
  namespace: erlmcp
  labels:
    app: erlmcp
spec:
  selector:
    app: erlmcp
  ports:
  - name: http
    port: 80
    targetPort: 8080
  - name: https
    port: 443
    targetPort: 8443
  - name: metrics
    port: 9090
    targetPort: 9090
  type: ClusterIP
---
apiVersion: networking.k8s.io/v1
kind: Ingress
metadata:
  name: erlmcp-ingress
  namespace: erlmcp
  annotations:
    kubernetes.io/ingress.class: nginx
    nginx.ingress.kubernetes.io/rewrite-target: /
    nginx.ingress.kubernetes.io/ssl-redirect: "true"
    cert-manager.io/cluster-issuer: "letsencrypt-prod"
    nginx.ingress.kubernetes.io/rate-limit: "100"
    nginx.ingress.kubernetes.io/rate-limit-window: "1m"
spec:
  tls:
  - hosts:
    - api.erlmcp.com
    secretName: erlmcp-tls
  rules:
  - host: api.erlmcp.com
    http:
      paths:
      - path: /v3
        pathType: Prefix
        backend:
          service:
            name: erlmcp-service
            port:
              name: https
```

### Horizontal Pod Autoscaler

Create `apps/erlmcp/hpa.yaml`:

```yaml
apiVersion: autoscaling/v2
kind: HorizontalPodAutoscaler
metadata:
  name: erlmcp-hpa
  namespace: erlmcp
spec:
  scaleTargetRef:
    apiVersion: apps/v1
    kind: Deployment
    name: erlmcp
  minReplicas: 3
  maxReplicas: 10
  metrics:
  - type: Resource
    resource:
      name: cpu
      target:
        type: Utilization
        averageUtilization: 70
  - type: Resource
    resource:
      name: memory
      target:
        type: Utilization
        averageUtilization: 80
  - type: Pods
    pods:
      metric:
        name: http_requests_per_second
      target:
        type: AverageValue
        averageValue: "1000"
  behavior:
    scaleDown:
      stabilizationWindowSeconds: 300
      policies:
      - type: Percent
        value: 10
        periodSeconds: 60
    scaleUp:
      stabilizationWindowSeconds: 60
      policies:
      - type: Percent
        value: 50
        periodSeconds: 60
      - type: Pods
        value: 2
        periodSeconds: 60
```

## Database Deployment

### PostgreSQL

Create `databases/postgres/postgres-deployment.yaml`:

```yaml
apiVersion: apps/v1
kind: Deployment
metadata:
  name: postgres
  namespace: erlmcp
spec:
  replicas: 1
  selector:
    matchLabels:
      app: postgres
  template:
    metadata:
      labels:
        app: postgres
    spec:
      containers:
      - name: postgres
        image: postgres:15
        ports:
        - containerPort: 5432
        env:
        - name: POSTGRES_DB
          value: "erlmcp"
        - name: POSTGRES_USER
          valueFrom:
            secretKeyRef:
              name: erlmcp-secrets
              key: db-user
        - name: POSTGRES_PASSWORD
          valueFrom:
            secretKeyRef:
              name: erlmcp-secrets
              key: db-password
        volumeMounts:
        - name: postgres-storage
          mountPath: /var/lib/postgresql/data
        resources:
          requests:
            memory: "2Gi"
            cpu: "1"
          limits:
            memory: "4Gi"
            cpu: "2"
        livenessProbe:
          exec:
            command:
            - pg_isready
            - -U
            - erlmcp
          initialDelaySeconds: 30
          periodSeconds: 10
      volumes:
      - name: postgres-storage
        persistentVolumeClaim:
          claimName: postgres-pvc
---
apiVersion: v1
kind: Service
metadata:
  name: postgres-service
  namespace: erlmcp
spec:
  selector:
    app: postgres
  ports:
  - port: 5432
    targetPort: 5432
  type: ClusterIP
```

### Redis

Create `databases/redis/redis-deployment.yaml`:

```yaml
apiVersion: apps/v1
kind: Deployment
metadata:
  name: redis
  namespace: erlmcp
spec:
  replicas: 1
  selector:
    matchLabels:
      app: redis
  template:
    metadata:
      labels:
        app: redis
    spec:
      containers:
      - name: redis
        image: redis:7-alpine
        ports:
        - containerPort: 6379
        command:
        - redis-server
        - --requirepass
        - $(REDIS_PASSWORD)
        env:
        - name: REDIS_PASSWORD
          valueFrom:
            secretKeyRef:
              name: erlmcp-secrets
              key: redis-password
        volumeMounts:
        - name: redis-storage
          mountPath: /data
        resources:
          requests:
            memory: "1Gi"
            cpu: "500m"
          limits:
            memory: "2Gi"
            cpu: "1"
        livenessProbe:
          exec:
            command:
            - redis-cli
            - ping
          initialDelaySeconds: 30
          periodSeconds: 10
      volumes:
      - name: redis-storage
        persistentVolumeClaim:
          claimName: redis-pvc
---
apiVersion: v1
kind: Service
metadata:
  name: redis-service
  namespace: erlmcp
spec:
  selector:
    app: redis
  ports:
  - port: 6379
    targetPort: 6379
  type: ClusterIP
```

## Monitoring and Observability

### Prometheus Operator

Create `monitoring/prometheus-operator.yaml`:

```yaml
apiVersion: monitoring.coreos.com/v1
kind: Prometheus
metadata:
  name: prometheus
  namespace: monitoring
spec:
  serviceMonitorSelector:
    matchExpressions:
    - key: app
      operator: In
      values:
      - erlmcp
  podMonitorSelector:
    matchExpressions:
    - key: app
      operator: In
      values:
      - erlmcp
  resources:
    requests:
      memory: "2Gi"
    limits:
      memory: "4Gi"
  retention: 30d
```

### Grafana Dashboard

Create `monitoring/grafana/dashboard.yaml`:

```yaml
apiVersion: v1
kind: ConfigMap
metadata:
  name: erlmcp-grafana-dashboard
  namespace: monitoring
data:
  erlmcp-dashboard.json: |
    {
      "dashboard": {
        "title": "erlmcp Cluster Overview",
        "panels": [
          {
            "title": "HTTP Requests",
            "type": "graph",
            "targets": [
              {
                "expr": "rate(http_requests_total[5m])",
                "legendFormat": "{{method}} - {{status}}"
              }
            ]
          },
          {
            "title": "Active Sessions",
            "type": "singlestat",
            "targets": [
              {
                "expr": "erlmcp_sessions_total",
                "legendFormat": "Sessions"
              }
            ]
          },
          {
            "title": "CPU Usage",
            "type": "graph",
            "targets": [
              {
                "expr": "rate(process_cpu_seconds_total[5m])",
                "legendFormat": "CPU"
              }
            ]
          }
        ]
      }
    }
```

## Advanced Features

### Pod Disruption Budget

```yaml
apiVersion: policy/v1
kind: PodDisruptionBudget
metadata:
  name: erlmcp-pdb
  namespace: erlmcp
spec:
  minAvailable: 2
  selector:
    matchLabels:
      app: erlmcp
```

### Network Policy

```yaml
apiVersion: networking.k8s.io/v1
kind: NetworkPolicy
metadata:
  name: erlmcp-network-policy
  namespace: erlmcp
spec:
  podSelector:
    matchLabels:
      app: erlmcp
  policyTypes:
  - Ingress
  - Egress
  ingress:
  - from:
    - namespaceSelector:
        matchLabels:
          name: default
  egress:
  - to:
    - namespaceSelector:
        matchLabels:
          name: default
```

### Resource Quotas

```yaml
apiVersion: v1
kind: ResourceQuota
metadata:
  name: erlmcp-quota
  namespace: erlmcp
spec:
  hard:
    requests.cpu: "8"
    requests.memory: "16Gi"
    limits.cpu: "16"
    limits.memory: "32Gi"
    pods: "10"
    services: "5"
    persistentvolumeclaims: "5"
```

## Backup and Recovery

### Velero Backup

```bash
# Install Velero
velero install --provider aws --namespace velero --bucket erlmcp-backups --secret-file credentials-velero

# Create backup schedule
velero schedule create erlmcp-daily --schedule="0 2 * * *" --ttl=720h

# Create backup
velero backup create erlmcp-full --include-namespaces=erlmcp

# Restore
velero restore create --from-backup=erlmcp-full-20240101020000
```

### Database Backup

```yaml
apiVersion: batch/v1
kind: CronJob
metadata:
  name: postgres-backup
  namespace: erlmcp
spec:
  schedule: "0 2 * * *"
  jobTemplate:
    spec:
      template:
        spec:
          containers:
          - name: backup
            image: postgres:15
            command:
            - /bin/bash
            - -c
            - >
              pg_dump -h postgres-service -U erlmcp erlmcp > /backups/backup_$(date +%Y%m%d_%H%M%S).sql &&
              gzip /backups/*.sql
            volumeMounts:
            - name: backup-storage
              mountPath: /backups
          volumes:
          - name: backup-storage
            persistentVolumeClaim:
              claimName: backup-pvc
```

## Troubleshooting

### Common Issues

#### 1. Pod Won't Start

```bash
# Check events
kubectl describe pod erlmcp-0 -n erlmcp

# Check logs
kubectl logs erlmcp-0 -n erlmcp

# Check pods in other namespaces
kubectl get events --all-namespaces
```

#### 2. Network Issues

```bash
# Test pod connectivity
kubectl exec -it erlmcp-0 -n erlmcp -- wget -qO- http://postgres-service:5432

# Check network policies
kubectl get networkpolicy -n erlmcp
kubectl describe networkpolicy erlmcp-network-policy -n erlmcp
```

#### 3. Resource Issues

```bash
# Check pod resource usage
kubectl top pods -n erlmcp

# Check cluster capacity
kubectl get nodes -o custom-columns=NAME:.metadata.name,CAPACITY:.status.capacity.cpu,ALLOCATABLE:.status.allocatable.cpu
```

### Debugging Tools

```bash
# Access pod
kubectl exec -it erlmcp-0 -n erlmcp -- bash

# Port forwarding
kubectl port-forward svc/erlmcp-service 8080:80 -n erlmcp

# Debug readiness probe
kubectl exec erlmcp-0 -n erlmcp -- curl -v http://localhost:8080/v3/health
```

## Security Best Practices

### 1. Network Security

- Use Network Policies
- Implement service mesh (Istio)
- Encrypt traffic with mTLS
- Use Ingress with TLS

### 2. Pod Security

- Use Pod Security Policies
- Implement RBAC
- Run containers as non-root
- Use resource limits

### 3. Secrets Management

- Use Kubernetes Secrets
- External secret management (Vault)
- Rotate secrets regularly

### 4. Audit Logging

```yaml
apiVersion: audit.k8s.io/v1
kind: Policy
rules:
- level: Metadata
  resources:
  - group: ""
    resources: ["pods", "services", "configmaps", "secrets"]
```

## Maintenance

### Regular Maintenance Tasks

```bash
# Update erlmcp version
kubectl set image deployment/erlmcp erlmcp=erlmcp/v3:3.0.1 -n erlmcp

# Scale down for maintenance
kubectl scale deployment erlmcp --replicas=1 -n erlmcp

# Rotate certificates
kubectl delete secret erlmcp-tls -n erlmcp
```

### Health Checks

```bash
# Monitor cluster health
kubectl get nodes -o wide

# Check pod status
kubectl get pods -n erlmcp -w

# Check service status
kubectl get svc -n erlmcp
```

## Support

For enterprise support, contact:
- **Email**: enterprise-support@erlmcp.com
- **Portal**: https://enterprise.erlmcp.com
- **Documentation**: https://docs.erlmcp.com/v3