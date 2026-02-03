#!/bin/bash
# erlmcp v3 Monitoring Configuration Script
set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Configuration
CLUSTER_NAME="erlmcp-cluster"
REGION="us-east-1"
PROFILE="erlmcp-prod"
NAMESPACE="erlmcp"
GRAFANA_DASHBOARD_URL="https://grafana.example.com"

log() {
    echo -e "${GREEN}[INFO]${NC} $1"
}

warn() {
    echo -e "${YELLOW}[WARN]${NC} $1"
}

error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

# Validate configuration
validate_config() {
    log "Validating monitoring configuration..."

    # Check required variables
    if [ -z "$CLUSTER_NAME" ] || [ -z "$REGION" ] || [ -z "$PROFILE" ]; then
        error "Required configuration variables not set"
        exit 1
    fi

    # Check if cluster exists
    if ! aws eks describe-cluster --name $CLUSTER_NAME --region $REGION --query 'Cluster.Name' --output text > /dev/null; then
        error "Cluster $CLUSTER_NAME not found"
        exit 1
    fi

    # Check if kubectl is configured
    if ! kubectl cluster-info > /dev/null 2>&1; then
        error "kubectl not configured for the cluster"
        exit 1
    fi

    log "Configuration validated"
}

# Install Prometheus Operator
install_prometheus() {
    log "Installing Prometheus operator..."

    # Create monitoring namespace
    kubectl create namespace monitoring --dry-run=client -o yaml | kubectl apply -f -

    # Add Prometheus Helm repository
    helm repo add prometheus-community https://prometheus-community.github.io/helm-charts
    helm repo update

    # Install Prometheus operator
    helm install prometheus prometheus-community/kube-prometheus-stack \
        --namespace monitoring \
        --set prometheus.prometheusSpec.serviceMonitorSelectorNilHelmValues=false \
        --set prometheus.prometheusSpec.serviceMonitorNamespaceSelector.matchNames=[$NAMESPACE] \
        --set grafana.adminPassword=admin123 \
        --set grafana.service.type=LoadBalancer \
        --set grafana.ingress.enabled=true \
        --set grafana.ingress.hosts={$GRAFANA_DASHBOARD_URL} \
        --set grafana.ingress.annotations."nginx\.ingress\.kubernetes\.io\/backend-protocol"="HTTP"

    log "Prometheus operator installed"
}

# Install Alertmanager
install_alertmanager() {
    log "Installing Alertmanager..."

    # Configure Alertmanager
    kubectl apply -f - << EOF
apiVersion: v1
kind: ConfigMap
metadata:
  name: alertmanager-config
  namespace: monitoring
data:
  alertmanager.yml: |
    global:
      smtp_smarthost: 'localhost:587'
      smtp_from: 'alerts@example.com'
      smtp_auth_username: 'alerts@example.com'
      smtp_auth_password: 'your-smtp-password'

    route:
      group_by: ['alertname']
      group_wait: 10s
      group_interval: 10s
      repeat_interval: 1h
      receiver: 'web.hook'
      routes:
        - match:
            severity: critical
          receiver: critical
        - match:
            severity: warning
          receiver: warning

    receivers:
    - name: 'web.hook'
      email_configs:
      - to: 'security@example.com'
        subject: "Alertmanager Alert: {{ .GroupLabels.alertname }}"
        body: "{{ .CommonAnnotations.description }}"

    - name: critical
      email_configs:
      - to: 'security-team@example.com'
        subject: "[CRITICAL] Alertmanager Alert: {{ .GroupLabels.alertname }}"
        body: "{{ .CommonAnnotations.description }}"
        send_resolved: true

    - name: warning
      email_configs:
      - to: 'operations@example.com'
        subject: "[WARNING] Alertmanager Alert: {{ .GroupLabels.alertname }}"
        body: "{{ .CommonAnnotations.description }}"
        send_resolved: true
EOF

    # Update Alertmanager deployment
    kubectl patch deployment alertmanager-prometheus-kube-alertmanager \
        -n monitoring \
        --patch '{"spec": {"template": {"spec": {"containers": [{"name": "alertmanager", "env": [{"name": "ALERTMANAGER_CONFIG_FILE", "value": "/etc/alertmanager/config/alertmanager.yml"}]}]}}}}'

    log "Alertmanager installed and configured"
}

# Install Thanos
install_thanos() {
    log "Installing Thanos for multi-cluster monitoring..."

    # Add Thanos Helm repository
    helm repo add thanos https://thanos-io.github.io/thanos
    helm repo update

    # Install Thanos query
    helm install thanos-query thanos/thanos \
        --namespace monitoring \
        --set query.replicaCount=2 \
        --set query.service.type=LoadBalancer \
        --set query.storage="prometheus" \
        --set query.prometheus.url="http://prometheus-operated.monitoring.svc.cluster.local:9090"

    # Install Thanos store
    helm install thanos-store thanos/thanos \
        --namespace monitoring \
        --set store.replicaCount=2 \
        --set store.service.type=ClusterIP \
        --set store.minReplicas=1

    # Install Thanos compact
    helm install thanos-compact thanos/thanos \
        --namespace monitoring \
        --set compact.retention="30d" \
        --set compact.interval="24h"

    log "Thanos installed"
}

# Install Grafana dashboards
install_grafana_dashboards() {
    log "Installing Grafana dashboards..."

    # Wait for Grafana to be ready
    kubectl wait --for=condition=ready pod -l app.kubernetes.io/name=grafana -n monitoring --timeout=300s

    # Install erlmcp dashboard
    kubectl apply -f - << EOF
apiVersion: v1
kind: ConfigMap
metadata:
  name: erlmcp-dashboard
  namespace: monitoring
  labels:
    grafana_dashboard: "1"
data:
  erlmcp-dashboard.json: |
    {
      "dashboard": {
        "title": "erlmcp Cluster Overview",
        "panels": [
          {
            "title": "erlmcp API Requests",
            "type": "graph",
            "targets": [
              {
                "expr": "rate(http_requests_total{job=\"erlmcp\"}[5m])",
                "legendFormat": "{{method}} {{endpoint}}"
              }
            ]
          },
          {
            "title": "erlmcp Response Latency",
            "type": "graph",
            "targets": [
              {
                "expr": "histogram_quantile(0.95, rate(http_request_duration_seconds_bucket{job=\"erlmcp\"}[5m]))",
                "legendFormat": "95th percentile"
              }
            ]
          },
          {
            "title": "erlmcp Error Rate",
            "type": "graph",
            "targets": [
              {
                "expr": "rate(http_requests_total{job=\"erlmcp\", status=~\"5..\"}[5m]) / rate(http_requests_total{job=\"erlmcp\"}[5m]) * 100",
                "legendFormat": "Error rate (%)"
              }
            ]
          }
        ]
      }
    }
EOF

    # Import dashboard via Grafana API
    GRAFANA_POD=$(kubectl get pods -n monitoring -l app.kubernetes.io/name=grafana -o jsonpath='{.items[0].metadata.name}')
    kubectl exec -n monitoring $GRAFANA_POD -- \
        curl -X POST "http://localhost:3000/api/dashboards/db" \
        -H "Content-Type: application/json" \
        -H "Authorization: Bearer $(kubectl get secret grafana-admin-password -n monitoring -o jsonpath='{.data.password}' | base64 -d)" \
        -d @- << EOF
{
  "dashboard": {
    "title": "erlmcp Cluster Overview",
    "panels": [
      {
        "title": "erlmcp API Requests",
        "type": "graph",
        "targets": [
          {
            "expr": "rate(http_requests_total{job=\"erlmcp\"}[5m])",
            "legendFormat": "{{method}} {{endpoint}}"
          }
        ]
      }
    ]
  }
}
EOF

    log "Grafana dashboards installed"
}

# Configure service monitors
configure_service_monitors() {
    log "Configuring service monitors..."

    # Create service monitor for erlmcp
    kubectl apply -f - << EOF
apiVersion: monitoring.coreos.com/v1
kind: ServiceMonitor
metadata:
  name: erlmcp-monitor
  namespace: monitoring
  labels:
    app: erlmcp
spec:
  selector:
    matchLabels:
      app: erlmcp
  endpoints:
  - port: metrics
    interval: 15s
    path: /metrics
    metricRelabelings:
    - sourceLabels: [__name__]
      regex: 'erlmcp_.*'
      replacement: '\$1'
    targetLabels:
      - erlmcp
EOF

    # Create service monitor for Kubernetes
    kubectl apply -f - << EOF
apiVersion: monitoring.coreos.com/v1
kind: ServiceMonitor
metadata:
  name: kubernetes-monitor
  namespace: monitoring
  labels:
    app: kubernetes
spec:
  selector:
    matchLabels:
      app: kubernetes
  endpoints:
  - port: https
    interval: 30s
    path: /metrics
EOF

    log "Service monitors configured"
}

# Configure alert rules
configure_alerts() {
    log "Configuring alert rules..."

    kubectl apply -f - << EOF
apiVersion: monitoring.coreos.com/v1
kind: PrometheusRule
metadata:
  name: erlmcp-alerts
  namespace: monitoring
spec:
  groups:
  - name: erlmcp.rules
    rules:
    - alert: erlmcpPodDown
      expr: up{job="erlmcp"} == 0
      for: 1m
      labels:
        severity: critical
      annotations:
        summary: "erlmcp pod is down"
        description: "erlmcp pod {{ \$labels.pod }} has been down for more than 1 minute."

    - alert: erlmcpHighErrorRate
      expr: rate(http_requests_total{job="erlmcp", status=~"5.."}[5m]) / rate(http_requests_total{job="erlmcp"}[5m]) * 100 > 5
      for: 5m
      labels:
        severity: warning
      annotations:
        summary: "High error rate in erlmcp"
        description: "erlmcp error rate is {{ \$value }}% for 5 minutes."

    - alert: erlmcpHighResponseTime
      expr: histogram_quantile(0.95, rate(http_request_duration_seconds_bucket{job="erlmcp"}[5m])) > 1
      for: 5m
      labels:
        severity: warning
      annotations:
        summary: "High response time in erlmcp"
        description: "95th percentile response time is {{ \$value }} seconds for 5 minutes."
EOF

    log "Alert rules configured"
}

# Configure logging
configure_logging() {
    log "Configuring logging..."

    # Install Fluentd
    helm repo add fluent https://fluent.github.io/helm-charts
    helm repo update

    # Install Fluentd
    helm install fluentd fluent/fluentd \
        --namespace logging \
        --set image.tag=v1.16.2-debian \
        --set serviceAccount.create=true \
        --set serviceAccount.name=fluentd \
        --set configMap.name=fluentd-config \
        --set configMap.create=true

    # Configure Fluentd
    kubectl create configmap fluentd-config -n logging --from-file=fluentd.conf=/dev/stdin << EOF
<source>
  @type tail
  path /var/log/containers/*erlmcp*.log
  pos_file /var/log/fluentd-containers.log.pos
  tag erlmcp.*
  format json
  time_format %Y-%m-%dT%H:%M:%S.%NZ
</source>

<filter erlmcp.**>
  @type record_modifier
  <record>
    hostname \${hostname}
  </record>
</filter>

<match erlmcp.**>
  @type elasticsearch
  host elasticsearch-logging
  port 9200
  index_name erlmcp
  type_name _doc
  include_tag_key true
</match>

<match **>
  @type stdout
</match>
EOF

    # Create Elasticsearch and Kibana
    helm install elasticsearch elastic/elasticsearch \
        --namespace logging \
        --set replicas=3 \
        --set persistence.enabled=true \
        --set persistence.size=10Gi

    helm install kibana elastic/kibana \
        --namespace logging \
        --set service.type=LoadBalancer \
        --set elasticsearch.hosts=http://elasticsearch-logging:9200

    log "Logging configured"
}

# Configure network policies
configure_network_policies() {
    log "Configuring network policies..."

    kubectl apply -f - << EOF
apiVersion: networking.k8s.io/v1
kind: NetworkPolicy
metadata:
  name: erlmcp-network-policy
  namespace: $NAMESPACE
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
          name: monitoring
    - namespaceSelector:
        matchLabels:
          name: logging
    ports:
    - protocol: TCP
      port: 8080
    - protocol: TCP
      port: 8443
  egress:
  - to:
    - namespaceSelector:
        matchLabels:
          name: monitoring
    - namespaceSelector:
        matchLabels:
          name: logging
    ports:
    - protocol: TCP
      port: 9090
    - protocol: TCP
      port: 5432
EOF

    log "Network policies configured"
}

# Verify deployment
verify_deployment() {
    log "Verifying monitoring deployment..."

    # Check Prometheus pods
    kubectl get pods -n monitoring -l app.kubernetes.io/component=prometheus

    # Check Alertmanager pods
    kubectl get pods -n monitoring -l app.kubernetes.io=alertmanager

    # Check Grafana pod
    kubectl get pods -n monitoring -l app.kubernetes.io=grafana

    # Check Thanos pods
    kubectl get pods -n monitoring -l app.kubernetes.io/name=thanos

    # Check if services are accessible
    kubectl get svc -n monitoring | grep grafana
    kubectl get svc -n monitoring | grep thanos

    log "Monitoring deployment verified"
}

# Main execution
main() {
    log "Starting erlmcp v3 monitoring configuration..."

    validate_config
    install_prometheus
    install_alertmanager
    install_thanos
    install_grafana_dashboards
    configure_service_monitors
    configure_alerts
    configure_logging
    configure_network_policies
    verify_deployment

    log "Monitoring configuration completed!"
    echo "Grafana URL: $GRAFANA_DASHBOARD_URL"
}

# Run main function
main "$@"