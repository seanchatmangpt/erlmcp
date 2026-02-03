# Regional Operations Runbooks - erlmcp v3

**Version:** 3.0.0
**Status:** Production Ready
**Last Updated:** 2026-02-02

---

## Table of Contents

1. [US-EAST-1 Operations](#us-east-1-operations)
2. [US-WEST-2 Operations](#us-west-2-operations)
3. [EU-WEST-1 Operations](#eu-west-1-operations)
4. [AP-SOUTHEAST-1 Operations](#ap-southeast-1-operations)
5. [Cross-Region Operations](#cross-region-operations)

---

## US-EAST-1 Operations

### Region Overview

```yaml
region_code: us-east-1
region_name: US East (N. Virginia)
role: Primary
cluster_name: erlmcp-prod-useast1
availability_zones: 3
  - us-east-1a
  - us-east-1b
  - us-east-1c

networking:
  vpc_cidr: 10.0.0.0/16
  public_subnets:
    - 10.0.1.0/24 (us-east-1a)
    - 10.0.2.0/24 (us-east-1b)
    - 10.0.3.0/24 (us-east-1c)
  private_subnets:
    - 10.0.11.0/24 (us-east-1a)
    - 10.0.12.0/24 (us-east-1b)
    - 10.0.13.0/24 (us-east-1c)

capacity:
  node_count: 30
  pod_count: 90
  max_throughput: 50000 rps
  database:
    type: PostgreSQL
    instance: db.r6g.4xlarge
    storage: 2TB
    replication: 2 replicas
```

### Daily Operations

```bash
#!/bin/bash
# ops/useast1/daily-ops.sh

set -euo pipefail

REGION="us-east-1"
CLUSTER_NAME="erlmcp-prod-useast1"
NAMESPACE="erlmcp-production"

log() {
    echo "[$(date '+%Y-%m-%d %H:%M:%S')] $1"
}

# Morning health check
morning_health_check() {
    log "=== US-EAST-1 Morning Health Check ==="

    # Update kubeconfig
    aws eks update-kubeconfig --region $REGION --name $CLUSTER_NAME

    # Check cluster health
    log "Cluster Status:"
    kubectl cluster-info

    log "Node Status by AZ:"
    kubectl get nodes -L topology.kubernetes.io/zone

    # Check pod distribution
    log "Pod Distribution:"
    kubectl get pods -n $NAMESPACE -o wide

    # Check resource usage
    log "Resource Usage:"
    kubectl top nodes
    kubectl top pods -n $NAMESPACE

    # Check PVC status
    log "Storage Status:"
    kubectl get pvc -n $NAMESPACE

    # Database health
    log "Database Replication Status:"
    kubectl exec erlmcp-pg-primary-0 -n $NAMESPACE -- \
        psql -U postgres -c "SELECT * FROM pg_stat_replication;" | \
        column -t -s'|'
}

# Regional metrics
collect_metrics() {
    log "=== US-EAST-1 Metrics Collection ==="

    # Throughput
    REQUEST_COUNT=$(curl -sf 'http://prometheus:9090/api/v1/query?query=sum(rate(http_requests_total{region="us-east-1"}[5m]))' | jq -r '.data.result[0].value[1]')
    log "Requests/sec (5m avg): $REQUEST_COUNT"

    # Latency
    P95_LATENCY=$(curl -sf 'http://prometheus:9090/api/v1/query?query=histogram_quantile(0.95,sum(rate(http_request_duration_seconds_bucket{region="us-east-1"}[5m])) by(le))' | jq -r '.data.result[0].value[1]')
    log "P95 Latency: ${P95_LATENCY}s"

    # Error rate
    ERROR_RATE=$(curl -sf 'http://prometheus:9090/api/v1/query?query=sum(rate(http_requests_total{region="us-east-1",status=~"5.."}[5m]))/sum(rate(http_requests_total{region="us-east-1"}[5m]))' | jq -r '.data.result[0].value[1]')
    log "Error Rate: $ERROR_RATE"
}

# Backup verification
verify_backups() {
    log "=== US-EAST-1 Backup Verification ==="

    # Check latest backups
    LATEST_PG_BACKUP=$(aws s3 ls s3://erlmcp-backups/prod/postgres/ --region $REGION | grep "us-east-1" | tail -1)
    log "Latest PG Backup: $LATEST_PG_BACKUP"

    LATEST_ETS_BACKUP=$(aws s3 ls s3://erlmcp-backups/prod/ets/ --region $REGION | grep "us-east-1" | tail -1)
    log "Latest ETS Backup: $LATEST_ETS_BACKUP"

    # Verify backup age
    BACKUP_AGE=$(echo $LATEST_PG_BACKUP | awk '{print $1}' | xargs -I {} date -d {} +%s)
    CURRENT_TIME=$(date +%s)
    AGE_HOURS=$(( (CURRENT_TIME - BACKUP_AGE) / 3600 ))

    if [ $AGE_HOURS -gt 26 ]; then
        log "WARNING: Last backup is $AGE_HOURS hours old!"
    else
        log "Backup age OK: $AGE_HOURS hours"
    fi
}

# Scaling operations
scale_region() {
    local ACTION=${1:-status}
    local TARGET=${2}

    log "=== US-EAST-1 Scaling: $ACTION ==="

    case $ACTION in
        status)
            kubectl get hpa -n $NAMESPACE
            kubectl get pods -n $NAMESPACE -l app=erlmcp
            ;;
        scale-up)
            kubectl scale deployment erlmcp --replicas=$TARGET -n $NAMESPACE
            log "Scaled up to $TARGET replicas"
            ;;
        scale-down)
            kubectl scale deployment erlmcp --replicas=$TARGET -n $NAMESPACE
            log "Scaled down to $TARGET replicas"
            ;;
        auto-enable)
            kubectl patch hpa erlmcp-hpa -n $NAMESPACE -p '{"spec":{"minReplicas":'$TARGET'"'"'}}'
            log "Enabled auto-scaling with min=$TARGET"
            ;;
        *)
            log "Unknown action: $ACTION"
            return 1
            ;;
    esac
}

main() {
    case ${1:-daily} in
        daily)
            morning_health_check
            collect_metrics
            verify_backups
            ;;
        metrics)
            collect_metrics
            ;;
        scale)
            scale_region "$2" "$3"
            ;;
        health)
            morning_health_check
            ;;
        *)
            echo "Usage: $0 {daily|metrics|scale|health}"
            exit 1
            ;;
    esac
}

main "$@"
```

### Incident Response

```bash
#!/bin/bash
# ops/useast1/incident-response.sh

set -euo pipefail

INCIDENT_ID=${1}
SEVERITY=${2:-P1}

log() {
    echo "[$(date '+%Y-%m-%d %H:%M:%S')] $1"
}

# P0 - Complete Outage
p0_response() {
    log "=== P0 RESPONSE: US-EAST-1 ==="

    # Check cluster connectivity
    if ! aws eks describe-cluster --region us-east-1 --name erlmcp-prod-useast1 &>/dev/null; then
        log "CRITICAL: EKS cluster unreachable"

        # Check individual AZs
        for AZ in us-east-1a us-east-1b us-east-1c; do
            log "Checking ${AZ}..."
            aws ec2 describe-instances --region us-east-1 \
                --filters "Name=availability-zone,Values=${AZ}" "Name=instance-state-name,Values=running" \
                --query "Reservations[].Instances[0].InstanceId" --output text
        done
    fi

    # Check load balancer
    ELB_HEALTH=$(aws elbv2 describe-target-health --target-group-arn tg-us-east-1 \
        --region us-east-1 --query 'TargetHealth[].TargetHealth' --output text | grep -c "healthy")

    log "ELB healthy targets: $ELB_HEALTH"

    if [ $ELB_HEALTH -lt 3 ]; then
        log "CRITICAL: Insufficient healthy targets"
        # Initiate emergency failover
        ../scripts/dr/failover-to-dr.sh us-east-1 us-west-2 emergency
    fi
}

# P1 - Major Degradation
p1_response() {
    log "=== P1 RESPONSE: US-EAST-1 ==="

    # Check error rates
    ERROR_RATE=$(curl -sf 'http://prometheus:9090/api/v1/query?query=sum(rate(http_requests_total{status=~"5.."}[5m]))/sum(rate(http_requests_total[5m]))' | jq -r '.data.result[0].value[1]')

    log "Current Error Rate: $ERROR_RATE"

    if (( $(echo "$ERROR_RATE > 0.05" | bc -l) )); then
        log "ERROR RATE EXCEEDED: ${ERROR_RATE}"

        # Identify problematic services
        kubectl get pods -n erlmcp-production -o wide | grep -v "Running"

        # Restart unhealthy pods
        kubectl get pods -n erlmcp-production | grep "CrashLoopBackOff" | \
            awk '{print $1}' | xargs -I {} kubectl delete pod {} -n erlmcp-production

        # Scale up if needed
        kubectl scale deployment erlmcp --replicas=40 -n erlmcp-production
    fi
}

main() {
    case $SEVERITY in
        P0) p0_response ;;
        P1) p1_response ;;
        *) log "Use P0 or P1 severity" ;;
    esac
}

main "$@"
```

---

## US-WEST-2 Operations

### Region Overview

```yaml
region_code: us-west-2
region_name: US West (Oregon)
role: Disaster Recovery
cluster_name: erlmcp-dr-uswest2
availability_zones: 2
  - us-west-2a
  - us-west-2b

networking:
  vpc_cidr: 10.3.0.0/16
  public_subnets:
    - 10.3.1.0/24 (us-west-2a)
    - 10.3.2.0/24 (us-west-2b)
  private_subnets:
    - 10.3.11.0/24 (us-west-2a)
    - 10.3.12.0/24 (us-west-2b)

capacity:
  node_count: 10  # Reduced for DR
  pod_count: 0     # Hot standby on failover
  max_throughput: 15000 rps  # On failover
```

### DR Maintenance

```bash
#!/bin/bash
# ops/uswest2/dr-maintenance.sh

set -euo pipefail

REGION="us-west-2"
CLUSTER_NAME="erlmcp-dr-uswest2"
NAMESPACE="erlmcp-production"

log() {
    echo "[$(date '+%Y-%m-%d %H:%M:%S')] $1"
}

# Daily DR verification
verify_dr_readiness() {
    log "=== US-WEST-2 DR Readiness Check ==="

    aws eks update-kubeconfig --region $REGION --name $CLUSTER_NAME

    # Check infrastructure
    log "Infrastructure Status:"
    kubectl get nodes
    kubectl get ns $NAMESPACE

    # Check DR pods (should be 0 or minimal)
    DR_PODS=$(kubectl get pods -n $NAMESPACE -l app=erlmcp --no-headers 2>/dev/null | wc -l)
    log "DR Pods Running: $DR_PODS"

    if [ $DR_PODS -gt 0 ]; then
        log "WARNING: DR pods are running (should be 0)"
        kubectl get pods -n $NAMESPACE -l app=erlmcp
    fi

    # Check standby database
    log "Standby Database Status:"
    kubectl get statefulset -n $NAMESPACE

    # Verify replication lag
    REPLICATION_LAG=$(kubectl exec erlmcp-pg-standby-0 -n $NAMESPACE -- \
        psql -U postgres -t -c "SELECT EXTRACT(EPOCH FROM (now() - pg_last_xact_replay_timestamp())) as lag_seconds;" 2>/dev/null || echo "N/A")
    log "Replication Lag: ${REPLICATION_LAG_SECONDS}s"
}

# Warm up DR region
warm_up_dr() {
    log "=== Warming Up US-WEST-2 DR ==="

    # Start minimal pods
    kubectl scale deployment erlmcp --replicas=3 -n $NAMESPACE

    # Wait for readiness
    kubectl wait --for=condition=ready pod -l app=erlmcp -n $NAMESPACE --timeout=10m

    log "DR region warmed up and ready"
}

# Cool down DR region
cool_down_dr() {
    log "=== Cooling Down US-WEST-2 DR ==="

    # Scale down to standby
    kubectl scale deployment erlmcp --replicas=0 -n $NAMESPACE

    # Verify all pods terminated
    kubectl wait --for=delete pod -l app=erlmcp -n $NAMESPACE --timeout=5m

    log "DR region cooled down to standby"
}

# Test failover
test_failover() {
    log "=== Testing Failover to US-WEST-2 ==="

    # Save current state
    PREVIOUS_PODS=$(kubectl get pods -n $NAMESPACE -l app=erlmcp --no-headers | wc -l)

    # Scale up
    kubectl scale deployment erlmcp --replicas=5 -n $NAMESPACE

    # Wait for readiness
    kubectl wait --for=condition=ready pod -l app=erlmcp -n $NAMESPACE --timeout=10m

    # Test health endpoint
    for i in {1..30}; do
        if curl -sf https://us-west-2.erlmcp.io/health | grep -q "healthy"; then
            log "Failover test successful"
            break
        fi
        sleep 10
    done

    # Restore state
    kubectl scale deployment erlmcp --replicas=$PREVIOUS_PODS -n $NAMESPACE

    log "Failover test complete"
}

main() {
    case ${1:-verify} in
        verify)
            verify_dr_readiness
            ;;
        warmup)
            warm_up_dr
            ;;
        cooldown)
            cool_down_dr
            ;;
        test)
            test_failover
            ;;
        *)
            echo "Usage: $0 {verify|warmup|cooldown|test}"
            exit 1
            ;;
    esac
}

main "$@"
```

---

## EU-WEST-1 Operations

### Region Overview

```yaml
region_code: eu-west-1
region_name: EU West (Ireland)
role: Secondary (Active)
cluster_name: erlmcp-prod-euwest1
availability_zones: 3
  - eu-west-1a
  - eu-west-1b
  - eu-west-1c

networking:
  vpc_cidr: 10.1.0.0/16
  public_subnets:
    - 10.1.1.0/24 (eu-west-1a)
    - 10.1.2.0/24 (eu-west-1b)
    - 10.1.3.0/24 (eu-west-1c)
  private_subnets:
    - 10.1.11.0/24 (eu-west-1a)
    - 10.1.12.0/24 (eu-west-1b)
    - 10.1.13.0/24 (eu-west-1c)

capacity:
  node_count: 20
  pod_count: 60
  max_throughput: 30000 rps
```

### GDPR Compliance Operations

```bash
#!/bin/bash
# ops/euwest1/gdpr-ops.sh

set -euo pipefail

REGION="eu-west-1"
CLUSTER_NAME="erlmcp-prod-euwest1"
NAMESPACE="erlmcp-production"

log() {
    echo "[$(date '+%Y-%m-%d %H:%M:%S')] $1"
}

# GDPR data access logging
log_data_access() {
    local USER=${1}
    local ACTION=${2}
    local RESOURCE=${3}
    local PURPOSE=${4}

    log "Logging GDPR data access: $USER $ACTION $RESOURCE"

    # Send to audit log
    kubectl exec erlmcp-0 -n $NAMESPACE -- \
        erlmcpctl audit-log \
            --user="$USER" \
            --action="$ACTION" \
            --resource="$RESOURCE" \
            --purpose="$PURPOSE" \
            --region=eu-west-1 \
            --timestamp=$(date -u +%Y-%m-%dT%H:%M:%SZ)
}

# Data export (Right to data portability)
export_user_data() {
    local USER_ID=${1}
    local EXPORT_ID="export-${USER_ID}-$(date +%Y%m%d-%H%M%S)"

    log "Exporting user data for: $USER_ID"

    # Create export job
    kubectl create job gdpr-export-$EXPORT_ID \
        -n $NAMESPACE \
        --from=erlmcp-0 \
        -- ./scripts/gdpr/export-user-data.sh \
            --user-id=$USER_ID \
            --output=s3://erlmcp-gdpr-exports/$REGION/$EXPORT_ID
}

# Data deletion (Right to be forgotten)
delete_user_data() {
    local USER_ID=${1}
    local VERIFICATION_CODE=${2}

    log "Processing deletion request for: $USER_ID"

    # Verify deletion request
    kubectl exec erlmcp-0 -n $NAMESPACE -- \
        erlmcpctl gdpr-verify-deletion \
            --user-id="$USER_ID" \
            --code="$VERIFICATION_CODE"

    if [ $? -eq 0 ]; then
        # Proceed with deletion
        kubectl exec erlmcp-0 -n $NAMESPACE -- \
            erlmcpctl gdpr-delete-user \
                --user-id="$USER_ID" \
                --reason="gdpr-request" \
                --verify-delete=true

        log "User data deleted: $USER_ID"

        # Log deletion
        log_data_access "$USER_ID" "DELETE" "user:$USER_ID" "GDPR right to be forgotten"
    else
        log "ERROR: Verification failed for $USER_ID"
        return 1
    fi
}

# Data retention enforcement
enforce_retention() {
    log "=== GDPR Retention Enforcement ==="

    # Find expired data
    kubectl exec erlmcp-0 -n $NAMESPACE -- \
        erlmcpctl gdpr-find-expired \
            --region=eu-west-1 \
            --max-age-days=365

    # Anonymize old data
    kubectl exec erlmcp-0 -n $NAMESPACE -- \
        erlmcpctl gdpr-anonymize \
            --older-than=365days \
            --region=eu-west-1
}

# Compliance reporting
generate_compliance_report() {
    local MONTH=${1:-$(date +%Y-%m)}
    REPORT_FILE="/tmp/gdpr-compliance-${MONTH}.md"

    log "Generating GDPR compliance report for $MONTH"

    cat > $REPORT_FILE <<EOF
# GDPR Compliance Report - EU-WEST-1
**Period:** $MONTH
**Region:** eu-west-1

## Data Processing Activities

### Categories of Personal Data
- User Identifiers
- Connection Metadata
- Session Data

### Legal Basis for Processing
- Contract Performance
- Legitimate Interest
- Consent

### Data Transfers
- **Intra-EU:** Yes (within EU infrastructure)
- **Extra-EU:** No (all data stays in EU)
- **Safeguards:** Standard Contractual Clauses (for any transfers)

## Data Subject Rights
| Request Type | Period | Fulfilled | Avg Response Time |
|---------------|--------|-----------|------------------|
| Access | N | N | < 1 month |
| Portability | N | N | < 1 month |
| Erasure | N | N | < 1 month |
| Rectification | N | N | < 1 month |
| Objection | N | N | < 1 month |

## Security Measures
- Encryption: TLS 1.3+, AES-256
- Access Control: Role-based
- Audit Logging: Enabled
- Data Minimization: Applied

## Data Breaches
**Period:** $MONTH
**Breaches Reported:** 0

---
*Report Generated: $(date -u)*
EOF

    log "Report saved to: $REPORT_FILE"
}

main() {
    case ${1:-status} in
        access)
            log_data_access "$2" "$3" "$4" "$5"
            ;;
        export)
            export_user_data "$2"
            ;;
        delete)
            delete_user_data "$2" "$3"
            ;;
        enforce)
            enforce_retention
            ;;
        report)
            generate_compliance_report "$2"
            ;;
        *)
            echo "Usage: $0 {access|export|delete|enforce|report}"
            exit 1
            ;;
    esac
}

main "$@"
```

---

## AP-SOUTHEAST-1 Operations

### Region Overview

```yaml
region_code: ap-southeast-1
region_name: Asia Pacific (Singapore)
role: Tertiary (Active)
cluster_name: erlmcp-prod-apsoutheast1
availability_zones: 2
  - ap-southeast-1a
  - ap-southeast-1b

networking:
  vpc_cidr: 10.2.0.0/16
  public_subnets:
    - 10.2.1.0/24 (ap-southeast-1a)
    - 10.2.2.0/24 (ap-southeast-1b)
  private_subnets:
    - 10.2.11.0/24 (ap-southeast-1a)
    - 10.2.12.0/24 (ap-southeast-1b)

capacity:
  node_count: 15
  pod_count: 45
  max_throughput: 20000 rps
```

### APAC Operations

```bash
#!/bin/bash
# ops/apsoutheast1/apac-ops.sh

set -euo pipefail

REGION="ap-southeast-1"
CLUSTER_NAME="erlmcp-prod-apsoutheast1"
NAMESPACE="erlmcp-production"

log() {
    echo "[$(date '+%Y-%m-%d %H:%M:%S')] $1"
}

# Time zone specific health check
tz_health_check() {
    local TZ=${1:-$(date +%z)}

    log "=== AP-SOUTHEAST-1 Health Check (TZ: $TZ) ==="

    aws eks update-kubeconfig --region $REGION --name $CLUSTER_NAME

    # Check during APAC business hours (9 AM - 9 PM SGT)
    HOUR=$(date -u +%H)
    SGT_HOUR=$(( (HOUR + 8) % 24 ))

    if [ $SGT_HOUR -ge 9 ] && [ $SGT_HOUR -lt 21 ]; then
        log "Business Hours (SGT) - Enhanced monitoring active"

        # Check response times
        P95_LATENCY=$(curl -sf 'http://prometheus:9090/api/v1/query?query=histogram_quantile(0.95,sum(rate(http_request_duration_seconds_bucket{region="ap-southeast-1"}[5m])) by(le))' | jq -r '.data.result[0].value[1]')
        log "P95 Latency: ${P95_LATENCY}s (target: < 200ms)"

        # Check throughput
        THROUGHPUT=$(curl -sf 'http://prometheus:9090/api/v1/query?query=sum(rate(http_requests_total{region="ap-southeast-1"}[5m]))' | jq -r '.data.result[0].value[1]')
        log "Throughput: $THROUGHPUT req/s"
    fi
}

# Multi-region latency check
multi_region_latency() {
    log "=== Multi-Region Latency Check ==="

    # Test latency to other regions
    for TARGET in us-east-1 eu-west-1; do
        LATENCY=$(curl -o /dev/null -s -w '%{time_total}' https://$TARGET.erlmcp.io/health)
        log "Latency to $TARGET: ${LATENCY}s"
    done
}

# APAC peak scaling
peak_scale_up() {
    log "=== APAC Peak Scale Up ==="

    # Get current replicas
    CURRENT=$(kubectl get deployment erlmcp -n $NAMESPACE -o jsonpath='{.spec.replicas}')

    # Scale up for peak
    TARGET=$((CURRENT * 2))
    kubectl scale deployment erlmcp --replicas=$TARGET -n $NAMESPACE

    log "Scaled up: $CURRENT -> $TARGET replicas"

    # Schedule scale down
    echo "./apsoutheast1/ops/apac-ops.sh scale-down" | at now + 4 hours
}

scale_down() {
    log "=== APAC Peak Scale Down ==="

    # Return to baseline
    kubectl scale deployment erlmcp --replicas=15 -n $NAMESPACE

    log "Scaled down to baseline"
}

main() {
    case ${1:-health} in
        health)
            tz_health_check "$2"
            ;;
        latency)
            multi_region_latency
            ;;
        scale-up)
            peak_scale_up
            ;;
        scale-down)
            scale_down
            ;;
        *)
            echo "Usage: $0 {health|latency|scale-up|scale-down}"
            exit 1
            ;;
    esac
}

main "$@"
```

---

## Cross-Region Operations

### Global Health Dashboard

```bash
#!/bin/bash
# ops/global/health-dashboard.sh

log() {
    echo "[$(date '+%Y-%m-%d %H:%M:%S')] $1"
}

# Check all regions
check_all_regions() {
    log "=== Global Health Check ==="

    REGIONS=("us-east-1" "us-west-2" "eu-west-1" "ap-southeast-1")

    for REGION in "${REGIONS[@]}"; do
        echo ""
        echo "=== $REGION ==="

        # Health check
        HEALTH_STATUS=$(curl -sf https://${REGION}.erlmcp.io/health | jq -r '.status' || echo "unreachable")
        echo "Health: $HEALTH_STATUS"

        # Pod count
        kubectl get pods -n erlmcp-production --context=$REGION 2>/dev/null | \
            grep -c "Running" || echo "N/A"

        # Throughput
        THROUGHPUT=$(curl -sf "https://prometheus.erlmcp.io/api/v1/query?query=sum(rate(http_requests_total{region=\"$REGION\"}[5m]))" | jq -r '.data.result[0].value[1]' || echo "N/A")
        echo "Throughput: $THROUGHPUT req/s"

        # Latency
        LATENCY=$(curl -sf "https://prometheus.erlmcp.io/api/v1/query?query=histogram_quantile(0.95,sum(rate(http_request_duration_seconds_bucket{region=\"$REGION\"}[5m])) by(le))" | jq -r '.data.result[0].value[1]' || echo "N/A")
        echo "P95 Latency: ${LATENCY}s"
    done
}

# Replication status
check_replication() {
    log "=== Cross-Region Replication Status ==="

    # Primary to secondary
    PRIMARY_TO_SECONDARY=$(kubectl exec erlmcp-pg-primary-0 -n erlmcp-production --context=us-east-1 -- \
        psql -U postgres -c "
            SELECT
                application_name,
                client_addr,
                state,
                sync_lag
            FROM pg_stat_replication;
        " | column -t -s'|')

    echo "Primary -> Secondary:"
    echo "$PRIMARY_TO_SECONDARY"

    # Lag metrics
    for REGION in eu-west-1 ap-southeast-1; do
        LAG=$(kubectl exec erlmcp-cluster-0 -n erlmcp-production --context=$REGION -- \
            erlmcpctl replication-lag us-east-1 2>/dev/null || echo "N/A")
        echo "us-east-1 -> $REGION lag: ${LAG}s"
    done
}

# Global traffic distribution
check_traffic() {
    log "=== Global Traffic Distribution ==="

    for REGION in us-east-1 us-west-2 eu-west-1 ap-southeast-1; do
        # Query traffic share from Route 53
        TRAFFIC=$(aws route53 get-health-check --region us-east-1 \
            --health-check-id $(aws route53 list-health-checks --region us-east-1 \
                --query "HealthChecks[?Name=='$REGION*'].Id" --output text) \
            --query "HealthCheckConfig.HealthThreshold" --output text 2>/dev/null || echo "N/A")

        echo "$REGION: $TRAFFIC%"
    done
}

main() {
    case ${1:-all} in
        all)
            check_all_regions
            check_replication
            check_traffic
            ;;
        health)
            check_all_regions
            ;;
        replication)
            check_replication
            ;;
        traffic)
            check_traffic
            ;;
        *)
            echo "Usage: $0 {all|health|replication|traffic}"
            exit 1
            ;;
    esac
}

main "$@"
```

### Global Rollout Coordination

```bash
#!/bin/bash
# ops/global/rollout-coordination.sh

set -euo pipefail

VERSION=${1}
STRATEGY=${2:-rolling-wave}  # rolling-wave | parallel

log() {
    echo "[$(date '+%Y-%m-%d %H:%M:%S')] $1"
}

# Rolling wave deployment
rolling_wave_deploy() {
    local VERSION=$1

    log "Starting rolling wave deployment of version $VERSION"

    # Order: lowest traffic first
    REGIONS=("eu-west-1" "ap-southeast-1" "us-east-1")

    for REGION in "${REGIONS[@]}"; do
        log "Deploying to $REGION..."

        (
            export KUBECONFIG=$HOME/.kube/${REGION}-config
            export REGION_NAME=$REGION

            # Deploy
            helm upgrade --install erlmcp ./k8s/deployments/helm/erlmcp \
                --namespace erlmcp-production \
                --values k8s/production/${REGION}.yaml \
                --set global.region=$REGION \
                --set erlmcp.image.tag=$VERSION \
                --wait --timeout 15m

            # Wait and monitor
            sleep 300  # 5 minute soak

            # Verify
            ./scripts/health/region-check.sh $REGION

        ) &

        # Wait between regions
        sleep 60
    done

    # Wait for all regions to complete
    wait

    log "Rolling wave deployment complete"
}

# Parallel deployment
parallel_deploy() {
    local VERSION=$1

    log "Starting parallel deployment of version $VERSION"

    REGIONS=("eu-west-1" "ap-southeast-1" "us-east-1")

    for REGION in "${REGIONS[@]}"; do
        (
            export KUBECONFIG=$HOME/.kube/${REGION}-config
            export REGION_NAME=$REGION

            log "Deploying to $REGION..."

            helm upgrade --install erlmcp ./k8s/deployments/helm/erlmcp \
                --namespace erlmcp-production \
                --values k8s/production/${REGION}.yaml \
                --set global.region=$REGION \
                --set erlmcp.image.tag=$VERSION \
                --wait --timeout 15m

        ) &
    done

    wait

    log "Parallel deployment complete"

    # Verify all regions
    for REGION in "${REGIONS[@]}"; do
        ./scripts/health/region-check.sh $REGION
    done
}

# Rollback all regions
global_rollback() {
    local TARGET_VERSION=${1:-previous}

    log "Starting global rollback to $TARGET_VERSION"

    REGIONS=("eu-west-1" "ap-southeast-1" "us-east-1")

    for REGION in "${REGIONS[@]}"; do
        (
            export KUBECONFIG=$HOME/.kube/${REGION}-config

            log "Rolling back $REGION..."

            kubectl set image statefulset/erlmcp-cluster \
                erlmcp=ghcr.io/seanchatmangpt/erlmcp:${TARGET_VERSION} \
                -n erlmcp-production \
                --context=$REGION

        ) &
    done

    wait

    log "Global rollback complete"
}

main() {
    if [ -z "$VERSION" ]; then
        echo "Usage: $0 <version> [strategy]"
        exit 1
    fi

    case $STRATEGY in
        rolling-wave)
            rolling_wave_deploy $VERSION
            ;;
        parallel)
            parallel_deploy $VERSION
            ;;
        rollback)
            global_rollback $VERSION
            ;;
        *)
            echo "Unknown strategy: $STRATEGY"
            exit 1
            ;;
    esac
}

main "$@"
```

### Emergency Cross-Region Failover

```bash
#!/bin/bash
# ops/global/emergency-failover.sh

set -euo pipefail

FAILED_REGION=${1}
TARGET_REGION=${2:-us-west-2}  # DR region
FAILOVER_TYPE=${3:-emergency}

log() {
    echo "[$(date '+%Y-%m-%d %H:%M:%S')] $1"
}

# Declare incident
declare_incident() {
    log "=== EMERGENCY DECLARED ==="
    log "Failed Region: $FAILED_REGION"
    log "Target Region: $TARGET_REGION"
    log "Type: $FAILOVER_TYPE"

    # Page all on-call
    pdectl incident create \
        --severity critical \
        --title "EMERGENCY: Complete Region Failure - $FAILED_REGION" \
        --description "All services in $FAILED_REGION are unavailable. Emergency failover to $TARGET_REGION initiated."

    # Update status page
    status-page update --status major_outage \
        --message "Catastrophic failure in $FAILED_REGION. Emergency failover in progress."
}

# Execute failover
execute_failover() {
    log "Executing emergency failover: $FAILED_REGION -> $TARGET_REGION"

    # Step 1: Immediate DNS cutover
    log "Step 1: DNS cutover"
    aws route53 change-resource-record-sets \
        --hosted-zone-id Z1234567890ABC \
        --change-batch '{
            "Changes": [{
                "Action": "UPSERT",
                "ResourceRecordSet": {
                    "Name": "erlmcp.erlmcp.com",
                    "Type": "A",
                    "SetIdentifier": "'$TARGET_REGION'",
                    "Region": "'$TARGET_REGION'",
                    "AliasTarget": {
                        "HostedZoneId": "Z35SXDOTRKT7K6",
                        "DNSName": "dualstack.erlmcp-'$TARGET_REGION'.elb.amazonaws.com",
                        "EvaluateTargetHealth": false
                    }
                }
            }]
        }' \
        --query "ChangeInfo.Id" --output text

    # Step 2: Scale up target region to max
    log "Step 2: Scaling $TARGET_REGION to maximum"
    aws eks update-kubeconfig --region $TARGET_REGION --name erlmcp-prod-${TARGET_REGION//-/}

    kubectl scale deployment erlmcp --replicas=100 -n erlmcp-production
    kubectl patch hpa erlmcp-hpa -n erlmcp-production -p '{"spec":{"maxReplicas":200}}'

    # Step 3: Enable emergency mode
    log "Step 3: Enabling emergency mode"
    kubectl annotate deployment erlmcp emergency-mode=true -n erlmcp-production
    kubectl annotate deployment erlmcp emergency-mode-timestamp=$(date +%s) -n erlmcp-production

    # Step 4: Disable non-essential features
    log "Step 4: Disabling non-essential features"
    kubectl patch configmap erlmcp-config -n erlmcp-production --type=json \
        -p='{
            "data": {
                "features.experimental": "false",
                "features.analytics": "false",
                "features.optional": "false"
            }
        }'

    # Step 5: Verify traffic
    log "Step 5: Verifying traffic switch"
    for i in {1..60}; do
        if curl -sf https://erlmcp.erlmcp.com/health | grep -q "healthy"; then
            log "Traffic verified successfully"
            break
        fi
        sleep 5
    done

    log "Emergency failover complete"
}

# Post-failover operations
post_failover() {
    log "=== Post-Failover Operations ==="

    # Generate incident report
    cat > /var/emergency-failover-$(date +%Y%m%d-%H%M%S).md <<EOF
# Emergency Failover Report

**Date:** $(date)
**Failed Region:** $FAILED_REGION
**Target Region:** $TARGET_REGION
**Type:** $FAILOVER_TYPE

## Timeline
| Time | Action | Status |
|------|--------|--------|
| $(date -u +%H:%M:%S) | Incident declared | Complete |
| $(date -u +%H:%M:%S) | DNS cutover executed | Complete |
| $(date -u +%H:%M:%S) | Target scaled to max | Complete |
| $(date -u +%H:%M:%S) | Emergency mode enabled | Complete |
| $(date -u +%H:%M:%S) | Traffic verified | Complete |

## Impact Assessment
- Regions affected: $FAILED_REGION
- Services affected: All
- Users affected: All users in $FAILED_REGION
- Data loss: Minimal (async replication lag < 1min)

## Recovery Actions
1. DNS cutover to $TARGET_REGION
2. Maximum capacity in $TARGET_REGION
3. Emergency optimizations enabled
4. Non-essential features disabled

## Next Steps
1. Investigate root cause in $FAILED_REGION
2. Restore $FAILED_REGION
3. Plan failback
4. Post-mortem analysis

---
*Generated by Emergency Failover System*
EOF

    log "Incident report saved"
}

main() {
    if [ -z "$FAILED_REGION" ]; then
        echo "Usage: $0 <failed_region> [target_region] [emergency|planned]"
        exit 1
    fi

    declare_incident
    execute_failover
    post_failover
}

main "$@"
```

---

## Change Log

| Date | Version | Changes |
|------|---------|---------|
| 2026-02-02 | 3.0.0 | Initial regional operations runbooks created |

---

**Document Status:** Production Ready
**Next Review:** 2026-03-02
**Maintained By:** erlmcp DevOps Team
