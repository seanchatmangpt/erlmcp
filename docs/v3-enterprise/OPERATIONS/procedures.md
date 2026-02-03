# Standard Operating Procedures (SOPs)

## Overview

This document provides detailed standard operating procedures for erlmcp v3 deployment, monitoring, maintenance, and troubleshooting. These procedures are designed for enterprise operations teams to ensure consistent, reliable service.

## Table of Contents
1. [Daily Operations](#daily-operations)
2. [Weekly Operations](#weekly-operations)
3. [Monthly Operations](#monthly-operations)
4. [Incident Response](#incident-response)
5. [Change Management](#change-management)
6. [Capacity Planning](#capacity-planning)
7. [Disaster Recovery](#disaster-recovery)

## Daily Operations

### 1. Morning Health Check

**Time**: 9:00 AM

**Procedure**:
```bash
#!/bin/bash
# daily_health_check.sh

# Check cluster status
kubectl cluster-info
kubectl get nodes

# Check erlmcp pods
kubectl get pods -n erlmcp -l app=erlmcp

# Check service endpoints
kubectl get svc -n erlmcp
kubectl get ingress -n erlmcp

# Check resource usage
kubectl top nodes
kubectl top pods -n erlmcp

# Check log errors
kubectl logs -n erlmcp deployment/erlmcp --since=1h | grep -i "error" || true

# Check application health
curl -f https://erlmcp.company.com/health || alert "Health check failed"

# Check metrics
curl -s https://erlmcp.company.com/metrics/prometheus | grep -E "(erlmcp_connections_total|erlmcp_requests_total)"
```

**Expected Output**:
- All pods should be Running and Ready
- < 5% CPU and memory usage spikes
- No critical errors in logs
- Health endpoint returns "healthy"
- Metrics within normal ranges

**Actions Required**:
- Investigate any pod restarts or failures
- Alert if resource usage > 80%
- Alert if error rate > 1%

### 2. Performance Monitoring

**Time**: 10:00 AM

**Procedure**:
```bash
#!/bin/bash
# performance_check.sh

# Get performance metrics
curl -s https://erlmcp.company.com/metrics/prometheus > /tmp/metrics.prom

# Check response times
curl -s -o /dev/null -w "%{time_total}\n" https://erlmcp.company.com/resources

# Check concurrent connections
grep "erlmcp_connections_total" /tmp/metrics.prom

# Check request rates
grep "erlmcp_requests_total" /tmp/metrics.prom

# Check error rates
grep "erlmcp_errors_total" /tmp/metrics.prom

# Check queue lengths
grep "erlmcp_queue_length" /tmp/metrics.prom
```

**Thresholds**:
- Response time: < 100ms
- Connections: < 80% of capacity
- Error rate: < 0.5%
- Queue length: < 50% of capacity

### 3. Security Audit

**Time**: 11:00 AM

**Procedure**:
```bash
#!/bin/bash
# security_audit.sh

# Check for unauthorized access attempts
kubectl logs -n erlmcp deployment/erlmcp --since=1h | grep "auth.failure"

# Check SSL certificate validity
openssl s_client -connect erlmcp.company.com:443 -showcerts </dev/null 2>/dev/null | openssl x509 -noout -dates

# Check firewall rules
sudo iptables -L -n | grep -E "(80|443)"

# Check system security patches
sudo apt list --upgradable 2>/dev/null || yum check-update 2>/dev/null

# Check security configurations
kubectl get configmap -n erlmcp erlmcp-config -o jsonpath='{.data.ssl\.conf}' | grep -E "(cipher|protocol)"
```

## Weekly Operations

### 1. System Patch Management

**Day**: Monday, 2:00 AM

**Procedure**:
```bash
#!/bin/bash
# weekly_patches.sh

# Create maintenance window notification
send_notification "Starting system patches - maintenance window 2AM-4AM"

# Backup critical systems
backup_erlmcp_deployment

# Apply OS patches
sudo apt update && sudo apt upgrade -y

# Restart affected services
sudo systemctl restart erlmcp

# Verify deployment
verify_deployment_health

# Run regression tests
run_regression_tests

# Send completion notification
send_notification "System patches completed"
```

### 2. Performance Optimization

**Day**: Tuesday, 10:00 AM

**Procedure**:
```bash
#!/bin/bash
# performance_optimization.sh

# Analyze performance metrics
analyze_performance_metrics

# Check resource utilization
kubectl top pods --sort-by=cpu -n erlmcp

# Identify hot paths
find_hot_paths_in_code

# Optimize queries
optimize_database_queries

# Tune JVM parameters
tune_jvm_settings

# Apply optimizations
apply_performance_patches

# Validate improvements
validate_performance_improvements
```

### 3. Capacity Planning Review

**Day**: Wednesday, 11:00 AM

**Procedure**:
```bash
#!/bin/bash
# capacity_planning.sh

# Gather capacity metrics
gather_capacity_metrics

# Project future needs
project_future_needs

# Analyze trends
analyze_usage_trends

# Calculate required resources
calculate_required_resources

# Review scaling limits
review_scaling_limits

# Update capacity plan
update_capacity_plan

# Present findings
present_capacity_report
```

### 4. Backup Verification

**Day**: Thursday, 2:00 PM

**Procedure**:
```bash
#!/bin/bash
# backup_verification.sh

# List recent backups
list_recent_backups

# Test backup integrity
test_backup_integrity

# Restore test environment
restore_test_environment

# Verify data consistency
verify_data_consistency

# Document results
document_backup_results

# Update backup status
update_backup_status
```

### 5. Configuration Management

**Day**: Friday, 3:00 PM

**Procedure**:
```bash
#!/bin/bash
# config_management.sh

# Audit current configurations
audit_current_configs

# Review configuration changes
review_config_changes

# Update documentation
update_documentation

# Apply approved changes
apply_config_changes

# Validate configurations
validate_configurations

# Archive old configs
archive_old_configs
```

## Monthly Operations

### 1. Disaster Recovery Test

**Day**: First Monday, 10:00 PM - 2:00 AM

**Procedure**:
```bash
#!/bin/bash
# dr_test.sh

# Setup test environment
setup_test_environment

# Execute failover procedures
execute_failover_procedures

# Test data recovery
test_data_recovery

# Verify service restoration
verify_service_restoration

# Performance validation
performance_validation

# Document results
document_dr_test_results

# Cleanup test environment
cleanup_test_environment
```

### 2. Security Compliance Audit

**Day**: Third Tuesday, 2:00 PM

**Procedure**:
```bash
#!/bin/bash
# security_audit.sh

# Run security scans
run_security_scans

# Check compliance requirements
check_compliance_requirements

# Review access controls
review_access_controls

# Update security policies
update_security_policies

# Test incident response
test_incident_response

# Generate compliance report
generate_compliance_report
```

### 3. Performance Benchmark

**Day**: Last Friday, 10:00 AM

**Procedure**:
```bash
#!/bin/bash
# benchmark.sh

# Setup benchmark environment
setup_benchmark_environment

# Run baseline benchmarks
run_baseline_benchmarks

# Execute load tests
execute_load_tests

# Analyze results
analyze_benchmark_results

# Generate report
generate_benchmark_report

# Archive benchmark data
archive_benchmark_data
```

### 4. Documentation Update

**Day**: Last Friday, 2:00 PM

**Procedure**:
```bash
#!/bin/bash
# doc_update.sh

# Review documentation
review_documentation

# Update procedures
update_procedures

# Add new guides
add_new_guides

# Update contact lists
update_contact_lists

# Review runbooks
review_runbooks

# Archive old versions
archive_old_versions
```

### 5. Hardware Maintenance

**Day**: As needed

**Procedure**:
```bash
#!/bin/bash
# hardware_maintenance.sh

# Schedule maintenance window
schedule_maintenance_window

# Notify stakeholders
notify_stakeholders

# Perform hardware checks
perform_hardware_checks

# Replace/upgrade hardware
replace_upgrade_hardware

# Test functionality
test_functionality

# Document maintenance
document_maintenance
```

## Incident Response

### Incident Classification

| Level | Severity | Response Time | Description |
|-------|----------|---------------|-------------|
| P0 | Critical | 15 minutes | Service down, affecting all users |
| P1 | High | 30 minutes | Major functionality loss |
| P2 | Medium | 2 hours | Reduced functionality |
| P3 | Low | 4 hours | Minor issues |

### Incident Response Procedures

#### P0 Incident - Service Down

**Procedure**:
```bash
#!/bin/bash
# incident_p0.sh

# Alert on-call engineer
alert_oncall "P0 Incident: Service down"

# Escalate to management
escalate_to_management

# Create incident ticket
create_incident_ticket "P0 Service Down"

# Check service status
check_service_status

# Check health endpoints
verify_health_endpoints

# Check logs for errors
check_logs_for_errors

# Restart services if needed
restart_services

# Engage vendor support if needed
engage_vendor_support

# Communicate with stakeholders
communicate_stakeholders

# Document incident
document_incident
```

#### P1 Incident - Major Functionality Loss

**Procedure**:
```bash
#!/bin/bash
# incident_p1.sh

# Alert on-call engineer
alert_oncall "P1 Incident: Major functionality loss"

# Create incident ticket
create_incident_ticket "P1 Functionality Loss"

# Affected users assessment
assess_affected_users

# Root cause analysis
perform_root_cause_analysis

# Implement temporary fix
implement_temporary_fix

# Monitor impact
monitor_incident_impact

# Communicate status
communicate_status

# Document resolution
document_resolution
```

### Incident Management Commands

#### Quick Commands
```bash
# Check service health
kubectl get pods -n erlmcp -l app=erlmcp

# Restart deployment
kubectl rollout restart deployment/erlmcp -n erlmcp

# Check logs
kubectl logs -n erlmcp deployment/erlmcp --tail=100

# Scale up
kubectl scale deployment/erlmcp --replicas=5 -n erlmcp

# Drain node
kubectl drain node-name --ignore-daemonsets --delete-emptydir-data
```

## Change Management

### Change Process

1. **Request Phase**
   - Submit change request
   - Business justification
   - Risk assessment
   - Approval required

2. **Planning Phase**
   - Detailed implementation plan
   - Rollback strategy
   - Testing plan
   - Communication plan

3. **Implementation Phase**
   - Schedule maintenance window
   - Notify stakeholders
   - Implement changes
   - Verify implementation

4. **Post-Implementation Phase**
   - Monitor for issues
   - Document changes
   - Update documentation
   - Review performance

### Change Approval Matrix

| Change Type | Approval Required | Testing Required |
|-------------|------------------|------------------|
| Minor config change | Lead Engineer | Unit tests |
| Major config change | Engineering Manager | Integration tests |
| Version upgrade | Engineering Director | Full test suite |
| Infrastructure change | DevOps Manager | Performance test |

### Change Management Commands

```bash
# Apply changes
kubectl apply -f change.yaml

# Roll back changes
kubectl rollout undo deployment/erlmcp -n erlmcp

# Check change status
kubectl rollout status deployment/erlmcp -n erlmcp

# Check deployment history
kubectl rollout history deployment/erlmcp -n erlmcp
```

## Capacity Planning

### Resource Monitoring

```bash
#!/bin/bash
# resource_monitoring.sh

# Monitor CPU usage
kubectl top pods -n erlmcp --sort-by=cpu

# Monitor memory usage
kubectl top pods -n erlmcp --sort-by=memory

# Monitor disk usage
kubectl get pvc -n erlmcp -o jsonpath='{range .items[*]}{.spec.volumeName} {.status.capacity.storage} {range .status.capacity.storage}{.} {end}{"\n"}{end}'

# Monitor network traffic
kubectl get svc -n erlmcp -o jsonpath='{range .items[*]}{.metadata.name} {.spec.ports[*].port}{"\n"}{end}'
```

### Scaling Procedures

#### Horizontal Scaling
```bash
# Scale deployment
kubectl scale deployment/erlmcp --replicas=10 -n erlmcp

# Check autoscaling
kubectl get hpa -n erlmcp

# Set autoscaling
kubectl autoscale deployment/erlmcp --min=5 --max=20 --cpu-percent=70 -n erlmcp
```

#### Vertical Scaling
```bash
# Update resource limits
kubectl set resources deployment/erlmcp -n erlmcp \
  --limits=cpu=2,memory=4Gi \
  --requests=cpu=1,memory=2Gi
```

## Disaster Recovery

### Disaster Scenarios

#### 1. Site Failure
```bash
# Failover to DR site
kubectl config use-context dr-site

# Verify services
kubectl get svc -n erlmcp

# Update DNS
update_dns_records "dr-erlmcp.company.com"

# Activate failover
activate_failover_procedures
```

#### 2. Data Corruption
```bash
# Restore from backup
kubectl exec -n erlmcp deployment/erlmcp -- erl -eval 'erlmcp_db:restore(), halt()'

# Verify data integrity
verify_data_integrity

# Sync with primary
sync_with_primary
```

#### 3. Complete System Failure
```bash
# Deploy to new cluster
deploy_erlmcp_new_cluster

# Restore data from backup
restore_full_backup

# Recreate services
recreate_services

# Test functionality
test_functionality
```

### Recovery Time Objectives (RTO)

| Scenario | RTO | RPO | Recovery Strategy |
|----------|-----|-----|------------------|
| Site Failure | 4 hours | 15 minutes | Auto-failover |
| Data Corruption | 2 hours | 1 hour | Backup restore |
| System Failure | 8 hours | 4 hours | Full recovery |
| Natural Disaster | 24 hours | 4 hours | Cloud recovery |

## Monitoring and Alerting

### Key Metrics

1. **Service Metrics**
   - Response time
   - Error rate
   - Availability

2. **Resource Metrics**
   - CPU utilization
   - Memory usage
   - Disk space
   - Network I/O

3. **Business Metrics**
   - Request count
   - User count
   - Feature usage

### Alert Configuration

```yaml
# alerts.yaml
groups:
- name: erlmcp
  rules:
  - alert: HighErrorRate
    expr: rate(erlmcp_errors_total[5m]) > 0.1
    for: 5m
    labels:
      severity: warning
    annotations:
      summary: High error rate on instance {{ $labels.instance }}
      description: "Error rate is {{ $value }} errors/s"

  - alert: HighCpuUsage
    expr: (rate(container_cpu_usage_seconds_total{container="erlmcp"}[5m]) * 100) > 80
    for: 5m
    labels:
      severity: critical
    annotations:
      summary: High CPU usage
      description: "CPU usage is {{ $value }}%"

  - alert: MemoryUsageHigh
    expr: (container_memory_usage_bytes{container="erlmcp"} / container_spec_memory_limit_bytes{container="erlmcp"}) * 100 > 85
    for: 5m
    labels:
      severity: critical
    annotations:
      summary: High memory usage
      description: "Memory usage is {{ $value }}%"
```

## Emergency Contacts

### On-Call Rotation
- **Primary**: John Doe - john.doe@company.com
- **Secondary**: Jane Smith - jane.smith@company.com
- **Escalation**: DevOps Manager - devops@company.com

### Vendor Contacts
- **erlmcp Support**: support@erlmcp.com
- **Emergency**: +1-800-ERLMCP
- **Status Page**: https://status.erlmcp.com

### Support Hours
- **24/7 Emergency**: On-call team
- **Business Hours**: 8AM-8PM EST
- **Weekend/Holidays**: On-call team with SLA 4 hours

## Documentation References

- [Architecture Guide](../ARCHITECTURE.md)
- [Deployment Guide](../DEPLOYMENT/)
- [Security Configuration](../SECURITY/configuration.md)
- [API Reference](../API/reference.md)

## Version History

| Version | Date | Author | Changes |
|---------|------|--------|---------|
| 1.0 | 2024-02-01 | Operations Team | Initial document |
| 1.1 | 2024-02-15 | Operations Team | Added monitoring procedures |
| 1.2 | 2024-03-01 | Operations Team | Updated disaster recovery procedures |