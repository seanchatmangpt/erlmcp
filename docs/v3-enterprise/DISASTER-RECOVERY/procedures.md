# Disaster Recovery Procedures

## Overview

This document provides comprehensive disaster recovery (DR) procedures for erlmcp v3 deployments. It covers various disaster scenarios, recovery strategies, RTO/RPO targets, and step-by-step recovery procedures.

## Disaster Classification

### Severity Levels

| Level | Severity | RTO | RPO | Impact |
|-------|----------|-----|-----|---------|
| P0 | Critical | 1 hour | < 15 minutes | Complete system failure |
| P1 | High | 4 hours | < 1 hour | Major functionality loss |
| P2 | Medium | 8 hours | < 4 hours | Reduced functionality |
| P3 | Low | 24 hours | < 8 hours | Minor issues |

### Disaster Scenarios

| Scenario | Type | Probability | Impact | Duration |
|----------|------|-------------|---------|----------|
| Site Failure | Natural Disaster | Low | High | 24-72 hours |
| Hardware Failure | Infrastructure | Medium | Medium | 4-8 hours |
| Data Corruption | System Error | Medium | High | 2-4 hours |
| Network Outage | Network | High | Medium | 1-2 hours |
| Security Breach | Security | Low | High | 24-48 hours |

## Recovery Architecture

### Multi-Site Architecture

```
┌─────────────────┐    ┌─────────────────┐    ┌─────────────────┐
│   Primary Site   │    │   DR Site       │    │   Cloud Site    │
│                 │    │                 │    │                 │
│ ┌─────────────┐ │    │ ┌─────────────┐ │    │ ┌─────────────┐ │
│ │ Production  │ │    │ │ Standby     │ │    │ │ Disaster    │ │
│ │ Environment │ │    │ │ Environment │ │    │ │ Recovery    │ │
│ └─────────────┘ │    │ └─────────────┘ │    │ │ Environment │ │
│                 │    │                 │    │ └─────────────┘ │
│ ┌─────────────┐ │    │ ┌─────────────┐ │    │                 │
│ │ Active Data │ │    │ │ Synced Data │ │    │ │ Cloud Backup │ │
│ │             │ │    │ │             │ │    │ │             │ │
│ │ - Active DB │ │    │ │ - Read-only │ │    │ │ - Object     │ │
│ │ - Live Logs │ │    │ │ - Archives  │ │    │ │ - Archives   │ │
│ │ - Config    │ │    │ │ - Config    │ │    │ │ - Config     │ │
│ └─────────────┘ │    │ └─────────────┘ │    │ └─────────────┘ │
└─────────────────┘    └─────────────────┘    └─────────────────┘
```

### Recovery Strategies

1. **Active-Active**: Both sites handle traffic
2. **Active-Passive**: Primary site only, DR site on standby
3. **Cloud-Based**: Cloud environment for quick recovery
4. **Hybrid**: Combination of strategies based on criticality

## Disaster Recovery Procedures

### 1. Site Failure Recovery

#### P0 - Primary Site Failure

**Initial Assessment (0-15 minutes)**
```bash
#!/bin/bash
# assess_site_failure.sh

# Check primary site status
check_primary_site() {
    # Ping check
    ping -c 3 primary-site

    # Service check
    curl -f https://primary-site.company.com/health || true

    # Network connectivity
    telnet primary-site 443 || true
}

# Notify stakeholders
notify_stakeholders() {
    # Send alert
    alert "P0 Incident: Primary site failure detected"

    # Create incident ticket
    create_incident_ticket "P0 - Primary Site Failure"

    # Notify on-call team
    notify_oncall_team

    # Notify management
    notify_management
}

# Assess damage
assess_damage() {
    # Check affected systems
    check_affected_systems

    # Estimate recovery time
    estimate_recovery_time

    # Identify priority systems
    identify_priority_systems
}
```

**Failover to DR Site (15-60 minutes)**
```bash
#!/bin/bash
# failover_to_dr.sh

# Activate DR site
activate_dr_site() {
    # Update DNS
    update_dns "erlmcp.company.com" "dr-site-ip"

    # Activate standby systems
    kubectl config use-context dr-site
    kubectl get nodes

    # Start erlmcp services
    kubectl apply -f erlmcp-dr.yaml

    # Verify services
    verify_dr_services

    # Enable write operations
    enable_write_operations
}

# Data recovery
recover_data() {
    # Verify data consistency
    verify_data_consistency

    # Apply any missing transactions
    apply_missing_transactions

    # Validate data integrity
    validate_data_integrity
}

# Test functionality
test_functionality() {
    # Run basic tests
    run_smoke_tests

    # Load testing
    run_load_tests

    # Full functional tests
    run_functional_tests
}

# Communicate status
communicate_failover() {
    # Notify all stakeholders
    notify_all_stakeholders "Failover completed"

    # Update status page
    update_status_page "Operational"

    # Document actions
    document_failover_actions
}
```

#### Recovery Steps

1. **Immediate Actions (0-15 min)**
   - Activate DR procedures
   - Notify all stakeholders
   - Create incident ticket

2. **Site Activation (15-60 min)**
   - Update DNS records
   - Activate standby systems
   - Start critical services

3. **Data Recovery (60-120 min)**
   - Sync latest data
   - Apply missing transactions
   - Validate data integrity

4. **Service Verification (120-180 min)**
   - Run smoke tests
   - Load testing
   - Functional testing

5. **Full Operations (180-240 min)**
   - Open all services
   - Monitor for issues
   - Document recovery

### 2. Data Recovery Procedures

#### Database Recovery

**Step 1: Assess Data Damage**
```bash
#!/bin/bash
# assess_data_damage.sh

# Check database status
check_db_status() {
    kubectl exec -n erlmcp deployment/erlmcp-db -- \
        pg_isready -h localhost -U erlmcp

    # Check replica status
    kubectl exec -n erlmcp deployment/erlmcp-db -- \
        psql -U erlmcp -c "SELECT * FROM pg_stat_replication;"
}

# Identify corrupted data
identify_corrupted_data() {
    # Check recent transactions
    kubectl exec -n erlmcp deployment/erlmcp-db -- \
        psql -U erlmcp -c "SELECT * FROM transactions WHERE status = 'failed';"

    # Check data consistency
    kubectl exec -n erlmcp deployment/erlmcp-db -- \
        psql -U erlmcp -c "SELECT * FROM consistency_check();"
}

# Determine recovery point
determine_recovery_point() {
    # Find last consistent backup
    find_last_consistent_backup

    # Check transaction logs
    check_transaction_logs

    # Calculate data loss
    calculate_data_loss
}
```

**Step 2: Restore Database**
```bash
#!/bin/bash
# restore_database.sh

# Restore from backup
restore_database() {
    # Stop write operations
    kubectl scale deployment/erlmcp --replicas=0 -n erlmcp

    # Restore database
    kubectl exec -n erlmcp deployment/erlmcp-db -- \
        pg_restore -U erlmcp -d erlmcp /backups/latest.dump

    # Apply WAL logs
    kubectl exec -n erlmcp deployment/erlmcp-db -- \
        psql -U erlmcp -c "SELECT pg_wal_replay_resume();"
}

# Validate restoration
validate_restoration() {
    # Check data integrity
    kubectl exec -n erlmcp deployment/erlmcp-db -- \
        psql -U erlmcp -c "SELECT * FROM data_integrity_check();"

    # Check consistency
    kubectl exec -n erlmcp deployment/erlmcp-db -- \
        psql -U erlmcp -c "SELECT * FROM consistency_check();"
}

# Restart services
restart_services() {
    # Start erlmcp services
    kubectl scale deployment/erlmcp --replicas=3 -n erlmcp

    # Start dependent services
    kubectl rollout restart deployment -n erlmcp

    # Verify services
    verify_services
}
```

#### File System Recovery

**Recovery Process**
```bash
#!/bin/bash
# recover_file_system.sh

# Backup current state
backup_current_state() {
    # Create backup directory
    mkdir -p /backups/recovery

    # Backup configuration
    cp -r /etc/erlmcp /backups/recovery/config-$(date +%Y%m%d)

    # Backup data
    cp -r /var/lib/erlmcp /backups/recovery/data-$(date +%Y%m%d)
}

# Restore from backup
restore_file_system() {
    # Stop services
    systemctl stop erlmcp

    # Restore files
    rsync -av /backups/latest/config/ /etc/erlmcp/
    rsync -av /backups/latest/data/ /var/lib/erlmcp/

    # Set permissions
    chown -R erlmcp:erlmcp /etc/erlmcp
    chown -R erlmcp:erlmcp /var/lib/erlmcp

    # Start services
    systemctl start erlmcp
}

# Verify restoration
verify_restoration() {
    # Check configuration
    /opt/erlmcp/bin/verify-config

    # Check data integrity
    /opt/erlmcp/bin/verify-data

    # Test functionality
    /opt/erlmcp/bin/test-functionality
}
```

### 3. Network Recovery Procedures

#### Site-to-Site Network Failure

**Initial Response**
```bash
#!/bin/bash
# network_failure_response.sh

# Check network connectivity
check_network_connectivity() {
    # Check primary site
    ping -c 3 primary-site

    # Check DR site
    ping -c 3 dr-site

    # Check internet connectivity
    ping -c 3 8.8.8.8

    # Check VPN status
    check_vpn_status
}

# Activate backup network
activate_backup_network() {
    # Activate backup ISP
    activate_backup_isp

    # Configure static routes
    configure_static_routes

    # Update firewall rules
    update_firewall_rules

    # Test connectivity
    test_connectivity
}

# Notify stakeholders
notify_network_failure() {
    # Alert on-call team
    alert "Network failure detected"

    # Create incident ticket
    create_incident_ticket "Network Failure"

    # Notify affected users
    notify_affected_users "Network issues detected"
}
```

#### DNS Failover

**DNS Update Process**
```bash
#!/bin/bash
# dns_failover.sh

# Update DNS records
update_dns_records() {
    # Primary to DR
    aws route53 change-resource-record-sets \
        --hosted-zone-id Z1234567890 \
        --change-batch '{
            "Comment": "Failover to DR site",
            "Changes": [{
                "Action": "UPSERT",
                "ResourceRecordSet": {
                    "Name": "erlmcp.company.com.",
                    "Type": "A",
                    "TTL": 300,
                    "ResourceRecords": [{
                        "Value": "10.1.1.100"
                    }]
                }
            }]
        }'

    # Update failover policy
    update_failover_policy
}

# Verify DNS propagation
verify_dns_propagation() {
    # Check DNS records
    dig erlmcp.company.com

    # Check from multiple locations
    check_from_multiple_locations

    # Test service availability
    test_service_availability
}
```

### 4. Application Recovery

#### Service Restart Procedures

**Emergency Restart**
```bash
#!/bin/bash
# emergency_restart.sh

# Identify affected services
identify_affected_services() {
    # Check service status
    kubectl get pods -n erlmcp

    # Check readiness probes
    kubectl describe pod -n erlmcp erlmcp-xxx | grep -i readiness

    # Check liveness probes
    kubectl describe pod -n erlmcp erlmcp-xxx | grep -i liveness
}

# Restart services
restart_services() {
    # Restart erlmcp pods
    kubectl rollout restart deployment/erlmcp -n erlmcp

    # Wait for pods to be ready
    kubectl wait --for=condition=ready pod -n erlmcp -l app=erlmcp

    # Check service health
    kubectl get pods -n erlmcp -l app=erlmcp
}

# Verify functionality
verify_functionality() {
    # Check endpoints
    curl -f https://erlmcp.company.com/health

    # Run smoke tests
    run_smoke_tests

    # Check logs
    kubectl logs -n erlmcp deployment/erlmcp --tail=100
}
```

#### Configuration Recovery

**Configuration Restoration**
```bash
#!/bin/bash
# restore_configuration.sh

# Backup current config
backup_current_config() {
    # Create backup directory
    mkdir -p /backups/config/$(date +%Y%m%d)

    # Export configuration
    kubectl get configmap -n erlmcp -o yaml > /backups/config/$(date +%Y%m%d)/configmaps.yaml
    kubectl get secret -n erlmcp -o yaml > /backups/config/$(date +%Y%m%d)/secrets.yaml
}

# Restore from backup
restore_config_from_backup() {
    # Restore configmaps
    kubectl apply -f /backups/latest/configmaps.yaml

    # Restore secrets
    kubectl apply -f /backups/latest/secrets.yaml

    # Restart services
    kubectl rollout restart deployment/erlmcp -n erlmcp
}

# Validate configuration
validate_configuration() {
    # Check configuration syntax
    kubectl exec -n erlmcp deployment/erlmcp -- erl -eval 'application:get_env(erlmcp, config), halt().'

    # Test configuration
    kubectl exec -n erlmcp deployment/erlmcp -- /opt/erlmcp/bin/test-config

    # Validate connectivity
    test_connectivity
}
```

## Testing and Validation

### 1. Disaster Recovery Testing

#### Tabletop Exercises
```bash
#!/bin/bash
# tabletop_exercise.sh

# Scenario 1: Site Failure
simulate_site_failure() {
    echo "=== Simulating Site Failure ==="

    # Present scenario
    present_scenario "Primary site power failure"

    # Discuss response
    discuss_response

    # Document lessons
    document_lessons

    # Update procedures
    update_procedures
}

# Scenario 2: Data Corruption
simulate_data_corruption() {
    echo "=== Simulating Data Corruption ==="

    # Present scenario
    present_scenario "Database corruption detected"

    # Discuss response
    discuss_response

    # Test recovery
    test_recovery

    # Document results
    document_results
}
```

#### Live Failover Testing
```bash
#!/bin/bash
# live_failover_test.sh

# Schedule maintenance window
schedule_maintenance_window() {
    # Notify stakeholders
    notify_stakeholders "DR testing - 10PM-2AM"

    # Backup production data
    backup_production_data

    # Verify backups
    verify_backups
}

# Execute failover
execute_failover_test() {
    # Activate DR site
    activate_dr_site

    # Test functionality
    test_functionality

    # Perform failback
    perform_failback_test

    # Clean up
    cleanup_test_environment
}

# Document results
document_test_results() {
    # Create test report
    generate_test_report

    # Share with team
    share_test_results

    # Update documentation
    update_documentation
}
```

### 2. Validation Scripts

#### Recovery Validation
```erlang
% dr_validation.erl
-module(dr_validation).
-export([validate_recovery/1]).

validate_recovery(Scenario) ->
    % Validate service availability
    case check_service_availability() of
        true ->
            % Validate data consistency
            case check_data_consistency() of
                true ->
                    % Validate functionality
                    case run_comprehensive_tests() of
                        true ->
                            {success, "Recovery successful"};
                        false ->
                            {error, "Functional tests failed"}
                    end;
                false ->
                    {error, "Data consistency check failed"}
            end;
        false ->
            {error, "Service availability check failed"}
    end.

check_service_availability() ->
    % Check all critical services
    Services = [erlmcp, database, redis],
    lists:all(fun(Service) -> check_service(Service) end, Services).

check_data_consistency() ->
    % Verify data integrity across sites
    {PrimaryData, DrData} = get_data_from_sites(),
    compare_data(PrimaryData, DrData).
```

## Communication Plan

### Stakeholder Communication

#### Emergency Contact List
```yaml
# contacts.yaml
contacts:
  oncall:
    primary:
      name: "John Doe"
      email: "john.doe@company.com"
      phone: "+1-555-1234"
      role: "DevOps Engineer"
    secondary:
      name: "Jane Smith"
      email: "jane.smith@company.com"
      phone: "+1-555-5678"
      role: "DevOps Engineer"

  management:
    cto:
      name: "CTO"
      email: "cto@company.com"
      phone: "+1-555-9999"
    cfo:
      name: "CFO"
      email: "cfo@company.com"
      phone: "+1-555-8888"

  customers:
    enterprise:
      list: "enterprise-support@company.com"
      escalation: "enterprise-escalation@company.com"
    standard:
      list: "support@company.com"

  partners:
    critical:
      list: "partners@company.com"
```

#### Communication Templates
```markdown
### Incident Notification Template

**Incident ID:** [ID]
**Severity:** [Level]
**Start Time:** [Time]
**Affected Systems:** [Systems]

**Issue:**
[Brief description of the issue]

**Impact:**
[Description of impact on users]

**Current Status:**
[Current status of the incident]

**Next Steps:**
[Immediate actions being taken]

**Estimated Time to Resolution:**
[RTO timeframe]

**Contact Information:**
[Who to contact for more information]

---

### Recovery Notification Template

**Incident ID:** [ID]
**Resolution Time:** [Time]
**Systems Restored:** [Systems]

**Resolution:**
[Description of resolution]

**Actions Taken:**
[List of actions taken]

**Verification:**
[How the resolution was verified]

**Next Steps:**
[Follow-up actions]

**Contact Information:**
[Who to contact for questions]
```

### Status Updates

#### Update Frequency
- **P0**: Every 15 minutes
- **P1**: Every 30 minutes
- **P2**: Every 2 hours
- **P3**: Every 4 hours

#### Update Content
```bash
#!/bin/bash
# incident_updates.sh

send_incident_update() {
    local incident_id=$1
    local status=$2
    local message=$3

    # Send email
    send_email_update "$incident_id" "$status" "$message"

    # Update status page
    update_status_page "$status" "$message"

    # Post to Slack
    post_slack_update "$incident_id" "$status" "$message"
}

send_email_update() {
    local incident_id=$1
    local status=$2
    local message=$3

    mail -s "Incident Update: $incident_id - $status" \
        -c "incident-team@company.com" \
        <<< "Incident ID: $incident_id\nStatus: $status\nMessage: $message"
}
```

## Documentation References

### Related Documents
- [Architecture Guide](../ARCHITECTURE.md)
- [Deployment Guide](../DEPLOYMENT/)
- [Operations Manual](../OPERATIONS/procedures.md)
- [Security Configuration](../SECURITY/configuration.md)

### Templates
- [Incident Response Template](../APPENDICES/templates/incident-response-template.md)
- [Failover Checklist](../APPENDICES/templates/failover-checklist.md)
- [Testing Report Template](../APPENDICES/templates/testing-report.md)

## Version History

| Version | Date | Author | Changes |
|---------|------|--------|---------|
| 1.0 | 2024-02-01 | DevOps Team | Initial document |
| 1.1 | 2024-02-15 | DevOps Team | Added testing procedures |
| 1.2 | 2024-03-01 | DevOps Team | Enhanced communication plan |