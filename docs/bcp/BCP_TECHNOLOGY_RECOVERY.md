# Technology Recovery Procedures - erlmcp v3
## Fortune 500 Business Continuity Plan

## Executive Summary
This document outlines comprehensive technology recovery procedures for erlmcp v3, including automated failover systems, manual recovery guides, and specialized protocols for different technology components.

## 1. Technology Recovery Architecture

### 1.1 Recovery Stack Architecture

```
Technology Recovery Stack:
┌─────────────────────────────────────────────────────────────────────────┐
│                    APPLICATION LAYER                                       │
│  ┌─────────────────┐  ┌─────────────────┐  ┌─────────────────┐         │
│  │ erlmcp_core     │  │ erlmcp_transports│  │ erlmcp_observ │         │
│  │                 │  │                 │  │  ability      │         │
│  └─────────────────┘  └─────────────────┘  └─────────────────┘         │
└────────────────────────────────────────┬─────────────────────────────────┘
                                        │
┌────────────────────────────────────────▼─────────────────────────────────┐
│                   DATA LAYER                                             │
│  ┌─────────────────┐  ┌─────────────────┐  ┌─────────────────┐         │
│  │ Session DB      │  │ Registry DB    │  │ Metrics DB     │         │
│  │ (Redis/Postgres)│  │ (Cassandra)     │  │ (InfluxDB)    │         │
│  └─────────────────┘  └─────────────────┘  └─────────────────┘         │
└────────────────────────────────────────┬─────────────────────────────────┘
                                        │
┌────────────────────────────────────────▼─────────────────────────────────┐
│                   INFRASTRUCTURE LAYER                                   │
│  ┌─────────────────┐  ┌─────────────────┐  ┌─────────────────┐         │
│  │ Compute        │  │ Network        │  │ Storage        │         │
│  │ (VM/Container) │  │ (SD-WAN/Load) │  │ (SAN/Cloud)    │         │
│  └─────────────────┘  └─────────────────┘  └─────────────────┘         │
└─────────────────────────────────────────────────────────────────────────┘
```

### 1.2 Recovery Tiers

| Tier | Technology | RTO | RPO | Recovery Method |
|------|------------|-----|-----|-----------------|
| Tier 1 | MCP Core | 15 min | 5 min | Active-Passive |
| Tier 2 | Session Management | 2 hours | 15 min | Multi-region |
| Tier 3 | Transport Layer | 4 hours | 1 hour | Redundant nodes |
| Tier 4 | Observability | 8 hours | 4 hours | Backup restore |

### 1.3 Recovery Automation Framework

```erlang
%% Technology Recovery System
-module(erlmcp_tech_recovery).

-behaviour(gen_server).

%% API
-export([start_recovery/1, check_recovery_status/1, execute_recovery_plan/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-record(recovery_state, {
    system :: binary(),
    status :: normal | degraded | recovery | failed,
    recovery_plan :: recovery_plan(),
    progress :: progress(),
    start_time :: timestamp(),
    estimated_completion :: timestamp()
}).

-record(recovery_plan, {
    steps :: [recovery_step()],
    dependencies :: [dependency()],
    rollback_plan :: rollback_plan(),
    success_criteria :: [criteria()]
}).

-spec start_recovery(binary()) -> ok | {error, reason()}.
start_recovery(System) ->
    case validate_recovery_readiness(System) of
        ready ->
            Plan = create_recovery_plan(System),
            execute_recovery_plan(Plan);
        {not_ready, Reasons} ->
            {error, {system_not_ready, Reasons}}
    end.

-spec execute_recovery_plan(recovery_plan()) -> ok.
execute_recovery_plan(Plan) ->
    % Start recovery process
    gen_server:start({local, recovery_sup}, erlmcp_tech_recovery, [Plan], []),

    % Monitor progress
    monitor_recovery_progress(Plan),

    % Notify stakeholders
    notify_recovery_started(Plan).
```

## 2. System-Specific Recovery Procedures

### 2.1 MCP Core Recovery

#### Recovery Procedure:

1. **Initial Assessment** (0-5 minutes)
   ```erlang
   %% Check MCP core health
   check_mcp_core_health() ->
       case erlmcp_json_rpc:check_system_health() of
           {ok, Status} when Status == healthy ->
               healthy;
           {ok, Status} when Status == degraded ->
               degraded;
           {error, Reason} ->
               failed
       end.
   ```

2. **Failover Activation** (5-10 minutes)
   ```bash
   # Activate standby MCP core
   ssh standby-admin@mcp-standby "
     cd /opt/erlmcp
     docker-compose stop mcp-core-primary
     docker-compose up -d mcp-core-standby
     systemctl start erlmcp-mcp-core
   "
   ```

3. **Registry Synchronization** (10-15 minutes)
   ```erlang
   %% Sync registry data
   sync_registry_data() ->
       PrimaryData = erlmcp_registry:get_all_data(),
       StandbyNodes = get_standby_nodes(),

       lists:foreach(fun(Node) ->
           rpc:call(Node, erlmcp_registry, sync_data, [PrimaryData])
       end, StandbyNodes).
   ```

4. **Validation** (15 minutes)
   - Test MCP protocol operations
   - Verify session integrity
   - Validate registry functionality

### 2.2 Session Management Recovery

#### Recovery Architecture:

```
Session Recovery Strategy:
┌─────────────────────────────────────────────────────────────────────────┐
│                   ACTIVE-REACTIVE CONFIGURATION                         │
├─────────────────────────────────────────┬─────────────────────────────────┤
│           PRIMARY REGION                │          SECONDARY REGION       │
├─────────────────────────────────────────┼─────────────────────────────────┤
│ Session DB (Active)                    │ Session DB (Standby)            │
│ ┌─────────────────┐                    │ ┌─────────────────┐              │
│ │ Real-time sync  │                    │ │ Async replica   │              │
│ │ (Raft/Paxos)    │                    │ │                │              │
│ └─────────────────┘                    │ └─────────────────┘              │
│ Session Manager (Active)                │ Session Manager (Standby)        │
│ ┌─────────────────┐                    │ ┌─────────────────┐              │
│ │ Handles active  │                    │ │ Warm standby    │              │
│ │ sessions        │                    │ │                │              │
│ └─────────────────┘                    │ └─────────────────┘              │
└─────────────────────────────────────────┴─────────────────────────────────┘
```

#### Recovery Steps:

1. **Failover Detection** (immediate)
   ```erlang
   %% Session manager health check
   check_session_manager_health() ->
       case whereis(erlmcp_session_manager) of
           undefined -> failed;
           Pid when is_pid(Pid) ->
               case process_info(Pid, status) of
                   {status, running} -> healthy;
                   _ -> degraded
               end
       end.
   ```

2. **Session Migration** (5-10 minutes)
   ```python
   # Session migration script
   class SessionMigration:
       def migrate_sessions(self, from_node, to_node):
           # Get active sessions
           active_sessions = self.get_active_sessions(from_node)

           # Migrate each session
           for session_id in active_sessions:
               session_data = self.get_session_data(from_node, session_id)
               self.create_session(to_node, session_id, session_data)
               self.update_routing(session_id, to_node)

           # Confirm migration
           self.verify_migration(active_sessions)
   ```

3. **Session Validation** (10 minutes)
   - Verify session state consistency
   - Test session operations
   - Monitor session performance

### 2.3 Transport Layer Recovery

#### Transport Recovery Matrix:

| Transport Type | Recovery Method | RTO | RPO |
|----------------|----------------|-----|-----|
| TCP | Failover to backup nodes | 5 min | 1 min |
| HTTP | Load balancer failover | 2 min | 1 min |
| WebSocket | Connection pool reset | 3 min | 1 min |
| SSE | Event store recovery | 4 min | 2 min |

#### Recovery Procedures:

```erlang
%% Transport Recovery Module
-module(erlmcp_transport_recovery).

-export([recover_transport/2, validate_transport/1]).

-spec recover_transport(transport_type(), node()) -> ok.
recover_transport(Type, Node) ->
    case Type of
        tcp ->
            recover_tcp_transport(Node);
        http ->
            recover_http_transport(Node);
        websocket ->
            recover_websocket_transport(Node);
        sse ->
            recover_sse_transport(Node)
    end.

%% TCP Transport Recovery
recover_tcp_transport(Node) ->
    % Stop failed transport
    rpc:call(Node, erlmcp_tcp_transport, stop, []),

    % Start backup transport
    rpc:call(Node, erlmcp_tcp_transport, start_backup, []),

    % Update routing
    update_routing(Node, tcp),

    % Validate connectivity
    validate_tcp_connectivity(Node).

%% HTTP Transport Recovery
recover_http_transport(Node) ->
    % Update load balancer
    update_load_balancer_routing(Node),

    % Restart HTTP service
    rpc:call(Node, erlmcp_http_transport, restart, []),

    % Validate endpoints
    validate_http_endpoints(Node).
```

### 2.4 Observability Recovery

#### Recovery Priorities:

1. **Metrics Collection** (30 minutes)
   - Restore metric collection agents
   - Reconnect to monitoring systems
   - Validate metric flow

2. **Logging System** (1 hour)
   - Restore log collection
   - Ensure log rotation
   - Validate log shipping

3. **Tracing System** (1 hour)
   - Restore trace collection
   - Reconnect to tracing backend
   - Validate trace correlation

#### Recovery Script:
```bash
#!/bin/bash
# Observability Recovery Script

# Configuration
METRICS_DB="metrics-primary"
LOGS_DB="logs-primary"
TRACING_BACKEND="tracing-primary"

# Function to recover metrics
recover_metrics() {
    echo "Recovering metrics collection..."

    # Stop metrics collection
    docker-compose stop metrics-collector

    # Start backup metrics collector
    docker-compose -f backup-metrics.yml up -d

    # Verify metrics flow
    curl -f http://localhost:9090/api/v1/query?query=up || exit 1

    echo "Metrics recovery completed"
}

# Function to recover logs
recover_logs() {
    echo "Recovering log collection..."

    # Restart log collectors
    systemctl restart fluentd
    systemctl restart logstash

    # Verify log shipping
    tail -n 100 /var/log/fluentd.log | grep -q "shipped to" || exit 1

    echo "Log recovery completed"
}

# Function to recover tracing
recover_tracing() {
    echo "Recovering tracing system..."

    # Restart Jaeger collector
    docker-compose restart jaeger-collector

    # Verify tracing backend
    curl -f http://jaeger-collector:14268/api/traces || exit 1

    echo "Tracing recovery completed"
}

# Execute recovery
recover_metrics
recover_logs
recover_tracing

echo "All observability systems recovered"
```

## 3. Data Recovery Procedures

### 3.1 Database Recovery

#### Recovery Strategy by Database Type:

| Database Type | Recovery Method | Backup Type | Recovery Time |
|---------------|----------------|-------------|---------------|
| Redis | Master-Slave failover | Daily snapshot + WAL | 10 min |
| PostgreSQL | Point-in-time recovery | Full + WAL | 30 min |
| Cassandra | Multi-DC replication | SSTables | 1 hour |
| InfluxDB | Backup restore | Full backup | 2 hours |

#### PostgreSQL Recovery Procedure:

```bash
#!/bin/bash
# PostgreSQL Recovery Script

# Configuration
PG_PRIMARY="pg-primary"
PG_STANDBY="pg-standby"
BACKUP_DIR="/backups/postgres"

# Function to recover PostgreSQL
recover_postgresql() {
    echo "Starting PostgreSQL recovery..."

    # Stop primary
    docker-compose stop postgresql-primary

    # Restore from latest backup
    docker run --rm -v postgres_data:/var/lib/postgresql/data \
        -v $BACKUP_DIR:/backup \
        postgres:13 pg_restore -U postgres -d erlmcp /backup/latest.dump

    # Start PostgreSQL
    docker-compose up -d postgresql-primary

    # Verify health
    until docker-compose exec postgresql-primary pg_isready; do
        echo "Waiting for PostgreSQL to be ready..."
        sleep 5
    done

    echo "PostgreSQL recovery completed"
}

# Execute recovery
recover_postgresql
```

### 3.2 File System Recovery

#### Recovery Procedures:

1. **System Configuration** (15 minutes)
   ```bash
   # Restore system configuration
   rsync -avz /backup/system-config/ /etc/

   # Restart services
   systemctl daemon-reload
   systemctl restart critical-services
   ```

2. **Application Data** (30 minutes)
   ```bash
   # Restore application data
   rsync -avz /backup/app-data/ /opt/erlmcp/data/

   # Set permissions
   chown -R erlmcp:erlmcp /opt/erlmcp/data
   chmod -R 750 /opt/erlmcp/data
   ```

3. **Certificates and Keys** (immediate)
   ```bash
   # Restore certificates
   cp /backup/certs/* /etc/ssl/certs/
   cp /backup/private/* /etc/ssl/private/

   # Update permissions
   chmod 600 /etc/ssl/private/*
   chown root:root /etc/ssl/private/*
   ```

### 3.3 Backup and Restore Automation

#### Backup System:
```erlang
%% Backup Management System
-module(erlmcp_backup_manager).

-export([schedule_backup/1, restore_from_backup/2]).

-record(backup, {
    id :: binary(),
    type :: full | incremental | differential,
    timestamp :: timestamp(),
    status :: scheduled | running | completed | failed,
    location :: binary(),
    checksum :: binary(),
    size :: integer()
}).

-spec schedule_backup(backup_type()) -> ok.
schedule_backup(Type) ->
    Backup = #backup{
        id = generate_backup_id(),
        type = Type,
        timestamp = erlang:timestamp(),
        status = scheduled,
        location = get_backup_location(Type),
        checksum = "",
        size = 0
    },

    % Update backup status
    update_backup_status(Backup#backup.id, running),

    % Execute backup
    case execute_backup(Backup) of
        {ok, BackupData} ->
            store_backup(Backup#backup{status = completed, data = BackupData});
        {error, Reason} ->
            update_backup_status(Backup#backup.id, failed),
            log_error(Reason)
    end.

-spec restore_from_backup(binary(), target_node()) -> ok.
restore_from_backup(BackupId, TargetNode) ->
    Backup = get_backup(BackupId),

    % Verify backup integrity
    case verify_backup_integrity(Backup) of
        valid ->
            % Prepare target node
            prepare_target_node(TargetNode),

            % Restore backup
            case execute_restore(Backup, TargetNode) of
                {ok, _} ->
                    % Validate restoration
                    case validate_restoration(Backup, TargetNode) of
                        valid ->
                            log_success("Restore completed successfully");
                        {invalid, Reasons} ->
                            handle_restore_failure(Reasons)
                    end;
                {error, Reason} ->
                    handle_restore_failure([Reason])
            end;
        {invalid, Reasons} ->
            handle_backup_integrity_failure(Reasons)
    end.
```

## 4. Network Recovery Procedures

### 4.1 Network Topology Recovery

#### Recovery Architecture:

```
Network Recovery Strategy:
┌─────────────────────────────────────────────────────────────────────────┐
│                   SD-WAN RECOVERY ARCHITECTURE                          │
├─────────────────────────────────────────┬─────────────────────────────────┤
│               PRIMARY DC                 │             BACKUP DC          │
├─────────────────────────────────────────┼─────────────────────────────────┤
│   ┌─────────────────┐                   │   ┌─────────────────┐          │
│   │ Core Switch    │                   │   │ Core Switch    │          │
│   │ (Active)        │                   │   │ (Standby)       │          │
│   └─────────────────┘                   │   └─────────────────┘          │
│       │                                   │       │                       │
│   ┌─────────────────┐                   │   ┌─────────────────┐          │
│   │ Firewall       │                   │   │ Firewall       │          │
│   │ (Active)        │                   │   │ (Standby)       │          │
│   └─────────────────┘                   │   └─────────────────┘          │
│       │                                   │       │                       │
│   ┌─────────────────┐                   │   ┌─────────────────┐          │
│   │ Load Balancer  │                   │   │ Load Balancer  │          │
│   │ (Active)        │                   │   │ (Standby)       │          │
│   └─────────────────┘                   │   └─────────────────┘          │
└─────────────────────────────────────────┴─────────────────────────────────┘
```

#### Recovery Steps:

1. **SD-WAN Failover** (2-5 minutes)
   ```python
   # SD-WAN Configuration Management
   class SDWANManager:
       def failover_to_secondary(self):
           # Configure primary SD-WAN controller
           primary_config = {
               'priority': 'secondary',
               'site': 'backup-dc',
               'state': 'active'
           }

           # Update configuration
           self.update_controller(primary_config)

           # Verify connectivity
           if not self.verify_connectivity():
               raise Exception("SD-WAN failover failed")

           # Update DNS
           self.update_dns_routing('backup-dc')
   ```

2. **Load Balancer Recovery** (5-10 minutes)
   ```bash
   # Load Balancer Recovery Script
   recover_load_balancer() {
       # Stop primary load balancer
       docker-compose stop lb-primary

       # Activate backup load balancer
       docker-compose up -d lb-secondary

       # Update health checks
       curl -X POST http://lb-secondary:8080/health \
           -H "Content-Type: application/json" \
           -d '{"service": "erlmcp", "check": "http"}'

       # Verify routing
       curl -f http://lb-secondary/health || exit 1
   }
   ```

### 4.2 DNS Recovery

#### DNS Failover Procedure:

1. **Primary DNS Failure Detection**
   ```python
   # DNS Health Monitoring
   class DNSMonitor:
       def monitor_dns_health(self):
           primary = self.check_dns_primary()
           secondary = self.check_dns_secondary()

           if not primary and secondary:
               self.trigger_dns_failover()
           elif not primary and not secondary:
               self.handle_critical_failure()
   ```

2. **DNS Failover Execution**
   ```bash
   # DNS Failover Script
   failover_dns() {
       # Update DNS records
       aws route53 change-resource-record-sets --cli-input-json '{
           "ChangeBatch": {
               "Changes": [{
                   "Action": "UPSERT",
                   "ResourceRecordSet": {
                       "Name": "erlmcp.example.com",
                       "Type": "A",
                       "TTL": 60,
                       "ResourceRecords": [{
                           "Value": "192.168.2.100"
                       }]
                   }
               }]
           }
       }'

       # Verify propagation
       until dig erlmcp.example.com | grep -q "192.168.2.100"; do
           echo "Waiting for DNS propagation..."
           sleep 5
       done
   }
   ```

## 5. Security Recovery Procedures

### 5.1 Certificate Management Recovery

#### Recovery Procedures:

1. **TLS Certificate Recovery** (15 minutes)
   ```bash
   # Certificate Recovery Script
   recover_certificates() {
       # Stop services using certificates
       systemctl stop erlmcp-core

       # Restore certificates from backup
       cp /backup/certs/server.crt /etc/ssl/certs/
       cp /backup/certs/server.key /etc/ssl/private/

       # Set permissions
       chmod 600 /etc/ssl/private/server.key
       chown root:root /etc/ssl/private/server.key

       # Restart services
       systemctl start erlmcp-core

       # Verify certificate
       openssl x509 -in /etc/ssl/certs/server.crt -text -noout
   }
   ```

2. **Certificate Rotation** (planned)
   ```python
   # Certificate Rotation System
   class CertificateManager:
       def rotate_certificates(self):
           # Generate new certificates
           new_certs = self.generate_certificates()

           # Deploy to all nodes
           self.deploy_certificates(new_certs)

           # Verify deployment
           if not self.verify_all_certificates():
               raise Exception("Certificate deployment failed")

           # Restart services with graceful restart
           self.graceful_service_restart()
   ```

### 5.2 Authentication Recovery

#### Recovery Process:

1. **Identity Provider Recovery** (30 minutes)
   ```erlang
   %% Identity Provider Recovery
   module(erlmcp_auth_recovery).

   -export([recover_identity_provider/1]).

  -spec recover_identity_provider(node()) -> ok.
   recover_identity_provider(Node) ->
       % Check IDP health
       case rpc:call(Node, erlmcp_auth, check_idp_health, []) of
           {ok, healthy} -> already_healthy;
           {ok, unhealthy} ->
               % Recover IDP
               rpc:call(Node, erlmcp_auth, recover_idp, []),
               % Verify recovery
               verify_idp_recovery(Node);
           {error, _} ->
               trigger_failover(Node)
       end.
   ```

2. **Multi-Factor Authentication Recovery** (1 hour)
   ```python
   # MFA Recovery System
   class MFAManager:
       def recover_mfa_system(self):
           # Backup MFA configurations
           mfa_configs = self.backup_mfa_configs()

           # Restore to backup MFA service
           self.restore_mfa_configs(mfa_configs)

           # Verify MFA functionality
           if not self.verify_mfa():
               raise Exception("MFA recovery failed")

           # Update user notifications
           self.notify_users_mfa_recovery()
   ```

## 6. Testing and Validation

### 6.1 Recovery Testing Matrix

| Test Type | Frequency | Systems Tested | Success Criteria |
|-----------|-----------|----------------|-------------------|
| Unit Test | Daily | Individual components | >95% pass rate |
| Integration Test | Weekly | Component interactions | 100% pass rate |
- Failover Test | Monthly | Critical systems | <30 minutes recovery |
- Full DR Test | Quarterly | All systems | <4 hours recovery |
- Chaos Test | Bi-annual | Resilience | >90% recovery success |

### 6.2 Test Scenarios

#### Scenario 1: Primary Data Center Failure
```python
# Primary DC Failure Test
class PrimaryDCTest:
    def execute_test(self):
        # Simulate primary DC failure
        self.simulate_dc_failure('primary')

        # Verify failover
        recovery_time = self.measure_recovery_time()

        # Validate services
        services_healthy = self.check_all_services()

        # Restore to normal
        self.restore_normal_operations()

        return {
            'recovery_time': recovery_time,
            'services_healthy': services_healthy,
            'success': recovery_time < 1800 and services_healthy
        }
```

#### Scenario 2: Network Partition
```python
# Network Partition Test
class NetworkPartitionTest:
    def execute_test(self):
        # Create network partition
        self.create_network_partition()

        # Verify graceful degradation
        self.check_graceful_degradation()

        # Test automatic recovery
        self.test_auto_recovery()

        # Clean up
        self.resolve_network_partition()

        return {
            'degradation_handled': self.check_degradation_handling(),
            'recovery_successful': self.check_recovery()
        }
```

### 6.3 Validation Scripts

#### Recovery Validation:
```python
# Recovery Validation System
class RecoveryValidator:
    def validate_recovery(self, system, recovery_plan):
        # Validate all recovery steps
        validation_results = []

        for step in recovery_plan.steps:
            result = self.validate_step(step)
            validation_results.append({
                'step': step.name,
                'status': result['status'],
                'details': result['details']
            })

        # Calculate overall success
        overall_success = all(r['status'] for r in validation_results)

        return {
            'system': system,
            'overall_success': overall_success,
            'validation_results': validation_results,
            'recommendations': self.generate_recommendations(validation_results)
        }
```

## 7. Documentation and Training

### 7.1 Recovery Documentation

#### Document Types:
1. **Recovery Runbooks** - Step-by-step procedures
2. **Technical Architecture** - System diagrams
3. **Contact Lists** - Emergency contacts
4. **SLA Documents** - Recovery commitments
5. **Training Materials** - User guides

#### Runbook Template:
```
TECHNOLOGY RECOVERY RUNBOOK - [SYSTEM NAME]

SYSTEM: [System Name]
CRITICALITY: [Critical/High/Medium/Low]
RTO: [Time]
RPO: [Time]

RECOVERY STEPS:
1. Step 1
   - Action
   - Expected Result
   - Time Estimate

2. Step 2
   - Action
   - Expected Result
   - Time Estimate

EMERGENCY CONTACTS:
- Technical Lead: [Name/Phone]
- Operations: [Name/Phone]
- Management: [Name/Phone]

BACKUP LOCATIONS:
- Primary: [Location]
- Secondary: [Location]
- Offsite: [Location]

SUCCESS CRITERIA:
- [Criteria 1]
- [Criteria 2]
- [Criteria 3]

ROLLBACK PROCEDURES:
- [Rollback Step 1]
- [Rollback Step 2]
```

### 7.2 Training Program

#### Training Modules:
1. **Recovery Fundamentals** - Overview of recovery procedures
2. **System-Specific Recovery** - Detailed recovery for each system
3. **Incident Response** - Crisis management during recovery
4. **Tools and Automation** - Using recovery tools
5. **Tabletop Exercises** - Simulation practice

#### Training Schedule:
- **New hires** - Within first week
- **Annual refresher** - Full team
- **Role-specific** - Quarterly updates
- **Advanced training** - For recovery team members

### 7.3 Performance Metrics

| Metric | Target | Measurement Method | Frequency |
|--------|--------|-------------------|-----------|
| Recovery time achievement | >95% | Actual vs target | After each recovery |
| Success rate | >95% | Recovery outcome | Monthly |
- Documentation accuracy | 100% | Document review | Quarterly |
- Training effectiveness | >90% | Assessment scores | Bi-annual |
- Tool reliability | >99% | Tool usage tracking | Monthly |

---

*Last Updated: February 2026*
*Version: 3.0*
*Owner: Chief Technology Officer*