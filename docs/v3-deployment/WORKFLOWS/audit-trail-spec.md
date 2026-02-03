# erlmcp v3 Deployment Audit Trail Specification

**Version:** 3.0.0
**Status:** Production Ready
**Last Updated:** 2026-02-02

---

## Overview

All deployment activities must be logged to an immutable audit trail for compliance, security, and operational visibility. This specification defines the audit data model, storage requirements, and query interfaces.

---

## Audit Data Model

### Core Event Structure

```erlang
-record(audit_event, {
    % Unique identifiers
    id              :: binary(),              % UUID v4
    deployment_id   :: binary(),              % Deployment UUID
    correlation_id  :: binary(),              % Correlates related events

    % Event classification
    event_type      :: audit_event_type(),
    category        :: atom(),
    severity        :: info | warning | error | critical,

    % Actor information
    actor_type      :: user | service | system,
    actor_id        :: binary(),
    actor_name      :: binary(),

    % Temporal
    timestamp       :: erlang:timestamp(),   % Microsecond precision
    received_at     :: erlang:timestamp(),

    % Deployment context
    environment     :: atom(),
    version         :: binary(),
    previous_version :: binary() | undefined,

    % Event details
    action          :: binary(),
    status          :: atom(),
    details         :: map(),

    % Security
    source_ip       :: binary() | undefined,
    user_agent      :: binary() | undefined,
    session_id      :: binary() | undefined,

    % Compliance
    tamper_evident  :: boolean(),
    digital_signature :: binary() | undefined,

    % Links
    parent_id       :: binary() | undefined,
    related_ids     :: [binary()],

    % Storage metadata
    stored_at       :: erlang:timestamp() | undefined,
    storage_location :: binary() | undefined,
    retention_until :: erlang:timestamp() | undefined
}).

-type audit_event_type() ::
    % Deployment lifecycle
    deployment_created |
    deployment_started |
    deployment_completed |
    deployment_failed |
    deployment_cancelled |

    % State changes
    state_transition |
    phase_started |
    phase_completed |
    phase_failed |

    % Approval workflow
    approval_requested |
    approval_granted |
    approval_denied |
    approval_expired |
    approval_cancelled |

    % Compliance gates
    gate_started |
    gate_passed |
    gate_failed |
    gate_warned |
    gate_skipped |

    % Rollback
    rollback_started |
    rollback_completed |
    rollback_failed |

    % Configuration
    config_changed |
    environment_configured |

    % Security
    access_granted |
    access_denied |
    security_violation |

    % System
    system_event |
    error_event.
```

---

## Event Schemas

### Deployment Events

#### deployment_created
```yaml
event_type: deployment_created
category: deployment_lifecycle
severity: info
required_fields:
  - deployment_id
  - environment
  - version
  - actor_id
  - source_trigger
example:
  id: "550e8400-e29b-41d4-a716-446655440000"
  deployment_id: "dep-1234567890"
  correlation_id: "corr-abc123"
  event_type: deployment_created
  category: deployment_lifecycle
  severity: info
  actor_type: user
  actor_id: "user@example.com"
  actor_name: "John Doe"
  timestamp: 1706900400000000
  environment: production
  version: "3.0.0"
  action: "create_deployment"
  status: created
  details:
    source_trigger: git_tag
    git_ref: "refs/tags/v3.0.0"
    commit_sha: "abc123def456789"
    workflow_id: "wf-987654"
  source_ip: "192.168.1.100"
  tamper_evident: true
```

#### deployment_started
```yaml
event_type: deployment_started
category: deployment_lifecycle
severity: info
required_fields:
  - deployment_id
  - environment
  - version
example:
  id: "550e8400-e29b-41d4-a716-446655440001"
  deployment_id: "dep-1234567890"
  correlation_id: "corr-abc123"
  parent_id: "550e8400-e29b-41d4-a716-446655440000"
  event_type: deployment_started
  category: deployment_lifecycle
  severity: info
  actor_type: service
  actor_id: "github-actions@erlmcp.io"
  actor_name: "GitHub Actions"
  timestamp: 1706900460000000
  environment: production
  version: "3.0.0"
  action: "start_deployment"
  status: in_progress
  details:
    deployment_strategy: canary
    canary_steps:
      - percentage: 10
        duration: 1800
      - percentage: 50
        duration: 1800
      - percentage: 100
    expected_duration: 3600
  tamper_evident: true
```

#### deployment_completed
```yaml
event_type: deployment_completed
category: deployment_lifecycle
severity: info
required_fields:
  - deployment_id
  - environment
  - version
  - duration_ms
example:
  id: "550e8400-e29b-41d4-a716-446655440002"
  deployment_id: "dep-1234567890"
  correlation_id: "corr-abc123"
  parent_id: "550e8400-e29b-41d4-a716-446655440001"
  event_type: deployment_completed
  category: deployment_lifecycle
  severity: info
  actor_type: service
  actor_id: "github-actions@erlmcp.io"
  timestamp: 1706904060000000
  environment: production
  version: "3.0.0"
  action: "complete_deployment"
  status: success
  details:
    duration_ms: 3600000
    actual_duration: "1h 0m 0s"
    deployment_strategy: canary
    canary_steps_completed: 3
    final_state:
      replicas: 5
      updated_replicas: 5
      ready_replicas: 5
      available_replicas: 5
    health_checks:
      liveness: passed
      readiness: passed
      startup: passed
    metrics:
      error_rate: 0.05
      p95_latency_ms: 245
      throughput_rps: 1234
  tamper_evident: true
```

#### deployment_failed
```yaml
event_type: deployment_failed
category: deployment_lifecycle
severity: critical
required_fields:
  - deployment_id
  - environment
  - version
  - failure_stage
  - error_message
example:
  id: "550e8400-e29b-41d4-a716-446655440003"
  deployment_id: "dep-1234567890"
  correlation_id: "corr-abc123"
  event_type: deployment_failed
  category: deployment_lifecycle
  severity: critical
  actor_type: system
  actor_id: "kubernetes-system"
  actor_name: "Kubernetes Controller"
  timestamp: 1706902000000000
  environment: production
  version: "3.0.0"
  action: "fail_deployment"
  status: failed
  details:
    failure_stage: health_check
    failure_phase: post_deployment_verification
    error_message: "Health check timeout after 5 minutes"
    error_code: "HEALTH_CHECK_TIMEOUT"
    error_category: availability
    exit_code: 1
    last_successful_state: deployed
    rollback_initiated: true
    rollback_version: "2.9.5"
  tamper_evident: true
```

### Approval Events

#### approval_requested
```yaml
event_type: approval_requested
category: approval_workflow
severity: info
required_fields:
  - deployment_id
  - gate_name
  - approvers
  - expires_at
example:
  id: "660e8400-e29b-41d4-a716-446655440000"
  deployment_id: "dep-1234567890"
  correlation_id: "corr-abc123"
  event_type: approval_requested
  category: approval_workflow
  severity: info
  actor_type: service
  actor_id: "deployment-system"
  timestamp: 1706900500000000
  action: "request_approval"
  status: pending
  details:
    gate_name: "production_deployment"
    gate_type: manual_approval
    required_approvers:
      - "change-board@erlmcp.io"
      - "tech-lead@erlmcp.io"
      - "devops-lead@erlmcp.io"
    min_required: 3
    expires_at: 1706979100000000  # 72 hours
    approval_url: "https://github.com/.../deployments/dep-1234567890/approve"
    reject_url: "https://github.com/.../deployments/dep-1234567890/reject"
    deployment_summary:
      version: "3.0.0"
      changes: "12 files changed, 500 additions, 50 deletions"
      risk_level: "medium"
  tamper_evident: true
```

#### approval_granted
```yaml
event_type: approval_granted
category: approval_workflow
severity: info
required_fields:
  - deployment_id
  - gate_name
  - approver_id
  - decision_comment
example:
  id: "660e8400-e29b-41d4-a716-446655440001"
  deployment_id: "dep-1234567890"
  correlation_id: "corr-abc123"
  parent_id: "660e8400-e29b-41d4-a716-446655440000"
  event_type: approval_granted
  category: approval_workflow
  severity: info
  actor_type: user
  actor_id: "tech-lead@erlmcp.io"
  actor_name: "Jane Smith"
  timestamp: 1706900800000000
  action: "grant_approval"
  status: approved
  details:
    gate_name: "production_deployment"
    approver_role: "tech_lead"
    decision_comment: "Code reviewed, all tests passed. Approved for deployment."
    review_duration_ms: 1800000
    associated_pr: "https://github.com/.../pull/123"
  source_ip: "192.168.1.50"
  tamper_evident: true
```

#### approval_denied
```yaml
event_type: approval_denied
category: approval_workflow
severity: warning
required_fields:
  - deployment_id
  - gate_name
  - approver_id
  - rejection_reason
example:
  id: "660e8400-e29b-41d4-a716-446655440002"
  deployment_id: "dep-1234567890"
  correlation_id: "corr-abc123"
  parent_id: "660e8400-e29b-41d4-a716-446655440000"
  event_type: approval_denied
  category: approval_workflow
  severity: warning
  actor_type: user
  actor_id: "security-lead@erlmcp.io"
  actor_name: "Bob Johnson"
  timestamp: 1706901000000000
  action: "deny_approval"
  status: rejected
  details:
    gate_name: "production_deployment"
    approver_role: "security_lead"
    rejection_reason: "Security scan found 2 high-severity vulnerabilities. Please address before deployment."
    vulnerabilities:
      - id: "CVE-2024-12345"
        severity: "high"
        package: "openssl"
      - id: "CVE-2024-67890"
        severity: "high"
        package: "libcurl"
    remediation_required: true
  source_ip: "192.168.1.75"
  tamper_evident: true
```

### Compliance Gate Events

#### gate_started
```yaml
event_type: gate_started
category: compliance_gate
severity: info
required_fields:
  - deployment_id
  - gate_name
  - gate_type
example:
  id: "770e8400-e29b-41d4-a716-446655440000"
  deployment_id: "dep-1234567890"
  correlation_id: "corr-abc123"
  event_type: gate_started
  category: compliance_gate
  severity: info
  actor_type: service
  actor_id: "quality-gate-system"
  timestamp: 1706900520000000
  action: "start_gate"
  status: in_progress
  details:
    gate_name: "security_scan"
    gate_type: automated
    gate_version: "1.2.0"
    configuration:
      tool: "trivy"
      severity_threshold: "high"
      timeout_seconds: 600
    expected_duration_ms: 120000
  tamper_evident: true
```

#### gate_passed
```yaml
event_type: gate_passed
category: compliance_gate
severity: info
required_fields:
  - deployment_id
  - gate_name
  - result_data
example:
  id: "770e8400-e29b-41d4-a716-446655440001"
  deployment_id: "dep-1234567890"
  correlation_id: "corr-abc123"
  parent_id: "770e8400-e29b-41d4-a716-446655440000"
  event_type: gate_passed
  category: compliance_gate
  severity: info
  actor_type: service
  actor_id: "quality-gate-system"
  timestamp: 1706900640000000
  action: "gate_passed"
  status: pass
  details:
    gate_name: "security_scan"
    gate_type: automated
    duration_ms: 120000
    result_data:
      critical_vulnerabilities: 0
      high_vulnerabilities: 0
      medium_vulnerabilities: 2
      low_vulnerabilities: 5
      total_dependencies_scanned: 150
    threshold_comparison:
      critical: "0 <= 0 ✓"
      high: "0 <= 0 ✓"
      medium: "2 <= 3 ✓"
      low: "5 <= 10 ✓"
    passed: true
  tamper_evident: true
```

#### gate_failed
```yaml
event_type: gate_failed
category: compliance_gate
severity: error
required_fields:
  - deployment_id
  - gate_name
  - failure_reason
  - result_data
example:
  id: "770e8400-e29b-41d4-a716-446655440002"
  deployment_id: "dep-1234567890"
  correlation_id: "corr-abc123"
  parent_id: "770e8400-e29b-41d4-a716-446655440000"
  event_type: gate_failed
  category: compliance_gate
  severity: error
  actor_type: service
  actor_id: "quality-gate-system"
  timestamp: 1706900640000000
  action: "gate_failed"
  status: fail
  details:
    gate_name: "test_coverage"
    gate_type: automated
    duration_ms: 90000
    failure_reason: "Coverage below required threshold"
    result_data:
      line_coverage: 72.5
      branch_coverage: 68.0
      function_coverage: 75.0
    threshold_comparison:
      line_coverage: "72.5% < 80.0% ✗"
      branch_coverage: "68.0% < 75.0% ✗"
      function_coverage: "75.0% >= 75.0% ✓"
    passed: false
    blocking: true
    remediation: "Add tests to increase coverage to at least 80%"
  tamper_evident: true
```

### Rollback Events

#### rollback_started
```yaml
event_type: rollback_started
category: rollback
severity: critical
required_fields:
  - deployment_id
  - environment
  - from_version
  - to_version
  - rollback_reason
  - rollback_type
example:
  id: "880e8400-e29b-41d4-a716-446655440000"
  deployment_id: "dep-1234567890"
  correlation_id: "corr-abc123"
  event_type: rollback_started
  category: rollback
  severity: critical
  actor_type: service
  actor_id: "deployment-system"
  timestamp: 1706902100000000
  environment: production
  version: "3.0.0"
  previous_version: "2.9.5"
  action: "start_rollback"
  status: in_progress
  details:
    rollback_type: automatic
    rollback_reason: "Health check failure - error rate exceeded 5% threshold"
    rollback_trigger:
      type: "health_check_consecutive_failures"
      threshold: 3
      actual: 3
      window_seconds: 300
    from_version: "3.0.0"
    to_version: "2.9.5"
    expected_duration_ms: 180000
    rollback_strategy: "rolling_update"
  tamper_evident: true
```

#### rollback_completed
```yaml
event_type: rollback_completed
category: rollback
severity: warning
required_fields:
  - deployment_id
  - environment
  - from_version
  - to_version
  - duration_ms
  - success
example:
  id: "880e8400-e29b-41d4-a716-446655440001"
  deployment_id: "dep-1234567890"
  correlation_id: "corr-abc123"
  parent_id: "880e8400-e29b-41d4-a716-446655440000"
  event_type: rollback_completed
  category: rollback
  severity: warning
  actor_type: service
  actor_id: "deployment-system"
  timestamp: 1706902280000000
  environment: production
  version: "2.9.5"
  previous_version: "3.0.0"
  action: "complete_rollback"
  status: success
  details:
    rollback_type: automatic
    from_version: "3.0.0"
    to_version: "2.9.5"
    duration_ms: 180000
    success: true
    pods_rolled: 5
    pods_failed: 0
    health_after_rollback:
      healthy_pods: 5
      total_pods: 5
      error_rate: 0.02
      p95_latency_ms: 200
    investigation_required: true
    incident_created: true
    incident_id: "INC-2026-02-02-001"
  tamper_evident: true
```

---

## Storage Requirements

### Primary Storage (Elasticsearch)

```yaml
index_name: erlmcp-deployment-audit
settings:
  number_of_shards: 3
  number_of_replicas: 2
  refresh_interval: "5s"
  index.lifecycle.name: "erlmcp-audit-policy"
  index.lifecycle.rollover_alias: "erlmcp-audit"

lifecycle_policy:
  phases:
    hot:
      actions:
        rollover:
          max_age: "1d"
          max_size: "50gb"
        set_priority:
          priority: 100
    warm:
      min_age: "7d"
      actions:
        forcemerge:
          max_num_segments: 1
        set_priority:
          priority: 50
    cold:
      min_age: "30d"
      actions:
        freeze: {}
    delete:
      min_age: "7y"
```

### Backup Storage (S3)

```yaml
bucket: erlmcp-audit-logs
prefix: deployment/
storage_class: STANDARD_INFARE
retention: permanent
versioning: enabled
encryption:
  type: AES256
  sse_kms_key_id: alias/erlmcp-audit
```

### Immutable Storage

All audit events must be tamper-evident:

```erlang
%% Add digital signature to audit event
-spec sign_event(audit_event()) -> {ok, audit_event()} | {error, term()}.
sign_event(Event) ->
    Payload = encode_for_signing(Event),
    case sign_payload(Payload) of
        {ok, Signature} ->
            {ok, Event#audit_event{
                digital_signature = Signature,
                tamper_evident = true
            }};
        {error, Reason} ->
            {error, Reason}
    end.

%% Verify event signature
-spec verify_event(audit_event()) -> {ok, boolean()} | {error, term()}.
verify_event(#audit_event{digital_signature = undefined}) ->
    {ok, false};
verify_event(Event) ->
    Payload = encode_for_signing(Event),
    Signature = Event#audit_event.digital_signature,
    verify_signature(Payload, Signature).
```

---

## Query Interface

### Query API

```erlang
%% Query audit events by deployment ID
-spec query_by_deployment(binary()) -> {ok, [audit_event()]} | {error, term()}.
query_by_deployment(DeploymentId) ->
    Query = #{
        <<"bool">> => #{
            <<"must">> => [
                #{<<"term">> => #{<<"deployment_id">> => DeploymentId}}
            ]
        }
    },
    search(Query).

%% Query audit events by time range
-spec query_by_timerange(erlang:timestamp(), erlang:timestamp()) ->
    {ok, [audit_event()]} | {error, term()}.
query_by_timerange(StartTime, EndTime) ->
    Query = #{
        <<"range">> => #{
            <<"timestamp">> => #{
                <<"gte">> => StartTime,
                <<"lte">> => EndTime
            }
        }
    },
    search(Query).

%% Query audit events by actor
-spec query_by_actor(binary()) -> {ok, [audit_event()]} | {error, term()}.
query_by_actor(ActorId) ->
    Query = #{
        <<"bool">> => #{
            <<"must">> => [
                #{<<"term">> => #{<<"actor_id">> => ActorId}}
            ]
        }
    },
    search(Query).

%% Query audit events by type
-spec query_by_type(audit_event_type()) -> {ok, [audit_event()]} | {error, term()}.
query_by_type(EventType) ->
    Query = #{
        <<"bool">> => #{
            <<"must">> => [
                #{<<"term">> => #{<<"event_type">> => EventType}}
            ]
        }
    },
    search(Query).

%% Complex query example: All failed production deployments in last 24h
-spec query_failed_prod_last_24h() -> {ok, [audit_event()]} | {error, term()}.
query_failed_prod_last_24h() ->
    Now = erlang:system_time(microsecond),
    DayAgo = Now - (24 * 60 * 60 * 1_000_000),
    Query => #{
        <<"bool">> => #{
            <<"must">> => [
                #{<<"term">> => #{<<"event_type">> => <<"deployment_failed">>}},
                #{<<"term">> => #{<<"environment">> => <<"production">>}},
                #{<<"range">> => #{
                    <<"timestamp">> => #{
                        <<"gte">> => DayAgo,
                        <<"lte">> => Now
                    }
                }}
            ]
        },
        <<"sort">> => [
            #{<<"timestamp">> => #{<<"order">> => <<"desc">>}}
        ]
    },
    search(Query).
```

### Compliance Report Generator

```erlang
%% Generate compliance report for audit
-spec generate_compliance_report(binary(), binary(), binary()) ->
    {ok, map()} | {error, term()}.
generate_compliance_report(DeploymentId, StartTime, EndTime) ->
    {ok, Events} = query_by_deployment(DeploymentId),

    Report = #{
        deployment_id => DeploymentId,
        report_period => #{
            start => StartTime,
            end => EndTime
        },
        summary => generate_summary(Events),
        events_by_type => group_by_type(Events),
        actors => list_actors(Events),
        timeline => generate_timeline(Events),
        compliance_check => verify_compliance(Events)
    },

    {ok, Report}.

%% Verify compliance requirements are met
-spec verify_compliance([audit_event()]) -> map().
verify_compliance(Events) ->
    #{
        immutable_storage => verify_all_signed(Events),
        complete_chain => verify_chain_complete(Events),
        approval_chain => verify_approvals(Events),
        gates_passed => verify_all_gates(Events),
        retention_met => verify_retention(Events),
        tamper_evident => verify_no_tampering(Events)
    }.
```

---

## Audit Trail Retention

| Data Type | Retention Period | Reason |
|-----------|-----------------|---------|
| Deployment events | 7 years | Compliance (SOX, ISO 27001) |
| Approval events | Permanent | Legal requirements |
| Security events | Permanent | Security investigations |
| State transitions | 1 year | Operational analysis |
| System events | 90 days | Troubleshooting |

---

## Security Considerations

1. **Immutable Logs**: Once written, audit events cannot be modified
2. **Tamper Evidence**: All events signed with private key
3. **Access Control**: RBAC for audit log access
4. **Encryption**: At-rest and in-transit encryption
5. **Retention**: Cannot delete before retention period
6. **Backup**: Multiple copies in different regions

---

## Example Audit Trail

```
2026-02-02T12:00:00.000000Z [INFO] deployment_created deployment_id=dep-1234567890 environment=production version=3.0.0 actor=user@example.com
2026-02-02T12:00:10.000000Z [INFO] deployment_started deployment_id=dep-1234567890 environment=production version=3.0.0 actor=github-actions
2026-02-02T12:00:30.000000Z [INFO] gate_started deployment_id=dep-1234567890 gate=compile
2026-02-02T12:01:00.000000Z [INFO] gate_passed deployment_id=dep-1234567890 gate=compile duration=30s
2026-02-02T12:01:10.000000Z [INFO] gate_started deployment_id=dep-1234567890 gate=test
2026-02-02T12:05:00.000000Z [INFO] gate_passed deployment_id=dep-1234567890 gate=test duration=230s
2026-02-02T12:05:10.000000Z [INFO] gate_started deployment_id=dep-1234567890 gate=security
2026-02-02T12:06:00.000000Z [INFO] gate_passed deployment_id=dep-1234567890 gate=security duration=50s
2026-02-02T12:06:10.000000Z [INFO] approval_requested deployment_id=dep-1234567890 gate=production_deployment
2026-02-02T12:15:00.000000Z [INFO] approval_granted deployment_id=dep-1234567890 approver=tech-lead@erlmcp.io
2026-02-02T12:16:00.000000Z [INFO] approval_granted deployment_id=dep-1234567890 approver=change-board@erlmcp.io
2026-02-02T12:17:00.000000Z [INFO] approval_granted deployment_id=dep-1234567890 approver=devops-lead@erlmcp.io
2026-02-02T12:17:10.000000Z [INFO] state_transition deployment_id=dep-1234567890 from=pending to=deploying
2026-02-02T12:17:20.000000Z [INFO] phase_started deployment_id=dep-1234567890 phase=canary_10
2026-02-02T12:47:20.000000Z [INFO] phase_completed deployment_id=dep-1234567890 phase=canary_10
2026-02-02T12:47:30.000000Z [INFO] phase_started deployment_id=dep-1234567890 phase=canary_50
2026-02-02T13:17:30.000000Z [INFO] phase_completed deployment_id=dep-1234567890 phase=canary_50
2026-02-02T13:17:40.000000Z [INFO] phase_started deployment_id=dep-1234567890 phase=full_rollout
2026-02-02T13:20:00.000000Z [INFO] phase_completed deployment_id=dep-1234567890 phase=full_rollout
2026-02-02T13:20:10.000000Z [INFO] state_transition deployment_id=dep-1234567890 from=deploying to=verifying
2026-02-02T13:25:00.000000Z [INFO] state_transition deployment_id=dep-1234567890 from=verifying to=monitoring
2026-02-03T13:25:00.000000Z [INFO] state_transition deployment_id=dep-1234567890 from=monitoring to=completed
2026-02-03T13:25:10.000000Z [INFO] deployment_completed deployment_id=dep-1234567890 duration=24h
```
