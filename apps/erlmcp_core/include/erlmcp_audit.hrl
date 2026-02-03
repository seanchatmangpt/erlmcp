%%%-------------------------------------------------------------------
%%% @doc erlmcp_audit.hrl - Audit Log Records and Definitions
%%%
%%% Common records and macros for audit logging across erlmcp modules.
%%% Provides integration points for authorization, cluster, and security.
%%%
%%% @end
%%%-------------------------------------------------------------------

-ifndef(erlmcp_audit_hrl).
-define(erlmcp_audit_hrl).

%%====================================================================
%% Audit Severity Levels
%%====================================================================

-define(AUDIT_DEBUG, debug).
-define(AUDIT_INFO, info).
-define(AUDIT_NOTICE, notice).
-define(AUDIT_WARNING, warning).
-define(AUDIT_ERROR, error).
-define(AUDIT_CRITICAL, critical).
-define(AUDIT_ALERT, alert).
-define(AUDIT_EMERGENCY, emergency).

%%====================================================================
%% Audit Categories
%%====================================================================

-define(AUDIT_CAT_PROTOCOL_VIOLATION, protocol_violation).
-define(AUDIT_CAT_SECURITY, security).
-define(AUDIT_CAT_AUTHORIZATION, authorization).
-define(AUDIT_CAT_AUTHENTICATION, authentication).
-define(AUDIT_CAT_CLUSTER, cluster).
-define(AUDIT_CAT_RESOURCE, resource).
-define(AUDIT_CAT_WORKFLOW, workflow).
-define(AUDIT_CAT_SYSTEM, system).
-define(AUDIT_CAT_COMPLIANCE, compliance).
-define(AUDIT_CAT_NETWORK, network).
-define(AUDIT_CAT_DATA_ACCESS, data_access).
-define(AUDIT_CAT_CONFIGURATION, configuration).

%%====================================================================
%% Audit Event Record
%%====================================================================

-record(audit_event, {
    id :: binary() | undefined,
    timestamp :: integer() | undefined,
    category :: atom(),
    event_type :: binary(),
    severity :: atom(),
    context :: map(),
    metadata :: map(),
    node :: node(),
    process_id :: pid() | undefined,
    correlation_id :: binary() | undefined,
    signature :: binary() | undefined
}).

-type audit_event() :: #audit_event{}.

%%====================================================================
%% Protocol Violation Types
%%====================================================================

-type violation_type() ::
    invalid_message_format |
    unknown_method |
    missing_required_field |
    type_constraint_violation |
    size_limit_exceeded |
    rate_limit_exceeded |
    authentication_failure |
    authorization_failure |
    protocol_version_mismatch |
    malformed_request |
    invalid_response |
    timeout_violation |
    state_violation |
    constraint_violation.

%%====================================================================
%% Security Event Types
%%====================================================================

-type security_event_type() ::
    intrusion_attempt |
    brute_force_attack |
    privilege_escalation |
    data_exfiltration |
    suspicious_activity |
    policy_violation |
    malware_detected |
    ddos_attack |
    unauthorized_access |
    credential_theft |
    session_hijack |
    sql_injection |
    xss_attempt |
    csrf_attempt |
    path_traversal.

%%====================================================================
%% Authorization Event Types
%%====================================================================

-type auth_event_type() ::
    login_success |
    login_failure |
    logout |
    session_created |
    session_destroyed |
    token_issued |
    token_refreshed |
    token_revoked |
    permission_granted |
    permission_denied |
    role_assigned |
    role_revoked |
    mfa_challenge |
    mfa_verified.

%%====================================================================
%% Cluster Event Types
%%====================================================================

-type cluster_event_type() ::
    node_joined |
    node_left |
    partition_detected |
    partition_healed |
    election_started |
    election_completed |
    leader_elected |
    step_down |
    sync_started |
    sync_completed |
    sync_failed |
    member_added |
    member_removed |
    config_update |
    gossip_received.

%%====================================================================
%% Audit Macros
%%====================================================================

%% Log protocol violation
-define(AUDIT_LOG_VIOLATION(Type, Context, Pid),
    erlmcp_audit_log:log_violation(Type, Context, Pid)).

%% Log protocol violation with metadata
-define(AUDIT_LOG_VIOLATION_MD(Type, Context, Pid, Metadata),
    erlmcp_audit_log:log_violation(Type, Context, Pid, Metadata)).

%% Log security event
-define(AUDIT_LOG_SECURITY(Type, Action, Result),
    erlmcp_audit_log:log_security_event(Type, Action, Result)).

%% Log security event with metadata
-define(AUDIT_LOG_SECURITY_MD(Type, Action, Result, Metadata),
    erlmcp_audit_log:log_security_event(Type, Action, Result, Metadata)).

%% Log auth event
-define(AUDIT_LOG_AUTH(Type, UserId, Action, Result),
    erlmcp_audit_log:log_auth_event(Type, UserId, Action, Result)).

%% Log cluster event
-define(AUDIT_LOG_CLUSTER(Type, Nodes, Action, Result),
    erlmcp_audit_log:log_cluster_event(Type, Nodes, Action, Result)).

%% Generic log event
-define(AUDIT_LOG(Category, EventType, Context, Severity, Metadata),
    erlmcp_audit_log:log_event(Category, EventType, Context, Severity, Metadata)).

%% Log event with correlation ID
-define(AUDIT_LOG_CORR(Category, EventType, Context, Severity, Metadata, CorrId),
    erlmcp_audit_log:log_event(Category, EventType, Context, Severity, Metadata,
                               #{correlation_id => CorrId})).

%%====================================================================
%% Helper Macros for Common Events
%%====================================================================

%% Log authentication failure
-define(AUDIT_AUTH_FAIL(UserId, Reason),
    ?AUDIT_LOG_AUTH(login_failure, UserId, authenticate, Reason)).

%% Log permission denial
-define(AUDIT_PERM_DENIED(UserId, Resource, Action),
    ?AUDIT_LOG(permission_denied,
               <<Resource/binary, ":", Action/binary>>,
               #{user_id => UserId, resource => Resource, action => Action},
               ?AUDIT_NOTICE,
               #{})).

%% Log rate limit exceeded
-define(AUDIT_RATE_LIMIT(UserId, Limit),
    ?AUDIT_LOG_VIOLATION(rate_limit_exceeded,
                         #{user_id => UserId, limit => Limit},
                         self())).

%% Log protocol error
-define(AUDIT_PROTO_ERROR(Reason, Details),
    ?AUDIT_LOG_VIOLATION(malformed_request,
                         #{reason => Reason, details => Details},
                         self())).

%% Log cluster partition
-define(AUDIT_PARTITION(Nodes),
    ?AUDIT_LOG_CLUSTER(partition_detected, Nodes, detect, detected)).

%% Log suspicious activity
-define(AUDIT_SUSPICIOUS(UserId, Activity),
    ?AUDIT_LOG_SECURITY(suspicious_activity, Activity, detected,
                       #{user_id => UserId})).

%%====================================================================
%% Export Types
%%====================================================================

-export_type([
    audit_event/0,
    violation_type/0,
    security_event_type/0,
    auth_event_type/0,
    cluster_event_type/0
]).

-endif. %% erlmcp_audit_hrl
