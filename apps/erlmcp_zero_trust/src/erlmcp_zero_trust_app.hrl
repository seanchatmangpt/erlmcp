%% -*- erlang -*-
%%====================================================================
%% Zero-Trust Security Architecture for erlmcp v3
%%====================================================================

-include_lib("kernel/include/logger.hrl").

%% API
-export_type([
    zero_trust_context/0,
    identity/0,
    access_request/0,
    security_policy/0,
    compliance_state/0,
    threat_level/0,
    verification_result/0
]).

%% Configuration records
-record(zero_trust_config, {
    %% Identity Management
    identity_provider :: string(),
    mfa_enabled :: boolean(),
    jwt_issuer :: string(),
    jwt_audience :: string(),
    jwt_expiration :: integer(),

    %% Access Control
    max_session_duration :: integer(),
    just_in_time_ttl :: integer(),
    privilege_escalation_timeout :: integer(),
    device_trust_score_threshold :: float(),

    %% Network Security
    micro_segments :: map(),
    network_isolation :: boolean(),
    allowed_networks :: [string()],
    blocked_ports :: [integer()],

    %% Data Protection
    encryption_algorithm :: string(),
    key_rotation_interval :: integer(),
    data_classification_levels :: [atom()],

    %% Monitoring
    threat_detection :: boolean(),
    anomaly_threshold :: float(),
    compliance_checks :: [atom()],
    audit_log_retention :: integer()
}).

%% Identity and Access Control
-record(identity, {
    id :: binary(),
    subject :: binary(),
    attributes :: map(),
    roles :: [binary()],
    permissions :: [binary()],
    trust_score :: float(),
    last_authenticated :: integer(),
    device_fingerprint :: binary(),
    risk_factors :: [binary()]
}).

-record(access_request, {
    id :: binary(),
    identity_id :: binary(),
    resource :: binary(),
    action :: binary(),
    requested_permissions :: [binary()],
    context :: map(),
    timestamp :: integer(),
    expiry :: integer(),
    justification :: binary()
}).

%% Security Policies
-record(security_policy, {
    id :: binary(),
    name :: binary(),
    type :: 'identity' | 'network' | 'data' | 'application' | 'compliance',
    enabled :: boolean(),
    rules :: [map()],
    enforcement :: 'deny' | 'allow' | 'log',
    conditions :: map(),
    actions :: [binary()],
    priority :: integer()
}).

%% Compliance State
-record(compliance_state, {
    framework :: binary(),
    version :: binary(),
    last_assessment :: integer(),
    status :: 'compliant' | 'non_compliant' | 'partial' | 'unknown',
    violations :: [map()],
    controls :: [map()],
    evidence :: [binary()]
}).

%% Threat Levels
-type threat_level() :: 'info' | 'low' | 'medium' | 'high' | 'critical'.

%% Verification Results
-record(verification_result, {
    success :: boolean(),
    message :: binary(),
    risk_score :: float(),
    checks :: [map()],
    timestamp :: integer(),
    expires :: integer()
}).

%% Zero-Trust Context
-record(zero_trust_context, {
    identity :: #identity{},
    request :: #access_request{},
    policies :: [#security_policy{}],
    environment :: map(),
    session :: map(),
    compliance :: #compliance_state{}
}).

-type zero_trust_context() :: #zero_trust_context{}.
-type identity() :: #identity{}.
-type access_request() :: #access_request{}.
-type security_policy() :: #security_policy{}.
-type compliance_state() :: #compliance_state{}.
-type verification_result() :: #verification_result{}.