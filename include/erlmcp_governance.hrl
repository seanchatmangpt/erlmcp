%%%-------------------------------------------------------------------
%% @doc MCP+ Governance Framework - Core Types and Records
%%
%% Implements the governance primitives from the MCP+ specification:
%% - Sealed Operating Contracts with cryptographic signatures
%% - Envelope enforcement for bounded behavior
%% - Deterministic refusals with remediation
%% - Cryptographic receipts for audit trails
%% - Evidence bundles (.mcpb) for text-blind verification
%%
%% Core Axioms:
%%   A = mu(O)              - Artifact equals mu of Origin
%%   mu . mu = mu           - Idempotent transformation
%%   hash(A) = hash(mu(O))  - Hash consistency
%%   O |= Sigma             - Origin satisfies Specification
%%
%% @end
%%%-------------------------------------------------------------------

-ifndef(ERLMCP_GOVERNANCE_HRL).
-define(ERLMCP_GOVERNANCE_HRL, 1).

%%====================================================================
%% Contract Version and Protocol
%%====================================================================

-define(MCPPLUS_VERSION, <<"2027-Q1">>).
-define(MCPPLUS_PROTOCOL, <<"mcp-plus-governance/1.0">>).

%%====================================================================
%% Contract Types
%%====================================================================

-type contract_id() :: binary().          % Unique contract identifier (UUID)
-type contract_family() :: binary().      % Contract family/category
-type epoch() :: pos_integer().           % Signing key epoch
-type signature() :: binary().            % Ed25519 or similar signature
-type hash() :: binary().                 % SHA-256 hash
-type timestamp_ms() :: pos_integer().    % Unix timestamp in milliseconds

%%====================================================================
%% Contract Authority Tiers
%%====================================================================

-define(TIER_A_AUTOMATE, tier_a_automate).
-define(TIER_B_ASSIST, tier_b_assist).
-define(TIER_C_DO_NOT_AUTOMATE, tier_c_do_not_automate).

-type automation_tier() :: tier_a_automate | tier_b_assist | tier_c_do_not_automate.

%%====================================================================
%% Contract Record - Sealed Operating Contract
%%====================================================================

-record(mcp_contract, {
    %% Identity
    id :: contract_id(),
    family :: contract_family(),
    version :: binary(),

    %% Specification (Sigma)
    specification :: map(),                 % JSON Schema for inputs/outputs
    invariants :: [binary()],               % Enforced invariants (text)
    preconditions :: [binary()],            % Required preconditions
    postconditions :: [binary()],           % Guaranteed postconditions

    %% Authorization
    tier :: automation_tier(),
    capabilities :: [binary()],             % Allowed capabilities
    boundaries :: map(),                    % Do-not-cross boundaries

    %% Signing
    epoch :: epoch(),
    public_key :: binary(),
    signature :: signature(),

    %% Metadata
    created_at :: timestamp_ms(),
    expires_at :: timestamp_ms() | undefined,
    revoked :: boolean(),

    %% Provenance
    origin_hash :: hash(),                  % hash(Origin)
    artifact_hash :: hash()                 % hash(mu(Origin)) = hash(Artifact)
}).

-type mcp_contract() :: #mcp_contract{}.

%%====================================================================
%% Envelope Record - Bounded Behavior Enforcement
%%====================================================================

-record(mcp_envelope, {
    %% Identity
    id :: binary(),
    contract_id :: contract_id(),

    %% Resource Limits
    max_duration_ms :: pos_integer(),       % Maximum execution time
    max_memory_bytes :: pos_integer(),      % Maximum memory usage
    max_cpu_ms :: pos_integer(),            % Maximum CPU time
    max_io_bytes :: pos_integer(),          % Maximum I/O bytes
    max_network_bytes :: pos_integer(),     % Maximum network transfer

    %% Rate Limits
    max_requests_per_sec :: pos_integer(),
    max_requests_per_min :: pos_integer(),
    max_concurrent :: pos_integer(),

    %% Scope Limits
    max_payload_bytes :: pos_integer(),
    max_response_bytes :: pos_integer(),
    max_recursion_depth :: pos_integer(),

    %% Behavioral Limits
    allowed_operations :: [binary()],       % Whitelist of operations
    denied_operations :: [binary()],        % Blacklist of operations

    %% Fail-Closed Policy
    fail_closed :: boolean(),               % Refuse on uncertainty
    uncertainty_threshold :: float(),       % 0.0 - 1.0

    %% Monitoring
    metrics_enabled :: boolean(),
    trace_enabled :: boolean()
}).

-type mcp_envelope() :: #mcp_envelope{}.

%%====================================================================
%% Refusal Codes for MCP+ Governance
%%====================================================================

%% Contract Violations (2001-2020)
-define(REFUSAL_CONTRACT_NOT_FOUND, 2001).
-define(REFUSAL_CONTRACT_EXPIRED, 2002).
-define(REFUSAL_CONTRACT_REVOKED, 2003).
-define(REFUSAL_CONTRACT_SIGNATURE_INVALID, 2004).
-define(REFUSAL_CONTRACT_EPOCH_MISMATCH, 2005).
-define(REFUSAL_CONTRACT_PRECONDITION_FAILED, 2006).
-define(REFUSAL_CONTRACT_INVARIANT_VIOLATED, 2007).
-define(REFUSAL_CONTRACT_TIER_VIOLATION, 2008).
-define(REFUSAL_CONTRACT_CAPABILITY_DENIED, 2009).
-define(REFUSAL_CONTRACT_BOUNDARY_CROSSED, 2010).

%% Envelope Violations (2021-2040)
-define(REFUSAL_ENVELOPE_DURATION_EXCEEDED, 2021).
-define(REFUSAL_ENVELOPE_MEMORY_EXCEEDED, 2022).
-define(REFUSAL_ENVELOPE_CPU_EXCEEDED, 2023).
-define(REFUSAL_ENVELOPE_IO_EXCEEDED, 2024).
-define(REFUSAL_ENVELOPE_NETWORK_EXCEEDED, 2025).
-define(REFUSAL_ENVELOPE_RATE_EXCEEDED, 2026).
-define(REFUSAL_ENVELOPE_CONCURRENT_EXCEEDED, 2027).
-define(REFUSAL_ENVELOPE_PAYLOAD_EXCEEDED, 2028).
-define(REFUSAL_ENVELOPE_RECURSION_EXCEEDED, 2029).
-define(REFUSAL_ENVELOPE_OPERATION_DENIED, 2030).
-define(REFUSAL_ENVELOPE_FAIL_CLOSED, 2031).
-define(REFUSAL_ENVELOPE_UNCERTAINTY, 2032).

%% Kill Switch (2041-2050)
-define(REFUSAL_KILL_SWITCH_GLOBAL, 2041).
-define(REFUSAL_KILL_SWITCH_FAMILY, 2042).
-define(REFUSAL_KILL_SWITCH_CAPABILITY, 2043).
-define(REFUSAL_KILL_SWITCH_EPOCH, 2044).
-define(REFUSAL_KILL_SWITCH_ACTIVE, 2045).

%%====================================================================
%% Refusal Record - Governance Denial
%%====================================================================

-record(mcp_refusal, {
    code :: pos_integer(),                  % Refusal code (2001-2050)
    reason :: binary(),                     % Human-readable reason
    timestamp :: timestamp_ms(),            % When refusal occurred
    contract_id :: contract_id() | undefined,
    details :: map() | undefined            % Additional context
}).

-type mcp_refusal() :: #mcp_refusal{}.

%%====================================================================
%% Receipt Record - Cryptographic Audit Trail
%%====================================================================

-record(mcp_receipt, {
    %% Identity
    id :: binary(),                         % Receipt ID (hash-based)
    sequence :: pos_integer(),              % Monotonic sequence number

    %% Request Context (structure only, no payload text)
    request_id :: binary(),
    contract_id :: contract_id(),
    envelope_id :: binary(),
    method :: binary(),

    %% Hashes (text-blind)
    request_hash :: hash(),                 % hash(request payload)
    response_hash :: hash(),                % hash(response payload)

    %% Outcome
    outcome :: ok | refused | error,
    refusal_code :: pos_integer() | undefined,

    %% Metrics (quantified, no interpretation)
    duration_us :: pos_integer(),           % Execution duration
    memory_bytes :: pos_integer(),          % Peak memory
    cpu_us :: pos_integer(),                % CPU time
    io_bytes :: pos_integer(),              % I/O bytes

    %% Chain
    previous_hash :: hash() | undefined,    % Previous receipt hash

    %% Signature
    epoch :: epoch(),
    signature :: signature(),

    %% Timing
    timestamp :: timestamp_ms()
}).

-type mcp_receipt() :: #mcp_receipt{}.

%%====================================================================
%% Evidence Bundle Record - Verifiable Proof Package (.mcpb)
%%====================================================================

-record(mcp_evidence_bundle, {
    %% Identity
    id :: binary(),
    version :: binary(),

    %% Scope
    contract_id :: contract_id(),
    time_range :: {timestamp_ms(), timestamp_ms()},

    %% Receipts (chained)
    receipts :: [#mcp_receipt{}],
    receipt_merkle_root :: hash(),

    %% Metrics Aggregates
    total_requests :: pos_integer(),
    total_refusals :: pos_integer(),
    refusal_breakdown :: #{pos_integer() => pos_integer()},

    %% Latency Distribution
    latency_p50_us :: pos_integer(),
    latency_p95_us :: pos_integer(),
    latency_p99_us :: pos_integer(),

    %% Resource Usage
    total_memory_bytes :: pos_integer(),
    total_cpu_us :: pos_integer(),
    total_io_bytes :: pos_integer(),

    %% Verification
    bundle_hash :: hash(),
    bundle_signature :: signature(),

    %% Provenance
    created_at :: timestamp_ms(),
    created_by :: binary()                  % Node/service identifier
}).

-type mcp_evidence_bundle() :: #mcp_evidence_bundle{}.

%%====================================================================
%% Verification Result Record
%%====================================================================

-record(mcp_verification_result, {
    %% Outcome
    passed :: boolean(),

    %% Checks Performed
    checks :: [{binary(), boolean(), binary()}],  % {check_name, passed, details}

    %% Verification Metadata
    bundle_id :: binary(),
    verified_at :: timestamp_ms(),
    verifier_version :: binary(),

    %% Signature
    verifier_signature :: signature()
}).

-type mcp_verification_result() :: #mcp_verification_result{}.

%%====================================================================
%% Kill Switch Record
%%====================================================================

-record(mcp_kill_switch, {
    %% Scope
    scope :: global | family | capability | epoch,
    target :: binary() | epoch(),           % Family name, capability, or epoch number

    %% Authority
    authorized_by :: binary(),
    authorization_signature :: signature(),

    %% Timing
    activated_at :: timestamp_ms(),
    expires_at :: timestamp_ms() | undefined,

    %% Reason (structure only)
    reason_code :: pos_integer(),
    reason_hash :: hash()                   % Hash of reason text
}).

-type mcp_kill_switch() :: #mcp_kill_switch{}.

%%====================================================================
%% Governance Configuration
%%====================================================================

-record(mcp_governance_config, {
    %% Contract Authority
    contract_authority :: binary(),         % Entity authorized to approve contracts
    signing_key_public :: binary(),
    current_epoch :: epoch(),

    %% Envelope Defaults
    default_envelope :: #mcp_envelope{},

    %% Verification Settings
    text_blind :: boolean(),                % Enforce text-blind verification
    receipt_chain_depth :: pos_integer(),   % How many receipts to chain
    bundle_interval_ms :: pos_integer(),    % How often to create bundles

    %% Kill Switch Authority
    kill_switch_authorities :: [binary()],  % Entities that can activate kill switch

    %% Retention
    receipt_retention_days :: pos_integer(),
    bundle_retention_days :: pos_integer()
}).

-type mcp_governance_config() :: #mcp_governance_config{}.

%%====================================================================
%% Operation Context (passed through execution)
%%====================================================================

-record(mcp_governance_context, {
    %% Active Governance
    contract :: #mcp_contract{} | undefined,
    envelope :: #mcp_envelope{} | undefined,

    %% Tracking
    request_id :: binary(),
    start_time :: timestamp_ms(),

    %% Metrics Accumulator
    memory_used :: pos_integer(),
    cpu_used :: pos_integer(),
    io_used :: pos_integer(),

    %% Chain
    parent_receipt :: #mcp_receipt{} | undefined
}).

-type mcp_governance_context() :: #mcp_governance_context{}.

%%====================================================================
%% Type Exports
%%====================================================================

-export_type([
    contract_id/0,
    contract_family/0,
    epoch/0,
    signature/0,
    hash/0,
    timestamp_ms/0,
    automation_tier/0,
    mcp_contract/0,
    mcp_envelope/0,
    mcp_receipt/0,
    mcp_evidence_bundle/0,
    mcp_verification_result/0,
    mcp_kill_switch/0,
    mcp_governance_config/0,
    mcp_governance_context/0
]).

-endif.
