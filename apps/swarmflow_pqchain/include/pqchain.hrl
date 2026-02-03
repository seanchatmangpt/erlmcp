%%% @doc Post-Quantum Blockchain Data Structures
%%%
%%% Core records and types for a post-quantum blockchain built on SwarmFlow OS.
%%% Implements NIST PQC standards: ML-KEM (FIPS 203), ML-DSA (FIPS 204), SLH-DSA (FIPS 205).
%%%
%%% Architecture:
%%% - Every consensus round is a supervised gen_statem
%%% - Every peer channel is a supervised process with crypto suite state
%%% - Crypto policy changes are workflow guards with canary rollout
%%% - Smart contracts are workflow nets with receipts
%%%
%%% @end
-ifndef(PQCHAIN_HRL).
-define(PQCHAIN_HRL, true).

%%% ============================================================================
%%% PQC Algorithm Constants
%%% ============================================================================

%% FIPS 203: ML-KEM (Key Encapsulation Mechanism derived from CRYSTALS-Kyber)
-define(PQC_KEM_ML_KEM_512, ml_kem_512).      % NIST Level 1 (128-bit security)
-define(PQC_KEM_ML_KEM_768, ml_kem_768).      % NIST Level 3 (192-bit security)
-define(PQC_KEM_ML_KEM_1024, ml_kem_1024).    % NIST Level 5 (256-bit security)

%% FIPS 204: ML-DSA (Digital Signature Algorithm derived from CRYSTALS-Dilithium)
-define(PQC_SIG_ML_DSA_44, ml_dsa_44).        % NIST Level 2
-define(PQC_SIG_ML_DSA_65, ml_dsa_65).        % NIST Level 3 (recommended default)
-define(PQC_SIG_ML_DSA_87, ml_dsa_87).        % NIST Level 5

%% FIPS 205: SLH-DSA (Stateless Hash-Based Digital Signature derived from SPHINCS+)
-define(PQC_SIG_SLH_DSA_128S, slh_dsa_128s).  % Small signatures, slower
-define(PQC_SIG_SLH_DSA_128F, slh_dsa_128f).  % Fast signatures, larger
-define(PQC_SIG_SLH_DSA_192S, slh_dsa_192s).
-define(PQC_SIG_SLH_DSA_192F, slh_dsa_192f).
-define(PQC_SIG_SLH_DSA_256S, slh_dsa_256s).
-define(PQC_SIG_SLH_DSA_256F, slh_dsa_256f).

%% Classical algorithms (for hybrid/migration)
-define(CLASSIC_SIG_ED25519, ed25519).
-define(CLASSIC_SIG_SECP256K1, secp256k1).
-define(CLASSIC_KEM_X25519, x25519).

%% Hash algorithms
-define(HASH_SHA3_256, sha3_256).
-define(HASH_SHA3_512, sha3_512).
-define(HASH_BLAKE3, blake3).
-define(HASH_KECCAK_256, keccak_256).

%%% ============================================================================
%%% Key Material Records
%%% ============================================================================

%% PQC Key Pair
-record(pqc_keypair, {
    algorithm :: pqc_sig_algorithm() | pqc_kem_algorithm(),
    public_key :: binary(),
    secret_key :: binary() | undefined,  % undefined for public-only
    created_at :: non_neg_integer(),
    metadata :: map()
}).

%% Key Encapsulation Result
-record(pqc_encapsulation, {
    algorithm :: pqc_kem_algorithm(),
    ciphertext :: binary(),
    shared_secret :: binary()
}).

%% Hybrid Key (classical + PQC for transition period)
-record(hybrid_keypair, {
    classical :: #pqc_keypair{},
    pqc :: #pqc_keypair{},
    binding_nonce :: binary()  % ties the two keys cryptographically
}).

%%% ============================================================================
%%% Signature Records
%%% ============================================================================

%% PQC Signature
-record(pqc_signature, {
    algorithm :: pqc_sig_algorithm(),
    signature :: binary(),
    public_key_hash :: binary(),  % hash of signing key (address derivation)
    timestamp :: non_neg_integer()
}).

%% Hybrid Signature (classical + PQC)
-record(hybrid_signature, {
    classical :: #pqc_signature{},
    pqc :: #pqc_signature{},
    binding_proof :: binary()  % proves both signed same message
}).

%% Multi-Signature (for validators)
-record(multi_signature, {
    threshold :: pos_integer(),
    signers :: [binary()],         % list of public key hashes
    signatures :: [#pqc_signature{} | #hybrid_signature{}],
    aggregated :: binary() | undefined  % optional aggregated form
}).

%%% ============================================================================
%%% Identity and Address Records
%%% ============================================================================

%% Account Identity
-record(pqc_identity, {
    id :: binary(),                     % ULID
    address :: binary(),                % hash of primary public key
    primary_key :: #pqc_keypair{},      % main signing key (public only stored)
    backup_key :: #pqc_keypair{} | undefined,  % SLH-DSA fallback
    kem_key :: #pqc_keypair{} | undefined,     % for encrypted communications
    registered_at :: non_neg_integer(),
    key_policy :: key_policy(),
    metadata :: map()
}).

%% Key Registration Transaction (anchors PQC key on-chain)
-record(key_registration, {
    id :: binary(),
    address :: binary(),
    algorithm :: pqc_sig_algorithm(),
    public_key :: binary(),
    proof_of_possession :: #pqc_signature{},  % proves ownership of secret key
    registration_type :: primary | backup | rotation,
    previous_key_hash :: binary() | undefined,
    timestamp :: non_neg_integer()
}).

%% Key Rotation Request
-record(key_rotation, {
    id :: binary(),
    address :: binary(),
    old_key_hash :: binary(),
    new_key :: #pqc_keypair{},
    authorization :: #pqc_signature{},  % signed by old key
    effective_height :: non_neg_integer(),
    grace_period_blocks :: non_neg_integer()
}).

%%% ============================================================================
%%% Transaction Records
%%% ============================================================================

%% Transaction Types
-define(TX_TYPE_TRANSFER, transfer).
-define(TX_TYPE_KEY_REGISTRATION, key_registration).
-define(TX_TYPE_KEY_ROTATION, key_rotation).
-define(TX_TYPE_CONTRACT_DEPLOY, contract_deploy).
-define(TX_TYPE_CONTRACT_CALL, contract_call).
-define(TX_TYPE_GOVERNANCE, governance).
-define(TX_TYPE_VALIDATOR_STAKE, validator_stake).
-define(TX_TYPE_VALIDATOR_UNSTAKE, validator_unstake).

%% Transaction Envelope
-record(pqc_transaction, {
    id :: binary(),                      % ULID
    type :: tx_type(),
    version :: pos_integer(),
    chain_id :: binary(),
    nonce :: non_neg_integer(),
    sender :: binary(),                  % address
    recipient :: binary() | undefined,   % address or undefined for deploys
    amount :: non_neg_integer(),
    fee :: non_neg_integer(),
    gas_limit :: non_neg_integer(),
    gas_price :: non_neg_integer(),
    payload :: term(),                   % type-specific data
    signature :: #pqc_signature{} | #hybrid_signature{},
    created_at :: non_neg_integer(),
    expires_at :: non_neg_integer() | undefined
}).

%% Transfer Payload
-record(transfer_payload, {
    memo :: binary() | undefined,
    encrypted_memo :: binary() | undefined  % encrypted with recipient's KEM key
}).

%% Contract Deploy Payload
-record(contract_deploy_payload, {
    workflow_net :: term(),              % SwarmFlow workflow net definition
    initial_state :: map(),
    constructor_args :: [term()]
}).

%% Contract Call Payload
-record(contract_call_payload, {
    contract_address :: binary(),
    transition :: atom(),                % workflow transition to fire
    arguments :: [term()],
    artifacts :: [binary()]              % A2A artifact references
}).

%%% ============================================================================
%%% Block Records
%%% ============================================================================

%% Block Header
-record(pqc_block_header, {
    version :: pos_integer(),
    height :: non_neg_integer(),
    timestamp :: non_neg_integer(),
    prev_hash :: binary(),
    state_root :: binary(),              % merkle root of world state
    tx_root :: binary(),                 % merkle root of transactions
    receipt_root :: binary(),            % merkle root of execution receipts
    validator :: binary(),               % proposer address
    validator_signature :: #pqc_signature{},
    consensus_proof :: consensus_proof(),
    chain_id :: binary()
}).

%% Full Block
-record(pqc_block, {
    header :: #pqc_block_header{},
    transactions :: [#pqc_transaction{}],
    receipts :: [#execution_receipt{}],
    validator_set_hash :: binary(),
    next_validator_set_hash :: binary() | undefined
}).

%% Execution Receipt (per transaction)
-record(execution_receipt, {
    tx_id :: binary(),
    status :: success | failure | reverted,
    gas_used :: non_neg_integer(),
    state_changes :: [state_change()],
    events :: [workflow_event()],
    error :: binary() | undefined
}).

%% State Change
-record(state_change, {
    address :: binary(),
    key :: binary(),
    old_value :: binary() | undefined,
    new_value :: binary() | undefined
}).

%%% ============================================================================
%%% Consensus Records (BFT with PQC)
%%% ============================================================================

%% Validator
-record(pqc_validator, {
    address :: binary(),
    public_key :: #pqc_keypair{},
    voting_power :: non_neg_integer(),
    stake :: non_neg_integer(),
    commission_rate :: float(),
    status :: active | jailed | unbonding | inactive,
    jailed_until :: non_neg_integer() | undefined,
    last_proposed :: non_neg_integer(),
    last_voted :: non_neg_integer(),
    uptime :: float(),
    metadata :: map()
}).

%% Validator Set
-record(validator_set, {
    epoch :: non_neg_integer(),
    validators :: [#pqc_validator{}],
    total_voting_power :: non_neg_integer(),
    proposer_selection :: round_robin | weighted_random | vrf,
    quorum_threshold :: float(),  % typically 2/3
    created_at :: non_neg_integer()
}).

%% Consensus Round State (gen_statem state)
-record(consensus_round, {
    height :: non_neg_integer(),
    round :: non_neg_integer(),
    phase :: propose | prevote | precommit | commit | finalized,
    proposer :: binary(),
    proposal :: #pqc_block{} | undefined,
    prevotes :: #{binary() => #pqc_signature{}},
    precommits :: #{binary() => #pqc_signature{}},
    locked_block :: #pqc_block{} | undefined,
    locked_round :: non_neg_integer() | undefined,
    valid_block :: #pqc_block{} | undefined,
    valid_round :: non_neg_integer() | undefined,
    started_at :: non_neg_integer(),
    timeout_ms :: non_neg_integer()
}).

%% Consensus Vote
-record(consensus_vote, {
    type :: prevote | precommit,
    height :: non_neg_integer(),
    round :: non_neg_integer(),
    block_hash :: binary() | nil,  % nil for nil vote
    validator :: binary(),
    signature :: #pqc_signature{},
    timestamp :: non_neg_integer()
}).

%% Quorum Certificate (aggregated proof of consensus)
-record(quorum_certificate, {
    height :: non_neg_integer(),
    round :: non_neg_integer(),
    block_hash :: binary(),
    vote_type :: prevote | precommit,
    validators :: [binary()],
    signatures :: [#pqc_signature{}],
    aggregated_signature :: binary() | undefined,
    voting_power :: non_neg_integer(),
    timestamp :: non_neg_integer()
}).

%% Consensus Proof (included in block header)
-record(consensus_proof, {
    type :: bft | pos_committee | hybrid,
    quorum_certificate :: #quorum_certificate{},
    commit_signatures :: [#pqc_signature{}],
    validator_set_hash :: binary()
}).

%%% ============================================================================
%%% Peer Network Records (ML-KEM channels)
%%% ============================================================================

%% Peer Identity
-record(peer_identity, {
    id :: binary(),
    address :: binary(),                 % blockchain address
    node_id :: binary(),                 % network node ID
    kem_public_key :: binary(),          % ML-KEM public key
    sig_public_key :: binary(),          % ML-DSA public key
    endpoints :: [peer_endpoint()],
    capabilities :: [atom()],
    version :: binary(),
    last_seen :: non_neg_integer()
}).

%% Peer Endpoint
-record(peer_endpoint, {
    protocol :: tcp | quic | websocket,
    host :: binary(),
    port :: inet:port_number(),
    priority :: non_neg_integer()
}).

%% Secure Channel (established with ML-KEM)
-record(secure_channel, {
    id :: binary(),
    local_peer :: binary(),
    remote_peer :: binary(),
    kem_algorithm :: pqc_kem_algorithm(),
    shared_secret :: binary(),
    session_keys :: session_keys(),
    established_at :: non_neg_integer(),
    last_rekey :: non_neg_integer(),
    rekey_interval_ms :: non_neg_integer(),
    messages_sent :: non_neg_integer(),
    messages_received :: non_neg_integer()
}).

%% Session Keys (derived from KEM shared secret)
-record(session_keys, {
    send_key :: binary(),
    receive_key :: binary(),
    send_nonce :: non_neg_integer(),
    receive_nonce :: non_neg_integer(),
    chain_key :: binary()  % for forward secrecy
}).

%% Network Message Envelope
-record(network_message, {
    id :: binary(),
    type :: message_type(),
    sender :: binary(),
    recipient :: binary() | broadcast,
    payload :: binary(),
    signature :: #pqc_signature{},
    encrypted :: boolean(),
    timestamp :: non_neg_integer(),
    ttl :: non_neg_integer()
}).

%%% ============================================================================
%%% Crypto Policy Records (Crypto-Agility)
%%% ============================================================================

%% Crypto Policy (workflow guard for algorithm selection)
-record(crypto_policy, {
    id :: binary(),
    name :: binary(),
    version :: pos_integer(),
    status :: draft | canary | active | deprecated | sunset,

    %% Signature requirements
    sig_algorithms :: [pqc_sig_algorithm()],
    sig_required :: pqc_only | hybrid | classical_allowed,
    sig_min_security_level :: pos_integer(),

    %% KEM requirements
    kem_algorithms :: [pqc_kem_algorithm()],
    kem_required :: pqc_only | hybrid | classical_allowed,
    kem_min_security_level :: pos_integer(),

    %% Hash requirements
    hash_algorithms :: [hash_algorithm()],
    hash_min_bits :: pos_integer(),

    %% Rollout configuration
    canary_percentage :: float(),
    canary_cohort :: [binary()],  % addresses in canary group
    effective_height :: non_neg_integer(),
    sunset_height :: non_neg_integer() | undefined,

    %% Metadata
    rationale :: binary(),
    approved_by :: [binary()],
    created_at :: non_neg_integer()
}).

%% Policy Transition (staged rollout)
-record(policy_transition, {
    id :: binary(),
    from_policy :: binary(),
    to_policy :: binary(),
    phase :: announced | canary | ramping | active | completed,
    ramp_percentage :: float(),
    ramp_increment :: float(),
    ramp_interval_blocks :: non_neg_integer(),
    rollback_trigger :: rollback_trigger(),
    metrics :: policy_metrics(),
    started_at :: non_neg_integer()
}).

%% Rollback Trigger
-record(rollback_trigger, {
    failure_rate_threshold :: float(),
    latency_threshold_ms :: non_neg_integer(),
    error_count_threshold :: non_neg_integer(),
    window_blocks :: non_neg_integer()
}).

%% Policy Metrics
-record(policy_metrics, {
    transactions_processed :: non_neg_integer(),
    verification_failures :: non_neg_integer(),
    avg_verification_time_us :: float(),
    p99_verification_time_us :: float(),
    rollback_count :: non_neg_integer()
}).

%%% ============================================================================
%%% Smart Contract Records (Workflow Nets)
%%% ============================================================================

%% Contract Definition (stored on-chain as workflow net)
-record(pqc_contract, {
    address :: binary(),
    creator :: binary(),
    workflow_net_id :: binary(),         % SwarmFlow net ID
    code_hash :: binary(),
    state_root :: binary(),
    balance :: non_neg_integer(),
    nonce :: non_neg_integer(),
    created_at_height :: non_neg_integer(),
    metadata :: map()
}).

%% Contract Instance (runtime state)
-record(contract_instance, {
    address :: binary(),
    case_id :: binary(),                 % SwarmFlow case ID
    marking :: map(),                    % current Petri net marking
    variables :: map(),
    pending_transitions :: [atom()],
    last_transition :: atom() | undefined,
    last_tx_id :: binary() | undefined
}).

%% Contract Event (emitted during execution)
-record(contract_event, {
    contract_address :: binary(),
    name :: atom(),
    data :: term(),
    indexed :: [term()],
    tx_id :: binary(),
    block_height :: non_neg_integer(),
    log_index :: non_neg_integer()
}).

%%% ============================================================================
%%% Governance Records
%%% ============================================================================

%% Governance Proposal
-record(governance_proposal, {
    id :: binary(),
    type :: crypto_policy | validator_set | parameter | upgrade | emergency,
    title :: binary(),
    description :: binary(),
    proposer :: binary(),
    deposit :: non_neg_integer(),

    %% Proposal content
    changes :: [governance_change()],

    %% Voting
    status :: pending | voting | passed | rejected | executed | vetoed,
    votes_for :: non_neg_integer(),
    votes_against :: non_neg_integer(),
    votes_abstain :: non_neg_integer(),
    quorum_reached :: boolean(),

    %% Timeline
    submit_height :: non_neg_integer(),
    voting_start_height :: non_neg_integer(),
    voting_end_height :: non_neg_integer(),
    execution_height :: non_neg_integer() | undefined
}).

%% Governance Change
-record(governance_change, {
    type :: parameter | policy | code,
    target :: binary(),                  % what's being changed
    old_value :: term(),
    new_value :: term(),
    rationale :: binary()
}).

%% Governance Vote
-record(governance_vote, {
    proposal_id :: binary(),
    voter :: binary(),
    vote :: yes | no | abstain | no_with_veto,
    voting_power :: non_neg_integer(),
    signature :: #pqc_signature{},
    timestamp :: non_neg_integer()
}).

%%% ============================================================================
%%% Chain State Records
%%% ============================================================================

%% Chain State
-record(chain_state, {
    chain_id :: binary(),
    height :: non_neg_integer(),
    last_block_hash :: binary(),
    last_block_time :: non_neg_integer(),
    state_root :: binary(),
    validator_set :: #validator_set{},
    crypto_policy :: #crypto_policy{},
    total_supply :: non_neg_integer(),
    inflation_rate :: float(),
    parameters :: chain_parameters()
}).

%% Chain Parameters
-record(chain_parameters, {
    block_time_ms :: non_neg_integer(),
    max_block_size :: non_neg_integer(),
    max_tx_per_block :: non_neg_integer(),
    min_gas_price :: non_neg_integer(),
    max_gas_per_block :: non_neg_integer(),

    %% Validator parameters
    min_stake :: non_neg_integer(),
    unbonding_period_blocks :: non_neg_integer(),
    max_validators :: non_neg_integer(),
    slash_fraction_double_sign :: float(),
    slash_fraction_downtime :: float(),

    %% Governance parameters
    min_proposal_deposit :: non_neg_integer(),
    voting_period_blocks :: non_neg_integer(),
    quorum_fraction :: float(),
    threshold_fraction :: float(),
    veto_threshold_fraction :: float()
}).

%%% ============================================================================
%%% Mempool Records
%%% ============================================================================

%% Mempool Entry
-record(mempool_entry, {
    tx :: #pqc_transaction{},
    received_at :: non_neg_integer(),
    validated :: boolean(),
    priority :: non_neg_integer(),
    gas_price :: non_neg_integer(),
    sender_nonce :: non_neg_integer(),
    expiry :: non_neg_integer()
}).

%% Mempool State
-record(mempool_state, {
    transactions :: #{binary() => #mempool_entry{}},
    by_sender :: #{binary() => [binary()]},
    by_priority :: gb_trees:tree(),
    size :: non_neg_integer(),
    max_size :: non_neg_integer(),
    total_gas :: non_neg_integer()
}).

%%% ============================================================================
%%% Type Definitions
%%% ============================================================================

-type pqc_sig_algorithm() :: ml_dsa_44 | ml_dsa_65 | ml_dsa_87 |
                             slh_dsa_128s | slh_dsa_128f |
                             slh_dsa_192s | slh_dsa_192f |
                             slh_dsa_256s | slh_dsa_256f |
                             ed25519 | secp256k1.

-type pqc_kem_algorithm() :: ml_kem_512 | ml_kem_768 | ml_kem_1024 | x25519.

-type hash_algorithm() :: sha3_256 | sha3_512 | blake3 | keccak_256.

-type tx_type() :: transfer | key_registration | key_rotation |
                   contract_deploy | contract_call | governance |
                   validator_stake | validator_unstake.

-type consensus_proof() :: #consensus_proof{}.

-type state_change() :: #state_change{}.

-type workflow_event() :: term().

-type peer_endpoint() :: #peer_endpoint{}.

-type session_keys() :: #session_keys{}.

-type message_type() :: handshake | consensus | transaction | sync | gossip.

-type rollback_trigger() :: #rollback_trigger{}.

-type policy_metrics() :: #policy_metrics{}.

-type governance_change() :: #governance_change{}.

-type chain_parameters() :: #chain_parameters{}.

-type key_policy() :: #{
    require_backup => boolean(),
    auto_rotate_blocks => non_neg_integer(),
    allowed_algorithms => [pqc_sig_algorithm()]
}.

%%% ============================================================================
%%% Error Codes
%%% ============================================================================

-define(ERR_INVALID_SIGNATURE, -34001).
-define(ERR_UNKNOWN_ALGORITHM, -34002).
-define(ERR_KEY_NOT_REGISTERED, -34003).
-define(ERR_KEY_EXPIRED, -34004).
-define(ERR_POLICY_VIOLATION, -34005).
-define(ERR_INSUFFICIENT_STAKE, -34006).
-define(ERR_INVALID_VALIDATOR, -34007).
-define(ERR_CONSENSUS_TIMEOUT, -34008).
-define(ERR_INVALID_BLOCK, -34009).
-define(ERR_INVALID_TRANSACTION, -34010).
-define(ERR_NONCE_TOO_LOW, -34011).
-define(ERR_NONCE_TOO_HIGH, -34012).
-define(ERR_INSUFFICIENT_BALANCE, -34013).
-define(ERR_GAS_LIMIT_EXCEEDED, -34014).
-define(ERR_CONTRACT_EXECUTION_FAILED, -34015).
-define(ERR_INVALID_PROOF, -34016).
-define(ERR_QUORUM_NOT_REACHED, -34017).
-define(ERR_DOUBLE_VOTE, -34018).
-define(ERR_FUTURE_BLOCK, -34019).
-define(ERR_ORPHAN_BLOCK, -34020).
-define(ERR_KEM_ENCAPSULATION_FAILED, -34021).
-define(ERR_KEM_DECAPSULATION_FAILED, -34022).
-define(ERR_CHANNEL_EXPIRED, -34023).
-define(ERR_PEER_NOT_FOUND, -34024).
-define(ERR_GOVERNANCE_PROPOSAL_INVALID, -34025).
-define(ERR_VOTING_PERIOD_ENDED, -34026).
-define(ERR_ALREADY_VOTED, -34027).

-endif. % PQCHAIN_HRL
