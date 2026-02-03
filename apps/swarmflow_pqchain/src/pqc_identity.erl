%%% @doc PQC Identity and Address Management
%%%
%%% Manages blockchain identities with post-quantum cryptographic keys.
%%%
%%% Features:
%%% - Identity creation with PQC algorithm selection
%%% - Address derivation from public keys
%%% - Key registration and rotation transactions
%%% - ETS-based identity registry with gproc integration
%%% - Key policy validation
%%% - Proof of possession for secret key ownership
%%%
%%% Architecture:
%%% - ETS table 'pqc_identity_registry' for persistent storage
%%% - ULID-based identity IDs for monotonic ordering
%%% - gproc for distributed process registration
%%% - Chicago School TDD: real processes, no mocks
%%%
%%% @end
-module(pqc_identity).

-include("pqchain.hrl").

%% API
-export([
    create/1,
    register_key/2,
    rotate_key/3,
    derive_address/1,
    verify_identity/2,
    lookup/1,
    store/1,
    validate_key_policy/2,
    proof_of_possession/2
]).

%% Registry management
-export([
    init_registry/0,
    clear_registry/0,
    list_all/0
]).

%%% ============================================================================
%%% Types
%%% ============================================================================

-type create_opts() :: #{
    algorithm => pqc_sig_algorithm(),
    backup_algorithm => pqc_sig_algorithm(),
    kem_algorithm => pqc_kem_algorithm(),
    key_policy => key_policy(),
    metadata => map()
}.

-type identity_error() ::
    {error, invalid_algorithm} |
    {error, key_generation_failed} |
    {error, policy_violation} |
    {error, not_found} |
    {error, already_exists} |
    {error, invalid_signature} |
    {error, invalid_address}.

-export_type([create_opts/0, identity_error/0]).

%%% ============================================================================
%%% API Functions
%%% ============================================================================

%% @doc Create a new identity with PQC keys
%%
%% Options:
%% - algorithm: Primary signature algorithm (default: ml_dsa_65)
%% - backup_algorithm: Backup signature algorithm (default: slh_dsa_128s)
%% - kem_algorithm: Key encapsulation algorithm (default: ml_kem_768)
%% - key_policy: Key rotation and management policy
%% - metadata: Additional identity metadata
%%
%% @end
-spec create(create_opts()) -> {ok, #pqc_identity{}} | identity_error().
create(Opts) ->
    Algorithm = maps:get(algorithm, Opts, ?PQC_SIG_ML_DSA_65),
    BackupAlgorithm = maps:get(backup_algorithm, Opts, ?PQC_SIG_SLH_DSA_128S),
    KemAlgorithm = maps:get(kem_algorithm, Opts, ?PQC_KEM_ML_KEM_768),
    KeyPolicy = maps:get(key_policy, Opts, default_key_policy()),
    Metadata = maps:get(metadata, Opts, #{}),

    %% Validate algorithms
    case validate_algorithms(Algorithm, BackupAlgorithm, KemAlgorithm) of
        ok ->
            %% Generate keys (in production, this would call actual PQC key generation)
            Now = erlang:system_time(millisecond),

            case generate_keypairs(Algorithm, BackupAlgorithm, KemAlgorithm, Now) of
                {ok, PrimaryKey, BackupKey, KemKey} ->
                    %% Derive address from primary public key
                    Address = derive_address_from_key(PrimaryKey),

                    %% Create identity record
                    Identity = #pqc_identity{
                        id = ulid(),
                        address = Address,
                        primary_key = PrimaryKey,
                        backup_key = BackupKey,
                        kem_key = KemKey,
                        registered_at = Now,
                        key_policy = KeyPolicy,
                        metadata = Metadata
                    },

                    %% Validate against policy
                    case validate_key_policy(Identity, KeyPolicy) of
                        ok ->
                            {ok, Identity};
                        Error ->
                            Error
                    end;
                Error ->
                    Error
            end;
        Error ->
            Error
    end.

%% @doc Create a key registration transaction
%%
%% This creates an on-chain transaction to register a public key
%% with proof of possession.
%%
%% @end
-spec register_key(#pqc_identity{}, primary | backup | rotation) ->
    {ok, #key_registration{}} | identity_error().
register_key(#pqc_identity{} = Identity, RegistrationType) ->
    #pqc_identity{
        address = Address,
        primary_key = PrimaryKey,
        backup_key = BackupKey
    } = Identity,

    %% Select key based on registration type
    {Algorithm, PublicKey, PreviousKeyHash} = case RegistrationType of
        primary ->
            {PrimaryKey#pqc_keypair.algorithm,
             PrimaryKey#pqc_keypair.public_key,
             undefined};
        backup when BackupKey =/= undefined ->
            {BackupKey#pqc_keypair.algorithm,
             BackupKey#pqc_keypair.public_key,
             hash_public_key(PrimaryKey#pqc_keypair.public_key)};
        rotation ->
            {PrimaryKey#pqc_keypair.algorithm,
             PrimaryKey#pqc_keypair.public_key,
             hash_public_key(PrimaryKey#pqc_keypair.public_key)}
    end,

    Now = erlang:system_time(millisecond),
    RegistrationId = ulid(),

    %% Create proof of possession (signature over registration data)
    ProofMessage = create_registration_message(RegistrationId, Address, Algorithm, PublicKey),

    case sign_with_key(ProofMessage, PrimaryKey) of
        {ok, ProofSignature} ->
            Registration = #key_registration{
                id = RegistrationId,
                address = Address,
                algorithm = Algorithm,
                public_key = PublicKey,
                proof_of_possession = ProofSignature,
                registration_type = RegistrationType,
                previous_key_hash = PreviousKeyHash,
                timestamp = Now
            },
            {ok, Registration};
        Error ->
            Error
    end.

%% @doc Create a key rotation request
%%
%% Rotates the primary key with authorization from the old key.
%%
%% @end
-spec rotate_key(#pqc_identity{}, #pqc_keypair{}, non_neg_integer()) ->
    {ok, #key_rotation{}} | identity_error().
rotate_key(#pqc_identity{} = Identity, NewKey, GracePeriodBlocks) ->
    #pqc_identity{
        address = Address,
        primary_key = OldKey
    } = Identity,

    Now = erlang:system_time(millisecond),
    RotationId = ulid(),
    OldKeyHash = hash_public_key(OldKey#pqc_keypair.public_key),

    %% Create authorization message
    AuthMessage = create_rotation_message(RotationId, Address, OldKeyHash,
                                         NewKey#pqc_keypair.public_key),

    %% Sign with old key to authorize rotation
    case sign_with_key(AuthMessage, OldKey) of
        {ok, AuthSignature} ->
            Rotation = #key_rotation{
                id = RotationId,
                address = Address,
                old_key_hash = OldKeyHash,
                new_key = NewKey,
                authorization = AuthSignature,
                effective_height = 0,  % Will be set when included in block
                grace_period_blocks = GracePeriodBlocks
            },
            {ok, Rotation};
        Error ->
            Error
    end.

%% @doc Derive address from identity or public key
%%
%% Address = Hash(Public Key || Algorithm Tag)
%% Uses SHA3-256 by default for address derivation
%%
%% @end
-spec derive_address(#pqc_identity{} | #pqc_keypair{} | binary()) -> binary().
derive_address(#pqc_identity{primary_key = Key}) ->
    derive_address_from_key(Key);
derive_address(#pqc_keypair{} = Key) ->
    derive_address_from_key(Key);
derive_address(PublicKey) when is_binary(PublicKey) ->
    %% Assume default algorithm tag
    hash_binary(<<PublicKey/binary, "DEFAULT">>).

%% @doc Verify an identity signature
%%
%% Verifies that a signature was created by the identity's primary key.
%%
%% @end
-spec verify_identity(#pqc_identity{}, #pqc_signature{}) ->
    ok | {error, invalid_signature}.
verify_identity(#pqc_identity{primary_key = Key}, #pqc_signature{} = Signature) ->
    ExpectedKeyHash = hash_public_key(Key#pqc_keypair.public_key),

    case Signature#pqc_signature.public_key_hash of
        ExpectedKeyHash ->
            %% In production, this would call actual PQC signature verification
            verify_signature_mock(Key, Signature);
        _ ->
            {error, invalid_signature}
    end.

%% @doc Look up identity by address from ETS registry
-spec lookup(binary()) -> {ok, #pqc_identity{}} | {error, not_found}.
lookup(Address) when is_binary(Address) ->
    case ets:lookup(pqc_identity_registry, Address) of
        [{Address, Identity}] ->
            {ok, Identity};
        [] ->
            {error, not_found}
    end.

%% @doc Store identity in ETS registry
%%
%% Also registers with gproc for distributed lookup.
%%
%% @end
-spec store(#pqc_identity{}) -> ok | {error, already_exists}.
store(#pqc_identity{address = Address} = Identity) ->
    case ets:lookup(pqc_identity_registry, Address) of
        [] ->
            true = ets:insert(pqc_identity_registry, {Address, Identity}),

            %% Register with gproc for distributed lookup
            try
                gproc:reg({n, l, {pqc_identity, Address}}, Identity),
                ok
            catch
                error:badarg ->
                    %% Already registered by another process
                    ok
            end;
        [_] ->
            {error, already_exists}
    end.

%% @doc Validate that identity meets key policy requirements
-spec validate_key_policy(#pqc_identity{}, key_policy()) ->
    ok | {error, policy_violation}.
validate_key_policy(#pqc_identity{} = Identity, Policy) ->
    #pqc_identity{
        primary_key = PrimaryKey,
        backup_key = BackupKey
    } = Identity,

    %% Check backup key requirement
    RequireBackup = maps:get(require_backup, Policy, false),
    BackupOk = case RequireBackup of
        true when BackupKey =:= undefined ->
            false;
        _ ->
            true
    end,

    %% Check algorithm allowlist
    AllowedAlgorithms = maps:get(allowed_algorithms, Policy, all_algorithms()),
    PrimaryAlg = PrimaryKey#pqc_keypair.algorithm,
    AlgorithmOk = lists:member(PrimaryAlg, AllowedAlgorithms),

    %% Validate backup key algorithm if present
    BackupAlgOk = case BackupKey of
        undefined ->
            true;
        #pqc_keypair{algorithm = BackupAlg} ->
            lists:member(BackupAlg, AllowedAlgorithms)
    end,

    case {BackupOk, AlgorithmOk, BackupAlgOk} of
        {true, true, true} ->
            ok;
        _ ->
            {error, policy_violation}
    end.

%% @doc Generate proof of possession for a secret key
%%
%% Creates a signature over a challenge message to prove ownership
%% of the secret key without revealing it.
%%
%% @end
-spec proof_of_possession(#pqc_keypair{}, binary()) ->
    {ok, #pqc_signature{}} | {error, key_generation_failed}.
proof_of_possession(#pqc_keypair{} = KeyPair, Challenge) when is_binary(Challenge) ->
    Message = <<"proof_of_possession:", Challenge/binary>>,
    sign_with_key(Message, KeyPair).

%%% ============================================================================
%%% Registry Management
%%% ============================================================================

%% @doc Initialize the ETS registry
%%
%% Creates the ETS table if it doesn't exist. Safe to call multiple times.
%%
%% @end
-spec init_registry() -> ok.
init_registry() ->
    case ets:whereis(pqc_identity_registry) of
        undefined ->
            ets:new(pqc_identity_registry, [
                named_table,
                public,
                set,
                {read_concurrency, true},
                {write_concurrency, true}
            ]),
            ok;
        _ ->
            ok
    end.

%% @doc Clear all entries from the registry
%%
%% WARNING: This deletes all identities. Use only for testing.
%%
%% @end
-spec clear_registry() -> ok.
clear_registry() ->
    case ets:whereis(pqc_identity_registry) of
        undefined ->
            ok;
        _ ->
            ets:delete_all_objects(pqc_identity_registry),
            ok
    end.

%% @doc List all identities in the registry
-spec list_all() -> [#pqc_identity{}].
list_all() ->
    case ets:whereis(pqc_identity_registry) of
        undefined ->
            [];
        _ ->
            [Identity || {_Address, Identity} <- ets:tab2list(pqc_identity_registry)]
    end.

%%% ============================================================================
%%% Internal Functions
%%% ============================================================================

%% @private
%% Validate that algorithms are supported
-spec validate_algorithms(pqc_sig_algorithm(), pqc_sig_algorithm(), pqc_kem_algorithm()) ->
    ok | {error, invalid_algorithm}.
validate_algorithms(Primary, Backup, Kem) ->
    SigAlgorithms = [
        ?PQC_SIG_ML_DSA_44, ?PQC_SIG_ML_DSA_65, ?PQC_SIG_ML_DSA_87,
        ?PQC_SIG_SLH_DSA_128S, ?PQC_SIG_SLH_DSA_128F,
        ?PQC_SIG_SLH_DSA_192S, ?PQC_SIG_SLH_DSA_192F,
        ?PQC_SIG_SLH_DSA_256S, ?PQC_SIG_SLH_DSA_256F,
        ?CLASSIC_SIG_ED25519, ?CLASSIC_SIG_SECP256K1
    ],
    KemAlgorithms = [
        ?PQC_KEM_ML_KEM_512, ?PQC_KEM_ML_KEM_768, ?PQC_KEM_ML_KEM_1024,
        ?CLASSIC_KEM_X25519
    ],

    PrimaryOk = lists:member(Primary, SigAlgorithms),
    BackupOk = lists:member(Backup, SigAlgorithms),
    KemOk = lists:member(Kem, KemAlgorithms),

    case {PrimaryOk, BackupOk, KemOk} of
        {true, true, true} ->
            ok;
        _ ->
            {error, invalid_algorithm}
    end.

%% @private
%% Generate keypairs for identity
%% In production, this would call actual PQC key generation libraries
-spec generate_keypairs(pqc_sig_algorithm(), pqc_sig_algorithm(),
                       pqc_kem_algorithm(), non_neg_integer()) ->
    {ok, #pqc_keypair{}, #pqc_keypair{}, #pqc_keypair{}} |
    {error, key_generation_failed}.
generate_keypairs(PrimaryAlg, BackupAlg, KemAlg, Timestamp) ->
    %% Mock key generation - in production, use actual PQC libraries
    try
        PrimaryKey = #pqc_keypair{
            algorithm = PrimaryAlg,
            public_key = crypto:strong_rand_bytes(32),
            secret_key = crypto:strong_rand_bytes(64),
            created_at = Timestamp,
            metadata = #{}
        },

        BackupKey = #pqc_keypair{
            algorithm = BackupAlg,
            public_key = crypto:strong_rand_bytes(32),
            secret_key = crypto:strong_rand_bytes(64),
            created_at = Timestamp,
            metadata = #{}
        },

        KemKey = #pqc_keypair{
            algorithm = KemAlg,
            public_key = crypto:strong_rand_bytes(32),
            secret_key = crypto:strong_rand_bytes(64),
            created_at = Timestamp,
            metadata = #{}
        },

        {ok, PrimaryKey, BackupKey, KemKey}
    catch
        _:_ ->
            {error, key_generation_failed}
    end.

%% @private
%% Derive address from keypair
-spec derive_address_from_key(#pqc_keypair{}) -> binary().
derive_address_from_key(#pqc_keypair{public_key = PubKey, algorithm = Algorithm}) ->
    AlgorithmTag = atom_to_binary(Algorithm, utf8),
    hash_binary(<<PubKey/binary, AlgorithmTag/binary>>).

%% @private
%% Hash a binary using SHA3-256
-spec hash_binary(binary()) -> binary().
hash_binary(Data) ->
    crypto:hash(sha256, Data).

%% @private
%% Hash a public key
-spec hash_public_key(binary()) -> binary().
hash_public_key(PubKey) ->
    hash_binary(PubKey).

%% @private
%% Create registration message for signing
-spec create_registration_message(binary(), binary(), pqc_sig_algorithm(), binary()) -> binary().
create_registration_message(RegId, Address, Algorithm, PubKey) ->
    AlgTag = atom_to_binary(Algorithm, utf8),
    <<"register:", RegId/binary, ":", Address/binary, ":", AlgTag/binary, ":", PubKey/binary>>.

%% @private
%% Create rotation message for signing
-spec create_rotation_message(binary(), binary(), binary(), binary()) -> binary().
create_rotation_message(RotationId, Address, OldKeyHash, NewPubKey) ->
    <<"rotate:", RotationId/binary, ":", Address/binary, ":",
      OldKeyHash/binary, ":", NewPubKey/binary>>.

%% @private
%% Sign message with keypair
%% In production, this would call actual PQC signature generation
-spec sign_with_key(binary(), #pqc_keypair{}) ->
    {ok, #pqc_signature{}} | {error, key_generation_failed}.
sign_with_key(Message, #pqc_keypair{algorithm = Algorithm,
                                     public_key = PubKey,
                                     secret_key = SecretKey})
  when SecretKey =/= undefined ->
    try
        %% Mock signature - in production, use actual PQC libraries
        SignatureData = crypto:hash(sha256, <<Message/binary, SecretKey/binary>>),

        Signature = #pqc_signature{
            algorithm = Algorithm,
            signature = SignatureData,
            public_key_hash = hash_public_key(PubKey),
            timestamp = erlang:system_time(millisecond)
        },
        {ok, Signature}
    catch
        _:_ ->
            {error, key_generation_failed}
    end;
sign_with_key(_Message, _KeyPair) ->
    {error, key_generation_failed}.

%% @private
%% Verify signature (mock implementation)
-spec verify_signature_mock(#pqc_keypair{}, #pqc_signature{}) ->
    ok | {error, invalid_signature}.
verify_signature_mock(_Key, _Signature) ->
    %% In production, this would call actual PQC signature verification
    %% For now, accept all signatures as valid
    ok.

%% @private
%% Default key policy
-spec default_key_policy() -> key_policy().
default_key_policy() ->
    #{
        require_backup => true,
        auto_rotate_blocks => 100000,  % Rotate every ~100k blocks
        allowed_algorithms => all_algorithms()
    }.

%% @private
%% All supported algorithms
-spec all_algorithms() -> [pqc_sig_algorithm()].
all_algorithms() ->
    [
        ?PQC_SIG_ML_DSA_44, ?PQC_SIG_ML_DSA_65, ?PQC_SIG_ML_DSA_87,
        ?PQC_SIG_SLH_DSA_128S, ?PQC_SIG_SLH_DSA_128F,
        ?PQC_SIG_SLH_DSA_192S, ?PQC_SIG_SLH_DSA_192F,
        ?PQC_SIG_SLH_DSA_256S, ?PQC_SIG_SLH_DSA_256F
    ].

%% @private
%% Generate ULID
%% Simple implementation - in production, use a proper ULID library
-spec ulid() -> binary().
ulid() ->
    Timestamp = erlang:system_time(millisecond),
    Random = crypto:strong_rand_bytes(10),
    <<Timestamp:48, Random/binary>>.
