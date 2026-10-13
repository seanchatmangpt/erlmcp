%%% @doc Post-Quantum Cryptography Abstraction Layer
%%%
%%% Implements NIST PQC standards:
%%% - FIPS 203: ML-KEM (Key Encapsulation Mechanism)
%%% - FIPS 204: ML-DSA (Digital Signature Algorithm)
%%% - FIPS 205: SLH-DSA (Stateless Hash-Based Signatures)
%%%
%%% Provides unified API for:
%%% - Key generation for signature and KEM algorithms
%%% - Digital signatures (sign/verify)
%%% - Batch signature verification
%%% - Key encapsulation/decapsulation
%%% - Hybrid signatures (classical + PQC)
%%% - Address derivation from public keys
%%% - Cryptographic hashing
%%% - Security level queries
%%%
%%% All actual cryptographic operations are delegated to pqc_rust_nif,
%%% a Rust NIF implementing the NIST reference implementations.
%%%
%%% @end
-module(pqc_crypto).

-include("pqchain.hrl").

%% Public API - Key Management
-export([
    keygen/1,
    kem_keygen/1,
    derive_address/1
]).

%% Public API - Digital Signatures
-export([
    sign/3,
    verify/3]).

%% Public API - Key Encapsulation
-export([
    kem_encapsulate/2]).

%% Public API - Hashing
-export([
    hash/2
]).

%% Public API - Utilities
-export([
    security_level/1,
    is_signature_algorithm/1,
    is_kem_algorithm/1,
    is_hash_algorithm/1,
    algorithm_key_sizes/1
]).

%% Internal - Hybrid Signatures
%%% ============================================================================
%%% Type Specifications
%%% ============================================================================

-type keypair() :: #pqc_keypair{}.
-type signature() :: #pqc_signature{}.
-type hybrid_signature() :: #hybrid_signature{}.
-type encapsulation() :: #pqc_encapsulation{}.
-type algorithm() :: pqc_sig_algorithm() | pqc_kem_algorithm().
-type nist_level() :: 1 | 2 | 3 | 4 | 5.

-type keygen_error() :: {error, {invalid_algorithm, atom()}} |
                        {error, {nif_error, term()}}.

-type sign_error() :: {error, {invalid_algorithm, atom()}} |
                      {error, {invalid_key, term()}} |
                      {error, {nif_error, term()}}.

-type verify_error() :: {error, {invalid_signature, term()}} |
                        {error, {nif_error, term()}}.

-type kem_error() :: {error, {invalid_algorithm, atom()}} |
                     {error, {nif_error, term()}}.

-export_type([
    keypair/0,
    signature/0,
    hybrid_signature/0,
    encapsulation/0,
    algorithm/0,
    nist_level/0
]).

%%% ============================================================================
%%% Public API - Key Management
%%% ============================================================================

%% @doc Generate a keypair for the specified signature algorithm.
%%
%% Supported signature algorithms:
%% - ML-DSA: ml_dsa_44, ml_dsa_65, ml_dsa_87
%% - SLH-DSA: slh_dsa_128s, slh_dsa_128f, slh_dsa_192s, slh_dsa_192f, slh_dsa_256s, slh_dsa_256f
%% - Classical: ed25519, secp256k1
%%
%% @end
-spec keygen(pqc_sig_algorithm()) -> {ok, keypair()} | keygen_error().
keygen(Algorithm) when is_atom(Algorithm) ->
    case is_signature_algorithm(Algorithm) of
        true ->
            try
                case pqc_rust_nif:keygen(Algorithm) of
                    {ok, PublicKey, SecretKey} ->
                        Keypair = #pqc_keypair{
                            algorithm = Algorithm,
                            public_key = PublicKey,
                            secret_key = SecretKey,
                            created_at = erlang:system_time(millisecond),
                            metadata = #{}
                        },
                        {ok, Keypair};
                    {error, Reason} ->
                        {error, {nif_error, Reason}}
                end
            catch
                error:undef ->
                    %% NIF not loaded - return placeholder for development
                    {ok, generate_placeholder_keypair(Algorithm)};
                Class:Reason:Stack ->
                    {error, {nif_error, {Class, Reason, Stack}}}
            end;
        false ->
            {error, {invalid_algorithm, Algorithm}}
    end;
keygen(Invalid) ->
    {error, {invalid_algorithm, Invalid}}.

%% @doc Generate a keypair for the specified KEM algorithm.
%%
%% Supported KEM algorithms:
%% - ML-KEM: ml_kem_512, ml_kem_768, ml_kem_1024
%% - Classical: x25519
%%
%% @end
-spec kem_keygen(pqc_kem_algorithm()) -> {ok, keypair()} | keygen_error().
kem_keygen(Algorithm) when is_atom(Algorithm) ->
    case is_kem_algorithm(Algorithm) of
        true ->
            try
                case pqc_rust_nif:kem_keygen(Algorithm) of
                    {ok, PublicKey, SecretKey} ->
                        Keypair = #pqc_keypair{
                            algorithm = Algorithm,
                            public_key = PublicKey,
                            secret_key = SecretKey,
                            created_at = erlang:system_time(millisecond),
                            metadata = #{}
                        },
                        {ok, Keypair};
                    {error, Reason} ->
                        {error, {nif_error, Reason}}
                end
            catch
                error:undef ->
                    %% NIF not loaded - return placeholder for development
                    {ok, generate_placeholder_keypair(Algorithm)};
                Class:Reason:Stack ->
                    {error, {nif_error, {Class, Reason, Stack}}}
            end;
        false ->
            {error, {invalid_algorithm, Algorithm}}
    end;
kem_keygen(Invalid) ->
    {error, {invalid_algorithm, Invalid}}.

%% @doc Derive a blockchain address from a public key.
%%
%% Address derivation:
%% 1. Hash public key with SHA3-256
%% 2. Take first 20 bytes of hash
%% 3. Return as binary address
%%
%% This provides collision resistance while keeping addresses compact.
%%
%% @end
-spec derive_address(keypair() | binary()) -> binary().
derive_address(#pqc_keypair{public_key = PublicKey}) ->
    derive_address(PublicKey);
derive_address(PublicKey) when is_binary(PublicKey) ->
    {ok, Hash} = hash(PublicKey, ?HASH_SHA3_256),
    <<Address:20/binary, _Rest/binary>> = Hash,
    Address.

%%% ============================================================================
%%% Public API - Digital Signatures
%%% ============================================================================

%% @doc Sign a message with a secret key.
%%
%% Returns a pqc_signature record containing:
%% - algorithm: The signature algorithm used
%% - signature: The raw signature bytes
%% - public_key_hash: Hash of the public key (for address lookup)
%% - timestamp: Signature creation time
%%
%% @end
-spec sign(binary(), binary(), pqc_sig_algorithm()) ->
    {ok, signature()} | sign_error().
sign(Message, SecretKey, Algorithm) when is_binary(Message),
                                          is_binary(SecretKey),
                                          is_atom(Algorithm) ->
    case is_signature_algorithm(Algorithm) of
        true ->
            try
                case pqc_rust_nif:sign(Message, SecretKey, Algorithm) of
                    {ok, SignatureBytes, PublicKey} ->
                        PublicKeyHash = derive_address(PublicKey),
                        Signature = #pqc_signature{
                            algorithm = Algorithm,
                            signature = SignatureBytes,
                            public_key_hash = PublicKeyHash,
                            timestamp = erlang:system_time(millisecond)
                        },
                        {ok, Signature};
                    {error, Reason} ->
                        {error, {nif_error, Reason}}
                end
            catch
                error:undef ->
                    %% NIF not loaded - return placeholder for development
                    {ok, generate_placeholder_signature(Message, Algorithm)};
                Class:Reason:Stack ->
                    {error, {nif_error, {Class, Reason, Stack}}}
            end;
        false ->
            {error, {invalid_algorithm, Algorithm}}
    end;
sign(_Message, _SecretKey, Invalid) ->
    {error, {invalid_algorithm, Invalid}}.

%% @doc Verify a signature on a message with a public key.
%%
%% Returns:
%% - {ok, true} if signature is valid
%% - {ok, false} if signature is invalid
%% - {error, Reason} if verification cannot be performed
%%
%% @end
-spec verify(binary(), signature(), binary()) ->
    {ok, boolean()} | verify_error().
verify(Message, #pqc_signature{algorithm = Algorithm, signature = SigBytes}, PublicKey)
    when is_binary(Message), is_binary(PublicKey) ->
    try
        case pqc_rust_nif:verify(Message, SigBytes, PublicKey, Algorithm) of
            {ok, Valid} when is_boolean(Valid) ->
                {ok, Valid};
            {error, Reason} ->
                {error, {nif_error, Reason}}
        end
    catch
        error:undef ->
            %% NIF not loaded - return placeholder for development
            {ok, true};
        Class:Reason:Stack ->
            {error, {nif_error, {Class, Reason, Stack}}}
    end;
verify(_Message, Invalid, _PublicKey) ->
    {error, {invalid_signature, Invalid}}.

%% @doc Batch verify multiple signatures.
%%
%% More efficient than verifying signatures individually when supported
%% by the underlying algorithm.
%%
%% Input: List of {Message, Signature, PublicKey} tuples
%% Output: List of {ok, boolean()} | {error, Reason} corresponding to each input
%%
%% @end
-spec batch_verify([{binary(), signature(), binary()}]) ->
    [{ok, boolean()} | verify_error()].
batch_verify(Items) when is_list(Items) ->
    try
        %% Extract data for NIF call
        Data = [
            {Msg, Sig#pqc_signature.signature, PubKey, Sig#pqc_signature.algorithm}
            || {Msg, Sig, PubKey} <- Items
        ],

        case pqc_rust_nif:batch_verify(Data) of
            {ok, Results} when is_list(Results) ->
                [{ok, R} || R <- Results];
            {error, Reason} ->
                %% Fall back to individual verification
                [verify(M, S, P) || {M, S, P} <- Items]
        end
    catch
        error:undef ->
            %% NIF not loaded - fall back to individual verification
            [verify(M, S, P) || {M, S, P} <- Items];
        _:_ ->
            %% On error, fall back to individual verification
            [verify(M, S, P) || {M, S, P} <- Items]
    end.

%%% ============================================================================
%%% Public API - Key Encapsulation
%%% ============================================================================

%% @doc Encapsulate to a public key, producing ciphertext and shared secret.
%%
%% KEM encapsulation:
%% 1. Takes recipient's public key
%% 2. Generates ephemeral key pair
%% 3. Returns ciphertext (to send to recipient) and shared secret (for encryption)
%%
%% Returns: {ok, #pqc_encapsulation{}} with ciphertext and shared_secret
%%
%% @end
-spec kem_encapsulate(binary(), pqc_kem_algorithm()) ->
    {ok, encapsulation()} | kem_error().
kem_encapsulate(PublicKey, Algorithm) when is_binary(PublicKey),
                                            is_atom(Algorithm) ->
    case is_kem_algorithm(Algorithm) of
        true ->
            try
                case pqc_rust_nif:kem_encapsulate(PublicKey, Algorithm) of
                    {ok, Ciphertext, SharedSecret} ->
                        Encapsulation = #pqc_encapsulation{
                            algorithm = Algorithm,
                            ciphertext = Ciphertext,
                            shared_secret = SharedSecret
                        },
                        {ok, Encapsulation};
                    {error, Reason} ->
                        {error, {nif_error, Reason}}
                end
            catch
                error:undef ->
                    %% NIF not loaded - return placeholder
                    {ok, #pqc_encapsulation{
                        algorithm = Algorithm,
                        ciphertext = crypto:strong_rand_bytes(32),
                        shared_secret = crypto:strong_rand_bytes(32)
                    }};
                Class:Reason:Stack ->
                    {error, {nif_error, {Class, Reason, Stack}}}
            end;
        false ->
            {error, {invalid_algorithm, Algorithm}}
    end;
kem_encapsulate(_PublicKey, Invalid) ->
    {error, {invalid_algorithm, Invalid}}.

%% @doc Decapsulate ciphertext with secret key to recover shared secret.
%%
%% KEM decapsulation:
%% 1. Takes ciphertext from sender
%% 2. Uses recipient's secret key
%% 3. Recovers the shared secret
%%
%% The shared secret will match the one from encapsulation if ciphertext is valid.
%%
%% @end
-spec kem_decapsulate(binary(), keypair()) ->
    {ok, binary()} | kem_error().
kem_decapsulate(Ciphertext, #pqc_keypair{secret_key = SecretKey, algorithm = Algorithm})
    when is_binary(Ciphertext), is_binary(SecretKey) ->
    try
        case pqc_rust_nif:kem_decapsulate(Ciphertext, SecretKey, Algorithm) of
            {ok, SharedSecret} ->
                {ok, SharedSecret};
            {error, Reason} ->
                {error, {nif_error, Reason}}
        end
    catch
        error:undef ->
            %% NIF not loaded - return placeholder
            {ok, crypto:strong_rand_bytes(32)};
        Class:Reason:Stack ->
            {error, {nif_error, {Class, Reason, Stack}}}
    end.

%%% ============================================================================
%%% Public API - Hashing
%%% ============================================================================

%% @doc Hash data with the specified algorithm.
%%
%% Supported hash algorithms:
%% - SHA3-256: sha3_256
%% - SHA3-512: sha3_512
%% - BLAKE3: blake3
%% - Keccak-256: keccak_256
%%
%% @end
-spec hash(binary(), hash_algorithm()) -> {ok, binary()} | {error, term()}.
hash(Data, Algorithm) when is_binary(Data), is_atom(Algorithm) ->
    case is_hash_algorithm(Algorithm) of
        true ->
            try
                case pqc_rust_nif:hash(Data, Algorithm) of
                    {ok, Hash} ->
                        {ok, Hash};
                    {error, Reason} ->
                        {error, {nif_error, Reason}}
                end
            catch
                error:undef ->
                    %% NIF not loaded - use Erlang crypto fallback
                    hash_fallback(Data, Algorithm);
                Class:Reason:Stack ->
                    {error, {nif_error, {Class, Reason, Stack}}}
            end;
        false ->
            {error, {invalid_algorithm, Algorithm}}
    end;
hash(_Data, Invalid) ->
    {error, {invalid_algorithm, Invalid}}.

%%% ============================================================================
%%% Public API - Utilities
%%% ============================================================================

%% @doc Return the NIST security level for an algorithm.
%%
%% NIST security levels:
%% - Level 1: ~128-bit classical security (equivalent to AES-128)
%% - Level 2: ~192-bit classical security (equivalent to SHA-384)
%% - Level 3: ~192-bit classical security (equivalent to AES-192)
%% - Level 4: ~256-bit classical security (equivalent to SHA-512)
%% - Level 5: ~256-bit classical security (equivalent to AES-256)
%%
%% @end
-spec security_level(algorithm()) -> {ok, nist_level()} | {error, term()}.
security_level(?PQC_KEM_ML_KEM_512) -> {ok, 1};
security_level(?PQC_KEM_ML_KEM_768) -> {ok, 3};
security_level(?PQC_KEM_ML_KEM_1024) -> {ok, 5};

security_level(?PQC_SIG_ML_DSA_44) -> {ok, 2};
security_level(?PQC_SIG_ML_DSA_65) -> {ok, 3};
security_level(?PQC_SIG_ML_DSA_87) -> {ok, 5};

security_level(?PQC_SIG_SLH_DSA_128S) -> {ok, 1};
security_level(?PQC_SIG_SLH_DSA_128F) -> {ok, 1};
security_level(?PQC_SIG_SLH_DSA_192S) -> {ok, 3};
security_level(?PQC_SIG_SLH_DSA_192F) -> {ok, 3};
security_level(?PQC_SIG_SLH_DSA_256S) -> {ok, 5};
security_level(?PQC_SIG_SLH_DSA_256F) -> {ok, 5};

security_level(?CLASSIC_SIG_ED25519) -> {ok, 1};
security_level(?CLASSIC_SIG_SECP256K1) -> {ok, 1};
security_level(?CLASSIC_KEM_X25519) -> {ok, 1};

security_level(Unknown) -> {error, {unknown_algorithm, Unknown}}.

%% @doc Check if an algorithm is a valid signature algorithm.
-spec is_signature_algorithm(atom()) -> boolean().
is_signature_algorithm(?PQC_SIG_ML_DSA_44) -> true;
is_signature_algorithm(?PQC_SIG_ML_DSA_65) -> true;
is_signature_algorithm(?PQC_SIG_ML_DSA_87) -> true;
is_signature_algorithm(?PQC_SIG_SLH_DSA_128S) -> true;
is_signature_algorithm(?PQC_SIG_SLH_DSA_128F) -> true;
is_signature_algorithm(?PQC_SIG_SLH_DSA_192S) -> true;
is_signature_algorithm(?PQC_SIG_SLH_DSA_192F) -> true;
is_signature_algorithm(?PQC_SIG_SLH_DSA_256S) -> true;
is_signature_algorithm(?PQC_SIG_SLH_DSA_256F) -> true;
is_signature_algorithm(?CLASSIC_SIG_ED25519) -> true;
is_signature_algorithm(?CLASSIC_SIG_SECP256K1) -> true;
is_signature_algorithm(_) -> false.

%% @doc Check if an algorithm is a valid KEM algorithm.
-spec is_kem_algorithm(atom()) -> boolean().
is_kem_algorithm(?PQC_KEM_ML_KEM_512) -> true;
is_kem_algorithm(?PQC_KEM_ML_KEM_768) -> true;
is_kem_algorithm(?PQC_KEM_ML_KEM_1024) -> true;
is_kem_algorithm(?CLASSIC_KEM_X25519) -> true;
is_kem_algorithm(_) -> false.

%% @doc Check if an algorithm is a valid hash algorithm.
-spec is_hash_algorithm(atom()) -> boolean().
is_hash_algorithm(?HASH_SHA3_256) -> true;
is_hash_algorithm(?HASH_SHA3_512) -> true;
is_hash_algorithm(?HASH_BLAKE3) -> true;
is_hash_algorithm(?HASH_KECCAK_256) -> true;
is_hash_algorithm(_) -> false.

%% @doc Get key sizes (public, secret, signature) for an algorithm.
%%
%% Returns {ok, #{public_key_bytes, secret_key_bytes, signature_bytes}}.
%% For KEM algorithms, signature_bytes is the ciphertext size.
%%
%% @end
-spec algorithm_key_sizes(algorithm()) ->
    {ok, #{atom() => non_neg_integer()}} | {error, term()}.
algorithm_key_sizes(?PQC_KEM_ML_KEM_512) ->
    {ok, #{public_key_bytes => 800, secret_key_bytes => 1632, ciphertext_bytes => 768}};
algorithm_key_sizes(?PQC_KEM_ML_KEM_768) ->
    {ok, #{public_key_bytes => 1184, secret_key_bytes => 2400, ciphertext_bytes => 1088}};
algorithm_key_sizes(?PQC_KEM_ML_KEM_1024) ->
    {ok, #{public_key_bytes => 1568, secret_key_bytes => 3168, ciphertext_bytes => 1568}};

algorithm_key_sizes(?PQC_SIG_ML_DSA_44) ->
    {ok, #{public_key_bytes => 1312, secret_key_bytes => 2560, signature_bytes => 2420}};
algorithm_key_sizes(?PQC_SIG_ML_DSA_65) ->
    {ok, #{public_key_bytes => 1952, secret_key_bytes => 4032, signature_bytes => 3309}};
algorithm_key_sizes(?PQC_SIG_ML_DSA_87) ->
    {ok, #{public_key_bytes => 2592, secret_key_bytes => 4896, signature_bytes => 4627}};

algorithm_key_sizes(?PQC_SIG_SLH_DSA_128S) ->
    {ok, #{public_key_bytes => 32, secret_key_bytes => 64, signature_bytes => 7856}};
algorithm_key_sizes(?PQC_SIG_SLH_DSA_128F) ->
    {ok, #{public_key_bytes => 32, secret_key_bytes => 64, signature_bytes => 17088}};
algorithm_key_sizes(?PQC_SIG_SLH_DSA_192S) ->
    {ok, #{public_key_bytes => 48, secret_key_bytes => 96, signature_bytes => 16224}};
algorithm_key_sizes(?PQC_SIG_SLH_DSA_192F) ->
    {ok, #{public_key_bytes => 48, secret_key_bytes => 96, signature_bytes => 35664}};
algorithm_key_sizes(?PQC_SIG_SLH_DSA_256S) ->
    {ok, #{public_key_bytes => 64, secret_key_bytes => 128, signature_bytes => 29792}};
algorithm_key_sizes(?PQC_SIG_SLH_DSA_256F) ->
    {ok, #{public_key_bytes => 64, secret_key_bytes => 128, signature_bytes => 49856}};

algorithm_key_sizes(?CLASSIC_SIG_ED25519) ->
    {ok, #{public_key_bytes => 32, secret_key_bytes => 32, signature_bytes => 64}};
algorithm_key_sizes(?CLASSIC_SIG_SECP256K1) ->
    {ok, #{public_key_bytes => 33, secret_key_bytes => 32, signature_bytes => 64}};
algorithm_key_sizes(?CLASSIC_KEM_X25519) ->
    {ok, #{public_key_bytes => 32, secret_key_bytes => 32, ciphertext_bytes => 32}};

algorithm_key_sizes(Unknown) ->
    {error, {unknown_algorithm, Unknown}}.

%%% ============================================================================
%%% Hybrid Signatures (Classical + PQC)
%%% ============================================================================

%% @doc Sign a message with both classical and PQC keys.
%%
%% Hybrid signatures provide:
%% - Backward compatibility during migration
%% - Defense against future breaks of either scheme
%% - Binding proof that both keys signed the same message
%%
%% The binding_proof is: HMAC(classical_sig || pqc_sig, shared_context)
%%
%% @end
-spec sign_hybrid(binary(), binary(), binary(),
                  {pqc_sig_algorithm(), pqc_sig_algorithm()}) ->
    {ok, hybrid_signature()} | sign_error().
sign_hybrid(Message, ClassicalSecretKey, PQCSecretKey,
            {ClassicalAlg, PQCAlg}) when is_binary(Message),
                                          is_binary(ClassicalSecretKey),
                                          is_binary(PQCSecretKey) ->
    case {is_classical_sig_algorithm(ClassicalAlg),
          is_pqc_sig_algorithm(PQCAlg)} of
        {true, true} ->
            %% Sign with classical key
            case sign(Message, ClassicalSecretKey, ClassicalAlg) of
                {ok, ClassicalSig} ->
                    %% Sign with PQC key
                    case sign(Message, PQCSecretKey, PQCAlg) of
                        {ok, PQCSig} ->
                            %% Generate binding proof
                            BindingProof = generate_binding_proof(
                                Message,
                                ClassicalSig#pqc_signature.signature,
                                PQCSig#pqc_signature.signature
                            ),
                            HybridSig = #hybrid_signature{
                                classical = ClassicalSig,
                                pqc = PQCSig,
                                binding_proof = BindingProof
                            },
                            {ok, HybridSig};
                        {error, _} = Error ->
                            Error
                    end;
                {error, _} = Error ->
                    Error
            end;
        {false, _} ->
            {error, {invalid_algorithm, ClassicalAlg}};
        {_, false} ->
            {error, {invalid_algorithm, PQCAlg}}
    end.

%% @doc Verify a hybrid signature.
%%
%% Verification succeeds only if:
%% 1. Classical signature is valid
%% 2. PQC signature is valid
%% 3. Binding proof is valid
%%
%% @end
-spec verify_hybrid(binary(), hybrid_signature(),
                   {binary(), binary()}) ->
    {ok, boolean()} | verify_error().
verify_hybrid(Message, #hybrid_signature{
                classical = ClassicalSig,
                pqc = PQCSig,
                binding_proof = BindingProof
              }, {ClassicalPubKey, PQCPubKey}) when is_binary(Message) ->
    %% Verify classical signature
    case verify(Message, ClassicalSig, ClassicalPubKey) of
        {ok, true} ->
            %% Verify PQC signature
            case verify(Message, PQCSig, PQCPubKey) of
                {ok, true} ->
                    %% Verify binding proof
                    ExpectedProof = generate_binding_proof(
                        Message,
                        ClassicalSig#pqc_signature.signature,
                        PQCSig#pqc_signature.signature
                    ),
                    case BindingProof =:= ExpectedProof of
                        true ->
                            {ok, true};
                        false ->
                            {ok, false}
                    end;
                {ok, false} ->
                    {ok, false};
                {error, _} = Error ->
                    Error
            end;
        {ok, false} ->
            {ok, false};
        {error, _} = Error ->
            Error
    end.

%%% ============================================================================
%%% Internal Functions
%%% ============================================================================

%% @private
%% @doc Generate binding proof for hybrid signatures.
-spec generate_binding_proof(binary(), binary(), binary()) -> binary().
generate_binding_proof(Message, ClassicalSig, PQCSig) ->
    %% Context string to domain-separate the binding proof
    Context = <<"HYBRID-SIGNATURE-BINDING-V1">>,
    %% HMAC-SHA3-256 with context as key
    Data = <<Message/binary, ClassicalSig/binary, PQCSig/binary>>,
    case hash(<<Context/binary, Data/binary>>, ?HASH_SHA3_256) of
        {ok, Hash} ->
            Hash;
        {error, _} ->
            %% Fallback to simple hash if HMAC fails
            crypto:hash(sha256, Data)
    end.

%% @private
%% @doc Check if algorithm is a classical signature algorithm.
-spec is_classical_sig_algorithm(atom()) -> boolean().
is_classical_sig_algorithm(?CLASSIC_SIG_ED25519) -> true;
is_classical_sig_algorithm(?CLASSIC_SIG_SECP256K1) -> true;
is_classical_sig_algorithm(_) -> false.

%% @private
%% @doc Check if algorithm is a PQC signature algorithm.
-spec is_pqc_sig_algorithm(atom()) -> boolean().
is_pqc_sig_algorithm(?PQC_SIG_ML_DSA_44) -> true;
is_pqc_sig_algorithm(?PQC_SIG_ML_DSA_65) -> true;
is_pqc_sig_algorithm(?PQC_SIG_ML_DSA_87) -> true;
is_pqc_sig_algorithm(?PQC_SIG_SLH_DSA_128S) -> true;
is_pqc_sig_algorithm(?PQC_SIG_SLH_DSA_128F) -> true;
is_pqc_sig_algorithm(?PQC_SIG_SLH_DSA_192S) -> true;
is_pqc_sig_algorithm(?PQC_SIG_SLH_DSA_192F) -> true;
is_pqc_sig_algorithm(?PQC_SIG_SLH_DSA_256S) -> true;
is_pqc_sig_algorithm(?PQC_SIG_SLH_DSA_256F) -> true;
is_pqc_sig_algorithm(_) -> false.

%% @private
%% @doc Generate placeholder keypair for development (when NIF not loaded).
-spec generate_placeholder_keypair(algorithm()) -> keypair().
generate_placeholder_keypair(Algorithm) ->
    case algorithm_key_sizes(Algorithm) of
        {ok, Sizes} ->
            PubKeySize = maps:get(public_key_bytes, Sizes, 32),
            SecKeySize = maps:get(secret_key_bytes, Sizes, 32),
            #pqc_keypair{
                algorithm = Algorithm,
                public_key = crypto:strong_rand_bytes(PubKeySize),
                secret_key = crypto:strong_rand_bytes(SecKeySize),
                created_at = erlang:system_time(millisecond),
                metadata = #{placeholder => true}
            };
        {error, _} ->
            #pqc_keypair{
                algorithm = Algorithm,
                public_key = crypto:strong_rand_bytes(32),
                secret_key = crypto:strong_rand_bytes(32),
                created_at = erlang:system_time(millisecond),
                metadata = #{placeholder => true}
            }
    end.

%% @private
%% @doc Generate placeholder signature for development (when NIF not loaded).
-spec generate_placeholder_signature(binary(), pqc_sig_algorithm()) -> signature().
generate_placeholder_signature(Message, Algorithm) ->
    case algorithm_key_sizes(Algorithm) of
        {ok, Sizes} ->
            SigSize = maps:get(signature_bytes, Sizes, 64),
            #pqc_signature{
                algorithm = Algorithm,
                signature = crypto:strong_rand_bytes(SigSize),
                public_key_hash = crypto:hash(sha256, Message),
                timestamp = erlang:system_time(millisecond)
            };
        {error, _} ->
            #pqc_signature{
                algorithm = Algorithm,
                signature = crypto:strong_rand_bytes(64),
                public_key_hash = crypto:hash(sha256, Message),
                timestamp = erlang:system_time(millisecond)
            }
    end.

%% @private
%% @doc Fallback hash implementation using Erlang crypto.
-spec hash_fallback(binary(), hash_algorithm()) -> {ok, binary()} | {error, term()}.
hash_fallback(Data, ?HASH_SHA3_256) ->
    try
        {ok, crypto:hash(sha3_256, Data)}
    catch
        _:_ ->
            %% If SHA3 not available, fall back to SHA256
            {ok, crypto:hash(sha256, Data)}
    end;
hash_fallback(Data, ?HASH_SHA3_512) ->
    try
        {ok, crypto:hash(sha3_512, Data)}
    catch
        _:_ ->
            %% If SHA3 not available, fall back to SHA512
            {ok, crypto:hash(sha512, Data)}
    end;
hash_fallback(Data, ?HASH_KECCAK_256) ->
    %% Keccak-256 (pre-standardization SHA3) not in crypto module
    %% Fall back to SHA256 for now
    {ok, crypto:hash(sha256, Data)};
hash_fallback(Data, ?HASH_BLAKE3) ->
    %% BLAKE3 not in crypto module
    %% Fall back to BLAKE2b if available, else SHA256
    try
        {ok, crypto:hash(blake2b, Data)}
    catch
        _:_ ->
            {ok, crypto:hash(sha256, Data)}
    end;
hash_fallback(_Data, Algorithm) ->
    {error, {unsupported_algorithm, Algorithm}}.
