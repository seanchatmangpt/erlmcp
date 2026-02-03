%%% @doc PQC-Signed Receipt Chain
%%%
%%% Implements tamper-evident audit trail with post-quantum cryptographic signatures.
%%% This is the "evidence replaces narrative" anchor for SwarmFlow operations.
%%%
%%% Receipt chains provide:
%%% - Tamper-evident audit trail via hash chaining
%%% - Post-quantum secure signatures (ML-DSA)
%%% - Verifiable event ordering and causality
%%% - Merkle proofs for efficient verification
%%% - Deterministic artifact identification
%%%
%%% Each receipt is cryptographically linked to the previous receipt via hash chaining,
%%% creating an immutable sequence. ML-DSA signatures ensure authenticity and
%%% non-repudiation even against quantum attackers.
%%%
%%% Architecture:
%%% - Genesis receipt: First receipt in chain, anchored by case_id + timestamp
%%% - Extended receipts: Linked to previous receipt via prev_hash
%%% - Signatures: ML-DSA-65 (NIST Level 3) by default
%%% - Hashing: SHA3-256 for all hash operations
%%% - Merkle proofs: For efficient batch verification
%%%
%%% @end
-module(pqc_receipt).

-include("pqchain.hrl").

%%% ============================================================================
%%% API Exports
%%% ============================================================================

-export([
    genesis/3,
    extend/4,
    verify/2,
    verify_chain/2,
    artifact_id/2,
    to_binary/1,
    from_binary/1,
    get_proof/2,
    verify_proof/3
]).

%%% ============================================================================
%%% Type Definitions
%%% ============================================================================

-type receipt() :: #{
    hash => binary(),
    prev_hash => binary() | undefined,
    event_hash => binary() | undefined,
    signature => #pqc_signature{},
    ts => non_neg_integer(),
    sequence => non_neg_integer()
}.

-type merkle_proof() :: #{
    index => non_neg_integer(),
    receipt_hash => binary(),
    siblings => [binary()],
    root => binary()
}.

-export_type([receipt/0, merkle_proof/0]).

%%% ============================================================================
%%% Public API - Receipt Creation
%%% ============================================================================

%% @doc Create genesis receipt for a new case.
%%
%% Genesis receipt anchors the receipt chain with:
%% - Hash: SHA3-256(case_id || ts)
%% - Signature: ML-DSA signature over hash
%% - Sequence: 0 (first receipt in chain)
%%
%% The genesis receipt establishes the root of trust for all subsequent
%% receipts in the chain.
%%
%% Example:
%%   CaseId = <<"case-123">>,
%%   Ts = erlang:system_time(millisecond),
%%   {ok, Keypair} = pqc_crypto:keygen(ml_dsa_65),
%%   {ok, Receipt} = pqc_receipt:genesis(CaseId, Ts, Keypair).
%%
%% @end
-spec genesis(CaseId :: binary(),
              Ts :: non_neg_integer(),
              SigningKey :: #pqc_keypair{}) ->
    {ok, receipt()} | {error, term()}.
genesis(CaseId, Ts, #pqc_keypair{} = SigningKey)
    when is_binary(CaseId), is_integer(Ts), Ts >= 0 ->
    try
        % Compute genesis hash: SHA3-256(case_id || ts)
        GenesisData = <<CaseId/binary, Ts:64/big-unsigned-integer>>,
        {ok, Hash} = pqc_crypto:hash(GenesisData, ?HASH_SHA3_256),

        % Sign the genesis hash with ML-DSA
        #pqc_keypair{secret_key = SecretKey, algorithm = Algorithm} = SigningKey,
        {ok, Signature} = pqc_crypto:sign(Hash, SecretKey, Algorithm),

        % Create genesis receipt
        Receipt = #{
            hash => Hash,
            prev_hash => undefined,
            event_hash => undefined,
            signature => Signature,
            ts => Ts,
            sequence => 0
        },

        {ok, Receipt}
    catch
        error:Reason:Stack ->
            {error, {genesis_failed, Reason, Stack}}
    end;
genesis(CaseId, Ts, _InvalidKey) when is_binary(CaseId), is_integer(Ts) ->
    {error, {invalid_signing_key, not_pqc_keypair}};
genesis(InvalidCaseId, _Ts, _SigningKey) when not is_binary(InvalidCaseId) ->
    {error, {invalid_case_id, InvalidCaseId}};
genesis(_CaseId, InvalidTs, _SigningKey) when not is_integer(InvalidTs); InvalidTs < 0 ->
    {error, {invalid_timestamp, InvalidTs}}.

%% @doc Extend receipt chain with a new event.
%%
%% Creates a new receipt linked to the previous receipt:
%% - Hash: SHA3-256(prev_hash || event_hash || ts)
%% - Signature: ML-DSA signature over hash
%% - Sequence: prev_sequence + 1
%%
%% The event parameter can be any term that represents the event to record.
%% It is hashed to produce event_hash for inclusion in the receipt.
%%
%% Example:
%%   Event = #{type => task_completed, task_id => <<"t1">>},
%%   NewTs = erlang:system_time(millisecond),
%%   {ok, NextReceipt} = pqc_receipt:extend(PrevReceipt, Event, NewTs, Keypair).
%%
%% @end
-spec extend(PrevReceipt :: receipt(),
             Event :: term(),
             Ts :: non_neg_integer(),
             SigningKey :: #pqc_keypair{}) ->
    {ok, receipt()} | {error, term()}.
extend(#{hash := PrevHash, sequence := PrevSeq} = _PrevReceipt,
       Event,
       Ts,
       #pqc_keypair{} = SigningKey)
    when is_binary(PrevHash), is_integer(PrevSeq), is_integer(Ts), Ts >= 0 ->
    try
        % Compute event hash: SHA3-256(event)
        EventBinary = erlang:term_to_binary(Event, [compressed]),
        {ok, EventHash} = pqc_crypto:hash(EventBinary, ?HASH_SHA3_256),

        % Compute receipt hash: SHA3-256(prev_hash || event_hash || ts)
        ReceiptData = <<PrevHash/binary,
                        EventHash/binary,
                        Ts:64/big-unsigned-integer>>,
        {ok, Hash} = pqc_crypto:hash(ReceiptData, ?HASH_SHA3_256),

        % Sign the receipt hash with ML-DSA
        #pqc_keypair{secret_key = SecretKey, algorithm = Algorithm} = SigningKey,
        {ok, Signature} = pqc_crypto:sign(Hash, SecretKey, Algorithm),

        % Create extended receipt
        Receipt = #{
            hash => Hash,
            prev_hash => PrevHash,
            event_hash => EventHash,
            signature => Signature,
            ts => Ts,
            sequence => PrevSeq + 1
        },

        {ok, Receipt}
    catch
        error:Reason:Stack ->
            {error, {extend_failed, Reason, Stack}}
    end;
extend(#{} = PrevReceipt, _Event, _Ts, _SigningKey) ->
    case maps:is_key(hash, PrevReceipt) andalso maps:is_key(sequence, PrevReceipt) of
        false -> {error, {invalid_prev_receipt, missing_required_fields}};
        true -> {error, {invalid_prev_receipt, invalid_format}}
    end;
extend(_InvalidReceipt, _Event, _Ts, _SigningKey) ->
    {error, {invalid_prev_receipt, not_a_map}}.

%%% ============================================================================
%%% Public API - Receipt Verification
%%% ============================================================================

%% @doc Verify ML-DSA signature on a single receipt.
%%
%% Validates that:
%% - Signature is valid for the receipt hash
%% - Signature was created with the provided public key
%%
%% Returns {ok, true} if valid, {ok, false} if invalid signature,
%% or {error, Reason} if verification cannot be performed.
%%
%% Example:
%%   {ok, Keypair} = pqc_crypto:keygen(ml_dsa_65),
%%   PublicKey = Keypair#pqc_keypair.public_key,
%%   {ok, IsValid} = pqc_receipt:verify(Receipt, PublicKey).
%%
%% @end
-spec verify(Receipt :: receipt(),
             PublicKey :: binary()) ->
    {ok, boolean()} | {error, term()}.
verify(#{hash := Hash, signature := Signature} = _Receipt, PublicKey)
    when is_binary(Hash), #pqc_signature{} = Signature, is_binary(PublicKey) ->
    try
        % Verify ML-DSA signature
        pqc_crypto:verify(Hash, Signature, PublicKey)
    catch
        error:Reason:Stack ->
            {error, {verify_failed, Reason, Stack}}
    end;
verify(#{} = Receipt, _PublicKey) ->
    case maps:is_key(hash, Receipt) andalso maps:is_key(signature, Receipt) of
        false -> {error, {invalid_receipt, missing_required_fields}};
        true -> {error, {invalid_receipt, invalid_signature_format}}
    end;
verify(_InvalidReceipt, _PublicKey) ->
    {error, {invalid_receipt, not_a_map}}.

%% @doc Verify entire receipt chain (all signatures + hash linkage).
%%
%% Validates that:
%% - All receipts have valid ML-DSA signatures
%% - Hash chain is correctly linked (prev_hash matches)
%% - Sequence numbers are consecutive
%% - First receipt is a valid genesis receipt
%%
%% Returns {ok, true} if entire chain is valid, {ok, false} if any validation
%% fails, or {error, Reason} if verification cannot be performed.
%%
%% Example:
%%   Receipts = [GenesisReceipt, Receipt1, Receipt2],
%%   {ok, IsValid} = pqc_receipt:verify_chain(Receipts, PublicKey).
%%
%% @end
-spec verify_chain(Receipts :: [receipt()],
                   PublicKey :: binary()) ->
    {ok, boolean()} | {error, term()}.
verify_chain([], _PublicKey) ->
    {error, {invalid_chain, empty_chain}};
verify_chain([GenesisReceipt | RestReceipts], PublicKey)
    when is_binary(PublicKey) ->
    try
        % Verify genesis receipt
        case verify_genesis_receipt(GenesisReceipt) of
            {ok, true} ->
                % Verify genesis signature
                case verify(GenesisReceipt, PublicKey) of
                    {ok, true} ->
                        % Verify rest of chain
                        verify_chain_loop(RestReceipts, GenesisReceipt, PublicKey);
                    {ok, false} ->
                        {ok, false};
                    {error, Reason} ->
                        {error, {genesis_signature_invalid, Reason}}
                end;
            {ok, false} ->
                {ok, false};
            {error, Reason} ->
                {error, {genesis_invalid, Reason}}
        end
    catch
        error:Reason:Stack ->
            {error, {verify_chain_failed, Reason, Stack}}
    end.

%%% ============================================================================
%%% Public API - Artifact Identification
%%% ============================================================================

%% @doc Generate stable artifact ID from receipt head + timestamp.
%%
%% Artifact IDs are deterministic identifiers computed from:
%% - Receipt head hash (genesis or latest receipt)
%% - Timestamp
%%
%% Format: SHA3-256(receipt_hash || ts) encoded as hex string
%%
%% These IDs are used to reference artifacts in A2A protocol messages
%% and provide verifiable linkage to the receipt chain.
%%
%% Example:
%%   ArtifactId = pqc_receipt:artifact_id(LatestReceipt, Ts).
%%
%% @end
-spec artifact_id(ReceiptHead :: receipt(),
                  Ts :: non_neg_integer()) ->
    binary().
artifact_id(#{hash := ReceiptHash}, Ts)
    when is_binary(ReceiptHash), is_integer(Ts), Ts >= 0 ->
    % Compute artifact ID: SHA3-256(receipt_hash || ts)
    ArtifactData = <<ReceiptHash/binary, Ts:64/big-unsigned-integer>>,
    {ok, Hash} = pqc_crypto:hash(ArtifactData, ?HASH_SHA3_256),
    % Encode as hex string
    binary:encode_hex(Hash, lowercase);
artifact_id(_InvalidReceipt, _Ts) ->
    erlang:error({invalid_receipt, missing_hash_field}).

%%% ============================================================================
%%% Public API - Serialization
%%% ============================================================================

%% @doc Serialize receipt to binary format.
%%
%% Uses erlang:term_to_binary/2 with compression for efficient storage
%% and transmission.
%%
%% In production, this could be replaced with Protocol Buffers, MessagePack,
%% or other efficient binary encoding.
%%
%% Example:
%%   Binary = pqc_receipt:to_binary(Receipt),
%%   ok = file:write_file("receipt.bin", Binary).
%%
%% @end
-spec to_binary(Receipt :: receipt()) -> binary().
to_binary(#{} = Receipt) ->
    erlang:term_to_binary(Receipt, [compressed, {minor_version, 2}]).

%% @doc Deserialize receipt from binary format.
%%
%% Returns {ok, Receipt} if deserialization succeeds and the structure is valid,
%% or {error, Reason} if deserialization fails or structure is invalid.
%%
%% Example:
%%   {ok, Binary} = file:read_file("receipt.bin"),
%%   {ok, Receipt} = pqc_receipt:from_binary(Binary).
%%
%% @end
-spec from_binary(Binary :: binary()) ->
    {ok, receipt()} | {error, term()}.
from_binary(Binary) when is_binary(Binary) ->
    try
        Term = erlang:binary_to_term(Binary, [safe]),
        case is_valid_receipt_structure(Term) of
            true -> {ok, Term};
            false -> {error, invalid_receipt_structure}
        end
    catch
        error:Reason ->
            {error, {decode_error, Reason}}
    end;
from_binary(_Invalid) ->
    {error, {invalid_input, not_binary}}.

%%% ============================================================================
%%% Public API - Merkle Proofs
%%% ============================================================================

%% @doc Generate merkle proof for receipt at index.
%%
%% Creates a Merkle inclusion proof that allows verification of a single
%% receipt's presence in the chain without requiring the full chain.
%%
%% The proof includes:
%% - Index: Position of receipt in chain
%% - Receipt hash: Hash of the receipt to prove
%% - Siblings: Hashes of sibling nodes in Merkle tree
%% - Root: Merkle root of entire chain
%%
%% Example:
%%   Receipts = [R0, R1, R2, R3],
%%   {ok, Proof} = pqc_receipt:get_proof(Receipts, 1).
%%
%% @end
-spec get_proof(Receipts :: [receipt()],
                Index :: non_neg_integer()) ->
    {ok, merkle_proof()} | {error, term()}.
get_proof(Receipts, Index)
    when is_list(Receipts), is_integer(Index), Index >= 0 ->
    try
        case Index >= length(Receipts) of
            true ->
                {error, {index_out_of_bounds, Index, length(Receipts)}};
            false ->
                % Build Merkle tree and extract proof
                Hashes = [maps:get(hash, R) || R <- Receipts],
                {Root, Proof} = compute_merkle_proof(Hashes, Index),

                ReceiptHash = lists:nth(Index + 1, Hashes),

                MerkleProof = #{
                    index => Index,
                    receipt_hash => ReceiptHash,
                    siblings => Proof,
                    root => Root
                },

                {ok, MerkleProof}
        end
    catch
        error:Reason:Stack ->
            {error, {proof_generation_failed, Reason, Stack}}
    end;
get_proof(_InvalidReceipts, _Index) ->
    {error, {invalid_input, receipts_must_be_list}}.

%% @doc Verify merkle inclusion proof.
%%
%% Validates that a receipt is included in a chain with the given Merkle root.
%%
%% Returns {ok, true} if the proof is valid, {ok, false} if invalid,
%% or {error, Reason} if verification cannot be performed.
%%
%% Example:
%%   {ok, Proof} = pqc_receipt:get_proof(Receipts, 1),
%%   {ok, IsValid} = pqc_receipt:verify_proof(Receipt, Proof, MerkleRoot).
%%
%% @end
-spec verify_proof(Receipt :: receipt(),
                   MerkleProof :: merkle_proof(),
                   MerkleRoot :: binary()) ->
    {ok, boolean()} | {error, term()}.
verify_proof(#{hash := ReceiptHash} = _Receipt,
             #{index := Index,
               receipt_hash := ProofReceiptHash,
               siblings := Siblings,
               root := ProofRoot} = _MerkleProof,
             ExpectedRoot)
    when is_binary(ReceiptHash), is_integer(Index),
         is_binary(ProofReceiptHash), is_list(Siblings),
         is_binary(ProofRoot), is_binary(ExpectedRoot) ->
    try
        % Verify receipt hash matches proof
        case ReceiptHash =:= ProofReceiptHash of
            false ->
                {ok, false};
            true ->
                % Reconstruct root from proof
                ReconstructedRoot = reconstruct_merkle_root(ReceiptHash, Index, Siblings),

                % Verify reconstructed root matches expected root
                IsValid = (ReconstructedRoot =:= ExpectedRoot)
                          andalso (ProofRoot =:= ExpectedRoot),

                {ok, IsValid}
        end
    catch
        error:Reason:Stack ->
            {error, {proof_verification_failed, Reason, Stack}}
    end;
verify_proof(_Receipt, _MerkleProof, _MerkleRoot) ->
    {error, {invalid_input, missing_required_fields}}.

%%% ============================================================================
%%% Internal Functions - Verification
%%% ============================================================================

%% @private
%% @doc Verify genesis receipt structure.
-spec verify_genesis_receipt(receipt()) -> {ok, boolean()} | {error, term()}.
verify_genesis_receipt(#{hash := _Hash,
                         prev_hash := undefined,
                         event_hash := undefined,
                         signature := _Signature,
                         ts := Ts,
                         sequence := 0})
    when is_integer(Ts), Ts >= 0 ->
    {ok, true};
verify_genesis_receipt(#{sequence := Seq}) when Seq =/= 0 ->
    {ok, false};
verify_genesis_receipt(#{prev_hash := PrevHash}) when PrevHash =/= undefined ->
    {ok, false};
verify_genesis_receipt(#{event_hash := EventHash}) when EventHash =/= undefined ->
    {ok, false};
verify_genesis_receipt(_) ->
    {ok, false}.

%% @private
%% @doc Verify chain loop - recursive verification of linked receipts.
-spec verify_chain_loop([receipt()], receipt(), binary()) ->
    {ok, boolean()} | {error, term()}.
verify_chain_loop([], _PrevReceipt, _PublicKey) ->
    % All receipts verified successfully
    {ok, true};
verify_chain_loop([CurrentReceipt | RestReceipts], PrevReceipt, PublicKey) ->
    #{hash := PrevHash, sequence := PrevSeq} = PrevReceipt,
    #{hash := CurrentHash,
      prev_hash := CurrentPrevHash,
      sequence := CurrentSeq} = CurrentReceipt,

    % Verify hash linkage
    case CurrentPrevHash =:= PrevHash of
        false ->
            {ok, false};
        true ->
            % Verify sequence number
            case CurrentSeq =:= (PrevSeq + 1) of
                false ->
                    {ok, false};
                true ->
                    % Verify signature
                    case verify(CurrentReceipt, PublicKey) of
                        {ok, true} ->
                            % Verify rest of chain
                            verify_chain_loop(RestReceipts, CurrentReceipt, PublicKey);
                        {ok, false} ->
                            {ok, false};
                        {error, Reason} ->
                            {error, {receipt_verification_failed, CurrentHash, Reason}}
                    end
            end
    end.

%% @private
%% @doc Check if term has valid receipt structure.
-spec is_valid_receipt_structure(term()) -> boolean().
is_valid_receipt_structure(#{hash := Hash,
                              signature := Sig,
                              ts := Ts,
                              sequence := Seq})
    when is_binary(Hash),
         element(1, Sig) =:= pqc_signature,
         is_integer(Ts), Ts >= 0,
         is_integer(Seq), Seq >= 0 ->
    true;
is_valid_receipt_structure(_) ->
    false.

%%% ============================================================================
%%% Internal Functions - Merkle Tree Operations
%%% ============================================================================

%% @private
%% @doc Compute merkle proof for element at index.
%%
%% Returns {Root, ProofSiblings} where ProofSiblings is a list of hashes
%% needed to reconstruct the root from the leaf at Index.
-spec compute_merkle_proof([binary()], non_neg_integer()) ->
    {Root :: binary(), Siblings :: [binary()]}.
compute_merkle_proof([SingleHash], 0) ->
    {SingleHash, []};
compute_merkle_proof(Hashes, Index) ->
    compute_merkle_proof_level(Hashes, Index, []).

%% @private
%% @doc Recursive helper for merkle proof computation.
-spec compute_merkle_proof_level([binary()], non_neg_integer(), [binary()]) ->
    {binary(), [binary()]}.
compute_merkle_proof_level([SingleHash], 0, Siblings) ->
    {SingleHash, lists:reverse(Siblings)};
compute_merkle_proof_level(Hashes, Index, Siblings) ->
    % Determine sibling position
    SiblingIndex = case Index rem 2 of
                       0 -> Index + 1;  % Right sibling
                       1 -> Index - 1   % Left sibling
                   end,

    % Get sibling hash (or duplicate if at end)
    Sibling = case SiblingIndex < length(Hashes) of
                  true -> lists:nth(SiblingIndex + 1, Hashes);
                  false -> lists:nth(Index + 1, Hashes)  % Duplicate last if odd
              end,

    % Compute parent level
    ParentHashes = compute_merkle_parent_level(Hashes),
    ParentIndex = Index div 2,

    % Recurse to next level
    compute_merkle_proof_level(ParentHashes, ParentIndex, [Sibling | Siblings]).

%% @private
%% @doc Compute parent level of merkle tree.
-spec compute_merkle_parent_level([binary()]) -> [binary()].
compute_merkle_parent_level([SingleHash]) ->
    [SingleHash];
compute_merkle_parent_level(Hashes) ->
    Paired = pair_hashes(Hashes),
    [hash_pair(L, R) || {L, R} <- Paired].

%% @private
%% @doc Pair up hashes for merkle tree construction.
-spec pair_hashes([binary()]) -> [{binary(), binary()}].
pair_hashes([]) ->
    [];
pair_hashes([H]) ->
    [{H, H}];  % Duplicate last hash if odd number
pair_hashes([H1, H2 | Rest]) ->
    [{H1, H2} | pair_hashes(Rest)].

%% @private
%% @doc Hash a pair of hashes.
-spec hash_pair(binary(), binary()) -> binary().
hash_pair(Left, Right) ->
    {ok, Hash} = pqc_crypto:hash(<<Left/binary, Right/binary>>, ?HASH_SHA3_256),
    Hash.

%% @private
%% @doc Reconstruct merkle root from leaf hash, index, and sibling hashes.
-spec reconstruct_merkle_root(binary(), non_neg_integer(), [binary()]) -> binary().
reconstruct_merkle_root(LeafHash, _Index, []) ->
    LeafHash;
reconstruct_merkle_root(CurrentHash, Index, [Sibling | RestSiblings]) ->
    % Determine if current node is left or right child
    ParentHash = case Index rem 2 of
                     0 -> hash_pair(CurrentHash, Sibling);  % Current is left
                     1 -> hash_pair(Sibling, CurrentHash)   % Current is right
                 end,

    ParentIndex = Index div 2,
    reconstruct_merkle_root(ParentHash, ParentIndex, RestSiblings).
