%%%-------------------------------------------------------------------
%% @doc MCP+ Governance NIF - Rust Computational Core
%%
%% Erlang interface to the Rust computational core for:
%% - SHA-256 hashing (text-blind)
%% - Ed25519 signatures
%% - Canonical JSON encoding
%% - Evidence bundle serialization (MCPB format)
%% - Merkle tree computation
%%
%% Falls back to pure Erlang implementations if NIF not loaded.
%%
%% @end
%%%-------------------------------------------------------------------
-module(mcp_governance_nif).

%% NIF loading
-export([load/0]).
-on_load(load/0).

%% Cryptographic Hashing
-export([
    sha256_hash/1,
    sha256_hash_json/1
]).

%% Ed25519 Signatures
-export([
    ed25519_generate_keypair/0,
    ed25519_sign/2,
    ed25519_verify/3
]).

%% Canonical JSON
-export([
    canonical_json_encode/1
]).

%% Evidence Bundle Serialization
-export([
    mcpb_serialize/5,
    mcpb_parse/2
]).

%% Merkle Tree
-export([
    merkle_root/1,
    merkle_verify/4
]).

%% Base64
-export([
    base64_encode/1,
    base64_decode/1
]).

%%====================================================================
%% NIF Loading
%%====================================================================

load() ->
    PrivDir = case code:priv_dir(erlmcp) of
        {error, _} ->
            %% Fallback for development
            filename:join([
                filename:dirname(code:which(?MODULE)),
                "..", "priv"
            ]);
        Dir -> Dir
    end,
    NifPath = filename:join(PrivDir, "mcp_governance"),
    case erlang:load_nif(NifPath, 0) of
        ok -> ok;
        {error, {reload, _}} -> ok;
        {error, Reason} ->
            logger:warning("Failed to load mcp_governance NIF: ~p, using fallback", [Reason]),
            ok
    end.

%%====================================================================
%% Cryptographic Hashing
%%====================================================================

%% @doc Compute SHA-256 hash of binary data.
-spec sha256_hash(binary()) -> binary().
sha256_hash(Data) when is_binary(Data) ->
    %% Fallback to Erlang crypto
    crypto:hash(sha256, Data).

%% @doc Compute SHA-256 hash of JSON with canonical encoding.
-spec sha256_hash_json(binary()) -> binary().
sha256_hash_json(JsonBinary) when is_binary(JsonBinary) ->
    %% Fallback: use erlmcp_json for canonical encoding
    Map = jsx:decode(JsonBinary, [return_maps]),
    CanonicalJson = erlmcp_json:canonical_encode(Map),
    crypto:hash(sha256, CanonicalJson).

%%====================================================================
%% Ed25519 Signatures
%%====================================================================

%% @doc Generate Ed25519 keypair.
-spec ed25519_generate_keypair() -> {ok, {PublicKey :: binary(), PrivateKey :: binary()}}.
ed25519_generate_keypair() ->
    %% Fallback to Erlang crypto
    %% OTP 25+ returns {PublicKey, PrivateKey} tuple directly
    {Pub, Priv} = crypto:generate_key(eddsa, ed25519),
    {ok, {Pub, Priv}}.

%% @doc Sign data with Ed25519 private key.
-spec ed25519_sign(binary(), binary()) -> {ok, binary()} | {error, term()}.
ed25519_sign(PrivateKey, Data) when is_binary(PrivateKey), is_binary(Data) ->
    %% Fallback to Erlang crypto
    try
        Signature = crypto:sign(eddsa, none, Data, [PrivateKey, ed25519]),
        {ok, Signature}
    catch
        error:_ -> {error, invalid_key}
    end.

%% @doc Verify Ed25519 signature.
-spec ed25519_verify(binary(), binary(), binary()) -> ok | {error, term()}.
ed25519_verify(PublicKey, Signature, Data)
  when is_binary(PublicKey), is_binary(Signature), is_binary(Data) ->
    %% Fallback to Erlang crypto
    try
        case crypto:verify(eddsa, none, Data, Signature, [PublicKey, ed25519]) of
            true -> ok;
            false -> {error, verification_failed}
        end
    catch
        error:_ -> {error, invalid_key}
    end.

%%====================================================================
%% Canonical JSON
%%====================================================================

%% @doc Encode JSON to canonical form (sorted keys).
-spec canonical_json_encode(binary()) -> {ok, binary()} | {error, term()}.
canonical_json_encode(JsonBinary) when is_binary(JsonBinary) ->
    %% Fallback: use erlmcp_json
    try
        Map = jsx:decode(JsonBinary, [return_maps]),
        {ok, erlmcp_json:canonical_encode(Map)}
    catch
        _:_ -> {error, invalid_json}
    end.

%%====================================================================
%% Evidence Bundle Serialization
%%====================================================================

%% @doc Serialize evidence bundle to MCPB format.
-spec mcpb_serialize(integer(), binary(), binary(), binary(), binary()) ->
    {ok, binary()} | {error, term()}.
mcpb_serialize(Timestamp, ContractHash, MerkleRoot, ReceiptsJson, PrivateKey)
  when is_integer(Timestamp),
       is_binary(ContractHash), byte_size(ContractHash) =:= 32,
       is_binary(MerkleRoot), byte_size(MerkleRoot) =:= 32,
       is_binary(ReceiptsJson),
       is_binary(PrivateKey), byte_size(PrivateKey) =:= 32 ->
    %% Fallback: pure Erlang implementation
    ReceiptCount = count_json_array(ReceiptsJson),
    ReceiptsLen = byte_size(ReceiptsJson),

    UnsignedData = <<
        "MCPB",                           % Magic
        "1.0", 0,                         % Version
        Timestamp:64/big-signed,          % Timestamp
        ContractHash:32/binary,           % Contract hash
        MerkleRoot:32/binary,             % Merkle root
        ReceiptCount:32/big-unsigned,     % Receipt count
        ReceiptsLen:32/big-unsigned,      % Receipts length
        ReceiptsJson/binary               % Receipts data
    >>,

    Signature = crypto:sign(eddsa, none, UnsignedData, [PrivateKey, ed25519]),
    {ok, <<UnsignedData/binary, Signature/binary>>};
mcpb_serialize(_, _, _, _, _) ->
    {error, invalid_key}.

%% @doc Parse MCPB binary and verify signature.
-spec mcpb_parse(binary(), binary()) ->
    {ok, {Timestamp :: integer(), ContractHash :: binary(),
          MerkleRoot :: binary(), ReceiptsJson :: binary()}} |
    {error, term()}.
mcpb_parse(Data, PublicKey) when is_binary(Data), is_binary(PublicKey), byte_size(PublicKey) =:= 32 ->
    %% Fallback: pure Erlang implementation
    case Data of
        <<"MCPB", "1.0", 0, Timestamp:64/big-signed,
          ContractHash:32/binary, MerkleRoot:32/binary,
          _ReceiptCount:32/big-unsigned, ReceiptsLen:32/big-unsigned,
          Rest/binary>> when byte_size(Rest) =:= ReceiptsLen + 64 ->
            ReceiptsJson = binary:part(Rest, 0, ReceiptsLen),
            Signature = binary:part(Rest, ReceiptsLen, 64),
            UnsignedData = binary:part(Data, 0, byte_size(Data) - 64),

            case crypto:verify(eddsa, none, UnsignedData, Signature, [PublicKey, ed25519]) of
                true ->
                    {ok, {Timestamp, ContractHash, MerkleRoot, ReceiptsJson}};
                false ->
                    {error, verification_failed}
            end;
        <<"MCPB", _/binary>> ->
            {error, invalid_format};
        _ ->
            {error, invalid_magic}
    end;
mcpb_parse(_, _) ->
    {error, invalid_key}.

%%====================================================================
%% Merkle Tree
%%====================================================================

%% @doc Compute Merkle root from list of hashes.
-spec merkle_root([binary()]) -> binary().
merkle_root([]) ->
    crypto:hash(sha256, <<>>);
merkle_root(Hashes) when is_list(Hashes) ->
    %% Fallback: pure Erlang implementation
    Leaves = [H || H <- Hashes, is_binary(H), byte_size(H) =:= 32],
    case Leaves of
        [] -> crypto:hash(sha256, <<>>);
        _ -> compute_merkle_root(pad_to_power_of_2(Leaves))
    end.

%% @doc Verify Merkle proof.
-spec merkle_verify(binary(), [binary()], binary(), non_neg_integer()) -> boolean().
merkle_verify(Leaf, Proof, Root, Index)
  when is_binary(Leaf), byte_size(Leaf) =:= 32,
       is_list(Proof),
       is_binary(Root), byte_size(Root) =:= 32,
       is_integer(Index), Index >= 0 ->
    %% Fallback: pure Erlang implementation
    verify_merkle_proof(Leaf, Proof, Root, Index);
merkle_verify(_, _, _, _) ->
    false.

%%====================================================================
%% Base64
%%====================================================================

%% @doc Encode binary to base64.
-spec base64_encode(binary()) -> binary().
base64_encode(Data) when is_binary(Data) ->
    %% Fallback to Erlang base64
    base64:encode(Data).

%% @doc Decode base64 to binary.
-spec base64_decode(binary()) -> {ok, binary()} | {error, term()}.
base64_decode(Data) when is_binary(Data) ->
    %% Fallback to Erlang base64
    try
        {ok, base64:decode(Data)}
    catch
        _:_ -> {error, invalid_base64}
    end.

%%====================================================================
%% Internal Functions
%%====================================================================

-spec count_json_array(binary()) -> non_neg_integer().
count_json_array(JsonBinary) ->
    try
        case jsx:decode(JsonBinary, [return_maps]) of
            List when is_list(List) -> length(List);
            _ -> 0
        end
    catch
        _:_ -> 0
    end.

-spec pad_to_power_of_2([binary()]) -> [binary()].
pad_to_power_of_2([]) -> [];
pad_to_power_of_2([H|_] = List) ->
    Len = length(List),
    TargetLen = next_power_of_2(Len),
    pad_list(List, H, TargetLen).

-spec next_power_of_2(pos_integer()) -> pos_integer().
next_power_of_2(N) when N =< 1 -> 1;
next_power_of_2(N) ->
    1 bsl ceil(math:log2(N)).

-spec pad_list([binary()], binary(), pos_integer()) -> [binary()].
pad_list(List, _Last, Target) when length(List) >= Target ->
    List;
pad_list(List, Last, Target) ->
    pad_list(List ++ [Last], Last, Target).

-spec compute_merkle_root([binary()]) -> binary().
compute_merkle_root([Root]) -> Root;
compute_merkle_root(Leaves) ->
    NewLevel = pair_hash(Leaves, []),
    compute_merkle_root(NewLevel).

-spec pair_hash([binary()], [binary()]) -> [binary()].
pair_hash([], Acc) -> lists:reverse(Acc);
pair_hash([A, B | Rest], Acc) ->
    Hash = crypto:hash(sha256, <<A/binary, B/binary>>),
    pair_hash(Rest, [Hash | Acc]).

-spec verify_merkle_proof(binary(), [binary()], binary(), non_neg_integer()) -> boolean().
verify_merkle_proof(Current, [], Root, _Index) ->
    Current =:= Root;
verify_merkle_proof(Current, [Sibling | Rest], Root, Index) ->
    NewHash = case Index rem 2 of
        0 -> crypto:hash(sha256, <<Current/binary, Sibling/binary>>);
        1 -> crypto:hash(sha256, <<Sibling/binary, Current/binary>>)
    end,
    verify_merkle_proof(NewHash, Rest, Root, Index div 2).
