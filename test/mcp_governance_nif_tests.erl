%%%-------------------------------------------------------------------
%% @doc MCP+ Governance NIF Tests
%%
%% Tests for the Rust NIF computational core (with Erlang fallback).
%%
%% @end
%%%-------------------------------------------------------------------
-module(mcp_governance_nif_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% SHA-256 Hashing Tests
%%====================================================================

sha256_hash_test() ->
    Hash = mcp_governance_nif:sha256_hash(<<"test">>),
    ?assertEqual(32, byte_size(Hash)),
    %% Verify deterministic
    Hash2 = mcp_governance_nif:sha256_hash(<<"test">>),
    ?assertEqual(Hash, Hash2),
    ok.

sha256_different_inputs_test() ->
    Hash1 = mcp_governance_nif:sha256_hash(<<"test1">>),
    Hash2 = mcp_governance_nif:sha256_hash(<<"test2">>),
    ?assertNotEqual(Hash1, Hash2),
    ok.

%%====================================================================
%% Ed25519 Signature Tests
%%====================================================================

ed25519_keypair_generation_test() ->
    {ok, {Pub, Priv}} = mcp_governance_nif:ed25519_generate_keypair(),
    ?assertEqual(32, byte_size(Pub)),
    ?assertEqual(32, byte_size(Priv)),
    ok.

ed25519_sign_verify_test() ->
    {ok, {Pub, Priv}} = mcp_governance_nif:ed25519_generate_keypair(),
    Message = <<"Hello, MCP+!">>,

    %% Sign
    {ok, Sig} = mcp_governance_nif:ed25519_sign(Priv, Message),
    ?assertEqual(64, byte_size(Sig)),

    %% Verify
    ?assertEqual(ok, mcp_governance_nif:ed25519_verify(Pub, Sig, Message)),

    %% Wrong message should fail
    ?assertEqual({error, verification_failed},
                 mcp_governance_nif:ed25519_verify(Pub, Sig, <<"Wrong message">>)),
    ok.

ed25519_different_keypairs_test() ->
    {ok, {Pub1, Priv1}} = mcp_governance_nif:ed25519_generate_keypair(),
    {ok, {Pub2, Priv2}} = mcp_governance_nif:ed25519_generate_keypair(),

    ?assertNotEqual(Pub1, Pub2),
    ?assertNotEqual(Priv1, Priv2),

    %% Cross-verification should fail
    {ok, Sig1} = mcp_governance_nif:ed25519_sign(Priv1, <<"message">>),
    ?assertEqual({error, verification_failed},
                 mcp_governance_nif:ed25519_verify(Pub2, Sig1, <<"message">>)),
    ok.

%%====================================================================
%% Merkle Tree Tests
%%====================================================================

merkle_root_empty_test() ->
    Root = mcp_governance_nif:merkle_root([]),
    ?assertEqual(32, byte_size(Root)),
    ok.

merkle_root_single_test() ->
    Hash = crypto:hash(sha256, <<"leaf">>),
    Root = mcp_governance_nif:merkle_root([Hash]),
    ?assertEqual(32, byte_size(Root)),
    ok.

merkle_root_multiple_test() ->
    Hash1 = crypto:hash(sha256, <<"leaf1">>),
    Hash2 = crypto:hash(sha256, <<"leaf2">>),
    Hash3 = crypto:hash(sha256, <<"leaf3">>),

    Root = mcp_governance_nif:merkle_root([Hash1, Hash2, Hash3]),
    ?assertEqual(32, byte_size(Root)),

    %% Same leaves should produce same root
    Root2 = mcp_governance_nif:merkle_root([Hash1, Hash2, Hash3]),
    ?assertEqual(Root, Root2),
    ok.

merkle_root_different_order_test() ->
    Hash1 = crypto:hash(sha256, <<"leaf1">>),
    Hash2 = crypto:hash(sha256, <<"leaf2">>),

    Root1 = mcp_governance_nif:merkle_root([Hash1, Hash2]),
    Root2 = mcp_governance_nif:merkle_root([Hash2, Hash1]),

    %% Different order should produce different root
    ?assertNotEqual(Root1, Root2),
    ok.

merkle_verify_test() ->
    %% For a tree with 2 leaves, proof is just the sibling
    Hash1 = crypto:hash(sha256, <<"leaf1">>),
    Hash2 = crypto:hash(sha256, <<"leaf2">>),

    Root = mcp_governance_nif:merkle_root([Hash1, Hash2]),

    %% Verify leaf1 at index 0
    ?assert(mcp_governance_nif:merkle_verify(Hash1, [Hash2], Root, 0)),

    %% Verify leaf2 at index 1
    ?assert(mcp_governance_nif:merkle_verify(Hash2, [Hash1], Root, 1)),

    %% Wrong proof should fail
    ?assertNot(mcp_governance_nif:merkle_verify(Hash1, [Hash1], Root, 0)),
    ok.

%%====================================================================
%% MCPB Serialization Tests
%%====================================================================

mcpb_roundtrip_test() ->
    {ok, {Pub, Priv}} = mcp_governance_nif:ed25519_generate_keypair(),

    Timestamp = erlang:system_time(millisecond),
    ContractHash = crypto:hash(sha256, <<"contract">>),
    MerkleRoot = crypto:hash(sha256, <<"merkle">>),
    ReceiptsJson = <<"[{\"id\":\"receipt1\"}]">>,

    %% Serialize
    {ok, McpbData} = mcp_governance_nif:mcpb_serialize(
        Timestamp, ContractHash, MerkleRoot, ReceiptsJson, Priv),

    %% Verify format
    ?assertEqual(<<"MCPB">>, binary:part(McpbData, 0, 4)),

    %% Parse
    {ok, {ParsedTs, ParsedCH, ParsedMR, ParsedRJ}} =
        mcp_governance_nif:mcpb_parse(McpbData, Pub),

    ?assertEqual(Timestamp, ParsedTs),
    ?assertEqual(ContractHash, ParsedCH),
    ?assertEqual(MerkleRoot, ParsedMR),
    ?assertEqual(ReceiptsJson, ParsedRJ),
    ok.

mcpb_invalid_signature_test() ->
    {ok, {Pub1, Priv1}} = mcp_governance_nif:ed25519_generate_keypair(),
    {ok, {Pub2, _Priv2}} = mcp_governance_nif:ed25519_generate_keypair(),

    Timestamp = erlang:system_time(millisecond),
    ContractHash = crypto:hash(sha256, <<"contract">>),
    MerkleRoot = crypto:hash(sha256, <<"merkle">>),
    ReceiptsJson = <<"[]">>,

    %% Serialize with key 1
    {ok, McpbData} = mcp_governance_nif:mcpb_serialize(
        Timestamp, ContractHash, MerkleRoot, ReceiptsJson, Priv1),

    %% Parse with key 2 should fail
    ?assertEqual({error, verification_failed},
                 mcp_governance_nif:mcpb_parse(McpbData, Pub2)),
    ok.

mcpb_invalid_magic_test() ->
    {ok, {Pub, _Priv}} = mcp_governance_nif:ed25519_generate_keypair(),
    InvalidData = <<"XXXX", 0:1024>>,
    ?assertEqual({error, invalid_magic},
                 mcp_governance_nif:mcpb_parse(InvalidData, Pub)),
    ok.

%%====================================================================
%% Base64 Tests
%%====================================================================

base64_roundtrip_test() ->
    Original = <<"Hello, MCP+ Governance!">>,
    Encoded = mcp_governance_nif:base64_encode(Original),
    {ok, Decoded} = mcp_governance_nif:base64_decode(Encoded),
    ?assertEqual(Original, Decoded),
    ok.

base64_binary_test() ->
    Original = crypto:strong_rand_bytes(32),
    Encoded = mcp_governance_nif:base64_encode(Original),
    {ok, Decoded} = mcp_governance_nif:base64_decode(Encoded),
    ?assertEqual(Original, Decoded),
    ok.

%%====================================================================
%% Test Runner
%%====================================================================

nif_test_() ->
    [
     {"SHA-256 hash", fun sha256_hash_test/0},
     {"SHA-256 different inputs", fun sha256_different_inputs_test/0},
     {"Ed25519 keypair generation", fun ed25519_keypair_generation_test/0},
     {"Ed25519 sign and verify", fun ed25519_sign_verify_test/0},
     {"Ed25519 different keypairs", fun ed25519_different_keypairs_test/0},
     {"Merkle root empty", fun merkle_root_empty_test/0},
     {"Merkle root single", fun merkle_root_single_test/0},
     {"Merkle root multiple", fun merkle_root_multiple_test/0},
     {"Merkle root different order", fun merkle_root_different_order_test/0},
     {"Merkle verify", fun merkle_verify_test/0},
     {"MCPB roundtrip", fun mcpb_roundtrip_test/0},
     {"MCPB invalid signature", fun mcpb_invalid_signature_test/0},
     {"MCPB invalid magic", fun mcpb_invalid_magic_test/0},
     {"Base64 roundtrip", fun base64_roundtrip_test/0},
     {"Base64 binary", fun base64_binary_test/0}
    ].
