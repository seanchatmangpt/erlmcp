%%%-------------------------------------------------------------------
%%% @doc PQC Receipt Chain Unit Tests
%%%
%%% Chicago School TDD: Real processes, no mocks.
%%% Tests verify actual PQC signature operations, hash chaining,
%%% merkle proofs, and receipt verification.
%%%
%%% Coverage Requirements: >= 80%
%%%
%%% Test Categories:
%%% - Genesis receipt creation and verification
%%% - Receipt chain extension and verification
%%% - Hash linkage validation
%%% - Signature verification
%%% - Merkle proof generation and verification
%%% - Serialization/deserialization
%%% - Artifact ID generation
%%% - Error handling
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(pqc_receipt_tests).

-include_lib("eunit/include/eunit.hrl").
-include("pqchain.hrl").

%%====================================================================
%% Test Fixtures
%%====================================================================

%% Setup fixture - generates test keypair
setup() ->
    %% Generate ML-DSA-65 keypair for signing receipts
    {ok, Keypair} = pqc_crypto:keygen(?PQC_SIG_ML_DSA_65),

    #{
        keypair => Keypair,
        public_key => Keypair#pqc_keypair.public_key,
        case_id => <<"test-case-", (integer_to_binary(erlang:system_time(microsecond)))/binary>>
    }.

%% Cleanup fixture (no resources to clean up)
cleanup(_State) ->
    ok.

%%====================================================================
%% Genesis Receipt Tests
%%====================================================================

genesis_creation_test() ->
    State = setup(),
    #{keypair := Keypair, case_id := CaseId} = State,

    Ts = erlang:system_time(millisecond),

    %% Create genesis receipt
    {ok, Receipt} = pqc_receipt:genesis(CaseId, Ts, Keypair),

    %% Verify receipt structure
    ?assertMatch(#{hash := _, signature := _, ts := _, sequence := _}, Receipt),

    %% Verify sequence is 0 for genesis
    ?assertEqual(0, maps:get(sequence, Receipt)),

    %% Verify prev_hash is undefined for genesis
    ?assertEqual(undefined, maps:get(prev_hash, Receipt)),

    %% Verify event_hash is undefined for genesis
    ?assertEqual(undefined, maps:get(event_hash, Receipt)),

    %% Verify timestamp
    ?assertEqual(Ts, maps:get(ts, Receipt)),

    %% Verify hash is binary
    Hash = maps:get(hash, Receipt),
    ?assert(is_binary(Hash)),
    ?assertEqual(32, byte_size(Hash)),  % SHA3-256 = 32 bytes

    %% Verify signature
    Signature = maps:get(signature, Receipt),
    ?assertMatch(#pqc_signature{}, Signature),
    ?assertEqual(?PQC_SIG_ML_DSA_65, Signature#pqc_signature.algorithm),

    cleanup(State).

genesis_verification_test() ->
    State = setup(),
    #{keypair := Keypair, public_key := PublicKey, case_id := CaseId} = State,

    Ts = erlang:system_time(millisecond),

    %% Create genesis receipt
    {ok, Receipt} = pqc_receipt:genesis(CaseId, Ts, Keypair),

    %% Verify signature
    {ok, IsValid} = pqc_receipt:verify(Receipt, PublicKey),
    ?assert(IsValid),

    cleanup(State).

genesis_invalid_case_id_test() ->
    State = setup(),
    #{keypair := Keypair} = State,

    Ts = erlang:system_time(millisecond),

    %% Try to create genesis with invalid case_id (not binary)
    Result = pqc_receipt:genesis(123, Ts, Keypair),
    ?assertMatch({error, {invalid_case_id, _}}, Result),

    cleanup(State).

genesis_invalid_timestamp_test() ->
    State = setup(),
    #{keypair := Keypair, case_id := CaseId} = State,

    %% Try to create genesis with invalid timestamp (negative)
    Result = pqc_receipt:genesis(CaseId, -1, Keypair),
    ?assertMatch({error, {invalid_timestamp, _}}, Result),

    cleanup(State).

genesis_invalid_key_test() ->
    State = setup(),
    #{case_id := CaseId} = State,

    Ts = erlang:system_time(millisecond),

    %% Try to create genesis with invalid keypair
    Result = pqc_receipt:genesis(CaseId, Ts, not_a_keypair),
    ?assertMatch({error, {invalid_signing_key, _}}, Result),

    cleanup(State).

%%====================================================================
%% Receipt Extension Tests
%%====================================================================

extend_receipt_test() ->
    State = setup(),
    #{keypair := Keypair, case_id := CaseId} = State,

    %% Create genesis receipt
    Ts1 = erlang:system_time(millisecond),
    {ok, Genesis} = pqc_receipt:genesis(CaseId, Ts1, Keypair),

    %% Extend with event
    Event = #{type => task_started, task_id => <<"task-1">>},
    Ts2 = Ts1 + 1000,
    {ok, Receipt2} = pqc_receipt:extend(Genesis, Event, Ts2, Keypair),

    %% Verify receipt structure
    ?assertMatch(#{hash := _, prev_hash := _, event_hash := _, signature := _, ts := _, sequence := _}, Receipt2),

    %% Verify sequence incremented
    ?assertEqual(1, maps:get(sequence, Receipt2)),

    %% Verify prev_hash links to genesis
    ?assertEqual(maps:get(hash, Genesis), maps:get(prev_hash, Receipt2)),

    %% Verify event_hash is present
    EventHash = maps:get(event_hash, Receipt2),
    ?assert(is_binary(EventHash)),
    ?assertEqual(32, byte_size(EventHash)),

    %% Verify timestamp
    ?assertEqual(Ts2, maps:get(ts, Receipt2)),

    cleanup(State).

extend_chain_multiple_test() ->
    State = setup(),
    #{keypair := Keypair, case_id := CaseId} = State,

    %% Create genesis receipt
    Ts1 = erlang:system_time(millisecond),
    {ok, R1} = pqc_receipt:genesis(CaseId, Ts1, Keypair),

    %% Extend multiple times
    Event2 = #{type => task_started, task_id => <<"task-1">>},
    Ts2 = Ts1 + 1000,
    {ok, R2} = pqc_receipt:extend(R1, Event2, Ts2, Keypair),

    Event3 = #{type => task_completed, task_id => <<"task-1">>, result => success},
    Ts3 = Ts2 + 2000,
    {ok, R3} = pqc_receipt:extend(R2, Event3, Ts3, Keypair),

    Event4 = #{type => task_started, task_id => <<"task-2">>},
    Ts4 = Ts3 + 1500,
    {ok, R4} = pqc_receipt:extend(R3, Event4, Ts4, Keypair),

    %% Verify sequences
    ?assertEqual(0, maps:get(sequence, R1)),
    ?assertEqual(1, maps:get(sequence, R2)),
    ?assertEqual(2, maps:get(sequence, R3)),
    ?assertEqual(3, maps:get(sequence, R4)),

    %% Verify hash linkage
    ?assertEqual(maps:get(hash, R1), maps:get(prev_hash, R2)),
    ?assertEqual(maps:get(hash, R2), maps:get(prev_hash, R3)),
    ?assertEqual(maps:get(hash, R3), maps:get(prev_hash, R4)),

    cleanup(State).

extend_invalid_prev_receipt_test() ->
    State = setup(),
    #{keypair := Keypair} = State,

    Event = #{type => test_event},
    Ts = erlang:system_time(millisecond),

    %% Try to extend with invalid prev_receipt (not a map)
    Result1 = pqc_receipt:extend(not_a_map, Event, Ts, Keypair),
    ?assertMatch({error, {invalid_prev_receipt, _}}, Result1),

    %% Try to extend with incomplete prev_receipt (missing hash)
    Result2 = pqc_receipt:extend(#{sequence => 0}, Event, Ts, Keypair),
    ?assertMatch({error, {invalid_prev_receipt, _}}, Result2),

    cleanup(State).

%%====================================================================
%% Chain Verification Tests
%%====================================================================

verify_chain_test() ->
    State = setup(),
    #{keypair := Keypair, public_key := PublicKey, case_id := CaseId} = State,

    %% Create chain of receipts
    Ts1 = erlang:system_time(millisecond),
    {ok, R1} = pqc_receipt:genesis(CaseId, Ts1, Keypair),

    Event2 = #{type => event2},
    Ts2 = Ts1 + 1000,
    {ok, R2} = pqc_receipt:extend(R1, Event2, Ts2, Keypair),

    Event3 = #{type => event3},
    Ts3 = Ts2 + 1000,
    {ok, R3} = pqc_receipt:extend(R2, Event3, Ts3, Keypair),

    %% Verify entire chain
    Chain = [R1, R2, R3],
    {ok, IsValid} = pqc_receipt:verify_chain(Chain, PublicKey),
    ?assert(IsValid),

    cleanup(State).

verify_chain_empty_test() ->
    State = setup(),
    #{public_key := PublicKey} = State,

    %% Try to verify empty chain
    Result = pqc_receipt:verify_chain([], PublicKey),
    ?assertMatch({error, {invalid_chain, empty_chain}}, Result),

    cleanup(State).

verify_chain_broken_link_test() ->
    State = setup(),
    #{keypair := Keypair, public_key := PublicKey, case_id := CaseId} = State,

    %% Create chain of receipts
    Ts1 = erlang:system_time(millisecond),
    {ok, R1} = pqc_receipt:genesis(CaseId, Ts1, Keypair),

    Event2 = #{type => event2},
    Ts2 = Ts1 + 1000,
    {ok, R2} = pqc_receipt:extend(R1, Event2, Ts2, Keypair),

    Event3 = #{type => event3},
    Ts3 = Ts2 + 1000,
    {ok, R3} = pqc_receipt:extend(R2, Event3, Ts3, Keypair),

    %% Break the chain by modifying prev_hash of R3
    R3Broken = R3#{prev_hash => <<0:256>>},

    %% Verify chain - should fail
    Chain = [R1, R2, R3Broken],
    {ok, IsValid} = pqc_receipt:verify_chain(Chain, PublicKey),
    ?assertNot(IsValid),

    cleanup(State).

verify_chain_broken_sequence_test() ->
    State = setup(),
    #{keypair := Keypair, public_key := PublicKey, case_id := CaseId} = State,

    %% Create chain of receipts
    Ts1 = erlang:system_time(millisecond),
    {ok, R1} = pqc_receipt:genesis(CaseId, Ts1, Keypair),

    Event2 = #{type => event2},
    Ts2 = Ts1 + 1000,
    {ok, R2} = pqc_receipt:extend(R1, Event2, Ts2, Keypair),

    %% Break sequence number
    R2Broken = R2#{sequence => 5},

    %% Verify chain - should fail
    Chain = [R1, R2Broken],
    {ok, IsValid} = pqc_receipt:verify_chain(Chain, PublicKey),
    ?assertNot(IsValid),

    cleanup(State).

%%====================================================================
%% Serialization Tests
%%====================================================================

serialize_deserialize_test() ->
    State = setup(),
    #{keypair := Keypair, case_id := CaseId} = State,

    %% Create receipt
    Ts = erlang:system_time(millisecond),
    {ok, Receipt} = pqc_receipt:genesis(CaseId, Ts, Keypair),

    %% Serialize
    Binary = pqc_receipt:to_binary(Receipt),
    ?assert(is_binary(Binary)),

    %% Deserialize
    {ok, Deserialized} = pqc_receipt:from_binary(Binary),
    ?assertEqual(Receipt, Deserialized),

    cleanup(State).

deserialize_invalid_binary_test() ->
    State = setup(),

    %% Try to deserialize invalid binary
    Result = pqc_receipt:from_binary(<<"invalid binary data">>),
    ?assertMatch({error, {decode_error, _}}, Result),

    cleanup(State).

deserialize_non_binary_test() ->
    State = setup(),

    %% Try to deserialize non-binary
    Result = pqc_receipt:from_binary(123),
    ?assertMatch({error, {invalid_input, not_binary}}, Result),

    cleanup(State).

%%====================================================================
%% Artifact ID Tests
%%====================================================================

artifact_id_test() ->
    State = setup(),
    #{keypair := Keypair, case_id := CaseId} = State,

    %% Create receipt
    Ts1 = erlang:system_time(millisecond),
    {ok, Receipt} = pqc_receipt:genesis(CaseId, Ts1, Keypair),

    %% Generate artifact ID
    Ts2 = Ts1 + 1000,
    ArtifactId = pqc_receipt:artifact_id(Receipt, Ts2),

    %% Verify artifact ID is binary (hex string)
    ?assert(is_binary(ArtifactId)),
    ?assertEqual(64, byte_size(ArtifactId)),  % SHA3-256 hex = 64 chars

    %% Verify deterministic (same inputs = same output)
    ArtifactId2 = pqc_receipt:artifact_id(Receipt, Ts2),
    ?assertEqual(ArtifactId, ArtifactId2),

    %% Verify different timestamp = different artifact ID
    Ts3 = Ts2 + 1000,
    ArtifactId3 = pqc_receipt:artifact_id(Receipt, Ts3),
    ?assertNotEqual(ArtifactId, ArtifactId3),

    cleanup(State).

%%====================================================================
%% Merkle Proof Tests
%%====================================================================

merkle_proof_single_receipt_test() ->
    State = setup(),
    #{keypair := Keypair, public_key := PublicKey, case_id := CaseId} = State,

    %% Create single receipt
    Ts = erlang:system_time(millisecond),
    {ok, Receipt} = pqc_receipt:genesis(CaseId, Ts, Keypair),

    %% Generate proof
    {ok, Proof} = pqc_receipt:get_proof([Receipt], 0),

    %% Verify proof structure
    ?assertMatch(#{index := 0, receipt_hash := _, siblings := [], root := _}, Proof),

    %% Verify proof
    Root = maps:get(root, Proof),
    {ok, IsValid} = pqc_receipt:verify_proof(Receipt, Proof, Root),
    ?assert(IsValid),

    cleanup(State).

merkle_proof_multiple_receipts_test() ->
    State = setup(),
    #{keypair := Keypair, case_id := CaseId} = State,

    %% Create chain of 4 receipts
    Ts1 = erlang:system_time(millisecond),
    {ok, R1} = pqc_receipt:genesis(CaseId, Ts1, Keypair),

    {ok, R2} = pqc_receipt:extend(R1, #{type => e2}, Ts1 + 1000, Keypair),
    {ok, R3} = pqc_receipt:extend(R2, #{type => e3}, Ts1 + 2000, Keypair),
    {ok, R4} = pqc_receipt:extend(R3, #{type => e4}, Ts1 + 3000, Keypair),

    Receipts = [R1, R2, R3, R4],

    %% Generate and verify proof for each receipt
    lists:foreach(
        fun({Index, Receipt}) ->
            {ok, Proof} = pqc_receipt:get_proof(Receipts, Index),
            Root = maps:get(root, Proof),
            {ok, IsValid} = pqc_receipt:verify_proof(Receipt, Proof, Root),
            ?assert(IsValid)
        end,
        lists:zip(lists:seq(0, 3), Receipts)
    ),

    cleanup(State).

merkle_proof_index_out_of_bounds_test() ->
    State = setup(),
    #{keypair := Keypair, case_id := CaseId} = State,

    %% Create single receipt
    Ts = erlang:system_time(millisecond),
    {ok, Receipt} = pqc_receipt:genesis(CaseId, Ts, Keypair),

    %% Try to get proof for out of bounds index
    Result = pqc_receipt:get_proof([Receipt], 5),
    ?assertMatch({error, {index_out_of_bounds, _, _}}, Result),

    cleanup(State).

merkle_proof_tampered_receipt_test() ->
    State = setup(),
    #{keypair := Keypair, case_id := CaseId} = State,

    %% Create chain of receipts
    Ts1 = erlang:system_time(millisecond),
    {ok, R1} = pqc_receipt:genesis(CaseId, Ts1, Keypair),
    {ok, R2} = pqc_receipt:extend(R1, #{type => e2}, Ts1 + 1000, Keypair),
    {ok, R3} = pqc_receipt:extend(R2, #{type => e3}, Ts1 + 2000, Keypair),

    Receipts = [R1, R2, R3],

    %% Generate proof for R2
    {ok, Proof} = pqc_receipt:get_proof(Receipts, 1),
    Root = maps:get(root, Proof),

    %% Tamper with R2
    R2Tampered = R2#{hash => <<0:256>>},

    %% Verify proof with tampered receipt - should fail
    {ok, IsValid} = pqc_receipt:verify_proof(R2Tampered, Proof, Root),
    ?assertNot(IsValid),

    cleanup(State).

merkle_proof_wrong_root_test() ->
    State = setup(),
    #{keypair := Keypair, case_id := CaseId} = State,

    %% Create chain of receipts
    Ts1 = erlang:system_time(millisecond),
    {ok, R1} = pqc_receipt:genesis(CaseId, Ts1, Keypair),
    {ok, R2} = pqc_receipt:extend(R1, #{type => e2}, Ts1 + 1000, Keypair),

    Receipts = [R1, R2],

    %% Generate proof
    {ok, Proof} = pqc_receipt:get_proof(Receipts, 1),

    %% Verify with wrong root
    WrongRoot = <<0:256>>,
    {ok, IsValid} = pqc_receipt:verify_proof(R2, Proof, WrongRoot),
    ?assertNot(IsValid),

    cleanup(State).

%%====================================================================
%% Integration Tests
%%====================================================================

full_workflow_test() ->
    State = setup(),
    #{keypair := Keypair, public_key := PublicKey, case_id := CaseId} = State,

    %% 1. Create genesis receipt
    Ts1 = erlang:system_time(millisecond),
    {ok, R1} = pqc_receipt:genesis(CaseId, Ts1, Keypair),
    ?assertEqual(0, maps:get(sequence, R1)),

    %% 2. Verify genesis
    {ok, Valid1} = pqc_receipt:verify(R1, PublicKey),
    ?assert(Valid1),

    %% 3. Extend with events
    Events = [
        #{type => task_started, task_id => <<"t1">>},
        #{type => task_completed, task_id => <<"t1">>, result => success},
        #{type => task_started, task_id => <<"t2">>},
        #{type => task_failed, task_id => <<"t2">>, error => timeout}
    ],

    %% Build chain
    {Receipts, _} = lists:foldl(
        fun(Event, {Acc, PrevReceipt}) ->
            Ts = erlang:system_time(millisecond),
            {ok, NextReceipt} = pqc_receipt:extend(PrevReceipt, Event, Ts, Keypair),
            {[NextReceipt | Acc], NextReceipt}
        end,
        {[R1], R1},
        Events
    ),

    Chain = lists:reverse(Receipts),

    %% 4. Verify entire chain
    {ok, ChainValid} = pqc_receipt:verify_chain(Chain, PublicKey),
    ?assert(ChainValid),
    ?assertEqual(5, length(Chain)),

    %% 5. Serialize and deserialize
    SerializedChain = [pqc_receipt:to_binary(R) || R <- Chain],
    ?assertEqual(5, length(SerializedChain)),

    {ok, DeserializedChain} = lists:foldl(
        fun(Binary, {ok, Acc}) ->
            {ok, Receipt} = pqc_receipt:from_binary(Binary),
            {ok, [Receipt | Acc]}
        end,
        {ok, []},
        SerializedChain
    ),
    ?assertEqual(Chain, lists:reverse(DeserializedChain)),

    %% 6. Generate artifact ID
    LatestReceipt = lists:last(Chain),
    ArtifactId = pqc_receipt:artifact_id(LatestReceipt, erlang:system_time(millisecond)),
    ?assertEqual(64, byte_size(ArtifactId)),

    %% 7. Generate and verify merkle proofs
    MiddleIndex = 2,
    MiddleReceipt = lists:nth(MiddleIndex + 1, Chain),
    {ok, MerkleProof} = pqc_receipt:get_proof(Chain, MiddleIndex),
    Root = maps:get(root, MerkleProof),
    {ok, ProofValid} = pqc_receipt:verify_proof(MiddleReceipt, MerkleProof, Root),
    ?assert(ProofValid),

    cleanup(State).

%%====================================================================
%% Performance Tests (optional)
%%====================================================================

performance_long_chain_test() ->
    State = setup(),
    #{keypair := Keypair, public_key := PublicKey, case_id := CaseId} = State,

    %% Create genesis
    Ts1 = erlang:system_time(millisecond),
    {ok, Genesis} = pqc_receipt:genesis(CaseId, Ts1, Keypair),

    %% Build chain of 100 receipts
    ChainLength = 100,
    {Chain, _} = lists:foldl(
        fun(N, {Acc, PrevReceipt}) ->
            Event = #{type => test_event, index => N},
            Ts = erlang:system_time(millisecond),
            {ok, NextReceipt} = pqc_receipt:extend(PrevReceipt, Event, Ts, Keypair),
            {[NextReceipt | Acc], NextReceipt}
        end,
        {[Genesis], Genesis},
        lists:seq(1, ChainLength - 1)
    ),

    FullChain = lists:reverse(Chain),

    %% Verify chain
    StartTime = erlang:monotonic_time(millisecond),
    {ok, IsValid} = pqc_receipt:verify_chain(FullChain, PublicKey),
    EndTime = erlang:monotonic_time(millisecond),

    ?assert(IsValid),
    ?assertEqual(ChainLength, length(FullChain)),

    %% Log performance (optional - for CI monitoring)
    ElapsedMs = EndTime - StartTime,
    io:format("Chain verification (~p receipts): ~p ms~n", [ChainLength, ElapsedMs]),

    cleanup(State).
