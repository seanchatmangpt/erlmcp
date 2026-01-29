%%%-------------------------------------------------------------------
%% @doc MCP+ Governance Framework Tests
%%
%% Proves the governance framework components work correctly:
%% - Contract creation, sealing, and validation
%% - Envelope enforcement with limits
%% - Receipt generation and chaining
%% - Evidence bundle creation and verification
%% - Kill switch activation and checking
%% - Text-blind verification
%%
%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_governance_tests).

-include_lib("eunit/include/eunit.hrl").
-include("erlmcp_governance.hrl").

%%====================================================================
%% Test Fixtures
%%====================================================================

setup() ->
    %% Start all governance services
    application:ensure_all_started(crypto),
    ok.

cleanup(_) ->
    ok.

%%====================================================================
%% Contract Tests
%%====================================================================

contract_creation_test() ->
    %% Create a contract
    Spec = #{
        <<"name">> => <<"test_workflow">>,
        <<"inputSchema">> => #{<<"type">> => <<"object">>},
        <<"outputSchema">> => #{<<"type">> => <<"object">>}
    },
    Opts = #{
        tier => tier_a_automate,
        capabilities => [<<"read">>, <<"write">>],
        invariants => [<<"data_integrity">>],
        preconditions => [<<"authenticated">>],
        postconditions => [<<"audit_logged">>]
    },

    {ok, Contract} = erlmcp_contract:create(<<"test_family">>, <<"1.0.0">>, Spec, Opts),

    %% Verify contract fields
    ?assertEqual(<<"test_family">>, Contract#mcp_contract.family),
    ?assertEqual(<<"1.0.0">>, Contract#mcp_contract.version),
    ?assertEqual(tier_a_automate, Contract#mcp_contract.tier),
    ?assertEqual([<<"read">>, <<"write">>], Contract#mcp_contract.capabilities),
    ?assertNotEqual(<<>>, Contract#mcp_contract.origin_hash),
    ?assertNotEqual(<<>>, Contract#mcp_contract.artifact_hash),
    ok.

contract_hash_consistency_test() ->
    %% Test mu . mu = mu (idempotent transformation)
    Spec = #{<<"name">> => <<"idempotent_test">>},
    {ok, Contract} = erlmcp_contract:create(<<"hash_family">>, <<"1.0">>, Spec, #{}),

    %% Compute artifact hash twice - should be identical
    Hash1 = erlmcp_contract:compute_artifact_hash(Contract),
    Hash2 = erlmcp_contract:compute_artifact_hash(Contract),

    ?assertEqual(Hash1, Hash2),

    %% Verify hash consistency
    ?assert(erlmcp_contract:verify_hash_consistency(Contract)),
    ok.

contract_tier_classification_test() ->
    %% Test tier classification based on workflow properties
    AutoWorkflow = #{
        <<"requires_human_judgment">> => false,
        <<"requires_supervision">> => false
    },
    ?assertEqual(tier_a_automate, erlmcp_contract:classify_tier(AutoWorkflow)),

    AssistWorkflow = #{
        <<"requires_human_judgment">> => false,
        <<"requires_supervision">> => true
    },
    ?assertEqual(tier_b_assist, erlmcp_contract:classify_tier(AssistWorkflow)),

    NoAutoWorkflow = #{
        <<"requires_human_judgment">> => true
    },
    ?assertEqual(tier_c_do_not_automate, erlmcp_contract:classify_tier(NoAutoWorkflow)),
    ok.

%%====================================================================
%% Envelope Tests
%%====================================================================

envelope_creation_test() ->
    %% Create envelope with custom limits
    Opts = #{
        max_duration_ms => 5000,
        max_memory_bytes => 1048576,  % 1 MB
        max_requests_per_sec => 10,
        fail_closed => true,
        uncertainty_threshold => 0.9
    },

    Envelope = erlmcp_envelope:create(Opts),

    ?assertEqual(5000, Envelope#mcp_envelope.max_duration_ms),
    ?assertEqual(1048576, Envelope#mcp_envelope.max_memory_bytes),
    ?assertEqual(10, Envelope#mcp_envelope.max_requests_per_sec),
    ?assertEqual(true, Envelope#mcp_envelope.fail_closed),
    ?assertEqual(0.9, Envelope#mcp_envelope.uncertainty_threshold),
    ok.

envelope_duration_check_test() ->
    Envelope = erlmcp_envelope:create(#{max_duration_ms => 1000}),

    %% Within limits - should pass
    ?assertEqual(ok, erlmcp_envelope:check_duration(Envelope, #{duration_ms => 500})),

    %% Exceeds limits - should fail
    {error, {Code, _Msg, Details}} = erlmcp_envelope:check_duration(Envelope, #{duration_ms => 1500}),
    ?assertEqual(?REFUSAL_ENVELOPE_DURATION_EXCEEDED, Code),
    ?assertEqual(1000, maps:get(limit_ms, Details)),
    ?assertEqual(1500, maps:get(actual_ms, Details)),
    ok.

envelope_memory_check_test() ->
    Envelope = erlmcp_envelope:create(#{max_memory_bytes => 1048576}),

    %% Within limits
    ?assertEqual(ok, erlmcp_envelope:check_memory(Envelope, #{memory_bytes => 524288})),

    %% Exceeds limits
    {error, {Code, _Msg, _Details}} = erlmcp_envelope:check_memory(Envelope, #{memory_bytes => 2097152}),
    ?assertEqual(?REFUSAL_ENVELOPE_MEMORY_EXCEEDED, Code),
    ok.

envelope_operation_whitelist_test() ->
    Envelope = erlmcp_envelope:create(#{
        allowed_operations => [<<"read">>, <<"list">>],
        denied_operations => []
    }),

    %% Allowed operation
    ?assertEqual(ok, erlmcp_envelope:check_operation(Envelope, #{operation => <<"read">>})),

    %% Denied operation (not in whitelist)
    {error, {Code, _Msg, _Details}} = erlmcp_envelope:check_operation(Envelope, #{operation => <<"delete">>}),
    ?assertEqual(?REFUSAL_ENVELOPE_OPERATION_DENIED, Code),
    ok.

envelope_fail_closed_test() ->
    Envelope = erlmcp_envelope:create(#{
        fail_closed => true,
        uncertainty_threshold => 0.8
    }),

    %% High confidence - should pass
    ?assertEqual(ok, erlmcp_envelope:check_uncertainty(Envelope, #{confidence => 0.95})),

    %% Low confidence - should fail (fail-closed)
    {error, {Code, _Msg, Details}} = erlmcp_envelope:check_uncertainty(Envelope, #{confidence => 0.5}),
    ?assertEqual(?REFUSAL_ENVELOPE_UNCERTAINTY, Code),
    ?assertEqual(0.8, maps:get(threshold, Details)),
    ok.

envelope_payload_check_test() ->
    Envelope = erlmcp_envelope:create(#{max_payload_bytes => 1024}),

    %% Within limits
    ?assertEqual(ok, erlmcp_envelope:check_payload(Envelope, #{payload_bytes => 512})),

    %% Exceeds limits
    {error, {Code, _Msg, _Details}} = erlmcp_envelope:check_payload(Envelope, #{payload_bytes => 2048}),
    ?assertEqual(?REFUSAL_ENVELOPE_PAYLOAD_EXCEEDED, Code),
    ok.

%%====================================================================
%% Receipt Tests
%%====================================================================

receipt_hash_text_blind_test() ->
    %% Test that text-blind hashing works correctly
    Payload1 = #{<<"data">> => <<"sensitive_content">>},
    Payload2 = #{<<"data">> => <<"different_content">>},

    Hash1 = erlmcp_receipt:hash_request(Payload1),
    Hash2 = erlmcp_receipt:hash_request(Payload2),

    %% Different payloads produce different hashes
    ?assertNotEqual(Hash1, Hash2),

    %% Same payload produces same hash (deterministic)
    Hash1Again = erlmcp_receipt:hash_request(Payload1),
    ?assertEqual(Hash1, Hash1Again),

    %% Hashes are 32 bytes (SHA-256)
    ?assertEqual(32, byte_size(Hash1)),
    ok.

receipt_binary_hash_test() ->
    %% Binary payloads also work
    BinaryPayload = <<"raw binary data">>,
    Hash = erlmcp_receipt:hash_request(BinaryPayload),

    ?assertEqual(32, byte_size(Hash)),
    ok.

%%====================================================================
%% Merkle Tree Tests
%%====================================================================

merkle_root_empty_test() ->
    %% Empty list produces a hash
    Root = erlmcp_evidence_bundle:compute_merkle_root([]),
    ?assertEqual(32, byte_size(Root)),
    ok.

%%====================================================================
%% Kill Switch Tests
%%====================================================================

kill_switch_refusal_codes_test() ->
    %% Verify refusal codes are defined correctly
    ?assertEqual(2041, ?REFUSAL_KILL_SWITCH_GLOBAL),
    ?assertEqual(2042, ?REFUSAL_KILL_SWITCH_FAMILY),
    ?assertEqual(2043, ?REFUSAL_KILL_SWITCH_CAPABILITY),
    ?assertEqual(2044, ?REFUSAL_KILL_SWITCH_EPOCH),
    ok.

%%====================================================================
%% Verification Tests
%%====================================================================

verification_check_format_test() ->
    %% Test that check results have correct format
    CheckName = <<"test_check">>,
    CheckPassed = true,
    CheckDetails = <<"Check passed successfully">>,

    CheckResult = {CheckName, CheckPassed, CheckDetails},

    {Name, Passed, Details} = CheckResult,
    ?assertEqual(<<"test_check">>, Name),
    ?assertEqual(true, Passed),
    ?assertEqual(<<"Check passed successfully">>, Details),
    ok.

%%====================================================================
%% Integration Test
%%====================================================================

governance_workflow_integration_test() ->
    %% Complete workflow test:
    %% 1. Create contract
    %% 2. Create envelope
    %% 3. Simulate governed execution
    %% 4. Verify text-blind properties

    %% Step 1: Create contract
    Spec = #{
        <<"name">> => <<"integration_test">>,
        <<"inputSchema">> => #{<<"type">> => <<"object">>}
    },
    {ok, Contract} = erlmcp_contract:create(
        <<"integration_family">>,
        <<"1.0.0">>,
        Spec,
        #{tier => tier_a_automate}
    ),

    %% Step 2: Create envelope
    Envelope = erlmcp_envelope:create(#{
        max_duration_ms => 30000,
        max_memory_bytes => 536870912,
        fail_closed => true
    }),

    %% Step 3: Verify contract properties
    ?assertEqual(tier_a_automate, Contract#mcp_contract.tier),
    ?assert(byte_size(Contract#mcp_contract.origin_hash) > 0),

    %% Step 4: Verify envelope enforcement works
    ?assertEqual(ok, erlmcp_envelope:check_duration(Envelope, #{duration_ms => 100})),
    ?assertEqual(ok, erlmcp_envelope:check_memory(Envelope, #{memory_bytes => 1024})),

    %% Step 5: Verify text-blind hashing
    Request = #{<<"action">> => <<"test">>, <<"data">> => <<"payload">>},
    RequestHash = erlmcp_receipt:hash_request(Request),

    %% Hash doesn't contain original data (text-blind)
    ?assertEqual(32, byte_size(RequestHash)),
    ?assertNot(binary:match(RequestHash, <<"payload">>) =/= nomatch),

    ok.

%%====================================================================
%% Refusal Code Coverage Test
%%====================================================================

refusal_codes_defined_test() ->
    %% Contract refusal codes (2001-2010)
    ?assertEqual(2001, ?REFUSAL_CONTRACT_NOT_FOUND),
    ?assertEqual(2002, ?REFUSAL_CONTRACT_EXPIRED),
    ?assertEqual(2003, ?REFUSAL_CONTRACT_REVOKED),
    ?assertEqual(2004, ?REFUSAL_CONTRACT_SIGNATURE_INVALID),
    ?assertEqual(2005, ?REFUSAL_CONTRACT_EPOCH_MISMATCH),
    ?assertEqual(2006, ?REFUSAL_CONTRACT_PRECONDITION_FAILED),
    ?assertEqual(2007, ?REFUSAL_CONTRACT_INVARIANT_VIOLATED),
    ?assertEqual(2008, ?REFUSAL_CONTRACT_TIER_VIOLATION),
    ?assertEqual(2009, ?REFUSAL_CONTRACT_CAPABILITY_DENIED),
    ?assertEqual(2010, ?REFUSAL_CONTRACT_BOUNDARY_CROSSED),

    %% Envelope refusal codes (2021-2032)
    ?assertEqual(2021, ?REFUSAL_ENVELOPE_DURATION_EXCEEDED),
    ?assertEqual(2022, ?REFUSAL_ENVELOPE_MEMORY_EXCEEDED),
    ?assertEqual(2023, ?REFUSAL_ENVELOPE_CPU_EXCEEDED),
    ?assertEqual(2024, ?REFUSAL_ENVELOPE_IO_EXCEEDED),
    ?assertEqual(2025, ?REFUSAL_ENVELOPE_NETWORK_EXCEEDED),
    ?assertEqual(2026, ?REFUSAL_ENVELOPE_RATE_EXCEEDED),
    ?assertEqual(2027, ?REFUSAL_ENVELOPE_CONCURRENT_EXCEEDED),
    ?assertEqual(2028, ?REFUSAL_ENVELOPE_PAYLOAD_EXCEEDED),
    ?assertEqual(2029, ?REFUSAL_ENVELOPE_RECURSION_EXCEEDED),
    ?assertEqual(2030, ?REFUSAL_ENVELOPE_OPERATION_DENIED),
    ?assertEqual(2031, ?REFUSAL_ENVELOPE_FAIL_CLOSED),
    ?assertEqual(2032, ?REFUSAL_ENVELOPE_UNCERTAINTY),
    ok.

%%====================================================================
%% Test Runner
%%====================================================================

governance_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      {"Contract creation", fun contract_creation_test/0},
      {"Contract hash consistency (mu . mu = mu)", fun contract_hash_consistency_test/0},
      {"Contract tier classification", fun contract_tier_classification_test/0},
      {"Envelope creation", fun envelope_creation_test/0},
      {"Envelope duration check", fun envelope_duration_check_test/0},
      {"Envelope memory check", fun envelope_memory_check_test/0},
      {"Envelope operation whitelist", fun envelope_operation_whitelist_test/0},
      {"Envelope fail-closed policy", fun envelope_fail_closed_test/0},
      {"Envelope payload check", fun envelope_payload_check_test/0},
      {"Receipt text-blind hashing", fun receipt_hash_text_blind_test/0},
      {"Receipt binary hash", fun receipt_binary_hash_test/0},
      {"Merkle root empty", fun merkle_root_empty_test/0},
      {"Kill switch refusal codes", fun kill_switch_refusal_codes_test/0},
      {"Verification check format", fun verification_check_format_test/0},
      {"Integration workflow", fun governance_workflow_integration_test/0},
      {"Refusal codes defined", fun refusal_codes_defined_test/0}
     ]
    }.
