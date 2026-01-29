%%%-------------------------------------------------------------------
%% @doc MCP+ Governance Framework Core Tests (No External Dependencies)
%%
%% Tests that prove the governance framework core logic works
%% without requiring jsx or other external dependencies.
%%
%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_governance_core_tests).

-include_lib("eunit/include/eunit.hrl").
-include("erlmcp_governance.hrl").

%%====================================================================
%% Envelope Tests (No External Dependencies)
%%====================================================================

envelope_default_creation_test() ->
    %% Create envelope with defaults
    Envelope = erlmcp_envelope:create_default(),

    %% Verify all defaults are set
    ?assert(is_binary(Envelope#mcp_envelope.id)),
    ?assertEqual(30000, Envelope#mcp_envelope.max_duration_ms),
    ?assertEqual(536870912, Envelope#mcp_envelope.max_memory_bytes),
    ?assertEqual(10000, Envelope#mcp_envelope.max_cpu_ms),
    ?assertEqual(104857600, Envelope#mcp_envelope.max_io_bytes),
    ?assertEqual(52428800, Envelope#mcp_envelope.max_network_bytes),
    ?assertEqual(100, Envelope#mcp_envelope.max_requests_per_sec),
    ?assertEqual(5000, Envelope#mcp_envelope.max_requests_per_min),
    ?assertEqual(50, Envelope#mcp_envelope.max_concurrent),
    ?assertEqual(16777216, Envelope#mcp_envelope.max_payload_bytes),
    ?assertEqual(10, Envelope#mcp_envelope.max_recursion_depth),
    ?assertEqual(true, Envelope#mcp_envelope.fail_closed),
    ok.

envelope_custom_creation_test() ->
    %% Create envelope with custom limits
    Opts = #{
        max_duration_ms => 5000,
        max_memory_bytes => 1048576,
        max_cpu_ms => 2000,
        max_io_bytes => 5242880,
        max_requests_per_sec => 50,
        fail_closed => false,
        uncertainty_threshold => 0.7
    },
    Envelope = erlmcp_envelope:create(Opts),

    ?assertEqual(5000, Envelope#mcp_envelope.max_duration_ms),
    ?assertEqual(1048576, Envelope#mcp_envelope.max_memory_bytes),
    ?assertEqual(2000, Envelope#mcp_envelope.max_cpu_ms),
    ?assertEqual(5242880, Envelope#mcp_envelope.max_io_bytes),
    ?assertEqual(50, Envelope#mcp_envelope.max_requests_per_sec),
    ?assertEqual(false, Envelope#mcp_envelope.fail_closed),
    ?assertEqual(0.7, Envelope#mcp_envelope.uncertainty_threshold),
    ok.

envelope_duration_within_limits_test() ->
    Envelope = erlmcp_envelope:create(#{max_duration_ms => 10000}),

    %% 5 seconds within 10 second limit
    ?assertEqual(ok, erlmcp_envelope:check_duration(Envelope, #{duration_ms => 5000})),

    %% Exactly at limit
    ?assertEqual(ok, erlmcp_envelope:check_duration(Envelope, #{duration_ms => 10000})),
    ok.

envelope_duration_exceeds_limits_test() ->
    Envelope = erlmcp_envelope:create(#{max_duration_ms => 10000}),

    %% 15 seconds exceeds 10 second limit
    Result = erlmcp_envelope:check_duration(Envelope, #{duration_ms => 15000}),
    ?assertMatch({error, {?REFUSAL_ENVELOPE_DURATION_EXCEEDED, _, _}}, Result),

    {error, {Code, Msg, Details}} = Result,
    ?assertEqual(?REFUSAL_ENVELOPE_DURATION_EXCEEDED, Code),
    ?assert(is_binary(Msg)),
    ?assertEqual(10000, maps:get(limit_ms, Details)),
    ?assertEqual(15000, maps:get(actual_ms, Details)),
    ok.

envelope_memory_within_limits_test() ->
    Envelope = erlmcp_envelope:create(#{max_memory_bytes => 1048576}),

    %% 512KB within 1MB limit
    ?assertEqual(ok, erlmcp_envelope:check_memory(Envelope, #{memory_bytes => 524288})),
    ok.

envelope_memory_exceeds_limits_test() ->
    Envelope = erlmcp_envelope:create(#{max_memory_bytes => 1048576}),

    %% 2MB exceeds 1MB limit
    Result = erlmcp_envelope:check_memory(Envelope, #{memory_bytes => 2097152}),
    ?assertMatch({error, {?REFUSAL_ENVELOPE_MEMORY_EXCEEDED, _, _}}, Result),
    ok.

envelope_cpu_within_limits_test() ->
    Envelope = erlmcp_envelope:create(#{max_cpu_ms => 5000}),
    ?assertEqual(ok, erlmcp_envelope:check_cpu(Envelope, #{cpu_ms => 2000})),
    ok.

envelope_cpu_exceeds_limits_test() ->
    Envelope = erlmcp_envelope:create(#{max_cpu_ms => 5000}),
    Result = erlmcp_envelope:check_cpu(Envelope, #{cpu_ms => 8000}),
    ?assertMatch({error, {?REFUSAL_ENVELOPE_CPU_EXCEEDED, _, _}}, Result),
    ok.

envelope_io_within_limits_test() ->
    Envelope = erlmcp_envelope:create(#{max_io_bytes => 104857600}),
    ?assertEqual(ok, erlmcp_envelope:check_io(Envelope, #{io_bytes => 52428800})),
    ok.

envelope_io_exceeds_limits_test() ->
    Envelope = erlmcp_envelope:create(#{max_io_bytes => 104857600}),
    Result = erlmcp_envelope:check_io(Envelope, #{io_bytes => 209715200}),
    ?assertMatch({error, {?REFUSAL_ENVELOPE_IO_EXCEEDED, _, _}}, Result),
    ok.

envelope_network_within_limits_test() ->
    Envelope = erlmcp_envelope:create(#{max_network_bytes => 52428800}),
    ?assertEqual(ok, erlmcp_envelope:check_network(Envelope, #{network_bytes => 26214400})),
    ok.

envelope_network_exceeds_limits_test() ->
    Envelope = erlmcp_envelope:create(#{max_network_bytes => 52428800}),
    Result = erlmcp_envelope:check_network(Envelope, #{network_bytes => 104857600}),
    ?assertMatch({error, {?REFUSAL_ENVELOPE_NETWORK_EXCEEDED, _, _}}, Result),
    ok.

envelope_payload_within_limits_test() ->
    Envelope = erlmcp_envelope:create(#{max_payload_bytes => 1024}),
    ?assertEqual(ok, erlmcp_envelope:check_payload(Envelope, #{payload_bytes => 512})),
    ok.

envelope_payload_exceeds_limits_test() ->
    Envelope = erlmcp_envelope:create(#{max_payload_bytes => 1024}),
    Result = erlmcp_envelope:check_payload(Envelope, #{payload_bytes => 2048}),
    ?assertMatch({error, {?REFUSAL_ENVELOPE_PAYLOAD_EXCEEDED, _, _}}, Result),
    ok.

envelope_recursion_within_limits_test() ->
    Envelope = erlmcp_envelope:create(#{max_recursion_depth => 10}),
    ?assertEqual(ok, erlmcp_envelope:check_recursion(Envelope, #{recursion_depth => 5})),
    ok.

envelope_recursion_exceeds_limits_test() ->
    Envelope = erlmcp_envelope:create(#{max_recursion_depth => 10}),
    Result = erlmcp_envelope:check_recursion(Envelope, #{recursion_depth => 15}),
    ?assertMatch({error, {?REFUSAL_ENVELOPE_RECURSION_EXCEEDED, _, _}}, Result),
    ok.

envelope_operation_whitelist_allowed_test() ->
    Envelope = erlmcp_envelope:create(#{
        allowed_operations => [<<"read">>, <<"list">>, <<"get">>]
    }),
    ?assertEqual(ok, erlmcp_envelope:check_operation(Envelope, #{operation => <<"read">>})),
    ?assertEqual(ok, erlmcp_envelope:check_operation(Envelope, #{operation => <<"list">>})),
    ?assertEqual(ok, erlmcp_envelope:check_operation(Envelope, #{operation => <<"get">>})),
    ok.

envelope_operation_whitelist_denied_test() ->
    Envelope = erlmcp_envelope:create(#{
        allowed_operations => [<<"read">>, <<"list">>]
    }),
    Result = erlmcp_envelope:check_operation(Envelope, #{operation => <<"delete">>}),
    ?assertMatch({error, {?REFUSAL_ENVELOPE_OPERATION_DENIED, _, _}}, Result),
    ok.

envelope_operation_blacklist_test() ->
    Envelope = erlmcp_envelope:create(#{
        allowed_operations => [],
        denied_operations => [<<"delete">>, <<"drop">>]
    }),
    %% Allowed (not in blacklist)
    ?assertEqual(ok, erlmcp_envelope:check_operation(Envelope, #{operation => <<"read">>})),

    %% Denied (in blacklist)
    Result = erlmcp_envelope:check_operation(Envelope, #{operation => <<"delete">>}),
    ?assertMatch({error, {?REFUSAL_ENVELOPE_OPERATION_DENIED, _, _}}, Result),
    ok.

envelope_fail_closed_high_confidence_test() ->
    Envelope = erlmcp_envelope:create(#{
        fail_closed => true,
        uncertainty_threshold => 0.8
    }),
    %% High confidence (0.95 >= 0.8) - should pass
    ?assertEqual(ok, erlmcp_envelope:check_uncertainty(Envelope, #{confidence => 0.95})),
    ?assertEqual(ok, erlmcp_envelope:check_uncertainty(Envelope, #{confidence => 0.8})),
    ok.

envelope_fail_closed_low_confidence_test() ->
    Envelope = erlmcp_envelope:create(#{
        fail_closed => true,
        uncertainty_threshold => 0.8
    }),
    %% Low confidence (0.5 < 0.8) - should fail
    Result = erlmcp_envelope:check_uncertainty(Envelope, #{confidence => 0.5}),
    ?assertMatch({error, {?REFUSAL_ENVELOPE_UNCERTAINTY, _, _}}, Result),

    {error, {_, _, Details}} = Result,
    ?assertEqual(0.8, maps:get(threshold, Details)),
    ?assertEqual(0.5, maps:get(actual, Details)),
    ok.

envelope_fail_closed_disabled_test() ->
    Envelope = erlmcp_envelope:create(#{
        fail_closed => false,
        uncertainty_threshold => 0.8
    }),
    %% With fail_closed disabled, even low confidence passes
    ?assertEqual(ok, erlmcp_envelope:check_uncertainty(Envelope, #{confidence => 0.1})),
    ok.

envelope_should_fail_closed_test() ->
    Envelope = erlmcp_envelope:create(#{
        fail_closed => true,
        uncertainty_threshold => 0.8
    }),
    ?assertEqual(false, erlmcp_envelope:should_fail_closed(Envelope, 0.9)),
    ?assertEqual(false, erlmcp_envelope:should_fail_closed(Envelope, 0.8)),
    ?assertEqual(true, erlmcp_envelope:should_fail_closed(Envelope, 0.7)),
    ?assertEqual(true, erlmcp_envelope:should_fail_closed(Envelope, 0.0)),
    ok.

%%====================================================================
%% Contract Tier Classification Tests
%%====================================================================

tier_classification_automate_test() ->
    Workflow = #{
        <<"requires_human_judgment">> => false,
        <<"requires_supervision">> => false
    },
    ?assertEqual(tier_a_automate, erlmcp_contract:classify_tier(Workflow)),
    ok.

tier_classification_assist_test() ->
    Workflow = #{
        <<"requires_human_judgment">> => false,
        <<"requires_supervision">> => true
    },
    ?assertEqual(tier_b_assist, erlmcp_contract:classify_tier(Workflow)),
    ok.

tier_classification_do_not_automate_test() ->
    Workflow = #{
        <<"requires_human_judgment">> => true,
        <<"requires_supervision">> => false
    },
    ?assertEqual(tier_c_do_not_automate, erlmcp_contract:classify_tier(Workflow)),

    %% Human judgment takes precedence over supervision
    Workflow2 = #{
        <<"requires_human_judgment">> => true,
        <<"requires_supervision">> => true
    },
    ?assertEqual(tier_c_do_not_automate, erlmcp_contract:classify_tier(Workflow2)),
    ok.

tier_validation_automate_test() ->
    ?assertEqual(ok, erlmcp_contract:validate_tier(tier_a_automate, <<"any_operation">>)),
    ok.

tier_validation_do_not_automate_test() ->
    Result = erlmcp_contract:validate_tier(tier_c_do_not_automate, <<"any_operation">>),
    ?assertMatch({error, {?REFUSAL_CONTRACT_TIER_VIOLATION, _, _}}, Result),
    ok.

%%====================================================================
%% Receipt Text-Blind Hashing Tests
%%====================================================================

receipt_hash_binary_test() ->
    %% Binary input produces SHA-256 hash
    Binary = <<"test payload">>,
    Hash = erlmcp_receipt:hash_request(Binary),

    ?assertEqual(32, byte_size(Hash)),  % SHA-256 = 32 bytes
    ok.

receipt_hash_deterministic_test() ->
    %% Same input always produces same hash
    Binary = <<"deterministic test">>,
    Hash1 = erlmcp_receipt:hash_request(Binary),
    Hash2 = erlmcp_receipt:hash_request(Binary),
    Hash3 = erlmcp_receipt:hash_request(Binary),

    ?assertEqual(Hash1, Hash2),
    ?assertEqual(Hash2, Hash3),
    ok.

receipt_hash_different_inputs_test() ->
    %% Different inputs produce different hashes
    Hash1 = erlmcp_receipt:hash_request(<<"input A">>),
    Hash2 = erlmcp_receipt:hash_request(<<"input B">>),

    ?assertNotEqual(Hash1, Hash2),
    ok.

receipt_response_hash_test() ->
    %% Response hashing works the same
    Hash = erlmcp_receipt:hash_response(<<"response data">>),
    ?assertEqual(32, byte_size(Hash)),
    ok.

%%====================================================================
%% Merkle Tree Tests
%%====================================================================

merkle_root_empty_list_test() ->
    Root = erlmcp_evidence_bundle:compute_merkle_root([]),
    %% Empty list produces hash of empty binary
    ?assertEqual(32, byte_size(Root)),
    ok.

%%====================================================================
%% Refusal Code Tests
%%====================================================================

refusal_codes_contract_test() ->
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
    ok.

refusal_codes_envelope_test() ->
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

refusal_codes_kill_switch_test() ->
    ?assertEqual(2041, ?REFUSAL_KILL_SWITCH_GLOBAL),
    ?assertEqual(2042, ?REFUSAL_KILL_SWITCH_FAMILY),
    ?assertEqual(2043, ?REFUSAL_KILL_SWITCH_CAPABILITY),
    ?assertEqual(2044, ?REFUSAL_KILL_SWITCH_EPOCH),
    ok.

%%====================================================================
%% Type Record Tests
%%====================================================================

mcp_envelope_record_test() ->
    %% Verify mcp_envelope record structure
    Envelope = #mcp_envelope{
        id = <<"test_id">>,
        contract_id = <<"contract_123">>,
        max_duration_ms = 5000,
        max_memory_bytes = 1048576,
        max_cpu_ms = 2000,
        max_io_bytes = 5242880,
        max_network_bytes = 2621440,
        max_requests_per_sec = 100,
        max_requests_per_min = 5000,
        max_concurrent = 10,
        max_payload_bytes = 1024,
        max_response_bytes = 2048,
        max_recursion_depth = 5,
        allowed_operations = [<<"read">>],
        denied_operations = [<<"delete">>],
        fail_closed = true,
        uncertainty_threshold = 0.8,
        metrics_enabled = true,
        trace_enabled = false
    },

    ?assertEqual(<<"test_id">>, Envelope#mcp_envelope.id),
    ?assertEqual(5000, Envelope#mcp_envelope.max_duration_ms),
    ?assertEqual(true, Envelope#mcp_envelope.fail_closed),
    ?assertEqual([<<"read">>], Envelope#mcp_envelope.allowed_operations),
    ok.

%%====================================================================
%% Test Runner
%%====================================================================

governance_core_test_() ->
    {setup,
     fun() -> application:ensure_all_started(crypto) end,
     fun(_) -> ok end,
     [
      %% Envelope Creation
      {"Envelope default creation", fun envelope_default_creation_test/0},
      {"Envelope custom creation", fun envelope_custom_creation_test/0},

      %% Duration Checks
      {"Duration within limits", fun envelope_duration_within_limits_test/0},
      {"Duration exceeds limits", fun envelope_duration_exceeds_limits_test/0},

      %% Memory Checks
      {"Memory within limits", fun envelope_memory_within_limits_test/0},
      {"Memory exceeds limits", fun envelope_memory_exceeds_limits_test/0},

      %% CPU Checks
      {"CPU within limits", fun envelope_cpu_within_limits_test/0},
      {"CPU exceeds limits", fun envelope_cpu_exceeds_limits_test/0},

      %% I/O Checks
      {"I/O within limits", fun envelope_io_within_limits_test/0},
      {"I/O exceeds limits", fun envelope_io_exceeds_limits_test/0},

      %% Network Checks
      {"Network within limits", fun envelope_network_within_limits_test/0},
      {"Network exceeds limits", fun envelope_network_exceeds_limits_test/0},

      %% Payload Checks
      {"Payload within limits", fun envelope_payload_within_limits_test/0},
      {"Payload exceeds limits", fun envelope_payload_exceeds_limits_test/0},

      %% Recursion Checks
      {"Recursion within limits", fun envelope_recursion_within_limits_test/0},
      {"Recursion exceeds limits", fun envelope_recursion_exceeds_limits_test/0},

      %% Operation Checks
      {"Operation whitelist allowed", fun envelope_operation_whitelist_allowed_test/0},
      {"Operation whitelist denied", fun envelope_operation_whitelist_denied_test/0},
      {"Operation blacklist", fun envelope_operation_blacklist_test/0},

      %% Fail-Closed Policy
      {"Fail-closed high confidence", fun envelope_fail_closed_high_confidence_test/0},
      {"Fail-closed low confidence", fun envelope_fail_closed_low_confidence_test/0},
      {"Fail-closed disabled", fun envelope_fail_closed_disabled_test/0},
      {"Should fail closed", fun envelope_should_fail_closed_test/0},

      %% Tier Classification
      {"Tier classification: automate", fun tier_classification_automate_test/0},
      {"Tier classification: assist", fun tier_classification_assist_test/0},
      {"Tier classification: do not automate", fun tier_classification_do_not_automate_test/0},
      {"Tier validation: automate", fun tier_validation_automate_test/0},
      {"Tier validation: do not automate", fun tier_validation_do_not_automate_test/0},

      %% Receipt Hashing
      {"Receipt hash binary", fun receipt_hash_binary_test/0},
      {"Receipt hash deterministic", fun receipt_hash_deterministic_test/0},
      {"Receipt hash different inputs", fun receipt_hash_different_inputs_test/0},
      {"Receipt response hash", fun receipt_response_hash_test/0},

      %% Merkle Tree
      {"Merkle root empty list", fun merkle_root_empty_list_test/0},

      %% Refusal Codes
      {"Refusal codes: contract", fun refusal_codes_contract_test/0},
      {"Refusal codes: envelope", fun refusal_codes_envelope_test/0},
      {"Refusal codes: kill switch", fun refusal_codes_kill_switch_test/0},

      %% Records
      {"MCP envelope record", fun mcp_envelope_record_test/0}
     ]
    }.
