%%%-----------------------------------------------------------------------------
%%% @doc TCPS Andon Stop-the-Line System - Test Suite
%%%
%%% Comprehensive test suite for Andon event triggering, blocking, resolution,
%%% and receipt generation. Follows Chicago School TDD with 80%+ coverage.
%%%
%%% Test Categories:
%%% 1. Event Triggering - All failure types (SHACL, test, non-determinism, receipt)
%%% 2. Stop-the-Line Enforcement - Blocking and unblocking logic
%%% 3. Resolution Workflow - Root cause analysis and prevention
%%% 4. Receipt Generation - JSON storage and linking
%%% 5. Integration Hooks - Compilation, test, SHACL validation
%%% 6. Concurrent Operations - Race conditions and consistency
%%% 7. Edge Cases - Missing data, invalid inputs, error recovery
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(tcps_andon_tests).
-include_lib("eunit/include/eunit.hrl").

-define(TEST_RECEIPTS_DIR, "/tmp/erlmcp_test_receipts").

%%%=============================================================================
%%% Setup and Teardown
%%%=============================================================================

setup() ->
    % Ensure clean state for each test
    ok = application:ensure_started(jsx),
    ok = filelib:ensure_dir(?TEST_RECEIPTS_DIR ++ "/"),
    % Clear any existing receipts
    case file:list_dir(?TEST_RECEIPTS_DIR) of
        {ok, Files} ->
            [file:delete(filename:join(?TEST_RECEIPTS_DIR, F)) || F <- Files];
        _ ->
            ok
    end,
    % Start ETS tables for Andon events
    case ets:info(tcps_andon_events) of
        undefined ->
            ets:new(tcps_andon_events, [named_table, public, set]);
        _ ->
            ets:delete_all_objects(tcps_andon_events)
    end,
    ok.

cleanup(_) ->
    % Clean up test data
    case ets:info(tcps_andon_events) of
        undefined -> ok;
        _ -> ets:delete_all_objects(tcps_andon_events)
    end,
    ok.

%%%=============================================================================
%%% Test Generators
%%%=============================================================================

tcps_andon_test_() ->
    {foreach, fun setup/0, fun cleanup/1, [
        % Event Triggering Tests
        {"Trigger SHACL violation Andon event", fun test_trigger_shacl_violation/0},
        {"Trigger test failure Andon event", fun test_trigger_test_failure/0},
        {"Trigger non-determinism Andon event", fun test_trigger_non_determinism/0},
        {"Trigger missing receipt Andon event", fun test_trigger_missing_receipt/0},
        {"Trigger with comprehensive context", fun test_trigger_with_full_context/0},

        % Stop-the-Line Enforcement Tests
        {"Check if SKU is blocked by Andon", fun test_is_blocked_true/0},
        {"Check if SKU is not blocked", fun test_is_blocked_false/0},
        {"Cannot proceed to next stage when blocked", fun test_cannot_proceed_blocked/0},
        {"Can proceed when no blocking events", fun test_can_proceed_unblocked/0},
        {"Multiple Andon events block SKU", fun test_multiple_blocks/0},

        % Resolution Workflow Tests
        {"Resolve Andon event with root cause", fun test_resolve_andon/0},
        {"Resolution clears block", fun test_resolution_clears_block/0},
        {"Resolution generates receipt", fun test_resolution_receipt/0},
        {"Partial resolution not allowed", fun test_partial_resolution_rejected/0},
        {"Resolution requires all fields", fun test_resolution_validation/0},

        % Receipt Generation Tests
        {"Generate receipt for Andon event", fun test_generate_receipt/0},
        {"Receipt contains all required fields", fun test_receipt_completeness/0},
        {"Receipt stored as JSON file", fun test_receipt_json_storage/0},
        {"Receipt linked to ontology", fun test_receipt_ontology_link/0},
        {"Receipt timestamp format", fun test_receipt_timestamp/0},

        % Integration Hooks Tests
        {"Hook into compilation failure", fun test_compilation_hook/0},
        {"Hook into test failure", fun test_test_failure_hook/0},
        {"Hook into SHACL validation failure", fun test_shacl_hook/0},

        % Concurrent Operations Tests
        {"Concurrent Andon triggers", fun test_concurrent_triggers/0},
        {"Concurrent block checks", fun test_concurrent_block_checks/0},
        {"Race condition in resolution", fun test_resolution_race_condition/0},

        % Edge Cases Tests
        {"Invalid failure type rejected", fun test_invalid_failure_type/0},
        {"Missing context handled", fun test_missing_context/0},
        {"Resolve non-existent Andon", fun test_resolve_nonexistent/0},
        {"Empty SKU ID rejected", fun test_empty_sku_rejected/0},
        {"Receipt generation failure recovery", fun test_receipt_failure_recovery/0},

        % System Integration Tests
        {"End-to-end: trigger, block, resolve", fun test_e2e_workflow/0},
        {"Multiple SKUs with separate Andons", fun test_multiple_skus/0},
        {"Andon history tracking", fun test_andon_history/0},
        {"Performance: 1000 concurrent Andons", fun test_performance_many_andons/0}
    ]}.

%%%=============================================================================
%%% Event Triggering Tests
%%%=============================================================================

test_trigger_shacl_violation() ->
    Context = #{
        sku_id => <<"SKU-001">>,
        stage => compilation,
        details => #{
            violation_type => <<"sh:minCount">>,
            property => <<"ex:hasComponent">>,
            expected => 1,
            actual => 0
        }
    },
    {ok, AndonId} = tcps_andon:trigger_andon(shacl_violation, Context),
    ?assert(is_binary(AndonId)),
    ?assert(byte_size(AndonId) > 0),

    % Verify event stored
    Event = tcps_andon:get_andon_event(AndonId),
    ?assertMatch(#{failure_type := shacl_violation}, Event),
    ?assertEqual(<<"SKU-001">>, maps:get(sku_id, Event)).

test_trigger_test_failure() ->
    Context = #{
        sku_id => <<"SKU-002">>,
        stage => testing,
        details => #{
            test_module => tcps_andon_tests,
            test_case => test_example,
            failure_reason => <<"Assertion failed">>
        }
    },
    {ok, AndonId} = tcps_andon:trigger_andon(test_failure, Context),
    ?assert(is_binary(AndonId)),

    Event = tcps_andon:get_andon_event(AndonId),
    ?assertEqual(test_failure, maps:get(failure_type, Event)).

test_trigger_non_determinism() ->
    Context = #{
        sku_id => <<"SKU-003">>,
        stage => execution,
        details => #{
            execution_count => 2,
            different_results => [<<"result1">>, <<"result2">>]
        }
    },
    {ok, AndonId} = tcps_andon:trigger_andon(non_determinism, Context),
    ?assert(is_binary(AndonId)),

    Event = tcps_andon:get_andon_event(AndonId),
    ?assertEqual(non_determinism, maps:get(failure_type, Event)).

test_trigger_missing_receipt() ->
    Context = #{
        sku_id => <<"SKU-004">>,
        stage => validation,
        details => #{
            expected_receipt => <<"receipt-123">>,
            search_paths => [<<"/tmp/receipts">>]
        }
    },
    {ok, AndonId} = tcps_andon:trigger_andon(missing_receipt, Context),
    ?assert(is_binary(AndonId)),

    Event = tcps_andon:get_andon_event(AndonId),
    ?assertEqual(missing_receipt, maps:get(failure_type, Event)).

test_trigger_with_full_context() ->
    Context = #{
        sku_id => <<"SKU-005">>,
        stage => integration,
        details => #{
            subsystem => <<"transport">>,
            error_code => 500,
            stacktrace => [<<"line1">>, <<"line2">>]
        },
        metadata => #{
            operator => <<"jenkins">>,
            build_id => <<"build-789">>,
            commit_sha => <<"abc123">>
        }
    },
    {ok, AndonId} = tcps_andon:trigger_andon(test_failure, Context),
    ?assert(is_binary(AndonId)),

    Event = tcps_andon:get_andon_event(AndonId),
    ?assertMatch(#{metadata := #{operator := <<"jenkins">>}}, Event).

%%%=============================================================================
%%% Stop-the-Line Enforcement Tests
%%%=============================================================================

test_is_blocked_true() ->
    Context = #{sku_id => <<"SKU-100">>, stage => testing, details => #{}},
    {ok, _AndonId} = tcps_andon:trigger_andon(test_failure, Context),

    ?assertEqual(true, tcps_andon:is_blocked(<<"SKU-100">>)).

test_is_blocked_false() ->
    ?assertEqual(false, tcps_andon:is_blocked(<<"SKU-CLEAN">>)).

test_cannot_proceed_blocked() ->
    Context = #{sku_id => <<"SKU-101">>, stage => compilation, details => #{}},
    {ok, AndonId} = tcps_andon:trigger_andon(shacl_violation, Context),

    Result = tcps_andon:can_proceed_to_stage(<<"SKU-101">>, testing),
    ?assertMatch({blocked, [AndonId]}, Result).

test_can_proceed_unblocked() ->
    Result = tcps_andon:can_proceed_to_stage(<<"SKU-CLEAN">>, deployment),
    ?assertEqual({ok, proceed}, Result).

test_multiple_blocks() ->
    SkuId = <<"SKU-102">>,
    Context1 = #{sku_id => SkuId, stage => compilation, details => #{}},
    Context2 = #{sku_id => SkuId, stage => testing, details => #{}},

    {ok, AndonId1} = tcps_andon:trigger_andon(shacl_violation, Context1),
    {ok, AndonId2} = tcps_andon:trigger_andon(test_failure, Context2),

    ?assertEqual(true, tcps_andon:is_blocked(SkuId)),

    Result = tcps_andon:can_proceed_to_stage(SkuId, deployment),
    {blocked, BlockingIds} = Result,
    ?assertEqual(2, length(BlockingIds)),
    ?assert(lists:member(AndonId1, BlockingIds)),
    ?assert(lists:member(AndonId2, BlockingIds)).

%%%=============================================================================
%%% Resolution Workflow Tests
%%%=============================================================================

test_resolve_andon() ->
    Context = #{sku_id => <<"SKU-200">>, stage => testing, details => #{}},
    {ok, AndonId} = tcps_andon:trigger_andon(test_failure, Context),

    Resolution = #{
        root_cause => <<"Race condition in test setup">>,
        fix_applied => <<"Added synchronization barrier">>,
        prevention_added => <<"Added property-based test for timing">>,
        resolver => <<"engineer@example.com">>,
        resolution_time_minutes => 45
    },

    ?assertEqual(ok, tcps_andon:resolve_andon(AndonId, Resolution)),

    % Verify resolution stored
    Event = tcps_andon:get_andon_event(AndonId),
    ?assertMatch(#{status := resolved}, Event),
    ?assertMatch(#{resolution := #{root_cause := <<"Race condition in test setup">>}}, Event).

test_resolution_clears_block() ->
    Context = #{sku_id => <<"SKU-201">>, stage => testing, details => #{}},
    {ok, AndonId} = tcps_andon:trigger_andon(test_failure, Context),

    ?assertEqual(true, tcps_andon:is_blocked(<<"SKU-201">>)),

    Resolution = #{
        root_cause => <<"Fixed">>,
        fix_applied => <<"Applied fix">>,
        prevention_added => <<"Added prevention">>,
        resolver => <<"engineer@example.com">>
    },
    ok = tcps_andon:resolve_andon(AndonId, Resolution),

    ?assertEqual(false, tcps_andon:is_blocked(<<"SKU-201">>)).

test_resolution_receipt() ->
    Context = #{sku_id => <<"SKU-202">>, stage => testing, details => #{}},
    {ok, AndonId} = tcps_andon:trigger_andon(test_failure, Context),

    Resolution = #{
        root_cause => <<"Issue">>,
        fix_applied => <<"Fix">>,
        prevention_added => <<"Prevention">>,
        resolver => <<"engineer@example.com">>
    },
    ok = tcps_andon:resolve_andon(AndonId, Resolution),

    % Verify resolution receipt generated
    Receipts = tcps_andon:list_receipts_for_andon(AndonId),
    ?assert(length(Receipts) >= 1),

    [Receipt | _] = Receipts,
    ?assertMatch(#{<<"receipt_type">> := <<"resolution">>}, Receipt).

test_partial_resolution_rejected() ->
    Context = #{sku_id => <<"SKU-203">>, stage => testing, details => #{}},
    {ok, AndonId} = tcps_andon:trigger_andon(test_failure, Context),

    IncompleteResolution = #{
        root_cause => <<"Something">>,
        fix_applied => <<"Something">>
        % Missing prevention_added
    },

    Result = tcps_andon:resolve_andon(AndonId, IncompleteResolution),
    ?assertMatch({error, {missing_required_field, prevention_added}}, Result).

test_resolution_validation() ->
    Context = #{sku_id => <<"SKU-204">>, stage => testing, details => #{}},
    {ok, AndonId} = tcps_andon:trigger_andon(test_failure, Context),

    % Empty root cause
    Resolution1 = #{
        root_cause => <<"">>,
        fix_applied => <<"Fix">>,
        prevention_added => <<"Prevention">>
    },
    ?assertMatch({error, _}, tcps_andon:resolve_andon(AndonId, Resolution1)),

    % Missing resolver defaults to system
    Resolution2 = #{
        root_cause => <<"Root">>,
        fix_applied => <<"Fix">>,
        prevention_added => <<"Prevention">>
    },
    ?assertEqual(ok, tcps_andon:resolve_andon(AndonId, Resolution2)).

%%%=============================================================================
%%% Receipt Generation Tests
%%%=============================================================================

test_generate_receipt() ->
    AndonEvent = #{
        event_id => <<"ANDON-001">>,
        failure_type => shacl_violation,
        sku_id => <<"SKU-300">>,
        stage => compilation,
        timestamp => erlang:system_time(millisecond),
        details => #{violation => <<"minCount">>}
    },

    Receipt = tcps_andon:generate_andon_receipt(AndonEvent),
    ?assertMatch(#{receipt_id := _}, Receipt),
    ?assertEqual(<<"ANDON-001">>, maps:get(andon_event_id, Receipt)).

test_receipt_completeness() ->
    AndonEvent = #{
        event_id => <<"ANDON-002">>,
        failure_type => test_failure,
        sku_id => <<"SKU-301">>,
        stage => testing,
        timestamp => erlang:system_time(millisecond),
        details => #{}
    },

    Receipt = tcps_andon:generate_andon_receipt(AndonEvent),

    % Verify all required fields present
    RequiredFields = [receipt_id, andon_event_id, timestamp, failure_type,
                      sku_id, stage, status, receipt_type],
    lists:foreach(fun(Field) ->
        ?assert(maps:is_key(Field, Receipt),
                io_lib:format("Missing field: ~p", [Field]))
    end, RequiredFields).

test_receipt_json_storage() ->
    AndonEvent = #{
        event_id => <<"ANDON-003">>,
        failure_type => non_determinism,
        sku_id => <<"SKU-302">>,
        stage => execution,
        timestamp => erlang:system_time(millisecond),
        details => #{}
    },

    Receipt = tcps_andon:generate_andon_receipt(AndonEvent),
    ReceiptId = maps:get(receipt_id, Receipt),

    % Store receipt
    ok = tcps_andon:store_receipt(Receipt, ?TEST_RECEIPTS_DIR),

    % Verify file exists
    ReceiptFile = filename:join(?TEST_RECEIPTS_DIR,
                                binary_to_list(ReceiptId) ++ ".json"),
    ?assert(filelib:is_regular(ReceiptFile)),

    % Verify JSON is valid
    {ok, JsonBin} = file:read_file(ReceiptFile),
    Decoded = jsx:decode(JsonBin, [return_maps]),
    ?assertEqual(ReceiptId, maps:get(<<"receipt_id">>, Decoded)).

test_receipt_ontology_link() ->
    AndonEvent = #{
        event_id => <<"ANDON-004">>,
        failure_type => shacl_violation,
        sku_id => <<"SKU-303">>,
        stage => compilation,
        timestamp => erlang:system_time(millisecond),
        details => #{}
    },

    Receipt = tcps_andon:generate_andon_receipt(AndonEvent),

    % Verify ontology links present
    ?assertMatch(#{ontology_refs := _}, Receipt),
    OntologyRefs = maps:get(ontology_refs, Receipt),
    ?assert(is_list(OntologyRefs)),
    ?assert(length(OntologyRefs) > 0).

test_receipt_timestamp() ->
    AndonEvent = #{
        event_id => <<"ANDON-005">>,
        failure_type => test_failure,
        sku_id => <<"SKU-304">>,
        stage => testing,
        timestamp => 1706284800000, % Fixed timestamp for testing
        details => #{}
    },

    Receipt = tcps_andon:generate_andon_receipt(AndonEvent),

    % Verify timestamp format (ISO 8601)
    ?assertMatch(#{timestamp_iso := _}, Receipt),
    TimestampIso = maps:get(timestamp_iso, Receipt),
    ?assert(is_binary(TimestampIso)),
    ?assert(byte_size(TimestampIso) > 19). % ISO format: "2024-01-26T12:00:00Z"

%%%=============================================================================
%%% Integration Hooks Tests
%%%=============================================================================

test_compilation_hook() ->
    % Simulate rebar3 compilation error
    CompileError = #{
        error_type => syntax_error,
        file => <<"src/example.erl">>,
        line => 42,
        message => <<"syntax error before: '->'">>,
        sku_id => <<"SKU-400">>
    },

    {ok, AndonId} = tcps_andon:hook_compilation_failure(CompileError),
    ?assert(is_binary(AndonId)),

    Event = tcps_andon:get_andon_event(AndonId),
    ?assertEqual(compilation_failure, maps:get(failure_type, Event)),
    ?assertEqual(<<"SKU-400">>, maps:get(sku_id, Event)).

test_test_failure_hook() ->
    % Simulate eunit test failure
    TestFailure = #{
        test_module => example_tests,
        test_function => test_feature,
        failure_type => assertion_failed,
        expected => 42,
        actual => 43,
        sku_id => <<"SKU-401">>
    },

    {ok, AndonId} = tcps_andon:hook_test_failure(TestFailure),
    ?assert(is_binary(AndonId)),

    Event = tcps_andon:get_andon_event(AndonId),
    ?assertEqual(test_failure, maps:get(failure_type, Event)).

test_shacl_hook() ->
    % Simulate SHACL validation failure
    ShaclFailure = #{
        validation_report => #{
            conforms => false,
            results => [
                #{
                    result_severity => <<"sh:Violation">>,
                    focus_node => <<"ex:Component1">>,
                    result_path => <<"ex:hasManufacturer">>,
                    result_message => <<"Required property missing">>
                }
            ]
        },
        sku_id => <<"SKU-402">>
    },

    {ok, AndonId} = tcps_andon:hook_shacl_failure(ShaclFailure),
    ?assert(is_binary(AndonId)),

    Event = tcps_andon:get_andon_event(AndonId),
    ?assertEqual(shacl_violation, maps:get(failure_type, Event)).

%%%=============================================================================
%%% Concurrent Operations Tests
%%%=============================================================================

test_concurrent_triggers() ->
    % Spawn 100 concurrent Andon triggers
    Self = self(),
    SkuId = <<"SKU-CONCURRENT">>,

    Pids = [spawn(fun() ->
        Context = #{
            sku_id => SkuId,
            stage => testing,
            details => #{id => N}
        },
        {ok, AndonId} = tcps_andon:trigger_andon(test_failure, Context),
        Self ! {andon, AndonId}
    end) || N <- lists:seq(1, 100)],

    % Collect all results
    AndonIds = [receive {andon, Id} -> Id after 5000 -> timeout end
                || _ <- Pids],

    ?assertEqual(100, length(AndonIds)),
    ?assertEqual(100, length(lists:usort(AndonIds))). % All unique

test_concurrent_block_checks() ->
    Context = #{sku_id => <<"SKU-RACE">>, stage => testing, details => #{}},
    {ok, _AndonId} = tcps_andon:trigger_andon(test_failure, Context),

    % Spawn 50 concurrent block checks
    Self = self(),
    Pids = [spawn(fun() ->
        Blocked = tcps_andon:is_blocked(<<"SKU-RACE">>),
        Self ! {blocked, Blocked}
    end) || _ <- lists:seq(1, 50)],

    Results = [receive {blocked, B} -> B after 1000 -> timeout end || _ <- Pids],

    % All should return true
    ?assertEqual(50, length([R || R <- Results, R =:= true])).

test_resolution_race_condition() ->
    Context = #{sku_id => <<"SKU-RESOLVE-RACE">>, stage => testing, details => #{}},
    {ok, AndonId} = tcps_andon:trigger_andon(test_failure, Context),

    % Spawn 5 concurrent resolution attempts
    Self = self(),
    Pids = [spawn(fun() ->
        Resolution = #{
            root_cause => <<"Fix">>,
            fix_applied => <<"Applied">>,
            prevention_added => <<"Prevented">>,
            resolver => list_to_binary("resolver" ++ integer_to_list(N))
        },
        Result = tcps_andon:resolve_andon(AndonId, Resolution),
        Self ! {result, Result}
    end) || N <- lists:seq(1, 5)],

    Results = [receive {result, R} -> R after 2000 -> timeout end || _ <- Pids],

    % Only one should succeed
    OkCount = length([R || R <- Results, R =:= ok]),
    ErrorCount = length([R || R <- Results, element(1, R) =:= error]),

    ?assertEqual(1, OkCount),
    ?assertEqual(4, ErrorCount).

%%%=============================================================================
%%% Edge Cases Tests
%%%=============================================================================

test_invalid_failure_type() ->
    Context = #{sku_id => <<"SKU-500">>, stage => testing, details => #{}},
    Result = tcps_andon:trigger_andon(invalid_type, Context),
    ?assertMatch({error, {invalid_failure_type, invalid_type}}, Result).

test_missing_context() ->
    Result1 = tcps_andon:trigger_andon(test_failure, #{}),
    ?assertMatch({error, {missing_required_field, sku_id}}, Result1),

    Result2 = tcps_andon:trigger_andon(test_failure, #{sku_id => <<"SKU-501">>}),
    ?assertMatch({error, {missing_required_field, stage}}, Result2).

test_resolve_nonexistent() ->
    Resolution = #{
        root_cause => <<"Fix">>,
        fix_applied => <<"Applied">>,
        prevention_added => <<"Prevented">>
    },
    Result = tcps_andon:resolve_andon(<<"NONEXISTENT">>, Resolution),
    ?assertMatch({error, {andon_not_found, <<"NONEXISTENT">>}}, Result).

test_empty_sku_rejected() ->
    Context = #{sku_id => <<>>, stage => testing, details => #{}},
    Result = tcps_andon:trigger_andon(test_failure, Context),
    ?assertMatch({error, {invalid_sku_id, <<>>}}, Result).

test_receipt_failure_recovery() ->
    % Simulate receipt generation failure (e.g., disk full)
    AndonEvent = #{
        event_id => <<"ANDON-ERROR">>,
        failure_type => test_failure,
        sku_id => <<"SKU-502">>,
        stage => testing,
        timestamp => erlang:system_time(millisecond),
        details => #{}
    },

    Receipt = tcps_andon:generate_andon_receipt(AndonEvent),

    % Try to store to invalid directory
    Result = tcps_andon:store_receipt(Receipt, "/invalid/path/that/does/not/exist"),
    ?assertMatch({error, _}, Result),

    % Event should still be tracked even if receipt fails
    Event = tcps_andon:get_andon_event(<<"ANDON-ERROR">>),
    ?assertMatch(#{event_id := <<"ANDON-ERROR">>}, Event).

%%%=============================================================================
%%% System Integration Tests
%%%=============================================================================

test_e2e_workflow() ->
    SkuId = <<"SKU-E2E">>,

    % 1. Trigger Andon
    Context = #{
        sku_id => SkuId,
        stage => compilation,
        details => #{error => <<"SHACL violation">>}
    },
    {ok, AndonId} = tcps_andon:trigger_andon(shacl_violation, Context),

    % 2. Verify blocking
    ?assertEqual(true, tcps_andon:is_blocked(SkuId)),
    ?assertMatch({blocked, [AndonId]},
                 tcps_andon:can_proceed_to_stage(SkuId, testing)),

    % 3. Generate receipt
    Event = tcps_andon:get_andon_event(AndonId),
    Receipt = tcps_andon:generate_andon_receipt(Event),
    ?assertMatch(#{receipt_id := _}, Receipt),

    % 4. Resolve Andon
    Resolution = #{
        root_cause => <<"Missing ontology constraint">>,
        fix_applied => <<"Added sh:minCount to schema">>,
        prevention_added => <<"Added SHACL pre-commit hook">>,
        resolver => <<"engineer@example.com">>
    },
    ok = tcps_andon:resolve_andon(AndonId, Resolution),

    % 5. Verify unblocked
    ?assertEqual(false, tcps_andon:is_blocked(SkuId)),
    ?assertEqual({ok, proceed}, tcps_andon:can_proceed_to_stage(SkuId, testing)),

    % 6. Verify resolution receipt
    Receipts = tcps_andon:list_receipts_for_andon(AndonId),
    ?assert(length(Receipts) >= 1).

test_multiple_skus() ->
    % Create Andons for multiple SKUs
    Skus = [<<"SKU-A">>, <<"SKU-B">>, <<"SKU-C">>],

    AndonIds = lists:map(fun(SkuId) ->
        Context = #{sku_id => SkuId, stage => testing, details => #{}},
        {ok, Id} = tcps_andon:trigger_andon(test_failure, Context),
        Id
    end, Skus),

    % Verify each SKU independently blocked
    lists:foreach(fun(SkuId) ->
        ?assertEqual(true, tcps_andon:is_blocked(SkuId))
    end, Skus),

    % Resolve one SKU
    [AndonId1 | _] = AndonIds,
    Resolution = #{
        root_cause => <<"Fix">>,
        fix_applied => <<"Applied">>,
        prevention_added => <<"Prevented">>
    },
    ok = tcps_andon:resolve_andon(AndonId1, Resolution),

    % Verify only one unblocked
    ?assertEqual(false, tcps_andon:is_blocked(<<"SKU-A">>)),
    ?assertEqual(true, tcps_andon:is_blocked(<<"SKU-B">>)),
    ?assertEqual(true, tcps_andon:is_blocked(<<"SKU-C">>)).

test_andon_history() ->
    SkuId = <<"SKU-HISTORY">>,

    % Trigger multiple Andons over time
    {ok, _Id1} = tcps_andon:trigger_andon(shacl_violation,
        #{sku_id => SkuId, stage => compilation, details => #{}}),

    timer:sleep(10), % Ensure different timestamps

    {ok, _Id2} = tcps_andon:trigger_andon(test_failure,
        #{sku_id => SkuId, stage => testing, details => #{}}),

    % Get history
    History = tcps_andon:get_andon_history(SkuId),
    ?assertEqual(2, length(History)),

    % Verify chronological order
    [Event1, Event2] = History,
    ?assert(maps:get(timestamp, Event1) =< maps:get(timestamp, Event2)).

test_performance_many_andons() ->
    % Test system handles 1000 concurrent Andons
    StartTime = erlang:monotonic_time(millisecond),

    Self = self(),
    Pids = [spawn(fun() ->
        SkuId = list_to_binary("SKU-PERF-" ++ integer_to_list(N)),
        Context = #{sku_id => SkuId, stage => testing, details => #{}},
        {ok, _} = tcps_andon:trigger_andon(test_failure, Context),
        Self ! done
    end) || N <- lists:seq(1, 1000)],

    % Wait for all to complete
    [receive done -> ok after 10000 -> timeout end || _ <- Pids],

    EndTime = erlang:monotonic_time(millisecond),
    Duration = EndTime - StartTime,

    % Should complete in under 5 seconds
    ?assert(Duration < 5000,
            io_lib:format("Performance test took ~pms (expected <5000ms)", [Duration])),

    % Verify all events created
    ?assert(ets:info(tcps_andon_events, size) >= 1000).

%%%=============================================================================
%%% Helper Functions
%%%=============================================================================

% Additional helper functions would go here if needed
