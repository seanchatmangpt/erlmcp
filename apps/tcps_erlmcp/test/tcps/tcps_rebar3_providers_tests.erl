%%%-----------------------------------------------------------------------------
%%% @doc Tests for TCPS Rebar3 Provider Integration
%%%
%%% Tests the rebar3 provider plugins:
%%% - SHACL validation provider
%%% - Receipt generation provider
%%% - Quality gates provider
%%% - Andon management provider
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(tcps_rebar3_providers_tests).

-include_lib("eunit/include/eunit.hrl").

%%%=============================================================================
%%% Test Fixtures
%%%=============================================================================

setup() ->
    %% Ensure Andon system is started
    tcps_andon:start(),
    %% Create receipts directory
    filelib:ensure_dir("priv/receipts/"),
    ok.

cleanup(_) ->
    %% Clean up test receipts
    case filelib:is_dir("priv/receipts") of
        true ->
            {ok, Files} = file:list_dir("priv/receipts"),
            [file:delete(filename:join("priv/receipts", F)) || F <- Files,
                                                                string:str(F, "test_") > 0];
        false ->
            ok
    end,
    ok.

%%%=============================================================================
%%% SHACL Validation Provider Tests
%%%=============================================================================

shacl_validation_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      {"SHACL validation detects missing namespaces",
       fun test_shacl_missing_namespace/0},
      {"SHACL validation passes on valid ontology",
       fun test_shacl_valid_ontology/0},
      {"SHACL validation generates receipt",
       fun test_shacl_generates_receipt/0}
     ]}.

test_shacl_missing_namespace() ->
    %% Create invalid test data file
    TestFile = "test_data_invalid.ttl",
    InvalidContent = <<"# Missing @prefix tcps:\n",
                       "@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .\n">>,

    file:write_file(TestFile, InvalidContent),

    %% Validate (should detect missing namespace)
    Violations = tcps_rebar3_shacl:perform_shacl_validation(InvalidContent, "shapes/tcps_shapes.ttl"),

    %% Cleanup
    file:delete(TestFile),

    %% Assert violations detected
    ?assertMatch([_ | _], Violations),
    ?assert(lists:any(fun(V) ->
        string:str(lists:flatten(io_lib:format("~s", [V])), "namespace") > 0
    end, Violations)).

test_shacl_valid_ontology() ->
    %% Create valid test data file
    ValidContent = <<"@prefix tcps: <http://taiea.io/ontology/tcps#> .\n",
                     "@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .\n",
                     "\n",
                     "tcps:TestWorkOrder a tcps:WorkOrder ;\n",
                     "    tcps:createdAt \"2026-01-27T00:00:00Z\"^^xsd:dateTime .\n">>,

    %% Validate
    Violations = tcps_rebar3_shacl:perform_shacl_validation(ValidContent, "shapes/tcps_shapes.ttl"),

    %% Assert no violations
    ?assertEqual([], Violations).

test_shacl_generates_receipt() ->
    %% Create test summary
    Summary = #{
        total_files => 3,
        passed_files => 2,
        failed_files => 1,
        total_violations => 2,
        critical_violations => 1,
        violations_by_file => #{
            <<"test.ttl">> => [<<"Missing namespace">>]
        }
    },

    %% Generate receipt
    Receipt = tcps_rebar3_shacl:generate_validation_receipt(Summary, undefined),

    %% Assert receipt structure
    ?assertMatch(#{receipt_type := <<"shacl_validation">>}, Receipt),
    ?assertMatch(#{stage := <<"validation">>}, Receipt),
    ?assertMatch(#{status := <<"fail">>}, Receipt),
    ?assertEqual(Summary, maps:get(summary, Receipt)).

%%%=============================================================================
%%% Receipt Generation Provider Tests
%%%=============================================================================

receipt_generation_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      {"Generate compilation receipt",
       fun test_generate_compilation_receipt/0},
      {"Generate test receipt",
       fun test_generate_test_receipt/0},
      {"Store receipt to filesystem",
       fun test_store_receipt/0},
      {"Receipt has required fields",
       fun test_receipt_required_fields/0}
     ]}.

test_generate_compilation_receipt() ->
    SkuId = <<"test_sku_compile">>,
    WorkOrderId = <<"test_wo_123">>,

    %% Generate compilation receipt (mock state)
    Receipt = tcps_rebar3_receipt:generate_compilation_receipt(SkuId, WorkOrderId, undefined),

    %% Assert receipt structure
    ?assertMatch(#{receipt_type := <<"compilation">>}, Receipt),
    ?assertMatch(#{stage := <<"compile">>}, Receipt),
    ?assertEqual(SkuId, maps:get(sku_id, Receipt)),
    ?assertEqual(WorkOrderId, maps:get(work_order_id, Receipt)),
    ?assertMatch(#{error_count := _}, maps:get(metrics, Receipt)).

test_generate_test_receipt() ->
    SkuId = <<"test_sku_test">>,
    WorkOrderId = <<"test_wo_456">>,

    %% Generate test receipt
    Receipt = tcps_rebar3_receipt:generate_test_receipt(SkuId, WorkOrderId, undefined),

    %% Assert receipt structure
    ?assertMatch(#{receipt_type := <<"test">>}, Receipt),
    ?assertMatch(#{stage := <<"test">>}, Receipt),
    ?assertMatch(#{total_tests := _}, maps:get(metrics, Receipt)),
    ?assertMatch(#{pass_rate := _}, maps:get(metrics, Receipt)),
    ?assertMatch(#{coverage := _}, maps:get(metrics, Receipt)).

test_store_receipt() ->
    %% Create test receipt
    Receipt = #{
        receipt_id => <<"test_rcpt_12345">>,
        receipt_type => <<"test">>,
        stage => <<"test_stage">>,
        timestamp => erlang:system_time(millisecond)
    },

    %% Store receipt
    ok = tcps_rebar3_receipt:store_receipt(Receipt),

    %% Verify file exists
    Filename = "priv/receipts/test_rcpt_12345.json",
    ?assert(filelib:is_regular(Filename)),

    %% Read and verify content
    {ok, JsonBin} = file:read_file(Filename),
    Decoded = jsx:decode(JsonBin, [return_maps]),
    ?assertEqual(<<"test_rcpt_12345">>, maps:get(<<"receipt_id">>, Decoded)),

    %% Cleanup
    file:delete(Filename).

test_receipt_required_fields() ->
    SkuId = <<"test_sku_fields">>,

    %% Generate receipt
    Receipt = tcps_rebar3_receipt:generate_compilation_receipt(SkuId, undefined, undefined),

    %% Assert all required fields present
    RequiredFields = [receipt_id, receipt_type, stage, sku_id, timestamp,
                     timestamp_iso, status, metrics, evidence, ontology_refs],

    lists:foreach(fun(Field) ->
        ?assert(maps:is_key(Field, Receipt),
               io_lib:format("Missing required field: ~p", [Field]))
    end, RequiredFields).

%%%=============================================================================
%%% Quality Gates Provider Tests
%%%=============================================================================

quality_gates_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      {"Compilation gate passes with zero errors",
       fun test_compilation_gate_pass/0},
      {"Test gate fails below 80% coverage",
       fun test_test_gate_fail_coverage/0},
      {"Test gate fails below 80% pass rate",
       fun test_test_gate_fail_pass_rate/0},
      {"Quality receipt generation",
       fun test_quality_receipt_generation/0}
     ]}.

test_compilation_gate_pass() ->
    Config = #{
        max_errors => 0,
        max_critical_warnings => 0
    },

    %% Check compilation gate (mock state with 0 errors)
    Result = tcps_rebar3_quality:check_compilation_gate(undefined, Config),

    %% Assert gate passed
    ?assertMatch(#{gate := compilation}, Result),
    ?assertMatch(#{passed := true}, Result),
    ?assertEqual([], maps:get(violations, Result)).

test_test_gate_fail_coverage() ->
    Config = #{
        min_pass_rate => 80.0,
        min_coverage => 80.0
    },

    %% Mock test metrics with low coverage
    meck:new(tcps_rebar3_quality, [passthrough]),
    meck:expect(tcps_rebar3_quality, get_test_metrics, fun(_) ->
        #{total_tests => 10, passed_tests => 10, coverage => 75.0}
    end),

    %% Check test gate
    Result = tcps_rebar3_quality:check_test_gate(undefined, Config),

    %% Cleanup
    meck:unload(tcps_rebar3_quality),

    %% Assert gate failed due to low coverage
    ?assertMatch(#{passed := false}, Result),
    Violations = maps:get(violations, Result),
    ?assert(lists:any(fun(V) ->
        maps:get(metric, V) =:= coverage
    end, Violations)).

test_test_gate_fail_pass_rate() ->
    Config = #{
        min_pass_rate => 80.0,
        min_coverage => 80.0
    },

    %% Mock test metrics with low pass rate
    meck:new(tcps_rebar3_quality, [passthrough]),
    meck:expect(tcps_rebar3_quality, get_test_metrics, fun(_) ->
        #{total_tests => 10, passed_tests => 7, coverage => 85.0}
    end),

    %% Check test gate
    Result = tcps_rebar3_quality:check_test_gate(undefined, Config),

    %% Cleanup
    meck:unload(tcps_rebar3_quality),

    %% Assert gate failed due to low pass rate (70%)
    ?assertMatch(#{passed := false}, Result),
    Violations = maps:get(violations, Result),
    ?assert(lists:any(fun(V) ->
        maps:get(metric, V) =:= pass_rate
    end, Violations)).

test_quality_receipt_generation() ->
    Results = [
        #{gate => compilation, passed => true, metrics => #{}, thresholds => #{}, violations => []},
        #{gate => test, passed => false, metrics => #{coverage => 75.0},
          thresholds => #{min_coverage => 80.0},
          violations => [#{metric => coverage, actual => 75.0, expected => 80.0}]}
    ],

    Config = #{sku_id => <<"test_sku_quality">>},

    %% Generate quality receipt
    Receipt = tcps_rebar3_quality:generate_quality_receipt(Results, Config),

    %% Assert receipt structure
    ?assertMatch(#{receipt_type := <<"quality_gates">>}, Receipt),
    ?assertMatch(#{status := <<"fail">>}, Receipt),
    ?assertEqual(Results, maps:get(results, Receipt)),
    ?assertEqual(Config, maps:get(configuration, Receipt)).

%%%=============================================================================
%%% Andon Provider Tests
%%%=============================================================================

andon_provider_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      {"List open Andons",
       fun test_andon_list_empty/0},
      {"Show Andon details",
       fun test_andon_show_details/0},
      {"Check build status - clear",
       fun test_andon_check_clear/0},
      {"Check build status - blocked",
       fun test_andon_check_blocked/0}
     ]}.

test_andon_list_empty() ->
    %% Reset Andon state
    tcps_andon:stop(),
    tcps_andon:start(),

    %% List Andons (should be empty)
    AllEvents = ets:tab2list(tcps_andon_events),
    OpenEvents = [{Id, Event} || {Id, Event} <- AllEvents,
                                 is_map(Event),
                                 maps:get(status, Event, undefined) =:= open],

    ?assertEqual([], OpenEvents).

test_andon_show_details() ->
    %% Create test Andon
    Context = #{
        sku_id => <<"test_sku_andon">>,
        stage => testing,
        details => #{error => "Test failure"}
    },

    {ok, AndonId} = tcps_andon:trigger_andon(test_failure, Context),

    %% Get Andon details
    Event = tcps_andon:get_andon_event(AndonId),

    %% Assert event structure
    ?assertEqual(AndonId, maps:get(event_id, Event)),
    ?assertEqual(test_failure, maps:get(failure_type, Event)),
    ?assertEqual(open, maps:get(status, Event)),

    %% Cleanup
    tcps_andon:resolve_andon(AndonId, #{
        root_cause => <<"Test">>,
        fix_applied => <<"Test">>,
        prevention_added => <<"Test">>
    }).

test_andon_check_clear() ->
    %% Reset Andon state
    tcps_andon:stop(),
    tcps_andon:start(),

    %% Check if build is blocked (should be clear)
    AllEvents = ets:tab2list(tcps_andon_events),
    OpenEvents = [{Id, Event} || {Id, Event} <- AllEvents,
                                 is_map(Event),
                                 maps:get(status, Event, undefined) =:= open],

    ?assertEqual([], OpenEvents).

test_andon_check_blocked() ->
    %% Create blocking Andon
    Context = #{
        sku_id => <<"test_sku_blocked">>,
        stage => compilation,
        details => #{error => "Compilation error"}
    },

    {ok, AndonId} = tcps_andon:trigger_andon(compilation_failure, Context),

    %% Check if blocked
    ?assert(tcps_andon:is_blocked(<<"test_sku_blocked">>)),

    %% Resolve to unblock
    ok = tcps_andon:resolve_andon(AndonId, #{
        root_cause => <<"Test">>,
        fix_applied => <<"Test">>,
        prevention_added => <<"Test">>
    }),

    %% Should no longer be blocked
    ?assertNot(tcps_andon:is_blocked(<<"test_sku_blocked">>)).

%%%=============================================================================
%%% Integration Tests
%%%=============================================================================

integration_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      {"Full workflow: SHACL → Compile → Test → Quality Gates",
       fun test_full_workflow/0}
     ]}.

test_full_workflow() ->
    SkuId = <<"integration_test_sku">>,

    %% Stage 1: SHACL validation
    ValidContent = <<"@prefix tcps: <http://taiea.io/ontology/tcps#> .\n">>,
    ShaclViolations = tcps_rebar3_shacl:perform_shacl_validation(
        ValidContent, "shapes/tcps_shapes.ttl"),

    %% Stage 2: Compilation receipt
    CompileReceipt = tcps_rebar3_receipt:generate_compilation_receipt(
        SkuId, undefined, undefined),
    ok = tcps_rebar3_receipt:store_receipt(CompileReceipt),

    %% Stage 3: Test receipt
    TestReceipt = tcps_rebar3_receipt:generate_test_receipt(
        SkuId, undefined, undefined),
    ok = tcps_rebar3_receipt:store_receipt(TestReceipt),

    %% Stage 4: Quality gates
    Config = #{
        min_pass_rate => 80.0,
        min_coverage => 80.0,
        sku_id => SkuId
    },
    CompileGate = tcps_rebar3_quality:check_compilation_gate(undefined, Config),
    TestGate = tcps_rebar3_quality:check_test_gate(undefined, Config),

    %% Assert workflow completed
    ?assertEqual([], ShaclViolations),
    ?assertMatch(#{status := <<"pass">>}, CompileReceipt),
    ?assertMatch(#{status := _}, TestReceipt),
    ?assertMatch(#{passed := true}, CompileGate),
    ?assertMatch(#{passed := _}, TestGate),

    %% Cleanup
    file:delete("priv/receipts/" ++ binary_to_list(maps:get(receipt_id, CompileReceipt)) ++ ".json"),
    file:delete("priv/receipts/" ++ binary_to_list(maps:get(receipt_id, TestReceipt)) ++ ".json").
