%%%-------------------------------------------------------------------
%%% @doc TCPS Root Cause Analysis Tests
%%%
%%% Comprehensive test suite for the 5 Whys framework following
%%% Lean Six Sigma quality standards and Chicago School TDD.
%%%
%%% Test Coverage:
%%% - Basic analysis workflow
%%% - Prevention action generation
%%% - Receipt generation
%%% - Error handling and validation
%%% - Integration with Andon events
%%% - Real-world failure scenarios
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(tcps_root_cause_tests).

-include_lib("eunit/include/eunit.hrl").
-include("tcps_root_cause.hrl").

%%%===================================================================
%%% Test Fixtures
%%%===================================================================

start_server() ->
    {ok, Pid} = tcps_root_cause:start_link(),
    Pid.

stop_server(Pid) ->
    tcps_root_cause:stop(),
    case is_process_alive(Pid) of
        true -> exit(Pid, kill);
        false -> ok
    end.

root_cause_test_() ->
    {foreach,
     fun start_server/0,
     fun stop_server/1,
     [
         fun test_start_analysis/1,
         fun test_add_whys_sequential/1,
         fun test_finalize_analysis/1,
         fun test_finalize_incomplete_analysis/1,
         fun test_multiple_analyses/1,
         fun test_get_analyses_by_andon/1,
         fun test_test_failure_scenario/1,
         fun test_compilation_error_scenario/1,
         fun test_non_determinism_scenario/1,
         fun test_prevention_shacl_detection/1,
         fun test_prevention_test_detection/1,
         fun test_prevention_template_detection/1,
         fun test_prevention_dependency_detection/1,
         fun test_receipt_generation/1
     ]}.

%%%===================================================================
%%% Basic Workflow Tests
%%%===================================================================

test_start_analysis(_Pid) ->
    [
        ?_test(begin
            AndonId = <<"andon_001">>,
            Problem = <<"Unit test test_foo_bar failed unexpectedly">>,

            {ok, AnalysisId} = tcps_root_cause:start_analysis(AndonId, Problem),

            ?assertMatch(<<_/binary>>, AnalysisId),

            {ok, Analysis} = tcps_root_cause:get_analysis(AnalysisId),
            ?assertEqual(AndonId, Analysis#five_whys.andon_event_id),
            ?assertEqual(Problem, Analysis#five_whys.problem),
            ?assertEqual(pending, Analysis#five_whys.status)
        end)
    ].

test_add_whys_sequential(_Pid) ->
    [
        ?_test(begin
            AndonId = <<"andon_002">>,
            Problem = <<"Test timeout after 5000ms">>,

            {ok, AnalysisId} = tcps_root_cause:start_analysis(AndonId, Problem),

            ok = tcps_root_cause:add_why(AnalysisId, 1, <<"Test exceeded timeout limit">>),
            ok = tcps_root_cause:add_why(AnalysisId, 2, <<"Database query took too long">>),
            ok = tcps_root_cause:add_why(AnalysisId, 3, <<"Missing index on user_id column">>),
            ok = tcps_root_cause:add_why(AnalysisId, 4, <<"Schema migration didn't include indexes">>),
            ok = tcps_root_cause:add_why(AnalysisId, 5, <<"No index validation in migration process">>),

            {ok, Analysis} = tcps_root_cause:get_analysis(AnalysisId),
            ?assertEqual(<<"Test exceeded timeout limit">>, Analysis#five_whys.why_1),
            ?assertEqual(<<"Database query took too long">>, Analysis#five_whys.why_2),
            ?assertEqual(<<"Missing index on user_id column">>, Analysis#five_whys.why_3),
            ?assertEqual(<<"Schema migration didn't include indexes">>, Analysis#five_whys.why_4),
            ?assertEqual(<<"No index validation in migration process">>, Analysis#five_whys.why_5),
            ?assertEqual(in_progress, Analysis#five_whys.status)
        end)
    ].

test_finalize_analysis(_Pid) ->
    [
        ?_test(begin
            AndonId = <<"andon_003">>,
            Problem = <<"Compilation failed: undefined function">>,

            {ok, AnalysisId} = tcps_root_cause:start_analysis(AndonId, Problem),

            ok = tcps_root_cause:add_why(AnalysisId, 1, <<"Function foo/2 not exported">>),
            ok = tcps_root_cause:add_why(AnalysisId, 2, <<"Template didn't generate export">>),
            ok = tcps_root_cause:add_why(AnalysisId, 3, <<"SPARQL query missed function definition">>),
            ok = tcps_root_cause:add_why(AnalysisId, 4, <<"Ontology missing tcps:publicFunction annotation">>),
            ok = tcps_root_cause:add_why(AnalysisId, 5, <<"No SHACL validation for function visibility">>),

            RootCause = <<"Missing SHACL validation for function export requirements">>,
            Prevention = <<"Add SHACL shape requiring tcps:publicFunction for all API functions">>,

            {ok, Result} = tcps_root_cause:finalize_analysis(AnalysisId, RootCause, Prevention),

            ?assertMatch(#{receipt := _, prevention_delta := _, analysis := _}, Result),

            Receipt = maps:get(receipt, Result),
            ?assertEqual(<<"root_cause_analysis">>, maps:get(receipt_type, Receipt)),
            ?assertEqual(RootCause, maps:get(root_cause, Receipt)),
            ?assertEqual(Prevention, maps:get(prevention_action, Receipt)),

            Delta = maps:get(prevention_delta, Result),
            ?assertMatch(#{shacl_additions := [_|_]}, Delta),

            {ok, FinalAnalysis} = tcps_root_cause:get_analysis(AnalysisId),
            ?assertEqual(finalized, FinalAnalysis#five_whys.status)
        end)
    ].

test_finalize_incomplete_analysis(_Pid) ->
    [
        ?_test(begin
            AndonId = <<"andon_004">>,
            Problem = <<"Test failed">>,

            {ok, AnalysisId} = tcps_root_cause:start_analysis(AndonId, Problem),

            %% Only add 3 whys
            ok = tcps_root_cause:add_why(AnalysisId, 1, <<"Why 1">>),
            ok = tcps_root_cause:add_why(AnalysisId, 2, <<"Why 2">>),
            ok = tcps_root_cause:add_why(AnalysisId, 3, <<"Why 3">>),

            %% Should fail to finalize
            Result = tcps_root_cause:finalize_analysis(
                AnalysisId,
                <<"Root cause">>,
                <<"Prevention">>
            ),

            ?assertMatch({error, missing_why_4}, Result)
        end)
    ].

test_multiple_analyses(_Pid) ->
    [
        ?_test(begin
            AndonId1 = <<"andon_005">>,
            AndonId2 = <<"andon_006">>,

            {ok, AnalysisId1} = tcps_root_cause:start_analysis(AndonId1, <<"Problem 1">>),
            {ok, AnalysisId2} = tcps_root_cause:start_analysis(AndonId2, <<"Problem 2">>),
            {ok, AnalysisId3} = tcps_root_cause:start_analysis(AndonId1, <<"Problem 3">>),

            ?assertNotEqual(AnalysisId1, AnalysisId2),
            ?assertNotEqual(AnalysisId1, AnalysisId3),
            ?assertNotEqual(AnalysisId2, AnalysisId3),

            Analyses = tcps_root_cause:list_analyses(),
            ?assertEqual(3, length(Analyses))
        end)
    ].

test_get_analyses_by_andon(_Pid) ->
    [
        ?_test(begin
            AndonId = <<"andon_007">>,

            {ok, _Id1} = tcps_root_cause:start_analysis(AndonId, <<"First occurrence">>),
            {ok, _Id2} = tcps_root_cause:start_analysis(AndonId, <<"Second occurrence">>),
            {ok, _Id3} = tcps_root_cause:start_analysis(<<"other_andon">>, <<"Different">>),

            Analyses = tcps_root_cause:get_analyses_by_andon(AndonId),
            ?assertEqual(2, length(Analyses)),

            lists:foreach(
                fun(A) ->
                    ?assertEqual(AndonId, A#five_whys.andon_event_id)
                end,
                Analyses
            )
        end)
    ].

%%%===================================================================
%%% Real-World Scenario Tests
%%%===================================================================

test_test_failure_scenario(_Pid) ->
    [
        ?_test(begin
            AndonId = <<"andon_test_failure_001">>,
            Problem = <<"test_concurrent_access fails intermittently">>,

            {ok, AnalysisId} = tcps_root_cause:start_analysis(AndonId, Problem),

            ok = tcps_root_cause:add_why(AnalysisId, 1,
                <<"Test shows race condition in data access">>),
            ok = tcps_root_cause:add_why(AnalysisId, 2,
                <<"Two processes modify shared ETS table simultaneously">>),
            ok = tcps_root_cause:add_why(AnalysisId, 3,
                <<"No synchronization mechanism for concurrent writes">>),
            ok = tcps_root_cause:add_why(AnalysisId, 4,
                <<"Template assumes single-process access pattern">>),
            ok = tcps_root_cause:add_why(AnalysisId, 5,
                <<"Ontology doesn't specify concurrency requirements">>),

            RootCause = <<"Missing concurrency specification in ontology causing race condition">>,
            Prevention = <<"Add tcps:concurrencyMode property to ontology and generate appropriate synchronization">>,

            {ok, Result} = tcps_root_cause:finalize_analysis(AnalysisId, RootCause, Prevention),

            Delta = maps:get(prevention_delta, Result),
            ShaclAdditions = maps:get(shacl_additions, Delta),
            TestAdditions = maps:get(test_additions, Delta),

            %% Should detect race condition and suggest tests
            ?assert(length(TestAdditions) > 0),

            %% Verify receipt links to Andon event
            Receipt = maps:get(receipt, Result),
            AndonRef = maps:get(andon_event_ref, Receipt),
            ?assertEqual(
                iolist_to_binary([<<"tcps:AndonEvent/">>, AndonId]),
                AndonRef
            )
        end)
    ].

test_compilation_error_scenario(_Pid) ->
    [
        ?_test(begin
            AndonId = <<"andon_compile_error_001">>,
            Problem = <<"Compilation error: undefined record info">>,

            {ok, AnalysisId} = tcps_root_cause:start_analysis(AndonId, Problem),

            ok = tcps_root_cause:add_why(AnalysisId, 1,
                <<"Record definition not included in generated module">>),
            ok = tcps_root_cause:add_why(AnalysisId, 2,
                <<"Template filter didn't extract record from ontology">>),
            ok = tcps_root_cause:add_why(AnalysisId, 3,
                <<"SPARQL query for records was incorrect">>),
            ok = tcps_root_cause:add_why(AnalysisId, 4,
                <<"Record ontology structure changed without updating query">>),
            ok = tcps_root_cause:add_why(AnalysisId, 5,
                <<"No SHACL validation ensuring record structure consistency">>),

            RootCause = <<"Missing SHACL validation for record structure consistency">>,
            Prevention = <<"Add SHACL shape enforcing tcps:recordField constraints">>,

            {ok, Result} = tcps_root_cause:finalize_analysis(AnalysisId, RootCause, Prevention),

            Delta = maps:get(prevention_delta, Result),
            ShaclAdditions = maps:get(shacl_additions, Delta),

            %% Should detect validation need
            ?assert(length(ShaclAdditions) > 0),

            %% Should suggest format validation for record structure
            HasValidationSuggestion = lists:any(
                fun(S) -> binary:match(S, <<"validation">>) =/= nomatch end,
                ShaclAdditions
            ),
            ?assert(HasValidationSuggestion)
        end)
    ].

test_non_determinism_scenario(_Pid) ->
    [
        ?_test(begin
            AndonId = <<"andon_nondeterminism_001">>,
            Problem = <<"Generated code differs between runs">>,

            {ok, AnalysisId} = tcps_root_cause:start_analysis(AndonId, Problem),

            ok = tcps_root_cause:add_why(AnalysisId, 1,
                <<"Dependency cowboy version changed between builds">>),
            ok = tcps_root_cause:add_why(AnalysisId, 2,
                <<"rebar.lock not checked into repository">>),
            ok = tcps_root_cause:add_why(AnalysisId, 3,
                <<"Template generation script didn't verify lock file">>),
            ok = tcps_root_cause:add_why(AnalysisId, 4,
                <<"No policy requiring dependency pinning">>),
            ok = tcps_root_cause:add_why(AnalysisId, 5,
                <<"Ontology doesn't model dependency version constraints">>),

            RootCause = <<"Dependency cowboy version 2.10.0 not pinned in ontology">>,
            Prevention = <<"Pin all dependencies in ontology with exact versions">>,

            {ok, Result} = tcps_root_cause:finalize_analysis(AnalysisId, RootCause, Prevention),

            Delta = maps:get(prevention_delta, Result),
            DependencyPins = maps:get(dependency_pins, Delta),

            %% Should detect and extract dependency version
            ?assertMatch(#{<<"cowboy">> := <<"2.10.0">>}, DependencyPins)
        end)
    ].

%%%===================================================================
%%% Prevention Action Generation Tests
%%%===================================================================

test_prevention_shacl_detection(_Pid) ->
    [
        ?_test(begin
            RootCause1 = <<"Missing validation for required properties">>,
            Delta1 = tcps_root_cause:generate_prevention_actions(RootCause1),
            ?assert(length(Delta1#prevention_delta.shacl_additions) > 0)
        end),

        ?_test(begin
            RootCause2 = <<"Invalid data type in field">>,
            Delta2 = tcps_root_cause:generate_prevention_actions(RootCause2),
            ShaclAdditions = Delta2#prevention_delta.shacl_additions,
            ?assert(length(ShaclAdditions) > 0),
            HasDatatypeConstraint = lists:any(
                fun(S) -> binary:match(S, <<"datatype">>) =/= nomatch end,
                ShaclAdditions
            ),
            ?assert(HasDatatypeConstraint)
        end),

        ?_test(begin
            RootCause3 = <<"Cardinality violation: multiple values where one expected">>,
            Delta3 = tcps_root_cause:generate_prevention_actions(RootCause3),
            ShaclAdditions = Delta3#prevention_delta.shacl_additions,
            HasCardinalityConstraint = lists:any(
                fun(S) -> binary:match(S, <<"Count">>) =/= nomatch end,
                ShaclAdditions
            ),
            ?assert(HasCardinalityConstraint)
        end)
    ].

test_prevention_test_detection(_Pid) ->
    [
        ?_test(begin
            RootCause1 = <<"Race condition in concurrent access">>,
            Delta1 = tcps_root_cause:generate_prevention_actions(RootCause1),
            TestAdditions = Delta1#prevention_delta.test_additions,
            ?assert(length(TestAdditions) > 0),
            HasConcurrencyTest = lists:any(
                fun(T) -> binary:match(T, <<"concurrency">>) =/= nomatch end,
                TestAdditions
            ),
            ?assert(HasConcurrencyTest)
        end),

        ?_test(begin
            RootCause2 = <<"Edge case with empty list not handled">>,
            Delta2 = tcps_root_cause:generate_prevention_actions(RootCause2),
            TestAdditions = Delta2#prevention_delta.test_additions,
            HasBoundaryTest = lists:any(
                fun(T) -> binary:match(T, <<"boundary">>) =/= nomatch end,
                TestAdditions
            ),
            ?assert(HasBoundaryTest)
        end),

        ?_test(begin
            RootCause3 = <<"Error handling missing for timeout scenario">>,
            Delta3 = tcps_root_cause:generate_prevention_actions(RootCause3),
            TestAdditions = Delta3#prevention_delta.test_additions,
            ?assert(length(TestAdditions) > 0)
        end)
    ].

test_prevention_template_detection(_Pid) ->
    [
        ?_test(begin
            RootCause1 = <<"Duplicated code across multiple modules">>,
            Delta1 = tcps_root_cause:generate_prevention_actions(RootCause1),
            TemplateImprovements = Delta1#prevention_delta.template_improvements,
            ?assert(length(TemplateImprovements) > 0),
            HasReusableComponent = lists:any(
                fun(T) -> binary:match(T, <<"reusable">>) =/= nomatch end,
                TemplateImprovements
            ),
            ?assert(HasReusableComponent)
        end),

        ?_test(begin
            RootCause2 = <<"Hard-coded configuration values in template">>,
            Delta2 = tcps_root_cause:generate_prevention_actions(RootCause2),
            TemplateImprovements = Delta2#prevention_delta.template_improvements,
            HasExtraction = lists:any(
                fun(T) -> binary:match(T, <<"Extract">>) =/= nomatch end,
                TemplateImprovements
            ),
            ?assert(HasExtraction)
        end)
    ].

test_prevention_dependency_detection(_Pid) ->
    [
        ?_test(begin
            RootCause = <<"Dependency cowboy version 2.10.0 caused compilation failure">>,
            Delta = tcps_root_cause:generate_prevention_actions(RootCause),
            DependencyPins = Delta#prevention_delta.dependency_pins,
            ?assertMatch(#{<<"cowboy">> := <<"2.10.0">>}, DependencyPins)
        end),

        ?_test(begin
            RootCause = <<"Dependency ranch version 1.8.0 broke API compatibility">>,
            Delta = tcps_root_cause:generate_prevention_actions(RootCause),
            DependencyPins = Delta#prevention_delta.dependency_pins,
            ?assertMatch(#{<<"ranch">> := <<"1.8.0">>}, DependencyPins)
        end)
    ].

%%%===================================================================
%%% Receipt Generation Tests
%%%===================================================================

test_receipt_generation(_Pid) ->
    [
        ?_test(begin
            AndonId = <<"andon_receipt_test">>,
            Problem = <<"Test receipt generation">>,

            {ok, AnalysisId} = tcps_root_cause:start_analysis(AndonId, Problem),

            ok = tcps_root_cause:add_why(AnalysisId, 1, <<"Why 1">>),
            ok = tcps_root_cause:add_why(AnalysisId, 2, <<"Why 2">>),
            ok = tcps_root_cause:add_why(AnalysisId, 3, <<"Why 3">>),
            ok = tcps_root_cause:add_why(AnalysisId, 4, <<"Why 4">>),
            ok = tcps_root_cause:add_why(AnalysisId, 5, <<"Why 5">>),

            RootCause = <<"Test root cause">>,
            Prevention = <<"Test prevention">>,

            {ok, Result} = tcps_root_cause:finalize_analysis(AnalysisId, RootCause, Prevention),

            Receipt = maps:get(receipt, Result),

            %% Verify receipt structure
            ?assertEqual(<<"root_cause_analysis">>, maps:get(receipt_type, Receipt)),
            ?assertEqual(<<"1.0.0">>, maps:get(receipt_version, Receipt)),
            ?assertEqual(AnalysisId, maps:get(analysis_id, Receipt)),
            ?assertEqual(AndonId, maps:get(andon_event_id, Receipt)),

            %% Verify 5 Whys chain
            FiveWhysChain = maps:get(five_whys_chain, Receipt),
            ?assertMatch(#{why_1 := _, why_2 := _, why_3 := _, why_4 := _, why_5 := _}, FiveWhysChain),

            %% Verify timeline
            Timeline = maps:get(timeline, Receipt),
            ?assertMatch(#{created_at := _, updated_at := _, finalized_at := _, duration_ms := _}, Timeline),

            %% Verify ontology references
            ?assertEqual(<<"tcps:RootCauseAnalysis">>, maps:get(ontology_ref, Receipt)),
            ?assertEqual(
                iolist_to_binary([<<"tcps:AndonEvent/">>, AndonId]),
                maps:get(andon_event_ref, Receipt)
            ),

            %% Verify prevention delta included
            ?assertMatch(#{prevention_delta := _}, Receipt)
        end)
    ].

%%%===================================================================
%%% Record Definitions
%%%===================================================================

%% Records are now defined in include/tcps_root_cause.hrl
