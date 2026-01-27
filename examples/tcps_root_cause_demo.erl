#!/usr/bin/env escript
%%! -pa _build/default/lib/erlmcp/ebin

%%%-------------------------------------------------------------------
%%% @doc TCPS Root Cause Analysis Demo
%%%
%%% Demonstrates the complete 5 Whys root cause analysis workflow
%%% for test failures, compilation errors, and non-determinism.
%%%
%%% Usage:
%%%   ./examples/tcps_root_cause_demo.erl
%%%
%%% @end
%%%-------------------------------------------------------------------

-mode(compile).

%% Include record definitions inline since escript has include path issues
-record(five_whys, {
    analysis_id :: binary(),
    andon_event_id :: binary(),
    problem :: binary(),
    why_1 :: binary() | undefined,
    why_2 :: binary() | undefined,
    why_3 :: binary() | undefined,
    why_4 :: binary() | undefined,
    why_5 :: binary() | undefined,
    root_cause :: binary() | undefined,
    prevention_action :: binary() | undefined,
    created_at :: integer(),
    updated_at :: integer(),
    finalized_at :: integer() | undefined,
    status :: pending | in_progress | finalized
}).

main(_Args) ->
    io:format("~n"),
    io:format("=======================================================================~n"),
    io:format("TCPS 5 Whys Root Cause Analysis Framework Demo~n"),
    io:format("Toyota Production System Applied to Software Manufacturing~n"),
    io:format("=======================================================================~n~n"),

    %% Start the root cause analysis server
    {ok, _Pid} = tcps_root_cause:start_link(),
    io:format("[✓] Root cause analysis server started~n~n"),

    %% Example 1: Test Failure Analysis
    demo_test_failure(),

    %% Example 2: Compilation Error Analysis
    demo_compilation_error(),

    %% Example 3: Non-Determinism Analysis
    demo_non_determinism(),

    %% Example 4: List All Analyses
    demo_list_analyses(),

    %% Example 5: Query by Andon Event
    demo_query_by_andon(),

    %% Clean up
    tcps_root_cause:stop(),
    io:format("~n[✓] Demo complete!~n~n"),

    halt(0).

%%%===================================================================
%%% Example 1: Test Failure → Root Cause → Prevention
%%%===================================================================

demo_test_failure() ->
    io:format("───────────────────────────────────────────────────────────────────────~n"),
    io:format("Example 1: Test Failure Root Cause Analysis~n"),
    io:format("───────────────────────────────────────────────────────────────────────~n~n"),

    AndonId = <<"andon_test_failure_001">>,
    Problem = <<"Unit test test_concurrent_access fails intermittently">>,

    io:format("Problem: ~s~n", [Problem]),
    io:format("Andon Event: ~s~n~n", [AndonId]),

    {ok, AnalysisId} = tcps_root_cause:start_analysis(AndonId, Problem),
    io:format("[✓] Started analysis: ~s~n~n", [AnalysisId]),

    io:format("5 Whys Analysis:~n"),

    ok = tcps_root_cause:add_why(AnalysisId, 1,
        <<"Test shows race condition in data access">>),
    io:format("  Why 1: Test shows race condition in data access~n"),

    ok = tcps_root_cause:add_why(AnalysisId, 2,
        <<"Two processes modify shared ETS table simultaneously">>),
    io:format("  Why 2: Two processes modify shared ETS table simultaneously~n"),

    ok = tcps_root_cause:add_why(AnalysisId, 3,
        <<"No synchronization mechanism for concurrent writes">>),
    io:format("  Why 3: No synchronization mechanism for concurrent writes~n"),

    ok = tcps_root_cause:add_why(AnalysisId, 4,
        <<"Template assumes single-process access pattern">>),
    io:format("  Why 4: Template assumes single-process access pattern~n"),

    ok = tcps_root_cause:add_why(AnalysisId, 5,
        <<"Ontology doesn't specify concurrency requirements">>),
    io:format("  Why 5: Ontology doesn't specify concurrency requirements~n~n"),

    RootCause = <<"Missing concurrency specification in ontology causing race condition">>,
    Prevention = <<"Add tcps:concurrencyMode property to ontology and generate appropriate synchronization">>,

    {ok, Result} = tcps_root_cause:finalize_analysis(AnalysisId, RootCause, Prevention),

    io:format("[✓] Analysis finalized~n~n"),
    io:format("Root Cause:~n  ~s~n~n", [RootCause]),
    io:format("Prevention Action:~n  ~s~n~n", [Prevention]),

    Delta = maps:get(prevention_delta, Result),
    io:format("Automated Prevention Delta:~n"),
    print_prevention_delta(Delta),

    Receipt = maps:get(receipt, Result),
    io:format("~nReceipt Generated:~n"),
    io:format("  Type: ~s~n", [maps:get(receipt_type, Receipt)]),
    io:format("  Version: ~s~n", [maps:get(receipt_version, Receipt)]),
    io:format("  Analysis ID: ~s~n", [maps:get(analysis_id, Receipt)]),
    io:format("  Ontology Ref: ~s~n~n", [maps:get(ontology_ref, Receipt)]),

    ok.

%%%===================================================================
%%% Example 2: Compilation Error → Root Cause → Template Improvement
%%%===================================================================

demo_compilation_error() ->
    io:format("───────────────────────────────────────────────────────────────────────~n"),
    io:format("Example 2: Compilation Error Root Cause Analysis~n"),
    io:format("───────────────────────────────────────────────────────────────────────~n~n"),

    AndonId = <<"andon_compile_error_001">>,
    Problem = <<"Compilation error: undefined record info">>,

    io:format("Problem: ~s~n", [Problem]),
    io:format("Andon Event: ~s~n~n", [AndonId]),

    {ok, AnalysisId} = tcps_root_cause:start_analysis(AndonId, Problem),

    io:format("5 Whys Analysis:~n"),
    ok = tcps_root_cause:add_why(AnalysisId, 1,
        <<"Record definition not included in generated module">>),
    io:format("  Why 1: Record definition not included in generated module~n"),

    ok = tcps_root_cause:add_why(AnalysisId, 2,
        <<"Template filter didn't extract record from ontology">>),
    io:format("  Why 2: Template filter didn't extract record from ontology~n"),

    ok = tcps_root_cause:add_why(AnalysisId, 3,
        <<"SPARQL query for records was incorrect">>),
    io:format("  Why 3: SPARQL query for records was incorrect~n"),

    ok = tcps_root_cause:add_why(AnalysisId, 4,
        <<"Record ontology structure changed without updating query">>),
    io:format("  Why 4: Record ontology structure changed without updating query~n"),

    ok = tcps_root_cause:add_why(AnalysisId, 5,
        <<"No SHACL validation ensuring record structure consistency">>),
    io:format("  Why 5: No SHACL validation ensuring record structure consistency~n~n"),

    RootCause = <<"Missing SHACL validation for record structure consistency">>,
    Prevention = <<"Add SHACL shape enforcing tcps:recordField constraints">>,

    {ok, Result} = tcps_root_cause:finalize_analysis(AnalysisId, RootCause, Prevention),

    io:format("[✓] Analysis finalized~n~n"),
    io:format("Root Cause:~n  ~s~n~n", [RootCause]),
    io:format("Prevention Action:~n  ~s~n~n", [Prevention]),

    Delta = maps:get(prevention_delta, Result),
    io:format("Automated Prevention Delta:~n"),
    print_prevention_delta(Delta),

    io:format("~n"),
    ok.

%%%===================================================================
%%% Example 3: Non-Determinism → Root Cause → Dependency Pinning
%%%===================================================================

demo_non_determinism() ->
    io:format("───────────────────────────────────────────────────────────────────────~n"),
    io:format("Example 3: Non-Determinism Root Cause Analysis~n"),
    io:format("───────────────────────────────────────────────────────────────────────~n~n"),

    AndonId = <<"andon_nondeterminism_001">>,
    Problem = <<"Generated code differs between runs">>,

    io:format("Problem: ~s~n", [Problem]),
    io:format("Andon Event: ~s~n~n", [AndonId]),

    {ok, AnalysisId} = tcps_root_cause:start_analysis(AndonId, Problem),

    io:format("5 Whys Analysis:~n"),
    ok = tcps_root_cause:add_why(AnalysisId, 1,
        <<"Dependency cowboy version changed between builds">>),
    io:format("  Why 1: Dependency cowboy version changed between builds~n"),

    ok = tcps_root_cause:add_why(AnalysisId, 2,
        <<"rebar.lock not checked into repository">>),
    io:format("  Why 2: rebar.lock not checked into repository~n"),

    ok = tcps_root_cause:add_why(AnalysisId, 3,
        <<"Template generation script didn't verify lock file">>),
    io:format("  Why 3: Template generation script didn't verify lock file~n"),

    ok = tcps_root_cause:add_why(AnalysisId, 4,
        <<"No policy requiring dependency pinning">>),
    io:format("  Why 4: No policy requiring dependency pinning~n"),

    ok = tcps_root_cause:add_why(AnalysisId, 5,
        <<"Ontology doesn't model dependency version constraints">>),
    io:format("  Why 5: Ontology doesn't model dependency version constraints~n~n"),

    RootCause = <<"Dependency cowboy version 2.10.0 not pinned in ontology">>,
    Prevention = <<"Pin all dependencies in ontology with exact versions">>,

    {ok, Result} = tcps_root_cause:finalize_analysis(AnalysisId, RootCause, Prevention),

    io:format("[✓] Analysis finalized~n~n"),
    io:format("Root Cause:~n  ~s~n~n", [RootCause]),
    io:format("Prevention Action:~n  ~s~n~n", [Prevention]),

    Delta = maps:get(prevention_delta, Result),
    io:format("Automated Prevention Delta:~n"),
    print_prevention_delta(Delta),

    io:format("~n"),
    ok.

%%%===================================================================
%%% Example 4: List All Analyses
%%%===================================================================

demo_list_analyses() ->
    io:format("───────────────────────────────────────────────────────────────────────~n"),
    io:format("Example 4: List All Analyses~n"),
    io:format("───────────────────────────────────────────────────────────────────────~n~n"),

    Analyses = tcps_root_cause:list_analyses(),
    io:format("Total analyses: ~p~n~n", [length(Analyses)]),

    lists:foreach(fun(Analysis) ->
        io:format("Analysis: ~s~n", [Analysis#five_whys.analysis_id]),
        io:format("  Andon Event: ~s~n", [Analysis#five_whys.andon_event_id]),
        io:format("  Problem: ~s~n", [Analysis#five_whys.problem]),
        io:format("  Status: ~p~n", [Analysis#five_whys.status]),
        io:format("  Root Cause: ~s~n~n", [Analysis#five_whys.root_cause])
    end, Analyses),

    ok.

%%%===================================================================
%%% Example 5: Query by Andon Event
%%%===================================================================

demo_query_by_andon() ->
    io:format("───────────────────────────────────────────────────────────────────────~n"),
    io:format("Example 5: Query Analyses by Andon Event~n"),
    io:format("───────────────────────────────────────────────────────────────────────~n~n"),

    %% Create multiple analyses for same Andon event
    AndonId = <<"andon_recurring_001">>,

    {ok, _Id1} = tcps_root_cause:start_analysis(AndonId, <<"First occurrence">>),
    {ok, _Id2} = tcps_root_cause:start_analysis(AndonId, <<"Second occurrence">>),
    {ok, _Id3} = tcps_root_cause:start_analysis(AndonId, <<"Third occurrence">>),

    io:format("Created 3 analyses for Andon event: ~s~n~n", [AndonId]),

    Analyses = tcps_root_cause:get_analyses_by_andon(AndonId),
    io:format("Found ~p analyses for this event:~n~n", [length(Analyses)]),

    lists:foreach(fun(Analysis) ->
        io:format("  - ~s: ~s~n", [
            Analysis#five_whys.analysis_id,
            Analysis#five_whys.problem
        ])
    end, Analyses),

    io:format("~n"),
    ok.

%%%===================================================================
%%% Helper Functions
%%%===================================================================

print_prevention_delta(Delta) ->
    ShaclAdditions = maps:get(shacl_additions, Delta),
    TestAdditions = maps:get(test_additions, Delta),
    TemplateImprovements = maps:get(template_improvements, Delta),
    DependencyPins = maps:get(dependency_pins, Delta),

    io:format("  SHACL Shape Additions (~p):~n", [length(ShaclAdditions)]),
    lists:foreach(fun(Shape) ->
        io:format("    • ~s~n", [Shape])
    end, ShaclAdditions),

    io:format("~n  Test Case Additions (~p):~n", [length(TestAdditions)]),
    lists:foreach(fun(Test) ->
        io:format("    • ~s~n", [Test])
    end, TestAdditions),

    io:format("~n  Template Improvements (~p):~n", [length(TemplateImprovements)]),
    lists:foreach(fun(Improvement) ->
        io:format("    • ~s~n", [Improvement])
    end, TemplateImprovements),

    io:format("~n  Dependency Pins (~p):~n", [maps:size(DependencyPins)]),
    maps:foreach(fun(Dep, Version) ->
        io:format("    • ~s = ~s~n", [Dep, Version])
    end, DependencyPins).
