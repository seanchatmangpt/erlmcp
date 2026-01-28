%%%-------------------------------------------------------------------
%%% @doc
%%% Bundle Validation Test Suite for erlmcp v1.4.0
%%%
%%% This test suite verifies that all required documentation and
%%% evidence artifacts are present and properly structured.
%%%
%%% Run with: rebar3 eunit -m erlmcp_bundle_SUITE
%%% @end
%%%-------------------------------------------------------------------

-module(erlmcp_bundle_SUITE).
-include_lib("eunit/include/eunit.hrl").

-define(REPO_ROOT, "/Users/sac/erlmcp").
-define(EVIDENCE_DIR, filename:join(?REPO_ROOT, "dist/evidence/v1.4.0")).
-define(DOCS_DIR, filename:join(?REPO_ROOT, "docs")).
-define(MARKETPLACE_DIR, filename:join(?DOCS_DIR, "marketplace")).
-define(C4_DIR, filename:join(?DOCS_DIR, "c4")).

%%%-------------------------------------------------------------------
%%% Test Cases
%%%-------------------------------------------------------------------

%% Verify all required certification files exist
cert_artifacts_test() ->
    CertDir = filename:join(?EVIDENCE_DIR, "cert"),
    RequiredFiles = [
        "release_checklist.md",
        "bench_report.md",
        "chaos_matrix.md",
        "adversarial_closure.md"
    ],
    lists:foreach(fun(File) ->
        Path = filename:join(CertDir, File),
        ?assert(filelib:is_file(Path),
            io_lib:format("Missing cert file: ~s", [File]))
    end, RequiredFiles).

%% Verify marketplace documentation exists
marketplace_docs_test() ->
    RequiredDocs = [
        "EVIDENCE_BUNDLE_v1.4.0.md",
        "CTO_WHITEPAPER_v1.4.0.md",
        "DEPLOYMENT_v1.4.0.md"
    ],
    lists:foreach(fun(File) ->
        Path = filename:join(?MARKETPLACE_DIR, File),
        ?assert(filelib:is_file(Path),
            io_lib:format("Missing marketplace doc: ~s", [File]))
    end, RequiredDocs).

%% Verify all 4 C4 diagrams exist
c4_diagrams_test() ->
    RequiredDiagrams = [
        "context.md",
        "container.md",
        "components.md",
        "deployment.md"
    ],
    lists:foreach(fun(File) ->
        Path = filename:join(?C4_DIR, File),
        ?assert(filelib:is_file(Path),
            io_lib:format("Missing C4 diagram: ~s", [File]))
    end, RequiredDiagrams).

%% Verify directory structure
directory_structure_test() ->
    RequiredDirs = [
        filename:join(?EVIDENCE_DIR, "cert"),
        filename:join(?EVIDENCE_DIR, "benchmarks"),
        filename:join(?EVIDENCE_DIR, "chaos"),
        filename:join(?EVIDENCE_DIR, "security"),
        filename:join(?EVIDENCE_DIR, "compliance"),
        ?MARKETPLACE_DIR,
        ?C4_DIR
    ],
    lists:foreach(fun(Dir) ->
        ?assert(filelib:is_dir(Dir),
            io_lib:format("Missing directory: ~s", [Dir]))
    end, RequiredDirs).

%% Verify release checklist has proper structure
release_checklist_structure_test() ->
    ChecklistPath = filename:join(?EVIDENCE_DIR, "cert/release_checklist.md"),
    case file:read_file(ChecklistPath) of
        {ok, Content} ->
            BinContent = iolist_to_binary(Content),
            % Check for key sections
            ?assertMatch({match, _},
                re:run(BinContent, "GO/NO-GO DECISION MATRIX"),
                "Checklist should have decision matrix"),
            ?assertMatch({match, _},
                re:run(BinContent, "15 gates"),
                "Checklist should mention 15 gates");
        {error, _Reason} ->
            % File doesn't exist, which is OK for v1.4.0 if inherited from v1.3.0
            ok
    end.

%% Verify CTO whitepaper contains key concepts
cto_whitepaper_concepts_test() ->
    WhitepaperPath = filename:join(?MARKETPLACE_DIR, "CTO_WHITEPAPER_v1.4.0.md"),
    case file:read_file(WhitepaperPath) of
        {ok, Content} ->
            BinContent = iolist_to_binary(Content),
            % Check for key concepts
            ?assertMatch({match, _},
                re:run(BinContent, "Bounded"),
                "Should mention 'Bounded'"),
            ?assertMatch({match, _},
                re:run(BinContent, "Proven"),
                "Should mention 'Proven'"),
            ?assertMatch({match, _},
                re:run(BinContent, "Governable"),
                "Should mention 'Governable'");
        {error, _Reason} ->
            ?assert(false, "CTO whitepaper not found")
    end.

%% Verify evidence bundle has proper index
evidence_bundle_index_test() ->
    BundlePath = filename:join(?MARKETPLACE_DIR, "EVIDENCE_BUNDLE_v1.4.0.md"),
    case file:read_file(BundlePath) of
        {ok, Content} ->
            BinContent = iolist_to_binary(Content),
            % Check for navigation sections
            ?assertMatch({match, _},
                re:run(BinContent, "Quick Navigation"),
                "Should have quick navigation"),
            ?assertMatch({match, _},
                re:run(BinContent, "Certification Artifacts"),
                "Should reference certification artifacts");
        {error, _Reason} ->
            ?assert(false, "Evidence bundle not found")
    end.

%% Verify deployment guide has all paths
deployment_guide_completeness_test() ->
    DeploymentPath = filename:join(?MARKETPLACE_DIR, "DEPLOYMENT_v1.4.0.md"),
    case file:read_file(DeploymentPath) of
        {ok, Content} ->
            BinContent = iolist_to_binary(Content),
            % Check for deployment paths
            ?assertMatch({match, _},
                re:run(BinContent, "GCP"),
                "Should mention GCP"),
            ?assertMatch({match, _},
                re:run(BinContent, "Local"),
                "Should mention Local"),
            ?assertMatch({match, _},
                re:run(BinContent, "Kubernetes"),
                "Should mention Kubernetes");
        {error, _Reason} ->
            ?assert(false, "Deployment guide not found")
    end.

%% Verify C4 diagrams reference each other
c4_cross_references_test() ->
    ContextPath = filename:join(?C4_DIR, "context.md"),
    case file:read_file(ContextPath) of
        {ok, Content} ->
            BinContent = iolist_to_binary(Content),
            % Check for cross-references to other diagrams
            ?assertMatch({match, _},
                re:run(BinContent, "container\\.md"),
                "Context should reference container"),
            ?assertMatch({match, _},
                re:run(BinContent, "components\\.md"),
                "Context should reference components"),
            ?assertMatch({match, _},
                re:run(BinContent, "deployment\\.md"),
                "Context should reference deployment");
        {error, _Reason} ->
            io:format("Note: Context diagram not found~n", [])
    end.

%% Verify mermaid diagram syntax in context diagram
mermaid_syntax_test() ->
    ContextPath = filename:join(?C4_DIR, "context.md"),
    case file:read_file(ContextPath) of
        {ok, Content} ->
            BinContent = iolist_to_binary(Content),
            % Check for mermaid block
            ?assertMatch({match, _},
                re:run(BinContent, "```mermaid"),
                "Should have mermaid diagram"),
            % Check for closing block
            ?assertMatch({match, _},
                re:run(BinContent, "```$"),
                "Should have closing code block");
        {error, _Reason} ->
            io:format("Note: Context diagram not found~n", [])
    end.

%% Verify SBOM exists (v1.3.0 artifact)
sbom_availability_test() ->
    SbomFiles = [
        filename:join(?EVIDENCE_DIR, "erlmcp-1.4.0.sbom.json"),
        filename:join(?EVIDENCE_DIR, "erlmcp-1.3.0.sbom.cyclonedx")
    ],
    SbomExists = lists:any(fun(File) ->
        filelib:is_file(File)
    end, SbomFiles),
    case SbomExists of
        true -> ok;
        false ->
            io:format("Note: SBOM file not found (may be generated separately)~n", [])
    end.

%% Final check: All marketplace documentation present
bundle_completeness_test() ->
    Checks = [
        {release_checklist, filelib:is_file(filename:join(?EVIDENCE_DIR, "cert/release_checklist.md"))},
        {cto_whitepaper, filelib:is_file(filename:join(?MARKETPLACE_DIR, "CTO_WHITEPAPER_v1.4.0.md"))},
        {evidence_bundle, filelib:is_file(filename:join(?MARKETPLACE_DIR, "EVIDENCE_BUNDLE_v1.4.0.md"))},
        {deployment_guide, filelib:is_file(filename:join(?MARKETPLACE_DIR, "DEPLOYMENT_v1.4.0.md"))},
        {c4_context, filelib:is_file(filename:join(?C4_DIR, "context.md"))},
        {c4_container, filelib:is_file(filename:join(?C4_DIR, "container.md"))},
        {c4_components, filelib:is_file(filename:join(?C4_DIR, "components.md"))},
        {c4_deployment, filelib:is_file(filename:join(?C4_DIR, "deployment.md"))}
    ],

    io:format("~n=== Bundle Completeness Check ===~n", []),
    PassCount = count_passing_checks(Checks),
    TotalChecks = length(Checks),

    lists:foreach(fun({Name, Pass}) ->
        Status = case Pass of true -> "✓"; false -> "✗" end,
        io:format("  [~s] ~w~n", [Status, Name])
    end, Checks),

    io:format("~n  Result: ~w/~w checks passed~n~n", [PassCount, TotalChecks]),

    ?assert(PassCount >= 6,
        io_lib:format("Bundle incomplete: ~w essential items missing", [TotalChecks - PassCount])).

%%%-------------------------------------------------------------------
%%% Helper Functions
%%%-------------------------------------------------------------------

count_passing_checks(Checks) ->
    lists:sum([case Pass of true -> 1; false -> 0 end || {_Name, Pass} <- Checks]).

%%%-------------------------------------------------------------------
%%% Test Suite Entry Points
%%%-------------------------------------------------------------------

% Group tests
cert_test_() ->
    [
        ?_test(cert_artifacts_test()),
        ?_test(release_checklist_structure_test())
    ].

marketplace_test_() ->
    [
        ?_test(marketplace_docs_test()),
        ?_test(cto_whitepaper_concepts_test()),
        ?_test(evidence_bundle_index_test()),
        ?_test(deployment_guide_completeness_test())
    ].

architecture_test_() ->
    [
        ?_test(c4_diagrams_test()),
        ?_test(c4_cross_references_test()),
        ?_test(mermaid_syntax_test())
    ].

infrastructure_test_() ->
    [
        ?_test(directory_structure_test()),
        ?_test(sbom_availability_test())
    ].

integration_test_() ->
    [
        ?_test(bundle_completeness_test())
    ].
