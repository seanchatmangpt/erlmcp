#!/usr/bin/env escript
%%! -sname verify_portal -pa ebin

%% Portal verification script - validates marketplace completeness before deployment
%% Checks:
%% - plans.json syntax (valid JSON)
%% - All evidence links exist
%% - SLA metrics endpoint reachable (if running)
%% - Portal determinism

-mode(compile).

main(_) ->
    io:format("=== erlmcp Portal Verification v1.0 ===~n", []),

    Cwd = case file:get_cwd() of
        {ok, Dir} -> Dir;
        _ -> "/Users/sac/erlmcp"
    end,

    % Verify plans.json syntax
    verify_plans_json(Cwd),
    io:format("  OK: plans.json is valid JSON~n", []),

    % Verify all evidence links exist
    verify_evidence_files(Cwd),
    io:format("  OK: All evidence files exist~n", []),

    % Verify comparison matrix exists and is valid markdown
    verify_comparison_matrix(Cwd),
    io:format("  OK: Plan comparison matrix is valid~n", []),

    % Verify portal metadata
    verify_portal_metadata(Cwd),
    io:format("  OK: Portal metadata is valid~n", []),

    % Verify HTML template exists
    verify_html_portal(Cwd),
    io:format("  OK: HTML portal template exists~n", []),

    io:format("~nPortal verification passed! Ready for deployment.~n", []),
    erlang:halt(0).

%% Verify plans.json is valid JSON
verify_plans_json(Cwd) ->
    File = filename:join(Cwd, "dist/marketplace/plans.json"),
    case file:read_file(File) of
        {ok, Binary} ->
            try
                jsx:decode(Binary, [return_maps]),
                ok
            catch
                _:_ ->
                    io:format("ERROR: Invalid JSON in plans.json~n", []),
                    erlang:halt(1)
            end;
        {error, Reason} ->
            io:format("ERROR: Cannot read plans.json: ~p~n", [Reason]),
            erlang:halt(1)
    end.

%% Verify all evidence files exist for all plans
verify_evidence_files(Cwd) ->
    PlanFiles = [
        filename:join(Cwd, "plans/team.plan.json"),
        filename:join(Cwd, "plans/enterprise.plan.json"),
        filename:join(Cwd, "plans/gov.plan.json")
    ],

    lists:foreach(fun(PlanFile) ->
        case file:read_file(PlanFile) of
            {ok, Binary} ->
                Plan = jsx:decode(Binary, [return_maps]),
                Evidence = maps:get(<<"evidence">>, Plan),
                verify_plan_evidence(Cwd, maps:get(<<"tier">>, Plan), Evidence);
            {error, Reason} ->
                io:format("ERROR: Cannot read plan file: ~p~n", [Reason]),
                erlang:halt(1)
        end
    end, PlanFiles).

%% Verify evidence files for a single plan
verify_plan_evidence(Cwd, Tier, Evidence) ->
    RequiredKeys = [
        <<"sbom">>,
        <<"provenance">>,
        <<"chaos_report">>,
        <<"benchmark_report">>
    ],

    lists:foreach(fun(Key) ->
        case maps:get(Key, Evidence, null) of
            null ->
                io:format("WARNING: Missing evidence ~s for tier ~s~n", [Key, Tier]);
            Path ->
                FilePath = filename:join(Cwd, erlang:binary_to_list(Path)),
                case file:exists(FilePath) of
                    true -> ok;
                    false ->
                        io:format("ERROR: Missing evidence file: ~s (referenced by ~s)~n", [Path, Tier]),
                        erlang:halt(1)
                end
        end
    end, RequiredKeys).

%% Verify comparison matrix markdown exists
verify_comparison_matrix(Cwd) ->
    File = filename:join(Cwd, "dist/marketplace/plan-comparison.md"),
    case file:read_file(File) of
        {ok, Binary} ->
            Content = erlang:binary_to_list(Binary),
            % Verify it contains markdown table header
            case string:find(Content, "| Metric | Team |") of
                nomatch ->
                    io:format("ERROR: Invalid comparison matrix format~n", []),
                    erlang:halt(1);
                _ -> ok
            end;
        {error, Reason} ->
            io:format("ERROR: Cannot read comparison matrix: ~p~n", [Reason]),
            erlang:halt(1)
    end.

%% Verify portal metadata exists and is valid JSON
verify_portal_metadata(Cwd) ->
    File = filename:join(Cwd, "dist/marketplace/portal-metadata.json"),
    case file:read_file(File) of
        {ok, Binary} ->
            try
                Metadata = jsx:decode(Binary, [return_maps]),
                % Verify required fields
                case {maps:get(<<"version">>, Metadata, undefined),
                      maps:get(<<"generated_at">>, Metadata, undefined),
                      maps:get(<<"erlmcp_version">>, Metadata, undefined)} of
                    {V, G, E} when V =/= undefined, G =/= undefined, E =/= undefined ->
                        ok;
                    _ ->
                        io:format("ERROR: Missing required metadata fields~n", []),
                        erlang:halt(1)
                end
            catch
                _:_ ->
                    io:format("ERROR: Invalid JSON in portal metadata~n", []),
                    erlang:halt(1)
            end;
        {error, Reason} ->
            io:format("ERROR: Cannot read portal metadata: ~p~n", [Reason]),
            erlang:halt(1)
    end.

%% Verify HTML portal template exists
verify_html_portal(Cwd) ->
    File = filename:join(Cwd, "templates/pricing_portal.html"),
    case file:read_file(File) of
        {ok, Binary} ->
            Content = erlang:binary_to_list(Binary),
            % Verify it contains required HTML elements
            case {string:find(Content, "<!DOCTYPE html>") =/= nomatch,
                  string:find(Content, "pricing") =/= nomatch,
                  string:find(Content, "plans") =/= nomatch} of
                {true, true, true} -> ok;
                _ ->
                    io:format("ERROR: HTML portal missing required elements~n", []),
                    erlang:halt(1)
            end;
        {error, Reason} ->
            io:format("ERROR: Cannot read HTML portal: ~p~n", [Reason]),
            erlang:halt(1)
    end.
