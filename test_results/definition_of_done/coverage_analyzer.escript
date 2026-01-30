#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa _build/test/lib/*/ebin

main(_) ->
    io:format("Analyzing coverage data...~n"),

    % Try to import coverdata
    CoverDir = "_build/test/cover",
    EunitCoverdata = filename:join(CoverDir, "eunit.coverdata"),
    CtCoverdata = filename:join(CoverDir, "ct.coverdata"),

    % Check which coverdata files exist
    EunitExists = filelib:is_file(EunitCoverdata),
    CtExists = filelib:is_file(CtCoverdata),

    io:format("EUnit coverdata exists: ~p~n", [EunitExists]),
    io:format("CT coverdata exists: ~p~n", [CtExists]),

    % Import available coverdata
    Modules1 = case EunitExists of
        true ->
            {ok, _EunitData} = cover:import(EunitCoverdata),
            {ok, EunitData} = cover:import(EunitCoverdata),
            cover:imported_modules(EunitData);
        false ->
            []
    end,

    Modules2 = case CtExists of
        true ->
            {ok, _CtData} = cover:import(CtCoverdata),
            {ok, CtData} = cover:import(CtCoverdata),
            cover:imported_modules(CtData);
        false ->
            []
    end,

    AllModules = lists:usort(Modules1 ++ Modules2),
    io:format("Total modules with coverage data: ~p~n", [length(AllModules)]),

    % Analyze each module
    CoverageFun = fun(Module) ->
        case cover:analyze(Module, coverage, line) of
            {ok, {Module, CovData}} ->
                {Covered, NotCovered} = lists:foldl(
                    fun({_, {C, NC}}, {CAcc, NCAcc}) ->
                        {CAcc + C, NCAcc + NC}
                    end,
                    {0, 0}, CovData),
                Total = Covered + NotCovered,
                Pct = case Total of
                    0 -> 0.0;
                    _ -> (Covered * 100.0) / Total
                end,
                {Module, Pct, Covered, NotCovered, Total};
            {error, Reason} ->
                io:format("Error analyzing ~p: ~p~n", [Module, Reason]),
                {Module, 0.0, 0, 0, 0}
        end
    end,

    CoverageList = lists:map(CoverageFun, AllModules),

    % Sort by coverage percentage (ascending)
    SortedCoverage = lists:sort(fun({_, P1, _, _, _}, {_, P2, _, _, _}) -> P1 =< P2 end, CoverageList),

    % Calculate overall coverage
    {TotalCovered, TotalNotCovered} = lists:foldl(
        fun({_, _, Cov, NotCov, _}, {TCAcc, TNCAcc}) ->
            {TCAcc + Cov, TNCAcc + NotCov}
        end,
        {0, 0}, CoverageList),
    OverallTotal = TotalCovered + TotalNotCovered,
    OverallCoverage = case OverallTotal of
        0 -> 0.0;
        _ -> (TotalCovered * 100.0) / OverallTotal
    end,

    % Output results to file
    OutputFile = "test_results/definition_of_done/COVERAGE_THRESHOLD_REPORT.md",
    ok = filelib:ensure_dir(OutputFile),

    {ok, F} = file:open(OutputFile, [write]),

    io:format(F, "# Coverage Threshold Verification Report~n~n", []),
    io:format(F, "**Generated**: ~p~n~n", [calendar:local_time()]),
    io:format(F, "**Threshold**: ≥80% coverage required~n~n", []),

    % Overall Summary
    io:format(F, "## Overall Project Coverage~n~n", []),
    io:format(F, "**Total Coverage**: ~.2f%~n~n", [OverallCoverage]),
    io:format(F, "- **Covered Lines**: ~p~n", [TotalCovered]),
    io:format(F, "- **Uncovered Lines**: ~p~n", [TotalNotCovered]),
    io:format(F, "- **Total Lines**: ~p~n~n", [OverallTotal]),

    case OverallCoverage >= 80.0 of
        true ->
            io:format(F, "✅ **PASS**: Overall coverage meets threshold (≥80%)~n~n", []);
        false ->
            io:format(F, "❌ **FAIL**: Overall coverage below threshold (<80%)~n~n", [])
    end,

    % Module Breakdown
    io:format(F, "## Module-by-Module Breakdown~n~n", []),
    io:format(F, "| Module | Coverage | Covered | Uncovered | Total Lines | Status |~n", []),
    io:format(F, "|--------|----------|---------|-----------|-------------|--------|~n", []),

    lists:foreach(fun({Mod, Pct, Cov, NotCov, Total}) ->
        Status = case Pct >= 80.0 of
            true -> "✅ PASS";
            false -> "❌ FAIL"
        end,
        io:format(F, "| ~p | ~.2f% | ~p | ~p | ~p | ~s |~n",
                  [Mod, Pct, Cov, NotCov, Total, Status])
    end, SortedCoverage),

    % Modules Below Threshold
    BelowThreshold = [{M, P, C, NC, T} || {M, P, C, NC, T} <- SortedCoverage, P < 80.0],

    io:format(F, "~n## Modules Below 80% Threshold~n~n", []),
    case BelowThreshold of
        [] ->
            io:format(F, "✅ **All modules meet coverage threshold!**~n~n", []);
        _ ->
            io:format(F, "**Count**: ~p modules below threshold~n~n", [length(BelowThreshold)]),
            io:format(F, "| Module | Coverage | Gap to 80% | Uncovered Lines | Priority |~n", []),
            io:format(F, "|--------|----------|-------------|-----------------|----------|~n", []),

            % Sort by gap to threshold (largest gap first = highest priority)
            SortedBelow = lists:sort(fun({_, P1, _, _, _}, {_, P2, _, _, _}) -> P1 < P2 end, BelowThreshold),

            lists:foreach(fun({Mod, Pct, _, NotCov, _}) ->
                Gap = 80.0 - Pct,
                Priority = case Gap of
                    G when G >= 50.0 -> "🔴 CRITICAL";
                    G when G >= 30.0 -> "🟠 HIGH";
                    G when G >= 10.0 -> "🟡 MEDIUM";
                    _ -> "🟢 LOW"
                end,
                io:format(F, "| ~p | ~.2f% | ~.2f% | ~p | ~s |~n",
                          [Mod, Pct, Gap, NotCov, Priority])
            end, SortedBelow)
    end,

    % Prioritized Recommendations
    io:format(F, "~n## Prioritized Recommendations~n~n", []),
    case BelowThreshold of
        [] ->
            io:format(F, "✅ All modules meet coverage requirements. No action needed.~n~n", []);
        _ ->
            io:format(F, "### Critical Priority (Gap ≥ 50%)~n~n", []),
            Critical = [{M, P, NC} || {M, P, _, NC, _} <- BelowThreshold, (80.0 - P) >= 50.0],
            case Critical of
                [] ->
                    io:format(F, "None~n~n", []);
                _ ->
                    lists:foreach(fun({Mod, Pct, NotCov}) ->
                        io:format(F, "- **~p** (~.2f%): ~p uncovered lines - Create comprehensive test suite~n",
                                  [Mod, Pct, NotCov])
                    end, Critical),
                    io:format(F, "~n", [])
            end,

            io:format(F, "### High Priority (Gap ≥ 30%)~n~n", []),
            High = [{M, P, NC} || {M, P, _, NC, _} <- BelowThreshold, (80.0 - P) >= 30.0, (80.0 - P) < 50.0],
            case High of
                [] ->
                    io:format(F, "None~n~n", []);
                _ ->
                    lists:foreach(fun({Mod, Pct, NotCov}) ->
                        io:format(F, "- **~p** (~.2f%): ~p uncovered lines - Expand test coverage~n",
                                  [Mod, Pct, NotCov])
                    end, High),
                    io:format(F, "~n", [])
            end,

            io:format(F, "### Medium Priority (Gap ≥ 10%)~n~n", []),
            Medium = [{M, P, NC} || {M, P, _, NC, _} <- BelowThreshold, (80.0 - P) >= 10.0, (80.0 - P) < 30.0],
            case Medium of
                [] ->
                    io:format(F, "None~n~n", []);
                _ ->
                    lists:foreach(fun({Mod, Pct, NotCov}) ->
                        io:format(F, "- **~p** (~.2f%): ~p uncovered lines - Add targeted tests~n",
                                  [Mod, Pct, NotCov])
                    end, Medium),
                    io:format(F, "~n", [])
            end,

            io:format(F, "### Low Priority (Gap < 10%)~n~n", []),
            Low = [{M, P, NC} || {M, P, _, NC, _} <- BelowThreshold, (80.0 - P) < 10.0],
            case Low of
                [] ->
                    io:format(F, "None~n~n", []);
                _ ->
                    lists:foreach(fun({Mod, Pct, NotCov}) ->
                        io:format(F, "- **~p** (~.2f%): ~p uncovered lines - Minor test additions~n",
                                  [Mod, Pct, NotCov])
                    end, Low),
                    io:format(F, "~n", [])
            end
    end,

    % Summary Statistics
    io:format(F, "## Summary Statistics~n~n", []),
    TotalModules = length(AllModules),
    PassModules = length([{M} || {M, P, _, _, _} <- SortedCoverage, P >= 80.0]),
    FailModules = TotalModules - PassModules,

    io:format(F, "- **Total Modules**: ~p~n", [TotalModules]),
    io:format(F, "- **Modules ≥ 80%**: ~p (~.2f%)~n", [PassModules, (PassModules * 100.0) / TotalModules]),
    io:format(F, "- **Modules < 80%**: ~p (~.2f%)~n", [FailModules, (FailModules * 100.0) / TotalModules]),
    io:format(F, "- **Overall Coverage**: ~.2f%~n", [OverallCoverage]),
    io:format(F, "- **Coverage Threshold**: 80%~n~n", []),

    case OverallCoverage >= 80.0 of
        true ->
            io:format(F, "## ✅ VERIFICATION PASSED~n~n", []),
            io:format(F, "Project meets coverage threshold requirement.~n", []);
        false ->
            io:format(F, "## ❌ VERIFICATION FAILED~n~n", []),
            io:format(F, "Project does NOT meet coverage threshold requirement.~n", [])
    end,

    file:close(F),

    io:format("~nCoverage analysis complete.~n", []),
    io:format("Report saved to: ~s~n", [OutputFile]),
    io:format("~nOverall Coverage: ~.2f%~n", [OverallCoverage]),
    io:format("Modules: ~p total, ~p pass, ~p fail~n", [TotalModules, PassModules, FailModules]),

    halt(0).
