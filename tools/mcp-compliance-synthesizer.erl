#!/usr/bin/env escript
%% -*- erlang -*-
%%! -sname mcp_compliance_synthesizer

%%% @doc MCP Compliance Report Synthesizer
%%% Aggregates findings from 9 agent assessment reports into unified compliance report
%%%
%%% Usage:
%%%   ./mcp-compliance-synthesizer.erl \
%%%     --agent1 agent1_output.json \
%%%     --agent2 agent2_output.json \
%%%     ... \
%%%     --agent9 agent9_output.json \
%%%     --output final_compliance_report.md \
%%%     --format markdown
%%%
%%% Input: 9 JSON files from agent assessments
%%% Output: Unified compliance report (markdown, JSON, or HTML)
%%%
%%% @reference MCP_COMPLIANCE_ASSESSMENT_FRAMEWORK.md
%%% @version 1.0.0

-mode(compile).

main(Args) ->
    io:format("MCP Compliance Report Synthesizer v1.0.0~n~n"),

    case parse_args(Args) of
        {ok, Config} ->
            synthesize_report(Config);
        {error, Reason} ->
            io:format("Error: ~s~n", [Reason]),
            usage(),
            halt(1)
    end.

%%% ============================================================================
%%% Configuration & Argument Parsing
%%% ============================================================================

parse_args(Args) ->
    try
        Config = parse_args_loop(Args, #{
            agent_files => [],
            output_file => "mcp_compliance_report.md",
            format => markdown,
            spec_version => "MCP 2025-11-25",
            implementation => "erlmcp"
        }),

        validate_config(Config)
    catch
        error:{badarg, Msg} -> {error, Msg}
    end.

parse_args_loop([], Config) ->
    Config;
parse_args_loop(["--agent" ++ N, File | Rest], Config) ->
    AgentNum = list_to_integer(N),
    Files = maps:get(agent_files, Config),
    parse_args_loop(Rest, Config#{agent_files => [{AgentNum, File} | Files]});
parse_args_loop(["--output", File | Rest], Config) ->
    parse_args_loop(Rest, Config#{output_file => File});
parse_args_loop(["--format", Format | Rest], Config) ->
    parse_args_loop(Rest, Config#{format => list_to_atom(Format)});
parse_args_loop(["--spec", Version | Rest], Config) ->
    parse_args_loop(Rest, Config#{spec_version => Version});
parse_args_loop(["--impl", Name | Rest], Config) ->
    parse_args_loop(Rest, Config#{implementation => Name});
parse_args_loop([Unknown | _], _Config) ->
    error({badarg, io_lib:format("Unknown argument: ~s", [Unknown])}).

validate_config(#{agent_files := Files} = Config) when length(Files) =:= 9 ->
    {ok, Config};
validate_config(#{agent_files := Files}) ->
    {error, io_lib:format("Expected 9 agent files, got ~p", [length(Files)])}.

%%% ============================================================================
%%% Main Synthesis Logic
%%% ============================================================================

synthesize_report(Config) ->
    io:format("Phase 1: Loading agent reports...~n"),
    AgentData = load_agent_data(Config),

    io:format("Phase 2: Aggregating features...~n"),
    Features = aggregate_features(AgentData),

    io:format("Phase 3: Deduplicating gaps...~n"),
    Gaps = deduplicate_gaps(AgentData),

    io:format("Phase 4: Calculating scores...~n"),
    Scores = calculate_scores(Features, Gaps),

    io:format("Phase 5: Generating report...~n"),
    Report = generate_report(Config, AgentData, Features, Gaps, Scores),

    io:format("Phase 6: Writing output...~n"),
    write_report(Config, Report),

    io:format("~nCompliance report generated: ~s~n", [maps:get(output_file, Config)]),
    print_summary(Scores),

    ok.

%%% ============================================================================
%%% Data Loading
%%% ============================================================================

load_agent_data(#{agent_files := Files}) ->
    lists:map(fun({AgentNum, File}) ->
        io:format("  Loading Agent ~p: ~s~n", [AgentNum, File]),
        case file:read_file(File) of
            {ok, Binary} ->
                case jsx:decode(Binary, [return_maps]) of
                    Data when is_map(Data) ->
                        {AgentNum, Data};
                    _ ->
                        error({invalid_json, File})
                end;
            {error, Reason} ->
                error({file_error, File, Reason})
        end
    end, lists:sort(Files)).

%%% ============================================================================
%%% Feature Aggregation
%%% ============================================================================

aggregate_features(AgentData) ->
    AllFeatures = lists:flatmap(fun({_AgentNum, Data}) ->
        maps:get(<<"features">>, Data, [])
    end, AgentData),

    %% Group by category and merge duplicates
    Grouped = group_by_category(AllFeatures),

    %% Calculate per-category stats
    maps:map(fun(_Category, Features) ->
        #{
            features => Features,
            total => length(Features),
            complete => count_by_status(Features, <<"COMPLETE">>),
            partial => count_by_status(Features, <<"PARTIAL">>),
            missing => count_by_status(Features, <<"MISSING">>),
            coverage => calculate_coverage(Features),
            quality => calculate_avg_quality(Features)
        }
    end, Grouped).

group_by_category(Features) ->
    lists:foldl(fun(Feature, Acc) ->
        Category = maps:get(<<"category">>, Feature, <<"Unknown">>),
        CategoryFeatures = maps:get(Category, Acc, []),
        Acc#{Category => [Feature | CategoryFeatures]}
    end, #{}, Features).

count_by_status(Features, Status) ->
    length(lists:filter(fun(F) ->
        maps:get(<<"status">>, F) =:= Status
    end, Features)).

calculate_coverage(Features) ->
    Total = length(Features),
    case Total of
        0 -> 0.0;
        _ ->
            Weighted = lists:sum(lists:map(fun(F) ->
                maps:get(<<"percentage">>, F, 0)
            end, Features)),
            Weighted / Total
    end.

calculate_avg_quality(Features) ->
    Total = length(Features),
    case Total of
        0 -> 0.0;
        _ ->
            Weighted = lists:sum(lists:map(fun(F) ->
                maps:get(<<"quality_score">>, F, 0)
            end, Features)),
            Weighted / Total
    end.

%%% ============================================================================
%%% Gap Deduplication
%%% ============================================================================

deduplicate_gaps(AgentData) ->
    AllGaps = lists:flatmap(fun({_AgentNum, Data}) ->
        maps:get(<<"gaps">>, Data, [])
    end, AgentData),

    %% Group by feature/title, keep highest priority
    Grouped = lists:foldl(fun(Gap, Acc) ->
        ID = maps:get(<<"id">>, Gap, generate_gap_id(Gap)),
        case maps:get(ID, Acc, undefined) of
            undefined ->
                Acc#{ID => Gap};
            ExistingGap ->
                %% Keep gap with higher priority score
                ExistingScore = maps:get(<<"priority_score">>, ExistingGap, 0),
                NewScore = maps:get(<<"priority_score">>, Gap, 0),
                if
                    NewScore > ExistingScore ->
                        Acc#{ID => Gap};
                    true ->
                        Acc
                end
        end
    end, #{}, AllGaps),

    %% Convert back to list and sort by priority
    Gaps = maps:values(Grouped),
    lists:sort(fun(A, B) ->
        maps:get(<<"priority_score">>, A, 0) >= maps:get(<<"priority_score">>, B, 0)
    end, Gaps).

generate_gap_id(Gap) ->
    Title = maps:get(<<"title">>, Gap, <<"unknown">>),
    Category = maps:get(<<"category">>, Gap, <<"unknown">>),
    iolist_to_binary([Category, "-", normalize_title(Title)]).

normalize_title(Title) ->
    %% Convert to lowercase, replace spaces with dashes
    Lower = string:lowercase(Title),
    re:replace(Lower, " ", "-", [global, {return, binary}]).

%%% ============================================================================
%%% Score Calculation
%%% ============================================================================

calculate_scores(Features, Gaps) ->
    %% Agent weights (from framework)
    Weights = #{
        <<"Initialization">> => 2.0,
        <<"Resources">> => 1.5,
        <<"Tools">> => 1.5,
        <<"Prompts">> => 1.5,
        <<"Transport">> => 1.8,
        <<"Security">> => 2.0,
        <<"Extensions">> => 1.0,
        <<"Content">> => 1.0,
        <<"Integration">> => 1.2
    },

    %% Calculate weighted coverage
    {TotalWeighted, TotalWeight} = maps:fold(fun(Category, Stats, {AccWeighted, AccWeight}) ->
        Coverage = maps:get(coverage, Stats),
        Weight = maps:get(Category, Weights, 1.0),
        {AccWeighted + (Coverage * Weight), AccWeight + Weight}
    end, {0.0, 0.0}, Features),

    WeightedCoverage = case TotalWeight of
        0.0 -> 0.0;
        _ -> TotalWeighted / TotalWeight
    end,

    %% Calculate average quality
    AvgQuality = maps:fold(fun(_Category, Stats, Acc) ->
        Quality = maps:get(quality, Stats),
        Acc + Quality
    end, 0.0, Features) / max(1, maps:size(Features)),

    %% Count gaps by priority
    GapCounts = lists:foldl(fun(Gap, Acc) ->
        Priority = maps:get(<<"priority">>, Gap, <<"UNKNOWN">>),
        Count = maps:get(Priority, Acc, 0),
        Acc#{Priority => Count + 1}
    end, #{}, Gaps),

    %% Calculate overall compliance
    OverallCompliance = (WeightedCoverage * 0.6) + (AvgQuality * 0.4),

    %% Determine certification level
    CertLevel = determine_certification(OverallCompliance, AvgQuality, GapCounts),

    #{
        weighted_coverage => WeightedCoverage,
        average_quality => AvgQuality,
        overall_compliance => OverallCompliance,
        certification_level => CertLevel,
        gap_counts => GapCounts,
        total_gaps => length(Gaps)
    }.

determine_certification(Compliance, Quality, GapCounts) ->
    Critical = maps:get(<<"CRITICAL">>, GapCounts, 0),
    High = maps:get(<<"HIGH">>, GapCounts, 0),

    if
        Compliance >= 95.0 andalso Quality >= 90.0 andalso Critical == 0 andalso High =< 2 ->
            "PRODUCTION READY";
        Compliance >= 90.0 andalso Quality >= 85.0 andalso Critical =< 1 ->
            "RELEASE CANDIDATE";
        Compliance >= 80.0 andalso Quality >= 75.0 ->
            "BETA QUALITY";
        Compliance >= 70.0 andalso Quality >= 65.0 ->
            "ALPHA QUALITY";
        true ->
            "NOT READY"
    end.

%%% ============================================================================
%%% Report Generation
%%% ============================================================================

generate_report(Config, AgentData, Features, Gaps, Scores) ->
    Format = maps:get(format, Config),
    case Format of
        markdown ->
            generate_markdown_report(Config, AgentData, Features, Gaps, Scores);
        json ->
            generate_json_report(Config, AgentData, Features, Gaps, Scores);
        html ->
            generate_html_report(Config, AgentData, Features, Gaps, Scores)
    end.

generate_markdown_report(Config, AgentData, Features, Gaps, Scores) ->
    [
        generate_markdown_header(Config, Scores),
        generate_markdown_executive_summary(Scores),
        generate_markdown_coverage_matrix(Features),
        generate_markdown_gap_analysis(Gaps),
        generate_markdown_agent_findings(AgentData),
        generate_markdown_recommendations(Scores, Gaps)
    ].

generate_markdown_header(Config, Scores) ->
    SpecVersion = maps:get(spec_version, Config),
    Implementation = maps:get(implementation, Config),
    Compliance = maps:get(overall_compliance, Scores),
    CertLevel = maps:get(certification_level, Scores),

    io_lib:format(
        "# MCP Specification Compliance Report~n~n"
        "**Implementation**: ~s~n"
        "**Specification**: ~s~n"
        "**Assessment Date**: ~s~n"
        "**Overall Compliance**: ~.1f%~n"
        "**Certification**: ~s~n~n"
        "---~n~n",
        [Implementation, SpecVersion, format_date(), Compliance, CertLevel]
    ).

generate_markdown_executive_summary(Scores) ->
    Coverage = maps:get(weighted_coverage, Scores),
    Quality = maps:get(average_quality, Scores),
    Compliance = maps:get(overall_compliance, Scores),
    GapCounts = maps:get(gap_counts, Scores),

    [
        "## Executive Summary~n~n",
        "| Metric | Value | Status |~n",
        "|--------|-------|--------|~n",
        io_lib:format("| **Weighted Coverage** | ~.1f% | ~s |~n", [Coverage, status_icon(Coverage, 90)]),
        io_lib:format("| **Average Quality** | ~.1f | ~s |~n", [Quality, quality_grade(Quality)]),
        io_lib:format("| **Overall Compliance** | ~.1f% | ~s |~n", [Compliance, status_icon(Compliance, 90)]),
        io_lib:format("| **Critical Gaps** | ~p | ~s |~n", [
            maps:get(<<"CRITICAL">>, GapCounts, 0),
            if maps:get(<<"CRITICAL">>, GapCounts, 0) == 0 -> "✅"; true -> "❌" end
        ]),
        io_lib:format("| **High-Priority Gaps** | ~p | ~s |~n", [
            maps:get(<<"HIGH">>, GapCounts, 0),
            if maps:get(<<"HIGH">>, GapCounts, 0) =< 2 -> "✅"; true -> "⚠️" end
        ]),
        "~n---~n~n"
    ].

generate_markdown_coverage_matrix(Features) ->
    [
        "## Specification Coverage by Area~n~n",
        "| Area | Total | Complete | Partial | Missing | Coverage | Quality |~n",
        "|------|-------|----------|---------|---------|----------|---------|~n",
        [begin
            Stats = maps:get(Category, Features),
            Total = maps:get(total, Stats),
            Complete = maps:get(complete, Stats),
            Partial = maps:get(partial, Stats),
            Missing = maps:get(missing, Stats),
            Coverage = maps:get(coverage, Stats),
            Quality = maps:get(quality, Stats),

            io_lib:format("| ~s | ~p | ~p | ~p | ~p | ~.1f% | ~s |~n",
                [Category, Total, Complete, Partial, Missing, Coverage, quality_grade(Quality)])
        end || Category <- lists:sort(maps:keys(Features))],
        "~n---~n~n"
    ].

generate_markdown_gap_analysis(Gaps) ->
    Critical = [G || G <- Gaps, maps:get(<<"priority">>, G) =:= <<"CRITICAL">>],
    High = [G || G <- Gaps, maps:get(<<"priority">>, G) =:= <<"HIGH">>],
    Medium = [G || G <- Gaps, maps:get(<<"priority">>, G) =:= <<"MEDIUM">>],
    Low = [G || G <- Gaps, maps:get(<<"priority">>, G) =:= <<"LOW">>],

    [
        "## Gap Analysis~n~n",
        generate_gap_section("Critical Gaps (P0)", Critical),
        generate_gap_section("High-Priority Gaps (P1)", High),
        generate_gap_section("Medium-Priority Gaps (P2)", Medium),
        generate_gap_section("Low-Priority Gaps (P3)", Low),
        "~n---~n~n"
    ].

generate_gap_section(Title, Gaps) ->
    [
        io_lib:format("### ~s (~p gaps)~n~n", [Title, length(Gaps)]),
        [begin
            ID = maps:get(<<"id">>, Gap, <<"unknown">>),
            GapTitle = maps:get(<<"title">>, Gap, <<"Unknown">>),
            Category = maps:get(<<"category">>, Gap, <<"Unknown">>),
            Effort = maps:get(<<"effort_hours">>, Gap, <<"unknown">>),
            Impact = maps:get(<<"compliance_impact">>, Gap, 0),

            io_lib:format(
                "- **~s**: ~s~n"
                "  - Category: ~s~n"
                "  - Effort: ~s hours~n"
                "  - Impact: ~p%~n~n",
                [ID, GapTitle, Category, Effort, Impact]
            )
        end || Gap <- Gaps],
        "~n"
    ].

generate_markdown_agent_findings(AgentData) ->
    [
        "## Agent Findings Summary~n~n",
        [begin
            Data = AgentData2,
            Agent = maps:get(<<"agent">>, Data),
            AgentName = maps:get(<<"name">>, Agent),
            Metrics = maps:get(<<"metrics">>, Data),
            Coverage = maps:get(<<"coverage_percentage">>, Metrics),
            Quality = maps:get(<<"quality_average">>, Metrics),

            io_lib:format(
                "### Agent ~p: ~s~n~n"
                "- Coverage: ~.1f%~n"
                "- Quality: ~.1f~n"
                "- Features Assessed: ~p~n"
                "- Gaps Found: ~p~n~n",
                [AgentNum, AgentName, Coverage, Quality,
                 maps:get(<<"features_total">>, Metrics),
                 maps:get(<<"gaps_critical">>, Metrics, 0) + maps:get(<<"gaps_high">>, Metrics, 0)]
            )
        end || {AgentNum, Data} = AgentData2 <- AgentData],
        "~n---~n~n"
    ].

generate_markdown_recommendations(Scores, Gaps) ->
    CertLevel = maps:get(certification_level, Scores),

    Recommendations = case CertLevel of
        "PRODUCTION READY" ->
            ["✅ Implementation is production ready", "✅ Deploy with confidence"];
        "RELEASE CANDIDATE" ->
            ["⚠️ Address remaining high-priority gaps before production", "⚠️ Consider beta release"];
        _ ->
            Critical = length([G || G <- Gaps, maps:get(<<"priority">>, G) =:= <<"CRITICAL">>]),
            [io_lib:format("❌ Fix ~p critical gaps before deployment", [Critical]),
             "❌ Significant work required"]
    end,

    [
        "## Recommendations~n~n",
        [io_lib:format("~s~n", [R]) || R <- Recommendations],
        "~n"
    ].

generate_json_report(_Config, _AgentData, Features, Gaps, Scores) ->
    jsx:encode(#{
        features => Features,
        gaps => Gaps,
        scores => Scores
    }, [space, indent]).

generate_html_report(Config, AgentData, Features, Gaps, Scores) ->
    %% Convert markdown to HTML wrapper
    Markdown = generate_markdown_report(Config, AgentData, Features, Gaps, Scores),
    [
        "<!DOCTYPE html>~n<html>~n<head>~n",
        "<title>MCP Compliance Report</title>~n",
        "<style>body{font-family:sans-serif;max-width:1200px;margin:auto;}</style>~n",
        "</head>~n<body>~n",
        "<pre>", Markdown, "</pre>~n",
        "</body>~n</html>~n"
    ].

%%% ============================================================================
%%% Output Writing
%%% ============================================================================

write_report(#{output_file := File}, Report) ->
    Content = iolist_to_binary(Report),
    case file:write_file(File, Content) of
        ok -> ok;
        {error, Reason} ->
            error({write_error, File, Reason})
    end.

%%% ============================================================================
%%% Utilities
%%% ============================================================================

status_icon(Value, Threshold) when Value >= Threshold -> "✅";
status_icon(Value, Threshold) when Value >= Threshold - 10 -> "⚠️";
status_icon(_, _) -> "❌".

quality_grade(Score) when Score >= 95 -> "A+";
quality_grade(Score) when Score >= 90 -> "A";
quality_grade(Score) when Score >= 80 -> "B";
quality_grade(Score) when Score >= 70 -> "C";
quality_grade(Score) when Score >= 60 -> "D";
quality_grade(_) -> "F".

format_date() ->
    {{Y, M, D}, _} = calendar:local_time(),
    io_lib:format("~4..0w-~2..0w-~2..0w", [Y, M, D]).

print_summary(Scores) ->
    io:format("~n=== SUMMARY ===~n"),
    io:format("Coverage: ~.1f%~n", [maps:get(weighted_coverage, Scores)]),
    io:format("Quality: ~.1f~n", [maps:get(average_quality, Scores)]),
    io:format("Compliance: ~.1f%~n", [maps:get(overall_compliance, Scores)]),
    io:format("Certification: ~s~n", [maps:get(certification_level, Scores)]),
    io:format("==============~n~n").

usage() ->
    io:format(
        "Usage: mcp-compliance-synthesizer.erl [OPTIONS]~n~n"
        "Options:~n"
        "  --agent1 <file>     Agent 1 JSON output~n"
        "  --agent2 <file>     Agent 2 JSON output~n"
        "  ...                 (up to --agent9)~n"
        "  --output <file>     Output report file (default: mcp_compliance_report.md)~n"
        "  --format <format>   Output format: markdown, json, html (default: markdown)~n"
        "  --spec <version>    MCP spec version (default: MCP 2025-11-25)~n"
        "  --impl <name>       Implementation name (default: erlmcp)~n~n"
        "Example:~n"
        "  ./mcp-compliance-synthesizer.erl \\~n"
        "    --agent1 agent1.json \\~n"
        "    --agent2 agent2.json \\~n"
        "    ... \\~n"
        "    --agent9 agent9.json \\~n"
        "    --output final_report.md~n"
    ).
