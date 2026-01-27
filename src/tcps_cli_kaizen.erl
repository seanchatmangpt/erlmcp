%%%-----------------------------------------------------------------------------
%%% @doc TCPS CLI Kaizen Continuous Improvement
%%%
%%% Handles all Kaizen continuous improvement CLI commands.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(tcps_cli_kaizen).

-export([run/1]).

-spec run([string()]) -> no_return().

%%%=============================================================================
%%% API
%%%=============================================================================

run(["report" | Args]) ->
    generate_report(parse_args(Args));

run(["proposals" | Args]) ->
    show_proposals(parse_args(Args));

run(["apply", ImprovementId | _]) ->
    apply_improvement(list_to_binary(ImprovementId));

run(["waste" | _]) ->
    show_waste();

run(["trends" | _]) ->
    show_trends();

run([]) ->
    tcps_cli_format:error("Missing subcommand. Use: report, proposals, apply, waste, trends"),
    halt(1);

run([Unknown | _]) ->
    tcps_cli_format:error("Unknown subcommand: ~s", [Unknown]),
    halt(1).

%%%=============================================================================
%%% Command Implementations
%%%=============================================================================

generate_report(Args) ->
    % Determine period
    {StartDate, EndDate} = case {maps:get(weekly, Args, false),
                                 maps:get(monthly, Args, false)} of
        {true, _} ->
            % Last 7 days
            Today = date(),
            WeekAgo = subtract_days(Today, 7),
            {WeekAgo, Today};
        {_, true} ->
            % Last 30 days
            Today = date(),
            MonthAgo = subtract_days(Today, 30),
            {MonthAgo, Today};
        _ ->
            % Default to weekly
            Today = date(),
            WeekAgo = subtract_days(Today, 7),
            {WeekAgo, Today}
    end,

    % Generate report
    Report = tcps_kaizen:generate_weekly_report({StartDate, EndDate}),

    % Output
    Format = tcps_cli_config:get(output_format, table),
    case Format of
        json ->
            tcps_cli_format:json(Report);
        _ ->
            print_kaizen_report(Report)
    end,

    % Save to file if requested
    case maps:get(output, Args, undefined) of
        undefined -> ok;
        OutputFile ->
            save_report(Report, OutputFile),
            tcps_cli_format:success("Report saved to: ~s", [OutputFile])
    end,

    halt(0).

show_proposals(Args) ->
    TopN = maps:get(top, Args, 10),
    RoiThreshold = maps:get(roi_threshold, Args, 0.0),

    % Get waste points and generate proposals
    WastePoints = tcps_kaizen:identify_waste_points(),
    Proposals = tcps_kaizen:propose_improvements(WastePoints),

    % Filter by ROI threshold and take top N
    Filtered = [P || P <- Proposals, maps:get(roi, P, 0.0) >= RoiThreshold],
    TopProposals = lists:sublist(Filtered, TopN),

    case TopProposals of
        [] ->
            io:format("No improvement proposals found.~n"),
            halt(0);
        _ ->
            Format = tcps_cli_config:get(output_format, table),
            Headers = [id, description, expected_benefit, effort, roi],
            FormattedData = [format_proposal(P) || P <- TopProposals],
            tcps_cli_format:output(FormattedData, Format, #{headers => Headers}),
            halt(0)
    end.

apply_improvement(ImprovementId) ->
    case tcps_kaizen:apply_improvement(ImprovementId) of
        {ok, Receipt} ->
            tcps_cli_format:success("Improvement applied: ~s", [ImprovementId]),
            tcps_cli_format:info("Receipt: ~s", [maps:get(id, Receipt)]),
            halt(0);
        {error, {improvement_not_found, _}} ->
            tcps_cli_format:error("Improvement not found: ~s", [ImprovementId]),
            halt(1);
        {error, {manual_intervention_required, Msg}} ->
            tcps_cli_format:error("~s", [Msg]),
            halt(1);
        {error, Reason} ->
            tcps_cli_format:error("Failed to apply improvement: ~p", [Reason]),
            halt(1)
    end.

show_waste() ->
    WastePoints = tcps_kaizen:identify_waste_points(),

    case WastePoints of
        [] ->
            io:format("No waste points identified.~n"),
            halt(0);
        _ ->
            Format = tcps_cli_config:get(output_format, table),
            Headers = [waste_type, stage, impact, frequency, total_waste],
            FormattedData = [format_waste_point(W) || W <- WastePoints],
            tcps_cli_format:output(FormattedData, Format, #{headers => Headers}),

            TotalWaste = lists:sum([maps:get(total_waste, W) || W <- WastePoints]),
            io:format("~nTotal waste: ~.1f hours/week~n", [TotalWaste]),
            halt(0)
    end.

show_trends() ->
    % Get last 8 weeks of metrics
    EndDate = date(),
    StartDate = subtract_days(EndDate, 56),

    MetricHistory = tcps_kaizen:get_metric_history(StartDate, EndDate),
    Trends = tcps_kaizen:analyze_trends(MetricHistory),

    Format = tcps_cli_config:get(output_format, table),
    case Format of
        json ->
            tcps_cli_format:json(Trends);
        _ ->
            print_trends(Trends)
    end,

    halt(0).

%%%=============================================================================
%%% Helper Functions
%%%=============================================================================

parse_args(Args) ->
    parse_args(Args, #{}).

parse_args([], Acc) ->
    Acc;
parse_args(["--weekly" | Rest], Acc) ->
    parse_args(Rest, Acc#{weekly => true});
parse_args(["--monthly" | Rest], Acc) ->
    parse_args(Rest, Acc#{monthly => true});
parse_args(["--output", File | Rest], Acc) ->
    parse_args(Rest, Acc#{output => File});
parse_args(["--top", Num | Rest], Acc) ->
    parse_args(Rest, Acc#{top => list_to_integer(Num)});
parse_args(["--roi-threshold", Threshold | Rest], Acc) ->
    parse_args(Rest, Acc#{roi_threshold => list_to_float(Threshold)});
parse_args([Unknown | Rest], Acc) ->
    tcps_cli_format:warning("Unknown argument: ~s", [Unknown]),
    parse_args(Rest, Acc).

subtract_days(Date, Days) ->
    GregorianDays = calendar:date_to_gregorian_days(Date),
    calendar:gregorian_days_to_date(GregorianDays - Days).

format_proposal(Proposal) ->
    #{
        id => maps:get(id, Proposal),
        description => truncate_text(maps:get(description, Proposal), 50),
        expected_benefit => truncate_text(maps:get(expected_benefit, Proposal), 40),
        effort => maps:get(implementation_effort, Proposal),
        roi => iolist_to_binary(io_lib:format("~.2f", [maps:get(roi, Proposal, 0.0)]))
    }.

format_waste_point(Waste) ->
    #{
        waste_type => maps:get(waste_type, Waste),
        stage => maps:get(stage, Waste),
        impact => iolist_to_binary(io_lib:format("~.1fh", [maps:get(impact, Waste)])),
        frequency => maps:get(frequency, Waste),
        total_waste => iolist_to_binary(io_lib:format("~.1fh", [maps:get(total_waste, Waste)]))
    }.

truncate_text(Text, MaxLen) when is_binary(Text) ->
    case byte_size(Text) > MaxLen of
        true ->
            <<Prefix:MaxLen/binary, _/binary>> = Text,
            <<Prefix/binary, "...">>;
        false ->
            Text
    end.

print_kaizen_report(Report) ->
    io:format("~n"),
    io:format("Kaizen Report - Week Ending ~p~n", [maps:get(week_ending, Report)]),
    io:format("========================================~n~n"),

    Summary = maps:get(summary, Report),
    CurrentMetrics = maps:get(current_metrics, Summary),

    io:format("Current Metrics:~n"),
    io:format("  Lead Time:        ~.1f hours~n", [maps:get(lead_time, CurrentMetrics)]),
    io:format("  Defect Rate:      ~.1f%%~n", [maps:get(defect_rate, CurrentMetrics)]),
    io:format("  Rework:           ~.1f%%~n", [maps:get(rework_pct, CurrentMetrics)]),
    io:format("  First Pass Yield: ~.1f%%~n", [maps:get(first_pass_yield, CurrentMetrics)]),
    io:format("  Throughput:       ~.1f SKUs/day~n", [maps:get(throughput, CurrentMetrics)]),
    io:format("~n"),

    WasteAnalysis = maps:get(waste_analysis, Report),
    io:format("Waste Analysis:~n"),
    io:format("  Total Waste: ~.1f hours/week~n", [maps:get(total_waste_hours, WasteAnalysis)]),
    io:format("~n"),

    Improvements = maps:get(improvements, Report),
    io:format("Improvements:~n"),
    io:format("  Proposed: ~p~n", [maps:get(total_proposed, Improvements)]),
    io:format("  Applied:  ~p~n", [maps:get(total_applied, Improvements)]),
    io:format("~n").

print_trends(Trends) ->
    io:format("~n"),
    io:format("Trend Analysis:~n"),
    io:format("===============~n~n"),

    maps:foreach(fun(Metric, TrendData) ->
        Improving = maps:get(improving, TrendData),
        Rate = maps:get(rate_of_improvement, TrendData),
        OnTrack = maps:get(on_track, TrendData),

        Status = case {Improving, OnTrack} of
            {true, true} -> "✓ On track";
            {true, false} -> "⚠ Improving slowly";
            {false, _} -> "✗ Declining"
        end,

        io:format("~s:~n", [Metric]),
        io:format("  Status: ~s~n", [Status]),
        io:format("  Rate:   ~.1f%% per week~n", [Rate]),
        io:format("  Target: ~.1f%%~n", [5.0]),  % Hardcoded target
        io:format("~n")
    end, Trends).

save_report(Report, Filename) ->
    JsonBin = jsx:encode(Report, [pretty]),
    file:write_file(Filename, JsonBin).
