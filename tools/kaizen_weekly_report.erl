#!/usr/bin/env escript
%%! -pa _build/default/lib/*/ebin

%%%-----------------------------------------------------------------------------
%%% @doc TCPS Kaizen Weekly Report Generator
%%%
%%% Generates and publishes weekly Kaizen reports automatically.
%%%
%%% Usage:
%%%   ./tools/kaizen_weekly_report.erl [options]
%%%
%%% Options:
%%%   --date YYYY-MM-DD    Report date (default: today)
%%%   --output FILE        Output file (default: stdout)
%%%   --format json|text   Output format (default: text)
%%%   --email ADDR         Send report via email
%%%   --slack URL          Post to Slack webhook
%%%
%%% Examples:
%%%   ./tools/kaizen_weekly_report.erl
%%%   ./tools/kaizen_weekly_report.erl --output report.txt
%%%   ./tools/kaizen_weekly_report.erl --format json --output report.json
%%%
%%% @end
%%%-----------------------------------------------------------------------------

-mode(compile).

main(Args) ->
    %% Parse arguments
    Options = parse_args(Args),

    %% Get report date
    ReportDate = maps:get(date, Options, today()),

    %% Generate report
    io:format("Generating Kaizen report for week ending ~s...~n", [format_date(ReportDate)]),
    Report = generate_report(ReportDate),

    %% Format output
    Format = maps:get(format, Options, text),
    Output = format_report(Report, Format),

    %% Write output
    case maps:get(output, Options, stdout) of
        stdout ->
            io:format("~s~n", [Output]);
        OutputFile ->
            ok = file:write_file(OutputFile, Output),
            io:format("Report written to ~s~n", [OutputFile])
    end,

    %% Send notifications
    send_notifications(Report, Options),

    io:format("Report generation complete.~n"),
    ok.

%%%=============================================================================
%%% Report Generation
%%%=============================================================================

generate_report(ReportDate) ->
    %% Ensure application started
    ensure_started(),

    %% Generate comprehensive report
    tcps_kaizen:generate_weekly_report(ReportDate).

%%%=============================================================================
%%% Formatting
%%%=============================================================================

format_report(Report, json) ->
    jsx:encode(Report);

format_report(Report, text) ->
    #{
        week_ending := WeekEnd,
        summary := Summary,
        waste_analysis := WasteAnalysis,
        improvements := Improvements,
        trends := Trends,
        week_over_week := WoW,
        recommendations := Recs
    } = Report,

    Lines = [
        "=" * 80,
        center("TCPS KAIZEN WEEKLY REPORT", 80),
        center(io_lib:format("Week Ending: ~s", [format_date(WeekEnd)]), 80),
        "=" * 80,
        "",
        "EXECUTIVE SUMMARY",
        "-" * 80,
        format_summary(Summary),
        "",
        "WASTE ANALYSIS",
        "-" * 80,
        format_waste_analysis(WasteAnalysis),
        "",
        "IMPROVEMENTS",
        "-" * 80,
        format_improvements(Improvements),
        "",
        "TREND ANALYSIS",
        "-" * 80,
        format_trends(Trends),
        "",
        "WEEK-OVER-WEEK COMPARISON",
        "-" * 80,
        format_wow(WoW),
        "",
        "RECOMMENDATIONS",
        "-" * 80,
        format_recommendations(Recs),
        "",
        "=" * 80,
        "Report generated: " ++ format_datetime(calendar:universal_time()),
        "=" * 80
    ],

    lists:flatten(lists:join("\n", Lines)).

format_summary(#{current_metrics := Metrics, targets := Targets, achievement := Achievement}) ->
    #{
        lead_time := LeadTime,
        defect_rate := DefectRate,
        rework_pct := ReworkPct,
        cycle_time := CycleTime,
        first_pass_yield := FirstPassYield,
        throughput := Throughput
    } = Metrics,

    #{overall := OverallAchievement} = Achievement,

    [
        io_lib:format("Overall Achievement: ~.1f%~n", [OverallAchievement]),
        "",
        "Current Metrics vs Targets:",
        io_lib:format("  Lead Time:        ~.2f hrs  (target: ~.2f hrs)  ~s",
            [LeadTime, maps:get(lead_time, Targets),
             achievement_indicator(maps:get(lead_time, Achievement))]),
        io_lib:format("  Defect Rate:      ~.2f%%    (target: ~.2f%%)    ~s",
            [DefectRate, maps:get(defect_rate, Targets),
             achievement_indicator(maps:get(defect_rate, Achievement))]),
        io_lib:format("  Rework:           ~.2f%%    (target: ~.2f%%)    ~s",
            [ReworkPct, maps:get(rework_pct, Targets),
             achievement_indicator(maps:get(rework_pct, Achievement))]),
        io_lib:format("  Cycle Time:       ~.2f hrs  (target: ~.2f hrs)  ~s",
            [CycleTime, maps:get(cycle_time, Targets),
             achievement_indicator(maps:get(cycle_time, Achievement))]),
        io_lib:format("  First Pass Yield: ~.2f%%    (target: ~.2f%%)    ~s",
            [FirstPassYield, maps:get(first_pass_yield, Targets),
             achievement_indicator(maps:get(first_pass_yield, Achievement))]),
        io_lib:format("  Throughput:       ~.2f/day  (target: ~.2f/day)  ~s",
            [Throughput, maps:get(throughput, Targets),
             achievement_indicator(maps:get(throughput, Achievement))])
    ].

format_waste_analysis(#{total_waste_hours := TotalWaste, top_waste_points := TopWaste,
                       waste_by_category := ByCategory}) ->
    [
        io_lib:format("Total Waste: ~.1f hours/week~n", [TotalWaste]),
        "",
        "Top 10 Waste Points:",
        lists:map(
            fun(#{waste_type := Type, stage := Stage, total_waste := Waste, frequency := Freq}) ->
                io_lib:format("  ~p. ~s (~s): ~.1f hrs (~p occurrences)",
                    [Freq, Type, Stage, Waste, Freq])
            end,
            lists:sublist(TopWaste, 10)
        ),
        "",
        "Waste by Category:",
        maps:fold(
            fun(Category, #{count := Count, total_waste := Waste}, Acc) ->
                [io_lib:format("  ~s: ~.1f hrs (~p instances)", [Category, Waste, Count]) | Acc]
            end,
            [],
            ByCategory
        )
    ].

format_improvements(#{proposed_this_week := Proposed, applied_this_week := Applied,
                     total_proposed := TotalProposed, total_applied := TotalApplied,
                     benefit_realized := Benefit}) ->
    [
        io_lib:format("Proposed: ~p  |  Applied: ~p  |  Benefit Realized: ~.1f hrs~n",
            [TotalProposed, TotalApplied, Benefit]),
        "",
        "Recently Applied Improvements:",
        case Applied of
            [] ->
                ["  (none this week)"];
            _ ->
                lists:map(
                    fun(#{description := Desc, metadata := Meta}) ->
                        BenefitVal = maps:get(benefit, Meta, 0.0),
                        io_lib:format("  ✓ ~s (~.1f hrs saved)", [Desc, BenefitVal])
                    end,
                    lists:sublist(Applied, 5)
                )
        end,
        "",
        "Top Proposed Improvements:",
        lists:map(
            fun(#{description := Desc, expected_benefit := ExpBenefit,
                  implementation_effort := Effort, roi := ROI}) ->
                io_lib:format("  → ~s~n    Benefit: ~s~n    Effort: ~s, ROI: ~.2f",
                    [Desc, ExpBenefit, Effort, ROI])
            end,
            lists:sublist(Proposed, 3)
        )
    ].

format_trends(Trends) ->
    Metrics = [lead_time, defect_rate, rework_pct, cycle_time, first_pass_yield, throughput],

    ["Metric Trends (5% per week target):", ""] ++
    lists:map(
        fun(Metric) ->
            case maps:get(Metric, Trends, #{}) of
                #{improving := Improving, rate_of_improvement := Rate, on_track := OnTrack} ->
                    Status = trend_indicator(Improving, OnTrack),
                    io_lib:format("  ~s ~-20s: ~.1f%% per week  ~s",
                        [Status, Metric, Rate, track_indicator(OnTrack)]);
                _ ->
                    io_lib:format("  ? ~-20s: No data", [Metric])
            end
        end,
        Metrics
    ).

format_wow(WoW) ->
    Metrics = [lead_time, defect_rate, rework_pct, cycle_time, first_pass_yield, throughput],

    ["Week-over-Week Changes:", ""] ++
    lists:map(
        fun(Metric) ->
            case maps:get(Metric, WoW, #{}) of
                #{current := Curr, previous := Prev, change_pct := Change, improved := Improved} ->
                    Direction = if
                        Improved -> "↓";
                        true -> "↑"
                    end,
                    io_lib:format("  ~s ~-20s: ~.2f → ~.2f (~+.1f%%)",
                        [Direction, Metric, Prev, Curr, Change]);
                _ ->
                    io_lib:format("  ? ~-20s: No data", [Metric])
            end
        end,
        Metrics
    ).

format_recommendations(Recs) ->
    case Recs of
        [] ->
            ["No specific recommendations this week."];
        _ ->
            ByPriority = lists:sort(
                fun(#{priority := P1}, #{priority := P2}) ->
                    priority_value(P1) >= priority_value(P2)
                end,
                Recs
            ),

            lists:map(
                fun(#{priority := Pri, category := Cat, message := Msg, action := Action}) ->
                    PriStr = string:uppercase(atom_to_list(Pri)),
                    [
                        io_lib:format("~n[~s] ~s", [PriStr, Cat]),
                        io_lib:format("  Issue: ~s", [Msg]),
                        io_lib:format("  Action: ~s", [Action])
                    ]
                end,
                ByPriority
            )
    end.

%%%=============================================================================
%%% Notifications
%%%=============================================================================

send_notifications(Report, Options) ->
    %% Email
    case maps:get(email, Options, undefined) of
        undefined -> ok;
        EmailAddr -> send_email(Report, EmailAddr)
    end,

    %% Slack
    case maps:get(slack, Options, undefined) of
        undefined -> ok;
        SlackURL -> post_slack(Report, SlackURL)
    end.

send_email(Report, EmailAddr) ->
    io:format("Sending report to ~s...~n", [EmailAddr]),
    %% TODO: Implement email sending
    %% For now, just acknowledge
    io:format("Email notification not yet implemented.~n"),
    ok.

post_slack(Report, WebhookURL) ->
    io:format("Posting to Slack...~n"),
    %% TODO: Implement Slack webhook posting
    %% For now, just acknowledge
    io:format("Slack notification not yet implemented.~n"),
    ok.

%%%=============================================================================
%%% Utilities
%%%=============================================================================

parse_args(Args) ->
    parse_args(Args, #{}).

parse_args([], Acc) ->
    Acc;
parse_args(["--date", DateStr | Rest], Acc) ->
    Date = parse_date(DateStr),
    parse_args(Rest, Acc#{date => Date});
parse_args(["--output", File | Rest], Acc) ->
    parse_args(Rest, Acc#{output => File});
parse_args(["--format", Format | Rest], Acc) ->
    parse_args(Rest, Acc#{format => list_to_atom(Format)});
parse_args(["--email", Email | Rest], Acc) ->
    parse_args(Rest, Acc#{email => Email});
parse_args(["--slack", URL | Rest], Acc) ->
    parse_args(Rest, Acc#{slack => URL});
parse_args([Arg | _Rest], _Acc) ->
    io:format("Unknown argument: ~s~n", [Arg]),
    usage(),
    halt(1).

parse_date(DateStr) ->
    %% Parse YYYY-MM-DD
    [Y, M, D] = string:split(DateStr, "-", all),
    {list_to_integer(Y), list_to_integer(M), list_to_integer(D)}.

today() ->
    {Date, _Time} = calendar:universal_time(),
    Date.

format_date({Y, M, D}) ->
    io_lib:format("~4..0w-~2..0w-~2..0w", [Y, M, D]).

format_datetime({{Y, M, D}, {H, Min, S}}) ->
    io_lib:format("~4..0w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w UTC",
        [Y, M, D, H, Min, S]).

center(Text, Width) ->
    Len = length(lists:flatten(Text)),
    Padding = (Width - Len) div 2,
    lists:duplicate(Padding, $\s) ++ Text.

achievement_indicator(Achievement) ->
    if
        Achievement >= 100.0 -> "✓✓";
        Achievement >= 90.0 -> "✓";
        Achievement >= 75.0 -> "~";
        true -> "✗"
    end.

trend_indicator(true, true) -> "✓";
trend_indicator(true, false) -> "↗";
trend_indicator(false, _) -> "✗".

track_indicator(true) -> "[ON TRACK]";
track_indicator(false) -> "[BELOW TARGET]".

priority_value(high) -> 3;
priority_value(medium) -> 2;
priority_value(low) -> 1.

ensure_started() ->
    %% Ensure necessary applications started
    application:ensure_all_started(jsx),
    ok.

usage() ->
    io:format("Usage: kaizen_weekly_report.erl [options]~n"),
    io:format("~n"),
    io:format("Options:~n"),
    io:format("  --date YYYY-MM-DD    Report date (default: today)~n"),
    io:format("  --output FILE        Output file (default: stdout)~n"),
    io:format("  --format json|text   Output format (default: text)~n"),
    io:format("  --email ADDR         Send report via email~n"),
    io:format("  --slack URL          Post to Slack webhook~n"),
    io:format("~n").

%% String multiply helper
'*'(Char, N) when is_integer(Char), N > 0 ->
    lists:duplicate(N, Char);
'*'(_, _) ->
    "".
