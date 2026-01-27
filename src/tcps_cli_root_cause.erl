%%%-----------------------------------------------------------------------------
%%% @doc TCPS CLI Root Cause Analysis (5 Whys)
%%%
%%% Handles all 5 Whys root cause analysis CLI commands.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(tcps_cli_root_cause).

-export([run/1]).

%%%=============================================================================
%%% API
%%%=============================================================================

run(["start", AndonId | Args]) ->
    start_analysis(list_to_binary(AndonId), parse_args(Args));

run(["add-why", AnalysisId, WhyNum, Answer | _]) ->
    add_why(list_to_binary(AnalysisId), list_to_integer(WhyNum), list_to_binary(Answer));

run(["finalize", AnalysisId | Args]) ->
    finalize_analysis(list_to_binary(AnalysisId), parse_args(Args));

run(["show", AnalysisId | _]) ->
    show_analysis(list_to_binary(AnalysisId));

run(["list" | _]) ->
    list_analyses();

run([]) ->
    tcps_cli_format:error("Missing subcommand. Use: start, add-why, finalize, show, list"),
    halt(1);

run([Unknown | _]) ->
    tcps_cli_format:error("Unknown subcommand: ~s", [Unknown]),
    halt(1).

%%%=============================================================================
%%% Command Implementations
%%%=============================================================================

start_analysis(AndonId, Args) ->
    ensure_server_running(),

    Problem = maps:get(problem, Args, <<"Issue from Andon event">>),

    case tcps_root_cause:start_analysis(AndonId, Problem) of
        {ok, AnalysisId} ->
            tcps_cli_format:success("Analysis started: ~s", [AnalysisId]),
            tcps_cli_format:info("Andon Event: ~s", [AndonId]),
            tcps_cli_format:info("~nNext: Run 5 whys analysis:"),
            tcps_cli_format:info("  tcps root-cause add-why ~s 1 \"<answer to why>\"", [AnalysisId]),
            halt(0);
        {error, Reason} ->
            tcps_cli_format:error("Failed to start analysis: ~p", [Reason]),
            halt(1)
    end.

add_why(AnalysisId, WhyNum, Answer) ->
    ensure_server_running(),

    case tcps_root_cause:add_why(AnalysisId, WhyNum, Answer) of
        ok ->
            tcps_cli_format:success("Why #~p added", [WhyNum]),
            case WhyNum of
                5 ->
                    tcps_cli_format:info("~nAll 5 whys completed. Finalize the analysis:"),
                    tcps_cli_format:info("  tcps root-cause finalize ~s \\", [AnalysisId]),
                    tcps_cli_format:info("    --root-cause \"<root cause>\" \\"),
                    tcps_cli_format:info("    --prevention \"<prevention action>\"");
                N ->
                    tcps_cli_format:info("~nNext: Add why #~p:", [N + 1]),
                    tcps_cli_format:info("  tcps root-cause add-why ~s ~p \"<answer>\"", [AnalysisId, N + 1])
            end,
            halt(0);
        {error, not_found} ->
            tcps_cli_format:error("Analysis not found: ~s", [AnalysisId]),
            halt(1);
        {error, Reason} ->
            tcps_cli_format:error("Failed to add why: ~p", [Reason]),
            halt(1)
    end.

finalize_analysis(AnalysisId, Args) ->
    ensure_server_running(),

    case {maps:get(root_cause, Args, undefined), maps:get(prevention, Args, undefined)} of
        {undefined, _} ->
            tcps_cli_format:error("Missing required argument: --root-cause"),
            halt(1);
        {_, undefined} ->
            tcps_cli_format:error("Missing required argument: --prevention"),
            halt(1);
        {RootCause, Prevention} ->
            case tcps_root_cause:finalize_analysis(AnalysisId,
                                                   list_to_binary(RootCause),
                                                   list_to_binary(Prevention)) of
                {ok, Result} ->
                    tcps_cli_format:success("Analysis finalized: ~s", [AnalysisId]),

                    Receipt = maps:get(receipt, Result),
                    PreventionDelta = maps:get(prevention_delta, Result),

                    tcps_cli_format:info("~nRoot Cause: ~s", [RootCause]),
                    tcps_cli_format:info("Prevention: ~s", [Prevention]),

                    % Show prevention delta
                    case maps:get(shacl_additions, PreventionDelta, []) of
                        [] -> ok;
                        ShaclAdds ->
                            tcps_cli_format:info("~nSuggested SHACL constraints:"),
                            lists:foreach(fun(S) ->
                                tcps_cli_format:info("  - ~s", [S])
                            end, ShaclAdds)
                    end,

                    case maps:get(test_additions, PreventionDelta, []) of
                        [] -> ok;
                        TestAdds ->
                            tcps_cli_format:info("~nSuggested test additions:"),
                            lists:foreach(fun(T) ->
                                tcps_cli_format:info("  - ~s", [T])
                            end, TestAdds)
                    end,

                    tcps_cli_format:info("~nReceipt saved: ~s",
                                        [maps:get(receipt_id, Receipt, <<"N/A">>)]),
                    halt(0);
                {error, not_found} ->
                    tcps_cli_format:error("Analysis not found: ~s", [AnalysisId]),
                    halt(1);
                {error, Reason} ->
                    tcps_cli_format:error("Failed to finalize analysis: ~p", [Reason]),
                    halt(1)
            end
    end.

show_analysis(AnalysisId) ->
    ensure_server_running(),

    case tcps_root_cause:get_analysis(AnalysisId) of
        {ok, Analysis} ->
            Format = tcps_cli_config:get(output_format, table),
            case Format of
                json ->
                    tcps_cli_format:json(analysis_to_map(Analysis));
                _ ->
                    print_analysis_details(Analysis)
            end,
            halt(0);
        {error, not_found} ->
            tcps_cli_format:error("Analysis not found: ~s", [AnalysisId]),
            halt(1)
    end.

list_analyses() ->
    ensure_server_running(),

    Analyses = tcps_root_cause:list_analyses(),

    case Analyses of
        [] ->
            io:format("No analyses found.~n"),
            halt(0);
        _ ->
            Format = tcps_cli_config:get(output_format, table),
            Headers = [analysis_id, andon_event_id, status, created_at],
            FormattedData = [format_analysis_summary(A) || A <- Analyses],
            tcps_cli_format:output(FormattedData, Format, #{headers => Headers}),
            halt(0)
    end.

%%%=============================================================================
%%% Helper Functions
%%%=============================================================================

parse_args(Args) ->
    parse_args(Args, #{}).

parse_args([], Acc) ->
    Acc;
parse_args(["--problem", Problem | Rest], Acc) ->
    parse_args(Rest, Acc#{problem => list_to_binary(Problem)});
parse_args(["--root-cause", RC | Rest], Acc) ->
    parse_args(Rest, Acc#{root_cause => RC});
parse_args(["--prevention", Prev | Rest], Acc) ->
    parse_args(Rest, Acc#{prevention => Prev});
parse_args([Unknown | Rest], Acc) ->
    tcps_cli_format:warning("Unknown argument: ~s", [Unknown]),
    parse_args(Rest, Acc).

ensure_server_running() ->
    case whereis(tcps_root_cause) of
        undefined ->
            case tcps_root_cause:start_link() of
                {ok, _Pid} -> ok;
                {error, {already_started, _Pid}} -> ok;
                {error, Reason} ->
                    tcps_cli_format:error("Failed to start root cause server: ~p", [Reason]),
                    halt(1)
            end;
        _Pid ->
            ok
    end.

analysis_to_map(Analysis) ->
    % Convert record to map for JSON output
    #{
        analysis_id => element(2, Analysis),
        andon_event_id => element(3, Analysis),
        problem => element(4, Analysis),
        why_1 => element(5, Analysis),
        why_2 => element(6, Analysis),
        why_3 => element(7, Analysis),
        why_4 => element(8, Analysis),
        why_5 => element(9, Analysis),
        root_cause => element(10, Analysis),
        prevention_action => element(11, Analysis),
        created_at => element(12, Analysis),
        finalized_at => element(14, Analysis),
        status => element(15, Analysis)
    }.

format_analysis_summary(Analysis) ->
    #{
        analysis_id => element(2, Analysis),
        andon_event_id => element(3, Analysis),
        status => element(15, Analysis),
        created_at => tcps_cli_format:format_timestamp(element(12, Analysis))
    }.

print_analysis_details(Analysis) ->
    io:format("~n"),
    io:format("5 Whys Analysis:~n"),
    io:format("================~n"),
    io:format("Analysis ID:   ~s~n", [element(2, Analysis)]),
    io:format("Andon Event:   ~s~n", [element(3, Analysis)]),
    io:format("Problem:       ~s~n", [element(4, Analysis)]),
    io:format("Status:        ~s~n", [element(15, Analysis)]),
    io:format("~n"),
    io:format("Five Whys:~n"),
    io:format("  1. Why? ~s~n", [element(5, Analysis)]),
    io:format("  2. Why? ~s~n", [element(6, Analysis)]),
    io:format("  3. Why? ~s~n", [element(7, Analysis)]),
    io:format("  4. Why? ~s~n", [element(8, Analysis)]),
    io:format("  5. Why? ~s~n", [element(9, Analysis)]),

    case element(10, Analysis) of
        undefined -> ok;
        RootCause ->
            io:format("~n"),
            io:format("Root Cause:    ~s~n", [RootCause]),
            io:format("Prevention:    ~s~n", [element(11, Analysis)])
    end,
    io:format("~n").
