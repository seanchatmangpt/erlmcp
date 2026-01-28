%%%-----------------------------------------------------------------------------
%%% @doc TCPS CLI Quality Management
%%%
%%% Handles all quality gates and metrics CLI commands.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(tcps_cli_quality).

-export([run/1]).

-spec run([string()]) -> no_return().

%%%=============================================================================
%%% API
%%%=============================================================================

run(["gates", SkuId | Args]) ->
    check_gates(list_to_binary(SkuId), parse_args(Args));

run(["metrics" | Args]) ->
    show_metrics(parse_args(Args));

run(["dashboard" | _]) ->
    show_dashboard();

run(["report" | _]) ->
    generate_report();

run([]) ->
    tcps_cli_format:error("Missing subcommand. Use: gates, metrics, dashboard, report"),
    halt(1);

run([Unknown | _]) ->
    tcps_cli_format:error("Unknown subcommand: ~s", [Unknown]),
    halt(1).

%%%=============================================================================
%%% Command Implementations
%%%=============================================================================

check_gates(SkuId, Args) ->
    Verbose = maps:get(verbose, Args, false),

    % Check all quality gates for the SKU
    Gates = [
        {compilation, check_compilation_gate(SkuId)},
        {testing, check_testing_gate(SkuId)},
        {validation, check_validation_gate(SkuId)},
        {receipt_chain, check_receipt_chain_gate(SkuId)},
        {andon_clear, check_andon_gate(SkuId)}
    ],

    AllPassed = lists:all(fun({_, Status}) -> Status =:= passed end, Gates),

    case AllPassed of
        true ->
            tcps_cli_format:success("All quality gates passed for SKU: ~s", [SkuId]);
        false ->
            tcps_cli_format:error("Quality gates failed for SKU: ~s", [SkuId])
    end,

    io:format("~n"),
    lists:foreach(fun({Gate, Status}) ->
        print_gate_status(Gate, Status, Verbose)
    end, Gates),
    io:format("~n"),

    case AllPassed of
        true -> halt(0);
        false -> halt(1)
    end.

show_metrics(Args) ->
    Period = case maps:get(period, Args, "weekly") of
        "daily" -> daily;
        "weekly" -> weekly;
        "monthly" -> monthly;
        _ -> weekly
    end,

    Compare = maps:get(compare, Args, false),

    % Get period dates
    {StartDate, EndDate} = get_period_dates(Period),

    % Collect metrics
    Metrics = tcps_kaizen:collect_metrics({StartDate, EndDate}),

    % Get previous period for comparison
    PrevMetrics = case Compare of
        true ->
            {PrevStart, PrevEnd} = get_previous_period_dates(Period),
            tcps_kaizen:collect_metrics({PrevStart, PrevEnd});
        false ->
            undefined
    end,

    % Display metrics
    Format = tcps_cli_config:get(output_format, table),
    case Format of
        json ->
            Output = case Compare of
                true -> #{current => Metrics, previous => PrevMetrics};
                false -> Metrics
            end,
            tcps_cli_format:json(Output);
        _ ->
            print_metrics(Metrics, PrevMetrics)
    end,

    halt(0).

show_dashboard() ->
    tcps_cli_format:error("Interactive dashboard not yet implemented"),
    tcps_cli_format:info("Suggestion: Use 'tcps quality metrics --period weekly --compare'"),
    halt(1).

generate_report() ->
    tcps_cli_format:error("Quality report generation not yet implemented"),
    tcps_cli_format:info("Suggestion: Use 'tcps kaizen report --weekly'"),
    halt(1).

%%%=============================================================================
%%% Helper Functions
%%%=============================================================================

parse_args(Args) ->
    parse_args(Args, #{}).

parse_args([], Acc) ->
    Acc;
parse_args(["--verbose" | Rest], Acc) ->
    parse_args(Rest, Acc#{verbose => true});
parse_args(["--period", Period | Rest], Acc) ->
    parse_args(Rest, Acc#{period => Period});
parse_args(["--bucket", Bucket | Rest], Acc) ->
    parse_args(Rest, Acc#{bucket => Bucket});
parse_args(["--compare" | Rest], Acc) ->
    parse_args(Rest, Acc#{compare => true});
parse_args([Unknown | Rest], Acc) ->
    tcps_cli_format:warning("Unknown argument: ~s", [Unknown]),
    parse_args(Rest, Acc).

check_compilation_gate(SkuId) ->
    % Check for compilation receipt
    ReceiptsDir = tcps_cli_config:get(receipts_path, "./priv/receipts"),
    case find_receipt_for_stage(ReceiptsDir, SkuId, <<"compilation">>) of
        {ok, _Receipt} -> passed;
        _ -> failed
    end.

check_testing_gate(SkuId) ->
    % Check for testing receipt
    ReceiptsDir = tcps_cli_config:get(receipts_path, "./priv/receipts"),
    case find_receipt_for_stage(ReceiptsDir, SkuId, <<"testing">>) of
        {ok, _Receipt} -> passed;
        _ -> failed
    end.

check_validation_gate(SkuId) ->
    % Check for validation receipt (SHACL)
    ReceiptsDir = tcps_cli_config:get(receipts_path, "./priv/receipts"),
    case find_receipt_for_stage(ReceiptsDir, SkuId, <<"validation">>) of
        {ok, _Receipt} -> passed;
        _ -> failed
    end.

check_receipt_chain_gate(SkuId) ->
    % Check that all receipts are present and valid
    ReceiptsDir = tcps_cli_config:get(receipts_path, "./priv/receipts"),
    RequiredStages = [<<"compilation">>, <<"testing">>, <<"validation">>],

    AllPresent = lists:all(fun(Stage) ->
        case find_receipt_for_stage(ReceiptsDir, SkuId, Stage) of
            {ok, _} -> true;
            _ -> false
        end
    end, RequiredStages),

    case AllPresent of
        true -> passed;
        false -> failed
    end.

check_andon_gate(SkuId) ->
    % Check that SKU is not blocked by Andon events
    tcps_andon:start(),
    case tcps_andon:is_blocked(SkuId) of
        false -> passed;
        true -> failed
    end.

find_receipt_for_stage(ReceiptsDir, SkuId, Stage) ->
    case file:list_dir(ReceiptsDir) of
        {ok, Files} ->
            JsonFiles = [F || F <- Files, filename:extension(F) =:= ".json"],
            find_matching_receipt(ReceiptsDir, JsonFiles, SkuId, Stage);
        {error, _} ->
            {error, not_found}
    end.

find_matching_receipt(_Dir, [], _SkuId, _Stage) ->
    {error, not_found};
find_matching_receipt(Dir, [File | Rest], SkuId, Stage) ->
    FullPath = filename:join(Dir, File),
    case file:read_file(FullPath) of
        {ok, JsonBin} ->
            Receipt = jsx:decode(JsonBin, [return_maps]),
            ReceiptSku = maps:get(<<"sku_id">>, Receipt, <<>>),
            ReceiptStage = maps:get(<<"stage">>, Receipt, <<>>),

            case {ReceiptSku, ReceiptStage} of
                {SkuId, Stage} -> {ok, Receipt};
                _ -> find_matching_receipt(Dir, Rest, SkuId, Stage)
            end;
        _ ->
            find_matching_receipt(Dir, Rest, SkuId, Stage)
    end.

print_gate_status(Gate, passed, _Verbose) ->
    io:format("  ✓ ~s: PASSED~n", [Gate]);
print_gate_status(Gate, failed, false) ->
    io:format("  ✗ ~s: FAILED~n", [Gate]);
print_gate_status(Gate, failed, true) ->
    io:format("  ✗ ~s: FAILED~n", [Gate]),
    io:format("      Reason: Gate check failed (use --verbose for details)~n").

get_period_dates(daily) ->
    Today = date(),
    Yesterday = subtract_days(Today, 1),
    {Yesterday, Today};
get_period_dates(weekly) ->
    Today = date(),
    WeekAgo = subtract_days(Today, 7),
    {WeekAgo, Today};
get_period_dates(monthly) ->
    Today = date(),
    MonthAgo = subtract_days(Today, 30),
    {MonthAgo, Today}.

get_previous_period_dates(daily) ->
    {End, _} = get_period_dates(daily),
    Start = subtract_days(End, 1),
    {Start, End};
get_previous_period_dates(weekly) ->
    {End, _} = get_period_dates(weekly),
    Start = subtract_days(End, 7),
    {Start, End};
get_previous_period_dates(monthly) ->
    {End, _} = get_period_dates(monthly),
    Start = subtract_days(End, 30),
    {Start, End}.

subtract_days(Date, Days) ->
    GregorianDays = calendar:date_to_gregorian_days(Date),
    calendar:gregorian_days_to_date(GregorianDays - Days).

print_metrics(Metrics, PrevMetrics) ->
    io:format("~n"),
    io:format("Quality Metrics:~n"),
    io:format("================~n~n"),

    print_metric("Lead Time", maps:get(lead_time, Metrics), "hours",
                 get_prev_value(lead_time, PrevMetrics)),
    print_metric("Defect Rate", maps:get(defect_rate, Metrics), "%",
                 get_prev_value(defect_rate, PrevMetrics)),
    print_metric("Rework", maps:get(rework_pct, Metrics), "%",
                 get_prev_value(rework_pct, PrevMetrics)),
    print_metric("Cycle Time", maps:get(cycle_time, Metrics), "hours",
                 get_prev_value(cycle_time, PrevMetrics)),
    print_metric("First Pass Yield", maps:get(first_pass_yield, Metrics), "%",
                 get_prev_value(first_pass_yield, PrevMetrics)),
    print_metric("Throughput", maps:get(throughput, Metrics), "SKUs/day",
                 get_prev_value(throughput, PrevMetrics)),
    io:format("~n").

print_metric(Name, Value, Unit, undefined) ->
    io:format("  ~-20s: ~.2f ~s~n", [Name, Value, Unit]);
print_metric(Name, Value, Unit, PrevValue) ->
    Change = Value - PrevValue,
    ChangePercent = case PrevValue of
        +0.0 -> +0.0;
        _ -> (Change / PrevValue) * 100
    end,
    Direction = case Change >= 0 of
        true -> "↑";
        false -> "↓"
    end,
    io:format("  ~-20s: ~.2f ~s (~s ~.1f%)~n",
              [Name, Value, Unit, Direction, abs(ChangePercent)]).

get_prev_value(_Key, undefined) -> undefined;
get_prev_value(Key, Metrics) -> maps:get(Key, Metrics, undefined).
