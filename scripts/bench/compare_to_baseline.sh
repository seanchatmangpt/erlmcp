#!/usr/bin/env bash
###===================================================================
### compare_to_baseline.sh - Compare Benchmark Results to Baseline
###===================================================================
###
### Compares benchmark results against baseline for regression detection.
### Analyzes key metrics: throughput, latency, memory usage.
###
### Usage:
###   ./scripts/bench/compare_to_baseline.sh <results_dir> [threshold]
###
### Arguments:
###   results_dir - Directory with current benchmark results
###   threshold   - Max allowed regression % (default: 10)
###
### Exit Codes:
###   0 - No regressions detected
###   1 - Invalid input or baseline not found
###   2 - Regressions detected above threshold
###
### Example:
###   ./scripts/bench/compare_to_baseline.sh bench/results/20260127_190000 10
###
###===================================================================

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
cd "$PROJECT_ROOT"

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
CYAN='\033[0;36m'
NC='\033[0m'

RESULTS_DIR="${1:-}"
THRESHOLD="${2:-10}"
BASELINE_DIR="bench/results/baseline"
COMPARISON_FILE="${RESULTS_DIR}/regression_report.txt"

# Validate input
if [ -z "$RESULTS_DIR" ]; then
    echo -e "${RED}Error: No results directory specified${NC}"
    echo "Usage: $0 <results_dir> [threshold]"
    exit 1
fi

if [ ! -d "$RESULTS_DIR" ]; then
    echo -e "${RED}Error: Results directory not found: $RESULTS_DIR${NC}"
    exit 1
fi

if [ ! -d "$BASELINE_DIR" ] || [ -z "$(ls -A "$BASELINE_DIR" 2>/dev/null)" ]; then
    echo -e "${RED}Error: Baseline not found or empty: $BASELINE_DIR${NC}"
    echo ""
    echo "Set a baseline first:"
    echo "  ./scripts/bench/set_baseline.sh <results_dir>"
    exit 1
fi

echo -e "${BLUE}========================================${NC}"
echo -e "${BLUE}  Regression Analysis${NC}"
echo -e "${BLUE}========================================${NC}"
echo ""
echo -e "${CYAN}Current:${NC}    $RESULTS_DIR"
echo -e "${CYAN}Baseline:${NC}   $BASELINE_DIR"
echo -e "${CYAN}Threshold:${NC}  ${THRESHOLD}%"
echo ""

# Run comparison analysis in Erlang
erl -pa _build/default/lib/*/ebin -noshell -eval "
    BaselineDir = \"$BASELINE_DIR\",
    CurrentDir = \"$RESULTS_DIR\",
    Threshold = $THRESHOLD,

    io:format(\"Loading baseline results...~n\"),
    BaselineFiles = filelib:wildcard(filename:join(BaselineDir, \"*.json\")),
    BaselineResults = lists:filtermap(fun(File) ->
        case file:read_file(File) of
            {ok, Bin} ->
                try jsx:decode(Bin, [return_maps]) of
                    Json ->
                        WorkloadId = maps:get(<<\"workload_id\">>, Json, filename:basename(File, \".json\")),
                        {true, {WorkloadId, Json}}
                catch _:_ -> false
                end;
            _ -> false
        end
    end, BaselineFiles),
    io:format(\"  Loaded ~p baseline results~n\", [length(BaselineResults)]),

    io:format(\"Loading current results...~n\"),
    CurrentFiles = filelib:wildcard(filename:join(CurrentDir, \"*.json\")),
    CurrentResults = lists:filtermap(fun(File) ->
        BaseName = filename:basename(File),
        case BaseName of
            <<\"summary.json\">> -> false;
            _ ->
                case file:read_file(File) of
                    {ok, Bin} ->
                        try jsx:decode(Bin, [return_maps]) of
                            Json ->
                                WorkloadId = maps:get(<<\"workload_id\">>, Json, filename:basename(File, \".json\")),
                                {true, {WorkloadId, Json}}
                        catch _:_ -> false
                        end;
                    _ -> false
                end
        end
    end, CurrentFiles),
    io:format(\"  Loaded ~p current results~n~n\", [length(CurrentResults)]),

    % Helper: Calculate regression percentage
    CalcRegression = fun(Current, Baseline) ->
        if
            Baseline == 0 -> 0.0;
            true -> ((Current - Baseline) / Baseline) * 100
        end
    end,

    % Helper: Extract metric safely
    GetMetric = fun(Result, Key, Default) ->
        case maps:get(Key, Result, Default) of
            V when is_map(V) -> maps:get(<<\"value\">>, V, Default);
            V -> V
        end
    end,

    % Compare results
    io:format(\"~n~s~n\", [string:chars($=, 80)]),
    io:format(\"Benchmark               Metric          Baseline      Current       Change~n\"),
    io:format(\"~s~n\", [string:chars($-, 80)]),

    Regressions = lists:foldl(fun({WorkloadId, Current}, Acc) ->
        case lists:keyfind(WorkloadId, 1, BaselineResults) of
            false ->
                io:format(\"~s: NO BASELINE~n\", [WorkloadId]),
                Acc;
            {_, Baseline} ->
                % Compare key metrics
                Metrics = [
                    {<<\"throughput\">>, <<\"throughput_msg_per_s\">>, higher_is_better},
                    {<<\"latency_p99\">>, <<\"latency_p99_us\">>, lower_is_better},
                    {<<\"memory\">>, <<\"memory_rss_mib_per_node\">>, lower_is_better}
                ],

                WorkloadRegressions = lists:filtermap(fun({Name, Key, Direction}) ->
                    BaselineVal = GetMetric(Baseline, Key, 0),
                    CurrentVal = GetMetric(Current, Key, 0),
                    Regression = CalcRegression(CurrentVal, BaselineVal),

                    % Adjust sign based on direction
                    AdjustedRegression = case Direction of
                        lower_is_better -> Regression;  % Positive = worse
                        higher_is_better -> -Regression  % Negative = worse (inverted)
                    end,

                    % Format output
                    Sign = if AdjustedRegression > 0 -> <<\"+\">>; true -> <<\"\">> end,
                    Color = if
                        AdjustedRegression > Threshold -> \"\\033[0;31m\";  % Red
                        AdjustedRegression > Threshold / 2 -> \"\\033[1;33m\";  % Yellow
                        true -> \"\\033[0;32m\"  % Green
                    end,
                    ResetColor = \"\\033[0m\",

                    io:format(\"~-23s ~-15s ~12.2f ~13.2f ~s~s~.1f%~s~n\",
                        [WorkloadId, Name, BaselineVal, CurrentVal, Color, Sign, AdjustedRegression, ResetColor]),

                    if
                        AdjustedRegression > Threshold ->
                            {true, #{
                                workload => WorkloadId,
                                metric => Name,
                                baseline => BaselineVal,
                                current => CurrentVal,
                                regression => AdjustedRegression
                            }};
                        true ->
                            false
                    end
                end, Metrics),

                Acc ++ WorkloadRegressions
        end
    end, [], CurrentResults),

    io:format(\"~s~n~n\", [string:chars($=, 80)]),

    case Regressions of
        [] ->
            io:format(\"\\033[0;32m✓ No regressions detected (threshold: ~.1f%)\\033[0m~n\", [Threshold]),
            halt(0);
        _ ->
            io:format(\"\\033[0;31m✗ ~p regression(s) detected above ~.1f% threshold:\\033[0m~n\",
                [length(Regressions), Threshold]),
            lists:foreach(fun(R) ->
                io:format(\"  - ~s / ~s: +~.1f% (~.2f → ~.2f)~n\",
                    [maps:get(workload, R), maps:get(metric, R),
                     maps:get(regression, R), maps:get(baseline, R), maps:get(current, R)])
            end, Regressions),
            io:format(\"~n\"),
            halt(2)
    end.
" 2>&1 | tee "$COMPARISON_FILE"

EXIT_CODE=${PIPESTATUS[0]}

echo ""
echo "Comparison report saved to: $COMPARISON_FILE"
echo ""

exit $EXIT_CODE
