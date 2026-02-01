#!/usr/bin/env bash
###===================================================================
### compare_to_baseline.sh - Enhanced Benchmark Regression Detection
###===================================================================
###
### Compares benchmark results against baseline with configurable thresholds.
### Analyzes key metrics: throughput, latency, memory usage.
###
### Usage:
###   ./scripts/bench/compare_to_baseline.sh [OPTIONS]
###
### Options:
###   --results=FILE      Path to current benchmark results JSON
###   --baseline=FILE     Path to baseline benchmark results JSON
###   --thresholds=FILE   Path to thresholds configuration JSON
###   --output=FILE       Output path for regression report (default: stdout)
###   --format=FORMAT     Output format: json, text, markdown (default: text)
###   --threshold=PCT     Global regression threshold % (default: 10)
###   --help              Show this help message
###
### Backward Compatibility:
###   ./scripts/bench/compare_to_baseline.sh <results_dir> [threshold]
###
### Exit Codes:
###   0 - No regressions detected
###   1 - Invalid input or baseline not found
###   2 - Regressions detected above threshold
###
### Example:
###   ./scripts/bench/compare_to_baseline.sh \
###     --results=bench/results/current.json \
###     --baseline=bench/results/baseline.json \
###     --thresholds=.github/performance-thresholds.json \
###     --output=regression_report.json \
###     --format=json
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

# Default configuration
RESULTS_FILE=""
BASELINE_FILE=""
THRESHOLDS_FILE=".github/performance-thresholds.json"
OUTPUT_FILE=""
OUTPUT_FORMAT="text"
GLOBAL_THRESHOLD=10
LEGACY_MODE=false

# Parse command line arguments
if [[ $# -eq 0 ]]; then
    echo -e "${RED}Error: No arguments provided${NC}"
    echo "Use --help for usage information"
    exit 1
elif [[ ! "$1" =~ ^-- ]]; then
    # Legacy mode: ./compare_to_baseline.sh <results_dir> [threshold]
    LEGACY_MODE=true
    RESULTS_DIR="${1:-}"
    GLOBAL_THRESHOLD="${2:-10}"
    BASELINE_DIR="bench/results/baseline"
    OUTPUT_FILE="${RESULTS_DIR}/regression_report.txt"
else
    # New mode: --flag=value
    while [[ $# -gt 0 ]]; do
        case $1 in
            --results=*)
                RESULTS_FILE="${1#*=}"
                shift
                ;;
            --baseline=*)
                BASELINE_FILE="${1#*=}"
                shift
                ;;
            --thresholds=*)
                THRESHOLDS_FILE="${1#*=}"
                shift
                ;;
            --output=*)
                OUTPUT_FILE="${1#*=}"
                shift
                ;;
            --format=*)
                OUTPUT_FORMAT="${1#*=}"
                shift
                ;;
            --threshold=*)
                GLOBAL_THRESHOLD="${1#*=}"
                shift
                ;;
            --help)
                grep "^###" "$0" | sed 's/^### \?//'
                exit 0
                ;;
            *)
                echo -e "${RED}Error: Unknown option: $1${NC}"
                echo "Use --help for usage information"
                exit 1
                ;;
        esac
    done
fi

# Validation
if [ "$LEGACY_MODE" = true ]; then
    # Legacy mode validation
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

    # Convert legacy arguments to new format
    RESULTS_FILE="$RESULTS_DIR"
    BASELINE_FILE="$BASELINE_DIR"
else
    # New mode validation
    if [ -z "$RESULTS_FILE" ]; then
        echo -e "${RED}Error: --results is required${NC}"
        exit 1
    fi

    if [ -z "$BASELINE_FILE" ]; then
        echo -e "${RED}Error: --baseline is required${NC}"
        exit 1
    fi

    # Check if files/directories exist
    if [ ! -e "$RESULTS_FILE" ]; then
        echo -e "${RED}Error: Results file/directory not found: $RESULTS_FILE${NC}"
        exit 1
    fi

    if [ ! -e "$BASELINE_FILE" ]; then
        echo -e "${RED}Error: Baseline file/directory not found: $BASELINE_FILE${NC}"
        exit 1
    fi
fi

# Load thresholds if file exists
THRESHOLD_LATENCY_P50=10
THRESHOLD_LATENCY_P95=15
THRESHOLD_LATENCY_P99=20
THRESHOLD_THROUGHPUT=10
THRESHOLD_MEMORY=20

if [ -f "$THRESHOLDS_FILE" ]; then
    echo -e "${BLUE}Loading thresholds from: $THRESHOLDS_FILE${NC}"
    THRESHOLD_LATENCY_P50=$(jq -r '.thresholds.latency.p50.threshold_percent // 10' "$THRESHOLDS_FILE" 2>/dev/null || echo "10")
    THRESHOLD_LATENCY_P95=$(jq -r '.thresholds.latency.p95.threshold_percent // 15' "$THRESHOLDS_FILE" 2>/dev/null || echo "15")
    THRESHOLD_LATENCY_P99=$(jq -r '.thresholds.latency.p99.threshold_percent // 20' "$THRESHOLDS_FILE" 2>/dev/null || echo "20")
    THRESHOLD_THROUGHPUT=$(jq -r '.thresholds.throughput.threshold_percent // 10' "$THRESHOLDS_FILE" 2>/dev/null || echo "10")
    THRESHOLD_MEMORY=$(jq -r '.thresholds.memory.total.threshold_percent // 20' "$THRESHOLDS_FILE" 2>/dev/null || echo "20")
else
    echo -e "${YELLOW}Warning: Thresholds file not found, using defaults${NC}"
fi

# Output redirection setup
if [ -n "$OUTPUT_FILE" ]; then
    # Create output directory if needed
    OUTPUT_DIR=$(dirname "$OUTPUT_FILE")
    mkdir -p "$OUTPUT_DIR"
    exec > >(tee "$OUTPUT_FILE")
fi

echo -e "${BLUE}========================================${NC}"
echo -e "${BLUE}  Regression Analysis${NC}"
echo -e "${BLUE}========================================${NC}"
echo ""
echo -e "${CYAN}Current:${NC}    $RESULTS_FILE"
echo -e "${CYAN}Baseline:${NC}   $BASELINE_FILE"
echo -e "${CYAN}Thresholds:${NC} Latency P50=$THRESHOLD_LATENCY_P50%, P95=$THRESHOLD_LATENCY_P95%, P99=$THRESHOLD_LATENCY_P99%; Throughput=$THRESHOLD_THROUGHPUT%; Memory=$THRESHOLD_MEMORY%"
echo -e "${CYAN}Format:${NC}     $OUTPUT_FORMAT"
echo ""

# Determine if we're working with directories or files
if [ -d "$RESULTS_FILE" ]; then
    RESULTS_FILES=$(find "$RESULTS_FILE" -name "*.json" -type f ! -name "summary.json" 2>/dev/null)
else
    RESULTS_FILES="$RESULTS_FILE"
fi

if [ -d "$BASELINE_FILE" ]; then
    BASELINE_FILES=$(find "$BASELINE_FILE" -name "*.json" -type f ! -name "summary.json" 2>/dev/null)
else
    BASELINE_FILES="$BASELINE_FILE"
fi

# Run comparison analysis in Erlang
TEMP_RESULT=$(mktemp)
trap "rm -f '$TEMP_RESULT'" EXIT

erl -pa _build/default/lib/*/ebin -noshell -eval "
    % Helper: Load JSON files from directory or single file
    LoadResults = fun(PathOrDir) ->
        case filelib:is_dir(PathOrDir) of
            true ->
                Files = filelib:wildcard(filename:join(PathOrDir, \"*.json\")),
                lists:filtermap(fun(File) ->
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
                end, Files);
            false ->
                case file:read_file(PathOrDir) of
                    {ok, Bin} ->
                        try jsx:decode(Bin, [return_maps]) of
                            Json when is_map(Json) ->
                                WorkloadId = maps:get(<<\"workload_id\">>, Json, <<\"benchmark\">>),
                                [{WorkloadId, Json}];
                            JsonList when is_list(JsonList) ->
                                lists:map(fun(J) ->
                                    WorkloadId = maps:get(<<\"workload_id\">>, J, <<\"benchmark\">>),
                                    {WorkloadId, J}
                                end, JsonList)
                        catch _:_ -> []
                        end;
                    _ -> []
                end
        end
    end,

    io:format(\"Loading baseline results...~n\"),
    BaselineResults = LoadResults(\"$BASELINE_FILE\"),
    io:format(\"  Loaded ~p baseline results~n\", [length(BaselineResults)]),

    io:format(\"Loading current results...~n\"),
    CurrentResults = LoadResults(\"$RESULTS_FILE\"),
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

    % Helper: Get threshold for metric
    GetThreshold = fun(MetricKey) ->
        case MetricKey of
            <<\"latency_p50_us\">> -> $THRESHOLD_LATENCY_P50;
            <<\"latency_p95_us\">> -> $THRESHOLD_LATENCY_P95;
            <<\"latency_p99_us\">> -> $THRESHOLD_LATENCY_P99;
            <<\"throughput_msg_per_s\">> -> $THRESHOLD_THROUGHPUT;
            <<\"memory_rss_mib_per_node\">> -> $THRESHOLD_MEMORY;
            _ -> $GLOBAL_THRESHOLD
        end
    end,

    % Compare results
    io:format(\"~n~s~n\", [string:chars(\$=, 80)]),
    io:format(\"Benchmark               Metric          Baseline      Current       Change~n\"),
    io:format(\"~s~n\", [string:chars(\$-, 80)]),

    Regressions = lists:foldl(fun({WorkloadId, Current}, Acc) ->
        case lists:keyfind(WorkloadId, 1, BaselineResults) of
            false ->
                io:format(\"~s: NO BASELINE~n\", [WorkloadId]),
                Acc;
            {_, Baseline} ->
                % Compare key metrics
                Metrics = [
                    {<<\"throughput\">>, <<\"throughput_msg_per_s\">>, higher_is_better},
                    {<<\"latency_p50\">>, <<\"latency_p50_us\">>, lower_is_better},
                    {<<\"latency_p95\">>, <<\"latency_p95_us\">>, lower_is_better},
                    {<<\"latency_p99\">>, <<\"latency_p99_us\">>, lower_is_better},
                    {<<\"memory\">>, <<\"memory_rss_mib_per_node\">>, lower_is_better}
                ],

                WorkloadRegressions = lists:filtermap(fun({Name, Key, Direction}) ->
                    BaselineVal = GetMetric(Baseline, Key, 0),
                    CurrentVal = GetMetric(Current, Key, 0),

                    if
                        BaselineVal == 0 andalso CurrentVal == 0 -> false;
                        true ->
                            Regression = CalcRegression(CurrentVal, BaselineVal),
                            Threshold = GetThreshold(Key),

                            % Adjust sign based on direction
                            AdjustedRegression = case Direction of
                                lower_is_better -> Regression;
                                higher_is_better -> -Regression
                            end,

                            % Format output
                            Sign = if AdjustedRegression > 0 -> <<\"+\">>; true -> <<\"\">> end,
                            Color = if
                                AdjustedRegression > Threshold -> \"\\033[0;31m\";
                                AdjustedRegression > Threshold / 2 -> \"\\033[1;33m\";
                                true -> \"\\033[0;32m\"
                            end,
                            ResetColor = \"\\033[0m\",

                            io:format(\"~-23s ~-15s ~12.2f ~13.2f ~s~s~.1f%~s~n\",
                                [WorkloadId, Name, BaselineVal, CurrentVal, Color, Sign, AdjustedRegression, ResetColor]),

                            if
                                AdjustedRegression > Threshold ->
                                    Severity = if
                                        AdjustedRegression > Threshold * 2 -> critical;
                                        AdjustedRegression > Threshold * 1.5 -> high;
                                        true -> medium
                                    end,
                                    {true, #{
                                        workload => WorkloadId,
                                        metric => Name,
                                        metric_key => Key,
                                        baseline => BaselineVal,
                                        current => CurrentVal,
                                        regression_percent => AdjustedRegression,
                                        threshold_percent => Threshold,
                                        severity => Severity,
                                        direction => Direction
                                    }};
                                true ->
                                    false
                            end
                    end
                end, Metrics),

                Acc ++ WorkloadRegressions
        end
    end, [], CurrentResults),

    io:format(\"~s~n~n\", [string:chars(\$=, 80)]),

    % Generate output based on format
    OutputFormat = \"$OUTPUT_FORMAT\",

    case OutputFormat of
        \"json\" ->
            JsonOutput = jsx:encode(#{
                timestamp => erlang:system_time(second),
                results_file => <<\"$RESULTS_FILE\">>,
                baseline_file => <<\"$BASELINE_FILE\">>,
                thresholds_file => <<\"$THRESHOLDS_FILE\">>,
                regressions => Regressions,
                summary => #{
                    total_comparisons => length(CurrentResults),
                    regressions_detected => length(Regressions),
                    status => case Regressions of
                        [] -> <<"passed">>;
                        _ -> <<"failed">>
                    end
                }
            }, [{space, 1}, {indent, 2}]),
            io:format(\"~s~n\", [JsonOutput]);
        _ ->
            case Regressions of
                [] ->
                    io:format(\"\\033[0;32m✓ No regressions detected\\033[0m~n\");
                _ ->
                    io:format(\"\\033[0;31m✗ ~p regression(s) detected:\\033[0m~n\", [length(Regressions)]),
                    lists:foreach(fun(R) ->
                        Severity = maps:get(severity, R),
                        SeverityColor = case Severity of
                            critical -> \"\\033[0;31m\";
                            high -> \"\\033[1;33m\";
                            _ -> \"\\033[0;33m\"
                        end,
                        io:format(\"  ~s[~s]\\033[0m ~s / ~s: +~.1f% (~.2f → ~.2f, threshold: ~.1f%)~n\",
                            [SeverityColor, string:uppercase(atom_to_list(Severity)),
                             maps:get(workload, R), maps:get(metric, R),
                             maps:get(regression_percent, R), maps:get(baseline, R),
                             maps:get(current, R), maps:get(threshold_percent, R)])
                    end, Regressions)
            end
    end,

    case Regressions of
        [] -> halt(0);
        _ -> halt(2)
    end.
" 2>&1 | tee -a "$TEMP_RESULT"

EXIT_CODE=${PIPESTATUS[0]}

if [ -n "$OUTPUT_FILE" ] && [ "$OUTPUT_FORMAT" != "json" ]; then
    echo ""
    echo "Comparison report saved to: $OUTPUT_FILE"
fi

exit $EXIT_CODE
