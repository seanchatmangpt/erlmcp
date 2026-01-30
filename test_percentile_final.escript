#!/usr/bin/env escript
%% Verify percentile calculation with new algorithm

%% Module declarations must come before function definitions
-module(erlmcp_metrics_aggregator).
-export([get_percentiles/1]).

main(_) ->
    % Test basic percentile calculation
    Values = [10, 20, 30, 40, 50, 60, 70, 80, 90, 100],
    Percentiles = get_percentiles(Values),

    io:format("Testing percentile calculation:~n"),
    io:format("  Dataset: ~p~n", [Values]),
    io:format("  P50:  ~p (expected: 55.0)~n", [maps:get(p50, Percentiles)]),
    io:format("  P95:  ~p (expected: approx 95.5)~n", [maps:get(p95, Percentiles)]),
    io:format("  P99:  ~p (expected: approx 99.1)~n", [maps:get(p99, Percentiles)]),
    io:format("  P999: ~p (expected: approx 99.91)~n~n", [maps:get(p999, Percentiles)]),

    % Test edge cases
    io:format("Testing edge cases:~n"),

    Empty = get_percentiles([]),
    io:format("  Empty: ~p~n", [Empty]),

    Single = get_percentiles([42]),
    io:format("  Single [42]: ~p~n", [Single]),

    Two = get_percentiles([10, 100]),
    io:format("  Two [10, 100]: ~p~n", [Two]),

    Large = get_percentiles(lists:seq(1, 100)),
    io:format("  Large (1-100):~n"),
    io:format("    P50:  ~p~n", [maps:get(p50, Large)]),
    io:format("    P95:  ~p~n", [maps:get(p95, Large)]),
    io:format("    P99:  ~p~n", [maps:get(p99, Large)]),
    io:format("    P999: ~p~n", [maps:get(p999, Large)]).

get_percentiles([]) ->
    #{p50 => 0, p95 => 0, p99 => 0, p999 => 0};
get_percentiles(Values) ->
    Sorted = lists:sort(Values),
    Len = length(Sorted),
    #{
        p50 => percentile(Sorted, Len, 0.50),
        p95 => percentile(Sorted, Len, 0.95),
        p99 => percentile(Sorted, Len, 0.99),
        p999 => percentile(Sorted, Len, 0.999)
    }.

percentile([SingleValue], _Len, _Percent) ->
    SingleValue;
percentile(Sorted, Len, Percent) ->
    Pos = 1.0 + (Len - 1) * Percent,
    LowerIdx = max(1, floor(Pos)),
    UpperIdx = min(Len, ceil(Pos)),
    LowerVal = lists:nth(LowerIdx, Sorted),
    UpperVal = lists:nth(UpperIdx, Sorted),
    if LowerIdx =:= UpperIdx ->
        LowerVal;
    true ->
        Fraction = Pos - LowerIdx,
        LowerVal + Fraction * (UpperVal - LowerVal)
    end.
