#!/usr/bin/env escript
%% Calculate correct percentile expectations

main(_) ->
    Values = [10, 20, 30, 40, 50, 60, 70, 80, 90, 100],
    Sorted = lists:sort(Values),
    Len = length(Sorted),

    io:format("Dataset: ~p~n", [Values]),
    io:format("Length: ~p~n~n", [Len]),

    P50 = calculate_percentile(Sorted, Len, 0.50),
    io:format("P50:  ~p (should be 55.0)~n", [P50]),

    P95 = calculate_percentile(Sorted, Len, 0.95),
    io:format("P95:  ~p (should be ~p)~n", [P95, P95]),

    P99 = calculate_percentile(Sorted, Len, 0.99),
    io:format("P99:  ~p (should be ~p)~n", [P99, P99]),

    P999 = calculate_percentile(Sorted, Len, 0.999),
    io:format("P999: ~p (should be ~p)~n", [P999, P999]),

    % Test with larger dataset (more realistic)
    Large = lists:seq(1, 100),
    LargeSorted = lists:sort(Large),
    LargeLen = length(LargeSorted),

    io:format("~nLarge dataset (1-100):~n"),
    P50_L = calculate_percentile(LargeSorted, LargeLen, 0.50),
    io:format("  P50:  ~p~n", [P50_L]),
    P95_L = calculate_percentile(LargeSorted, LargeLen, 0.95),
    io:format("  P95:  ~p~n", [P95_L]),
    P99_L = calculate_percentile(LargeSorted, LargeLen, 0.99),
    io:format("  P99:  ~p~n", [P99_L]),
    P999_L = calculate_percentile(LargeSorted, LargeLen, 0.999),
    io:format("  P999: ~p~n", [P999_L]).

calculate_percentile(Sorted, Len, Percent) when length(Sorted) =:= 1 ->
    hd(Sorted);
calculate_percentile(Sorted, Len, Percent) ->
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
