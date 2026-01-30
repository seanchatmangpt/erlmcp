#!/usr/bin/env escript
%% Test percentile calculation

main(_) ->
    Values = [10, 20, 30, 40, 50, 60, 70, 80, 90, 100],
    Sorted = lists:sort(Values),
    Len = length(Sorted),

    % Test current algorithm
    P50_Index = max(1, round(Len * 0.50)),
    P50 = lists:nth(min(P50_Index, Len), Sorted),
    io:format("Current Algorithm:~n"),
    io:format("  P50:  Index=~p, Value=~p (Expected: 50)~n", [P50_Index, P50]),

    P95_Index = max(1, round(Len * 0.95)),
    P95 = lists:nth(min(P95_Index, Len), Sorted),
    io:format("  P95:  Index=~p, Value=~p (Expected: 95)~n", [P95_Index, P95]),

    P99_Index = max(1, round(Len * 0.99)),
    P99 = lists:nth(min(P99_Index, Len), Sorted),
    io:format("  P99:  Index=~p, Value=~p (Expected: 99)~n", [P99_Index, P99]),

    P999_Index = max(1, round(Len * 0.999)),
    P999 = lists:nth(min(P999_Index, Len), Sorted),
    io:format("  P999: Index=~p, Value=~p (Expected: 99.9)~n~n", [P999_Index, P999]),

    % Test with proper linear interpolation algorithm
    io:format("Linear Interpolation Algorithm:~n"),
    P50_Proper = percentile_linear(Sorted, 0.50),
    io:format("  P50:  ~p (Expected: 55)~n", [P50_Proper]),

    P95_Proper = percentile_linear(Sorted, 0.95),
    io:format("  P95:  ~p (Expected: 95)~n", [P95_Proper]),

    P99_Proper = percentile_linear(Sorted, 0.99),
    io:format("  P99:  ~p (Expected: 99)~n", [P99_Proper]),

    P999_Proper = percentile_linear(Sorted, 0.999),
    io:format("  P999: ~p (Expected: 99.9)~n~n", [P999_Proper]),

    % Test edge cases
    io:format("Edge Cases:~n"),
    Empty = [],
    EmptyResult = calculate_percentiles(Empty),
    io:format("  Empty list: ~p~n", [EmptyResult]),

    Single = [50],
    SingleResult = calculate_percentiles(Single),
    io:format("  Single value [50]: ~p~n", [SingleResult]),

    TwoValues = [10, 100],
    TwoResult = calculate_percentiles(TwoValues),
    io:format("  Two values [10, 100]: ~p~n", [TwoResult]),

    % Large dataset test
    Large = lists:seq(1, 1000),
    LargeResult = calculate_percentiles(Large),
    io:format("~n  Large dataset (1-1000):~n"),
    io:format("    P50: ~p (Expected: 500 or 501)~n", [maps:get(p50, LargeResult)]),
    io:format("    P95: ~p (Expected: 950 or 951)~n", [maps:get(p95, LargeResult)]),
    io:format("    P99: ~p (Expected: 990 or 991)~n", [maps:get(p99, LargeResult)]),
    io:format("    P999: ~p (Expected: 999 or 1000)~n", [maps:get(p999, LargeResult)]).

%% Current algorithm (nearest-rank)
percentile_current(Sorted, Len, Percent) ->
    Index = max(1, round(Len * Percent)),
    lists:nth(min(Index, Len), Sorted).

%% Linear interpolation algorithm (more accurate)
percentile_linear(Sorted, Percent) when is_list(Sorted), length(Sorted) > 0 ->
    Len = length(Sorted),
    case Len of
        1 ->
            hd(Sorted);
        _ ->
            % Use linear interpolation between closest ranks
            % Position = 1 + (N - 1) * p
            Pos = 1.0 + (Len - 1) * Percent,
            LowerIdx = max(1, floor(Pos)),
            UpperIdx = min(Len, ceil(Pos)),
            LowerVal = lists:nth(LowerIdx, Sorted),
            UpperVal = lists:nth(UpperIdx, Sorted),
            if LowerIdx =/= UpperIdx -> % Need interpolation
                Fraction = Pos - LowerIdx,
                LowerVal + Fraction * (UpperVal - LowerVal);
            true -> % Exact index
                LowerVal
            end
    end.

%% Calculate all percentiles using current algorithm
calculate_percentiles([]) ->
    #{p50 => 0, p95 => 0, p99 => 0, p999 => 0};
calculate_percentiles(Values) ->
    Sorted = lists:sort(Values),
    Len = length(Sorted),
    #{
        p50 => percentile_current(Sorted, Len, 0.50),
        p95 => percentile_current(Sorted, Len, 0.95),
        p99 => percentile_current(Sorted, Len, 0.99),
        p999 => percentile_current(Sorted, Len, 0.999)
    }.
