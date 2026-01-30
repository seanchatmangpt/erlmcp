-module(test_percentile_standalone).
-include_lib("eunit/include/eunit.hrl").

%% Test basic percentile calculation
percentile_basic_test() ->
    Values = [10, 20, 30, 40, 50, 60, 70, 80, 90, 100],
    Percentiles = erlmcp_metrics_aggregator:get_percentiles(Values),

    ?assertEqual(55.0, maps:get(p50, Percentiles)),
    P95 = maps:get(p95, Percentiles),
    ?assert(P95 > 95.0 andalso P95 < 96.0),
    P99 = maps:get(p99, Percentiles),
    ?assert(P99 > 99.0 andalso P99 < 100.0),
    P999 = maps:get(p999, Percentiles),
    ?assert(P999 > 99.9 andalso P999 =< 100.0).

%% Test edge cases
percentile_empty_test() ->
    Percentiles = erlmcp_metrics_aggregator:get_percentiles([]),
    ?assertEqual(0, maps:get(p50, Percentiles)),
    ?assertEqual(0, maps:get(p95, Percentiles)),
    ?assertEqual(0, maps:get(p99, Percentiles)),
    ?assertEqual(0, maps:get(p999, Percentiles)).

percentile_single_test() ->
    Percentiles = erlmcp_metrics_aggregator:get_percentiles([42]),
    ?assertEqual(42, maps:get(p50, Percentiles)),
    ?assertEqual(42, maps:get(p95, Percentiles)),
    ?assertEqual(42, maps:get(p99, Percentiles)),
    ?assertEqual(42, maps:get(p999, Percentiles)).

percentile_two_values_test() ->
    Percentiles = erlmcp_metrics_aggregator:get_percentiles([10, 100]),
    ?assertEqual(55.0, maps:get(p50, Percentiles)),
    ?assertEqual(95.0, maps:get(p95, Percentiles)),
    ?assertEqual(99.0, maps:get(p99, Percentiles)),
    ?assertEqual(99.9, maps:get(p999, Percentiles)).

percentile_large_dataset_test() ->
    Values = lists:seq(1, 100),
    Percentiles = erlmcp_metrics_aggregator:get_percentiles(Values),
    ?assertEqual(50.5, maps:get(p50, Percentiles)),
    P95 = maps:get(p95, Percentiles),
    ?assert(P95 > 95.0 andalso P95 < 96.0),
    P99 = maps:get(p99, Percentiles),
    ?assert(P99 > 99.0 andalso P99 < 100.0).

%% Test monotonicity - percentiles should be increasing
percentile_monotonic_test() ->
    Values = lists:seq(1, 1000),
    Percentiles = erlmcp_metrics_aggregator:get_percentiles(Values),
    P50 = maps:get(p50, Percentiles),
    P95 = maps:get(p95, Percentiles),
    P99 = maps:get(p99, Percentiles),
    P999 = maps:get(p999, Percentiles),
    ?assert(P50 =< P95),
    ?assert(P95 =< P99),
    ?assert(P99 =< P999).
