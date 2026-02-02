%%%-------------------------------------------------------------------
%%% @doc
%%% CLI Metrics Test Suite (EUnit)
%%%
%%% Tests for erlmcp_cli_metrics module - Metrics collection and reporting
%%%
%%% Chicago School TDD:
%%% - Tests FIRST, real gen_server for metrics state
%%% - NO mocks, real metrics collection
%%% - State-based verification (metric values, aggregation)
%%%
%%% Coverage Target: ≥90%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_cli_metrics_tests).

-include_lib("eunit/include/eunit.hrl").

%%%====================================================================
%%% Test Fixtures
%%%====================================================================

metrics_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [{"Counter - increment counter", fun test_increment_counter/0},
      {"Counter - increment by value", fun test_increment_by_value/0},
      {"Counter - decrement counter", fun test_decrement_counter/0},
      {"Counter - reset counter", fun test_reset_counter/0},
      {"Counter - get counter value", fun test_get_counter/0},
      {"Counter - multiple counters", fun test_multiple_counters/0},
      {"Gauge - set gauge value", fun test_set_gauge/0},
      {"Gauge - adjust gauge up", fun test_adjust_gauge_up/0},
      {"Gauge - adjust gauge down", fun test_adjust_gauge_down/0},
      {"Gauge - reset gauge", fun test_reset_gauge/0},
      {"Gauge - get gauge value", fun test_get_gauge/0},
      {"Histogram - record value", fun test_record_histogram/0},
      {"Histogram - record multiple values", fun test_record_multiple_histogram/0},
      {"Histogram - get histogram stats", fun test_get_histogram_stats/0},
      {"Histogram - calculate percentiles", fun test_calculate_percentiles/0},
      {"Metrics export - export all metrics", fun test_export_all_metrics/0},
      {"Metrics export - export counters", fun test_export_counters/0},
      {"Metrics export - export gauges", fun test_export_gauges/0},
      {"Metrics export - export histograms", fun test_export_histograms/0},
      {"Concurrent updates - concurrent counter increments", fun test_concurrent_counter_updates/0},
      {"Concurrent updates - concurrent gauge sets", fun test_concurrent_gauge_sets/0},
      {"Metrics reset - reset all metrics", fun test_reset_all_metrics/0},
      {"Metrics reset - reset by pattern", fun test_reset_by_pattern/0},
      {"Metrics aggregation - aggregate counters", fun test_aggregate_counters/0},
      {"Metrics aggregation - aggregate gauges", fun test_aggregate_gauges/0},
      {"OTEL integration - verify OTEL format", fun test_otel_format/0},
      {"Metrics validation - validate metric names", fun test_validate_metric_names/0}]}.

setup() ->
    %% Start required applications
    application:ensure_all_started(crypto),
    application:ensure_all_started(erlmcp_cli),

    %% Reset metrics before each test
    erlmcp_cli_metrics:reset_all_metrics(),
    ok.

cleanup(_Args) ->
    %% Clean up metrics
    erlmcp_cli_metrics:reset_all_metrics(),
    ok.

%%%====================================================================
%%% Counter Tests
%%%====================================================================

test_increment_counter() ->
    %% Increment counter
    ok = erlmcp_cli_metrics:increment_counter(<<"test_counter">>),

    %% Verify value
    ?assertEqual(1, erlmcp_cli_metrics:get_counter_value(<<"test_counter">>)).

test_increment_by_value() ->
    %% Increment by value
    ok = erlmcp_cli_metrics:increment_counter(<<"test_counter_by">>, 5),

    %% Verify value
    ?assertEqual(5, erlmcp_cli_metrics:get_counter_value(<<"test_counter_by">>)).

test_decrement_counter() ->
    %% Set counter
    ok = erlmcp_cli_metrics:increment_counter(<<"test_counter_dec">>, 10),

    %% Decrement
    ok = erlmcp_cli_metrics:decrement_counter(<<"test_counter_dec">>),

    %% Verify value
    ?assertEqual(9, erlmcp_cli_metrics:get_counter_value(<<"test_counter_dec">>)).

test_reset_counter() ->
    %% Set counter
    ok = erlmcp_cli_metrics:increment_counter(<<"test_counter_reset">>, 10),

    %% Reset
    ok = erlmcp_cli_metrics:reset_counter(<<"test_counter_reset">>),

    %% Verify reset
    ?assertEqual(0, erlmcp_cli_metrics:get_counter_value(<<"test_counter_reset">>)).

test_get_counter() ->
    %% Get non-existent counter
    ?assertEqual(0, erlmcp_cli_metrics:get_counter_value(<<"nonexistent_counter">>)).

test_multiple_counters() ->
    %% Increment multiple counters
    ok = erlmcp_cli_metrics:increment_counter(<<"counter1">>),
    ok = erlmcp_cli_metrics:increment_counter(<<"counter2">>),
    ok = erlmcp_cli_metrics:increment_counter(<<"counter3">>, 5),

    %% Verify all counters
    ?assertEqual(1, erlmcp_cli_metrics:get_counter_value(<<"counter1">>)),
    ?assertEqual(1, erlmcp_cli_metrics:get_counter_value(<<"counter2">>)),
    ?assertEqual(5, erlmcp_cli_metrics:get_counter_value(<<"counter3">>)).

%%%====================================================================
%%% Gauge Tests
%%%====================================================================

test_set_gauge() ->
    %% Set gauge
    ok = erlmcp_cli_metrics:set_gauge(<<"test_gauge">>, 42),

    %% Verify value
    ?assertEqual(42, erlmcp_cli_metrics:get_gauge_value(<<"test_gauge">>)).

test_adjust_gauge_up() ->
    %% Set gauge
    ok = erlmcp_cli_metrics:set_gauge(<<"test_gauge_up">>, 10),

    %% Adjust up
    ok = erlmcp_cli_metrics:adjust_gauge(<<"test_gauge_up">>, 5),

    %% Verify value
    ?assertEqual(15, erlmcp_cli_metrics:get_gauge_value(<<"test_gauge_up">>)).

test_adjust_gauge_down() ->
    %% Set gauge
    ok = erlmcp_cli_metrics:set_gauge(<<"test_gauge_down">>, 10),

    %% Adjust down
    ok = erlmcp_cli_metrics:adjust_gauge(<<"test_gauge_down">>, -5),

    %% Verify value
    ?assertEqual(5, erlmcp_cli_metrics:get_gauge_value(<<"test_gauge_down">>)).

test_reset_gauge() ->
    %% Set gauge
    ok = erlmcp_cli_metrics:set_gauge(<<"test_gauge_reset">>, 42),

    %% Reset
    ok = erlmcp_cli_metrics:reset_gauge(<<"test_gauge_reset">>),

    %% Verify reset
    ?assertEqual(0, erlmcp_cli_metrics:get_gauge_value(<<"test_gauge_reset">>)).

test_get_gauge() ->
    %% Get non-existent gauge
    ?assertEqual(0, erlmcp_cli_metrics:get_gauge_value(<<"nonexistent_gauge">>)).

%%%====================================================================
%%% Histogram Tests
%%%====================================================================

test_record_histogram() ->
    %% Record value
    ok = erlmcp_cli_metrics:record_histogram(<<"test_histogram">>, 100),

    %% Verify recorded
    {ok, Stats} = erlmcp_cli_metrics:get_histogram_stats(<<"test_histogram">>),
    ?assertEqual(1, maps:get(<<"count">>, Stats)),
    ?assertEqual(100, maps:get(<<"sum">>, Stats)).

test_record_multiple_histogram() ->
    %% Record multiple values
    Values = [10, 20, 30, 40, 50],
    lists:foreach(fun(V) ->
        erlmcp_cli_metrics:record_histogram(<<"test_histogram_multi">>, V)
    end, Values),

    %% Verify stats
    {ok, Stats} = erlmcp_cli_metrics:get_histogram_stats(<<"test_histogram_multi">>),
    ?assertEqual(5, maps:get(<<"count">>, Stats)),
    ?assertEqual(150, maps:get(<<"sum">>, Stats)),
    ?assertEqual(10, maps:get(<<"min">>, Stats)),
    ?assertEqual(50, maps:get(<<"max">>, Stats)).

test_get_histogram_stats() ->
    %% Record values
    lists:foreach(fun(V) ->
        erlmcp_cli_metrics:record_histogram(<<"test_histogram_stats">>, V)
    end, [5, 15, 25, 35, 45]),

    %% Get stats
    {ok, Stats} = erlmcp_cli_metrics:get_histogram_stats(<<"test_histogram_stats">>),

    %% Verify all stats present
    ?assert(maps:get(<<"count">>, Stats) > 0),
    ?assert(maps:get(<<"sum">>, Stats) > 0),
    ?assert(maps:get(<<"min">>, Stats) > 0),
    ?assert(maps:get(<<"max">>, Stats) > 0),
    ?assert(maps:get(<<"avg">>, Stats) > 0).

test_calculate_percentiles() ->
    %% Record values
    lists:foreach(fun(V) ->
        erlmcp_cli_metrics:record_histogram(<<"test_histogram_pctl">>, V)
    end, lists:seq(1, 100)),

    %% Get percentiles
    {ok, Pctl} = erlmcp_cli_metrics:get_histogram_percentiles(<<"test_histogram_pctl">>, [50, 90, 95, 99]),

    %% Verify percentiles (approximate)
    ?assert(maps:get(<<"p50">>, Pctl) >= 40 andalso maps:get(<<"p50">>, Pctl) =< 60),
    ?assert(maps:get(<<"p90">>, Pctl) >= 85 andalso maps:get(<<"p90">>, Pctl) =< 95),
    ?assert(maps:get(<<"p95">>, Pctl) >= 90 andalso maps:get(<<"p95">>, Pctl) =< 100).

%%%====================================================================
%%% Metrics Export Tests
%%%====================================================================

test_export_all_metrics() ->
    %% Create various metrics
    ok = erlmcp_cli_metrics:increment_counter(<<"counter1">>, 5),
    ok = erlmcp_cli_metrics:set_gauge(<<"gauge1">>, 42),
    ok = erlmcp_cli_metrics:record_histogram(<<"histogram1">>, 100),

    %% Export all
    AllMetrics = erlmcp_cli_metrics:export_metrics(),

    %% Verify structure
    ?assert(is_map(AllMetrics)),
    ?assert(is_map(maps:get(<<"counters">>, AllMetrics))),
    ?assert(is_map(maps:get(<<"gauges">>, AllMetrics))),
    ?assert(is_map(maps:get(<<"histograms">>, AllMetrics))).

test_export_counters() ->
    %% Create counters
    ok = erlmcp_cli_metrics:increment_counter(<<"counter1">>, 5),
    ok = erlmcp_cli_metrics:increment_counter(<<"counter2">>, 10),

    %% Export counters
    Counters = erlmcp_cli_metrics:export_counters(),

    %% Verify
    ?assert(is_map(Counters)),
    ?assertEqual(5, maps:get(<<"counter1">>, Counters)),
    ?assertEqual(10, maps:get(<<"counter2">>, Counters)).

test_export_gauges() ->
    %% Create gauges
    ok = erlmcp_cli_metrics:set_gauge(<<"gauge1">>, 42),
    ok = erlmcp_cli_metrics:set_gauge(<<"gauge2">>, 100),

    %% Export gauges
    Gauges = erlmcp_cli_metrics:export_gauges(),

    %% Verify
    ?assert(is_map(Gauges)),
    ?assertEqual(42, maps:get(<<"gauge1">>, Gauges)),
    ?assertEqual(100, maps:get(<<"gauge2">>, Gauges)).

test_export_histograms() ->
    %% Create histogram
    lists:foreach(fun(V) ->
        erlmcp_cli_metrics:record_histogram(<<"histogram1">>, V)
    end, [10, 20, 30]),

    %% Export histograms
    Histograms = erlmcp_cli_metrics:export_histograms(),

    %% Verify
    ?assert(is_map(Histograms)),
    ?assert(is_map(maps:get(<<"histogram1">>, Histograms))).

%%%====================================================================
%%% Concurrent Updates Tests
%%%====================================================================

test_concurrent_counter_updates() ->
    %% Spawn multiple processes incrementing same counter
    NumProcesses = 10,
    IncrementsPerProcess = 10,

    Pids = [spawn(fun() ->
        lists:foreach(fun(_) ->
            erlmcp_cli_metrics:increment_counter(<<"concurrent_counter">>)
        end, lists:seq(1, IncrementsPerProcess)),
        self() ! done
    end) || _ <- lists:seq(1, NumProcesses)],

    %% Wait for all processes
    [receive
        done -> ok
    after 5000 ->
        ct:fail("Concurrent counter timeout")
    end || _ <- Pids],

    %% Verify final value
    Expected = NumProcesses * IncrementsPerProcess,
    ?assertEqual(Expected, erlmcp_cli_metrics:get_counter_value(<<"concurrent_counter">>)).

test_concurrent_gauge_sets() ->
    %% Spawn multiple processes setting same gauge
    NumProcesses = 10,
    Pids = [spawn(fun() ->
        lists:foreach(fun(N) ->
            erlmcp_cli_metrics:set_gauge(<<"concurrent_gauge">>, N)
        end, lists:seq(1, 10)),
        self() ! done
    end) || _ <- lists:seq(1, NumProcesses)],

    %% Wait for all processes
    [receive
        done -> ok
    after 5000 ->
        ct:fail("Concurrent gauge timeout")
    end || _ <- Pids],

    %% Verify gauge has some value (last write wins)
    Value = erlmcp_cli_metrics:get_gauge_value(<<"concurrent_gauge">>),
    ?assert(Value >= 1 andalso Value =< 10).

%%%====================================================================
%%% Metrics Reset Tests
%%%====================================================================

test_reset_all_metrics() ->
    %% Create various metrics
    ok = erlmcp_cli_metrics:increment_counter(<<"counter1">>, 5),
    ok = erlmcp_cli_metrics:set_gauge(<<"gauge1">>, 42),
    ok = erlmcp_cli_metrics:record_histogram(<<"histogram1">>, 100),

    %% Reset all
    ok = erlmcp_cli_metrics:reset_all_metrics(),

    %% Verify all reset
    ?assertEqual(0, erlmcp_cli_metrics:get_counter_value(<<"counter1">>)),
    ?assertEqual(0, erlmcp_cli_metrics:get_gauge_value(<<"gauge1">>)),
    {error, not_found} = erlmcp_cli_metrics:get_histogram_stats(<<"histogram1">>).

test_reset_by_pattern() ->
    %% Create metrics
    ok = erlmcp_cli_metrics:increment_counter(<<"test.counter1">>, 5),
    ok = erlmcp_cli_metrics:increment_counter(<<"test.counter2">>, 10),
    ok = erlmcp_cli_metrics:increment_counter(<<"other.counter">>, 15),

    %% Reset by pattern
    ok = erlmcp_cli_metrics:reset_by_pattern(<<"test.">>),

    %% Verify pattern-matched metrics reset
    ?assertEqual(0, erlmcp_cli_metrics:get_counter_value(<<"test.counter1">>)),
    ?assertEqual(0, erlmcp_cli_metrics:get_counter_value(<<"test.counter2">>)),
    ?assertEqual(15, erlmcp_cli_metrics:get_counter_value(<<"other.counter">>)).

%%%====================================================================
%%% Metrics Aggregation Tests
%%%====================================================================

test_aggregate_counters() ->
    %% Create counters
    ok = erlmcp_cli_metrics:increment_counter(<<"agg.counter1">>, 5),
    ok = erlmcp_cli_metrics:increment_counter(<<"agg.counter2">>, 10),
    ok = erlmcp_cli_metrics:increment_counter(<<"agg.counter3">>, 15),

    %% Aggregate
    Sum = erlmcp_cli_metrics:aggregate_counters([<<"agg.counter1">>, <<"agg.counter2">>, <<"agg.counter3">>]),

    %% Verify
    ?assertEqual(30, Sum).

test_aggregate_gauges() ->
    %% Create gauges
    ok = erlmcp_cli_metrics:set_gauge(<<"agg.gauge1">>, 10),
    ok = erlmcp_cli_metrics:set_gauge(<<"agg.gauge2">>, 20),
    ok = erlmcp_cli_metrics:set_gauge(<<"agg.gauge3">>, 30),

    %% Aggregate (average)
    Avg = erlmcp_cli_metrics:aggregate_gauges([<<"agg.gauge1">>, <<"agg.gauge2">>, <<"agg.gauge3">>], avg),

    %% Verify
    ?assertEqual(20, Avg).

%%%====================================================================
%%% OTEL Integration Tests
%%%====================================================================

test_otel_format() ->
    %% Create metrics
    ok = erlmcp_cli_metrics:increment_counter(<<"otel_counter">>, 5),
    ok = erlmcp_cli_metrics:set_gauge(<<"otel_gauge">>, 42),

    %% Export in OTEL format
    OtelMetrics = erlmcp_cli_metrics:export_otel_format(),

    %% Verify OTEL format
    ?assert(is_map(OtelMetrics)),
    ?assert(is_list(maps:get(<<"resource_metrics">>, OtelMetrics, []))).

%%%====================================================================
%%% Metrics Validation Tests
%%%====================================================================

test_validate_metric_names() ->
    %% Valid metric names
    ValidNames = [
        <<"test_metric">>,
        <<"test.metric">>,
        <<"test-metric">>,
        <<"test_metric_123">>
    ],

    lists:foreach(fun(Name) ->
        ?assertEqual(true, erlmcp_cli_metrics:validate_metric_name(Name))
    end, ValidNames),

    %% Invalid metric names
    InvalidNames = [
        <<"">>,  %% Empty
        <<"123_metric">>,  %% Starts with number
        <<"Test Metric">>  %% Contains space
    ],

    lists:foreach(fun(Name) ->
        ?assertEqual(false, erlmcp_cli_metrics:validate_metric_name(Name))
    end, InvalidNames).
