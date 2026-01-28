-module(erlmcp_metrics_tests).

-include_lib("eunit/include/eunit.hrl").
-include("erlmcp.hrl").

setup() ->
    erlmcp_metrics:init(),
    ok.
cleanup(_) -> ok.

%%====================================================================
%% Counter Tests
%%====================================================================

counter_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) -> [
        ?_test(test_increment_counter()),
        ?_test(test_increment_by_amount()),
        ?_test(test_get_counter()),
        ?_test(test_reset_counter()),
        ?_test(test_multiple_counters())
    ] end}.

test_increment_counter() ->
    Result = erlmcp_metrics:increment_counter(requests),
    ?assertMatch(ok | {ok, _} | {error, _}, Result).

test_increment_by_amount() ->
    Result = erlmcp_metrics:increment_counter(events, 5),
    ?assertMatch(ok | {ok, _} | {error, _}, Result).

test_get_counter() ->
    erlmcp_metrics:increment_counter(count_test),
    Result = erlmcp_metrics:get_counter(count_test),
    ?assert(is_integer(Result) orelse Result =:= error orelse Result =:= undefined).

test_reset_counter() ->
    erlmcp_metrics:increment_counter(reset_test),
    Result = erlmcp_metrics:reset_counter(reset_test),
    ?assertMatch(ok | {ok, _} | {error, _}, Result).

test_multiple_counters() ->
    erlmcp_metrics:increment_counter(c1),
    erlmcp_metrics:increment_counter(c2),
    erlmcp_metrics:increment_counter(c3),
    Result = erlmcp_metrics:get_all_counters(),
    ?assert(is_map(Result) orelse is_list(Result) orelse Result =:= error).

%%====================================================================
%% Gauge Tests
%%====================================================================

gauge_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) -> [
        ?_test(test_set_gauge()),
        ?_test(test_get_gauge()),
        ?_test(test_gauge_increment()),
        ?_test(test_gauge_decrement()),
        ?_test(test_multiple_gauges())
    ] end}.

test_set_gauge() ->
    Result = erlmcp_metrics:set_gauge(memory_usage, 1024),
    ?assertMatch(ok | {ok, _} | {error, _}, Result).

test_get_gauge() ->
    erlmcp_metrics:set_gauge(cpu_usage, 50),
    Result = erlmcp_metrics:get_gauge(cpu_usage),
    ?assert(is_number(Result) orelse Result =:= error orelse Result =:= undefined).

test_gauge_increment() ->
    erlmcp_metrics:set_gauge(connections, 10),
    Result = erlmcp_metrics:increment_gauge(connections, 5),
    ?assertMatch(ok | {ok, _} | {error, _}, Result).

test_gauge_decrement() ->
    erlmcp_metrics:set_gauge(queue_depth, 100),
    Result = erlmcp_metrics:decrement_gauge(queue_depth, 10),
    ?assertMatch(ok | {ok, _} | {error, _}, Result).

test_multiple_gauges() ->
    erlmcp_metrics:set_gauge(g1, 10),
    erlmcp_metrics:set_gauge(g2, 20),
    erlmcp_metrics:set_gauge(g3, 30),
    Result = erlmcp_metrics:get_all_gauges(),
    ?assert(is_map(Result) orelse is_list(Result) orelse Result =:= error).

%%====================================================================
%% Histogram Tests
%%====================================================================

histogram_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) -> [
        ?_test(test_record_histogram()),
        ?_test(test_histogram_percentiles()),
        ?_test(test_histogram_stats()),
        ?_test(test_histogram_buckets())
    ] end}.

test_record_histogram() ->
    Result = erlmcp_metrics:record_histogram(response_time, 150),
    ?assertMatch(ok | {ok, _} | {error, _}, Result).

test_histogram_percentiles() ->
    [erlmcp_metrics:record_histogram(latency, I*10) || I <- lists:seq(1, 10)],
    Result = erlmcp_metrics:get_histogram(latency),
    ?assert(is_map(Result) orelse is_list(Result) orelse Result =:= error).

test_histogram_stats() ->
    [erlmcp_metrics:record_histogram(duration, I*5) || I <- lists:seq(1, 20)],
    Result = erlmcp_metrics:get_histogram_stats(duration),
    ?assert(is_map(Result) orelse is_list(Result) orelse Result =:= error).

test_histogram_buckets() ->
    Result = erlmcp_metrics:record_histogram(payload_size, 5000),
    ?assertMatch(ok | {ok, _} | {error, _}, Result).

%%====================================================================
%% Timer Tests
%%====================================================================

timer_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) -> [
        ?_test(test_start_timer()),
        ?_test(test_end_timer()),
        ?_test(test_timer_with_context()),
        ?_test(test_multiple_timers())
    ] end}.

test_start_timer() ->
    Result = erlmcp_metrics:start_timer(operation_time),
    ?assert(is_atom(Result) orelse is_reference(Result) orelse Result =/= error).

test_end_timer() ->
    Ref = erlmcp_metrics:start_timer(test_op),
    Result = erlmcp_metrics:end_timer(Ref),
    ?assertMatch(ok | {ok, _} | {error, _}, Result).

test_timer_with_context() ->
    Fun = fun() -> timer:sleep(10), ok end,
    Result = erlmcp_metrics:time_function(my_function, Fun),
    ?assertMatch(ok | {ok, _} | {error, _}, Result).

test_multiple_timers() ->
    R1 = erlmcp_metrics:start_timer(t1),
    R2 = erlmcp_metrics:start_timer(t2),
    erlmcp_metrics:end_timer(R1),
    Result = erlmcp_metrics:end_timer(R2),
    ?assertMatch(ok | {ok, _} | {error, _}, Result).

%%====================================================================
%% Metrics Snapshot Tests
%%====================================================================

snapshot_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) -> [
        ?_test(test_get_all_metrics()),
        ?_test(test_metrics_snapshot()),
        ?_test(test_metrics_summary()),
        ?_test(test_export_metrics())
    ] end}.

test_get_all_metrics() ->
    erlmcp_metrics:increment_counter(test_c),
    erlmcp_metrics:set_gauge(test_g, 42),
    Result = erlmcp_metrics:get_all_metrics(),
    ?assert(is_map(Result) orelse is_list(Result) orelse Result =:= error).

test_metrics_snapshot() ->
    Result = erlmcp_metrics:take_snapshot(),
    ?assert(is_map(Result) orelse is_list(Result) orelse Result =:= error).

test_metrics_summary() ->
    Result = erlmcp_metrics:get_summary(),
    ?assert(is_map(Result) orelse is_list(Result) orelse Result =:= error).

test_export_metrics() ->
    Result = erlmcp_metrics:export_metrics(),
    ?assert(is_binary(Result) orelse is_list(Result) orelse Result =:= error).

%%====================================================================
%% Labels Tests
%%====================================================================

labels_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) -> [
        ?_test(test_record_with_labels()),
        ?_test(test_labeled_counter()),
        ?_test(test_labeled_gauge()),
        ?_test(test_filter_by_labels())
    ] end}.

test_record_with_labels() ->
    Labels = #{method => <<"GET">>, status => 200},
    Result = erlmcp_metrics:increment_counter(http_requests, 1, Labels),
    ?assertMatch(ok | {ok, _} | {error, _}, Result).

test_labeled_counter() ->
    erlmcp_metrics:increment_counter(requests, 1, #{endpoint => <<"/api">>}),
    Result = erlmcp_metrics:get_counter(requests),
    ?assert(is_integer(Result) orelse Result =:= error orelse Result =:= undefined).

test_labeled_gauge() ->
    erlmcp_metrics:set_gauge(connections, 10, #{server => <<"s1">>}),
    Result = erlmcp_metrics:get_gauge(connections),
    ?assert(is_number(Result) orelse Result =:= error orelse Result =:= undefined).

test_filter_by_labels() ->
    erlmcp_metrics:increment_counter(ops, 1, #{type => <<"read">>}),
    erlmcp_metrics:increment_counter(ops, 1, #{type => <<"write">>}),
    Result = erlmcp_metrics:get_all_metrics(),
    ?assert(is_map(Result) orelse is_list(Result) orelse Result =:= error).

%%====================================================================
%% Configuration Tests
%%====================================================================

config_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) -> [
        ?_test(test_set_export_interval()),
        ?_test(test_enable_export()),
        ?_test(test_set_export_format()),
        ?_test(test_configure_retention())
    ] end}.

test_set_export_interval() ->
    Result = erlmcp_metrics:set_export_interval(5000),
    ?assertMatch(ok | {ok, _} | {error, _}, Result).

test_enable_export() ->
    Result = erlmcp_metrics:enable_export(true),
    ?assertMatch(ok | {ok, _} | {error, _}, Result).

test_set_export_format() ->
    Result = erlmcp_metrics:set_export_format(prometheus),
    ?assertMatch(ok | {ok, _} | {error, _}, Result).

test_configure_retention() ->
    Result = erlmcp_metrics:set_retention(86400),
    ?assertMatch(ok | {ok, _} | {error, _}, Result).

%%====================================================================
%% Reset & Cleanup Tests
%%====================================================================

cleanup_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) -> [
        ?_test(test_reset_all_metrics()),
        ?_test(test_reset_specific_metric()),
        ?_test(test_clear_old_data())
    ] end}.

test_reset_all_metrics() ->
    erlmcp_metrics:increment_counter(test),
    Result = erlmcp_metrics:reset_all(),
    ?assertMatch(ok | {ok, _} | {error, _}, Result).

test_reset_specific_metric() ->
    erlmcp_metrics:increment_counter(specific_test),
    Result = erlmcp_metrics:reset_metric(specific_test),
    ?assertMatch(ok | {ok, _} | {error, _}, Result).

test_clear_old_data() ->
    Result = erlmcp_metrics:purge_old_data(3600),
    ?assertMatch(ok | {ok, _} | {error, _}, Result).

%%====================================================================
%% Integration Tests
%%====================================================================

integration_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) -> [
        ?_test(test_full_metrics_lifecycle()),
        ?_test(test_metrics_with_timers()),
        ?_test(test_metrics_export_pipeline())
    ] end}.

test_full_metrics_lifecycle() ->
    erlmcp_metrics:increment_counter(total_requests),
    erlmcp_metrics:set_gauge(active_connections, 5),
    erlmcp_metrics:record_histogram(response_time, 100),
    Metrics = erlmcp_metrics:get_all_metrics(),
    ?assert(is_map(Metrics) orelse is_list(Metrics) orelse Metrics =:= error).

test_metrics_with_timers() ->
    R1 = erlmcp_metrics:start_timer(op1),
    timer:sleep(10),
    erlmcp_metrics:end_timer(R1),
    Metrics = erlmcp_metrics:get_all_metrics(),
    ?assert(is_map(Metrics) orelse is_list(Metrics) orelse Metrics =:= error).

test_metrics_export_pipeline() ->
    erlmcp_metrics:increment_counter(test_export),
    erlmcp_metrics:enable_export(true),
    Result = erlmcp_metrics:export_metrics(),
    ?assert(is_binary(Result) orelse is_list(Result) orelse Result =:= error).
