%%%====================================================================
%%% @doc erlmcp_bench_helpers_tests - Unit Tests for Benchmark Helpers
%%%
%%% Tests metric formatting, JSON generation, and metrology compliance.
%%% @end
%%%====================================================================
-module(erlmcp_bench_helpers_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Fixtures
%%====================================================================

helpers_test_() ->
    {setup,
        fun setup/0,
        fun cleanup/1,
        [
            {"Format simple metric", fun test_format_metric_simple/0},
            {"Format metric with scope", fun test_format_metric_with_scope/0},
            {"Format time metric with precision", fun test_format_metric_with_precision/0},
            {"Generate workload ID", fun test_generate_workload_id/0},
            {"Format result JSON", fun test_format_result_json/0},
            {"Environment info", fun test_get_environment_info/0},
            {"Timestamp formatting", fun test_format_timestamp/0},
            {"Save result to file", fun test_save_result/0},
            {"Validate and save", fun test_validate_and_save/0}
        ]
    }.

setup() ->
    application:ensure_all_started(jsx),
    ok.

cleanup(_) ->
    ok.

%%====================================================================
%% Test Cases
%%====================================================================

test_format_metric_simple() ->
    Metric = erlmcp_bench_helpers:format_metric(100, <<"msg/s">>),

    ?assertMatch(#{<<"value">> := 100, <<"unit">> := <<"msg/s">>}, Metric),
    ?assertEqual(100, maps:get(<<"value">>, Metric)),
    ?assertEqual(<<"msg/s">>, maps:get(<<"unit">>, Metric)).

test_format_metric_with_scope() ->
    Metric = erlmcp_bench_helpers:format_metric(48.5, <<"MiB">>, <<"/conn">>),

    ?assertMatch(#{<<"value">> := 48.5, <<"unit">> := <<"MiB/conn">>}, Metric),
    ?assertEqual(<<"MiB/conn">>, maps:get(<<"unit">>, Metric)).

test_format_metric_with_precision() ->
    % 8.5 ms with microsecond precision
    Metric = erlmcp_bench_helpers:format_metric(8.5, <<"ms">>, <<"">>, microseconds),

    ?assertMatch(#{
        <<"value">> := 8.5,
        <<"unit">> := <<"ms">>,
        <<"precision_us">> := 8500
    }, Metric),

    ?assertEqual(8500, maps:get(<<"precision_us">>, Metric)).

test_generate_workload_id() ->
    WorkloadId = erlmcp_bench_helpers:generate_workload_id(<<"tcp">>, <<"sustained_25k">>),

    ?assertEqual(<<"tcp_sustained_25k">>, WorkloadId).

test_format_result_json() ->
    Input = #{
        <<"transport">> => <<"tcp">>,
        <<"duration_seconds">> => 30,
        <<"throughput">> => 1000,
        <<"custom_field">> => <<"test">>
    },

    Result = erlmcp_bench_helpers:format_result_json(<<"test_workload">>, Input),

    % Check required fields
    ?assertEqual(<<"test_workload">>, maps:get(<<"workload_id">>, Result)),
    ?assertEqual(<<"tcp">>, maps:get(<<"transport">>, Result)),
    ?assertEqual(30, maps:get(<<"duration_seconds">>, Result)),
    ?assert(maps:is_key(<<"timestamp">>, Result)),
    ?assert(maps:is_key(<<"timestamp_iso">>, Result)),
    ?assert(maps:is_key(<<"environment">>, Result)),

    % Check custom field preserved
    ?assertEqual(<<"test">>, maps:get(<<"custom_field">>, Result)),

    % Check environment structure
    Env = maps:get(<<"environment">>, Result),
    ?assert(maps:is_key(<<"os">>, Env)),
    ?assert(maps:is_key(<<"otp_version">>, Env)),
    ?assert(maps:is_key(<<"cores">>, Env)).

test_get_environment_info() ->
    Env = erlmcp_bench_helpers:get_environment_info(),

    ?assert(maps:is_key(<<"os">>, Env)),
    ?assert(maps:is_key(<<"otp_version">>, Env)),
    ?assert(maps:is_key(<<"erts_version">>, Env)),
    ?assert(maps:is_key(<<"cores">>, Env)),
    ?assert(maps:is_key(<<"schedulers">>, Env)),
    ?assert(maps:is_key(<<"hostname">>, Env)),

    % Cores should be positive integer
    Cores = maps:get(<<"cores">>, Env),
    ?assert(is_integer(Cores)),
    ?assert(Cores > 0).

test_format_timestamp() ->
    % Test current timestamp
    Timestamp1 = erlmcp_bench_helpers:format_timestamp(),
    ?assert(is_binary(Timestamp1)),
    ?assertEqual(20, byte_size(Timestamp1)),  % ISO8601 format "YYYY-MM-DDTHH:MM:SSZ"
    ?assert(binary:match(Timestamp1, <<"T">>) =/= nomatch),
    ?assert(binary:match(Timestamp1, <<"Z">>) =/= nomatch),

    % Test specific Unix timestamp
    Timestamp2 = erlmcp_bench_helpers:format_timestamp(1738000000),  % 2025-01-27
    ?assertEqual(<<"2025-01-27T11:33:20Z">>, Timestamp2).

test_save_result() ->
    TempFile = "/tmp/erlmcp_bench_test_result.json",

    Result = #{
        <<"workload_id">> => <<"test">>,
        <<"transport">> => <<"tcp">>,
        <<"duration_seconds">> => 10,
        <<"throughput">> => 1000
    },

    % Save
    ?assertEqual(ok, erlmcp_bench_helpers:save_result(Result, TempFile)),

    % Verify file exists
    ?assert(filelib:is_file(TempFile)),

    % Read and verify content
    {ok, Binary} = file:read_file(TempFile),
    Decoded = jsx:decode(Binary, [return_maps]),

    ?assertEqual(<<"test">>, maps:get(<<"workload_id">>, Decoded)),
    ?assertEqual(<<"tcp">>, maps:get(<<"transport">>, Decoded)),

    % Cleanup
    file:delete(TempFile).

test_validate_and_save() ->
    TempFile = "/tmp/erlmcp_bench_test_validated.json",

    % Valid result with all required fields
    ValidResult = #{
        <<"workload_id">> => <<"test_validated">>,
        <<"transport">> => <<"tcp">>,
        <<"duration_seconds">> => 30,
        <<"timestamp">> => erlang:system_time(second),
        <<"environment">> => erlmcp_bench_helpers:get_environment_info(),
        <<"throughput">> => erlmcp_bench_helpers:format_metric(1000, <<"msg/s">>)
    },

    % Should succeed
    ?assertEqual(ok, erlmcp_bench_helpers:validate_and_save(ValidResult, TempFile)),
    ?assert(filelib:is_file(TempFile)),

    % Cleanup
    file:delete(TempFile).

%%====================================================================
%% Integration Tests
%%====================================================================

integration_test_() ->
    {setup,
        fun setup/0,
        fun cleanup/1,
        [
            {"Complete benchmark flow", fun test_complete_benchmark_flow/0}
        ]
    }.

test_complete_benchmark_flow() ->
    % Simulate complete benchmark result generation
    WorkloadId = erlmcp_bench_helpers:generate_workload_id(
        <<"integration">>, <<"complete_flow">>
    ),

    Metrics = #{
        <<"throughput">> => erlmcp_bench_helpers:format_metric(1000, <<"msg/s">>),
        <<"latency_p99">> => erlmcp_bench_helpers:format_metric(8.5, <<"ms">>, <<"">>, microseconds),
        <<"memory_per_conn">> => erlmcp_bench_helpers:format_metric(48.5, <<"MiB">>, <<"/conn">>)
    },

    BaseResult = maps:merge(Metrics, #{
        <<"transport">> => <<"tcp">>,
        <<"duration_seconds">> => 30,
        <<"connections">> => 1000
    }),

    Result = erlmcp_bench_helpers:format_result_json(WorkloadId, BaseResult),

    % Validate structure
    ?assertEqual(<<"integration_complete_flow">>, maps:get(<<"workload_id">>, Result)),
    ?assert(maps:is_key(<<"timestamp">>, Result)),
    ?assert(maps:is_key(<<"environment">>, Result)),

    % Validate metrics
    Throughput = maps:get(<<"throughput">>, Result),
    ?assertEqual(1000, maps:get(<<"value">>, Throughput)),
    ?assertEqual(<<"msg/s">>, maps:get(<<"unit">>, Throughput)),

    Latency = maps:get(<<"latency_p99">>, Result),
    ?assertEqual(8500, maps:get(<<"precision_us">>, Latency)),

    Memory = maps:get(<<"memory_per_conn">>, Result),
    ?assertEqual(<<"MiB/conn">>, maps:get(<<"unit">>, Memory)),

    % Validate metrology compliance
    ?assertEqual(ok, erlmcp_metrology_validator:validate_report(Result)).
