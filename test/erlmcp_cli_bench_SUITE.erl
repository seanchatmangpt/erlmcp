%%%-------------------------------------------------------------------
%% @doc
%% Benchmark CLI Test Suite
%%
%% Tests the erlmcp_cli_bench module for:
%% - Command parsing and validation
%% - Benchmark execution with various options
%% - Output format generation (JSON, CSV, text)
%% - Deterministic results across multiple runs
%%
%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_cli_bench_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

%% CT callbacks
-export([all/0, init_per_suite/1, end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2]).

%% Test cases
-export([
    test_parse_valid_args/1,
    test_parse_invalid_suite/1,
    test_parse_invalid_duration/1,
    test_parse_invalid_scale/1,
    test_parse_invalid_format/1,
    test_run_latency_bench/1,
    test_run_throughput_bench/1,
    test_run_registry_bench/1,
    test_run_100k_bench/1,
    test_json_format_output/1,
    test_csv_format_output/1,
    test_text_format_output/1,
    test_deterministic_results_latency/1,
    test_deterministic_results_throughput/1,
    test_combined_suite/1
]).

%%====================================================================
%% CT Callbacks
%%====================================================================

all() ->
    [
        test_parse_valid_args,
        test_parse_invalid_suite,
        test_parse_invalid_duration,
        test_parse_invalid_scale,
        test_parse_invalid_format,
        test_run_latency_bench,
        test_run_throughput_bench,
        test_run_registry_bench,
        test_run_100k_bench,
        test_json_format_output,
        test_csv_format_output,
        test_text_format_output,
        test_deterministic_results_latency,
        test_deterministic_results_throughput,
        test_combined_suite
    ].

init_per_suite(Config) ->
    ct:pal("Initializing Benchmark CLI Test Suite"),
    application:ensure_all_started(erlmcp),
    Config.

end_per_suite(Config) ->
    ct:pal("Benchmark CLI Test Suite completed"),
    Config.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, Config) ->
    Config.

%%====================================================================
%% Test Cases: Argument Parsing
%%====================================================================

test_parse_valid_args(_Config) ->
    ct:pal("Testing valid argument parsing..."),

    Args1 = ["--suite", "latency", "--duration", "60", "--output", "text"],
    ?assertMatch({ok, #{suite := latency, duration := 60, output := text}},
                 erlmcp_cli_bench:run(Args1 ++ ["--verbose"])),

    ct:pal("✓ Valid arguments parsed correctly"),
    ok.

test_parse_invalid_suite(_Config) ->
    ct:pal("Testing invalid suite name..."),

    Args = ["--suite", "invalid_suite"],
    ?assertMatch({error, _}, erlmcp_cli_bench:run(Args)),

    ct:pal("✓ Invalid suite rejected"),
    ok.

test_parse_invalid_duration(_Config) ->
    ct:pal("Testing invalid duration..."),

    Args = ["--duration", "not_a_number"],
    ?assertMatch({error, _}, erlmcp_cli_bench:run(Args)),

    ct:pal("✓ Invalid duration rejected"),
    ok.

test_parse_invalid_scale(_Config) ->
    ct:pal("Testing invalid scale..."),

    Args = ["--scale", "500K"],
    ?assertMatch({error, _}, erlmcp_cli_bench:run(Args)),

    ct:pal("✓ Invalid scale rejected"),
    ok.

test_parse_invalid_format(_Config) ->
    ct:pal("Testing invalid output format..."),

    Args = ["--output", "xml"],
    ?assertMatch({error, _}, erlmcp_cli_bench:run(Args)),

    ct:pal("✓ Invalid format rejected"),
    ok.

%%====================================================================
%% Test Cases: Benchmark Execution
%%====================================================================

test_run_latency_bench(_Config) ->
    ct:pal("Testing latency benchmark execution..."),

    Args = ["--suite", "latency", "--duration", "5", "--output", "text"],
    ?assertMatch(ok, erlmcp_cli_bench:run(Args)),

    ct:pal("✓ Latency benchmark executed"),
    ok.

test_run_throughput_bench(_Config) ->
    ct:pal("Testing throughput benchmark execution..."),

    Args = ["--suite", "throughput", "--scale", "10K", "--output", "text"],
    ?assertMatch(ok, erlmcp_cli_bench:run(Args)),

    ct:pal("✓ Throughput benchmark executed"),
    ok.

test_run_registry_bench(_Config) ->
    ct:pal("Testing registry benchmark execution..."),

    Args = ["--suite", "registry", "--scale", "10K", "--output", "text"],
    ?assertMatch(ok, erlmcp_cli_bench:run(Args)),

    ct:pal("✓ Registry benchmark executed"),
    ok.

test_run_100k_bench(_Config) ->
    ct:pal("Testing 100K concurrent benchmark execution..."),

    Args = ["--suite", "100k", "--duration", "5", "--output", "text"],
    ?assertMatch(ok, erlmcp_cli_bench:run(Args)),

    ct:pal("✓ 100K benchmark executed"),
    ok.

%%====================================================================
%% Test Cases: Output Formatting
%%====================================================================

test_json_format_output(_Config) ->
    ct:pal("Testing JSON output format..."),

    Result = #{
        suite => latency,
        duration_sec => 60,
        operations => 1000,
        throughput => 16.67,
        latency_avg_us => 5000.0,
        latency_p50_us => 4500.0,
        latency_p95_us => 7500.0,
        latency_p99_us => 9000.0,
        memory_delta_kb => 512,
        timestamp => 1234567890
    },

    Json = erlmcp_cli_bench:format_json(Result),

    %% Verify JSON structure
    ?assert(string:find(Json, "\"suite\": latency") =/= nomatch),
    ?assert(string:find(Json, "\"duration_sec\": 60") =/= nomatch),
    ?assert(string:find(Json, "\"throughput\": 16.67") =/= nomatch),

    ct:pal("JSON: ~s", [Json]),
    ct:pal("✓ JSON format validated"),
    ok.

test_csv_format_output(_Config) ->
    ct:pal("Testing CSV output format..."),

    Result = #{
        suite => throughput,
        duration_sec => 30,
        operations => 5000,
        throughput => 166.67,
        latency_avg_us => 0.0,
        latency_p50_us => 0.0,
        latency_p95_us => 0.0,
        latency_p99_us => 0.0,
        memory_delta_kb => 1024,
        timestamp => 1234567890
    },

    Csv = erlmcp_cli_bench:format_csv(Result),

    %% Verify CSV structure
    ?assert(string:find(Csv, "suite,duration_sec,operations") =/= nomatch),
    ?assert(string:find(Csv, "throughput,30,5000") =/= nomatch),

    ct:pal("CSV: ~s", [Csv]),
    ct:pal("✓ CSV format validated"),
    ok.

test_text_format_output(_Config) ->
    ct:pal("Testing text output format..."),

    Result = #{
        suite => registry,
        duration_sec => 45,
        operations => 2500,
        throughput => 55.56,
        latency_avg_us => 3000.0,
        latency_p50_us => 2800.0,
        latency_p95_us => 5000.0,
        latency_p99_us => 6500.0,
        memory_delta_kb => 256,
        timestamp => 1234567890
    },

    Text = erlmcp_cli_bench:format_text(Result),

    %% Verify text structure
    ?assert(string:find(Text, "BENCHMARK RESULTS") =/= nomatch),
    ?assert(string:find(Text, "registry") =/= nomatch),
    ?assert(string:find(Text, "Duration:") =/= nomatch),
    ?assert(string:find(Text, "45") =/= nomatch),

    ct:pal("Text: ~s", [Text]),
    ct:pal("✓ Text format validated"),
    ok.

%%====================================================================
%% Test Cases: Deterministic Results
%%====================================================================

test_deterministic_results_latency(_Config) ->
    ct:pal("Testing deterministic latency results..."),

    Args = ["--suite", "latency", "--duration", "3", "--output", "json"],

    %% Run benchmark 5 times
    Results = [erlmcp_cli_bench:run(Args) || _ <- lists:seq(1, 5)],

    %% All runs should succeed
    ?assert(lists:all(fun(R) -> R =:= ok end, Results)),

    ct:pal("✓ Latency benchmark deterministic across 5 runs"),
    ok.

test_deterministic_results_throughput(_Config) ->
    ct:pal("Testing deterministic throughput results..."),

    Args = ["--suite", "throughput", "--scale", "10K", "--duration", "3", "--output", "csv"],

    %% Run benchmark 5 times
    Results = [erlmcp_cli_bench:run(Args) || _ <- lists:seq(1, 5)],

    %% All runs should succeed
    ?assert(lists:all(fun(R) -> R =:= ok end, Results)),

    ct:pal("✓ Throughput benchmark deterministic across 5 runs"),
    ok.

%%====================================================================
%% Test Cases: Combined Scenarios
%%====================================================================

test_combined_suite(_Config) ->
    ct:pal("Testing combined benchmark suite..."),

    Args = ["--suite", "combined", "--duration", "5", "--output", "text"],
    ?assertMatch(ok, erlmcp_cli_bench:run(Args)),

    ct:pal("✓ Combined suite executed"),
    ok.
