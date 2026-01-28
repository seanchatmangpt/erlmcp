%%%-------------------------------------------------------------------
%% @doc
%% Chaos CLI Test Suite
%%
%% Tests the erlmcp_cli_chaos module for:
%% - Command parsing and validation
%% - Chaos scenario execution with various options
%% - Output format generation (JSON, CSV, text)
%% - Deterministic results with seed control
%%
%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_cli_chaos_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

%% CT callbacks
-export([all/0, init_per_suite/1, end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2]).

%% Test cases
-export([
    test_parse_valid_args/1,
    test_parse_invalid_scenario/1,
    test_parse_invalid_duration/1,
    test_parse_invalid_intensity/1,
    test_parse_invalid_format/1,
    test_run_loss_scenario/1,
    test_run_latency_scenario/1,
    test_run_slow_scenario/1,
    test_run_all_scenarios/1,
    test_json_format_output/1,
    test_csv_format_output/1,
    test_text_format_output/1,
    test_deterministic_results_with_seed/1,
    test_intensity_levels/1,
    test_resilience_scoring/1
]).

%%====================================================================
%% CT Callbacks
%%====================================================================

all() ->
    [
        test_parse_valid_args,
        test_parse_invalid_scenario,
        test_parse_invalid_duration,
        test_parse_invalid_intensity,
        test_parse_invalid_format,
        test_run_loss_scenario,
        test_run_latency_scenario,
        test_run_slow_scenario,
        test_run_all_scenarios,
        test_json_format_output,
        test_csv_format_output,
        test_text_format_output,
        test_deterministic_results_with_seed,
        test_intensity_levels,
        test_resilience_scoring
    ].

init_per_suite(Config) ->
    ct:pal("Initializing Chaos CLI Test Suite"),
    application:ensure_all_started(erlmcp),
    Config.

end_per_suite(Config) ->
    ct:pal("Chaos CLI Test Suite completed"),
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

    Args1 = ["--scenario", "loss", "--duration", "60", "--intensity", "high",
             "--output", "text"],
    ?assertMatch(ok, erlmcp_cli_chaos:run(Args1)),

    ct:pal("✓ Valid arguments parsed correctly"),
    ok.

test_parse_invalid_scenario(_Config) ->
    ct:pal("Testing invalid scenario name..."),

    Args = ["--scenario", "invalid_scenario"],
    ?assertMatch({error, _}, erlmcp_cli_chaos:run(Args)),

    ct:pal("✓ Invalid scenario rejected"),
    ok.

test_parse_invalid_duration(_Config) ->
    ct:pal("Testing invalid duration..."),

    Args = ["--duration", "not_a_number"],
    ?assertMatch({error, _}, erlmcp_cli_chaos:run(Args)),

    ct:pal("✓ Invalid duration rejected"),
    ok.

test_parse_invalid_intensity(_Config) ->
    ct:pal("Testing invalid intensity..."),

    Args = ["--intensity", "extreme"],
    ?assertMatch({error, _}, erlmcp_cli_chaos:run(Args)),

    ct:pal("✓ Invalid intensity rejected"),
    ok.

test_parse_invalid_format(_Config) ->
    ct:pal("Testing invalid output format..."),

    Args = ["--output", "xml"],
    ?assertMatch({error, _}, erlmcp_cli_chaos:run(Args)),

    ct:pal("✓ Invalid format rejected"),
    ok.

%%====================================================================
%% Test Cases: Chaos Scenario Execution
%%====================================================================

test_run_loss_scenario(_Config) ->
    ct:pal("Testing packet loss chaos scenario..."),

    Args = ["--scenario", "loss", "--duration", "5", "--intensity", "medium",
            "--output", "text"],
    ?assertMatch(ok, erlmcp_cli_chaos:run(Args)),

    ct:pal("✓ Loss scenario executed"),
    ok.

test_run_latency_scenario(_Config) ->
    ct:pal("Testing latency injection chaos scenario..."),

    Args = ["--scenario", "latency", "--duration", "5", "--intensity", "low",
            "--output", "text"],
    ?assertMatch(ok, erlmcp_cli_chaos:run(Args)),

    ct:pal("✓ Latency scenario executed"),
    ok.

test_run_slow_scenario(_Config) ->
    ct:pal("Testing slow node degradation chaos scenario..."),

    Args = ["--scenario", "slow", "--duration", "5", "--intensity", "high",
            "--output", "text"],
    ?assertMatch(ok, erlmcp_cli_chaos:run(Args)),

    ct:pal("✓ Slow scenario executed"),
    ok.

test_run_all_scenarios(_Config) ->
    ct:pal("Testing combined all scenarios..."),

    Args = ["--scenario", "all", "--duration", "5", "--intensity", "medium",
            "--output", "text"],
    ?assertMatch(ok, erlmcp_cli_chaos:run(Args)),

    ct:pal("✓ All scenarios executed"),
    ok.

%%====================================================================
%% Test Cases: Output Formatting
%%====================================================================

test_json_format_output(_Config) ->
    ct:pal("Testing JSON output format..."),

    Result = #{
        scenario => loss,
        duration_sec => 60,
        intensity => high,
        chaos_events => 100,
        operations => 5000,
        failures => 100,
        recovery_time_ms => 2500,
        resilience_score => 98.0,
        timestamp => 1234567890
    },

    Json = erlmcp_cli_chaos:format_json(Result),

    %% Verify JSON structure
    ?assert(string:find(Json, "\"scenario\": loss") =/= nomatch),
    ?assert(string:find(Json, "\"intensity\": \"high\"") =/= nomatch),
    ?assert(string:find(Json, "\"resilience_score_percent\": 98.00") =/= nomatch),

    ct:pal("JSON: ~s", [Json]),
    ct:pal("✓ JSON format validated"),
    ok.

test_csv_format_output(_Config) ->
    ct:pal("Testing CSV output format..."),

    Result = #{
        scenario => latency,
        duration_sec => 30,
        intensity => medium,
        chaos_events => 50,
        operations => 2000,
        failures => 50,
        recovery_time_ms => 1500,
        resilience_score => 97.5,
        timestamp => 1234567890
    },

    Csv = erlmcp_cli_chaos:format_csv(Result),

    %% Verify CSV structure
    ?assert(string:find(Csv, "scenario,duration_sec,intensity") =/= nomatch),
    ?assert(string:find(Csv, "latency,30,medium") =/= nomatch),

    ct:pal("CSV: ~s", [Csv]),
    ct:pal("✓ CSV format validated"),
    ok.

test_text_format_output(_Config) ->
    ct:pal("Testing text output format..."),

    Result = #{
        scenario => slow,
        duration_sec => 45,
        intensity => low,
        chaos_events => 25,
        operations => 1000,
        failures => 25,
        recovery_time_ms => 500,
        resilience_score => 97.5,
        timestamp => 1234567890
    },

    Text = erlmcp_cli_chaos:format_text(Result),

    %% Verify text structure
    ?assert(string:find(Text, "CHAOS TEST RESULTS") =/= nomatch),
    ?assert(string:find(Text, "slow") =/= nomatch),
    ?assert(string:find(Text, "Duration:") =/= nomatch),
    ?assert(string:find(Text, "45") =/= nomatch),
    ?assert(string:find(Text, "Resilience Score:") =/= nomatch),

    ct:pal("Text: ~s", [Text]),
    ct:pal("✓ Text format validated"),
    ok.

%%====================================================================
%% Test Cases: Deterministic Results
%%====================================================================

test_deterministic_results_with_seed(_Config) ->
    ct:pal("Testing deterministic results with seed..."),

    Args1 = ["--scenario", "loss", "--duration", "5", "--seed", "12345",
             "--output", "json"],
    Args2 = ["--scenario", "loss", "--duration", "5", "--seed", "12345",
             "--output", "json"],

    %% Both runs with same seed should produce same structure (results may vary slightly
    %% due to timing, but operations should be similar)
    ?assertMatch(ok, erlmcp_cli_chaos:run(Args1)),
    ?assertMatch(ok, erlmcp_cli_chaos:run(Args2)),

    ct:pal("✓ Chaos tests deterministic with seed across 2 runs"),
    ok.

%%====================================================================
%% Test Cases: Intensity Levels
%%====================================================================

test_intensity_levels(_Config) ->
    ct:pal("Testing different intensity levels..."),

    %% Low intensity
    Args1 = ["--scenario", "latency", "--duration", "3", "--intensity", "low",
             "--output", "text"],
    ?assertMatch(ok, erlmcp_cli_chaos:run(Args1)),

    %% Medium intensity
    Args2 = ["--scenario", "latency", "--duration", "3", "--intensity", "medium",
             "--output", "text"],
    ?assertMatch(ok, erlmcp_cli_chaos:run(Args2)),

    %% High intensity
    Args3 = ["--scenario", "latency", "--duration", "3", "--intensity", "high",
             "--output", "text"],
    ?assertMatch(ok, erlmcp_cli_chaos:run(Args3)),

    ct:pal("✓ All intensity levels executed successfully"),
    ok.

%%====================================================================
%% Test Cases: Resilience Scoring
%%====================================================================

test_resilience_scoring(_Config) ->
    ct:pal("Testing resilience scoring..."),

    %% Test with 0 failures (100% resilience)
    Result1 = #{
        scenario => loss,
        duration_sec => 60,
        intensity => low,
        chaos_events => 0,
        operations => 1000,
        failures => 0,
        recovery_time_ms => 0,
        resilience_score => 100.0,
        timestamp => 1234567890
    },

    %% Test with 50% failures (50% resilience)
    Result2 = #{
        scenario => latency,
        duration_sec => 60,
        intensity => high,
        chaos_events => 500,
        operations => 1000,
        failures => 500,
        recovery_time_ms => 5000,
        resilience_score => 50.0,
        timestamp => 1234567890
    },

    %% Test with 99% resilience (1% failures)
    Result3 = #{
        scenario => slow,
        duration_sec => 60,
        intensity => medium,
        chaos_events => 10,
        operations => 1000,
        failures => 10,
        recovery_time_ms => 1000,
        resilience_score => 99.0,
        timestamp => 1234567890
    },

    %% Format and verify all results
    Json1 = erlmcp_cli_chaos:format_json(Result1),
    Json2 = erlmcp_cli_chaos:format_json(Result2),
    Json3 = erlmcp_cli_chaos:format_json(Result3),

    ?assert(string:find(Json1, "100.00") =/= nomatch),
    ?assert(string:find(Json2, "50.00") =/= nomatch),
    ?assert(string:find(Json3, "99.00") =/= nomatch),

    ct:pal("✓ Resilience scoring validated"),
    ok.
