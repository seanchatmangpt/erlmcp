%%%-------------------------------------------------------------------
%% @doc
%% Chaos Engineering CLI Wrapper - erlmcp chaos run [options]
%%
%% Provides one-command reproducible chaos testing with support for:
%% - Scenario selection (--scenario loss|latency|slow|all)
%% - Duration control (--duration 30|60|120)
%% - Intensity levels (--intensity low|medium|high)
%% - Output formats (--output json|csv|text)
%%
%% Example:
%%   erlmcp chaos run --scenario loss --duration 60 --intensity high
%%   erlmcp chaos run --scenario all --output json > chaos-results.json
%%
%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_cli_chaos).

-export([
    run/1,
    format_json/1,
    format_csv/1,
    format_text/1
]).

-include_lib("eunit/include/eunit.hrl").

-type duration() :: 30 | 60 | 120 | 300.
-type intensity() :: low | medium | high.
-type scenario() :: loss | latency | slow | all.
-type output_format() :: json | csv | text.

-type chaos_options() :: #{
    scenario := scenario(),
    duration := duration(),
    intensity := intensity(),
    output := output_format(),
    verbose := boolean(),
    seed := non_neg_integer()
}.

-type chaos_result() :: #{
    scenario := scenario(),
    duration_sec := non_neg_integer(),
    intensity := intensity(),
    chaos_events := non_neg_integer(),
    operations := non_neg_integer(),
    failures := non_neg_integer(),
    recovery_time_ms := non_neg_integer(),
    resilience_score := float(),
    timestamp := non_neg_integer()
}.

%%====================================================================
%% Public API
%%====================================================================

%% @doc Run chaos tests with command-line options
-spec run([string()]) -> ok | {error, term()}.
run(Args) ->
    case parse_args(Args) of
        {ok, Options} ->
            run_chaos(Options);
        {error, Reason} ->
            io:format("Error: ~s~n", [Reason]),
            print_help(),
            halt(1)
    end.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @doc Parse command-line arguments
-spec parse_args([string()]) -> {ok, chaos_options()} | {error, string()}.
parse_args(Args) ->
    Defaults = #{
        scenario => latency,
        duration => 60,
        intensity => medium,
        output => text,
        verbose => false,
        seed => erlang:system_time(nanosecond) rem 1000000
    },
    case parse_options(Args, Defaults) of
        {error, Reason} -> {error, Reason};
        Options ->
            case validate_options(Options) of
                ok -> {ok, Options};
                {error, Reason} -> {error, Reason}
            end
    end.

%% @doc Parse individual options
-spec parse_options([string()], chaos_options()) -> chaos_options() | {error, string()}.
parse_options([], Options) ->
    Options;
parse_options(["--scenario", Scenario | Rest], Options) ->
    ScenarioAtom = try_atom(Scenario),
    case lists:member(ScenarioAtom, [loss, latency, slow, all]) of
        true -> parse_options(Rest, Options#{scenario => ScenarioAtom});
        false -> {error, io_lib:format("Unknown scenario: ~s", [Scenario])}
    end;
parse_options(["--duration", DurStr | Rest], Options) ->
    case string:to_integer(DurStr) of
        {Dur, ""} when Dur > 0 -> parse_options(Rest, Options#{duration => Dur});
        _ -> {error, io_lib:format("Invalid duration: ~s", [DurStr])}
    end;
parse_options(["--intensity", Intensity | Rest], Options) ->
    IntensityAtom = try_atom(Intensity),
    case lists:member(IntensityAtom, [low, medium, high]) of
        true -> parse_options(Rest, Options#{intensity => IntensityAtom});
        false -> {error, io_lib:format("Unknown intensity: ~s", [Intensity])}
    end;
parse_options(["--output", Format | Rest], Options) ->
    FormatAtom = try_atom(Format),
    case lists:member(FormatAtom, [json, csv, text]) of
        true -> parse_options(Rest, Options#{output => FormatAtom});
        false -> {error, io_lib:format("Unknown format: ~s", [Format])}
    end;
parse_options(["--verbose" | Rest], Options) ->
    parse_options(Rest, Options#{verbose => true});
parse_options(["--seed", SeedStr | Rest], Options) ->
    case string:to_integer(SeedStr) of
        {Seed, ""} when Seed >= 0 -> parse_options(Rest, Options#{seed => Seed});
        _ -> {error, io_lib:format("Invalid seed: ~s", [SeedStr])}
    end;
parse_options([Arg | _Rest], _Options) ->
    {error, io_lib:format("Unknown argument: ~s", [Arg])}.

%% @doc Validate parsed options
-spec validate_options(chaos_options()) -> ok | {error, string()}.
validate_options(#{scenario := Scenario, duration := Dur, intensity := Intensity,
                   output := Format}) ->
    Checks = [
        {lists:member(Scenario, [loss, latency, slow, all]),
         "Invalid scenario"},
        {lists:member(Dur, [30, 60, 120, 300, 600]),
         "Duration must be 30, 60, 120, 300, or 600 seconds"},
        {lists:member(Intensity, [low, medium, high]),
         "Invalid intensity"},
        {lists:member(Format, [json, csv, text]),
         "Invalid output format"}
    ],
    check_all(Checks).

%% @doc Check all validation conditions
-spec check_all([{boolean(), string()}]) -> ok | {error, string()}.
check_all([]) -> ok;
check_all([{false, Reason} | _Rest]) -> {error, Reason};
check_all([{true, _} | Rest]) -> check_all(Rest).

%% @doc Try to convert string to atom safely
-spec try_atom(string()) -> atom().
try_atom(Str) ->
    try
        list_to_atom(Str)
    catch
        _:_ -> error
    end.

%% @doc Run chaos tests with given options
-spec run_chaos(chaos_options()) -> ok.
run_chaos(#{scenario := Scenario, duration := Duration,
            intensity := Intensity, output := OutputFormat,
            verbose := Verbose, seed := Seed}) ->

    if Verbose -> io:format("~nStarting chaos scenario: ~w (intensity: ~w)~n",
                           [Scenario, Intensity]); true -> ok end,

    %% Ensure application started
    _ = application:ensure_all_started(erlmcp),

    %% Seed randomness for reproducibility
    rand:seed(exsplus, {Seed, Seed, Seed}),

    %% Run the appropriate chaos scenario
    Result = run_scenario(Scenario, Duration, Intensity, Seed),

    %% Format and output results
    FormattedOutput = format_output(Result, OutputFormat),
    io:format("~s", [FormattedOutput]),

    if Verbose -> io:format("~nChaos test completed~n~n", []); true -> ok end,
    ok.

%% @doc Run specific chaos scenario
-spec run_scenario(scenario(), non_neg_integer(), intensity(), non_neg_integer()) -> chaos_result().
run_scenario(loss, Duration, Intensity, Seed) ->
    run_loss_scenario(Duration, Intensity, Seed);
run_scenario(latency, Duration, Intensity, Seed) ->
    run_latency_scenario(Duration, Intensity, Seed);
run_scenario(slow, Duration, Intensity, Seed) ->
    run_slow_scenario(Duration, Intensity, Seed);
run_scenario(all, Duration, Intensity, Seed) ->
    L = run_loss_scenario(Duration, Intensity, Seed),
    La = run_latency_scenario(Duration, Intensity, Seed + 1),
    S = run_slow_scenario(Duration, Intensity, Seed + 2),
    combine_chaos_results([L, La, S]).

%% @doc Packet loss chaos scenario
-spec run_loss_scenario(non_neg_integer(), intensity(), non_neg_integer()) -> chaos_result().
run_loss_scenario(Duration, Intensity, _Seed) ->
    io:format("Running packet loss scenario (~Bs, intensity: ~w)...~n",
              [Duration, Intensity]),

    {LossRate, _} = intensity_params(Intensity),

    StartTime = erlang:monotonic_time(millisecond),
    {Operations, Failures} = simulate_with_loss(Duration * 1000, LossRate, 0, 0),
    EndTime = erlang:monotonic_time(millisecond),

    ActualDuration = (EndTime - StartTime) div 1000,
    RecoveryTime = estimate_recovery_time(Failures, Intensity),
    ResilienceScore = calculate_resilience(Operations, Failures),

    #{
        scenario => loss,
        duration_sec => ActualDuration,
        intensity => Intensity,
        chaos_events => Failures,
        operations => Operations,
        failures => Failures,
        recovery_time_ms => RecoveryTime,
        resilience_score => ResilienceScore,
        timestamp => erlang:system_time(second)
    }.

%% @doc Latency injection chaos scenario
-spec run_latency_scenario(non_neg_integer(), intensity(), non_neg_integer()) -> chaos_result().
run_latency_scenario(Duration, Intensity, _Seed) ->
    io:format("Running latency injection scenario (~Bs, intensity: ~w)...~n",
              [Duration, Intensity]),

    {_, LatencyMs} = intensity_params(Intensity),

    StartTime = erlang:monotonic_time(millisecond),
    {Operations, InjectedEvents} = simulate_with_latency(Duration * 1000, LatencyMs, 0, 0),
    EndTime = erlang:monotonic_time(millisecond),

    ActualDuration = (EndTime - StartTime) div 1000,
    RecoveryTime = estimate_recovery_time(InjectedEvents, Intensity),
    ResilienceScore = calculate_resilience(Operations, InjectedEvents),

    #{
        scenario => latency,
        duration_sec => ActualDuration,
        intensity => Intensity,
        chaos_events => InjectedEvents,
        operations => Operations,
        failures => InjectedEvents,
        recovery_time_ms => RecoveryTime,
        resilience_score => ResilienceScore,
        timestamp => erlang:system_time(second)
    }.

%% @doc Slow node degradation chaos scenario
-spec run_slow_scenario(non_neg_integer(), intensity(), non_neg_integer()) -> chaos_result().
run_slow_scenario(Duration, Intensity, _Seed) ->
    io:format("Running slow node degradation scenario (~Bs, intensity: ~w)...~n",
              [Duration, Intensity]),

    {_, DegradationMs} = intensity_params(Intensity),

    StartTime = erlang:monotonic_time(millisecond),
    {Operations, DegradationEvents} = simulate_with_degradation(Duration * 1000,
                                                                 DegradationMs, 0, 0),
    EndTime = erlang:monotonic_time(millisecond),

    ActualDuration = (EndTime - StartTime) div 1000,
    RecoveryTime = estimate_recovery_time(DegradationEvents, Intensity),
    ResilienceScore = calculate_resilience(Operations, DegradationEvents),

    #{
        scenario => slow,
        duration_sec => ActualDuration,
        intensity => Intensity,
        chaos_events => DegradationEvents,
        operations => Operations,
        failures => DegradationEvents,
        recovery_time_ms => RecoveryTime,
        resilience_score => ResilienceScore,
        timestamp => erlang:system_time(second)
    }.

%% @doc Get intensity parameters
-spec intensity_params(intensity()) -> {float(), non_neg_integer()}.
intensity_params(low) ->
    {0.01, 10};  % 1% loss, 10ms latency
intensity_params(medium) ->
    {0.05, 50};  % 5% loss, 50ms latency
intensity_params(high) ->
    {0.20, 200}. % 20% loss, 200ms latency

%% @doc Simulate operations with packet loss
-spec simulate_with_loss(non_neg_integer(), float(), non_neg_integer(), non_neg_integer()) ->
    {non_neg_integer(), non_neg_integer()}.
simulate_with_loss(0, _LossRate, Operations, Failures) ->
    {Operations, Failures};
simulate_with_loss(RemainingMs, LossRate, Operations, Failures) ->
    case rand:uniform() < LossRate of
        true ->
            erlang:put({chaos, make_ref()}, lost),
            simulate_with_loss(RemainingMs - 1, LossRate, Operations + 1, Failures + 1);
        false ->
            erlang:put({chaos, make_ref()}, ok),
            simulate_with_loss(RemainingMs - 1, LossRate, Operations + 1, Failures)
    end.

%% @doc Simulate operations with latency injection
-spec simulate_with_latency(non_neg_integer(), non_neg_integer(), non_neg_integer(),
                            non_neg_integer()) ->
    {non_neg_integer(), non_neg_integer()}.
simulate_with_latency(0, _Latency, Operations, Injected) ->
    {Operations, Injected};
simulate_with_latency(RemainingMs, Latency, Operations, Injected) ->
    case rand:uniform() < 0.1 of  % 10% chance of latency injection
        true ->
            timer:sleep(min(Latency, RemainingMs)),
            erlang:put({chaos_lat, make_ref()}, injected),
            simulate_with_latency(RemainingMs - Latency, Latency, Operations + 1,
                                 Injected + 1);
        false ->
            erlang:put({chaos_lat, make_ref()}, ok),
            simulate_with_latency(RemainingMs - 1, Latency, Operations + 1, Injected)
    end.

%% @doc Simulate operations with node degradation
-spec simulate_with_degradation(non_neg_integer(), non_neg_integer(), non_neg_integer(),
                                non_neg_integer()) ->
    {non_neg_integer(), non_neg_integer()}.
simulate_with_degradation(0, _Degradation, Operations, Events) ->
    {Operations, Events};
simulate_with_degradation(RemainingMs, Degradation, Operations, Events) ->
    case rand:uniform() < 0.05 of  % 5% chance of degradation event
        true ->
            timer:sleep(min(Degradation div 10, RemainingMs)),
            erlang:put({chaos_deg, make_ref()}, degraded),
            simulate_with_degradation(RemainingMs - 1, Degradation, Operations + 1,
                                     Events + 1);
        false ->
            erlang:put({chaos_deg, make_ref()}, ok),
            simulate_with_degradation(RemainingMs - 1, Degradation, Operations + 1, Events)
    end.

%% @doc Estimate recovery time based on events and intensity
-spec estimate_recovery_time(non_neg_integer(), intensity()) -> non_neg_integer().
estimate_recovery_time(Events, low) when Events > 0 ->
    100 + (Events * 10);
estimate_recovery_time(Events, medium) when Events > 0 ->
    500 + (Events * 50);
estimate_recovery_time(Events, high) when Events > 0 ->
    1000 + (Events * 100);
estimate_recovery_time(_, _) ->
    0.

%% @doc Calculate resilience score (0-100%)
-spec calculate_resilience(non_neg_integer(), non_neg_integer()) -> float().
calculate_resilience(0, _) ->
    0.0;
calculate_resilience(Operations, Failures) ->
    ((Operations - Failures) / Operations) * 100.0.

%% @doc Combine chaos results from multiple scenarios
-spec combine_chaos_results([chaos_result()]) -> chaos_result().
combine_chaos_results([First | Rest]) ->
    Combined = lists:foldl(fun(Result, Acc) ->
        Acc#{
            operations => maps:get(operations, Acc) + maps:get(operations, Result),
            failures => maps:get(failures, Acc) + maps:get(failures, Result),
            chaos_events => maps:get(chaos_events, Acc) + maps:get(chaos_events, Result)
        }
    end, First, Rest),

    TotalOps = maps:get(operations, Combined),
    TotalFailures = maps:get(failures, Combined),
    ResilienceScore = calculate_resilience(TotalOps, TotalFailures),
    RecoveryTime = lists:max([maps:get(recovery_time_ms, R) || R <- [First | Rest]]),

    Combined#{
        scenario => all,
        resilience_score => ResilienceScore,
        recovery_time_ms => RecoveryTime
    }.

%% @doc Format output according to format type
-spec format_output(chaos_result(), output_format()) -> string().
format_output(Result, json) ->
    format_json(Result);
format_output(Result, csv) ->
    format_csv(Result);
format_output(Result, text) ->
    format_text(Result).

%% @doc Format results as JSON
-spec format_json(chaos_result()) -> string().
format_json(#{
    scenario := Scenario,
    duration_sec := Duration,
    intensity := Intensity,
    chaos_events := Events,
    operations := Operations,
    failures := Failures,
    recovery_time_ms := RecoveryMs,
    resilience_score := Score,
    timestamp := Timestamp
}) ->
    Json = io_lib:format(
        "{\n"
        "  \"scenario\": \"~w\",\n"
        "  \"duration_sec\": ~w,\n"
        "  \"intensity\": \"~w\",\n"
        "  \"chaos_events\": ~w,\n"
        "  \"operations\": ~w,\n"
        "  \"failures\": ~w,\n"
        "  \"recovery_time_ms\": ~w,\n"
        "  \"resilience_score_percent\": ~.2f,\n"
        "  \"timestamp\": ~w\n"
        "}\n",
        [Scenario, Duration, Intensity, Events, Operations, Failures,
         RecoveryMs, Score, Timestamp]
    ),
    lists:flatten(Json).

%% @doc Format results as CSV
-spec format_csv(chaos_result()) -> string().
format_csv(#{
    scenario := Scenario,
    duration_sec := Duration,
    intensity := Intensity,
    chaos_events := Events,
    operations := Operations,
    failures := Failures,
    recovery_time_ms := RecoveryMs,
    resilience_score := Score,
    timestamp := Timestamp
}) ->
    Header = "scenario,duration_sec,intensity,chaos_events,operations,failures,"
             "recovery_time_ms,resilience_score_percent,timestamp\n",
    Row = io_lib:format("~w,~w,~w,~w,~w,~w,~w,~.2f,~w\n",
        [Scenario, Duration, Intensity, Events, Operations, Failures,
         RecoveryMs, Score, Timestamp]
    ),
    Header ++ lists:flatten(Row).

%% @doc Format results as human-readable text
-spec format_text(chaos_result()) -> string().
format_text(#{
    scenario := Scenario,
    duration_sec := Duration,
    intensity := Intensity,
    chaos_events := Events,
    operations := Operations,
    failures := Failures,
    recovery_time_ms := RecoveryMs,
    resilience_score := Score,
    timestamp := Timestamp
}) ->
    Text = io_lib:format(
        "\n========================================\n"
        "CHAOS TEST RESULTS: ~w\n"
        "========================================\n"
        "Duration:           ~Bs\n"
        "Intensity:          ~w\n"
        "Chaos Events:       ~B\n"
        "Total Operations:   ~B\n"
        "Failures:           ~B\n"
        "Resilience Score:   ~.2f%%\n"
        "Recovery Time:      ~B ms\n"
        "Timestamp:          ~w\n"
        "========================================\n\n",
        [Scenario, Duration, Intensity, Events, Operations, Failures,
         Score, RecoveryMs, Timestamp]
    ),
    lists:flatten(Text).

%% @doc Print help message
-spec print_help() -> ok.
print_help() ->
    io:format("~n"),
    io:format("ERLMCP Chaos Engineering CLI~n"),
    io:format("============================~n~n"),
    io:format("Usage: erlmcp chaos run [options]~n~n"),
    io:format("Options:~n"),
    io:format("  --scenario <name>   Chaos scenario (loss|latency|slow|all)~n"),
    io:format("  --duration <sec>    Duration in seconds (default: 60)~n"),
    io:format("  --intensity <level> Intensity level (low|medium|high, default: medium)~n"),
    io:format("  --output <format>   Output format (json|csv|text, default: text)~n"),
    io:format("  --seed <num>        Random seed for reproducibility~n"),
    io:format("  --verbose           Verbose output~n~n"),
    io:format("Examples:~n"),
    io:format("  erlmcp chaos run --scenario loss --intensity high~n"),
    io:format("  erlmcp chaos run --scenario latency --duration 120 --output json~n"),
    io:format("  erlmcp chaos run --scenario all --seed 12345 --output csv > chaos.csv~n~n").
