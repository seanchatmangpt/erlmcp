%%%====================================================================
%%% ERLMCP CLI COMMAND BENCHMARK
%%%====================================================================
%%% Measures: Command execution time for common CLI operations
%%% Target: <500ms for simple commands, <2s for complex commands
%%% Output: Metrology-compliant JSON with per-command breakdown
%%%====================================================================

-module(erlmcp_cli_command_bench).

-export([
    run/0,
    run/1,
    bench_spec_validation/1,
    bench_protocol_validation/1,
    bench_transport_validation/1,
    bench_quick_check/1
]).

-include_lib("kernel/include/logger.hrl").

%% Entry point
-spec run() -> ok.
run() ->
    run(#{iterations => 10}).

-spec run(map()) -> ok.
run(Opts) ->
    Iterations = maps:get(iterations, Opts, 10),

    io:format("~n==============================================~n"),
    io:format("ERLMCP CLI COMMAND BENCHMARK~n"),
    io:format("Iterations: ~p per command~n", [Iterations]),
    io:format("==============================================~n~n"),

    %% Ensure apps are started
    ensure_apps_started(),

    %% Benchmark each command type
    SpecResults = bench_spec_validation(Iterations),
    ProtocolResults = bench_protocol_validation(Iterations),
    TransportResults = bench_transport_validation(Iterations),
    QuickCheckResults = bench_quick_check(Iterations),

    %% Build report
    Report = build_report(SpecResults, ProtocolResults, TransportResults, QuickCheckResults),

    %% Print and save
    print_results(Report),
    write_report(Report),

    ok.

%% Benchmark spec validation
bench_spec_validation(Iterations) ->
    io:format("Benchmarking spec validation...~n"),

    Times = lists:map(fun(_) ->
        StartTime = erlang:monotonic_time(millisecond),
        _ = erlmcp_validate_cli:validate_spec(),
        EndTime = erlang:monotonic_time(millisecond),
        EndTime - StartTime
    end, lists:seq(1, Iterations)),

    #{
        command => <<"spec_validation">>,
        target_ms => 500,
        iterations => Iterations,
        times_ms => Times,
        mean_ms => mean(Times),
        median_ms => median(Times),
        p95_ms => percentile(Times, 0.95),
        p99_ms => percentile(Times, 0.99),
        min_ms => lists:min(Times),
        max_ms => lists:max(Times),
        status => determine_status(mean(Times), 500)
    }.

%% Benchmark protocol validation
bench_protocol_validation(Iterations) ->
    io:format("Benchmarking protocol validation...~n"),

    %% Sample message
    Message = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"method">> => <<"initialize">>,
        <<"id">> => 1,
        <<"params">> => #{
            <<"protocolVersion">> => <<"2025-11-25">>,
            <<"capabilities">> => #{},
            <<"clientInfo">> => #{
                <<"name">> => <<"test-client">>,
                <<"version">> => <<"1.0.0">>
            }
        }
    },

    Times = lists:map(fun(_) ->
        StartTime = erlang:monotonic_time(millisecond),
        _ = erlmcp_validate_cli:validate_protocol_message(Message),
        EndTime = erlang:monotonic_time(millisecond),
        EndTime - StartTime
    end, lists:seq(1, Iterations)),

    #{
        command => <<"protocol_validation">>,
        target_ms => 100,
        iterations => Iterations,
        times_ms => Times,
        mean_ms => mean(Times),
        median_ms => median(Times),
        p95_ms => percentile(Times, 0.95),
        p99_ms => percentile(Times, 0.99),
        min_ms => lists:min(Times),
        max_ms => lists:max(Times),
        status => determine_status(mean(Times), 100)
    }.

%% Benchmark transport validation
bench_transport_validation(Iterations) ->
    io:format("Benchmarking transport validation...~n"),

    Times = lists:map(fun(_) ->
        StartTime = erlang:monotonic_time(millisecond),
        _ = erlmcp_validate_cli:validate_transport(stdio),
        EndTime = erlang:monotonic_time(millisecond),
        EndTime - StartTime
    end, lists:seq(1, Iterations)),

    #{
        command => <<"transport_validation">>,
        target_ms => 1000,
        iterations => Iterations,
        times_ms => Times,
        mean_ms => mean(Times),
        median_ms => median(Times),
        p95_ms => percentile(Times, 0.95),
        p99_ms => percentile(Times, 0.99),
        min_ms => lists:min(Times),
        max_ms => lists:max(Times),
        status => determine_status(mean(Times), 1000)
    }.

%% Benchmark quick check
bench_quick_check(Iterations) ->
    io:format("Benchmarking quick check...~n"),

    Times = lists:map(fun(_) ->
        StartTime = erlang:monotonic_time(millisecond),
        _ = quick_check_impl(),
        EndTime = erlang:monotonic_time(millisecond),
        EndTime - StartTime
    end, lists:seq(1, Iterations)),

    #{
        command => <<"quick_check">>,
        target_ms => 200,
        iterations => Iterations,
        times_ms => Times,
        mean_ms => mean(Times),
        median_ms => median(Times),
        p95_ms => percentile(Times, 0.95),
        p99_ms => percentile(Times, 0.99),
        min_ms => lists:min(Times),
        max_ms => lists:max(Times),
        status => determine_status(mean(Times), 200)
    }.

quick_check_impl() ->
    %% Minimal checks - just verify modules are loaded
    Modules = [erlmcp_validate_cli, erlmcp_spec_parser],
    lists:all(fun(M) -> code:is_loaded(M) =/= false end, Modules).

%% Build report
build_report(SpecResults, ProtocolResults, TransportResults, QuickCheckResults) ->
    AllResults = [SpecResults, ProtocolResults, TransportResults, QuickCheckResults],

    TotalPassed = length([R || R <- AllResults, maps:get(status, R) =:= <<"passed">>]),
    TotalCommands = length(AllResults),

    #{
        benchmark => <<"cli_commands">>,
        timestamp => erlang:system_time(second),
        environment => capture_environment(),
        commands => #{
            spec_validation => SpecResults,
            protocol_validation => ProtocolResults,
            transport_validation => TransportResults,
            quick_check => QuickCheckResults
        },
        summary => #{
            total_commands => TotalCommands,
            passed => TotalPassed,
            failed => TotalCommands - TotalPassed,
            overall_status => if TotalPassed =:= TotalCommands -> <<"passed">>;
                                 true -> <<"failed">>
                              end
        }
    }.

%% Print results
print_results(#{commands := Commands, summary := Summary}) ->
    io:format("~n==============================================~n"),
    io:format("COMMAND EXECUTION RESULTS~n"),
    io:format("==============================================~n~n"),

    maps:foreach(fun(CmdName, CmdResults) ->
        MeanMs = maps:get(mean_ms, CmdResults),
        TargetMs = maps:get(target_ms, CmdResults),
        Status = maps:get(status, CmdResults),

        StatusIcon = case Status of
            <<"passed">> -> "✓";
            <<"warning">> -> "⚠";
            _ -> "✗"
        end,

        io:format("~s ~s:~n", [StatusIcon, CmdName]),
        io:format("  Mean:   ~.2f ms (target: ~p ms)~n", [MeanMs, TargetMs]),
        io:format("  Median: ~.2f ms~n", [maps:get(median_ms, CmdResults)]),
        io:format("  P95:    ~.2f ms~n", [maps:get(p95_ms, CmdResults)]),
        io:format("  P99:    ~.2f ms~n", [maps:get(p99_ms, CmdResults)]),
        io:format("~n")
    end, Commands),

    io:format("Summary: ~p/~p commands passed~n",
              [maps:get(passed, Summary), maps:get(total_commands, Summary)]),
    io:format("==============================================~n~n").

%% Write report
write_report(Report) ->
    Timestamp = erlang:system_time(second),
    Filename = io_lib:format("bench/results/cli_commands_~p.json", [Timestamp]),

    filelib:ensure_dir(Filename),
    JSON = jsx:encode(Report, [{space, 1}, {indent, 2}]),
    file:write_file(Filename, JSON),

    io:format("Report written to: ~s~n", [Filename]).

%% Helper functions
ensure_apps_started() ->
    Apps = [crypto, asn1, public_key, ssl, inets],
    lists:foreach(fun(App) ->
        case application:start(App) of
            ok -> ok;
            {error, {already_started, App}} -> ok
        end
    end, Apps).

determine_status(MeanMs, TargetMs) when MeanMs < TargetMs -> <<"passed">>;
determine_status(MeanMs, TargetMs) when MeanMs < TargetMs * 1.5 -> <<"warning">>;
determine_status(_, _) -> <<"failed">>.

capture_environment() ->
    #{
        otp_release => list_to_binary(erlang:system_info(otp_release)),
        erts_version => list_to_binary(erlang:system_info(version)),
        schedulers => erlang:system_info(schedulers),
        timestamp_iso => iso8601_timestamp()
    }.

iso8601_timestamp() ->
    {{Year, Month, Day}, {Hour, Min, Sec}} = calendar:universal_time(),
    iolist_to_binary(io_lib:format("~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0BZ",
                  [Year, Month, Day, Hour, Min, Sec])).

mean([]) -> 0.0;
mean(List) -> lists:sum(List) / length(List).

median([]) -> 0.0;
median(List) ->
    Sorted = lists:sort(List),
    Len = length(Sorted),
    Mid = Len div 2,
    case Len rem 2 of
        0 -> (lists:nth(Mid, Sorted) + lists:nth(Mid + 1, Sorted)) / 2;
        1 -> lists:nth(Mid + 1, Sorted)
    end.

percentile([], _) -> 0.0;
percentile(List, P) ->
    Sorted = lists:sort(List),
    Len = length(Sorted),
    Index = max(1, min(Len, round(P * Len))),
    lists:nth(Index, Sorted).
