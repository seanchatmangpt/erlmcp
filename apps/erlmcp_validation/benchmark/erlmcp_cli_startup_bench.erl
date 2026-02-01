%%%====================================================================
%%% ERLMCP CLI STARTUP BENCHMARK
%%%====================================================================
%%% Measures: CLI startup time, module loading, app initialization
%%% Target: <100ms total startup time
%%% Output: Metrology-compliant JSON with breakdown
%%%====================================================================

-module(erlmcp_cli_startup_bench).

-export([
    run/0,
    run/1,
    measure_startup/0,
    measure_module_loading/0,
    measure_app_loading/0,
    profile_startup/0
]).

-include_lib("kernel/include/logger.hrl").

%% Entry point
-spec run() -> ok.
run() ->
    run(#{iterations => 100, profile => false}).

-spec run(map()) -> ok.
run(Opts) ->
    Iterations = maps:get(iterations, Opts, 100),
    Profile = maps:get(profile, Opts, false),
    
    io:format("~n==============================================~n"),
    io:format("ERLMCP CLI STARTUP BENCHMARK~n"),
    io:format("Target: <100ms startup time~n"),
    io:format("Iterations: ~p~n", [Iterations]),
    io:format("==============================================~n~n"),
    
    %% Baseline measurements
    FullStartupResults = measure_full_startup(Iterations),
    ModuleLoadingResults = measure_module_loading_multi(Iterations),
    AppLoadingResults = measure_app_loading_multi(Iterations),
    
    %% Optional profiling
    ProfileData = if Profile -> profile_startup();
                     true -> #{}
                  end,
    
    %% Build report
    Report = build_report(FullStartupResults, ModuleLoadingResults, AppLoadingResults, ProfileData),
    
    %% Print results
    print_results(Report),
    
    %% Write to file
    write_report(Report),
    
    %% Check against target
    check_target(Report),
    
    ok.

%% Measure full startup time (from exec to ready)
measure_full_startup(Iterations) ->
    io:format("Measuring full CLI startup (~p iterations)...~n", [Iterations]),
    
    Times = [measure_startup() || _ <- lists:seq(1, Iterations)],
    
    #{
        iterations => Iterations,
        times_ms => Times,
        mean_ms => mean(Times),
        median_ms => median(Times),
        p95_ms => percentile(Times, 0.95),
        p99_ms => percentile(Times, 0.99),
        min_ms => lists:min(Times),
        max_ms => lists:max(Times)
    }.

%% Measure single startup
measure_startup() ->
    StartTime = erlang:monotonic_time(millisecond),
    
    %% Simulate CLI startup: load module, parse args, ensure apps
    code:purge(erlmcp_validate_cli),
    code:delete(erlmcp_validate_cli),
    {module, erlmcp_validate_cli} = code:ensure_loaded(erlmcp_validate_cli),
    
    %% Simulate minimal app loading (crypto, ssl, inets)
    _ = application:start(crypto),
    _ = application:start(asn1),
    _ = application:start(public_key),
    _ = application:start(ssl),
    
    EndTime = erlang:monotonic_time(millisecond),
    EndTime - StartTime.

%% Measure module loading time
measure_module_loading() ->
    Modules = [
        erlmcp_validate_cli,
        erlmcp_spec_parser,
        erlmcp_protocol_validator,
        erlmcp_transport_validator,
        erlmcp_security_validator,
        erlmcp_compliance_report
    ],
    
    StartTime = erlang:monotonic_time(microsecond),
    
    lists:foreach(fun(Mod) ->
        code:purge(Mod),
        code:delete(Mod),
        code:ensure_loaded(Mod)
    end, Modules),
    
    EndTime = erlang:monotonic_time(microsecond),
    (EndTime - StartTime) / 1000.

measure_module_loading_multi(Iterations) ->
    io:format("Measuring module loading (~p iterations)...~n", [Iterations]),
    
    Times = [measure_module_loading() || _ <- lists:seq(1, Iterations)],
    
    #{
        iterations => Iterations,
        times_ms => Times,
        mean_ms => mean(Times),
        median_ms => median(Times),
        min_ms => lists:min(Times),
        max_ms => lists:max(Times)
    }.

%% Measure app loading time
measure_app_loading() ->
    Apps = [crypto, asn1, public_key, ssl, inets],
    
    %% Stop apps first
    lists:foreach(fun(App) -> application:stop(App) end, Apps),
    
    StartTime = erlang:monotonic_time(microsecond),
    
    lists:foreach(fun(App) ->
        case application:start(App) of
            ok -> ok;
            {error, {already_started, App}} -> ok
        end
    end, Apps),
    
    EndTime = erlang:monotonic_time(microsecond),
    (EndTime - StartTime) / 1000.

measure_app_loading_multi(Iterations) ->
    io:format("Measuring app initialization (~p iterations)...~n", [Iterations]),
    
    Times = [measure_app_loading() || _ <- lists:seq(1, Iterations)],
    
    #{
        iterations => Iterations,
        times_ms => Times,
        mean_ms => mean(Times),
        median_ms => median(Times),
        min_ms => lists:min(Times),
        max_ms => lists:max(Times)
    }.

%% Profile startup with fprof
profile_startup() ->
    io:format("Profiling startup with fprof...~n"),
    
    %% Start profiling
    fprof:trace([start, {procs, [self()]}]),
    
    %% Run startup
    measure_startup(),
    
    %% Stop and analyze
    fprof:trace(stop),
    fprof:profile(),
    
    %% Analyze to file
    ProfileFile = "/tmp/erlmcp_cli_startup_profile.txt",
    fprof:analyse([{dest, ProfileFile}, {totals, true}, {details, true}]),
    
    io:format("Profile written to: ~s~n", [ProfileFile]),
    
    #{profile_file => list_to_binary(ProfileFile)}.

%% Build metrology report
build_report(FullStartup, ModuleLoading, AppLoading, ProfileData) ->
    #{
        benchmark => <<"cli_startup">>,
        timestamp => erlang:system_time(second),
        target_ms => 100,
        environment => capture_environment(),
        results => #{
            full_startup => FullStartup,
            module_loading => ModuleLoading,
            app_loading => AppLoading
        },
        profile => ProfileData,
        status => determine_status(maps:get(mean_ms, FullStartup))
    }.

determine_status(MeanMs) when MeanMs < 100 -> <<"passed">>;
determine_status(MeanMs) when MeanMs < 150 -> <<"warning">>;
determine_status(_) -> <<"failed">>.

%% Print results
print_results(#{results := Results, target_ms := Target}) ->
    #{full_startup := FullStartup} = Results,
    MeanMs = maps:get(mean_ms, FullStartup),
    
    io:format("~n==============================================~n"),
    io:format("RESULTS~n"),
    io:format("==============================================~n"),
    io:format("Full Startup:~n"),
    io:format("  Mean:   ~.2f ms~n", [MeanMs]),
    io:format("  Median: ~.2f ms~n", [maps:get(median_ms, FullStartup)]),
    io:format("  P95:    ~.2f ms~n", [maps:get(p95_ms, FullStartup)]),
    io:format("  P99:    ~.2f ms~n", [maps:get(p99_ms, FullStartup)]),
    io:format("  Min:    ~.2f ms~n", [maps:get(min_ms, FullStartup)]),
    io:format("  Max:    ~.2f ms~n", [maps:get(max_ms, FullStartup)]),
    io:format("~n"),
    
    Status = if MeanMs < Target -> "PASS ✓";
                MeanMs < Target * 1.5 -> "WARN ⚠";
                true -> "FAIL ✗"
             end,
    io:format("Target: ~p ms | Status: ~s~n", [Target, Status]),
    io:format("==============================================~n~n").

%% Check against target
check_target(#{results := Results, target_ms := Target}) ->
    #{full_startup := FullStartup} = Results,
    MeanMs = maps:get(mean_ms, FullStartup),
    
    if MeanMs < Target ->
            io:format("✓ Performance target met!~n~n");
       MeanMs < Target * 1.5 ->
            io:format("⚠ Performance warning: ~.2f ms (target: ~p ms)~n~n", [MeanMs, Target]);
       true ->
            io:format("✗ Performance target missed: ~.2f ms (target: ~p ms)~n~n", [MeanMs, Target])
    end.

%% Write report to file
write_report(Report) ->
    Timestamp = erlang:system_time(second),
    Filename = io_lib:format("bench/results/cli_startup_~p.json", [Timestamp]),
    
    %% Ensure directory exists
    filelib:ensure_dir(Filename),
    
    %% Write JSON
    JSON = jsx:encode(Report, [{space, 1}, {indent, 2}]),
    file:write_file(Filename, JSON),
    
    io:format("Report written to: ~s~n", [Filename]).

%% Utility functions
capture_environment() ->
    #{
        otp_release => list_to_binary(erlang:system_info(otp_release)),
        erts_version => list_to_binary(erlang:system_info(version)),
        schedulers => erlang:system_info(schedulers),
        wordsize => erlang:system_info(wordsize),
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
