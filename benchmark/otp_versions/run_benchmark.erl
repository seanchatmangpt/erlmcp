%% Benchmark execution script
%%
%% This script runs the actual benchmark and generates performance data
%% for analysis across OTP versions.

-module(run_benchmark).
-export([main/0, run_all_benchmarks/0]).

-include_lib("kernel/include/logger.hrl").

%% Main entry point
main() ->
    io:format("OTP Version Performance Benchmark~n", []),
    io:format("===================================~n~n", []),

    %% Run benchmarks for current OTP version
    Results = run_all_benchmarks(),

    %% Generate detailed analysis
    Analysis = analyze_benchmark_results(Results),

    %% Save results to file
    save_results(Results, Analysis).

%% Run all benchmarks for current OTP version
run_all_benchmarks() ->
    OTP_Version = erlang:system_info(otp_release),
    io:format("Running benchmark for OTP version: ~p~n", [OTP_Version]),

    %% Process creation benchmarks
    io:format("Running process creation benchmarks...~n"),
    ProcessResults = otp_bench:process_creation_bench(),

    %% Message passing benchmarks
    io:format("Running message passing benchmarks...~n"),
    MessageResults = otp_bench:message_passing_bench(),

    %% Supervisor overhead benchmarks
    io:format("Running supervisor overhead benchmarks...~n"),
    SupervisorResults = otp_bench:supervisor_overhead_bench(),

    %% Memory usage benchmarks
    io:format("Running memory usage benchmarks...~n"),
    MemoryResults = otp_bench:memory_usage_bench(),

    #{
        version => OTP_Version,
        timestamp => erlang:system_time(millisecond),
        process_creation => ProcessResults,
        message_passing => MessageResults,
        supervisor_overhead => SupervisorResults,
        memory_usage => MemoryResults
    }.

%% Analyze benchmark results
analyze_benchmark_results(Results) ->
    #{
        version := Version,
        process_creation := ProcResults,
        message_passing := MsgResults,
        supervisor_overhead := SupResults,
        memory_usage := MemResults
    } = Results,

    io:format("~n=== Performance Analysis for OTP ~p ===~n", [Version]),

    %% Process creation analysis
    DirectSpawn = maps:get(direct_spawn, ProcResults),
    GenServer = maps:get(gen_server, ProcResults),
    Supervisor = maps:get(supervisor_children, ProcResults),

    io:format("~nProcess Creation Performance:~n"),
    io:format("  Direct Spawn: ~p ops/sec~n", [maps:get(spawns_per_second, DirectSpawn)]),
    io:format("  Gen Server:  ~p ops/sec~n", [maps:get(servers_per_second, GenServer)]),
    io:format("  Supervisor:  ~p ops/sec~n", [maps:get(children_per_second, Supervisor)]),

    %% Message passing analysis
    LocalMsg = maps:get(local, MsgResults),
    RegisteredMsg = maps:get(registered, MsgResults),
    SelfMsg = maps:get(self, MsgResults),

    io:format("~nMessage Passing Performance:~n"),
    io:format("  Local:    ~p ops/sec~n", [maps:get(msg_per_second, LocalMsg)]),
    io:format("  Registered: ~p ops/sec~n", [maps:get(msg_per_second, RegisteredMsg)]),
    io:format("  Self:     ~p ops/sec~n", [maps:get(msg_per_second, SelfMsg)]),

    %% Supervisor overhead analysis
    Startup = maps:get(startup, SupResults),
    Restarts = maps:get(restarts, SupResults),
    Strategies = maps:get(strategies, SupResults),

    io:format("~nSupervisor Overhead:~n"),
    io:format("  Startup:    ~p ops/sec~n", [maps:get(startups_per_second, Startup)]),
    io:format("  Restarts:   ~p ops/sec~n", [maps:get(restarts_per_second, Restarts)]),
    io:format("  One-for-all: ~p ops/sec~n", [maps:get(per_second, maps:get(one_for_all, Strategies))]),
    io:format("  Simple:     ~p ops/sec~n", [maps:get(per_second, maps:get(simple_one_for_one, Strategies))]),

    %% Memory usage analysis
    SpawnMem = maps:get(spawn_memory, MemResults),
    GenMem = maps:get(gen_server_memory, MemResults),
    RegMem = maps:get(registered_memory, MemResults),

    io:format("~nMemory Usage:~n"),
    io:format("  Spawn growth: ~.2f MB per process~n", [maps:get(avg_per_process, SpawnMem) / 1024 / 1024]),
    io:format("  GenServer growth: ~.2f MB per process~n", [maps:get(avg_per_server, GenMem) / 1024 / 1024]),
    io:format("  Registered growth: ~.2f MB per process~n", [maps:get(avg_per_process, RegMem) / 1024 / 1024]),

    %% Calculate performance scores
    PerfScore = calculate_performance_score(Results),

    io:format("~nPerformance Score: ~.2f~n", [PerfScore]),

    Results.

%% Calculate overall performance score
calculate_performance_score(Results) ->
    #{
        process_creation := ProcResults,
        message_passing := MsgResults,
        supervisor_overhead := SupResults,
        memory_usage := MemResults
    } = Results,

    %% Normalize scores (0-100 scale)
    ProcessScore = calculate_process_score(ProcResults),
    MessageScore = calculate_message_score(MsgResults),
    SupervisorScore = calculate_supervisor_score(SupResults),
    MemoryScore = calculate_memory_score(MemResults),

    %% Weighted average
    ProcessScore * 0.4 + MessageScore * 0.3 + SupervisorScore * 0.2 + MemoryScore * 0.1.

calculate_process_score(Results) ->
    DirectSpawn = maps:get(direct_spawn, Results),
    GenServer = maps:get(gen_server, Results),
    Supervisor = maps:get(supervisor_children, Results),

    Normalize = fun(Value, Max) -> min(100, (Value / Max) * 100) end,

    %% Normalize to expected maximums
    SpawnNorm = Normalize(maps:get(spawns_per_second, DirectSpawn), 100000),
    GenNorm = Normalize(maps:get(servers_per_second, GenServer), 5000),
    SuperNorm = Normalize(maps:get(children_per_second, Supervisor), 2000),

    (SpawnNorm + GenNorm + SuperNorm) / 3.

calculate_message_score(Results) ->
    Local = maps:get(local, Results),
    Registered = maps:get(registered, Results),
    Self = maps:get(self, Results),

    Normalize = fun(Value, Max) -> min(100, (Value / Max) * 100) end,

    LocalNorm = Normalize(maps:get(msg_per_second, Local), 50000),
    RegNorm = Normalize(maps:get(msg_per_second, Registered), 50000),
    SelfNorm = Normalize(maps:get(msg_per_second, Self), 100000),

    (LocalNorm + RegNorm + SelfNorm) / 3.

calculate_supervisor_score(Results) ->
    Startup = maps:get(startup, Results),
    Restarts = maps:get(restarts, Results),
    Strategies = maps:get(strategies, Results),

    Normalize = fun(Value, Max) -> min(100, (Value / Max) * 100) end,

    StartupNorm = Normalize(maps:get(startups_per_second, Startup), 2000),
    RestartNorm = Normalize(maps:get(restarts_per_second, Restarts), 500),
    OneForAllNorm = Normalize(maps:get(per_second, maps:get(one_for_all, Strategies)), 3000),
    SimpleNorm = Normalize(maps:get(per_second, maps:get(simple_one_for_one, Strategies)), 3000),

    (StartupNorm + RestartNorm + OneForAllNorm + SimpleNorm) / 4.

calculate_memory_score(Results) ->
    SpawnMem = maps:get(spawn_memory, Results),
    GenMem = maps:get(gen_server_memory, Results),
    RegMem = maps:get(registered_memory, Results),

    %% Lower memory usage is better, so invert the score
    Normalize = fun(Value) -> max(0, 100 - (Value / 1000000) * 10) end,

    SpawnNorm = Normalize(maps:get(avg_per_process, SpawnMem)),
    GenNorm = Normalize(maps:get(avg_per_server, GenMem)),
    RegNorm = Normalize(maps:get(avg_per_process, RegMem)),

    (SpawnNorm + GenNorm + RegNorm) / 3.

%% Save results to file
save_results(Results, Analysis) ->
    Filename = "benchmark_results_" ++ integer_to_list(erlang:system_time(millisecond)) ++ ".json",

    JsonData = jsone:encode(Results, [{pretty, true}]),

    case file:write_file(Filename, JsonData) of
        ok ->
            io:format("~nResults saved to: ~s~n", [Filename]);
        {error, Reason} ->
            ?LOG_ERROR("Failed to save results: ~p", [Reason])
    end,

    %% Generate detailed report
    generate_detailed_report(Results, Analysis).

%% Generate detailed report
generate_detailed_report(Results, Analysis) ->
    Report = #{
        summary => #{
            version => maps:get(version, Results),
            timestamp => maps:get(timestamp, Results),
            performance_score => calculate_performance_score(Results)
        },
        benchmarks => Results,
        analysis => Analysis,
        recommendations => generate_recommendations(Results)
    },

    ReportFilename = "performance_report_" ++ integer_to_list(erlang:system_time(millisecond)) ++ ".md",

    ReportContent = format_report(Report),

    case file:write_file(ReportFilename, ReportContent) of
        ok ->
            io:format("Detailed report saved to: ~s~n", [ReportFilename]);
        {error, Reason} ->
            ?LOG_ERROR("Failed to save report: ~p", [Reason])
    end.

%% Generate optimization recommendations
generate_recommendations(Results) ->
    #{
        process_creation := ProcResults,
        message_passing := MsgResults,
        supervisor_overhead := SupResults,
        memory_usage := MemResults
    } = Results,

    Recommendations = [],

    %% Process creation recommendations
    DirectSpawn = maps:get(spawns_per_second, maps:get(direct_spawn, ProcResults)),
    if DirectSpawn < 50000 ->
        recommendations(["Process creation could be improved with process pooling"]);
    true ->
        ok
    end,

    %% Message passing recommendations
    LocalMsg = maps:get(msg_per_second, maps:get(local, MsgResults)),
    if LocalMsg < 30000 ->
        recommendations(["Consider message batching for higher throughput"]);
    true ->
        ok
    end,

    %% Supervisor recommendations
    Startup = maps:get(startups_per_second, maps:get(startup, SupResults)),
    if Startup < 1000 ->
        recommendations(["Use simple_one_for_one supervisors for better performance"]);
    true ->
        ok
    end,

    %% Memory recommendations
    SpawnMem = maps:get(avg_per_process, maps:get(spawn_memory, MemResults)),
    if SpawnMem > 50000000 ->
        recommendations(["Implement object pooling to reduce memory allocation"]);
    true ->
        ok
    end,

    Recommendations.

format_report(Report) ->
    #{
        summary := Summary,
        benchmarks := Benchmarks,
        analysis := _Analysis,
        recommendations := Recommendations
    } = Report,

    "OTP Performance Benchmark Report\n" ++
    "===============================\n\n" ++
    "Version: " ++ integer_to_list(maps:get(version, Summary)) ++ "\n" ++
    "Timestamp: " ++ integer_to_list(maps:get(timestamp, Summary)) ++ "\n" ++
    "Performance Score: " ++ float_to_list(maps:get(performance_score, Summary), [{decimals, 2}]) ++ "\n\n" ++

    "Recommendations:\n" ++
    "---------------\n" ++
    string:join(Recommendations, "\n") ++ "\n".

recommendations(List) ->
    List.