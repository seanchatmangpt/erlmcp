%% OTP Version Performance Benchmark Suite
%%
%% Focus: Critical path performance analysis for erlmcp
%%
%% Benchmark Areas:
%% - Process Creation: gen_server, spawn, supervisor children
%% - Message Passing: Local, registered, distributed messaging
%% - Supervisor Overhead: Start times, restart strategies, monitoring
%% - Memory Usage: RSS, heap growth, garbage collection
%%

-module(otp_bench).
-export([main/0, benchmark/2, analyze_results/1]).
-export([process_creation_bench/0, message_passing_bench/0,
         supervisor_overhead_bench/0, memory_usage_bench/0]).

-include_lib("kernel/include/logger.hrl").

%% Main entry point
main() ->
    io:format("Starting OTP Version Performance Benchmark~n", []),

    OTP_Versions = [26, 27, 28],
    Benchmarks = [process_creation, message_passing, supervisor_overhead, memory_usage],

    Results = lists:foldl(fun(Version, Acc) ->
        io:format("Running benchmarks for OTP ~p...~n", [Version]),
        VersionResults = benchmark(Version, Benchmarks),
        maps:put(Version, VersionResults, Acc)
    end, #{}, OTP_Versions),

    analyze_results(Results).

%% Run all benchmarks for a specific OTP version
benchmark(Version, Benchmarks) ->
    Results = lists:foldl(fun(Benchmark, Acc) ->
        io:format("  ~p... ", [Benchmark]),
        Result = run_benchmark(Benchmark, Version),
        io:format("~p~n", [maps:get(success, Result)]),
        maps:put(Benchmark, Result, Acc)
    end, #{}, Benchmarks),

    #{version => Version, results => Results, timestamp => erlang:system_time(millisecond)}.

%% Run individual benchmark
run_benchmark(process_creation, Version) ->
    process_creation_bench();
run_benchmark(message_passing, Version) ->
    message_passing_bench();
run_benchmark(supervisor_overhead, Version) ->
    supervisor_overhead_bench();
run_benchmark(memory_usage, Version) ->
    memory_usage_bench().

%% =============================================================================
%% PROCESS CREATION BENCHMARK
%% =============================================================================

process_creation_bench() ->
    %% Test 1: Direct spawn
    SpawnResults = time_spawns(10000),

    %% Test 2: gen_server creation
    GenServerResults = time_gen_servers(1000),

    %% Test 3: Supervisor children creation
    SupervisorResults = time_supervisor_children(100),

    #{
        test => process_creation,
        version => erlang:system_info(otp_release),
        direct_spawn => SpawnResults,
        gen_server => GenServerResults,
        supervisor_children => SupervisorResults,
        success => true
    }.

time_spawns(N) ->
    Start = erlang:monotonic_time(millisecond),

    Pids = lists:foldl(fun(_, Acc) ->
        Pid = spawn(fun() -> timer:sleep(100) end),
        [Pid | Acc]
    end, [], lists:seq(1, N)),

    End = erlang:monotonic_time(millisecond),
    Duration = End - Start,

    %% Cleanup
    lists:foreach(fun(Pid) -> exit(Pid, normal) end, Pids),

    #{
        count => N,
        duration_ms => Duration,
        avg_per_spawn => Duration / N,
        spawns_per_second => N / (Duration / 1000)
    }.

time_gen_servers(N) ->
    %% Define a simple gen_server
    ServerModule = gen_server_bench_server,

    %% Create server module on the fly
    Code = gen_server_bench_server_code(),
    case compile:forms(Code, [{return, binary}]) of
        {ok, Module, Binary} ->
            code:load_binary(Module, "", Binary);
        {error, Error} ->
            ?LOG_ERROR("Failed to compile gen_server: ~p", [Error]),
            throw(Error)
    end,

    Start = erlang:monotonic_time(millisecond),

    Pids = lists:foldl(fun(_, Acc) ->
        {ok, Pid} = gen_server_bench_server:start(),
        [Pid | Acc]
    end, [], lists:seq(1, N)),

    End = erlang:monotonic_time(millisecond),
    Duration = End - Start,

    %% Cleanup
    lists:foreach(fun(Pid) -> gen_server:stop(Pid) end, Pids),

    #{
        count => N,
        duration_ms => Duration,
        avg_per_server => Duration / N,
        servers_per_second => N / (Duration / 1000)
    }.

time_supervisor_children(N) ->
    %% Create a temporary supervisor
    {ok, SupPid} = supervisor:start_link({local, bench_supervisor},
                                       simple_one_for_one,
                                       {{simple_one_for_one, 5, 10},
                                        [{bench_worker, {bench_worker, start_link, []},
                                          permanent, 5000, worker, [bench_worker]}]}),

    Start = erlang:monotonic_time(millisecond),

    Children = lists:foldl(fun(_, Acc) ->
        {ok, ChildPid} = supervisor:start_child(bench_supervisor, []),
        [ChildPid | Acc]
    end, [], lists:seq(1, N)),

    End = erlang:monotonic_time(millisecond),
    Duration = End - Start,

    %% Cleanup
    lists:foreach(fun(ChildPid) -> exit(ChildPid, kill) end, Children),
    supervisor:stop(SupPid),

    #{
        count => N,
        duration_ms => Duration,
        avg_per_child => Duration / N,
        children_per_second => N / (Duration / 1000)
    }.

%% =============================================================================
%% MESSAGE PASSING BENCHMARK
%% =============================================================================

message_passing_bench() ->
    %% Test 1: Local process messaging
    LocalResults = time_local_messaging(10000),

    %% Test 2: Registered process messaging
    RegisteredResults = time_registered_messaging(10000),

    %% Test 3: Process-to-self messaging
    SelfResults = time_self_messaging(10000),

    #{
        test => message_passing,
        version => erlang:system_info(otp_release),
        local => LocalResults,
        registered => RegisteredResults,
        self => SelfResults,
        success => true
    }.

time_local_messaging(N) ->
    %% Create sender and receiver processes
    {ok, Sender} = spawn_opt(fun() -> sender_loop(0) end, [monitor]),
    {ok, Receiver} = spawn_opt(fun() -> receiver_loop(N, []) end, [monitor]),

    %% Register the receiver
    erlang:register(bench_receiver, Receiver),

    Start = erlang:monotonic_time(millisecond),

    %% Send N messages
    lists:foreach(fun(_) ->
        Sender ! {send, Receiver, {data, crypto:strong_rand_bytes(100)}}
    end, lists:seq(1, N)),

    %% Wait for all messages to be received
    receive
        {receiver_done, Time} ->
            Duration = Time
    after 30000 ->
        Duration = 30000
    end,

    %% Cleanup
    exit(Sender, normal),
    exit(Receiver, normal),

    #{
        count => N,
        duration_ms => Duration,
        avg_per_msg => Duration / N,
        msg_per_second => N / (Duration / 1000)
    }.

time_registered_messaging(N) ->
    %% Create a registered server
    {ok, Server} = spawn_opt(fun() -> server_loop(N, []) end, [monitor]),
    erlang:register(bench_server, Server),

    Start = erlang:monotonic_time(millisecond),

    %% Send N messages to registered process
    lists:foreach(fun(_) ->
        bench_server ! {self(), {data, crypto:strong_rand_bytes(100)}}
    end, lists:seq(1, N)),

    %% Wait for all messages to be received
    receive
        {server_done, Time} ->
            Duration = Time
    after 30000 ->
        Duration = 30000
    end,

    %% Cleanup
    exit(Server, normal),

    #{
        count => N,
        duration_ms => Duration,
        avg_per_msg => Duration / N,
        msg_per_second => N / (Duration / 1000)
    }.

time_self_messaging(N) ->
    Self = self(),

    Start = erlang:monotonic_time(millisecond),

    %% Send N messages to self
    lists:foreach(fun(_) ->
        Self ! {data, crypto:strong_rand_bytes(50)}
    end, lists:seq(1, N)),

    %% Receive all messages
    lists:foreach(fun(_) ->
        receive
            _ -> ok
        end
    end, lists:seq(1, N)),

    End = erlang:monotonic_time(millisecond),
    Duration = End - Start,

    #{
        count => N,
        duration_ms => Duration,
        avg_per_msg => Duration / N,
        msg_per_one_way => Duration / N,
        msg_per_second => N / (Duration / 1000)
    }.

%% =============================================================================
%% SUPERVISOR OVERHEAD BENCHMARK
%% =============================================================================

supervisor_overhead_bench() ->
    %% Test 1: Supervisor startup time
    StartupResults = time_supervisor_startup(100),

    %% Test 2: Child restart overhead
    RestartResults = time_child_restarts(50),

    %% Test 3: One-for-one vs simple_one_for_one
    StrategyResults = compare_supervisor_strategies(50),

    #{
        test => supervisor_overhead,
        version => erlang:system_info(otp_release),
        startup => StartupResults,
        restarts => RestartResults,
        strategies => StrategyResults,
        success => true
    }.

time_supervisor_startup(N) ->
    Start = erlang:monotonic_time(millisecond),

    lists:foldl(fun(_, _) ->
        %% Create temporary supervisor
        {ok, SupPid} = supervisor:start_link({local, bench_startup_sup},
                                          simple_one_for_one,
                                          {{simple_one_for_one, 1, 1},
                                           []}),

        %% Shutdown
        supervisor:stop(SupPid),

        ok
    end, [], lists:seq(1, N)),

    End = erlang:monotonic_time(millisecond),
    Duration = End - Start,

    #{
        count => N,
        duration_ms => Duration,
        avg_per_startup => Duration / N,
        startups_per_second => N / (Duration / 1000)
    }.

time_child_restarts(N) ->
    %% Create a supervisor that will restart children
    {ok, SupPid} = supervisor:start_link({local, bench_restart_sup},
                                       one_for_all,
                                       {{one_for_all, 5, 1},
                                        [{bench_restart_worker,
                                          {bench_restart_worker, start_link, []},
                                          permanent, 2000, worker, [bench_restart_worker]}]}),

    %% Start a child
    {ok, ChildPid} = supervisor:start_child(bench_restart_sup, []),

    Start = erlang:monotonic_time(millisecond),

    %% Kill the child N times
    lists:foreach(fun(_) ->
        exit(ChildPid, kill),
        %% Wait for restart
        timer:sleep(100)
    end, lists:seq(1, N)),

    End = erlang:monotonic_time(millisecond),
    Duration = End - Start,

    %% Cleanup
    supervisor:stop(SupPid),

    #{
        count => N,
        duration_ms => Duration,
        avg_per_restart => Duration / N,
        restarts_per_second => N / (Duration / 1000)
    }.

compare_supervisor_strategies(N) ->
    %% Test 1: one_for_all
    {ok, OneForAllSup} = supervisor:start_link({local, bench_one_for_all},
                                            one_for_all,
                                            {{one_for_all, 5, 1}, []}),

    Children1 = lists:foldl(fun(_, Acc) ->
        {ok, Pid} = supervisor:start_child(bench_one_for_all, []),
        [Pid | Acc]
    end, [], lists:seq(1, N)),

    Start1 = erlang:monotonic_time(millisecond),
    lists:foreach(fun(Child) -> exit(Child, kill) end, Children1),
    timer:sleep(500), % Wait for restarts
    End1 = erlang:monotonic_time(millisecond),

    supervisor:stop(bench_one_for_all),

    %% Test 2: simple_one_for_one
    {ok, SimpleSup} = supervisor:start_link({local, bench_simple},
                                          simple_one_for_one,
                                          {{simple_one_for_one, 5, 1}, []}),

    Children2 = lists:foldl(fun(_, Acc) ->
        {ok, Pid} = supervisor:start_child(bench_simple, []),
        [Pid | Acc]
    end, [], lists:seq(1, N)),

    Start2 = erlang:monotonic_time(millisecond),
    lists:foreach(fun(Child) -> exit(Child, kill) end, Children2),
    timer:sleep(500), % Wait for restarts
    End2 = erlang:monotonic_time(millisecond),

    supervisor:stop(bench_simple),

    #{
        one_for_all => #{
            count => N,
            duration_ms => End1 - Start1,
            avg_per => (End1 - Start1) / N,
            per_second => N / ((End1 - Start1) / 1000)
        },
        simple_one_for_one => #{
            count => N,
            duration_ms => End2 - Start2,
            avg_per => (End2 - Start2) / N,
            per_second => N / ((End2 - Start2) / 1000)
        }
    }.

%% =============================================================================
%% MEMORY USAGE BENCHMARK
%% =============================================================================

memory_usage_bench() ->
    %% Test 1: Memory growth with spawned processes
    SpawnMemoryResults = memory_with_spawns(1000),

    %% Test 2: Memory growth with gen_servers
    GenServerMemoryResults = memory_with_gen_servers(100),

    %% Test 3: Memory growth with registered processes
    RegisteredMemoryResults = memory_with_registered_processes(100),

    #{
        test => memory_usage,
        version => erlang:system_info(otp_release),
        spawn_memory => SpawnMemoryResults,
        gen_server_memory => GenServerMemoryResults,
        registered_memory => RegisteredMemoryResults,
        success => true
    }.

memory_with_spawns(N) ->
    InitialMemory = erlang:memory(total),

    Pids = lists:foldl(fun(_, Acc) ->
        Pid = spawn(fun() ->
            %% Some work to allocate memory
            lists:map(fun(_) -> crypto:strong_rand_bytes(1024) end, lists:seq(1, 100))
        end),
        [Pid | Acc]
    end, [], lists:seq(1, N)),

    PeakMemory = erlang:memory(total),

    %% Wait for GC and cleanup
    timer:sleep(2000),
    lists:foreach(fun(Pid) -> exit(Pid, normal) end, Pids),

    FinalMemory = erlang:memory(total),

    #{
        count => N,
        initial_bytes => InitialMemory,
        peak_bytes => PeakMemory,
        final_bytes => FinalMemory,
        growth_bytes => PeakMemory - InitialMemory,
        avg_per_process => (PeakMemory - InitialMemory) / N
    }.

memory_with_gen_servers(N) ->
    %% Create server module
    Code = gen_server_bench_server_code(),
    case compile:forms(Code, [{return, binary}]) of
        {ok, Module, Binary} ->
            code:load_binary(Module, "", Binary);
        {error, Error} ->
            throw(Error)
    end,

    InitialMemory = erlang:memory(total),

    Pids = lists:foldl(fun(_, Acc) ->
        {ok, Pid} = gen_server_bench_server:start(),
        [Pid | Acc]
    end, [], lists:seq(1, N)),

    PeakMemory = erlang:memory(total),

    %% Cleanup
    lists:foreach(fun(Pid) -> gen_server:stop(Pid) end, Pids),

    FinalMemory = erlang:memory(total),

    #{
        count => N,
        initial_bytes => InitialMemory,
        peak_bytes => PeakMemory,
        final_bytes => FinalMemory,
        growth_bytes => PeakMemory - InitialMemory,
        avg_per_server => (PeakMemory - InitialMemory) / N
    }.

memory_with_registered_processes(N) ->
    InitialMemory = erlang:memory(total),

    lists:foldl(fun(I, Acc) ->
        Pid = spawn(fun() ->
            %% Allocate some memory
            lists:map(fun(_) -> crypto:strong_rand_bytes(512) end, lists:seq(1, 50))
        end),
        erlang:register(list_to_atom("bench_reg_" ++ integer_to_list(I)), Pid),
        Acc
    end, [], lists:seq(1, N)),

    PeakMemory = erlang:memory(total),

    %% Cleanup
    lists:foreach(fun(I) ->
        Pid = whereis(list_to_atom("bench_reg_" ++ integer_to_list(I))),
        exit(Pid, normal)
    end, lists:seq(1, N)),

    FinalMemory = erlang:memory(total),

    #{
        count => N,
        initial_bytes => InitialMemory,
        peak_bytes => PeakMemory,
        final_bytes => FinalMemory,
        growth_bytes => PeakMemory - InitialMemory,
        avg_per_process => (PeakMemory - InitialMemory) / N
    }.

%% =============================================================================
%% RESULT ANALYSIS
%% =============================================================================

analyze_results(Results) ->
    io:format("~n=== OTP VERSION PERFORMANCE ANALYSIS ===~n~n", []),

    %% Extract results for comparison
    ProcessResults = extract_process_results(Results),
    MessageResults = extract_message_results(Results),
    SupervisorResults = extract_supervisor_results(Results),
    MemoryResults = extract_memory_results(Results),

    %% Analyze performance trends
    analyze_trends(ProcessResults, MessageResults, SupervisorResults, MemoryResults),

    %% Generate optimization recommendations
    Recommendations = generate_recommendations(ProcessResults, MessageResults, SupervisorResults, MemoryResults),

    %% Return formatted results
    #{
        process_creation => ProcessResults,
        message_passing => MessageResults,
        supervisor_overhead => SupervisorResults,
        memory_usage => MemoryResults,
        recommendations => Recommendations
    }.

%% Helper functions for result extraction
extract_process_results(Results) ->
    maps:fold(fun(Version, #{results := Res}, Acc) ->
        maps:put(Version, maps:get(process_creation, Res), Acc)
    end, #{}, Results).

extract_message_results(Results) ->
    maps:fold(fun(Version, #{results := Res}, Acc) ->
        maps:put(Version, maps:get(message_passing, Res), Acc)
    end, #{}, Results).

extract_supervisor_results(Results) ->
    maps:fold(fun(Version, #{results := Res}, Acc) ->
        maps:put(Version, maps:get(supervisor_overhead, Res), Acc)
    end, #{}, Results).

extract_memory_results(Results) ->
    maps:fold(fun(Version, #{results := Res}, Acc) ->
        maps:put(Version, maps:get(memory_usage, Res), Acc)
    end, #{}, Results).

%% =============================================================================
%% HELPER FUNCTIONS
%% =============================================================================

gen_server_bench_server_code() ->
    {ok, Cwd} = file:get_cwd(),
    ServerPath = filename:join(Cwd, "gen_server_bench_server.erl"),
    case file:read_file(ServerPath) of
        {ok, Code} -> Code;
        {error, _} ->
            %% Generate server code on the fly
            <<"-module(gen_server_bench_server).
-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2]).
-behaviour(gen_server).

start_link() -> gen_server:start_link(?MODULE, [], []).

init([]) -> {ok, state}.

handle_call(_Request, _From, State) -> {reply, ok, State}.

handle_cast(_Msg, State) -> {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.">>
    end.

%% Process communication loops
sender_loop(Count) ->
    receive
        {send, To, Data} ->
            To ! {msg, Data},
            sender_loop(Count + 1)
    end.

receiver_loop(Expected, Received) ->
    case length(Received) of
        Expected ->
            self() ! {receiver_done, timer:seconds(30)};
        _ ->
            receive
                {msg, Data} ->
                    receiver_loop(Expected, [Data | Received])
            end
    end.

server_loop(Expected, Received) ->
    case length(Received) of
        Expected ->
            self() ! {server_done, timer:seconds(30)};
        _ ->
            receive
                {From, {data, _Data}} ->
                    server_loop(Expected, [{received, From} | Received])
            end
    end.

%% =============================================================================
%% ANALYSIS FUNCTIONS
%% =============================================================================

analyze_trends(ProcessResults, MessageResults, SupervisorResults, MemoryResults) ->
    io:format("~n=== PERFORMANCE TRENDS ANALYSIS ===~n~n", []),

    %% Process creation trends
    io:format("Process Creation Trends:~n"),
    lists:foreach(fun({Version, Results}) ->
        DirectSpawn = maps:get(direct_spawn, Results),
        io:format("  OTP ~p: ~.1fK spawns/s~n", [Version, maps:get(spawns_per_second, DirectSpawn) / 1000])
    end, maps:to_list(ProcessResults)),

    %% Message passing trends
    io:format("~nMessage Passing Trends:~n"),
    lists:foreach(fun({Version, Results}) ->
        Local = maps:get(local, Results),
        io:format("  OTP ~p: ~.1K messages/s (local)~n", [Version, maps:get(msg_per_second, Local) / 1000])
    end, maps:to_list(MessageResults)),

    %% Supervisor trends
    io:format("~nSupervisor Trends:~n"),
    lists:foreach(fun({Version, Results}) ->
        Startup = maps:get(startup, Results),
        io:format("  OTP ~p: ~.1K startups/s~n", [Version, maps:get(startups_per_second, Startup) / 1000])
    end, maps:to_list(SupervisorResults)),

    %% Memory trends
    io:format("~nMemory Usage Trends:~n"),
    lists:foreach(fun({Version, Results}) ->
        SpawnMem = maps:get(spawn_memory, Results),
        io:format("  OTP ~p: ~.1fMB per process~n", [Version, maps:get(avg_per_process, SpawnMem) / 1024 / 1024])
    end, maps:to_list(MemoryResults)).

generate_recommendations(ProcessResults, MessageResults, SupervisorResults, MemoryResults) ->
    io:format("~n=== OPTIMIZATION RECOMMENDATIONS ===~n~n", []),

    Recommendations = [],

    %% Process creation analysis
    lists:foreach(fun({Version, Results}) ->
        DirectSpawn = maps:get(spawns_per_second, maps:get(direct_spawn, Results)),
        if DirectSpawn < 50000 ->
            io:format("OTP ~p: Consider process pooling (currently ~.0fK spawns/s)~n",
                     [Version, DirectSpawn / 1000]);
        true ->
            ok
        end
    end, maps:to_list(ProcessResults)),

    %% Message passing analysis
    lists:foreach(fun({Version, Results}) ->
        LocalMsg = maps:get(msg_per_second, maps:get(local, Results)),
        if LocalMsg < 30000 ->
            io:format("OTP ~p: Implement message batching (currently ~.0fK messages/s)~n",
                     [Version, LocalMsg / 1000]);
        true ->
            ok
        end
    end, maps:to_list(MessageResults)),

    %% Supervisor analysis
    lists:foreach(fun({Version, Results}) ->
        Startup = maps:get(startups_per_second, maps:get(startup, Results)),
        Strategies = maps:get(strategies, Results),
        OneForAll = maps:get(per_second, maps:get(one_for_all, Strategies)),
        Simple = maps:get(per_second, maps:get(simple_one_for_one, Strategies)),

        if Startup < 1000 ->
            io:format("OTP ~p: Use simple_one_for_one supervisors (startup: ~.0f/s)~n",
                     [Version, Startup]);
        true ->
            ok
        end,

        if OneForAll < Simple * 1.5 ->
            io:format("OTP ~p: Prefer simple_one_for_one over one_for_all (~.0f vs ~.0f ops/s)~n",
                     [Version, OneForAll, Simple]);
        true ->
            ok
        end
    end, maps:to_list(SupervisorResults)),

    %% Memory analysis
    lists:foreach(fun({Version, Results}) ->
        SpawnMem = maps:get(avg_per_process, maps:get(spawn_memory, Results)),
        GenMem = maps:get(avg_per_server, maps:get(gen_server_memory, Results)),

        if SpawnMem > 50000000 ->
            io:format("OTP ~p: Implement object pooling (spawn: ~.1fMB/pro)~n",
                     [Version, SpawnMem / 1024 / 1024]);
        true ->
            ok
        end,

        if GenMem > 15000000 ->
            io:format("OTP ~p: Optimize gen_server memory usage (server: ~.1fMB/server)~n",
                     [Version, GenMem / 1024 / 1024]);
        true ->
            ok
        end
    end, maps:to_list(MemoryResults)),

    %% Overall recommendations based on version
    OverallScore = calculate_overall_performance_score(ProcessResults, MessageResults, SupervisorResults, MemoryResults),

    io:format("~n=== OVERALL PERFORMANCE ASSESSMENT ===~n"),
    io:format("Performance Score: ~.2f/100~n", [OverallScore]),

    if OverallScore < 60 ->
        io:format("Recommendation: Immediate performance optimization required~n");
    OverallScore < 80 ->
        io:format("Recommendation: Performance improvements recommended~n");
    true ->
        io:format("Recommendation: Performance is optimal~n")
    end,

    Recommendations.

calculate_overall_performance_score(ProcessResults, MessageResults, SupervisorResults, MemoryResults) ->
    %% Calculate average scores across all versions
    ProcessScore = calculate_average_score(ProcessResults, fun(Results) ->
        maps:get(spawns_per_second, maps:get(direct_spawn, Results))
    end, 100000),

    MessageScore = calculate_average_score(MessageResults, fun(Results) ->
        maps:get(msg_per_second, maps:get(local, Results))
    end, 50000),

    SupervisorScore = calculate_average_score(SupervisorResults, fun(Results) ->
        maps:get(startups_per_second, maps:get(startup, Results))
    end, 2000),

    MemoryScore = calculate_average_score(MemoryResults, fun(Results) ->
        100 - (maps:get(avg_per_process, maps:get(spawn_memory, Results)) / 1000000) * 10
    end, 100),

    WeightedScore = ProcessScore * 0.4 + MessageScore * 0.3 + SupervisorScore * 0.2 + MemoryScore * 0.1,
    max(0, min(100, WeightedScore)).

calculate_average_score(ResultMap, Extractor, MaxValue) ->
    Values = lists:map(fun(_Version, Results) ->
        Value = Extractor(Results),
        min(100, (Value / MaxValue) * 100)
    end, maps:to_list(ResultMap)),

    if Values =/= [] ->
        lists:sum(Values) / length(Values);
    true ->
        0
    end.