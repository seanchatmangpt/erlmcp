%%====================================================================
%% PERFORMANCE DEBUGGING EXAMPLE - CLI Integration
%%====================================================================
%% This module demonstrates practical debugging and performance
%% optimization workflows using Claude Code CLI patterns.
%%====================================================================

-module(performance_debug_example).

-export([
    run_debug_session/0,
    profile_json_encoding/0,
    detect_memory_leak/1,
    analyze_bottleneck/0
]).

-include_lib("kernel/include/logger.hrl").

%%====================================================================
%% DEBUG SESSION EXAMPLE
%%====================================================================

-spec run_debug_session() -> ok.
run_debug_session() ->
    ?LOG_INFO("Starting performance debug session"),
    
    %% 1. Compile check
    {ok, _} = rebar3_compile:compile(),
    
    %% 2. Start monitoring
    Monitor = spawn_link(fun() -> monitor_loop(#{}) end),
    
    %% 3. Run workload with monitoring
    erlmcp_bench_core_ops:run(<<"core_ops_100k">>),
    
    %% 4. Collect metrics
    Monitor ! {get_metrics, self()},
    receive
        {metrics, Metrics} ->
            analyze_metrics(Metrics)
    after 5000 ->
        ?LOG_ERROR("Monitor timeout"),
        exit(monitor_timeout)
    end.

%%====================================================================
%% JSON ENCODING PROFILING
%%====================================================================

-spec profile_json_encoding() -> ok.
profile_json_encoding() ->
    ?LOG_INFO("Profiling JSON encoding performance"),
    
    %% Create test data
    TestData = generate_test_data(1000),
    
    %% Profile jsx
    fprof:trace([start, {procs, [self()]}, {sort, self}]),
    jsx_encode_benchmark jsx, TestData,
    fprof:profile(),
    fprof:analyse([{dest, "jsx_profile.txt"}]),
    fprof:stop(),
    
    %% Profile jiffy
    fprof:trace([start, {procs, [self()]}, {sort, self}]),
    jsx_encode_benchmark jiffy, TestData,
    fprof:profile(),
    fprof:analyse([{dest, "jiffy_profile.txt"}]),
    fprof:stop(),
    
    %% Compare results
    compare_encoders().

%%====================================================================
%% MEMORY LEAK DETECTION
%%====================================================================

-spec detect_memory_leak(DurationMs :: pos_integer()) -> ok.
detect_memory_leak(DurationMs) ->
    ?LOG_INFO("Starting memory leak detection for ~p ms", [DurationMs]),
    
    InitialMemory = process_info(self(), memory),
    ?LOG_INFO("Initial memory: ~p", [InitialMemory]),
    
    %% Run workload in loop
    EndTime = erlang:monotonic_time(millisecond) + DurationMs,
    memory_loop(EndTime, [InitialMemory]).
    
memory_loop(EndTime, Samples) ->
    Now = erlang:monotonic_time(millisecond),
    
    if
        Now >= EndTime ->
            analyze_memory_samples(Samples);
        true ->
            Memory = process_info(self(), memory),
            NewSamples = [Memory | Samples],
            timer:sleep(1000),
            memory_loop(EndTime, NewSamples)
    end.

%%====================================================================
%% BOTTLENECK ANALYSIS
%%====================================================================

-spec analyze_bottleneck() -> ok.
analyze_bottleneck() ->
    ?LOG_INFO("Starting bottleneck analysis"),
    
    %% Test different components
    RegistryResults = erlmcp_bench_core_ops:run(<<"registry_test_100k">>),
    QueueResults = erlmcp_bench_core_ops:run(<<"queue_test_100k">>),
    SessionResults = erlmcp_bench_core_ops:run(<<"session_test_100k">>),
    
    %% Analyze results
    analyze_bottlenecks([RegistryResults, QueueResults, SessionResults]).

%%====================================================================
%% PRIVATE HELPER FUNCTIONS
%%====================================================================

monitor_loop(State) ->
    receive
        {get_metrics, Pid} ->
            Metrics = collect_current_metrics(),
            Pid ! {metrics, Metrics},
            monitor_loop(State);
        stop ->
            ok
    after 1000 ->
        monitor_loop(State)
    end.

collect_current_metrics() ->
    #{
        memory => process_info(self(), memory),
        messages => process_info(self(), message_queue_len),
        status => process_info(self(), status)
    }.

analyze_metrics(Metrics) ->
    ?LOG_INFO("Performance metrics: ~p", [Metrics]),
    
    %% Check for issues
    #{memory := {memory, Mem}} = Metrics,
    if
        Mem > 100000000 ->  % 100MB
            ?LOG_WARNING("High memory usage: ~p bytes", [Mem]);
        true ->
            ok
    end.

generate_test_data(Size) ->
    generate_test_data(Size, []).

generate_test_data(0, Acc) -> Acc;
generate_test_data(N, Acc) ->
    Data = #{<<"id">> => integer_to_binary(N),
             <<"name">> => <<"test_item">>,
             <<"value">> => N},
    generate_test_data(N-1, [Data | Acc]).

jsx_encode_benchmark(Library, Data) ->
    case Library of
        jsx ->
            lists:foreach(fun(D) -> jsx:encode(D) end, Data);
        jiffy ->
            lists:foreach(fun(D) -> jiffy:encode(D) end, Data)
    end.

compare_encoders() ->
    %% Parse profile files and compare
    jsx_time = extract_time_from_profile("jsx_profile.txt"),
    jiffy_time = extract_time_from_profile("jiffy_profile.txt"),
    
    Improvement = ((jsx_time - jiffy_time) / jsx_time) * 100,
    ?LOG_INFO("Jiffy is ~p% faster than jsx", [Improvement]).

extract_time_from_profile(File) ->
    %% Implementation to extract total time from profile
    0.  % Placeholder
    
analyze_memory_samples(Samples) ->
    %% Linear regression to detect memory growth
    Slope = calculate_memory_growth(Samples),
    if
        Slope > 1048576 ->  % 1 MB/min
            ?LOG_ERROR("Memory leak detected: ~p bytes/min", [Slope]);
        true ->
            ?LOG_INFO("No significant memory growth detected")
    end.

calculate_memory_growth(Samples) ->
    %% Calculate memory growth rate
    0.  % Placeholder

analyze_bottlenecks(Results) ->
    %% Analyze results to identify bottlenecks
    lists:foreach(fun(Result) ->
        Throughput = proplists:get_value(throughput_msg_per_s, Result, 0),
        ThroughputType = proplists:get_value(benchmark, Result, "unknown"),
        
        case ThroughputType of
            "registry" when Throughput < 400000 ->
                ?LOG_WARNING("Registry bottleneck: ~p msg/s", [Throughput]);
            "queue" when Throughput < 800000 ->
                ?LOG_WARNING("Queue bottleneck: ~p msg/s", [Throughput]);
            _ ->
                ok
        end
    end, Results).
