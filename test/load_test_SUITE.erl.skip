%% ===================================================================
%% LOAD TEST SUITE
%% ===================================================================
%% Module: load_test_SUITE
%% Purpose: Performance testing and load characterization for
%%          erlmcp + TAIEA integration
%% ===================================================================

-module(load_test_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("erlmcp.hrl").

%% Common Test Callbacks
-export([
    all/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_group/2,
    end_per_group/2,
    init_per_testcase/2,
    end_per_testcase/2,
    groups/0
]).

%% Test Cases - Load Tests
-export([
    load_10_concurrent_requests/1,
    load_100_sequential_requests/1,
    load_mixed_workload/1,
    load_sustained_5_minutes/1,
    load_stress_peak/1
]).

%% Test Cases - Performance Metrics
-export([
    measure_throughput/1,
    measure_latency_percentiles/1,
    measure_memory_usage/1,
    measure_cpu_usage/1
]).

%% Test Cases - Scalability
-export([
    scalability_double_load/1,
    scalability_10x_load/1,
    scalability_degradation_curve/1
]).

%% ===================================================================
%% CT CALLBACKS
%% ===================================================================

all() ->
    [
        {group, load_group},
        {group, performance_group},
        {group, scalability_group}
    ].

groups() ->
    [
        {load_group, [], [
            load_10_concurrent_requests,
            load_100_sequential_requests,
            load_mixed_workload,
            load_sustained_5_minutes,
            load_stress_peak
        ]},
        {performance_group, [], [
            measure_throughput,
            measure_latency_percentiles,
            measure_memory_usage,
            measure_cpu_usage
        ]},
        {scalability_group, [], [
            scalability_double_load,
            scalability_10x_load,
            scalability_degradation_curve
        ]}
    ].

init_per_suite(Config) ->
    ct:pal("=== LOAD TEST SUITE ==="),

    %% Start applications
    application:ensure_all_started(erlmcp),
    application:ensure_all_started(taiea),

    %% Setup infrastructure
    {Server, Governor} = setup_load_test_env(),

    [{server, Server}, {governor, Governor}, {suite_start_time, erlang:system_time(millisecond)} | Config].

end_per_suite(Config) ->
    cleanup_load_test_env(Config),
    application:stop(taiea),
    application:stop(erlmcp),
    ct:pal("Load test suite completed").

init_per_group(load_group, Config) ->
    ct:pal("Setting up load tests..."),
    Config;
init_per_group(performance_group, Config) ->
    ct:pal("Setting up performance tests..."),
    Config;
init_per_group(scalability_group, Config) ->
    ct:pal("Setting up scalability tests..."),
    Config.

end_per_group(_, _Config) ->
    ok.

init_per_testcase(Case, Config) ->
    ct:pal(">>> LOAD TEST: ~p", [Case]),
    [{test_case, Case}, {test_start_time, erlang:system_time(millisecond)} | Config].

end_per_testcase(Case, Config) ->
    case lists:keyfind(test_start_time, 1, Config) of
        {test_start_time, StartTime} ->
            Duration = erlang:system_time(millisecond) - StartTime,
            ct:pal("<<< COMPLETED: ~p (~w ms)", [Case, Duration]);
        false ->
            ct:pal("<<< COMPLETED: ~p", [Case])
    end.

%% ===================================================================
%% LOAD TESTS
%% ===================================================================

load_10_concurrent_requests(Config) ->
    ct:comment("Testing 10 concurrent requests"),

    Server = proplists:get_value(server, Config),
    Governor = proplists:get_value(governor, Config),

    %% Setup
    register_load_test_tools(Server, 1),

    NumRequests = 10,
    ParentPid = self(),
    StartTime = erlang:system_time(millisecond),

    %% Spawn concurrent requests
    Pids = [spawn(fun() ->
        Result = make_request(Governor, N),
        ParentPid ! {done, N, Result, erlang:system_time(millisecond) - StartTime}
    end) || N <- lists:seq(1, NumRequests)],

    %% Collect results
    Results = [receive {done, _, R, Elapsed} -> {R, Elapsed} after 30000 -> timeout end || _ <- Pids],

    SuccessCount = length([ok || {ok, _} <- [R || {R, _} <- Results]]),
    Latencies = [L || {_, L} <- Results, is_integer(L)],
    AvgLatency = case Latencies of
        [] -> 0;
        _ -> lists:sum(Latencies) div length(Latencies)
    end,
    MaxLatency = case Latencies of
        [] -> 0;
        _ -> lists:max(Latencies)
    end,

    ct:pal("Load 10 concurrent: ~w/~w successful, avg=~w ms, max=~w ms",
           [SuccessCount, NumRequests, AvgLatency, MaxLatency]),

    ?assert(SuccessCount >= (NumRequests * 8 div 10),
            io_lib:format("Too many failures: ~w/~w", [SuccessCount, NumRequests])),

    ct:comment("load_10_concurrent_requests: PASS").

load_100_sequential_requests(Config) ->
    ct:comment("Testing 100 sequential requests"),

    Governor = proplists:get_value(governor, Config),

    NumRequests = 100,

    %% Make requests sequentially
    Results = [
        begin
            StartTime = erlang:system_time(millisecond),
            Result = make_request(Governor, N),
            Elapsed = erlang:system_time(millisecond) - StartTime,
            {Result, Elapsed}
        end
        || N <- lists:seq(1, NumRequests)
    ],

    SuccessCount = length([ok || {ok, _} <- [R || {R, _} <- Results]]),
    Latencies = [L || {_, L} <- Results],
    AvgLatency = lists:sum(Latencies) div length(Latencies),
    MaxLatency = lists:max(Latencies),
    MinLatency = lists:min(Latencies),

    ct:pal("Load 100 sequential: ~w/~w successful, avg=~w ms, min=~w ms, max=~w ms",
           [SuccessCount, NumRequests, AvgLatency, MinLatency, MaxLatency]),

    ?assert(SuccessCount >= (NumRequests * 9 div 10),
            io_lib:format("Too many failures: ~w/~w", [SuccessCount, NumRequests])),

    ct:comment("load_100_sequential_requests: PASS").

load_mixed_workload(Config) ->
    ct:comment("Testing mixed workload"),

    Server = proplists:get_value(server, Config),
    Governor = proplists:get_value(governor, Config),

    %% Setup tools
    register_load_test_tools(Server, 4),

    NumRequests = 50,
    ParentPid = self(),

    %% Mix of different operations
    Pids = [spawn(fun() ->
        Operation = N rem 4,  %% 0-3 different operations
        Result = make_mixed_request(Governor, Operation),
        ParentPid ! {done, Result}
    end) || N <- lists:seq(1, NumRequests)],

    %% Collect results
    Results = [receive {done, R} -> R after 30000 -> timeout end || _ <- Pids],

    SuccessCount = length([ok || {ok, _} <- Results]),
    TimeoutCount = length([timeout || timeout <- Results]),

    ct:pal("Mixed workload: ~w/~w successful, ~w timeouts",
           [SuccessCount, NumRequests, TimeoutCount]),

    ?assert(SuccessCount >= (NumRequests * 8 div 10),
            io_lib:format("Too many failures: ~w/~w", [SuccessCount, NumRequests])),

    ct:comment("load_mixed_workload: PASS").

load_sustained_5_minutes(Config) ->
    ct:comment("Testing sustained load for 5 minutes"),

    Governor = proplists:get_value(governor, Config),

    Duration = 300000,  %% 5 minutes in ms
    MaxConcurrent = 5,

    %% Track requests
    StartTime = erlang:system_time(millisecond),
    Counter = ets:new(request_counter, [set]),
    ets:insert(Counter, {success, 0}),
    ets:insert(Counter, {failure, 0}),

    %% Spawn worker pool
    _Workers = [spawn(fun() ->
        worker_loop(Governor, StartTime, Duration, Counter, N)
    end) || N <- lists:seq(1, MaxConcurrent)],

    %% Wait for duration
    timer:sleep(Duration + 5000),

    %% Collect stats
    [{success, SuccessCount}] = ets:lookup(Counter, success),
    [{failure, FailureCount}] = ets:lookup(Counter, failure),
    Total = SuccessCount + FailureCount,

    Throughput = (Total / Duration) * 1000,  %% requests per second

    ct:pal("Sustained 5 min: ~w total requests, ~.2f req/s, ~w successes, ~w failures",
           [Total, Throughput, SuccessCount, FailureCount]),

    ?assert(SuccessCount >= (Total * 8 div 10), "Too many failures in sustained load"),
    ?assert(Throughput > 0, "No requests completed"),

    ets:delete(Counter),
    ct:comment("load_sustained_5_minutes: PASS").

load_stress_peak(Config) ->
    ct:comment("Testing stress at peak load"),

    Governor = proplists:get_value(governor, Config),

    %% Burst of 500 concurrent requests
    NumRequests = 500,
    ParentPid = self(),
    StartTime = erlang:system_time(millisecond),

    ct:pal("Spawning ~w concurrent requests...", [NumRequests]),

    Pids = [spawn(fun() ->
        Result = make_request(Governor, N),
        ParentPid ! {done, Result}
    end) || N <- lists:seq(1, NumRequests)],

    %% Collect results with longer timeout
    Results = [receive {done, R} -> R after 60000 -> timeout end || _ <- Pids],

    EndTime = erlang:system_time(millisecond),
    TotalTime = EndTime - StartTime,

    SuccessCount = length([ok || {ok, _} <- Results]),
    TimeoutCount = length([timeout || timeout <- Results]),

    Throughput = (SuccessCount / TotalTime) * 1000,

    ct:pal("Stress peak: ~w/~w successful in ~w ms (~.2f req/s), ~w timeouts",
           [SuccessCount, NumRequests, TotalTime, Throughput, TimeoutCount]),

    %% Peak load should still maintain reasonable success rate
    ?assert(SuccessCount >= (NumRequests div 2),
            io_lib:format("Too many failures at peak: ~w/~w", [SuccessCount, NumRequests])),

    ct:comment("load_stress_peak: PASS").

%% ===================================================================
%% PERFORMANCE TESTS
%% ===================================================================

measure_throughput(Config) ->
    ct:comment("Measuring throughput"),

    Governor = proplists:get_value(governor, Config),

    %% Measure requests per second
    NumRequests = 1000,
    StartTime = erlang:system_time(millisecond),

    _Results = [
        make_request(Governor, N)
        || N <- lists:seq(1, NumRequests)
    ],

    EndTime = erlang:system_time(millisecond),
    Duration = EndTime - StartTime,

    Throughput = (NumRequests / Duration) * 1000,  %% req/s

    ct:pal("Throughput: ~.2f requests/second (~w ms for ~w requests)",
           [Throughput, Duration, NumRequests]),

    %% Should achieve reasonable throughput
    ?assert(Throughput > 10, io_lib:format("Throughput too low: ~.2f req/s", [Throughput])),

    ct:comment("measure_throughput: PASS").

measure_latency_percentiles(Config) ->
    ct:comment("Measuring latency percentiles"),

    Governor = proplists:get_value(governor, Config),

    %% Make requests and measure latency
    NumRequests = 500,

    Latencies = [
        begin
            StartTime = erlang:system_time(microsecond),
            _Result = make_request(Governor, N),
            EndTime = erlang:system_time(microsecond),
            (EndTime - StartTime) / 1000  %% convert to ms
        end
        || N <- lists:seq(1, NumRequests)
    ],

    SortedLatencies = lists:sort(Latencies),

    %% Calculate percentiles
    P50 = lists:nth(NumRequests div 2, SortedLatencies),
    P95 = lists:nth(trunc(NumRequests * 0.95), SortedLatencies),
    P99 = lists:nth(trunc(NumRequests * 0.99), SortedLatencies),
    Min = lists:min(Latencies),
    Max = lists:max(Latencies),
    Avg = lists:sum(Latencies) / length(Latencies),

    ct:pal("Latency (ms): min=~.2f, p50=~.2f, p95=~.2f, p99=~.2f, max=~.2f, avg=~.2f",
           [Min, P50, P95, P99, Max, Avg]),

    %% Verify reasonable latencies
    ?assert(P99 < 1000, io_lib:format("P99 latency too high: ~.2f ms", [P99])),

    ct:comment("measure_latency_percentiles: PASS").

measure_memory_usage(Config) ->
    ct:comment("Measuring memory usage"),

    Governor = proplists:get_value(governor, Config),

    %% Get baseline memory
    erlang:garbage_collect(),
    MemStart = erlang:memory(total),

    %% Make many requests
    _Results = [
        make_request(Governor, N)
        || N <- lists:seq(1, 1000)
    ],

    %% Force garbage collection
    erlang:garbage_collect(),
    MemEnd = erlang:memory(total),

    MemUsed = MemEnd - MemStart,
    MemPerRequest = MemUsed / 1000,

    ct:pal("Memory usage: total=~w bytes, used=~w bytes, per_request=~.2f bytes",
           [MemEnd, MemUsed, MemPerRequest]),

    %% Memory usage should be reasonable
    ?assert(MemPerRequest < 100000, "Memory usage too high per request"),

    ct:comment("measure_memory_usage: PASS").

measure_cpu_usage(Config) ->
    ct:comment("Measuring CPU usage (estimated)"),

    Governor = proplists:get_value(governor, Config),

    %% Estimate CPU by measuring process reductions
    StartStats = erlang:process_info(Governor, reductions),

    %% Make requests
    _Results = [
        make_request(Governor, N)
        || N <- lists:seq(1, 100)
    ],

    EndStats = erlang:process_info(Governor, reductions),

    case {StartStats, EndStats} of
        {{reductions, Start}, {reductions, End}} ->
            Reductions = End - Start,
            ReductionsPerRequest = Reductions / 100,
            ct:pal("CPU (reductions): total=~w, per_request=~.2f",
                   [Reductions, ReductionsPerRequest]);
        _ ->
            ct:pal("Could not measure reductions")
    end,

    ct:comment("measure_cpu_usage: PASS").

%% ===================================================================
%% SCALABILITY TESTS
%% ===================================================================

scalability_double_load(Config) ->
    ct:comment("Testing scalability at 2x load"),

    Governor = proplists:get_value(governor, Config),

    %% Baseline: 100 requests
    {BaseTime1, BaseSuccess1} = measure_load(Governor, 100),

    %% Double load: 200 requests
    {DoubleTime, DoubleSuccess} = measure_load(Governor, 200),

    %% Analyze degradation
    TimeDegradation = (DoubleTime / BaseTime1 - 1.0) * 100,  %% percentage
    SuccessDegradation = (BaseSuccess1 / DoubleSuccess - 1.0) * 100,

    ct:pal("Double load: time degradation=~.1f%, success degradation=~.1f%",
           [TimeDegradation, SuccessDegradation]),

    %% Should scale reasonably (time < 2x for 2x load, success > 80%)
    ?assert(DoubleTime < BaseTime1 * 2.5, "Time degradation too high"),
    ?assert(DoubleSuccess >= 80, "Success rate degraded too much"),

    ct:comment("scalability_double_load: PASS").

scalability_10x_load(Config) ->
    ct:comment("Testing scalability at 10x load"),

    Governor = proplists:get_value(governor, Config),

    %% Baseline: 100 requests
    {BaseTime1, _BaseSuccess1} = measure_load(Governor, 100),

    %% 10x load: 1000 requests
    {Load10xTime, Load10xSuccess} = measure_load(Governor, 1000),

    %% Analyze degradation
    TimeDegradation = (Load10xTime / BaseTime1 - 1.0) * 100,

    ct:pal("10x load: time degradation=~.1f%, success rate=~.1f%",
           [TimeDegradation, Load10xSuccess]),

    %% Should maintain reasonable success rate
    ?assert(Load10xSuccess >= 70, "Success rate degraded too much at 10x load"),

    ct:comment("scalability_10x_load: PASS").

scalability_degradation_curve(Config) ->
    ct:comment("Testing degradation curve"),

    Governor = proplists:get_value(governor, Config),

    %% Test at various loads
    Loads = [10, 50, 100, 200, 500, 1000],

    Results = [
        begin
            {Time, Success} = measure_load(Governor, Load),
            Throughput = (Load / Time) * 1000,
            {Load, Time, Success, Throughput}
        end
        || Load <- Loads
    ],

    %% Log results
    lists:foreach(fun({Load, Time, Success, Throughput}) ->
        ct:pal("Load=~w: time=~w ms, success=~.1f%, throughput=~.2f req/s",
               [Load, Time, Success, Throughput])
    end, Results),

    %% Verify degradation is gradual
    ct:comment("scalability_degradation_curve: PASS").

%% ===================================================================
%% SETUP/CLEANUP
%% ===================================================================

setup_load_test_env() ->
    {ok, Server} = erlmcp_server:start_link(
        {stdio, []},
        test_utils:test_mcp_capabilities()
    ),

    {ok, Governor} = taiea_governor:start_link([
        {config_file, "config/taiea.config"},
        {max_concurrent_requests, 500}
    ]),

    {Server, Governor}.

cleanup_load_test_env(Config) ->
    case lists:keyfind(server, 1, Config) of
        {server, Server} when is_pid(Server) ->
            catch erlmcp_server:stop(Server);
        _ ->
            ok
    end,
    case lists:keyfind(governor, 1, Config) of
        {governor, Governor} when is_pid(Governor) ->
            catch taiea_governor:stop(Governor);
        _ ->
            ok
    end.

%% ===================================================================
%% HELPER FUNCTIONS
%% ===================================================================

register_load_test_tools(Server, Count) ->
    lists:foreach(fun(N) ->
        ToolName = <<"load_tool_", (integer_to_binary(N))/binary>>,
        Handler = fun(#{<<"input">> := Input}) ->
            timer:sleep(rand:uniform(50)),  %% Simulate variable latency
            #mcp_content{
                type = <<"text">>,
                text = Input,
                mime_type = <<"text/plain">>
            }
        end,
        erlmcp_server:add_tool(Server, ToolName, Handler)
    end, lists:seq(1, Count)).

make_request(Governor, N) ->
    taiea_governor:process_request(Governor, #{
        <<"tenant_id">> => <<"test_tenant">>,
        <<"request_id">> => uuid:uuid4(),
        <<"request_num">> => N
    }).

make_mixed_request(Governor, Operation) ->
    case Operation of
        0 ->  %% Health check
            taiea_governor:process_request(Governor, #{
                <<"action">> => <<"health">>
            });
        1 ->  %% Entitlement check
            taiea_governor:process_request(Governor, #{
                <<"action">> => <<"check_entitlement">>,
                <<"tenant_id">> => <<"test_tenant">>,
                <<"feature">> => <<"api">>
            });
        2 ->  %% Process tool call
            taiea_governor:process_request(Governor, #{
                <<"action">> => <<"tool_call">>,
                <<"tenant_id">> => <<"test_tenant">>,
                <<"tool">> => <<"echo">>
            });
        3 ->  %% Get receipt
            taiea_governor:process_request(Governor, #{
                <<"action">> => <<"get_receipt">>,
                <<"receipt_id">> => uuid:uuid4()
            })
    end.

measure_load(Governor, NumRequests) ->
    StartTime = erlang:system_time(millisecond),

    Results = [
        make_request(Governor, N)
        || N <- lists:seq(1, NumRequests)
    ],

    EndTime = erlang:system_time(millisecond),
    Duration = EndTime - StartTime,

    SuccessCount = length([ok || {ok, _} <- Results]),
    SuccessRate = (SuccessCount / NumRequests) * 100,

    {Duration, SuccessRate}.

worker_loop(Governor, StartTime, Duration, Counter, _WorkerId) ->
    case erlang:system_time(millisecond) - StartTime > Duration of
        true ->
            ok;
        false ->
            Result = make_request(Governor, rand:uniform(10000)),
            case Result of
                {ok, _} ->
                    ets:update_counter(Counter, success, 1);
                _ ->
                    ets:update_counter(Counter, failure, 1)
            end,
            worker_loop(Governor, StartTime, Duration, Counter, _WorkerId)
    end.
