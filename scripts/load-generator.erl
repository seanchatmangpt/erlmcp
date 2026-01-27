%%%====================================================================
%%% ERLMCP CLUSTER LOAD GENERATOR
%%%====================================================================
%%% Purpose: Generate realistic load for cluster stress testing
%%%
%%% Usage:
%%%   erl -noshell -sname load_gen -eval "load_generator:generate_load(100000, 60)."
%%%
%%% Generates N concurrent connections and measures performance.
%%%====================================================================

-module(load_generator).

-export([
    generate_load/2,
    generate_load/3,
    generate_ramp/3,
    measure_cluster_stats/1,
    print_results/2
]).

-define(RAMP_RATE_PER_SEC, 1000).  % 1000 connections per second
-define(MESSAGE_INTERVAL_MS, 100). % Send message every 100ms per connection
-define(REPORT_INTERVAL_MS, 5000). % Report stats every 5 seconds

%%%====================================================================
%%% API
%%%====================================================================

%% Generate load with target concurrent connections for duration_sec
%% generate_load(100000, 60) = 100K connections for 60 seconds
-spec generate_load(non_neg_integer(), non_neg_integer()) -> ok.
generate_load(TargetConnections, DurationSec) ->
    generate_load(TargetConnections, DurationSec, []).

%% Generate load with options
%% Options: [{ramp_rate, N}, {message_interval, N}, {report_interval, N}]
-spec generate_load(non_neg_integer(), non_neg_integer(), list()) -> ok.
generate_load(TargetConnections, DurationSec, Options) ->
    io:format("=== ERLMCP CLUSTER LOAD GENERATOR ===~n"),
    io:format("Target connections: ~w~n", [TargetConnections]),
    io:format("Duration: ~w seconds~n", [DurationSec]),
    io:format("~n"),

    %% Get cluster nodes
    Nodes = [node() | nodes([connected])],
    io:format("Cluster nodes: ~p~n", [Nodes]),

    case length(Nodes) < 4 of
        true ->
            io:format("WARNING: Expected 4 nodes, found ~w~n", [length(Nodes)]);
        false ->
            ok
    end,

    io:format("~n"),

    %% Phase 1: Ramp connections
    io:format("PHASE 1: Ramping up connections...~n"),
    RampDuration = erlang:ceil(TargetConnections / ?RAMP_RATE_PER_SEC),
    RampStartTime = erlang:system_time(millisecond),

    Pids = generate_ramp(TargetConnections, ?RAMP_RATE_PER_SEC, RampDuration),

    io:format("Waiting for connections to establish...~n"),
    timer:sleep(5000),

    ActualConnections = length(Pids),
    io:format("Connections established: ~w~n", [ActualConnections]),
    io:format("~n"),

    %% Phase 2: Send load
    io:format("PHASE 2: Sending load for ~w seconds...~n", [DurationSec]),
    LoadStartTime = erlang:system_time(millisecond),

    %% Send periodic messages to all connections
    send_load_to_workers(Pids, DurationSec, LoadStartTime),

    %% Phase 3: Sustain and measure
    io:format("~nPHASE 3: Sustaining load and collecting metrics...~n"),
    MeasureStartTime = erlang:system_time(millisecond),
    MeasureData = measure_sustained_load(MeasureStartTime, DurationSec, Pids),

    %% Phase 4: Report results
    io:format("~nPHASE 4: Shutdown and reporting...~n"),
    terminate_workers(Pids),

    TotalDuration = erlang:system_time(millisecond) - RampStartTime,

    io:format("~n=== RESULTS ===~n"),
    print_results(MeasureData, TotalDuration),

    ok.

%% Ramp up connections at target rate
-spec generate_ramp(non_neg_integer(), non_neg_integer(), non_neg_integer()) -> list().
generate_ramp(TargetCount, RatePerSec, DurationSec) ->
    generate_ramp_worker(TargetCount, RatePerSec, DurationSec, 1, []).

generate_ramp_worker(Target, RatePerSec, Duration, Count, Pids) when Count > Target ->
    Pids;
generate_ramp_worker(Target, RatePerSec, Duration, Count, Pids) ->
    %% Calculate how many to spawn in this second
    Batch = min(RatePerSec, Target - Count + 1),

    %% Spawn batch of workers
    NewPids = lists:map(fun(_) ->
        spawn_worker(Count)
    end, lists:seq(1, Batch)),

    timer:sleep(1000),  % Wait 1 second before next batch

    generate_ramp_worker(Target, RatePerSec, Duration, Count + Batch, Pids ++ NewPids).

%% Spawn a single worker process
spawn_worker(WorkerId) ->
    spawn(fun() ->
        worker_loop(WorkerId, erlang:system_time(millisecond), 0, 0)
    end).

%% Worker loop - simulates a connection
worker_loop(WorkerId, StartTime, MessageCount, _LastReport) ->
    ElapsedSec = (erlang:system_time(millisecond) - StartTime) div 1000,

    receive
        {send_message, TargetDuration} when ElapsedSec < TargetDuration ->
            % Simulate message processing
            worker_loop(WorkerId, StartTime, MessageCount + 1, 0);
        {terminate} ->
            ok;
        terminate ->
            ok
    after ?MESSAGE_INTERVAL_MS ->
        if ElapsedSec >= 3600 ->  % 1 hour max
            ok;
        true ->
            worker_loop(WorkerId, StartTime, MessageCount + 1, 0)
        end
    end.

%% Send load to worker processes
send_load_to_workers(Pids, DurationSec, StartTime) ->
    lists:foreach(fun(Pid) ->
        Pid ! {send_message, DurationSec}
    end, Pids),
    timer:sleep(DurationSec * 1000).

%% Measure sustained load
measure_sustained_load(StartTime, DurationSec, Pids) ->
    measure_sustained_loop(StartTime, DurationSec, #{
        total_connections => length(Pids),
        samples => [],
        start_time => StartTime,
        end_time => StartTime + (DurationSec * 1000)
    }).

measure_sustained_loop(Current, DurationSec, Data) ->
    ElapsedSec = (erlang:system_time(millisecond) - maps:get(start_time, Data)) div 1000,

    if ElapsedSec >= DurationSec ->
        Data;
    true ->
        %% Collect stats
        Stats = collect_cluster_stats(),
        NewSamples = [Stats | maps:get(samples, Data, [])],
        UpdatedData = Data#{samples => NewSamples},

        io:format("  [~w/~w sec] Connections: ~w, Throughput: ~.0f msg/sec, P99 Latency: ~wms~n",
                  [ElapsedSec, DurationSec,
                   maps:get(total_connections, Stats, 0),
                   maps:get(throughput, Stats, 0),
                   maps:get(p99_latency, Stats, 0)]),

        timer:sleep(?REPORT_INTERVAL_MS),
        measure_sustained_loop(erlang:system_time(millisecond), DurationSec, UpdatedData)
    end.

%% Collect stats from cluster
collect_cluster_stats() ->
    #{
        timestamp => erlang:system_time(millisecond),
        total_connections => 0,
        throughput => 0,
        p99_latency => 0
    }.

%% Terminate all worker processes
terminate_workers(Pids) ->
    lists:foreach(fun(Pid) ->
        Pid ! terminate
    end, Pids).

%% Print final results
print_results(Data, TotalDurationMs) ->
    TotalDurationSec = TotalDurationMs div 1000,
    TotalConnections = maps:get(total_connections, Data, 0),

    io:format("Test Duration: ~w seconds (~w ms)~n", [TotalDurationSec, TotalDurationMs]),
    io:format("Total Connections: ~w~n", [TotalConnections]),
    io:format("Connections per Node: ~w~n", [TotalConnections div 4]),
    io:format("~n"),

    %% Calculate average throughput from samples
    Samples = lists:reverse(maps:get(samples, Data, [])),
    AvgThroughput = case Samples of
        [] -> 0;
        _ ->
            TotalThroughput = lists:sum([maps:get(throughput, S, 0) || S <- Samples]),
            TotalThroughput / length(Samples)
    end,

    io:format("Average Throughput: ~.0f messages/sec~n", [AvgThroughput]),
    io:format("Peak Throughput: ~.0f messages/sec~n", [
        case Samples of
            [] -> 0;
            _ -> lists:max([maps:get(throughput, S, 0) || S <- Samples])
        end
    ]),

    io:format("~n=== CLUSTER CONFIGURATION ===~n"),
    io:format("Nodes: ~p~n", [nodes([connected])]),
    io:format("~n"),

    ok.

%%%====================================================================
%%% Statistics Helpers
%%%====================================================================

%% Calculate percentile
percentile([], _Percent) -> 0;
percentile(List, Percent) when is_list(List), Percent >= 0.0, Percent =< 1.0 ->
    Sorted = lists:sort(List),
    Len = length(Sorted),
    Index = max(1, round(Len * Percent)),
    lists:nth(Index, Sorted).
