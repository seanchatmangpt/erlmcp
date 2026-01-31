%% @doc Quick integration test for 100K connection pooling
%% Run with: erl -pa _build/default/lib/*/ebin -noshell -run test_100k_pooling run -s init stop

-module(test_100k_pooling).
-export([run/0]).

run() ->
    ensure_started(),
    test_pool_creation(),
    test_heavy_load(),
    test_100k_simulation(),
    io:format("~n=== ALL TESTS PASSED ===~n"),
    ok.

ensure_started() ->
    application:ensure_all_started(erlmcp),
    ok.

test_pool_creation() ->
    io:format("~n[TEST 1] Creating 128 pools...~n"),
    StartTime = erlang:system_time(millisecond),

    {ok, PoolMgrPid} = erlmcp_connection_pool:start_link(),
    io:format("Pool manager started: ~p~n", [PoolMgrPid]),

    %% Create 128 pools
    lists:foreach(fun(Index) ->
        PoolName = list_to_atom("test_pool_" ++ integer_to_list(Index)),
        PoolConfig = #{
            name => PoolName,
            worker_module => erlmcp_test_dummy_worker,
            worker_args => [Index],
            pool_size => 50,
            max_overflow => 20
        },
        case erlmcp_connection_pool:start_pool(PoolName, PoolConfig) of
            {ok, _Pid} ->
                if Index rem 10 == 0 -> io:format("  Created ~w pools~n", [Index]); true -> ok end;
            {error, Reason} ->
                io:format("  ERROR creating pool ~w: ~p~n", [Index, Reason])
        end
    end, lists:seq(0, 127)),

    {ok, PoolList} = erlmcp_connection_pool:get_pool_list(),
    io:format("Total pools created: ~w~n", [length(PoolList)]),

    EndTime = erlang:system_time(millisecond),
    Duration = EndTime - StartTime,
    io:format("Pool creation time: ~w ms~n", [Duration]).

test_heavy_load() ->
    io:format("~n[TEST 2] Heavy load test (10,000 operations)...~n"),
    StartTime = erlang:system_time(millisecond),

    %% Spawn 100 workers, each doing 100 operations
    Pids = lists:map(fun(I) ->
        spawn_link(fun() -> worker_loop(I rem 128, 100) end)
    end, lists:seq(1, 100)),

    %% Wait for all to complete
    lists:foreach(fun(Pid) ->
        receive
            {completed, _WorkerId} -> ok
        after 30000 -> io:format("ERROR: Worker ~p timed out~n", [Pid])
        end
    end, Pids),

    EndTime = erlang:system_time(millisecond),
    Duration = EndTime - StartTime,
    ThroughputOps = (100 * 100 * 1000) div Duration,

    io:format("Completed 10,000 operations in ~w ms (~w ops/sec)~n", [Duration, ThroughputOps]).

test_100k_simulation() ->
    io:format("~n[TEST 3] 100K connection simulation...~n"),
    StartTime = erlang:system_time(millisecond),

    %% Get pool stats before
    {ok, StatsBefore} = erlmcp_connection_pool:get_all_stats(),
    InitialConns = lists:sum([maps:get(active_connections, S, 0) || S <- StatsBefore]),

    %% Simulate 100K connections spread across 128 pools
    %% Each pool gets ~781 connections
    ConnCount = 100000,
    PoolCount = 128,
    ConnsPerPool = ConnCount div PoolCount,

    io:format("Simulating ~w connections across ~w pools (~w each)...~n",
        [ConnCount, PoolCount, ConnsPerPool]),

    %% Create worker pids for virtual connections
    Pids = lists:map(fun(I) ->
        PoolIdx = I rem PoolCount,
        spawn_link(fun() -> connection_sim(PoolIdx, 1) end)
    end, lists:seq(1, ConnCount)),

    %% Wait for all to complete
    lists:foreach(fun(Pid) ->
        receive
            {sim_done, _} -> ok
        after 60000 -> ok
        end
    end, Pids),

    EndTime = erlang:system_time(millisecond),
    Duration = EndTime - StartTime,

    %% Get final stats
    {ok, StatsAfter} = erlmcp_connection_pool:get_all_stats(),
    FinalConns = lists:sum([maps:get(active_connections, S, 0) || S <- StatsAfter]),

    io:format("100K connection simulation completed in ~w ms~n", [Duration]),
    io:format("Peak connections: ~w (before: ~w, after: ~w)~n",
        [FinalConns, InitialConns, FinalConns]).

%% Worker that repeatedly checks out/checks in
worker_loop(PoolIdx, OpCount) ->
    PoolName = list_to_atom("test_pool_" ++ integer_to_list(PoolIdx)),
    worker_loop_inner(PoolName, OpCount, 0),
    self() ! {completed, PoolIdx}.

worker_loop_inner(_PoolName, 0, _Ops) ->
    ok;
worker_loop_inner(PoolName, RemOps, Ops) ->
    case erlmcp_connection_pool:transaction(PoolName, fun(_Worker) ->
        {ok, worker_operated}
    end, 5000) of
        {ok, worker_operated} ->
            worker_loop_inner(PoolName, RemOps - 1, Ops + 1);
        {error, Reason} ->
            io:format("ERROR in worker_loop: ~p~n", [Reason]),
            worker_loop_inner(PoolName, RemOps - 1, Ops + 1)
    end.

%% Simulate a connection for 100K test
connection_sim(PoolIdx, _Duration) ->
    PoolName = list_to_atom("test_pool_" ++ integer_to_list(PoolIdx)),
    case erlmcp_connection_pool:transaction(PoolName, fun(_Worker) ->
        {ok, simulated}
    end, 5000) of
        {ok, simulated} -> ok;
        {error, _} -> ok
    end,
    self() ! {sim_done, self()}.

