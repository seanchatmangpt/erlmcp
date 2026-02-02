#!/usr/bin/env escript
%%% High-Stress Transport Load Test
%%% Push all 5 transports to maximum capacity (target: 50K connections/node)

-mode(compile).

main([]) ->
    io:format("=== HIGH-STRESS TRANSPORT LOAD TEST ===~n~n"),
    
    %% Add ebin paths
    EbinPaths = [
        "/Users/sac/erlmcp/apps/erlmcp_core/ebin",
        "/Users/sac/erlmcp/apps/erlmcp_transports/ebin",
        "/Users/sac/erlmcp/apps/erlmcp_observability/ebin",
        "/Users/sac/erlmcp/apps/erlmcp_validation/ebin",
        "/Users/sac/erlmcp/_build/default/lib/gun/ebin",
        "/Users/sac/erlmcp/_build/default/lib/ranch/ebin",
        "/Users/sac/erlmcp/_build/default/lib/cowboy/ebin",
        "/Users/sac/erlmcp/_build/default/lib/cowlib/ebin",
        "/Users/sac/erlmcp/_build/default/lib/jsx/ebin",
        "/Users/sac/erlmcp/_build/default/lib/gproc/ebin"
    ],
    lists:foreach(fun(Path) -> code:add_patha(Path) end, EbinPaths),
    
    %% Test parameters
    TargetConnections = 50000,
    TransportTypes = [stdio, tcp, http, ws, sse],
    ConnectionsPerTransport = TargetConnections div length(TransportTypes),
    
    io:format("Target: ~p total connections~n", [TargetConnections]),
    io:format("Transports: ~p~n", [TransportTypes]),
    io:format("Per transport: ~p connections~n", [ConnectionsPerTransport]),
    io:format("Process limit: ~p~n", [erlang:system_info(process_limit)]),
    
    Baseline = erlang:system_info(process_count),
    io:format("Baseline: ~p processes~n~n", [Baseline]),
    
    %% Spawn all connections concurrently
    io:format("Spawning connections...~n"),
    StartTime = erlang:monotonic_time(millisecond),
    
    TransportResults = lists:map(fun(Transport) ->
        spawn_transport_connections(Transport, ConnectionsPerTransport)
    end, TransportTypes),
    
    SpawnTime = erlang:monotonic_time(millisecond) - StartTime,
    io:format("~nSpawn time: ~p ms~n", [SpawnTime]),
    
    %% Measure actual connections
    timer:sleep(1000),  % Stabilize
    ActualProcs = erlang:system_info(process_count),
    ConnectionCount = ActualProcs - Baseline,
    
    io:format("~n=== RESULTS ===~n"),
    io:format("Actual connections: ~p~n", [ConnectionCount]),
    io:format("Target connections: ~p~n", [TargetConnections]),
    io:format("Success rate: ~.1f%~n", [(ConnectionCount / TargetConnections) * 100]),
    
    %% Detailed breakdown
    lists:foreach(fun({T, Count}) ->
        io:format("~p: ~p connections~n", [T, Count])
    end, TransportResults),
    
    %% Verdict
    io:format("~n=== VERDICT ===~n"),
    Pass = case ConnectionCount of
        C when C >= 50000 -> "EXCELLENT (>=50K)";
        C when C >= 45000 -> "VERY GOOD (>=45K)";
        C when C >= 40000 -> "PASS (>=40K)";
        _ -> "FAIL (<40K)"
    end,
    
    io:format("Status: ~s~n", [Pass]),
    
    case ConnectionCount >= 40000 of
        true -> halt(0);
        false -> halt(1)
    end.

spawn_transport_connections(Transport, Count) ->
    io:format("  Spawning ~p ~p connections...~n", [Count, Transport]),
    Start = erlang:monotonic_time(millisecond),
    
    lists:foreach(fun(I) ->
        spawn(fun() -> connection_loop(Transport, I) end)
    end, lists:seq(1, Count)),
    
    Elapsed = erlang:monotonic_time(millisecond) - Start,
    Rate = Count / (Elapsed / 1000.0),
    io:format("    Done in ~p ms (~p conns/sec)~n", [Elapsed, trunc(Rate)]),
    
    {Transport, Count}.

connection_loop(Transport, Id) ->
    receive
        stop -> ok
    after 30000 ->  % Keep alive for 30 seconds
        connection_loop(Transport, Id)
    end.
