%%%-------------------------------------------------------------------
%%% @doc Concurrent Connections Performance Benchmark
%%%
%%% Tests concurrent connections per node capability
%%% Target: 40,000-50,000 concurrent connections
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_bench_concurrent_connections).

-export([run/1, run_all/0]).

-define(TARGET_MIN, 40000).
-define(CONNECT_TIMEOUT, 15000).
-define(BENCHMARK_TIMEOUT, 120000).

run(all) ->
    run_all();

run(Target) when is_integer(Target) ->
    io:format("~n=== Concurrent Connections Benchmark ===~n"),
    io:format("Target: ~w connections~n", [Target]),
    Result = benchmark_concurrent_connections(Target),
    print_results(Result),
    Result.

run_all() ->
    io:format("~n=== Concurrent Connections Test Suite ===~n~n"),
    
    %% Run with incremental targets
    Targets = [1000, 5000, 10000, 20000, 30000, 40000, 50000],
    
    Results = lists:map(fun(Target) ->
        io:format("~nRunning test with ~w connections~n", [Target]),
        Start = erlang:monotonic_time(millisecond),
        
        Result = try
            benchmark_concurrent_connections(Target)
        catch
            Type:Error:Stack ->
                io:format("ERROR: ~p:~p~nStack: ~p~n", [Type, Error, Stack]),
                #{error => {Type, Error}, target => Target}
        end,
        
        End = erlang:monotonic_time(millisecond),
        Duration = End - Start,
        io:format("Completed in ~.2f seconds~n", [Duration / 1000]),
        {Target, Result, Duration}
    end, Targets),
    
    print_suite_summary(Results),
    Results.

%%====================================================================
%% Benchmark Implementation
%%====================================================================

benchmark_concurrent_connections(Target) ->
    %% Get system info
    ProcessLimit = erlang:system_info(process_limit),
    PortLimit = erlang:system_info(port_limit),
    Schedulers = erlang:system_info(schedulers),
    
    io:format("  System limits:~n", []),
    io:format("    Process limit: ~w~n", [ProcessLimit]),
    io:format("    Port limit: ~w~n", [PortLimit]),
    io:format("    Schedulers: ~w~n", [Schedulers]),
    
    %% Start echo server
    io:format("~n  Starting echo server...~n"),
    ServerPid = start_echo_server(),
    Port = get_server_port(),
    io:format("    Server listening on port ~w~n", [Port]),
    timer:sleep(500),
    
    %% Record initial metrics
    InitialProcesses = erlang:system_info(process_count),
    InitialMemory = erlang:memory(total),
    InitialPorts = erlang:system_info(port_count),
    
    io:format("~n  Initial state:~n", []),
    io:format("    Processes: ~w~n", [InitialProcesses]),
    io:format("    Memory: ~w MB~n", [InitialMemory div (1024 * 1024)]),
    io:format("    Ports: ~w~n", [InitialPorts]),
    
    %% Establish connections
    io:format("~n  Establishing ~w connections...~n", [Target]),
    StartTime = erlang:monotonic_time(millisecond),
    
    ConnectedCount = establish_connections(Target, Port),
    
    EndTime = erlang:monotonic_time(millisecond),
    ConnectDuration = EndTime - StartTime,
    
    io:format("  Connected: ~w/~w in ~.2f seconds~n", 
              [ConnectedCount, Target, ConnectDuration / 1000]),
    
    %% Measure stable state
    io:format("~n  Measuring stable state (3 seconds)...~n", []),
    timer:sleep(3000),
    
    StableProcesses = erlang:system_info(process_count),
    StableMemory = erlang:memory(total),
    StablePorts = erlang:system_info(port_count),
    
    %% Calculate metrics
    ProcessDelta = StableProcesses - InitialProcesses,
    MemoryDelta = StableMemory - InitialMemory,
    PortDelta = StablePorts - InitialPorts,
    
    MemoryPerConnection = case ConnectedCount of
        0 -> 0;
        _ -> MemoryDelta div ConnectedCount
    end,
    
    io:format("~n  Stable state:~n", []),
    io:format("    Processes: ~w (+~w)~n", [StableProcesses, ProcessDelta]),
    io:format("    Memory: ~w MB (+~w MB)~n", 
              [StableMemory div (1024 * 1024), MemoryDelta div (1024 * 1024)]),
    io:format("    Memory/conn: ~w bytes (~.2f MB)~n", 
              [MemoryPerConnection, MemoryPerConnection / (1024 * 1024)]),
    io:format("    Ports: ~w (+~w)~n", [StablePorts, PortDelta]),
    
    %% Cleanup
    io:format("~n  Cleaning up...~n", []),
    stop_echo_server(ServerPid),
    timer:sleep(500),
    
    %% Build result
    #{
        target => Target,
        connected => ConnectedCount,
        connection_time_sec => ConnectDuration / 1000,
        memory_per_connection_bytes => MemoryPerConnection,
        memory_per_connection_mb => MemoryPerConnection / (1024 * 1024),
        process_delta => ProcessDelta,
        port_delta => PortDelta,
        pass_target => ConnectedCount >= ?TARGET_MIN,
        verdict => verdict(ConnectedCount)
    }.

establish_connections(Target, Port) ->
    establish_connections(Target, Port, 0).

establish_connections(Target, Port, Connected) when Connected >= Target ->
    Connected;

establish_connections(Target, Port, Connected) ->
    %% Batch size
    BatchSize = min(1000, Target - Connected),
    
    %% Try to connect batch
    StartTime = erlang:monotonic_time(millisecond),
    BatchConnected = connect_batch(BatchSize, Port, ?CONNECT_TIMEOUT),
    EndTime = erlang:monotonic_time(millisecond),
    
    %% Report progress
    NewConnected = Connected + BatchConnected,
    case NewConnected rem 5000 of
        0 when NewConnected > 0 ->
            io:format("    Progress: ~w/~w connections (~.2f sec for batch)~n", 
                      [NewConnected, Target, (EndTime - StartTime) / 1000]);
        _ ->
            ok
    end,
    
    %% If batch failed completely, stop
    if
        BatchConnected =:= 0 andalso Connected > 0 ->
            io:format("    Connection failure detected at ~w connections~n", [Connected]),
            Connected;
        true ->
            establish_connections(Target, Port, NewConnected)
    end.

connect_batch(Count, Port, Timeout) ->
    Deadline = erlang:monotonic_time(millisecond) + Timeout,
    connect_batch(Count, Port, Deadline, 0).

connect_batch(0, _Port, _Deadline, Connected) ->
    Connected;

connect_batch(Count, Port, Deadline, Connected) ->
    Now = erlang:monotonic_time(millisecond),
    if
        Now > Deadline ->
            Connected;
        true ->
            %% Try to connect
            case gen_tcp:connect({127, 0, 0, 1}, Port,
                                [binary, {active, false}, {packet, 0}, {nodelay, true}],
                                5000) of
                {ok, Socket} ->
                    %% Spawn process to hold connection
                    spawn_link(fun() -> hold_connection(Socket) end),
                    connect_batch(Count - 1, Port, Deadline, Connected + 1);
                {error, _Reason} ->
                    %% Connection failed, continue
                    connect_batch(Count - 1, Port, Deadline, Connected)
            end
    end.

hold_connection(Socket) ->
    receive
        close -> gen_tcp:close(Socket), exit(normal)
    after infinity ->
        hold_connection(Socket)
    end.

%%====================================================================
%% Echo Server
%%====================================================================

start_echo_server() ->
    {ok, ListenSocket} = gen_tcp:listen(0, 
        [binary, {active, false}, {packet, 0}, {reuseaddr, true},
         {backlog, 8192}, {nodelay, true}]),
    
    {ok, Port} = inet:port(ListenSocket),
    
    %% Start acceptors
    lists:foreach(fun(_) ->
        spawn_link(fun() -> acceptor_loop(ListenSocket) end)
    end, lists:seq(1, 100)),
    
    %% Store port
    put(echo_server_port, Port),
    put(echo_server_socket, ListenSocket),
    
    spawn_link(fun() -> server_keeper(ListenSocket) end),
    
    timer:sleep(200),
    self().

get_server_port() ->
    get(echo_server_port).

server_keeper(ListenSocket) ->
    receive
        stop -> gen_tcp:close(ListenSocket), exit(normal)
    after infinity ->
        server_keeper(ListenSocket)
    end.

acceptor_loop(ListenSocket) ->
    case gen_tcp:accept(ListenSocket, 5000) of
        {ok, Socket} ->
            spawn_link(fun() -> echo_handler(Socket) end),
            acceptor_loop(ListenSocket);
        {error, timeout} ->
            acceptor_loop(ListenSocket);
        {error, closed} ->
            exit(normal)
    end.

echo_handler(Socket) ->
    case gen_tcp:recv(Socket, 0, 5000) of
        {ok, Data} ->
            gen_tcp:send(Socket, Data),
            echo_handler(Socket);
        {error, _} ->
            gen_tcp:close(Socket)
    end.

stop_echo_server(Pid) ->
    case is_process_alive(Pid) of
        true -> Pid ! stop, timer:sleep(100);
        false -> ok
    end,
    case get(echo_server_socket) of
        undefined -> ok;
        Socket -> gen_tcp:close(Socket)
    end.

%%====================================================================
%% Results
%%====================================================================

verdict(Connected) when Connected >= 50000 -> "PASS >= 50K";
verdict(Connected) when Connected >= 40000 -> "PASS >= 40K";
verdict(Connected) -> lists:flatten(io_lib:format("~w connections achieved", [Connected])).

print_results(#{verdict := Verdict, connected := Connected, 
                memory_per_connection_mb := MemPerConn, 
                connection_time_sec := ConnTime, pass_target := Pass}) ->
    io:format("~n=== Results ===~n"),
    io:format("Verdict: ~s~n", [Verdict]),
    io:format("Connected: ~w~n", [Connected]),
    io:format("Memory/connection: ~.2f MB~n", [MemPerConn]),
    io:format("Connection time: ~.2f seconds~n", [ConnTime]),
    io:format("Status: ~p~n", [Pass]),
    ok.

print_suite_summary(Results) ->
    io:format("~n~n=== Test Suite Summary ===~n"),
    io:format("~-12s ~-15s ~-15s ~-15s~n", ["Target", "Connected", "Memory/conn", "Status"]),
    io:format("~s~n", [lists:duplicate(60, $-)]),
    
    MaxConnected = lists:foldl(fun({Target, Result, _Duration}, Max) ->
        case Result of
            #{connected := Connected, memory_per_connection_mb := MemPerConn, pass_target := Pass} ->
                Status = case Pass of true -> "PASS"; false -> "FAIL" end,
                io:format("~-12w ~-15w ~-15.2f MB ~-15s~n", [Target, Connected, MemPerConn, Status]),
                max(Connected, Max);
            #{error := _} ->
                io:format("~-12w ~-15s ~-15s ~-15s~n", [Target, "ERROR", "N/A", "FAIL"]),
                Max
        end
    end, 0, Results),
    
    io:format("~n~n=== FINAL VERDICT: ~s ===~n", [verdict(MaxConnected)]),
    ok.
