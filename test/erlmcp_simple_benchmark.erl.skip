%% Simple benchmarking module for ErlMCP
%% Usage: erl -pa ebin -s erlmcp_simple_benchmark run
-module(erlmcp_simple_benchmark).
-export([run/0, throughput_test/0, latency_test/0, concurrent_test/0]).

%% Entry point for command line execution
run() ->
    io:format("=== ErlMCP Simple Benchmark Suite ===~n"),
    io:format("Starting benchmark tests...~n~n"),
    
    % Run all tests
    throughput_test(),
    io:format("~n"),
    latency_test(),
    io:format("~n"),
    concurrent_test(),
    
    io:format("~n=== Benchmark Complete ===~n"),
    init:stop().

%% Throughput test - Send 1000 messages and measure time
throughput_test() ->
    io:format("--- Throughput Test ---~n"),
    MessageCount = 1000,
    
    % Start a simple echo server
    ServerPid = spawn(fun() -> echo_server() end),
    
    % Measure time to send messages
    {Time, _} = timer:tc(fun() -> 
        send_messages(ServerPid, MessageCount)
    end),
    
    % Stop the server
    ServerPid ! stop,
    
    % Calculate and display results
    TimeSeconds = Time / 1000000,
    Throughput = MessageCount / TimeSeconds,
    
    io:format("Messages sent: ~p~n", [MessageCount]),
    io:format("Time taken: ~.3f seconds~n", [TimeSeconds]),
    io:format("Throughput: ~.2f messages/second~n", [Throughput]).

%% Latency test - Measure round-trip time for 100 messages
latency_test() ->
    io:format("--- Latency Test ---~n"),
    MessageCount = 100,
    
    % Start echo server
    ServerPid = spawn(fun() -> echo_server() end),
    
    % Measure individual round-trip times
    Latencies = lists:map(fun(_) ->
        {Time, _} = timer:tc(fun() ->
            ServerPid ! {self(), ping},
            receive
                pong -> ok
            after 5000 ->
                timeout
            end
        end),
        Time / 1000 % Convert to milliseconds
    end, lists:seq(1, MessageCount)),
    
    % Stop server
    ServerPid ! stop,
    
    % Calculate statistics
    ValidLatencies = [L || L <- Latencies, L < 5000], % Filter out timeouts
    case ValidLatencies of
        [] ->
            io:format("All messages timed out!~n");
        _ ->
            AvgLatency = lists:sum(ValidLatencies) / length(ValidLatencies),
            MinLatency = lists:min(ValidLatencies),
            MaxLatency = lists:max(ValidLatencies),
            
            io:format("Messages tested: ~p~n", [MessageCount]),
            io:format("Successful: ~p~n", [length(ValidLatencies)]),
            io:format("Average latency: ~.3f ms~n", [AvgLatency]),
            io:format("Min latency: ~.3f ms~n", [MinLatency]),
            io:format("Max latency: ~.3f ms~n", [MaxLatency])
    end.

%% Concurrent test - Spawn 10 processes sending messages
concurrent_test() ->
    io:format("--- Concurrent Test ---~n"),
    ProcessCount = 10,
    MessagesPerProcess = 100,
    
    % Start echo server
    ServerPid = spawn(fun() -> echo_server() end),
    
    % Spawn concurrent senders and measure time
    {Time, Results} = timer:tc(fun() ->
        Parent = self(),
        
        % Spawn sender processes
        Pids = [spawn(fun() -> 
            concurrent_sender(ServerPid, MessagesPerProcess, Parent, N)
        end) || N <- lists:seq(1, ProcessCount)],
        
        % Wait for all processes to complete
        [receive
            {done, ProcessId, ProcessTime} -> 
                {ProcessId, ProcessTime}
        end || _ <- Pids]
    end),
    
    % Stop server
    ServerPid ! stop,
    
    % Calculate results
    TimeSeconds = Time / 1000000,
    TotalMessages = ProcessCount * MessagesPerProcess,
    OverallThroughput = TotalMessages / TimeSeconds,
    
    % Process individual results
    {_ProcessIds, ProcessTimes} = lists:unzip(Results),
    AvgProcessTime = lists:sum(ProcessTimes) / length(ProcessTimes) / 1000000,
    MinProcessTime = lists:min(ProcessTimes) / 1000000,
    MaxProcessTime = lists:max(ProcessTimes) / 1000000,
    
    io:format("Concurrent processes: ~p~n", [ProcessCount]),
    io:format("Messages per process: ~p~n", [MessagesPerProcess]),
    io:format("Total messages: ~p~n", [TotalMessages]),
    io:format("Total time: ~.3f seconds~n", [TimeSeconds]),
    io:format("Overall throughput: ~.2f messages/second~n", [OverallThroughput]),
    io:format("Average process time: ~.3f seconds~n", [AvgProcessTime]),
    io:format("Min process time: ~.3f seconds~n", [MinProcessTime]),
    io:format("Max process time: ~.3f seconds~n", [MaxProcessTime]).

%% Helper functions

%% Simple echo server for testing
echo_server() ->
    receive
        {From, ping} ->
            From ! pong,
            echo_server();
        {From, Msg} ->
            From ! {echo, Msg},
            echo_server();
        stop ->
            ok;
        _ ->
            echo_server()
    end.

%% Send multiple messages to server
send_messages(_ServerPid, 0) ->
    ok;
send_messages(ServerPid, Count) ->
    ServerPid ! {self(), {message, Count}},
    receive
        {echo, _} -> ok
    after 1000 ->
        timeout
    end,
    send_messages(ServerPid, Count - 1).

%% Concurrent sender process
concurrent_sender(ServerPid, MessageCount, Parent, ProcessId) ->
    StartTime = erlang:monotonic_time(microsecond),
    
    % Send messages
    send_messages(ServerPid, MessageCount),
    
    EndTime = erlang:monotonic_time(microsecond),
    ProcessTime = EndTime - StartTime,
    
    Parent ! {done, ProcessId, ProcessTime}.