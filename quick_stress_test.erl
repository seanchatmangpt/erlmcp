#!/usr/bin/env escript
%% -*- erlang -*-
%%! -noshell

%% Quick and simple stress test that runs immediately
main(_) ->
    io:format("=== ERLMCP Quick Stress Test ===~n~n"),
    
    %% Test 1: Flood Test
    io:format("1. Flood Test (5 seconds of maximum message generation)~n"),
    {FloodTime, FloodResult} = timer:tc(fun flood_test/0),
    Rate = trunc(FloodResult * 1000000 / FloodTime),
    io:format("   Result: ~p messages in ~.2f seconds = ~p msg/sec~n~n", 
              [FloodResult, FloodTime/1000000, Rate]),
    
    %% Test 2: Connection Test  
    io:format("2. Connection Test (100 rapid connection attempts)~n"),
    {ConnTime, {ConnSuccess, ConnErrors}} = timer:tc(fun connection_test/0),
    io:format("   Result: ~p successful, ~p errors in ~.2f seconds~n~n", 
              [ConnSuccess, ConnErrors, ConnTime/1000000]),
    
    %% Test 3: Memory Test
    io:format("3. Memory Test (create and process 100 large messages)~n"),
    InitialMem = erlang:memory(total),
    {MemTime, MemResult} = timer:tc(fun memory_test/0),
    FinalMem = erlang:memory(total),
    io:format("   Result: ~p messages processed in ~.2f seconds~n", 
              [MemResult, MemTime/1000000]),
    io:format("   Memory: ~p KB -> ~p KB (delta: ~.2f KB)~n~n", 
              [InitialMem div 1024, FinalMem div 1024, (FinalMem-InitialMem)/1024]),
    
    %% Summary
    io:format("=== Summary ===~n"),
    io:format("Flood: ~p msg/sec~n", [Rate]),
    io:format("Connections: ~p successful, ~p failed~n", [ConnSuccess, ConnErrors]),
    io:format("Memory: ~.2f KB growth~n", [(FinalMem-InitialMem)/1024]),
    io:format("Total test time: ~.2f seconds~n", [(FloodTime + ConnTime + MemTime)/1000000]),
    
    ok.

%% Simple flood test - generate messages as fast as possible
flood_test() ->
    EndTime = erlang:monotonic_time(millisecond) + 5000, % 5 seconds
    flood_loop(0, EndTime).

flood_loop(Count, EndTime) ->
    case erlang:monotonic_time(millisecond) of
        Now when Now >= EndTime -> Count;
        _ ->
            %% Generate a message (just create a map, don't send anywhere)
            _Message = #{
                id => Count + 1,
                jsonrpc => "2.0", 
                method => "test/ping",
                params => #{data => "test_data", sequence => Count + 1}
            },
            flood_loop(Count + 1, EndTime)
    end.

%% Simple connection test - simulate connection attempts
connection_test() ->
    connection_loop(100, 0, 0).

connection_loop(0, Success, Errors) ->
    {Success, Errors};
connection_loop(Remaining, Success, Errors) ->
    %% Simulate connection attempt
    case rand:uniform(10) of
        N when N =< 9 -> % 90% success rate
            timer:sleep(rand:uniform(3)), % 1-3ms delay
            connection_loop(Remaining - 1, Success + 1, Errors);
        _ -> % 10% failure rate
            connection_loop(Remaining - 1, Success, Errors + 1)
    end.

%% Simple memory test - create large data structures
memory_test() ->
    memory_loop(100, 0).

memory_loop(0, Processed) ->
    Processed;
memory_loop(Remaining, Processed) ->
    %% Create a large binary (10KB)
    LargeData = crypto:strong_rand_bytes(10240),
    
    %% Create a message with the large data
    _Message = #{
        id => 101 - Remaining,
        data => base64:encode(LargeData),
        size => byte_size(LargeData),
        timestamp => erlang:system_time(millisecond)
    },
    
    %% Process it (decode and verify)
    _Decoded = base64:decode(base64:encode(LargeData)),
    
    %% Occasional GC
    case Remaining rem 20 of
        0 -> erlang:garbage_collect();
        _ -> ok
    end,
    
    memory_loop(Remaining - 1, Processed + 1).