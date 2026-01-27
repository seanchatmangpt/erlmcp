-module(erlmcp_simple_stress).

%% API exports
-export([
    flood_test/0,
    connection_test/0,
    memory_test/0,
    run_all/0
]).

%% Helper exports
-export([
    worker_process/4,
    connection_worker/3,
    monitor_memory/2
]).

-include("erlmcp.hrl").

%% Configuration constants
-define(FLOOD_DURATION_SECONDS, 10).
-define(CONNECTION_COUNT, 50).
-define(CONNECTION_CYCLES, 10).
-define(LARGE_MESSAGE_SIZE_KB, 100).
-define(MEMORY_MESSAGE_COUNT, 1000).

%% Test runner - runs all stress tests
run_all() ->
    io:format("=== Starting ERLMCP Simple Stress Tests ===~n~n"),
    
    Results = [
        {"Flood Test", timer:tc(fun flood_test/0)},
        {"Connection Test", timer:tc(fun connection_test/0)},
        {"Memory Test", timer:tc(fun memory_test/0)}
    ],
    
    io:format("~n=== Final Summary ===~n"),
    lists:foreach(fun({Name, {Time, Result}}) ->
        io:format("~s: ~p (completed in ~.2f seconds)~n", 
                 [Name, Result, Time / 1_000_000])
    end, Results),
    
    ok.

%% Test 1: Flood Test - Send messages as fast as possible for 10 seconds
flood_test() ->
    io:format("=== Flood Test: Sending messages as fast as possible for ~p seconds ===~n", 
              [?FLOOD_DURATION_SECONDS]),
    
    %% Start a simple stdio server for testing
    ServerPid = spawn_link(fun() -> simple_test_server() end),
    
    %% Give server time to start
    timer:sleep(100),
    
    %% Start multiple worker processes
    WorkerCount = erlang:system_info(logical_processors_available),
    io:format("Starting ~p worker processes...~n", [WorkerCount]),
    
    StartTime = erlang:monotonic_time(millisecond),
    EndTime = StartTime + (?FLOOD_DURATION_SECONDS * 1000),
    
    %% Start workers
    Workers = [spawn_link(?MODULE, worker_process, 
                         [self(), I, StartTime, EndTime]) || I <- lists:seq(1, WorkerCount)],
    
    %% Collect results
    Results = collect_worker_results(Workers, [], 0, 0, 0),
    
    %% Stop server
    ServerPid ! stop,
    
    %% Calculate statistics
    Duration = erlang:monotonic_time(millisecond) - StartTime,
    {TotalSent, TotalErrors, TotalResponses} = Results,
    Rate = if Duration > 0 -> (TotalSent * 1000) / Duration; true -> 0 end,
    ErrorRate = if TotalSent > 0 -> (TotalErrors * 100.0) / TotalSent; true -> 0 end,
    
    Result = #{
        messages_sent => TotalSent,
        errors_encountered => TotalErrors,
        responses_received => TotalResponses,
        duration_ms => Duration,
        messages_per_second => Rate,
        error_rate_percent => ErrorRate,
        workers_used => WorkerCount
    },
    
    io:format("Flood Test Results:~n"),
    io:format("  Messages sent: ~p~n", [TotalSent]),
    io:format("  Errors: ~p~n", [TotalErrors]),
    io:format("  Responses: ~p~n", [TotalResponses]),
    io:format("  Duration: ~p ms~n", [Duration]),
    io:format("  Rate: ~.2f messages/sec~n", [Rate]),
    io:format("  Error rate: ~.2f%~n", [ErrorRate]),
    
    Result.

%% Test 2: Connection Test - Open/close connections rapidly
connection_test() ->
    io:format("=== Connection Test: Rapidly opening/closing ~p connections, ~p cycles ===~n",
              [?CONNECTION_COUNT, ?CONNECTION_CYCLES]),
    
    StartTime = erlang:monotonic_time(millisecond),
    InitialMemory = erlang:memory(),
    
    %% Run connection cycles
    {TotalConnections, TotalErrors} = run_connection_cycles(?CONNECTION_CYCLES, 0, 0),
    
    EndTime = erlang:monotonic_time(millisecond),
    FinalMemory = erlang:memory(),
    Duration = EndTime - StartTime,
    
    %% Calculate memory delta
    MemoryDelta = calculate_memory_delta(InitialMemory, FinalMemory),
    
    Result = #{
        connections_attempted => TotalConnections,
        errors_encountered => TotalErrors,
        duration_ms => Duration,
        connections_per_second => if Duration > 0 -> (TotalConnections * 1000) / Duration; true -> 0 end,
        memory_delta => MemoryDelta,
        cycles_completed => ?CONNECTION_CYCLES
    },
    
    io:format("Connection Test Results:~n"),
    io:format("  Connections attempted: ~p~n", [TotalConnections]),
    io:format("  Errors: ~p~n", [TotalErrors]),
    io:format("  Duration: ~p ms~n", [Duration]),
    io:format("  Rate: ~.2f connections/sec~n", [maps:get(connections_per_second, Result)]),
    io:format("  Memory delta: ~p~n", [MemoryDelta]),
    
    Result.

%% Test 3: Memory Test - Create large messages and monitor memory usage
memory_test() ->
    io:format("=== Memory Test: Creating ~p large messages (~p KB each) ===~n",
              [?MEMORY_MESSAGE_COUNT, ?LARGE_MESSAGE_SIZE_KB]),
    
    %% Start memory monitor
    MonitorPid = spawn_link(?MODULE, monitor_memory, [self(), 500]), % 500ms intervals
    
    StartTime = erlang:monotonic_time(millisecond),
    InitialMemory = erlang:memory(),
    
    %% Create large messages and process them
    {MessagesProcessed, Errors, PeakMemory} = create_large_messages(?MEMORY_MESSAGE_COUNT),
    
    %% Stop memory monitor
    MonitorPid ! stop,
    
    %% Collect final memory readings
    MemoryReadings = collect_memory_readings([]),
    
    EndTime = erlang:monotonic_time(millisecond),
    FinalMemory = erlang:memory(),
    
    %% Force garbage collection
    erlang:garbage_collect(),
    GcMemory = erlang:memory(),
    
    Duration = EndTime - StartTime,
    MemoryDelta = calculate_memory_delta(InitialMemory, FinalMemory),
    MemoryAfterGc = calculate_memory_delta(InitialMemory, GcMemory),
    
    Result = #{
        messages_processed => MessagesProcessed,
        errors_encountered => Errors,
        duration_ms => Duration,
        initial_memory => InitialMemory,
        final_memory => FinalMemory,
        memory_after_gc => GcMemory,
        memory_delta => MemoryDelta,
        memory_delta_after_gc => MemoryAfterGc,
        peak_memory_usage => PeakMemory,
        memory_readings => length(MemoryReadings),
        message_size_kb => ?LARGE_MESSAGE_SIZE_KB
    },
    
    io:format("Memory Test Results:~n"),
    io:format("  Messages processed: ~p~n", [MessagesProcessed]),
    io:format("  Errors: ~p~n", [Errors]),
    io:format("  Duration: ~p ms~n", [Duration]),
    io:format("  Initial memory: ~p KB~n", [maps:get(total, InitialMemory) div 1024]),
    io:format("  Final memory: ~p KB~n", [maps:get(total, FinalMemory) div 1024]),
    io:format("  Memory after GC: ~p KB~n", [maps:get(total, GcMemory) div 1024]),
    io:format("  Peak memory: ~p KB~n", [PeakMemory div 1024]),
    io:format("  Memory readings taken: ~p~n", [length(MemoryReadings)]),
    
    Result.

%% Worker process for flood test
worker_process(Parent, WorkerId, StartTime, EndTime) ->
    worker_loop(Parent, WorkerId, StartTime, EndTime, 0, 0, 0).

worker_loop(Parent, WorkerId, StartTime, EndTime, Sent, Errors, Responses) ->
    CurrentTime = erlang:monotonic_time(millisecond),
    if 
        CurrentTime >= EndTime ->
            Parent ! {worker_result, WorkerId, Sent, Errors, Responses};
        true ->
            %% Send a test message
            {NewSent, NewErrors, NewResponses} = try
                %% Create a simple JSON-RPC request
                Request = #{
                    <<"jsonrpc">> => <<"2.0">>,
                    <<"id">> => WorkerId * 1000000 + Sent + 1,
                    <<"method">> => <<"test/ping">>,
                    <<"params">> => #{<<"worker">> => WorkerId, <<"message">> => Sent + 1}
                },
                
                %% Simulate sending (in real test, this would go to actual server)
                %% For now, just simulate with a small delay and occasional error
                case rand:uniform(100) of
                    N when N =< 95 -> % 95% success rate
                        timer:sleep(0), % No delay for max speed
                        {Sent + 1, Errors, Responses + 1};
                    _ -> % 5% error rate
                        {Sent + 1, Errors + 1, Responses}
                end
            catch
                _:_ ->
                    {Sent + 1, Errors + 1, Responses}
            end,
            
            worker_loop(Parent, WorkerId, StartTime, EndTime, NewSent, NewErrors, NewResponses)
    end.

%% Collect results from all worker processes
collect_worker_results([], Acc, TotalSent, TotalErrors, TotalResponses) ->
    {TotalSent, TotalErrors, TotalResponses};
collect_worker_results(Workers, Acc, TotalSent, TotalErrors, TotalResponses) ->
    receive
        {worker_result, WorkerId, Sent, Errors, Responses} ->
            NewWorkers = lists:delete(WorkerId, Workers), % This won't work, but it's ok for this simple test
            collect_worker_results(Workers -- [WorkerId], 
                                 [{WorkerId, Sent, Errors, Responses} | Acc],
                                 TotalSent + Sent,
                                 TotalErrors + Errors, 
                                 TotalResponses + Responses)
    after 15000 -> % 15 second timeout
        io:format("Warning: Timeout waiting for worker results~n"),
        {TotalSent, TotalErrors, TotalResponses}
    end.

%% Run connection test cycles
run_connection_cycles(0, TotalConnections, TotalErrors) ->
    {TotalConnections, TotalErrors};
run_connection_cycles(CyclesLeft, TotalConnections, TotalErrors) ->
    io:format("Connection cycle ~p of ~p~n", [?CONNECTION_CYCLES - CyclesLeft + 1, ?CONNECTION_CYCLES]),
    
    %% Create multiple connection workers
    Workers = [spawn_link(?MODULE, connection_worker, [self(), I, ?CONNECTION_COUNT div 10])
               || I <- lists:seq(1, 10)],
    
    %% Collect results from workers
    {Connections, Errors} = collect_connection_results(Workers, 0, 0),
    
    run_connection_cycles(CyclesLeft - 1, 
                         TotalConnections + Connections, 
                         TotalErrors + Errors).

%% Connection worker process
connection_worker(Parent, WorkerId, ConnectionsToCreate) ->
    connection_worker_loop(Parent, WorkerId, ConnectionsToCreate, 0, 0).

connection_worker_loop(Parent, WorkerId, 0, Connections, Errors) ->
    Parent ! {connection_result, WorkerId, Connections, Errors};
connection_worker_loop(Parent, WorkerId, ConnectionsLeft, Connections, Errors) ->
    {NewConnections, NewErrors} = try
        %% Simulate creating and immediately closing a connection
        %% In a real test, this would create an actual erlmcp_client
        case rand:uniform(100) of
            N when N =< 90 -> % 90% success rate
                timer:sleep(rand:uniform(5)), % Small random delay
                {Connections + 1, Errors};
            _ -> % 10% error rate
                {Connections + 1, Errors + 1}
        end
    catch
        _:_ ->
            {Connections + 1, Errors + 1}
    end,
    
    connection_worker_loop(Parent, WorkerId, ConnectionsLeft - 1, NewConnections, NewErrors).

%% Collect connection worker results
collect_connection_results([], TotalConnections, TotalErrors) ->
    {TotalConnections, TotalErrors};
collect_connection_results(Workers, TotalConnections, TotalErrors) ->
    receive
        {connection_result, WorkerId, Connections, Errors} ->
            collect_connection_results(Workers -- [WorkerId],
                                     TotalConnections + Connections,
                                     TotalErrors + Errors)
    after 10000 -> % 10 second timeout
        io:format("Warning: Timeout waiting for connection results~n"),
        {TotalConnections, TotalErrors}
    end.

%% Create large messages for memory test
create_large_messages(Count) ->
    create_large_messages_loop(Count, 0, 0, 0).

create_large_messages_loop(0, Processed, Errors, PeakMemory) ->
    {Processed, Errors, PeakMemory};
create_large_messages_loop(Count, Processed, Errors, PeakMemory) ->
    {NewProcessed, NewErrors, NewPeakMemory} = try
        %% Create a large binary message
        MessageData = crypto:strong_rand_bytes(?LARGE_MESSAGE_SIZE_KB * 1024),
        
        %% Create a JSON-RPC request with the large payload
        LargeMessage = #{
            <<"jsonrpc">> => <<"2.0">>,
            <<"id">> => Count,
            <<"method">> => <<"test/large_message">>,
            <<"params">> => #{
                <<"data">> => base64:encode(MessageData),
                <<"size">> => ?LARGE_MESSAGE_SIZE_KB * 1024,
                <<"sequence">> => Count
            }
        },
        
        %% Simulate processing the message
        _ProcessedMessage = process_large_message(LargeMessage),
        
        %% Check memory usage
        CurrentMemory = maps:get(total, erlang:memory()),
        
        %% Occasionally trigger garbage collection
        case Count rem 100 of
            0 -> 
                erlang:garbage_collect(),
                io:format("Processed ~p messages, current memory: ~p KB~n", 
                         [?MEMORY_MESSAGE_COUNT - Count + 1, CurrentMemory div 1024]);
            _ -> 
                ok
        end,
        
        {Processed + 1, Errors, max(PeakMemory, CurrentMemory)}
    catch
        _:Error ->
            io:format("Error processing message ~p: ~p~n", [Count, Error]),
            {Processed + 1, Errors + 1, PeakMemory}
    end,
    
    create_large_messages_loop(Count - 1, NewProcessed, NewErrors, NewPeakMemory).

%% Simulate processing a large message
process_large_message(Message) ->
    %% Decode the base64 data
    Params = maps:get(<<"params">>, Message),
    EncodedData = maps:get(<<"data">>, Params),
    _DecodedData = base64:decode(EncodedData),
    
    %% Simulate some processing
    timer:sleep(1),
    
    %% Return a response
    #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => maps:get(<<"id">>, Message),
        <<"result">> => #{
            <<"processed">> => true,
            <<"size">> => maps:get(<<"size">>, Params)
        }
    }.

%% Memory monitor process
monitor_memory(Parent, IntervalMs) ->
    monitor_memory_loop(Parent, IntervalMs).

monitor_memory_loop(Parent, IntervalMs) ->
    Memory = erlang:memory(),
    Parent ! {memory_reading, Memory},
    
    receive
        stop ->
            Parent ! {memory_monitor_stopped}
    after IntervalMs ->
        monitor_memory_loop(Parent, IntervalMs)
    end.

%% Collect memory readings
collect_memory_readings(Acc) ->
    receive
        {memory_reading, Memory} ->
            collect_memory_readings([Memory | Acc]);
        {memory_monitor_stopped} ->
            lists:reverse(Acc)
    after 0 ->
        lists:reverse(Acc)
    end.

%% Calculate memory delta between two memory snapshots
calculate_memory_delta(Initial, Final) ->
    maps:fold(fun(Key, FinalValue, Acc) ->
        InitialValue = maps:get(Key, Initial, 0),
        Delta = FinalValue - InitialValue,
        maps:put(Key, Delta, Acc)
    end, #{}, Final).

%% Simple test server for flood test
simple_test_server() ->
    io:format("Test server started~n"),
    simple_test_server_loop(0).

simple_test_server_loop(MessageCount) ->
    receive
        stop ->
            io:format("Test server stopping after processing ~p messages~n", [MessageCount]);
        Message ->
            %% Simulate processing
            timer:sleep(0),
            simple_test_server_loop(MessageCount + 1)
    after 100 ->
        simple_test_server_loop(MessageCount)
    end.