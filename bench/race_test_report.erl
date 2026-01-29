-module(race_test_report).
-export([run/0, run/2]).

run() ->
    run(1000, 100).

run(NumClients, OpsPerClient) ->
    io:format("~n=== RACE CONDITION BOMBARDMENT CRASH TEST ===~n~n"),
    
    io:format("Test Configuration:~n"),
    io:format("  Shared Resource: ETS counter~n"),
    io:format("  Initial Value: 0~n"),
    io:format("  Concurrent Clients: ~p~n", [NumClients]),
    io:format("  Operations per Client: ~p~n", [OpsPerClient]),
    io:format("  Total Operations: ~p~n", [NumClients * OpsPerClient]),
    io:format("~n"),
    io:format("Operation Mix:~n"),
    io:format("  Increment: 70% (expected: ~p)~n", [round(NumClients * OpsPerClient * 0.7)]),
    io:format("  Read: 20%~n"),
    io:format("  Set: 10%~n"),
    io:format("~n"),
    
    ets:new(race_counter, [named_table, public]),
    ets:insert(race_counter, {count, 0}),
    ets:insert(race_counter, {negative_reads, 0}),
    ets:insert(race_counter, {total_reads, 0}),
    
    StartTime = erlang:monotonic_time(millisecond),
    
    Pids = [spawn_monitor(fun() -> worker(OpsPerClient) end) 
            || _ <- lists:seq(1, NumClients)],
    
    Completed = wait_for(Pids, 0, 30000),
    
    EndTime = erlang:monotonic_time(millisecond),
    
    [{count, Final}] = ets:lookup(race_counter, count),
    [{negative_reads, NegReads}] = ets:lookup(race_counter, negative_reads),
    [{total_reads, TotalReads}] = ets:lookup(race_counter, total_reads),
    
    DurationS = (EndTime - StartTime) / 1000.0,
    TotalOps = NumClients * OpsPerClient,
    Expected = round(TotalOps * 0.7),
    Lost = max(0, Expected - Final),
    
    io:format("RESULTS:~n"),
    io:format("  Expected Final Value: ~p~n", [Expected]),
    io:format("  Actual Final Value: ~p~n", [Final]),
    io:format("  Lost Updates: ~p (~.2f%)~n", [Lost, (Lost / Expected * 100.0)]),
    io:format("~n"),
    
    io:format("CORRUPTION DETECTED:~n"),
    io:format("  Negative Values Read: ~p~n", [NegReads]),
    io:format("  ETS Corruption: ~p~n", [Final < 0]),
    io:format("~n"),
    
    io:format("PROCESS FAILURES:~n"),
    io:format("  Completed Workers: ~p/~p~n", [Completed, NumClients]),
    io:format("~n"),
    
    io:format("PERFORMANCE:~n"),
    io:format("  Test Duration: ~.2f seconds~n", [DurationS]),
    Throughput = round(TotalOps / DurationS),
    io:format("  Operations/Second: ~p~n", [Throughput]),
    io:format("~n"),
    
    io:format("ANALYSIS:~n"),
    if Lost > 0 ->
        io:format("  CORRUPTION DETECTED!~n"),
        io:format("  Lost ~p updates due to race conditions~n", [Lost]),
        io:format("  CLASSIC LOST UPDATE ANOMALY~n"),
        io:format("  FIX: Use ets:update_counter/3 for atomic increments~n");
       Final < 0 ->
        io:format("  ETS CORRUPTION DETECTED!~n");
       true ->
        io:format("  NO CORRUPTION DETECTED~n")
    end,
    io:format("~n"),
    
    io:format("=== END OF RACE CONDITION BOMBARDMENT TEST ===~n~n"),
    
    ets:delete(race_counter),
    
    {ok, #{expected => Expected, actual => Final, lost => Lost}}.

worker(0) -> ok;
worker(N) ->
    case rand:uniform(100) of
        X when X =< 70 ->
            [{count, Val}] = ets:lookup(race_counter, count),
            ets:insert(race_counter, {count, Val + 1});
        _ ->
            case ets:lookup(race_counter, count) of
                [{count, V}] when V < 0 ->
                    ets:update_counter(race_counter, negative_reads, {2, 1}, {negative_reads, 0}),
                    ets:update_counter(race_counter, total_reads, {2, 1}, {total_reads, 0});
                [{count, _}] ->
                    ets:update_counter(race_counter, total_reads, {2, 1}, {total_reads, 0});
                [] -> ok
            end
    end,
    worker(N - 1).

wait_for([], Completed, _) -> Completed;
wait_for([{Pid, Ref}|Rest], Completed, Timeout) ->
    receive
        {'DOWN', Ref, process, Pid, _} ->
            wait_for(Rest, Completed + 1, Timeout)
    after Timeout ->
        exit(Pid, kill),
        wait_for(Rest, Completed, 0)
    end.
