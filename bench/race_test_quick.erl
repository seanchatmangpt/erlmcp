%%% Quick Race Condition Test - Smaller scale for faster completion
-module(race_test_quick).
-export([run/0, run/2]).

run() ->
    run(1000, 100).

run(NumClients, OpsPerClient) ->
    io:format("~n=== RACE CONDITION BOMBARDMENT (QUICK) ===~n~n"),
    
    io:format("Configuration: ~p clients, ~p ops each = ~p total ops~n", 
              [NumClients, OpsPerClient, NumClients * OpsPerClient]),
    io:format("~n"),
    
    ets:new(race_counter, [named_table, public]),
    ets:insert(race_counter, {count, 0}),
    ets:insert(race_counter, {negative_reads, 0}),
    ets:insert(race_counter, {total_ops, 0}),
    
    StartTime = erlang:monotonic_time(millisecond),
    
    Pids = [spawn_monitor(fun() -> worker(OpsPerClient) end) 
            || _ <- lists:seq(1, NumClients)],
    
    Completed = wait_for(Pids, 0, 30000),
    
    EndTime = erlang:monotonic_time(millisecond),
    
    [{count, Final}] = ets:lookup(race_counter, count),
    [{negative_reads, NegReads}] = ets:lookup(race_counter, negative_reads),
    [{total_ops, TotalOps}] = ets:lookup(race_counter, total_ops),
    
    DurationS = (EndTime - StartTime) / 1000,
    Expected = round(NumClients * OpsPerClient * 0.7),
    Lost = max(0, Expected - Final),
    
    io:format("~n=== RESULTS ===~n~n"),
    io:format("Expected Final Value: ~p~n", [Expected]),
    io:format("Actual Final Value: ~p~n", [Final]),
    io:format("Lost Updates: ~p (~.2f%)~n", [Lost, (Lost/Expected*100)]),
    io:format("Negative Reads: ~p~n", [NegReads]),
    io:format("Completed Workers: ~p/~p~n", [Completed, NumClients]),
    io:format("Duration: ~.2f seconds~n", [DurationS]),
    io:format("Throughput: ~.0f ops/sec~n", [TotalOps / DurationS]),
    io:format("~n"),
    
    if Lost > 0 ->
        io:format("CORRUPTION DETECTED!~n"),
        io:format("  Lost update anomaly: ~p increments lost~n", [Lost]),
        io:format("  Cause: Non-atomic read-modify-write operations~n"),
        io:format("  Fix: Use ets:update_counter/3 for atomic ops~n");
       Final < 0 ->
        io:format("ETS CORRUPTION! Negative counter value~n");
       true ->
        io:format("NO CORRUPTION - System handled ~p concurrent ops correctly~n", [TotalOps])
    end,
    io:format("~n=== END ===~n~n"),
    
    ets:delete(race_counter),
    {ok, #{expected => Expected, actual => Final, lost => Lost}}.

worker(0) -> ok;
worker(N) ->
    case rand:uniform(100) of
        X when X =< 70 ->
            [{count, Val}] = ets:lookup(race_counter, count),
            ets:insert(race_counter, {count, Val + 1}),
            ets:update_counter(race_counter, total_ops, {2, 1});
        _ ->
            case ets:lookup(race_counter, count) of
                [{count, V}] when V < 0 ->
                    ets:update_counter(race_counter, negative_reads, {2, 1});
                _ -> ok
            end,
            ets:update_counter(race_counter, total_ops, {2, 1})
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
