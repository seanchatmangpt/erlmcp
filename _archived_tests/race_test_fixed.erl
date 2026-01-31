%%% Race Condition Test - FIXED VERSION with atomic operations
-module(race_test_fixed).
-export([run/0]).

run() ->
    io:format("~n=== RACE CONDITION TEST - FIXED (Atomic Operations) ===~n~n"),
    
    NumClients = 1000,
    OpsPerClient = 100,
    
    io:format("Same workload: ~p clients, ~p ops each = ~p total ops~n", 
              [NumClients, OpsPerClient, NumClients * OpsPerClient]),
    io:format("FIX: Using ets:update_counter/3 for atomic increments~n"),
    io:format("~n"),
    
    ets:new(race_counter_fixed, [named_table, public]),
    ets:insert(race_counter_fixed, {count, 0}),
    
    StartTime = erlang:monotonic_time(millisecond),
    
    Pids = [spawn_monitor(fun() -> worker_fixed(OpsPerClient) end) 
            || _ <- lists:seq(1, NumClients)],
    
    Completed = wait_for(Pids, 0, 30000),
    
    EndTime = erlang:monotonic_time(millisecond),
    
    [{count, Final}] = ets:lookup(race_counter_fixed, count),
    
    DurationS = (EndTime - StartTime) / 1000.0,
    TotalOps = NumClients * OpsPerClient,
    Expected = round(TotalOps * 0.7),
    Lost = max(0, Expected - Final),
    
    io:format("RESULTS (FIXED):~n"),
    io:format("  Expected Final Value: ~p~n", [Expected]),
    io:format("  Actual Final Value: ~p~n", [Final]),
    io:format("  Lost Updates: ~p~n", [Lost]),
    io:format("~n"),
    
    if Lost =:= 0 ->
        io:format("SUCCESS! Atomic operations prevent race conditions~n");
       true ->
        io:format("Unexpected: ~p lost updates remain~n", [Lost])
    end,
    io:format("~n"),
    
    ets:delete(race_counter_fixed),
    
    {ok, #{expected => Expected, actual => Final, lost => Lost}}.

worker_fixed(0) -> ok;
worker_fixed(N) ->
    case rand:uniform(100) of
        X when X =< 70 ->
            % ATOMIC INCREMENT using update_counter
            ets:update_counter(race_counter_fixed, count, {2, 1}, {count, 0});
        _ ->
            % Read
            ets:lookup(race_counter_fixed, count)
    end,
    worker_fixed(N - 1).

wait_for([], Completed, _) -> Completed;
wait_for([{Pid, Ref}|Rest], Completed, Timeout) ->
    receive
        {'DOWN', Ref, process, Pid, _} ->
            wait_for(Rest, Completed + 1, Timeout)
    after Timeout ->
        exit(Pid, kill),
        wait_for(Rest, Completed, 0)
    end.
