%%%-------------------------------------------------------------------
%%% @doc Race Condition Bombardment Test - Standalone Version
%%%
%%% DESTRUCTIVE TEST #12: Identify corruption bugs under concurrent load
%%% This is a standalone test that can be run directly with erlc + erl
%%%-------------------------------------------------------------------
-module(race_condition_bombardment).
-export([run_test/0, run_test/2, main/0]).

-define(ETS_TABLE, race_counter).
-define(DEFAULT_CLIENTS, 10000).
-define(DEFAULT_OPS, 1000).

%% @doc Main entry point
main() ->
    io:format("~n=== RACE CONDITION BOMBARDMENT CRASH TEST ===~n~n"),
    
    io:format("Test Configuration:~n"),
    io:format("  Shared Resource: ETS counter~n"),
    io:format("  Initial Value: 0~n"),
    io:format("  Concurrent Clients: ~p~n", [?DEFAULT_CLIENTS]),
    io:format("  Operations per Client: ~p~n", [?DEFAULT_OPS]),
    io:format("  Total Operations: ~p~n", [?DEFAULT_CLIENTS * ?DEFAULT_OPS]),
    io:format("~n"),
    
    io:format("Operation Mix:~n"),
    io:format("  Increment: 70% (should be ~p)~n", [round(?DEFAULT_CLIENTS * ?DEFAULT_OPS * 0.7)]),
    io:format("  Read: 20%~n"),
    io:format("  Set: 10%~n"),
    io:format("~n"),
    
    run_test(?DEFAULT_CLIENTS, ?DEFAULT_OPS).

%% @doc Run the race condition test
run_test() ->
    run_test(?DEFAULT_CLIENTS, ?DEFAULT_OPS).

%% @doc Run with custom parameters
run_test(NumClients, OpsPerClient) ->
    % Create ETS table for shared counter
    ets:new(?ETS_TABLE, [named_table, public, {read_concurrency, true}, {write_concurrency, true}]),
    ets:insert(?ETS_TABLE, {count, 0}),
    ets:insert(?ETS_TABLE, {negative_reads, 0}),
    ets:insert(?ETS_TABLE, {impossible_reads, 0}),
    ets:insert(?ETS_TABLE, {total_reads, 0}),
    ets:insert(?ETS_TABLE, {total_increments, 0}),
    
    io:format("Starting race condition bombardment...~n"),
    io:format("Spawning ~p concurrent worker processes...~n", [NumClients]),
    
    StartTime = erlang:monotonic_time(microsecond),
    
    % Spawn all workers
    WorkerPids = [spawn_monitor(fun() -> worker_loop(OpsPerClient) end) || _ <- lists:seq(1, NumClients)],
    
    % Wait for completion
    Completed = wait_for_completion(WorkerPids, 0, 60000),
    
    EndTime = erlang:monotonic_time(microsecond),
    DurationUs = EndTime - StartTime,
    DurationS = DurationUs / 1000000,
    
    % Get final metrics
    [{count, FinalCount}] = ets:lookup(?ETS_TABLE, count),
    [{negative_reads, NegReads}] = ets:lookup(?ETS_TABLE, negative_reads),
    [{impossible_reads, ImpossibleReads}] = ets:lookup(?ETS_TABLE, impossible_reads),
    [{total_reads, TotalReads}] = ets:lookup(?ETS_TABLE, total_reads),
    [{total_increments, TotalIncrements}] = ets:lookup(?ETS_TABLE, total_increments),
    
    % Calculate expected value
    TotalOps = NumClients * OpsPerClient,
    ExpectedIncrements = round(TotalOps * 0.7),
    ExpectedValue = ExpectedIncrements,
    
    % Calculate lost updates
    LostUpdates = case FinalCount of
        Val when Val >= 0 -> max(0, ExpectedValue - Val);
        _ -> ExpectedValue
    end,
    
    UpdateSuccessRate = case ExpectedValue of
        0 -> 100.0;
        _ -> (FinalCount / ExpectedValue) * 100
    end,
    
    % Print results
    print_results(#{
        num_clients => NumClients,
        ops_per_client => OpsPerClient,
        total_ops => TotalOps,
        expected_value => ExpectedValue,
        actual_value => FinalCount,
        lost_updates => LostUpdates,
        update_success_rate => UpdateSuccessRate,
        negative_reads => NegReads,
        impossible_reads => ImpossibleReads,
        total_reads => TotalReads,
        total_increments => TotalIncrements,
        completed_workers => Completed,
        duration_s => DurationS,
        ops_per_second => TotalOps / DurationS
    }),
    
    % Cleanup
    ets:delete(?ETS_TABLE),
    
    % Return result
    {ok, #{
        final_count => FinalCount,
        lost_updates => LostUpdates,
        corruption_detected => LostUpdates > 0 orelse FinalCount < 0
    }}.

%% @doc Worker process - performs mixed operations
worker_loop(0) ->
    ok;
worker_loop(RemainingOps) ->
    % Random operation: 70% increment, 20% read, 10% set
    Op = case rand:uniform(100) of
        N when N =< 70 -> increment;
        N when N =< 90 -> read;
        _ -> set
    end,
    
    perform_op(Op),
    worker_loop(RemainingOps - 1).

%% @doc Perform operation WITHOUT LOCKING (classic race condition)
perform_op(increment) ->
    % Non-atomic increment (classic lost update bug)
    case ets:lookup(?ETS_TABLE, count) of
        [{count, Current}] when Current >= 0 ->
            NewVal = Current + 1,
            ets:insert(?ETS_TABLE, {count, NewVal}),
            ets:update_counter(?ETS_TABLE, total_increments, {2, 1}, {total_increments, 0});
        [{count, Current}] when Current < 0 ->
            % Negative value detected - CORRUPTION!
            ets:update_counter(?ETS_TABLE, negative_reads, {2, 1}, {negative_reads, 0}),
            ets:insert(?ETS_TABLE, {count, 0});  % Reset corrupted counter
        [] ->
            ets:insert(?ETS_TABLE, {count, 1})
    end;

perform_op(read) ->
    % Read operation - check for corruption
    case ets:lookup(?ETS_TABLE, count) of
        [{count, Val}] when Val < 0 ->
            % Corruption detected!
            ets:update_counter(?ETS_TABLE, negative_reads, {2, 1}, {negative_reads, 0}),
            ets:update_counter(?ETS_TABLE, total_reads, {2, 1}, {total_reads, 0});
        [{count, Val}] when Val > 1000000000 ->
            % Impossible value - corruption
            ets:update_counter(?ETS_TABLE, impossible_reads, {2, 1}, {impossible_reads, 0}),
            ets:update_counter(?ETS_TABLE, total_reads, {2, 1}, {total_reads, 0});
        [{count, _Val}] ->
            ets:update_counter(?ETS_TABLE, total_reads, {2, 1}, {total_reads, 0});
        [] ->
            ok
    end;

perform_op(set) ->
    % Set to random value (chaos operation)
    RandomVal = rand:uniform(1000),
    ets:insert(?ETS_TABLE, {count, RandomVal}).

%% @doc Wait for all workers to complete
wait_for_completion([], Completed, _Timeout) ->
    Completed;
wait_for_completion([{Pid, Ref} | Rest], Completed, Timeout) ->
    receive
        {'DOWN', Ref, process, Pid, normal} ->
            wait_for_completion(Rest, Completed + 1, Timeout);
        {'DOWN', Ref, process, Pid, _Reason} ->
            % Worker crashed
            wait_for_completion(Rest, Completed, Timeout)
    after Timeout ->
        % Timeout - kill remaining
        exit(Pid, kill),
        wait_for_completion(Rest, Completed, 0)
    end.

%% @doc Print comprehensive results
print_results(Results) ->
    io:format("~n=== RACE CONDITION BOMBARDMENT RESULTS ===~n~n"),
    
    io:format("Test Configuration:~n"),
    io:format("  Shared Resource: ETS counter~n"),
    io:format("  Initial Value: 0~n"),
    io:format("  Concurrent Clients: ~p~n", [maps:get(num_clients, Results)]),
    io:format("  Operations per Client: ~p~n", [maps:get(ops_per_client, Results)]),
    io:format("  Total Operations: ~p~n", [maps:get(total_ops, Results)]),
    io:format("~n"),
    
    io:format("Operation Mix:~n"),
    io:format("  Increment: 70% (expected: ~p)~n", [maps:get(expected_value, Results)]),
    io:format("  Read: 20% (~p actual reads)~n", [maps:get(total_reads, Results)]),
    io:format("  Set: 10%~n"),
    io:format("~n"),
    
    io:format("RESULTS:~n"),
    io:format("  Expected Final Value: ~p~n", [maps:get(expected_value, Results)]),
    io:format("  Actual Final Value: ~p~n", [maps:get(actual_value, Results)]),
    io:format("  Lost Updates: ~p (~.2f%)~n", 
              [maps:get(lost_updates, Results),
               (maps:get(lost_updates, Results) / maps:get(expected_value, Results) * 100)]),
    io:format("  Update Success Rate: ~.2f%~n", [maps:get(update_success_rate, Results)]),
    io:format("~n"),
    
    io:format("CORRUPTION DETECTED:~n"),
    io:format("  Negative Values Read: ~p~n", [maps:get(negative_reads, Results)]),
    io:format("  Impossible Values: ~p~n", [maps:get(impossible_reads, Results)]),
    io:format("  ETS Corruption: ~p~n", [maps:get(actual_value, Results) < 0]),
    io:format("~n"),
    
    io:format("READ CONSISTENCY:~n"),
    TotalReads = maps:get(total_reads, Results),
    NegReads = maps:get(negative_reads, Results),
    ImpossibleReads = maps:get(impossible_reads, Results),
    ReadConsistency = case TotalReads of
        0 -> 100.0;
        _ -> 100 - ((NegReads + ImpossibleReads) / TotalReads * 100)
    end,
    io:format("  Total Reads: ~p~n", [TotalReads]),
    io:format("  Reads Saw Negative: ~p~n", [NegReads]),
    io:format("  Reads Saw Unexpected: ~p~n", [ImpossibleReads]),
    io:format("  Read Consistency: ~.2f%~n", [ReadConsistency]),
    io:format("~n"),
    
    io:format("PROCESS FAILURES:~n"),
    ExpectedWorkers = maps:get(num_clients, Results),
    CompletedWorkers = maps:get(completed_workers, Results),
    CrashedWorkers = ExpectedWorkers - CompletedWorkers,
    io:format("  Expected Workers: ~p~n", [ExpectedWorkers]),
    io:format("  Completed Workers: ~p~n", [CompletedWorkers]),
    io:format("  Crashed Workers: ~p~n", [CrashedWorkers]),
    io:format("~n"),
    
    io:format("PERFORMANCE:~n"),
    io:format("  Test Duration: ~.2f seconds~n", [maps:get(duration_s, Results)]),
    io:format("  Operations/Second: ~.0f~n", [maps:get(ops_per_second, Results)]),
    io:format("~n"),
    
    LostUpdates = maps:get(lost_updates, Results),
    ActualValue = maps:get(actual_value, Results),
    
    io:format("ANALYSIS:~n"),
    if LostUpdates > 0 ->
        io:format("  ✗ CORRUPTION DETECTED!~n"),
        io:format("  Lost ~p updates due to race conditions~n", [LostUpdates]),
        io:format("  This is the CLASSIC LOST UPDATE ANOMALY~n"),
        io:format("  Non-atomic read-modify-write operations cause data loss~n"),
        io:format("  FIX: Use ets:update_counter/3 for atomic increments~n");
       ActualValue < 0 ->
        io:format("  ✗ ETS CORRUPTION DETECTED!~n"),
        io:format("  Counter has negative value - impossible under normal operation~n"),
        io:format("  This indicates ETS table corruption~n");
       true ->
        io:format("  ✓ NO CORRUPTION DETECTED~n"),
        io:format("  ETS handled ~p concurrent operations correctly~n", [maps:get(total_ops, Results)]),
        io:format("  System is race-condition safe for this workload~n")
    end,
    
    io:format("~n"),
    
    if CrashedWorkers > 0 ->
        io:format("CRASH TRIGGERS:~n"),
        io:format("  ~p workers crashed during test~n", [CrashedWorkers]),
        io:format("  Possible causes: OOM, process limit, or corruption~n");
       true ->
        ok
    end,
    
    io:format("~n=== END OF RACE CONDITION BOMBARDMENT TEST ===~n~n").
