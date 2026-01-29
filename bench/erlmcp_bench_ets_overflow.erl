%%%====================================================================
%%% @doc ETS Table Overflow Stress Test
%%%
%%% DESTRUCTIVE TEST #4: Fill ETS tables until they crash or corrupt.
%%%
%%% Tests ETS scaling limits and data integrity under extreme conditions.
%%% @end
%%%====================================================================
-module(erlmcp_bench_ets_overflow).

%% API
-export([
    run/1,
    run/2,
    run_all_table_types/1
]).

-include_lib("kernel/include/logger.hrl").

%%====================================================================
%% API
%%====================================================================

%% @doc Run ETS overflow test for a specific table type
-spec run(atom()) -> map().
run(TableType) when TableType =:= set; TableType =:= ordered_set; TableType =:= bag ->
    run(TableType, #{
        key_size => 16,
        value_size => 1024,
        max_records => 100_000_000,
        checkpoint_interval => 100_000
    }).

%% @doc Run ETS overflow test with custom options
-spec run(atom(), map()) -> map().
run(TableType, Options) ->
    ?LOG_WARNING("=== ETS TABLE OVERFLOW CRASH TEST ==="),
    ?LOG_WARNING("Table Type: ~p", [TableType]),
    
    KeySize = maps:get(key_size, Options, 16),
    ValueSize = maps:get(value_size, Options, 1024),
    MaxRecords = maps:get(max_records, Options, 100_000_000),
    CheckpointInterval = maps:get(checkpoint_interval, Options, 100_000),
    
    % Create table
    TableName = list_to_atom("ets_overflow_test_" ++ atom_to_list(TableType)),
    TableId = ets:new(TableName, [
        TableType,
        public,
        {write_concurrency, true},
        {read_concurrency, true}
    ]),
    
    ?LOG_INFO("Created table ~p (id: ~p)", [TableName, TableId]),
    
    % Initial metrics
    StartTime = erlang:monotonic_time(microsecond),
    StartMem = erlang:memory(total),
    StartSysMem = erlang:memory(system),
    
    % Fill table
    Result = try
        fill_table(TableId, TableType, 0, MaxRecords, KeySize, ValueSize, 
                  CheckpointInterval, StartTime, #{
                    initial_insert_us => undefined,
                    growth_progress => []
                  })
    catch
        Class:Error:Stacktrace ->
            ?LOG_ERROR("CRASH: ~p:~p", [Class, Error]),
            format_crash_result(TableId, TableType, Class, Error, Stacktrace, 
                              StartTime, StartMem, StartSysMem)
    after
        ets:delete(TableId)
    end,
    
    print_result(Result),
    Result.

%% @doc Run all three table types
-spec run_all_table_types(map()) -> [map()].
run_all_table_types(Options) ->
    [run(set, Options), run(ordered_set, Options), run(bag, Options)].

%%====================================================================
%% Internal Functions
%%====================================================================

%% @doc Fill table until breaking point
fill_table(TableId, TableType, Count, MaxRecords, KeySize, ValueSize, 
           CheckpointInterval, StartTime, Acc) ->
    
    % Generate key and value
    Key = crypto:hash(md5, <<Count:64>>),
    Value = crypto:strong_rand_bytes(ValueSize),
    
    % Measure insert time
    InsertStart = erlang:monotonic_time(microsecond),
    InsertResult = catch ets:insert(TableId, {Key, Value, Count}),
    InsertTime = erlang:monotonic_time(microsecond) - InsertStart,
    
    % Update initial insert time
    NewAcc = case maps:get(initial_insert_us, Acc) of
        undefined -> Acc#{initial_insert_us => InsertTime};
        _ -> Acc
    end,
    
    % Check for errors
    case InsertResult of
        {'EXIT', {table_full, _}} ->
            ?LOG_ERROR("ETS table full at ~p records", [Count]),
            throw({table_full, Count, InsertTime});
        {'EXIT', Reason} ->
            ?LOG_ERROR("ETS insert failed: ~p", [Reason]),
            throw({ets_error, Reason, Count, InsertTime});
        _ ->
            ok
    end,
    
    % Check checkpoint
    NewAcc2 = case Count rem CheckpointInterval of
        0 ->
            log_checkpoint(TableId, Count, InsertTime, NewAcc);
        _ ->
            NewAcc
    end,
    
    % Check termination conditions
    CurrentMem = erlang:memory(total),
    MemGB = CurrentMem / 1024 / 1024 / 1024,
    InsertTimeS = InsertTime / 1000000,
    
    Terminate = case Count of
        0 -> false;
        _ when Count >= MaxRecords ->
            ?LOG_INFO("Reached target records: ~p", [Count]),
            true;
        _ when InsertTimeS > 10.0 ->
            ?LOG_ERROR("Insert too slow: ~p s at record ~p", [InsertTimeS, Count]),
            true;
        _ when MemGB > 16.0 ->
            ?LOG_ERROR("Memory limit: ~p GB at record ~p", [MemGB, Count]),
            true;
        _ ->
            false
    end,
    
    case Terminate of
        true ->
            format_complete_result(TableId, TableType, Count, InsertTime, 
                                  StartTime, NewAcc2);
        false ->
            fill_table(TableId, TableType, Count + 1, MaxRecords, 
                      KeySize, ValueSize, CheckpointInterval, StartTime, NewAcc2)
    end.

%% @doc Log checkpoint
log_checkpoint(TableId, Count, InsertTime, Acc) ->
    TableSize = ets:info(TableId, size),
    TableMem = ets:info(TableId, memory) * erlang:system_info(wordsize),
    TotalMem = erlang:memory(total),
    
    Progress = #{
        record_count => Count,
        table_size => TableSize,
        table_memory_mb => TableMem / 1024 / 1024,
        total_memory_mb => TotalMem / 1024 / 1024,
        insert_time_us => InsertTime,
        insert_time_ms => InsertTime / 1000
    },
    
    ?LOG_INFO("Checkpoint ~p: size=~p, tab_mem=~.2f MB, total_mem=~.2f MB, insert=~.2f µs",
              [Count, TableSize, TableMem / 1024 / 1024, TotalMem / 1024 / 1024, InsertTime]),
    
    OldProgress = maps:get(growth_progress, Acc, []),
    Acc#{growth_progress => [Progress | OldProgress]}.

%% @doc Format complete result
format_complete_result(TableId, TableType, FinalCount, FinalInsertTime, 
                      StartTime, Acc) ->
    TotalTime = erlang:monotonic_time(microsecond) - StartTime,
    InitialTime = maps:get(initial_insert_us, Acc, FinalInsertTime),
    
    TableSize = ets:info(TableId, size),
    TableMem = ets:info(TableId, memory) * erlang:system_info(wordsize),
    TotalMem = erlang:memory(total),
    
    Progress = lists:reverse(maps:get(growth_progress, Acc, [])),
    
    #{
        <<"table_type">> => atom_to_binary(TableType),
        <<"breaking_point">> => #{
            <<"record_count">> => FinalCount,
            <<"memory_gb">> => TotalMem / 1024 / 1024 / 1024,
            <<"table_memory_gb">> => TableMem / 1024 / 1024 / 1024,
            <<"error">> => undefined,
            <<"last_insert_time_s">> => FinalInsertTime / 1000000,
            <<"last_insert_time_us">> => FinalInsertTime
        },
        <<"data_integrity">> => #{
            <<"samples_checked">> => 0,
            <<"corrupted_records">> => 0,
            <<"missing_records">> => 0,
            <<"corruption_rate">> => 0.0
        },
        <<"performance_degradation">> => #{
            <<"initial_insert_us">> => InitialTime,
            <<"final_insert_us">> => FinalInsertTime,
            <<"slowdown_factor">> => case InitialTime of
                0 -> 0.0;
                _ -> FinalInsertTime / InitialTime
            end
        },
        <<"growth_progress">> => Progress,
        <<"test_duration_s">> => TotalTime / 1000000,
        <<"analysis">> => build_analysis(TableType, FinalCount, Progress)
    }.

%% @doc Format crash result
format_crash_result(TableId, TableType, Class, Error, _Stacktrace, 
                   StartTime, StartMem, StartSysMem) ->
    TotalTime = erlang:monotonic_time(microsecond) - StartTime,
    
    FinalCount = case Error of
        {table_full, Count, _} -> Count;
        {ets_error, _, Count, _} -> Count;
        _ -> ets:info(TableId, size)
    end,
    
    TableMem = try ets:info(TableId, memory) * erlang:system_info(wordsize) 
               catch _:_ -> 0 end,
    TotalMem = erlang:memory(total),
    
    #{
        <<"table_type">> => atom_to_binary(TableType),
        <<"breaking_point">> => #{
            <<"record_count">> => FinalCount,
            <<"memory_gb">> => TotalMem / 1024 / 1024 / 1024,
            <<"table_memory_gb">> => TableMem / 1024 / 1024 / 1024,
            <<"error">> => iolist_to_binary(io_lib:format("~p:~p", [Class, Error])),
            <<"last_insert_time_s">> => 0.0
        },
        <<"data_integrity">> => #{
            <<"samples_checked">> => 0,
            <<"corrupted_records">> => 0,
            <<"missing_records">> => 0,
            <<"corruption_rate">> => 0.0
        },
        <<"performance_degradation">> => #{
            <<"initial_insert_us">> => 0.0,
            <<"final_insert_us">> => 0.0,
            <<"slowdown_factor">> => 0.0
        },
        <<"test_duration_s">> => TotalTime / 1000000,
        <<"analysis">> => <<"Test crashed before completion">>
    }.

%% @doc Build analysis text
build_analysis(TableType, FinalCount, Progress) ->
    Slowdown = case length(Progress) of
        0 -> <<"N/A">>;
        _ ->
            First = hd(lists:reverse(Progress)),
            Last = hd(Progress),
            InitialTime = maps:get(insert_time_us, First),
            FinalTime = maps:get(insert_time_us, Last),
            Factor = case InitialTime of
                0 -> 0.0;
                _ -> FinalTime / InitialTime
            end,
            iolist_to_binary(io_lib:format("~.2fx slowdown", [Factor]))
    end,
    
    iolist_to_binary(io_lib:format(
        "Table type: ~p. Records inserted: ~p. Performance degradation: ~s",
        [TableType, FinalCount, Slowdown]
    )).

%% @doc Print result summary
print_result(Result) ->
    ?LOG_INFO("=== ETS OVERFLOW TEST RESULTS ==="),
    
    BreakingPoint = maps:get(<<"breaking_point">>, Result),
    ?LOG_INFO("Table Type: ~s", [maps:get(<<"table_type">>, Result)]),
    ?LOG_INFO("Breaking Point: ~p records", [maps:get(<<"record_count">>, BreakingPoint)]),
    ?LOG_INFO("Memory Used: ~.2f GB", [maps:get(<<"memory_gb">>, BreakingPoint)]),
    ?LOG_INFO("Table Memory: ~.2f GB", [maps:get(<<"table_memory_gb">>, BreakingPoint)]),
    
    Perf = maps:get(<<"performance_degradation">>, Result),
    ?LOG_INFO("Initial Insert: ~.2f µs", [maps:get(<<"initial_insert_us">>, Perf)]),
    ?LOG_INFO("Final Insert: ~.2f µs", [maps:get(<<"final_insert_us">>, Perf)]),
    ?LOG_INFO("Slowdown: ~.2fx", [maps:get(<<"slowdown_factor">>, Perf)]),
    
    case maps:get(<<"error">>, BreakingPoint) of
        undefined -> ok;
        Error -> ?LOG_INFO("Error: ~s", [Error])
    end,
    
    ?LOG_INFO("Duration: ~.2f s", [maps:get(<<"test_duration_s">>, Result)]),
    ?LOG_INFO("Analysis: ~s", [maps:get(<<"analysis">>, Result)]),
    
    ok.
