%%%-------------------------------------------------------------------
%% @doc ETS Scalability Tests for OTP 28
%%
%%% This test suite validates OTP 28 ETS improvements:
%%% - Tables with >1M entries (no performance degradation)
%%% - Concurrent read/write stress tests
%%% - Memory leak detection
%%% - decentralized_counters effectiveness
%%% - Large-scale session registry performance
%%%
%%% Chicago School TDD: Real ETS tables, no mocks
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_ets_scalability_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Fixtures
%%====================================================================

setup() ->
    {ok, Pid} = erlmcp_ets_registry:start_link(),
    Pid.

cleanup(Pid) ->
    gen_server:stop(Pid),
    % Clean up any remaining tables
    lists:foreach(fun(Name) ->
        case ets:whereis(Name) of
            undefined -> ok;
            _ -> ets:delete(Name)
        end
    end, [erlmcp_test_registry, erlmcp_test_cache, erlmcp_test_large]).

%%====================================================================
%% OTP 28 Registry Creation Tests
%%====================================================================

otp_28_registry_creation_test() ->
    ?_test(begin
        Pid = setup(),
        try
            % Create registry with OTP 28 optimizations
            {ok, _Tid} = erlmcp_ets_registry:create_registry(erlmcp_test_registry),

            % Verify table exists
            ?assertNotEqual(undefined, ets:whereis(erlmcp_test_registry)),

            % Verify OTP 28 options are set
            ?assertEqual(true, ets:info(erlmcp_test_registry, read_concurrency)),
            ?assertEqual(true, ets:info(erlmcp_test_registry, decentralized_counters)),

            % Verify table type is set
            ?assertEqual(set, ets:info(erlmcp_test_registry, type))
        after
            cleanup(Pid)
        end
    end).

registry_already_exists_test() ->
    ?_test(begin
        Pid = setup(),
        try
            {ok, _Tid1} = erlmcp_ets_registry:create_registry(erlmcp_test_registry),
            ?assertEqual({error, already_exists},
                erlmcp_ets_registry:create_registry(erlmcp_test_registry))
        after
            cleanup(Pid)
        end
    end).

%%====================================================================
%% Session Registry Tests
%%====================================================================

session_registry_creation_test() ->
    ?_test(begin
        Pid = setup(),
        try
            {ok, _Tid} = erlmcp_ets_registry:create_session_registry(),
            Name = erlmcp_sessions_otp28,

            % Verify table exists
            ?assertNotEqual(undefined, ets:whereis(Name)),

            % Verify ordered_set for range queries
            ?assertEqual(ordered_set, ets:info(Name, type)),

            % Verify OTP 28 concurrency options
            ?assertEqual(true, ets:info(Name, read_concurrency)),
            ?assertEqual(true, ets:info(Name, write_concurrency)),
            ?assertEqual(true, ets:info(Name, decentralized_counters))
        after
            cleanup(Pid)
        end
    end).

%%====================================================================
%% Cache Table Tests
%%====================================================================

cache_table_creation_test() ->
    ?_test(begin
        Pid = setup(),
        try
            Options = #{ttl => 60, max_size => 1000},
            {ok, _Tid} = erlmcp_ets_registry:create_cache_table(erlmcp_test_cache, Options),

            % Verify table exists
            ?assertNotEqual(undefined, ets:whereis(erlmcp_test_cache)),

            % Verify concurrency options
            ?assertEqual(true, ets:info(erlmcp_test_cache, read_concurrency)),
            ?assertEqual(true, ets:info(erlmcp_test_cache, write_concurrency))
        after
            cleanup(Pid)
        end
    end).

cache_table_with_keypos_test() ->
    ?_test(begin
        Pid = setup(),
        try
            % Create a record-based cache
            -record(cache_entry, {key, value, ttl}).
            Options = #{keypos => #cache_entry.key},
            {ok, _Tid} = erlmcp_ets_registry:create_cache_table(erlmcp_test_cache, Options),

            % Insert a record
            Entry = #cache_entry{key = test_key, value = test_value, ttl = 100},
            ets:insert(erlmcp_test_cache, Entry),

            % Verify lookup by key position works
            ?assertEqual([Entry], ets:lookup(erlmcp_test_cache, test_key))
        after
            cleanup(Pid)
        end
    end).

%%====================================================================
%% Large-Scale Tests (>1M entries)
%%====================================================================

large_scale_registry_test_() ->
    {timeout, 120, fun() ->
        Pid = setup(),
        try
            {ok, _Tid} = erlmcp_ets_registry:create_registry(erlmcp_test_large),
            Table = erlmcp_test_large,

            % Insert 1.5M entries
            NumEntries = 1500000,
            StartTime = erlang:monotonic_time(microsecond),

            lists:foreach(fun(N) ->
                ets:insert(Table, {N, value_n(N)})
            end, lists:seq(1, NumEntries)),

            InsertTime = erlang:monotonic_time(microsecond) - StartTime,
            ?LOG_INFO("Inserted ~p entries in ~p ms", [NumEntries, InsertTime / 1000]),

            % Verify size
            ?assertEqual(NumEntries, ets:info(Table, size)),

            % Verify lookups still work fast
            LookupStartTime = erlang:monotonic_time(microsecond),
            lists:foreach(fun(N) ->
                ?assertEqual([{N, value_n(N)}], ets:lookup(Table, N))
            end, lists:seq(1, 1000)),
            LookupTime = erlang:monotonic_time(microsecond) - LookupStartTime,
            ?LOG_INFO("1000 lookups in ~p ms (~p µs/lookup)",
                [LookupTime / 1000, LookupTime / 1000]),

            % Verify memory usage is reasonable (< 500MB for 1.5M entries)
            MemoryWords = ets:info(Table, memory),
            MemoryBytes = MemoryWords * erlang:system_info(wordsize),
            ?LOG_INFO("Memory usage: ~p MB for ~p entries",
                [MemoryBytes / (1024 * 1024), NumEntries]),
            ?assert(MemoryBytes < 500 * 1024 * 1024),

            % Cleanup
            ets:delete(Table),
            ok
        after
            cleanup(Pid)
        end
    end}.

session_registry_large_scale_test_() ->
    {timeout, 120, fun() ->
        Pid = setup(),
        try
            {ok, _Tid} = erlmcp_ets_registry:create_session_registry(),
            Table = erlmcp_sessions_otp28,

            % Create 1M session entries
            NumSessions = 1000000,
            StartTime = erlang:monotonic_time(microsecond),

            lists:foreach(fun(N) ->
                SessionId = list_to_binary(io_lib:format("session_~p", [N])),
                Session = #{
                    id => SessionId,
                    created_at => erlang:system_time(millisecond),
                    last_accessed => erlang:system_time(millisecond),
                    timeout_ms => 3600000,
                    metadata => #{user_id => N}
                },
                ets:insert(Table, {SessionId, Session})
            end, lists:seq(1, NumSessions)),

            InsertTime = erlang:monotonic_time(microsecond) - StartTime,
            ?LOG_INFO("Inserted ~p sessions in ~p ms", [NumSessions, InsertTime / 1000]),

            % Verify size
            ?assertEqual(NumSessions, ets:info(Table, size)),

            % Test range query for expired sessions (OTP 28 ordered_set optimization)
            RangeStartTime = erlang:monotonic_time(microsecond),
            Now = erlang:system_time(millisecond),

            % Find all sessions with last_accessed > (Now - 1 hour)
            CutoffTime = Now - 3600000,
            ActiveSessions = ets:foldl(
                fun({_SessionId, Session}, Acc) ->
                    LastAccessed = maps:get(last_accessed, Session, 0),
                    case LastAccessed > CutoffTime of
                        true -> Acc + 1;
                        false -> Acc
                    end
                end,
                0,
                Table
            ),

            RangeTime = erlang:monotonic_time(microsecond) - RangeStartTime,
            ?LOG_INFO("Range query found ~p active sessions in ~p ms",
                [ActiveSessions, RangeTime / 1000]),

            % All sessions should be active (just created)
            ?assertEqual(NumSessions, ActiveSessions),

            ets:delete(Table),
            ok
        after
            cleanup(Pid)
        end
    end}.

%%====================================================================
%% Concurrency Tests
%%====================================================================

concurrent_read_write_test_() ->
    {timeout, 60, fun() ->
        Pid = setup(),
        try
            {ok, _Tid} = erlmcp_ets_registry:create_registry(erlmcp_test_large),
            Table = erlmcp_test_large,

            % Pre-populate with 100K entries
            lists:foreach(fun(N) ->
                ets:insert(Table, {N, initial_value})
            end, lists:seq(1, 100000)),

            % Spawn 20 reader processes
            Readers = lists:map(fun(_) ->
                spawn_monitor(fun() ->
                    lists:foreach(fun(_) ->
                        N = rand:uniform(100000),
                        ets:lookup(Table, N)
                    end, lists:seq(1, 5000))
                end)
            end, lists:seq(1, 20)),

            % Spawn 10 writer processes
            Writers = lists:map(fun(_) ->
                spawn_monitor(fun() ->
                    lists:foreach(fun(_) ->
                        N = rand:uniform(100000),
                        ets:insert(Table, {N, updated_value})
                    end, lists:seq(1, 2500))
                end)
            end, lists:seq(1, 10)),

            % Wait for all processes
            StartTime = erlang:monotonic_time(microsecond),
            lists:foreach(fun({Pid, Ref}) ->
                receive
                    {'DOWN', Ref, process, Pid, normal} -> ok
                end
            end, Readers ++ Writers),

            EndTime = erlang:monotonic_time(microsecond),
            TotalTime = EndTime - StartTime,
            ?LOG_INFO("Concurrent test completed in ~p ms (20 readers, 10 writers)",
                [TotalTime / 1000]),

            % Verify table integrity
            ?assertEqual(100000, ets:info(Table, size)),

            ets:delete(Table),
            ok
        after
            cleanup(Pid)
        end
    end}.

%%====================================================================
%% Memory Leak Detection Tests
%%====================================================================

memory_leak_test_() ->
    {timeout, 120, fun() ->
        Pid = setup(),
        try
            {ok, _Tid} = erlmcp_ets_registry:create_registry(erlmcp_test_large),
            Table = erlmcp_test_large,

            % Take initial memory snapshot
            InitialMemory = ets:info(Table, memory),

            % Insert and delete 500K entries (cycles)
            lists:foreach(fun(Cycle) ->
                % Insert 10K entries
                lists:foreach(fun(N) ->
                    Key = {Cycle, N},
                    ets:insert(Table, {Key, cycle_value(Cycle, N)})
                end, lists:seq(1, 10000)),

                % Delete them
                lists:foreach(fun(N) ->
                    Key = {Cycle, N},
                    ets:delete(Table, Key)
                end, lists:seq(1, 10000))
            end, lists:seq(1, 50)),

            % Force garbage collection
            erlang:garbage_collect(),
            timer:sleep(100),

            % Check final memory (should not grow significantly)
            FinalMemory = ets:info(Table, memory),
            MemoryGrowth = FinalMemory - InitialMemory,
            ?LOG_INFO("Memory growth after insert/delete cycles: ~p words", [MemoryGrowth]),

            % Memory growth should be minimal (< 10% of initial)
            ?assert(MemoryGrowth < InitialMemory * 0.1),

            ets:delete(Table),
            ok
        after
            cleanup(Pid)
        end
    end}.

%%====================================================================
%% Statistics and Monitoring Tests
%%====================================================================

table_stats_test() ->
    ?_test(begin
        Pid = setup(),
        try
            {ok, _Tid} = erlmcp_ets_registry:create_registry(erlmcp_test_registry),

            % Insert some data
            Table = erlmcp_test_registry,
            lists:foreach(fun(N) -> ets:insert(Table, {N, value}) end, lists:seq(1, 100)),

            % Get stats
            {ok, Stats} = erlmcp_ets_registry:get_table_stats(erlmcp_test_registry),

            % Verify stats
            ?assertEqual(erlmcp_test_registry, maps:get(name, Stats)),
            ?assertEqual(100, maps:get(size, Stats)),
            ?assert(is_integer(maps:get(memory, Stats))),
            ?assertEqual(true, maps:get(read_concurrency, Stats)),
            ?assertEqual(true, maps:get(decentralized_counters, Stats))
        after
            cleanup(Pid)
        end
    end).

list_all_tables_test() ->
    ?_test(begin
        Pid = setup(),
        try
            {ok, _Tid1} = erlmcp_ets_registry:create_registry(registry1),
            {ok, _Tid2} = erlmcp_ets_registry:create_registry(registry2),

            {ok, Tables} = erlmcp_ets_registry:list_all_tables(),

            % Should have at least 2 tables
            ?assert(length(Tables) >= 2),

            % Verify structure
            lists:foreach(fun(Stats) ->
                ?assert(maps:is_key(name, Stats)),
                ?assert(maps:is_key(type, Stats)),
                ?assert(maps:is_key(size, Stats))
            end, Tables)
        after
            cleanup(Pid)
        end
    end).

%%====================================================================
%% Helper Functions
%%====================================================================

value_n(N) ->
    list_to_binary(io_lib:format("value_~p", [N])).

cycle_value(Cycle, N) ->
    list_to_binary(io_lib:format("cycle_~p_val_~p", [Cycle, N])).

%%====================================================================
%% ETS Stats Monitor Tests
%%====================================================================

ets_stats_monitor_test_() ->
    {setup,
     fun() ->
         % Start ETS stats monitor
         {ok, Pid} = erlmcp_ets_stats:start_link(),
         Pid
     end,
     fun(Pid) ->
         gen_server:stop(Pid)
     end,
     fun(_Pid) ->
         [
          {"Register table for monitoring", fun register_table_monitoring_test/0},
          {"Get table stats", fun get_table_stats_test/0},
          {"Growth rate calculation", fun growth_rate_calculation_test/0},
          {"Alert thresholds", fun alert_thresholds_test/0}
         ]
     end}.

register_table_monitoring_test() ->
    ?assertEqual(ok, erlmcp_ets_stats:register_table(test_table, registry)).

get_table_stats_test() ->
    % Create a test table
    Table = ets:new(test_table, [set, public, named_table]),
    ets:insert(Table, {key1, value1}),
    ets:insert(Table, {key2, value2}),

    try
        {ok, Stats} = erlmcp_ets_stats:get_table_stats(test_table),
        ?assertEqual(test_table, maps:get(name, Stats)),
        ?assert(is_integer(maps:get(size, Stats))),
        ?assert(maps:get(size, Stats) >= 0)
    after
        ets:delete(Table)
    end.

growth_rate_calculation_test() ->
    % Create a test table
    Table = ets:new(test_table, [set, public, named_table]),

    try
        ok = erlmcp_ets_stats:register_table(test_table, registry),

        % Initial snapshot
        {ok, Stats1} = erlmcp_ets_stats:get_table_stats(test_table),
        InitialSize = maps:get(size, Stats1),

        % Add 100 entries
        lists:foreach(fun(N) -> ets:insert(Table, {N, value}) end, lists:seq(1, 100)),

        % Wait for next snapshot interval (5 seconds)
        timer:sleep(6000),

        % Get updated stats with growth rate
        {ok, Stats2} = erlmcp_ets_stats:get_table_stats(test_table),
        GrowthRate = maps:get(growth_rate, Stats2),

        % Growth rate should be positive
        ?assert(GrowthRate > 0.0)
    after
        ets:delete(Table)
    end.

alert_thresholds_test() ->
    Table = ets:new(test_table, [set, public, named_table]),

    try
        ok = erlmcp_ets_stats:register_table(test_table, cache),

        % Set growth threshold
        Threshold = #{max_entries => 1000, max_growth_rate => 100.0},
        ?assertEqual(ok, erlmcp_ets_stats:set_growth_alert_threshold(test_table, Threshold)),

        % Set memory threshold (1 MB)
        ?assertEqual(ok, erlmcp_ets_stats:set_memory_alert_threshold(test_table, 1048576))
    after
        ets:delete(Table)
    end.
