%%%-----------------------------------------------------------------------------
%%% @doc Tests for erlmcp_memory_analyzer
%%% @end
%%%-----------------------------------------------------------------------------
-module(erlmcp_memory_analyzer_tests).

-include_lib("eunit/include/eunit.hrl").

%%%=============================================================================
%%% TEST FIXTURES
%%%=============================================================================

memory_analyzer_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
      {"Full analysis", fun test_analyze/0},
      {"Top processes", fun test_top_processes/0},
      {"ETS tables analysis", fun test_ets_tables/0},
      {"Leak detection", fun test_detect_leaks/0},
      {"Heap analysis", fun test_heap_analysis/0}
     ]}.

setup() ->
    application:ensure_all_started(erlmcp_observability),
    
    %% Create test ETS table
    Tab = ets:new(test_table, [public, named_table]),
    [ets:insert(Tab, {N, N * 2}) || N <- lists:seq(1, 1000)],
    
    Tab.

cleanup(Tab) ->
    catch ets:delete(Tab),
    ok.

%%%=============================================================================
%%% ANALYSIS TESTS
%%%=============================================================================

test_analyze() ->
    {ok, Analysis} = erlmcp_memory_analyzer:analyze(#{
        top => 5,
        include_ets => true,
        include_binaries => true
    }),
    
    ?assertMatch(#{
        timestamp := _,
        system_memory := _,
        top_processes := _,
        total_processes := _,
        ets_tables := _,
        binary_leaks := _
    }, Analysis),
    
    %% Verify system memory structure
    SystemMem = maps:get(system_memory, Analysis),
    ?assertMatch(#{
        total := _,
        processes := _,
        system := _,
        binary := _,
        ets := _
    }, SystemMem),
    
    %% Verify top processes
    TopProcs = maps:get(top_processes, Analysis),
    ?assert(is_list(TopProcs)),
    ?assert(length(TopProcs) =< 5).

test_top_processes() ->
    TopProcs = erlmcp_memory_analyzer:top_processes(10),
    
    ?assert(is_list(TopProcs)),
    ?assert(length(TopProcs) =< 10),
    
    %% Verify each process entry
    lists:foreach(fun(ProcInfo) ->
        ?assertMatch(#{
            pid := _,
            memory_bytes := _,
            memory_mb := _,
            message_queue_len := _,
            reductions := _
        }, ProcInfo)
    end, TopProcs).

test_ets_tables() ->
    %% Get all tables
    AllTables = erlmcp_memory_analyzer:ets_tables(all),
    
    ?assert(is_list(AllTables)),
    ?assert(length(AllTables) > 0),
    
    %% Verify our test table is included
    TestTable = lists:filter(
        fun(#{name := Name}) -> Name == test_table end,
        AllTables
    ),
    ?assertEqual(1, length(TestTable)),
    
    [TableInfo] = TestTable,
    ?assertMatch(#{
        table := _,
        name := test_table,
        size := 1000,
        memory_bytes := _,
        type := _,
        owner := _
    }, TableInfo),
    
    %% Get top 5
    Top5 = erlmcp_memory_analyzer:ets_tables(5),
    ?assert(length(Top5) =< 5).

test_detect_leaks() ->
    LeakInfo = erlmcp_memory_analyzer:detect_leaks(),
    
    ?assertMatch(#{
        binary_leaks := _,
        long_message_queues := _,
        large_ets_tables := _,
        leak_score := _
    }, LeakInfo),
    
    %% Verify leak score is a number between 0 and 100
    LeakScore = maps:get(leak_score, LeakInfo),
    ?assert(is_float(LeakScore)),
    ?assert(LeakScore >= 0.0),
    ?assert(LeakScore =< 100.0).

test_heap_analysis() ->
    {ok, HeapAnalysis} = erlmcp_memory_analyzer:heap_analysis(#{
        threshold => 20.0
    }),
    
    ?assertMatch(#{
        threshold_pct := 20.0,
        fragmented_processes := _,
        count := _
    }, HeapAnalysis),
    
    %% Verify fragmented processes structure
    Fragmented = maps:get(fragmented_processes, HeapAnalysis),
    ?assert(is_list(Fragmented)),
    
    %% Each should have fragmentation >= 20%
    lists:foreach(fun(ProcInfo) ->
        ?assertMatch(#{
            pid := _,
            fragmentation_pct := Frag,
            heap_size_words := _,
            total_heap_size_words := _
        }, ProcInfo),
        
        ?assert(maps:get(fragmentation_pct, ProcInfo) >= 20.0)
    end, Fragmented).
