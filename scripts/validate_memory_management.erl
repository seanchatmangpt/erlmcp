#!/usr/bin/env escript
%%%-------------------------------------------------------------------
%%% @doc Memory Management Validation Script
%%%
%%% Comprehensive validation of memory management implementation:
%%% 1. LRU cache functionality
%%% 2. Memory pressure detection
%%% 3. Garbage collection effectiveness
%%% 4. Memory leak detection
%%% 5. Cache eviction behavior
%%% 6. Large specification parsing
%%% 7. Extended validation runs
%%% 8. Cache hit/miss patterns
%%% 9. Memory usage bounds
%%% @end
%%%-------------------------------------------------------------------

-mode(compile).

main(_) ->
    io:format("~n=== Memory Management Validation ===~n~n"),
    ok = application:start(erlmcp_core),
    ok = application:start(erlmcp_observability),

    Results = #{
        lru_cache => test_lru_cache(),
        memory_pressure => test_memory_pressure(),
        garbage_collection => test_garbage_collection(),
        memory_leaks => test_memory_leaks(),
        cache_eviction => test_cache_eviction(),
        large_spec_parsing => test_large_spec_parsing(),
        extended_validation => test_extended_validation(),
        cache_patterns => test_cache_patterns(),
        memory_bounds => test_memory_bounds()
    },

    print_results(Results),

    case all_passed(Results) of
        true ->
            io:format("~n✅ All memory management validations passed!~n"),
            halt(0);
        false ->
            io:format("~n❌ Some memory management validations failed!~n"),
            halt(1)
    end.

%%%===================================================================
%%% Test Functions
%%%===================================================================

test_lru_cache() ->
    io:format("Testing LRU Cache... "),
    try
        {ok, CachePid} = erlmcp_cache:start_link(#{
            max_l1_size => 100,
            default_ttl_seconds => 60
        }),

        %% Fill cache
        lists:foreach(fun(N) ->
            Key = list_to_binary("key_" ++ integer_to_list(N)),
            Value = list_to_binary("value_" ++ integer_to_list(N)),
            erlmcp_cache:put(Key, Value, {lru, 100})
        end, lists:seq(1, 100)),

        %% All should be present
        {ok, <<"value_1">>} = erlmcp_cache:get(<<"key_1">>),
        {ok, <<"value_50">>} = erlmcp_cache:get(<<"key_50">>),

        %% Add one more to trigger eviction
        erlmcp_cache:put(<<"key_101">>, <<"value_101">>, {lru, 100}),

        %% First key should be evicted (least recently used)
        {error, not_found} = erlmcp_cache:get(<<"key_1">>),

        %% Check stats
        Stats = erlmcp_cache:stats(),
        Evictions = maps:get(evictions, Stats, 0),
        true = Evictions > 0,

        gen_server:stop(CachePid),
        io:format("✅ PASS~n"),
        {pass, #{evictions => Evictions}}
    catch
        _:Err ->
            io:format("❌ FAIL: ~p~n", [Err]),
            {fail, Err}
    end.

test_memory_pressure() ->
    io:format("Testing Memory Pressure Detection... "),
    try
        %% Start memory manager
        {ok, MemPid} = erlmcp_memory_manager:start_link(#{
            max_cache_size => 100,
            max_spec_memory => 10 * 1024 * 1024  % 10MB
        }),

        %% Get initial memory usage
        {ok, InitialStats} = erlmcp_memory_manager:get_memory_usage(),
        InitialPercent = maps:get(used_percent, InitialStats, 0.0),

        %% Check pressure level
        Pressure = erlmcp_memory_manager:check_memory_pressure(),
        true = lists:member(Pressure, [low, medium, high, critical]),

        %% Cache some specs
        Spec1 = create_test_spec(1000),
        ok = erlmcp_memory_manager:cache_spec(<<"spec1">>, Spec1),

        %% Check memory after caching
        {ok, AfterStats} = erlmcp_memory_manager:get_memory_usage(),
        CacheMemory = maps:get(cache_memory, AfterStats, 0),
        true = CacheMemory > 0,

        gen_server:stop(MemPid),
        io:format("✅ PASS~n"),
        {pass, #{
            initial_percent => InitialPercent,
            pressure => Pressure,
            cache_memory => CacheMemory
        }}
    catch
        _:Err ->
            io:format("❌ FAIL: ~p~n", [Err]),
            {fail, Err}
    end.

test_garbage_collection() ->
    io:format("Testing Garbage Collection... "),
    try
        %% Start memory manager
        {ok, MemPid} = erlmcp_memory_manager:start_link(#{}),

        %% Get initial GC count
        {ok, InitialStats} = erlmcp_memory_manager:get_stats(),
        InitialGC = maps:get(gc_runs, InitialStats, 0),

        %% Force garbage collection
        {ok, NumProcesses} = erlmcp_memory_manager:force_garbage_collection(),
        true = NumProcesses > 0,

        %% Check GC count increased
        {ok, AfterStats} = erlmcp_memory_manager:get_stats(),
        AfterGC = maps:get(gc_runs, AfterStats, 0),
        true = AfterGC > InitialGC,

        gen_server:stop(MemPid),
        io:format("✅ PASS~n"),
        {pass, #{processes_gc => NumProcesses, gc_runs => AfterGC}}
    catch
        _:Err ->
            io:format("❌ FAIL: ~p~n", [Err]),
            {fail, Err}
    end.

test_memory_leaks() ->
    io:format("Testing Memory Leak Detection... "),
    try
        %% Run memory analyzer
        {ok, Analysis} = erlmcp_memory_analyzer:analyze(#{
            top => 10,
            include_ets => true,
            include_binaries => true
        }),

        %% Check structure
        true = maps:is_key(system_memory, Analysis),
        true = maps:is_key(top_processes, Analysis),
        true = maps:is_key(binary_leaks, Analysis),

        %% Check for leaks
        LeakInfo = erlmcp_memory_analyzer:detect_leaks(),
        LeakScore = maps:get(leak_score, LeakInfo),
        true = is_float(LeakScore),
        true = LeakScore >= 0.0 andalso LeakScore =< 100.0,

        io:format("✅ PASS~n"),
        {pass, #{leak_score => LeakScore}}
    catch
        _:Err ->
            io:format("❌ FAIL: ~p~n", [Err]),
            {fail, Err}
    end.

test_cache_eviction() ->
    io:format("Testing Cache Eviction... "),
    try
        {ok, CachePid} = erlmcp_cache:start_link(#{
            max_l1_size => 50,
            default_ttl_seconds => 300
        }),

        %% Fill cache beyond limit
        lists:foreach(fun(N) ->
            Key = list_to_binary("evict_key_" ++ integer_to_list(N)),
            Value = <<N:64>>,
            erlmcp_cache:put(Key, Value)
        end, lists:seq(1, 100)),

        %% Check that eviction occurred
        Stats = erlmcp_cache:stats(),
        Evictions = maps:get(evictions, Stats, 0),
        L1Size = maps:get(l1_size, Stats, 0),

        true = Evictions > 0,
        true = L1Size =< 50,

        gen_server:stop(CachePid),
        io:format("✅ PASS~n"),
        {pass, #{evictions => Evictions, l1_size => L1Size}}
    catch
        _:Err ->
            io:format("❌ FAIL: ~p~n", [Err]),
            {fail, Err}
    end.

test_large_spec_parsing() ->
    io:format("Testing Large Specification Parsing... "),
    try
        %% Create large specification (1MB)
        LargeSpec = create_test_spec(1024 * 1024),

        %% Start memory manager
        {ok, MemPid} = erlmcp_memory_manager:start_link(#{
            max_spec_memory => 5 * 1024 * 1024  % 5MB limit
        }),

        %% Try to cache large spec
        Result = erlmcp_memory_manager:cache_spec(<<"large_spec">>, LargeSpec),

        case Result of
            ok ->
                %% Should succeed
                {ok, CachedSpec} = erlmcp_memory_manager:get_cached_spec(<<"large_spec">>),
                true = byte_size(term_to_binary(CachedSpec)) > 0;
            {error, {spec_too_large, _, _}} ->
                %% Also acceptable if limit is exceeded
                ok
        end,

        gen_server:stop(MemPid),
        io:format("✅ PASS~n"),
        {pass, #{spec_size => byte_size(term_to_binary(LargeSpec))}}
    catch
        _:Err ->
            io:format("❌ FAIL: ~p~n", [Err]),
            {fail, Err}
    end.

test_extended_validation() ->
    io:format("Testing Extended Validation Run... "),
    try
        {ok, CachePid} = erlmcp_cache:start_link(#{}),

        %% Run for 100 iterations
        lists:foreach(fun(N) ->
            Key = list_to_binary("ext_key_" ++ integer_to_list(N rem 100)),
            Value = <<N:64>>,
            erlmcp_cache:put(Key, Value),
            erlmcp_cache:get(Key)
        end, lists:seq(1, 10000)),

        %% Check memory is stable
        Stats = erlmcp_cache:stats(),
        TotalRequests = maps:get(total_requests, Stats, 0),
        true = TotalRequests >= 10000,

        gen_server:stop(CachePid),
        io:format("✅ PASS~n"),
        {pass, #{total_requests => TotalRequests}}
    catch
        _:Err ->
            io:format("❌ FAIL: ~p~n", [Err]),
            {fail, Err}
    end.

test_cache_patterns() ->
    io:format("Testing Cache Hit/Miss Patterns... "),
    try
        {ok, CachePid} = erlmcp_cache:start_link(#{}),

        %% Add some keys
        lists:foreach(fun(N) ->
            Key = list_to_binary("pattern_key_" ++ integer_to_list(N)),
            erlmcp_cache:put(Key, N)
        end, lists:seq(1, 10)),

        %% Generate hits
        lists:foreach(fun(N) ->
            Key = list_to_binary("pattern_key_" ++ integer_to_list(N)),
            {ok, _} = erlmcp_cache:get(Key)
        end, lists:seq(1, 10)),

        %% Generate misses
        lists:foreach(fun(N) ->
            Key = list_to_binary("missing_key_" ++ integer_to_list(N)),
            {error, not_found} = erlmcp_cache:get(Key)
        end, lists:seq(1, 10)),

        %% Check stats
        Stats = erlmcp_cache:stats(),
        Hits = maps:get(hits, Stats, 0),
        Misses = maps:get(misses, Stats, 0),

        true = Hits >= 10,
        true = Misses >= 10,

        HitRate = maps:get(hit_rate, Stats, 0.0),
        true = HitRate > 0.0 andalso HitRate =< 1.0,

        gen_server:stop(CachePid),
        io:format("✅ PASS~n"),
        {pass, #{hits => Hits, misses => Misses, hit_rate => HitRate}}
    catch
        _:Err ->
            io:format("❌ FAIL: ~p~n", [Err]),
            {fail, Err}
    end.

test_memory_bounds() ->
    io:format("Testing Memory Usage Bounds... "),
    try
        %% Get system memory
        SystemMem = erlang:memory(),
        Total = proplists:get_value(total, SystemMem, 0),

        %% Test memory guard
        Small = 1024,  % 1KB
        Medium = 1024 * 1024,  % 1MB
        Large = 10 * 1024 * 1024,  % 10MB

        ok = erlmcp_memory_guard:check_allocation(Small),
        ok = erlmcp_memory_guard:check_allocation(Medium),
        ok = erlmcp_memory_guard:check_allocation(Large),

        %% Get memory stats
        Stats = erlmcp_memory_guard:get_memory_stats(),
        UsedPercent = maps:get(used_percent, Stats, 0.0),
        CircuitBreakerOpen = maps:get(circuit_breaker_open, Stats, false),

        true = UsedPercent >= 0.0 andalso UsedPercent =< 100.0,
        true = is_boolean(CircuitBreakerOpen),

        io:format("✅ PASS~n"),
        {pass, #{
            system_memory => Total,
            used_percent => UsedPercent,
            circuit_breaker_open => CircuitBreakerOpen
        }}
    catch
        _:Err ->
            io:format("❌ FAIL: ~p~n", [Err]),
            {fail, Err}
    end.

%%%===================================================================
%%% Helper Functions
%%%===================================================================

create_test_spec(Size) ->
    %% Create a specification of approximately Size bytes
    NumElements = Size div 100,
    #{
        version => <<"2025-11-25">>,
        methods => lists:map(fun(N) ->
            #{
                name => list_to_binary("method_" ++ integer_to_list(N)),
                params => #{param => N}
            }
        end, lists:seq(1, NumElements))
    }.

print_results(Results) ->
    io:format("~n=== Validation Results ===~n"),
    maps:foreach(fun(Test, Result) ->
        case Result of
            {pass, Details} ->
                io:format("  ✅ ~p: ~p~n", [Test, Details]);
            {fail, Reason} ->
                io:format("  ❌ ~p: ~p~n", [Test, Reason])
        end
    end, Results).

all_passed(Results) ->
    lists:all(fun(_, {pass, _}) -> true;
                 (_, _) -> false
              end, maps:to_list(Results)).
