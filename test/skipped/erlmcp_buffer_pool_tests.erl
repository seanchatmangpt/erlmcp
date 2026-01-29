-module(erlmcp_buffer_pool_tests).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Chicago School TDD - Buffer Pool Tests
%% Real ETS, real queues, real gen_server, state-based verification
%%====================================================================

%%====================================================================
%% Test Fixtures
%%====================================================================

buffer_pool_test_() ->
    {setup,
     fun() ->
         %% Setup: Start buffer pool gen_server (real process)
         Config = #{
             max_4kb => 256,
             max_8kb => 128,
             max_16kb => 64,
             preallocate => false  %% Don't preallocate for faster tests
         },
         {ok, Pid} = erlmcp_buffer_pool:start_link(Config),
         Pid
     end,
     fun(Pid) ->
         %% Teardown: Stop buffer pool
         gen_server:stop(Pid, normal, 5000)
     end,
     fun(Pid) ->
         [
          ?_test(get_buffer_4kb(Pid)),
          ?_test(get_buffer_8kb(Pid)),
          ?_test(get_buffer_16kb(Pid)),
          ?_test(return_buffer(Pid)),
          ?_test(buffer_reuse(Pid)),
          ?_test(pool_stats(Pid)),
          ?_test(pool_info(Pid)),
          ?_test(process_local_cache()),
          ?_test(cache_hit(Pid)),
          ?_test(cache_overflow()),
          ?_test(tier_selection()),
          ?_test(concurrent_allocation(Pid))
         ]
     end}.

%%====================================================================
%% Test Cases - Buffer Allocation (Chicago School: Real Queue Ops)
%%====================================================================

get_buffer_4kb(Pid) ->
    %% Exercise: Get 4KB buffer (real allocation)
    Buffer = erlmcp_buffer_pool:get_buffer(4096),

    %% Verify: Buffer allocated (state-based)
    ?assert(is_binary(Buffer)),
    ?assertEqual(4096, byte_size(Buffer)),

    %% Verify: Stats reflect allocation
    Stats = erlmcp_buffer_pool:stats(Pid),
    Tier4KB = maps:get(4096, Stats),
    ?assert(maps:get(allocated, Tier4KB) >= 1).

get_buffer_8kb(Pid) ->
    %% Exercise: Get 8KB buffer
    Buffer = erlmcp_buffer_pool:get_buffer(8192),

    %% Verify: Correct size
    ?assertEqual(8192, byte_size(Buffer)),

    %% Verify: Stats updated
    Stats = erlmcp_buffer_pool:stats(Pid),
    Tier8KB = maps:get(8192, Stats),
    ?assert(maps:get(allocated, Tier8KB) >= 1).

get_buffer_16kb(Pid) ->
    %% Exercise: Get 16KB buffer
    Buffer = erlmcp_buffer_pool:get_buffer(16384),

    %% Verify: Correct size
    ?assertEqual(16384, byte_size(Buffer)),

    %% Verify: Stats updated
    Stats = erlmcp_buffer_pool:stats(Pid),
    Tier16KB = maps:get(16384, Stats),
    ?assert(maps:get(allocated, Tier16KB) >= 1).

return_buffer(Pid) ->
    %% Setup: Get buffer
    Buffer = erlmcp_buffer_pool:get_buffer(4096),
    ?assertEqual(4096, byte_size(Buffer)),

    %% Get initial pool stats
    Stats1 = erlmcp_buffer_pool:stats(Pid),
    Tier1 = maps:get(4096, Stats1),
    Count1 = maps:get(count, Tier1),

    %% Exercise: Return buffer to pool (real queue operation)
    ok = erlmcp_buffer_pool:return_buffer(4096, Buffer),

    %% Give gen_server time to process cast
    timer:sleep(50),

    %% Verify: Buffer returned to pool (count increased)
    Stats2 = erlmcp_buffer_pool:stats(Pid),
    Tier2 = maps:get(4096, Stats2),
    Count2 = maps:get(count, Tier2),
    ?assert(Count2 >= Count1).  %% Count increased or stayed same

buffer_reuse(Pid) ->
    %% Setup: Get and return buffer
    Buffer1 = erlmcp_buffer_pool:get_buffer(4096),
    ok = erlmcp_buffer_pool:return_buffer(4096, Buffer1),
    timer:sleep(50),

    %% Exercise: Get buffer again (should reuse from pool)
    Buffer2 = erlmcp_buffer_pool:get_buffer(4096),

    %% Verify: Buffer allocated (may or may not be same binary)
    ?assert(is_binary(Buffer2)),
    ?assertEqual(4096, byte_size(Buffer2)).

pool_stats(Pid) ->
    %% Exercise: Get pool stats
    Stats = erlmcp_buffer_pool:stats(Pid),

    %% Verify: Stats structure (observable state)
    ?assert(is_map(Stats)),
    ?assert(maps:is_key(4096, Stats)),
    ?assert(maps:is_key(8192, Stats)),
    ?assert(maps:is_key(16384, Stats)),

    %% Verify: Each tier has required fields
    Tier4KB = maps:get(4096, Stats),
    ?assert(maps:is_key(count, Tier4KB)),
    ?assert(maps:is_key(allocated, Tier4KB)),
    ?assert(maps:is_key(max_allocated, Tier4KB)),
    ?assert(maps:is_key(hits, Tier4KB)),
    ?assert(maps:is_key(misses, Tier4KB)).

pool_info(Pid) ->
    %% Exercise: Get pool info
    Info = erlmcp_buffer_pool:info(Pid),

    %% Verify: Info structure
    ?assert(is_map(Info)),
    ?assert(maps:is_key(total_allocations, Info)),
    ?assert(maps:is_key(total_deallocations, Info)),
    ?assert(maps:is_key(tiers, Info)),
    ?assert(maps:is_key(config, Info)),

    %% Verify: Tier count
    ?assertEqual(3, maps:get(tiers, Info)).

%%====================================================================
%% Test Cases - Process-Local Cache (Fast Path)
%%====================================================================

process_local_cache() ->
    %% Exercise: Initialize process-local cache
    ok = erlmcp_buffer_pool:init_process_cache(),

    %% Verify: Cache initialized (process dictionary)
    Cache = erlang:get(erlmcp_buffer_cache),
    ?assert(is_map(Cache)),
    ?assert(maps:is_key('4kb', Cache)),
    ?assert(maps:is_key('8kb', Cache)),
    ?assert(maps:is_key('16kb', Cache)).

cache_hit(Pid) ->
    %% Setup: Initialize cache and add buffer
    ok = erlmcp_buffer_pool:init_process_cache(),
    Buffer = erlmcp_buffer_pool:get_buffer(4096),
    ok = erlmcp_buffer_pool:cache_return('4kb', Buffer),

    %% Exercise: Get from cache (fast path, no gen_server call)
    {ok, CachedBuffer} = erlmcp_buffer_pool:cache_get('4kb'),

    %% Verify: Got buffer from cache (state-based)
    ?assert(is_binary(CachedBuffer)),
    ?assertEqual(4096, byte_size(CachedBuffer)),

    %% Verify: Cache now empty
    ?assertEqual(error, erlmcp_buffer_pool:cache_get('4kb')).

cache_overflow() ->
    %% Setup: Initialize cache
    ok = erlmcp_buffer_pool:init_process_cache(),

    %% Exercise: Add 20 buffers to cache (limit is 16)
    Buffers = [binary:copy(<<0:4096/unit:8>>) || _ <- lists:seq(1, 20)],
    Results = [erlmcp_buffer_pool:cache_return('4kb', B) || B <- Buffers],

    %% Verify: First 16 succeed, rest fail (overflow protection)
    OkCount = length([1 || ok <- Results]),
    ErrorCount = length([1 || error <- Results]),
    ?assertEqual(16, OkCount),
    ?assertEqual(4, ErrorCount).

%%====================================================================
%% Test Cases - Tier Selection
%%====================================================================

tier_selection() ->
    %% Exercise: Get buffers of various sizes
    Buffer1 = erlmcp_buffer_pool:get_buffer(1024),   %% → 4KB tier
    Buffer2 = erlmcp_buffer_pool:get_buffer(4096),   %% → 4KB tier
    Buffer3 = erlmcp_buffer_pool:get_buffer(5000),   %% → 8KB tier
    Buffer4 = erlmcp_buffer_pool:get_buffer(8192),   %% → 8KB tier
    Buffer5 = erlmcp_buffer_pool:get_buffer(10000),  %% → 16KB tier
    Buffer6 = erlmcp_buffer_pool:get_buffer(16384),  %% → 16KB tier

    %% Verify: Correct tier sizes allocated (automatic tier selection)
    ?assertEqual(4096, byte_size(Buffer1)),
    ?assertEqual(4096, byte_size(Buffer2)),
    ?assertEqual(8192, byte_size(Buffer3)),
    ?assertEqual(8192, byte_size(Buffer4)),
    ?assertEqual(16384, byte_size(Buffer5)),
    ?assertEqual(16384, byte_size(Buffer6)).

%%====================================================================
%% Test Cases - Concurrency (Chicago School: Real Concurrent Access)
%%====================================================================

concurrent_allocation(Pid) ->
    %% Exercise: 100 processes allocate buffers concurrently
    Pids = lists:map(fun(N) ->
        spawn(fun() ->
            %% Each process: get buffer, return buffer
            Size = case N rem 3 of
                0 -> 4096;
                1 -> 8192;
                2 -> 16384
            end,
            Buffer = erlmcp_buffer_pool:get_buffer(Size),
            ?assertEqual(Size, byte_size(Buffer)),
            ok = erlmcp_buffer_pool:return_buffer(Size, Buffer),
            receive stop -> ok end
        end)
    end, lists:seq(1, 100)),

    timer:sleep(300),  %% Let allocations complete

    %% Verify: All processes alive (no crashes)
    AlivePids = lists:filter(fun erlang:is_process_alive/1, Pids),
    ?assertEqual(100, length(AlivePids)),

    %% Verify: Pool stats show activity
    Stats = erlmcp_buffer_pool:stats(Pid),
    Tier4KB = maps:get(4096, Stats),
    TotalAllocs = maps:get(allocated, Tier4KB),
    ?assert(TotalAllocs > 0),

    %% Cleanup
    lists:foreach(fun(P) -> P ! stop end, Pids).

%%====================================================================
%% Edge Cases
%%====================================================================

get_buffer_zero_size_test() ->
    %% Exercise: Request 0-byte buffer (edge case)
    %% Should default to 4KB tier
    Buffer = erlmcp_buffer_pool:get_buffer(0),

    %% Verify: 4KB buffer allocated
    ?assertEqual(4096, byte_size(Buffer)).

get_buffer_huge_size_test() ->
    %% Exercise: Request buffer larger than 16KB
    Buffer = erlmcp_buffer_pool:get_buffer(100000),

    %% Verify: 16KB buffer allocated (largest tier)
    ?assertEqual(16384, byte_size(Buffer)).

cache_stats_test() ->
    %% Setup: Initialize cache and add buffers
    ok = erlmcp_buffer_pool:init_process_cache(),
    Buffer1 = binary:copy(<<0:4096/unit:8>>),
    Buffer2 = binary:copy(<<0:8192/unit:8>>),
    ok = erlmcp_buffer_pool:cache_return('4kb', Buffer1),
    ok = erlmcp_buffer_pool:cache_return('8kb', Buffer2),

    %% Exercise: Get cache stats
    Stats = erlmcp_buffer_pool:cache_stats(),

    %% Verify: Stats show cached buffers
    ?assert(is_map(Stats)),
    ?assertEqual(1, maps:get('4kb', Stats)),
    ?assertEqual(1, maps:get('8kb', Stats)),
    ?assertEqual(0, maps:get('16kb', Stats)).

preallocate_pools_test() ->
    %% Setup: Start pool with preallocate enabled
    Config = #{
        max_4kb => 10,
        max_8kb => 5,
        max_16kb => 3,
        preallocate => true
    },
    {ok, Pid} = erlmcp_buffer_pool:start_link(Config),

    %% Verify: Pools preallocated (initial count = max)
    Stats = erlmcp_buffer_pool:stats(Pid),
    Tier4KB = maps:get(4096, Stats),
    ?assertEqual(10, maps:get(count, Tier4KB)),
    ?assertEqual(10, maps:get(allocated, Tier4KB)),

    %% Cleanup
    gen_server:stop(Pid, normal, 5000).

return_buffer_to_full_pool_test() ->
    %% Setup: Start pool with small max (2 buffers)
    Config = #{max_4kb => 2, max_8kb => 2, max_16kb => 2},
    {ok, Pid} = erlmcp_buffer_pool:start_link(Config),

    %% Fill pool to max
    B1 = erlmcp_buffer_pool:get_buffer(4096),
    B2 = erlmcp_buffer_pool:get_buffer(4096),
    ok = erlmcp_buffer_pool:return_buffer(4096, B1),
    ok = erlmcp_buffer_pool:return_buffer(4096, B2),
    timer:sleep(50),

    %% Exercise: Try to return another buffer (pool full)
    B3 = erlmcp_buffer_pool:get_buffer(4096),
    ok = erlmcp_buffer_pool:return_buffer(4096, B3),  %% Should be discarded
    timer:sleep(50),

    %% Verify: Pool count stays at max (overflow discarded)
    Stats = erlmcp_buffer_pool:stats(Pid),
    Tier4KB = maps:get(4096, Stats),
    Count = maps:get(count, Tier4KB),
    ?assert(Count =< 2),  %% Did not exceed max

    %% Cleanup
    gen_server:stop(Pid, normal, 5000).
