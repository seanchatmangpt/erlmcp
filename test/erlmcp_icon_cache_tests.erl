-module(erlmcp_icon_cache_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Suite for erlmcp_icon_cache module
%% Gap #37: Icon Metadata Caching with TTL Enforcement
%%====================================================================

%%====================================================================
%% Setup and Teardown
%%====================================================================

setup_cache() ->
    %% Start the cache process
    case erlmcp_icon_cache:start_link() of
        {ok, Pid} -> Pid;
        {error, {already_started, Pid}} -> Pid
    end.

teardown_cache(_Pid) ->
    %% Stop the cache
    case catch gen_server:stop(erlmcp_icon_cache) of
        ok -> ok;
        _ -> ok
    end.

%%====================================================================
%% Test Suite Definition
%%====================================================================

icon_cache_test_() ->
    {setup,
     fun setup_cache/0,
     fun teardown_cache/1,
     [
         ?_test(test_cache_basic_operations()),
         ?_test(test_cache_expiration()),
         ?_test(test_cache_invalidation()),
         ?_test(test_cache_hits_and_misses()),
         ?_test(test_cache_ttl_configuration()),
         ?_test(test_cache_stats_tracking()),
         ?_test(test_cache_cleanup()),
         ?_test(test_cache_custom_ttl()),
         ?_test(test_cache_metadata_preservation()),
         ?_test(test_cache_concurrent_access()),
         ?_test(test_cache_invalid_inputs()),
         ?_test(test_cache_invalidate_all()),
         ?_test(test_cache_get_expired_entry()),
         ?_test(test_cache_multiple_icons()),
         ?_test(test_cache_ttl_enforcement())
     ]}.

%%====================================================================
%% Test Cases
%%====================================================================

%% Test 1: Basic cache operations (store and retrieve)
test_cache_basic_operations() ->
    Uri = <<"https://example.com/icon.png">>,
    Metadata = #{
        <<"size">> => 2048,
        <<"mime_type">> => <<"image/png">>,
        <<"width">> => 128,
        <<"height">> => 128
    },

    %% Store in cache
    ok = erlmcp_icon_cache:cache_icon(Uri, Metadata, 3600000),

    %% Retrieve from cache
    {ok, Retrieved} = erlmcp_icon_cache:get_cached_icon(Uri),

    %% Verify metadata matches
    ?assertEqual(Metadata, Retrieved).

%% Test 2: Cache expiration
test_cache_expiration() ->
    Uri = <<"https://example.com/expiring.png">>,
    Metadata = #{<<"data">> => <<"test">>},
    ShortTtl = 100,  %% 100ms TTL

    %% Cache with short TTL
    CachedAt = erlang:monotonic_time(millisecond),
    ok = erlmcp_icon_cache:cache_icon(Uri, Metadata, ShortTtl, CachedAt),

    %% Should be retrievable immediately
    {ok, _} = erlmcp_icon_cache:get_cached_icon(Uri),

    %% Wait for expiration
    timer:sleep(150),

    %% Should return expired result
    {expired, Retrieved} = erlmcp_icon_cache:get_cached_icon(Uri),
    ?assertEqual(Metadata, Retrieved).

%% Test 3: Icon invalidation
test_cache_invalidation() ->
    Uri = <<"https://example.com/invalidate.png">>,
    Metadata = #{<<"test">> => true},

    %% Cache the icon
    ok = erlmcp_icon_cache:cache_icon(Uri, Metadata, 3600000),
    {ok, _} = erlmcp_icon_cache:get_cached_icon(Uri),

    %% Invalidate the icon
    ok = erlmcp_icon_cache:invalidate_icon(Uri),

    %% Should be gone
    not_found = erlmcp_icon_cache:get_cached_icon(Uri).

%% Test 4: Cache hits and misses statistics
test_cache_hits_and_misses() ->
    Uri = <<"https://example.com/hits.png">>,
    Metadata = #{<<"test">> => true},

    %% Get initial stats
    Stats1 = erlmcp_icon_cache:get_cache_stats(),
    Misses1 = maps:get(misses, Stats1, 0),
    Hits1 = maps:get(hits, Stats1, 0),

    %% Miss on non-existent icon
    not_found = erlmcp_icon_cache:get_cached_icon(Uri),
    Stats2 = erlmcp_icon_cache:get_cache_stats(),
    Misses2 = maps:get(misses, Stats2, 0),

    %% Should increment misses
    ?assert(Misses2 > Misses1),

    %% Cache the icon
    ok = erlmcp_icon_cache:cache_icon(Uri, Metadata, 3600000),

    %% Hit on cached icon
    {ok, _} = erlmcp_icon_cache:get_cached_icon(Uri),
    Stats3 = erlmcp_icon_cache:get_cache_stats(),
    Hits2 = maps:get(hits, Stats3, 0),

    %% Should increment hits
    ?assert(Hits2 > Hits1).

%% Test 5: TTL configuration
test_cache_ttl_configuration() ->
    NewTtl = 5000,

    %% Set new TTL
    ok = erlmcp_icon_cache:set_ttl(NewTtl),

    %% Verify stats reflect new TTL
    Stats = erlmcp_icon_cache:get_cache_stats(),
    CurrentTtl = maps:get(current_ttl, Stats),

    ?assertEqual(NewTtl, CurrentTtl).

%% Test 6: Statistics tracking
test_cache_stats_tracking() ->
    Stats = erlmcp_icon_cache:get_cache_stats(),

    %% All expected fields should be present
    ?assert(maps:is_key(hits, Stats)),
    ?assert(maps:is_key(misses, Stats)),
    ?assert(maps:is_key(expirations, Stats)),
    ?assert(maps:is_key(invalidations, Stats)),
    ?assert(maps:is_key(cache_size, Stats)),
    ?assert(maps:is_key(current_ttl, Stats)),

    %% All stats should be non-negative
    ?assert(maps:get(hits, Stats) >= 0),
    ?assert(maps:get(misses, Stats) >= 0),
    ?assert(maps:get(expirations, Stats) >= 0),
    ?assert(maps:get(invalidations, Stats) >= 0).

%% Test 7: Cache cleanup
test_cache_cleanup() ->
    %% Cache several icons
    Uri1 = <<"https://example.com/cleanup1.png">>,
    Uri2 = <<"https://example.com/cleanup2.png">>,
    Metadata = #{<<"test">> => true},
    ShortTtl = 100,

    CachedAt = erlang:monotonic_time(millisecond),
    ok = erlmcp_icon_cache:cache_icon(Uri1, Metadata, ShortTtl, CachedAt),
    ok = erlmcp_icon_cache:cache_icon(Uri2, Metadata, 3600000),

    %% Wait for one to expire
    timer:sleep(150),

    %% Cache the expired entry's count in stats
    StatsAfterExpiry = erlmcp_icon_cache:get_cache_stats(),
    SizeAfterExpiry = maps:get(cache_size, StatsAfterExpiry),

    %% The expired entry should be removed during cleanup
    %% (or at least verified on next access)
    {expired, _} = erlmcp_icon_cache:get_cached_icon(Uri1),

    %% After accessing expired entry, it should be removed
    StatsAfterCleanup = erlmcp_icon_cache:get_cache_stats(),
    SizeAfterCleanup = maps:get(cache_size, StatsAfterCleanup),

    ?assert(SizeAfterCleanup =< SizeAfterExpiry).

%% Test 8: Custom TTL per cache entry
test_cache_custom_ttl() ->
    Uri1 = <<"https://example.com/custom1.png">>,
    Uri2 = <<"https://example.com/custom2.png">>,
    Metadata = #{<<"test">> => true},

    LongTtl = 10000,   %% 10 seconds
    ShortTtl = 200,    %% 200 milliseconds

    CachedAt = erlang:monotonic_time(millisecond),

    %% Cache both with different TTLs
    ok = erlmcp_icon_cache:cache_icon(Uri1, Metadata, LongTtl, CachedAt),
    ok = erlmcp_icon_cache:cache_icon(Uri2, Metadata, ShortTtl, CachedAt),

    %% Both should be retrievable immediately
    {ok, _} = erlmcp_icon_cache:get_cached_icon(Uri1),
    {ok, _} = erlmcp_icon_cache:get_cached_icon(Uri2),

    %% Wait for short TTL to expire
    timer:sleep(250),

    %% Long TTL should still be valid
    {ok, _} = erlmcp_icon_cache:get_cached_icon(Uri1),

    %% Short TTL should be expired
    {expired, _} = erlmcp_icon_cache:get_cached_icon(Uri2).

%% Test 9: Metadata preservation
test_cache_metadata_preservation() ->
    Uri = <<"https://example.com/preserve.png">>,

    %% Complex metadata
    Metadata = #{
        <<"uri">> => Uri,
        <<"size">> => 4096,
        <<"mime_type">> => <<"image/png">>,
        <<"width">> => 256,
        <<"height">> => 256,
        <<"hash">> => <<"abc123def456">>,
        <<"checksum">> => <<"xyz789">>,
        <<"format">> => <<"PNG">>,
        <<"color_space">> => <<"RGBA">>,
        <<"nested">> => #{
            <<"key">> => <<"value">>,
            <<"number">> => 42
        },
        <<"list">> => [1, 2, 3, 4, 5]
    },

    ok = erlmcp_icon_cache:cache_icon(Uri, Metadata, 3600000),
    {ok, Retrieved} = erlmcp_icon_cache:get_cached_icon(Uri),

    %% Verify exact match
    ?assertEqual(Metadata, Retrieved),

    %% Verify nested structures preserved
    ?assertEqual(42, maps:get(<<"number">>, maps:get(<<"nested">>, Retrieved))),
    ?assertEqual([1, 2, 3, 4, 5], maps:get(<<"list">>, Retrieved)).

%% Test 10: Concurrent access patterns
test_cache_concurrent_access() ->
    Uri = <<"https://example.com/concurrent.png">>,
    Metadata = #{<<"data">> => <<"shared">>},

    %% Spawn multiple processes accessing cache
    Parent = self(),

    Spawner = fun(N) ->
        spawn(fun() ->
            case N rem 3 of
                0 ->
                    %% Cache operation
                    erlmcp_icon_cache:cache_icon(
                        iolist_to_binary([Uri, "_", integer_to_binary(N)]),
                        Metadata,
                        3600000
                    ),
                    Parent ! {ok, N};
                1 ->
                    %% Read operation
                    erlmcp_icon_cache:get_cached_icon(Uri),
                    Parent ! {ok, N};
                2 ->
                    %% Stats operation
                    erlmcp_icon_cache:get_cache_stats(),
                    Parent ! {ok, N}
            end
        end)
    end,

    %% Spawn 10 concurrent operations
    lists:foreach(Spawner, lists:seq(1, 10)),

    %% Wait for all to complete
    lists:foreach(fun(_) ->
        receive
            {ok, _} -> ok
        after 5000 -> error(timeout)
        end
    end, lists:seq(1, 10)).

%% Test 11: Invalid input handling
test_cache_invalid_inputs() ->
    %% These should not crash
    catch erlmcp_icon_cache:cache_icon(<<"valid">>, #{}, -100),
    catch erlmcp_icon_cache:cache_icon(<<"valid">>, undefined, 1000),
    catch erlmcp_icon_cache:get_cached_icon(123),

    %% Should return not_found or error gracefully
    Result = erlmcp_icon_cache:get_cached_icon(<<"non_existent">>),
    ?assertEqual(not_found, Result).

%% Test 12: Invalidate all
test_cache_invalidate_all() ->
    %% Cache multiple icons
    lists:foreach(fun(N) ->
        Uri = iolist_to_binary([<<"icon_">>, integer_to_binary(N)]),
        Metadata = #{<<"id">> => N},
        erlmcp_icon_cache:cache_icon(Uri, Metadata, 3600000)
    end, lists:seq(1, 5)),

    %% Verify they're cached
    Stats1 = erlmcp_icon_cache:get_cache_stats(),
    Size1 = maps:get(cache_size, Stats1),
    ?assert(Size1 >= 5),

    %% Invalidate all
    ok = erlmcp_icon_cache:invalidate_all(),

    %% Verify cache is empty
    Stats2 = erlmcp_icon_cache:get_cache_stats(),
    Size2 = maps:get(cache_size, Stats2),
    ?assertEqual(0, Size2).

%% Test 13: Get expired entry details
test_cache_get_expired_entry() ->
    Uri = <<"https://example.com/expired.png">>,
    Metadata = #{<<"data">> => <<"original">>},
    ShortTtl = 100,

    CachedAt = erlang:monotonic_time(millisecond),
    ok = erlmcp_icon_cache:cache_icon(Uri, Metadata, ShortTtl, CachedAt),

    %% Wait for expiration
    timer:sleep(150),

    %% Get expired should return the original metadata
    {expired, Retrieved} = erlmcp_icon_cache:get_cached_icon(Uri),
    ?assertEqual(Metadata, Retrieved).

%% Test 14: Multiple icons in cache
test_cache_multiple_icons() ->
    Uris = [
        <<"https://example.com/icon1.png">>,
        <<"https://example.com/icon2.jpg">>,
        <<"https://example.com/icon3.svg">>,
        <<"https://example.com/icon4.webp">>,
        <<"https://example.com/icon5.gif">>
    ],

    %% Cache all icons
    lists:foreach(fun(Uri) ->
        Metadata = #{
            <<"uri">> => Uri,
            <<"size">> => rand:uniform(5000)
        },
        erlmcp_icon_cache:cache_icon(Uri, Metadata, 3600000)
    end, Uris),

    %% Verify all are cached
    Stats = erlmcp_icon_cache:get_cache_stats(),
    CacheSize = maps:get(cache_size, Stats),
    ?assert(CacheSize >= length(Uris)),

    %% Retrieve and verify each
    lists:foreach(fun(Uri) ->
        {ok, Retrieved} = erlmcp_icon_cache:get_cached_icon(Uri),
        ?assertEqual(Uri, maps:get(<<"uri">>, Retrieved))
    end, Uris).

%% Test 15: TTL enforcement across operations
test_cache_ttl_enforcement() ->
    Uri = <<"https://example.com/ttl_test.png">>,
    Metadata = #{<<"test">> => <<"ttl_enforcement">>},

    %% Set specific TTL
    LongTtl = 1000,    %% 1 second
    CachedAt = erlang:monotonic_time(millisecond),

    %% Cache with long TTL
    ok = erlmcp_icon_cache:cache_icon(Uri, Metadata, LongTtl, CachedAt),

    %% Should be valid at t=500ms
    timer:sleep(300),
    {ok, _} = erlmcp_icon_cache:get_cached_icon(Uri),

    %% Should still be valid at t=800ms
    timer:sleep(300),
    {ok, _} = erlmcp_icon_cache:get_cached_icon(Uri),

    %% Should expire at t=1100ms
    timer:sleep(400),
    {expired, _} = erlmcp_icon_cache:get_cached_icon(Uri).
