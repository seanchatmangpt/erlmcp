%%%-------------------------------------------------------------------
%%% @doc Property-Based Tests for erlmcp_cache Module
%%%
%%% Tests invariants:
%%% - Cache get/put roundtrip: put(K,V), get(K) = V
%%% - TTL expiration behavior
%%% - LRU eviction maintains max size
%%% - ETag consistency for same value
%%% - Cache statistics accuracy
%%% - Tag-based invalidation correctness
%%%
%%% Chicago School TDD: Real cache gen_server, no mocks, state-based verification
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_cache_proper_tests).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

%%%====================================================================
%%% Generators
%%%====================================================================

%% Generate valid cache keys
cache_key() ->
    proper_types:oneof([
        proper_types:binary(),
        proper_types:atom(),
        proper_types:int()
    ]).

%% Generate valid cache values
cache_value() ->
    proper_types:oneof([
        proper_types:binary(),
        proper_types:int(),
        proper_types:bool(),
        proper_types:atom(),
        proper_types:list(proper_types:int()),
        proper_types:map(proper_types:binary(), proper_types:int())
    ]).

%% Generate TTL values (0-10 seconds for testing)
ttl_value() ->
    proper_types:range(0, 10).

%% Generate LRU max sizes (10-100 for testing)
lru_max_size() ->
    proper_types:range(10, 100).

%% Generate cache tags
cache_tag() ->
    proper_types:binary().

%% Generate cache strategies
cache_strategy() ->
    proper_types:oneof([
        ?LET(TTL, ttl_value(), {ttl, TTL}),
        ?LET(Size, lru_max_size(), {lru, Size}),
        proper_types:oneof([write_through, write_back])
    ]).

%%%====================================================================
%%% Properties: Basic Cache Operations
%%%====================================================================

%% Property: Put followed by get returns same value (roundtrip)
prop_cache_put_get_roundtrip() ->
    ?FORALL({Key, Value, Strategy}, {cache_key(), cache_value(), cache_strategy()},
        begin
            {ok, Cache} = start_test_cache(#{max_l1_size => 100}),

            %% Put value
            ok = erlmcp_cache:put(Key, Value, Strategy),

            %% Get value
            Result = erlmcp_cache:get(Key),

            %% Cleanup
            stop_test_cache(Cache),

            %% Should return same value
            Result =:= {ok, Value}
        end).

%% Property: Get of non-existent key returns not_found
prop_cache_get_nonexistent() ->
    ?FORALL(Key, cache_key(),
        begin
            {ok, Cache} = start_test_cache(#{max_l1_size => 100}),

            %% Get without put
            Result = erlmcp_cache:get(Key),

            %% Cleanup
            stop_test_cache(Cache),

            %% Should return not_found
            Result =:= {error, not_found}
        end).

%% Property: Put with same key overwrites previous value
prop_cache_put_overwrites() ->
    ?FORALL({Key, Value1, Value2}, {cache_key(), cache_value(), cache_value()},
        ?IMPLIES(Value1 =/= Value2,
        begin
            {ok, Cache} = start_test_cache(#{max_l1_size => 100}),

            %% Put first value
            ok = erlmcp_cache:put(Key, Value1),

            %% Put second value
            ok = erlmcp_cache:put(Key, Value2),

            %% Get should return second value
            Result = erlmcp_cache:get(Key),

            %% Cleanup
            stop_test_cache(Cache),

            %% Should return second value
            Result =:= {ok, Value2}
        end)).

%% Property: Delete removes cached value
prop_cache_delete() ->
    ?FORALL({Key, Value}, {cache_key(), cache_value()},
        begin
            {ok, Cache} = start_test_cache(#{max_l1_size => 100}),

            %% Put value
            ok = erlmcp_cache:put(Key, Value),

            %% Delete value
            ok = erlmcp_cache:delete(Key),

            %% Get should return not_found
            Result = erlmcp_cache:get(Key),

            %% Cleanup
            stop_test_cache(Cache),

            %% Should return not_found
            Result =:= {error, not_found}
        end).

%% Property: Delete of non-existent key is idempotent (no error)
prop_cache_delete_nonexistent() ->
    ?FORALL(Key, cache_key(),
        begin
            {ok, Cache} = start_test_cache(#{max_l1_size => 100}),

            %% Delete without put
            Result = erlmcp_cache:delete(Key),

            %% Cleanup
            stop_test_cache(Cache),

            %% Should succeed (idempotent)
            Result =:= ok
        end).

%%%====================================================================
%%% Properties: TTL Expiration
%%%====================================================================

%% Property: Value with TTL=0 expires immediately after cleanup
prop_cache_ttl_zero_expires() ->
    ?FORALL({Key, Value}, {cache_key(), cache_value()},
        begin
            {ok, Cache} = start_test_cache(#{max_l1_size => 100, default_ttl_seconds => 1}),

            %% Put with TTL=0
            ok = erlmcp_cache:put(Key, Value, {ttl, 0}),

            %% Should be available immediately
            {ok, Value} = erlmcp_cache:get(Key),

            %% Wait for cleanup (cleanup_interval_ms = 1000 in test config, but we use shorter)
            timer:sleep(1200),

            %% Should be expired
            Result = erlmcp_cache:get(Key),

            %% Cleanup
            stop_test_cache(Cache),

            %% Should return not_found
            Result =:= {error, not_found}
        end).

%% Property: Value with positive TTL doesn't expire immediately
prop_cache_ttl_positive_persists() ->
    ?FORALL({Key, Value}, {cache_key(), cache_value()},
        begin
            {ok, Cache} = start_test_cache(#{max_l1_size => 100, default_ttl_seconds => 10}),

            %% Put with TTL=10
            ok = erlmcp_cache:put(Key, Value, {ttl, 10}),

            %% Wait less than TTL
            timer:sleep(500),

            %% Should still be available
            Result = erlmcp_cache:get(Key),

            %% Cleanup
            stop_test_cache(Cache),

            %% Should return value
            Result =:= {ok, Value}
        end).

%%%====================================================================
%%% Properties: LRU Eviction
%%%====================================================================

%% Property: LRU cache size never exceeds max size
prop_lru_max_size_limit() ->
    ?FORALL({MaxSize, Count}, {lru_max_size(), proper_types:range(1, 200)},
        ?IMPLIES(Count >= MaxSize,
        begin
            {ok, Cache} = start_test_cache(#{max_l1_size => MaxSize}),

            %% Fill cache beyond max size
            lists:foreach(fun(N) ->
                Key = list_to_atom("lru_key_" ++ integer_to_list(N)),
                Value = list_to_binary("lru_value_" ++ integer_to_list(N)),
                erlmcp_cache:put(Key, Value, {lru, MaxSize})
            end, lists:seq(1, Count)),

            %% Check stats
            Stats = erlmcp_cache:stats(),
            L1Size = maps:get(l1_size, Stats, 0),

            %% Cleanup
            stop_test_cache(Cache),

            %% Size should not exceed max
            L1Size =< MaxSize
        end)).

%% Property: LRU eviction increases eviction counter
prop_lru_eviction_increases_counter() ->
    ?FORALL(MaxSize, proper_types:range(10, 50),
        begin
            {ok, Cache} = start_test_cache(#{max_l1_size => MaxSize}),

            %% Get initial eviction count
            Stats0 = erlmcp_cache:stats(),
            InitialEvictions = maps:get(evictions, Stats0, 0),

            %% Fill cache beyond max size (MaxSize + 10)
            lists:foreach(fun(N) ->
                Key = list_to_atom("evict_key_" ++ integer_to_list(N)),
                Value = list_to_binary("evict_value_" ++ integer_to_list(N)),
                erlmcp_cache:put(Key, Value, {lru, MaxSize})
            end, lists:seq(1, MaxSize + 10)),

            %% Check eviction count increased
            Stats1 = erlmcp_cache:stats(),
            FinalEvictions = maps:get(evictions, Stats1, 0),

            %% Cleanup
            stop_test_cache(Cache),

            %% Should have evictions
            FinalEvictions > InitialEvictions
        end).

%%%====================================================================
%%% Properties: ETag Generation
%%%====================================================================

%% Property: Same value always generates same ETag
prop_etag_consistent() ->
    ?FORALL(Value, cache_value(),
        begin
            ETag1 = erlmcp_cache:generate_etag(Value),
            ETag2 = erlmcp_cache:generate_etag(Value),
            ETag1 =:= ETag2
        end).

%% Property: Different values likely generate different ETags
prop_etag_unique() ->
    ?FORALL({Value1, Value2}, {cache_value(), cache_value()},
        ?IMPLIES(Value1 =/= Value2,
        begin
            ETag1 = erlmcp_cache:generate_etag(Value1),
            ETag2 = erlmcp_cache:generate_etag(Value2),
            ETag1 =/= ETag2
        end)).

%% Property: ETag format is valid (quoted hex string)
prop_etag_format_valid() ->
    ?FORALL(Value, cache_value(),
        begin
            ETag = erlmcp_cache:generate_etag(Value),
            is_valid_etag_format(ETag)
        end).

%% Property: check_etag returns true for matching ETag
prop_etag_check_match() ->
    ?FORALL({Key, Value}, {cache_key(), cache_value()},
        begin
            {ok, Cache} = start_test_cache(#{max_l1_size => 100}),

            %% Put value
            ok = erlmcp_cache:put(Key, Value),

            %% Generate ETag
            ETag = erlmcp_cache:generate_etag(Value),

            %% Check ETag
            Result = erlmcp_cache:check_etag(Key, ETag),

            %% Cleanup
            stop_test_cache(Cache),

            %% Should match
            Result =:= true
        end).

%% Property: check_etag returns false for non-matching ETag
prop_etag_check_no_match() ->
    ?FORALL({Key, Value}, {cache_key(), cache_value()},
        begin
            {ok, Cache} = start_test_cache(#{max_l1_size => 100}),

            %% Put value
            ok = erlmcp_cache:put(Key, Value),

            %% Use different ETag
            WrongETag = <<"\"wrongetag\"">>,

            %% Check ETag
            Result = erlmcp_cache:check_etag(Key, WrongETag),

            %% Cleanup
            stop_test_cache(Cache),

            %% Should not match
            Result =:= false
        end).

%%%====================================================================
%%% Properties: Cache Statistics
%%%====================================================================

%% Property: Hit rate is between 0 and 1
prop_stats_hit_rate_valid() ->
    ?FORALL(Count, proper_types:range(1, 20),
        begin
            {ok, Cache} = start_test_cache(#{max_l1_size => 100}),

            %% Put some values
            lists:foreach(fun(N) ->
                Key = list_to_atom("stats_key_" ++ integer_to_list(N)),
                Value = list_to_binary("stats_value_" ++ integer_to_list(N)),
                erlmcp_cache:put(Key, Value)
            end, lists:seq(1, Count)),

            %% Generate some hits and misses
            lists:foreach(fun(N) ->
                Key = list_to_atom("stats_key_" ++ integer_to_list(N)),
                erlmcp_cache:get(Key),  % Hit
                erlmcp_cache:get(<<"nonexistent">>)  % Miss
            end, lists:seq(1, Count)),

            %% Check stats
            Stats = erlmcp_cache:stats(),
            HitRate = maps:get(hit_rate, Stats, 0.0),

            %% Cleanup
            stop_test_cache(Cache),

            %% Hit rate should be valid
            HitRate >= 0.0 andalso HitRate =< 1.0
        end).

%% Property: Write counter increments on put
prop_stats_write_counter() ->
    ?FORALL(Count, proper_types:range(1, 10),
        begin
            {ok, Cache} = start_test_cache(#{max_l1_size => 100}),

            %% Get initial write count
            Stats0 = erlmcp_cache:stats(),
            InitialWrites = maps:get(writes, Stats0, 0),

            %% Put values
            lists:foreach(fun(N) ->
                Key = list_to_atom("write_key_" ++ integer_to_list(N)),
                Value = list_to_binary("write_value_" ++ integer_to_list(N)),
                erlmcp_cache:put(Key, Value)
            end, lists:seq(1, Count)),

            %% Check write count
            Stats1 = erlmcp_cache:stats(),
            FinalWrites = maps:get(writes, Stats1, 0),

            %% Cleanup
            stop_test_cache(Cache),

            %% Should have incremented
            FinalWrites >= InitialWrites + Count
        end).

%% Property: Delete counter increments on delete
prop_stats_delete_counter() ->
    ?FORALL(Count, proper_types:range(1, 10),
        begin
            {ok, Cache} = start_test_cache(#{max_l1_size => 100}),

            %% Put values first
            lists:foreach(fun(N) ->
                Key = list_to_atom("delete_key_" ++ integer_to_list(N)),
                Value = list_to_binary("delete_value_" ++ integer_to_list(N)),
                erlmcp_cache:put(Key, Value)
            end, lists:seq(1, Count)),

            %% Get initial delete count
            Stats0 = erlmcp_cache:stats(),
            InitialDeletes = maps:get(deletes, Stats0, 0),

            %% Delete values
            lists:foreach(fun(N) ->
                Key = list_to_atom("delete_key_" ++ integer_to_list(N)),
                erlmcp_cache:delete(Key)
            end, lists:seq(1, Count)),

            %% Check delete count
            Stats1 = erlmcp_cache:stats(),
            FinalDeletes = maps:get(deletes, Stats1, 0),

            %% Cleanup
            stop_test_cache(Cache),

            %% Should have incremented
            FinalDeletes >= InitialDeletes + Count
        end).

%%%====================================================================
%%% Properties: Tag-Based Invalidation
%%%====================================================================

%% Property: Invalidate by tag removes all entries with that tag
prop_invalidate_by_tag() ->
    ?FORALL({Tag, Count}, {cache_tag(), proper_types:range(1, 10)},
        begin
            {ok, Cache} = start_test_cache(#{max_l1_size => 100}),

            %% Put values with same tag
            lists:foreach(fun(N) ->
                Key = list_to_atom("tag_key_" ++ integer_to_list(N)),
                Value = list_to_binary("tag_value_" ++ integer_to_list(N)),
                erlmcp_cache:put(Key, Value, {ttl, 300}, #{tags => [Tag]})
            end, lists:seq(1, Count)),

            %% Put one value with different tag
            ok = erlmcp_cache:put(<<"different_tag_key">>, <<"different_value">>, {ttl, 300},
                                  #{tags => [<<"different_tag">>]}),

            %% Invalidate by tag
            {ok, InvalidatedCount} = erlmcp_cache:invalidate_by_tag(Tag),

            %% Verify tagged entries are gone
            TaggedResults = [erlmcp_cache:get(list_to_atom("tag_key_" ++ integer_to_list(N))) || N <- lists:seq(1, Count)],
            AllTaggedGone = lists:all(fun(R) -> R =:= {error, not_found} end, TaggedResults),

            %% Verify untagged entry remains
            UntaggedResult = erlmcp_cache:get(<<"different_tag_key">>),

            %% Cleanup
            stop_test_cache(Cache),

            %% Should invalidate all tagged entries
            InvalidatedCount =:= Count andalso AllTaggedGone andalso UntaggedResult =:= {ok, <<"different_value">>}
        end).

%% Property: Invalidate by non-existent tag returns 0
prop_invalidate_by_nonexistent_tag() ->
    ?FORALL(Tag, cache_tag(),
        begin
            {ok, Cache} = start_test_cache(#{max_l1_size => 100}),

            %% Put some values with different tags
            ok = erlmcp_cache:put(<<"key1">>, <<"value1">>, {ttl, 300}, #{tags => [<<"other">>]}),

            %% Invalidate by non-existent tag
            {ok, Count} = erlmcp_cache:invalidate_by_tag(Tag),

            %% Cleanup
            stop_test_cache(Cache),

            %% Should invalidate 0 entries
            Count =:= 0
        end).

%%%====================================================================
%%% Properties: Cache Clear
%%%====================================================================

%% Property: Clear removes all entries
prop_cache_clear() ->
    ?FORALL(Count, proper_types:range(1, 10),
        begin
            {ok, Cache} = start_test_cache(#{max_l1_size => 100}),

            %% Put values
            lists:foreach(fun(N) ->
                Key = list_to_atom("clear_key_" ++ integer_to_list(N)),
                Value = list_to_binary("clear_value_" ++ integer_to_list(N)),
                erlmcp_cache:put(Key, Value)
            end, lists:seq(1, Count)),

            %% Clear cache
            ok = erlmcp_cache:clear(),

            %% Verify all gone
            Results = [erlmcp_cache:get(list_to_atom("clear_key_" ++ integer_to_list(N))) || N <- lists:seq(1, Count)],
            AllGone = lists:all(fun(R) -> R =:= {error, not_found} end, Results),

            %% Cleanup
            stop_test_cache(Cache),

            %% All should be gone
            AllGone
        end).

%% Property: Clear resets statistics
prop_cache_clear_resets_stats() ->
    ?FORALL(Count, proper_types:range(1, 10),
        begin
            {ok, Cache} = start_test_cache(#{max_l1_size => 100}),

            %% Put and get some values
            lists:foreach(fun(N) ->
                Key = list_to_atom("stats_key_" ++ integer_to_list(N)),
                Value = list_to_binary("stats_value_" ++ integer_to_list(N)),
                erlmcp_cache:put(Key, Value),
                erlmcp_cache:get(Key)
            end, lists:seq(1, Count)),

            %% Clear cache
            ok = erlmcp_cache:clear(),

            %% Check stats reset
            Stats = erlmcp_cache:stats(),
            Hits = maps:get(hits, Stats, 0),
            Misses = maps:get(misses, Stats, 0),

            %% Cleanup
            stop_test_cache(Cache),

            %% Stats should be reset
            Hits =:= 0 andalso Misses =:= 0
        end).

%%%====================================================================
%%% Helper Functions
%%%====================================================================

%% Start test cache with configuration
start_test_cache(Config) ->
    %% Stop Mnesia and cache if running to ensure clean state
    case whereis(erlmcp_cache) of
        undefined -> ok;
        _Pid -> gen_server:stop(erlmcp_cache)
    end,

    case mnesia:system_info(is_running) of
        yes ->
            application:stop(mnesia),
            timer:sleep(100);
        _ -> ok
    end,

    %% Delete old schema to start fresh (only if needed)
    case mnesia:system_info(is_running) of
        yes -> ok;
        _ ->
            catch mnesia:delete_schema([node()]),
            timer:sleep(50)
    end,

    %% Create schema
    case mnesia:create_schema([node()]) of
        ok -> ok;
        {error, {already_exists, _}} -> ok
    end,

    %% Start Mnesia
    case application:start(mnesia) of
        ok -> ok;
        {error, {already_started, mnesia}} -> ok
    end,

    %% Wait for Mnesia to be fully running
    ok = mnesia:wait_for_tables([schema], 5000),

    %% Merge with test defaults
    TestConfig = maps:merge(#{
        max_l1_size => 100,
        max_l2_size => 1000,
        default_ttl_seconds => 5,
        cleanup_interval_ms => 1000
    }, Config),

    case erlmcp_cache:start_link(TestConfig) of
        {ok, Pid} -> {ok, Pid};
        {error, {already_started, Pid}} -> {ok, Pid}
    end.

%% Stop test cache
stop_test_cache(Cache) ->
    case is_process_alive(Cache) of
        true ->
            erlmcp_cache:clear(),
            gen_server:stop(Cache),
            timer:sleep(50);
        false ->
            ok
    end,

    %% Stop Mnesia
    case mnesia:system_info(is_running) of
        yes -> application:stop(mnesia);
        _ -> ok
    end.

%% Check if ETag format is valid (quoted hex string)
is_valid_etag_format(ETag) ->
    is_binary(ETag) andalso
    byte_size(ETag) >= 2 andalso
    binary:first(ETag) =:= $" andalso
    binary:last(ETag) =:= $" andalso
    is_valid_hex(binary:part(ETag, 1, byte_size(ETag) - 2)).

%% Check if binary is valid hex
is_valid_hex(Bin) ->
    lists:all(fun(C) ->
        (C >= $0 andalso C =< $9) orelse
        (C >= $a andalso C =< $f) orelse
        (C >= $A andalso C =< $F)
    end, binary_to_list(Bin)).

%%%====================================================================
%%% EUnit Integration
%%%====================================================================

proper_test_() ->
    [
        {"Cache put/get roundtrip", ?_assertEqual(true, proper:quickcheck(prop_cache_put_get_roundtrip(), 20))},
        {"Cache get nonexistent", ?_assertEqual(true, proper:quickcheck(prop_cache_get_nonexistent(), 20))},
        {"Cache put overwrites", ?_assertEqual(true, proper:quickcheck(prop_cache_put_overwrites(), 20))},
        {"Cache delete", ?_assertEqual(true, proper:quickcheck(prop_cache_delete(), 20))},
        {"Cache delete nonexistent", ?_assertEqual(true, proper:quickcheck(prop_cache_delete_nonexistent(), 20))},
        {"Cache TTL zero expires", ?_assertEqual(true, proper:quickcheck(prop_cache_ttl_zero_expires(), 5))},
        {"Cache TTL positive persists", ?_assertEqual(true, proper:quickcheck(prop_cache_ttl_positive_persists(), 5))},
        {"LRU max size limit", ?_assertEqual(true, proper:quickcheck(prop_lru_max_size_limit(), 10))},
        {"LRU eviction increases counter", ?_assertEqual(true, proper:quickcheck(prop_lru_eviction_increases_counter(), 10))},
        {"ETag consistent", ?_assertEqual(true, proper:quickcheck(prop_etag_consistent(), 100))},
        {"ETag unique", ?_assertEqual(true, proper:quickcheck(prop_etag_unique(), 50))},
        {"ETag format valid", ?_assertEqual(true, proper:quickcheck(prop_etag_format_valid(), 100))},
        {"ETag check match", ?_assertEqual(true, proper:quickcheck(prop_etag_check_match(), 20))},
        {"ETag check no match", ?_assertEqual(true, proper:quickcheck(prop_etag_check_no_match(), 20))},
        {"Stats hit rate valid", ?_assertEqual(true, proper:quickcheck(prop_stats_hit_rate_valid(), 10))},
        {"Stats write counter", ?_assertEqual(true, proper:quickcheck(prop_stats_write_counter(), 10))},
        {"Stats delete counter", ?_assertEqual(true, proper:quickcheck(prop_stats_delete_counter(), 10))},
        {"Invalidate by tag", ?_assertEqual(true, proper:quickcheck(prop_invalidate_by_tag(), 10))},
        {"Invalidate by nonexistent tag", ?_assertEqual(true, proper:quickcheck(prop_invalidate_by_nonexistent_tag(), 10))},
        {"Cache clear", ?_assertEqual(true, proper:quickcheck(prop_cache_clear(), 10))},
        {"Cache clear resets stats", ?_assertEqual(true, proper:quickcheck(prop_cache_clear_resets_stats(), 10))}
    ].

%%%====================================================================
%%% Run All Properties
%%%====================================================================

run_all_properties() ->
    proper:module(?MODULE, [{numtests, 50}]).
