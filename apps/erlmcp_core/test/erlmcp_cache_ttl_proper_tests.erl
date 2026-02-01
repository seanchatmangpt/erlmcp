%%%-------------------------------------------------------------------
%%% @doc Property-Based Tests for erlmcp_cache TTL Expiration
%%%
%%% Tests invariants:
%%% - TTL expiration behavior
%%% - Value with TTL=0 expires after cleanup
%%% - Value with positive TTL persists
%%% - Tag-based invalidation
%%% - Cache statistics accuracy
%%%
%%% Chicago School TDD: Real cache gen_server, API-only testing
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_cache_ttl_proper_tests).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

%%%====================================================================
%%% Generators
%%%====================================================================

%% Generate valid cache keys
cache_key() ->
    proper_types:oneof([proper_types:binary(), proper_types:atom(), proper_types:int()]).

%% Generate valid cache values
cache_value() ->
    proper_types:oneof([proper_types:binary(),
                        proper_types:int(),
                        proper_types:bool(),
                        proper_types:atom()]).

%% Generate TTL values (0-10 seconds for testing)
ttl_value() ->
    proper_types:range(0, 10).

%% Generate cache tags
cache_tag() ->
    proper_types:binary().

%%%====================================================================
%%% Properties: TTL Expiration
%%%====================================================================

%% Property: Value with TTL=0 expires immediately after cleanup
prop_cache_ttl_zero_expires() ->
    ?FORALL({Key, Value},
            {cache_key(), cache_value()},
            begin
                {ok, _Cache} =
                    erlmcp_test_helpers:start_test_cache(#{max_l1_size => 100,
                                                           default_ttl_seconds => 1}),

                %% Put with TTL=0
                ok = erlmcp_cache:put(Key, Value, {ttl, 0}),

                %% Should be available immediately
                {ok, Value} = erlmcp_cache:get(Key),

                %% Wait for cleanup (cleanup_interval_ms = 1000 in test config, but we use shorter)
                timer:sleep(1200),

                %% Should be expired
                Result = erlmcp_cache:get(Key),

                %% Cleanup
                erlmcp_test_helpers:stop_test_cache(),

                %% Should return not_found
                Result =:= {error, not_found}
            end).

%% Property: Value with positive TTL doesn't expire immediately
prop_cache_ttl_positive_persists() ->
    ?FORALL({Key, Value},
            {cache_key(), cache_value()},
            begin
                {ok, _Cache} =
                    erlmcp_test_helpers:start_test_cache(#{max_l1_size => 100,
                                                           default_ttl_seconds => 10}),

                %% Put with TTL=10
                ok = erlmcp_cache:put(Key, Value, {ttl, 10}),

                %% Wait less than TTL
                timer:sleep(500),

                %% Should still be available
                Result = erlmcp_cache:get(Key),

                %% Cleanup
                erlmcp_test_helpers:stop_test_cache(),

                %% Should return value
                Result =:= {ok, Value}
            end).

%%%====================================================================
%%% Properties: Tag-Based Invalidation
%%%====================================================================

%% Property: Invalidate by tag removes all entries with that tag
prop_invalidate_by_tag() ->
    ?FORALL({Tag, Count},
            {cache_tag(), proper_types:range(1, 10)},
            begin
                {ok, _Cache} = erlmcp_test_helpers:start_test_cache(#{max_l1_size => 100}),

                %% Put values with same tag
                lists:foreach(fun(N) ->
                                 Key = list_to_atom("tag_key_" ++ integer_to_list(N)),
                                 Value = list_to_binary("tag_value_" ++ integer_to_list(N)),
                                 erlmcp_cache:put(Key, Value, {ttl, 300}, #{tags => [Tag]})
                              end,
                              lists:seq(1, Count)),

                %% Put one value with different tag
                ok =
                    erlmcp_cache:put(<<"different_tag_key">>,
                                     <<"different_value">>,
                                     {ttl, 300},
                                     #{tags => [<<"different_tag">>]}),

                %% Invalidate by tag
                {ok, InvalidatedCount} = erlmcp_cache:invalidate_by_tag(Tag),

                %% Verify tagged entries are gone
                TaggedResults =
                    [erlmcp_cache:get(list_to_atom("tag_key_" ++ integer_to_list(N)))
                     || N <- lists:seq(1, Count)],
                AllTaggedGone = lists:all(fun(R) -> R =:= {error, not_found} end, TaggedResults),

                %% Verify untagged entry remains
                UntaggedResult = erlmcp_cache:get(<<"different_tag_key">>),

                %% Cleanup
                erlmcp_test_helpers:stop_test_cache(),

                %% Should invalidate all tagged entries
                InvalidatedCount =:= Count
                andalso AllTaggedGone
                andalso UntaggedResult =:= {ok, <<"different_value">>}
            end).

%% Property: Invalidate by non-existent tag returns 0
prop_invalidate_by_nonexistent_tag() ->
    ?FORALL(Tag,
            cache_tag(),
            begin
                {ok, _Cache} = erlmcp_test_helpers:start_test_cache(#{max_l1_size => 100}),

                %% Put some values with different tags
                ok =
                    erlmcp_cache:put(<<"key1">>,
                                     <<"value1">>,
                                     {ttl, 300},
                                     #{tags => [<<"other">>]}),

                %% Invalidate by non-existent tag
                {ok, Count} = erlmcp_cache:invalidate_by_tag(Tag),

                %% Cleanup
                erlmcp_test_helpers:stop_test_cache(),

                %% Should invalidate 0 entries
                Count =:= 0
            end).

%%%====================================================================
%%% Properties: Cache Statistics
%%%====================================================================

%% Property: Hit rate is between 0 and 1
prop_stats_hit_rate_valid() ->
    ?FORALL(Count,
            proper_types:range(1, 20),
            begin
                {ok, _Cache} = erlmcp_test_helpers:start_test_cache(#{max_l1_size => 100}),

                %% Put some values
                lists:foreach(fun(N) ->
                                 Key = list_to_atom("stats_key_" ++ integer_to_list(N)),
                                 Value = list_to_binary("stats_value_" ++ integer_to_list(N)),
                                 erlmcp_cache:put(Key, Value)
                              end,
                              lists:seq(1, Count)),

                %% Generate some hits and misses
                lists:foreach(fun(N) ->
                                 Key = list_to_atom("stats_key_" ++ integer_to_list(N)),
                                 erlmcp_cache:get(Key),  % Hit
                                 erlmcp_cache:get(<<"nonexistent">>)  % Miss
                              end,
                              lists:seq(1, Count)),

                %% Check stats
                Stats = erlmcp_cache:stats(),
                HitRate = maps:get(hit_rate, Stats, 0.0),

                %% Cleanup
                erlmcp_test_helpers:stop_test_cache(),

                %% Hit rate should be valid
                HitRate >= 0.0 andalso HitRate =< 1.0
            end).

%% Property: Write counter increments on put
prop_stats_write_counter() ->
    ?FORALL(Count,
            proper_types:range(1, 10),
            begin
                {ok, _Cache} = erlmcp_test_helpers:start_test_cache(#{max_l1_size => 100}),

                %% Get initial write count
                Stats0 = erlmcp_cache:stats(),
                InitialWrites = maps:get(writes, Stats0, 0),

                %% Put values
                lists:foreach(fun(N) ->
                                 Key = list_to_atom("write_key_" ++ integer_to_list(N)),
                                 Value = list_to_binary("write_value_" ++ integer_to_list(N)),
                                 erlmcp_cache:put(Key, Value)
                              end,
                              lists:seq(1, Count)),

                %% Check write count
                Stats1 = erlmcp_cache:stats(),
                FinalWrites = maps:get(writes, Stats1, 0),

                %% Cleanup
                erlmcp_test_helpers:stop_test_cache(),

                %% Should have incremented
                FinalWrites >= InitialWrites + Count
            end).

%% Property: Delete counter increments on delete
prop_stats_delete_counter() ->
    ?FORALL(Count,
            proper_types:range(1, 10),
            begin
                {ok, _Cache} = erlmcp_test_helpers:start_test_cache(#{max_l1_size => 100}),

                %% Put values first
                lists:foreach(fun(N) ->
                                 Key = list_to_atom("delete_key_" ++ integer_to_list(N)),
                                 Value = list_to_binary("delete_value_" ++ integer_to_list(N)),
                                 erlmcp_cache:put(Key, Value)
                              end,
                              lists:seq(1, Count)),

                %% Get initial delete count
                Stats0 = erlmcp_cache:stats(),
                InitialDeletes = maps:get(deletes, Stats0, 0),

                %% Delete values
                lists:foreach(fun(N) ->
                                 Key = list_to_atom("delete_key_" ++ integer_to_list(N)),
                                 erlmcp_cache:delete(Key)
                              end,
                              lists:seq(1, Count)),

                %% Check delete count
                Stats1 = erlmcp_cache:stats(),
                FinalDeletes = maps:get(deletes, Stats1, 0),

                %% Cleanup
                erlmcp_test_helpers:stop_test_cache(),

                %% Should have incremented
                FinalDeletes >= InitialDeletes + Count
            end).

%% Property: Clear resets statistics
prop_cache_clear_resets_stats() ->
    ?FORALL(Count,
            proper_types:range(1, 10),
            begin
                {ok, _Cache} = erlmcp_test_helpers:start_test_cache(#{max_l1_size => 100}),

                %% Put and get some values
                lists:foreach(fun(N) ->
                                 Key = list_to_atom("stats_key_" ++ integer_to_list(N)),
                                 Value = list_to_binary("stats_value_" ++ integer_to_list(N)),
                                 erlmcp_cache:put(Key, Value),
                                 erlmcp_cache:get(Key)
                              end,
                              lists:seq(1, Count)),

                %% Clear cache
                ok = erlmcp_cache:clear(),

                %% Check stats reset
                Stats = erlmcp_cache:stats(),
                Hits = maps:get(hits, Stats, 0),
                Misses = maps:get(misses, Stats, 0),

                %% Cleanup
                erlmcp_test_helpers:stop_test_cache(),

                %% Stats should be reset
                Hits =:= 0 andalso Misses =:= 0
            end).

%%%====================================================================
%%% EUnit Integration
%%%====================================================================

proper_test_() ->
    [{"Cache TTL zero expires",
      ?_assertEqual(true, proper:quickcheck(prop_cache_ttl_zero_expires(), 5))},
     {"Cache TTL positive persists",
      ?_assertEqual(true, proper:quickcheck(prop_cache_ttl_positive_persists(), 5))},
     {"Invalidate by tag", ?_assertEqual(true, proper:quickcheck(prop_invalidate_by_tag(), 10))},
     {"Invalidate by nonexistent tag",
      ?_assertEqual(true, proper:quickcheck(prop_invalidate_by_nonexistent_tag(), 10))},
     {"Stats hit rate valid",
      ?_assertEqual(true, proper:quickcheck(prop_stats_hit_rate_valid(), 10))},
     {"Stats write counter",
      ?_assertEqual(true, proper:quickcheck(prop_stats_write_counter(), 10))},
     {"Stats delete counter",
      ?_assertEqual(true, proper:quickcheck(prop_stats_delete_counter(), 10))},
     {"Cache clear resets stats",
      ?_assertEqual(true, proper:quickcheck(prop_cache_clear_resets_stats(), 10))}].

%%%====================================================================
%%% Run All Properties
%%%====================================================================

run_all_properties() ->
    proper:module(?MODULE, [{numtests, 50}]).
