%%%-------------------------------------------------------------------
%%% @doc Property-Based Tests for erlmcp_cache LRU Eviction
%%%
%%% Tests invariants:
%%% - LRU eviction maintains max size
%%% - LRU eviction increases eviction counter
%%% - Cache size never exceeds configured maximum
%%% - Least recently used items are evicted first
%%%
%%% Chicago School TDD: Real cache gen_server, API-only testing
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_cache_lru_proper_tests).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

%%%====================================================================
%%% Generators
%%%====================================================================

%% Generate LRU max sizes (10-100 for testing)
lru_max_size() ->
    proper_types:range(10, 100).

%%%====================================================================
%%% Properties: LRU Eviction
%%%====================================================================

%% Property: LRU cache size never exceeds max size
prop_lru_max_size_limit() ->
    ?FORALL({MaxSize, Count}, {lru_max_size(), proper_types:range(1, 200)},
        ?IMPLIES(Count >= MaxSize,
        begin
            {ok, _Cache} = erlmcp_test_helpers:start_test_cache(#{max_l1_size => MaxSize}),

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
            erlmcp_test_helpers:stop_test_cache(),

            %% Size should not exceed max
            L1Size =< MaxSize
        end)).

%% Property: LRU eviction increases eviction counter
prop_lru_eviction_increases_counter() ->
    ?FORALL(MaxSize, proper_types:range(10, 50),
        begin
            {ok, _Cache} = erlmcp_test_helpers:start_test_cache(#{max_l1_size => MaxSize}),

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
            erlmcp_test_helpers:stop_test_cache(),

            %% Should have evictions
            FinalEvictions > InitialEvictions
        end).

%% Property: Least recently used items are evicted first
prop_lru_evicts_least_recently_used() ->
    ?FORALL(MaxSize, proper_types:range(10, 30),
        begin
            {ok, _Cache} = erlmcp_test_helpers:start_test_cache(#{max_l1_size => MaxSize}),

            %% Fill cache to max size
            Keys = lists:map(fun(N) ->
                Key = list_to_atom("lru_key_" ++ integer_to_list(N)),
                Value = list_to_binary("lru_value_" ++ integer_to_list(N)),
                erlmcp_cache:put(Key, Value, {lru, MaxSize}),
                Key
            end, lists:seq(1, MaxSize)),

            %% Access first item to make it more recently used
            FirstKey = hd(Keys),
            {ok, _} = erlmcp_cache:get(FirstKey),

            %% Add one more item to trigger eviction
            NewKey = list_to_atom("lru_key_new"),
            NewValue = <<"new_value">>,
            ok = erlmcp_cache:put(NewKey, NewValue, {lru, MaxSize}),

            %% First item should still be available (was accessed)
            FirstResult = erlmcp_cache:get(FirstKey),

            %% Second item (least recently used) should have been evicted
            SecondKey = lists:nth(2, Keys),
            SecondResult = erlmcp_cache:get(SecondKey),

            %% New item should be available
            NewResult = erlmcp_cache:get(NewKey),

            %% Cleanup
            erlmcp_test_helpers:stop_test_cache(),

            %% Verify LRU behavior: first and new available, second evicted
            FirstResult =:= {ok, list_to_binary("lru_value_1")} andalso
            SecondResult =:= {error, not_found} andalso
            NewResult =:= {ok, NewValue}
        end).

%% Property: Cache with LRU strategy maintains size limit
prop_lru_strategy_enforces_limit() ->
    ?FORALL({MaxSize, OverCount}, {lru_max_size(), proper_types:range(1, 20)},
        begin
            {ok, _Cache} = erlmcp_test_helpers:start_test_cache(#{max_l1_size => MaxSize}),

            %% Fill cache beyond max size
            lists:foreach(fun(N) ->
                Key = list_to_atom("limit_key_" ++ integer_to_list(N)),
                Value = list_to_binary("limit_value_" ++ integer_to_list(N)),
                erlmcp_cache:put(Key, Value, {lru, MaxSize})
            end, lists:seq(1, MaxSize + OverCount)),

            %% Check final size
            Stats = erlmcp_cache:stats(),
            L1Size = maps:get(l1_size, Stats, 0),

            %% Cleanup
            erlmcp_test_helpers:stop_test_cache(),

            %% Size should be at most max size
            L1Size =< MaxSize
        end).

%% Property: Multiple cache operations maintain LRU invariant
prop_lru_multiple_operations() ->
    ?FORALL(MaxSize, proper_types:range(10, 30),
        begin
            {ok, _Cache} = erlmcp_test_helpers:start_test_cache(#{max_l1_size => MaxSize}),

            %% Perform mix of puts and gets
            lists:foreach(fun(N) ->
                Key = list_to_atom("multi_key_" ++ integer_to_list(N rem MaxSize + 1)),
                Value = list_to_binary("multi_value_" ++ integer_to_list(N)),
                erlmcp_cache:put(Key, Value, {lru, MaxSize}),
                %% Occasionally access existing keys
                case N rem 3 of
                    0 ->
                        OldKey = list_to_atom("multi_key_" ++ integer_to_list(max(1, N rem MaxSize))),
                        erlmcp_cache:get(OldKey);
                    _ -> ok
                end
            end, lists:seq(1, MaxSize * 2)),

            %% Check final size and evictions
            Stats = erlmcp_cache:stats(),
            L1Size = maps:get(l1_size, Stats, 0),
            Evictions = maps:get(evictions, Stats, 0),

            %% Cleanup
            erlmcp_test_helpers:stop_test_cache(),

            %% Size should be within limit, evictions should have occurred
            L1Size =< MaxSize andalso Evictions > 0
        end).

%%%====================================================================
%%% EUnit Integration
%%%====================================================================

proper_test_() ->
    [
        {"LRU max size limit", ?_assertEqual(true, proper:quickcheck(prop_lru_max_size_limit(), 10))},
        {"LRU eviction increases counter", ?_assertEqual(true, proper:quickcheck(prop_lru_eviction_increases_counter(), 10))},
        {"LRU evicts least recently used", ?_assertEqual(true, proper:quickcheck(prop_lru_evicts_least_recently_used(), 10))},
        {"LRU strategy enforces limit", ?_assertEqual(true, proper:quickcheck(prop_lru_strategy_enforces_limit(), 10))},
        {"LRU multiple operations", ?_assertEqual(true, proper:quickcheck(prop_lru_multiple_operations(), 5))}
    ].

%%%====================================================================
%%% Run All Properties
%%%====================================================================

run_all_properties() ->
    proper:module(?MODULE, [{numtests, 50}]).
