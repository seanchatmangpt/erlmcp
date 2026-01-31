%%%-------------------------------------------------------------------
%%% @doc Property-Based Tests for erlmcp_cache Basic Operations
%%%
%%% Tests invariants:
%%% - Cache get/put roundtrip: put(K,V), get(K) = V
%%% - Delete operations are idempotent
%%% - Clear removes all entries
%%% - ETag generation and validation
%%%
%%% Chicago School TDD: Real cache gen_server, API-only testing
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_cache_basic_proper_tests).
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

%% Generate cache strategies
cache_strategy() ->
    proper_types:oneof([
        ?LET(TTL, proper_types:range(0, 10), {ttl, TTL}),
        ?LET(Size, proper_types:range(10, 100), {lru, Size}),
        proper_types:oneof([write_through, write_back])
    ]).

%%%====================================================================
%%% Properties: Basic Cache Operations
%%%====================================================================

%% Property: Put followed by get returns same value (roundtrip)
prop_cache_put_get_roundtrip() ->
    ?FORALL({Key, Value, Strategy}, {cache_key(), cache_value(), cache_strategy()},
        begin
            {ok, _Cache} = erlmcp_test_helpers:start_test_cache(#{max_l1_size => 100}),

            %% Put value
            ok = erlmcp_cache:put(Key, Value, Strategy),

            %% Get value
            Result = erlmcp_cache:get(Key),

            %% Cleanup
            erlmcp_test_helpers:stop_test_cache(),

            %% Should return same value
            Result =:= {ok, Value}
        end).

%% Property: Get of non-existent key returns not_found
prop_cache_get_nonexistent() ->
    ?FORALL(Key, cache_key(),
        begin
            {ok, _Cache} = erlmcp_test_helpers:start_test_cache(#{max_l1_size => 100}),

            %% Get without put
            Result = erlmcp_cache:get(Key),

            %% Cleanup
            erlmcp_test_helpers:stop_test_cache(),

            %% Should return not_found
            Result =:= {error, not_found}
        end).

%% Property: Put with same key overwrites previous value
prop_cache_put_overwrites() ->
    ?FORALL({Key, Value1, Value2}, {cache_key(), cache_value(), cache_value()},
        ?IMPLIES(Value1 =/= Value2,
        begin
            {ok, _Cache} = erlmcp_test_helpers:start_test_cache(#{max_l1_size => 100}),

            %% Put first value
            ok = erlmcp_cache:put(Key, Value1),

            %% Put second value
            ok = erlmcp_cache:put(Key, Value2),

            %% Get should return second value
            Result = erlmcp_cache:get(Key),

            %% Cleanup
            erlmcp_test_helpers:stop_test_cache(),

            %% Should return second value
            Result =:= {ok, Value2}
        end)).

%% Property: Delete removes cached value
prop_cache_delete() ->
    ?FORALL({Key, Value}, {cache_key(), cache_value()},
        begin
            {ok, _Cache} = erlmcp_test_helpers:start_test_cache(#{max_l1_size => 100}),

            %% Put value
            ok = erlmcp_cache:put(Key, Value),

            %% Delete value
            ok = erlmcp_cache:delete(Key),

            %% Get should return not_found
            Result = erlmcp_cache:get(Key),

            %% Cleanup
            erlmcp_test_helpers:stop_test_cache(),

            %% Should return not_found
            Result =:= {error, not_found}
        end).

%% Property: Delete of non-existent key is idempotent (no error)
prop_cache_delete_nonexistent() ->
    ?FORALL(Key, cache_key(),
        begin
            {ok, _Cache} = erlmcp_test_helpers:start_test_cache(#{max_l1_size => 100}),

            %% Delete without put
            Result = erlmcp_cache:delete(Key),

            %% Cleanup
            erlmcp_test_helpers:stop_test_cache(),

            %% Should succeed (idempotent)
            Result =:= ok
        end).

%% Property: Clear removes all entries
prop_cache_clear() ->
    ?FORALL(Count, proper_types:range(1, 10),
        begin
            {ok, _Cache} = erlmcp_test_helpers:start_test_cache(#{max_l1_size => 100}),

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
            erlmcp_test_helpers:stop_test_cache(),

            %% All should be gone
            AllGone
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
            {ok, _Cache} = erlmcp_test_helpers:start_test_cache(#{max_l1_size => 100}),

            %% Put value
            ok = erlmcp_cache:put(Key, Value),

            %% Generate ETag
            ETag = erlmcp_cache:generate_etag(Value),

            %% Check ETag
            Result = erlmcp_cache:check_etag(Key, ETag),

            %% Cleanup
            erlmcp_test_helpers:stop_test_cache(),

            %% Should match
            Result =:= true
        end).

%% Property: check_etag returns false for non-matching ETag
prop_etag_check_no_match() ->
    ?FORALL({Key, Value}, {cache_key(), cache_value()},
        begin
            {ok, _Cache} = erlmcp_test_helpers:start_test_cache(#{max_l1_size => 100}),

            %% Put value
            ok = erlmcp_cache:put(Key, Value),

            %% Use different ETag
            WrongETag = <<"\"wrongetag\"">>,

            %% Check ETag
            Result = erlmcp_cache:check_etag(Key, WrongETag),

            %% Cleanup
            erlmcp_test_helpers:stop_test_cache(),

            %% Should not match
            Result =:= false
        end).

%%%====================================================================
%%% Helper Functions
%%%====================================================================

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
        {"Cache clear", ?_assertEqual(true, proper:quickcheck(prop_cache_clear(), 10))},
        {"ETag consistent", ?_assertEqual(true, proper:quickcheck(prop_etag_consistent(), 100))},
        {"ETag unique", ?_assertEqual(true, proper:quickcheck(prop_etag_unique(), 50))},
        {"ETag format valid", ?_assertEqual(true, proper:quickcheck(prop_etag_format_valid(), 100))},
        {"ETag check match", ?_assertEqual(true, proper:quickcheck(prop_etag_check_match(), 20))},
        {"ETag check no match", ?_assertEqual(true, proper:quickcheck(prop_etag_check_no_match(), 20))}
    ].

%%%====================================================================
%%% Run All Properties
%%%====================================================================

run_all_properties() ->
    proper:module(?MODULE, [{numtests, 50}]).
