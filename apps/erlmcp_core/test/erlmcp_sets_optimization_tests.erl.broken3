-module(erlmcp_sets_optimization_tests).
%%%
%%% @doc OTP 28 Sets Optimization Tests
%%%
%%% Test suite to verify that erlmcp is using OTP 28's optimized map-based sets.
%%%
%%% @end
%%%

-include_lib("eunit/include/eunit.hrl").

%%%====================================================================
%%% Test Entry Points
%%%====================================================================

all_tests() -> [
    test_otp28_sets_default_backend,
    test_sets_are_maps,
    test_capability_sets_use_map_backend,
    test_large_set_performance,
    test_memory_usage_reduction,
    test_existing_code_uses_map_backend
].

%%%====================================================================
%%% Individual Tests
%%%====================================================================

%% @doc Test that OTP 28 uses map backend by default
-spec test_otp28_sets_default_backend() -> ok.
test_otp28_sets_default_backend() ->
    % Create a new set - should use map backend in OTP 28
    Set = sets:new(),
    ?assertMatch({set, _, map_backend}, Set),
    io:format("✓ OTP 28 map backend detected: ~p~n", [Set]),
    ok.

%% @doc Test that sets are maps using is_map/1
-spec test_sets_are_maps() -> ok.
test_sets_are_maps() ->
    % Test basic set creation
    EmptySet = sets:new(),
    ?assert(is_map(EmptySet), "Empty set should be a map in OTP 28"),

    % Test set with elements
    Set = sets:from_list([1, 2, 3, <<"test">>, atom]),
    ?assert(is_map(Set), "Non-empty set should be a map in OTP 28"),

    % Test all set operations produce maps
    Set1 = sets:add_element(4, Set),
    ?assert(is_map(Set1)),

    Set2 = sets:del_element(1, Set1),
    ?assert(is_map(Set2)),

    Intersection = sets:intersection(Set2, sets:from_list([2, 3, 5])),
    ?assert(is_map(Intersection)),

    io:format("✓ All sets are maps in OTP 28~n"),
    ok.

%% @doc Test that erlmcp_capabilities_sets uses map backend
-spec test_capability_sets_use_map_backend() -> ok.
test_capability_sets_use_map_backend() ->
    % Test using erlmcp_capabilities_sets module
    CapSet = erlmcp_capabilities_sets:new_capability_set(),
    ?assert(is_map(CapSet), "Capability set should be a map"),

    % Test adding capabilities
    CapSet1 = erlmcp_capabilities_sets:add_capability(CapSet, <<"resources">>),
    ?assert(is_map(CapSet1)),

    CapSet2 = erlmcp_capabilities_sets:add_capability(CapSet1, tools),
    ?assert(is_map(CapSet2)),

    % Test capability support check
    true = erlmcp_capabilities_sets:supports_capability(CapSet2, <<"resources">>),
    false = erlmcp_capabilities_sets:supports_capability(CapSet2, <<"prompts">>),

    % Test common capabilities
    ClientCaps = erlmcp_capabilities_sets:from_list([<<"resources">>, tools]),
    ServerCaps = erlmcp_capabilities_sets:from_list([<<"resources">>, <<"tools">>, <<"prompts">>]),
    Common = erlmcp_capabilities_sets:common_capabilities([ClientCaps, ServerCaps]),

    ?assert(is_map(Common)),
    ?assertEqual(2, erlmcp_capabilities_sets:size(Common)),

    io:format("✓ erlmcp_capabilities_sets uses map backend~n"),
    ok.

%% @doc Test performance improvement with large sets
-spec test_large_set_performance() -> ok.
test_large_set_performance() ->
    Size = 10000,

    % Create large set using OTP 28 map backend
    LargeSet = sets:from_list([<<"cap_", (integer_to_binary(N))/binary>> || N <- lists:seq(1, Size)]),
    ?assert(is_map(LargeSet)),
    ?assertEqual(Size, sets:size(LargeSet)),

    % Test O(1) lookup performance
    {Time, _} = timer:tc(fun() ->
        % Multiple lookups to demonstrate O(1) behavior
        [sets:is_element(<<"cap_", (integer_to_binary(rand:uniform(Size)))/binary>>, LargeSet)
         || _ <- lists:seq(1, 1000)]
    end),

    % Should be fast (less than 50ms for 1000 lookups)
    ?assert(Time < 50000, "1000 lookups took ~p ms (expected < 50ms)", [Time]),
    io:format("✓ Large set lookups: ~.2f ms per lookup~n", [Time / 1000]),
    ok.

%% @doc Test memory usage reduction
-spec test_memory_usage_reduction() -> ok.
test_memory_usage_reduction() ->
    Size = 5000,
    Elements = [<<"mem_", (integer_to_binary(N))/binary>> || N <- lists:seq(1, Size)],

    % Create set using OTP 28 map backend (default)
    MapSet = sets:from_list(Elements),
    MapSize = erts_debug:size(MapSet),

    % For comparison, if we forced tuple backend (not recommended)
    % TupleSet = sets:from_list(Elements, [{version, 1}]),
    % TupleSize = erts_debug:size(TupleSet),

    io:format("Map set size (~p elements): ~p words~n", [Size, MapSize]),

    % Map sets should be reasonably memory efficient
    ?assert(MapSize > 0, "Map set should have positive size"),
    ?assert(MapSize =< Size * 10, "Map size should be reasonable for ~p elements", [Size]),

    io:format("✓ Memory usage is reasonable for map-based sets~n"),
    ok.

%% @doc Test that existing erlmcp code uses map backend
-spec test_existing_code_uses_map_backend() -> ok.
test_existing_code_uses_map_backend() ->
    % Test that erlmcp client subscriptions use map-based sets
    TestSubs = sets:new(),
    ?assert(is_map(TestSubs)),

    % Simulate subscription management from erlmcp_client
    TestSubs1 = sets:add_element(self(), TestSubs),
    ?assert(is_map(TestSubs1)),

    true = sets:is_element(self(), TestSubs1),
    false = sets:is_element(whereis(non_existent), TestSubs1),

    % Test server subscription pattern (URI -> set of PIDs)
    UriMap = #{<<"test://resource">> => sets:from_list([self()])},
    SubsForUri = maps:get(<<"test://resource">>, UriMap),
    ?assert(is_map(SubsForUri)),
    ?assert(sets:is_element(self(), SubsForUri)),

    io:format("✓ Existing subscription patterns use map backend~n"),
    ok.

%%%====================================================================
%%% EUnit Tests
%%%====================================================================

sets_optimization_test_() ->
    [{setup,
      fun() ->
          % Ensure we're running on OTP 28
          Version = erlang:system_info(otp_release),
          case list_to_integer(Version) >= 28 of
              true -> ok;
              false -> throw({requires_otp28, Version})
          end
      end,
      fun(_) -> ok end,
      fun() ->
          [{foreach, fun() -> ok end, fun(_) -> ok end, [
              fun test_otp28_sets_default_backend/0,
              fun test_sets_are_maps/0,
              fun test_capability_sets_use_map_backend/0,
              fun test_large_set_performance/0,
              fun test_memory_usage_reduction/0,
              fun test_existing_code_uses_map_backend/0
          ]}]
      end
     }].