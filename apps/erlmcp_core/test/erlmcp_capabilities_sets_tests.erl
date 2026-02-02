-module(erlmcp_capabilities_sets_tests).
%%%
%%% @doc Unit Tests for OTP 28 Optimized Capabilities Sets
%%%
%%% Tests the erlmcp_capabilities_sets module which provides a high-level
%%% API for using OTP 28's map-based sets for MCP capability negotiation.
%%%
%%% Run with:
%%% ```bash
%%% rebar3 eunit --module=erlmcp_capabilities_sets_tests
%%% ```
%%%
%%% @end
%%%

-include_lib("eunit/include/eunit.hrl").

%%%====================================================================
%%% Test Data
%%%====================================================================

-define(CAPABILITIES, [
    <<"resources">>,
    <<"tools">>,
    <<"prompts">>,
    <<"logging">>,
    <<"sampling">>,
    <<"roots">>
]).

%%%====================================================================
%%% Basic Set Operations Tests
%%%====================================================================

%% @doc Test creating a new empty capability set
new_capability_set_test() ->
    Set = erlmcp_capabilities_sets:new_capability_set(),
    ?assertEqual(true, erlmcp_capabilities_sets:is_empty(Set)),
    ?assertEqual(0, erlmcp_capabilities_sets:size(Set)).

%% @doc Test adding capabilities to a set
add_capability_test() ->
    Set0 = erlmcp_capabilities_sets:new_capability_set(),
    Set1 = erlmcp_capabilities_sets:add_capability(Set0, <<"resources">>),
    ?assertEqual(false, erlmcp_capabilities_sets:is_empty(Set1)),
    ?assertEqual(1, erlmcp_capabilities_sets:size(Set1)),
    ?assertEqual(true, erlmcp_capabilities_sets:supports_capability(Set1, <<"resources">>)).

%% @doc Test adding multiple capabilities
add_multiple_capabilities_test() ->
    Set0 = erlmcp_capabilities_sets:new_capability_set(),
    Set1 = lists:foldl(fun(Cap, Acc) ->
        erlmcp_capabilities_sets:add_capability(Acc, Cap)
    end, Set0, ?CAPABILITIES),

    ?assertEqual(length(?CAPABILITIES), erlmcp_capabilities_sets:size(Set1)),
    lists:foreach(fun(Cap) ->
        ?assertEqual(true, erlmcp_capabilities_sets:supports_capability(Set1, Cap))
    end, ?CAPABILITIES).

%% @doc Test removing capabilities from a set
remove_capability_test() ->
    Set0 = erlmcp_capabilities_sets:from_list(?CAPABILITIES),
    Set1 = erlmcp_capabilities_sets:remove_capability(Set0, <<"sampling">>),

    ?assertEqual(length(?CAPABILITIES) - 1, erlmcp_capabilities_sets:size(Set1)),
    ?assertEqual(false, erlmcp_capabilities_sets:supports_capability(Set1, <<"sampling">>)),
    ?assertEqual(true, erlmcp_capabilities_sets:supports_capability(Set1, <<"resources">>)).

%% @doc Test checking capability support
supports_capability_test() ->
    Set = erlmcp_capabilities_sets:from_list([<<"tools">>, <<"prompts">>]),

    ?assertEqual(true, erlmcp_capabilities_sets:supports_capability(Set, <<"tools">>)),
    ?assertEqual(true, erlmcp_capabilities_sets:supports_capability(Set, <<"prompts">>)),
    ?assertEqual(false, erlmcp_capabilities_sets:supports_capability(Set, <<"resources">>)),
    ?assertEqual(false, erlmcp_capabilities_sets:supports_capability(Set, <<"sampling">>)).

%% @doc Test with atom capabilities (not just binaries)
atom_capabilities_test() ->
    Set0 = erlmcp_capabilities_sets:new_capability_set(),
    Set1 = erlmcp_capabilities_sets:add_capability(Set0, resources),
    Set2 = erlmcp_capabilities_sets:add_capability(Set1, tools),

    ?assertEqual(true, erlmcp_capabilities_sets:supports_capability(Set2, resources)),
    ?assertEqual(true, erlmcp_capabilities_sets:supports_capability(Set2, tools)),
    ?assertEqual(false, erlmcp_capabilities_sets:supports_capability(Set2, prompts)).

%%%====================================================================
%%% Set Conversion Tests
%%%====================================================================

%% @doc Test converting list to set
from_list_test() ->
    Set = erlmcp_capabilities_sets:from_list(?CAPABILITIES),
    ?assertEqual(length(?CAPABILITIES), erlmcp_capabilities_sets:size(Set)),
    lists:foreach(fun(Cap) ->
        ?assertEqual(true, erlmcp_capabilities_sets:supports_capability(Set, Cap))
    end, ?CAPABILITIES).

%% @doc Test converting set to list
to_list_test() ->
    Set = erlmcp_capabilities_sets:from_list(?CAPABILITIES),
    List = erlmcp_capabilities_sets:to_list(Set),

    ?assertEqual(length(?CAPABILITIES), length(List)),
    lists:foreach(fun(Cap) ->
        ?assertEqual(true, lists:member(Cap, List))
    end, ?CAPABILITIES).

%% @doc Test round-trip conversion (list -> set -> list)
round_trip_conversion_test() ->
    OriginalList = ?CAPABILITIES,
    Set = erlmcp_capabilities_sets:from_list(OriginalList),
    ResultList = lists:sort(erlmcp_capabilities_sets:to_list(Set)),

    ?assertEqual(lists:sort(OriginalList), ResultList).

%%%====================================================================
%%% Set Operations Tests
%%%====================================================================

%% @doc Test union of two sets
union_capabilities_test() ->
    Set1 = erlmcp_capabilities_sets:from_list([<<"resources">>, <<"tools">>]),
    Set2 = erlmcp_capabilities_sets:from_list([<<"prompts">>, <<"logging">>]),
    Union = erlmcp_capabilities_sets:union_capabilities(Set1, Set2),

    ?assertEqual(4, erlmcp_capabilities_sets:size(Union)),
    ?assertEqual(true, erlmcp_capabilities_sets:supports_capability(Union, <<"resources">>)),
    ?assertEqual(true, erlmcp_capabilities_sets:supports_capability(Union, <<"tools">>)),
    ?assertEqual(true, erlmcp_capabilities_sets:supports_capability(Union, <<"prompts">>)),
    ?assertEqual(true, erlmcp_capabilities_sets:supports_capability(Union, <<"logging">>)).

%% @doc Test intersection of two sets
intersection_test() ->
    Set1 = erlmcp_capabilities_sets:from_list([<<"resources">>, <<"tools">>, <<"prompts">>]),
    Set2 = erlmcp_capabilities_sets:from_list([<<"tools">>, <<"prompts">>, <<"logging">>]),
    Intersection = erlmcp_capabilities_sets:intersection(Set1, Set2),

    ?assertEqual(2, erlmcp_capabilities_sets:size(Intersection)),
    ?assertEqual(false, erlmcp_capabilities_sets:supports_capability(Intersection, <<"resources">>)),
    ?assertEqual(true, erlmcp_capabilities_sets:supports_capability(Intersection, <<"tools">>)),
    ?assertEqual(true, erlmcp_capabilities_sets:supports_capability(Intersection, <<"prompts">>)),
    ?assertEqual(false, erlmcp_capabilities_sets:supports_capability(Intersection, <<"logging">>)).

%% @doc Test subtracting sets
subtract_capabilities_test() ->
    Set1 = erlmcp_capabilities_sets:from_list([<<"resources">>, <<"tools">>, <<"prompts">>]),
    Set2 = erlmcp_capabilities_sets:from_list([<<"tools">>]),
    Difference = erlmcp_capabilities_sets:subtract_capabilities(Set1, Set2),

    ?assertEqual(2, erlmcp_capabilities_sets:size(Difference)),
    ?assertEqual(true, erlmcp_capabilities_sets:supports_capability(Difference, <<"resources">>)),
    ?assertEqual(false, erlmcp_capabilities_sets:supports_capability(Difference, <<"tools">>)),
    ?assertEqual(true, erlmcp_capabilities_sets:supports_capability(Difference, <<"prompts">>)).

%% @doc Test subset check
subset_test() ->
    SmallSet = erlmcp_capabilities_sets:from_list([<<"tools">>, <<"prompts">>]),
    LargeSet = erlmcp_capabilities_sets:from_list([
        <<"resources">>, <<"tools">>, <<"prompts">>, <<"logging">>
    ]),

    ?assertEqual(true, erlmcp_capabilities_sets:subset(SmallSet, LargeSet)),
    ?assertEqual(false, erlmcp_capabilities_sets:subset(LargeSet, SmallSet)).

%% @doc Test equality check
equal_test() ->
    Set1 = erlmcp_capabilities_sets:from_list([<<"resources">>, <<"tools">>]),
    Set2 = erlmcp_capabilities_sets:from_list([<<"tools">>, <<"resources">>]),
    Set3 = erlmcp_capabilities_sets:from_list([<<"resources">>, <<"prompts">>]),

    ?assertEqual(true, erlmcp_capabilities_sets:equal(Set1, Set2)),
    ?assertEqual(false, erlmcp_capabilities_sets:equal(Set1, Set3)).

%%%====================================================================
%%% Common Capabilities Tests (Multi-set Intersection)
%%%====================================================================

%% @doc Test common capabilities across multiple sets
common_capabilities_two_sets_test() ->
    ClientCaps = erlmcp_capabilities_sets:from_list([<<"resources">>, <<"tools">>]),
    ServerCaps = erlmcp_capabilities_sets:from_list([
        <<"resources">>, <<"tools">>, <<"prompts">>
    ]),
    Common = erlmcp_capabilities_sets:common_capabilities([ClientCaps, ServerCaps]),

    ?assertEqual(2, erlmcp_capabilities_sets:size(Common)),
    ?assertEqual(true, erlmcp_capabilities_sets:supports_capability(Common, <<"resources">>)),
    ?assertEqual(true, erlmcp_capabilities_sets:supports_capability(Common, <<"tools">>)),
    ?assertEqual(false, erlmcp_capabilities_sets:supports_capability(Common, <<"prompts">>)).

%% @doc Test common capabilities across three sets
common_capabilities_three_sets_test() ->
    Set1 = erlmcp_capabilities_sets:from_list([<<"a">>, <<"b">>, <<"c">>]),
    Set2 = erlmcp_capabilities_sets:from_list([<<"b">>, <<"c">>, <<"d">>]),
    Set3 = erlmcp_capabilities_sets:from_list([<<"b">>, <<"c">>, <<"e">>]),
    Common = erlmcp_capabilities_sets:common_capabilities([Set1, Set2, Set3]),

    ?assertEqual(2, erlmcp_capabilities_sets:size(Common)),
    ?assertEqual(true, erlmcp_capabilities_sets:supports_capability(Common, <<"b">>)),
    ?assertEqual(true, erlmcp_capabilities_sets:supports_capability(Common, <<"c">>)),
    ?assertEqual(false, erlmcp_capabilities_sets:supports_capability(Common, <<"a">>)),
    ?assertEqual(false, erlmcp_capabilities_sets:supports_capability(Common, <<"d">>)),
    ?assertEqual(false, erlmcp_capabilities_sets:supports_capability(Common, <<"e">>)).

%% @doc Test common capabilities with empty list
common_capabilities_empty_test() ->
    Empty = erlmcp_capabilities_sets:common_capabilities([]),
    ?assertEqual(true, erlmcp_capabilities_sets:is_empty(Empty)).

%% @doc Test common capabilities with single set
common_capabilities_single_test() ->
    Set = erlmcp_capabilities_sets:from_list([<<"a">>, <<"b">>]),
    Result = erlmcp_capabilities_sets:common_capabilities([Set]),
    ?assertEqual(true, erlmcp_capabilities_sets:equal(Set, Result)).

%%%====================================================================
%%% MCP Capability Negotiation Tests
%%%====================================================================

%% @doc Test realistic MCP capability negotiation scenario
mcp_capability_negotiation_test() ->
    % Server offers all standard capabilities
    ServerCaps = erlmcp_capabilities_sets:from_list([
        <<"resources">>,
        <<"tools">>,
        <<"prompts">>,
        <<"logging">>
    ]),

    % Client requests subset of capabilities
    ClientCaps = erlmcp_capabilities_sets:from_list([
        <<"resources">>,
        <<"tools">>
    ]),

    % Negotiated capabilities = intersection
    Negotiated = erlmcp_capabilities_sets:common_capabilities([ServerCaps, ClientCaps]),

    ?assertEqual(true, erlmcp_capabilities_sets:subset(ClientCaps, ServerCaps)),
    ?assertEqual(2, erlmcp_capabilities_sets:size(Negotiated)),
    ?assertEqual(true, erlmcp_capabilities_sets:supports_capability(Negotiated, <<"resources">>)),
    ?assertEqual(true, erlmcp_capabilities_sets:supports_capability(Negotiated, <<"tools">>)).

%% @doc Test client requesting unsupported capability
unsupported_capability_test() ->
    ServerCaps = erlmcp_capabilities_sets:from_list([<<"resources">>, <<"tools">>]),
    ClientCaps = erlmcp_capabilities_sets:from_list([
        <<"resources">>,
        <<"tools">>,
        <<"sampling">>  % Not supported by server
    ]),

    ?assertEqual(false, erlmcp_capabilities_sets:subset(ClientCaps, ServerCaps)),

    Negotiated = erlmcp_capabilities_sets:common_capabilities([ServerCaps, ClientCaps]),
    ?assertEqual(2, erlmcp_capabilities_sets:size(Negotiated)),
    ?assertEqual(false, erlmcp_capabilities_sets:supports_capability(Negotiated, <<"sampling">>)).

%%%====================================================================
%%% Edge Cases Tests
%%%====================================================================

%% @doc Test operations on empty set
empty_set_operations_test() ->
    EmptySet = erlmcp_capabilities_sets:new_capability_set(),

    ?assertEqual(true, erlmcp_capabilities_sets:is_empty(EmptySet)),
    ?assertEqual(0, erlmcp_capabilities_sets:size(EmptySet)),
    ?assertEqual([], erlmcp_capabilities_sets:to_list(EmptySet)),
    ?assertEqual(false, erlmcp_capabilities_sets:supports_capability(EmptySet, <<"anything">>)),

    % Union with empty set
    NonEmpty = erlmcp_capabilities_sets:from_list([<<"a">>]),
    ?assertEqual(true, erlmcp_capabilities_sets:equal(
        NonEmpty,
        erlmcp_capabilities_sets:union_capabilities(EmptySet, NonEmpty)
    )).

%% @doc Test duplicate capabilities (sets should handle automatically)
duplicate_capabilities_test() ->
    ListWithDupes = [<<"a">>, <<"b">>, <<"a">>, <<"c">>, <<"b">>],
    Set = erlmcp_capabilities_sets:from_list(ListWithDupes),

    ?assertEqual(3, erlmcp_capabilities_sets:size(Set)),
    ?assertEqual(true, erlmcp_capabilities_sets:supports_capability(Set, <<"a">>)),
    ?assertEqual(true, erlmcp_capabilities_sets:supports_capability(Set, <<"b">>)),
    ?assertEqual(true, erlmcp_capabilities_sets:supports_capability(Set, <<"c">>)).

%% @doc Test large set performance (stress test)
large_set_test() ->
    % Create a set with 10K capabilities
    LargeList = [<<"cap_", (integer_to_binary(N))/binary>> || N <- lists:seq(1, 10000)],
    LargeSet = erlmcp_capabilities_sets:from_list(LargeList),

    ?assertEqual(10000, erlmcp_capabilities_sets:size(LargeSet)),

    % Test lookup performance (should be O(1) with map backend)
    ?assertEqual(true, erlmcp_capabilities_sets:supports_capability(LargeSet, <<"cap_5000">>)),
    ?assertEqual(false, erlmcp_capabilities_sets:supports_capability(LargeSet, <<"cap_15000">>)).

%%%====================================================================
%%% OTP 28 Backend Verification Tests
%%%====================================================================

%% @doc Verify that sets are using map backend (OTP 28 default)
verify_map_backend_test() ->
    Set = erlmcp_capabilities_sets:new_capability_set(),

    % OTP 28 sets:new() returns a plain map by default
    % The internal implementation changed from {set, ..., tuple_backend}
    % to just a map() for better performance
    IsMap = is_map(Set),

    % Verify it's a map (OTP 28 default)
    ?assertEqual(true, IsMap),

    % Test that operations work correctly (implementation detail hidden)
    Set1 = erlmcp_capabilities_sets:add_capability(Set, <<"test">>),
    ?assertEqual(true, erlmcp_capabilities_sets:supports_capability(Set1, <<"test">>)).
