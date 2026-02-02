-module(erlmcp_capabilities_sets).
%%%
%%% @doc OTP 28 Optimized Sets for MCP Capabilities
%%%
%%% This module provides a high-level API for using OTP 28's optimized
%%% map-based sets for MCP capability negotiation and subscription management.
%%%
%%% == OTP 28 Innovation ==
%%%
%%% Starting with OTP 28, `sets:new/0` returns a map-based set by default
%%% instead of the old tuple-based implementation. This provides significant
%%% performance improvements for set operations.
%%%
%%% Performance Benefits:
%%% - O(1) average case for `is_element/2` (was O(log N))
%%% - Better memory efficiency for sparse sets
%%% - Improved GC characteristics
%%%
%%% == Usage
%%%
%%% ```erlang
%%% % Create a new capability set (uses OTP 28 map backend)
%%% CapSet = new_capability_set(),
%%%
%%% % Add capabilities
%%% CapSet1 = add_capability(CapSet, <<"resources">>),
%%% CapSet2 = add_capability(CapSet1, <<"tools">>),
%%%
%%% % Check support
%%% true = supports_capability(CapSet2, <<"tools">>),
%%% false = supports_capability(CapSet2, <<"sampling">>),
%%%
%%% % Capability negotiation (intersection)
%%% ClientCaps = from_list([<<"resources">>, <<"tools">>]),
%%% ServerCaps = from_list([<<"resources">>, <<"tools">>, <<"prompts">>]),
%%% Negotiated = common_capabilities([ClientCaps, ServerCaps]),
%%% ```
%%%
%%% @end
%%%

%%%====================================================================
%%% API Exports
%%%====================================================================

-export([
    new_capability_set/0,
    add_capability/2,
    remove_capability/2,
    supports_capability/2,
    common_capabilities/1,
    union_capabilities/2,
    subtract_capabilities/2,
    to_list/1,
    from_list/1,
    is_empty/1,
    size/1,
    intersection/2,
    subset/2,
    equal/2
]).

%%%====================================================================
%%% Types
%%%====================================================================

-type capability() :: binary() | atom().
-type capability_set() :: sets:set(capability()).

-export_type([capability_set/0]).

%%%====================================================================
%%% API Functions
%%%====================================================================

%% @doc Create a new empty capability set.
%%
%% Uses OTP 28's optimized map-based set backend.
%%
%% @returns An empty capability set
%% @end
-spec new_capability_set() -> capability_set().
new_capability_set() ->
    sets:new().

%% @doc Add a capability to the set.
%%
%% @param Set The capability set
%% @param Capability The capability to add (binary or atom)
%% @returns Updated capability set
%% @end
-spec add_capability(capability_set(), capability()) -> capability_set().
add_capability(Set, Capability) when is_binary(Capability) ->
    sets:add_element(Capability, Set);
add_capability(Set, Capability) when is_atom(Capability) ->
    sets:add_element(Capability, Set).

%% @doc Remove a capability from the set.
%%
%% @param Set The capability set
%% @param Capability The capability to remove
%% @returns Updated capability set
%% @end
-spec remove_capability(capability_set(), capability()) -> capability_set().
remove_capability(Set, Capability) when is_binary(Capability) ->
    sets:del_element(Capability, Set);
remove_capability(Set, Capability) when is_atom(Capability) ->
    sets:del_element(Capability, Set).

%% @doc Check if a capability is supported (is in the set).
%%
%% O(1) average case with OTP 28 map backend.
%%
%% @param Set The capability set
%% @param Capability The capability to check
%% @returns true if capability is in the set, false otherwise
%% @end
-spec supports_capability(capability_set(), capability()) -> boolean().
supports_capability(Set, Capability) when is_binary(Capability) ->
    sets:is_element(Capability, Set);
supports_capability(Set, Capability) when is_atom(Capability) ->
    sets:is_element(Capability, Set).

%% @doc Find common capabilities across multiple sets (intersection).
%%
%% Useful for capability negotiation between client and server.
%% Returns the intersection of all sets in the list.
%%
%% @param SetList List of capability sets
%% @returns Intersection of all sets (capabilities common to all)
%% @end
-spec common_capabilities([capability_set()]) -> capability_set().
common_capabilities([]) ->
    new_capability_set();
common_capabilities([SingleSet]) ->
    SingleSet;
common_capabilities([FirstSet, SecondSet | Rest]) ->
    InitialIntersection = sets:intersection(FirstSet, SecondSet),
    lists:foldl(fun sets:intersection/2, InitialIntersection, Rest).

%% @doc Union two capability sets (all capabilities from both).
%%
%% @param Set1 First capability set
%% @param Set2 Second capability set
%% @returns Union of both sets
%% @end
-spec union_capabilities(capability_set(), capability_set()) -> capability_set().
union_capabilities(Set1, Set2) ->
    sets:union(Set1, Set2).

%% @doc Subtract capabilities from a set.
%%
%% @param Set1 The capability set to subtract from
%% @param Set2 The capabilities to remove
%% @returns Set1 minus Set2
%% @end
-spec subtract_capabilities(capability_set(), capability_set()) -> capability_set().
subtract_capabilities(Set1, Set2) ->
    sets:subtract(Set1, Set2).

%% @doc Convert capability set to a list.
%%
%% @param Set The capability set
%% @returns List of capabilities
%% @end
-spec to_list(capability_set()) -> [capability()].
to_list(Set) ->
    sets:to_list(Set).

%% @doc Create capability set from a list.
%%
%% @param List List of capabilities
%% @returns Capability set containing all elements from the list
%% @end
-spec from_list([capability()]) -> capability_set().
from_list(List) when is_list(List) ->
    sets:from_list(List).

%% @doc Check if capability set is empty.
%%
%% @param Set The capability set
%% @returns true if set is empty, false otherwise
%% @end
-spec is_empty(capability_set()) -> boolean().
is_empty(Set) ->
    sets:is_empty(Set).

%% @doc Get the number of capabilities in the set.
%%
%% @param Set The capability set
%% @returns Number of capabilities in the set
%% @end
-spec size(capability_set()) -> non_neg_integer().
size(Set) ->
    sets:size(Set).

%% @doc Intersection of two capability sets.
%%
%% Alias for common_capabilities/2 for two-set case.
%%
%% @param Set1 First capability set
%% @param Set2 Second capability set
%% @returns Intersection of both sets
%% @end
-spec intersection(capability_set(), capability_set()) -> capability_set().
intersection(Set1, Set2) ->
    sets:intersection(Set1, Set2).

%% @doc Check if Set1 is a subset of Set2.
%%
%% Useful for validating if client requirements are satisfied by server capabilities.
%%
%% @param Set1 The subset to check
%% @param Set2 The superset to check against
%% @returns true if Set1 is a subset of Set2, false otherwise
%% @end
-spec subset(capability_set(), capability_set()) -> boolean().
subset(Set1, Set2) ->
    sets:is_subset(Set1, Set2).

%% @doc Check if two capability sets are equal.
%%
%% Note: sets:equal/2 is not available in OTP 28, so we implement
%% equality checking using subset comparison in both directions.
%%
%% @param Set1 First capability set
%% @param Set2 Second capability set
%% @returns true if sets contain exactly the same elements
%% @end
-spec equal(capability_set(), capability_set()) -> boolean().
equal(Set1, Set2) ->
    sets:is_subset(Set1, Set2) andalso sets:is_subset(Set2, Set1).
