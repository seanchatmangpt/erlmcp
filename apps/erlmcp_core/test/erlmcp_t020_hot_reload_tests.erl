%%%-------------------------------------------------------------------
%%% @doc T020 — Hot reload in-flight count and state compatibility tests
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_t020_hot_reload_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% T020 — in-flight count tests
%%%===================================================================

%% With N requests in the registry, get_in_flight_count returns N.
in_flight_count_with_requests_test() ->
    Table = erlmcp_request_registry,
    %% Ensure table exists (owned by test process)
    case ets:info(Table) of
        undefined -> ets:new(Table, [named_table, public, bag]);
        _         -> ok
    end,
    ets:delete_all_objects(Table),
    %% Insert two fake requests for erlmcp_json_rpc
    ets:insert(Table, {req1, erlmcp_json_rpc, #{}}),
    ets:insert(Table, {req2, erlmcp_json_rpc, #{}}),
    %% Third entry for a different module — must NOT be counted
    ets:insert(Table, {req3, some_other_module, #{}}),

    Count = erlmcp_code_reload:get_in_flight_count(erlmcp_json_rpc),
    ?assertEqual(2, Count),

    ets:delete_all_objects(Table).

%% With an empty registry the count must be 0.
in_flight_count_empty_test() ->
    Table = erlmcp_request_registry,
    case ets:info(Table) of
        undefined -> ok;
        _         -> ets:delete_all_objects(Table)
    end,
    Count = erlmcp_code_reload:get_in_flight_count(erlmcp_json_rpc),
    ?assertEqual(0, Count).

%% With no ETS table the count must also be 0 (safe fallback).
in_flight_count_no_table_test() ->
    Table = erlmcp_request_registry,
    case ets:info(Table) of
        undefined -> ok;
        _         -> ets:delete(Table)
    end,
    Count = erlmcp_code_reload:get_in_flight_count(erlmcp_json_rpc),
    ?assertEqual(0, Count).

%%%===================================================================
%%% T020 — check_state_compatibility tests
%%%===================================================================

%% A module with no state_fields attribute returns ok (compatible by assumption).
check_state_compatibility_no_annotation_test() ->
    %% erlmcp_json_rpc has no gen_server state_fields annotation
    %% so check_state_compatibility falls through to ok.
    Result = erlmcp_code_reload:check_state_compatibility(erlmcp_json_rpc),
    ?assertEqual(ok, Result).
