%%% erlmcp_pricing_state.erl
%%% State management for pricing tier and upgrade tracking
-module(erlmcp_pricing_state).

%% Public API
-export([
    get_current_plan/0,
    set_current_plan/1,
    get_last_upgrade_time/1,
    set_last_upgrade_time/2,
    get_certification_valid/1,
    set_certification_valid/2,
    get_upgrade_timestamp/0,
    set_upgrade_timestamp/1,
    get_all_state/0,
    get_plan/1,
    get_all_plans/0,
    get_usage/1,
    set_plans/1
]).

-include("erlmcp.hrl").

%% ETS table for state storage
-define(STATE_TABLE, erlmcp_pricing_state_table).

%% === Initialization ===

%% Ensure ETS table exists
ensure_table() ->
    case ets:info(?STATE_TABLE) of
        undefined ->
            ets:new(?STATE_TABLE, [
                named_table,
                public,
                {write_concurrency, true},
                {read_concurrency, true}
            ]);
        _ ->
            ok
    end.

%% === Public API ===

%% @doc Get current pricing plan
-spec get_current_plan() -> {ok, team | enterprise | gov} | {error, not_found}.
get_current_plan() ->
    ensure_table(),
    case ets:lookup(?STATE_TABLE, current_plan) of
        [{_, Plan}] -> {ok, Plan};
        [] -> {error, not_found}
    end.

%% @doc Set current pricing plan
-spec set_current_plan(Plan :: team | enterprise | gov) -> ok.
set_current_plan(Plan) ->
    ensure_table(),
    ets:insert(?STATE_TABLE, {current_plan, Plan}).

%% @doc Get last upgrade time for a plan
-spec get_last_upgrade_time(Plan :: team | enterprise | gov) -> integer() | not_found.
get_last_upgrade_time(Plan) ->
    ensure_table(),
    Key = {last_upgrade_time, Plan},
    case ets:lookup(?STATE_TABLE, Key) of
        [{_, Time}] -> Time;
        [] -> not_found
    end.

%% @doc Set last upgrade time for a plan
-spec set_last_upgrade_time(Plan :: team | enterprise | gov, Time :: integer()) -> ok.
set_last_upgrade_time(Plan, Time) ->
    ensure_table(),
    ets:insert(?STATE_TABLE, {{last_upgrade_time, Plan}, Time}).

%% @doc Get whether plan certification is valid
-spec get_certification_valid(Plan :: team | enterprise | gov) -> boolean().
get_certification_valid(Plan) ->
    ensure_table(),
    Key = {certification_valid, Plan},
    case ets:lookup(?STATE_TABLE, Key) of
        [{_, Valid}] -> Valid;
        [] -> false
    end.

%% @doc Set plan certification validity
-spec set_certification_valid(Plan :: team | enterprise | gov, Valid :: boolean()) -> ok.
set_certification_valid(Plan, Valid) ->
    ensure_table(),
    ets:insert(?STATE_TABLE, {{certification_valid, Plan}, Valid}).

%% @doc Get last upgrade timestamp
-spec get_upgrade_timestamp() -> integer() | not_found.
get_upgrade_timestamp() ->
    ensure_table(),
    case ets:lookup(?STATE_TABLE, upgrade_timestamp) of
        [{_, Timestamp}] -> Timestamp;
        [] -> not_found
    end.

%% @doc Set upgrade timestamp
-spec set_upgrade_timestamp(Timestamp :: integer()) -> ok.
set_upgrade_timestamp(Timestamp) ->
    ensure_table(),
    ets:insert(?STATE_TABLE, {upgrade_timestamp, Timestamp}).

%% @doc Get all pricing state
-spec get_all_state() -> map().
get_all_state() ->
    ensure_table(),
    Entries = ets:tab2list(?STATE_TABLE),
    maps:from_list(Entries).

%% @doc Get pricing plan by ID
%% @doc Get pricing plan by tier atom
%% Returns plan data from erlmcp_pricing_plan module
-spec get_plan(Plan :: atom()) -> {ok, map()} | {error, not_found}.
get_plan(Plan) when is_atom(Plan) ->
    case erlmcp_pricing_plan:load_plan(Plan) of
        {ok, PlanSpec} ->
            {ok, PlanSpec};
        {error, {invalid_tier, _}} ->
            {error, not_found};
        {error, {file_not_found, _}} ->
            {error, not_found};
        Error ->
            Error
    end.

%% @doc Get all available pricing plans
%% Returns map of all loaded pricing plans
-spec get_all_plans() -> {ok, map()} | {error, term()}.
get_all_plans() ->
    case erlmcp_pricing_plan:list_available_plans() of
        {ok, Tiers} ->
            PlansMap = lists:foldl(
                fun(Tier, Acc) ->
                    case erlmcp_pricing_plan:load_plan(Tier) of
                        {ok, PlanSpec} ->
                            maps:put(Tier, PlanSpec, Acc);
                        {error, _} ->
                            Acc
                    end
                end,
                #{},
                Tiers
            ),
            {ok, PlansMap};
        Error ->
            Error
    end.

%% @doc Get usage metrics for a user
%% Returns current usage statistics for the specified user
-spec get_usage(User :: binary()) -> {ok, map()} | {error, term()}.
get_usage(User) when is_binary(User) ->
    ensure_table(),
    Key = {usage, User},
    case ets:lookup(?STATE_TABLE, Key) of
        [{_, Usage}] when is_map(Usage) ->
            {ok, Usage};
        [] ->
            %% Return default zero usage if not found
            {ok, #{
                user => User,
                requests => 0,
                connections => 0,
                memory_bytes => 0,
                last_updated => erlang:system_time(second)
            }}
    end.

%% @doc Set pricing plans in state
%% Stores map of all pricing plans for quick access
-spec set_plans(Plans :: map()) -> ok.
set_plans(Plans) when is_map(Plans) ->
    ensure_table(),
    ets:insert(?STATE_TABLE, {plans_cache, Plans}),
    ok.
