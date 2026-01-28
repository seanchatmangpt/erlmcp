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
    get_all_state/0
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
