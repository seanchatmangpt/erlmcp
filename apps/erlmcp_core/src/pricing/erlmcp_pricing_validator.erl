-module(erlmcp_pricing_validator).

-export([validate_usage/2, check_limit/3, validate_upgrade_path/2]).

%% @doc Validate usage against plan limits
-spec validate_usage(Plan :: map(), Usage :: map()) -> ok | {error, term()}.
validate_usage(#{<<"limits">> := Limits}, Usage) ->
    validate_limits(maps:to_list(Limits), Usage);
validate_usage(_, _) ->
    {error, no_limits_defined}.

validate_limits([], _Usage) ->
    ok;
validate_limits([{Key, Limit} | Rest], Usage) ->
    case maps:get(Key, Usage, 0) of
        Value when Value =< Limit ->
            validate_limits(Rest, Usage);
        Value ->
            {error, {limit_exceeded, Key, Value, Limit}}
    end.

%% @doc Check if specific limit is exceeded
-spec check_limit(Limit :: atom(), Current :: integer(), Max :: integer()) -> ok | {error, term()}.
check_limit(_Limit, Current, Max) when Current =< Max ->
    ok;
check_limit(Limit, Current, Max) ->
    {error, {limit_exceeded, Limit, Current, Max}}.

%% @doc Validate upgrade path between plans
-spec validate_upgrade_path(FromPlan :: atom(), ToPlan :: atom()) -> ok | {error, term()}.
validate_upgrade_path(free, _ToPlan) ->
    ok;  % Can upgrade from free to anything
validate_upgrade_path(pro, enterprise) ->
    ok;
validate_upgrade_path(enterprise, _) ->
    {error, cannot_upgrade_from_enterprise};
validate_upgrade_path(_From, _To) ->
    {error, invalid_upgrade_path}.
