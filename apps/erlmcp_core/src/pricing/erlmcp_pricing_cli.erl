-module(erlmcp_pricing_cli).
-export([show_plan/1, list_plans/0, check_usage/1, upgrade/2]).

%% @doc Show pricing plan details
-spec show_plan(Plan :: atom()) -> ok | {error, term()}.
show_plan(Plan) ->
    case erlmcp_pricing_state:get_plan(Plan) of
        {ok, PlanData} ->
            io:format("Plan: ~p~n", [Plan]),
            io:format("~p~n", [PlanData]),
            ok;
        {error, Reason} ->
            io:format("Error: ~p~n", [Reason]),
            {error, Reason}
    end.

%% @doc List all available pricing plans
-spec list_plans() -> ok.
list_plans() ->
    case erlmcp_pricing_state:get_all_plans() of
        {ok, Plans} ->
            io:format("Available plans:~n"),
            maps:fold(fun(Name, _Plan, _) ->
                io:format("  - ~p~n", [Name])
            end, ok, Plans);
        {error, Reason} ->
            io:format("Error: ~p~n", [Reason])
    end,
    ok.

%% @doc Check current usage against plan
-spec check_usage(User :: binary()) -> ok | {error, term()}.
check_usage(User) ->
    case erlmcp_pricing_state:get_usage(User) of
        {ok, Usage} ->
            io:format("Usage for ~s:~n", [User]),
            io:format("~p~n", [Usage]),
            ok;
        {error, Reason} ->
            io:format("Error: ~p~n", [Reason]),
            {error, Reason}
    end.

%% @doc Upgrade user to new plan
-spec upgrade(User :: binary(), NewPlan :: atom()) -> ok | {error, term()}.
upgrade(User, NewPlan) ->
    TierId = plan_to_tier_id(NewPlan),
    case erlmcp_pricing:upgrade(User, TierId) of
        {ok, Result} ->
            TierName = maps:get(tier_name, Result),
            Payment = maps:get(payment, Result),
            Amount = maps:get(amount, Payment),
            FormattedPrice = erlmcp_pricing_util:format_price(Amount),
            io:format("Upgraded ~s to ~s tier~n", [User, TierName]),
            io:format("  Amount charged: ~s~n", [FormattedPrice]),
            io:format("  Transaction ID: ~s~n", [maps:get(transaction_id, Payment)]),
            ok;
        {error, {payment_failed, _Reason}} ->
            io:format("Payment failed~n"),
            {error, payment_failed};
        {error, already_on_tier} ->
            io:format("User already on this tier~n"),
            {error, already_on_tier};
        {error, invalid_tier_upgrade} ->
            io:format("Invalid upgrade: can only upgrade to higher tiers~n"),
            {error, invalid_tier_upgrade};
        {error, Reason} ->
            io:format("Upgrade failed: ~p~n", [Reason]),
            {error, Reason}
    end.

%% @private
plan_to_tier_id(free) -> 0;
plan_to_tier_id(pro) -> 1;
plan_to_tier_id(enterprise) -> 2;
plan_to_tier_id(_) -> error(invalid_plan).
