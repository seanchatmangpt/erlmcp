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
    % TODO: Implement pricing upgrade logic
    io:format("Upgrade not yet implemented: ~s to ~p~n", [User, NewPlan]),
    {error, not_implemented}.
