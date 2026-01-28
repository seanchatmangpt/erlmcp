-module(erlmcp_pricing_http).
-export([handle_request/2]).

%% @doc Handle HTTP pricing API requests
-spec handle_request(Method :: binary(), Path :: binary()) -> {ok, map()} | {error, term()}.
handle_request(<<"GET">>, <<"/api/pricing/plans">>) ->
    case erlmcp_pricing_state:get_all_plans() of
        {ok, Plans} -> {ok, #{<<"plans">> => Plans}};
        Error -> Error
    end;

handle_request(<<"GET">>, <<"/api/pricing/plans/", Plan/binary>>) ->
    PlanAtom = binary_to_atom(Plan, utf8),
    case erlmcp_pricing_state:get_plan(PlanAtom) of
        {ok, PlanData} -> {ok, PlanData};
        Error -> Error
    end;

handle_request(<<"POST">>, <<"/api/pricing/upgrade">>) ->
    {error, not_implemented};

handle_request(<<"GET">>, <<"/api/pricing/usage/", User/binary>>) ->
    case erlmcp_pricing_state:get_usage(User) of
        {ok, Usage} -> {ok, #{<<"usage">> => Usage}};
        Error -> Error
    end;

handle_request(_Method, _Path) ->
    {error, not_found}.
