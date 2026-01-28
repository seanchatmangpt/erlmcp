%%%-------------------------------------------------------------------
%% @doc
%% Plan Specification Loader
%%
%% Loads plan specifications from JSON files.
%%
%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_plan_loader).

-export([
    load_plan/1,
    load_plan_file/1
]).

-type plan_name() :: team | enterprise | gov.
-type load_result() :: {ok, Plan :: map()} | {error, Reason :: atom()}.

-spec load_plan(PlanName :: plan_name()) -> load_result().

load_plan(team) ->
    load_plan_file("plans/team.plan.json");
load_plan(enterprise) ->
    load_plan_file("plans/enterprise.plan.json");
load_plan(gov) ->
    load_plan_file("plans/gov.plan.json");
load_plan(_) ->
    {error, invalid_plan}.

-spec load_plan_file(FilePath :: string()) -> load_result().

load_plan_file(FilePath) ->
    case file:read_file(FilePath) of
        {ok, Content} ->
            try
                PlanSpec = jsx:decode(Content, [return_maps]),
                {ok, PlanSpec}
            catch
                _:_ -> {error, json_decode_failed}
            end;
        {error, enoent} ->
            {error, plan_file_not_found};
        {error, Reason} ->
            {error, Reason}
    end.
