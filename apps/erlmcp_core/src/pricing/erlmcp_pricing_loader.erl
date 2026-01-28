-module(erlmcp_pricing_loader).
-export([load_plan/1, load_all_plans/0, reload/0]).

%% @doc Load pricing plan from file
-spec load_plan(Plan :: atom()) -> {ok, map()} | {error, term()}.
load_plan(Plan) ->
    SchemaPath = code:priv_dir(erlmcp_core) ++ "/schemas/pricing_plan.schema.json",
    PlanFile = atom_to_list(Plan) ++ ".json",
    case file:read_file(PlanFile) of
        {ok, Binary} ->
            case jsx:decode(Binary, [return_maps]) of
                PlanData when is_map(PlanData) ->
                    case jesse:validate_with_schema(SchemaPath, PlanData) of
                        {ok, Validated} -> {ok, Validated};
                        {error, Reasons} -> {error, {validation_failed, Reasons}}
                    end;
                _ ->
                    {error, invalid_json}
            end;
        {error, Reason} ->
            {error, {file_error, Reason}}
    end.

%% @doc Load all pricing plans from priv/plans/
-spec load_all_plans() -> {ok, #{atom() => map()}} | {error, term()}.
load_all_plans() ->
    PlansDir = code:priv_dir(erlmcp_core) ++ "/plans",
    case file:list_dir(PlansDir) of
        {ok, Files} ->
            Plans = lists:foldl(fun(File, Acc) ->
                case filename:extension(File) of
                    ".json" ->
                        PlanName = list_to_atom(filename:basename(File, ".json")),
                        case load_plan(PlansDir ++ "/" ++ File) of
                            {ok, Plan} -> maps:put(PlanName, Plan, Acc);
                            _ -> Acc
                        end;
                    _ ->
                        Acc
                end
            end, #{}, Files),
            {ok, Plans};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Reload all pricing plans
-spec reload() -> ok | {error, term()}.
reload() ->
    case load_all_plans() of
        {ok, Plans} ->
            erlmcp_pricing_state:set_plans(Plans),
            ok;
        {error, Reason} ->
            {error, Reason}
    end.
