%%%-------------------------------------------------------------------
%% @doc
%% Plan Specification Loader
%%
%% Loads plan specifications from JSON files and converts them to
%% erlmcp_marketplace_copy:plan_spec() records.
%%
%% Functions:
%%   - load_plan/1 - Load plan by name (team | enterprise | gov)
%%   - load_plan_file/1 - Load plan from specific JSON file path
%%   - list_available_plans/0 - List available plan specs in config/
%%
%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_plan_loader).

-export([
    load_plan/1,
    load_plan_file/1,
    list_available_plans/0
]).

-type plan_name() :: team | enterprise | gov.
-type load_result() :: {ok, Plan :: erlmcp_marketplace_copy:plan_spec()} | {error, Reason :: atom()}.

%% ===================================================================
%% Public API
%% ===================================================================

%%--------------------------------------------------------------------
%% @doc Load plan by name.
%%
%% Looks for config/plan_NAME.json file relative to project root.
%% @end
%%--------------------------------------------------------------------
-spec load_plan(PlanName :: plan_name()) ->
    load_result().

load_plan(PlanName) ->
    FileName = filename:join([get_config_dir(), "plan_" ++ atom_to_list(PlanName) ++ ".json"]),
    load_plan_file(FileName).

%%--------------------------------------------------------------------
%% @doc Load plan from specific file path.
%%
%% Parses JSON file and converts to plan_spec() map.
%%
%% Expected JSON structure:
%%   {
%%     "name": "team",
%%     "tier": "team",
%%     "envelope": { ... },
%%     "sla": { ... },
%%     "refusal": { ... },
%%     "evidence": { ... },
%%     "pricing": { ... }
%%   }
%%
%% @end
%%--------------------------------------------------------------------
-spec load_plan_file(FilePath :: string()) ->
    load_result().

load_plan_file(FilePath) ->
    case file:read_file(FilePath) of
        {ok, Json} ->
            parse_plan_json(Json);
        {error, enoent} ->
            {error, plan_file_not_found};
        {error, _Reason} ->
            {error, plan_file_read_failed}
    end.

%%--------------------------------------------------------------------
%% @doc List available plan specifications in config/ directory.
%% @end
%%--------------------------------------------------------------------
-spec list_available_plans() ->
    {ok, Plans :: [atom()]} | {error, Reason :: atom()}.

list_available_plans() ->
    ConfigDir = get_config_dir(),
    case file:list_dir(ConfigDir) of
        {ok, Files} ->
            Plans = [
                parse_plan_filename(F)
             || F <- Files,
                is_plan_file(F)
            ],
            {ok, lists:sort(lists:filtermap(fun(X) -> X end, Plans))};
        {error, _Reason} ->
            {error, config_dir_not_found}
    end.

%% ===================================================================
%% Internal Functions
%% ===================================================================

-spec get_config_dir() ->
    string().

get_config_dir() ->
    case code:priv_dir(erlmcp) of
        {error, _} ->
            %% Fallback to relative path for development
            filename:join([get_project_root(), "config"]);
        PrivDir ->
            filename:join([filename:dirname(PrivDir), "config"])
    end.

-spec get_project_root() ->
    string().

get_project_root() ->
    case application:get_env(erlmcp, project_root) of
        {ok, Root} ->
            Root;
        undefined ->
            %% Try to find project root from module location
            case code:which(erlmcp_app) of
                Path when is_list(Path) ->
                    %% Path should be /path/to/erlmcp/_build/...
                    %% Walk back to find project root
                    Parts = filename:split(Path),
                    case find_project_root(Parts) of
                        {ok, Root} -> Root;
                        error -> "."
                    end;
                _ ->
                    "."
            end
    end.

-spec find_project_root(Parts :: [string()]) ->
    {ok, string()} | error.

find_project_root(Parts) ->
    case lists:member("_build", Parts) of
        true ->
            {Prefix, [_Build | _Rest]} = lists:split(
                length(Parts) - length(lists:dropwhile(fun(X) -> X =/= "_build" end, Parts)),
                Parts
            ),
            {ok, filename:join(Prefix)};
        false ->
            error
    end.

-spec is_plan_file(Filename :: string()) ->
    boolean().

is_plan_file(Filename) ->
    case filename:extension(Filename) of
        ".json" ->
            case filename:basename(Filename, ".json") of
                Basename ->
                    string:prefix(Basename, "plan_") =:= nomatch
            end;
        _ ->
            false
    end.

-spec parse_plan_filename(Filename :: string()) ->
    {ok, atom()} | error.

parse_plan_filename(Filename) ->
    case filename:basename(Filename, ".json") of
        "plan_" ++ PlanName ->
            {ok, list_to_atom(PlanName)};
        _ ->
            error
    end.

-spec parse_plan_json(JsonBinary :: binary()) ->
    load_result().

parse_plan_json(JsonBinary) ->
    try jsx:decode(JsonBinary, [return_maps]) of
        Json ->
            convert_json_to_plan_spec(Json)
    catch
        error:_ ->
            {error, json_parse_failed}
    end.

-spec convert_json_to_plan_spec(JsonMap :: map()) ->
    load_result().

convert_json_to_plan_spec(Json) ->
    try
        PlanSpec = #{
            name => get_atom(Json, <<"name">>),
            tier => get_atom(Json, <<"tier">>),
            envelope => convert_envelope(maps:get(<<"envelope">>, Json)),
            sla => convert_sla(maps:get(<<"sla">>, Json)),
            refusal => convert_refusal(maps:get(<<"refusal">>, Json)),
            evidence => convert_evidence(maps:get(<<"evidence">>, Json)),
            pricing => convert_pricing(maps:get(<<"pricing">>, Json))
        },
        {ok, PlanSpec}
    catch
        error:_ ->
            {error, plan_spec_conversion_failed}
    end.

-spec convert_envelope(EnvelopeJson :: map()) ->
    erlmcp_marketplace_copy:envelope_spec().

convert_envelope(Json) ->
    #{
        requests_per_second => get_int(Json, <<"requests_per_second">>),
        concurrent_connections => get_int(Json, <<"concurrent_connections">>),
        queue_depth => get_int(Json, <<"queue_depth">>),
        message_size_bytes => get_int(Json, <<"message_size_bytes">>)
    }.

-spec convert_sla(SLAJson :: map()) ->
    erlmcp_marketplace_copy:sla_spec().

convert_sla(Json) ->
    #{
        latency_p99_ms => get_int(Json, <<"latency_p99_ms">>),
        failover_seconds => get_int(Json, <<"failover_seconds">>),
        uptime_percent => get_float(Json, <<"uptime_percent">>),
        incident_response_hours => get_int(Json, <<"incident_response_hours">>)
    }.

-spec convert_refusal(RefusalJson :: map()) ->
    erlmcp_marketplace_copy:refusal_spec().

convert_refusal(Json) ->
    #{
        at_capacity => get_binary(Json, <<"at_capacity">>),
        rate_limit_exceeded => get_binary(Json, <<"rate_limit_exceeded">>),
        message_too_large => get_binary(Json, <<"message_too_large">>),
        invalid_protocol => get_binary(Json, <<"invalid_protocol">>)
    }.

-spec convert_evidence(EvidenceJson :: map()) ->
    erlmcp_marketplace_copy:evidence_spec().

convert_evidence(Json) ->
    #{
        sbom_included => get_bool(Json, <<"sbom_included">>),
        provenance_included => get_bool(Json, <<"provenance_included">>),
        chaos_matrix_included => get_bool(Json, <<"chaos_matrix_included">>),
        benchmark_results_included => get_bool(Json, <<"benchmark_results_included">>)
    }.

-spec convert_pricing(PricingJson :: map()) ->
    erlmcp_marketplace_copy:pricing_spec().

convert_pricing(Json) ->
    #{
        model => get_atom(Json, <<"model">>),
        base_cost_usd => get_int(Json, <<"base_cost_usd">>),
        deployment_unit => get_binary(Json, <<"deployment_unit">>)
    }.

-spec get_atom(Map :: map(), Key :: binary()) ->
    atom().

get_atom(Map, Key) ->
    Value = maps:get(Key, Map),
    binary_to_atom(Value, utf8).

-spec get_binary(Map :: map(), Key :: binary()) ->
    binary().

get_binary(Map, Key) ->
    maps:get(Key, Map).

-spec get_int(Map :: map(), Key :: binary()) ->
    integer().

get_int(Map, Key) ->
    maps:get(Key, Map).

-spec get_float(Map :: map(), Key :: binary()) ->
    float().

get_float(Map, Key) ->
    Value = maps:get(Key, Map),
    case Value of
        V when is_float(V) -> V;
        V when is_integer(V) -> float(V)
    end.

-spec get_bool(Map :: map(), Key :: binary()) ->
    boolean().

get_bool(Map, Key) ->
    maps:get(Key, Map).
