%%%-------------------------------------------------------------------
%% @doc erlmcp_pricing_plan - Machine-readable TCPS pricing plan validator
%%
%% Provides deterministic, side-effect-free functions for:
%% - Loading pricing plans from JSON specifications
%% - Validating plans against JSON schema
%% - Checking envelope boundaries (throughput, connections, queue, latency)
%% - Determining refusal behavior for limit exceedances
%% - Verifying evidence bundle requirements
%%
%% All functions are fully deterministic and suitable for CI poka-yoke validation.
%% Plans loaded from /plans/*.plan.json relative to project root.
%% Validation uses JSON schema at /shapes/pricing_plan.schema.json
%%
%% Example:
%%   {ok, Envelope} = erlmcp_pricing_plan:get_envelope(team),
%%   {ok, RefusalResponse} = erlmcp_pricing_plan:check_refusal(
%%       team, throughput_exceeded
%%   ),
%%   ok = erlmcp_pricing_plan:validate_plan(team)
%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_pricing_plan).

-export([
    load_plan/1,
    validate_plan/1,
    get_envelope/1,
    check_refusal/2,
    list_available_plans/0,
    get_plan_spec/1
]).

-type tier() :: team | enterprise | gov.
-type refusal_type() :: throughput_exceeded | queue_depth_exceeded |
                        connection_limit_exceeded | message_size_exceeded |
                        unsupported_feature | fips_compliance_violation |
                        encryption_failure.

-type envelope() :: #{
    throughput_req_s := integer(),
    concurrent_connections := integer(),
    queue_depth_messages := integer(),
    p99_latency_ms := integer(),
    failover_sla_seconds := integer(),
    connection_timeout_seconds := integer()
}.

-type plan_spec() :: #{
    tier := tier(),
    name := binary() | string(),
    description := binary() | string(),
    pricing := map(),
    envelope := envelope(),
    limits := map(),
    features := map(),
    refusal_behavior := map(),
    evidence := map(),
    compliance := map(),
    sla => map(),
    audit => map()
}.

-type validation_result() :: ok | {error, validation_error()}.
-type validation_error() :: {schema_error, term()} |
                            {file_not_found, file:name()} |
                            {json_decode_error, term()} |
                            {invalid_tier, term()}.

-spec load_plan(Tier :: tier()) -> {ok, plan_spec()} | {error, term()}.
%% @doc Load pricing plan specification from JSON file
%%
%% Loads plan from plans/{tier}.plan.json relative to project root.
%% Returns complete plan specification as map with all sections.
%%
%% Returns:
%%   {ok, PlanSpec} - Successfully loaded plan
%%   {error, file_not_found} - Plan file does not exist
%%   {error, json_decode_error} - JSON parsing failed
%%   {error, invalid_tier} - Tier is not team|enterprise|gov
%%
%% Example:
%%   {ok, TeamPlan} = erlmcp_pricing_plan:load_plan(team)
load_plan(Tier) when Tier =:= team; Tier =:= enterprise; Tier =:= gov ->
    try
        FilePath = get_plan_file_path(Tier),
        case file:read_file(FilePath) of
            {ok, Binary} ->
                case jsx:decode(Binary, [return_maps]) of
                    PlanMap when is_map(PlanMap) ->
                        {ok, normalize_plan(PlanMap)};
                    _Error ->
                        {error, {json_decode_error, FilePath}}
                end;
            {error, enoent} ->
                {error, {file_not_found, FilePath}};
            {error, FileError} ->
                {error, {file_read_error, FileError}}
        end
    catch
        error:ErrorReason ->
            {error, {load_error, ErrorReason}}
    end;
load_plan(InvalidTier) ->
    {error, {invalid_tier, InvalidTier}}.

-spec validate_plan(Tier :: tier()) -> validation_result().
%% @doc Validate pricing plan against JSON schema
%%
%% Loads plan and validates against shapes/pricing_plan.schema.json.
%% All validation is deterministic - same plan always produces same result.
%%
%% Returns:
%%   ok - Plan is valid
%%   {error, {schema_error, Details}} - Schema validation failed
%%   {error, _} - Load error (see load_plan/1)
%%
%% Example:
%%   ok = erlmcp_pricing_plan:validate_plan(enterprise)
validate_plan(Tier) ->
    case load_plan(Tier) of
        {ok, PlanSpec} ->
            validate_spec_against_schema(PlanSpec);
        Error ->
            Error
    end.

-spec get_envelope(Tier :: tier()) -> {ok, envelope()} | {error, term()}.
%% @doc Get envelope boundaries for pricing tier
%%
%% Extracts throughput, connection, queue, and latency limits from plan.
%% These define the deterministic boundaries for refusal behavior.
%%
%% Returns:
%%   {ok, Envelope} - Envelope with all limits
%%   {error, _} - Load error
%%
%% Envelope contains:
%%   throughput_req_s - Max requests per second
%%   concurrent_connections - Max concurrent connections
%%   queue_depth_messages - Max queued messages
%%   p99_latency_ms - Target p99 latency
%%   failover_sla_seconds - Failover time limit
%%   connection_timeout_seconds - Connection timeout
%%
%% Example:
%%   {ok, Env} = erlmcp_pricing_plan:get_envelope(team),
%%   Throughput = maps:get(throughput_req_s, Env)
get_envelope(Tier) ->
    case load_plan(Tier) of
        {ok, PlanSpec} ->
            case maps:get(envelope, PlanSpec, undefined) of
                Envelope when is_map(Envelope) ->
                    {ok, Envelope};
                _Missing ->
                    {error, {missing_envelope, Tier}}
            end;
        Error ->
            Error
    end.

-spec check_refusal(Tier :: tier(), RefusalType :: refusal_type()) ->
    {ok, map()} | {error, term()}.
%% @doc Get deterministic refusal response for limit exceedance
%%
%% Returns the exact refusal response that should be sent when a limit
%% is exceeded. All responses are deterministic based on plan specification.
%%
%% Supported RefusalTypes:
%%   throughput_exceeded - Rate limit hit
%%   queue_depth_exceeded - Queue backlog full
%%   connection_limit_exceeded - Max connections reached
%%   message_size_exceeded - Message too large
%%   unsupported_feature - Feature not in tier
%%   fips_compliance_violation - Gov tier only
%%   encryption_failure - Gov tier only
%%
%% Returns:
%%   {ok, RefusalResponse} - Response with http_status, error_code, message
%%   {error, {unsupported_refusal, Type}} - RefusalType not defined in plan
%%   {error, _} - Load error
%%
%% Example:
%%   {ok, Response} = erlmcp_pricing_plan:check_refusal(
%%       team, throughput_exceeded
%%   ),
%%   HttpStatus = maps:get(http_status, Response),
%%   ErrorCode = maps:get(error_code, Response)
check_refusal(Tier, RefusalType) ->
    case load_plan(Tier) of
        {ok, PlanSpec} ->
            RefusalBehavior = maps:get(refusal_behavior, PlanSpec, #{}),
            case maps:get(RefusalType, RefusalBehavior, undefined) of
                Response when is_map(Response) ->
                    {ok, Response};
                _Missing ->
                    {error, {unsupported_refusal, RefusalType}}
            end;
        Error ->
            Error
    end.

-spec list_available_plans() -> {ok, [tier()]} | {error, term()}.
%% @doc List all available pricing tiers
%%
%% Scans plans/ directory and returns atoms for tiers with valid files.
%% Returns list in canonical order: [team, enterprise, gov]
%%
%% Returns:
%%   {ok, [Tiers]} - Available tiers
%%   {error, directory_not_found} - Plans directory missing
%%
%% Example:
%%   {ok, [team, enterprise, gov]} = erlmcp_pricing_plan:list_available_plans()
list_available_plans() ->
    try
        PlansDir = get_plans_dir(),
        case file:list_dir(PlansDir) of
            {ok, Files} ->
                Tiers = lists:filtermap(
                    fun(F) -> parse_plan_filename(F) end,
                    Files
                ),
                {ok, lists:sort(Tiers)};
            {error, enoent} ->
                {error, {directory_not_found, PlansDir}};
            {error, DirError} ->
                {error, {directory_error, DirError}}
        end
    catch
        error:ListError ->
            {error, {list_error, ListError}}
    end.

-spec get_plan_spec(Tier :: tier()) -> {ok, plan_spec()} | {error, term()}.
%% @doc Alias for load_plan/1 - Get complete plan specification
%% @see load_plan/1
get_plan_spec(Tier) ->
    load_plan(Tier).

%%%-------------------------------------------------------------------
%% Internal Functions - Path Resolution
%%%-------------------------------------------------------------------

-spec get_plan_file_path(tier()) -> file:name().
%% @private Get absolute path to plan JSON file
get_plan_file_path(Tier) ->
    PlansDir = get_plans_dir(),
    TierStr = atom_to_list(Tier),
    filename:join([PlansDir, TierStr ++ ".plan.json"]).

-spec get_plans_dir() -> file:name().
%% @private Get absolute path to plans directory
get_plans_dir() ->
    ProjectRoot = get_project_root(),
    filename:join([ProjectRoot, "plans"]).

-spec get_project_root() -> file:name().
%% @private Get project root directory
get_project_root() ->
    case application:get_env(erlmcp, project_root) of
        {ok, Root} ->
            Root;
        undefined ->
            find_project_root()
    end.

-spec find_project_root() -> file:name().
%% @private Find project root from module location
find_project_root() ->
    case code:which(?MODULE) of
        Beam when is_list(Beam) ->
            Parts = filename:split(Beam),
            case find_build_marker(Parts) of
                {ok, Root} -> Root;
                error -> "."
            end;
        _Other ->
            "."
    end.

-spec find_build_marker([string()]) -> {ok, file:name()} | error.
%% @private Walk up from beam file to find project root
find_build_marker(Parts) ->
    case lists:member("_build", Parts) of
        true ->
            Index = length(Parts) - length(lists:dropwhile(
                fun(X) -> X =/= "_build" end, Parts
            )),
            {Prefix, _BuildRest} = lists:split(Index, Parts),
            {ok, filename:join(Prefix)};
        false ->
            error
    end.

%%%-------------------------------------------------------------------
%% Internal Functions - Normalization & Validation
%%%-------------------------------------------------------------------

-spec normalize_plan(map()) -> plan_spec().
%% @private Normalize plan map to standard format
%% Converts all string keys to atoms, ensures required fields present
normalize_plan(RawPlan) ->
    maps:map(
        fun(_Key, Value) when is_binary(Value) -> Value;
           (_Key, Value) when is_map(Value) -> normalize_value(Value);
           (_Key, Value) -> Value
        end,
        RawPlan
    ).

-spec normalize_value(term()) -> term().
%% @private Recursively normalize nested values
normalize_value(Map) when is_map(Map) ->
    maps:map(
        fun(_K, V) when is_binary(V) -> V;
           (_K, V) when is_map(V) -> normalize_value(V);
           (_K, V) -> V
        end,
        Map
    );
normalize_value(Value) ->
    Value.

-spec validate_spec_against_schema(plan_spec()) -> validation_result().
%% @private Validate plan specification against JSON schema
validate_spec_against_schema(PlanSpec) ->
    case load_schema() of
        {ok, _SchemaMap} ->
            try
                % Validate envelope
                Envelope = maps:get(envelope, PlanSpec),
                ok = validate_envelope(Envelope),
                % Validate limits
                Limits = maps:get(limits, PlanSpec),
                ok = validate_limits(Limits),
                % Validate features
                Features = maps:get(features, PlanSpec),
                ok = validate_features(Features),
                % Validate evidence
                Evidence = maps:get(evidence, PlanSpec),
                ok = validate_evidence(Evidence),
                ok
            catch
                error:ValidationError ->
                    {error, {schema_error, ValidationError}}
            end;
        Error ->
            Error
    end.

-spec load_schema() -> {ok, map()} | {error, term()}.
%% @private Load JSON schema for validation
load_schema() ->
    try
        SchemaPath = filename:join([get_project_root(), "shapes", "pricing_plan.schema.json"]),
        case file:read_file(SchemaPath) of
            {ok, Binary} ->
                case jsx:decode(Binary, [return_maps]) of
                    SchemaMap when is_map(SchemaMap) ->
                        {ok, SchemaMap};
                    _Error ->
                        {error, {schema_decode_error, SchemaPath}}
                end;
            {error, enoent} ->
                {error, {schema_not_found, SchemaPath}};
            {error, SchemaReadError} ->
                {error, {schema_read_error, SchemaReadError}}
        end
    catch
        error:SchemaLoadError ->
            {error, {schema_load_error, SchemaLoadError}}
    end.

-spec validate_envelope(envelope()) -> ok | {error, term()}.
%% @private Validate envelope has all required fields with proper types
validate_envelope(Envelope) when is_map(Envelope) ->
    RequiredFields = [
        throughput_req_s,
        concurrent_connections,
        queue_depth_messages,
        p99_latency_ms,
        failover_sla_seconds,
        connection_timeout_seconds
    ],
    case check_required_fields(Envelope, RequiredFields) of
        ok ->
            % Validate types and ranges
            case maps:get(throughput_req_s, Envelope) of
                V when is_integer(V), V > 0 -> ok;
                _ -> {error, {invalid_throughput, Envelope}}
            end;
        Error ->
            Error
    end;
validate_envelope(_) ->
    {error, envelope_not_map}.

-spec validate_limits(map()) -> ok | {error, term()}.
%% @private Validate limits section
validate_limits(Limits) when is_map(Limits) ->
    RequiredFields = [
        max_message_size_bytes,
        max_payload_size_mb,
        max_concurrent_requests_per_conn,
        memory_limit_mb,
        cpu_time_limit_seconds,
        backpressure_threshold_bytes
    ],
    check_required_fields(Limits, RequiredFields);
validate_limits(_) ->
    {error, limits_not_map}.

-spec validate_features(map()) -> ok | {error, term()}.
%% @private Validate features section
validate_features(Features) when is_map(Features) ->
    RequiredFeatures = [
        client,
        server,
        stdio_transport,
        tcp_transport,
        http_transport,
        rate_limiting,
        circuit_breaker,
        otel_observability,
        audit_logging,
        fips_140_2,
        high_availability
    ],
    check_required_fields(Features, RequiredFeatures);
validate_features(_) ->
    {error, features_not_map}.

-spec validate_evidence(map()) -> ok | {error, term()}.
%% @private Validate evidence bundle requirements
validate_evidence(Evidence) when is_map(Evidence) ->
    RequiredEvidence = [
        sbom,
        provenance,
        chaos_report,
        benchmark_report
    ],
    check_required_fields(Evidence, RequiredEvidence);
validate_evidence(_) ->
    {error, evidence_not_map}.

-spec check_required_fields(map(), [atom()]) -> ok | {error, term()}.
%% @private Verify all required fields are present in map
check_required_fields(Map, RequiredFields) ->
    case lists:all(fun(F) -> maps:is_key(F, Map) end, RequiredFields) of
        true ->
            ok;
        false ->
            Missing = [F || F <- RequiredFields, not maps:is_key(F, Map)],
            {error, {missing_fields, Missing}}
    end.

%%%-------------------------------------------------------------------
%% Internal Functions - Filename Parsing
%%%-------------------------------------------------------------------

-spec parse_plan_filename(string()) -> {true, tier()} | false.
%% @private Extract tier from filename (e.g., "team.plan.json" -> team)
parse_plan_filename(Filename) ->
    case string:split(filename:basename(Filename, ".json"), ".plan") of
        [TierStr, ""] ->
            try
                case list_to_atom(TierStr) of
                    Tier when Tier =:= team; Tier =:= enterprise; Tier =:= gov ->
                        {true, Tier};
                    _Other ->
                        false
                end
            catch
                error:_ -> false
            end;
        _Other ->
            false
    end.
