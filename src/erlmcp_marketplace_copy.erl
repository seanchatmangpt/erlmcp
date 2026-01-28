%%%-------------------------------------------------------------------
%% @doc
%% Automated Marketplace Listing Generation from Plan Specs
%%
%% Generates deterministic, zero-manual-edit marketplace copy from plan JSON specs.
%% All fields auto-populated from plan configurations with full cross-reference validation.
%%
%% Functions:
%%   - generate_team_listing/1 - Generate Team tier listing
%%   - generate_enterprise_listing/1 - Generate Enterprise tier listing
%%   - generate_gov_listing/1 - Generate Government tier listing
%%   - generate_from_plan/2 - Generic generator (PlanSpec, TemplateFile)
%%   - validate_plan/1 - Validate plan spec has all required fields
%%   - render_template/2 - Render template with plan values
%%
%% All outputs are deterministic (same input → bit-identical output).
%% Cross-references validated: refusal codes exist, SLA numbers match plan, no unclosed blocks.
%%
%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_marketplace_copy).

-export([
    generate_team_listing/1,
    generate_enterprise_listing/1,
    generate_gov_listing/1,
    generate_from_plan/2,
    validate_plan/1,
    render_template/2
]).

-type plan_spec() :: #{
    name := atom(),
    tier := atom(),
    envelope := envelope_spec(),
    sla := sla_spec(),
    refusal := refusal_spec(),
    evidence := evidence_spec(),
    pricing := pricing_spec()
}.

-type envelope_spec() :: #{
    requests_per_second := pos_integer(),
    concurrent_connections := pos_integer(),
    queue_depth := pos_integer(),
    message_size_bytes := pos_integer()
}.

-type sla_spec() :: #{
    latency_p99_ms := pos_integer(),
    failover_seconds := pos_integer(),
    uptime_percent := float(),
    incident_response_hours := pos_integer()
}.

-type refusal_spec() :: #{
    at_capacity := binary(),
    rate_limit_exceeded := binary(),
    message_too_large := binary(),
    invalid_protocol := binary()
}.

-type evidence_spec() :: #{
    sbom_included := boolean(),
    provenance_included := boolean(),
    chaos_matrix_included := boolean(),
    benchmark_results_included := boolean()
}.

-type pricing_spec() :: #{
    model := flat_per_deployment,
    base_cost_usd := pos_integer(),
    deployment_unit := binary()
}.

-export_type([
    plan_spec/0,
    envelope_spec/0,
    sla_spec/0,
    refusal_spec/0,
    evidence_spec/0,
    pricing_spec/0
]).

%% ===================================================================
%% Public API
%% ===================================================================

%%--------------------------------------------------------------------
%% @doc Generate Team tier marketplace listing from plan spec.
%% Returns markdown string ready for marketplace submission.
%%
%% Errors: {error, invalid_plan} if plan spec incomplete
%%         {error, template_not_found} if template file missing
%%         {error, cross_reference_failed} if refusal codes or SLA invalid
%% @end
%%--------------------------------------------------------------------
-spec generate_team_listing(PlanSpec :: plan_spec()) ->
    {ok, Listing :: binary()} | {error, Reason :: atom()}.

generate_team_listing(PlanSpec) ->
    generate_from_plan(PlanSpec, "templates/marketplace_team.md").

%%--------------------------------------------------------------------
%% @doc Generate Enterprise tier marketplace listing from plan spec.
%% @end
%%--------------------------------------------------------------------
-spec generate_enterprise_listing(PlanSpec :: plan_spec()) ->
    {ok, Listing :: binary()} | {error, Reason :: atom()}.

generate_enterprise_listing(PlanSpec) ->
    generate_from_plan(PlanSpec, "templates/marketplace_enterprise.md").

%%--------------------------------------------------------------------
%% @doc Generate Government tier marketplace listing from plan spec.
%% @end
%%--------------------------------------------------------------------
-spec generate_gov_listing(PlanSpec :: plan_spec()) ->
    {ok, Listing :: binary()} | {error, Reason :: atom()}.

generate_gov_listing(PlanSpec) ->
    generate_from_plan(PlanSpec, "templates/marketplace_gov.md").

%%--------------------------------------------------------------------
%% @doc Generate marketplace listing from plan spec and template.
%%
%% Process:
%%   1. Validate plan spec (all required fields present)
%%   2. Load and render template with plan values
%%   3. Validate generated markdown (no unclosed blocks, valid syntax)
%%   4. Cross-reference check (refusal codes, SLA fields exist)
%%   5. Return deterministic binary output
%%
%% @end
%%--------------------------------------------------------------------
-spec generate_from_plan(PlanSpec :: plan_spec(), TemplateFile :: string()) ->
    {ok, Listing :: binary()} | {error, Reason :: atom()}.

generate_from_plan(PlanSpec, TemplateFile) ->
    case validate_plan(PlanSpec) of
        ok ->
            case load_template(TemplateFile) of
                {ok, Template} ->
                    render_and_validate(Template, PlanSpec);
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @doc Validate plan spec has all required fields.
%%
%% Checks:
%%   - Top-level: name, tier, envelope, sla, refusal, evidence, pricing
%%   - Envelope: requests_per_second, concurrent_connections, queue_depth, message_size_bytes
%%   - SLA: latency_p99_ms, failover_seconds, uptime_percent, incident_response_hours
%%   - Refusal: at_capacity, rate_limit_exceeded, message_too_large, invalid_protocol
%%   - Evidence: all boolean flags present
%%   - Pricing: model, base_cost_usd, deployment_unit
%%
%% @end
%%--------------------------------------------------------------------
-spec validate_plan(PlanSpec :: plan_spec()) ->
    ok | {error, {missing_field, Field :: atom()}}.

validate_plan(PlanSpec) ->
    case check_required_fields(PlanSpec, [name, tier, envelope, sla, refusal, evidence, pricing]) of
        ok -> validate_nested_specs(PlanSpec);
        Error -> Error
    end.

%%--------------------------------------------------------------------
%% @doc Render template with plan values.
%% Replaces {{field}} placeholders with actual plan values.
%% @end
%%--------------------------------------------------------------------
-spec render_template(Template :: binary(), PlanSpec :: plan_spec()) ->
    {ok, Listing :: binary()} | {error, Reason :: atom()}.

render_template(Template, PlanSpec) ->
    render_and_validate(Template, PlanSpec).

%% ===================================================================
%% Internal Functions
%% ===================================================================

-spec check_required_fields(map(), [atom()]) ->
    ok | {error, {missing_field, atom()}}.
check_required_fields(Map, []) ->
    ok;
check_required_fields(Map, [Field | Rest]) ->
    case maps:is_key(Field, Map) of
        true -> check_required_fields(Map, Rest);
        false -> {error, {missing_field, Field}}
    end.

-spec validate_nested_specs(plan_spec()) ->
    ok | {error, {missing_field, atom()}}.
validate_nested_specs(PlanSpec) ->
    Envelope = maps:get(envelope, PlanSpec),
    case check_required_fields(Envelope, [
        requests_per_second,
        concurrent_connections,
        queue_depth,
        message_size_bytes
    ]) of
        ok ->
            SLA = maps:get(sla, PlanSpec),
            case check_required_fields(SLA, [
                latency_p99_ms,
                failover_seconds,
                uptime_percent,
                incident_response_hours
            ]) of
                ok ->
                    Refusal = maps:get(refusal, PlanSpec),
                    case check_required_fields(Refusal, [
                        at_capacity,
                        rate_limit_exceeded,
                        message_too_large,
                        invalid_protocol
                    ]) of
                        ok ->
                            Evidence = maps:get(evidence, PlanSpec),
                            case check_required_fields(Evidence, [
                                sbom_included,
                                provenance_included,
                                chaos_matrix_included,
                                benchmark_results_included
                            ]) of
                                ok ->
                                    Pricing = maps:get(pricing, PlanSpec),
                                    check_required_fields(Pricing, [
                                        model,
                                        base_cost_usd,
                                        deployment_unit
                                    ]);
                                Error ->
                                    Error
                            end;
                        Error ->
                            Error
                    end;
                Error ->
                    Error
            end;
        Error ->
            Error
    end.

-spec load_template(TemplateFile :: string()) ->
    {ok, Template :: binary()} | {error, Reason :: atom()}.
load_template(TemplateFile) ->
    case file:read_file(TemplateFile) of
        {ok, Template} ->
            {ok, Template};
        {error, enoent} ->
            {error, template_not_found};
        {error, _Reason} ->
            {error, template_read_failed}
    end.

-spec render_and_validate(Template :: binary(), PlanSpec :: plan_spec()) ->
    {ok, Listing :: binary()} | {error, Reason :: atom()}.
render_and_validate(Template, PlanSpec) ->
    %% Render template with plan values
    Rendered = render_template_vars(Template, PlanSpec),

    %% Validate rendered markdown
    case validate_markdown(Rendered) of
        ok ->
            %% Cross-reference validation
            case validate_cross_references(Rendered, PlanSpec) of
                ok ->
                    {ok, Rendered};
                Error ->
                    Error
            end;
        Error ->
            Error
    end.

-spec render_template_vars(Template :: binary(), PlanSpec :: plan_spec()) ->
    binary().
render_template_vars(Template, PlanSpec) ->
    Envelope = maps:get(envelope, PlanSpec),
    SLA = maps:get(sla, PlanSpec),
    Refusal = maps:get(refusal, PlanSpec),
    Evidence = maps:get(evidence, PlanSpec),
    Pricing = maps:get(pricing, PlanSpec),

    %% Build replacement map - all values as binaries
    Replacements = #{
        <<"{{plan_name}}">> => atom_to_binary(maps:get(name, PlanSpec), utf8),
        <<"{{plan_tier}}">> => atom_to_binary(maps:get(tier, PlanSpec), utf8),
        <<"{{requests_per_second}}">> => integer_to_binary(maps:get(requests_per_second, Envelope)),
        <<"{{concurrent_connections}}">> => integer_to_binary(maps:get(concurrent_connections, Envelope)),
        <<"{{queue_depth}}">> => integer_to_binary(maps:get(queue_depth, Envelope)),
        <<"{{message_size_bytes}}">> => integer_to_binary(maps:get(message_size_bytes, Envelope)),
        <<"{{latency_p99_ms}}">> => integer_to_binary(maps:get(latency_p99_ms, SLA)),
        <<"{{failover_seconds}}">> => integer_to_binary(maps:get(failover_seconds, SLA)),
        <<"{{uptime_percent}}">> => format_float(maps:get(uptime_percent, SLA)),
        <<"{{incident_response_hours}}">> => integer_to_binary(maps:get(incident_response_hours, SLA)),
        <<"{{at_capacity}}">> => maps:get(at_capacity, Refusal),
        <<"{{rate_limit_exceeded}}">> => maps:get(rate_limit_exceeded, Refusal),
        <<"{{message_too_large}}">> => maps:get(message_too_large, Refusal),
        <<"{{invalid_protocol}}">> => maps:get(invalid_protocol, Refusal),
        <<"{{sbom_included}}">> => boolean_to_binary(maps:get(sbom_included, Evidence)),
        <<"{{provenance_included}}">> => boolean_to_binary(maps:get(provenance_included, Evidence)),
        <<"{{chaos_matrix_included}}">> => boolean_to_binary(maps:get(chaos_matrix_included, Evidence)),
        <<"{{benchmark_results_included}}">> => boolean_to_binary(maps:get(benchmark_results_included, Evidence)),
        <<"{{base_cost_usd}}">> => integer_to_binary(maps:get(base_cost_usd, Pricing)),
        <<"{{deployment_unit}}">> => maps:get(deployment_unit, Pricing)
    },

    %% Apply all replacements in stable order (deterministic)
    replace_vars(Template, Replacements).

-spec replace_vars(Template :: binary(), Replacements :: map()) ->
    binary().
replace_vars(Template, Replacements) ->
    %% Get keys in sorted order for deterministic results
    Keys = lists:sort(maps:keys(Replacements)),
    lists:foldl(
        fun(Key, Acc) ->
            Value = maps:get(Key, Replacements),
            binary:replace(Acc, Key, Value, [global])
        end,
        Template,
        Keys
    ).

-spec validate_markdown(Markdown :: binary()) ->
    ok | {error, invalid_markdown}.
validate_markdown(Markdown) ->
    case validate_code_blocks(Markdown) of
        ok ->
            validate_links(Markdown);
        Error ->
            Error
    end.

-spec validate_code_blocks(Markdown :: binary()) ->
    ok | {error, invalid_markdown}.
validate_code_blocks(Markdown) ->
    case count_delimiters(Markdown, <<"```">>) rem 2 of
        0 -> ok;
        _ -> {error, invalid_markdown}
    end.

-spec count_delimiters(Markdown :: binary(), Delimiter :: binary()) ->
    non_neg_integer().
count_delimiters(Markdown, Delimiter) ->
    count_delimiters_acc(Markdown, Delimiter, 0).

-spec count_delimiters_acc(Markdown :: binary(), Delimiter :: binary(), Count :: non_neg_integer()) ->
    non_neg_integer().
count_delimiters_acc(Markdown, Delimiter, Count) ->
    case binary:match(Markdown, Delimiter) of
        nomatch ->
            Count;
        {Pos, Len} ->
            Rest = binary:part(Markdown, Pos + Len, byte_size(Markdown) - Pos - Len),
            count_delimiters_acc(Rest, Delimiter, Count + 1)
    end.

-spec validate_links(Markdown :: binary()) ->
    ok | {error, invalid_markdown}.
validate_links(_Markdown) ->
    %% Basic link validation - can be extended
    ok.

-spec validate_cross_references(Rendered :: binary(), PlanSpec :: plan_spec()) ->
    ok | {error, Reason :: atom()}.
validate_cross_references(Rendered, PlanSpec) ->
    %% Verify refusal codes are mentioned in rendered output
    Refusal = maps:get(refusal, PlanSpec),
    RefusalCodes = maps:values(Refusal),

    case check_codes_referenced(Rendered, RefusalCodes) of
        ok ->
            %% Verify SLA numbers are in rendered output
            SLA = maps:get(sla, PlanSpec),
            SLAValues = [
                integer_to_binary(maps:get(latency_p99_ms, SLA)),
                integer_to_binary(maps:get(failover_seconds, SLA)),
                integer_to_binary(maps:get(incident_response_hours, SLA))
            ],
            check_codes_referenced(Rendered, SLAValues);
        Error ->
            Error
    end.

-spec check_codes_referenced(Rendered :: binary(), Codes :: [binary()]) ->
    ok | {error, cross_reference_failed}.
check_codes_referenced(_Rendered, []) ->
    ok;
check_codes_referenced(Rendered, [Code | Rest]) ->
    case binary:match(Rendered, Code) of
        nomatch ->
            {error, cross_reference_failed};
        {_, _} ->
            check_codes_referenced(Rendered, Rest)
    end.

-spec format_float(Value :: float()) ->
    binary().
format_float(Value) ->
    erlang:float_to_binary(Value, [{decimals, 2}]).

-spec boolean_to_binary(Value :: boolean()) ->
    binary().
boolean_to_binary(true) ->
    <<"Yes">>;
boolean_to_binary(false) ->
    <<"No">>.
