%%%-------------------------------------------------------------------
%%% @doc
%%% TCPS Template Renderer Module
%%%
%%% This module provides template rendering functionality for Toyota Code
%%% Production System (TCPS) artifact generation. It uses bbmustache for
%%% Mustache template rendering with deterministic output.
%%%
%%% Supported templates:
%%% - receipt.json.mustache - Production stage receipts (JSON)
%%% - work_order.ttl.mustache - Work order RDF instances (Turtle)
%%% - andon_event.ttl.mustache - Andon stop-the-line events (Turtle)
%%% - sku_listing.md.mustache - Marketplace SKU listings (Markdown)
%%% - standard_work.md.mustache - Standard work procedures (Markdown)
%%% - kaizen_report.md.mustache - Continuous improvement reports (Markdown)
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_templates).

-export([
    render_receipt/1,
    render_work_order/1,
    render_andon_event/1,
    render_sku_listing/1,
    render_standard_work/1,
    render_kaizen_report/1,
    render_template/2,
    list_templates/0
]).

-type template_context() :: #{atom() => term()}.
-type template_name() :: receipt | work_order | andon_event | sku_listing |
                         standard_work | kaizen_report.
-type render_result() :: {ok, binary()} | {error, term()}.

-define(TEMPLATES_DIR, "templates/tcps").

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Render a receipt JSON template.
%% Context must include: stage_name, timestamp, status, sku_id, evidence_data
-spec render_receipt(template_context()) -> render_result().
render_receipt(Context) ->
    RequiredKeys = [stage_name, timestamp, status, sku_id, evidence_data],
    case validate_context(Context, RequiredKeys) of
        ok ->
            % Convert evidence_data map to JSON string for embedding
            EvidenceJSON = jsx:encode(maps:get(evidence_data, Context)),
            Context1 = maps:put(evidence_data, EvidenceJSON, Context),
            render_template(receipt, Context1);
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Render a work order RDF template.
%% Context must include: work_order_id, demand_signal, bucket, priority,
%%                       created_by, created_at, sku_id
-spec render_work_order(template_context()) -> render_result().
render_work_order(Context) ->
    RequiredKeys = [work_order_id, demand_signal, bucket, priority,
                    created_by, created_at, sku_id],
    case validate_context(Context, RequiredKeys) of
        ok ->
            render_template(work_order, Context);
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Render an Andon event RDF template.
%% Context must include: andon_id, failure_reason, affected_sku, severity,
%%                       timestamp, detected_by, stage_name, error_details,
%%                       remediation_status
-spec render_andon_event(template_context()) -> render_result().
render_andon_event(Context) ->
    RequiredKeys = [andon_id, failure_reason, affected_sku, severity,
                    timestamp, detected_by, stage_name, error_details,
                    remediation_status],
    case validate_context(Context, RequiredKeys) of
        ok ->
            % Process five_whys array to add indices
            Context1 = case maps:get(five_whys, Context, undefined) of
                undefined -> Context;
                FiveWhys when is_list(FiveWhys) ->
                    IndexedWhys = lists:zipwith(
                        fun(I, Why) -> maps:put('-index', I, Why) end,
                        lists:seq(1, length(FiveWhys)),
                        FiveWhys
                    ),
                    maps:put(five_whys, IndexedWhys, Context)
            end,
            render_template(andon_event, Context1);
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Render a SKU marketplace listing template.
%% Context must include: sku_id, name, version, description, category,
%%                       author, license, repository_url, features
-spec render_sku_listing(template_context()) -> render_result().
render_sku_listing(Context) ->
    RequiredKeys = [sku_id, name, version, description, category,
                    author, license, repository_url, features],
    case validate_context(Context, RequiredKeys) of
        ok ->
            render_template(sku_listing, Context);
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Render a standard work procedure template.
%% Context must include: stage_name, stage_id, description, inputs, outputs,
%%                       steps, slo_time_budget_ms, slo_success_rate
-spec render_standard_work(template_context()) -> render_result().
render_standard_work(Context) ->
    RequiredKeys = [stage_name, stage_id, description, inputs, outputs,
                    steps, slo_time_budget_ms, slo_success_rate],
    case validate_context(Context, RequiredKeys) of
        ok ->
            % Add calculated fields
            SLOTimeMs = maps:get(slo_time_budget_ms, Context),
            SLORate = maps:get(slo_success_rate, Context),
            Context1 = Context#{
                slo_time_budget_seconds => SLOTimeMs / 1000.0,
                slo_success_rate_percent => SLORate * 100.0
            },
            % Add step numbers
            Steps = maps:get(steps, Context),
            NumberedSteps = lists:zipwith(
                fun(I, Step) -> maps:put(step_number, I, Step) end,
                lists:seq(1, length(Steps)),
                Steps
            ),
            Context2 = maps:put(steps, NumberedSteps, Context1),
            render_template(standard_work, Context2);
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Render a Kaizen continuous improvement report template.
%% Context must include: report_id, report_period_start, report_period_end,
%%                       generated_at, generated_by, metrics_source
-spec render_kaizen_report(template_context()) -> render_result().
render_kaizen_report(Context) ->
    RequiredKeys = [report_id, report_period_start, report_period_end,
                    generated_at, generated_by, metrics_source],
    case validate_context(Context, RequiredKeys) of
        ok ->
            % Add indices to opportunities
            Context1 = case maps:get(opportunities, Context, undefined) of
                undefined -> Context;
                Opps when is_list(Opps) ->
                    IndexedOpps = lists:zipwith(
                        fun(I, Opp) -> maps:put('-index', I, Opp) end,
                        lists:seq(1, length(Opps)),
                        Opps
                    ),
                    maps:put(opportunities, IndexedOpps, Context)
            end,
            render_template(kaizen_report, Context1);
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Generic template renderer.
%% Renders any template by name with provided context.
-spec render_template(template_name(), template_context()) -> render_result().
render_template(TemplateName, Context) ->
    TemplateFile = template_file(TemplateName),
    case file:read_file(TemplateFile) of
        {ok, TemplateBinary} ->
            try
                % bbmustache:render returns iolist, convert to binary
                Rendered = bbmustache:render(TemplateBinary, Context, [
                    {key_type, atom}  % Use atoms for map keys
                ]),
                {ok, iolist_to_binary(Rendered)}
            catch
                error:Reason ->
                    {error, {render_error, Reason}};
                Class:Reason:Stack ->
                    {error, {render_exception, Class, Reason, Stack}}
            end;
        {error, Reason} ->
            {error, {template_not_found, TemplateFile, Reason}}
    end.

%% @doc List all available TCPS templates.
-spec list_templates() -> [template_name()].
list_templates() ->
    [receipt, work_order, andon_event, sku_listing, standard_work, kaizen_report].

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @doc Get the full path to a template file.
-spec template_file(template_name()) -> string().
template_file(receipt) ->
    filename:join([?TEMPLATES_DIR, "receipt.json.mustache"]);
template_file(work_order) ->
    filename:join([?TEMPLATES_DIR, "work_order.ttl.mustache"]);
template_file(andon_event) ->
    filename:join([?TEMPLATES_DIR, "andon_event.ttl.mustache"]);
template_file(sku_listing) ->
    filename:join([?TEMPLATES_DIR, "sku_listing.md.mustache"]);
template_file(standard_work) ->
    filename:join([?TEMPLATES_DIR, "standard_work.md.mustache"]);
template_file(kaizen_report) ->
    filename:join([?TEMPLATES_DIR, "kaizen_report.md.mustache"]).

%% @doc Validate that all required keys are present in context.
-spec validate_context(template_context(), [atom()]) -> ok | {error, term()}.
validate_context(Context, RequiredKeys) ->
    Missing = [K || K <- RequiredKeys, not maps:is_key(K, Context)],
    case Missing of
        [] -> ok;
        _ -> {error, {missing_context_keys, Missing}}
    end.
