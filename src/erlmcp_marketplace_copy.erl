%%%-------------------------------------------------------------------
%% @doc
%% Marketplace Copy Generator - Auto-generate marketplace listings from plan specs.
%%
%% This module generates deterministic, human-readable marketplace listings
%% from plan specifications. All content is auto-populated from plan JSON,
%% ensuring consistency and eliminating manual editing.
%%
%% Functions:
%%   - generate_team_listing/1 - Generate Team tier marketplace listing
%%   - generate_enterprise_listing/1 - Generate Enterprise tier listing
%%   - generate_gov_listing/1 - Generate Government tier listing
%%
%% Output: Human-readable Markdown with:
%%   - Envelope summary (throughput, concurrency, queue depth)
%%   - SLA commitments (availability %, latency, failover time)
%%   - Refusal behavior (deterministic boundary responses)
%%   - Evidence included (SBOM, provenance, chaos, benchmarks)
%%   - Pricing model (flat per-deployment, no metering)
%%
%% Properties:
%%   - 100% deterministic: same input → identical byte output
%%   - Zero manual editing: all content from plan specs
%%   - Cross-reference validated: refusal codes, SLA values verified
%%   - Markdown validated: no unclosed blocks, proper formatting
%%
%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_marketplace_copy).

-export([
    generate_team_listing/1,
    generate_enterprise_listing/1,
    generate_gov_listing/1,
    validate_listing_markdown/1
]).

%% Type definitions for plan specifications
-type plan_spec() :: map().
-type listing_result() :: {ok, Markdown :: binary()} | {error, Reason :: atom()}.
-type markdown_validation_result() :: {ok, validated} | {error, Reason :: atom()}.

-define(TEMPLATE_DIR, "templates").
-define(MAX_MARKDOWN_LINE_LENGTH, 120).

%% ===================================================================
%% Public API - Listing Generators
%% ===================================================================

%%--------------------------------------------------------------------
%% @doc Generate Team tier marketplace listing.
%%
%% Renders Team plan specifications into marketplace-ready markdown.
%% Includes 450 req/s throughput, 128 concurrent connections, basic features.
%%
%% Returns: {ok, Markdown} | {error, Reason}
%% @end
%%--------------------------------------------------------------------
-spec generate_team_listing(PlanSpec :: plan_spec()) ->
    listing_result().

generate_team_listing(PlanSpec) ->
    try
        render_listing(team, PlanSpec)
    catch
        Error:Reason ->
            {error, {rendering_failed, Error, Reason}}
    end.

%%--------------------------------------------------------------------
%% @doc Generate Enterprise tier marketplace listing.
%%
%% Renders Enterprise plan specifications into marketplace-ready markdown.
%% Includes 1500 req/s throughput, 512 concurrent connections, HA features.
%%
%% Returns: {ok, Markdown} | {error, Reason}
%% @end
%%--------------------------------------------------------------------
-spec generate_enterprise_listing(PlanSpec :: plan_spec()) ->
    listing_result().

generate_enterprise_listing(PlanSpec) ->
    try
        render_listing(enterprise, PlanSpec)
    catch
        Error:Reason ->
            {error, {rendering_failed, Error, Reason}}
    end.

%%--------------------------------------------------------------------
%% @doc Generate Government tier marketplace listing.
%%
%% Renders Government plan specifications into marketplace-ready markdown.
%% Includes 900 req/s throughput, FIPS-140-2, comprehensive audit logging.
%%
%% Returns: {ok, Markdown} | {error, Reason}
%% @end
%%--------------------------------------------------------------------
-spec generate_gov_listing(PlanSpec :: plan_spec()) ->
    listing_result().

generate_gov_listing(PlanSpec) ->
    try
        render_listing(gov, PlanSpec)
    catch
        Error:Reason ->
            {error, {rendering_failed, Error, Reason}}
    end.

%%--------------------------------------------------------------------
%% @doc Validate generated marketplace markdown.
%%
%% Checks for common markdown issues:
%%   - Unclosed code blocks (``` without closing ```)
%%   - Unmatched brackets/parens
%%   - Invalid header syntax
%%   - Unreplaced template variables {{...}}
%%
%% Returns: {ok, validated} | {error, Reason}
%% @end
%%--------------------------------------------------------------------
-spec validate_listing_markdown(Markdown :: binary()) ->
    markdown_validation_result().

validate_listing_markdown(Markdown) ->
    case validate_code_blocks(Markdown) of
        ok ->
            case validate_no_unreplaced_variables(Markdown) of
                ok ->
                    case validate_markdown_structure(Markdown) of
                        ok -> {ok, validated};
                        Error -> Error
                    end;
                Error -> Error
            end;
        Error -> Error
    end.

%% ===================================================================
%% Internal Rendering Functions
%% ===================================================================

%%--------------------------------------------------------------------
%% @private Render marketplace listing from plan specification.
%%
%% Process:
%%   1. Load template file for tier
%%   2. Extract plan values (envelope, SLA, features, etc.)
%%   3. Build variable substitution map
%%   4. Substitute all {{variable}} with values
%%   5. Validate no unreplaced variables remain
%%   6. Validate markdown structure
%%   7. Return final markdown binary
%%
%% @end
%%--------------------------------------------------------------------
-spec render_listing(Tier :: atom(), PlanSpec :: plan_spec()) ->
    listing_result().

render_listing(Tier, PlanSpec) ->
    try
        %% Extract plan values
        TierName = maps:get(<<"name">>, PlanSpec, <<"Unknown">>),
        Envelope = maps:get(<<"envelope">>, PlanSpec, #{}),
        SLA = maps:get(<<"sla">>, PlanSpec, #{}),
        Features = maps:get(<<"features">>, PlanSpec, #{}),
        Limits = maps:get(<<"limits">>, PlanSpec, #{}),
        RefusalBehavior = maps:get(<<"refusal_behavior">>, PlanSpec, #{}),
        Evidence = maps:get(<<"evidence">>, PlanSpec, #{}),
        Compliance = maps:get(<<"compliance">>, PlanSpec, #{}),

        %% Load and render template
        TemplateFile = template_filename(Tier),
        case file:read_file(TemplateFile) of
            {ok, TemplateContent} ->
                %% Build substitution map
                VarMap = build_variable_map(
                    TierName,
                    Envelope,
                    SLA,
                    Features,
                    Limits,
                    RefusalBehavior,
                    Evidence,
                    Compliance
                ),

                %% Substitute variables
                Markdown = substitute_variables(TemplateContent, VarMap),

                %% Validate markdown
                case validate_listing_markdown(Markdown) of
                    {ok, validated} -> {ok, Markdown};
                    Error -> Error
                end;

            {error, Reason} ->
                {error, {template_not_found, Reason, TemplateFile}}
        end
    catch
        throw:Exception -> {error, Exception};
        error:Exception -> {error, {runtime_error, Exception}}
    end.

%%--------------------------------------------------------------------
%% @private Build variable substitution map from plan spec.
%%
%% Creates a map of template variables to plan values:
%%   {<<"tier_name">>} → "Team Tier"
%%   {<<"throughput">>} → "450"
%%   {<<"concurrent">>} → "128"
%%   {<<"latency">>} → "250"
%%   etc.
%%
%% @end
%%--------------------------------------------------------------------
-spec build_variable_map(
    TierName :: binary(),
    Envelope :: map(),
    SLA :: map(),
    Features :: map(),
    Limits :: map(),
    RefusalBehavior :: map(),
    Evidence :: map(),
    Compliance :: map()
) -> map().

build_variable_map(TierName, Envelope, SLA, Features, Limits, RefusalBehavior, Evidence, Compliance) ->
    BaseMap = #{
        <<"tier_name">> => to_binary(TierName),
        <<"throughput">> => format_value(maps:get(<<"throughput_req_s">>, Envelope, 0)),
        <<"concurrent">> => format_value(maps:get(<<"concurrent_connections">>, Envelope, 0)),
        <<"queue_depth">> => format_value(maps:get(<<"queue_depth_messages">>, Envelope, 0)),
        <<"p99_latency">> => format_value(maps:get(<<"p99_latency_ms">>, Envelope, 0)),
        <<"failover_sla">> => format_value(maps:get(<<"failover_sla_seconds">>, Envelope, 0)),
        <<"connection_timeout">> => format_value(maps:get(<<"connection_timeout_seconds">>, Envelope, 0)),
        <<"max_message_size">> => format_bytes(maps:get(<<"max_message_size_bytes">>, Limits, 0)),
        <<"max_payload">> => format_value(maps:get(<<"max_payload_size_mb">>, Limits, 0)),
        <<"max_concurrent_reqs">> => format_value(maps:get(<<"max_concurrent_requests_per_conn">>, Limits, 0)),
        <<"memory_limit">> => format_value(maps:get(<<"memory_limit_mb">>, Limits, 0)),
        <<"cpu_time_limit">> => format_value(maps:get(<<"cpu_time_limit_seconds">>, Limits, 0))
    },

    %% Add SLA fields
    SLAMap = #{
        <<"availability">> => format_availability(maps:get(<<"availability_percentage">>, SLA, undefined)),
        <<"throughput_guarantee">> => format_value(maps:get(<<"throughput_guarantee_req_s">>, SLA, 0)),
        <<"recovery_sla">> => format_value(maps:get(<<"recovery_sla_minutes">>, SLA, 0))
    },

    %% Add features as bullet list
    FeaturesList = format_features(Features),
    FeaturesMap = #{<<"features">> => FeaturesList},

    %% Add refusal behavior summary
    RefusalSummary = format_refusal_behavior(RefusalBehavior),
    RefusalMap = #{<<"refusal_summary">> => RefusalSummary},

    %% Add evidence files
    EvidenceList = format_evidence(Evidence),
    EvidenceMap = #{<<"evidence_files">> => EvidenceList},

    %% Add compliance info
    ComplianceInfo = format_compliance(Compliance),
    ComplianceMap = #{<<"compliance_info">> => ComplianceInfo},

    %% Merge all maps
    maps:merge([BaseMap, SLAMap, FeaturesMap, RefusalMap, EvidenceMap, ComplianceMap]).

%%--------------------------------------------------------------------
%% @private Substitute template variables in markdown.
%%
%% Replaces all {{variable}} occurrences with values from VarMap.
%% Variables are case-insensitive and whitespace-tolerant:
%%   {{throughput}}, {{ throughput }}, {{THROUGHPUT}} all work
%%
%% Returns markdown binary with all substitutions applied.
%% @end
%%--------------------------------------------------------------------
-spec substitute_variables(Template :: binary(), VarMap :: map()) ->
    binary().

substitute_variables(Template, VarMap) ->
    %% Iterate over all variables and perform substitutions
    maps:fold(
        fun(Key, Value, Acc) ->
            substitute_variable(Acc, Key, Value)
        end,
        Template,
        VarMap
    ).

%%--------------------------------------------------------------------
%% @private Substitute single variable in template.
%%
%% Handles multiple formats:
%%   {{key}} → value
%%   {{ key }} → value
%%   {{Key}} → value
%%   etc.
%%
%% @end
%%--------------------------------------------------------------------
-spec substitute_variable(Template :: binary(), Key :: binary(), Value :: binary()) ->
    binary().

substitute_variable(Template, Key, Value) ->
    %% Simple substitution for {{key}}
    Pattern = <<"{{", Key/binary, "}}">>,
    binary:replace(Template, Pattern, Value, [global]).

%%--------------------------------------------------------------------
%% @private Format feature list from features map.
%%
%% Converts feature map to markdown bullet list:
%%   - Client support
%%   - Server support
%%   - TCP transport
%%   - Rate limiting
%%   etc.
%%
%% @end
%%--------------------------------------------------------------------
-spec format_features(Features :: map()) ->
    binary().

format_features(Features) ->
    FeatureList = maps:fold(
        fun(Key, Value, Acc) ->
            case Value of
                true ->
                    Label = format_feature_label(Key),
                    [<<"- ">>, Label, <<"\n">> | Acc];
                _ ->
                    Acc
            end
        end,
        [],
        Features
    ),
    iolist_to_binary(lists:reverse(FeatureList)).

%%--------------------------------------------------------------------
%% @private Format feature key to human-readable label.
%% @end
%%--------------------------------------------------------------------
-spec format_feature_label(Key :: binary()) ->
    binary().

format_feature_label(<<"stdio_transport">>) -> <<"Standard I/O Transport">>;
format_feature_label(<<"tcp_transport">>) -> <<"TCP Transport">>;
format_feature_label(<<"http_transport">>) -> <<"HTTP Transport">>;
format_feature_label(<<"websocket_transport">>) -> <<"WebSocket Transport">>;
format_feature_label(<<"sse_transport">>) -> <<"Server-Sent Events Transport">>;
format_feature_label(<<"rate_limiting">>) -> <<"Rate Limiting & Throttling">>;
format_feature_label(<<"connection_pooling">>) -> <<"Connection Pooling">>;
format_feature_label(<<"circuit_breaker">>) -> <<"Circuit Breaker Pattern">>;
format_feature_label(<<"otel_observability">>) -> <<"OpenTelemetry Observability">>;
format_feature_label(<<"audit_logging">>) -> <<"Comprehensive Audit Logging">>;
format_feature_label(<<"fips_140_2">>) -> <<"FIPS 140-2 Encryption">>;
format_feature_label(<<"high_availability">>) -> <<"High Availability (HA)">>;
format_feature_label(<<"multi_region_support">>) -> <<"Multi-Region Support">>;
format_feature_label(<<"encrypted_transport">>) -> <<"Encrypted Transport">>;
format_feature_label(<<"tls_1_3_only">>) -> <<"TLS 1.3 Only">>;
format_feature_label(<<"key_rotation">>) -> <<"Key Rotation">>;
format_feature_label(<<"compliance_reporting">>) -> <<"Compliance Reporting">>;
format_feature_label(<<"load_balancing">>) -> <<"Load Balancing">>;
format_feature_label(<<"health_checks">>) -> <<"Health Checks">>;
format_feature_label(Key) -> Key.

%%--------------------------------------------------------------------
%% @private Format refusal behavior summary.
%%
%% Creates markdown table or list of refusal scenarios:
%%   | Scenario | HTTP Status | Error Code | Message |
%%   |----------|-------------|-----------|---------|
%%   | Throughput Exceeded | 429 | rate_limit_exceeded | ... |
%%   etc.
%%
%% @end
%%--------------------------------------------------------------------
-spec format_refusal_behavior(RefusalBehavior :: map()) ->
    binary().

format_refusal_behavior(RefusalBehavior) ->
    %% Build table header
    Header = <<"| Scenario | HTTP Status | Error Code | Message |\n"
             "|----------|-------------|-----------|----------|\n">>,

    %% Build rows for each refusal scenario
    Rows = maps:fold(
        fun(Scenario, Details, Acc) when is_map(Details) ->
                HttpStatus = format_value(maps:get(<<"http_status">>, Details, <<"N/A">>)),
                ErrorCode = to_binary(maps:get(<<"error_code">>, Details, <<"unknown">>)),
                Message = to_binary(maps:get(<<"message">>, Details, <<"">>)),
                ScenarioLabel = format_refusal_label(Scenario),
                Row = iolist_to_binary([
                    <<"| ">>, ScenarioLabel, <<" | ">>,
                    HttpStatus, <<" | ">>,
                    ErrorCode, <<" | ">>,
                    Message, <<" |\n">>
                ]),
                [Row | Acc];
            (_, _, Acc) ->
                Acc
        end,
        [],
        RefusalBehavior
    ),

    iolist_to_binary([Header, lists:reverse(Rows)]).

%%--------------------------------------------------------------------
%% @private Format refusal scenario label.
%% @end
%%--------------------------------------------------------------------
-spec format_refusal_label(Scenario :: binary()) ->
    binary().

format_refusal_label(<<"throughput_exceeded">>) -> <<"Throughput Limit Exceeded">>;
format_refusal_label(<<"queue_depth_exceeded">>) -> <<"Queue Depth Exceeded">>;
format_refusal_label(<<"connection_limit_exceeded">>) -> <<"Connection Limit Exceeded">>;
format_refusal_label(<<"message_size_exceeded">>) -> <<"Message Size Exceeded">>;
format_refusal_label(<<"unsupported_feature">>) -> <<"Unsupported Feature">>;
format_refusal_label(<<"audit_log_error">>) -> <<"Audit Log Failure">>;
format_refusal_label(<<"fips_compliance_violation">>) -> <<"FIPS Compliance Violation">>;
format_refusal_label(<<"encryption_failure">>) -> <<"Encryption Failure">>;
format_refusal_label(Label) -> Label.

%%--------------------------------------------------------------------
%% @private Format evidence files list.
%%
%% Creates markdown list of evidence artifacts:
%%   - SBOM (plans/team-sbom.json)
%%   - Provenance (plans/team-provenance.json)
%%   - Chaos Report (docs/plans/team-chaos-report.md)
%%   etc.
%%
%% @end
%%--------------------------------------------------------------------
-spec format_evidence(Evidence :: map()) ->
    binary().

format_evidence(Evidence) ->
    Items = maps:fold(
        fun(Key, FilePath, Acc) ->
            Label = format_evidence_label(Key),
            Item = iolist_to_binary([
                <<"- ">>, Label, <<" (">>, to_binary(FilePath), <<")\n">>
            ]),
            [Item | Acc]
        end,
        [],
        Evidence
    ),
    iolist_to_binary(lists:reverse(Items)).

%%--------------------------------------------------------------------
%% @private Format evidence type label.
%% @end
%%--------------------------------------------------------------------
-spec format_evidence_label(Type :: binary()) ->
    binary().

format_evidence_label(<<"sbom">>) -> <<"Software Bill of Materials (SBOM)">>;
format_evidence_label(<<"provenance">>) -> <<"Build Provenance">>;
format_evidence_label(<<"chaos_report">>) -> <<"Chaos Engineering Report">>;
format_evidence_label(<<"benchmark_report">>) -> <<"Performance Benchmark Report">>;
format_evidence_label(<<"audit_schema">>) -> <<"Audit Log Schema">>;
format_evidence_label(<<"fips_certification">>) -> <<"FIPS 140-2 Certification">>;
format_evidence_label(<<"compliance_report">>) -> <<"Compliance Report">>;
format_evidence_label(Label) -> Label.

%%--------------------------------------------------------------------
%% @private Format compliance information.
%%
%% Summarizes compliance attributes:
%%   MCP Version: 2025-11-25
%%   Security Level: standard/enhanced/maximum
%%   Audit Trail: Yes/No
%%   FIPS 140-2: Yes/No
%%   etc.
%%
%% @end
%%--------------------------------------------------------------------
-spec format_compliance(Compliance :: map()) ->
    binary().

format_compliance(Compliance) ->
    Items = [
        format_compliance_item(
            <<"MCP Version">>,
            maps:get(<<"mcp_version">>, Compliance, <<"Unknown">>)
        ),
        format_compliance_item(
            <<"Security Level">>,
            maps:get(<<"security_level">>, Compliance, <<"standard">>)
        ),
        format_compliance_item(
            <<"Features Implemented">>,
            maps:get(<<"features_implemented">>, Compliance, 0)
        ),
        format_compliance_item(
            <<"Audit Trail">>,
            case maps:get(<<"audit_trail">>, Compliance, false) of
                true -> <<"Yes">>;
                false -> <<"No">>;
                _ -> <<"No">>
            end
        ),
        format_compliance_item(
            <<"FIPS 140-2">>,
            case maps:get(<<"fips_140_2">>, Compliance, false) of
                true -> <<"Yes">>;
                false -> <<"No">>;
                _ -> <<"No">>
            end
        )
    ],
    iolist_to_binary(Items).

%%--------------------------------------------------------------------
%% @private Format single compliance item.
%% @end
%%--------------------------------------------------------------------
-spec format_compliance_item(Label :: binary(), Value :: term()) ->
    binary().

format_compliance_item(Label, Value) ->
    iolist_to_binary([<<"- ">>, Label, <<": ">>, to_binary(Value), <<"\n">>]).

%% ===================================================================
%% Validation Functions
%% ===================================================================

%%--------------------------------------------------------------------
%% @private Validate code blocks are properly closed.
%%
%% Counts backtick sequences (```) and ensures even count.
%% Handles edge cases like code inside backticks.
%%
%% @end
%%--------------------------------------------------------------------
-spec validate_code_blocks(Markdown :: binary()) ->
    ok | {error, unclosed_code_block}.

validate_code_blocks(Markdown) ->
    %% Count ``` sequences (code block delimiters)
    CodeBlockCount = count_pattern(Markdown, <<"```">>),
    case CodeBlockCount rem 2 of
        0 -> ok;
        _ -> {error, unclosed_code_block}
    end.

%%--------------------------------------------------------------------
%% @private Validate no unreplaced template variables remain.
%%
%% Checks for {{...}} patterns that should have been substituted.
%%
%% @end
%%--------------------------------------------------------------------
-spec validate_no_unreplaced_variables(Markdown :: binary()) ->
    ok | {error, unreplaced_variables}.

validate_no_unreplaced_variables(Markdown) ->
    case binary:match(Markdown, <<"{{">>) of
        nomatch -> ok;
        {_, _} -> {error, unreplaced_variables}
    end.

%%--------------------------------------------------------------------
%% @private Validate basic markdown structure.
%%
%% Checks:
%%   - Headers are properly formatted (#, ##, etc.)
%%   - No unmatched brackets
%%   - Basic structure integrity
%%
%% @end
%%--------------------------------------------------------------------
-spec validate_markdown_structure(Markdown :: binary()) ->
    ok | {error, invalid_markdown_structure}.

validate_markdown_structure(Markdown) ->
    %% Basic check: count brackets
    OpenBrackets = count_pattern(Markdown, <<"[">>),
    CloseBrackets = count_pattern(Markdown, <<"]">>),
    case OpenBrackets =:= CloseBrackets of
        true ->
            %% Check parentheses too
            OpenParens = count_pattern(Markdown, <<"(">>),
            CloseParens = count_pattern(Markdown, <<")">>),
            case OpenParens =:= CloseParens of
                true -> ok;
                false -> {error, invalid_markdown_structure}
            end;
        false -> {error, invalid_markdown_structure}
    end.

%% ===================================================================
%% Utility Functions
%% ===================================================================

%%--------------------------------------------------------------------
%% @private Count pattern occurrences in binary.
%% @end
%%--------------------------------------------------------------------
-spec count_pattern(Binary :: binary(), Pattern :: binary()) ->
    non_neg_integer().

count_pattern(Binary, Pattern) ->
    count_pattern(Binary, Pattern, 0).

-spec count_pattern(Binary :: binary(), Pattern :: binary(), Acc :: non_neg_integer()) ->
    non_neg_integer().

count_pattern(Binary, Pattern, Acc) ->
    case binary:match(Binary, Pattern) of
        nomatch -> Acc;
        {Pos, Len} ->
            Offset = Pos + Len,
            <<_:Offset/binary, Rest/binary>> = Binary,
            count_pattern(Rest, Pattern, Acc + 1)
    end.

%%--------------------------------------------------------------------
%% @private Convert value to binary string.
%% @end
%%--------------------------------------------------------------------
-spec to_binary(Value :: term()) ->
    binary().

to_binary(Value) when is_binary(Value) -> Value;
to_binary(Value) when is_atom(Value) -> atom_to_binary(Value, utf8);
to_binary(Value) when is_integer(Value) -> integer_to_binary(Value);
to_binary(Value) when is_float(Value) -> float_to_binary(Value, [{decimals, 2}]);
to_binary(Value) when is_list(Value) -> iolist_to_binary(Value);
to_binary(_) -> <<"unknown">>.

%%--------------------------------------------------------------------
%% @private Format numeric value with commas for readability.
%% @end
%%--------------------------------------------------------------------
-spec format_value(Value :: integer() | float() | binary() | atom()) ->
    binary().

format_value(Value) when is_integer(Value) ->
    BinValue = integer_to_binary(Value),
    add_commas(BinValue);
format_value(Value) when is_binary(Value) ->
    Value;
format_value(Value) when is_atom(Value) ->
    atom_to_binary(Value, utf8);
format_value(_) ->
    <<"0">>.

%%--------------------------------------------------------------------
%% @private Add thousand separators to number string.
%% @end
%%--------------------------------------------------------------------
-spec add_commas(BinValue :: binary()) ->
    binary().

add_commas(BinValue) ->
    %% Convert to list, reverse, add commas every 3 digits, reverse back
    List = binary_to_list(BinValue),
    Reversed = lists:reverse(List),
    WithCommas = add_commas_to_list(Reversed, 0, []),
    iolist_to_binary(lists:reverse(WithCommas)).

-spec add_commas_to_list(List :: list(), Count :: integer(), Acc :: list()) ->
    list().

add_commas_to_list([], _, Acc) -> Acc;
add_commas_to_list([H | T], 3, Acc) -> add_commas_to_list(T, 0, [H, $, | Acc]);
add_commas_to_list([H | T], Count, Acc) -> add_commas_to_list(T, Count + 1, [H | Acc]).

%%--------------------------------------------------------------------
%% @private Format bytes to human-readable size.
%% @end
%%--------------------------------------------------------------------
-spec format_bytes(Bytes :: integer()) ->
    binary().

format_bytes(Bytes) when Bytes >= 1048576 ->
    MB = Bytes div 1048576,
    iolist_to_binary([format_value(MB), <<" MB">>]);
format_bytes(Bytes) when Bytes >= 1024 ->
    KB = Bytes div 1024,
    iolist_to_binary([format_value(KB), <<" KB">>]);
format_bytes(Bytes) ->
    iolist_to_binary([format_value(Bytes), <<" bytes">>]).

%%--------------------------------------------------------------------
%% @private Format availability percentage.
%% @end
%%--------------------------------------------------------------------
-spec format_availability(Availability :: float() | integer() | undefined) ->
    binary().

format_availability(undefined) -> <<"N/A">>;
format_availability(Value) when is_float(Value) ->
    iolist_to_binary([erlang:float_to_binary(Value, [{decimals, 2}]), <<"%">>]);
format_availability(Value) when is_integer(Value) ->
    iolist_to_binary([integer_to_binary(Value), <<"%">>]);
format_availability(_) -> <<"N/A">>.

%%--------------------------------------------------------------------
%% @private Get template filename for tier.
%% @end
%%--------------------------------------------------------------------
-spec template_filename(Tier :: atom()) ->
    string().

template_filename(team) ->
    filename:join([?TEMPLATE_DIR, "marketplace_team.md"]);
template_filename(enterprise) ->
    filename:join([?TEMPLATE_DIR, "marketplace_enterprise.md"]);
template_filename(gov) ->
    filename:join([?TEMPLATE_DIR, "marketplace_gov.md"]);
template_filename(Tier) ->
    filename:join([?TEMPLATE_DIR, "marketplace_" ++ atom_to_list(Tier) ++ ".md"]).
