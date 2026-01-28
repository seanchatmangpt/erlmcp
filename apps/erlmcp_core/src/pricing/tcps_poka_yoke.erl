%%%-------------------------------------------------------------------
%% @doc Pricing Plan Poka-Yoke Validation - CI Quality Gates (v1.0.0)
%%
%% Comprehensive validation system for TCPS pricing plans ensuring:
%% - All required fields present per tier
%% - Envelope bounds realistic (throughput < 2x baseline, concurrent < 200K)
%% - All refusal codes exist in erlmcp_refusal.erl
%% - All evidence artifacts defined (SBOM/provenance/chaos/bench)
%% - Full error reporting with line numbers and remediation hints
%%
%% Usage:
%%   erlmcp_pricing_poka_yoke:validate_plan(PlanJson)
%%   {ok, []} -> Plan is valid
%%   {error, Errors} -> List of validation errors with line numbers/hints
%%
%% @end
%%%-------------------------------------------------------------------

-module(tcps_poka_yoke).

-include("erlmcp_refusal.hrl").
-include("erlmcp.hrl").

%% API - Main validation functions
-export([
    validate_plan/1,
    validate_plan_file/1,
    validate_all_plans/1
]).

%% API - Individual validators (for unit testing)
-export([
    validate_plan_schema/1,
    validate_envelope_consistency/1,
    validate_refusal_codes_exist/1,
    validate_evidence_requirements/1
]).

%% API - Utility functions
-export([
    format_error/1,
    format_errors/1
]).

-type validation_error() :: {atom(), atom(), binary(), binary(), non_neg_integer()}.

-type plan_map() :: map().

%%====================================================================
%% Main Validation Entry Points
%%====================================================================

%% @doc Validate plan map (from JSON parsing)
-spec validate_plan(plan_map()) -> {ok, []} | {error, [validation_error()]}.
validate_plan(Plan) when is_map(Plan) ->
    Validators = [
        fun validate_plan_schema/1,
        fun validate_envelope_consistency/1,
        fun validate_refusal_codes_exist/1,
        fun validate_evidence_requirements/1
    ],

    Errors = lists:flatmap(fun(Validator) ->
        case Validator(Plan) of
            {ok, []} -> [];
            {error, Errs} -> Errs;
            ok -> []
        end
    end, Validators),

    case Errors of
        [] -> {ok, []};
        _ -> {error, Errors}
    end;
validate_plan(_Data) ->
    {error, [{schema, root, <<"Plan must be a JSON object">>,
        <<"Ensure input is valid JSON map">>, 0}]}.

%% @doc Validate plan from file path
-spec validate_plan_file(file:filename()) ->
    {ok, []} | {error, [validation_error()]} | {error, file_not_found}.
validate_plan_file(Path) ->
    case file:read_file(Path) of
        {ok, Json} ->
            try
                Plan = jsx:decode(Json, [return_maps]),
                validate_plan(Plan)
            catch
                error:_ ->
                    {error, [{parse, root,
                        <<"Invalid JSON in plan file">>,
                        <<"Check JSON syntax - must be valid RFC 7158">>, 1}]}
            end;
        {error, enoent} ->
            {error, file_not_found};
        {error, Reason} ->
            {error, [{file, root,
                iolist_to_binary(io_lib:format("File read error: ~p", [Reason])),
                <<"Check file permissions">>, 0}]}
    end.

%% @doc Validate all plan files in directory
-spec validate_all_plans(file:filename()) ->
    {ok, map()} | {error, map()}.
validate_all_plans(PlansDir) ->
    case filelib:wildcard(filename:join(PlansDir, "*.plan.json")) of
        [] ->
            {error, #{
                no_plans => <<"No .plan.json files found">>,
                directory => list_to_binary(PlansDir)
            }};
        PlanFiles ->
            Results = maps:from_list([
                {filename:basename(F), validate_plan_file(F)} || F <- PlanFiles
            ]),

            case lists:partition(fun({_, R}) -> R == {ok, []} end,
                    maps:to_list(Results)) of
                {Passed, []} ->
                    {ok, maps:from_list(Passed)};
                {_, Failed} ->
                    {error, maps:from_list(Failed)}
            end
    end.

%%====================================================================
%% Validator 1: Plan Schema Validation
%%====================================================================

%% @doc Validate required fields and structure per tier
-spec validate_plan_schema(plan_map()) -> {ok, []} | {error, [validation_error()]}.
validate_plan_schema(Plan) ->
    Errors = [
        check_required_field(Plan, tier, 1),
        check_required_field(Plan, name, 2),
        check_required_field(Plan, description, 3),
        check_required_field(Plan, pricing, 4),
        check_pricing_map(maps:get(pricing, Plan, #{}), 5),
        check_required_field(Plan, envelope, 6),
        check_envelope_fields(maps:get(envelope, Plan, #{}), 7),
        check_required_field(Plan, limits, 8),
        check_limits_fields(maps:get(limits, Plan, #{}), 9),
        check_required_field(Plan, features, 10),
        check_required_field(Plan, refusal_behavior, 11),
        check_required_field(Plan, evidence, 12),
        check_evidence_fields_schema(maps:get(evidence, Plan, #{}), 13),
        check_required_field(Plan, compliance, 14)
    ],

    FilteredErrors = lists:filter(fun(E) -> E =/= ok end, Errors),
    case FilteredErrors of
        [] -> {ok, []};
        _ -> {error, FilteredErrors}
    end.

check_required_field(Map, Field, Line) ->
    case maps:find(Field, Map) of
        {ok, _} -> ok;
        error ->
            {schema, Field,
                iolist_to_binary(io_lib:format("Required field missing: ~w", [Field])),
                iolist_to_binary(io_lib:format("Add '~w' field to plan", [Field])),
                Line}
    end.

check_pricing_map(Pricing, Line) ->
    case Pricing of
        #{} ->
            Errors = [
                check_required_field(Pricing, model, Line),
                check_required_field(Pricing, description, Line + 1),
                check_required_field(Pricing, cost, Line + 2)
            ],
            case lists:filter(fun(E) -> E =/= ok end, Errors) of
                [] -> ok;
                _Errs -> {schema, pricing, <<"pricing has missing fields">>,
                    <<"Check pricing.model, pricing.description, pricing.cost">>, Line}
            end;
        _ ->
            {schema, pricing, <<"pricing must be an object">>,
                <<"Ensure pricing is a JSON object">>, Line}
    end.

check_envelope_fields(Envelope, Line) ->
    RequiredFields = [
        throughput_req_s,
        concurrent_connections,
        queue_depth_messages,
        p99_latency_ms,
        failover_sla_seconds,
        connection_timeout_seconds
    ],

    MissingFields = lists:filter(
        fun(F) -> not maps:is_key(F, Envelope) end,
        RequiredFields),

    case MissingFields of
        [] -> ok;
        Missing ->
            {schema, envelope,
                iolist_to_binary(io_lib:format(
                    "Missing envelope fields: ~w", [Missing])),
                <<"Add all required envelope metrics">>,
                Line}
    end.

check_limits_fields(Limits, Line) ->
    RequiredFields = [
        max_message_size_bytes,
        max_payload_size_mb,
        max_concurrent_requests_per_conn,
        memory_limit_mb,
        cpu_time_limit_seconds,
        backpressure_threshold_bytes
    ],

    MissingFields = lists:filter(
        fun(F) -> not maps:is_key(F, Limits) end,
        RequiredFields),

    case MissingFields of
        [] -> ok;
        Missing ->
            {schema, limits,
                iolist_to_binary(io_lib:format(
                    "Missing limits fields: ~w", [Missing])),
                <<"Add all required limit values">>,
                Line}
    end.

check_evidence_fields_schema(Evidence, Line) ->
    RequiredFields = [sbom, provenance, chaos_report, benchmark_report],

    MissingFields = lists:filter(
        fun(F) -> not maps:is_key(F, Evidence) end,
        RequiredFields),

    case MissingFields of
        [] -> ok;
        Missing ->
            {schema, evidence,
                iolist_to_binary(io_lib:format(
                    "Missing evidence artifacts: ~w", [Missing])),
                <<"Add paths to SBOM, provenance, chaos_report, benchmark_report">>,
                Line}
    end.

%%====================================================================
%% Validator 2: Envelope Consistency Validation
%%====================================================================

%% @doc Validate envelope bounds are realistic
-spec validate_envelope_consistency(plan_map()) -> {ok, []} | {error, [validation_error()]}.
validate_envelope_consistency(Plan) ->
    Envelope = maps:get(envelope, Plan, #{}),
    Limits = maps:get(limits, Plan, #{}),

    Errors = [
        check_concurrent_connections_limit(maps:get(concurrent_connections, Envelope, 0)),
        check_throughput_variance(
            maps:get(throughput_req_s, Envelope, 0),
            maps:get(concurrent_connections, Envelope, 0)),
        check_queue_depth(
            maps:get(queue_depth_messages, Envelope, 0),
            maps:get(max_message_size_bytes, Limits, 0)),
        check_p99_latency(maps:get(p99_latency_ms, Envelope, 0)),
        check_failover_sla(maps:get(failover_sla_seconds, Envelope, 0)),
        check_timeout_values(
            maps:get(failover_sla_seconds, Envelope, 0),
            maps:get(connection_timeout_seconds, Envelope, 0))
    ],

    FilteredErrors = lists:filter(fun(E) -> E =/= ok end, Errors),
    case FilteredErrors of
        [] -> {ok, []};
        _ -> {error, FilteredErrors}
    end.

check_concurrent_connections_limit(Concurrent) when Concurrent > 200000 ->
    {envelope, concurrent_connections,
        <<"Concurrent connections > 200K (unsupported)">>,
        <<"Reduce concurrent_connections to ≤200K">>,
        11};
check_concurrent_connections_limit(Concurrent) when Concurrent < 1 ->
    {envelope, concurrent_connections,
        <<"Concurrent connections must be ≥1">>,
        <<"Set concurrent_connections to valid positive integer">>,
        11};
check_concurrent_connections_limit(_) ->
    ok.

check_throughput_variance(Throughput, Concurrent) when Throughput > 0, Concurrent > 0 ->
    MaxThroughput = Concurrent * 2,  % Conservative: 2x baseline
    case Throughput > MaxThroughput of
        true ->
            {envelope, throughput_req_s,
                iolist_to_binary(io_lib:format(
                    "Throughput ~w exceeds 2x baseline (~w) for ~w connections",
                    [Throughput, MaxThroughput, Concurrent])),
                <<"Lower throughput or increase concurrent_connections">>,
                10};
        false -> ok
    end;
check_throughput_variance(_, _) ->
    ok.

check_queue_depth(QueueDepth, MaxMsgSize)
    when QueueDepth > 0, MaxMsgSize > 0 ->
    MaxQueueBytes = QueueDepth * MaxMsgSize,
    MaxAllowedBytes = 100 * 1024 * 1024 * 1024,  % 100GB hard limit

    case MaxQueueBytes > MaxAllowedBytes of
        true ->
            {envelope, queue_depth_messages,
                <<"Queue depth * message size > 100GB">>,
                <<"Reduce queue_depth_messages or max_message_size_bytes">>,
                12};
        false -> ok
    end;
check_queue_depth(_, _) ->
    ok.

check_p99_latency(P99) when P99 < 10 ->
    {envelope, p99_latency_ms,
        <<"P99 latency < 10ms (unrealistic)">>,
        <<"Set p99_latency_ms to ≥10ms">>,
        13};
check_p99_latency(P99) when P99 > 60000 ->
    {envelope, p99_latency_ms,
        <<"P99 latency > 60s (SLA violation risk)">>,
        <<"Reduce p99_latency_ms or increase throughput">>,
        13};
check_p99_latency(_) ->
    ok.

check_failover_sla(Failover) when Failover < 1 ->
    {envelope, failover_sla_seconds,
        <<"Failover SLA < 1 second (unrealistic)">>,
        <<"Set failover_sla_seconds to ≥1">>,
        14};
check_failover_sla(Failover) when Failover > 300 ->
    {envelope, failover_sla_seconds,
        <<"Failover SLA > 5 minutes (production risk)">>,
        <<"Reduce failover_sla_seconds to ≤300">>,
        14};
check_failover_sla(_) ->
    ok.

check_timeout_values(FailoverSLA, ConnectionTimeout)
    when FailoverSLA > ConnectionTimeout ->
    {envelope, connection_timeout_seconds,
        <<"connection_timeout_seconds < failover_sla_seconds (logical error)">>,
        <<"Set connection_timeout_seconds ≥ failover_sla_seconds">>,
        15};
check_timeout_values(_, _) ->
    ok.

%%====================================================================
%% Validator 3: Refusal Codes Validation
%%====================================================================

%% @doc Validate all refusal codes in plan exist in erlmcp_refusal.erl
-spec validate_refusal_codes_exist(plan_map()) -> {ok, []} | {error, [validation_error()]}.
validate_refusal_codes_exist(Plan) ->
    RefusalBehavior = maps:get(refusal_behavior, Plan, #{}),

    Errors = maps:fold(fun(Key, RefusalEntry, Acc) ->
        case RefusalEntry of
            #{<<"error_code">> := CodeStr} ->
                case validate_error_code_exists(CodeStr) of
                    ok -> Acc;
                    Error -> [Error | Acc]
                end;
            _ ->
                [{refusal, Key,
                    <<"Missing 'error_code' field in refusal_behavior entry">>,
                    iolist_to_binary(io_lib:format(
                        "Add error_code to refusal_behavior.~w", [Key])),
                    20} | Acc]
        end
    end, [], RefusalBehavior),

    case Errors of
        [] -> {ok, []};
        _ -> {error, Errors}
    end.

validate_error_code_exists(CodeStr) ->
    try
        Code = binary_to_integer(CodeStr),
        case erlmcp_refusal:is_valid_code(Code) of
            true -> ok;
            false ->
                {refusal, error_code,
                    iolist_to_binary(io_lib:format(
                        "Error code ~w does not exist in erlmcp_refusal.erl", [Code])),
                    <<"Check refusal code range (1001-1095) and verify in erlmcp_refusal.hrl">>,
                    21}
        end
    catch
        error:badarg ->
            {refusal, error_code,
                <<"error_code must be integer, got: ", CodeStr/binary>>,
                <<"Convert error_code to valid integer (1001-1095)">>,
                21}
    end.

%%====================================================================
%% Validator 4: Evidence Requirements Validation
%%====================================================================

%% @doc Validate all evidence artifacts are defined and paths valid
-spec validate_evidence_requirements(plan_map()) -> {ok, []} | {error, [validation_error()]}.
validate_evidence_requirements(Plan) ->
    Evidence = maps:get(evidence, Plan, #{}),
    Tier = maps:get(tier, Plan, unknown),

    Errors = [
        check_evidence_path(Evidence, sbom, 30),
        check_evidence_path(Evidence, provenance, 31),
        check_evidence_path(Evidence, chaos_report, 32),
        check_evidence_path(Evidence, benchmark_report, 33),
        check_evidence_for_tier(Evidence, Tier, 34)
    ],

    FilteredErrors = lists:filter(fun(E) -> E =/= ok end, Errors),
    case FilteredErrors of
        [] -> {ok, []};
        _ -> {error, FilteredErrors}
    end.

check_evidence_path(Evidence, ArtifactName, Line) ->
    case maps:find(ArtifactName, Evidence) of
        {ok, Path} when is_binary(Path), byte_size(Path) > 0 ->
            case validate_path_format(Path) of
                ok -> ok;
                {error, Reason} ->
                    {evidence, ArtifactName,
                        Reason,
                        <<"Ensure path is absolute or relative and contains no null bytes">>,
                        Line}
            end;
        {ok, Path} ->
            {evidence, ArtifactName,
                iolist_to_binary(io_lib:format(
                    "Evidence path must be non-empty string, got: ~w", [Path])),
                <<"Provide valid file path as string">>,
                Line};
        error ->
            {evidence, ArtifactName,
                iolist_to_binary(io_lib:format(
                    "Missing evidence artifact: ~w", [ArtifactName])),
                iolist_to_binary(io_lib:format(
                    "Add ~w path to evidence section", [ArtifactName])),
                Line}
    end.

check_evidence_for_tier(Evidence, gov, Line) ->
    % Government tier requires additional evidence
    Errors = [
        check_evidence_path(Evidence, audit_schema, Line + 1),
        check_evidence_path(Evidence, fips_certification, Line + 2),
        check_evidence_path(Evidence, compliance_report, Line + 3)
    ],
    case lists:filter(fun(E) -> E =/= ok end, Errors) of
        [] -> ok;
        _ -> {evidence, gov_tier,
            <<"Government tier missing required compliance evidence">>,
            <<"Add audit_schema, fips_certification, compliance_report to evidence">>,
            Line}
    end;
check_evidence_for_tier(Evidence, enterprise, Line) ->
    % Enterprise tier requires audit schema
    case maps:find(audit_schema, Evidence) of
        {ok, Path} when is_binary(Path), byte_size(Path) > 0 -> ok;
        _ ->
            {evidence, enterprise_tier,
                <<"Enterprise tier requires audit_schema in evidence">>,
                <<"Add audit_schema path to evidence section">>,
                Line}
    end;
check_evidence_for_tier(_, _, _) ->
    ok.

validate_path_format(Path) when is_binary(Path) ->
    case binary:match(Path, <<0>>) of
        nomatch ->
            case byte_size(Path) > 0 of
                true -> ok;
                false ->
                    {error, <<"Path cannot be empty">>}
            end;
        _ ->
            {error, <<"Path contains null bytes">>}
    end;
validate_path_format(_) ->
    {error, <<"Path must be a string">>}.

%%====================================================================
%% Formatting Functions
%%====================================================================

%% @doc Format single error for human display
-spec format_error(validation_error()) -> iolist().
format_error({Gate, Field, Message, Remediation, Line}) ->
    [
        "  [", atom_to_list(Gate), ":", atom_to_list(Field),
        "] Line ", integer_to_list(Line), ":\n",
        "    ERROR: ", Message, "\n",
        "    HINT:  ", Remediation, "\n"
    ].

%% @doc Format all errors for human display
-spec format_errors([validation_error()]) -> iolist().
format_errors(Errors) ->
    [
        "════════════════════════════════════════════════════════════\n",
        "  PRICING PLAN VALIDATION FAILURES\n",
        "════════════════════════════════════════════════════════════\n",
        "\n"
        | lists:flatmap(fun format_error/1, Errors)
    ] ++ [
        "\n",
        "════════════════════════════════════════════════════════════\n",
        "Total errors: ", integer_to_list(length(Errors)), "\n",
        "════════════════════════════════════════════════════════════\n"
    ].
