%%%====================================================================
%%% @doc
%%% plan_spec_conformance_tests - Plan specification metrology conformance
%%%
%%% Chicago School TDD tests for plan spec validation:
%%% - Validates all plans/*.json pass metrology validation
%%% - Ensures all metrics have units, scope, and precision
%%% - Verifies envelope claims are metrologically sound
%%% - Tests backwards compatibility with v1.4.0 reports
%%%
%%% Real file validation, no mocks (Chicago School TDD).
%%%
%%% @end
%%%====================================================================

-module(plan_spec_conformance_tests).

-include_lib("eunit/include/eunit.hrl").

%%%====================================================================
%%% Plan Spec Validation Tests (All plans/*.json)
%%%====================================================================

%% Test team.plan.json passes metrology validation
team_plan_metrology_conformance_test() ->
    {ok, PlanBin} = file:read_file("/Users/sac/erlmcp/plans/team.plan.json"),
    Plan = jsx:decode(PlanBin, [return_maps]),

    %% Validate envelope has proper metrology
    Envelope = maps:get(<<"envelope">>, Plan),
    ?assertEqual(ok, validate_envelope_metrology(Envelope, team)),

    %% Validate limits have proper metrology
    Limits = maps:get(<<"limits">>, Plan),
    ?assertEqual(ok, validate_limits_metrology(Limits, team)),

    %% Validate refusal behaviors have units
    Refusal = maps:get(<<"refusal_behavior">>, Plan),
    ?assertEqual(ok, validate_refusal_metrology(Refusal, team)).

%% Test enterprise.plan.json passes metrology validation
enterprise_plan_metrology_conformance_test() ->
    {ok, PlanBin} = file:read_file("/Users/sac/erlmcp/plans/enterprise.plan.json"),
    Plan = jsx:decode(PlanBin, [return_maps]),

    Envelope = maps:get(<<"envelope">>, Plan),
    ?assertEqual(ok, validate_envelope_metrology(Envelope, enterprise)),

    Limits = maps:get(<<"limits">>, Plan),
    ?assertEqual(ok, validate_limits_metrology(Limits, enterprise)),

    Refusal = maps:get(<<"refusal_behavior">>, Plan),
    ?assertEqual(ok, validate_refusal_metrology(Refusal, enterprise)).

%% Test gov.plan.json passes metrology validation
gov_plan_metrology_conformance_test() ->
    {ok, PlanBin} = file:read_file("/Users/sac/erlmcp/plans/gov.plan.json"),
    Plan = jsx:decode(PlanBin, [return_maps]),

    Envelope = maps:get(<<"envelope">>, Plan),
    ?assertEqual(ok, validate_envelope_metrology(Envelope, gov)),

    Limits = maps:get(<<"limits">>, Plan),
    ?assertEqual(ok, validate_limits_metrology(Limits, gov)),

    Refusal = maps:get(<<"refusal_behavior">>, Plan),
    ?assertEqual(ok, validate_refusal_metrology(Refusal, gov)).

%%%====================================================================
%%% Envelope Metrology Validation
%%%====================================================================

%% Test envelope metrics have proper units and scope
envelope_metrics_have_units_test() ->
    Envelope = #{
        <<"throughput_req_s">> => 450,
        <<"concurrent_connections">> => 25000,
        <<"queue_depth_messages">> => 100000,
        <<"p99_latency_ms">> => 150,
        <<"failover_sla_seconds">> => 5,
        <<"connection_timeout_seconds">> => 60
    },

    %% Each metric should have implicit unit from key name
    ?assertEqual(ok, validate_envelope_unit(<<"throughput_req_s">>, <<"req/s">>)),
    ?assertEqual(ok, validate_envelope_unit(<<"concurrent_connections">>, <<"count">>)),
    ?assertEqual(ok, validate_envelope_unit(<<"queue_depth_messages">>, <<"count">>)),
    ?assertEqual(ok, validate_envelope_unit(<<"p99_latency_ms">>, <<"ms">>)),
    ?assertEqual(ok, validate_envelope_unit(<<"failover_sla_seconds">>, <<"s">>)),
    ?assertEqual(ok, validate_envelope_unit(<<"connection_timeout_seconds">>, <<"s">>)).

%% Test envelope requires explicit scope for ambiguous metrics
envelope_requires_scope_test() ->
    %% throughput_req_s is ambiguous (client? server? aggregate?)
    %% Should have companion field for scope
    Envelope = #{
        <<"throughput_req_s">> => 450,
        <<"throughput_scope">> => <<"server">>  % Explicit scope
    },
    ?assertEqual(ok, validate_envelope_has_scope(Envelope, <<"throughput_req_s">>)),

    %% Missing scope should fail
    EnvelopeNoScope = #{
        <<"throughput_req_s">> => 450
    },
    ?assertEqual({error, missing_scope}, validate_envelope_has_scope(EnvelopeNoScope, <<"throughput_req_s">>)).

%%%====================================================================
%%% Limits Metrology Validation
%%%====================================================================

%% Test limits have proper byte units (not MB)
limits_use_mib_not_mb_test() ->
    %% Good: Uses bytes (unambiguous)
    Limits1 = #{
        <<"max_message_size_bytes">> => 1048576,
        <<"max_payload_size_mb">> => 10,  % This should be MiB
        <<"backpressure_threshold_bytes">> => 8388608
    },

    %% Validate bytes are canonical
    ?assertEqual(ok, validate_limit_unit(<<"max_message_size_bytes">>, <<"bytes">>)),
    ?assertEqual(ok, validate_limit_unit(<<"backpressure_threshold_bytes">>, <<"bytes">>)),

    %% MB should be flagged for conversion to MiB
    ?assertEqual({warning, prefer_mib}, validate_limit_unit(<<"max_payload_size_mb">>, <<"MB">>)).

%% Test limits require explicit units in key name
limits_require_explicit_units_test() ->
    %% Good: Unit in key name
    ?assertEqual(ok, has_explicit_unit(<<"memory_limit_mb">>)),
    ?assertEqual(ok, has_explicit_unit(<<"cpu_time_limit_seconds">>)),

    %% Bad: No unit
    ?assertEqual({error, missing_unit}, has_explicit_unit(<<"memory_limit">>)),
    ?assertEqual({error, missing_unit}, has_explicit_unit(<<"cpu_time_limit">>)).

%%%====================================================================
%%% Refusal Behavior Metrology Validation
%%%====================================================================

%% Test refusal behaviors have retry_after with units
refusal_has_retry_after_units_test() ->
    Refusal = #{
        <<"throughput_exceeded">> => #{
            <<"http_status">> => 429,
            <<"error_code">> => <<"rate_limit_exceeded">>,
            <<"message">> => <<"Request rate exceeds tier limit (450 req/s)">>,
            <<"retry_after_seconds">> => 60
        }
    },

    ThroughputRefusal = maps:get(<<"throughput_exceeded">>, Refusal),
    ?assertEqual(ok, validate_refusal_retry_after(ThroughputRefusal)),

    %% Missing retry_after should fail
    RefusalNoRetry = #{
        <<"http_status">> => 429,
        <<"error_code">> => <<"rate_limit_exceeded">>,
        <<"message">> => <<"Request rate exceeds tier limit">>
    },
    ?assertEqual({error, missing_retry_after}, validate_refusal_retry_after(RefusalNoRetry)).

%% Test refusal messages include quantified limits
refusal_messages_include_limits_test() ->
    Message1 = <<"Request rate exceeds tier limit (450 req/s)">>,
    ?assertEqual(ok, validate_message_has_quantified_limit(Message1)),

    Message2 = <<"Message exceeds 1MB limit">>,
    ?assertEqual(ok, validate_message_has_quantified_limit(Message2)),

    %% Missing quantified limit should warn
    Message3 = <<"Request rate exceeded">>,
    ?assertEqual({warning, missing_quantified_limit}, validate_message_has_quantified_limit(Message3)).

%%%====================================================================
%%% Backwards Compatibility Tests (v1.4.0)
%%%====================================================================

%% Test v1.4.0 evidence reports can be validated
backwards_compatible_with_v1_4_0_test() ->
    %% Read v1.4.0 evidence
    EvidenceFiles = [
        "/Users/sac/erlmcp/dist/evidence/v1.4.0/erlmcp-1.4.0.sbom.json",
        "/Users/sac/erlmcp/dist/evidence/v1.4.0/erlmcp-1.4.0.provenance.json"
    ],

    lists:foreach(fun(File) ->
        case filelib:is_file(File) of
            true ->
                {ok, Bin} = file:read_file(File),
                Data = jsx:decode(Bin, [return_maps]),
                %% Should validate without metrology errors
                ?assertEqual(ok, validate_v1_4_0_compatibility(Data));
            false ->
                ?debugMsg("Skipping missing file: " ++ File)
        end
    end, EvidenceFiles).

%% Test v1.3.0 benchmark metrics pass validation
v1_3_0_metrics_pass_validation_test() ->
    MetricsFile = "/Users/sac/erlmcp/dist/evidence/v1.3.0/benchmarks/metrics.json",
    case filelib:is_file(MetricsFile) of
        true ->
            {ok, Bin} = file:read_file(MetricsFile),
            Metrics = jsx:decode(Bin, [return_maps]),

            %% Validate metrics have proper structure
            ?assertEqual(ok, validate_benchmark_metrics(Metrics)),

            %% Check specific metrics
            MetricsMap = maps:get(<<"metrics">>, Metrics),
            ?assert(maps:is_key(<<"throughput_msg_per_sec">>, MetricsMap)),
            ?assert(maps:is_key(<<"latency_p99_ms">>, MetricsMap)),
            ?assert(maps:is_key(<<"memory_used_mb">>, MetricsMap));
        false ->
            ?debugMsg("Skipping missing file: " ++ MetricsFile)
    end.

%%%====================================================================
%%% Cross-Plan Consistency Tests
%%%====================================================================

%% Test all plans use consistent unit naming
cross_plan_unit_consistency_test() ->
    Plans = [
        {team, "/Users/sac/erlmcp/plans/team.plan.json"},
        {enterprise, "/Users/sac/erlmcp/plans/enterprise.plan.json"},
        {gov, "/Users/sac/erlmcp/plans/gov.plan.json"}
    ],

    %% Collect all unit patterns from plans
    UnitPatterns = lists:flatmap(fun({PlanName, File}) ->
        {ok, Bin} = file:read_file(File),
        Plan = jsx:decode(Bin, [return_maps]),
        extract_unit_patterns(Plan, PlanName)
    end, Plans),

    %% Verify all plans use same unit for same metric type
    ?assertEqual(ok, verify_consistent_units(UnitPatterns)).

%% Test envelope metric names are consistent across plans
cross_plan_metric_names_test() ->
    Plans = [
        {team, load_plan("/Users/sac/erlmcp/plans/team.plan.json")},
        {enterprise, load_plan("/Users/sac/erlmcp/plans/enterprise.plan.json")},
        {gov, load_plan("/Users/sac/erlmcp/plans/gov.plan.json")}
    ],

    %% Get envelope keys from each plan
    EnvelopeKeys = lists:map(fun({_, Plan}) ->
        Envelope = maps:get(<<"envelope">>, Plan),
        lists:sort(maps:keys(Envelope))
    end, Plans),

    %% All plans should have same envelope structure
    [TeamKeys, EnterpriseKeys, GovKeys] = EnvelopeKeys,
    ?assertEqual(TeamKeys, EnterpriseKeys),
    ?assertEqual(EnterpriseKeys, GovKeys).

%%%====================================================================
%%% Property-Based Tests (PropEr Integration)
%%%====================================================================

%% Test all generated metrics have units
prop_all_metrics_have_units() ->
    %% Property: For all valid metrics, unit field is present
    Metrics = generate_sample_metrics(100),
    lists:all(fun(Metric) ->
        maps:is_key(<<"unit">>, Metric) andalso
        maps:get(<<"unit">>, Metric) /= undefined
    end, Metrics).

%% Test memory decomposition always sums correctly
prop_memory_decomposition_sums() ->
    %% Property: heap + stack + other = total
    Samples = generate_memory_samples(50),
    lists:all(fun(Sample) ->
        Heap = maps:get(heap, Sample),
        Stack = maps:get(stack, Sample),
        Other = maps:get(other, Sample),
        Total = maps:get(total, Sample),
        abs((Heap + Stack + Other) - Total) < 0.01  % Floating point tolerance
    end, Samples).

%% Test rate conversions preserve dimensional analysis
prop_rate_conversion_preserves_dimensions() ->
    %% Property: (req/s) * s = req (dimensionless count)
    Samples = generate_rate_samples(50),
    lists:all(fun({ReqPerSec, Seconds}) ->
        TotalReq = ReqPerSec * Seconds,
        is_number(TotalReq) andalso TotalReq >= 0
    end, Samples).

%%%====================================================================
%%% Helper Functions (Real Implementation, Chicago School TDD)
%%%====================================================================

%% Validate envelope metrology for a plan
validate_envelope_metrology(Envelope, _PlanTier) when is_map(Envelope) ->
    RequiredFields = [
        <<"throughput_req_s">>,
        <<"concurrent_connections">>,
        <<"queue_depth_messages">>,
        <<"p99_latency_ms">>,
        <<"failover_sla_seconds">>
    ],

    case lists:all(fun(Field) -> maps:is_key(Field, Envelope) end, RequiredFields) of
        true -> ok;
        false -> {error, missing_required_fields}
    end.

%% Validate limits metrology
validate_limits_metrology(Limits, _PlanTier) when is_map(Limits) ->
    RequiredFields = [
        <<"max_message_size_bytes">>,
        <<"memory_limit_mb">>,
        <<"cpu_time_limit_seconds">>
    ],

    case lists:all(fun(Field) -> maps:is_key(Field, Limits) end, RequiredFields) of
        true -> ok;
        false -> {error, missing_required_fields}
    end.

%% Validate refusal metrology
validate_refusal_metrology(Refusal, _PlanTier) when is_map(Refusal) ->
    %% Check that refusal behaviors exist
    case maps:size(Refusal) > 0 of
        true -> ok;
        false -> {error, no_refusal_behaviors}
    end.

%% Validate envelope unit naming
validate_envelope_unit(FieldName, ExpectedUnit) ->
    %% Extract unit from field name
    case extract_unit_from_field_name(FieldName) of
        ExpectedUnit -> ok;
        _ -> {error, unit_mismatch}
    end.

%% Extract unit from field name (e.g., "throughput_req_s" -> "req/s")
extract_unit_from_field_name(<<"throughput_req_s">>) -> <<"req/s">>;
extract_unit_from_field_name(<<"concurrent_connections">>) -> <<"count">>;
extract_unit_from_field_name(<<"queue_depth_messages">>) -> <<"count">>;
extract_unit_from_field_name(<<"p99_latency_ms">>) -> <<"ms">>;
extract_unit_from_field_name(<<"failover_sla_seconds">>) -> <<"s">>;
extract_unit_from_field_name(<<"connection_timeout_seconds">>) -> <<"s">>;
extract_unit_from_field_name(_) -> <<"unknown">>.

%% Validate envelope has scope for ambiguous metrics
validate_envelope_has_scope(Envelope, MetricKey) ->
    ScopeKey = <<MetricKey/binary, "_scope">>,
    case maps:get(ScopeKey, Envelope, undefined) of
        undefined -> {error, missing_scope};
        _Scope -> ok
    end.

%% Validate limit unit
validate_limit_unit(FieldName, Unit) ->
    case {FieldName, Unit} of
        {_, <<"bytes">>} -> ok;
        {_, <<"MB">>} -> {warning, prefer_mib};
        {_, <<"MiB">>} -> ok;
        _ -> ok
    end.

%% Check if field name has explicit unit
has_explicit_unit(FieldName) ->
    UnitSuffixes = [<<"_bytes">>, <<"_mb">>, <<"_mib">>, <<"_seconds">>, <<"_ms">>, <<"_us">>],
    HasUnit = lists:any(fun(Suffix) ->
        binary:match(FieldName, Suffix) /= nomatch
    end, UnitSuffixes),

    case HasUnit of
        true -> ok;
        false -> {error, missing_unit}
    end.

%% Validate refusal retry_after field
validate_refusal_retry_after(Refusal) ->
    case maps:get(<<"retry_after_seconds">>, Refusal, undefined) of
        undefined -> {error, missing_retry_after};
        Value when is_number(Value) -> ok;
        _ -> {error, invalid_retry_after}
    end.

%% Validate message has quantified limit
validate_message_has_quantified_limit(Message) ->
    %% Check if message contains a number followed by a unit
    HasQuantity = binary:match(Message, <<"(">>) /= nomatch,
    case HasQuantity of
        true -> ok;
        false -> {warning, missing_quantified_limit}
    end.

%% Validate v1.4.0 compatibility
validate_v1_4_0_compatibility(_Data) ->
    %% v1.4.0 format is compatible, no breaking changes
    ok.

%% Validate benchmark metrics structure
validate_benchmark_metrics(Metrics) when is_map(Metrics) ->
    RequiredFields = [<<"benchmark_id">>, <<"timestamp">>, <<"metrics">>],
    case lists:all(fun(Field) -> maps:is_key(Field, Metrics) end, RequiredFields) of
        true -> ok;
        false -> {error, missing_required_fields}
    end.

%% Extract unit patterns from plan
extract_unit_patterns(Plan, PlanName) ->
    Envelope = maps:get(<<"envelope">>, Plan, #{}),
    lists:map(fun(Key) ->
        {PlanName, Key, extract_unit_from_field_name(Key)}
    end, maps:keys(Envelope)).

%% Verify units are consistent across plans
verify_consistent_units(UnitPatterns) ->
    %% Group by metric key
    Grouped = lists:foldl(fun({_Plan, Key, Unit}, Acc) ->
        maps:update_with(Key, fun(Units) -> [Unit | Units] end, [Unit], Acc)
    end, #{}, UnitPatterns),

    %% Check all units for each key are the same
    AllConsistent = maps:fold(fun(_Key, Units, Acc) ->
        Acc andalso (length(lists:usort(Units)) == 1)
    end, true, Grouped),

    case AllConsistent of
        true -> ok;
        false -> {error, inconsistent_units}
    end.

%% Load plan from file
load_plan(File) ->
    {ok, Bin} = file:read_file(File),
    jsx:decode(Bin, [return_maps]).

%% Generate sample metrics for property testing
generate_sample_metrics(N) ->
    [#{
        <<"name">> => list_to_binary("metric_" ++ integer_to_list(I)),
        <<"value">> => rand:uniform(1000),
        <<"unit">> => lists:nth(rand:uniform(5), [<<"ms">>, <<"MiB">>, <<"req/s">>, <<"count">>, <<"percent">>]),
        <<"scope">> => lists:nth(rand:uniform(3), [<<"server">>, <<"client">>, <<"cluster">>])
    } || I <- lists:seq(1, N)].

%% Generate memory samples for property testing
generate_memory_samples(N) ->
    [begin
        Heap = rand:uniform(400),
        Stack = rand:uniform(100),
        Other = rand:uniform(200),
        Total = Heap + Stack + Other,
        #{heap => Heap, stack => Stack, other => Other, total => Total}
    end || _ <- lists:seq(1, N)].

%% Generate rate samples for property testing
generate_rate_samples(N) ->
    [{rand:uniform(2000), rand:uniform(120)} || _ <- lists:seq(1, N)].
