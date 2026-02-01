%%%-------------------------------------------------------------------
%%% @doc
%%% Comprehensive test suite for erlmcp_metrology_validator.
%%%
%%% Tests ALL observable behavior through the public API.
%%% Uses real validator processes, never mocks or fakes.
%%%
%%% Chicago School TDD: Tests drive behavior, no implementation details.
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_metrology_validator_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Fixtures
%%====================================================================

%% Valid benchmark output fixture
valid_benchmark_output() ->
    #{workload_id => <<"core_ops_100k">>,
      transport => <<"stdio">>,
      duration_s => 10,
      scope => <<"per_node_total">>,
      precision => <<"microsecond">>,
      throughput_msg_per_s => 2690000,
      latency_p50_us => 1,
      latency_p95_us => 5,
      latency_p99_us => 10,
      memory_heap_mib_per_conn => 0.5,
      memory_rss_mib_per_node => 256}.

%%====================================================================
%% Lifecycle Tests
%%====================================================================

metrology_validator_lifecycle_test_() ->
    {setup,
     fun setup_validator/0,
     fun cleanup_validator/1,
     fun(_State) ->
        [?_test(begin
                    %% Validator is running
                    Pid = whereis(erlmcp_metrology_validator),
                    ?assert(is_pid(Pid))
                end)]
     end}.

%%====================================================================
%% validate_benchmark_output/1 Tests
%%====================================================================

validate_complete_valid_output_test() ->
    Output = valid_benchmark_output(),
    Result = erlmcp_metrology_validator:validate_benchmark_output(Output),
    ?assertMatch({ok, _}, Result),
    {ok, Details} = Result,
    ?assert(maps:is_key(status, Details)),
    ?assertEqual(valid, maps:get(status, Details)).

validate_all_required_fields_present_test() ->
    Output = valid_benchmark_output(),
    Result = erlmcp_metrology_validator:validate_benchmark_output(Output),
    ?assertMatch({ok, _}, Result).

validate_missing_workload_id_test() ->
    Output = maps:remove(workload_id, valid_benchmark_output()),
    Result = erlmcp_metrology_validator:validate_benchmark_output(Output),
    ?assertMatch({error, _}, Result),
    {error, Errors} = Result,
    ?assert(lists:any(fun(E) -> binary:match(E, <<"workload_id">>) =/= nomatch end, Errors)).

validate_missing_transport_test() ->
    Output = maps:remove(transport, valid_benchmark_output()),
    Result = erlmcp_metrology_validator:validate_benchmark_output(Output),
    ?assertMatch({error, _}, Result),
    {error, Errors} = Result,
    ?assert(lists:any(fun(E) -> binary:match(E, <<"transport">>) =/= nomatch end, Errors)).

validate_missing_duration_test() ->
    Output = maps:remove(duration_s, valid_benchmark_output()),
    Result = erlmcp_metrology_validator:validate_benchmark_output(Output),
    ?assertMatch({error, _}, Result),
    {error, Errors} = Result,
    ?assert(lists:any(fun(E) -> binary:match(E, <<"duration_s">>) =/= nomatch end, Errors)).

validate_missing_scope_test() ->
    Output = maps:remove(scope, valid_benchmark_output()),
    Result = erlmcp_metrology_validator:validate_benchmark_output(Output),
    ?assertMatch({error, _}, Result),
    {error, Errors} = Result,
    ?assert(lists:any(fun(E) ->
                         binary:match(E, <<"scope">>) =/= nomatch
                         orelse binary:match(E, <<"Scope field">>) =/= nomatch
                      end,
                      Errors)).

validate_multiple_missing_fields_test() ->
    Output = #{},
    Result = erlmcp_metrology_validator:validate_benchmark_output(Output),
    ?assertMatch({error, _}, Result),
    {error, Errors} = Result,
    ?assert(length(Errors) >= 4).  %% At least 4 required fields missing

validate_ambiguous_throughput_unit_test() ->
    Output = maps:put(<<"req/s">>, 100000, valid_benchmark_output()),
    Result = erlmcp_metrology_validator:validate_benchmark_output(Output),
    ?assertMatch({error, _}, Result).

validate_ambiguous_latency_unit_test() ->
    Output = maps:put(<<"latency_ms">>, 10, valid_benchmark_output()),
    Result = erlmcp_metrology_validator:validate_benchmark_output(Output),
    ?assertMatch({error, _}, Result).

validate_ambiguous_memory_unit_test() ->
    Output = maps:put(<<"memory">>, 512, valid_benchmark_output()),
    Result = erlmcp_metrology_validator:validate_benchmark_output(Output),
    ?assertMatch({error, _}, Result).

%%====================================================================
%% validate_metric_units/1 Tests
%%====================================================================

validate_canonical_throughput_units_test() ->
    Output = valid_benchmark_output(),
    Result = erlmcp_metrology_validator:validate_metric_units(Output),
    ?assertMatch({ok, _}, Result).

validate_canonical_latency_units_test() ->
    Output =
        #{latency_p50_us => 1,
          latency_p95_us => 5,
          latency_p99_us => 10},
    Result = erlmcp_metrology_validator:validate_metric_units(Output),
    ?assertMatch({ok, _}, Result).

validate_canonical_memory_units_test() ->
    Output = #{memory_heap_mib_per_conn => 0.5, memory_rss_mib_per_node => 256},
    Result = erlmcp_metrology_validator:validate_metric_units(Output),
    ?assertMatch({ok, _}, Result).

detect_req_per_s_as_ambiguous_test() ->
    Output = #{<<"req/s">> => 1000000},
    Violations = erlmcp_metrology_validator:detect_ambiguous_units(Output),
    ?assert(length(Violations) > 0),
    Violation = hd(Violations),
    ?assertEqual(<<"ambiguous_unit">>, maps:get(type, Violation)),
    ?assertEqual(<<"req/s">>, maps:get(field, Violation)).

detect_ops_per_s_as_ambiguous_test() ->
    Output = #{<<"ops/s">> => 500000},
    Violations = erlmcp_metrology_validator:detect_ambiguous_units(Output),
    ?assert(length(Violations) > 0),
    Violation = hd(Violations),
    ?assertEqual(<<"ambiguous_unit">>, maps:get(type, Violation)),
    ?assertEqual(<<"ops/s">>, maps:get(field, Violation)).

detect_latency_ms_as_ambiguous_test() ->
    Output = #{<<"latency_ms">> => 10},
    Violations = erlmcp_metrology_validator:detect_ambiguous_units(Output),
    ?assert(length(Violations) > 0),
    Violation = hd(Violations),
    ?assertEqual(<<"ambiguous_unit">>, maps:get(type, Violation)),
    ?assertEqual(<<"latency_ms">>, maps:get(field, Violation)).

detect_p50_ms_as_ambiguous_test() ->
    Output = #{<<"p50_ms">> => 5},
    Violations = erlmcp_metrology_validator:detect_ambiguous_units(Output),
    ?assert(length(Violations) > 0),
    Violation = hd(Violations),
    ?assertEqual(<<"p50_ms">>, maps:get(field, Violation)).

detect_p95_ms_as_ambiguous_test() ->
    Output = #{<<"p95_ms">> => 15},
    Violations = erlmcp_metrology_validator:detect_ambiguous_units(Output),
    ?assert(length(Violations) > 0),
    Violation = hd(Violations),
    ?assertEqual(<<"p95_ms">>, maps:get(field, Violation)).

detect_p99_ms_as_ambiguous_test() ->
    Output = #{<<"p99_ms">> => 25},
    Violations = erlmcp_metrology_validator:detect_ambiguous_units(Output),
    ?assert(length(Violations) > 0),
    Violation = hd(Violations),
    ?assertEqual(<<"p99_ms">>, maps:get(field, Violation)).

detect_bare_memory_as_ambiguous_test() ->
    Output = #{<<"memory">> => 512},
    Violations = erlmcp_metrology_validator:detect_ambiguous_units(Output),
    ?assert(length(Violations) > 0),
    Violation = hd(Violations),
    ?assertEqual(<<"memory">>, maps:get(field, Violation)).

detect_bare_mem_as_ambiguous_test() ->
    Output = #{<<"mem">> => 512},
    Violations = erlmcp_metrology_validator:detect_ambiguous_units(Output),
    ?assert(length(Violations) > 0),
    Violation = hd(Violations),
    ?assertEqual(<<"mem">>, maps:get(field, Violation)).

no_violations_for_canonical_units_test() ->
    Output =
        #{throughput_msg_per_s => 1000000,
          latency_p50_us => 1,
          latency_p95_us => 5,
          latency_p99_us => 10,
          memory_heap_mib_per_conn => 0.5,
          memory_rss_mib_per_node => 256},
    Violations = erlmcp_metrology_validator:detect_ambiguous_units(Output),
    ?assertEqual([], Violations).

detect_multiple_ambiguous_units_test() ->
    Output =
        #{<<"req/s">> => 1000000,
          <<"latency_ms">> => 10,
          <<"memory">> => 512},
    Violations = erlmcp_metrology_validator:detect_ambiguous_units(Output),
    ?assert(length(Violations) >= 3).

%%====================================================================
%% validate_required_fields/1 Tests
%%====================================================================

validate_required_fields_all_present_test() ->
    Output = valid_benchmark_output(),
    Result = erlmcp_metrology_validator:validate_required_fields(Output),
    ?assertMatch({ok, _}, Result).

detect_missing_workload_id_field_test() ->
    Output = maps:remove(workload_id, valid_benchmark_output()),
    Result = erlmcp_metrology_validator:validate_required_fields(Output),
    ?assertMatch({error, _}, Result),
    {error, Errors} = Result,
    ?assert(length(Errors) > 0).

detect_missing_transport_field_test() ->
    Output = maps:remove(transport, valid_benchmark_output()),
    Result = erlmcp_metrology_validator:validate_required_fields(Output),
    ?assertMatch({error, _}, Result),
    {error, Errors} = Result,
    ?assert(length(Errors) > 0).

detect_missing_duration_s_field_test() ->
    Output = maps:remove(duration_s, valid_benchmark_output()),
    Result = erlmcp_metrology_validator:validate_required_fields(Output),
    ?assertMatch({error, _}, Result),
    {error, Errors} = Result,
    ?assert(length(Errors) > 0).

detect_missing_scope_field_test() ->
    Output = maps:remove(scope, valid_benchmark_output()),
    Result = erlmcp_metrology_validator:validate_required_fields(Output),
    ?assertMatch({error, _}, Result),
    {error, Errors} = Result,
    ?assert(length(Errors) > 0).

detect_multiple_missing_fields_test() ->
    Output = #{},
    Result = erlmcp_metrology_validator:validate_required_fields(Output),
    ?assertMatch({error, _}, Result),
    {error, Errors} = Result,
    ?assert(length(Errors) >= 4).

allow_extra_fields_beyond_required_test() ->
    Output = maps:put(<<"custom_field">>, <<"value">>, valid_benchmark_output()),
    Result = erlmcp_metrology_validator:validate_required_fields(Output),
    ?assertMatch({ok, _}, Result).

precision_field_is_optional_test() ->
    Output = maps:remove(precision, valid_benchmark_output()),
    Result = erlmcp_metrology_validator:validate_required_fields(Output),
    ?assertMatch({ok, _}, Result).

%%====================================================================
%% validate_scope/1 Tests
%%====================================================================

validate_per_connection_heap_scope_test() ->
    Output = maps:put(scope, <<"per_connection_heap">>, valid_benchmark_output()),
    Result = erlmcp_metrology_validator:validate_scope(Output),
    ?assertMatch({ok, _}, Result).

validate_per_connection_total_scope_test() ->
    Output = maps:put(scope, <<"per_connection_total">>, valid_benchmark_output()),
    Result = erlmcp_metrology_validator:validate_scope(Output),
    ?assertMatch({ok, _}, Result).

validate_per_node_heap_scope_test() ->
    Output = maps:put(scope, <<"per_node_heap">>, valid_benchmark_output()),
    Result = erlmcp_metrology_validator:validate_scope(Output),
    ?assertMatch({ok, _}, Result).

validate_per_node_total_scope_test() ->
    Output = maps:put(scope, <<"per_node_total">>, valid_benchmark_output()),
    Result = erlmcp_metrology_validator:validate_scope(Output),
    ?assertMatch({ok, _}, Result).

validate_system_wide_scope_test() ->
    Output = maps:put(scope, <<"system_wide">>, valid_benchmark_output()),
    Result = erlmcp_metrology_validator:validate_scope(Output),
    ?assertMatch({ok, _}, Result).

reject_invalid_scope_test() ->
    Output = maps:put(scope, <<"invalid_scope">>, valid_benchmark_output()),
    Result = erlmcp_metrology_validator:validate_scope(Output),
    ?assertMatch({error, _}, Result),
    {error, Errors} = Result,
    ?assert(is_list(Errors)),
    ?assert(length(Errors) > 0).

reject_missing_scope_test() ->
    Output = maps:remove(scope, valid_benchmark_output()),
    Result = erlmcp_metrology_validator:validate_scope(Output),
    ?assertMatch({error, _}, Result),
    {error, Errors} = Result,
    ?assert(length(Errors) > 0).

reject_atom_scope_test() ->
    Output = maps:put(scope, per_node_total, valid_benchmark_output()),
    Result = erlmcp_metrology_validator:validate_scope(Output),
    ?assertMatch({error, _}, Result).

reject_integer_scope_test() ->
    Output = maps:put(scope, 123, valid_benchmark_output()),
    Result = erlmcp_metrology_validator:validate_scope(Output),
    ?assertMatch({error, _}, Result).

%%====================================================================
%% generate_violations_report/1 Tests
%%====================================================================

generate_clean_report_for_valid_output_test() ->
    Output = valid_benchmark_output(),
    Report = erlmcp_metrology_validator:generate_violations_report(Output),
    ?assertEqual(true, maps:get(valid, Report)),
    ?assertEqual([], maps:get(violations, Report)),
    ?assertEqual(0, maps:get(total_violations, Report)).

generate_report_with_ambiguous_units_test() ->
    Output = #{<<"req/s">> => 1000000},
    Report = erlmcp_metrology_validator:generate_violations_report(Output),
    ?assertEqual(false, maps:get(valid, Report)),
    Violations = maps:get(violations, Report),
    ?assert(length(Violations) >= 1),
    ?assertEqual(length(Violations), maps:get(total_violations, Report)).

generate_report_with_missing_fields_test() ->
    Output = #{},
    Report = erlmcp_metrology_validator:generate_violations_report(Output),
    ?assertEqual(false, maps:get(valid, Report)),
    Violations = maps:get(violations, Report),
    ?assert(length(Violations) >= 4),
    ?assertEqual(length(Violations), maps:get(total_violations, Report)).

generate_report_with_invalid_scope_test() ->
    Output = maps:put(scope, <<"bad_scope">>, valid_benchmark_output()),
    Report = erlmcp_metrology_validator:generate_violations_report(Output),
    ?assertEqual(false, maps:get(valid, Report)),
    Violations = maps:get(violations, Report),
    ?assert(length(Violations) > 0).

generate_report_with_multiple_violations_test() ->
    Output = #{<<"req/s">> => 1000000, <<"latency_ms">> => 10},
    Report = erlmcp_metrology_validator:generate_violations_report(Output),
    ?assertEqual(false, maps:get(valid, Report)),
    Violations = maps:get(violations, Report),
    ?assert(length(Violations) >= 2).

report_includes_timestamp_test() ->
    Output = valid_benchmark_output(),
    Report = erlmcp_metrology_validator:generate_violations_report(Output),
    ?assert(maps:is_key(timestamp, Report)),
    Timestamp = maps:get(timestamp, Report),
    ?assert(is_integer(Timestamp)),
    ?assert(Timestamp > 0).

violation_type_is_ambiguous_unit_test() ->
    Output = #{<<"req/s">> => 1000000},
    Report = erlmcp_metrology_validator:generate_violations_report(Output),
    Violations = maps:get(violations, Report),
    Violation = hd(Violations),
    ?assertEqual(<<"ambiguous_unit">>, maps:get(type, Violation)).

violation_type_is_missing_required_test() ->
    Output = #{},
    Report = erlmcp_metrology_validator:generate_violations_report(Output),
    Violations = maps:get(violations, Report),
    ?assert(lists:any(fun(V) -> maps:get(type, V, <<>>) =:= <<"missing_required">> end,
                      Violations)).

violation_type_is_invalid_scope_test() ->
    Output = maps:put(scope, <<"bad">>, valid_benchmark_output()),
    Report = erlmcp_metrology_validator:generate_violations_report(Output),
    Violations = maps:get(violations, Report),
    ?assert(lists:any(fun(V) -> maps:get(type, V, <<>>) =:= <<"invalid_scope">> end, Violations)).

%%====================================================================
%% get_validation_rules/1 Tests
%%====================================================================

get_validation_rules_test() ->
    {ok, Pid} = erlmcp_metrology_validator:start_link(),
    Rules = erlmcp_metrology_validator:get_validation_rules(),
    ?assert(is_map(Rules)),
    ?assert(maps:is_key(throughput, Rules)),
    ?assert(maps:is_key(latency, Rules)),
    ?assert(maps:is_key(memory, Rules)),
    ?assert(maps:is_key(required_fields, Rules)),
    gen_server:stop(Pid).

throughput_rules_include_canonical_unit_test() ->
    {ok, Pid} = erlmcp_metrology_validator:start_link(),
    Rules = erlmcp_metrology_validator:get_validation_rules(),
    ThroughputRules = maps:get(throughput, Rules),
    ?assertEqual(<<"msg_per_s">>, maps:get(canonical, ThroughputRules)),
    gen_server:stop(Pid).

throughput_rules_include_forbidden_units_test() ->
    {ok, Pid} = erlmcp_metrology_validator:start_link(),
    Rules = erlmcp_metrology_validator:get_validation_rules(),
    ThroughputRules = maps:get(throughput, Rules),
    Forbidden = maps:get(forbidden, ThroughputRules),
    ?assert(lists:member(<<"req/s">>, Forbidden)),
    ?assert(lists:member(<<"ops/s">>, Forbidden)),
    gen_server:stop(Pid).

latency_rules_include_canonical_unit_test() ->
    {ok, Pid} = erlmcp_metrology_validator:start_link(),
    Rules = erlmcp_metrology_validator:get_validation_rules(),
    LatencyRules = maps:get(latency, Rules),
    ?assertEqual(<<"_us">>, maps:get(canonical, LatencyRules)),
    gen_server:stop(Pid).

latency_rules_include_percentiles_test() ->
    {ok, Pid} = erlmcp_metrology_validator:start_link(),
    Rules = erlmcp_metrology_validator:get_validation_rules(),
    LatencyRules = maps:get(latency, Rules),
    Percentiles = maps:get(percentiles, LatencyRules),
    ?assert(lists:member(<<"p50_us">>, Percentiles)),
    ?assert(lists:member(<<"p95_us">>, Percentiles)),
    ?assert(lists:member(<<"p99_us">>, Percentiles)),
    gen_server:stop(Pid).

memory_rules_include_scopes_test() ->
    {ok, Pid} = erlmcp_metrology_validator:start_link(),
    Rules = erlmcp_metrology_validator:get_validation_rules(),
    MemoryRules = maps:get(memory, Rules),
    Scopes = maps:get(scopes, MemoryRules),
    ?assert(lists:member(<<"per_connection_heap">>, Scopes)),
    ?assert(lists:member(<<"per_node_total">>, Scopes)),
    gen_server:stop(Pid).

required_fields_list_test() ->
    {ok, Pid} = erlmcp_metrology_validator:start_link(),
    Rules = erlmcp_metrology_validator:get_validation_rules(),
    RequiredFields = maps:get(required_fields, Rules),
    ?assert(lists:keymember(workload_id, 1, RequiredFields)),
    ?assert(lists:keymember(transport, 1, RequiredFields)),
    ?assert(lists:keymember(duration_s, 1, RequiredFields)),
    ?assert(lists:keymember(scope, 1, RequiredFields)),
    gen_server:stop(Pid).

%%====================================================================
%% Integration Tests
%%====================================================================

real_world_benchmark_validation_test() ->
    %% Real benchmark output from core_ops
    Output =
        #{workload_id => <<"core_ops_100k">>,
          transport => <<"stdio">>,
          duration_s => 10,
          scope => <<"per_node_total">>,
          precision => <<"high_resolution">>,
          throughput_msg_per_s => 2690000,
          latency_p50_us => 1,
          latency_p95_us => 5,
          latency_p99_us => 10,
          memory_heap_mib_per_conn => 0.5,
          memory_rss_mib_per_node => 256,
          timestamp => erlang:system_time(millisecond)},
    Result = erlmcp_metrology_validator:validate_benchmark_output(Output),
    ?assertMatch({ok, _}, Result).

tcp_benchmark_validation_test() ->
    Output =
        #{workload_id => <<"tcp_sustained_10k">>,
          transport => <<"tcp">>,
          duration_s => 60,
          scope => <<"per_connection_total">>,
          throughput_msg_per_s => 43000,
          latency_p50_us => 100,
          latency_p95_us => 500,
          latency_p99_us => 1000,
          memory_heap_mib_per_conn => 2.5,
          memory_rss_mib_per_node => 512},
    Result = erlmcp_metrology_validator:validate_benchmark_output(Output),
    ?assertMatch({ok, _}, Result).

http_benchmark_validation_test() ->
    Output =
        #{workload_id => <<"http_sustained_5k">>,
          transport => <<"http">>,
          duration_s => 30,
          scope => <<"per_node_total">>,
          throughput_msg_per_s => 25000,
          latency_p50_us => 150,
          latency_p95_us => 750,
          latency_p99_us => 1500,
          memory_heap_mib_per_conn => 3.0,
          memory_rss_mib_per_node => 768},
    Result = erlmcp_metrology_validator:validate_benchmark_output(Output),
    ?assertMatch({ok, _}, Result).

reject_legacy_benchmark_format_test() ->
    %% Legacy format with ambiguous units (req/s instead of msg_per_s)
    Output =
        #{workload_id => <<"legacy_benchmark">>,
          transport => <<"stdio">>,
          duration_s => 10,
          scope => <<"per_node_total">>,
          <<"req/s">> => 1000000,  %% Ambiguous unit
          <<"latency_ms">> => 10,  %% Should be _us
          <<"memory">> => 512},      %% Missing scope specification
    Result = erlmcp_metrology_validator:validate_benchmark_output(Output),
    ?assertMatch({error, _}, Result).

%%====================================================================
%% Setup/Teardown
%%====================================================================

setup_validator() ->
    %% Start validator for tests that need it
    {ok, Pid} = erlmcp_metrology_validator:start_link(),
    Pid.

cleanup_validator(_Pid) ->
    case whereis(erlmcp_metrology_validator) of
        undefined ->
            ok;
        Pid ->
            gen_server:stop(Pid)
    end.
