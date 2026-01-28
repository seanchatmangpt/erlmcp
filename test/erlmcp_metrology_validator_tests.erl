%%%-------------------------------------------------------------------
%%% @doc erlmcp_metrology_validator_tests - Comprehensive test suite
%%%
%%% Tests all validation rules, error reporting, and edge cases
%%% Following Chicago School TDD - behavior-driven testing
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_metrology_validator_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Fixtures
%%====================================================================

valid_metric() ->
    #{
        <<"name">> => <<"latency_p99">>,
        <<"value">> => 5.234,
        <<"unit">> => <<"ms">>,
        <<"precision_us">> => 5234
    }.

valid_composite_metric() ->
    #{
        <<"name">> => <<"memory_per_connection">>,
        <<"value">> => 2.5,
        <<"unit">> => <<"MiB/conn">>
    }.

valid_report() ->
    #{
        <<"workload_id">> => <<"bench_100k">>,
        <<"transport">> => <<"stdio">>,
        <<"duration_seconds">> => 30,
        <<"plan">> => <<"team">>,
        <<"metrics">> => #{
            <<"throughput">> => #{
                <<"value">> => 95000,
                <<"unit">> => <<"req/s">>
            },
            <<"latency_p99">> => #{
                <<"value">> => 8.5,
                <<"unit">> => <<"ms">>,
                <<"precision_us">> => 8500
            }
        }
    }.

invalid_report_missing_unit() ->
    #{
        <<"workload_id">> => <<"test">>,
        <<"transport">> => <<"tcp">>,
        <<"duration_seconds">> => 10,
        <<"metrics">> => #{
            <<"throughput">> => #{
                <<"value">> => 1000
                %% Missing unit field
            }
        }
    }.

invalid_report_non_canonical_unit() ->
    #{
        <<"workload_id">> => <<"test">>,
        <<"transport">> => <<"tcp">>,
        <<"duration_seconds">> => 10,
        <<"metrics">> => #{
            <<"memory">> => #{
                <<"value">> => 100,
                <<"unit">> => <<"MB">>  % Should be MiB
            }
        }
    }.

%%====================================================================
%% Metric Validation Tests
%%====================================================================

validate_metric_valid_test() ->
    Metric = valid_metric(),
    Result = erlmcp_metrology_validator:validate_metric(Metric),
    % Should pass - no explicit "per_" pattern
    ?assertEqual(ok, Result).

validate_metric_missing_unit_test() ->
    Metric = maps:remove(<<"unit">>, valid_metric()),
    Result = erlmcp_metrology_validator:validate_metric(Metric),
    % Should have missing_unit violation (may have others too)
    ?assertMatch({error, Violations} when is_list(Violations), Result),
    {error, Violations} = Result,
    ?assert(lists:any(fun(#{type := Type}) -> Type == missing_unit end, Violations)).

validate_metric_invalid_unit_test() ->
    Metric = (valid_metric())#{<<"unit">> => <<"invalid_unit">>},
    Result = erlmcp_metrology_validator:validate_metric(Metric),
    % Should have invalid_unit violation (may have others too)
    ?assertMatch({error, Violations} when is_list(Violations), Result),
    {error, Violations} = Result,
    ?assert(lists:any(fun(#{type := Type}) -> Type == invalid_unit end, Violations)).

validate_metric_composite_valid_test() ->
    Metric = valid_composite_metric(),
    ?assertEqual(ok, erlmcp_metrology_validator:validate_metric(Metric)).

validate_metric_missing_precision_test() ->
    Metric = maps:remove(<<"precision_us">>, valid_metric()),
    Result = erlmcp_metrology_validator:validate_metric(Metric),
    % Should have missing_precision violation (may have others too)
    ?assertMatch({error, Violations} when is_list(Violations), Result),
    {error, Violations} = Result,
    ?assert(lists:any(fun(#{type := Type}) -> Type == missing_precision end, Violations)).

validate_metric_zero_time_without_precision_test() ->
    Metric = #{
        <<"name">> => <<"latency">>,
        <<"value">> => 0.0,
        <<"unit">> => <<"ms">>
        %% Missing precision_us
    },
    Result = erlmcp_metrology_validator:validate_metric(Metric),
    ?assertMatch({error, [_ | _]}, Result),
    {error, Violations} = Result,
    Types = [maps:get(type, V) || V <- Violations],
    ?assert(lists:member(zero_time_without_precision, Types)).

validate_metric_inconsistent_precision_test() ->
    Metric = #{
        <<"name">> => <<"latency">>,
        <<"value">> => 5.0,        % 5 ms = 5000 µs
        <<"unit">> => <<"ms">>,
        <<"precision_us">> => 3000  % Inconsistent: says 3000 µs
    },
    Result = erlmcp_metrology_validator:validate_metric(Metric),
    ?assertMatch({error, [#{type := inconsistent_precision}]}, Result).

%%====================================================================
%% Report Validation Tests
%%====================================================================

validate_report_valid_test() ->
    Report = valid_report(),
    ?assertEqual(ok, erlmcp_metrology_validator:validate_report(Report)).

validate_report_missing_workload_id_test() ->
    Report = maps:remove(<<"workload_id">>, valid_report()),
    Result = erlmcp_metrology_validator:validate_report(Report),
    ?assertMatch({error, [_ | _]}, Result),
    {error, Violations} = Result,
    ?assert(lists:any(
        fun(#{field := Field}) -> Field == <<"workload_id">> end,
        Violations
    )).

validate_report_missing_transport_test() ->
    Report = maps:remove(<<"transport">>, valid_report()),
    Result = erlmcp_metrology_validator:validate_report(Report),
    ?assertMatch({error, [_ | _]}, Result).

validate_report_missing_duration_test() ->
    Report = maps:remove(<<"duration_seconds">>, valid_report()),
    Result = erlmcp_metrology_validator:validate_report(Report),
    ?assertMatch({error, [_ | _]}, Result).

validate_report_invalid_plan_test() ->
    Report = (valid_report())#{<<"plan">> => <<"invalid_plan">>},
    Result = erlmcp_metrology_validator:validate_report(Report),
    % Should have invalid_plan violation
    ?assertMatch({error, Violations} when is_list(Violations), Result),
    {error, Violations} = Result,
    ?assert(lists:any(fun(#{type := Type}) -> Type == invalid_plan end, Violations)).

validate_report_nested_metrics_test() ->
    Report = #{
        <<"workload_id">> => <<"test">>,
        <<"transport">> => <<"stdio">>,
        <<"duration_seconds">> => 10,
        <<"nested">> => #{
            <<"level1">> => #{
                <<"level2">> => #{
                    <<"metric">> => #{
                        <<"value">> => 100,
                        <<"unit">> => <<"req/s">>
                    }
                }
            }
        }
    },
    ?assertEqual(ok, erlmcp_metrology_validator:validate_report(Report)).

validate_report_list_metrics_test() ->
    Report = #{
        <<"workload_id">> => <<"test">>,
        <<"transport">> => <<"stdio">>,
        <<"duration_seconds">> => 10,
        <<"percentiles">> => [
            #{<<"p">> => 50, <<"value">> => 5.0, <<"unit">> => <<"ms">>, <<"precision_us">> => 5000},
            #{<<"p">> => 95, <<"value">> => 10.0, <<"unit">> => <<"ms">>, <<"precision_us">> => 10000},
            #{<<"p">> => 99, <<"value">> => 15.0, <<"unit">> => <<"ms">>, <<"precision_us">> => 15000}
        ]
    },
    ?assertEqual(ok, erlmcp_metrology_validator:validate_report(Report)).

validate_report_multiple_violations_test() ->
    Report = #{
        %% Missing workload_id, transport, duration
        <<"metrics">> => #{
            <<"bad_metric1">> => #{
                <<"value">> => 100
                %% Missing unit
            },
            <<"bad_metric2">> => #{
                <<"value">> => 200,
                <<"unit">> => <<"invalid">>
            }
        }
    },
    Result = erlmcp_metrology_validator:validate_report(Report),
    ?assertMatch({error, Violations} when length(Violations) >= 3, Result).

%%====================================================================
%% Plan Validation Tests
%%====================================================================

validate_plan_valid_test() ->
    PlanSpec = #{
        <<"plan">> => <<"team">>,
        <<"envelope">> => #{
            <<"throughput">> => #{
                <<"value">> => 100000,
                <<"unit">> => <<"req/s">>
            },
            <<"latency_p99">> => #{
                <<"value">> => 10.0,
                <<"unit">> => <<"ms">>,
                <<"precision_us">> => 10000
            }
        }
    },
    ?assertEqual(ok, erlmcp_metrology_validator:validate_plan(PlanSpec)).

validate_plan_missing_envelope_test() ->
    PlanSpec = #{<<"plan">> => <<"team">>},
    Result = erlmcp_metrology_validator:validate_plan(PlanSpec),
    % Should have missing_field violation for envelope
    ?assertMatch({error, Violations} when is_list(Violations), Result),
    {error, Violations} = Result,
    ?assert(lists:any(fun(#{type := Type, field := Field}) ->
        Type == missing_field andalso Field == <<"envelope">>
    end, Violations)).

%%====================================================================
%% Canonical Unit Tests
%%====================================================================

canonical_unit_already_canonical_test() ->
    ?assertEqual({ok, <<"MiB">>}, erlmcp_metrology_validator:canonical_unit(<<"MiB">>)),
    ?assertEqual({ok, <<"µs">>}, erlmcp_metrology_validator:canonical_unit(<<"µs">>)),
    ?assertEqual({ok, <<"req/s">>}, erlmcp_metrology_validator:canonical_unit(<<"req/s">>)).

canonical_unit_needs_conversion_test() ->
    ?assertEqual({ok, <<"MiB">>}, erlmcp_metrology_validator:canonical_unit(<<"MB">>)),
    ?assertEqual({ok, <<"KiB">>}, erlmcp_metrology_validator:canonical_unit(<<"KB">>)),
    ?assertEqual({ok, <<"GiB">>}, erlmcp_metrology_validator:canonical_unit(<<"GB">>)),
    ?assertEqual({ok, <<"µs">>}, erlmcp_metrology_validator:canonical_unit(<<"ms">>)),
    ?assertEqual({ok, <<"req/s">>}, erlmcp_metrology_validator:canonical_unit(<<"req/sec">>)).

canonical_unit_unknown_test() ->
    ?assertEqual({error, unknown_unit},
                 erlmcp_metrology_validator:canonical_unit(<<"invalid">>)).

%%====================================================================
%% Memory Decomposition Tests
%%====================================================================

decompose_memory_simple_test() ->
    ?assertEqual({ok, {<<"MiB">>, <<>>}},
                 erlmcp_metrology_validator:decompose_memory(<<"MiB">>)).

decompose_memory_with_scope_test() ->
    ?assertEqual({ok, {<<"MiB">>, <<"/conn">>}},
                 erlmcp_metrology_validator:decompose_memory(<<"MiB/conn">>)),
    ?assertEqual({ok, {<<"KiB">>, <<"/msg">>}},
                 erlmcp_metrology_validator:decompose_memory(<<"KiB/msg">>)).

decompose_memory_invalid_base_test() ->
    Result = erlmcp_metrology_validator:decompose_memory(<<"MB/conn">>),
    ?assertMatch({error, {invalid_base_unit, <<"MB">>}}, Result).

decompose_memory_invalid_scope_test() ->
    Result = erlmcp_metrology_validator:decompose_memory(<<"MiB/invalid">>),
    ?assertMatch({error, {invalid_scope, _}}, Result).

%%====================================================================
%% Violation Formatting Tests
%%====================================================================

format_violation_test() ->
    Violation = #{
        type => missing_unit,
        field => <<"unit">>,
        location => <<"report.metrics.throughput">>,
        expected => <<"unit string">>,
        actual => <<"undefined">>,
        suggestion => <<"Add 'unit' field with canonical unit">>
    },
    Formatted = erlmcp_metrology_validator:format_violation(Violation),
    ?assert(is_binary(Formatted)),
    ?assert(byte_size(Formatted) > 0),
    %% Check contains key parts
    ?assert(binary:match(Formatted, <<"missing_unit">>) =/= nomatch),
    ?assert(binary:match(Formatted, <<"unit">>) =/= nomatch).

%%====================================================================
%% File Validation Tests
%%====================================================================

validate_file_valid_test() ->
    %% Create temporary valid report file
    TempFile = "/tmp/erlmcp_test_valid_report.json",
    Report = valid_report(),
    ok = file:write_file(TempFile, jsx:encode(Report)),

    Result = erlmcp_metrology_validator:validate_file(TempFile),
    file:delete(TempFile),

    ?assertEqual(ok, Result).

validate_file_invalid_json_test() ->
    TempFile = "/tmp/erlmcp_test_invalid_json.json",
    ok = file:write_file(TempFile, <<"not valid json {">>),

    Result = erlmcp_metrology_validator:validate_file(TempFile),
    file:delete(TempFile),

    ?assertMatch({error, [#{type := json_parse_error}]}, Result).

validate_file_not_found_test() ->
    Result = erlmcp_metrology_validator:validate_file("/nonexistent/file.json"),
    ?assertMatch({error, [#{type := file_error}]}, Result).

validate_file_with_violations_test() ->
    TempFile = "/tmp/erlmcp_test_invalid_report.json",
    Report = invalid_report_missing_unit(),
    ok = file:write_file(TempFile, jsx:encode(Report)),

    Result = erlmcp_metrology_validator:validate_file(TempFile),
    file:delete(TempFile),

    ?assertMatch({error, [_ | _]}, Result).

%%====================================================================
%% Integration Tests - Real-world Scenarios
%%====================================================================

integration_benchmark_report_test() ->
    Report = #{
        <<"workload_id">> => <<"benchmark_100k">>,
        <<"transport">> => <<"stdio">>,
        <<"duration_seconds">> => 30,
        <<"plan">> => <<"team">>,
        <<"timestamp">> => <<"2026-01-27T20:00:00Z">>,
        <<"results">> => #{
            <<"throughput">> => #{
                <<"value">> => 95000,
                <<"unit">> => <<"req/s">>
            },
            <<"latency">> => #{
                <<"p50">> => #{<<"value">> => 3.2, <<"unit">> => <<"ms">>, <<"precision_us">> => 3200},
                <<"p95">> => #{<<"value">> => 7.5, <<"unit">> => <<"ms">>, <<"precision_us">> => 7500},
                <<"p99">> => #{<<"value">> => 9.8, <<"unit">> => <<"ms">>, <<"precision_us">> => 9800}
            },
            <<"memory">> => #{
                <<"rss">> => #{<<"value">> => 256, <<"unit">> => <<"MiB">>},
                <<"per_connection">> => #{<<"value">> => 2.5, <<"unit">> => <<"MiB/conn">>}
            }
        }
    },
    ?assertEqual(ok, erlmcp_metrology_validator:validate_report(Report)).

integration_chaos_report_test() ->
    Report = #{
        <<"workload_id">> => <<"chaos_connection_failure">>,
        <<"transport">> => <<"tcp">>,
        <<"duration_seconds">> => 60,
        <<"plan">> => <<"enterprise">>,
        <<"scenario">> => <<"connection_failure">>,
        <<"results">> => #{
            <<"failover_time">> => #{
                <<"value">> => 2.5,
                <<"unit">> => <<"s">>,
                <<"precision_us">> => 2500000
            },
            <<"recovery_rate">> => #{
                <<"value">> => 0.98,
                <<"unit">> => <<"ratio">>
            },
            <<"error_rate">> => #{
                <<"value">> => 2.5,
                <<"unit">> => <<"%">>
            }
        }
    },
    ?assertEqual(ok, erlmcp_metrology_validator:validate_report(Report)).

integration_conformance_report_test() ->
    Report = #{
        <<"plan">> => <<"gov">>,
        <<"timestamp">> => 1738012800,
        <<"envelope">> => #{
            <<"throughput_req_s">> => #{
                <<"limit">> => 80000,
                <<"unit">> => <<"req/s">>
            },
            <<"p99_latency_ms">> => #{
                <<"limit">> => 15.0,
                <<"unit">> => <<"ms">>,
                <<"precision_us">> => 15000
            }
        },
        <<"benchmark_conformance">> => #{
            <<"throughput_req_s">> => #{
                <<"actual">> => 85000,
                <<"unit">> => <<"req/s">>,
                <<"status">> => <<"pass">>
            }
        }
    },
    ?assertEqual(ok, erlmcp_metrology_validator:validate_report(Report)).

%%====================================================================
%% Edge Cases and Boundary Tests
%%====================================================================

edge_case_empty_report_test() ->
    Result = erlmcp_metrology_validator:validate_report(#{}),
    ?assertMatch({error, Violations} when length(Violations) >= 3, Result).

edge_case_non_map_input_test() ->
    ?assertMatch({error, [#{type := invalid_input}]},
                 erlmcp_metrology_validator:validate_report([])),
    ?assertMatch({error, [#{type := invalid_input}]},
                 erlmcp_metrology_validator:validate_metric(<<"not a map">>)).

edge_case_nested_empty_maps_test() ->
    Report = #{
        <<"workload_id">> => <<"test">>,
        <<"transport">> => <<"stdio">>,
        <<"duration_seconds">> => 1,
        <<"metrics">> => #{},
        <<"nested">> => #{}
    },
    ?assertEqual(ok, erlmcp_metrology_validator:validate_report(Report)).

edge_case_time_precision_boundary_test() ->
    %% Test 1% tolerance boundary
    Metric = #{
        <<"value">> => 5.0,        % 5000 µs
        <<"unit">> => <<"ms">>,
        <<"precision_us">> => 5005  % Within 1% tolerance
    },
    ?assertEqual(ok, erlmcp_metrology_validator:validate_metric(Metric)).

edge_case_scope_detection_test() ->
    %% Metrics with "memory_per_" pattern should suggest scope
    Metric = #{
        <<"name">> => <<"memory_per_connection">>,
        <<"value">> => 100,
        <<"unit">> => <<"MiB">>  % No scope suffix
    },
    Result = erlmcp_metrology_validator:validate_metric(Metric),
    % Should suggest adding scope since name has "memory_per_"
    ?assertMatch({error, Violations} when is_list(Violations), Result),
    {error, Violations} = Result,
    ?assert(lists:any(fun(#{type := Type}) -> Type == missing_scope end, Violations)).

%%====================================================================
%% Performance and Stress Tests
%%====================================================================

stress_test_many_violations_test() ->
    %% Report with 100 invalid metrics
    InvalidMetrics = maps:from_list([
        {integer_to_binary(N), #{<<"value">> => N}}  % All missing units
        || N <- lists:seq(1, 100)
    ]),

    Report = #{
        <<"workload_id">> => <<"stress">>,
        <<"transport">> => <<"stdio">>,
        <<"duration_seconds">> => 1,
        <<"metrics">> => InvalidMetrics
    },

    Result = erlmcp_metrology_validator:validate_report(Report),
    % Should collect violations for all invalid metrics
    ?assertMatch({error, Violations} when length(Violations) >= 100, Result).

stress_test_deeply_nested_test() ->
    %% Test deeply nested metric structures
    DeepNested = lists:foldl(
        fun(N, Acc) ->
            Key = integer_to_binary(N),
            #{Key => Acc}
        end,
        #{<<"metric">> => #{<<"value">> => 100, <<"unit">> => <<"ops/s">>}},  % Use ops/s to avoid scope issues
        lists:seq(1, 20)
    ),

    Report = #{
        <<"workload_id">> => <<"deep">>,
        <<"transport">> => <<"stdio">>,
        <<"duration_seconds">> => 1,
        <<"nested">> => DeepNested
    },

    Result = erlmcp_metrology_validator:validate_report(Report),
    ?assertEqual(ok, Result).
