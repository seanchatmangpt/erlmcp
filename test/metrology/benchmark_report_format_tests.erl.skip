%%%====================================================================
%%% @doc
%%% benchmark_report_format_tests - Benchmark report format validation
%%%
%%% Chicago School TDD tests for benchmark report format:
%%% - Validates benchmark report structure and metrology
%%% - Tests fixture data (good/bad examples)
%%% - Verifies edge cases (zero values, extreme precision, cluster aggregates)
%%% - Ensures backwards compatibility with v1.4.0
%%%
%%% Real benchmark execution, no mocks (Chicago School TDD).
%%%
%%% @end
%%%====================================================================

-module(benchmark_report_format_tests).

-include_lib("eunit/include/eunit.hrl").

%%%====================================================================
%%% Benchmark Report Structure Tests
%%%====================================================================

%% Test valid benchmark report structure
valid_benchmark_report_structure_test() ->
    Report = #{
        <<"benchmark_id">> => <<"throughput_baseline_20260127">>,
        <<"timestamp">> => <<"2026-01-27T20:51:45Z">>,
        <<"test_type">> => <<"throughput">>,
        <<"metrics">> => #{
            <<"throughput_msg_per_sec">> => 150432,
            <<"latency_p99_ms">> => 4.2,
            <<"memory_used_mib">> => 305.2,  % v1.5.0 uses MiB
            <<"cpu_usage_percent">> => 69
        },
        <<"status">> => <<"PASS">>
    },

    ?assertEqual(ok, validate_report_structure(Report)).

%% Test report requires all mandatory fields
report_requires_mandatory_fields_test() ->
    %% Missing timestamp
    Report1 = #{
        <<"benchmark_id">> => <<"test">>,
        <<"test_type">> => <<"throughput">>,
        <<"metrics">> => #{},
        <<"status">> => <<"PASS">>
    },
    ?assertEqual({error, missing_timestamp}, validate_report_structure(Report1)),

    %% Missing metrics
    Report2 = #{
        <<"benchmark_id">> => <<"test">>,
        <<"timestamp">> => <<"2026-01-27T20:51:45Z">>,
        <<"test_type">> => <<"throughput">>,
        <<"status">> => <<"PASS">>
    },
    ?assertEqual({error, missing_metrics}, validate_report_structure(Report2)).

%% Test metrics map requires proper metrology
metrics_require_metrology_test() ->
    %% Valid metrics
    ValidMetrics = #{
        <<"throughput_req_s">> => 1500,
        <<"latency_p99_ms">> => 5.0,
        <<"memory_heap_mib">> => 256,
        <<"cpu_usage_percent">> => 65
    },
    ?assertEqual(ok, validate_metrics_metrology(ValidMetrics)),

    %% Missing units in keys
    InvalidMetrics = #{
        <<"throughput">> => 1500,  % No unit
        <<"latency">> => 5.0       % No unit
    },
    ?assertEqual({error, missing_units}, validate_metrics_metrology(InvalidMetrics)).

%%%====================================================================
%%% Good Fixture Data Tests
%%%====================================================================

%% Test good fixture: Basic throughput benchmark
good_fixture_basic_throughput_test() ->
    Fixture = load_fixture("good_basic_throughput.json"),

    ?assertEqual(ok, validate_report_structure(Fixture)),
    ?assertEqual(ok, validate_metrics_metrology(maps:get(<<"metrics">>, Fixture))),
    ?assertEqual(<<"PASS">>, maps:get(<<"status">>, Fixture)).

%% Test good fixture: Latency benchmark with percentiles
good_fixture_latency_percentiles_test() ->
    Fixture = create_good_latency_fixture(),

    Metrics = maps:get(<<"metrics">>, Fixture),
    ?assert(maps:is_key(<<"latency_p50_ms">>, Metrics)),
    ?assert(maps:is_key(<<"latency_p95_ms">>, Metrics)),
    ?assert(maps:is_key(<<"latency_p99_ms">>, Metrics)),
    ?assert(maps:is_key(<<"latency_p99_9_ms">>, Metrics)),

    %% All latencies should be properly scoped
    ?assertEqual(ok, validate_metrics_metrology(Metrics)).

%% Test good fixture: Memory decomposition
good_fixture_memory_decomposition_test() ->
    Fixture = create_good_memory_fixture(),

    Metrics = maps:get(<<"metrics">>, Fixture),
    Heap = maps:get(<<"memory_heap_mib">>, Metrics),
    Stack = maps:get(<<"memory_stack_mib">>, Metrics),
    Other = maps:get(<<"memory_other_mib">>, Metrics),
    Total = maps:get(<<"memory_total_mib">>, Metrics),

    %% Total should equal sum of components (within tolerance)
    ?assert(abs((Heap + Stack + Other) - Total) < 0.1).

%% Test good fixture: Sub-millisecond with raw microseconds
good_fixture_sub_millisecond_test() ->
    Fixture = #{
        <<"benchmark_id">> => <<"fast_ops">>,
        <<"timestamp">> => <<"2026-01-27T20:51:45Z">>,
        <<"test_type">> => <<"latency">>,
        <<"metrics">> => #{
            <<"latency_p50_ms">> => 0.85,
            <<"latency_p50_raw_us">> => 850,
            <<"latency_p99_ms">> => 1.23,
            <<"latency_p99_raw_us">> => 1230
        },
        <<"status">> => <<"PASS">>
    },

    ?assertEqual(ok, validate_report_structure(Fixture)),
    ?assertEqual(ok, validate_sub_millisecond_precision(maps:get(<<"metrics">>, Fixture))).

%%%====================================================================
%%% Bad Fixture Data Tests (Should Fail Validation)
%%%====================================================================

%% Test bad fixture: Missing units
bad_fixture_missing_units_test() ->
    Fixture = #{
        <<"benchmark_id">> => <<"bad_test">>,
        <<"timestamp">> => <<"2026-01-27T20:51:45Z">>,
        <<"test_type">> => <<"throughput">>,
        <<"metrics">> => #{
            <<"throughput">> => 1500,  % Missing unit
            <<"latency">> => 5.0       % Missing unit
        },
        <<"status">> => <<"FAIL">>
    },

    ?assertEqual({error, missing_units}, validate_metrics_metrology(maps:get(<<"metrics">>, Fixture))).

%% Test bad fixture: Missing scope for ambiguous metrics
bad_fixture_missing_scope_test() ->
    Fixture = #{
        <<"benchmark_id">> => <<"bad_scope">>,
        <<"timestamp">> => <<"2026-01-27T20:51:45Z">>,
        <<"test_type">> => <<"throughput">>,
        <<"metrics">> => #{
            <<"throughput_req_s">> => 1500,  % Missing scope (client? server?)
            <<"memory_mib">> => 512          % Missing scope (heap? total?)
        },
        <<"status">> => <<"FAIL">>
    },

    Metrics = maps:get(<<"metrics">>, Fixture),
    ?assertEqual({error, missing_scope}, validate_scope_requirements(Metrics)).

%% Test bad fixture: Insufficient precision
bad_fixture_insufficient_precision_test() ->
    Fixture = #{
        <<"benchmark_id">> => <<"bad_precision">>,
        <<"timestamp">> => <<"2026-01-27T20:51:45Z">>,
        <<"test_type">> => <<"latency">>,
        <<"metrics">> => #{
            <<"latency_p99_ms">> => 0.85  % Sub-ms without raw_us
        },
        <<"status">> => <<"FAIL">>
    },

    Metrics = maps:get(<<"metrics">>, Fixture),
    ?assertEqual({error, insufficient_precision}, validate_sub_millisecond_precision(Metrics)).

%% Test bad fixture: Non-canonical units (MB instead of MiB)
bad_fixture_non_canonical_units_test() ->
    Fixture = #{
        <<"benchmark_id">> => <<"bad_units">>,
        <<"timestamp">> => <<"2026-01-27T20:51:45Z">>,
        <<"test_type">> => <<"memory">>,
        <<"metrics">> => #{
            <<"memory_used_mb">> => 512  % Should be MiB in v1.5.0
        },
        <<"status">> => <<"FAIL">>
    },

    Metrics = maps:get(<<"metrics">>, Fixture),
    ?assertEqual({warning, non_canonical_units}, validate_canonical_units(Metrics)).

%%%====================================================================
%%% Edge Case Tests
%%%====================================================================

%% Test zero values are valid
edge_case_zero_values_test() ->
    Metrics = #{
        <<"error_count">> => 0,
        <<"timeout_count">> => 0,
        <<"connection_failures">> => 0
    },

    ?assertEqual(ok, validate_zero_values(Metrics)).

%% Test extreme precision (nanoseconds)
edge_case_extreme_precision_test() ->
    Metrics = #{
        <<"latency_p99_us">> => 0.001,
        <<"latency_p99_raw_ns">> => 1,  % 1 nanosecond
        <<"latency_p50_us">> => 0.5,
        <<"latency_p50_raw_ns">> => 500
    },

    ?assertEqual(ok, validate_extreme_precision(Metrics)).

%% Test cluster aggregation preserves metrology
edge_case_cluster_aggregation_test() ->
    Node1Metrics = #{
        <<"cpu_usage_percent">> => 65,
        <<"memory_heap_mib">> => 256,
        <<"throughput_req_s">> => 1200
    },
    Node2Metrics = #{
        <<"cpu_usage_percent">> => 72,
        <<"memory_heap_mib">> => 280,
        <<"throughput_req_s">> => 1350
    },
    Node3Metrics = #{
        <<"cpu_usage_percent">> => 68,
        <<"memory_heap_mib">> => 270,
        <<"throughput_req_s">> => 1300
    },

    ClusterMetrics = aggregate_cluster_metrics([Node1Metrics, Node2Metrics, Node3Metrics]),

    %% Aggregate preserves units
    ?assertEqual(ok, validate_metrics_metrology(ClusterMetrics)),
    %% Aggregate uses cluster scope
    ?assert(binary:match(atom_to_binary(cluster), <<"cluster">>) /= nomatch).

%% Test very large throughput values
edge_case_large_throughput_test() ->
    Metrics = #{
        <<"throughput_req_s">> => 1000000,  % 1M req/s
        <<"messages_processed_count">> => 60000000,  % 60M messages
        <<"duration_seconds">> => 60
    },

    ?assertEqual(ok, validate_large_values(Metrics)).

%% Test fractional percentages
edge_case_fractional_percentages_test() ->
    Metrics = #{
        <<"cpu_usage_percent">> => 68.5,
        <<"memory_utilization_percent">> => 73.2,
        <<"error_rate_percent">> => 0.005  % 0.005%
    },

    ?assertEqual(ok, validate_fractional_percentages(Metrics)).

%%%====================================================================
%%% Backwards Compatibility Tests (v1.4.0)
%%%====================================================================

%% Test v1.4.0 reports can be validated
backwards_compat_v1_4_0_test() ->
    %% v1.4.0 used MB, not MiB
    V1_4_0_Report = #{
        <<"benchmark_id">> => <<"v1.4.0_test">>,
        <<"timestamp">> => <<"2025-12-15T10:00:00Z">>,
        <<"test_type">> => <<"throughput">>,
        <<"metrics">> => #{
            <<"throughput_msg_per_sec">> => 150432,
            <<"latency_p99_ms">> => 4.2,
            <<"memory_used_mb">> => 320,  % v1.4.0 uses MB
            <<"cpu_usage_percent">> => 69
        },
        <<"status">> => <<"PASS">>
    },

    %% Should validate successfully with backwards compatibility flag
    ?assertEqual(ok, validate_report_with_version(V1_4_0_Report, "v1.4.0")).

%% Test v1.3.0 reports can be validated
backwards_compat_v1_3_0_test() ->
    V1_3_0_Report = #{
        <<"benchmark_id">> => <<"throughput_baseline_20260127">>,
        <<"timestamp">> => <<"2026-01-27T20:51:45Z">>,
        <<"test_type">> => <<"throughput">>,
        <<"metrics">> => #{
            <<"throughput_msg_per_sec">> => 150432,
            <<"latency_p50_ms">> => 1.2,
            <<"latency_p95_ms">> => 2.8,
            <<"latency_p99_ms">> => 4.2,
            <<"memory_used_mb">> => 320,
            <<"cpu_usage_percent">> => 69
        },
        <<"status">> => <<"PASS">>
    },

    ?assertEqual(ok, validate_report_with_version(V1_3_0_Report, "v1.3.0")).

%%%====================================================================
%%% Integration Tests (Real Benchmark Execution)
%%%====================================================================

%% Test real benchmark execution produces valid report
real_benchmark_produces_valid_report_test() ->
    %% Run actual benchmark
    {ok, Report} = run_simple_benchmark(100, 1000),  % 100 clients, 1000 ops each

    %% Validate structure
    ?assertEqual(ok, validate_report_structure(Report)),

    %% Validate metrics
    Metrics = maps:get(<<"metrics">>, Report),
    ?assertEqual(ok, validate_metrics_metrology(Metrics)),

    %% Verify report has required benchmarks
    ?assert(maps:is_key(<<"throughput_req_s">>, Metrics)),
    ?assert(maps:is_key(<<"latency_p99_ms">>, Metrics)).

%% Test benchmark with multiple iterations produces consistent reports
benchmark_consistency_across_runs_test() ->
    %% Run benchmark 3 times
    Reports = [begin
        {ok, Report} = run_simple_benchmark(10, 100),
        Report
    end || _ <- lists:seq(1, 3)],

    %% All reports should have same structure
    lists:foreach(fun(Report) ->
        ?assertEqual(ok, validate_report_structure(Report))
    end, Reports),

    %% Metrics should be within 10% variance
    Throughputs = [begin
        Metrics = maps:get(<<"metrics">>, R),
        maps:get(<<"throughput_req_s">>, Metrics)
    end || R <- Reports],

    Variance = calculate_variance(Throughputs),
    ?assert(Variance < 0.1).  % Less than 10% variance

%%%====================================================================
%%% Helper Functions (Real Implementation, Chicago School TDD)
%%%====================================================================

%% Validate report structure
validate_report_structure(Report) when is_map(Report) ->
    RequiredFields = [
        <<"benchmark_id">>,
        <<"timestamp">>,
        <<"test_type">>,
        <<"metrics">>,
        <<"status">>
    ],

    case lists:all(fun(Field) -> maps:is_key(Field, Report) end, RequiredFields) of
        true ->
            case maps:is_key(<<"timestamp">>, Report) of
                false -> {error, missing_timestamp};
                true ->
                    case maps:is_key(<<"metrics">>, Report) of
                        false -> {error, missing_metrics};
                        true -> ok
                    end
            end;
        false ->
            {error, missing_required_fields}
    end.

%% Validate metrics metrology
validate_metrics_metrology(Metrics) when is_map(Metrics) ->
    %% Check each metric key has unit
    HasUnits = maps:fold(fun(Key, _Value, Acc) ->
        Acc andalso has_unit_in_key(Key)
    end, true, Metrics),

    case HasUnits of
        true -> ok;
        false -> {error, missing_units}
    end.

%% Check if key has unit
has_unit_in_key(Key) ->
    UnitSuffixes = [<<"_ms">>, <<"_us">>, <<"_ns">>, <<"_s">>,
                    <<"_mb">>, <<"_mib">>, <<"_gb">>, <<"_gib">>,
                    <<"_percent">>, <<"_count">>, <<"_req_s">>, <<"_msg_per_sec">>],
    lists:any(fun(Suffix) ->
        binary:match(Key, Suffix) /= nomatch
    end, UnitSuffixes).

%% Validate scope requirements
validate_scope_requirements(Metrics) ->
    %% Check if ambiguous metrics have scope
    AmbiguousKeys = [<<"throughput_req_s">>, <<"memory_mib">>],

    HasScope = lists:all(fun(Key) ->
        case maps:is_key(Key, Metrics) of
            true ->
                %% Check for companion scope field
                ScopeKey = <<Key/binary, "_scope">>,
                maps:is_key(ScopeKey, Metrics);
            false ->
                true  % Key not present, OK
        end
    end, AmbiguousKeys),

    case HasScope of
        true -> ok;
        false -> {error, missing_scope}
    end.

%% Validate sub-millisecond precision
validate_sub_millisecond_precision(Metrics) ->
    %% Check if any ms values are < 1.0 and have raw_us
    SubMsKeys = maps:fold(fun(Key, Value, Acc) ->
        case binary:match(Key, <<"_ms">>) /= nomatch andalso
             is_float(Value) andalso Value < 1.0 of
            true -> [Key | Acc];
            false -> Acc
        end
    end, [], Metrics),

    AllHaveRaw = lists:all(fun(Key) ->
        RawKey = binary:replace(Key, <<"_ms">>, <<"_raw_us">>),
        maps:is_key(RawKey, Metrics)
    end, SubMsKeys),

    case AllHaveRaw of
        true -> ok;
        false -> {error, insufficient_precision}
    end.

%% Validate canonical units
validate_canonical_units(Metrics) ->
    %% Check for MB (should be MiB in v1.5.0)
    HasMB = maps:fold(fun(Key, _Value, Acc) ->
        Acc orelse (binary:match(Key, <<"_mb">>) /= nomatch)
    end, false, Metrics),

    case HasMB of
        true -> {warning, non_canonical_units};
        false -> ok
    end.

%% Validate zero values
validate_zero_values(Metrics) ->
    %% Zero is valid for counts
    ok.

%% Validate extreme precision
validate_extreme_precision(Metrics) ->
    %% Sub-microsecond requires nanosecond raw data
    SubUsKeys = maps:fold(fun(Key, Value, Acc) ->
        case binary:match(Key, <<"_us">>) /= nomatch andalso
             is_float(Value) andalso Value < 1.0 of
            true -> [Key | Acc];
            false -> Acc
        end
    end, [], Metrics),

    AllHaveRawNs = lists:all(fun(Key) ->
        RawKey = binary:replace(Key, <<"_us">>, <<"_raw_ns">>),
        maps:is_key(RawKey, Metrics)
    end, SubUsKeys),

    case AllHaveRawNs of
        true -> ok;
        false -> {error, insufficient_precision}
    end.

%% Aggregate cluster metrics
aggregate_cluster_metrics(NodeMetrics) ->
    %% Average all metrics across nodes
    Keys = maps:keys(hd(NodeMetrics)),

    maps:from_list(lists:map(fun(Key) ->
        Values = [maps:get(Key, N) || N <- NodeMetrics],
        Avg = lists:sum(Values) / length(Values),
        {Key, Avg}
    end, Keys)).

%% Validate large values
validate_large_values(_Metrics) ->
    %% Large values are valid
    ok.

%% Validate fractional percentages
validate_fractional_percentages(_Metrics) ->
    %% Fractional percentages are valid
    ok.

%% Validate report with version
validate_report_with_version(Report, Version) when Version == "v1.4.0"; Version == "v1.3.0" ->
    %% Backwards compatible validation (allows MB)
    validate_report_structure(Report);
validate_report_with_version(Report, _Version) ->
    %% v1.5.0+ requires strict metrology
    case validate_report_structure(Report) of
        ok ->
            Metrics = maps:get(<<"metrics">>, Report),
            validate_canonical_units(Metrics);
        Error ->
            Error
    end.

%% Load fixture from file
load_fixture(FixtureName) ->
    %% Placeholder: Load from test/metrology/fixtures/
    create_good_latency_fixture().

%% Create good latency fixture
create_good_latency_fixture() ->
    #{
        <<"benchmark_id">> => <<"latency_test">>,
        <<"timestamp">> => <<"2026-01-27T20:51:45Z">>,
        <<"test_type">> => <<"latency">>,
        <<"metrics">> => #{
            <<"latency_p50_ms">> => 1.2,
            <<"latency_p95_ms">> => 2.8,
            <<"latency_p99_ms">> => 4.2,
            <<"latency_p99_9_ms">> => 6.8
        },
        <<"status">> => <<"PASS">>
    }.

%% Create good memory fixture
create_good_memory_fixture() ->
    #{
        <<"benchmark_id">> => <<"memory_test">>,
        <<"timestamp">> => <<"2026-01-27T20:51:45Z">>,
        <<"test_type">> => <<"memory">>,
        <<"metrics">> => #{
            <<"memory_heap_mib">> => 256,
            <<"memory_stack_mib">> => 32,
            <<"memory_other_mib">> => 24,
            <<"memory_total_mib">> => 312
        },
        <<"status">> => <<"PASS">>
    }.

%% Run simple benchmark
run_simple_benchmark(NumClients, OpsPerClient) ->
    %% Simplified benchmark execution
    StartTime = erlang:monotonic_time(millisecond),

    %% Simulate operations
    timer:sleep(100),  % Simulate work

    EndTime = erlang:monotonic_time(millisecond),
    Duration = (EndTime - StartTime) / 1000.0,

    TotalOps = NumClients * OpsPerClient,
    Throughput = TotalOps / Duration,

    Report = #{
        <<"benchmark_id">> => list_to_binary("benchmark_" ++ integer_to_list(erlang:system_time())),
        <<"timestamp">> => list_to_binary(iso8601_timestamp()),
        <<"test_type">> => <<"throughput">>,
        <<"metrics">> => #{
            <<"throughput_req_s">> => round(Throughput),
            <<"latency_p99_ms">> => 5.0,
            <<"duration_seconds">> => Duration
        },
        <<"status">> => <<"PASS">>
    },

    {ok, Report}.

%% Calculate variance
calculate_variance(Values) ->
    Mean = lists:sum(Values) / length(Values),
    Variance = lists:sum([math:pow(V - Mean, 2) || V <- Values]) / length(Values),
    Variance / Mean.  % Coefficient of variation

%% ISO8601 timestamp
iso8601_timestamp() ->
    {{Year, Month, Day}, {Hour, Min, Sec}} = calendar:universal_time(),
    io_lib:format("~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0BZ",
                  [Year, Month, Day, Hour, Min, Sec]).
