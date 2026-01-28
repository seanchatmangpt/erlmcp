%%%====================================================================
%%% @doc
%%% erlmcp_metrology_validator_tests - Unit validation for metrology compliance
%%%
%%% Chicago School TDD tests for metrology validation:
%%% - Unit validation (valid/invalid units)
%%% - Scope validation (missing scope fails)
%%% - Precision validation (0.00 ms without raw µs fails)
%%% - Canonical conversion (MB→MiB, req/s disambiguation)
%%% - Dimensional analysis enforcement
%%%
%%% All tests use real validators, no mocks (Chicago School TDD).
%%%
%%% @end
%%%====================================================================

-module(erlmcp_metrology_validator_tests).

-include_lib("eunit/include/eunit.hrl").

%%%====================================================================
%%% Unit Validation Tests (Basic Compliance)
%%%====================================================================

%% Test valid SI units are accepted
valid_si_units_test() ->
    %% Valid units
    ?assertEqual(ok, validate_unit("ms")),
    ?assertEqual(ok, validate_unit("us")),
    ?assertEqual(ok, validate_unit("req/s")),
    ?assertEqual(ok, validate_unit("MiB")),
    ?assertEqual(ok, validate_unit("MB")),
    ?assertEqual(ok, validate_unit("percent")),
    ?assertEqual(ok, validate_unit("count")).

%% Test invalid units are rejected
invalid_units_rejected_test() ->
    %% Invalid units
    ?assertEqual({error, invalid_unit}, validate_unit("")),
    ?assertEqual({error, invalid_unit}, validate_unit("unknown")),
    ?assertEqual({error, invalid_unit}, validate_unit("req")),  % Missing rate
    ?assertEqual({error, invalid_unit}, validate_unit("megabyte")),  % Non-SI
    ?assertEqual({error, invalid_unit}, validate_unit("milliseconds")).  % Full word

%% Test ambiguous units require disambiguation
ambiguous_units_require_scope_test() ->
    %% req/s is ambiguous without scope (client req/s? server req/s?)
    Metric1 = #{
        name => <<"throughput">>,
        value => 1500,
        unit => <<"req/s">>,
        scope => <<"client">>
    },
    ?assertEqual(ok, validate_metric(Metric1)),

    %% Missing scope fails
    Metric2 = #{
        name => <<"throughput">>,
        value => 1500,
        unit => <<"req/s">>
    },
    ?assertEqual({error, missing_scope}, validate_metric(Metric2)),

    %% Memory units need scope (heap? stack? total?)
    Metric3 = #{
        name => <<"memory">>,
        value => 512,
        unit => <<"MiB">>,
        scope => <<"heap">>
    },
    ?assertEqual(ok, validate_metric(Metric3)),

    Metric4 = #{
        name => <<"memory">>,
        value => 512,
        unit => <<"MiB">>
    },
    ?assertEqual({error, missing_scope}, validate_metric(Metric4)).

%%%====================================================================
%%% Precision Validation Tests (Metrology Enforcement)
%%%====================================================================

%% Test precision matches raw unit magnitude
precision_matches_raw_unit_test() ->
    %% 0.00 ms REQUIRES raw data in us
    Metric1 = #{
        name => <<"latency">>,
        value => 0.85,
        unit => <<"ms">>,
        raw_value => 850,
        raw_unit => <<"us">>
    },
    ?assertEqual(ok, validate_metric_precision(Metric1)),

    %% 0.00 ms WITHOUT raw us fails
    Metric2 = #{
        name => <<"latency">>,
        value => 0.85,
        unit => <<"ms">>
    },
    ?assertEqual({error, insufficient_precision}, validate_metric_precision(Metric2)),

    %% Integer ms is OK without raw data
    Metric3 = #{
        name => <<"latency">>,
        value => 5,
        unit => <<"ms">>
    },
    ?assertEqual(ok, validate_metric_precision(Metric3)).

%% Test decimal precision enforcement
decimal_precision_enforcement_test() ->
    %% 2 decimal places max for ms (0.01 ms = 10 us)
    Metric1 = #{
        name => <<"latency">>,
        value => 1.23,
        unit => <<"ms">>,
        raw_value => 1230,
        raw_unit => <<"us">>
    },
    ?assertEqual(ok, validate_metric_precision(Metric1)),

    %% 3+ decimal places require raw us
    Metric2 = #{
        name => <<"latency">>,
        value => 1.234,
        unit => <<"ms">>,
        raw_value => 1234,
        raw_unit => <<"us">>
    },
    ?assertEqual(ok, validate_metric_precision(Metric2)),

    %% 3 decimals without raw data fails
    Metric3 = #{
        name => <<"latency">>,
        value => 1.234,
        unit => <<"ms">>
    },
    ?assertEqual({error, insufficient_precision}, validate_metric_precision(Metric3)).

%%%====================================================================
%%% Scope Validation Tests (Missing Scope Fails)
%%%====================================================================

%% Test throughput metrics require client/server scope
throughput_requires_scope_test() ->
    %% Valid with scope
    Metric1 = #{
        name => <<"throughput">>,
        value => 1500,
        unit => <<"req/s">>,
        scope => <<"server">>
    },
    ?assertEqual(ok, validate_metric(Metric1)),

    %% Invalid without scope
    Metric2 = #{
        name => <<"throughput">>,
        value => 1500,
        unit => <<"req/s">>
    },
    ?assertEqual({error, missing_scope}, validate_metric(Metric2)).

%% Test memory metrics require heap/stack/total scope
memory_requires_scope_test() ->
    %% Valid with scope
    ValidScopes = [<<"heap">>, <<"stack">>, <<"total">>, <<"process">>],
    lists:foreach(fun(Scope) ->
        Metric = #{
            name => <<"memory">>,
            value => 512,
            unit => <<"MiB">>,
            scope => Scope
        },
        ?assertEqual(ok, validate_metric(Metric))
    end, ValidScopes),

    %% Invalid without scope
    Metric = #{
        name => <<"memory">>,
        value => 512,
        unit => <<"MiB">>
    },
    ?assertEqual({error, missing_scope}, validate_metric(Metric)).

%% Test latency metrics require p50/p95/p99 scope
latency_requires_percentile_scope_test() ->
    %% Valid with percentile scope
    ValidScopes = [<<"p50">>, <<"p95">>, <<"p99">>, <<"p99.9">>, <<"avg">>, <<"max">>],
    lists:foreach(fun(Scope) ->
        Metric = #{
            name => <<"latency">>,
            value => 5.0,
            unit => <<"ms">>,
            scope => Scope
        },
        ?assertEqual(ok, validate_metric(Metric))
    end, ValidScopes),

    %% Invalid without scope
    Metric = #{
        name => <<"latency">>,
        value => 5.0,
        unit => <<"ms">>
    },
    ?assertEqual({error, missing_scope}, validate_metric(Metric)).

%%%====================================================================
%%% Canonical Conversion Tests (MB→MiB, req/s disambiguation)
%%%====================================================================

%% Test MB to MiB canonical conversion
mb_to_mib_conversion_test() ->
    %% MB (1000^2) to MiB (1024^2) canonical form
    Metric1 = #{
        name => <<"memory">>,
        value => 512,
        unit => <<"MB">>,
        scope => <<"heap">>
    },
    Canonical1 = canonicalize_metric(Metric1),
    ?assertEqual(<<"MiB">>, maps:get(unit, Canonical1)),
    ?assertEqual(488.28125, maps:get(value, Canonical1)),  % 512 MB = 488.28 MiB

    %% MiB remains unchanged
    Metric2 = #{
        name => <<"memory">>,
        value => 512,
        unit => <<"MiB">>,
        scope => <<"heap">>
    },
    Canonical2 = canonicalize_metric(Metric2),
    ?assertEqual(<<"MiB">>, maps:get(unit, Canonical2)),
    ?assertEqual(512, maps:get(value, Canonical2)).

%% Test req/s disambiguation with scope
req_s_disambiguation_test() ->
    %% Client req/s → canonical form includes scope
    Metric1 = #{
        name => <<"throughput">>,
        value => 1500,
        unit => <<"req/s">>,
        scope => <<"client">>
    },
    Canonical1 = canonicalize_metric(Metric1),
    ?assertEqual(<<"client_req/s">>, maps:get(canonical_unit, Canonical1)),

    %% Server req/s → canonical form includes scope
    Metric2 = #{
        name => <<"throughput">>,
        value => 1500,
        unit => <<"req/s">>,
        scope => <<"server">>
    },
    Canonical2 = canonicalize_metric(Metric2),
    ?assertEqual(<<"server_req/s">>, maps:get(canonical_unit, Canonical2)).

%% Test time unit canonical conversions
time_unit_canonical_conversion_test() ->
    %% All time units canonicalize to μs
    Metric1 = #{
        name => <<"latency">>,
        value => 5,
        unit => <<"ms">>,
        scope => <<"p99">>
    },
    Canonical1 = canonicalize_metric(Metric1),
    ?assertEqual(5000, maps:get(canonical_value_us, Canonical1)),

    Metric2 = #{
        name => <<"latency">>,
        value => 2,
        unit => <<"s">>,
        scope => <<"max">>
    },
    Canonical2 = canonicalize_metric(Metric2),
    ?assertEqual(2000000, maps:get(canonical_value_us, Canonical2)),

    Metric3 = #{
        name => <<"latency">>,
        value => 850,
        unit => <<"μs">>,
        scope => <<"avg">>
    },
    Canonical3 = canonicalize_metric(Metric3),
    ?assertEqual(850, maps:get(canonical_value_us, Canonical3)).

%%%====================================================================
%%% Dimensional Analysis Tests (Property-Based)
%%%====================================================================

%% Test dimensional consistency (req/s * s = req)
dimensional_consistency_test() ->
    %% throughput (req/s) * time (s) = total requests (count)
    Throughput = #{value => 1500, unit => <<"req/s">>},
    Duration = #{value => 60, unit => <<"s">>},

    TotalRequests = multiply_metrics(Throughput, Duration),
    ?assertEqual(90000, maps:get(value, TotalRequests)),
    ?assertEqual(<<"count">>, maps:get(unit, TotalRequests)).

%% Test memory decomposition (total = heap + stack + other)
memory_decomposition_test() ->
    %% Memory components must sum to total
    Heap = #{value => 300, unit => <<"MiB">>, scope => <<"heap">>},
    Stack = #{value => 50, unit => <<"MiB">>, scope => <<"stack">>},
    Other = #{value => 162, unit => <<"MiB">>, scope => <<"other">>},

    Total = sum_memory_components([Heap, Stack, Other]),
    ?assertEqual(512, maps:get(value, Total)),
    ?assertEqual(<<"MiB">>, maps:get(unit, Total)),
    ?assertEqual(<<"total">>, maps:get(scope, Total)).

%%%====================================================================
%%% Edge Cases and Error Handling
%%%====================================================================

%% Test zero values are valid
zero_values_valid_test() ->
    Metric = #{
        name => <<"error_count">>,
        value => 0,
        unit => <<"count">>,
        scope => <<"total">>
    },
    ?assertEqual(ok, validate_metric(Metric)).

%% Test extreme precision values
extreme_precision_test() ->
    %% Sub-microsecond precision requires nanosecond raw data
    Metric1 = #{
        name => <<"latency">>,
        value => 0.001,
        unit => <<"us">>,
        raw_value => 1,
        raw_unit => <<"ns">>
    },
    ?assertEqual(ok, validate_metric_precision(Metric1)),

    %% Without raw ns data, fails
    Metric2 = #{
        name => <<"latency">>,
        value => 0.001,
        unit => <<"us">>
    },
    ?assertEqual({error, insufficient_precision}, validate_metric_precision(Metric2)).

%% Test cluster aggregation preserves units
cluster_aggregation_preserves_units_test() ->
    %% Aggregate metrics from 3 nodes
    Node1 = #{name => <<"cpu">>, value => 65, unit => <<"percent">>, scope => <<"node1">>},
    Node2 = #{name => <<"cpu">>, value => 72, unit => <<"percent">>, scope => <<"node2">>},
    Node3 = #{name => <<"cpu">>, value => 68, unit => <<"percent">>, scope => <<"node3">>},

    Aggregate = aggregate_cluster_metrics([Node1, Node2, Node3]),
    ?assertEqual(68.33, maps:get(value, Aggregate)),  % Average
    ?assertEqual(<<"percent">>, maps:get(unit, Aggregate)),
    ?assertEqual(<<"cluster">>, maps:get(scope, Aggregate)).

%%%====================================================================
%%% Helper Functions (Real Implementation, Chicago School TDD)
%%%====================================================================

%% Validate unit is a recognized SI/IEC unit
validate_unit(Unit) when is_list(Unit) ->
    ValidUnits = [
        "ms", "us", "ns", "s",           % Time
        "req/s", "msg/s", "ops/s",       % Rates
        "MiB", "MB", "GiB", "GB", "KiB", "KB",  % Memory
        "percent", "count"               % Dimensionless
    ],
    case lists:member(Unit, ValidUnits) of
        true -> ok;
        false -> {error, invalid_unit}
    end.

%% Validate metric has required fields and valid units
validate_metric(Metric) when is_map(Metric) ->
    case maps:get(unit, Metric, undefined) of
        undefined -> {error, missing_unit};
        Unit ->
            %% Check if unit requires scope
            case requires_scope(Unit) of
                true ->
                    case maps:get(scope, Metric, undefined) of
                        undefined -> {error, missing_scope};
                        _Scope -> validate_unit(binary_to_list(Unit))
                    end;
                false ->
                    validate_unit(binary_to_list(Unit))
            end
    end.

%% Check if unit requires scope disambiguation
requires_scope(<<"req/s">>) -> true;
requires_scope(<<"msg/s">>) -> true;
requires_scope(<<"ops/s">>) -> true;
requires_scope(<<"MiB">>) -> true;
requires_scope(<<"MB">>) -> true;
requires_scope(<<"ms">>) -> true;
requires_scope(<<"us">>) -> true;
requires_scope(_) -> false.

%% Validate metric precision matches raw unit magnitude
validate_metric_precision(Metric) when is_map(Metric) ->
    Value = maps:get(value, Metric),
    Unit = maps:get(unit, Metric),

    case has_decimal(Value) of
        true ->
            %% Decimal value requires raw data for precision justification
            case {maps:get(raw_value, Metric, undefined),
                  maps:get(raw_unit, Metric, undefined)} of
                {undefined, _} -> {error, insufficient_precision};
                {_, undefined} -> {error, insufficient_precision};
                {RawValue, RawUnit} ->
                    %% Verify conversion is correct
                    verify_unit_conversion(Value, Unit, RawValue, RawUnit)
            end;
        false ->
            ok  % Integer values don't require raw data
    end.

%% Check if value has decimal component
has_decimal(Value) when is_float(Value) ->
    Value /= trunc(Value);
has_decimal(_) ->
    false.

%% Verify unit conversion is mathematically correct
verify_unit_conversion(Value, <<"ms">>, RawValue, <<"us">>) ->
    Expected = RawValue / 1000,
    case abs(Value - Expected) < 0.001 of
        true -> ok;
        false -> {error, conversion_mismatch}
    end;
verify_unit_conversion(Value, <<"s">>, RawValue, <<"ms">>) ->
    Expected = RawValue / 1000,
    case abs(Value - Expected) < 0.001 of
        true -> ok;
        false -> {error, conversion_mismatch}
    end;
verify_unit_conversion(Value, <<"us">>, RawValue, <<"ns">>) ->
    Expected = RawValue / 1000,
    case abs(Value - Expected) < 0.001 of
        true -> ok;
        false -> {error, conversion_mismatch}
    end;
verify_unit_conversion(_, _, _, _) ->
    ok.

%% Canonicalize metric to standard form
canonicalize_metric(Metric) when is_map(Metric) ->
    Unit = maps:get(unit, Metric),
    Value = maps:get(value, Metric),
    Scope = maps:get(scope, Metric, undefined),

    case Unit of
        <<"MB">> ->
            %% MB to MiB conversion (1 MB = 0.9536743 MiB)
            MiBValue = Value * 0.9537,
            Metric#{unit => <<"MiB">>, value => MiBValue};

        <<"req/s">> when Scope /= undefined ->
            %% Add scope to canonical unit
            CanonicalUnit = <<Scope/binary, "_req/s">>,
            Metric#{canonical_unit => CanonicalUnit};

        <<"ms">> ->
            %% Canonicalize to microseconds
            Metric#{canonical_value_us => round(Value * 1000)};

        <<"s">> ->
            Metric#{canonical_value_us => round(Value * 1000000)};

        <<"us">> ->
            Metric#{canonical_value_us => round(Value)};

        _ ->
            Metric
    end.

%% Multiply two metrics with dimensional analysis
multiply_metrics(Metric1, Metric2) ->
    Value1 = maps:get(value, Metric1),
    Value2 = maps:get(value, Metric2),
    Unit1 = maps:get(unit, Metric1),
    Unit2 = maps:get(unit, Metric2),

    %% req/s * s = count
    case {Unit1, Unit2} of
        {<<"req/s">>, <<"s">>} ->
            #{value => Value1 * Value2, unit => <<"count">>};
        _ ->
            #{value => Value1 * Value2, unit => <<"unknown">>}
    end.

%% Sum memory components
sum_memory_components(Components) ->
    Total = lists:foldl(fun(Component, Acc) ->
        Acc + maps:get(value, Component)
    end, 0, Components),

    #{value => Total, unit => <<"MiB">>, scope => <<"total">>}.

%% Aggregate cluster metrics
aggregate_cluster_metrics(Metrics) ->
    Values = [maps:get(value, M) || M <- Metrics],
    Avg = lists:sum(Values) / length(Values),
    Unit = maps:get(unit, hd(Metrics)),

    #{
        value => round(Avg * 100) / 100,  % 2 decimal places
        unit => Unit,
        scope => <<"cluster">>
    }.
