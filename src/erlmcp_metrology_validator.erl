%%%-------------------------------------------------------------------
%%% @doc erlmcp_metrology_validator - Metrology Compliance Enforcement
%%%
%%% Validates that all performance metrics conform to strict metrology
%%% standards for reproducibility, traceability, and auditability.
%%%
%%% Core Requirements:
%%% - Unit strings must be present and from allowed set
%%% - Scope field required for memory/rate metrics (e.g., "/conn", "/msg")
%%% - Precision field required for time metrics (raw µs/ns values)
%%% - Workload_id + transport + duration required for performance claims
%%% - No "0.00 ms" without raw microsecond values
%%% - Canonical unit forms enforced (MiB not MB, µs not ms for precision)
%%%
%%% Used By:
%%% - Benchmark runners (pre-write validation)
%%% - Test suites (artifact validation)
%%% - CI/CD pipelines (quality gates)
%%% - Evidence certification (plan conformance)
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_metrology_validator).

-export([
    validate_report/1,
    validate_plan/1,
    validate_metric/1,
    canonical_unit/1,
    decompose_memory/1,
    validate_file/1,
    collect_violations/1,
    format_violation/1
]).

-include_lib("kernel/include/logger.hrl").

%%====================================================================
%% Types
%%====================================================================

-type metric() :: map().
-type report() :: map().
-type plan_spec() :: map().
-type violation() :: #{
    type := atom(),
    field := binary(),
    location := binary(),
    expected := term(),
    actual := term(),
    suggestion := binary()
}.
-type validation_result() :: ok | {error, [violation()]}.

-export_type([metric/0, report/0, plan_spec/0, violation/0, validation_result/0]).

%%====================================================================
%% Constants - Allowed Units and Validation Rules
%%====================================================================

%% Time units (prefer µs/ns for precision, ms/s for display)
-define(ALLOWED_TIME_UNITS, [
    <<"ns">>,   % nanoseconds (raw precision)
    <<"µs">>,   % microseconds (raw precision)
    <<"us">>,   % microseconds (ASCII fallback)
    <<"ms">>,   % milliseconds (display)
    <<"s">>     % seconds (display)
]).

%% Memory units (binary prefixes only)
-define(ALLOWED_MEMORY_UNITS, [
    <<"B">>,     % bytes
    <<"KiB">>,   % kibibytes (1024)
    <<"MiB">>,   % mebibytes (1024^2)
    <<"GiB">>    % gibibytes (1024^3)
]).

%% Rate units (operations per time)
-define(ALLOWED_RATE_UNITS, [
    <<"msg/s">>,   % messages per second
    <<"req/s">>,   % requests per second
    <<"ops/s">>,   % operations per second
    <<"MB/s">>,    % megabytes per second (throughput)
    <<"MiB/s">>    % mebibytes per second (throughput)
]).

%% Percentage units
-define(ALLOWED_PERCENTAGE_UNITS, [
    <<"%">>,       % percentage (0-100)
    <<"ratio">>    % ratio (0.0-1.0)
]).

%% Scope suffixes for composite metrics
-define(ALLOWED_SCOPES, [
    <<"/conn">>,   % per connection
    <<"/msg">>,    % per message
    <<"/req">>,    % per request
    <<"/worker">>, % per worker process
    <<"/core">>    % per CPU core
]).

%% Required fields for performance claims
-define(REQUIRED_PERF_FIELDS, [
    <<"workload_id">>,
    <<"transport">>,
    <<"duration_seconds">>
]).

%% Canonical unit mappings (non-canonical -> canonical)
-define(UNIT_CANONICALIZATION, #{
    <<"MB">> => <<"MiB">>,           % Use binary prefixes
    <<"KB">> => <<"KiB">>,
    <<"GB">> => <<"GiB">>,
    <<"ms">> => <<"µs">>,            % Prefer µs for precision
    <<"milliseconds">> => <<"µs">>,
    <<"microseconds">> => <<"µs">>,
    <<"nanoseconds">> => <<"ns">>,
    <<"req/sec">> => <<"req/s">>,
    <<"requests/s">> => <<"req/s">>,
    <<"ops/sec">> => <<"ops/s">>,
    <<"msg/sec">> => <<"msg/s">>
}).

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Validate a complete benchmark/chaos/conformance report
%% Checks all metrics, metadata, and required fields
%% @end
%%--------------------------------------------------------------------
-spec validate_report(report()) -> validation_result().
validate_report(Report) when is_map(Report) ->
    Violations = collect_report_violations(Report, []),
    case Violations of
        [] -> ok;
        _ -> {error, Violations}
    end;
validate_report(_) ->
    {error, [#{
        type => invalid_input,
        field => <<"report">>,
        location => <<"root">>,
        expected => <<"map">>,
        actual => <<"non-map">>,
        suggestion => <<"Report must be a map/object">>
    }]}.

%%--------------------------------------------------------------------
%% @doc Validate a plan specification JSON
%% Ensures envelope bounds have proper units and metadata
%% @end
%%--------------------------------------------------------------------
-spec validate_plan(plan_spec()) -> validation_result().
validate_plan(PlanSpec) when is_map(PlanSpec) ->
    Violations = collect_plan_violations(PlanSpec, []),
    case Violations of
        [] -> ok;
        _ -> {error, Violations}
    end;
validate_plan(_) ->
    {error, [#{
        type => invalid_input,
        field => <<"plan_spec">>,
        location => <<"root">>,
        expected => <<"map">>,
        actual => <<"non-map">>,
        suggestion => <<"Plan specification must be a map/object">>
    }]}.

%%--------------------------------------------------------------------
%% @doc Validate an individual metric with unit/scope/precision
%% @end
%%--------------------------------------------------------------------
-spec validate_metric(metric()) -> validation_result().
validate_metric(Metric) when is_map(Metric) ->
    Violations = collect_metric_violations(Metric, <<"metric">>, []),
    case Violations of
        [] -> ok;
        _ -> {error, Violations}
    end;
validate_metric(_) ->
    {error, [#{
        type => invalid_input,
        field => <<"metric">>,
        location => <<"root">>,
        expected => <<"map">>,
        actual => <<"non-map">>,
        suggestion => <<"Metric must be a map/object">>
    }]}.

%%--------------------------------------------------------------------
%% @doc Convert non-canonical units to canonical form
%% Returns {ok, Canonical} or {error, unknown_unit}
%% @end
%%--------------------------------------------------------------------
-spec canonical_unit(binary()) -> {ok, binary()} | {error, unknown_unit}.
canonical_unit(Unit) when is_binary(Unit) ->
    case maps:get(Unit, ?UNIT_CANONICALIZATION, undefined) of
        undefined ->
            %% Check if already canonical
            AllUnits = ?ALLOWED_TIME_UNITS ++ ?ALLOWED_MEMORY_UNITS ++
                       ?ALLOWED_RATE_UNITS ++ ?ALLOWED_PERCENTAGE_UNITS,
            case lists:member(Unit, AllUnits) of
                true -> {ok, Unit};
                false -> {error, unknown_unit}
            end;
        Canonical ->
            {ok, Canonical}
    end;
canonical_unit(_) ->
    {error, unknown_unit}.

%%--------------------------------------------------------------------
%% @doc Decompose composite memory unit into base + scope
%% Example: "MiB/conn" -> {<<"MiB">>, <<"/conn">>}
%% @end
%%--------------------------------------------------------------------
-spec decompose_memory(binary()) -> {ok, {binary(), binary()}} | {error, invalid_format}.
decompose_memory(Unit) when is_binary(Unit) ->
    case binary:split(Unit, <<"/">>) of
        [Base, Scope] ->
            ScopeWithSlash = <<"/", Scope/binary>>,
            case {is_valid_memory_unit(Base), is_valid_scope(ScopeWithSlash)} of
                {true, true} -> {ok, {Base, ScopeWithSlash}};
                {false, _} -> {error, {invalid_base_unit, Base}};
                {_, false} -> {error, {invalid_scope, ScopeWithSlash}}
            end;
        [_] ->
            %% No scope, just validate base unit
            case is_valid_memory_unit(Unit) of
                true -> {ok, {Unit, <<>>}};
                false -> {error, {invalid_base_unit, Unit}}
            end
    end;
decompose_memory(_) ->
    {error, invalid_format}.

%%--------------------------------------------------------------------
%% @doc Validate a report file at given path
%% Reads JSON and validates contents
%% @end
%%--------------------------------------------------------------------
-spec validate_file(file:filename()) -> validation_result().
validate_file(FilePath) ->
    case file:read_file(FilePath) of
        {ok, Binary} ->
            try jsx:decode(Binary, [return_maps]) of
                Report when is_map(Report) ->
                    validate_report(Report);
                _ ->
                    {error, [#{
                        type => invalid_json,
                        field => <<"file">>,
                        location => list_to_binary(FilePath),
                        expected => <<"valid JSON object">>,
                        actual => <<"invalid structure">>,
                        suggestion => <<"File must contain valid JSON object">>
                    }]}
            catch
                error:Reason ->
                    {error, [#{
                        type => json_parse_error,
                        field => <<"file">>,
                        location => list_to_binary(FilePath),
                        expected => <<"valid JSON">>,
                        actual => list_to_binary(io_lib:format("~p", [Reason])),
                        suggestion => <<"Check JSON syntax">>
                    }]}
            end;
        {error, Reason} ->
            {error, [#{
                type => file_error,
                field => <<"file">>,
                location => list_to_binary(FilePath),
                expected => <<"readable file">>,
                actual => list_to_binary(io_lib:format("~p", [Reason])),
                suggestion => <<"Ensure file exists and is readable">>
            }]}
    end.

%%--------------------------------------------------------------------
%% @doc Collect all violations from a report (don't stop on first)
%% @end
%%--------------------------------------------------------------------
-spec collect_violations(report() | plan_spec()) -> [violation()].
collect_violations(Data) when is_map(Data) ->
    collect_report_violations(Data, []);
collect_violations(_) ->
    [#{
        type => invalid_input,
        field => <<"data">>,
        location => <<"root">>,
        expected => <<"map">>,
        actual => <<"non-map">>,
        suggestion => <<"Input must be a map/object">>
    }].

%%--------------------------------------------------------------------
%% @doc Format a violation for human-readable output
%% Returns "file:field:line - expected X, got Y (suggestion: Z)"
%% @end
%%--------------------------------------------------------------------
-spec format_violation(violation()) -> binary().
format_violation(#{
    type := Type,
    field := Field,
    location := Location,
    expected := Expected,
    actual := Actual,
    suggestion := Suggestion
}) ->
    ExpectedStr = format_term(Expected),
    ActualStr = format_term(Actual),
    iolist_to_binary(io_lib:format(
        "~s:~s - ~s: expected ~s, got ~s (suggestion: ~s)",
        [Location, Field, Type, ExpectedStr, ActualStr, Suggestion]
    )).

%%====================================================================
%% Internal Functions - Validation Logic
%%====================================================================

%%--------------------------------------------------------------------
%% @private Collect all violations in a report
%%--------------------------------------------------------------------
-spec collect_report_violations(report(), [violation()]) -> [violation()].
collect_report_violations(Report, Acc) ->
    %% Check required performance fields
    Acc1 = check_required_perf_fields(Report, Acc),

    %% Check all metrics recursively
    Acc2 = check_nested_metrics(Report, <<"report">>, Acc1),

    %% Check plan conformance if present
    Acc3 = case maps:get(<<"plan">>, Report, undefined) of
        undefined -> Acc2;
        Plan -> check_plan_reference(Plan, Acc2)
    end,

    lists:reverse(Acc3).

%%--------------------------------------------------------------------
%% @private Collect violations in plan specification
%%--------------------------------------------------------------------
-spec collect_plan_violations(plan_spec(), [violation()]) -> [violation()].
collect_plan_violations(PlanSpec, Acc) ->
    %% Check envelope bounds
    Acc1 = case maps:get(<<"envelope">>, PlanSpec, undefined) of
        undefined ->
            [#{
                type => missing_field,
                field => <<"envelope">>,
                location => <<"plan_spec">>,
                expected => <<"envelope map">>,
                actual => <<"undefined">>,
                suggestion => <<"Plan must define performance envelope">>
            } | Acc];
        Envelope ->
            check_envelope_metrics(Envelope, Acc)
    end,

    lists:reverse(Acc1).

%%--------------------------------------------------------------------
%% @private Collect violations in a single metric
%%--------------------------------------------------------------------
-spec collect_metric_violations(metric(), binary(), [violation()]) -> [violation()].
collect_metric_violations(Metric, Location, Acc) ->
    %% Check unit field
    Acc1 = case maps:get(<<"unit">>, Metric, undefined) of
        undefined ->
            [#{
                type => missing_unit,
                field => <<"unit">>,
                location => Location,
                expected => <<"unit string">>,
                actual => <<"undefined">>,
                suggestion => <<"Add 'unit' field with canonical unit">>
            } | Acc];
        Unit ->
            check_unit_validity(Unit, Location, Acc)
    end,

    %% Check scope for composite metrics
    Acc2 = check_metric_scope(Metric, Location, Acc1),

    %% Check precision for time metrics
    Acc3 = check_time_precision(Metric, Location, Acc2),

    %% Check for "0.00 ms" anti-pattern
    Acc4 = check_zero_time_antipattern(Metric, Location, Acc3),

    Acc4.

%%--------------------------------------------------------------------
%% @private Check required performance claim fields
%%--------------------------------------------------------------------
-spec check_required_perf_fields(report(), [violation()]) -> [violation()].
check_required_perf_fields(Report, Acc) ->
    lists:foldl(
        fun(Field, AccIn) ->
            case maps:get(Field, Report, undefined) of
                undefined ->
                    [#{
                        type => missing_required_field,
                        field => Field,
                        location => <<"report">>,
                        expected => Field,
                        actual => <<"undefined">>,
                        suggestion => iolist_to_binary([
                            <<"Performance claims require '">>, Field, <<"' field">>
                        ])
                    } | AccIn];
                _ ->
                    AccIn
            end
        end,
        Acc,
        ?REQUIRED_PERF_FIELDS
    ).

%%--------------------------------------------------------------------
%% @private Recursively check metrics in nested structures
%%--------------------------------------------------------------------
-spec check_nested_metrics(map(), binary(), [violation()]) -> [violation()].
check_nested_metrics(Map, Location, Acc) when is_map(Map) ->
    maps:fold(
        fun(Key, Value, AccIn) ->
            NewLocation = <<Location/binary, ".", Key/binary>>,
            case is_metric_map(Value) of
                true ->
                    collect_metric_violations(Value, NewLocation, AccIn);
                false when is_map(Value) ->
                    check_nested_metrics(Value, NewLocation, AccIn);
                false when is_list(Value) ->
                    check_list_metrics(Value, NewLocation, AccIn);
                false ->
                    AccIn
            end
        end,
        Acc,
        Map
    );
check_nested_metrics(_, _, Acc) ->
    Acc.

%%--------------------------------------------------------------------
%% @private Check metrics in list structures
%%--------------------------------------------------------------------
-spec check_list_metrics(list(), binary(), [violation()]) -> [violation()].
check_list_metrics(List, Location, Acc) ->
    {_, FinalAcc} = lists:foldl(
        fun(Item, {Index, AccIn}) ->
            NewLocation = iolist_to_binary([Location, "[", integer_to_binary(Index), "]"]),
            NewAccIn = case is_metric_map(Item) of
                true -> collect_metric_violations(Item, NewLocation, AccIn);
                false when is_map(Item) -> check_nested_metrics(Item, NewLocation, AccIn);
                false -> AccIn
            end,
            {Index + 1, NewAccIn}
        end,
        {0, Acc},
        List
    ),
    FinalAcc.

%%--------------------------------------------------------------------
%% @private Check if map looks like a metric (has value + unit)
%%--------------------------------------------------------------------
-spec is_metric_map(term()) -> boolean().
is_metric_map(Map) when is_map(Map) ->
    maps:is_key(<<"value">>, Map) andalso maps:is_key(<<"unit">>, Map);
is_metric_map(_) ->
    false.

%%--------------------------------------------------------------------
%% @private Validate unit string
%%--------------------------------------------------------------------
-spec check_unit_validity(binary(), binary(), [violation()]) -> [violation()].
check_unit_validity(Unit, Location, Acc) ->
    AllUnits = ?ALLOWED_TIME_UNITS ++ ?ALLOWED_MEMORY_UNITS ++
               ?ALLOWED_RATE_UNITS ++ ?ALLOWED_PERCENTAGE_UNITS,

    %% Check if it's a valid unit directly first (handles rate units like "req/s")
    case lists:member(Unit, AllUnits) of
        true ->
            Acc;  % Valid unit
        false ->
            %% Check for composite units (e.g., "MiB/conn")
            {BaseUnit, _Scope} = case binary:split(Unit, <<"/">>) of
                [Base, Scope] -> {Base, <<"/", Scope/binary>>};
                [Base] -> {Base, <<>>}
            end,

            case lists:member(BaseUnit, AllUnits) of
                true ->
                    Acc;  % Valid base unit
                false ->
                    %% Check if canonicalization available
                    Suggestion = case canonical_unit(Unit) of
                        {ok, Canonical} ->
                            iolist_to_binary([<<"Use canonical form: '">>, Canonical, <<"'">>]);
                        {error, unknown_unit} ->
                            iolist_to_binary([<<"Unknown unit. Allowed: ">>,
                                              list_to_binary(lists:join(", ", AllUnits))])
                    end,
                    [#{
                        type => invalid_unit,
                        field => <<"unit">>,
                        location => Location,
                        expected => <<"canonical unit">>,
                        actual => Unit,
                        suggestion => Suggestion
                    } | Acc]
            end
    end.

%%--------------------------------------------------------------------
%% @private Check scope for memory/rate metrics
%%--------------------------------------------------------------------
-spec check_metric_scope(metric(), binary(), [violation()]) -> [violation()].
check_metric_scope(Metric, Location, Acc) ->
    Unit = maps:get(<<"unit">>, Metric, <<>>),

    %% Skip rate units (msg/s, req/s, ops/s) - they have "/" but aren't scoped
    case is_rate_unit(Unit) of
        true ->
            Acc;  % Rate units are fine
        false ->
            %% If unit has scope delimiter, scope should be present
            case binary:split(Unit, <<"/">>) of
                [_Base, Scope] ->
                    ScopeWithSlash = <<"/", Scope/binary>>,
                    case is_valid_scope(ScopeWithSlash) of
                        true ->
                            Acc;
                        false ->
                            [#{
                                type => invalid_scope,
                                field => <<"unit">>,
                                location => Location,
                                expected => <<"valid scope suffix">>,
                                actual => ScopeWithSlash,
                                suggestion => iolist_to_binary([
                                    <<"Allowed scopes: ">>,
                                    list_to_binary(lists:join(", ", ?ALLOWED_SCOPES))
                                ])
                            } | Acc]
                    end;
                [_] ->
                    %% Check if this metric type SHOULD have scope
                    case should_have_scope(maps:get(<<"name">>, Metric, <<>>)) of
                        true ->
                            [#{
                                type => missing_scope,
                                field => <<"unit">>,
                                location => Location,
                                expected => <<"unit with scope (e.g., MiB/conn)">>,
                                actual => Unit,
                                suggestion => <<"Add scope suffix to clarify measurement context">>
                            } | Acc];
                        false ->
                            Acc
                    end
            end
    end.

%%--------------------------------------------------------------------
%% @private Check time precision requirements
%%--------------------------------------------------------------------
-spec check_time_precision(metric(), binary(), [violation()]) -> [violation()].
check_time_precision(Metric, Location, Acc) ->
    Unit = maps:get(<<"unit">>, Metric, <<>>),
    Value = maps:get(<<"value">>, Metric, undefined),

    case is_time_unit(Unit) of
        true ->
            %% Time metrics should have raw precision field
            case maps:get(<<"precision_us">>, Metric, undefined) of
                undefined ->
                    [#{
                        type => missing_precision,
                        field => <<"precision_us">>,
                        location => Location,
                        expected => <<"raw microsecond value">>,
                        actual => <<"undefined">>,
                        suggestion => <<"Add 'precision_us' field with raw µs measurement">>
                    } | Acc];
                PrecisionUs when is_number(PrecisionUs), PrecisionUs >= 0 ->
                    %% Verify consistency
                    check_time_consistency(Unit, Value, PrecisionUs, Location, Acc);
                _ ->
                    [#{
                        type => invalid_precision,
                        field => <<"precision_us">>,
                        location => Location,
                        expected => <<"non-negative number">>,
                        actual => maps:get(<<"precision_us">>, Metric, undefined),
                        suggestion => <<"Precision must be >= 0">>
                    } | Acc]
            end;
        false ->
            Acc
    end.

%%--------------------------------------------------------------------
%% @private Check for "0.00 ms" anti-pattern
%%--------------------------------------------------------------------
-spec check_zero_time_antipattern(metric(), binary(), [violation()]) -> [violation()].
check_zero_time_antipattern(Metric, Location, Acc) ->
    Unit = maps:get(<<"unit">>, Metric, <<>>),
    Value = maps:get(<<"value">>, Metric, undefined),

    case {Unit, Value} of
        {<<"ms">>, V} when V == 0.0; V == +0.0 ->
            case maps:get(<<"precision_us">>, Metric, undefined) of
                undefined ->
                    [#{
                        type => zero_time_without_precision,
                        field => <<"value">>,
                        location => Location,
                        expected => <<"raw microsecond precision">>,
                        actual => <<"0.00 ms">>,
                        suggestion => <<"Provide 'precision_us' to show actual sub-millisecond value">>
                    } | Acc];
                _ ->
                    Acc
            end;
        _ ->
            Acc
    end.

%%--------------------------------------------------------------------
%% @private Check plan reference validity
%%--------------------------------------------------------------------
-spec check_plan_reference(binary(), [violation()]) -> [violation()].
check_plan_reference(Plan, Acc) ->
    ValidPlans = [<<"team">>, <<"enterprise">>, <<"gov">>],
    case lists:member(Plan, ValidPlans) of
        true -> Acc;
        false -> [#{
            type => invalid_plan,
            field => <<"plan">>,
            location => <<"report.plan">>,
            expected => <<"team | enterprise | gov">>,
            actual => Plan,
            suggestion => <<"Use one of: team, enterprise, gov">>
        } | Acc]
    end.

%%--------------------------------------------------------------------
%% @private Check envelope metrics in plan spec
%%--------------------------------------------------------------------
-spec check_envelope_metrics(map(), [violation()]) -> [violation()].
check_envelope_metrics(Envelope, Acc) ->
    %% Each envelope bound should have proper units
    maps:fold(
        fun(Key, Value, AccIn) ->
            Location = <<"plan_spec.envelope.", Key/binary>>,
            case is_metric_map(Value) of
                true -> collect_metric_violations(Value, Location, AccIn);
                false -> AccIn
            end
        end,
        Acc,
        Envelope
    ).

%%--------------------------------------------------------------------
%% @private Check time value consistency with precision
%%--------------------------------------------------------------------
-spec check_time_consistency(binary(), number(), number(), binary(), [violation()]) -> [violation()].
check_time_consistency(Unit, Value, PrecisionUs, Location, Acc) ->
    %% Convert display value to µs and compare with precision
    ValueInUs = case Unit of
        <<"ms">> -> Value * 1000;
        <<"s">> -> Value * 1000000;
        <<"µs">> -> Value;
        <<"us">> -> Value;
        <<"ns">> -> Value / 1000;
        _ -> Value
    end,

    %% Allow 1% tolerance for rounding
    Tolerance = max(0.01 * PrecisionUs, 0.01),
    case abs(ValueInUs - PrecisionUs) =< Tolerance of
        true ->
            Acc;
        false ->
            [#{
                type => inconsistent_precision,
                field => <<"value">>,
                location => Location,
                expected => list_to_binary(io_lib:format("~.2f µs (from precision_us)", [PrecisionUs])),
                actual => list_to_binary(io_lib:format("~.2f µs (from value)", [ValueInUs])),
                suggestion => <<"Ensure value and precision_us are consistent">>
            } | Acc]
    end.

%%====================================================================
%% Helper Functions
%%====================================================================

-spec is_valid_memory_unit(binary()) -> boolean().
is_valid_memory_unit(Unit) ->
    lists:member(Unit, ?ALLOWED_MEMORY_UNITS).

-spec is_valid_scope(binary()) -> boolean().
is_valid_scope(Scope) ->
    lists:member(Scope, ?ALLOWED_SCOPES).

-spec is_time_unit(binary()) -> boolean().
is_time_unit(Unit) ->
    lists:member(Unit, ?ALLOWED_TIME_UNITS).

-spec is_rate_unit(binary()) -> boolean().
is_rate_unit(Unit) ->
    lists:member(Unit, ?ALLOWED_RATE_UNITS).

-spec should_have_scope(binary()) -> boolean().
should_have_scope(Name) ->
    %% Only suggest scope for metrics that clearly need context
    %% "per_" prefix explicitly indicates scoped metric
    lists:any(
        fun(Pattern) -> binary:match(Name, Pattern) =/= nomatch end,
        [<<"memory_per_">>, <<"latency_per_">>, <<"per_">>]
    ).

-spec format_term(term()) -> binary().
format_term(Term) when is_binary(Term) -> Term;
format_term(Term) when is_atom(Term) -> atom_to_binary(Term);
format_term(Term) when is_list(Term) ->
    try
        list_to_binary(Term)
    catch
        _:_ -> list_to_binary(io_lib:format("~p", [Term]))
    end;
format_term(Term) ->
    list_to_binary(io_lib:format("~p", [Term])).
