%%%-----------------------------------------------------------------------------
%%% @doc TCPS Rebar3 Quality Gates Provider
%%%
%%% Enforces TCPS quality gates at each build stage by delegating to
%%% tcps_quality_gates module. Acts as thin rebar3 integration layer.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(tcps_rebar3_quality).
-behaviour(provider).

-export([init/1, do/1, format_error/1]).

%% Test exports - test helper functions
-ifdef(TEST).
-export([
    check_compilation_gate/2,
    check_test_gate/2,
    generate_quality_receipt/2,
    get_test_metrics/1
]).
-endif.

-define(PROVIDER, check_quality_gates).
-define(NAMESPACE, tcps).
-define(DEPS, []).

%%%=============================================================================
%%% API
%%%=============================================================================

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
        {name, ?PROVIDER},
        {module, ?MODULE},
        {namespace, ?NAMESPACE},
        {bare, true},
        {deps, ?DEPS},
        {example, "rebar3 tcps check_quality_gates"},
        {short_desc, "Check TCPS quality gates"},
        {desc, "Enforces zero-defect quality standards for TCPS production pipeline"},
        {opts, [
            {stage, $s, "stage", {atom, all},
             "Build stage to check (compile, test, release, all)"},
            {sku_id, $k, "sku-id", string,
             "SKU identifier being validated"},
            {strict, $x, "strict", {boolean, true},
             "Fail build on any gate violation (default: true)"}
        ]}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    rebar_api:info("~n=== TCPS Quality Gates Verification ===~n", []),

    {Args, _} = rebar_state:command_parsed_args(State),
    Stage = proplists:get_value(stage, Args, all),
    SkuIdOpt = proplists:get_value(sku_id, Args, undefined),
    SkuId = case SkuIdOpt of
        undefined -> generate_sku_id();
        Id when is_list(Id) -> list_to_binary(Id);
        Id when is_binary(Id) -> Id;
        Id -> iolist_to_binary(io_lib:format("~p", [Id]))
    end,
    Strict = proplists:get_value(strict, Args, true),

    %% Start quality gates gen_server if not running
    case whereis(tcps_quality_gates) of
        undefined ->
            {ok, _Pid} = tcps_quality_gates:start_link();
        _ ->
            ok
    end,

    %% Run quality gates
    Result = case Stage of
        all ->
            tcps_quality_gates:check_all_gates(SkuId);
        compile ->
            case tcps_quality_gates:check_gate(compilation, SkuId) of
                {pass, Receipt} -> {ok, [Receipt]};
                {fail, Violations} -> {failed_at, compilation, Violations}
            end;
        test ->
            case tcps_quality_gates:check_gate(test_execution, SkuId) of
                {pass, Receipt} -> {ok, [Receipt]};
                {fail, Violations} -> {failed_at, test_execution, Violations}
            end;
        release ->
            case tcps_quality_gates:check_gate(release_verification, SkuId) of
                {pass, Receipt} -> {ok, [Receipt]};
                {fail, Violations} -> {failed_at, release_verification, Violations}
            end;
        _ ->
            {failed_at, unknown, [#{error => <<"Unknown stage">>, stage => Stage}]}
    end,

    evaluate_result(Result, Strict, SkuId, State).

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("TCPS Quality Gates Error: ~p", [Reason]).

%%%=============================================================================
%%% Internal Functions
%%%=============================================================================

generate_sku_id() ->
    Timestamp = erlang:system_time(microsecond),
    iolist_to_binary(io_lib:format("sku_~p", [Timestamp])).

evaluate_result({ok, Receipts}, _Strict, _SkuId, State) ->
    rebar_api:info("~nQuality Gates Summary:~n", []),
    rebar_api:info("  Total Gates Passed: ~p~n", [length(Receipts)]),
    rebar_api:info("~n✓ All quality gates passed!~n~n", []),
    {ok, State};

evaluate_result({failed_at, Gate, Violations}, Strict, SkuId, State) ->
    rebar_api:info("~nQuality Gates Summary:~n", []),
    rebar_api:info("  Failed at gate: ~p~n", [Gate]),
    rebar_api:info("  Violations: ~p~n", [length(Violations)]),

    lists:foreach(fun(Violation) ->
        rebar_api:warn("    - ~p~n", [Violation])
    end, Violations),

    case Strict of
        false ->
            rebar_api:warn("~nQuality gates failed (non-strict mode)~n~n", []),
            {ok, State};
        true ->
            rebar_api:error("~nQuality gates failed - build blocked~n~n", []),
            rebar_api:error("  Failed SKU: ~s~n", [SkuId]),
            rebar_api:error("  Failed Gate: ~p~n", [Gate]),
            {error, io_lib:format("Quality gate ~p failed", [Gate])}
    end.

%%%=============================================================================
%%% Test Helper Functions (only available when compiled with TEST flag)
%%%=============================================================================

-ifdef(TEST).

%% @doc Check compilation gate - mock implementation for testing
check_compilation_gate(_State, _Config) ->
    #{
        gate => compilation,
        passed => true,
        metrics => #{
            error_count => 0,
            warning_count => 0
        },
        thresholds => #{
            max_errors => 0,
            max_critical_warnings => 0
        },
        violations => []
    }.

%% @doc Check test gate - mock implementation for testing
check_test_gate(_State, Config) ->
    MinCoverage = maps:get(min_coverage, Config, 80.0),
    MinPassRate = maps:get(min_pass_rate, Config, 80.0),

    %% Get metrics (would normally come from test results)
    Metrics = get_test_metrics(Config),
    Coverage = maps:get(coverage, Metrics, 85.0),
    TotalTests = maps:get(total_tests, Metrics, 10),
    PassedTests = maps:get(passed_tests, Metrics, 10),
    PassRate = (PassedTests / TotalTests) * 100.0,

    %% Check violations
    Violations = lists:flatten([
        case Coverage < MinCoverage of
            true -> [#{metric => coverage, actual => Coverage, expected => MinCoverage}];
            false -> []
        end,
        case PassRate < MinPassRate of
            true -> [#{metric => pass_rate, actual => PassRate, expected => MinPassRate}];
            false -> []
        end
    ]),

    #{
        gate => test,
        passed => length(Violations) =:= 0,
        metrics => #{
            coverage => Coverage,
            pass_rate => PassRate,
            total_tests => TotalTests,
            passed_tests => PassedTests
        },
        thresholds => #{
            min_coverage => MinCoverage,
            min_pass_rate => MinPassRate
        },
        violations => Violations
    }.

%% @doc Generate quality receipt - mock implementation for testing
generate_quality_receipt(Results, Config) ->
    %% Determine overall status
    Status = case lists:all(fun(R) -> maps:get(passed, R, false) end, Results) of
        true -> <<"pass">>;
        false -> <<"fail">>
    end,

    #{
        receipt_id => generate_receipt_id(),
        receipt_type => <<"quality_gates">>,
        stage => <<"quality_verification">>,
        status => Status,
        results => Results,
        configuration => Config,
        timestamp => erlang:system_time(millisecond),
        timestamp_iso => format_timestamp(erlang:system_time(millisecond))
    }.

%% @doc Get test metrics - mock implementation for testing
get_test_metrics(_State) ->
    #{
        total_tests => 10,
        passed_tests => 10,
        coverage => 85.0
    }.

%% Helper functions for test implementations
generate_receipt_id() ->
    Timestamp = erlang:system_time(microsecond),
    iolist_to_binary(io_lib:format("rcpt_~p", [Timestamp])).

format_timestamp(Milliseconds) ->
    Seconds = Milliseconds div 1000,
    DateTime = calendar:gregorian_seconds_to_datetime(Seconds + 62167219200),
    {{Y, M, D}, {H, Min, S}} = DateTime,
    iolist_to_binary(io_lib:format("~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0BZ",
                                    [Y, M, D, H, Min, S])).

-endif.
