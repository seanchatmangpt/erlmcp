%%%-------------------------------------------------------------------
%%% @doc
%%% Machine-Readable JSON Compliance Reporter for CI/CD
%%%
%%% "DATA SHOULD BE MACHINE-READABLE OR IT'S USELESS." - Joe Armstrong
%%%
%%% Generates structured JSON that CI/CD pipelines can ACTUALLY PARSE:
%%% - Parseable by jq, Python, Go, any JSON consumer
%%% - Easy to filter failures
%%% - Structured data (no strings in maps where primitives work)
%%% - Include timestamps and versions
%%% - Zero ambiguity - atomic types (integers, booleans, binaries)
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_compliance_report_json).

%% API
-export([generate_report/1, generate_report/2, format_validator_result/2, compute_status/1,
         count_total/1, count_passed/1, count_failed/1, compute_rate/1, extract_failures/1,
         extract_warnings/1]).

%% Types
-type validator_result() ::
    #{name := binary(),
      status := passed | failed | warning,
      timestamp := integer(),
      checks := [map()],
      passed := non_neg_integer(),
      failed := non_neg_integer()}.
-type validation_results() ::
    #{protocol := validator_result(),
      transport := validator_result(),
      security := validator_result(),
      performance := validator_result()}.
-type json_report() ::
    #{timestamp := integer(),
      spec_version := binary(),
      overall_status := pass | fail | partial,
      validators := map(),
      summary :=
          #{total_tests := non_neg_integer(),
            passed := non_neg_integer(),
            failed := non_neg_integer(),
            pass_rate := float()}}.

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Generate full JSON compliance report from validation results
-spec generate_report(validation_results()) -> binary().
generate_report(ValidationResults) ->
    generate_report(ValidationResults, #{}).

%% @doc Generate JSON report with options
%% Options:
%%   - pretty: boolean() - pretty print JSON (default: true)
%%   - spec_version: binary() - override spec version (default: <<"2025-11-25">>)
-spec generate_report(validation_results(), map()) -> binary().
generate_report(ValidationResults, Options) ->
    %% Extract options with defaults
    PrettyPrint = maps:get(pretty, Options, true),
    SpecVersion = maps:get(spec_version, Options, <<"2025-11-25">>),

    %% Build report structure - MACHINE READABLE
    Report =
        #{timestamp => erlang:system_time(millisecond),
          spec_version => SpecVersion,
          overall_status => compute_status(ValidationResults),
          validators =>
              #{protocol => format_validator_result(protocol, ValidationResults),
                transport => format_validator_result(transport, ValidationResults),
                security => format_validator_result(security, ValidationResults),
                performance => format_validator_result(performance, ValidationResults)},
          summary =>
              #{total_tests => count_total(ValidationResults),
                passed => count_passed(ValidationResults),
                failed => count_failed(ValidationResults),
                pass_rate => compute_rate(ValidationResults)},
          %% CI/CD can easily filter failures
          failures => extract_failures(ValidationResults),
          warnings => extract_warnings(ValidationResults)},

    %% Encode to JSON - PRETTY FOR HUMAN READABILITY, MACHINE PARSEABLE
    JSEncodeOptions =
        case PrettyPrint of
            true ->
                [{space, 1}, {indent, 2}];
            false ->
                []
        end,
    jsx:encode(Report, JSEncodeOptions).

%% @doc Format individual validator result for JSON
-spec format_validator_result(atom(), validation_results()) -> map().
format_validator_result(ValidatorKey, ValidationResults) ->
    case maps:get(ValidatorKey, ValidationResults, undefined) of
        undefined ->
            #{status => <<"skipped">>,
              message => <<"Validator not executed">>,
              timestamp => erlang:system_time(millisecond),
              checks => [],
              passed => 0,
              failed => 0};
        Result when is_map(Result) ->
            %% Convert status atom to binary for JSON
            StatusBin =
                case maps:get(status, Result, unknown) of
                    passed ->
                        <<"passed">>;
                    failed ->
                        <<"failed">>;
                    warning ->
                        <<"warning">>;
                    Other ->
                        atom_to_binary(Other)
                end,

            %% Build clean result map - NO NESTED STRINGS, USE PRIMITIVES
            Base =
                #{status => StatusBin,
                  timestamp => maps:get(timestamp, Result, erlang:system_time(millisecond)),
                  passed => maps:get(passed, Result, 0),
                  failed => maps:get(failed, Result, 0)},

            %% Add checks if present - KEEP STRUCTURED
            case maps:get(checks, Result, undefined) of
                undefined ->
                    Base;
                Checks ->
                    Base#{checks => format_checks(Checks)}
            end
    end.

%% @doc Format checks list - ensure machine-readable structure
-spec format_checks([map()]) -> [map()].
format_checks(Checks) when is_list(Checks) ->
    [format_check(Check) || Check <- Checks];
format_checks(_) ->
    [].

%% @doc Format individual check - convert atoms to binaries
-spec format_check(map()) -> map().
format_check(Check) ->
    StatusBin =
        case maps:get(status, Check, unknown) of
            passed ->
                <<"passed">>;
            failed ->
                <<"failed">>;
            warning ->
                <<"warning">>;
            Other ->
                atom_to_binary(Other)
        end,
    Base = #{name => maps:get(name, Check, <<"unknown">>), status => StatusBin},
    %% Add optional fields if present
    Maybe1 =
        case maps:get(message, Check, undefined) of
            undefined ->
                Base;
            Msg ->
                Base#{message => Msg}
        end,
    Maybe2 =
        case maps:get(details, Check, undefined) of
            undefined ->
                Maybe1;
            Details ->
                Maybe1#{details => Details}
        end,
    Maybe2.

%% @doc Compute overall status from all validators
-spec compute_status(validation_results()) -> binary().
compute_status(ValidationResults) ->
    TotalFailed = count_failed(ValidationResults),
    TotalPassed = count_passed(ValidationResults),
    Total = TotalPassed + TotalFailed,

    if Total =:= 0 ->
           <<"skipped">>;
       TotalFailed =:= 0 ->
           <<"pass">>;
       TotalPassed > TotalFailed ->
           <<"partial">>;
       true ->
           <<"fail">>
    end.

%% @doc Count total tests across all validators
-spec count_total(validation_results()) -> non_neg_integer().
count_total(ValidationResults) ->
    count_passed(ValidationResults) + count_failed(ValidationResults).

%% @doc Count passed tests across all validators
-spec count_passed(validation_results()) -> non_neg_integer().
count_passed(ValidationResults) ->
    Validators = [protocol, transport, security, performance],
    lists:foldl(fun(Key, Acc) ->
                   case maps:get(Key, ValidationResults, undefined) of
                       undefined ->
                           Acc;
                       Result ->
                           Acc + maps:get(passed, Result, 0)
                   end
                end,
                0,
                Validators).

%% @doc Count failed tests across all validators
-spec count_failed(validation_results()) -> non_neg_integer().
count_failed(ValidationResults) ->
    Validators = [protocol, transport, security, performance],
    lists:foldl(fun(Key, Acc) ->
                   case maps:get(Key, ValidationResults, undefined) of
                       undefined ->
                           Acc;
                       Result ->
                           Acc + maps:get(failed, Result, 0)
                   end
                end,
                0,
                Validators).

%% @doc Compute pass rate as percentage
-spec compute_rate(validation_results()) -> float().
compute_rate(ValidationResults) ->
    Total = count_total(ValidationResults),
    Passed = count_passed(ValidationResults),
    case Total of
        0 ->
            0.0;
        _ ->
            Passed / Total * 100.0
    end.

%% @doc Extract all failures for CI/CD filtering
-spec extract_failures(validation_results()) -> [map()].
extract_failures(ValidationResults) ->
    Validators = [protocol, transport, security, performance],
    lists:foldl(fun(Key, Acc) ->
                   case maps:get(Key, ValidationResults, undefined) of
                       undefined ->
                           Acc;
                       Result ->
                           ValidatorName = atom_to_binary(Key),
                           Checks = maps:get(checks, Result, []),
                           FailedChecks =
                               lists:filter(fun(C) -> maps:get(status, C, passed) =:= failed end,
                                            Checks),
                           %% Add validator context to each failure
                           FailedWithCtx =
                               lists:map(fun(C) -> C#{validator => ValidatorName} end,
                                         FailedChecks),
                           Acc ++ FailedWithCtx
                   end
                end,
                [],
                Validators).

%% @doc Extract all warnings for CI/CD filtering
-spec extract_warnings(validation_results()) -> [map()].
extract_warnings(ValidationResults) ->
    Validators = [protocol, transport, security, performance],
    lists:foldl(fun(Key, Acc) ->
                   case maps:get(Key, ValidationResults, undefined) of
                       undefined ->
                           Acc;
                       Result ->
                           ValidatorName = atom_to_binary(Key),
                           Checks = maps:get(checks, Result, []),
                           WarningChecks =
                               lists:filter(fun(C) -> maps:get(status, C, passed) =:= warning end,
                                            Checks),
                           %% Add validator context to each warning
                           WarningWithCtx =
                               lists:map(fun(C) -> C#{validator => ValidatorName} end,
                                         WarningChecks),
                           Acc ++ WarningWithCtx
                   end
                end,
                [],
                Validators).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @doc Validate report structure (for testing)
-spec validate_report_structure(json_report()) -> boolean().
validate_report_structure(Report) ->
    RequiredKeys = [timestamp, spec_version, overall_status, validators, summary],
    HasRequired = lists:all(fun(K) -> maps:is_key(K, Report) end, RequiredKeys),
    HasValidators =
        maps:is_key(validators, Report)
        andalso lists:all(fun(K) -> maps:is_key(K, maps:get(validators, Report)) end,
                          [protocol, transport, security, performance]),
    HasSummary =
        maps:is_key(summary, Report)
        andalso lists:all(fun(K) -> maps:is_key(K, maps:get(summary, Report)) end,
                          [total_tests, passed, failed, pass_rate]),
    HasRequired andalso HasValidators andalso HasSummary.
