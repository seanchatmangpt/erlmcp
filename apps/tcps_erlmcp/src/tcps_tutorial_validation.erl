%%%-----------------------------------------------------------------------------
%%% @doc TCPS Tutorial Progress Validation and Checkpoint System
%%%
%%% Provides comprehensive validation for tutorial step completion, progress
%%% checkpoints, and learning verification. Ensures users achieve learning
%%% objectives before progressing.
%%%
%%% Features:
%%% - Step completion validation with custom criteria
%%% - Output verification against expected results
%%% - Code quality checks for hands-on exercises
%%% - Progress checkpoints with state capture
%%% - Prerequisite validation
%%% - Achievement unlocking logic
%%% - Learning assessment and feedback
%%%
%%% Validation Types:
%%% - output_match: Verify output matches expected pattern
%%% - state_check: Validate system state (e.g., service running, WIP limits)
%%% - code_quality: Check code follows best practices
%%% - comprehension: Test understanding with questions
%%% - integration: Verify components work together
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(tcps_tutorial_validation).

%% API exports
-export([
    validate_step/3,
    validate_prerequisites/2,
    verify_checkpoint/2,
    assess_comprehension/2,
    validate_code_quality/1,
    get_validation_criteria/2
]).

%%%=============================================================================
%%% Type Definitions
%%%=============================================================================

-type tutorial_id() :: atom().
-type step_id() :: binary().
-type validation_result() :: #{
    valid := boolean(),
    errors := [binary()],
    warnings := [binary()],
    suggestions := [binary()],
    score := float()  % 0.0 to 1.0
}.

-type validation_criteria() :: #{
    type := output_match | state_check | code_quality | comprehension | integration,
    required := boolean(),
    weight := float(),
    validator := fun((map()) -> validation_result())
}.

-export_type([validation_result/0, validation_criteria/0]).

%%%=============================================================================
%%% API Functions
%%%=============================================================================

%% @doc Validate step completion with custom validation data
-spec validate_step(tutorial_id(), step_id(), map()) -> validation_result().
validate_step(TutorialId, StepId, ValidationData) ->
    Criteria = get_validation_criteria(TutorialId, StepId),
    Results = lists:map(fun(C) ->
        Validator = maps:get(validator, C),
        Weight = maps:get(weight, C),
        Result = Validator(ValidationData),
        {Result, Weight}
    end, Criteria),

    aggregate_results(Results).

%% @doc Validate prerequisites for a tutorial or step
-spec validate_prerequisites(tutorial_id(), map()) -> {ok, valid} | {error, [binary()]}.
validate_prerequisites(TutorialId, UserProgress) ->
    Prerequisites = get_prerequisites(TutorialId),
    CompletedTutorials = maps:get(completed_tutorials, UserProgress, []),

    Unmet = [P || P <- Prerequisites, not lists:member(P, CompletedTutorials)],

    case Unmet of
        [] -> {ok, valid};
        _ -> {error, Unmet}
    end.

%% @doc Verify checkpoint state is valid
-spec verify_checkpoint(step_id(), map()) -> {ok, valid} | {error, term()}.
verify_checkpoint(StepId, CheckpointState) ->
    RequiredKeys = get_required_checkpoint_keys(StepId),

    case validate_checkpoint_keys(RequiredKeys, CheckpointState) of
        ok -> {ok, valid};
        {error, MissingKeys} -> {error, {missing_keys, MissingKeys}}
    end.

%% @doc Assess user comprehension with questions
-spec assess_comprehension(step_id(), map()) -> validation_result().
assess_comprehension(StepId, Answers) ->
    Questions = get_comprehension_questions(StepId),
    Results = lists:map(fun({QuestionId, CorrectAnswer}) ->
        UserAnswer = maps:get(QuestionId, Answers, undefined),
        case UserAnswer of
            CorrectAnswer -> {correct, 1.0};
            _ -> {incorrect, 0.0}
        end
    end, Questions),

    {Correct, Total} = lists:foldl(fun
        ({correct, _}, {C, T}) -> {C + 1, T + 1};
        ({incorrect, _}, {C, T}) -> {C, T + 1}
    end, {0, 0}, Results),

    Score = case Total of
        0 -> 1.0;
        _ -> Correct / Total
    end,

    #{
        valid => Score >= 0.7,  % 70% passing score
        errors => [],
        warnings => case Score < 1.0 of
            true -> [<<"Some answers were incorrect - review the material">>];
            false -> []
        end,
        suggestions => case Score < 0.7 of
            true -> [<<"Consider reviewing the tutorial steps before continuing">>];
            false -> []
        end,
        score => Score
    }.

%% @doc Validate code quality for hands-on exercises
-spec validate_code_quality(binary()) -> validation_result().
validate_code_quality(Code) ->
    Checks = [
        {check_syntax, fun check_erlang_syntax/1},
        {check_style, fun check_code_style/1},
        {check_best_practices, fun check_best_practices/1}
    ],

    Results = lists:map(fun({_CheckName, CheckFun}) ->
        CheckFun(Code)
    end, Checks),

    aggregate_results([{R, 1.0} || R <- Results]).

%% @doc Get validation criteria for specific step
-spec get_validation_criteria(tutorial_id(), step_id()) -> [validation_criteria()].
get_validation_criteria(quality_gate, <<"qg_step_3">>) ->
    [
        #{
            type => state_check,
            required => true,
            weight => 0.6,
            validator => fun(Data) ->
                case maps:get(gate_result, Data, undefined) of
                    {pass, _} ->
                        #{valid => true, errors => [], warnings => [], suggestions => [], score => 1.0};
                    {fail, _} ->
                        #{valid => true, errors => [], warnings => [<<"Gate failed - expected for learning">>], suggestions => [], score => 1.0};
                    _ ->
                        #{valid => false, errors => [<<"Gate not executed">>], warnings => [], suggestions => [<<"Run tcps_quality_gates:check_gate/2">>], score => 0.0}
                end
            end
        },
        #{
            type => output_match,
            required => true,
            weight => 0.4,
            validator => fun(Data) ->
                case maps:get(sku_id, Data, undefined) of
                    undefined ->
                        #{valid => false, errors => [<<"SKU ID required">>], warnings => [], suggestions => [<<"Provide sku_id in input">>], score => 0.0};
                    _ ->
                        #{valid => true, errors => [], warnings => [], suggestions => [], score => 1.0}
                end
            end
        }
    ];

get_validation_criteria(kanban_wip, <<"kb_step_5">>) ->
    [
        #{
            type => state_check,
            required => true,
            weight => 1.0,
            validator => fun(Data) ->
                case maps:get(pull_signal_result, Data, undefined) of
                    {ok, _WorkOrder} ->
                        #{valid => true, errors => [], warnings => [], suggestions => [], score => 1.0};
                    {error, wip_limit_reached} ->
                        #{valid => true, errors => [], warnings => [<<"WIP limit reached">>], suggestions => [<<"This is expected behavior">>], score => 1.0};
                    _ ->
                        #{valid => false, errors => [<<"Pull signal not processed">>], warnings => [], suggestions => [<<"Use tcps_kanban:process_pull_signal/1">>], score => 0.0}
                end
            end
        }
    ];

get_validation_criteria(andon_event, <<"ae_step_2">>) ->
    [
        #{
            type => state_check,
            required => true,
            weight => 0.7,
            validator => fun(Data) ->
                case maps:get(andon_event_id, Data, undefined) of
                    undefined ->
                        #{valid => false, errors => [<<"Andon event not triggered">>], warnings => [], suggestions => [<<"Use tcps_andon:trigger_andon/2">>], score => 0.0};
                    EventId when is_binary(EventId) ->
                        #{valid => true, errors => [], warnings => [], suggestions => [], score => 1.0};
                    _ ->
                        #{valid => false, errors => [<<"Invalid event ID">>], warnings => [], suggestions => [], score => 0.0}
                end
            end
        },
        #{
            type => integration,
            required => true,
            weight => 0.3,
            validator => fun(Data) ->
                EventId = maps:get(andon_event_id, Data, undefined),
                case EventId of
                    undefined ->
                        #{valid => false, errors => [<<"No event to verify">>], warnings => [], suggestions => [], score => 0.0};
                    _ ->
                        case tcps_andon:get_andon_event(EventId) of
                            {ok, _Event} ->
                                #{valid => true, errors => [], warnings => [], suggestions => [], score => 1.0};
                            {error, _} ->
                                #{valid => false, errors => [<<"Event not found in system">>], warnings => [], suggestions => [], score => 0.0}
                        end
                end
            end
        }
    ];

get_validation_criteria(five_whys, <<"fw_step_3">>) ->
    [
        #{
            type => state_check,
            required => true,
            weight => 0.6,
            validator => fun(Data) ->
                Whys = maps:get(whys, Data, []),
                case length(Whys) of
                    5 ->
                        #{valid => true, errors => [], warnings => [], suggestions => [], score => 1.0};
                    N when N < 5 ->
                        #{valid => false, errors => [io_lib:format("Only ~p whys provided, need 5", [N])], warnings => [], suggestions => [<<"Keep asking why until you reach 5 levels">>], score => N / 5.0};
                    N when N > 5 ->
                        #{valid => true, errors => [], warnings => [<<"More than 5 whys provided">>], suggestions => [], score => 1.0}
                end
            end
        },
        #{
            type => code_quality,
            required => true,
            weight => 0.4,
            validator => fun(Data) ->
                Whys = maps:get(whys, Data, []),
                case lists:all(fun(Why) ->
                    is_binary(Why) andalso byte_size(Why) > 10
                end, Whys) of
                    true ->
                        #{valid => true, errors => [], warnings => [], suggestions => [], score => 1.0};
                    false ->
                        #{valid => false, errors => [<<"Some answers are too short or invalid">>], warnings => [], suggestions => [<<"Provide detailed, meaningful answers">>], score => 0.5}
                end
            end
        }
    ];

get_validation_criteria(complete_workflow, <<"cw_step_4">>) ->
    [
        #{
            type => integration,
            required => true,
            weight => 1.0,
            validator => fun(Data) ->
                Results = maps:get(gate_results, Data, []),
                case Results of
                    [] ->
                        #{valid => false, errors => [<<"No gate results">>], warnings => [], suggestions => [<<"Run tcps_quality_gates:check_all_gates/1">>], score => 0.0};
                    _ ->
                        TotalGates = length(Results),
                        PassedGates = length([1 || {_, {pass, _}} <- Results]),
                        Score = PassedGates / TotalGates,

                        #{
                            valid => TotalGates > 0,
                            errors => [],
                            warnings => case Score < 1.0 of
                                true -> [io_lib:format("~p/~p gates passed", [PassedGates, TotalGates])];
                                false -> []
                            end,
                            suggestions => case Score < 1.0 of
                                true -> [<<"Failed gates will trigger Andon - proceed to resolution">>];
                                false -> [<<"All gates passed! Ready to complete.">>]
                            end,
                            score => Score
                        }
                end
            end
        }
    ];

get_validation_criteria(_, _) ->
    [
        #{
            type => output_match,
            required => false,
            weight => 1.0,
            validator => fun(_) ->
                #{valid => true, errors => [], warnings => [], suggestions => [], score => 1.0}
            end
        }
    ].

%%%=============================================================================
%%% Internal Functions
%%%=============================================================================

%% @private Aggregate validation results
aggregate_results(Results) ->
    {TotalScore, TotalWeight, AllErrors, AllWarnings, AllSuggestions} =
        lists:foldl(fun({Result, Weight}, {ScoreAcc, WeightAcc, ErrAcc, WarnAcc, SugAcc}) ->
            Score = maps:get(score, Result),
            Errors = maps:get(errors, Result),
            Warnings = maps:get(warnings, Result),
            Suggestions = maps:get(suggestions, Result),

            {
                ScoreAcc + (Score * Weight),
                WeightAcc + Weight,
                ErrAcc ++ Errors,
                WarnAcc ++ Warnings,
                SugAcc ++ Suggestions
            }
        end, {0.0, 0.0, [], [], []}, Results),

    FinalScore = case TotalWeight of
        +0.0 -> 1.0;
        _ -> TotalScore / TotalWeight
    end,

    #{
        valid => FinalScore >= 0.7 andalso AllErrors =:= [],
        errors => lists:usort(AllErrors),
        warnings => lists:usort(AllWarnings),
        suggestions => lists:usort(AllSuggestions),
        score => FinalScore
    }.

%% @private Get prerequisites for tutorial
get_prerequisites(quality_gate) -> [];
get_prerequisites(kanban_wip) -> [];
get_prerequisites(andon_event) -> [quality_gate];
get_prerequisites(five_whys) -> [andon_event];
get_prerequisites(complete_workflow) -> [kanban_wip, five_whys];
get_prerequisites(_) -> [].

%% @private Get required checkpoint keys for step
get_required_checkpoint_keys(<<"qg_step_2">>) -> [service_started];
get_required_checkpoint_keys(<<"qg_step_3">>) -> [gate_passed, receipt];
get_required_checkpoint_keys(<<"kb_step_3">>) -> [wip_checked];
get_required_checkpoint_keys(<<"kb_step_4">>) -> [limit_set, new_limit];
get_required_checkpoint_keys(<<"ae_step_2">>) -> [andon_triggered, event_id];
get_required_checkpoint_keys(<<"fw_step_2">>) -> [analysis_started, analysis_id];
get_required_checkpoint_keys(<<"cw_step_2">>) -> [work_order_created, work_order_id];
get_required_checkpoint_keys(_) -> [].

%% @private Validate checkpoint has required keys
validate_checkpoint_keys(RequiredKeys, CheckpointState) ->
    MissingKeys = [K || K <- RequiredKeys, not maps:is_key(K, CheckpointState)],
    case MissingKeys of
        [] -> ok;
        _ -> {error, MissingKeys}
    end.

%% @private Get comprehension questions for step
get_comprehension_questions(<<"qg_step_1">>) ->
    [
        {<<"q1">>, <<"prevent_defects">>},
        {<<"q2">>, <<"jidoka">>}
    ];
get_comprehension_questions(<<"kb_step_1">>) ->
    [
        {<<"q1">>, <<"prevent_overload">>},
        {<<"q2">>, <<"five">>}
    ];
get_comprehension_questions(<<"ae_step_1">>) ->
    [
        {<<"q1">>, <<"stop_production">>},
        {<<"q2">>, <<"immediate_visibility">>}
    ];
get_comprehension_questions(<<"fw_step_1">>) ->
    [
        {<<"q1">>, <<"root_cause">>},
        {<<"q2">>, <<"systemic">>}
    ];
get_comprehension_questions(_) ->
    [].

%% @private Check Erlang syntax
check_erlang_syntax(Code) ->
    try
        case erl_scan:string(binary_to_list(Code)) of
            {ok, Tokens, _} ->
                case erl_parse:parse_exprs(Tokens) of
                    {ok, _} ->
                        #{valid => true, errors => [], warnings => [], suggestions => [], score => 1.0};
                    {error, {_, _, ErrorMsg}} ->
                        #{valid => false, errors => [list_to_binary(ErrorMsg)], warnings => [], suggestions => [<<"Check syntax errors">>], score => 0.0}
                end;
            {error, {_, _, ErrorMsg}, _} ->
                #{valid => false, errors => [list_to_binary(ErrorMsg)], warnings => [], suggestions => [<<"Check for syntax errors">>], score => 0.0}
        end
    catch
        _:_ ->
            #{valid => false, errors => [<<"Cannot parse code">>], warnings => [], suggestions => [<<"Ensure valid Erlang syntax">>], score => 0.0}
    end.

%% @private Check code style
check_code_style(Code) ->
    Warnings = [],

    %% Check line length (soft limit 100 chars)
    Lines = binary:split(Code, <<"\n">>, [global]),
    LongLines = [L || L <- Lines, byte_size(L) > 100],

    Warnings2 = case LongLines of
        [] -> Warnings;
        _ -> [<<"Some lines exceed 100 characters">> | Warnings]
    end,

    %% Check for proper indentation (basic check)
    Warnings3 = case binary:match(Code, <<"\t">>) of
        nomatch -> Warnings2;
        _ -> [<<"Use spaces instead of tabs">> | Warnings2]
    end,

    #{
        valid => true,
        errors => [],
        warnings => Warnings3,
        suggestions => case Warnings3 of
            [] -> [];
            _ -> [<<"Follow Erlang style guidelines">>]
        end,
        score => case Warnings3 of
            [] -> 1.0;
            _ -> 0.8
        end
    }.

%% @private Check best practices
check_best_practices(Code) ->
    Suggestions = [],

    %% Check for pattern matching instead of case when possible
    Suggestions2 = case binary:match(Code, <<"case">>) of
        nomatch -> Suggestions;
        _ -> [<<"Consider using pattern matching in function heads">> | Suggestions]
    end,

    %% Check for proper error handling
    HasErrorHandling = (binary:match(Code, <<"try">>) =/= nomatch) orelse
                       (binary:match(Code, <<"{error,">>) =/= nomatch),

    Suggestions3 = case HasErrorHandling of
        true -> Suggestions2;
        false -> [<<"Consider adding error handling">> | Suggestions2]
    end,

    #{
        valid => true,
        errors => [],
        warnings => [],
        suggestions => Suggestions3,
        score => case Suggestions3 of
            [] -> 1.0;
            [_] -> 0.9;
            _ -> 0.8
        end
    }.
