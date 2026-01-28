%%%-------------------------------------------------------------------
%% @doc CI Execution Script for Pricing Plan Validation
%%
%% Loads all plan JSON files from plans/ directory and validates them
%% using erlmcp_pricing_poka_yoke validators.
%%
%% Output: Both human-readable and JSON formats
%% Exit Code: 0 if all plans pass, non-zero with error summary if failures
%%
%% Usage:
%%   escript scripts/validate_plans.erl [plans_dir] [output_format]
%%
%% Arguments:
%%   plans_dir       - Directory containing .plan.json files (default: plans/)
%%   output_format   - 'human' or 'json' (default: human)
%%
%% Example:
%%   escript scripts/validate_plans.erl plans/ human
%%   escript scripts/validate_plans.erl plans/ json > validation_results.json
%%
%% @end
%%%-------------------------------------------------------------------

-module(validate_plans).

%%====================================================================
%% Escript Main Entry Point
%%====================================================================

main(Args) ->
    PlansDir = case Args of
        [] -> "plans/";
        [D | _] -> D;
        _ -> "plans/"
    end,

    OutputFormat = case Args of
        [_, Format | _] ->
            case Format of
                "json" -> json;
                "human" -> human;
                _ -> human
            end;
        _ -> human
    end,

    case validate_plans_directory(PlansDir, OutputFormat) of
        ok ->
            halt(0);
        {error, _} ->
            halt(1)
    end.

%%====================================================================
%% Main Validation Logic
%%====================================================================

validate_plans_directory(PlansDir, OutputFormat) ->
    io:format("Loading pricing plans from: ~s~n", [PlansDir]),

    case filelib:wildcard(filename:join(PlansDir, "*.plan.json")) of
        [] ->
            Output = {error, #{
                timestamp => calendar:universal_time(),
                status => failed,
                reason => <<"No .plan.json files found">>,
                directory => list_to_binary(PlansDir),
                plans_validated => 0,
                plans_passed => 0,
                plans_failed => 0
            }},
            output_results(Output, OutputFormat);

        PlanFiles ->
            io:format("Found ~w plan files~n~n", [length(PlanFiles)]),
            Results = validate_all_files(PlanFiles, []),
            output_and_exit(Results, OutputFormat)
    end.

validate_all_files([], Acc) ->
    lists:reverse(Acc);
validate_all_files([File | Rest], Acc) ->
    PlanName = filename:basename(File),
    io:format("Validating: ~s ... ", [PlanName]),

    case erlmcp_pricing_poka_yoke:validate_plan_file(File) of
        {ok, []} ->
            io:format("✓ PASS~n", []),
            validate_all_files(Rest, [{PlanName, pass, []} | Acc]);

        {error, Errors} ->
            io:format("✗ FAIL (~w errors)~n", [length(Errors)]),
            validate_all_files(Rest, [{PlanName, fail, Errors} | Acc]);

        {error, file_not_found} ->
            io:format("✗ NOT FOUND~n", []),
            Error = {schema, root,
                <<"File not found">>,
                <<"Check file path and permissions">>,
                0},
            validate_all_files(Rest, [{PlanName, fail, [Error]} | Acc])
    end.

%%====================================================================
%% Output Formatting
%%====================================================================

output_and_exit(Results, OutputFormat) ->
    PassedCount = count_passed(Results),
    FailedCount = count_failed(Results),
    TotalCount = length(Results),

    case FailedCount of
        0 ->
            io:format("~n", []),
            output_success(Results, PassedCount, OutputFormat),
            ok;

        _ ->
            io:format("~n", []),
            output_failure(Results, PassedCount, FailedCount, TotalCount, OutputFormat),
            {error, validation_failed}
    end.

output_success(Results, PassedCount, human) ->
    io:format("════════════════════════════════════════════════════════════~n"),
    io:format("  PRICING PLAN VALIDATION SUCCESS~n"),
    io:format("════════════════════════════════════════════════════════════~n~n"),
    io:format("✓ All ~w plans validated successfully~n~n", [PassedCount]),

    lists:foreach(fun({PlanName, pass, _}) ->
        io:format("  ✓ ~s~n", [PlanName])
    end, Results),

    io:format("~n"),
    io:format("════════════════════════════════════════════════════════════~n"),
    io:format("Validation gates passed:~n"),
    io:format("  ✓ Schema validation~n"),
    io:format("  ✓ Envelope consistency (throughput < 2x, concurrent < 200K)~n"),
    io:format("  ✓ Refusal code verification~n"),
    io:format("  ✓ Evidence artifacts present~n"),
    io:format("════════════════════════════════════════════════════════════~n");

output_success(Results, PassedCount, json) ->
    Timestamp = format_timestamp(calendar:universal_time()),
    Output = #{
        <<"status">> => <<"success">>,
        <<"timestamp">> => Timestamp,
        <<"plans_validated">> => PassedCount,
        <<"plans_passed">> => PassedCount,
        <<"plans_failed">> => 0,
        <<"results">> => [
            #{
                <<"plan">> => list_to_binary(PlanName),
                <<"status">> => <<"pass">>,
                <<"errors">> => []
            }
            || {PlanName, pass, _} <- Results
        ]
    },
    io:format("~s~n", [jsx:encode(Output, [space, space_after_comma])]).

output_failure(Results, PassedCount, FailedCount, TotalCount, human) ->
    io:format("════════════════════════════════════════════════════════════~n"),
    io:format("  PRICING PLAN VALIDATION FAILED~n"),
    io:format("════════════════════════════════════════════════════════════~n~n"),
    io:format("Results: ~w passed, ~w failed (of ~w total)~n~n", [PassedCount, FailedCount, TotalCount]),

    lists:foreach(fun({PlanName, pass, _}) ->
        io:format("  ✓ ~s~n", [PlanName])
    end, Results),

    io:format("~n"),

    lists:foreach(fun({PlanName, fail, Errors}) ->
        io:format("  ✗ ~s (~w errors)~n", [PlanName, length(Errors)]),
        lists:foreach(fun format_error_human/1, Errors)
    end, Results),

    io:format("~n"),
    io:format("════════════════════════════════════════════════════════════~n"),
    io:format("Failed validation gates:~n"),
    lists:foreach(fun({_Name, fail, Errors}) ->
        Gates = lists:usort([G || {G, _, _, _, _} <- Errors]),
        lists:foreach(fun(G) ->
            io:format("  ✗ ~w~n", [G])
        end, Gates)
    end, Results),
    io:format("════════════════════════════════════════════════════════════~n");

output_failure(Results, PassedCount, FailedCount, TotalCount, json) ->
    Timestamp = format_timestamp(calendar:universal_time()),
    JsonResults = [
        case Status of
            pass ->
                #{
                    <<"plan">> => list_to_binary(PlanName),
                    <<"status">> => <<"pass">>,
                    <<"errors">> => []
                };
            fail ->
                #{
                    <<"plan">> => list_to_binary(PlanName),
                    <<"status">> => <<"fail">>,
                    <<"errors">> => [format_error_json(E) || E <- Errors]
                }
        end
        || {PlanName, Status, Errors} <- Results
    ],

    Output = #{
        <<"status">> => <<"failed">>,
        <<"timestamp">> => Timestamp,
        <<"plans_validated">> => TotalCount,
        <<"plans_passed">> => PassedCount,
        <<"plans_failed">> => FailedCount,
        <<"results">> => JsonResults
    },
    io:format("~s~n", [jsx:encode(Output, [space, space_after_comma])]).

format_error_human({Gate, Field, Message, Remediation, Line}) ->
    io:format("    [~w:~w] Line ~w~n", [Gate, Field, Line]),
    io:format("      ERROR: ~s~n", [Message]),
    io:format("      HINT:  ~s~n", [Remediation]).

format_error_json({Gate, Field, Message, Remediation, Line}) ->
    #{
        <<"gate">> => atom_to_binary(Gate, utf8),
        <<"field">> => atom_to_binary(Field, utf8),
        <<"message">> => Message,
        <<"remediation">> => Remediation,
        <<"line">> => Line
    }.

format_timestamp({{Year, Month, Day}, {Hour, Min, Sec}}) ->
    io_lib:format("~4w-~2..0w-~2..0wT~2..0w:~2..0w:~2..0wZ",
        [Year, Month, Day, Hour, Min, Sec]) |> list_to_binary.

count_passed(Results) ->
    length(lists:filter(fun({_, Status, _}) -> Status == pass end, Results)).

count_failed(Results) ->
    length(lists:filter(fun({_, Status, _}) -> Status == fail end, Results)).

output_results({ok, _}, human) ->
    ok;
output_results({ok, _}, json) ->
    ok;
output_results({error, Error}, human) ->
    io:format("ERROR: ~p~n", [Error]),
    {error, Error};
output_results({error, Error}, json) ->
    io:format("~s~n", [jsx:encode(Error, [space, space_after_comma])]),
    {error, Error}.
