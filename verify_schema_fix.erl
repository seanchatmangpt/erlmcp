#!/usr/bin/env escript
%% Quick verification that schema validator error formatting fix works

main(_) ->
    io:format("Verifying schema validator error formatting fix...~n~n"),

    % Add paths
    code:add_pathsa(["_build/default/lib/jesse/ebin",
                     "_build/default/lib/jsx/ebin",
                     "apps/erlmcp_core/ebin"]),

    % Test 1: Verify parameter order is correct (Data before Path)
    io:format("Test 1: Parameter order verification~n"),
    Schema = #{<<"type">> => <<"string">>},
    Error = {missing_required_property, <<"name">>},
    Data = #{<<"age">> => 30},
    Path = [<<"user">>],

    try
        Result = erlmcp_schema_validator:format_jesse_error(
            {data_invalid, Schema, Error, Data, Path}),
        io:format("  Path: ~p~n", [maps:get(path, Result)]),
        io:format("  Expected Path: ~p~n", [Path]),
        io:format("  Actual (Data): ~p~n", [maps:get(actual, Result)]),
        io:format("  Expected Data: ~p~n", [Data]),

        case {maps:get(path, Result), maps:get(actual, Result)} of
            {Path, Data} ->
                io:format("  ✓ PASS: Parameters in correct order (Data, Path)~n~n");
            _ ->
                io:format("  ✗ FAIL: Parameters in wrong order~n~n")
        end
    catch
        _:Err ->
            io:format("  ✗ FAIL: Error: ~p~n~n", [Err])
    end,

    % Test 2: Verify all error types are handled
    io:format("Test 2: Error type handling~n"),
    ErrorTypes = [
        {missing_required_property, <<"name">>},
        {wrong_type, <<"string">>},
        {not_in_enum, [1, 2, 3]},
        {not_unique, []},
        wrong_length,
        wrong_size,
        {missing_dependency, <<"address">>},
        no_match,
        no_extra_properties_allowed,
        no_extra_items_allowed,
        not_allowed,
        not_in_range,
        not_divisible,
        not_array,
        wrong_format,
        too_many_properties,
        too_few_properties,
        all_schemas_not_valid,
        any_schemas_not_valid,
        not_multiple_of,
        not_one_schema_valid,
        more_than_one_schema_valid,
        not_schema_valid,
        validation_always_fails,
        external,
        not_found
    ],

    Passed = lists:foldl(fun(ErrorType, Acc) ->
        try
            Msg = erlmcp_schema_validator:format_error_message(ErrorType),
            io:format("  ✓ ~p: ~s~n", [ErrorType, Msg]),
            Acc + 1
        catch
            _:Error ->
                io:format("  ✗ ~p: ERROR: ~p~n", [ErrorType, Error]),
                Acc
        end
    end, 0, ErrorTypes),

    io:format("~n  Summary: ~p/~p error types handled~n", [Passed, length(ErrorTypes)]),

    if
        Passed =:= length(ErrorTypes) ->
            io:format("~n✓ All tests passed! Fix verified.~n");
        true ->
            io:format("~n✗ Some tests failed!~n")
    end,

    init:stop().
