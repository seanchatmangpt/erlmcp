#!/usr/bin/env escript
%% Test script to verify the schema error formatting fix

main(_) ->
    code:add_pathsa(["_build/default/lib/jesse/ebin",
                     "_build/default/lib/jsx/ebin",
                     "_build/default/lib/gproc/ebin",
                     "_build/default/lib/poolboy/ebin",
                     "apps/erlmcp_core/ebin"]),

    io:format("Testing schema error formatting fix...~n"),

    % Test 1: data_invalid error (the main fix)
    io:format("~nTest 1: data_invalid error format~n"),
    Schema = #{<<"type">> => <<"string">>},
    Error = {missing_required_property, <<"name">>},
    Data = #{<<"age">> => 30},
    Path = [<<"user">>],

    try
        Result = erlmcp_schema_validator:format_jesse_error(
            {data_invalid, Schema, Error, Data, Path}),
        io:format("  Result: ~p~n", [Result]),
        io:format("  Path: ~p~n", [maps:get(path, Result)]),
        io:format("  Message: ~p~n", [maps:get(message, Result)]),
        io:format("  Expected: ~p~n", [maps:get(expected, Result)]),
        io:format("  Actual: ~p~n", [maps:get(actual, Result)]),
        io:format("  ✓ PASS: data_invalid error formatted correctly~n")
    catch
        _:Err ->
            io:format("  ✗ FAIL: ~p~n", [Err])
    end,

    % Test 2: schema_invalid error
    io:format("~nTest 2: schema_invalid error format~n"),
    try
        Result2 = erlmcp_schema_validator:format_jesse_error(
            {schema_invalid, Schema, Error}),
        io:format("  Result: ~p~n", [Result2]),
        io:format("  ✓ PASS: schema_invalid error formatted correctly~n")
    catch
        _:Err2 ->
            io:format("  ✗ FAIL: ~p~n", [Err2])
    end,

    % Test 3: parse errors
    io:format("~nTest 3: parse error format~n"),
    try
        Result3 = erlmcp_schema_validator:format_jesse_error(
            {data_error, {parse_error, "unexpected token"}}),
        io:format("  Result: ~p~n", [Result3]),
        io:format("  ✓ PASS: parse error formatted correctly~n")
    catch
        _:Err3 ->
            io:format("  ✗ FAIL: ~p~n", [Err3])
    end,

    % Test 4: format_path
    io:format("~nTest 4: format_path with various types~n"),
    try
        Path1 = erlmcp_schema_validator:format_path([<<"user">>, <<"name">>]),
        io:format("  Binary path: ~p~n", [Path1]),

        Path2 = erlmcp_schema_validator:format_path([user, name]),
        io:format("  Atom path: ~p~n", [Path2]),

        Path3 = erlmcp_schema_validator:format_path([<<"items">>, 0, <<"name">>]),
        io:format("  Mixed path: ~p~n", [Path3]),

        io:format("  ✓ PASS: format_path works correctly~n")
    catch
        _:Err4 ->
            io:format("  ✗ FAIL: ~p~n", [Err4])
    end,

    % Test 5: format_error_message with various error types
    io:format("~nTest 5: format_error_message~n"),
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
        not_allowed,
        not_in_range,
        not_divisible
    ],

    lists:foreach(fun(ErrorType) ->
        try
            Msg = erlmcp_schema_validator:format_error_message(ErrorType),
            io:format("  ~p -> ~s~n", [ErrorType, Msg])
        catch
            _:Err5 ->
                io:format("  ~p -> ERROR: ~p~n", [ErrorType, Err5])
        end
    end, ErrorTypes),

    io:format("~n✓ All tests completed~n"),
    init:stop().
