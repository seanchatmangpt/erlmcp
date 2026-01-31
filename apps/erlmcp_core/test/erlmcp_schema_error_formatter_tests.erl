%%%-------------------------------------------------------------------
%%% @doc Schema Error Formatter Tests
%%%
%%% Chicago School TDD tests for erlmcp_schema_validator error formatting.
%%% Tests observable behavior through public API only.
%%% NO internal state inspection or record duplication.
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_schema_error_formatter_tests).
-include_lib("eunit/include/eunit.hrl").

%%%====================================================================
%%% Error Formatting Tests
%%%====================================================================

format_jesse_error_test_() ->
    [
        {"format_jesse_error with data_invalid", fun test_format_data_invalid/0},
        {"format_jesse_error with schema_invalid", fun test_format_schema_invalid/0},
        {"format_jesse_error with parse errors", fun test_format_parse_errors/0},
        {"format_jesse_error with unknown errors", fun test_format_unknown_errors/0}
    ].

test_format_data_invalid() ->
    Schema = #{<<"type">> => <<"string">>},
    Error = {missing_required_property, <<"name">>},
    Data = #{<<"age">> => 30},
    Path = [<<"user">>],

    Result = erlmcp_schema_validator:format_jesse_error(
        {data_invalid, Schema, Error, Data, Path}),

    ?assertEqual(Path, maps:get(path, Result)),
    ?assertEqual(<<"Missing required property: name">>, maps:get(message, Result)),
    ?assertEqual(Schema, maps:get(expected, Result)),
    ?assertEqual(Data, maps:get(actual, Result)).

test_format_schema_invalid() ->
    Schema = #{<<"type">> => <<"invalid">>},
    Error = wrong_type,

    Result = erlmcp_schema_validator:format_jesse_error(
        {schema_invalid, Schema, Error}),

    ?assertEqual([], maps:get(path, Result)),
    ?assert(is_binary(maps:get(message, Result))),
    ?assertEqual(Schema, maps:get(expected, Result)),
    ?assertEqual(schema_error, maps:get(actual, Result)).

test_format_parse_errors() ->
    Result1 = erlmcp_schema_validator:format_jesse_error(
        {data_error, {parse_error, "unexpected token"}}),

    ?assertEqual([], maps:get(path, Result1)),
    ExpectedMsg1 = <<"Parse error: \"unexpected token\"">>,
    ?assertEqual(ExpectedMsg1, maps:get(message, Result1)),

    Result2 = erlmcp_schema_validator:format_jesse_error(
        {schema_error, {parse_error, "invalid schema"}}),

    ?assertEqual([], maps:get(path, Result2)),
    ExpectedMsg2 = <<"Schema parse error: \"invalid schema\"">>,
    ?assertEqual(ExpectedMsg2, maps:get(message, Result2)).

test_format_unknown_errors() ->
    Result = erlmcp_schema_validator:format_jesse_error(
        {unknown_error, some, random, stuff}),

    ?assertEqual([], maps:get(path, Result)),
    ?assert(is_binary(maps:get(message, Result))),
    ?assertEqual(undefined, maps:get(expected, Result)),
    ?assertEqual(undefined, maps:get(actual, Result)).

%%%====================================================================
%%% Path Formatting Tests
%%%====================================================================

format_path_test_() ->
    [
        {"Binary path elements", fun test_binary_path/0},
        {"Atom path elements", fun test_atom_path/0},
        {"Integer path elements", fun test_integer_path/0},
        {"Mixed types", fun test_mixed_path/0}
    ].

test_binary_path() ->
    ?assertEqual([<<"user">>, <<"name">>],
                 erlmcp_schema_validator:format_path([<<"user">>, <<"name">>])).

test_atom_path() ->
    ?assertEqual([<<"user">>, <<"name">>],
                 erlmcp_schema_validator:format_path([user, name])).

test_integer_path() ->
    ?assertEqual([<<"items">>, <<"0">>, <<"name">>],
                 erlmcp_schema_validator:format_path([<<"items">>, 0, <<"name">>])).

test_mixed_path() ->
    ?assertEqual([<<"root">>, <<"1">>, <<"nested">>],
                 erlmcp_schema_validator:format_path([root, 1, <<"nested">>])).

%%%====================================================================
%%% Error Message Formatting Tests
%%%====================================================================

format_error_message_test_() ->
    [
        {"Missing required property", fun test_missing_required_property/0},
        {"Wrong type", fun test_wrong_type/0},
        {"Not in enum", fun test_not_in_enum/0},
        {"Not unique", fun test_not_unique/0},
        {"Wrong length", fun test_wrong_length/0},
        {"Wrong size", fun test_wrong_size/0},
        {"Missing dependency", fun test_missing_dependency/0},
        {"No match", fun test_no_match/0},
        {"No extra properties", fun test_no_extra_properties/0},
        {"No extra items", fun test_no_extra_items/0},
        {"Not allowed", fun test_not_allowed/0},
        {"Not in range", fun test_not_in_range/0},
        {"Not divisible", fun test_not_divisible/0},
        {"Not array", fun test_not_array/0},
        {"Wrong format", fun test_wrong_format/0},
        {"Too many properties", fun test_too_many_properties/0},
        {"Too few properties", fun test_too_few_properties/0},
        {"All schemas not valid", fun test_all_schemas_not_valid/0},
        {"Any schemas not valid", fun test_any_schemas_not_valid/0},
        {"Not multiple of", fun test_not_multiple_of/0},
        {"Not one schema valid", fun test_not_one_schema_valid/0},
        {"More than one schema valid", fun test_more_than_one_schema_valid/0},
        {"Not schema valid", fun test_not_schema_valid/0},
        {"Validation always fails", fun test_validation_always_fails/0},
        {"External error", fun test_external/0},
        {"Not found", fun test_not_found/0},
        {"Custom tuple error", fun test_custom_tuple_error/0},
        {"Custom atom error", fun test_custom_atom_error/0}
    ].

test_missing_required_property() ->
    ?assertEqual(<<"Missing required property: name">>,
                 erlmcp_schema_validator:format_error_message(
                     {missing_required_property, <<"name">>})).

test_wrong_type() ->
    ?assertEqual(<<"Wrong type, expected: string">>,
                 erlmcp_schema_validator:format_error_message(
                     {wrong_type, <<"string">>})).

test_not_in_enum() ->
    ?assertEqual(<<"Value not in enum: [1,2,3]">>,
                 erlmcp_schema_validator:format_error_message(
                     {not_in_enum, [1, 2, 3]})).

test_not_unique() ->
    ?assertEqual(<<"Array items must be unique">>,
                 erlmcp_schema_validator:format_error_message({not_unique, []})).

test_wrong_length() ->
    ?assertEqual(<<"Array/string has wrong length">>,
                 erlmcp_schema_validator:format_error_message(wrong_length)).

test_wrong_size() ->
    ?assertEqual(<<"Array/string has wrong size">>,
                 erlmcp_schema_validator:format_error_message(wrong_size)).

test_missing_dependency() ->
    ?assertEqual(<<"Missing dependency: address">>,
                 erlmcp_schema_validator:format_error_message(
                     {missing_dependency, <<"address">>})).

test_no_match() ->
    ?assertEqual(<<"Pattern does not match">>,
                 erlmcp_schema_validator:format_error_message(no_match)).

test_no_extra_properties() ->
    ?assertEqual(<<"No extra properties allowed">>,
                 erlmcp_schema_validator:format_error_message(no_extra_properties_allowed)).

test_no_extra_items() ->
    ?assertEqual(<<"No extra items allowed">>,
                 erlmcp_schema_validator:format_error_message(no_extra_items_allowed)).

test_not_allowed() ->
    ?assertEqual(<<"Value not allowed">>,
                 erlmcp_schema_validator:format_error_message(not_allowed)).

test_not_in_range() ->
    ?assertEqual(<<"Value not in allowed range">>,
                 erlmcp_schema_validator:format_error_message(not_in_range)).

test_not_divisible() ->
    ?assertEqual(<<"Value not divisible">>,
                 erlmcp_schema_validator:format_error_message(not_divisible)).

test_not_array() ->
    ?assertEqual(<<"Value is not an array">>,
                 erlmcp_schema_validator:format_error_message(not_array)).

test_wrong_format() ->
    ?assertEqual(<<"Value has wrong format">>,
                 erlmcp_schema_validator:format_error_message(wrong_format)).

test_too_many_properties() ->
    ?assertEqual(<<"Object has too many properties">>,
                 erlmcp_schema_validator:format_error_message(too_many_properties)).

test_too_few_properties() ->
    ?assertEqual(<<"Object has too few properties">>,
                 erlmcp_schema_validator:format_error_message(too_few_properties)).

test_all_schemas_not_valid() ->
    ?assertEqual(<<"All schemas failed validation">>,
                 erlmcp_schema_validator:format_error_message(all_schemas_not_valid)).

test_any_schemas_not_valid() ->
    ?assertEqual(<<"No schemas validated">>,
                 erlmcp_schema_validator:format_error_message(any_schemas_not_valid)).

test_not_multiple_of() ->
    ?assertEqual(<<"Value is not a multiple">>,
                 erlmcp_schema_validator:format_error_message(not_multiple_of)).

test_not_one_schema_valid() ->
    ?assertEqual(<<"No schema validated">>,
                 erlmcp_schema_validator:format_error_message(not_one_schema_valid)).

test_more_than_one_schema_valid() ->
    ?assertEqual(<<"More than one schema validated">>,
                 erlmcp_schema_validator:format_error_message(more_than_one_schema_valid)).

test_not_schema_valid() ->
    ?assertEqual(<<"Schema not valid">>,
                 erlmcp_schema_validator:format_error_message(not_schema_valid)).

test_validation_always_fails() ->
    ?assertEqual(<<"Validation always fails">>,
                 erlmcp_schema_validator:format_error_message(validation_always_fails)).

test_external() ->
    ?assertEqual(<<"External validation error">>,
                 erlmcp_schema_validator:format_error_message(external)).

test_not_found() ->
    ?assertEqual(<<"Resource not found">>,
                 erlmcp_schema_validator:format_error_message(not_found)).

test_custom_tuple_error() ->
    ?assert(is_binary(
        erlmcp_schema_validator:format_error_message({custom_error, some_details}))).

test_custom_atom_error() ->
    ?assert(is_binary(
        erlmcp_schema_validator:format_error_message(some_atom_error))).
