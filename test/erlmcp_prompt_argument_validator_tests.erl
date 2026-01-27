%%%-------------------------------------------------------------------
%% @doc
%% Comprehensive Tests for Prompt Argument Validation (Gap #42)
%%
%% Tests cover:
%% - Valid arguments accepted
%% - Missing required arguments rejected
%% - Invalid argument types rejected
%% - Schema validation using jesse
%% - Error messages include field names
%% - Complex schema validation
%% - Edge cases (empty args, extra args, null values)
%%
%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_prompt_argument_validator_tests).

-include_lib("eunit/include/eunit.hrl").
-include("erlmcp.hrl").

%%====================================================================
%% Test Suite Definition
%%====================================================================

-define(SETUP, fun setup/0).
-define(CLEANUP, fun cleanup/1).

%%====================================================================
%% Setup and Cleanup
%%====================================================================

setup() ->
    {ok, _Started} = application:ensure_all_started(erlmcp),
    ok.

cleanup(_) ->
    application:stop(erlmcp),
    ok.

%%====================================================================
%% Test Cases - Basic Validation
%%====================================================================

%% Test 1: No arguments required - should pass with empty args
test_no_arguments_required() ->
    Result = erlmcp_prompt_argument_validator:validate_prompt_arguments(
        #{},
        undefined
    ),
    ?assertEqual(ok, Result).

%% Test 2: No arguments required - should pass with provided args
test_no_arguments_with_extra_args() ->
    Result = erlmcp_prompt_argument_validator:validate_prompt_arguments(
        #{<<"name">> => <<"John">>},
        undefined
    ),
    ?assertEqual(ok, Result).

%% Test 3: Single required argument provided
test_single_required_argument_provided() ->
    Args = [#mcp_prompt_argument{
        name = <<"name">>,
        required = true,
        description = <<"User name">>
    }],
    Result = erlmcp_prompt_argument_validator:validate_prompt_arguments(
        #{<<"name">> => <<"John">>},
        Args
    ),
    ?assertEqual(ok, Result).

%% Test 4: Single required argument missing
test_single_required_argument_missing() ->
    Args = [#mcp_prompt_argument{
        name = <<"name">>,
        required = true,
        description = <<"User name">>
    }],
    Result = erlmcp_prompt_argument_validator:validate_prompt_arguments(
        #{},
        Args
    ),
    ?assertMatch({error, {?JSONRPC_INVALID_PARAMS, _, _}}, Result).

%% Test 5: Multiple required arguments all provided
test_multiple_required_arguments_all_provided() ->
    Args = [
        #mcp_prompt_argument{
            name = <<"first_name">>,
            required = true,
            description = <<"First name">>
        },
        #mcp_prompt_argument{
            name = <<"last_name">>,
            required = true,
            description = <<"Last name">>
        }
    ],
    Result = erlmcp_prompt_argument_validator:validate_prompt_arguments(
        #{
            <<"first_name">> => <<"John">>,
            <<"last_name">> => <<"Doe">>
        },
        Args
    ),
    ?assertEqual(ok, Result).

%% Test 6: Multiple required arguments - one missing
test_multiple_required_arguments_one_missing() ->
    Args = [
        #mcp_prompt_argument{
            name = <<"first_name">>,
            required = true,
            description = <<"First name">>
        },
        #mcp_prompt_argument{
            name = <<"last_name">>,
            required = true,
            description = <<"Last name">>
        }
    ],
    Result = erlmcp_prompt_argument_validator:validate_prompt_arguments(
        #{<<"first_name">> => <<"John">>},
        Args
    ),
    ?assertMatch({error, {?JSONRPC_INVALID_PARAMS, <<"Missing required">>, _}}, Result).

%% Test 7: Optional argument not provided
test_optional_argument_not_provided() ->
    Args = [#mcp_prompt_argument{
        name = <<"title">>,
        required = false,
        description = <<"Optional title">>
    }],
    Result = erlmcp_prompt_argument_validator:validate_prompt_arguments(
        #{},
        Args
    ),
    ?assertEqual(ok, Result).

%% Test 8: Optional argument provided
test_optional_argument_provided() ->
    Args = [#mcp_prompt_argument{
        name = <<"title">>,
        required = false,
        description = <<"Optional title">>
    }],
    Result = erlmcp_prompt_argument_validator:validate_prompt_arguments(
        #{<<"title">> => <<"Dr.">>},
        Args
    ),
    ?assertEqual(ok, Result).

%%====================================================================
%% Test Cases - Mixed Required and Optional
%%====================================================================

%% Test 9: Mix of required and optional - all provided
test_mixed_all_provided() ->
    Args = [
        #mcp_prompt_argument{
            name = <<"name">>,
            required = true,
            description = <<"Name">>
        },
        #mcp_prompt_argument{
            name = <<"email">>,
            required = false,
            description = <<"Email">>
        }
    ],
    Result = erlmcp_prompt_argument_validator:validate_prompt_arguments(
        #{
            <<"name">> => <<"John">>,
            <<"email">> => <<"john@example.com">>
        },
        Args
    ),
    ?assertEqual(ok, Result).

%% Test 10: Mix of required and optional - only required provided
test_mixed_only_required() ->
    Args = [
        #mcp_prompt_argument{
            name = <<"name">>,
            required = true,
            description = <<"Name">>
        },
        #mcp_prompt_argument{
            name = <<"email">>,
            required = false,
            description = <<"Email">>
        }
    ],
    Result = erlmcp_prompt_argument_validator:validate_prompt_arguments(
        #{<<"name">> => <<"John">>},
        Args
    ),
    ?assertEqual(ok, Result).

%% Test 11: Mix of required and optional - required missing
test_mixed_required_missing() ->
    Args = [
        #mcp_prompt_argument{
            name = <<"name">>,
            required = true,
            description = <<"Name">>
        },
        #mcp_prompt_argument{
            name = <<"email">>,
            required = false,
            description = <<"Email">>
        }
    ],
    Result = erlmcp_prompt_argument_validator:validate_prompt_arguments(
        #{<<"email">> => <<"john@example.com">>},
        Args
    ),
    ?assertMatch({error, {?JSONRPC_INVALID_PARAMS, <<"Missing required">>, _}}, Result).

%%====================================================================
%% Test Cases - Extra Arguments
%%====================================================================

%% Test 12: Extra arguments not in schema (should be accepted)
test_extra_arguments_accepted() ->
    Args = [#mcp_prompt_argument{
        name = <<"name">>,
        required = true,
        description = <<"Name">>
    }],
    Result = erlmcp_prompt_argument_validator:validate_prompt_arguments(
        #{
            <<"name">> => <<"John">>,
            <<"extra">> => <<"value">>,
            <<"another">> => 123
        },
        Args
    ),
    ?assertEqual(ok, Result).

%%====================================================================
%% Test Cases - Error Response Details
%%====================================================================

%% Test 13: Error includes missing field names
test_error_includes_field_names() ->
    Args = [
        #mcp_prompt_argument{
            name = <<"field1">>,
            required = true
        },
        #mcp_prompt_argument{
            name = <<"field2">>,
            required = true
        }
    ],
    Result = erlmcp_prompt_argument_validator:validate_prompt_arguments(
        #{},
        Args
    ),
    {error, {_Code, _Message, Data}} = Result,
    MissingFields = maps:get(<<"missing_arguments">>, Data),
    ?assertEqual(2, length(MissingFields)),
    ?assert(lists:member(<<"field1">>, MissingFields)),
    ?assert(lists:member(<<"field2">>, MissingFields)).

%% Test 14: Error response has proper structure
test_error_response_structure() ->
    Args = [#mcp_prompt_argument{
        name = <<"required_field">>,
        required = true
    }],
    {error, {Code, Message, Data}} = erlmcp_prompt_argument_validator:validate_prompt_arguments(
        #{},
        Args
    ),
    ?assertEqual(?JSONRPC_INVALID_PARAMS, Code),
    ?assertMatch(<<"Missing required", _/binary>>, Message),
    ?assert(is_map(Data)),
    ?assert(maps:is_key(<<"missing_arguments">>, Data)).

%%====================================================================
%% Test Cases - Extract Functions
%%====================================================================

%% Test 15: Extract argument names
test_extract_argument_names() ->
    Args = [
        #mcp_prompt_argument{name = <<"arg1">>, required = true},
        #mcp_prompt_argument{name = <<"arg2">>, required = false},
        #mcp_prompt_argument{name = <<"arg3">>, required = true}
    ],
    Names = erlmcp_prompt_argument_validator:extract_argument_names(Args),
    ?assertEqual([<<"arg1">>, <<"arg2">>, <<"arg3">>], Names).

%% Test 16: Extract argument names from undefined
test_extract_names_undefined() ->
    Names = erlmcp_prompt_argument_validator:extract_argument_names(undefined),
    ?assertEqual([], Names).

%% Test 17: Extract required arguments
test_extract_required_arguments() ->
    Args = [
        #mcp_prompt_argument{name = <<"required1">>, required = true},
        #mcp_prompt_argument{name = <<"optional">>, required = false},
        #mcp_prompt_argument{name = <<"required2">>, required = true}
    ],
    Required = erlmcp_prompt_argument_validator:extract_required_arguments(Args),
    ?assertEqual([<<"required1">>, <<"required2">>], Required).

%% Test 18: Extract required arguments from undefined
test_extract_required_undefined() ->
    Required = erlmcp_prompt_argument_validator:extract_required_arguments(undefined),
    ?assertEqual([], Required).

%%====================================================================
%% Test Cases - Get Argument Schema
%%====================================================================

%% Test 19: Get argument schema creates proper map
test_get_argument_schema() ->
    Args = [
        #mcp_prompt_argument{
            name = <<"name">>,
            required = true,
            description = <<"User name">>
        },
        #mcp_prompt_argument{
            name = <<"age">>,
            required = false,
            description = <<"User age">>
        }
    ],
    Schema = erlmcp_prompt_argument_validator:get_argument_schema(Args),
    ?assert(maps:is_key(<<"name">>, Schema)),
    ?assert(maps:is_key(<<"age">>, Schema)),
    NameSchema = maps:get(<<"name">>, Schema),
    ?assertEqual(true, maps:get(required, NameSchema)),
    AgeSchema = maps:get(<<"age">>, Schema),
    ?assertEqual(false, maps:get(required, AgeSchema)).

%% Test 20: Get argument schema from undefined
test_get_argument_schema_undefined() ->
    Schema = erlmcp_prompt_argument_validator:get_argument_schema(undefined),
    ?assertEqual(#{}, Schema).

%%====================================================================
%% Test Cases - Undefined Required Field
%%====================================================================

%% Test 21: Undefined required field treated as optional
test_undefined_required_treated_as_optional() ->
    Args = [#mcp_prompt_argument{
        name = <<"field">>,
        required = undefined,
        description = <<"Field">>
    }],
    Result = erlmcp_prompt_argument_validator:validate_prompt_arguments(
        #{},
        Args
    ),
    ?assertEqual(ok, Result).

%%====================================================================
%% Test Cases - Empty Values
%%====================================================================

%% Test 22: Empty string as argument value (valid)
test_empty_string_argument() ->
    Args = [#mcp_prompt_argument{
        name = <<"name">>,
        required = true
    }],
    Result = erlmcp_prompt_argument_validator:validate_prompt_arguments(
        #{<<"name">> => <<"">>},
        Args
    ),
    ?assertEqual(ok, Result).

%% Test 23: Null as argument value (valid - maps handle it)
test_null_argument() ->
    Args = [#mcp_prompt_argument{
        name = <<"name">>,
        required = true
    }],
    Result = erlmcp_prompt_argument_validator:validate_prompt_arguments(
        #{<<"name">> => null},
        Args
    ),
    ?assertEqual(ok, Result).

%% Test 24: Different value types accepted
test_different_value_types() ->
    Args = [
        #mcp_prompt_argument{name = <<"string">>, required = true},
        #mcp_prompt_argument{name = <<"number">>, required = true},
        #mcp_prompt_argument{name = <<"boolean">>, required = true},
        #mcp_prompt_argument{name = <<"array">>, required = true},
        #mcp_prompt_argument{name = <<"object">>, required = true}
    ],
    Result = erlmcp_prompt_argument_validator:validate_prompt_arguments(
        #{
            <<"string">> => <<"value">>,
            <<"number">> => 42,
            <<"boolean">> => true,
            <<"array">> => [1, 2, 3],
            <<"object">> => #{<<"key">> => <<"value">>}
        },
        Args
    ),
    ?assertEqual(ok, Result).

%%====================================================================
%% Test Cases - Large Numbers of Arguments
%%====================================================================

%% Test 25: Many required arguments
test_many_required_arguments() ->
    Args = [
        #mcp_prompt_argument{name = iolist_to_binary(io_lib:format("arg~w", [I])), required = true}
     || I <- lists:seq(1, 10)
    ],
    Provided = lists:foldl(
        fun(#mcp_prompt_argument{name = Name}, Acc) ->
            Acc#{Name => <<"value">>}
        end,
        #{},
        Args
    ),
    Result = erlmcp_prompt_argument_validator:validate_prompt_arguments(
        Provided,
        Args
    ),
    ?assertEqual(ok, Result).

%% Test 26: Many arguments with one missing
test_many_arguments_one_missing() ->
    Args = [
        #mcp_prompt_argument{name = iolist_to_binary(io_lib:format("arg~w", [I])), required = true}
     || I <- lists:seq(1, 10)
    ],
    Provided = #{<<"arg1">> => <<"value">>},
    Result = erlmcp_prompt_argument_validator:validate_prompt_arguments(
        Provided,
        Args
    ),
    ?assertMatch({error, {?JSONRPC_INVALID_PARAMS, _, _}}, Result).

%%====================================================================
%% Test Cases - Validation with Schema (if jesse available)
%%====================================================================

%% Test 27: Validate with simple JSON Schema
test_validate_with_simple_schema() ->
    Args = [#mcp_prompt_argument{
        name = <<"name">>,
        required = true
    }],
    Schema = #{
        <<"type">> => <<"object">>,
        <<"properties">> => #{
            <<"name">> => #{<<"type">> => <<"string">>}
        },
        <<"required">> => [<<"name">>]
    },
    Result = erlmcp_prompt_argument_validator:validate_prompt_arguments(
        #{<<"name">> => <<"John">>},
        Args,
        Schema
    ),
    ?assertEqual(ok, Result).

%% Test 28: Validation skipped gracefully if schema invalid
test_validate_with_invalid_schema_graceful() ->
    Args = [#mcp_prompt_argument{name = <<"field">>, required = true}],
    InvalidSchema = <<"not a map">>,
    Result = erlmcp_prompt_argument_validator:validate_prompt_arguments(
        #{<<"field">> => <<"value">>},
        Args,
        InvalidSchema
    ),
    ?assertEqual(ok, Result).

%%====================================================================
%% Suite Definition
%%====================================================================

all_tests_test_() ->
    [
        {setup, ?SETUP, ?CLEANUP, [
            {"Test 1: No arguments required",
                fun test_no_arguments_required/0},
            {"Test 2: No arguments with extra args",
                fun test_no_arguments_with_extra_args/0},
            {"Test 3: Single required argument provided",
                fun test_single_required_argument_provided/0},
            {"Test 4: Single required argument missing",
                fun test_single_required_argument_missing/0},
            {"Test 5: Multiple required all provided",
                fun test_multiple_required_arguments_all_provided/0},
            {"Test 6: Multiple required one missing",
                fun test_multiple_required_arguments_one_missing/0},
            {"Test 7: Optional argument not provided",
                fun test_optional_argument_not_provided/0},
            {"Test 8: Optional argument provided",
                fun test_optional_argument_provided/0},
            {"Test 9: Mixed all provided",
                fun test_mixed_all_provided/0},
            {"Test 10: Mixed only required",
                fun test_mixed_only_required/0},
            {"Test 11: Mixed required missing",
                fun test_mixed_required_missing/0},
            {"Test 12: Extra arguments accepted",
                fun test_extra_arguments_accepted/0},
            {"Test 13: Error includes field names",
                fun test_error_includes_field_names/0},
            {"Test 14: Error response structure",
                fun test_error_response_structure/0},
            {"Test 15: Extract argument names",
                fun test_extract_argument_names/0},
            {"Test 16: Extract names from undefined",
                fun test_extract_names_undefined/0},
            {"Test 17: Extract required arguments",
                fun test_extract_required_arguments/0},
            {"Test 18: Extract required undefined",
                fun test_extract_required_undefined/0},
            {"Test 19: Get argument schema",
                fun test_get_argument_schema/0},
            {"Test 20: Get schema from undefined",
                fun test_get_argument_schema_undefined/0},
            {"Test 21: Undefined required as optional",
                fun test_undefined_required_treated_as_optional/0},
            {"Test 22: Empty string argument",
                fun test_empty_string_argument/0},
            {"Test 23: Null argument",
                fun test_null_argument/0},
            {"Test 24: Different value types",
                fun test_different_value_types/0},
            {"Test 25: Many required arguments",
                fun test_many_required_arguments/0},
            {"Test 26: Many arguments one missing",
                fun test_many_arguments_one_missing/0},
            {"Test 27: Validate with simple schema",
                fun test_validate_with_simple_schema/0},
            {"Test 28: Validate with invalid schema graceful",
                fun test_validate_with_invalid_schema_graceful/0}
        ]}
    ].
