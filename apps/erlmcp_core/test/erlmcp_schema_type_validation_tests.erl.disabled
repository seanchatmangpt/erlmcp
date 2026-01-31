%%%-------------------------------------------------------------------
%%% @doc Schema Type Validation Tests
%%%
%%% Chicago School TDD tests for erlmcp_schema_validator type validation.
%%% Tests observable behavior through public API only.
%%% NO internal state inspection or record duplication.
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_schema_type_validation_tests).
-include_lib("eunit/include/eunit.hrl").

%%%====================================================================
%%% Enum Validation Tests
%%%====================================================================

enum_validation_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          {"validate enum valid value", fun test_validate_enum_valid/0},
          {"validate enum invalid value", fun test_validate_enum_invalid/0},
          {"validate enum with integers", fun test_validate_enum_integers/0},
          {"validate enum with mixed types", fun test_validate_enum_mixed/0},
          {"validate enum with strings", fun test_validate_enum_strings/0}
         ]
     end}.

setup() ->
    ok.

cleanup(_) ->
    ok.

test_validate_enum_valid() ->
    EnumValues = [<<"active">>, <<"inactive">>, <<"pending">>],
    ?assertEqual(ok, erlmcp_schema_validator:validate_enum(<<"active">>, EnumValues)),
    ?assertEqual(ok, erlmcp_schema_validator:validate_enum(<<"inactive">>, EnumValues)),
    ?assertEqual(ok, erlmcp_schema_validator:validate_enum(<<"pending">>, EnumValues)).

test_validate_enum_invalid() ->
    EnumValues = [<<"active">>, <<"inactive">>],
    {error, _Message} = erlmcp_schema_validator:validate_enum(<<"deleted">>, EnumValues),
    ?assertMatch({error, _}, erlmcp_schema_validator:validate_enum(<<"deleted">>, EnumValues)).

test_validate_enum_integers() ->
    EnumValues = [1, 2, 3, 5, 8],
    ?assertEqual(ok, erlmcp_schema_validator:validate_enum(1, EnumValues)),
    ?assertEqual(ok, erlmcp_schema_validator:validate_enum(5, EnumValues)),
    ?assertMatch({error, _}, erlmcp_schema_validator:validate_enum(10, EnumValues)).

test_validate_enum_mixed() ->
    EnumValues = [<<"on">>, <<"off">>, null, 0],
    ?assertEqual(ok, erlmcp_schema_validator:validate_enum(<<"on">>, EnumValues)),
    ?assertEqual(ok, erlmcp_schema_validator:validate_enum(null, EnumValues)),
    ?assertEqual(ok, erlmcp_schema_validator:validate_enum(0, EnumValues)),
    ?assertMatch({error, _}, erlmcp_schema_validator:validate_enum(<<"auto">>, EnumValues)).

test_validate_enum_strings() ->
    EnumValues = [<<"red">>, <<"green">>, <<"blue">>],
    ?assertEqual(ok, erlmcp_schema_validator:validate_enum(<<"red">>, EnumValues)),
    ?assertMatch({error, _}, erlmcp_schema_validator:validate_enum(<<"yellow">>, EnumValues)).

%%%====================================================================
%%% Type Validation Tests
%%%====================================================================

type_validation_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          {"validate type string", fun test_validate_type_string/0},
          {"validate type number", fun test_validate_type_number/0},
          {"validate type integer", fun test_validate_type_integer/0},
          {"validate type boolean", fun test_validate_type_boolean/0},
          {"validate type null", fun test_validate_type_null/0},
          {"validate type array", fun test_validate_type_array/0},
          {"validate type object", fun test_validate_type_object/0},
          {"validate type union", fun test_validate_type_union/0}
         ]
     end}.

test_validate_type_string() ->
    ?assertEqual(ok, erlmcp_schema_validator:validate_type(<<"hello">>, <<"string">>)),
    ?assertMatch({error, _}, erlmcp_schema_validator:validate_type(42, <<"string">>)).

test_validate_type_number() ->
    ?assertEqual(ok, erlmcp_schema_validator:validate_type(3.14, <<"number">>)),
    ?assertEqual(ok, erlmcp_schema_validator:validate_type(42, <<"number">>)),
    ?assertMatch({error, _}, erlmcp_schema_validator:validate_type(<<"42">>, <<"number">>)).

test_validate_type_integer() ->
    ?assertEqual(ok, erlmcp_schema_validator:validate_type(42, <<"integer">>)),
    ?assertMatch({error, _}, erlmcp_schema_validator:validate_type(3.14, <<"integer">>)),
    ?assertMatch({error, _}, erlmcp_schema_validator:validate_type(<<"42">>, <<"integer">>)).

test_validate_type_boolean() ->
    ?assertEqual(ok, erlmcp_schema_validator:validate_type(true, <<"boolean">>)),
    ?assertEqual(ok, erlmcp_schema_validator:validate_type(false, <<"boolean">>)),
    ?assertMatch({error, _}, erlmcp_schema_validator:validate_type(<<"true">>, <<"boolean">>)).

test_validate_type_null() ->
    ?assertEqual(ok, erlmcp_schema_validator:validate_type(null, <<"null">>)),
    ?assertMatch({error, _}, erlmcp_schema_validator:validate_type(undefined, <<"null">>)).

test_validate_type_array() ->
    ?assertEqual(ok, erlmcp_schema_validator:validate_type([1, 2, 3], <<"array">>)),
    ?assertMatch({error, _}, erlmcp_schema_validator:validate_type(#{}, <<"array">>)).

test_validate_type_object() ->
    ?assertEqual(ok, erlmcp_schema_validator:validate_type(#{<<"a">> => 1}, <<"object">>)),
    ?assertMatch({error, _}, erlmcp_schema_validator:validate_type([], <<"object">>)).

test_validate_type_union() ->
    ?assertEqual(ok, erlmcp_schema_validator:validate_type(<<"hello">>, [<<"string">>, <<"null">>])),
    ?assertEqual(ok, erlmcp_schema_validator:validate_type(null, [<<"string">>, <<"null">>])),
    ?assertMatch({error, _}, erlmcp_schema_validator:validate_type(42, [<<"string">>, <<"null">>])).

%%%====================================================================
%%% Required Fields Validation Tests
%%%====================================================================

required_validation_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          {"validate required all present", fun test_validate_required_all_present/0},
          {"validate required missing one", fun test_validate_required_missing_one/0},
          {"validate required missing multiple", fun test_validate_required_missing_multiple/0},
          {"validate required empty list", fun test_validate_required_empty/0}
         ]
     end}.

test_validate_required_all_present() ->
    Object = #{<<"name">> => <<"John">>, <<"age">> => 30},
    Required = [<<"name">>, <<"age">>],
    ?assertEqual(ok, erlmcp_schema_validator:validate_required(Object, Required)).

test_validate_required_missing_one() ->
    Object = #{<<"name">> => <<"John">>},
    Required = [<<"name">>, <<"age">>],
    {error, Missing} = erlmcp_schema_validator:validate_required(Object, Required),
    ?assertEqual([<<"age">>], Missing).

test_validate_required_missing_multiple() ->
    Object = #{<<"name">> => <<"John">>},
    Required = [<<"name">>, <<"age">>, <<"email">>],
    {error, Missing} = erlmcp_schema_validator:validate_required(Object, Required),
    ?assert(lists:member(<<"age">>, Missing)),
    ?assert(lists:member(<<"email">>, Missing)),
    ?assertNot(lists:member(<<"name">>, Missing)).

test_validate_required_empty() ->
    Object = #{<<"name">> => <<"John">>},
    Required = [],
    ?assertEqual(ok, erlmcp_schema_validator:validate_required(Object, Required)).

%%%====================================================================
%%% Properties Validation Tests
%%%====================================================================

properties_validation_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          {"validate properties all valid", fun test_validate_properties_all_valid/0},
          {"validate properties one invalid", fun test_validate_properties_one_invalid/0},
          {"validate properties nested", fun test_validate_properties_nested/0},
          {"validate properties missing optional", fun test_validate_properties_missing_optional/0}
         ]
     end}.

test_validate_properties_all_valid() ->
    Object = #{<<"name">> => <<"John">>, <<"age">> => 30},
    PropertySchemas = #{
        <<"name">> => #{<<"type">> => <<"string">>},
        <<"age">> => #{<<"type">> => <<"integer">>}
    },
    ?assertEqual(ok, erlmcp_schema_validator:validate_properties(Object, PropertySchemas)).

test_validate_properties_one_invalid() ->
    Object = #{<<"name">> => <<"John">>, <<"age">> => <<"thirty">>},
    PropertySchemas = #{
        <<"name">> => #{<<"type">> => <<"string">>},
        <<"age">> => #{<<"type">> => <<"integer">>}
    },
    {error, Errors} = erlmcp_schema_validator:validate_properties(Object, PropertySchemas),
    ?assert(length(Errors) > 0).

test_validate_properties_nested() ->
    Object = #{<<"user">> => #{<<"name">> => <<"John">>, <<"age">> => 30}},
    PropertySchemas = #{
        <<"user">> => #{
            <<"type">> => <<"object">>,
            <<"properties">> => #{
                <<"name">> => #{<<"type">> => <<"string">>},
                <<"age">> => #{<<"type">> => <<"integer">>}
            }
        }
    },
    ?assertEqual(ok, erlmcp_schema_validator:validate_properties(Object, PropertySchemas)).

test_validate_properties_missing_optional() ->
    Object = #{<<"name">> => <<"John">>},
    PropertySchemas = #{
        <<"name">> => #{<<"type">> => <<"string">>},
        <<"age">> => #{<<"type">> => <<"integer">>}
    },
    ?assertEqual(ok, erlmcp_schema_validator:validate_properties(Object, PropertySchemas)).
