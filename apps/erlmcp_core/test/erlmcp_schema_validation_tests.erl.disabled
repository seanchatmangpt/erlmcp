%%%-------------------------------------------------------------------
%%% @doc Schema Validation Tests
%%%
%%% Chicago School TDD tests for erlmcp_schema_validator validation logic.
%%% Tests observable behavior through public API only.
%%% NO internal state inspection or record duplication.
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_schema_validation_tests).
-include_lib("eunit/include/eunit.hrl").

%%%====================================================================
%%% Integration Tests with Jesse
%%%====================================================================

schema_validation_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
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
          {"Not in range", fun test_not_in_range/0},
          {"Too many properties", fun test_too_many_properties/0},
          {"Too few properties", fun test_too_few_properties/0},
          {"All schemas not valid", fun test_all_schemas_not_valid/0},
          {"Not multiple of", fun test_not_multiple_of/0},
          {"Not one schema valid", fun test_not_one_schema_valid/0},
          {"More than one schema valid", fun test_more_than_one_schema_valid/0}
         ]
     end}.

setup() ->
    application:ensure_all_started(jsx),
    application:ensure_all_started(jesse),
    ok.

cleanup(_) ->
    ok.

test_missing_required_property() ->
    Schema = #{
        <<"type">> => <<"object">>,
        <<"properties">> => #{
            <<"name">> => #{<<"type">> => <<"string">>},
            <<"age">> => #{<<"type">> => <<"integer">>}
        },
        <<"required">> => [<<"name">>]
    },
    Data = #{<<"age">> => 30},

    {error, Errors} = erlmcp_schema_validator:do_validate(Schema, Data),

    ?assert(length(Errors) > 0),
    Error = lists:nth(1, Errors),
    ?assertEqual([], maps:get(path, Error)),
    ?assertNotEqual(<<>>, maps:get(message, Error)).

test_wrong_type() ->
    Schema = #{
        <<"type">> => <<"object">>,
        <<"properties">> => #{
            <<"age">> => #{<<"type">> => <<"integer">>}
        }
    },
    Data = #{<<"age">> => <<"thirty">>},

    {error, Errors} = erlmcp_schema_validator:do_validate(Schema, Data),

    ?assert(length(Errors) > 0),
    Error = lists:nth(1, Errors),
    ?assertEqual([<<"age">>], maps:get(path, Error)),
    ?assertNotEqual(<<>>, maps:get(message, Error)).

test_not_in_enum() ->
    Schema = #{
        <<"type">> => <<"object">>,
        <<"properties">> => #{
            <<"status">> => #{
                <<"type">> => <<"string">>,
                <<"enum">> => [<<"active">>, <<"inactive">>, <<"pending">>]
            }
        }
    },
    Data = #{<<"status">> => <<"deleted">>},

    {error, Errors} = erlmcp_schema_validator:do_validate(Schema, Data),

    ?assert(length(Errors) > 0),
    Error = lists:nth(1, Errors),
    ?assertEqual([<<"status">>], maps:get(path, Error)).

test_not_unique() ->
    Schema = #{
        <<"type">> => <<"array">>,
        <<"items">> => #{<<"type">> => <<"integer">>},
        <<"uniqueItems">> => true
    },
    Data = [1, 2, 3, 2],

    {error, Errors} = erlmcp_schema_validator:do_validate(Schema, Data),

    ?assert(length(Errors) > 0).

test_wrong_length() ->
    Schema = #{
        <<"type">> => <<"string">>,
        <<"minLength">> => 5,
        <<"maxLength">> => 10
    },
    Data = <<"abc">>,

    {error, Errors} = erlmcp_schema_validator:do_validate(Schema, Data),

    ?assert(length(Errors) > 0).

test_wrong_size() ->
    Schema = #{
        <<"type">> => <<"array">>,
        <<"items">> => #{<<"type">> => <<"integer">>},
        <<"minItems">> => 3,
        <<"maxItems">> => 5
    },
    Data = [1, 2],

    {error, Errors} = erlmcp_schema_validator:do_validate(Schema, Data),

    ?assert(length(Errors) > 0).

test_missing_dependency() ->
    Schema = #{
        <<"type">> => <<"object">>,
        <<"properties">> => #{
            <<"credit_card">> => #{<<"type">> => <<"number">>},
            <<"billing_address">> => #{<<"type">> => <<"string">>}
        },
        <<"dependencies">> => #{
            <<"credit_card">> => [<<"billing_address">>]
        }
    },
    Data = #{<<"credit_card">> => 1234567890},

    {error, Errors} = erlmcp_schema_validator:do_validate(Schema, Data),

    ?assert(length(Errors) > 0).

test_no_match() ->
    Schema = #{
        <<"type">> => <<"string">>,
        <<"pattern">> => <<"^[a-z]+$">>
    },
    Data = <<"ABC123">>,

    {error, Errors} = erlmcp_schema_validator:do_validate(Schema, Data),

    ?assert(length(Errors) > 0).

test_no_extra_properties() ->
    Schema = #{
        <<"type">> => <<"object">>,
        <<"properties">> => #{
            <<"name">> => #{<<"type">> => <<"string">>}
        },
        <<"additionalProperties">> => false
    },
    Data = #{<<"name">> => <<"John">>, <<"extra">> => <<"not allowed">>},

    {error, Errors} = erlmcp_schema_validator:do_validate(Schema, Data),

    ?assert(length(Errors) > 0).

test_no_extra_items() ->
    Schema = #{
        <<"type">> => <<"array">>,
        <<"items">> => [#{<<"type">> => <<"string">>}, #{<<"type">> => <<"integer">>}],
        <<"additionalItems">> => false
    },
    Data = [<<"a">>, 1, <<"extra">>],

    {error, Errors} = erlmcp_schema_validator:do_validate(Schema, Data),

    ?assert(length(Errors) > 0).

test_not_in_range() ->
    Schema = #{
        <<"type">> => <<"number">>,
        <<"minimum">> => 10,
        <<"maximum">> => 20
    },
    Data = 25,

    {error, Errors} = erlmcp_schema_validator:do_validate(Schema, Data),

    ?assert(length(Errors) > 0).

test_too_many_properties() ->
    Schema = #{
        <<"type">> => <<"object">>,
        <<"maxProperties">> => 2
    },
    Data = #{<<"a">> => 1, <<"b">> => 2, <<"c">> => 3},

    {error, Errors} = erlmcp_schema_validator:do_validate(Schema, Data),

    ?assert(length(Errors) > 0).

test_too_few_properties() ->
    Schema = #{
        <<"type">> => <<"object">>,
        <<"minProperties">> => 3
    },
    Data = #{<<"a">> => 1, <<"b">> => 2},

    {error, Errors} = erlmcp_schema_validator:do_validate(Schema, Data),

    ?assert(length(Errors) > 0).

test_all_schemas_not_valid() ->
    Schema = #{
        <<"type">> => <<"object">>,
        <<"allOf">> => [
            #{<<"required">> => [<<"a">>]},
            #{<<"required">> => [<<"b">>]},
            #{<<"required">> => [<<"c">>]}
        ]
    },
    Data = #{<<"a">> => 1, <<"b">> => 2},

    {error, Errors} = erlmcp_schema_validator:do_validate(Schema, Data),

    ?assert(length(Errors) > 0).

test_not_multiple_of() ->
    Schema = #{
        <<"type">> => <<"number">>,
        <<"multipleOf">> => 10
    },
    Data = 25,

    {error, Errors} = erlmcp_schema_validator:do_validate(Schema, Data),

    ?assert(length(Errors) > 0).

test_not_one_schema_valid() ->
    Schema = #{
        <<"type">> => <<"object">>,
        <<"oneOf">> => [
            #{<<"required">> => [<<"a">>]},
            #{<<"required">> => [<<"b">>]}
        ]
    },
    Data = #{<<"a">> => 1, <<"b">> => 2},

    {error, Errors} = erlmcp_schema_validator:do_validate(Schema, Data),

    ?assert(length(Errors) > 0).

test_more_than_one_schema_valid() ->
    Schema = #{
        <<"type">> => <<"object">>,
        <<"oneOf">> => [
            #{<<"properties">> => #{<<"a">> => #{<<"type">> => <<"string">>}}},
            #{<<"properties">> => #{<<"b">> => #{<<"type">> => <<"string">>}}}
        ]
    },
    Data = #{<<"a">> => <<"valid">>, <<"b">> => <<"also_valid">>},

    {error, Errors} = erlmcp_schema_validator:do_validate(Schema, Data),

    ?assert(is_list(Errors) orelse Errors =:= ok).

%%%====================================================================
%%% Custom Validator Tests
%%%====================================================================

custom_validator_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          {"validate_regex valid", fun test_validate_regex_valid/0},
          {"validate_regex invalid", fun test_validate_regex_invalid/0},
          {"validate_regex invalid pattern", fun test_validate_regex_invalid_pattern/0},
          {"validate_range in range", fun test_validate_range_in_range/0},
          {"validate_range out of range", fun test_validate_range_out_of_range/0},
          {"validate_dependencies satisfied", fun test_validate_dependencies_satisfied/0},
          {"validate_dependencies missing", fun test_validate_dependencies_missing/0},
          {"validate_dependencies property absent", fun test_validate_dependencies_property_absent/0}
         ]
     end}.

test_validate_regex_valid() ->
    ?assert(erlmcp_schema_validator:validate_regex(<<"^[a-z]+$">>, <<"abc">>)),
    ?assert(erlmcp_schema_validator:validate_regex(<<"^\\d+$">>, <<"123">>)).

test_validate_regex_invalid() ->
    ?assertNot(erlmcp_schema_validator:validate_regex(<<"^[a-z]+$">>, <<"ABC">>)),
    ?assertNot(erlmcp_schema_validator:validate_regex(<<"^\\d+$">>, <<"abc">>)).

test_validate_regex_invalid_pattern() ->
    ?assertNot(erlmcp_schema_validator:validate_regex(<<"[invalid(">>, <<"anything">>)).

test_validate_range_in_range() ->
    ?assert(erlmcp_schema_validator:validate_range(5, 1, 10)),
    ?assert(erlmcp_schema_validator:validate_range(1, 1, 10)),
    ?assert(erlmcp_schema_validator:validate_range(10, 1, 10)).

test_validate_range_out_of_range() ->
    ?assertNot(erlmcp_schema_validator:validate_range(0, 1, 10)),
    ?assertNot(erlmcp_schema_validator:validate_range(11, 1, 10)).

test_validate_dependencies_satisfied() ->
    Data = #{<<"credit_card">> => 1234, <<"billing_address">> => <<"123 Main St">>},
    Dependencies = #{<<"credit_card">> => [<<"billing_address">>]},

    ?assertEqual(ok, erlmcp_schema_validator:validate_dependencies(Data, Dependencies)).

test_validate_dependencies_missing() ->
    Data = #{<<"credit_card">> => 1234},
    Dependencies = #{<<"credit_card">> => [<<"billing_address">>]},

    {error, Errors} = erlmcp_schema_validator:validate_dependencies(Data, Dependencies),
    ?assert(length(Errors) > 0).

test_validate_dependencies_property_absent() ->
    Data = #{<<"other">> => <<"value">>},
    Dependencies = #{<<"credit_card">> => [<<"billing_address">>]},

    ?assertEqual(ok, erlmcp_schema_validator:validate_dependencies(Data, Dependencies)).
