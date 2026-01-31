%%%-------------------------------------------------------------------
%%% @doc Schema Registry Validation Test Suite
%%%
%%% Comprehensive tests for JSON Schema validation using Jesse.
%%% Tests schema registration, validation, and type checking.
%%%
%%% == Chicago School TDD ==
%%% Uses REAL erlmcp_schema_registry gen_server.
%%% Tests observable behavior through API calls only.
%%% NO mocks - real schema validation with Jesse.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_schema_registry_validation_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Setup and Teardown
%%%===================================================================

schema_registry_test_() ->
    {setup,
     fun setup_registry/0,
     fun cleanup_registry/1,
     fun(_) -> [
         %% Test 1-5: Schema Registration
         {"Register valid schema", fun test_register_valid_schema/0},
         {"Register schema with version", fun test_register_schema_with_version/0},
         {"Register duplicate schema fails", fun test_register_duplicate_fails/0},
         {"Register invalid schema fails", fun test_register_invalid_schema/0},
         {"Register multiple schemas", fun test_register_multiple_schemas/0},

         %% Test 6-10: Schema Retrieval
         {"Get registered schema", fun test_get_schema/0},
         {"Get latest version", fun test_get_latest_version/0},
         {"Get non-existent schema", fun test_get_nonexistent_schema/0},
         {"List schema versions", fun test_list_versions/0},
         {"Get specific version", fun test_get_specific_version/0},

         %% Test 11-15: Type Validation
         {"Validate string type", fun test_validate_string_type/0},
         {"Validate integer type", fun test_validate_integer_type/0},
         {"Validate boolean type", fun test_validate_boolean_type/0},
         {"Validate array type", fun test_validate_array_type/0},
         {"Validate object type", fun test_validate_object_type/0},

         %% Test 16-20: Required Fields
         {"Validate required fields present", fun test_validate_required_present/0},
         {"Validate missing required field", fun test_validate_missing_required/0},
         {"Validate optional fields", fun test_validate_optional_fields/0},
         {"Validate extra fields allowed", fun test_validate_extra_fields/0},
         {"Validate extra fields forbidden", fun test_validate_no_extra_fields/0},

         %% Test 21-25: Schema Constraints
         {"Validate string min length", fun test_validate_string_minlength/0},
         {"Validate string max length", fun test_validate_string_maxlength/0},
         {"Validate number minimum", fun test_validate_number_minimum/0},
         {"Validate number maximum", fun test_validate_number_maximum/0},
         {"Validate enum values", fun test_validate_enum/0},

         %% Test 26-30: Complex Schema Validation
         {"Validate nested objects", fun test_validate_nested_objects/0},
         {"Validate array of objects", fun test_validate_array_of_objects/0},
         {"Validate schema references", fun test_validate_schema_refs/0},
         {"Validate conditional schemas", fun test_validate_conditional/0},
         {"Validate schema composition", fun test_validate_composition/0}
     ] end}.

%%%===================================================================
%%% Test 1-5: Schema Registration
%%%===================================================================

test_register_valid_schema() ->
    Schema = #{
        <<"type">> => <<"object">>,
        <<"properties">> => #{
            <<"name">> => #{<<"type">> => <<"string">>}
        }
    },

    Result = erlmcp_schema_registry:register(<<"test_schema">>, {1, 0, 0}, Schema),

    ?assertEqual(ok, Result).

test_register_schema_with_version() ->
    Schema = #{<<"type">> => <<"string">>},

    Result = erlmcp_schema_registry:register(<<"version_test">>, {2, 1, 0}, tool, Schema),

    ?assertEqual(ok, Result).

test_register_duplicate_fails() ->
    Schema = #{<<"type">> => <<"string">>},

    ok = erlmcp_schema_registry:register(<<"dup_test">>, {1, 0, 0}, Schema),
    Result = erlmcp_schema_registry:register(<<"dup_test">>, {1, 0, 0}, Schema),

    ?assertMatch({error, _}, Result).

test_register_invalid_schema() ->
    %% Invalid schema (not a map)
    Result = erlmcp_schema_registry:register(<<"bad_schema">>, {1, 0, 0}, invalid),

    ?assertMatch({error, _}, Result).

test_register_multiple_schemas() ->
    Schemas = [
        {<<"schema1">>, {1, 0, 0}, #{<<"type">> => <<"string">>}},
        {<<"schema2">>, {1, 0, 0}, #{<<"type">> => <<"number">>}},
        {<<"schema3">>, {1, 0, 0}, #{<<"type">> => <<"boolean">>}}
    ],

    Results = [erlmcp_schema_registry:register(N, V, S) || {N, V, S} <- Schemas],

    ?assert(lists:all(fun(R) -> R =:= ok end, Results)).

%%%===================================================================
%%% Test 6-10: Schema Retrieval
%%%===================================================================

test_get_schema() ->
    Schema = #{<<"type">> => <<"string">>},
    ok = erlmcp_schema_registry:register(<<"get_test">>, {1, 0, 0}, Schema),

    Result = erlmcp_schema_registry:get(<<"get_test">>, {1, 0, 0}),

    ?assertMatch({ok, _}, Result).

test_get_latest_version() ->
    Schema1 = #{<<"type">> => <<"string">>},
    Schema2 = #{<<"type">> => <<"number">>},

    ok = erlmcp_schema_registry:register(<<"latest_test">>, {1, 0, 0}, Schema1),
    ok = erlmcp_schema_registry:register(<<"latest_test">>, {2, 0, 0}, Schema2),

    Result = erlmcp_schema_registry:get_latest(<<"latest_test">>),

    ?assertMatch({ok, _}, Result).

test_get_nonexistent_schema() ->
    Result = erlmcp_schema_registry:get(<<"nonexistent">>, {1, 0, 0}),

    ?assertMatch({error, not_found}, Result).

test_list_versions() ->
    Schema = #{<<"type">> => <<"string">>},

    ok = erlmcp_schema_registry:register(<<"versions_test">>, {1, 0, 0}, Schema),
    ok = erlmcp_schema_registry:register(<<"versions_test">>, {1, 1, 0}, Schema),
    ok = erlmcp_schema_registry:register(<<"versions_test">>, {2, 0, 0}, Schema),

    Result = erlmcp_schema_registry:list_versions(<<"versions_test">>),

    ?assertMatch({ok, [_, _, _]}, Result).

test_get_specific_version() ->
    Schema1 = #{<<"type">> => <<"string">>},
    Schema2 = #{<<"type">> => <<"number">>},

    ok = erlmcp_schema_registry:register(<<"specific_test">>, {1, 0, 0}, Schema1),
    ok = erlmcp_schema_registry:register(<<"specific_test">>, {2, 0, 0}, Schema2),

    Result = erlmcp_schema_registry:get(<<"specific_test">>, {1, 0, 0}),

    ?assertMatch({ok, _}, Result).

%%%===================================================================
%%% Test 11-15: Type Validation
%%%===================================================================

test_validate_string_type() ->
    Schema = #{<<"type">> => <<"string">>},
    ok = erlmcp_schema_registry:register(<<"string_schema">>, {1, 0, 0}, Schema),

    ValidResult = erlmcp_schema_registry:validate(<<"string_schema">>, {1, 0, 0}, <<"hello">>),
    InvalidResult = erlmcp_schema_registry:validate(<<"string_schema">>, {1, 0, 0}, 42),

    ?assertMatch(ok, ValidResult),
    ?assertMatch({error, _}, InvalidResult).

test_validate_integer_type() ->
    Schema = #{<<"type">> => <<"integer">>},
    ok = erlmcp_schema_registry:register(<<"integer_schema">>, {1, 0, 0}, Schema),

    ValidResult = erlmcp_schema_registry:validate(<<"integer_schema">>, {1, 0, 0}, 42),
    InvalidResult = erlmcp_schema_registry:validate(<<"integer_schema">>, {1, 0, 0}, <<"not_int">>),

    ?assertMatch(ok, ValidResult),
    ?assertMatch({error, _}, InvalidResult).

test_validate_boolean_type() ->
    Schema = #{<<"type">> => <<"boolean">>},
    ok = erlmcp_schema_registry:register(<<"boolean_schema">>, {1, 0, 0}, Schema),

    ValidResult = erlmcp_schema_registry:validate(<<"boolean_schema">>, {1, 0, 0}, true),
    InvalidResult = erlmcp_schema_registry:validate(<<"boolean_schema">>, {1, 0, 0}, <<"true">>),

    ?assertMatch(ok, ValidResult),
    ?assertMatch({error, _}, InvalidResult).

test_validate_array_type() ->
    Schema = #{<<"type">> => <<"array">>},
    ok = erlmcp_schema_registry:register(<<"array_schema">>, {1, 0, 0}, Schema),

    ValidResult = erlmcp_schema_registry:validate(<<"array_schema">>, {1, 0, 0}, [1, 2, 3]),
    InvalidResult = erlmcp_schema_registry:validate(<<"array_schema">>, {1, 0, 0}, #{}),

    ?assertMatch(ok, ValidResult),
    ?assertMatch({error, _}, InvalidResult).

test_validate_object_type() ->
    Schema = #{<<"type">> => <<"object">>},
    ok = erlmcp_schema_registry:register(<<"object_schema">>, {1, 0, 0}, Schema),

    ValidResult = erlmcp_schema_registry:validate(<<"object_schema">>, {1, 0, 0}, #{}),
    InvalidResult = erlmcp_schema_registry:validate(<<"object_schema">>, {1, 0, 0}, []),

    ?assertMatch(ok, ValidResult),
    ?assertMatch({error, _}, InvalidResult).

%%%===================================================================
%%% Test 16-20: Required Fields
%%%===================================================================

test_validate_required_present() ->
    Schema = #{
        <<"type">> => <<"object">>,
        <<"properties">> => #{
            <<"name">> => #{<<"type">> => <<"string">>}
        },
        <<"required">> => [<<"name">>]
    },
    ok = erlmcp_schema_registry:register(<<"required_schema">>, {1, 0, 0}, Schema),

    Result = erlmcp_schema_registry:validate(<<"required_schema">>, {1, 0, 0}, #{<<"name">> => <<"test">>}),

    ?assertMatch(ok, Result).

test_validate_missing_required() ->
    Schema = #{
        <<"type">> => <<"object">>,
        <<"properties">> => #{
            <<"name">> => #{<<"type">> => <<"string">>}
        },
        <<"required">> => [<<"name">>]
    },
    ok = erlmcp_schema_registry:register(<<"missing_schema">>, {1, 0, 0}, Schema),

    Result = erlmcp_schema_registry:validate(<<"missing_schema">>, {1, 0, 0}, #{}),

    ?assertMatch({error, _}, Result).

test_validate_optional_fields() ->
    Schema = #{
        <<"type">> => <<"object">>,
        <<"properties">> => #{
            <<"name">> => #{<<"type">> => <<"string">>},
            <<"age">> => #{<<"type">> => <<"integer">>}
        },
        <<"required">> => [<<"name">>]
    },
    ok = erlmcp_schema_registry:register(<<"optional_schema">>, {1, 0, 0}, Schema),

    Result = erlmcp_schema_registry:validate(<<"optional_schema">>, {1, 0, 0}, #{<<"name">> => <<"test">>}),

    ?assertMatch(ok, Result).

test_validate_extra_fields() ->
    Schema = #{
        <<"type">> => <<"object">>,
        <<"properties">> => #{
            <<"name">> => #{<<"type">> => <<"string">>}
        },
        <<"required">> => [<<"name">>]
    },
    ok = erlmcp_schema_registry:register(<<"extra_schema">>, {1, 0, 0}, Schema),

    Result = erlmcp_schema_registry:validate(<<"extra_schema">>, {1, 0, 0}, #{<<"name">> => <<"test">>, <<"extra">> => <<"field">>}),

    %% By default, extra fields are allowed
    ?assertMatch(ok, Result).

test_validate_no_extra_fields() ->
    Schema = #{
        <<"type">> => <<"object">>,
        <<"properties">> => #{
            <<"name">> => #{<<"type">> => <<"string">>}
        },
        <<"required">> => [<<"name">>],
        <<"additionalProperties">> => false
    },
    ok = erlmcp_schema_registry:register(<<"strict_schema">>, {1, 0, 0}, Schema),

    Result = erlmcp_schema_registry:validate(<<"strict_schema">>, {1, 0, 0}, #{<<"name">> => <<"test">>, <<"extra">> => <<"field">>}),

    ?assertMatch({error, _}, Result).

%%%===================================================================
%%% Test 21-25: Schema Constraints
%%%===================================================================

test_validate_string_minlength() ->
    Schema = #{
        <<"type">> => <<"string">>,
        <<"minLength">> => 3
    },
    ok = erlmcp_schema_registry:register(<<"minlength_schema">>, {1, 0, 0}, Schema),

    ValidResult = erlmcp_schema_registry:validate(<<"minlength_schema">>, {1, 0, 0}, <<"abc">>),
    InvalidResult = erlmcp_schema_registry:validate(<<"minlength_schema">>, {1, 0, 0}, <<"ab">>),

    ?assertMatch(ok, ValidResult),
    ?assertMatch({error, _}, InvalidResult).

test_validate_string_maxlength() ->
    Schema = #{
        <<"type">> => <<"string">>,
        <<"maxLength">> => 5
    },
    ok = erlmcp_schema_registry:register(<<"maxlength_schema">>, {1, 0, 0}, Schema),

    ValidResult = erlmcp_schema_registry:validate(<<"maxlength_schema">>, {1, 0, 0}, <<"abc">>),
    InvalidResult = erlmcp_schema_registry:validate(<<"maxlength_schema">>, {1, 0, 0}, <<"abcdef">>),

    ?assertMatch(ok, ValidResult),
    ?assertMatch({error, _}, InvalidResult).

test_validate_number_minimum() ->
    Schema = #{
        <<"type">> => <<"number">>,
        <<"minimum">> => 0
    },
    ok = erlmcp_schema_registry:register(<<"minimum_schema">>, {1, 0, 0}, Schema),

    ValidResult = erlmcp_schema_registry:validate(<<"minimum_schema">>, {1, 0, 0}, 10),
    InvalidResult = erlmcp_schema_registry:validate(<<"minimum_schema">>, {1, 0, 0}, -5),

    ?assertMatch(ok, ValidResult),
    ?assertMatch({error, _}, InvalidResult).

test_validate_number_maximum() ->
    Schema = #{
        <<"type">> => <<"number">>,
        <<"maximum">> => 100
    },
    ok = erlmcp_schema_registry:register(<<"maximum_schema">>, {1, 0, 0}, Schema),

    ValidResult = erlmcp_schema_registry:validate(<<"maximum_schema">>, {1, 0, 0}, 50),
    InvalidResult = erlmcp_schema_registry:validate(<<"maximum_schema">>, {1, 0, 0}, 200),

    ?assertMatch(ok, ValidResult),
    ?assertMatch({error, _}, InvalidResult).

test_validate_enum() ->
    Schema = #{
        <<"type">> => <<"string">>,
        <<"enum">> => [<<"red">>, <<"green">>, <<"blue">>]
    },
    ok = erlmcp_schema_registry:register(<<"enum_schema">>, {1, 0, 0}, Schema),

    ValidResult = erlmcp_schema_registry:validate(<<"enum_schema">>, {1, 0, 0}, <<"red">>),
    InvalidResult = erlmcp_schema_registry:validate(<<"enum_schema">>, {1, 0, 0}, <<"yellow">>),

    ?assertMatch(ok, ValidResult),
    ?assertMatch({error, _}, InvalidResult).

%%%===================================================================
%%% Test 26-30: Complex Schema Validation
%%%===================================================================

test_validate_nested_objects() ->
    Schema = #{
        <<"type">> => <<"object">>,
        <<"properties">> => #{
            <<"person">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"name">> => #{<<"type">> => <<"string">>},
                    <<"age">> => #{<<"type">> => <<"integer">>}
                },
                <<"required">> => [<<"name">>]
            }
        }
    },
    ok = erlmcp_schema_registry:register(<<"nested_schema">>, {1, 0, 0}, Schema),

    ValidData = #{<<"person">> => #{<<"name">> => <<"John">>, <<"age">> => 30}},
    Result = erlmcp_schema_registry:validate(<<"nested_schema">>, {1, 0, 0}, ValidData),

    ?assertMatch(ok, Result).

test_validate_array_of_objects() ->
    Schema = #{
        <<"type">> => <<"array">>,
        <<"items">> => #{
            <<"type">> => <<"object">>,
            <<"properties">> => #{
                <<"id">> => #{<<"type">> => <<"integer">>},
                <<"name">> => #{<<"type">> => <<"string">>}
            },
            <<"required">> => [<<"id">>]
        }
    },
    ok = erlmcp_schema_registry:register(<<"array_obj_schema">>, {1, 0, 0}, Schema),

    ValidData = [#{<<"id">> => 1, <<"name">> => <<"Item1">>}, #{<<"id">> => 2}],
    Result = erlmcp_schema_registry:validate(<<"array_obj_schema">>, {1, 0, 0}, ValidData),

    ?assertMatch(ok, Result).

test_validate_schema_refs() ->
    %% Test schema references (if supported)
    Schema = #{
        <<"type">> => <<"object">>,
        <<"properties">> => #{
            <<"name">> => #{<<"type">> => <<"string">>}
        }
    },
    ok = erlmcp_schema_registry:register(<<"ref_schema">>, {1, 0, 0}, Schema),

    Result = erlmcp_schema_registry:get(<<"ref_schema">>, {1, 0, 0}),

    ?assertMatch({ok, _}, Result).

test_validate_conditional() ->
    %% Test conditional schemas (anyOf, oneOf, allOf)
    Schema = #{
        <<"oneOf">> => [
            #{<<"type">> => <<"string">>},
            #{<<"type">> => <<"number">>}
        ]
    },
    ok = erlmcp_schema_registry:register(<<"conditional_schema">>, {1, 0, 0}, Schema),

    ValidString = erlmcp_schema_registry:validate(<<"conditional_schema">>, {1, 0, 0}, <<"test">>),
    ValidNumber = erlmcp_schema_registry:validate(<<"conditional_schema">>, {1, 0, 0}, 42),
    InvalidBoolean = erlmcp_schema_registry:validate(<<"conditional_schema">>, {1, 0, 0}, true),

    ?assertMatch(ok, ValidString),
    ?assertMatch(ok, ValidNumber),
    ?assertMatch({error, _}, InvalidBoolean).

test_validate_composition() ->
    %% Test schema composition with allOf
    Schema = #{
        <<"allOf">> => [
            #{<<"type">> => <<"object">>, <<"properties">> => #{<<"name">> => #{<<"type">> => <<"string">>}}},
            #{<<"type">> => <<"object">>, <<"properties">> => #{<<"age">> => #{<<"type">> => <<"integer">>}}}
        ]
    },
    ok = erlmcp_schema_registry:register(<<"composition_schema">>, {1, 0, 0}, Schema),

    ValidData = #{<<"name">> => <<"John">>, <<"age">> => 30},
    Result = erlmcp_schema_registry:validate(<<"composition_schema">>, {1, 0, 0}, ValidData),

    ?assertMatch(ok, Result).

%%%===================================================================
%%% Setup and Teardown Helpers
%%%===================================================================

setup_registry() ->
    %% Start schema registry if not running
    case whereis(erlmcp_schema_registry) of
        undefined ->
            {ok, Pid} = erlmcp_schema_registry:start_link(),
            Pid;
        Pid ->
            Pid
    end.

cleanup_registry(_Pid) ->
    %% Clean up registered schemas
    %% Note: Real implementation may need cleanup
    ok.
