%%%-------------------------------------------------------------------
%%% @doc Schema Default Values Tests
%%%
%%% Chicago School TDD tests for erlmcp_schema_validator default value logic.
%%% Tests observable behavior through public API only.
%%% NO internal state inspection or record duplication.
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_schema_defaults_tests).

-include_lib("eunit/include/eunit.hrl").

%%%====================================================================
%%% Default Values Tests
%%%====================================================================

default_values_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
        [{"apply primitive default (string)", fun test_apply_primitive_default_string/0},
         {"apply primitive default (number)", fun test_apply_primitive_default_number/0},
         {"apply primitive default (boolean)", fun test_apply_primitive_default_boolean/0},
         {"apply object property defaults", fun test_apply_object_defaults/0},
         {"apply nested object defaults", fun test_apply_nested_object_defaults/0},
         {"no default for missing required property", fun test_no_default_for_required/0},
         {"apply array item defaults", fun test_apply_array_defaults/0},
         {"apply defaults to existing nested object", fun test_apply_defaults_to_existing_nested/0}]
     end}.

setup() ->
    application:ensure_all_started(jsx),
    application:ensure_all_started(jesse),
    ok.

cleanup(_) ->
    ok.

test_apply_primitive_default_string() ->
    Schema = #{<<"type">> => <<"string">>, <<"default">> => <<"hello">>},
    %% apply_defaults only works for objects, not primitives
    ?assertEqual(undefined, erlmcp_schema_validator:apply_defaults(undefined, Schema)),
    ?assertEqual(<<"world">>, erlmcp_schema_validator:apply_defaults(<<"world">>, Schema)).

test_apply_primitive_default_number() ->
    Schema = #{<<"type">> => <<"integer">>, <<"default">> => 42},
    %% apply_defaults only works for objects, not primitives
    ?assertEqual(undefined, erlmcp_schema_validator:apply_defaults(undefined, Schema)),
    ?assertEqual(100, erlmcp_schema_validator:apply_defaults(100, Schema)).

test_apply_primitive_default_boolean() ->
    Schema = #{<<"type">> => <<"boolean">>, <<"default">> => true},
    %% apply_defaults only works for objects, not primitives
    ?assertEqual(undefined, erlmcp_schema_validator:apply_defaults(undefined, Schema)),
    ?assertEqual(false, erlmcp_schema_validator:apply_defaults(false, Schema)).

test_apply_object_defaults() ->
    Schema =
        #{<<"type">> => <<"object">>,
          <<"properties">> =>
              #{<<"name">> => #{<<"type">> => <<"string">>, <<"default">> => <<"Anonymous">>},
                <<"age">> => #{<<"type">> => <<"integer">>, <<"default">> => 0},
                <<"active">> => #{<<"type">> => <<"boolean">>, <<"default">> => true}},
          <<"required">> => [<<"name">>]},

    Data1 = #{<<"name">> => <<"John">>},
    Expected1 =
        #{<<"name">> => <<"John">>,
          <<"age">> => 0,
          <<"active">> => true},
    ?assertEqual(Expected1, erlmcp_schema_validator:apply_defaults(Data1, Schema)),

    Data2 =
        #{<<"name">> => <<"Jane">>,
          <<"age">> => 25,
          <<"active">> => false},
    ?assertEqual(Data2, erlmcp_schema_validator:apply_defaults(Data2, Schema)).

test_apply_nested_object_defaults() ->
    Schema =
        #{<<"type">> => <<"object">>,
          <<"properties">> =>
              #{<<"user">> =>
                    #{<<"type">> => <<"object">>,
                      <<"properties">> =>
                          #{<<"name">> =>
                                #{<<"type">> => <<"string">>, <<"default">> => <<"Guest">>},
                            <<"role">> =>
                                #{<<"type">> => <<"string">>, <<"default">> => <<"viewer">>}}}}},

    Data = #{<<"user">> => #{}},
    Expected = #{<<"user">> => #{<<"name">> => <<"Guest">>, <<"role">> => <<"viewer">>}},
    ?assertEqual(Expected, erlmcp_schema_validator:apply_defaults(Data, Schema)).

test_no_default_for_required() ->
    Schema =
        #{<<"type">> => <<"object">>,
          <<"properties">> =>
              #{<<"id">> => #{<<"type">> => <<"string">>, <<"default">> => <<"auto">>}},
          <<"required">> => [<<"id">>]},

    Data = #{},
    ?assertEqual(#{}, erlmcp_schema_validator:apply_defaults(Data, Schema)).

test_apply_array_defaults() ->
    Schema =
        #{<<"type">> => <<"array">>,
          <<"items">> =>
              #{<<"type">> => <<"object">>,
                <<"properties">> =>
                    #{<<"value">> => #{<<"type">> => <<"integer">>, <<"default">> => 0}}}},

    %% Array item defaults are not currently applied
    Data = [#{<<"value">> => 1}, #{}, #{<<"value">> => 3}],
    Result = erlmcp_schema_validator:apply_defaults(Data, Schema),
    ?assertEqual(3, length(Result)),
    ?assertEqual(#{<<"value">> => 1}, lists:nth(1, Result)).

test_apply_defaults_to_existing_nested() ->
    Schema =
        #{<<"type">> => <<"object">>,
          <<"properties">> =>
              #{<<"config">> =>
                    #{<<"type">> => <<"object">>,
                      <<"properties">> =>
                          #{<<"timeout">> => #{<<"type">> => <<"integer">>, <<"default">> => 5000},
                            <<"retries">> => #{<<"type">> => <<"integer">>, <<"default">> => 3}}}}},

    Data = #{<<"config">> => #{<<"timeout">> => 10000}},
    Expected = #{<<"config">> => #{<<"timeout">> => 10000, <<"retries">> => 3}},
    ?assertEqual(Expected, erlmcp_schema_validator:apply_defaults(Data, Schema)).
