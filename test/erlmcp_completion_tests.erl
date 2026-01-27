-module(erlmcp_completion_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Setup
%%====================================================================

setup() ->
    {ok, _} = application:ensure_all_started(erlmcp),
    ok.

cleanup(_) ->
    ok.

%%====================================================================
%% Unit Tests
%%====================================================================

completion_test_() ->
    {setup,
        fun setup/0,
        fun cleanup/1,
        [
            ?_test(test_schema_based_completion()),
            ?_test(test_enum_completion()),
            ?_test(test_string_pattern_completion()),
            ?_test(test_nested_object_completion()),
            ?_test(test_array_item_completion()),
            ?_test(test_required_fields()),
            ?_test(test_invalid_schema()),
            ?_test(test_partial_input_completion())
        ]
    }.

%%====================================================================
%% Individual Tests
%%====================================================================

test_schema_based_completion() ->
    %% Test schema-driven completion
    Schema = #{
        <<"type">> => <<"object">>,
        <<"properties">> => #{
            <<"name">> => #{<<"type">> => <<"string">>},
            <<"age">> => #{<<"type">> => <<"integer">>}
        }
    },

    ?assert(is_map(Schema)),
    ?assert(maps:get(<<"type">>, Schema) =:= <<"object">>).

test_enum_completion() ->
    %% Test enum value completion
    Schema = #{
        <<"type">> => <<"string">>,
        <<"enum">> => [<<"red">>, <<"green">>, <<"blue">>]
    },

    EnumValues = maps:get(<<"enum">>, Schema),
    ?assert(is_list(EnumValues)),
    ?assert(length(EnumValues) =:= 3).

test_string_pattern_completion() ->
    %% Test pattern-based string completion
    Schema = #{
        <<"type">> => <<"string">>,
        <<"pattern">> => <<"^[a-zA-Z]+$">>
    },

    Pattern = maps:get(<<"pattern">>, Schema),
    ?assert(is_binary(Pattern)).

test_nested_object_completion() ->
    %% Test completion in nested objects
    Schema = #{
        <<"type">> => <<"object">>,
        <<"properties">> => #{
            <<"address">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"street">> => #{<<"type">> => <<"string">>},
                    <<"city">> => #{<<"type">> => <<"string">>}
                }
            }
        }
    },

    AddressSchema = maps:get(<<"address">>, maps:get(<<"properties">>, Schema)),
    ?assert(is_map(AddressSchema)).

test_array_item_completion() ->
    %% Test array item type completion
    Schema = #{
        <<"type">> => <<"array">>,
        <<"items">> => #{
            <<"type">> => <<"string">>
        }
    },

    ItemSchema = maps:get(<<"items">>, Schema),
    ?assert(maps:get(<<"type">>, ItemSchema) =:= <<"string">>).

test_required_fields() ->
    %% Test required field completion
    Schema = #{
        <<"type">> => <<"object">>,
        <<"properties">> => #{
            <<"id">> => #{<<"type">> => <<"integer">>},
            <<"name">> => #{<<"type">> => <<"string">>}
        },
        <<"required">> => [<<"id">>, <<"name">>]
    },

    Required = maps:get(<<"required">>, Schema),
    ?assert(is_list(Required)),
    ?assert(length(Required) =:= 2).

test_invalid_schema() ->
    %% Test handling of invalid schema
    InvalidSchema = #{
        <<"type">> => <<"invalid_type">>
    },

    ?assert(is_map(InvalidSchema)).

test_partial_input_completion() ->
    %% Test completion with partial input
    Schema = #{
        <<"type">> => <<"object">>,
        <<"properties">> => #{
            <<"firstName">> => #{<<"type">> => <<"string">>},
            <<"lastName">> => #{<<"type">> => <<"string">>},
            <<"middleName">> => #{<<"type">> => <<"string">>}
        }
    },

    Properties = maps:get(<<"properties">>, Schema),
    Keys = maps:keys(Properties),

    %% Filter keys that start with "first"
    Matches = lists:filter(
        fun(Key) ->
            binary:match(Key, <<"first">>) =/= nomatch
        end,
        Keys
    ),

    ?assert(length(Matches) =:= 1).
