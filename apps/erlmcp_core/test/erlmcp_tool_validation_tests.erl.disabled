-module(erlmcp_tool_validation_tests).
-include_lib("eunit/include/eunit.hrl").
-include("erlmcp.hrl").

%%====================================================================
%% Test Suite for erlmcp_tool Validation
%% Chicago School TDD - Test API boundaries, no state inspection
%%====================================================================

%%====================================================================
%% Tool Name Validation Tests
%%====================================================================

validate_tool_name_test_() ->
    [
        ?_test(test_valid_tool_name()),
        ?_test(test_empty_tool_name()),
        ?_test(test_non_binary_tool_name())
    ].

test_valid_tool_name() ->
    ?assertEqual(ok, erlmcp_tool:validate_tool_name(<<"calculator">>)),
    ?assertEqual(ok, erlmcp_tool:validate_tool_name(<<"weather-api">>)),
    ?assertEqual(ok, erlmcp_tool:validate_tool_name(<<"tool_123">>)).

test_empty_tool_name() ->
    ?assertEqual({error, invalid_tool_name}, erlmcp_tool:validate_tool_name(<<>>)).

test_non_binary_tool_name() ->
    ?assertEqual({error, invalid_tool_name}, erlmcp_tool:validate_tool_name("string")),
    ?assertEqual({error, invalid_tool_name}, erlmcp_tool:validate_tool_name(atom)),
    ?assertEqual({error, invalid_tool_name}, erlmcp_tool:validate_tool_name(123)).

%%====================================================================
%% Tool Description Validation Tests
%%====================================================================

validate_tool_description_test_() ->
    [
        ?_test(test_valid_description()),
        ?_test(test_long_description()),
        ?_test(test_non_binary_description())
    ].

test_valid_description() ->
    ?assertEqual(ok, erlmcp_tool:validate_tool_description(<<"A simple calculator tool">>)),
    ?assertEqual(ok, erlmcp_tool:validate_tool_description(<<"Weather API">>)),
    ?assertEqual(ok, erlmcp_tool:validate_tool_description(<<>>)).

test_long_description() ->
    %% Create a description longer than default max length (10000 chars)
    LongDesc = binary:copy(<<"x">>, 10001),
    Result = erlmcp_tool:validate_tool_description(LongDesc),
    ?assertMatch({error, {description_too_long, _}}, Result).

test_non_binary_description() ->
    ?assertEqual({error, invalid_description}, erlmcp_tool:validate_tool_description("string")),
    ?assertEqual({error, invalid_description}, erlmcp_tool:validate_tool_description(atom)),
    ?assertEqual({error, invalid_description}, erlmcp_tool:validate_tool_description(123)).

%%====================================================================
%% Input Schema Validation Tests
%%====================================================================

validate_input_schema_test_() ->
    [
        ?_test(test_undefined_schema()),
        ?_test(test_valid_schema()),
        ?_test(test_invalid_schema())
    ].

test_undefined_schema() ->
    ?assertEqual(ok, erlmcp_tool:validate_input_schema(undefined)).

test_valid_schema() ->
    Schema = #{
        type => <<"object">>,
        properties => #{
            input => #{type => <<"string">>}
        }
    },
    ?assertEqual(ok, erlmcp_tool:validate_input_schema(Schema)).

test_invalid_schema() ->
    ?assertEqual({error, invalid_input_schema}, erlmcp_tool:validate_input_schema("not a map")),
    ?assertEqual({error, invalid_input_schema}, erlmcp_tool:validate_input_schema([list])),
    ?assertEqual({error, invalid_input_schema}, erlmcp_tool:validate_input_schema(atom)).

%%====================================================================
%% Tool Validation Tests
%%====================================================================

validate_tool_test_() ->
    [
        ?_test(test_valid_tool()),
        ?_test(test_invalid_tool_name_in_record()),
        ?_test(test_invalid_tool_description_in_record())
    ].

test_valid_tool() ->
    Tool = #mcp_tool{
        name = <<"calculator">>,
        description = <<"Performs calculations">>,
        input_schema = undefined
    },
    ?assertEqual(ok, erlmcp_tool:validate_tool(Tool)).

test_invalid_tool_name_in_record() ->
    Tool = #mcp_tool{
        name = <<>>,
        description = <<"Valid description">>
    },
    ?assertEqual({error, invalid_tool_name}, erlmcp_tool:validate_tool(Tool)).

test_invalid_tool_description_in_record() ->
    LongDesc = binary:copy(<<"x">>, 10001),
    Tool = #mcp_tool{
        name = <<"valid_tool">>,
        description = LongDesc
    },
    Result = erlmcp_tool:validate_tool(Tool),
    ?assertMatch({error, {description_too_long, _}}, Result).

%%====================================================================
%% Edge Cases
%%====================================================================

edge_cases_test_() ->
    [
        ?_test(test_unicode_tool_name()),
        ?_test(test_unicode_description()),
        ?_test(test_complex_schema())
    ].

test_unicode_tool_name() ->
    UnicodeName = <<"天気_tool"/utf8>>,
    ?assertEqual(ok, erlmcp_tool:validate_tool_name(UnicodeName)).

test_unicode_description() ->
    UnicodeDesc = <<"This tool provides 天気 information"/utf8>>,
    ?assertEqual(ok, erlmcp_tool:validate_tool_description(UnicodeDesc)).

test_complex_schema() ->
    ComplexSchema = #{
        type => <<"object">>,
        properties => #{
            query => #{
                type => <<"object">>,
                properties => #{
                    field => #{type => <<"string">>},
                    operator => #{type => <<"string">>, enum => [<<"eq">>, <<"ne">>, <<"gt">>, <<"lt">>]},
                    value => #{oneOf => [
                        #{type => <<"string">>},
                        #{type => <<"number">>},
                        #{type => <<"boolean">>}
                    ]}
                },
                required => [<<"field">>, <<"operator">>, <<"value">>]
            },
            options => #{
                type => <<"object">>,
                properties => #{
                    limit => #{type => <<"integer">>, minimum => 1, maximum => 1000},
                    offset => #{type => <<"integer">>, minimum => 0}
                }
            }
        },
        required => [<<"query">>]
    },
    ?assertEqual(ok, erlmcp_tool:validate_input_schema(ComplexSchema)).
