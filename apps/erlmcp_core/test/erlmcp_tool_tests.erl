-module(erlmcp_tool_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("erlmcp_core/include/erlmcp.hrl").

%%====================================================================
%% Test Suite for erlmcp_tool Module
%% Chicago School TDD - Real tool validation, no mocks
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
%% Tool Encoding Tests
%%====================================================================

encode_tool_test_() ->
    [
        ?_test(test_encode_basic_tool()),
        ?_test(test_encode_tool_with_schema()),
        ?_test(test_encode_tool_without_schema())
    ].

test_encode_basic_tool() ->
    Tool = #mcp_tool{
        name = <<"calculator">>,
        description = <<"Basic calculator">>,
        input_schema = undefined
    },
    Encoded = erlmcp_tool:encode_tool(Tool),
    ?assertMatch(#{<<"name">> := <<"calculator">>, <<"description">> := <<"Basic calculator">>}, Encoded),
    ?assertNot(maps:is_key(<<"inputSchema">>, Encoded)).

test_encode_tool_with_schema() ->
    Schema = #{
        type => <<"object">>,
        properties => #{value => #{type => <<"number">>}}
    },
    Tool = #mcp_tool{
        name = <<"math_tool">>,
        description = <<"Math operations">>,
        input_schema = Schema
    },
    Encoded = erlmcp_tool:encode_tool(Tool),
    ?assertMatch(#{
        <<"name">> := <<"math_tool">>,
        <<"description">> := <<"Math operations">>,
        <<"inputSchema">> := Schema
    }, Encoded).

test_encode_tool_without_schema() ->
    Tool = #mcp_tool{
        name = <<"simple">>,
        description = <<"Simple tool">>,
        input_schema = undefined
    },
    Encoded = erlmcp_tool:encode_tool(Tool),
    ?assertEqual(2, maps:size(Encoded)),
    ?assertNot(maps:is_key(<<"inputSchema">>, Encoded)).

%%====================================================================
%% Tool Decoding Tests
%%====================================================================

decode_tool_test_() ->
    [
        ?_test(test_decode_basic_tool()),
        ?_test(test_decode_tool_with_schema()),
        ?_test(test_decode_tool_roundtrip())
    ].

test_decode_basic_tool() ->
    Map = #{
        <<"name">> => <<"test_tool">>,
        <<"description">> => <<"Test description">>
    },
    Tool = erlmcp_tool:decode_tool(Map),
    ?assertMatch(#mcp_tool{
        name = <<"test_tool">>,
        description = <<"Test description">>,
        input_schema = undefined
    }, Tool).

test_decode_tool_with_schema() ->
    Schema = #{type => <<"object">>},
    Map = #{
        <<"name">> => <<"schema_tool">>,
        <<"description">> => <<"With schema">>,
        <<"inputSchema">> => Schema
    },
    Tool = erlmcp_tool:decode_tool(Map),
    ?assertMatch(#mcp_tool{
        name = <<"schema_tool">>,
        description = <<"With schema">>,
        input_schema = Schema
    }, Tool).

test_decode_tool_roundtrip() ->
    Original = #mcp_tool{
        name = <<"roundtrip">>,
        description = <<"Roundtrip test">>,
        input_schema = #{type => <<"string">>}
    },
    Encoded = erlmcp_tool:encode_tool(Original),
    Decoded = erlmcp_tool:decode_tool(Encoded),
    ?assertEqual(Original, Decoded).

%%====================================================================
%% Integration Tests
%%====================================================================

integration_test_() ->
    [
        ?_test(test_complete_tool_workflow()),
        ?_test(test_tool_validation_pipeline()),
        ?_test(test_multiple_tools())
    ].

test_complete_tool_workflow() ->
    %% Create a tool
    Tool = #mcp_tool{
        name = <<"weather">>,
        description = <<"Get weather information">>,
        input_schema = #{
            type => <<"object">>,
            properties => #{
                city => #{type => <<"string">>},
                units => #{type => <<"string">>, enum => [<<"C">>, <<"F">>]}
            },
            required => [<<"city">>]
        }
    },

    %% Validate it
    ?assertEqual(ok, erlmcp_tool:validate_tool(Tool)),

    %% Encode it
    Encoded = erlmcp_tool:encode_tool(Tool),
    ?assert(maps:is_key(<<"name">>, Encoded)),
    ?assert(maps:is_key(<<"description">>, Encoded)),
    ?assert(maps:is_key(<<"inputSchema">>, Encoded)),

    %% Decode it back
    Decoded = erlmcp_tool:decode_tool(Encoded),
    ?assertEqual(Tool, Decoded).

test_tool_validation_pipeline() ->
    %% Valid tool
    ValidTool = #mcp_tool{
        name = <<"valid">>,
        description = <<"Valid tool">>,
        input_schema = #{}
    },
    ?assertEqual(ok, erlmcp_tool:validate_tool(ValidTool)),

    %% Invalid name
    InvalidName = #mcp_tool{
        name = <<>>,
        description = <<"Valid description">>
    },
    ?assertMatch({error, invalid_tool_name}, erlmcp_tool:validate_tool(InvalidName)),

    %% Invalid description (too long)
    InvalidDesc = #mcp_tool{
        name = <<"valid">>,
        description = binary:copy(<<"x">>, 10001)
    },
    ?assertMatch({error, {description_too_long, _}}, erlmcp_tool:validate_tool(InvalidDesc)).

test_multiple_tools() ->
    Tools = [
        #mcp_tool{name = <<"tool1">>, description = <<"First tool">>},
        #mcp_tool{name = <<"tool2">>, description = <<"Second tool">>},
        #mcp_tool{name = <<"tool3">>, description = <<"Third tool">>}
    ],

    %% Validate all
    Results = [erlmcp_tool:validate_tool(T) || T <- Tools],
    ?assert(lists:all(fun(R) -> R =:= ok end, Results)),

    %% Encode all
    Encoded = [erlmcp_tool:encode_tool(T) || T <- Tools],
    ?assertEqual(3, length(Encoded)),

    %% Decode all
    Decoded = [erlmcp_tool:decode_tool(E) || E <- Encoded],
    ?assertEqual(Tools, Decoded).

%%====================================================================
%% Edge Case Tests
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
    ?assertEqual(ok, erlmcp_tool:validate_input_schema(ComplexSchema)),

    Tool = #mcp_tool{
        name = <<"complex_tool">>,
        description = <<"Tool with complex schema">>,
        input_schema = ComplexSchema
    },
    ?assertEqual(ok, erlmcp_tool:validate_tool(Tool)),

    %% Roundtrip test
    Encoded = erlmcp_tool:encode_tool(Tool),
    Decoded = erlmcp_tool:decode_tool(Encoded),
    ?assertEqual(Tool, Decoded).
