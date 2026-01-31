-module(erlmcp_tool_encoding_tests).
-include_lib("eunit/include/eunit.hrl").
-include("erlmcp.hrl").

%%====================================================================
%% Test Suite for erlmcp_tool Encoding/Decoding
%% Chicago School TDD - Test API boundaries, no state inspection
%%====================================================================

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
%% Edge Cases
%%====================================================================

edge_cases_test_() ->
    [
        ?_test(test_unicode_tool_roundtrip()),
        ?_test(test_nested_schema_roundtrip())
    ].

test_unicode_tool_roundtrip() ->
    Original = #mcp_tool{
        name = <<"天気"/utf8>>,
        description = <<"天気予報ツール"/utf8>>,
        input_schema = undefined
    },
    Encoded = erlmcp_tool:encode_tool(Original),
    Decoded = erlmcp_tool:decode_tool(Encoded),
    ?assertEqual(Original, Decoded).

test_nested_schema_roundtrip() ->
    ComplexSchema = #{
        type => <<"object">>,
        properties => #{
            nested => #{
                type => <<"object">>,
                properties => #{
                    deep => #{type => <<"string">>}
                }
            }
        }
    },
    Original = #mcp_tool{
        name = <<"nested_tool">>,
        description = <<"Tool with nested schema">>,
        input_schema = ComplexSchema
    },
    Encoded = erlmcp_tool:encode_tool(Original),
    Decoded = erlmcp_tool:decode_tool(Encoded),
    ?assertEqual(Original, Decoded).
