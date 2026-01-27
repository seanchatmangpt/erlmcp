-module(erlmcp_annotations_tests).

-include_lib("eunit/include/eunit.hrl").
-include("erlmcp.hrl").

%%====================================================================
%% Test Setup
%%====================================================================

setup() ->
    {ok, _} = application:ensure_all_started(erlmcp),
    ok.

cleanup(_) ->
    ok.

%%====================================================================
%% Test Suite
%%====================================================================

annotation_tests_() ->
    {setup,
        fun setup/0,
        fun cleanup/1,
        [
            ?_test(test_single_annotation()),
            ?_test(test_multiple_annotations()),
            ?_test(test_annotation_encoding()),
            ?_test(test_content_with_text_and_annotations()),
            ?_test(test_content_with_data_and_annotations()),
            ?_test(test_empty_annotations_list()),
            ?_test(test_annotation_with_different_value_types()),
            ?_test(test_resource_content_with_annotations()),
            ?_test(test_annotations_json_serialization()),
            ?_test(test_backward_compatibility_no_annotations()),
            ?_test(test_tool_result_with_annotations()),
            ?_test(test_prompt_result_with_annotations())
        ]
    }.

%%====================================================================
%% Individual Tests
%%====================================================================

test_single_annotation() ->
    Annotation = #mcp_annotation{
        name = <<"audience">>,
        value = <<"user">>
    },
    Annotations = [Annotation],
    Result = erlmcp_server:encode_annotations(Annotations),
    Expected = #{<<"audience">> => <<"user">>},
    ?assertEqual(Expected, Result).

test_multiple_annotations() ->
    Annotations = [
        #mcp_annotation{name = <<"audience">>, value = <<"user">>},
        #mcp_annotation{name = <<"priority">>, value = 0.9},
        #mcp_annotation{name = <<"lastModified">>, value = <<"2026-01-27T12:00:00Z">>}
    ],
    Result = erlmcp_server:encode_annotations(Annotations),
    ?assert(maps:is_key(<<"audience">>, Result)),
    ?assert(maps:is_key(<<"priority">>, Result)),
    ?assert(maps:is_key(<<"lastModified">>, Result)),
    ?assertEqual(<<"user">>, maps:get(<<"audience">>, Result)),
    ?assertEqual(0.9, maps:get(<<"priority">>, Result)).

test_annotation_encoding() ->
    Annotation = #mcp_annotation{
        name = <<"priority">>,
        value = 0.85
    },
    Result = erlmcp_server:encode_annotation(Annotation),
    Expected = #{<<"priority">> => 0.85},
    ?assertEqual(Expected, Result).

test_content_with_text_and_annotations() ->
    Annotations = [
        #mcp_annotation{name = <<"audience">>, value = <<"developer">>}
    ],
    Content = #mcp_content{
        type = ?MCP_CONTENT_TYPE_TEXT,
        text = <<"Hello World">>,
        annotations = Annotations
    },
    Resource = #mcp_resource{
        uri = <<"file:///test.txt">>,
        name = <<"test.txt">>,
        mime_type = ?MCP_MIME_TEXT_PLAIN
    },
    Result = erlmcp_server:encode_content_item(Content, Resource, <<"file:///test.txt">>),

    ?assert(maps:is_key(?MCP_PARAM_TEXT, Result)),
    ?assert(maps:is_key(?MCP_PARAM_TYPE, Result)),
    ?assert(maps:is_key(?MCP_PARAM_ANNOTATIONS, Result)),

    Annotations_In_Result = maps:get(?MCP_PARAM_ANNOTATIONS, Result),
    ?assert(maps:is_key(<<"audience">>, Annotations_In_Result)),
    ?assertEqual(<<"developer">>, maps:get(<<"audience">>, Annotations_In_Result)).

test_content_with_data_and_annotations() ->
    Annotations = [
        #mcp_annotation{name = <<"encoding">>, value = <<"base64">>}
    ],
    Content = #mcp_content{
        type = ?MCP_CONTENT_TYPE_IMAGE,
        data = <<"base64encodeddata">>,
        mime_type = <<"image/png">>,
        annotations = Annotations
    },
    Resource = #mcp_resource{
        uri = <<"file:///image.png">>,
        name = <<"image.png">>,
        mime_type = <<"image/png">>
    },
    Result = erlmcp_server:encode_content_item(Content, Resource, <<"file:///image.png">>),

    ?assert(maps:is_key(?MCP_PARAM_DATA, Result)),
    ?assert(maps:is_key(?MCP_PARAM_ANNOTATIONS, Result)),
    ?assertEqual(<<"base64encodeddata">>, maps:get(?MCP_PARAM_DATA, Result)).

test_empty_annotations_list() ->
    Content = #mcp_content{
        type = ?MCP_CONTENT_TYPE_TEXT,
        text = <<"No annotations">>,
        annotations = []
    },
    Resource = #mcp_resource{
        uri = <<"file:///test.txt">>,
        name = <<"test.txt">>,
        mime_type = ?MCP_MIME_TEXT_PLAIN
    },
    Result = erlmcp_server:encode_content_item(Content, Resource, <<"file:///test.txt">>),

    ?assert(maps:is_key(?MCP_PARAM_TEXT, Result)),
    ?assertNot(maps:is_key(?MCP_PARAM_ANNOTATIONS, Result)).

test_annotation_with_different_value_types() ->
    Annotations = [
        #mcp_annotation{name = <<"string_anno">>, value = <<"text">>},
        #mcp_annotation{name = <<"number_anno">>, value = 42},
        #mcp_annotation{name = <<"float_anno">>, value = 3.14},
        #mcp_annotation{name = <<"boolean_anno">>, value = true},
        #mcp_annotation{name = <<"map_anno">>, value = #{<<"key">> => <<"value">>}}
    ],
    Result = erlmcp_server:encode_annotations(Annotations),

    ?assertEqual(<<"text">>, maps:get(<<"string_anno">>, Result)),
    ?assertEqual(42, maps:get(<<"number_anno">>, Result)),
    ?assertEqual(3.14, maps:get(<<"float_anno">>, Result)),
    ?assertEqual(true, maps:get(<<"boolean_anno">>, Result)),
    ?assertEqual(#{<<"key">> => <<"value">>}, maps:get(<<"map_anno">>, Result)).

test_resource_content_with_annotations() ->
    Annotations = [
        #mcp_annotation{name = <<"resource_type">>, value = <<"database">>},
        #mcp_annotation{name = <<"read_only">>, value = true}
    ],
    Content = #mcp_content{
        type = <<"resource">>,
        text = <<"resource://db/table">>,
        annotations = Annotations
    },
    Resource = #mcp_resource{
        uri = <<"resource://db/table">>,
        name = <<"db.table">>,
        mime_type = <<"application/json">>
    },
    Result = erlmcp_server:encode_content_item(Content, Resource, <<"resource://db/table">>),

    EncodedAnnotations = maps:get(?MCP_PARAM_ANNOTATIONS, Result),
    ?assertEqual(<<"database">>, maps:get(<<"resource_type">>, EncodedAnnotations)),
    ?assertEqual(true, maps:get(<<"read_only">>, EncodedAnnotations)).

test_annotations_json_serialization() ->
    %% Test that annotations can be serialized to JSON
    Annotations = [
        #mcp_annotation{name = <<"audience">>, value = <<"user">>},
        #mcp_annotation{name = <<"priority">>, value = 0.9}
    ],
    Content = #mcp_content{
        type = ?MCP_CONTENT_TYPE_TEXT,
        text = <<"Test content">>,
        annotations = Annotations
    },
    Resource = #mcp_resource{
        uri = <<"test">>,
        name = <<"test">>,
        mime_type = ?MCP_MIME_TEXT_PLAIN
    },
    Result = erlmcp_server:encode_content_item(Content, Resource, <<"test">>),

    %% Should be serializable
    try
        Json = jsx:encode(Result),
        ?assert(is_binary(Json)),
        ?assert(byte_size(Json) > 0),

        %% Should be deserializable
        Decoded = jsx:decode(Json, [return_maps]),
        ?assert(maps:is_key(?MCP_PARAM_ANNOTATIONS, Decoded))
    catch
        Error ->
            ?fail(Error)
    end.

test_backward_compatibility_no_annotations() ->
    %% Test that content without annotations still works
    Content = #mcp_content{
        type = ?MCP_CONTENT_TYPE_TEXT,
        text = <<"Simple content">>
    },
    Resource = #mcp_resource{
        uri = <<"test">>,
        name = <<"test">>,
        mime_type = ?MCP_MIME_TEXT_PLAIN
    },
    Result = erlmcp_server:encode_content_item(Content, Resource, <<"test">>),

    ?assert(maps:is_key(?MCP_PARAM_TEXT, Result)),
    ?assertNot(maps:is_key(?MCP_PARAM_ANNOTATIONS, Result)).

test_tool_result_with_annotations() ->
    %% Test normalize_tool_result with #mcp_content containing annotations
    Annotations = [
        #mcp_annotation{name = <<"tool_output">>, value = <<"success">>}
    ],
    Content = #mcp_content{
        type = ?MCP_CONTENT_TYPE_TEXT,
        text = <<"Tool executed successfully">>,
        annotations = Annotations
    },
    Result = erlmcp_server:normalize_tool_result(Content),

    ?assert(is_list(Result)),
    ?assertEqual(1, length(Result)),

    [Item] = Result,
    ?assert(maps:is_key(?MCP_PARAM_TYPE, Item)),
    ?assert(maps:is_key(?MCP_PARAM_TEXT, Item)),
    ?assert(maps:is_key(?MCP_PARAM_ANNOTATIONS, Item)).

test_prompt_result_with_annotations() ->
    %% Test normalize_prompt_result with #mcp_content containing annotations
    Annotations = [
        #mcp_annotation{name = <<"prompt_type">>, value = <<"system">>}
    ],
    Content = #mcp_content{
        type = ?MCP_CONTENT_TYPE_TEXT,
        text = <<"You are a helpful assistant">>,
        annotations = Annotations
    },
    Result = erlmcp_server:normalize_prompt_result(Content),

    ?assert(is_list(Result)),
    ?assertEqual(1, length(Result)),

    [Message] = Result,
    ?assert(maps:is_key(?MCP_PARAM_ROLE, Message)),
    ?assert(maps:is_key(?MCP_PARAM_CONTENT, Message)),

    ContentMap = maps:get(?MCP_PARAM_CONTENT, Message),
    ?assert(maps:is_key(?MCP_PARAM_ANNOTATIONS, ContentMap)).
