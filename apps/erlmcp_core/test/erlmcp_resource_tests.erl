-module(erlmcp_resource_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("erlmcp/include/erlmcp.hrl").

%%====================================================================
%% Test Suite for erlmcp_resource Module
%% Chicago School TDD - Real resource validation, no mocks
%%====================================================================

%%====================================================================
%% URI Validation Tests
%%====================================================================

validate_uri_test_() ->
    [
        ?_test(test_valid_uri()),
        ?_test(test_empty_uri()),
        ?_test(test_non_binary_uri())
    ].

test_valid_uri() ->
    ?assertEqual(ok, erlmcp_resource:validate_uri(<<"file:///test.txt">>)),
    ?assertEqual(ok, erlmcp_resource:validate_uri(<<"http://example.com/resource">>)),
    ?assertEqual(ok, erlmcp_resource:validate_uri(<<"custom://namespace/item">>)).

test_empty_uri() ->
    ?assertEqual({error, invalid_uri}, erlmcp_resource:validate_uri(<<>>)).

test_non_binary_uri() ->
    ?assertEqual({error, invalid_uri}, erlmcp_resource:validate_uri("string")),
    ?assertEqual({error, invalid_uri}, erlmcp_resource:validate_uri(atom)),
    ?assertEqual({error, invalid_uri}, erlmcp_resource:validate_uri(123)).

%%====================================================================
%% Resource Validation Tests
%%====================================================================

validate_resource_test_() ->
    [
        ?_test(test_valid_resource()),
        ?_test(test_invalid_resource_uri()),
        ?_test(test_invalid_resource_structure())
    ].

test_valid_resource() ->
    Resource = #mcp_resource{
        uri = <<"file:///document.txt">>,
        name = <<"Document">>,
        description = <<"A text document">>,
        mime_type = <<"text/plain">>
    },
    ?assertEqual(ok, erlmcp_resource:validate_resource(Resource)).

test_invalid_resource_uri() ->
    Resource = #mcp_resource{
        uri = <<>>,
        name = <<"Invalid">>,
        description = undefined,
        mime_type = undefined
    },
    ?assertEqual({error, invalid_uri}, erlmcp_resource:validate_resource(Resource)).

test_invalid_resource_structure() ->
    %% Non-binary name
    ?assertEqual({error, invalid_resource},
                 erlmcp_resource:validate_resource(#mcp_resource{uri = <<"file:///test">>, name = atom})).

%%====================================================================
%% Resource Template Validation Tests
%%====================================================================

validate_resource_template_test_() ->
    [
        ?_test(test_valid_template()),
        ?_test(test_invalid_template())
    ].

test_valid_template() ->
    Template = #mcp_resource_template{
        uri_template = <<"file:///{path}">>,
        name = <<"File Template">>,
        description = <<"Dynamic file template">>,
        mime_type = <<"text/plain">>
    },
    ?assertEqual(ok, erlmcp_resource:validate_resource_template(Template)).

test_invalid_template() ->
    %% Non-binary name
    ?assertEqual({error, invalid_resource_template},
                 erlmcp_resource:validate_resource_template(
                     #mcp_resource_template{uri_template = <<"file:///{id}">>, name = 123})).

%%====================================================================
%% Resource Encoding Tests
%%====================================================================

encode_resource_test_() ->
    [
        ?_test(test_encode_minimal_resource()),
        ?_test(test_encode_full_resource()),
        ?_test(test_encode_resource_with_metadata())
    ].

test_encode_minimal_resource() ->
    Resource = #mcp_resource{
        uri = <<"file:///minimal.txt">>,
        name = <<"Minimal">>,
        description = undefined,
        mime_type = undefined,
        metadata = undefined
    },
    Encoded = erlmcp_resource:encode_resource(Resource),
    ?assertMatch(#{<<"uri">> := <<"file:///minimal.txt">>, <<"name">> := <<"Minimal">>}, Encoded),
    ?assertNot(maps:is_key(<<"description">>, Encoded)),
    ?assertNot(maps:is_key(<<"mimeType">>, Encoded)),
    ?assertNot(maps:is_key(<<"metadata">>, Encoded)).

test_encode_full_resource() ->
    Resource = #mcp_resource{
        uri = <<"http://example.com/resource">>,
        name = <<"Full Resource">>,
        description = <<"A complete resource">>,
        mime_type = <<"application/json">>,
        metadata = undefined
    },
    Encoded = erlmcp_resource:encode_resource(Resource),
    ?assertMatch(#{
        <<"uri">> := <<"http://example.com/resource">>,
        <<"name">> := <<"Full Resource">>,
        <<"description">> := <<"A complete resource">>,
        <<"mimeType">> := <<"application/json">>
    }, Encoded).

test_encode_resource_with_metadata() ->
    Metadata = #{author => <<"alice">>, version => <<"1.0">>},
    Resource = #mcp_resource{
        uri = <<"custom://item">>,
        name = <<"Item">>,
        description = <<"With metadata">>,
        mime_type = <<"text/plain">>,
        metadata = Metadata
    },
    Encoded = erlmcp_resource:encode_resource(Resource),
    ?assertMatch(#{
        <<"uri">> := <<"custom://item">>,
        <<"name">> := <<"Item">>,
        <<"description">> := <<"With metadata">>,
        <<"mimeType">> := <<"text/plain">>,
        <<"metadata">> := Metadata
    }, Encoded).

%%====================================================================
%% Resource Decoding Tests
%%====================================================================

decode_resource_test_() ->
    [
        ?_test(test_decode_minimal_resource()),
        ?_test(test_decode_full_resource()),
        ?_test(test_decode_resource_roundtrip())
    ].

test_decode_minimal_resource() ->
    Map = #{
        <<"uri">> => <<"file:///test.txt">>,
        <<"name">> => <<"Test File">>
    },
    Resource = erlmcp_resource:decode_resource(Map),
    ?assertMatch(#mcp_resource{
        uri = <<"file:///test.txt">>,
        name = <<"Test File">>,
        description = undefined,
        mime_type = undefined,
        metadata = undefined
    }, Resource).

test_decode_full_resource() ->
    Metadata = #{key => <<"value">>},
    Map = #{
        <<"uri">> => <<"http://api.example.com/data">>,
        <<"name">> => <<"API Data">>,
        <<"description">> => <<"Data from API">>,
        <<"mimeType">> => <<"application/json">>,
        <<"metadata">> => Metadata
    },
    Resource = erlmcp_resource:decode_resource(Map),
    ?assertMatch(#mcp_resource{
        uri = <<"http://api.example.com/data">>,
        name = <<"API Data">>,
        description = <<"Data from API">>,
        mime_type = <<"application/json">>,
        metadata = Metadata
    }, Resource).

test_decode_resource_roundtrip() ->
    Original = #mcp_resource{
        uri = <<"custom://resource/123">>,
        name = <<"Resource 123">>,
        description = <<"Test resource">>,
        mime_type = <<"text/plain">>,
        metadata = #{id => 123, active => true}
    },
    Encoded = erlmcp_resource:encode_resource(Original),
    Decoded = erlmcp_resource:decode_resource(Encoded),
    ?assertEqual(Original, Decoded).

%%====================================================================
%% Integration Tests
%%====================================================================

integration_test_() ->
    [
        ?_test(test_complete_resource_workflow()),
        ?_test(test_multiple_resources()),
        ?_test(test_resource_template_workflow())
    ].

test_complete_resource_workflow() ->
    %% Create a resource
    Resource = #mcp_resource{
        uri = <<"file:///documents/report.pdf">>,
        name = <<"Annual Report">>,
        description = <<"Company annual report 2024">>,
        mime_type = <<"application/pdf">>,
        metadata = #{year => 2024, department => <<"Finance">>}
    },

    %% Validate it
    ?assertEqual(ok, erlmcp_resource:validate_resource(Resource)),
    ?assertEqual(ok, erlmcp_resource:validate_uri(Resource#mcp_resource.uri)),

    %% Encode it
    Encoded = erlmcp_resource:encode_resource(Resource),
    ?assert(maps:is_key(<<"uri">>, Encoded)),
    ?assert(maps:is_key(<<"name">>, Encoded)),
    ?assert(maps:is_key(<<"description">>, Encoded)),
    ?assert(maps:is_key(<<"mimeType">>, Encoded)),
    ?assert(maps:is_key(<<"metadata">>, Encoded)),

    %% Decode it back
    Decoded = erlmcp_resource:decode_resource(Encoded),
    ?assertEqual(Resource, Decoded).

test_multiple_resources() ->
    Resources = [
        #mcp_resource{uri = <<"file:///doc1.txt">>, name = <<"Doc 1">>},
        #mcp_resource{uri = <<"file:///doc2.txt">>, name = <<"Doc 2">>},
        #mcp_resource{uri = <<"file:///doc3.txt">>, name = <<"Doc 3">>}
    ],

    %% Validate all
    ValidationResults = [erlmcp_resource:validate_resource(R) || R <- Resources],
    ?assert(lists:all(fun(R) -> R =:= ok end, ValidationResults)),

    %% Encode all
    Encoded = [erlmcp_resource:encode_resource(R) || R <- Resources],
    ?assertEqual(3, length(Encoded)),

    %% Decode all
    Decoded = [erlmcp_resource:decode_resource(E) || E <- Encoded],
    ?assertEqual(Resources, Decoded).

test_resource_template_workflow() ->
    %% Create a resource template
    Template = #mcp_resource_template{
        uri_template = <<"file:///users/{userId}/documents/{docId}">>,
        name = <<"User Document Template">>,
        description = <<"Template for user-specific documents">>,
        mime_type = <<"text/plain">>
    },

    %% Validate it
    ?assertEqual(ok, erlmcp_resource:validate_resource_template(Template)).

%%====================================================================
%% Edge Case Tests
%%====================================================================

edge_cases_test_() ->
    [
        ?_test(test_unicode_resource_name()),
        ?_test(test_unicode_uri()),
        ?_test(test_complex_metadata()),
        ?_test(test_long_description())
    ].

test_unicode_resource_name() ->
    Resource = #mcp_resource{
        uri = <<"file:///文書.txt"/utf8>>,
        name = <<"日本語文書"/utf8>>,
        description = undefined,
        mime_type = <<"text/plain">>
    },
    ?assertEqual(ok, erlmcp_resource:validate_resource(Resource)),
    Encoded = erlmcp_resource:encode_resource(Resource),
    Decoded = erlmcp_resource:decode_resource(Encoded),
    ?assertEqual(Resource, Decoded).

test_unicode_uri() ->
    Uri = <<"http://example.com/文書"/utf8>>,
    ?assertEqual(ok, erlmcp_resource:validate_uri(Uri)).

test_complex_metadata() ->
    ComplexMetadata = #{
        nested => #{
            level1 => #{
                level2 => #{
                    level3 => <<"deep value">>
                }
            }
        },
        array => [1, 2, 3, <<"four">>, #{five => 5}],
        mixed => #{
            string => <<"text">>,
            number => 42,
            boolean => true,
            null_value => null
        }
    },
    Resource = #mcp_resource{
        uri = <<"custom://complex">>,
        name = <<"Complex Metadata">>,
        metadata = ComplexMetadata
    },
    ?assertEqual(ok, erlmcp_resource:validate_resource(Resource)),
    Encoded = erlmcp_resource:encode_resource(Resource),
    Decoded = erlmcp_resource:decode_resource(Encoded),
    ?assertEqual(Resource, Decoded).

test_long_description() ->
    LongDescription = binary:copy(<<"Long description text. ">>, 1000),
    Resource = #mcp_resource{
        uri = <<"file:///long.txt">>,
        name = <<"Long Description Resource">>,
        description = LongDescription,
        mime_type = <<"text/plain">>
    },
    ?assertEqual(ok, erlmcp_resource:validate_resource(Resource)),
    Encoded = erlmcp_resource:encode_resource(Resource),
    Decoded = erlmcp_resource:decode_resource(Encoded),
    ?assertEqual(Resource, Decoded).
