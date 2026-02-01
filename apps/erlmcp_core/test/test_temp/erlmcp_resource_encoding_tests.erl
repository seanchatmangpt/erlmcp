-module(erlmcp_resource_encoding_tests).

-include_lib("eunit/include/eunit.hrl").

-include("erlmcp.hrl").

%%====================================================================
%% Test Suite for erlmcp_resource Encoding/Decoding
%% Chicago School TDD - Test API boundaries, no state inspection
%%====================================================================

%%====================================================================
%% Resource Encoding Tests
%%====================================================================

encode_resource_test_() ->
    [?_test(test_encode_minimal_resource()),
     ?_test(test_encode_full_resource()),
     ?_test(test_encode_resource_with_metadata())].

test_encode_minimal_resource() ->
    Resource =
        #mcp_resource{uri = <<"file:///minimal.txt">>,
                      name = <<"Minimal">>,
                      description = undefined,
                      mime_type = undefined,
                      metadata = undefined},
    Encoded = erlmcp_resource:encode_resource(Resource),
    ?assertMatch(#{<<"uri">> := <<"file:///minimal.txt">>, <<"name">> := <<"Minimal">>}, Encoded),
    ?assertNot(maps:is_key(<<"description">>, Encoded)),
    ?assertNot(maps:is_key(<<"mimeType">>, Encoded)),
    ?assertNot(maps:is_key(<<"metadata">>, Encoded)).

test_encode_full_resource() ->
    Resource =
        #mcp_resource{uri = <<"http://example.com/resource">>,
                      name = <<"Full Resource">>,
                      description = <<"A complete resource">>,
                      mime_type = <<"application/json">>,
                      metadata = undefined},
    Encoded = erlmcp_resource:encode_resource(Resource),
    ?assertMatch(#{<<"uri">> := <<"http://example.com/resource">>,
                   <<"name">> := <<"Full Resource">>,
                   <<"description">> := <<"A complete resource">>,
                   <<"mimeType">> := <<"application/json">>},
                 Encoded).

test_encode_resource_with_metadata() ->
    Metadata = #{author => <<"alice">>, version => <<"1.0">>},
    Resource =
        #mcp_resource{uri = <<"custom://item">>,
                      name = <<"Item">>,
                      description = <<"With metadata">>,
                      mime_type = <<"text/plain">>,
                      metadata = Metadata},
    Encoded = erlmcp_resource:encode_resource(Resource),
    ?assertMatch(#{<<"uri">> := <<"custom://item">>,
                   <<"name">> := <<"Item">>,
                   <<"description">> := <<"With metadata">>,
                   <<"mimeType">> := <<"text/plain">>,
                   <<"metadata">> := Metadata},
                 Encoded).

%%====================================================================
%% Resource Decoding Tests
%%====================================================================

decode_resource_test_() ->
    [?_test(test_decode_minimal_resource()),
     ?_test(test_decode_full_resource()),
     ?_test(test_decode_resource_roundtrip())].

test_decode_minimal_resource() ->
    Map = #{<<"uri">> => <<"file:///test.txt">>, <<"name">> => <<"Test File">>},
    Resource = erlmcp_resource:decode_resource(Map),
    ?assertMatch(#mcp_resource{uri = <<"file:///test.txt">>,
                               name = <<"Test File">>,
                               description = undefined,
                               mime_type = undefined,
                               metadata = undefined},
                 Resource).

test_decode_full_resource() ->
    Metadata = #{key => <<"value">>},
    Map = #{<<"uri">> => <<"http://api.example.com/data">>,
            <<"name">> => <<"API Data">>,
            <<"description">> => <<"Data from API">>,
            <<"mimeType">> => <<"application/json">>,
            <<"metadata">> => Metadata},
    Resource = erlmcp_resource:decode_resource(Map),
    ?assertMatch(#mcp_resource{uri = <<"http://api.example.com/data">>,
                               name = <<"API Data">>,
                               description = <<"Data from API">>,
                               mime_type = <<"application/json">>,
                               metadata = Metadata},
                 Resource).

test_decode_resource_roundtrip() ->
    Original =
        #mcp_resource{uri = <<"custom://resource/123">>,
                      name = <<"Resource 123">>,
                      description = <<"Test resource">>,
                      mime_type = <<"text/plain">>,
                      metadata = #{id => 123, active => true}},
    Encoded = erlmcp_resource:encode_resource(Original),
    Decoded = erlmcp_resource:decode_resource(Encoded),
    ?assertEqual(Original, Decoded).

%%====================================================================
%% Edge Cases
%%====================================================================

edge_cases_test_() ->
    [?_test(test_unicode_resource_name()), ?_test(test_complex_metadata())].

test_unicode_resource_name() ->
    Resource =
        #mcp_resource{uri = <<"file:///文書.txt"/utf8>>,
                      name = <<"日本語文書"/utf8>>,
                      description = undefined,
                      mime_type = <<"text/plain">>},
    Encoded = erlmcp_resource:encode_resource(Resource),
    Decoded = erlmcp_resource:decode_resource(Encoded),
    ?assertEqual(Resource, Decoded).

test_complex_metadata() ->
    ComplexMetadata =
        #{nested => #{level1 => #{level2 => #{level3 => <<"deep value">>}}},
          array => [1, 2, 3, <<"four">>, #{five => 5}],
          mixed =>
              #{string => <<"text">>,
                number => 42,
                boolean => true,
                null_value => null}},
    Resource =
        #mcp_resource{uri = <<"custom://complex">>,
                      name = <<"Complex Metadata">>,
                      metadata = ComplexMetadata},
    Encoded = erlmcp_resource:encode_resource(Resource),
    Decoded = erlmcp_resource:decode_resource(Encoded),
    ?assertEqual(Resource, Decoded).
