-module(erlmcp_resource_integration_tests).

-include_lib("eunit/include/eunit.hrl").

-include("erlmcp.hrl").

%%====================================================================
%% Test Suite for erlmcp_resource Integration Workflows
%% Chicago School TDD - Real processes, API boundaries only
%%====================================================================

%%====================================================================
%% Integration Tests
%%====================================================================

integration_test_() ->
    [?_test(test_complete_resource_workflow()),
     ?_test(test_multiple_resources()),
     ?_test(test_resource_template_workflow())].

test_complete_resource_workflow() ->
    %% Create a resource
    Resource =
        #mcp_resource{uri = <<"file:///documents/report.pdf">>,
                      name = <<"Annual Report">>,
                      description = <<"Company annual report 2024">>,
                      mime_type = <<"application/pdf">>,
                      metadata = #{year => 2024, department => <<"Finance">>}},

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
    Resources =
        [#mcp_resource{uri = <<"file:///doc1.txt">>, name = <<"Doc 1">>},
         #mcp_resource{uri = <<"file:///doc2.txt">>, name = <<"Doc 2">>},
         #mcp_resource{uri = <<"file:///doc3.txt">>, name = <<"Doc 3">>}],

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
    Template =
        #mcp_resource_template{uri_template = <<"file:///users/{userId}/documents/{docId}">>,
                               name = <<"User Document Template">>,
                               description = <<"Template for user-specific documents">>,
                               mime_type = <<"text/plain">>},

    %% Validate it
    ?assertEqual(ok, erlmcp_resource:validate_resource_template(Template)).

%%====================================================================
%% End-to-End Tests
%%====================================================================

end_to_end_test_() ->
    [?_test(test_long_description_workflow()), ?_test(test_batch_processing_workflow())].

test_long_description_workflow() ->
    LongDescription = binary:copy(<<"Long description text. ">>, 1000),
    Resource =
        #mcp_resource{uri = <<"file:///long.txt">>,
                      name = <<"Long Description Resource">>,
                      description = LongDescription,
                      mime_type = <<"text/plain">>},
    ?assertEqual(ok, erlmcp_resource:validate_resource(Resource)),
    Encoded = erlmcp_resource:encode_resource(Resource),
    Decoded = erlmcp_resource:decode_resource(Encoded),
    ?assertEqual(Resource, Decoded).

test_batch_processing_workflow() ->
    %% Simulate batch processing of resources
    ResourceList =
        [#mcp_resource{uri = <<"file:///batch/", (integer_to_binary(I))/binary, ".txt">>,
                       name = <<"Batch File ", (integer_to_binary(I))/binary>>,
                       description = <<"Batch item ", (integer_to_binary(I))/binary>>}
         || I <- lists:seq(1, 100)],

    %% Validate all
    ?assertEqual(100, length(ResourceList)),
    Validated = [erlmcp_resource:validate_resource(R) || R <- ResourceList],
    ?assert(lists:all(fun(R) -> R =:= ok end, Validated)),

    %% Encode all
    EncodedList = [erlmcp_resource:encode_resource(R) || R <- ResourceList],
    ?assertEqual(100, length(EncodedList)),

    %% Decode all
    DecodedList = [erlmcp_resource:decode_resource(E) || E <- EncodedList],
    ?assertEqual(ResourceList, DecodedList).
