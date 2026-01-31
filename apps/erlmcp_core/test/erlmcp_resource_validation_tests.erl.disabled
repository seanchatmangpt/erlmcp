-module(erlmcp_resource_validation_tests).
-include_lib("eunit/include/eunit.hrl").
-include("erlmcp.hrl").

%%====================================================================
%% Test Suite for erlmcp_resource Validation
%% Chicago School TDD - Test API boundaries, no state inspection
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
%% Edge Cases
%%====================================================================

edge_cases_test_() ->
    [
        ?_test(test_unicode_uri()),
        ?_test(test_long_uri())
    ].

test_unicode_uri() ->
    Uri = <<"http://example.com/文書"/utf8>>,
    ?assertEqual(ok, erlmcp_resource:validate_uri(Uri)).

test_long_uri() ->
    LongUri = binary:copy(<<"a">>, 10000),
    ?assertEqual(ok, erlmcp_resource:validate_uri(LongUri)).
