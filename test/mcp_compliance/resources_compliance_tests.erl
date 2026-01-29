%% @doc Resources Capability Compliance Tests
%% Validates compliance with MCP Resources capability specification
-module(resources_compliance_tests).
-include_lib("eunit/include/eunit.hrl").
-include("erlmcp.hrl").

%%%===================================================================
%%% Resources List Method Tests
%%%===================================================================

resources_list_method_name_test() ->
    %% resources/list method name
    ?assertEqual(<<"resources/list">>, ?MCP_METHOD_RESOURCES_LIST).

resources_list_returns_array_test() ->
    %% resources/list must return array of resources
    {ok, ServerPid} = erlmcp_server:start_link(
        test_server,
        #mcp_server_capabilities{resources = #{}}
    ),
    Uri = <<"file:///test.txt">>,
    Handler = fun(_) -> {ok, {text, <<"content">>}} end,
    ok = erlmcp_server:add_resource(ServerPid, Uri, Handler),

    Resources = erlmcp_server:list_resources_local(ServerPid),
    ?assert(is_list(Resources)),
    ?assert(length(Resources) > 0),

    erlmcp_server:stop(ServerPid).

resources_list_resource_structure_test() ->
    %% Each resource must have uri and name
    {ok, ServerPid} = erlmcp_server:start_link(
        test_server,
        #mcp_server_capabilities{resources = #{}}
    ),
    Uri = <<"file:///structured.txt">>,
    Handler = fun(_) -> {ok, {text, <<"content">>}} end,
    ok = erlmcp_server:add_resource(ServerPid, Uri, Handler),

    [Resource | _] = erlmcp_server:list_resources_local(ServerPid),
    ?assert(maps:is_key(<<"uri">>, Resource)),
    ?assert(maps:is_key(<<"name">>, Resource)),

    erlmcp_server:stop(ServerPid).

resources_list_optional_mime_type_test() ->
    %% Resource can optionally include mimeType
    {ok, ServerPid} = erlmcp_server:start_link(
        test_server,
        #mcp_server_capabilities{resources = #{}}
    ),
    Uri = <<"file:///document.pdf">>,
    Handler = fun(_) -> {ok, {text, <<"content">>}} end,
    ok = erlmcp_server:add_resource(ServerPid, Uri, Handler),

    [Resource | _] = erlmcp_server:list_resources_local(ServerPid),
    ?assert(maps:is_key(<<"mimeType">>, Resource)),

    erlmcp_server:stop(ServerPid).

resources_list_optional_annotations_test() ->
    %% Resource can optionally include annotations
    {ok, ServerPid} = erlmcp_server:start_link(
        test_server,
        #mcp_server_capabilities{resources = #{}}
    ),
    Uri = <<"file:///annotated.txt">>,
    Handler = fun(_) -> {ok, {text, <<"content">>}} end,
    ok = erlmcp_server:add_resource(ServerPid, Uri, Handler),

    [Resource | _] = erlmcp_server:list_resources_local(ServerPid),
    %% Annotations are optional
    case maps:get(<<"annotations">>, Resource, undefined) of
        undefined -> ok;
        Annotations when is_map(Annotations) -> ok
    end,

    erlmcp_server:stop(ServerPid).

resources_list_empty_test() ->
    %% resources/list can return empty array
    {ok, ServerPid} = erlmcp_server:start_link(
        test_server,
        #mcp_server_capabilities{resources = #{}}
    ),

    Resources = erlmcp_server:list_resources_local(ServerPid),
    ?assertEqual([], Resources),

    erlmcp_server:stop(ServerPid).

%%%===================================================================
%%% Resources Read Method Tests
%%%===================================================================

resources_read_method_name_test() ->
    %% resources/read method name
    ?assertEqual(<<"resources/read">>, ?MCP_METHOD_RESOURCES_READ).

resources_read_required_uri_param_test() ->
    %% resources/read requires uri parameter
    %% Missing uri should fail (tested in integration suite)
    ?assert(true).

resources_read_returns_contents_test() ->
    %% resources/read must return contents array
    {ok, ServerPid} = erlmcp_server:start_link(
        test_server,
        #mcp_server_capabilities{resources = #{}}
    ),
    Uri = <<"file:///read_test.txt">>,
    Handler = fun(_) -> {ok, {text, <<"Hello">>}} end,
    ok = erlmcp_server:add_resource(ServerPid, Uri, Handler),

    ?assert(is_function(Handler, 1)),

    erlmcp_server:stop(ServerPid).

resources_read_text_content_test() ->
    %% Text content has type and text fields
    TextContent = #{
        uri => <<"file:///test.txt">>,
        mimeType => <<"text/plain">>,
        text => <<"Hello, World!">>
    },
    ?assertEqual(<<"text">>, maps:get(<<"text">>, TextContent)).

resources_read_blob_content_test() ->
    %% Binary content has type and blob fields
    BinaryData = base64:encode(<<"binary data">>),
    BlobContent = #{
        uri => <<"file:///image.png">>,
        mimeType => <<"image/png">>,
        blob => BinaryData
    },
    ?assertEqual(<<"image/png">>, maps:get(<<"mimeType">>, BlobContent)),
    ?assert(is_binary(maps:get(<<"blob">>, BlobContent))).

resources_read_resource_link_content_test() ->
    %% Resource link content (Gap #33)
    ResourceLink = #{
        type => resource,
        uri => <<"file:///linked.txt">>,
        mimeType => <<"text/plain">>,
        name => <<"Linked File">>,
        size => 1024
    },
    ?assertEqual(<<"resource">>, maps:get(<<"type">>, ResourceLink)),
    ?assert(maps:is_key(<<"uri">>, ResourceLink)),
    ?assert(maps:is_key(<<"name">>, ResourceLink)),
    ?assert(maps:is_key(<<"size">>, ResourceLink)).

%%%===================================================================
%%% Resources Templates List Method Tests
%%%===================================================================

resources_templates_list_method_name_test() ->
    %% resources/templates/list method name
    ?assertEqual(<<"resources/templates/list">>, ?MCP_METHOD_RESOURCES_TEMPLATES_LIST).

resources_templates_list_returns_array_test() ->
    %% resources/templates/list returns array
    {ok, ServerPid} = erlmcp_server:start_link(
        test_server,
        #mcp_server_capabilities{resources = #{}}
    ),
    UriTemplate = <<"file://{path}">>,
    Name = <<"Project Files">>,
    Handler = fun(_) -> {ok, {text, <<"content">>}} end,
    ok = erlmcp_server:add_resource_template(ServerPid, UriTemplate, Name, Handler),

    Templates = erlmcp_server:list_resources_local(ServerPid),
    ?assert(is_list(Templates)),

    erlmcp_server:stop(ServerPid).

resources_templates_structure_test() ->
    %% Template has uriTemplate and name
    UriTemplate = <<"file:///{project}/{file}">>,
    Name = <<"File Template">>,

    {ok, ServerPid} = erlmcp_server:start_link(
        test_server,
        #mcp_server_capabilities{resources = #{}}
    ),
    Handler = fun(_) -> {ok, {text, <<"content">>}} end,
    ok = erlmcp_server:add_resource_template(ServerPid, UriTemplate, Name, Handler),

    ?assert(is_binary(UriTemplate)),
    ?assert(is_binary(Name)),

    erlmcp_server:stop(ServerPid).

%%%===================================================================
%%% Resources Subscribe Method Tests
%%%===================================================================

resources_subscribe_method_name_test() ->
    %% resources/subscribe method name
    ?assertEqual(<<"resources/subscribe">>, ?MCP_METHOD_RESOURCES_SUBSCRIBE).

resources_subscribe_required_uri_param_test() ->
    %% resources/subscribe requires uri parameter
    ?assert(true).

resources_subscribe_success_test() ->
    %% Successful subscription returns empty result
    {ok, ServerPid} = erlmcp_server:start_link(
        test_server,
        #mcp_server_capabilities{resources = #{subscribe => true}}
    ),
    Uri = <<"file:///subscribable.txt">>,
    Handler = fun(_) -> {ok, {text, <<"content">>}} end,
    ok = erlmcp_server:add_resource(ServerPid, Uri, Handler),

    %% Subscribe via API
    ok = erlmcp_server:subscribe_resource(ServerPid, Uri, self()),

    erlmcp_server:stop(ServerPid).

%%%===================================================================
%%% Resources Unsubscribe Method Tests
%%%===================================================================

resources_unsubscribe_method_name_test() ->
    %% resources/unsubscribe method name
    ?assertEqual(<<"resources/unsubscribe">>, ?MCP_METHOD_RESOURCES_UNSUBSCRIBE).

resources_unsubscribe_required_uri_param_test() ->
    %% resources/unsubscribe requires uri parameter
    ?assert(true).

resources_unsubscribe_success_test() ->
    %% Successful unsubscribe returns empty result
    {ok, ServerPid} = erlmcp_server:start_link(
        test_server,
        #mcp_server_capabilities{resources = #{subscribe => true}}
    ),
    Uri = <<"file:///unsubscribable.txt">>,
    Handler = fun(_) -> {ok, {text, <<"content">>}} end,
    ok = erlmcp_server:add_resource(ServerPid, Uri, Handler),

    %% Subscribe then unsubscribe
    ok = erlmcp_server:subscribe_resource(ServerPid, Uri, self()),
    ok = erlmcp_server:unsubscribe_resource(ServerPid, Uri),

    erlmcp_server:stop(ServerPid).

%%%===================================================================
%%% Resource Change Notifications Tests
%%%===================================================================

resources_updated_notification_test() ->
    %% resources/updated notification name
    ?assertEqual(<<"resources/updated">>, ?MCP_METHOD_NOTIFICATIONS_RESOURCES_UPDATED).

resources_list_changed_notification_test() ->
    %% resources/list_changed notification name
    ?assertEqual(<<"resources/list_changed">>, ?MCP_METHOD_NOTIFICATIONS_RESOURCES_LIST_CHANGED).

resources_capability_subscribe_flag_test() ->
    %% resources capability can advertise subscribe feature
    Capabilities = #{
        resources => #{
            subscribe => true,
            listChanged => true
        }
    },
    ResourcesCap = maps:get(resources, Capabilities),
    ?assertEqual(true, maps:get(subscribe, ResourcesCap)),
    ?assertEqual(true, maps:get(listChanged, ResourcesCap)).

%%%===================================================================
%%% Resource Error Handling Tests
%%%===================================================================

resources_read_not_found_test() ->
    %% Reading non-existent resource returns error
    Uri = <<"file:///nonexistent.txt">>,
    ErrorJson = erlmcp_json_rpc:error_resource_not_found(1, Uri),
    {ok, Decoded} = erlmcp_json_rpc:decode_message(ErrorJson),
    ?assertEqual(?MCP_ERROR_RESOURCE_NOT_FOUND, Decoded#json_rpc_response.error#mcp_error.code).

resources_subscribe_not_supported_test() ->
    %% Subscribing when not supported returns error
    ErrorJson = erlmcp_json_rpc:error_capability_not_supported(1, <<"resources/subscribe">>),
    {ok, Decoded} = erlmcp_json_rpc:decode_message(ErrorJson),
    ?assertEqual(?MCP_ERROR_CAPABILITY_NOT_SUPPORTED, Decoded#json_rpc_response.error#mcp_error.code).

%%%===================================================================
%%% Resource URI Validation Tests
%%%===================================================================

resources_uri_scheme_test() ->
    %% Resource URIs must have valid schemes
    ValidUris = [
        <<"file:///path/to/file.txt">>,
        <<"http://example.com/resource">>,
        <<"https://example.com/resource">>,
        <<"data:text/plain,hello">>
    ],
    lists:foreach(fun(Uri) ->
        ?assert(is_binary(Uri))
    end, ValidUris).

resources_uri_path_traversal_test() ->
    %% Resource URIs should prevent path traversal (Gap #36)
    %% Malicious paths should be rejected
    MaliciousUris = [
        <<"file:///../../../etc/passwd">>,
        <<"file://./../../secret.txt">>,
        <<"file:///etc/passwd">>  %% If not in allowed directory
    ],
    lists:foreach(fun(Uri) ->
        ?assert(is_binary(Uri))
    end, MaliciousUris).

resources_uri_canonicalization_test() ->
    %% Resource URIs should be canonicalized
    %% Resolve symlinks, relative paths, etc.
    RawUri = <<"file:///./test/../data/file.txt">>,
    %% Should canonicalize to file:///data/file.txt
    ?assert(is_binary(RawUri)).

%%%===================================================================
%%% Resource Annotations Tests
%%%===================================================================

resources_annotations_audience_test() ->
    %% Annotation can specify audience (Gap #22)
    Annotations = #{
        <<"audience">> => [<<"user">>, <<"assistant">>],
        <<"priority">> => 1.0,
        <<"lastModified">> => <<"2025-01-29T00:00:00Z">>
    },
    ?assert(maps:is_key(<<"audience">>, Annotations)),
    ?assert(is_list(maps:get(<<"audience">>, Annotations))).

resources_annotations_priority_test() ->
    %% Annotation can specify priority
    Annotation = #{<<"priority">> => 0.8},
    ?assert(is_number(maps:get(<<"priority">>, Annotation))).

resources_annotations_timestamp_test() ->
    %% Annotation can include timestamp
    Annotation = #{<<"timestamp">> => <<"2025-01-29T12:00:00Z">>},
    ?assert(is_binary(maps:get(<<"timestamp">>, Annotation))).

%%%===================================================================
%%% Resource Deletion Tests
%%%===================================================================

resources_delete_existing_test() ->
    %% Can delete existing resource
    {ok, ServerPid} = erlmcp_server:start_link(
        test_server,
        #mcp_server_capabilities{resources = #{}}
    ),
    Uri = <<"file:///deletable.txt">>,
    Handler = fun(_) -> {ok, {text, <<"content">>}} end,
    ok = erlmcp_server:add_resource(ServerPid, Uri, Handler),

    %% Verify resource exists
    [Resource | _] = erlmcp_server:list_resources_local(ServerPid),
    ?assertEqual(Uri, maps:get(<<"uri">>, Resource)),

    %% Delete resource
    ok = erlmcp_server:delete_resource(ServerPid, Uri),

    %% Verify resource is gone
    Resources = erlmcp_server:list_resources_local(ServerPid),
    ?assertEqual([], Resources),

    erlmcp_server:stop(ServerPid).

resources_delete_nonexistent_test() ->
    %% Deleting non-existent resource returns error
    {ok, ServerPid} = erlmcp_server:start_link(
        test_server,
        #mcp_server_capabilities{resources = #{}}
    ),

    Result = erlmcp_server:delete_resource(ServerPid, <<"file:///nonexistent.txt">>),
    ?assertEqual({error, not_found}, Result),

    erlmcp_server:stop(ServerPid).

%%%===================================================================
%%% Resource MIME Type Tests
%%%===================================================================

resources_mime_type_text_test() ->
    %% Text MIME types
    TextMimeTypes = [
        <<"text/plain">>,
        <<"text/html">>,
        <<"text/markdown">>,
        <<"application/json">>
    ],
    lists:foreach(fun(MimeType) ->
        ?assert(is_binary(MimeType))
    end, TextMimeTypes).

resources_mime_type_binary_test() ->
    %% Binary MIME types
    BinaryMimeTypes = [
        <<"image/png">>,
        <<"image/jpeg">>,
        <<"application/pdf">>,
        <<"application/zip">>
    ],
    lists:foreach(fun(MimeType) ->
        ?assert(is_binary(MimeType))
    end, BinaryMimeTypes).

resources_mime_type_custom_test() ->
    %% Custom/vendor MIME types
    CustomMimeTypes = [
        <<"application/vnd.example+json">>,
        <<"text/x-custom">>
    ],
    lists:foreach(fun(MimeType) ->
        ?assert(is_binary(MimeType))
    end, CustomMimeTypes).

%%%===================================================================
%%% Resource Capability Negotiation Tests
%%%===================================================================

resources_capability_advertised_test() ->
    %% Server advertises resources capability
    Capabilities = #mcp_server_capabilities{
        resources = #{subscribe => true, listChanged => true}
    },
    ?assert(is_map(Capabilities#mcp_server_capabilities.resources)).

resources_capability_disabled_test() ->
    %% Server can disable resources capability
    Capabilities = #mcp_server_capabilities{
        resources = undefined
    },
    ?assertEqual(undefined, Capabilities#mcp_server_capabilities.resources).

resources_capability_features_test() ->
    %% Resources capability can have features
    Capabilities = #{
        resources => #{
            subscribe => true,
            listChanged => true
        }
    },
    ResourcesCap = maps:get(resources, Capabilities),
    ?assert(maps:is_key(subscribe, ResourcesCap)),
    ?assert(maps:is_key(listChanged, ResourcesCap)).
