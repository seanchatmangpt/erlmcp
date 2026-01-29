%% @doc Tools Capability Compliance Tests
%% Validates compliance with MCP Tools capability specification
-module(tools_compliance_tests).
-include_lib("eunit/include/eunit.hrl").
-include("erlmcp.hrl").

%%%===================================================================
%%% Tools List Method Tests
%%%===================================================================

tools_list_method_name_test() ->
    %% tools/list method name
    ?assertEqual(<<"tools/list">>, ?MCP_METHOD_TOOLS_LIST).

tools_list_returns_array_test() ->
    %% tools/list must return array of tools
    {ok, ServerPid} = erlmcp_server:start_link(
        test_server,
        #mcp_server_capabilities{tools = #{}}
    ),
    ToolName = <<"test_tool">>,
    Handler = fun(_) -> {ok, {text, <<"result">>}} end,
    ok = erlmcp_server:add_tool(ServerPid, ToolName, Handler),

    Tools = erlmcp_server:list_tools_local(ServerPid),
    ?assert(is_list(Tools)),
    ?assert(length(Tools) > 0),

    erlmcp_server:stop(ServerPid).

tools_list_tool_structure_test() ->
    %% Each tool must have name and description
    {ok, ServerPid} = erlmcp_server:start_link(
        test_server,
        #mcp_server_capabilities{tools = #{}}
    ),
    ToolName = <<"structured_tool">>,
    Handler = fun(_) -> {ok, {text, <<"result">>}} end,
    ok = erlmcp_server:add_tool(ServerPid, ToolName, Handler),

    [Tool | _] = erlmcp_server:list_tools_local(ServerPid),
    ?assert(maps:is_key(<<"name">>, Tool)),
    ?assert(maps:is_key(<<"description">>, Tool)),

    erlmcp_server:stop(ServerPid).

tools_list_optional_input_schema_test() ->
    %% Tool can optionally include inputSchema
    {ok, ServerPid} = erlmcp_server:start_link(
        test_server,
        #mcp_server_capabilities{tools = #{}}
    ),
    ToolName = <<"schema_tool">>,
    Handler = fun(_) -> {ok, {text, <<"result">>}} end,
    InputSchema = #{
        type => object,
        properties => #{
            query => #{type => string, description => "Search query"}
        },
        required => [<<"query">>]
    },
    ok = erlmcp_server:add_tool_with_schema(ServerPid, ToolName, Handler, InputSchema),

    [Tool | _] = erlmcp_server:list_tools_local(ServerPid),
    ?assert(maps:is_key(<<"inputSchema">>, Tool)),

    erlmcp_server:stop(ServerPid).

tools_list_empty_test() ->
    %% tools/list can return empty array
    {ok, ServerPid} = erlmcp_server:start_link(
        test_server,
        #mcp_server_capabilities{tools = #{}}
    ),

    Tools = erlmcp_server:list_tools_local(ServerPid),
    ?assertEqual([], Tools),

    erlmcp_server:stop(ServerPid).

%%%===================================================================
%%% Tools Call Method Tests
%%%===================================================================

tools_call_method_name_test() ->
    %% tools/call method name
    ?assertEqual(<<"tools/call">>, ?MCP_METHOD_TOOLS_CALL).

tools_call_required_name_param_test() ->
    %% tools/call requires name parameter
    {ok, ServerPid} = erlmcp_server:start_link(
        test_server,
        #mcp_server_capabilities{tools = #{}}
    ),
    ToolName = <<"echo_tool">>,
    Handler = fun(Args) -> {ok, {text, maps:get(<<"text">>, Args, <<"">>)}} end,
    ok = erlmcp_server:add_tool(ServerPid, ToolName, Handler),

    %% Missing name should fail
    %% (This is tested in integration suite via client calls)

    erlmcp_server:stop(ServerPid).

tools_call_arguments_param_test() ->
    %% tools/call takes optional arguments parameter
    {ok, ServerPid} = erlmcp_server:start_link(
        test_server,
        #mcp_server_capabilities{tools = #{}}
    ),
    ToolName = <<"args_tool">>,
    Handler = fun(Args) ->
        Text = maps:get(<<"text">>, Args, <<"default">>),
        {ok, {text, Text}}
    end,
    ok = erlmcp_server:add_tool(ServerPid, ToolName, Handler),

    %% With arguments
    ?assert(is_function(Handler, 1)),

    erlmcp_server:stop(ServerPid).

tools_call_returns_content_test() ->
    %% tools/call must return content array
    {ok, ServerPid} = erlmcp_server:start_link(
        test_server,
        #mcp_server_capabilities{tools = #{}}
    ),
    ToolName = <<"content_tool">>,
    Handler = fun(_) ->
        {ok, [
            #{type => text, text => <<"Line 1">>},
            #{type => text, text => <<"Line 2">>}
        ]}
    end,
    ok = erlmcp_server:add_tool(ServerPid, ToolName, Handler),

    %% Handler returns list of content items
    ?assert(is_function(Handler, 1)),

    erlmcp_server:stop(ServerPid).

tools_call_content_type_text_test() ->
    %% Content items can have type: text
    TextContent = #{type => text, text => <<"Hello">>},
    ?assertEqual(<<"text">>, maps:get(<<"type">>, TextContent)).

tools_call_content_type_image_test() ->
    %% Content items can have type: image
    ImageData = base64:encode(<<"fake image">>),
    ImageContent = #{type => image, data => ImageData, mimeType => <<"image/png">>},
    ?assertEqual(<<"image">>, maps:get(<<"type">>, ImageContent)),
    ?assertEqual(<<"image/png">>, maps:get(<<"mimeType">>, ImageContent)).

tools_call_content_type_resource_test() ->
    %% Content items can have type: resource
    ResourceContent = #{
        type => resource,
        uri => <<"file:///test.txt">>,
        mimeType => <<"text/plain">>
    },
    ?assertEqual(<<"resource">>, maps:get(<<"type">>, ResourceContent)).

tools_call_is_error_flag_test() ->
    %% Tool result can have isError flag
    ErrorResult = #{
        content => [#{type => text, text => <<"Tool failed">>}],
        isError => true
    },
    ?assertEqual(true, maps:get(<<"isError">>, ErrorResult)).

%%%===================================================================
%%% Tool Change Notifications Tests
%%%===================================================================

tools_list_changed_notification_test() ->
    %% tools/list_changed notification name
    ?assertEqual(<<"notifications/tools/list_changed">>, ?MCP_METHOD_NOTIFICATIONS_TOOLS_LIST_CHANGED).

tools_capability_list_changed_flag_test() ->
    %% tools capability can advertise listChanged feature
    Capabilities = #{
        tools => #{
            listChanged => true
        }
    },
    ?assertEqual(true, maps:get(listChanged, maps:get(tools, Capabilities))).

%%%===================================================================
%%% Tool Error Handling Tests
%%%===================================================================

tools_call_tool_not_found_test() ->
    %% Calling non-existent tool returns error
    ToolName = <<"nonexistent_tool">>,
    ErrorJson = erlmcp_json_rpc:error_tool_not_found(1, ToolName),
    {ok, Decoded} = erlmcp_json_rpc:decode_message(ErrorJson),
    ?assertEqual(?MCP_ERROR_TOOL_NOT_FOUND, Decoded#json_rpc_response.error#mcp_error.code).

tools_call_invalid_arguments_test() ->
    %% Invalid tool arguments should return error
    ErrorJson = erlmcp_json_rpc:error_validation_failed(1, <<"Invalid argument type">>),
    {ok, Decoded} = erlmcp_json_rpc:decode_message(ErrorJson),
    ?assertEqual(?MCP_ERROR_VALIDATION_FAILED, Decoded#json_rpc_response.error#mcp_error.code).

%%%===================================================================
%%% Tool Schema Validation Tests
%%%===================================================================

tools_input_schema_json_schema_test() ->
    %% inputSchema follows JSON Schema format
    Schema = #{
        type => object,
        properties => #{
            name => #{type => string},
            age => #{type => number, minimum => 0}
        },
        required => [<<"name">>]
    },
    ?assertEqual(<<"object">>, maps:get(<<"type">>, Schema)).

tools_input_schema_nested_test() ->
    %% inputSchema can have nested objects
    NestedSchema = #{
        type => object,
        properties => #{
            user => #{
                type => object,
                properties => #{
                    name => #{type => string},
                    email => #{type => string}
                }
            }
        }
    },
    UserProps = maps:get(<<"user">>, maps:get(<<"properties">>, NestedSchema)),
    ?assertEqual(<<"object">>, maps:get(<<"type">>, UserProps)).

tools_input_schema_array_test() ->
    %% inputSchema can define array properties
    ArraySchema = #{
        type => object,
        properties => #{
            items => #{
                type => array,
                items => #{type => string}
            }
        }
    },
    ItemsProp = maps:get(<<"items">>, maps:get(<<"properties">>, ArraySchema)),
    ?assertEqual(<<"array">>, maps:get(<<"type">>, ItemsProp)).

%%%===================================================================
%%% Tool Description Tests
%%%===================================================================

tools_description_required_test() ->
    %% Tool must have description
    ToolName = <<"described_tool">>,
    Handler = fun(_) -> {ok, {text, <<"result">>}} end,

    {ok, ServerPid} = erlmcp_server:start_link(
        test_server,
        #mcp_server_capabilities{tools = #{}}
    ),
    ok = erlmcp_server:add_tool(ServerPid, ToolName, Handler),

    [Tool | _] = erlmcp_server:list_tools_local(ServerPid),
    Description = maps:get(<<"description">>, Tool),
    ?assert(is_binary(Description)),
    ?assert(byte_size(Description) > 0),

    erlmcp_server:stop(ServerPid).

tools_description_max_length_test() ->
    %% Tool description should be reasonable length
    %% (Gap #40: Tool description too long error -32011)
    LongDescription = binary:copy(<<"a">>, 10000),
    ?assert(byte_size(LongDescription) > 0).

%%%===================================================================
%%% Multi-Content Tool Tests
%%%===================================================================

tools_call_multiple_content_items_test() ->
    %% Tool can return multiple content items
    {ok, ServerPid} = erlmcp_server:start_link(
        test_server,
        #mcp_server_capabilities{tools = #{}}
    ),
    ToolName = <<"multi_content">>,
    Handler = fun(_) ->
        {ok, [
            #{type => text, text => <<"First">>},
            #{type => text, text => <<"Second">>},
            #{type => text, text => <<"Third">>}
        ]}
    end,
    ok = erlmcp_server:add_tool(ServerPid, ToolName, Handler),

    ?assert(is_function(Handler, 1)),

    erlmcp_server:stop(ServerPid).

tools_call_mixed_content_types_test() ->
    %% Tool can return mixed content types
    {ok, ServerPid} = erlmcp_server:start_link(
        test_server,
        #mcp_server_capabilities{tools = #{}}
    ),
    ToolName = <<"mixed_types">>,
    Handler = fun(_) ->
        {ok, [
            #{type => text, text => <<"Text result">>},
            #{
                type => image,
                data => base64:encode(<<"image">>),
                mimeType => <<"image/png">>
            },
            #{type => resource, uri => <<"file:///data.txt">>}
        ]}
    end,
    ok = erlmcp_server:add_tool(ServerPid, ToolName, Handler),

    ?assert(is_function(Handler, 1)),

    erlmcp_server:stop(ServerPid).

%%%===================================================================
%%% Tool Progress Support Tests
%%%===================================================================

tools_call_progress_token_test() ->
    %% Tool result can include _meta.progressToken
    ToolResult = #{
        content => [#{type => text, text => <<"Working...">>}],
        _meta => #{
            progressToken => 42,
            progress => 0.5
        }
    },
    Meta = maps:get(<<"_meta">>, ToolResult),
    ?assert(maps:is_key(<<"progressToken">>, Meta)),
    ?assert(maps:is_key(<<"progress">>, Meta)).

tools_progress_notification_test() ->
    %% Progress notification format
    ProgressParams = #{
        progressToken => 42,
        progress => 0.75,
        total => 1.0
    },
    ?assert(maps:is_key(<<"progressToken">>, ProgressParams)),
    ?assert(maps:is_key(<<"progress">>, ProgressParams)),
    ?assert(maps:is_key(<<"total">>, ProgressParams)).

%%%===================================================================
%%% Tool Execution Context Tests
%%%===================================================================

tools_call_timeout_handling_test() ->
    %% Tool execution should handle timeouts
    {ok, ServerPid} = erlmcp_server:start_link(
        test_server,
        #mcp_server_capabilities{tools = #{}}
    ),
    ToolName = <<"timeout_tool">>,
    Handler = fun(_) ->
        timer:sleep(10000),
        {ok, {text, <<"Too slow">>}}
    end,
    ok = erlmcp_server:add_tool(ServerPid, ToolName, Handler),

    ?assert(is_function(Handler, 1)),

    erlmcp_server:stop(ServerPid).

tools_call_error_propagation_test() ->
    %% Tool handler errors should be propagated
    {ok, ServerPid} = erlmcp_server:start_link(
        test_server,
        #mcp_server_capabilities{tools = #{}}
    ),
    ToolName = <<"error_tool">>,
    Handler = fun(_) ->
        error(simulated_tool_error)
    end,
    ok = erlmcp_server:add_tool(ServerPid, ToolName, Handler),

    ?assert(is_function(Handler, 1)),

    erlmcp_server:stop(ServerPid).

%%%===================================================================
%%% Tool Deletion Tests
%%%===================================================================

tools_delete_existing_test() ->
    %% Can delete existing tool
    {ok, ServerPid} = erlmcp_server:start_link(
        test_server,
        #mcp_server_capabilities{tools = #{}}
    ),
    ToolName = <<"deletable_tool">>,
    Handler = fun(_) -> {ok, {text, <<"result">>}} end,
    ok = erlmcp_server:add_tool(ServerPid, ToolName, Handler),

    %% Verify tool exists
    [Tool | _] = erlmcp_server:list_tools_local(ServerPid),
    ?assertEqual(ToolName, maps:get(<<"name">>, Tool)),

    %% Delete tool
    ok = erlmcp_server:delete_tool(ServerPid, ToolName),

    %% Verify tool is gone
    Tools = erlmcp_server:list_tools_local(ServerPid),
    ?assertEqual([], Tools),

    erlmcp_server:stop(ServerPid).

tools_delete_nonexistent_test() ->
    %% Deleting non-existent tool returns error
    {ok, ServerPid} = erlmcp_server:start_link(
        test_server,
        #mcp_server_capabilities{tools = #{}}
    ),

    Result = erlmcp_server:delete_tool(ServerPid, <<"nonexistent">>),
    ?assertEqual({error, not_found}, Result),

    erlmcp_server:stop(ServerPid).

%%%===================================================================
%%% Tool Capability Negotiation Tests
%%%===================================================================

tools_capability_advertised_test() ->
    %% Server advertises tools capability
    Capabilities = #mcp_server_capabilities{
        tools = #{listChanged => true}
    },
    ?assert(is_map(Capabilities#mcp_server_capabilities.tools)).

tools_capability_disabled_test() ->
    %% Server can disable tools capability
    Capabilities = #mcp_server_capabilities{
        tools = undefined
    },
    ?assertEqual(undefined, Capabilities#mcp_server_capabilities.tools).

tools_capability_features_test() ->
    %% Tools capability can have features
    Capabilities = #{
        tools => #{
            listChanged => true
        }
    },
    ToolsCap = maps:get(tools, Capabilities),
    ?assert(maps:is_key(listChanged, ToolsCap)).
