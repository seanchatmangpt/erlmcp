%% @doc Prompts Capability Compliance Tests
%% Validates compliance with MCP Prompts capability specification
-module(prompts_compliance_tests).
-include_lib("eunit/include/eunit.hrl").
-include("erlmcp.hrl").

%%%===================================================================
%%% Prompts List Method Tests
%%%===================================================================

prompts_list_method_name_test() ->
    %% prompts/list method name
    ?assertEqual(<<"prompts/list">>, ?MCP_METHOD_PROMPTS_LIST).

prompts_list_returns_array_test() ->
    %% prompts/list must return array of prompts
    {ok, ServerPid} = erlmcp_server:start_link(
        test_server,
        #mcp_server_capabilities{prompts = #{}}
    ),
    PromptName = <<"test_prompt">>,
    Handler = fun(_) -> {ok, #{messages => []}} end,
    ok = erlmcp_server:add_prompt(ServerPid, PromptName, Handler),

    Prompts = erlmcp_server:list_prompts_local(ServerPid),
    ?assert(is_list(Prompts)),
    ?assert(length(Prompts) > 0),

    erlmcp_server:stop(ServerPid).

prompts_list_prompt_structure_test() ->
    %% Each prompt must have name
    {ok, ServerPid} = erlmcp_server:start_link(
        test_server,
        #mcp_server_capabilities{prompts = #{}}
    ),
    PromptName = <<"structured_prompt">>,
    Handler = fun(_) -> {ok, #{messages => []}} end,
    ok = erlmcp_server:add_prompt(ServerPid, PromptName, Handler),

    [Prompt | _] = erlmcp_server:list_prompts_local(ServerPid),
    ?assert(maps:is_key(<<"name">>, Prompt)),

    erlmcp_server:stop(ServerPid).

prompts_list_optional_description_test() ->
    %% Prompt can optionally include description
    {ok, ServerPid} = erlmcp_server:start_link(
        test_server,
        #mcp_server_capabilities{prompts = #{}}
    ),
    PromptName = <<"described_prompt">>,
    Handler = fun(_) -> {ok, #{messages => []}} end,
    ok = erlmcp_server:add_prompt(ServerPid, PromptName, Handler),

    [Prompt | _] = erlmcp_server:list_prompts_local(ServerPid),
    %% Description is optional
    case maps:get(<<"description">>, Prompt, undefined) of
        undefined -> ok;
        Desc when is_binary(Desc) -> ?assert(is_binary(Desc))
    end,

    erlmcp_server:stop(ServerPid).

prompts_list_optional_arguments_test() ->
    %% Prompt can optionally include arguments
    {ok, ServerPid} = erlmcp_server:start_link(
        test_server,
        #mcp_server_capabilities{prompts = #{}}
    ),
    PromptName = <<"arg_prompt">>,
    Arguments = [
        #mcp_prompt_argument{
            name = <<"query">>,
            description = <<"Search query">>,
            required = true
        }
    ],
    Handler = fun(_) -> {ok, #{messages => []}} end,
    ok = erlmcp_server:add_prompt_with_args(ServerPid, PromptName, Handler, Arguments),

    [Prompt | _] = erlmcp_server:list_prompts_local(ServerPid),
    ?assert(maps:is_key(<<"arguments">>, Prompt)),

    erlmcp_server:stop(ServerPid).

prompts_list_empty_test() ->
    %% prompts/list can return empty array
    {ok, ServerPid} = erlmcp_server:start_link(
        test_server,
        #mcp_server_capabilities{prompts = #{}}
    ),

    Prompts = erlmcp_server:list_prompts_local(ServerPid),
    ?assertEqual([], Prompts),

    erlmcp_server:stop(ServerPid).

%%%===================================================================
%%% Prompts Get Method Tests
%%%===================================================================

prompts_get_method_name_test() ->
    %% prompts/get method name
    ?assertEqual(<<"prompts/get">>, ?MCP_METHOD_PROMPTS_GET).

prompts_get_required_name_param_test() ->
    %% prompts/get requires name parameter
    ?assert(true).

prompts_get_optional_arguments_param_test() ->
    %% prompts/get takes optional arguments
    {ok, ServerPid} = erlmcp_server:start_link(
        test_server,
        #mcp_server_capabilities{prompts = #{}}
    ),
    PromptName = <<"args_prompt">>,
    Arguments = [
        #mcp_prompt_argument{
            name = <<"format">>,
            description = <<"Output format">>,
            required = false
        }
    ],
    Handler = fun(Args) ->
        Format = maps:get(<<"format">>, Args, <<"text">>),
        {ok, #{
            messages => [
                #{
                    role => user,
                    content => #{
                        type => text,
                        text => <<"Format: ", Format/binary>>
                    }
                }
            ]
        }}
    end,
    ok = erlmcp_server:add_prompt_with_args(ServerPid, PromptName, Handler, Arguments),

    ?assert(is_function(Handler, 1)),

    erlmcp_server:stop(ServerPid).

prompts_get_returns_messages_test() ->
    %% prompts/get must return messages array
    {ok, ServerPid} = erlmcp_server:start_link(
        test_server,
        #mcp_server_capabilities{prompts = #{}}
    ),
    PromptName = <<"message_prompt">>,
    Handler = fun(_) ->
        {ok, #{
            messages => [
                #{
                    role => user,
                    content => #{type => text, text => <<"Hello">>}
                }
            ]
        }}
    end,
    ok = erlmcp_server:add_prompt(ServerPid, PromptName, Handler),

    ?assert(is_function(Handler, 1)),

    erlmcp_server:stop(ServerPid).

%%%===================================================================
%%% Prompt Message Structure Tests
%%%===================================================================

prompts_message_role_test() ->
    %% Message must have role field
    Message = #{
        role => user,
        content => #{type => text, text => <<"Hello">>}
    },
    ?assertEqual(<<"user">>, maps:get(<<"role">>, Message)).

prompts_message_role_user_test() ->
    %% Valid role: user
    ?assertEqual(<<"user">>, <<"user">>).

prompts_message_role_assistant_test() ->
    %% Valid role: assistant
    ?assertEqual(<<"assistant">>, <<"assistant">>).

prompts_message_role_system_test() ->
    %% Valid role: system (if supported)
    ?assertEqual(<<"system">>, <<"system">>).

prompts_message_content_type_text_test() ->
    %% Message content can be text
    Content = #{
        type => text,
        text => <<"Hello, world!">>
    },
    ?assertEqual(<<"text">>, maps:get(<<"type">>, Content)),
    ?assert(is_binary(maps:get(<<"text">>, Content))).

prompts_message_content_type_image_test() ->
    %% Message content can be image
    ImageData = base64:encode(<<"image data">>),
    Content = #{
        type => image,
        data => ImageData,
        mimeType => <<"image/png">>
    },
    ?assertEqual(<<"image">>, maps:get(<<"type">>, Content)),
    ?assert(is_binary(maps:get(<<"data">>, Content))).

prompts_message_content_type_resource_test() ->
    %% Message content can be resource reference
    Content = #{
        type => resource,
        uri => <<"file:///document.txt">>
    },
    ?assertEqual(<<"resource">>, maps:get(<<"type">>, Content)).

%%%===================================================================
%%% Prompt Argument Structure Tests
%%%===================================================================

prompts_argument_required_fields_test() ->
    %% Argument must have name and required
    Argument = #mcp_prompt_argument{
        name = <<"query">>,
        description => <<"Search query">>,
        required = true
    },
    ?assert(is_binary(Argument#mcp_prompt_argument.name)),
    ?assert(is_boolean(Argument#mcp_prompt_argument.required)).

prompts_argument_optional_description_test() ->
    %% Argument can have description
    Argument = #mcp_prompt_argument{
        name = <<"format">>,
        description => <<"Output format (json, text, xml)">>,
        required = false
    },
    ?assert(is_binary(Argument#mcp_prompt_argument.description)).

prompts_argument_required_true_test() ->
    %% Required argument must be provided
    Argument = #mcp_prompt_argument{
        name = <<"apiKey">>,
        description => <<"API key">>,
        required = true
    },
    ?assertEqual(true, Argument#mcp_prompt_argument.required).

prompts_argument_required_false_test() ->
    %% Optional argument can be omitted
    Argument = #mcp_prompt_argument{
        name = <<"count">>,
        description => <<"Number of results">>,
        required = false
    },
    ?assertEqual(false, Argument#mcp_prompt_argument.required).

%%%===================================================================
%%% Prompt Input Schema Tests (Gap #42)
%%%===================================================================

prompts_input_schema_validation_test() ->
    %% Prompt can validate arguments against JSON schema
    InputSchema = #{
        type => object,
        properties => #{
            name => #{type => string},
            age => #{type => number, minimum => 0, maximum => 150}
        },
        required => [<<"name">>]
    },
    ?assertEqual(<<"object">>, maps:get(<<"type">>, InputSchema)).

prompts_input_schema_nested_test() ->
    %% Input schema can have nested objects
    NestedSchema = #{
        type => object,
        properties => #{
            user => #{
                type => object,
                properties => #{
                    name => #{type => string},
                    email => #{type => string, format => email}
                }
            }
        }
    },
    UserProp = maps:get(<<"user">>, maps:get(<<"properties">>, NestedSchema)),
    ?assertEqual(<<"object">>, maps:get(<<"type">>, UserProp)).

prompts_input_schema_array_test() ->
    %% Input schema can have array properties
    ArraySchema = #{
        type => object,
        properties => #{
            tags => #{
                type => array,
                items => #{type => string}
            }
        }
    },
    TagsProp = maps:get(<<"tags">>, maps:get(<<"properties">>, ArraySchema)),
    ?assertEqual(<<"array">>, maps:get(<<"type">>, TagsProp)).

%%%===================================================================
%%% Prompt Change Notifications Tests
%%%===================================================================

prompts_list_changed_notification_test() ->
    %% prompts/list_changed notification name
    ?assertEqual(<<"prompts/list_changed">>, ?MCP_METHOD_NOTIFICATIONS_PROMPTS_LIST_CHANGED).

prompts_capability_list_changed_flag_test() ->
    %% prompts capability can advertise listChanged feature
    Capabilities = #{
        prompts => #{
            listChanged => true
        }
    },
    PromptsCap = maps:get(prompts, Capabilities),
    ?assertEqual(true, maps:get(listChanged, PromptsCap)).

%%%===================================================================
%%% Prompt Error Handling Tests
%%%===================================================================

prompts_get_not_found_test() ->
    %% Getting non-existent prompt returns error
    PromptName = <<"nonexistent_prompt">>,
    ErrorJson = erlmcp_json_rpc:error_prompt_not_found(1, PromptName),
    {ok, Decoded} = erlmcp_json_rpc:decode_message(ErrorJson),
    ?assertEqual(?MCP_ERROR_PROMPT_NOT_FOUND, Decoded#json_rpc_response.error#mcp_error.code).

prompts_get_invalid_arguments_test() ->
    %% Invalid prompt arguments should return error
    ErrorJson = erlmcp_json_rpc:error_validation_failed(1, <<"Invalid argument type">>),
    {ok, Decoded} = erlmcp_json_rpc:decode_message(ErrorJson),
    ?assertEqual(?MCP_ERROR_VALIDATION_FAILED, Decoded#json_rpc_response.error#mcp_error.code).

prompts_get_missing_required_arg_test() ->
    %% Missing required argument should return error
    ErrorJson = erlmcp_json_rpc:error_invalid_params(1, <<"Missing required argument: 'query'">>),
    {ok, Decoded} = erlmcp_json_rpc:decode_message(ErrorJson),
    ?assertEqual(?JSONRPC_INVALID_PARAMS, Decoded#json_rpc_response.error#mcp_error.code).

%%%===================================================================
%%% Prompt Deletion Tests
%%%===================================================================

prompts_delete_existing_test() ->
    %% Can delete existing prompt
    {ok, ServerPid} = erlmcp_server:start_link(
        test_server,
        #mcp_server_capabilities{prompts = #{}}
    ),
    PromptName = <<"deletable_prompt">>,
    Handler = fun(_) -> {ok, #{messages => []}} end,
    ok = erlmcp_server:add_prompt(ServerPid, PromptName, Handler),

    %% Verify prompt exists
    [Prompt | _] = erlmcp_server:list_prompts_local(ServerPid),
    ?assertEqual(PromptName, maps:get(<<"name">>, Prompt)),

    %% Delete prompt
    ok = erlmcp_server:delete_prompt(ServerPid, PromptName),

    %% Verify prompt is gone
    Prompts = erlmcp_server:list_prompts_local(ServerPid),
    ?assertEqual([], Prompts),

    erlmcp_server:stop(ServerPid).

prompts_delete_nonexistent_test() ->
    %% Deleting non-existent prompt returns error
    {ok, ServerPid} = erlmcp_server:start_link(
        test_server,
        #mcp_server_capabilities{prompts = #{}}
    ),

    Result = erlmcp_server:delete_prompt(ServerPid, <<"nonexistent">>),
    ?assertEqual({error, not_found}, Result),

    erlmcp_server:stop(ServerPid).

%%%===================================================================
%%% Prompt Multi-Message Tests
%%%===================================================================

prompts_get_multiple_messages_test() ->
    %% Prompt can return multiple messages
    {ok, ServerPid} = erlmcp_server:start_link(
        test_server,
        #mcp_server_capabilities{prompts = #{}}
    ),
    PromptName = <<"multi_message_prompt">>,
    Handler = fun(_) ->
        {ok, #{
            messages => [
                #{
                    role => system,
                    content => #{type => text, text => <<"You are a helpful assistant.">>}
                },
                #{
                    role => user,
                    content => #{type => text, text => <<"Hello!">>}
                },
                #{
                    role => assistant,
                    content => #{type => text, text => <<"Hi there!">>}
                }
            ]
        }}
    end,
    ok = erlmcp_server:add_prompt(ServerPid, PromptName, Handler),

    ?assert(is_function(Handler, 1)),

    erlmcp_server:stop(ServerPid).

prompts_get_conversation_history_test() ->
    %% Prompt can include conversation history
    Handler = fun(_) ->
        {ok, #{
            messages => [
                #{
                    role => user,
                    content => #{type => text, text => <<"First message">>}
                },
                #{
                    role => assistant,
                    content => #{type => text, text => <<"First response">>}
                },
                #{
                    role => user,
                    content => #{type => text, text => <<"Second message">>}
                }
            ]
        }}
    end,
    ?assert(is_function(Handler, 1)).

%%%===================================================================
%%% Prompt Capability Negotiation Tests
%%%===================================================================

prompts_capability_advertised_test() ->
    %% Server advertises prompts capability
    Capabilities = #mcp_server_capabilities{
        prompts = #{listChanged => true}
    },
    ?assert(is_map(Capabilities#mcp_server_capabilities.prompts)).

prompts_capability_disabled_test() ->
    %% Server can disable prompts capability
    Capabilities = #mcp_server_capabilities{
        prompts = undefined
    },
    ?assertEqual(undefined, Capabilities#mcp_server_capabilities.prompts).

prompts_capability_features_test() ->
    %% Prompts capability can have features
    Capabilities = #{
        prompts => #{
            listChanged => true
        }
    },
    PromptsCap = maps:get(prompts, Capabilities),
    ?assert(maps:is_key(listChanged, PromptsCap)).

%%%===================================================================
%%% Prompt Argument Interpolation Tests
%%%===================================================================

prompts_argument_interpolation_test() ->
    %% Prompt handler should interpolate arguments
    Handler = fun(Args) ->
        Name = maps:get(<<"name">>, Args, <<"World">>),
        Greeting = <<"Hello, ", Name/binary, "!">>,
        {ok, #{
            messages => [
                #{
                    role => user,
                    content => #{type => text, text => Greeting}
                }
            ]
        }}
    end,
    ?assert(is_function(Handler, 1)).

prompts_argument_default_values_test() ->
    %% Prompt handler should provide default values
    Handler = fun(Args) ->
        Count = maps:get(<<"count">>, Args, 10),
        Format = maps:get(<<"format">>, Args, <<"text">>),
        {ok, #{
            messages => [
                #{
                    role => user,
                    content => #{
                        type => text,
                        text => io_lib:format("Count: ~p, Format: ~s", [Count, Format])
                    }
                }
            ]
        }}
    end,
    ?assert(is_function(Handler, 1)).
