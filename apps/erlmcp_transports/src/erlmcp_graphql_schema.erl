%%%-------------------------------------------------------------------
%%% @doc
%%% GraphQL Schema Generator for MCP Protocol
%%%
%%% Generates GraphQL schema from MCP server definitions including:
%%% - Tools (queries and mutations)
%%% - Resources (queries and subscriptions)
%%% - Prompts (queries)
%%% - Introspection support
%%%
%%% Schema follows GraphQL June 2018 specification.
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_graphql_schema).

-include("erlmcp.hrl").
-include_lib("kernel/include/logger.hrl").

%% API exports
-export([
    build_schema/1,
    build_schema/2,
    get_type_definitions/0,
    refresh_schema/2
]).

%% Types
-type schema_config() :: #{
    server_id := atom(),
    tools => [map()],
    resources => [map()],
    prompts => [map()],
    enable_introspection => boolean(),
    max_query_depth => pos_integer(),
    enable_subscriptions => boolean()
}.

-export_type([schema_config/0]).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Build GraphQL schema from MCP server configuration
-spec build_schema(schema_config()) -> {ok, term()} | {error, term()}.
build_schema(Config) ->
    build_schema(Config, #{}).

%% @doc Build GraphQL schema with additional options
-spec build_schema(schema_config(), map()) -> {ok, term()} | {error, term()}.
build_schema(Config, Opts) ->
    ?LOG_INFO("Building GraphQL schema for server: ~p", [maps:get(server_id, Config)]),

    try
        %% Extract MCP entities
        Tools = maps:get(tools, Config, []),
        Resources = maps:get(resources, Config, []),
        Prompts = maps:get(prompts, Config, []),

        %% Build type definitions
        TypeDefs = build_type_definitions(Tools, Resources, Prompts),

        %% Build root types
        QueryType = build_query_type(Tools, Resources, Prompts),
        MutationType = build_mutation_type(Tools),
        SubscriptionType = case maps:get(enable_subscriptions, Config, true) of
            true -> build_subscription_type(Resources);
            false -> undefined
        end,

        %% Create schema configuration
        SchemaConfig = #{
            query => QueryType,
            mutation => MutationType,
            types => TypeDefs
        },

        SchemaConfigWithSubs = case SubscriptionType of
            undefined -> SchemaConfig;
            SubType -> SchemaConfig#{subscription => SubType}
        end,

        %% Apply additional options
        FinalConfig = maps:merge(SchemaConfigWithSubs, Opts),

        %% Build the schema
        case graphql:load_schema(FinalConfig) of
            {ok, Schema} ->
                ?LOG_INFO("GraphQL schema built successfully"),
                {ok, Schema};
            {error, Reason} ->
                ?LOG_ERROR("Failed to build GraphQL schema: ~p", [Reason]),
                {error, Reason}
        end
    catch
        Class:Error:Stack ->
            ?LOG_ERROR("Exception building GraphQL schema: ~p:~p~n~p", [Class, Error, Stack]),
            {error, {schema_build_failed, Error}}
    end.

%% @doc Get type definitions for GraphQL schema
-spec get_type_definitions() -> map().
get_type_definitions() ->
    #{
        %% Base types
        'Tool' => build_tool_type(),
        'Resource' => build_resource_type(),
        'Prompt' => build_prompt_type(),
        'PromptArgument' => build_prompt_argument_type(),
        'Content' => build_content_type(),
        'ToolResult' => build_tool_result_type(),
        'PromptMessage' => build_prompt_message_type(),

        %% Input types
        'ToolCallInput' => build_tool_call_input_type(),
        'PromptGetInput' => build_prompt_get_input_type()
    }.

%% @doc Refresh schema with updated MCP definitions
-spec refresh_schema(term(), schema_config()) -> {ok, term()} | {error, term()}.
refresh_schema(_OldSchema, Config) ->
    %% Rebuild schema from scratch
    build_schema(Config).

%%====================================================================
%% Internal Functions - Type Definitions
%%====================================================================

%% @private
%% Build all type definitions
-spec build_type_definitions(list(), list(), list()) -> map().
build_type_definitions(_Tools, _Resources, _Prompts) ->
    #{
        'Tool' => build_tool_type(),
        'Resource' => build_resource_type(),
        'ResourceTemplate' => build_resource_template_type(),
        'Prompt' => build_prompt_type(),
        'PromptArgument' => build_prompt_argument_type(),
        'Content' => build_content_type(),
        'TextContent' => build_text_content_type(),
        'ImageContent' => build_image_content_type(),
        'AudioContent' => build_audio_content_type(),
        'ToolResult' => build_tool_result_type(),
        'PromptMessage' => build_prompt_message_type(),
        'ToolCallInput' => build_tool_call_input_type(),
        'PromptGetInput' => build_prompt_get_input_type(),
        'JSON' => build_json_scalar_type()
    }.

%% @private
%% Tool type definition
-spec build_tool_type() -> map().
build_tool_type() ->
    #{
        kind => object,
        fields => #{
            name => #{type => {non_null, string}, description => <<"Tool name">>},
            description => #{type => string, description => <<"Tool description">>},
            inputSchema => #{type => 'JSON', description => <<"JSON Schema for tool input">>}
        }
    }.

%% @private
%% Resource type definition
-spec build_resource_type() -> map().
build_resource_type() ->
    #{
        kind => object,
        fields => #{
            uri => #{type => {non_null, string}, description => <<"Resource URI">>},
            name => #{type => {non_null, string}, description => <<"Resource name">>},
            mimeType => #{type => string, description => <<"MIME type">>},
            description => #{type => string, description => <<"Resource description">>}
        }
    }.

%% @private
%% Resource template type definition
-spec build_resource_template_type() -> map().
build_resource_template_type() ->
    #{
        kind => object,
        fields => #{
            uriTemplate => #{type => {non_null, string}, description => <<"URI template">>},
            name => #{type => {non_null, string}, description => <<"Template name">>},
            mimeType => #{type => string, description => <<"MIME type">>},
            description => #{type => string, description => <<"Template description">>}
        }
    }.

%% @private
%% Prompt type definition
-spec build_prompt_type() -> map().
build_prompt_type() ->
    #{
        kind => object,
        fields => #{
            name => #{type => {non_null, string}, description => <<"Prompt name">>},
            description => #{type => string, description => <<"Prompt description">>},
            arguments => #{type => {list, 'PromptArgument'}, description => <<"Prompt arguments">>}
        }
    }.

%% @private
%% Prompt argument type definition
-spec build_prompt_argument_type() -> map().
build_prompt_argument_type() ->
    #{
        kind => object,
        fields => #{
            name => #{type => {non_null, string}, description => <<"Argument name">>},
            description => #{type => string, description => <<"Argument description">>},
            required => #{type => boolean, description => <<"Is argument required">>}
        }
    }.

%% @private
%% Content type definition (union type)
-spec build_content_type() -> map().
build_content_type() ->
    #{
        kind => union,
        types => ['TextContent', 'ImageContent', 'AudioContent'],
        resolve_type => fun resolve_content_type/1
    }.

%% @private
%% Text content type
-spec build_text_content_type() -> map().
build_text_content_type() ->
    #{
        kind => object,
        fields => #{
            type => #{type => {non_null, string}, description => <<"Content type">>},
            text => #{type => {non_null, string}, description => <<"Text content">>}
        }
    }.

%% @private
%% Image content type
-spec build_image_content_type() -> map().
build_image_content_type() ->
    #{
        kind => object,
        fields => #{
            type => #{type => {non_null, string}, description => <<"Content type">>},
            data => #{type => {non_null, string}, description => <<"Base64 encoded image data">>},
            mimeType => #{type => {non_null, string}, description => <<"Image MIME type">>}
        }
    }.

%% @private
%% Audio content type
-spec build_audio_content_type() -> map().
build_audio_content_type() ->
    #{
        kind => object,
        fields => #{
            type => #{type => {non_null, string}, description => <<"Content type">>},
            data => #{type => {non_null, string}, description => <<"Base64 encoded audio data">>},
            mimeType => #{type => {non_null, string}, description => <<"Audio MIME type">>}
        }
    }.

%% @private
%% Tool result type
-spec build_tool_result_type() -> map().
build_tool_result_type() ->
    #{
        kind => object,
        fields => #{
            content => #{type => {non_null, {list, 'Content'}}, description => <<"Result content">>},
            isError => #{type => boolean, description => <<"Is error result">>}
        }
    }.

%% @private
%% Prompt message type
-spec build_prompt_message_type() -> map().
build_prompt_message_type() ->
    #{
        kind => object,
        fields => #{
            role => #{type => {non_null, string}, description => <<"Message role">>},
            content => #{type => {non_null, 'Content'}, description => <<"Message content">>}
        }
    }.

%% @private
%% Tool call input type
-spec build_tool_call_input_type() -> map().
build_tool_call_input_type() ->
    #{
        kind => input_object,
        fields => #{
            name => #{type => {non_null, string}, description => <<"Tool name">>},
            arguments => #{type => 'JSON', description => <<"Tool arguments as JSON">>}
        }
    }.

%% @private
%% Prompt get input type
-spec build_prompt_get_input_type() -> map().
build_prompt_get_input_type() ->
    #{
        kind => input_object,
        fields => #{
            name => #{type => {non_null, string}, description => <<"Prompt name">>},
            arguments => #{type => 'JSON', description => <<"Prompt arguments as JSON">>}
        }
    }.

%% @private
%% JSON scalar type (for arbitrary JSON data)
-spec build_json_scalar_type() -> map().
build_json_scalar_type() ->
    #{
        kind => scalar,
        serialize => fun(Value) -> jsx:encode(Value) end,
        parse_value => fun(Value) when is_binary(Value) -> jsx:decode(Value, [return_maps]) end,
        parse_literal => fun parse_json_literal/1
    }.

%%====================================================================
%% Internal Functions - Root Types
%%====================================================================

%% @private
%% Build Query type with all queries
-spec build_query_type(list(), list(), list()) -> map().
build_query_type(Tools, Resources, Prompts) ->
    BaseFields = #{
        tools => #{
            type => {non_null, {list, 'Tool'}},
            description => <<"List all available tools">>,
            resolve => fun(_Ctx, _Args) -> {ok, Tools} end
        },
        tool => #{
            type => 'Tool',
            description => <<"Get a specific tool by name">>,
            args => #{name => #{type => {non_null, string}}},
            resolve => fun(_Ctx, #{<<"name">> := Name}) ->
                case lists:filter(fun(T) -> maps:get(<<"name">>, T) =:= Name end, Tools) of
                    [Tool] -> {ok, Tool};
                    [] -> {error, <<"Tool not found">>}
                end
            end
        },
        resources => #{
            type => {non_null, {list, 'Resource'}},
            description => <<"List all available resources">>,
            resolve => fun(_Ctx, _Args) -> {ok, Resources} end
        },
        resource => #{
            type => 'Resource',
            description => <<"Get a specific resource by URI">>,
            args => #{uri => #{type => {non_null, string}}},
            resolve => fun(_Ctx, #{<<"uri">> := Uri}) ->
                case lists:filter(fun(R) -> maps:get(<<"uri">>, R) =:= Uri end, Resources) of
                    [Resource] -> {ok, Resource};
                    [] -> {error, <<"Resource not found">>}
                end
            end
        },
        prompts => #{
            type => {non_null, {list, 'Prompt'}},
            description => <<"List all available prompts">>,
            resolve => fun(_Ctx, _Args) -> {ok, Prompts} end
        },
        prompt => #{
            type => 'Prompt',
            description => <<"Get a specific prompt by name">>,
            args => #{name => #{type => {non_null, string}}},
            resolve => fun(_Ctx, #{<<"name">> := Name}) ->
                case lists:filter(fun(P) -> maps:get(<<"name">>, P) =:= Name end, Prompts) of
                    [Prompt] -> {ok, Prompt};
                    [] -> {error, <<"Prompt not found">>}
                end
            end
        }
    },

    #{kind => object, fields => BaseFields}.

%% @private
%% Build Mutation type with all mutations
-spec build_mutation_type(list()) -> map().
build_mutation_type(_Tools) ->
    #{
        kind => object,
        fields => #{
            callTool => #{
                type => 'ToolResult',
                description => <<"Call a tool with arguments">>,
                args => #{
                    name => #{type => {non_null, string}, description => <<"Tool name">>},
                    arguments => #{type => 'JSON', description => <<"Tool arguments">>}
                },
                resolve => fun(Ctx, Args) ->
                    erlmcp_graphql_resolver:resolve_call_tool(Ctx, Args)
                end
            }
        }
    }.

%% @private
%% Build Subscription type for resource updates
-spec build_subscription_type(list()) -> map().
build_subscription_type(_Resources) ->
    #{
        kind => object,
        fields => #{
            resourceUpdated => #{
                type => 'Resource',
                description => <<"Subscribe to resource updates">>,
                args => #{uri => #{type => string, description => <<"Resource URI to watch">>}},
                subscribe => fun(Ctx, Args) ->
                    erlmcp_graphql_resolver:subscribe_resource_updated(Ctx, Args)
                end,
                resolve => fun(_Ctx, Resource) -> {ok, Resource} end
            }
        }
    }.

%%====================================================================
%% Internal Functions - Helpers
%%====================================================================

%% @private
%% Resolve content type from union
-spec resolve_content_type(map()) -> atom().
resolve_content_type(#{<<"type">> := <<"text">>}) -> 'TextContent';
resolve_content_type(#{<<"type">> := <<"image">>}) -> 'ImageContent';
resolve_content_type(#{<<"type">> := <<"audio">>}) -> 'AudioContent';
resolve_content_type(_) -> 'TextContent'.

%% @private
%% Parse JSON literal from GraphQL AST
-spec parse_json_literal(term()) -> term().
parse_json_literal({string, Value}) -> Value;
parse_json_literal({int, Value}) -> Value;
parse_json_literal({float, Value}) -> Value;
parse_json_literal({bool, Value}) -> Value;
parse_json_literal(null) -> null;
parse_json_literal({object, Fields}) ->
    maps:from_list([{K, parse_json_literal(V)} || {K, V} <- Fields]);
parse_json_literal({list, Values}) ->
    [parse_json_literal(V) || V <- Values];
parse_json_literal(_) -> null.
