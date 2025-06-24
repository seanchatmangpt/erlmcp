-module(erlmcp_server).
-behaviour(gen_server).

-include("erlmcp.hrl").

-export([
    start_link/2,
    add_resource/3,
    add_resource_template/4,
    add_tool/3,
    add_tool_with_schema/4,
    add_prompt/3,
    add_prompt_with_args/4,
    subscribe_resource/3,
    unsubscribe_resource/2,
    report_progress/4,
    stop/1
]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
    transport :: module(),
    transport_state :: term(),
    capabilities :: #mcp_server_capabilities{},
    resources = #{} :: map(),
    resource_templates = #{} :: map(),
    tools = #{} :: map(),
    prompts = #{} :: map(),
    subscriptions = #{} :: map(),
    progress_tokens = #{} :: map(),
    initialized = false :: boolean()
}).

start_link(TransportOpts, Capabilities) ->
    gen_server:start_link(?MODULE, [TransportOpts, Capabilities], []).

add_resource(Server, Name, Handler) ->
    gen_server:call(Server, {add_resource, Name, Handler}).

add_resource_template(Server, UriTemplate, Name, Handler) ->
    gen_server:call(Server, {add_resource_template, UriTemplate, Name, Handler}).

add_tool(Server, Name, Handler) ->
    gen_server:call(Server, {add_tool, Name, Handler}).

add_tool_with_schema(Server, Name, Handler, Schema) ->
    gen_server:call(Server, {add_tool_with_schema, Name, Handler, Schema}).

add_prompt(Server, Name, Handler) ->
    gen_server:call(Server, {add_prompt, Name, Handler}).

add_prompt_with_args(Server, Name, Handler, Arguments) ->
    gen_server:call(Server, {add_prompt_with_args, Name, Handler, Arguments}).

subscribe_resource(Server, Uri, Subscriber) ->
    gen_server:call(Server, {subscribe_resource, Uri, Subscriber}).

unsubscribe_resource(Server, Uri) ->
    gen_server:call(Server, {unsubscribe_resource, Uri}).

report_progress(Server, Token, Progress, Total) ->
    gen_server:cast(Server, {report_progress, Token, Progress, Total}).

stop(Server) ->
    gen_server:stop(Server).

init([TransportOpts, Capabilities]) ->
    {Transport, TransportState} = init_transport(TransportOpts),
    {ok, #state{
        transport = Transport,
        transport_state = TransportState,
        capabilities = Capabilities,
        resources = #{},
        resource_templates = #{},
        tools = #{},
        prompts = #{},
        subscriptions = #{},
        progress_tokens = #{}
    }}.

handle_call({add_resource, Name, Handler}, _From, State) ->
    Resource = #mcp_resource{uri = Name, name = Name, mime_type = <<"text/plain">>},
    NewResources = maps:put(Name, {Resource, Handler}, State#state.resources),
    {reply, ok, State#state{resources = NewResources}};

handle_call({add_resource_template, UriTemplate, Name, Handler}, _From, State) ->
    Template = #mcp_resource_template{uri_template = UriTemplate, name = Name, mime_type = <<"text/plain">>},
    NewTemplates = maps:put(UriTemplate, {Template, Handler}, State#state.resource_templates),
    {reply, ok, State#state{resource_templates = NewTemplates}};

handle_call({add_tool, Name, Handler}, _From, State) ->
    Tool = #mcp_tool{name = Name, description = <<"Tool: ", Name/binary>>},
    NewTools = maps:put(Name, {Tool, Handler, undefined}, State#state.tools),
    {reply, ok, State#state{tools = NewTools}};

handle_call({add_tool_with_schema, Name, Handler, Schema}, _From, State) ->
    Tool = #mcp_tool{name = Name, description = <<"Tool: ", Name/binary>>, input_schema = Schema},
    NewTools = maps:put(Name, {Tool, Handler, Schema}, State#state.tools),
    {reply, ok, State#state{tools = NewTools}};

handle_call({add_prompt, Name, Handler}, _From, State) ->
    Prompt = #mcp_prompt{name = Name},
    NewPrompts = maps:put(Name, {Prompt, Handler}, State#state.prompts),
    {reply, ok, State#state{prompts = NewPrompts}};

handle_call({add_prompt_with_args, Name, Handler, Arguments}, _From, State) ->
    Prompt = #mcp_prompt{name = Name, arguments = Arguments},
    NewPrompts = maps:put(Name, {Prompt, Handler}, State#state.prompts),
    {reply, ok, State#state{prompts = NewPrompts}};

handle_call({subscribe_resource, Uri, Subscriber}, _From, State) ->
    Subscription = #mcp_resource_subscription{uri = Uri, subscriber = Subscriber},
    NewSubscriptions = maps:put({Uri, Subscriber}, Subscription, State#state.subscriptions),
    {reply, ok, State#state{subscriptions = NewSubscriptions}};

handle_call({unsubscribe_resource, Uri}, _From, State) ->
    NewSubscriptions = maps:filter(fun({SubUri, _}, _) -> SubUri =/= Uri end, State#state.subscriptions),
    {reply, ok, State#state{subscriptions = NewSubscriptions}};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({report_progress, Token, Progress, Total}, State) ->
    ProgressToken = #mcp_progress_token{token = Token},
    Notification = #mcp_progress_notification{
        progress_token = ProgressToken,
        progress = Progress,
        total = Total
    },
    NotificationParams = #{
        <<"progressToken">> => Token,
        <<"progress">> => Progress,
        <<"total">> => Total
    },
    Json = erlmcp_json_rpc:encode_notification(<<"notifications/progress">>, NotificationParams),
    send_message(State, Json),
    NewTokens = maps:put(Token, Notification, State#state.progress_tokens),
    {noreply, State#state{progress_tokens = NewTokens}};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({transport_message, Data}, State) ->
    case erlmcp_json_rpc:decode_message(Data) of
        {ok, #json_rpc_request{id = Id, method = Method, params = Params}} ->
            handle_request(Id, Method, Params, State);
        {ok, #json_rpc_notification{method = Method, params = Params}} ->
            handle_notification(Method, Params, State);
        {error, Reason} ->
            error_logger:error_msg("Failed to decode message: ~p~n", [Reason]),
            {noreply, State}
    end;

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

init_transport({stdio, _Opts}) ->
    {erlmcp_transport_stdio, undefined};
init_transport({tcp, Opts}) ->
    {erlmcp_transport_tcp, Opts}.

handle_request(Id, <<"initialize">>, _Params, State) ->
    Response = #{
        <<"protocolVersion">> => ?MCP_VERSION,
        <<"capabilities">> => encode_server_capabilities(State#state.capabilities),
        <<"serverInfo">> => #{
            <<"name">> => <<"erlmcp">>,
            <<"version">> => <<"0.1.0">>
        }
    },
    Json = erlmcp_json_rpc:encode_response(Id, Response),
    send_message(State, Json),
    {noreply, State#state{initialized = true}};

handle_request(Id, <<"resources/list">>, _Params, State) ->
    Resources = maps:fold(fun(_Name, {Resource, _Handler}, Acc) ->
        ResourceMap = #{
            <<"uri">> => Resource#mcp_resource.uri,
            <<"name">> => Resource#mcp_resource.name
        },
        ResourceMapWithDesc = case Resource#mcp_resource.description of
            undefined -> ResourceMap;
            Desc -> ResourceMap#{<<"description">> => Desc}
        end,
        ResourceMapWithMime = case Resource#mcp_resource.mime_type of
            undefined -> ResourceMapWithDesc;
            MimeType -> ResourceMapWithDesc#{<<"mimeType">> => MimeType}
        end,
        [ResourceMapWithMime | Acc]
    end, [], State#state.resources),
    Response = #{<<"resources">> => Resources},
    Json = erlmcp_json_rpc:encode_response(Id, Response),
    send_message(State, Json),
    {noreply, State};

handle_request(Id, <<"resources/read">>, #{<<"uri">> := Uri}, State) ->
    case find_resource(Uri, State) of
        {ok, {Resource, Handler}} ->
            Json = try
                Content = Handler(Uri),
                ContentItem = case Content of
                    #mcp_content{} = McpContent ->
                        ContentMap = #{<<"type">> => McpContent#mcp_content.type},
                        ContentMap1 = case McpContent#mcp_content.text of
                            undefined -> ContentMap;
                            Text -> ContentMap#{<<"text">> => Text}
                        end,
                        ContentMap2 = case McpContent#mcp_content.data of
                            undefined -> ContentMap1;
                            Data -> ContentMap1#{<<"data">> => Data}
                        end,
                        case McpContent#mcp_content.mime_type of
                            undefined -> ContentMap2;
                            MimeType -> ContentMap2#{<<"mimeType">> => MimeType}
                        end;
                    BinaryContent when is_binary(BinaryContent) ->
                        #{
                            <<"uri">> => Uri,
                            <<"mimeType">> => Resource#mcp_resource.mime_type,
                            <<"text">> => BinaryContent
                        }
                end,
                Response = #{<<"contents">> => [ContentItem]},
                erlmcp_json_rpc:encode_response(Id, Response)
            catch
                error:_Reason ->
                    erlmcp_json_rpc:encode_error_response(Id, -32603, <<"Internal error">>)
            end,
            send_message(State, Json);
        {error, not_found} ->
            Json = erlmcp_json_rpc:encode_error_response(Id, -32602, <<"Resource not found">>),
            send_message(State, Json)
    end,
    {noreply, State};

handle_request(Id, <<"tools/list">>, _Params, State) ->
    Tools = maps:fold(fun(_Name, {Tool, _Handler, _Schema}, Acc) ->
        ToolMap = #{
            <<"name">> => Tool#mcp_tool.name,
            <<"description">> => Tool#mcp_tool.description
        },
        ToolMapWithSchema = case Tool#mcp_tool.input_schema of
            undefined -> ToolMap;
            Schema -> ToolMap#{<<"inputSchema">> => Schema}
        end,
        [ToolMapWithSchema | Acc]
    end, [], State#state.tools),
    Response = #{<<"tools">> => Tools},
    Json = erlmcp_json_rpc:encode_response(Id, Response),
    send_message(State, Json),
    {noreply, State};

handle_request(Id, <<"tools/call">>, #{<<"name">> := Name, <<"arguments">> := Args}, State) ->
    case maps:get(Name, State#state.tools, undefined) of
        undefined ->
            Json = erlmcp_json_rpc:encode_error_response(Id, -32602, <<"Tool not found">>),
            send_message(State, Json);
        {_Tool, Handler, Schema} ->
            case validate_tool_input(Args, Schema) of
                ok ->
                    Json = try
                        Result = Handler(Args),
                        ContentList = case Result of
                            #mcp_content{} = Content ->
                                [encode_content(Content)];
                            BinaryResult when is_binary(BinaryResult) ->
                                [#{<<"type">> => <<"text">>, <<"text">> => BinaryResult}];
                            ResultList when is_list(ResultList) ->
                                [encode_content(C) || C <- ResultList]
                        end,
                        Response = #{<<"content">> => ContentList},
                        erlmcp_json_rpc:encode_response(Id, Response)
                    catch
                        error:_Reason ->
                            erlmcp_json_rpc:encode_error_response(Id, -32603, <<"Internal error">>)
                    end,
                    send_message(State, Json);
                {error, ValidationError} ->
                    Json = erlmcp_json_rpc:encode_error_response(Id, -32602, ValidationError),
                    send_message(State, Json)
            end
    end,
    {noreply, State};

handle_request(Id, <<"resources/templates">>, _Params, State) ->
    Templates = maps:fold(fun(_UriTemplate, {Template, _Handler}, Acc) ->
        TemplateMap = #{
            <<"uriTemplate">> => Template#mcp_resource_template.uri_template,
            <<"name">> => Template#mcp_resource_template.name
        },
        TemplateMapWithDesc = case Template#mcp_resource_template.description of
            undefined -> TemplateMap;
            Desc -> TemplateMap#{<<"description">> => Desc}
        end,
        TemplateMapWithMime = case Template#mcp_resource_template.mime_type of
            undefined -> TemplateMapWithDesc;
            MimeType -> TemplateMapWithDesc#{<<"mimeType">> => MimeType}
        end,
        [TemplateMapWithMime | Acc]
    end, [], State#state.resource_templates),
    Response = #{<<"resourceTemplates">> => Templates},
    Json = erlmcp_json_rpc:encode_response(Id, Response),
    send_message(State, Json),
    {noreply, State};

handle_request(Id, <<"resources/subscribe">>, #{<<"uri">> := Uri}, State) ->
    Subscription = #mcp_resource_subscription{uri = Uri, subscriber = self()},
    NewSubscriptions = maps:put({Uri, self()}, Subscription, State#state.subscriptions),
    Response = #{},
    Json = erlmcp_json_rpc:encode_response(Id, Response),
    send_message(State, Json),
    {noreply, State#state{subscriptions = NewSubscriptions}};

handle_request(Id, <<"resources/unsubscribe">>, #{<<"uri">> := Uri}, State) ->
    NewSubscriptions = maps:filter(fun({SubUri, _}, _) -> SubUri =/= Uri end, State#state.subscriptions),
    Response = #{},
    Json = erlmcp_json_rpc:encode_response(Id, Response),
    send_message(State, Json),
    {noreply, State#state{subscriptions = NewSubscriptions}};

handle_request(Id, <<"prompts/list">>, _Params, State) ->
    Prompts = maps:fold(fun(_Name, {Prompt, _Handler}, Acc) ->
        PromptMap = #{<<"name">> => Prompt#mcp_prompt.name},
        PromptMapWithDesc = case Prompt#mcp_prompt.description of
            undefined -> PromptMap;
            Desc -> PromptMap#{<<"description">> => Desc}
        end,
        PromptMapWithArgs = case Prompt#mcp_prompt.arguments of
            undefined -> PromptMapWithDesc;
            Args -> PromptMapWithDesc#{<<"arguments">> => [encode_prompt_argument(Arg) || Arg <- Args]}
        end,
        [PromptMapWithArgs | Acc]
    end, [], State#state.prompts),
    Response = #{<<"prompts">> => Prompts},
    Json = erlmcp_json_rpc:encode_response(Id, Response),
    send_message(State, Json),
    {noreply, State};

handle_request(Id, <<"prompts/get">>, #{<<"name">> := Name} = Params, State) ->
    case maps:get(Name, State#state.prompts, undefined) of
        undefined ->
            Json = erlmcp_json_rpc:encode_error_response(Id, -32602, <<"Prompt not found">>),
            send_message(State, Json);
        {Prompt, Handler} ->
            Arguments = maps:get(<<"arguments">>, Params, #{}),
            Json = try
                Result = Handler(Arguments),
                Messages = case Result of
                    BinaryResult when is_binary(BinaryResult) ->
                        [#{<<"role">> => <<"user">>, <<"content">> => #{<<"type">> => <<"text">>, <<"text">> => BinaryResult}}];
                    MessageList when is_list(MessageList) ->
                        MessageList
                end,
                Response = #{
                    <<"description">> => Prompt#mcp_prompt.description,
                    <<"messages">> => Messages
                },
                erlmcp_json_rpc:encode_response(Id, Response)
            catch
                error:_Reason ->
                    erlmcp_json_rpc:encode_error_response(Id, -32603, <<"Internal error">>)
            end,
            send_message(State, Json)
    end,
    {noreply, State};

handle_request(Id, _Method, _Params, State) ->
    Json = erlmcp_json_rpc:encode_error_response(Id, -32601, <<"Method not found">>),
    send_message(State, Json),
    {noreply, State}.

handle_notification(_Method, _Params, State) ->
    {noreply, State}.

send_message(#state{transport = Transport, transport_state = TransportState}, Message) ->
    Transport:send(TransportState, Message).

encode_server_capabilities(#mcp_server_capabilities{resources = Resources, tools = Tools, prompts = Prompts}) ->
    Caps = #{},
    Caps1 = case Resources of
        #mcp_capability{enabled = true} ->
            Caps#{<<"resources">> => #{<<"subscribe">> => true, <<"listChanged">> => true}};
        _ -> Caps
    end,
    Caps2 = case Tools of
        #mcp_capability{enabled = true} ->
            Caps1#{<<"tools">> => #{}};
        _ -> Caps1
    end,
    case Prompts of
        #mcp_capability{enabled = true} ->
            Caps2#{<<"prompts">> => #{<<"listChanged">> => true}};
        _ -> Caps2
    end.

find_resource(Uri, State) ->
    case maps:get(Uri, State#state.resources, undefined) of
        undefined ->
            find_resource_template(Uri, State);
        Resource ->
            {ok, Resource}
    end.

find_resource_template(Uri, State) ->
    Templates = maps:to_list(State#state.resource_templates),
    find_matching_template(Uri, Templates).

find_matching_template(_Uri, []) ->
    {error, not_found};
find_matching_template(Uri, [{UriTemplate, {Template, Handler}} | Rest]) ->
    case match_uri_template(Uri, UriTemplate) of
        {ok, _Params} ->
            {ok, {Template, Handler}};
        nomatch ->
            find_matching_template(Uri, Rest)
    end.

match_uri_template(Uri, UriTemplate) ->
    Pattern = uri_template_to_regex(UriTemplate),
    case re:run(Uri, Pattern, [{capture, all, binary}]) of
        {match, _Captures} ->
            {ok, #{}};
        nomatch ->
            nomatch
    end.

uri_template_to_regex(UriTemplate) ->
    EscapedTemplate = re:replace(UriTemplate, "\\{[^}]+\\}", "([^/]+)", [global, {return, binary}]),
    <<"^", EscapedTemplate/binary, "$">>.

validate_tool_input(_Args, undefined) ->
    ok;
validate_tool_input(Args, Schema) ->
    case jesse:validate(Schema, Args) of
        {ok, _} ->
            ok;
        {error, ValidationErrors} ->
            ErrorMsg = format_validation_errors(ValidationErrors),
            {error, ErrorMsg}
    end.

format_validation_errors(Errors) ->
    ErrorStrings = [format_single_error(Error) || Error <- Errors],
    iolist_to_binary(string:join(ErrorStrings, "; ")).

format_single_error({data_invalid, Schema, Error, Data}) ->
    io_lib:format("Validation error: ~p for data ~p with schema ~p", [Error, Data, Schema]);
format_single_error(Error) ->
    io_lib:format("Validation error: ~p", [Error]).

encode_content(#mcp_content{type = Type, text = Text, data = Data, mime_type = MimeType}) ->
    ContentMap = #{<<"type">> => Type},
    ContentMap1 = case Text of
        undefined -> ContentMap;
        _ -> ContentMap#{<<"text">> => Text}
    end,
    ContentMap2 = case Data of
        undefined -> ContentMap1;
        _ -> ContentMap1#{<<"data">> => Data}
    end,
    case MimeType of
        undefined -> ContentMap2;
        _ -> ContentMap2#{<<"mimeType">> => MimeType}
    end.

encode_prompt_argument(#mcp_prompt_argument{name = Name, description = Description, required = Required}) ->
    ArgMap = #{<<"name">> => Name, <<"required">> => Required},
    case Description of
        undefined -> ArgMap;
        _ -> ArgMap#{<<"description">> => Description}
    end.
