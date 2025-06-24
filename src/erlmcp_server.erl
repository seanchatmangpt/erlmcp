-module(erlmcp_server).
-behaviour(gen_server).

-include("erlmcp.hrl").

-export([
    start_link/2,
    add_resource/3,
    add_tool/3,
    add_prompt/3,
    stop/1
]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
    transport :: module(),
    transport_state :: term(),
    capabilities :: #mcp_server_capabilities{},
    resources = #{} :: map(),
    tools = #{} :: map(),
    prompts = #{} :: map(),
    initialized = false :: boolean()
}).

start_link(TransportOpts, Capabilities) ->
    gen_server:start_link(?MODULE, [TransportOpts, Capabilities], []).

add_resource(Server, Name, Handler) ->
    gen_server:call(Server, {add_resource, Name, Handler}).

add_tool(Server, Name, Handler) ->
    gen_server:call(Server, {add_tool, Name, Handler}).

add_prompt(Server, Name, Handler) ->
    gen_server:call(Server, {add_prompt, Name, Handler}).

stop(Server) ->
    gen_server:stop(Server).

init([TransportOpts, Capabilities]) ->
    {Transport, TransportState} = init_transport(TransportOpts),
    {ok, #state{
        transport = Transport,
        transport_state = TransportState,
        capabilities = Capabilities
    }}.

handle_call({add_resource, Name, Handler}, _From, State) ->
    NewResources = maps:put(Name, Handler, State#state.resources),
    {reply, ok, State#state{resources = NewResources}};

handle_call({add_tool, Name, Handler}, _From, State) ->
    NewTools = maps:put(Name, Handler, State#state.tools),
    {reply, ok, State#state{tools = NewTools}};

handle_call({add_prompt, Name, Handler}, _From, State) ->
    NewPrompts = maps:put(Name, Handler, State#state.prompts),
    {reply, ok, State#state{prompts = NewPrompts}};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

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
    Resources = maps:fold(fun(Name, _Handler, Acc) ->
        [#{<<"uri">> => Name, <<"name">> => Name} | Acc]
    end, [], State#state.resources),
    Response = #{<<"resources">> => Resources},
    Json = erlmcp_json_rpc:encode_response(Id, Response),
    send_message(State, Json),
    {noreply, State};

handle_request(Id, <<"resources/read">>, #{<<"uri">> := Uri}, State) ->
    case maps:get(Uri, State#state.resources, undefined) of
        undefined ->
            Json = erlmcp_json_rpc:encode_error_response(Id, -32602, <<"Resource not found">>),
            send_message(State, Json);
        Handler ->
            Json = try
                Content = Handler(),
                Response = #{
                    <<"contents">> => [#{
                        <<"uri">> => Uri,
                        <<"mimeType">> => <<"text/plain">>,
                        <<"text">> => Content
                    }]
                },
                erlmcp_json_rpc:encode_response(Id, Response)
            catch
                error:_Reason ->
                    erlmcp_json_rpc:encode_error_response(Id, -32603, <<"Internal error">>)
            end,
            send_message(State, Json)
    end,
    {noreply, State};

handle_request(Id, <<"tools/list">>, _Params, State) ->
    Tools = maps:fold(fun(Name, _Handler, Acc) ->
        [#{<<"name">> => Name, <<"description">> => <<"Tool: ", Name/binary>>} | Acc]
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
        Handler ->
            Json = try
                Result = Handler(Args),
                Response = #{
                    <<"content">> => [#{
                        <<"type">> => <<"text">>,
                        <<"text">> => Result
                    }]
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
            Caps#{<<"resources">> => #{}};
        _ -> Caps
    end,
    Caps2 = case Tools of
        #mcp_capability{enabled = true} ->
            Caps1#{<<"tools">> => #{}};
        _ -> Caps1
    end,
    case Prompts of
        #mcp_capability{enabled = true} ->
            Caps2#{<<"prompts">> => #{}};
        _ -> Caps2
    end.
