-module(erlmcp_client).
-behaviour(gen_server).

-include("erlmcp.hrl").

-export([
    start_link/1,
    initialize/2,
    list_roots/1,
    list_resources/1,
    read_resource/2,
    list_prompts/1,
    get_prompt/2,
    list_tools/1,
    call_tool/3,
    stop/1
]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
    transport :: module(),
    transport_state :: term(),
    capabilities :: #mcp_server_capabilities{},
    request_id = 1 :: integer(),
    pending_requests = #{} :: map()
}).

start_link(TransportOpts) ->
    gen_server:start_link(?MODULE, [TransportOpts], []).

initialize(Client, Capabilities) ->
    gen_server:call(Client, {initialize, Capabilities}).

list_roots(Client) ->
    gen_server:call(Client, list_roots).

list_resources(Client) ->
    gen_server:call(Client, list_resources).

read_resource(Client, Uri) ->
    gen_server:call(Client, {read_resource, Uri}).

list_prompts(Client) ->
    gen_server:call(Client, list_prompts).

get_prompt(Client, Name) ->
    gen_server:call(Client, {get_prompt, Name}).

list_tools(Client) ->
    gen_server:call(Client, list_tools).

call_tool(Client, Name, Arguments) ->
    gen_server:call(Client, {call_tool, Name, Arguments}).

stop(Client) ->
    gen_server:stop(Client).

init([TransportOpts]) ->
    {Transport, TransportState} = init_transport(TransportOpts),
    {ok, #state{
        transport = Transport,
        transport_state = TransportState
    }}.

handle_call({initialize, Capabilities}, _From, State) ->
    RequestId = State#state.request_id,
    InitRequest = #{
        <<"protocolVersion">> => ?MCP_VERSION,
        <<"capabilities">> => encode_capabilities(Capabilities),
        <<"clientInfo">> => #{
            <<"name">> => <<"erlmcp">>,
            <<"version">> => <<"0.1.0">>
        }
    },
    Json = erlmcp_json_rpc:encode_request(RequestId, <<"initialize">>, InitRequest),
    case send_message(State, Json) of
        ok ->
            NewState = State#state{
                request_id = RequestId + 1,
                pending_requests = maps:put(RequestId, {initialize, self()}, State#state.pending_requests)
            },
            {noreply, NewState};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call(list_resources, _From, State) ->
    RequestId = State#state.request_id,
    Json = erlmcp_json_rpc:encode_request(RequestId, <<"resources/list">>, #{}),
    case send_message(State, Json) of
        ok ->
            NewState = State#state{
                request_id = RequestId + 1,
                pending_requests = maps:put(RequestId, {list_resources, self()}, State#state.pending_requests)
            },
            {noreply, NewState};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({read_resource, Uri}, _From, State) ->
    RequestId = State#state.request_id,
    Params = #{<<"uri">> => Uri},
    Json = erlmcp_json_rpc:encode_request(RequestId, <<"resources/read">>, Params),
    case send_message(State, Json) of
        ok ->
            NewState = State#state{
                request_id = RequestId + 1,
                pending_requests = maps:put(RequestId, {read_resource, self()}, State#state.pending_requests)
            },
            {noreply, NewState};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call(list_tools, _From, State) ->
    RequestId = State#state.request_id,
    Json = erlmcp_json_rpc:encode_request(RequestId, <<"tools/list">>, #{}),
    case send_message(State, Json) of
        ok ->
            NewState = State#state{
                request_id = RequestId + 1,
                pending_requests = maps:put(RequestId, {list_tools, self()}, State#state.pending_requests)
            },
            {noreply, NewState};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({call_tool, Name, Arguments}, _From, State) ->
    RequestId = State#state.request_id,
    Params = #{
        <<"name">> => Name,
        <<"arguments">> => Arguments
    },
    Json = erlmcp_json_rpc:encode_request(RequestId, <<"tools/call">>, Params),
    case send_message(State, Json) of
        ok ->
            NewState = State#state{
                request_id = RequestId + 1,
                pending_requests = maps:put(RequestId, {call_tool, self()}, State#state.pending_requests)
            },
            {noreply, NewState};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({transport_message, Data}, State) ->
    case erlmcp_json_rpc:decode_message(Data) of
        {ok, #json_rpc_response{id = Id, result = Result}} ->
            handle_response(Id, Result, State);
        {ok, #json_rpc_response{id = Id, error = Error}} ->
            handle_error_response(Id, Error, State);
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

send_message(#state{transport = Transport, transport_state = TransportState}, Message) ->
    Transport:send(TransportState, Message).

encode_capabilities(#mcp_client_capabilities{roots = Roots, sampling = Sampling}) ->
    Caps = #{},
    Caps1 = case Roots of
        #mcp_capability{enabled = true} ->
            Caps#{<<"roots">> => #{<<"listChanged">> => true}};
        _ -> Caps
    end,
    case Sampling of
        #mcp_capability{enabled = true} ->
            Caps1#{<<"sampling">> => #{}};
        _ -> Caps1
    end.

handle_response(Id, Result, State) ->
    case maps:get(Id, State#state.pending_requests, undefined) of
        {initialize, From} ->
            gen_server:reply(From, {ok, Result}),
            NewState = State#state{
                pending_requests = maps:remove(Id, State#state.pending_requests)
            },
            {noreply, NewState};
        {RequestType, From} ->
            gen_server:reply(From, {ok, Result}),
            NewState = State#state{
                pending_requests = maps:remove(Id, State#state.pending_requests)
            },
            {noreply, NewState};
        undefined ->
            error_logger:warning_msg("Received response for unknown request ID: ~p~n", [Id]),
            {noreply, State}
    end.

handle_error_response(Id, Error, State) ->
    case maps:get(Id, State#state.pending_requests, undefined) of
        {_RequestType, From} ->
            gen_server:reply(From, {error, Error}),
            NewState = State#state{
                pending_requests = maps:remove(Id, State#state.pending_requests)
            },
            {noreply, NewState};
        undefined ->
            error_logger:warning_msg("Received error response for unknown request ID: ~p~n", [Id]),
            {noreply, State}
    end.

handle_notification(_Method, _Params, State) ->
    {noreply, State}.
