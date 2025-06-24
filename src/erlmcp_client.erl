-module(erlmcp_client).
-behaviour(gen_server).

-include("erlmcp.hrl").

-export([
    start_link/1, start_link/2,
    initialize/2, initialize/3,
    list_roots/1,
    list_resources/1, list_resource_templates/1,
    read_resource/2, subscribe_to_resource/2, unsubscribe_from_resource/2,
    list_prompts/1, get_prompt/2, get_prompt/3,
    list_tools/1, call_tool/3,
    with_batch/2, send_batch_request/4,
    set_notification_handler/3, remove_notification_handler/2,
    set_sampling_handler/2, remove_sampling_handler/1,
    set_strict_mode/2,
    stop/1
]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
    transport :: module(),
    transport_state :: term(),
    capabilities :: #mcp_server_capabilities{},
    request_id = 1 :: integer(),
    pending_requests = #{} :: map(),
    batch_requests = #{} :: map(),
    notification_handlers = #{} :: map(),
    sampling_handler :: pid() | undefined,
    strict_mode = false :: boolean(),
    subscriptions = #{} :: map(),
    initialized = false :: boolean()
}).

start_link(TransportOpts) ->
    gen_server:start_link(?MODULE, [TransportOpts, #{}], []).

start_link(TransportOpts, Options) ->
    gen_server:start_link(?MODULE, [TransportOpts, Options], []).

initialize(Client, Capabilities) ->
    gen_server:call(Client, {initialize, Capabilities, #{}}).

initialize(Client, Capabilities, Options) ->
    gen_server:call(Client, {initialize, Capabilities, Options}).

list_roots(Client) ->
    gen_server:call(Client, list_roots).

list_resources(Client) ->
    gen_server:call(Client, list_resources).

read_resource(Client, Uri) ->
    gen_server:call(Client, {read_resource, Uri}).

list_prompts(Client) ->
    gen_server:call(Client, list_prompts).

get_prompt(Client, Name) ->
    gen_server:call(Client, {get_prompt, Name, #{}}).

get_prompt(Client, Name, Arguments) ->
    gen_server:call(Client, {get_prompt, Name, Arguments}).

list_tools(Client) ->
    gen_server:call(Client, list_tools).

call_tool(Client, Name, Arguments) ->
    gen_server:call(Client, {call_tool, Name, Arguments}).

stop(Client) ->
    gen_server:stop(Client).

list_resource_templates(Client) ->
    gen_server:call(Client, list_resource_templates).

subscribe_to_resource(Client, Uri) ->
    gen_server:call(Client, {subscribe_resource, Uri}).

unsubscribe_from_resource(Client, Uri) ->
    gen_server:call(Client, {unsubscribe_resource, Uri}).

with_batch(Client, BatchFun) ->
    BatchId = make_ref(),
    gen_server:call(Client, {start_batch, BatchId}),
    try
        Result = BatchFun(BatchId),
        gen_server:call(Client, {execute_batch, BatchId}),
        Result
    catch
        Error:Reason:Stacktrace ->
            gen_server:call(Client, {cancel_batch, BatchId}),
            erlang:raise(Error, Reason, Stacktrace)
    end.

send_batch_request(Client, BatchId, Method, Params) ->
    gen_server:call(Client, {add_to_batch, BatchId, Method, Params}).

set_notification_handler(Client, Method, Handler) ->
    gen_server:call(Client, {set_notification_handler, Method, Handler}).

remove_notification_handler(Client, Method) ->
    gen_server:call(Client, {remove_notification_handler, Method}).

set_sampling_handler(Client, Handler) ->
    gen_server:call(Client, {set_sampling_handler, Handler}).

remove_sampling_handler(Client) ->
    gen_server:call(Client, remove_sampling_handler).

set_strict_mode(Client, Enabled) ->
    gen_server:call(Client, {set_strict_mode, Enabled}).

init([TransportOpts, Options]) ->
    {Transport, TransportState} = init_transport(TransportOpts),
    StrictMode = maps:get(strict_mode, Options, false),
    {ok, #state{
        transport = Transport,
        transport_state = TransportState,
        strict_mode = StrictMode,
        batch_requests = #{},
        notification_handlers = #{},
        subscriptions = #{},
        initialized = false
    }}.

handle_call({initialize, Capabilities, _Options}, _From, State) ->
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
    case validate_capability(State, resources) of
        ok ->
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
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({read_resource, Uri}, _From, State) ->
    case validate_capability(State, resources) of
        ok ->
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
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call(list_tools, _From, State) ->
    case validate_capability(State, tools) of
        ok ->
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
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({call_tool, Name, Arguments}, _From, State) ->
    case validate_capability(State, tools) of
        ok ->
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
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call(list_prompts, _From, State) ->
    case validate_capability(State, prompts) of
        ok ->
            RequestId = State#state.request_id,
            Json = erlmcp_json_rpc:encode_request(RequestId, <<"prompts/list">>, #{}),
            case send_message(State, Json) of
                ok ->
                    NewState = State#state{
                        request_id = RequestId + 1,
                        pending_requests = maps:put(RequestId, {list_prompts, self()}, State#state.pending_requests)
                    },
                    {noreply, NewState};
                {error, Reason} ->
                    {reply, {error, Reason}, State}
            end;
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({get_prompt, Name, Arguments}, _From, State) ->
    case validate_capability(State, prompts) of
        ok ->
            RequestId = State#state.request_id,
            Params = case Arguments of
                #{} when map_size(Arguments) =:= 0 ->
                    #{<<"name">> => Name};
                _ ->
                    #{<<"name">> => Name, <<"arguments">> => Arguments}
            end,
            Json = erlmcp_json_rpc:encode_request(RequestId, <<"prompts/get">>, Params),
            case send_message(State, Json) of
                ok ->
                    NewState = State#state{
                        request_id = RequestId + 1,
                        pending_requests = maps:put(RequestId, {get_prompt, self()}, State#state.pending_requests)
                    },
                    {noreply, NewState};
                {error, Reason} ->
                    {reply, {error, Reason}, State}
            end;
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call(list_resource_templates, _From, State) ->
    case validate_capability(State, resources) of
        ok ->
            RequestId = State#state.request_id,
            Json = erlmcp_json_rpc:encode_request(RequestId, <<"resources/templates/list">>, #{}),
            case send_message(State, Json) of
                ok ->
                    NewState = State#state{
                        request_id = RequestId + 1,
                        pending_requests = maps:put(RequestId, {list_resource_templates, self()}, State#state.pending_requests)
                    },
                    {noreply, NewState};
                {error, Reason} ->
                    {reply, {error, Reason}, State}
            end;
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({subscribe_resource, Uri}, _From, State) ->
    case validate_capability(State, resources) of
        ok ->
            RequestId = State#state.request_id,
            Params = #{<<"uri">> => Uri},
            Json = erlmcp_json_rpc:encode_request(RequestId, <<"resources/subscribe">>, Params),
            case send_message(State, Json) of
                ok ->
                    NewState = State#state{
                        request_id = RequestId + 1,
                        pending_requests = maps:put(RequestId, {subscribe_resource, self()}, State#state.pending_requests),
                        subscriptions = maps:put(Uri, true, State#state.subscriptions)
                    },
                    {noreply, NewState};
                {error, Reason} ->
                    {reply, {error, Reason}, State}
            end;
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({unsubscribe_resource, Uri}, _From, State) ->
    case validate_capability(State, resources) of
        ok ->
            RequestId = State#state.request_id,
            Params = #{<<"uri">> => Uri},
            Json = erlmcp_json_rpc:encode_request(RequestId, <<"resources/unsubscribe">>, Params),
            case send_message(State, Json) of
                ok ->
                    NewState = State#state{
                        request_id = RequestId + 1,
                        pending_requests = maps:put(RequestId, {unsubscribe_resource, self()}, State#state.pending_requests),
                        subscriptions = maps:remove(Uri, State#state.subscriptions)
                    },
                    {noreply, NewState};
                {error, Reason} ->
                    {reply, {error, Reason}, State}
            end;
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({start_batch, BatchId}, _From, State) ->
    NewState = State#state{
        batch_requests = maps:put(BatchId, [], State#state.batch_requests)
    },
    {reply, ok, NewState};

handle_call({add_to_batch, BatchId, Method, Params}, _From, State) ->
    case maps:get(BatchId, State#state.batch_requests, undefined) of
        undefined ->
            {reply, {error, batch_not_found}, State};
        Requests ->
            RequestId = State#state.request_id,
            Request = {RequestId, Method, Params},
            NewRequests = [Request | Requests],
            NewState = State#state{
                request_id = RequestId + 1,
                batch_requests = maps:put(BatchId, NewRequests, State#state.batch_requests)
            },
            {reply, {ok, RequestId}, NewState}
    end;

handle_call({execute_batch, BatchId}, _From, State) ->
    case maps:get(BatchId, State#state.batch_requests, undefined) of
        undefined ->
            {reply, {error, batch_not_found}, State};
        Requests ->
            NewState = State#state{
                batch_requests = maps:remove(BatchId, State#state.batch_requests)
            },
            {reply, {ok, length(Requests)}, NewState}
    end;

handle_call({cancel_batch, BatchId}, _From, State) ->
    NewState = State#state{
        batch_requests = maps:remove(BatchId, State#state.batch_requests)
    },
    {reply, ok, NewState};

handle_call({set_notification_handler, Method, Handler}, _From, State) ->
    NewState = State#state{
        notification_handlers = maps:put(Method, Handler, State#state.notification_handlers)
    },
    {reply, ok, NewState};

handle_call({remove_notification_handler, Method}, _From, State) ->
    NewState = State#state{
        notification_handlers = maps:remove(Method, State#state.notification_handlers)
    },
    {reply, ok, NewState};

handle_call({set_sampling_handler, Handler}, _From, State) ->
    NewState = State#state{
        sampling_handler = Handler
    },
    {reply, ok, NewState};

handle_call(remove_sampling_handler, _From, State) ->
    NewState = State#state{
        sampling_handler = undefined
    },
    {reply, ok, NewState};

handle_call({set_strict_mode, Enabled}, _From, State) ->
    NewState = State#state{
        strict_mode = Enabled
    },
    {reply, ok, NewState};

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
    {erlmcp_transport_tcp, Opts};
init_transport({http, Opts}) ->
    case erlmcp_transport_http:init(Opts) of
        {ok, State} -> {erlmcp_transport_http, State};
        {error, Reason} -> {error, Reason}
    end.

send_message(#state{transport = Transport, transport_state = TransportState}, Message) ->
    Transport:send(TransportState, Message).

encode_capabilities(#mcp_client_capabilities{roots = Roots, sampling = Sampling, experimental = Experimental}) ->
    Caps = #{},
    Caps1 = case Roots of
        #mcp_capability{enabled = true} ->
            Caps#{<<"roots">> => #{<<"listChanged">> => true}};
        _ -> Caps
    end,
    Caps2 = case Sampling of
        #mcp_capability{enabled = true} ->
            Caps1#{<<"sampling">> => #{}};
        _ -> Caps1
    end,
    case Experimental of
        undefined -> Caps2;
        ExpMap when is_map(ExpMap) -> maps:merge(Caps2, ExpMap);
        _ -> Caps2
    end;
encode_capabilities(undefined) ->
    #{}.

validate_capability(State, _Capability) when not State#state.strict_mode ->
    ok;
validate_capability(State, _Capability) when not State#state.initialized ->
    {error, not_initialized};
validate_capability(State, Capability) ->
    check_server_capability(State#state.capabilities, Capability).

check_server_capability(undefined, _Capability) ->
    {error, no_server_capabilities};
check_server_capability(Capabilities, resources) ->
    case Capabilities#mcp_server_capabilities.resources of
        #mcp_capability{enabled = true} -> ok;
        _ -> {error, capability_not_supported}
    end;
check_server_capability(Capabilities, tools) ->
    case Capabilities#mcp_server_capabilities.tools of
        #mcp_capability{enabled = true} -> ok;
        _ -> {error, capability_not_supported}
    end;
check_server_capability(Capabilities, prompts) ->
    case Capabilities#mcp_server_capabilities.prompts of
        #mcp_capability{enabled = true} -> ok;
        _ -> {error, capability_not_supported}
    end;
check_server_capability(_Capabilities, logging) ->
    ok;
check_server_capability(_Capabilities, _Capability) ->
    ok.

extract_server_capabilities(InitResult) ->
    case maps:get(<<"capabilities">>, InitResult, #{}) of
        Caps when is_map(Caps) ->
            #mcp_server_capabilities{
                resources = extract_capability(maps:get(<<"resources">>, Caps, undefined)),
                tools = extract_capability(maps:get(<<"tools">>, Caps, undefined)),
                prompts = extract_capability(maps:get(<<"prompts">>, Caps, undefined)),
                logging = extract_capability(maps:get(<<"logging">>, Caps, undefined))
            };
        _ ->
            undefined
    end.

extract_capability(undefined) ->
    undefined;
extract_capability(Cap) when is_map(Cap) ->
    #mcp_capability{enabled = true};
extract_capability(_) ->
    undefined.

handle_sampling_request(Method, Params, State) ->
    case State#state.sampling_handler of
        undefined ->
            error_logger:warning_msg("Received sampling request but no handler set: ~p~n", [Method]),
            {noreply, State};
        Handler when is_function(Handler) ->
            spawn(fun() -> Handler(Method, Params) end),
            {noreply, State};
        {Module, Function} ->
            spawn(fun() -> Module:Function(Method, Params) end),
            {noreply, State};
        Pid when is_pid(Pid) ->
            Pid ! {sampling_request, Method, Params},
            {noreply, State}
    end.

handle_resource_update(Params, State) ->
    Uri = maps:get(<<"uri">>, Params, undefined),
    case maps:get(Uri, State#state.subscriptions, undefined) of
        undefined ->
            {noreply, State};
        _ ->
            case maps:get(<<"resources/updated">>, State#state.notification_handlers, undefined) of
                undefined ->
                    error_logger:info_msg("Resource updated but no handler: ~p~n", [Uri]),
                    {noreply, State};
                Handler when is_function(Handler) ->
                    spawn(fun() -> Handler(<<"resources/updated">>, Params) end),
                    {noreply, State};
                {Module, Function} ->
                    spawn(fun() -> Module:Function(<<"resources/updated">>, Params) end),
                    {noreply, State}
            end
    end.

handle_resource_list_changed(Params, State) ->
    case maps:get(<<"resources/list_changed">>, State#state.notification_handlers, undefined) of
        undefined ->
            error_logger:info_msg("Resource list changed but no handler~n"),
            {noreply, State};
        Handler when is_function(Handler) ->
            spawn(fun() -> Handler(<<"resources/list_changed">>, Params) end),
            {noreply, State};
        {Module, Function} ->
            spawn(fun() -> Module:Function(<<"resources/list_changed">>, Params) end),
            {noreply, State}
    end.

handle_response(Id, Result, State) ->
    case maps:get(Id, State#state.pending_requests, undefined) of
        {initialize, From} ->
            gen_server:reply(From, {ok, Result}),
            ServerCapabilities = extract_server_capabilities(Result),
            NewState = State#state{
                pending_requests = maps:remove(Id, State#state.pending_requests),
                capabilities = ServerCapabilities,
                initialized = true
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

handle_notification(Method, Params, State) ->
    case Method of
        <<"sampling/createMessage">> ->
            handle_sampling_request(Method, Params, State);
        <<"resources/updated">> ->
            handle_resource_update(Params, State);
        <<"resources/list_changed">> ->
            handle_resource_list_changed(Params, State);
        _ ->
            case maps:get(Method, State#state.notification_handlers, undefined) of
                undefined ->
                    error_logger:info_msg("Unhandled notification: ~p~n", [Method]),
                    {noreply, State};
                Handler when is_function(Handler) ->
                    spawn(fun() -> Handler(Method, Params) end),
                    {noreply, State};
                {Module, Function} ->
                    spawn(fun() -> Module:Function(Method, Params) end),
                    {noreply, State}
            end
    end.
