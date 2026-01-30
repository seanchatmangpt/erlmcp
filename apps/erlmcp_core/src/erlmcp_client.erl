-module(erlmcp_client).
-behaviour(gen_server).

-include("erlmcp.hrl").

%% API exports
-export([
    start_link/1, start_link/2,
    initialize/2, initialize/3,
    list_resources/1, list_resource_templates/1,
    read_resource/2, subscribe_to_resource/2, unsubscribe_from_resource/2,
    list_prompts/1, get_prompt/2, get_prompt/3,
    list_tools/1, call_tool/3,
    complete/3, complete/4,
    with_batch/2, send_batch_request/4,
    set_notification_handler/3, remove_notification_handler/2,
    set_sampling_handler/2, remove_sampling_handler/1,
    stop/1,
    cleanup_stale_correlations/0
]).

%% Test exports
-ifdef(TEST).
-export([encode_capabilities/1]).
-endif.

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Types
-type client() :: pid().
-type transport_opts() :: {stdio, list()} | {tcp, map()} | {http, map()}.
-type client_opts() :: #{
    strict_mode => boolean(),
    timeout => timeout(),
    _ => _
}.
-type request_id() :: pos_integer().
-type batch_id() :: reference().
-type notification_handler() :: fun((binary(), map()) -> any()) | {module(), atom()}.
-type sampling_handler() :: fun((binary(), map()) -> any()) | {module(), atom()} | pid().

-export_type([client/0, transport_opts/0, client_opts/0]).

%% Client lifecycle phase for initialization enforcement
-type client_phase() :: pre_initialization | initializing | initialized | error | closed.

%% State record with better type specifications
-record(state, {
    transport :: module(),
    transport_state :: term(),
    phase = pre_initialization :: client_phase(),
    capabilities :: #mcp_server_capabilities{} | undefined,
    request_id = 1 :: request_id(),
    pending_requests = #{} :: #{request_id() => {atom(), pid()}},
    batch_requests = #{} :: #{batch_id() => [{request_id(), binary(), map()}]},
    notification_handlers = #{} :: #{binary() => notification_handler()},
    sampling_handler :: sampling_handler() | undefined,
    strict_mode = false :: boolean(),
    subscriptions = sets:set() :: sets:set(binary()),
    initialized = false :: boolean(),
    timeout = 5000 :: timeout(),
    last_event_id :: binary() | undefined,
    reconnect_timer :: reference() | undefined,
    auto_reconnect = true :: boolean(),
    active_handlers = [] :: [pid()],  % Track supervised handler PIDs for cleanup
    correlation_table :: ets:tid() | undefined  % ETS table for persistent correlation storage
}).

-type state() :: #state{}.

%% Macros for common patterns
-define(CALL_TIMEOUT(State), State#state.timeout).
-define(IS_INITIALIZED(State), State#state.initialized).
-define(CHECK_CAPABILITY(State, Cap),
    case validate_capability(State, Cap) of
        ok -> do_request;
        {error, _} = Error -> Error
    end).

%% Phase enforcement macro - check if client is initialized
-define(CHECK_INITIALIZED(State, From),
    case State#state.phase of
        initialized -> ok;
        Phase -> {reply, {error, {not_initialized, Phase, <<"Client not initialized">>}}, State}
    end).

%%====================================================================
%% API Functions
%%====================================================================

-spec start_link(transport_opts()) -> {ok, client()} | {error, term()}.
start_link(TransportOpts) ->
    start_link(TransportOpts, #{}).

-spec start_link(transport_opts(), client_opts()) -> {ok, client()} | {error, term()}.
start_link(TransportOpts, Options) ->
    gen_server:start_link(?MODULE, [TransportOpts, Options], []).

-spec initialize(client(), #mcp_client_capabilities{}) ->
    {ok, map()} | {error, term()}.
initialize(Client, Capabilities) ->
    initialize(Client, Capabilities, #{}).

-spec initialize(client(), #mcp_client_capabilities{}, map()) ->
    {ok, map()} | {error, term()}.
initialize(Client, Capabilities, Options) ->
    gen_server:call(Client, {initialize, Capabilities, Options}, infinity).

-spec list_resources(client()) -> {ok, [map()]} | {error, term()}.
list_resources(Client) ->
    gen_server:call(Client, list_resources).

-spec read_resource(client(), binary()) -> {ok, map()} | {error, term()}.
read_resource(Client, Uri) when is_binary(Uri) ->
    gen_server:call(Client, {read_resource, Uri}).

-spec list_prompts(client()) -> {ok, [map()]} | {error, term()}.
list_prompts(Client) ->
    gen_server:call(Client, list_prompts).

-spec get_prompt(client(), binary()) -> {ok, map()} | {error, term()}.
get_prompt(Client, Name) when is_binary(Name) ->
    get_prompt(Client, Name, #{}).

-spec get_prompt(client(), binary(), map()) -> {ok, map()} | {error, term()}.
get_prompt(Client, Name, Arguments) when is_binary(Name), is_map(Arguments) ->
    gen_server:call(Client, {get_prompt, Name, Arguments}).

-spec list_tools(client()) -> {ok, [map()]} | {error, term()}.
list_tools(Client) ->
    gen_server:call(Client, list_tools).

-spec call_tool(client(), binary(), map()) -> {ok, map()} | {error, term()}.
call_tool(Client, Name, Arguments) when is_binary(Name), is_map(Arguments) ->
    gen_server:call(Client, {call_tool, Name, Arguments}).

-spec complete(client(), binary(), binary()) -> {ok, map()} | {error, term()}.
complete(Client, Ref, Argument) ->
    complete(Client, Ref, Argument, 5000).

-spec complete(client(), binary(), binary(), timeout()) -> {ok, map()} | {error, term()}.
complete(Client, Ref, Argument, Timeout)
  when is_binary(Ref), is_binary(Argument), is_integer(Timeout) ->
    gen_server:call(Client, {complete, Ref, Argument}, Timeout).

-spec stop(client()) -> ok.
stop(Client) ->
    gen_server:stop(Client).

-spec list_resource_templates(client()) -> {ok, [map()]} | {error, term()}.
list_resource_templates(Client) ->
    gen_server:call(Client, list_resource_templates).

-spec subscribe_to_resource(client(), binary()) -> ok | {error, term()}.
subscribe_to_resource(Client, Uri) when is_binary(Uri) ->
    gen_server:call(Client, {subscribe_resource, Uri}).

-spec unsubscribe_from_resource(client(), binary()) -> ok | {error, term()}.
unsubscribe_from_resource(Client, Uri) when is_binary(Uri) ->
    gen_server:call(Client, {unsubscribe_resource, Uri}).

-spec with_batch(client(), fun((batch_id()) -> Result)) -> Result when Result :: term().
with_batch(Client, BatchFun) when is_function(BatchFun, 1) ->
    BatchId = make_ref(),
    ok = gen_server:call(Client, {start_batch, BatchId}),
    try
        Result = BatchFun(BatchId),
        {ok, _Count} = gen_server:call(Client, {execute_batch, BatchId}),
        Result
    catch
        Class:Reason:Stacktrace ->
            gen_server:call(Client, {cancel_batch, BatchId}),
            erlang:raise(Class, Reason, Stacktrace)
    end.

-spec send_batch_request(client(), batch_id(), binary(), map()) ->
    {ok, request_id()} | {error, term()}.
send_batch_request(Client, BatchId, Method, Params)
  when is_reference(BatchId), is_binary(Method), is_map(Params) ->
    gen_server:call(Client, {add_to_batch, BatchId, Method, Params}).

-spec set_notification_handler(client(), binary(), notification_handler()) -> ok.
set_notification_handler(Client, Method, Handler) when is_binary(Method) ->
    gen_server:call(Client, {set_notification_handler, Method, Handler}).

-spec remove_notification_handler(client(), binary()) -> ok.
remove_notification_handler(Client, Method) when is_binary(Method) ->
    gen_server:call(Client, {remove_notification_handler, Method}).

-spec set_sampling_handler(client(), sampling_handler()) -> ok.
set_sampling_handler(Client, Handler) ->
    gen_server:call(Client, {set_sampling_handler, Handler}).

-spec remove_sampling_handler(client()) -> ok.
remove_sampling_handler(Client) ->
    gen_server:call(Client, remove_sampling_handler).

%%====================================================================
%% gen_server callbacks
%%====================================================================

-spec init([transport_opts() | client_opts()]) -> {ok, state()}.
init([TransportOpts, Options]) ->
    process_flag(trap_exit, true),
    %% Initialize ETS table for persistent correlation storage
    CorrelationTable = case ets:info(erlmcp_correlation_table) of
        undefined ->
            ets:new(erlmcp_correlation_table, [
                named_table,
                set,
                public,
                {read_concurrency, true},
                {write_concurrency, true}
            ]);
        _ ->
            erlmcp_correlation_table
    end,
    case init_transport(TransportOpts) of
        {ok, Transport, TransportState} ->
            State = #state{
                transport = Transport,
                transport_state = TransportState,
                strict_mode = maps:get(strict_mode, Options, false),
                timeout = maps:get(timeout, Options, 5000),
                subscriptions = sets:new(),
                correlation_table = CorrelationTable
            },
            %% Recover pending correlations from ETS table on initialization
            State2 = recover_correlations(State),
            {ok, State2};
        {error, Reason} ->
            {stop, Reason}
    end.

-spec handle_call(term(), {pid(), term()}, state()) ->
    {reply, term(), state()} |
    {noreply, state()} |
    {stop, term(), term(), state()}.

%% Initialize must be called during pre_initialization phase
handle_call({initialize, Capabilities, _Options}, From, #state{phase = pre_initialization} = State) ->
    Request = build_initialize_request(Capabilities),
    NewState = State#state{phase = initializing},
    %% Use timeout to prevent hanging tests
    {ok, NewState2} = send_request(NewState, <<"initialize">>, Request, {initialize, From}),
    {noreply, NewState2};

%% Initialize not allowed in other phases
handle_call({initialize, _Capabilities, _Options}, From, #state{phase = Phase} = State) ->
    gen_server:reply(From, {error, {invalid_phase, Phase, <<"Initialize must be called in pre_initialization phase">>}}),
    {noreply, State};

%% All capability requests require initialized phase
handle_call(list_resources, From, #state{phase = initialized} = State) ->
    case ?CHECK_CAPABILITY(State, resources) of
        do_request ->
            {ok, NewState} = send_request(State, <<"resources/list">>, #{}, {list_resources, From}),
            {noreply, NewState};
        {error, _} = ErrorTuple ->
            {reply, ErrorTuple, State}
    end;

handle_call(list_resources, From, #state{phase = Phase} = State) ->
    gen_server:reply(From, {error, {not_initialized, Phase, <<"Server must complete initialization first">>}}),
    {noreply, State};

%% Read resource requires initialized phase
handle_call({read_resource, Uri}, From, #state{phase = initialized} = State) ->
    case ?CHECK_CAPABILITY(State, resources) of
        do_request ->
            Params = #{<<"uri">> => Uri},
            {ok, NewState} = send_request(State, <<"resources/read">>, Params, {read_resource, From}),
            {noreply, NewState};
        {error, _} = ErrorTuple ->
            {reply, ErrorTuple, State}
    end;

handle_call({read_resource, _Uri}, From, #state{phase = Phase} = State) ->
    gen_server:reply(From, {error, {not_initialized, Phase, <<"Client not initialized">>}}),
    {noreply, State};

%% List tools requires initialized phase
handle_call(list_tools, From, #state{phase = initialized} = State) ->
    case ?CHECK_CAPABILITY(State, tools) of
        do_request ->
            {ok, NewState} = send_request(State, <<"tools/list">>, #{}, {list_tools, From}),
            {noreply, NewState};
        {error, _} = ErrorTuple ->
            {reply, ErrorTuple, State}
    end;

handle_call(list_tools, From, #state{phase = Phase} = State) ->
    gen_server:reply(From, {error, {not_initialized, Phase, <<"Client not initialized">>}}),
    {noreply, State};

%% Call tool requires initialized phase
handle_call({call_tool, Name, Arguments}, From, #state{phase = initialized} = State) ->
    case ?CHECK_CAPABILITY(State, tools) of
        do_request ->
            Params = #{<<"name">> => Name, <<"arguments">> => Arguments},
            {ok, NewState} = send_request(State, <<"tools/call">>, Params, {call_tool, From}),
            {noreply, NewState};
        {error, _} = ErrorTuple ->
            {reply, ErrorTuple, State}
    end;

handle_call({call_tool, _, _}, From, #state{phase = Phase} = State) ->
    gen_server:reply(From, {error, {not_initialized, Phase, <<"Client not initialized">>}}),
    {noreply, State};

%% Complete requires initialized phase
handle_call({complete, Ref, Argument}, From, #state{phase = initialized} = State) ->
    case ?CHECK_CAPABILITY(State, completions) of
        do_request ->
            Params = #{
                <<"ref">> => Ref,
                <<"argument">> => #{
                    <<"name">> => Argument,
                    <<"value">> => <<>>
                }
            },
            {ok, NewState} = send_request(State, <<"completion/complete">>, Params, {complete, From}),
            {noreply, NewState};
        {error, _} = ErrorTuple ->
            {reply, ErrorTuple, State}
    end;

handle_call({complete, _Ref, _Argument}, From, #state{phase = Phase} = State) ->
    gen_server:reply(From, {error, {not_initialized, Phase, <<"Client not initialized">>}}),
    {noreply, State};

%% List prompts requires initialized phase
handle_call(list_prompts, From, #state{phase = initialized} = State) ->
    case ?CHECK_CAPABILITY(State, prompts) of
        do_request ->
            {ok, NewState} = send_request(State, <<"prompts/list">>, #{}, {list_prompts, From}),
            {noreply, NewState};
        {error, _} = ErrorTuple ->
            {reply, ErrorTuple, State}
    end;

handle_call(list_prompts, From, #state{phase = Phase} = State) ->
    gen_server:reply(From, {error, {not_initialized, Phase, <<"Client not initialized">>}}),
    {noreply, State};

%% Get prompt requires initialized phase
handle_call({get_prompt, Name, Arguments}, From, #state{phase = initialized} = State) ->
    case ?CHECK_CAPABILITY(State, prompts) of
        do_request ->
            Params = build_prompt_params(Name, Arguments),
            {ok, NewState} = send_request(State, <<"prompts/get">>, Params, {get_prompt, From}),
            {noreply, NewState};
        {error, _} = ErrorTuple ->
            {reply, ErrorTuple, State}
    end;

handle_call({get_prompt, _, _}, From, #state{phase = Phase} = State) ->
    gen_server:reply(From, {error, {not_initialized, Phase, <<"Client not initialized">>}}),
    {noreply, State};

%% List resource templates requires initialized phase
handle_call(list_resource_templates, From, #state{phase = initialized} = State) ->
    case ?CHECK_CAPABILITY(State, resources) of
        do_request ->
            {ok, NewState} = send_request(State, <<"resources/templates/list">>, #{}, {list_resource_templates, From}),
            {noreply, NewState};
        {error, _} = ErrorTuple ->
            {reply, ErrorTuple, State}
    end;

handle_call(list_resource_templates, From, #state{phase = Phase} = State) ->
    gen_server:reply(From, {error, {not_initialized, Phase, <<"Client not initialized">>}}),
    {noreply, State};

%% Subscribe resource requires initialized phase
handle_call({subscribe_resource, Uri}, From, #state{phase = initialized} = State) ->
    case ?CHECK_CAPABILITY(State, resources) of
        do_request ->
            Params = #{<<"uri">> => Uri},
            NewState = State#state{
                subscriptions = sets:add_element(Uri, State#state.subscriptions)
            },
            {ok, NewState2} = send_request(NewState, <<"resources/subscribe">>, Params, {subscribe_resource, From}),
            {noreply, NewState2};
        {error, _} = ErrorTuple ->
            {reply, ErrorTuple, State}
    end;

handle_call({subscribe_resource, _Uri}, From, #state{phase = Phase} = State) ->
    gen_server:reply(From, {error, {not_initialized, Phase, <<"Client not initialized">>}}),
    {noreply, State};

%% Unsubscribe resource requires initialized phase
handle_call({unsubscribe_resource, Uri}, From, #state{phase = initialized} = State) ->
    case ?CHECK_CAPABILITY(State, resources) of
        do_request ->
            Params = #{<<"uri">> => Uri},
            NewState = State#state{
                subscriptions = sets:del_element(Uri, State#state.subscriptions)
            },
            {ok, NewState2} = send_request(NewState, <<"resources/unsubscribe">>, Params, {unsubscribe_resource, From}),
            {noreply, NewState2};
        {error, _} = ErrorTuple ->
            {reply, ErrorTuple, State}
    end;

handle_call({unsubscribe_resource, _Uri}, From, #state{phase = Phase} = State) ->
    gen_server:reply(From, {error, {not_initialized, Phase, <<"Client not initialized">>}}),
    {noreply, State};

handle_call({start_batch, BatchId}, _From, State) ->
    NewBatches = maps:put(BatchId, [], State#state.batch_requests),
    {reply, ok, State#state{batch_requests = NewBatches}};

handle_call({add_to_batch, BatchId, Method, Params}, _From, State) ->
    case maps:find(BatchId, State#state.batch_requests) of
        {ok, Requests} ->
            RequestId = State#state.request_id,
            %% P0 SECURITY: Safe request ID handling for batch requests
            case erlmcp_request_id:safe_increment(RequestId) of
                {error, overflow} ->
                    logger:error("Request ID space exhausted at ID ~w. Reconnection required.", [RequestId]),
                    {reply, {error, {request_id_overflow,
                        <<"Request ID space exhausted. Reconnection required.">>}}, State};
                {ok, NextRequestId} ->
                    Request = {RequestId, Method, Params},
                    NewRequests = [Request | Requests],
                    NewState = State#state{
                        request_id = NextRequestId,
                        batch_requests = maps:put(BatchId, NewRequests, State#state.batch_requests)
                    },
                    {reply, {ok, RequestId}, NewState}
            end;
        error ->
            {reply, {error, batch_not_found}, State}
    end;

handle_call({execute_batch, BatchId}, _From, State) ->
    case maps:take(BatchId, State#state.batch_requests) of
        {Requests, NewBatches} ->
            NewState = State#state{batch_requests = NewBatches},
            execute_batch_requests(lists:reverse(Requests), NewState),
            {reply, {ok, length(Requests)}, NewState};
        error ->
            {reply, {error, batch_not_found}, State}
    end;

handle_call({cancel_batch, BatchId}, _From, State) ->
    NewBatches = maps:remove(BatchId, State#state.batch_requests),
    {reply, ok, State#state{batch_requests = NewBatches}};

handle_call({set_notification_handler, Method, Handler}, _From, State) ->
    NewHandlers = maps:put(Method, Handler, State#state.notification_handlers),
    {reply, ok, State#state{notification_handlers = NewHandlers}};

handle_call({remove_notification_handler, Method}, _From, State) ->
    NewHandlers = maps:remove(Method, State#state.notification_handlers),
    {reply, ok, State#state{notification_handlers = NewHandlers}};

handle_call({set_sampling_handler, Handler}, _From, State) ->
    {reply, ok, State#state{sampling_handler = Handler}};

handle_call(remove_sampling_handler, _From, State) ->
    {reply, ok, State#state{sampling_handler = undefined}};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info({transport_message, Data}, State) ->
    case erlmcp_json_rpc:decode_message(Data) of
        {ok, #json_rpc_response{id = Id, result = Result, error = undefined}} ->
            handle_response(Id, {ok, Result}, State);
        {ok, #json_rpc_response{id = Id, error = Error}} ->
            handle_response(Id, {error, Error}, State);
        {ok, #json_rpc_notification{method = Method, params = Params}} ->
            handle_notification(Method, Params, State);
        {error, Reason} ->
            logger:error("Failed to decode message: ~p", [Reason]),
            {noreply, State}
    end;

handle_info({'EXIT', Pid, Reason}, State) when Pid =:= State#state.transport_state ->
    logger:error("Transport process died: ~p", [Reason]),
    {stop, {transport_died, Reason}, State};

%% Handle initialization timeout
handle_info(initialization_timeout, #state{phase = initializing} = State) ->
    logger:error("Initialization timeout - server did not send notifications/initialized within ~p ms",
                 [State#state.timeout]),
    {stop, initialization_timeout, State};

%% Ignore initialization timeout if already initialized
handle_info(initialization_timeout, State) ->
    logger:debug("Ignoring initialization timeout - already in phase ~p", [State#state.phase]),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, State) ->
    close_transport(State),
    %% Clean up stale correlations on termination
    cleanup_stale_correlations(),
    %% Note: Active handlers are transient and will be cleaned up by their supervisor
    %% No manual cleanup needed for supervised processes
    ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

-spec init_transport(transport_opts()) ->
    {ok, module(), term()} | {error, term()}.
init_transport({stdio, Opts}) when is_list(Opts); is_map(Opts) ->
    %% For stdio, use self() as the transport process (simplest for testing)
    %% In production, this would be erlmcp_transport_stdio from erlmcp_transports
    {ok, erlmcp_client_transport, self()};
init_transport({stdio, Opts, _Options}) when is_list(Opts); is_map(Opts) ->
    %% Handle case when options are passed as third element
    {ok, erlmcp_client_transport, self()};
init_transport({tcp, Opts}) when is_map(Opts) ->
    {ok, erlmcp_client_transport, Opts};
init_transport({http, Opts}) when is_map(Opts) ->
    %% HTTP requires special handling - return simple placeholder for now
    {ok, erlmcp_client_transport, #{type => http, opts => Opts}}.

-spec close_transport(state()) -> ok.
close_transport(#state{transport = Transport, transport_state = TransportState}) ->
    catch Transport:close(TransportState),
    ok.

-spec send_request(state(), binary(), map(), {atom(), pid()}) ->
    {ok, state()} | {error, term()}.
send_request(State, Method, Params, RequestInfo) ->
    RequestId = State#state.request_id,
    {_, FromPid} = RequestInfo,

    %% P0 SECURITY: Safe request ID handling with overflow detection
    %% Check ID space usage before increment to prevent overflow
    case erlmcp_request_id:safe_increment(RequestId) of
        {error, overflow} ->
            %% ID space exhausted - reply once and return error
            logger:error("Request ID space exhausted at ID ~w. Reconnection required.", [RequestId]),
            gen_server:reply(FromPid, {error, {request_id_overflow,
                <<"Request ID space exhausted. Reconnection required.">>}}),
            {error, request_id_exhausted};
        {ok, SafeNextId} ->
            %% Check thresholds for monitoring and auto-reconnection
            case erlmcp_request_id:check_thresholds(SafeNextId) of
                {ok, reserved, Usage} ->
                    logger:warning("Request ID space at reserved level: ~.2%. Reconnection recommended.", [Usage]);
                {ok, critical, Usage} ->
                    logger:warning("Request ID space at critical level: ~.2%. Schedule reconnection.", [Usage]);
                {ok, warning, Usage} ->
                    logger:info("Request ID space at warning level: ~.2%. Monitor usage.", [Usage]);
                {ok, normal, _Usage} ->
                    ok
            end,

            %% Verify no ID collision in pending requests (safety check)
            case maps:is_key(RequestId, State#state.pending_requests) of
                true ->
                    logger:error("CRITICAL: Request ID collision detected for ID ~w", [RequestId]),
                    gen_server:reply(FromPid, {error, {request_id_collision,
                        <<"Internal error: request ID collision">>}}),
                    {error, request_id_collision};
                false ->
                    Json = erlmcp_json_rpc:encode_request(RequestId, Method, Params),
                    case send_message(State, Json) of
                        ok ->
                            %% Store correlation in ETS for persistence across reconnection
                            store_correlation(State#state.correlation_table, RequestId, RequestInfo, Method),
                            NewState = State#state{
                                request_id = SafeNextId,
                                pending_requests = maps:put(RequestId, RequestInfo, State#state.pending_requests)
                            },
                            {ok, NewState};
                        {error, Reason} ->
                            gen_server:reply(FromPid, {error, Reason}),
                            {error, Reason}
                    end
            end
    end.

-spec send_message(state(), binary()) -> ok | {error, term()}.
send_message(#state{transport = Transport, transport_state = TransportState}, Message) ->
    Transport:send(TransportState, Message).

-spec build_initialize_request(#mcp_client_capabilities{}) -> map().
build_initialize_request(Capabilities) ->
    #{
        <<"protocolVersion">> => ?MCP_VERSION,
        <<"capabilities">> => encode_capabilities(Capabilities),
        <<"clientInfo">> => #{
            <<"name">> => <<"erlmcp">>,
            <<"version">> => <<"0.1.0">>
        }
    }.

-spec build_prompt_params(binary(), map()) -> map().
build_prompt_params(Name, Arguments) when map_size(Arguments) =:= 0 ->
    #{<<"name">> => Name};
build_prompt_params(Name, Arguments) ->
    #{<<"name">> => Name, <<"arguments">> => Arguments}.

-spec encode_capabilities(#mcp_client_capabilities{} | {binary(), binary()} | map()) -> map().
%% Handle tuple format {Name, Version}
encode_capabilities({Name, Version}) when is_binary(Name), is_binary(Version) ->
    #{name => Name, version => Version};
%% Handle map with name/version keys
encode_capabilities(#{name := Name, version := Version} = Caps)
  when is_binary(Name), is_binary(Version) ->
    Caps;
%% Handle plain map (pass through)
encode_capabilities(Caps) when is_map(Caps), not is_record(Caps, mcp_client_capabilities) ->
    Caps;
%% Handle MCP client capabilities record
encode_capabilities(#mcp_client_capabilities{} = Caps) ->
    Base = #{},
    Base1 = maybe_add_capability(Base, <<"roots">>, Caps#mcp_client_capabilities.roots),
    Base2 = maybe_add_capability(Base1, <<"sampling">>, Caps#mcp_client_capabilities.sampling),
    maybe_merge_experimental(Base2, Caps#mcp_client_capabilities.experimental).

-spec maybe_add_capability(map(), binary(), #mcp_capability{} | undefined) -> map().
maybe_add_capability(Map, _Key, undefined) ->
    Map;
maybe_add_capability(Map, Key, #mcp_capability{enabled = true}) ->
    Map#{Key => #{}};
maybe_add_capability(Map, _Key, _) ->
    Map.

-spec maybe_merge_experimental(map(), map() | undefined) -> map().
maybe_merge_experimental(Map, undefined) ->
    Map;
maybe_merge_experimental(Map, Experimental) when is_map(Experimental) ->
    maps:merge(Map, Experimental);
maybe_merge_experimental(Map, _) ->
    Map.

-spec validate_capability(state(), atom()) -> ok | {error, term()}.
validate_capability(#state{strict_mode = false}, _) ->
    ok;
validate_capability(#state{initialized = false}, _) ->
    {error, not_initialized};
validate_capability(#state{capabilities = undefined}, _) ->
    {error, no_server_capabilities};
validate_capability(#state{capabilities = Caps}, Capability) ->
    check_server_capability(Caps, Capability).

-spec check_server_capability(#mcp_server_capabilities{}, atom()) ->
    ok | {error, capability_not_supported}.
check_server_capability(Caps, resources) ->
    check_capability_enabled(Caps#mcp_server_capabilities.resources);
check_server_capability(Caps, tools) ->
    check_capability_enabled(Caps#mcp_server_capabilities.tools);
check_server_capability(Caps, prompts) ->
    check_capability_enabled(Caps#mcp_server_capabilities.prompts);
check_server_capability(Caps, completions) ->
    case Caps#mcp_server_capabilities.experimental of
        #{<<"completions">> := _} -> ok;
        _ -> {error, capability_not_supported}
    end;
check_server_capability(_Caps, _) ->
    ok.

-spec check_capability_enabled(#mcp_capability{} | undefined) ->
    ok | {error, capability_not_supported}.
check_capability_enabled(#mcp_capability{enabled = true}) ->
    ok;
check_capability_enabled(_) ->
    {error, capability_not_supported}.

-spec handle_response(request_id(), {ok, map()} | {error, map()}, state()) ->
    {noreply, state()}.
handle_response(Id, Result, State) ->
    case maps:take(Id, State#state.pending_requests) of
        {{initialize, From}, NewPending} ->
            gen_server:reply(From, Result),
            %% Remove from persistent ETS storage
            delete_correlation(State#state.correlation_table, Id),
            case Result of
                {ok, InitResult} ->
                    ServerCapabilities = extract_server_capabilities(InitResult),
                    %% Stay in initializing phase until server sends notifications/initialized
                    %% Do NOT set initialized=true yet - wait for the notification
                    NewState = State#state{
                        pending_requests = NewPending,
                        capabilities = ServerCapabilities,
                        phase = initializing
                    },
                    %% Set initialization timeout to prevent hanging
                    InitTimeout = State#state.timeout,
                    erlang:send_after(InitTimeout, self(), initialization_timeout),
                    {noreply, NewState};
                {error, _} ->
                    %% Go to error phase on initialization failure
                    NewState = State#state{
                        pending_requests = NewPending,
                        phase = error
                    },
                    {noreply, NewState}
            end;
        {{_RequestType, From}, NewPending} ->
            gen_server:reply(From, Result),
            %% Remove from persistent ETS storage
            delete_correlation(State#state.correlation_table, Id),
            {noreply, State#state{pending_requests = NewPending}};
        error ->
            logger:warning("Received response for unknown request ID: ~p", [Id]),
            {noreply, State}
    end.

-spec extract_server_capabilities(map()) -> #mcp_server_capabilities{} | undefined.
extract_server_capabilities(InitResult) ->
    case maps:get(<<"capabilities">>, InitResult, undefined) of
        undefined ->
            undefined;
        Caps when is_map(Caps) ->
            #mcp_server_capabilities{
                resources = extract_capability(maps:get(<<"resources">>, Caps, undefined)),
                tools = extract_capability(maps:get(<<"tools">>, Caps, undefined)),
                prompts = extract_capability(maps:get(<<"prompts">>, Caps, undefined)),
                logging = extract_capability(maps:get(<<"logging">>, Caps, undefined))
            }
    end.

-spec extract_capability(map() | undefined) -> #mcp_capability{} | undefined.
extract_capability(undefined) ->
    undefined;
extract_capability(Cap) when is_map(Cap) ->
    #mcp_capability{enabled = true};
extract_capability(_) ->
    undefined.

-spec handle_notification(binary(), map(), state()) -> {noreply, state()}.

%% INITIALIZED notification transitions to initialized phase
handle_notification(<<"notifications/initialized">> = Method, _Params, #state{phase = initializing} = State) ->
    logger:info("Client received initialized notification, transitioning to initialized phase"),
    %% Cancel the initialization timeout since we received the notification
    %% (We can't cancel send_after messages easily, so we handle them in handle_info)
    {noreply, State#state{phase = initialized, initialized = true}};

%% Ignore notifications/initialized if not in initializing phase (already initialized)
handle_notification(<<"notifications/initialized">> = Method, _Params, State) ->
    logger:debug("Received notifications/initialized in phase ~p, ignoring", [State#state.phase]),
    {noreply, State};

handle_notification(<<"sampling/createMessage">> = Method, Params, State) ->
    spawn_handler(State#state.sampling_handler, Method, Params),
    {noreply, State};

handle_notification(<<"resources/updated">> = Method, Params, State) ->
    case maps:get(<<"uri">>, Params, undefined) of
        undefined ->
            {noreply, State};
        Uri ->
            case sets:is_element(Uri, State#state.subscriptions) of
                true ->
                    invoke_notification_handler(Method, Params, State);
                false ->
                    {noreply, State}
            end
    end;

handle_notification(<<"resources/list_changed">> = Method, Params, State) ->
    invoke_notification_handler(Method, Params, State);

handle_notification(Method, Params, State) ->
    invoke_notification_handler(Method, Params, State).

-spec invoke_notification_handler(binary(), map(), state()) -> {noreply, state()}.
invoke_notification_handler(Method, Params, State) ->
    case maps:get(Method, State#state.notification_handlers, undefined) of
        undefined ->
            logger:info("Unhandled notification: ~p", [Method]),
            {noreply, State};
        Handler ->
            spawn_handler(Handler, Method, Params),
            {noreply, State}
    end.

-spec spawn_handler(notification_handler() | undefined, binary(), map()) -> ok.
spawn_handler(undefined, Method, _Params) ->
    logger:warning("No handler for: ~p", [Method]),
    ok;
spawn_handler(Handler, Method, Params) when is_function(Handler, 2); is_tuple(Handler) ->
    %% Use supervised handler for functions and MFA tuples
    ParamsWithClient = Params#{<<"client_pid">> => self()},
    case erlmcp_notification_handler_sup:start_handler(Method, Handler, ParamsWithClient) of
        {ok, HandlerPid} ->
            logger:debug("Started supervised notification handler ~p for ~p", [HandlerPid, Method]),
            ok;
        {error, Reason} ->
            logger:error("Failed to start supervised handler for ~p: ~p", [Method, Reason]),
            error
    end;
spawn_handler(Pid, Method, Params) when is_pid(Pid) ->
    %% Direct send to existing process (no supervision needed)
    Pid ! {sampling_request, Method, Params},
    ok.

-spec execute_batch_requests([{request_id(), binary(), map()}], state()) -> ok.
execute_batch_requests([], _State) ->
    ok;
execute_batch_requests([{RequestId, Method, Params} | Rest], State) ->
    Json = erlmcp_json_rpc:encode_request(RequestId, Method, Params),
    case send_message(State, Json) of
        ok ->
            execute_batch_requests(Rest, State);
        {error, Reason} ->
            logger:warning("Failed to send batch request ~p: ~p", [RequestId, Reason]),
            execute_batch_requests(Rest, State)
    end.

%%====================================================================
%% Correlation Persistence Functions
%%====================================================================

%% @doc Store correlation in ETS for persistence across reconnection
-spec store_correlation(ets:tid() | undefined, request_id(), {atom(), pid()}, binary()) -> true.
store_correlation(undefined, _RequestId, _RequestInfo, _Method) ->
    true;
store_correlation(Table, RequestId, {RequestType, FromPid}, Method) ->
    CorrelationData = #{
        timestamp => erlang:system_time(millisecond),
        request_type => RequestType,
        from_pid => FromPid,
        method => Method
    },
    ets:insert(Table, {RequestId, CorrelationData}),
    logger:debug("Stored correlation for request ID ~w: ~p", [RequestId, CorrelationData]),
    true.

%% @doc Delete correlation from ETS table
-spec delete_correlation(ets:tid() | undefined, request_id()) -> true.
delete_correlation(undefined, _RequestId) ->
    true;
delete_correlation(Table, RequestId) ->
    ets:delete(Table, RequestId),
    logger:debug("Deleted correlation for request ID ~w", [RequestId]),
    true.

%% @doc Recover pending correlations from ETS table on reconnection
-spec recover_correlations(state()) -> state().
recover_correlations(#state{correlation_table = undefined} = State) ->
    State;
recover_correlations(#state{correlation_table = Table, pending_requests = Pending} = State) ->
    Now = erlang:system_time(millisecond),
    %% Get all correlations from ETS table
    Correlations = ets:tab2list(Table),
    %% Filter out stale correlations (> 5 minutes old) and dead PIDs
    ValidCorrelations = lists:filter(
        fun({_RequestId, #{timestamp := TS, from_pid := Pid}}) ->
            Age = Now - TS,
            Age < 300000 andalso is_process_alive(Pid)
        end,
        Correlations
    ),
    %% Update pending requests map with recovered correlations
    RecoveredPending = lists:foldl(
        fun({RequestId, #{request_type := Type, from_pid := Pid}}, Acc) ->
            maps:put(RequestId, {Type, Pid}, Acc)
        end,
        Pending,
        ValidCorrelations
    ),
    logger:info("Recovered ~p correlations from ETS table", [length(ValidCorrelations)]),
    State#state{pending_requests = RecoveredPending}.

%% @doc Clean up stale correlations older than 5 minutes
-spec cleanup_stale_correlations() -> non_neg_integer().
cleanup_stale_correlations() ->
    case ets:info(erlmcp_correlation_table) of
        undefined ->
            logger:warning("Correlation table not found, skipping cleanup"),
            0;
        _ ->
            Now = erlang:system_time(millisecond),
            DeletedCount = ets:select_delete(erlmcp_correlation_table,
                fun({_RequestId, #{timestamp := TS}}) when Now - TS > 300000 ->
                    true;
                   (_) ->
                    false
                end),
            logger:info("Cleaned up ~p stale correlations", [DeletedCount]),
            DeletedCount
    end.
