%%%-------------------------------------------------------------------
%%% @doc
%%% HTTP Transport Implementation
%%% 
%%% This module implements HTTP transport for MCP.
%%% It provides bidirectional communication through HTTP requests/responses.
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_transport_http_new).
-behaviour(gen_server).

-include("erlmcp.hrl").
-include_lib("kernel/include/logger.hrl").

%% API
-export([
    start_link/2,
    send/2,
    close/1,
    get_info/1
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% State record
-record(state, {
    transport_id :: atom(),
    config :: map(),
    port :: integer(),
    path :: string(),
    ssl_enabled :: boolean(),
    http_pid :: pid() | undefined,
    registry_pid :: pid() | undefined,
    test_mode :: boolean()
}).

-type state() :: #state{}.

%%====================================================================
%% API Functions
%%====================================================================

-spec start_link(atom(), map()) -> {ok, pid()} | {error, term()}.
start_link(TransportId, Config) ->
    ?LOG_INFO("Starting HTTP transport: ~p with config: ~p", [TransportId, Config]),
    gen_server:start_link(?MODULE, [TransportId, Config], []).

-spec send(pid() | state(), binary() | string()) -> ok | {error, term()}.
send(Pid, Data) when is_pid(Pid) ->
    gen_server:call(Pid, {send, Data});
send(#state{} = State, Data) ->
    send_data(State, Data).

-spec close(pid() | state()) -> ok.
close(Pid) when is_pid(Pid) ->
    gen_server:call(Pid, close);
close(#state{} = State) ->
    stop_http_server(State).

-spec get_info(pid() | state()) -> map().
get_info(Pid) when is_pid(Pid) ->
    gen_server:call(Pid, get_info);
get_info(#state{transport_id = Id, config = Config, port = Port, path = Path, 
                ssl_enabled = SSL, test_mode = TestMode}) ->
    #{
        transport_id => Id,
        type => http,
        config => Config,
        port => Port,
        path => Path,
        ssl_enabled => SSL,
        test_mode => TestMode,
        status => running
    }.

%%====================================================================
%% gen_server callbacks
%%====================================================================

-spec init([atom() | map()]) -> {ok, state()} | {error, term()}.
init([TransportId, Config]) ->
    ?LOG_INFO("Initializing HTTP transport ~p", [TransportId]),
    
    TestMode = maps:get(test_mode, Config, false),
    Port = maps:get(port, Config, 8080),
    Path = maps:get(path, Config, "/mcp"),
    SSLEnabled = maps:get(ssl_enabled, Config, false),
    
    State = #state{
        transport_id = TransportId,
        config = Config,
        port = Port,
        path = Path,
        ssl_enabled = SSLEnabled,
        test_mode = TestMode
    },
    
    case TestMode of
        true ->
            ?LOG_INFO("HTTP transport ~p initialized in test mode", [TransportId]),
            {ok, State};
        false ->
            case start_http_server(State) of
                {ok, NewState} ->
                    register_with_registry(NewState),
                    ?LOG_INFO("HTTP transport ~p initialized successfully on port ~p path ~p", 
                             [TransportId, Port, Path]),
                    {ok, NewState};
                {error, Reason} = Error ->
                    ?LOG_ERROR("Failed to initialize HTTP transport ~p: ~p", [TransportId, Reason]),
                    Error
            end
    end.

-spec handle_call(term(), {pid(), term()}, state()) ->
    {reply, term(), state()} | {noreply, state()} | {stop, term(), term(), state()}.
handle_call({send, Data}, _From, State) ->
    case send_data(State, Data) of
        ok ->
            {reply, ok, State};
        {error, Reason} = Error ->
            ?LOG_ERROR("Failed to send data: ~p", [Reason]),
            {reply, Error, State}
    end;

handle_call(close, _From, State) ->
    NewState = stop_http_server(State),
    {reply, ok, NewState};

handle_call(get_info, _From, State) ->
    Info = get_info(State),
    {reply, Info, State};

handle_call(get_state, _From, State) ->
    {reply, {ok, State}, State};

handle_call({http_request, Method, Path, Headers, Body}, _From, State) ->
    Response = handle_http_request(State, Method, Path, Headers, Body),
    {reply, Response, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast({data, Data}, State) ->
    NewState = handle_incoming_data(State, Data),
    {noreply, NewState};

handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()} | {stop, term(), state()}.
handle_info({http_server_started, Pid}, State) ->
    ?LOG_INFO("HTTP server started with PID ~p for transport ~p", [Pid, State#state.transport_id]),
    {noreply, State#state{http_pid = Pid}};

handle_info({http_server_error, Reason}, State) ->
    ?LOG_ERROR("HTTP server error for transport ~p: ~p", [State#state.transport_id, Reason]),
    {stop, {http_server_error, Reason}, State};

handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), state()) -> ok.
terminate(Reason, State) ->
    ?LOG_INFO("Terminating HTTP transport ~p: ~p", [State#state.transport_id, Reason]),
    stop_http_server(State),
    unregister_from_registry(State),
    ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

-spec start_http_server(state()) -> {ok, state()} | {error, term()}.
start_http_server(#state{test_mode = true} = State) ->
    {ok, State};
start_http_server(#state{port = Port, path = Path, ssl_enabled = SSLEnabled} = State) ->
    try
        % For now, simulate HTTP server startup
        % In a real implementation, you would start cowboy, elli, or similar
        case SSLEnabled of
            true ->
                ?LOG_INFO("Starting HTTPS server on port ~p with path ~p", [Port, Path]);
            false ->
                ?LOG_INFO("Starting HTTP server on port ~p with path ~p", [Port, Path])
        end,
        
        % Simulate server PID
        ServerPid = spawn(fun() -> http_server_loop(State) end),
        
        {ok, State#state{http_pid = ServerPid}}
    catch
        Class:Reason:Stacktrace ->
            ?LOG_ERROR("Failed to start HTTP server: ~p:~p~n~p", [Class, Reason, Stacktrace]),
            {error, {http_server_start_failed, Reason}}
    end.

-spec http_server_loop(state()) -> ok.
http_server_loop(State) ->
    ?LOG_DEBUG("HTTP server loop running for transport ~p", [State#state.transport_id]),
    receive
        stop ->
            ?LOG_INFO("HTTP server stopping for transport ~p", [State#state.transport_id]),
            ok;
        {http_request, From, Method, Path, Headers, Body} ->
            Response = handle_http_request(State, Method, Path, Headers, Body),
            From ! {http_response, Response},
            http_server_loop(State);
        _Other ->
            http_server_loop(State)
    after 30000 ->
        % Keepalive ping
        http_server_loop(State)
    end.

-spec handle_http_request(state(), atom(), string(), list(), binary()) -> {integer(), list(), binary()}.
handle_http_request(#state{path = ConfigPath} = State, Method, RequestPath, Headers, Body) ->
    case {Method, RequestPath} of
        {'POST', ConfigPath} ->
            % Handle MCP JSON-RPC request
            handle_mcp_request(State, Headers, Body);
        {'GET', ConfigPath} ->
            % Handle health check or info request
            {200, [{"Content-Type", "application/json"}], <<"{\"status\":\"ok\"}">>};
        {_, _} ->
            % Not found
            {404, [{"Content-Type", "text/plain"}], <<"Not Found">>}
    end.

-spec handle_mcp_request(state(), list(), binary()) -> {integer(), list(), binary()}.
handle_mcp_request(State, _Headers, Body) ->
    try
        % Here you would parse JSON-RPC and route to appropriate handler
        ?LOG_DEBUG("Processing MCP request on transport ~p: ~p", [State#state.transport_id, Body]),
        
        % For now, just echo back a success response
        Response = <<"{\"jsonrpc\":\"2.0\",\"id\":1,\"result\":{\"status\":\"ok\"}}">>,
        {200, [{"Content-Type", "application/json"}], Response}
    catch
        Class:Reason ->
            ?LOG_ERROR("Error processing MCP request: ~p:~p", [Class, Reason]),
            ErrorResponse = <<"{\"jsonrpc\":\"2.0\",\"id\":null,\"error\":{\"code\":-32603,\"message\":\"Internal error\"}}">>,
            {500, [{"Content-Type", "application/json"}], ErrorResponse}
    end.

-spec send_data(state(), binary() | string()) -> ok | {error, term()}.
send_data(#state{test_mode = true}, _Data) ->
    % In test mode, just pretend to send
    ok;
send_data(#state{http_pid = undefined}, _Data) ->
    {error, http_server_not_started};
send_data(#state{http_pid = _ServerPid}, Data) when is_binary(Data) ->
    try
        % In HTTP transport, "sending" typically means preparing a response
        % or making an outbound HTTP request
        ?LOG_DEBUG("Sending HTTP data: ~p", [Data]),
        ok
    catch
        Class:Reason ->
            {error, {send_failed, Class, Reason}}
    end;
send_data(State, Data) when is_list(Data) ->
    send_data(State, list_to_binary(Data)).

-spec handle_incoming_data(state(), binary()) -> state().
handle_incoming_data(#state{transport_id = TransportId} = State, Data) ->
    ?LOG_DEBUG("Processing incoming HTTP data on transport ~p: ~p", [TransportId, Data]),
    % Process the HTTP data (typically JSON-RPC over HTTP)
    process_message(State, Data),
    State.

-spec process_message(state(), binary()) -> ok.
process_message(#state{transport_id = TransportId}, Message) ->
    ?LOG_DEBUG("Processing HTTP message on transport ~p: ~p", [TransportId, Message]),
    % Here we would normally parse JSON-RPC and route to appropriate handler
    % For now, just log the message
    ok.

-spec stop_http_server(state()) -> state().
stop_http_server(#state{http_pid = undefined} = State) ->
    State;
stop_http_server(#state{http_pid = ServerPid} = State) ->
    case is_process_alive(ServerPid) of
        true ->
            ServerPid ! stop,
            % Wait a bit for graceful shutdown
            timer:sleep(100);
        false ->
            ok
    end,
    State#state{http_pid = undefined}.

-spec register_with_registry(state()) -> ok.
register_with_registry(#state{transport_id = TransportId, config = Config}) ->
    case whereis(erlmcp_registry) of
        undefined ->
            ?LOG_WARNING("Registry not available for transport ~p", [TransportId]),
            ok;
        _RegistryPid ->
            TransportConfig = Config#{type => http},
            case erlmcp_registry:register_transport(TransportId, self(), TransportConfig) of
                ok ->
                    ?LOG_DEBUG("Registered HTTP transport ~p with registry", [TransportId]),
                    ok;
                {error, Reason} ->
                    ?LOG_ERROR("Failed to register HTTP transport ~p: ~p", [TransportId, Reason]),
                    ok
            end
    end.

-spec unregister_from_registry(state()) -> ok.
unregister_from_registry(#state{transport_id = TransportId}) ->
    case whereis(erlmcp_registry) of
        undefined ->
            ok;
        _RegistryPid ->
            case erlmcp_registry:unregister_transport(TransportId) of
                ok ->
                    ?LOG_DEBUG("Unregistered HTTP transport ~p from registry", [TransportId]),
                    ok;
                {error, Reason} ->
                    ?LOG_WARNING("Failed to unregister HTTP transport ~p: ~p", [TransportId, Reason]),
                    ok
            end
    end.