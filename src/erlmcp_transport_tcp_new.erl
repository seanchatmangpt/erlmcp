%%%-------------------------------------------------------------------
%%% @doc
%%% TCP Transport Implementation
%%% 
%%% This module implements TCP transport for MCP.
%%% It provides bidirectional communication through TCP sockets.
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_transport_tcp_new).
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
    socket :: gen_tcp:socket() | undefined,
    listen_socket :: gen_tcp:socket() | undefined,
    port :: integer(),
    host :: string(),
    buffer :: binary(),
    registry_pid :: pid() | undefined,
    test_mode :: boolean()
}).

-type state() :: #state{}.

%%====================================================================
%% API Functions
%%====================================================================

-spec start_link(atom(), map()) -> {ok, pid()} | {error, term()}.
start_link(TransportId, Config) ->
    ?LOG_INFO("Starting TCP transport: ~p with config: ~p", [TransportId, Config]),
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
    close_socket(State).

-spec get_info(pid() | state()) -> map().
get_info(Pid) when is_pid(Pid) ->
    gen_server:call(Pid, get_info);
get_info(#state{transport_id = Id, config = Config, port = Port, host = Host, test_mode = TestMode}) ->
    #{
        transport_id => Id,
        type => tcp,
        config => Config,
        port => Port,
        host => Host,
        test_mode => TestMode,
        status => running
    }.

%%====================================================================
%% gen_server callbacks
%%====================================================================

-spec init([atom() | map()]) -> {ok, state()} | {error, term()}.
init([TransportId, Config]) ->
    ?LOG_INFO("Initializing TCP transport ~p", [TransportId]),
    
    TestMode = maps:get(test_mode, Config, false),
    Port = maps:get(port, Config, 8080),
    Host = maps:get(host, Config, "127.0.0.1"),
    
    State = #state{
        transport_id = TransportId,
        config = Config,
        port = Port,
        host = Host,
        buffer = <<>>,
        test_mode = TestMode
    },
    
    case TestMode of
        true ->
            ?LOG_INFO("TCP transport ~p initialized in test mode", [TransportId]),
            {ok, State};
        false ->
            case init_socket(State) of
                {ok, NewState} ->
                    register_with_registry(NewState),
                    ?LOG_INFO("TCP transport ~p initialized successfully on ~s:~p", [TransportId, Host, Port]),
                    {ok, NewState};
                {error, Reason} = Error ->
                    ?LOG_ERROR("Failed to initialize TCP transport ~p: ~p", [TransportId, Reason]),
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
    NewState = close_socket(State),
    {reply, ok, NewState};

handle_call(get_info, _From, State) ->
    Info = get_info(State),
    {reply, Info, State};

handle_call(get_state, _From, State) ->
    {reply, {ok, State}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast({data, Data}, State) ->
    NewState = handle_incoming_data(State, Data),
    {noreply, NewState};

handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()} | {stop, term(), state()}.
handle_info({tcp, Socket, Data}, #state{socket = Socket} = State) ->
    NewState = handle_incoming_data(State, Data),
    inet:setopts(Socket, [{active, once}]),
    {noreply, NewState};

handle_info({tcp_closed, Socket}, #state{socket = Socket} = State) ->
    ?LOG_WARNING("TCP socket closed for transport ~p", [State#state.transport_id]),
    {stop, tcp_closed, State};

handle_info({tcp_error, Socket, Reason}, #state{socket = Socket} = State) ->
    ?LOG_ERROR("TCP socket error for transport ~p: ~p", [State#state.transport_id, Reason]),
    {stop, {tcp_error, Reason}, State};

handle_info({tcp, ListenSocket, _}, #state{listen_socket = ListenSocket} = State) ->
    % Accept new connection
    case gen_tcp:accept(ListenSocket) of
        {ok, ClientSocket} ->
            inet:setopts(ClientSocket, [{active, once}]),
            NewState = State#state{socket = ClientSocket},
            ?LOG_INFO("Accepted new TCP connection for transport ~p", [State#state.transport_id]),
            {noreply, NewState};
        {error, Reason} ->
            ?LOG_ERROR("Failed to accept TCP connection: ~p", [Reason]),
            {noreply, State}
    end;

handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), state()) -> ok.
terminate(Reason, State) ->
    ?LOG_INFO("Terminating TCP transport ~p: ~p", [State#state.transport_id, Reason]),
    close_socket(State),
    unregister_from_registry(State),
    ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

-spec init_socket(state()) -> {ok, state()} | {error, term()}.
init_socket(#state{test_mode = true} = State) ->
    {ok, State};
init_socket(#state{port = Port, host = Host} = State) ->
    try
        case gen_tcp:listen(Port, [
            binary,
            {packet, 0},
            {active, false},
            {reuseaddr, true},
            {keepalive, true}
        ]) of
            {ok, ListenSocket} ->
                ?LOG_INFO("TCP listening on ~s:~p", [Host, Port]),
                {ok, State#state{listen_socket = ListenSocket}};
            {error, ListenReason} ->
                {error, {listen_failed, ListenReason}}
        end
    catch
        Class:CatchReason:Stacktrace ->
            ?LOG_ERROR("Failed to initialize TCP socket: ~p:~p~n~p", [Class, CatchReason, Stacktrace]),
            {error, {socket_init_failed, CatchReason}}
    end.

-spec send_data(state(), binary() | string()) -> ok | {error, term()}.
send_data(#state{test_mode = true}, _Data) ->
    % In test mode, just pretend to send
    ok;
send_data(#state{socket = undefined}, _Data) ->
    {error, socket_not_connected};
send_data(#state{socket = Socket}, Data) when is_binary(Data) ->
    case gen_tcp:send(Socket, Data) of
        ok ->
            ok;
        {error, Reason} ->
            {error, {send_failed, Reason}}
    end;
send_data(State, Data) when is_list(Data) ->
    send_data(State, list_to_binary(Data)).

-spec handle_incoming_data(state(), binary()) -> state().
handle_incoming_data(#state{buffer = Buffer} = State, NewData) ->
    UpdatedBuffer = <<Buffer/binary, NewData/binary>>,
    {ProcessedLines, RemainingBuffer} = extract_lines(UpdatedBuffer),
    
    % Process each complete line
    lists:foreach(fun(Line) ->
        case Line of
            <<>> -> ok; % Skip empty lines
            _ -> process_message(State, Line)
        end
    end, ProcessedLines),
    
    State#state{buffer = RemainingBuffer}.

-spec extract_lines(binary()) -> {[binary()], binary()}.
extract_lines(Buffer) ->
    extract_lines(Buffer, [], <<>>).

extract_lines(<<>>, Lines, CurrentLine) ->
    case CurrentLine of
        <<>> -> {lists:reverse(Lines), <<>>};
        _ -> {lists:reverse(Lines), CurrentLine}
    end;
extract_lines(<<$\n, Rest/binary>>, Lines, CurrentLine) ->
    extract_lines(Rest, [CurrentLine | Lines], <<>>);
extract_lines(<<Char, Rest/binary>>, Lines, CurrentLine) ->
    extract_lines(Rest, Lines, <<CurrentLine/binary, Char>>).

-spec process_message(state(), binary()) -> ok.
process_message(#state{transport_id = TransportId}, Message) ->
    ?LOG_DEBUG("Processing TCP message on transport ~p: ~p", [TransportId, Message]),
    % Here we would normally parse JSON-RPC and route to appropriate handler
    % For now, just log the message
    ok.

-spec close_socket(state()) -> state().
close_socket(#state{socket = Socket, listen_socket = ListenSocket} = State) ->
    case Socket of
        undefined -> ok;
        S when is_port(S) -> 
            gen_tcp:close(S)
    end,
    case ListenSocket of
        undefined -> ok;
        LS when is_port(LS) -> 
            gen_tcp:close(LS)
    end,
    State#state{socket = undefined, listen_socket = undefined}.

-spec register_with_registry(state()) -> ok.
register_with_registry(#state{transport_id = TransportId, config = Config}) ->
    case whereis(erlmcp_registry) of
        undefined ->
            ?LOG_WARNING("Registry not available for transport ~p", [TransportId]),
            ok;
        _RegistryPid ->
            TransportConfig = Config#{type => tcp},
            case erlmcp_registry:register_transport(TransportId, self(), TransportConfig) of
                ok ->
                    ?LOG_DEBUG("Registered TCP transport ~p with registry", [TransportId]),
                    ok;
                {error, Reason} ->
                    ?LOG_ERROR("Failed to register TCP transport ~p: ~p", [TransportId, Reason]),
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
                    ?LOG_DEBUG("Unregistered TCP transport ~p from registry", [TransportId]),
                    ok;
                {error, Reason} ->
                    ?LOG_WARNING("Failed to unregister TCP transport ~p: ~p", [TransportId, Reason]),
                    ok
            end
    end.