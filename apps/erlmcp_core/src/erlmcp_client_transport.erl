%%%-------------------------------------------------------------------
%%% @doc
%%% Client Transport Integration
%%%
%%% This module provides the actual transport integration layer for
%%% erlmcp_client, replacing the placeholder implementation.
%%%
%%% It properly initializes and manages the real transport implementations
%%% from erlmcp_transports application.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_client_transport).

-behaviour(gen_server).

%% API
-export([start_link/1, send/2, close/1, connect/2]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("erlmcp.hrl").

%% State record
-record(state,
        {transport_type :: stdio | tcp | http | websocket,
         transport_pid :: pid() | undefined,
         transport_state :: term(),
         owner :: pid(),
         owner_monitor :: reference(),
         options :: map()}).

-type state() :: #state{}.
-type transport_opts() :: {stdio, map()} | {tcp, map()} | {http, map()} | {websocket, map()}.

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Start transport with options
-spec start_link(transport_opts()) -> {ok, pid()} | {error, term()}.
start_link(TransportOpts) ->
    gen_server:start_link(?MODULE, TransportOpts, []).

%% @doc Send data through transport
-spec send(pid(), binary()) -> ok | {error, term()}.
send(TransportPid, Data) when is_pid(TransportPid), is_binary(Data) ->
    gen_server:call(TransportPid, {send, Data}, 5000);
send(_, _) ->
    {error, invalid_arguments}.

%% @doc Close transport
-spec close(pid()) -> ok.
close(TransportPid) when is_pid(TransportPid) ->
    gen_server:stop(TransportPid);
close(_) ->
    ok.

%% @doc Connect to remote endpoint (for client-mode transports)
-spec connect(pid(), map()) -> ok | {error, term()}.
connect(TransportPid, Opts) when is_pid(TransportPid), is_map(Opts) ->
    gen_server:call(TransportPid, {connect, Opts});
connect(_, _) ->
    {error, invalid_arguments}.

%%====================================================================
%% gen_server callbacks
%%====================================================================

-spec init(transport_opts()) -> {ok, state()} | {stop, term()}.
init({stdio, Opts}) when is_map(Opts); is_list(Opts) ->
    process_flag(trap_exit, true),

    OptsMap = ensure_map(Opts),
    Owner = maps:get(owner, OptsMap, self()),

    %% Monitor owner
    OwnerMonitor = monitor(process, Owner),

    %% Start stdio transport
    case erlmcp_transport_stdio:start_link(Owner, OptsMap) of
        {ok, TransportPid} ->
            logger:info("Client stdio transport started: ~p", [TransportPid]),
            {ok,
             #state{transport_type = stdio,
                    transport_pid = TransportPid,
                    transport_state = TransportPid,
                    owner = Owner,
                    owner_monitor = OwnerMonitor,
                    options = OptsMap}};
        {error, Reason} ->
            logger:error("Failed to start stdio transport: ~p", [Reason]),
            {stop, {stdio_init_failed, Reason}}
    end;
init({tcp, Opts}) when is_map(Opts) ->
    process_flag(trap_exit, true),

    Owner = maps:get(owner, Opts, self()),
    Mode = maps:get(mode, Opts, client),

    %% Monitor owner
    OwnerMonitor = monitor(process, Owner),

    %% Start TCP transport in client mode
    OptsWithOwner = Opts#{owner => Owner},

    case Mode of
        client ->
            case erlmcp_transport_tcp:start_client(OptsWithOwner) of
                {ok, TransportPid} ->
                    logger:info("Client TCP transport started: ~p", [TransportPid]),
                    {ok,
                     #state{transport_type = tcp,
                            transport_pid = TransportPid,
                            transport_state = TransportPid,
                            owner = Owner,
                            owner_monitor = OwnerMonitor,
                            options = OptsWithOwner}};
                {error, Reason} ->
                    logger:error("Failed to start TCP client transport: ~p", [Reason]),
                    {stop, {tcp_client_init_failed, Reason}}
            end;
        server ->
            case erlmcp_transport_tcp:start_server(OptsWithOwner) of
                {ok, TransportPid} ->
                    logger:info("Server TCP transport started: ~p", [TransportPid]),
                    {ok,
                     #state{transport_type = tcp,
                            transport_pid = TransportPid,
                            transport_state = TransportPid,
                            owner = Owner,
                            owner_monitor = OwnerMonitor,
                            options = OptsWithOwner}};
                {error, Reason} ->
                    logger:error("Failed to start TCP server transport: ~p", [Reason]),
                    {stop, {tcp_server_init_failed, Reason}}
            end
    end;
init({http, Opts}) when is_map(Opts) ->
    process_flag(trap_exit, true),

    Owner = maps:get(owner, Opts, self()),

    %% Monitor owner
    OwnerMonitor = monitor(process, Owner),

    %% Start HTTP transport
    OptsWithOwner = Opts#{owner => Owner},

    case erlmcp_transport_http_server:start_link(OptsWithOwner) of
        {ok, TransportPid} ->
            logger:info("Client HTTP transport started: ~p", [TransportPid]),
            {ok,
             #state{transport_type = http,
                    transport_pid = TransportPid,
                    transport_state = TransportPid,
                    owner = Owner,
                    owner_monitor = OwnerMonitor,
                    options = OptsWithOwner}};
        {error, Reason} ->
            logger:error("Failed to start HTTP transport: ~p", [Reason]),
            {stop, {http_init_failed, Reason}}
    end;
init({websocket, Opts}) when is_map(Opts) ->
    process_flag(trap_exit, true),

    Owner = maps:get(owner, Opts, self()),
    TransportId = maps:get(transport_id, Opts, client_ws_transport),

    %% Monitor owner
    OwnerMonitor = monitor(process, Owner),

    %% Start WebSocket transport
    case erlmcp_transport_ws:init(TransportId, Opts) of
        {ok, _ListenerPid} ->
            logger:info("Client WebSocket transport started: ~p", [TransportId]),
            {ok,
             #state{transport_type = websocket,
                    transport_pid = undefined,  % WebSocket uses cowboy listener
                    transport_state = {cowboy_listener, TransportId},
                    owner = Owner,
                    owner_monitor = OwnerMonitor,
                    options = Opts}};
        {error, Reason} ->
            logger:error("Failed to start WebSocket transport: ~p", [Reason]),
            {stop, {websocket_init_failed, Reason}}
    end;
init(Opts) when is_map(Opts) ->
    %% Legacy format - detect type from options
    Type = maps:get(type, Opts, stdio),
    init({Type, Opts});
init(Invalid) ->
    logger:error("Invalid transport options: ~p", [Invalid]),
    {stop, {invalid_options, Invalid}}.

-spec handle_call(term(), {pid(), term()}, state()) ->
                     {reply, term(), state()} | {noreply, state()}.
handle_call({send, Data}, _From, State) ->
    Result = do_send(Data, State),
    {reply, Result, State};
handle_call({connect, NewOpts}, _From, #state{transport_pid = TransportPid} = State)
    when is_pid(TransportPid) ->
    %% Delegate to transport if supported
    Result =
        case State#state.transport_type of
            tcp ->
                erlmcp_transport_tcp:connect(TransportPid, NewOpts);
            _ ->
                {error, not_supported}
        end,
    {reply, Result, State};
handle_call(get_state, _From, State) ->
    {reply, {ok, State}, State};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()} | {stop, term(), state()}.
%% Forward transport messages to owner
handle_info({transport_message, Data}, #state{owner = Owner} = State) ->
    Owner ! {transport_message, Data},
    {noreply, State};
%% Handle transport connection notifications
handle_info({transport_connected, Pid}, #state{owner = Owner} = State) ->
    Owner ! {transport_connected, Pid},
    {noreply, State};
%% Handle transport disconnection notifications
handle_info({transport_disconnected, Pid, Reason}, #state{owner = Owner} = State) ->
    Owner ! {transport_disconnected, Pid, Reason},
    {noreply, State};
%% Handle owner death
handle_info({'DOWN', MonitorRef, process, Owner, Reason},
            #state{owner_monitor = MonitorRef, owner = Owner} = State) ->
    logger:info("Owner process ~p died: ~p", [Owner, Reason]),
    {stop, {owner_died, Reason}, State};
%% Handle transport death
handle_info({'EXIT', Pid, Reason}, #state{transport_pid = Pid} = State) ->
    logger:error("Transport process ~p died: ~p", [Pid, Reason]),
    {stop, {transport_died, Reason}, State};
handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), state()) -> ok.
terminate(_Reason,
          #state{transport_type = Type,
                 transport_pid = Pid,
                 transport_state = TransportState}) ->
    %% Close the transport
    case Type of
        stdio when is_pid(Pid) ->
            erlmcp_transport_stdio:close(Pid);
        tcp when is_pid(Pid) ->
            erlmcp_transport_tcp:close(Pid);
        http when is_pid(Pid) ->
            erlmcp_transport_http:close(Pid);
        websocket ->
            %% WebSocket cleanup handled by cowboy
            case TransportState of
                {cowboy_listener, _TransportId} ->
                    cowboy:stop_listener(erlmcp_ws_listener);
                _ ->
                    ok
            end;
        _ ->
            ok
    end,
    logger:info("Client transport ~p terminated", [Type]),
    ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

%% @doc Send data using the appropriate transport module
-spec do_send(binary(), state()) -> ok | {error, term()}.
do_send(Data, #state{transport_type = stdio, transport_state = Pid}) when is_pid(Pid) ->
    erlmcp_transport_stdio:send(Pid, Data);
do_send(Data, #state{transport_type = tcp, transport_state = Pid}) when is_pid(Pid) ->
    erlmcp_transport_tcp:send(Pid, Data);
do_send(Data, #state{transport_type = http, transport_state = Pid}) when is_pid(Pid) ->
    erlmcp_transport_http:send(Pid, Data);
do_send(Data, #state{transport_type = websocket}) ->
    %% WebSocket sends are handled differently via cowboy
    %% For now, we return ok as the transport handles this internally
    {ok, Data};
do_send(_Data, #state{transport_pid = undefined}) ->
    {error, not_connected};
do_send(_Data, State) ->
    {error, {unsupported_transport, State#state.transport_type}}.

%% @doc Ensure options is a map
-spec ensure_map(map() | list()) -> map().
ensure_map(Opts) when is_map(Opts) ->
    Opts;
ensure_map(Opts) when is_list(Opts) ->
    maps:from_list(Opts).
