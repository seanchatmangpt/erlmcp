-module(erlmcp_registry).
-behaviour(gen_server).

-include("erlmcp.hrl").

%% API exports
-export([
    start_link/0,
    register_server/3, register_transport/3,
    unregister_server/1, unregister_transport/1,
    route_to_server/3, route_to_transport/3,
    find_server/1, find_transport/1,
    list_servers/0, list_transports/0,
    bind_transport_to_server/2, unbind_transport/1,
    get_server_for_transport/1
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Types
-type transport_id() :: atom() | binary().
-type server_config() :: #{
    capabilities => #mcp_server_capabilities{},
    options => map(),
    _ => _
}.
-type transport_config() :: #{
    type => stdio | tcp | http,
    server_id => server_id(),
    config => map(),
    _ => _
}.

-export_type([server_id/0, transport_id/0]).

%% State record - significantly simplified with gproc
-record(registry_state, {
    server_transport_map = #{} :: #{transport_id() => server_id()}
}).

-type state() :: #registry_state{}.

%%====================================================================
%% API Functions
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec register_server(server_id(), pid(), server_config()) -> ok | {error, term()}.
register_server(ServerId, ServerPid, Config) when is_pid(ServerPid) ->
    gen_server:call(?MODULE, {register_server, ServerId, ServerPid, Config}).

-spec register_transport(transport_id(), pid(), transport_config()) -> ok | {error, term()}.
register_transport(TransportId, TransportPid, Config) when is_pid(TransportPid) ->
    gen_server:call(?MODULE, {register_transport, TransportId, TransportPid, Config}).

-spec unregister_server(server_id()) -> ok.
unregister_server(ServerId) ->
    gen_server:call(?MODULE, {unregister_server, ServerId}).

-spec unregister_transport(transport_id()) -> ok.
unregister_transport(TransportId) ->
    gen_server:call(?MODULE, {unregister_transport, TransportId}).

-spec route_to_server(server_id(), transport_id(), term()) -> ok | {error, term()}.
route_to_server(ServerId, TransportId, Message) ->
    gen_server:cast(?MODULE, {route_to_server, ServerId, TransportId, Message}).

-type transport_target() :: transport_id() | broadcast.

-spec route_to_transport(transport_target(), server_id(), term()) -> ok | {error, term()}.
route_to_transport(TransportId, ServerId, Message) ->
    gen_server:cast(?MODULE, {route_to_transport, TransportId, ServerId, Message}).

-spec find_server(server_id()) -> {ok, {pid(), server_config()}} | {error, not_found}.
find_server(ServerId) ->
    gen_server:call(?MODULE, {find_server, ServerId}).

-spec find_transport(transport_id()) -> {ok, {pid(), transport_config()}} | {error, not_found}.
find_transport(TransportId) ->
    gen_server:call(?MODULE, {find_transport, TransportId}).

-spec list_servers() -> [{server_id(), {pid(), server_config()}}].
list_servers() ->
    gen_server:call(?MODULE, list_servers).

-spec list_transports() -> [{transport_id(), {pid(), transport_config()}}].
list_transports() ->
    gen_server:call(?MODULE, list_transports).

-spec bind_transport_to_server(transport_id(), server_id()) -> ok | {error, term()}.
bind_transport_to_server(TransportId, ServerId) ->
    gen_server:call(?MODULE, {bind_transport_to_server, TransportId, ServerId}).

-spec unbind_transport(transport_id()) -> ok.
unbind_transport(TransportId) ->
    gen_server:call(?MODULE, {unbind_transport, TransportId}).

-spec get_server_for_transport(transport_id()) -> {ok, server_id()} | {error, not_found}.
get_server_for_transport(TransportId) ->
    gen_server:call(?MODULE, {get_server_for_transport, TransportId}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

-spec init([]) -> {ok, state()}.
init([]) ->
    process_flag(trap_exit, true),
    logger:info("Starting MCP registry with gproc"),
    {ok, #registry_state{}}.

-spec handle_call(term(), {pid(), term()}, state()) ->
    {reply, term(), state()}.

handle_call({register_server, ServerId, ServerPid, Config}, _From, State) ->
    % Use gproc to register the server
    Key = {n, l, {mcp, server, ServerId}},
    case gproc:where(Key) of
        undefined ->
            % Register on behalf of the server process and monitor it
            try
                gproc:reg_other(Key, ServerPid, Config),
                gproc:monitor(Key),
                logger:info("Registered server ~p with pid ~p via gproc", [ServerId, ServerPid]),
                {reply, ok, State}
            catch
                error:badarg ->
                    % Already registered by another process
                    logger:warning("Server ~p already registered", [ServerId]),
                    {reply, {error, already_registered}, State}
            end;
        ExistingPid ->
            logger:warning("Server ~p already registered with pid ~p", [ServerId, ExistingPid]),
            {reply, {error, already_registered}, State}
    end;

handle_call({register_transport, TransportId, TransportPid, Config}, _From, State) ->
    % Use gproc to register the transport
    Key = {n, l, {mcp, transport, TransportId}},
    case gproc:where(Key) of
        undefined ->
            try
                gproc:reg_other(Key, TransportPid, Config),
                gproc:monitor(Key),

                % Auto-bind to server if specified in config
                NewState = case maps:get(server_id, Config, undefined) of
                    undefined ->
                        State;
                    ServerId ->
                        NewMap = maps:put(TransportId, ServerId, State#registry_state.server_transport_map),
                        State#registry_state{server_transport_map = NewMap}
                end,

                logger:info("Registered transport ~p with pid ~p via gproc", [TransportId, TransportPid]),
                {reply, ok, NewState}
            catch
                error:badarg ->
                    logger:warning("Transport ~p already registered", [TransportId]),
                    {reply, {error, already_registered}, State}
            end;
        ExistingPid ->
            logger:warning("Transport ~p already registered with pid ~p", [TransportId, ExistingPid]),
            {reply, {error, already_registered}, State}
    end;

handle_call({unregister_server, ServerId}, _From, State) ->
    Key = {n, l, {mcp, server, ServerId}},
    case gproc:where(Key) of
        undefined ->
            {reply, ok, State};
        Pid ->
            % Unregister from gproc
            try
                gproc:unreg_other(Key, Pid)
            catch
                error:badarg -> ok  % Already unregistered
            end,

            % Remove any transport bindings
            NewTransportMap = maps:filter(fun(_, SId) -> SId =/= ServerId end,
                                         State#registry_state.server_transport_map),

            NewState = State#registry_state{server_transport_map = NewTransportMap},
            logger:info("Unregistered server ~p", [ServerId]),
            {reply, ok, NewState}
    end;

handle_call({unregister_transport, TransportId}, _From, State) ->
    Key = {n, l, {mcp, transport, TransportId}},
    case gproc:where(Key) of
        undefined ->
            {reply, ok, State};
        Pid ->
            % Unregister from gproc
            try
                gproc:unreg_other(Key, Pid)
            catch
                error:badarg -> ok  % Already unregistered
            end,

            % Remove binding
            NewTransportMap = maps:remove(TransportId, State#registry_state.server_transport_map),
            NewState = State#registry_state{server_transport_map = NewTransportMap},
            logger:info("Unregistered transport ~p", [TransportId]),
            {reply, ok, NewState}
    end;

handle_call({find_server, ServerId}, _From, State) ->
    Key = {n, l, {mcp, server, ServerId}},
    case gproc:where(Key) of
        undefined ->
            {reply, {error, not_found}, State};
        Pid ->
            % Get the config from gproc's value
            Config = gproc:get_value(Key, Pid),
            {reply, {ok, {Pid, Config}}, State}
    end;

handle_call({find_transport, TransportId}, _From, State) ->
    Key = {n, l, {mcp, transport, TransportId}},
    case gproc:where(Key) of
        undefined ->
            {reply, {error, not_found}, State};
        Pid ->
            % Get the config from gproc's value
            Config = gproc:get_value(Key, Pid),
            {reply, {ok, {Pid, Config}}, State}
    end;

handle_call(list_servers, _From, State) ->
    % Query all registered servers from gproc
    Servers = gproc:select([{{{n, l, {mcp, server, '$1'}}, '$2', '$3'}, [], [{{'$1', {{'$2', '$3'}}}}]}]),
    {reply, Servers, State};

handle_call(list_transports, _From, State) ->
    % Query all registered transports from gproc
    Transports = gproc:select([{{{n, l, {mcp, transport, '$1'}}, '$2', '$3'}, [], [{{'$1', {{'$2', '$3'}}}}]}]),
    {reply, Transports, State};

handle_call({bind_transport_to_server, TransportId, ServerId}, _From, State) ->
    % Verify both exist using gproc
    ServerExists = gproc:where({n, l, {mcp, server, ServerId}}) =/= undefined,
    TransportExists = gproc:where({n, l, {mcp, transport, TransportId}}) =/= undefined,

    case {ServerExists, TransportExists} of
        {true, true} ->
            NewMap = maps:put(TransportId, ServerId, State#registry_state.server_transport_map),
            NewState = State#registry_state{server_transport_map = NewMap},
            logger:info("Bound transport ~p to server ~p", [TransportId, ServerId]),
            {reply, ok, NewState};
        {false, _} ->
            {reply, {error, server_not_found}, State};
        {_, false} ->
            {reply, {error, transport_not_found}, State}
    end;

handle_call({unbind_transport, TransportId}, _From, State) ->
    NewMap = maps:remove(TransportId, State#registry_state.server_transport_map),
    NewState = State#registry_state{server_transport_map = NewMap},
    {reply, ok, NewState};

handle_call({get_server_for_transport, TransportId}, _From, State) ->
    case maps:get(TransportId, State#registry_state.server_transport_map, undefined) of
        undefined -> {reply, {error, not_found}, State};
        ServerId -> {reply, {ok, ServerId}, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.

handle_cast({route_to_server, ServerId, TransportId, Message}, State) ->
    case gproc:where({n, l, {mcp, server, ServerId}}) of
        undefined ->
            logger:warning("Cannot route to server ~p: not found", [ServerId]),
            {noreply, State};
        ServerPid ->
            ServerPid ! {mcp_message, TransportId, Message},
            {noreply, State}
    end;

handle_cast({route_to_transport, broadcast, ServerId, Message}, State) ->
    case transports_for_server(ServerId, State) of
        [] ->
            logger:warning("Cannot broadcast to transports for server ~p: none bound", [ServerId]),
            {noreply, State};
        TransportIds ->
            lists:foreach(fun(TransportId) ->
                send_to_transport(TransportId, ServerId, Message)
            end, TransportIds),
            {noreply, State}
    end;

handle_cast({route_to_transport, TransportId, ServerId, Message}, State) ->
    send_to_transport(TransportId, ServerId, Message),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), state()) ->
    {noreply, state()}.

handle_info({gproc, unreg, _Ref, {n, l, {mcp, server, ServerId}}}, State) ->
    % gproc notifies us when a server process dies
    logger:warning("Server ~p unregistered (process died)", [ServerId]),

    % Remove any transport bindings
    NewTransportMap = maps:filter(fun(_, SId) -> SId =/= ServerId end,
                                 State#registry_state.server_transport_map),

    NewState = State#registry_state{server_transport_map = NewTransportMap},
    {noreply, NewState};

handle_info({gproc, unreg, _Ref, {n, l, {mcp, transport, TransportId}}}, State) ->
    % gproc notifies us when a transport process dies
    logger:warning("Transport ~p unregistered (process died)", [TransportId]),

    % Remove binding
    NewTransportMap = maps:remove(TransportId, State#registry_state.server_transport_map),
    NewState = State#registry_state{server_transport_map = NewTransportMap},
    {noreply, NewState};

handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, _State) ->
    % gproc handles cleanup automatically
    logger:info("MCP registry terminating"),
    ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal helper functions
%%====================================================================

-spec transports_for_server(server_id(), state()) -> [transport_id()].
transports_for_server(ServerId, #registry_state{server_transport_map = Map}) ->
    [TransportId || {TransportId, SId} <- maps:to_list(Map), SId =:= ServerId].

-spec send_to_transport(transport_id(), server_id(), term()) -> ok.
send_to_transport(TransportId, ServerId, Message) ->
    case gproc:where({n, l, {mcp, transport, TransportId}}) of
        undefined ->
            logger:warning("Cannot route to transport ~p: not found", [TransportId]),
            ok;
        TransportPid ->
            TransportPid ! {mcp_response, ServerId, Message},
            ok
    end.
