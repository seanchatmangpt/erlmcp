-module(erlmcp_registry).
-behaviour(gen_server).

-include("erlmcp.hrl").

%% API exports - Pure gproc wrapper
-export([
    start_link/0,
    register_server/3, register_transport/3,
    unregister_server/1, unregister_transport/1,
    route_to_server/3, route_to_transport/3,
    find_server/1, find_transport/1,
    list_servers/0, list_transports/0,
    bind_transport_to_server/2, unbind_transport/1,
    get_server_for_transport/1,
    % Legacy/compatibility functions
    get_servers/0, route_message/2, get_pid/0
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

%% State record - minimal state, gproc handles registration
-record(registry_state, {}).
-type state() :: #registry_state{}.

%%====================================================================
%% API Functions - Pure gproc wrappers
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec register_server(server_id(), pid(), server_config()) -> ok | {error, term()}.
register_server(ServerId, ServerPid, Config) when is_pid(ServerPid) ->
    Key = {n, l, {mcp, server, ServerId}},
    try
        gproc:reg_other(Key, ServerPid, Config),
        logger:info("Registered server ~p with pid ~p via gproc", [ServerId, ServerPid]),
        ok
    catch
        error:badarg ->
            logger:warning("Server ~p already registered", [ServerId]),
            {error, already_registered}
    end.

-spec register_transport(transport_id(), pid(), transport_config()) -> ok | {error, term()}.
register_transport(TransportId, TransportPid, Config) when is_pid(TransportPid) ->
    Key = {n, l, {mcp, transport, TransportId}},
    case try_register_transport(Key, TransportPid, Config) of
        ok ->
            % If server_id is in config, create binding property
            case maps:get(server_id, Config, undefined) of
                undefined ->
                    ok;
                ServerId ->
                    % Store binding as gproc property on the transport
                    BindingKey = {p, l, {mcp, binding, TransportId}},
                    try
                        gproc:reg_other(BindingKey, TransportPid, ServerId)
                    catch
                        error:badarg ->
                            % Binding property already exists, that's ok
                            ok
                    end
            end,
            logger:info("Registered transport ~p with pid ~p via gproc", [TransportId, TransportPid]),
            ok;
        {error, _} = Error ->
            Error
    end.

try_register_transport(Key, Pid, Config) ->
    try
        gproc:reg_other(Key, Pid, Config),
        ok
    catch
        error:badarg ->
            logger:warning("Transport already registered: ~p", [Key]),
            {error, already_registered}
    end.

-spec unregister_server(server_id()) -> ok.
unregister_server(ServerId) ->
    Key = {n, l, {mcp, server, ServerId}},
    case gproc:where(Key) of
        undefined ->
            ok;
        Pid ->
            try
                % Remove any bindings where this server is referenced
                cleanup_server_bindings(ServerId),
                gproc:unreg_other(Key, Pid),
                logger:info("Unregistered server ~p", [ServerId])
            catch
                error:badarg -> ok
            end,
            ok
    end.

-spec unregister_transport(transport_id()) -> ok.
unregister_transport(TransportId) ->
    Key = {n, l, {mcp, transport, TransportId}},
    case gproc:where(Key) of
        undefined ->
            ok;
        Pid ->
            try
                % Remove binding property
                BindingKey = {p, l, {mcp, binding, TransportId}},
                catch gproc:unreg_other(BindingKey, Pid),

                gproc:unreg_other(Key, Pid),
                logger:info("Unregistered transport ~p", [TransportId])
            catch
                error:badarg -> ok
            end,
            ok
    end.

-spec route_to_server(server_id(), transport_id(), term()) -> ok | {error, term()}.
route_to_server(ServerId, TransportId, Message) ->
    case gproc:where({n, l, {mcp, server, ServerId}}) of
        undefined ->
            logger:warning("Cannot route to server ~p: not found", [ServerId]),
            {error, not_found};
        ServerPid ->
            ServerPid ! {mcp_message, TransportId, Message},
            ok
    end.

-type transport_target() :: transport_id() | broadcast.

-spec route_to_transport(transport_target(), server_id(), term()) -> ok | {error, term()}.
route_to_transport(broadcast, ServerId, Message) ->
    % Find all transports bound to this server
    TransportIds = find_transports_for_server(ServerId),
    case TransportIds of
        [] ->
            logger:warning("Cannot broadcast to transports for server ~p: none bound", [ServerId]),
            {error, no_transports};
        _ ->
            lists:foreach(fun(TransportId) ->
                send_to_transport(TransportId, ServerId, Message)
            end, TransportIds),
            ok
    end;
route_to_transport(TransportId, ServerId, Message) ->
    send_to_transport(TransportId, ServerId, Message).

-spec find_server(server_id()) -> {ok, {pid(), server_config()}} | {error, not_found}.
find_server(ServerId) ->
    Key = {n, l, {mcp, server, ServerId}},
    case gproc:where(Key) of
        undefined ->
            {error, not_found};
        Pid ->
            Config = gproc:get_value(Key, Pid),
            {ok, {Pid, Config}}
    end.

-spec find_transport(transport_id()) -> {ok, {pid(), transport_config()}} | {error, not_found}.
find_transport(TransportId) ->
    Key = {n, l, {mcp, transport, TransportId}},
    case gproc:where(Key) of
        undefined ->
            {error, not_found};
        Pid ->
            Config = gproc:get_value(Key, Pid),
            {ok, {Pid, Config}}
    end.

-spec list_servers() -> [{server_id(), {pid(), server_config()}}].
list_servers() ->
    Pattern = [{{{n, l, {mcp, server, '$1'}}, '$2', '$3'}, [], [{{'$1', {{'$2', '$3'}}}}]}],
    gproc:select(Pattern).

-spec list_transports() -> [{transport_id(), {pid(), transport_config()}}].
list_transports() ->
    Pattern = [{{{n, l, {mcp, transport, '$1'}}, '$2', '$3'}, [], [{{'$1', {{'$2', '$3'}}}}]}],
    gproc:select(Pattern).

-spec bind_transport_to_server(transport_id(), server_id()) -> ok | {error, term()}.
bind_transport_to_server(TransportId, ServerId) ->
    % Verify both exist
    ServerExists = gproc:where({n, l, {mcp, server, ServerId}}) =/= undefined,
    TransportKey = {n, l, {mcp, transport, TransportId}},
    TransportPid = gproc:where(TransportKey),

    case {ServerExists, TransportPid} of
        {true, Pid} when is_pid(Pid) ->
            % Store binding as gproc property
            BindingKey = {p, l, {mcp, binding, TransportId}},
            try
                % Remove old binding if exists
                catch gproc:unreg_other(BindingKey, Pid),
                % Add new binding
                gproc:reg_other(BindingKey, Pid, ServerId),
                logger:info("Bound transport ~p to server ~p", [TransportId, ServerId]),
                ok
            catch
                error:badarg ->
                    {error, binding_failed}
            end;
        {false, _} ->
            {error, server_not_found};
        {_, undefined} ->
            {error, transport_not_found}
    end.

-spec unbind_transport(transport_id()) -> ok.
unbind_transport(TransportId) ->
    TransportKey = {n, l, {mcp, transport, TransportId}},
    case gproc:where(TransportKey) of
        undefined ->
            ok;
        Pid ->
            BindingKey = {p, l, {mcp, binding, TransportId}},
            catch gproc:unreg_other(BindingKey, Pid),
            ok
    end.

-spec get_server_for_transport(transport_id()) -> {ok, server_id()} | {error, not_found}.
get_server_for_transport(TransportId) ->
    TransportKey = {n, l, {mcp, transport, TransportId}},
    case gproc:where(TransportKey) of
        undefined ->
            {error, not_found};
        Pid ->
            BindingKey = {p, l, {mcp, binding, TransportId}},
            try
                ServerId = gproc:get_value(BindingKey, Pid),
                {ok, ServerId}
            catch
                error:badarg ->
                    {error, not_found}
            end
    end.

%% Legacy/compatibility functions

-spec get_servers() -> [pid()].
get_servers() ->
    % Return list of server PIDs (for legacy compatibility)
    Pattern = [{{{n, l, {mcp, server, '_'}}, '$1', '_'}, [], ['$1']}],
    gproc:select(Pattern).

-spec route_message(transport_id(), term()) -> ok | {error, term()}.
route_message(TransportId, RawData) ->
    % Route message from transport to its bound server
    case get_server_for_transport(TransportId) of
        {ok, ServerId} ->
            route_to_server(ServerId, TransportId, RawData);
        {error, not_found} ->
            logger:warning("Cannot route message: transport ~p not bound to any server", [TransportId]),
            {error, transport_not_bound}
    end.

-spec get_pid() -> pid() | undefined.
get_pid() ->
    % Return the registry gen_server PID (for legacy compatibility)
    whereis(?MODULE).

%%====================================================================
%% gen_server callbacks - Minimal implementation
%%====================================================================

-spec init([]) -> {ok, state()}.
init([]) ->
    process_flag(trap_exit, true),
    logger:info("Starting MCP registry (pure gproc wrapper)"),
    {ok, #registry_state{}}.

-spec handle_call(term(), {pid(), term()}, state()) ->
    {reply, term(), state()}.

% Legacy compatibility - redirect to direct API calls
handle_call({register_server, ServerId, ServerPid, Config}, _From, State) ->
    Result = register_server(ServerId, ServerPid, Config),
    {reply, Result, State};

handle_call({register_transport, TransportId, TransportPid, Config}, _From, State) ->
    Result = register_transport(TransportId, TransportPid, Config),
    {reply, Result, State};

handle_call({unregister_server, ServerId}, _From, State) ->
    Result = unregister_server(ServerId),
    {reply, Result, State};

handle_call({unregister_transport, TransportId}, _From, State) ->
    Result = unregister_transport(TransportId),
    {reply, Result, State};

handle_call({find_server, ServerId}, _From, State) ->
    Result = find_server(ServerId),
    {reply, Result, State};

handle_call({find_transport, TransportId}, _From, State) ->
    Result = find_transport(TransportId),
    {reply, Result, State};

handle_call(list_servers, _From, State) ->
    Result = list_servers(),
    {reply, Result, State};

handle_call(list_transports, _From, State) ->
    Result = list_transports(),
    {reply, Result, State};

handle_call({bind_transport_to_server, TransportId, ServerId}, _From, State) ->
    Result = bind_transport_to_server(TransportId, ServerId),
    {reply, Result, State};

handle_call({unbind_transport, TransportId}, _From, State) ->
    Result = unbind_transport(TransportId),
    {reply, Result, State};

handle_call({get_server_for_transport, TransportId}, _From, State) ->
    Result = get_server_for_transport(TransportId),
    {reply, Result, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.

handle_cast({route_to_server, ServerId, TransportId, Message}, State) ->
    _ = route_to_server(ServerId, TransportId, Message),
    {noreply, State};

handle_cast({route_to_transport, TransportId, ServerId, Message}, State) ->
    _ = route_to_transport(TransportId, ServerId, Message),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()}.

% gproc automatically handles process monitoring and cleanup
handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, _State) ->
    logger:info("MCP registry terminating (gproc handles cleanup)"),
    ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal helper functions
%%====================================================================

-spec find_transports_for_server(server_id()) -> [transport_id()].
find_transports_for_server(ServerId) ->
    % Query all binding properties and filter by server ID
    Pattern = [{{{p, l, {mcp, binding, '$1'}}, '_', ServerId}, [], ['$1']}],
    gproc:select(Pattern).

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

-spec cleanup_server_bindings(server_id()) -> ok.
cleanup_server_bindings(ServerId) ->
    % Find all transports bound to this server and remove bindings
    TransportIds = find_transports_for_server(ServerId),
    lists:foreach(fun(TransportId) ->
        unbind_transport(TransportId)
    end, TransportIds),
    ok.
