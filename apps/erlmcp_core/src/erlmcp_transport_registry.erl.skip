%%%-------------------------------------------------------------------
%%% @doc Transport Registry GenServer
%%%
%%% Registry for tracking and managing transport lifecycle in erlmcp.
%%% Provides fast lookups via ETS, transport type registration, and
%%% process monitoring for automatic cleanup.
%%%
%%% == Features ==
%%% - ETS-based fast lookups for transport resolution
%%% - Transport type registration (stdio, tcp, http, websocket, sse)
%%% - Process lifecycle monitoring with automatic cleanup
%%% - OTP 28+ optimized with decentralized_counters
%%%
%%% == Architecture ==
%%% The registry maintains two ETS tables:
%%% - transports_tab: transport_id -> {pid(), config()} mappings
%%% - types_tab: transport_type -> module() mappings
%%%
%%% Process monitors ensure automatic cleanup when a transport dies.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_transport_registry).

-behaviour(gen_server).

%% API
-export([start_link/0,
         register_transport/3,
         unregister_transport/1,
         resolve_transport/1,
         list_transports/0,
         register_type/2,
         get_type/1,
         get_stats/0,
         health_check/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include_lib("kernel/include/logger.hrl").

%%====================================================================
%% Macros
%%====================================================================

-define(SERVER, ?MODULE).
-define(TRANSPORTS_TAB, erlmcp_transports).
-define(TYPES_TAB, erlmcp_transport_types).

%%====================================================================
%% Records
%%====================================================================

-record(state,
        {transports_tab :: ets:tid(),
         types_tab :: ets:tid(),
         monitors = #{} :: #{reference() => {transport_id(), pid()}}}).

-type transport_id() :: atom() | binary().
-type transport_type() :: stdio | tcp | http | websocket | sse | atom().
-type transport_config() ::
    #{type := transport_type(),
      options => map(),
      metadata => map()}.
-type transport_info() ::
    {transport_id(), pid(), transport_config()}.

-export_type([transport_id/0, transport_type/0, transport_config/0]).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Starts the transport registry.
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Registers an active transport process with its configuration.
%%
%% The transport will be monitored for automatic cleanup if the process dies.
%%
%% @end
-spec register_transport(transport_id(), pid(), transport_config()) ->
    ok | {error, already_registered | term()}.
register_transport(TransportId, Pid, Config)
  when (is_atom(TransportId) orelse is_binary(TransportId)),
       is_pid(Pid),
       is_map(Config) ->
    gen_server:call(?SERVER, {register_transport, TransportId, Pid, Config}).

%% @doc Unregisters a transport by its identifier.
%%
%% Removes the transport from the registry and cancels monitoring.
%% @end
-spec unregister_transport(transport_id()) -> ok | {error, not_found}.
unregister_transport(TransportId) ->
    gen_server:call(?SERVER, {unregister_transport, TransportId}).

%% @doc Resolves a transport identifier to its PID and configuration.
%%
%% Returns the transport information if registered.
%% @end
-spec resolve_transport(transport_id()) ->
    {ok, pid(), transport_config()} | {error, not_found}.
resolve_transport(TransportId) ->
    case ets:lookup(?TRANSPORTS_TAB, TransportId) of
        [{TransportId, Pid, Config}] ->
            {ok, Pid, Config};
        [] ->
            {error, not_found}
    end.

%% @doc Lists all registered transports with their metadata.
%%
%% Returns a list of {TransportId, Pid, Config} tuples.
%% @end
-spec list_transports() -> [transport_info()].
list_transports() ->
    ets:tab2list(?TRANSPORTS_TAB).

%% @doc Registers a transport type (stdio, tcp, http, etc.) with its module.
%%
%% Associates a transport type atom with its implementing module.
%% @end
-spec register_type(transport_type(), module()) ->
    ok | {error, already_exists}.
register_type(TransportType, Module) when is_atom(TransportType), is_atom(Module) ->
    gen_server:call(?SERVER, {register_type, TransportType, Module}).

%% @doc Gets the module implementing a specific transport type.
%%
%% Returns the module name for the given transport type.
%% @end
-spec get_type(transport_type()) -> {ok, module()} | {error, unknown_type}.
get_type(TransportType) when is_atom(TransportType) ->
    case ets:lookup(?TYPES_TAB, TransportType) of
        [{TransportType, Module}] ->
            {ok, Module};
        [] ->
            {error, unknown_type}
    end.

%% @doc Gets registry statistics.
%%
%% Returns counts and memory usage information.
%% @end
-spec get_stats() ->
    #{transport_count := non_neg_integer(),
      type_count := non_neg_integer(),
      monitor_count := non_neg_integer(),
      memory := non_neg_integer()}.
get_stats() ->
    gen_server:call(?SERVER, get_stats).

%% @doc Performs a health check on the registry.
%%
%% Verifies ETS tables are accessible and returns registry status.
%% @end
-spec health_check() ->
    #{status := healthy | degraded,
      transport_count := non_neg_integer(),
      active_monitors := non_neg_integer()}.
health_check() ->
    gen_server:call(?SERVER, health_check).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @private
-spec init([]) -> {ok, #state{}}.
init([]) ->
    process_flag(trap_exit, true),

    %% Create ETS table for transport lookups
    %% Using named_table for direct access from API functions
    %% OTP 28: decentralized_counters for scalability
    TransportsTab = ets:new(?TRANSPORTS_TAB,
                           [set,
                            named_table,
                            public,
                            {read_concurrency, true},
                            {write_concurrency, true},
                            {decentralized_counters, true}]),

    %% Create ETS table for transport type registry
    TypesTab = ets:new(?TYPES_TAB,
                      [set,
                       named_table,
                       public,
                       {read_concurrency, true},
                       {decentralized_counters, true}]),

    %% Register built-in transport types
    register_builtin_types(TypesTab),

    ?LOG_INFO("Transport registry started with OTP 28 optimizations",
              #{transports_tab => TransportsTab,
                types_tab => TypesTab}),

    {ok, #state{transports_tab = TransportsTab,
                types_tab = TypesTab,
                monitors = #{}}}.

%% @private
-spec handle_call(term(), {pid(), term()}, #state{}) ->
    {reply, term(), #state{}}.
handle_call({register_transport, TransportId, Pid, Config}, _From, State) ->
    case ets:lookup(?TRANSPORTS_TAB, TransportId) of
        [] ->
            %% Verify the process is alive before registering
            case erlang:is_process_alive(Pid) of
                true ->
                    %% Store transport info
                    ets:insert(?TRANSPORTS_TAB, {TransportId, Pid, Config}),

                    %% Monitor the process for automatic cleanup
                    Ref = monitor(process, Pid),
                    NewMonitors = maps:put(Ref, {TransportId, Pid}, State#state.monitors),

                    ?LOG_INFO("Transport registered",
                              #{transport_id => TransportId,
                                pid => Pid,
                                type => maps:get(type, Config, unknown)}),

                    {reply, ok, State#state{monitors = NewMonitors}};
                false ->
                    {reply, {error, process_not_alive}, State}
            end;
        [_Existing] ->
            {reply, {error, already_registered}, State}
    end;

handle_call({unregister_transport, TransportId}, _From, State) ->
    case ets:lookup(?TRANSPORTS_TAB, TransportId) of
        [{TransportId, _Pid, _Config}] ->
            ets:delete(?TRANSPORTS_TAB, TransportId),

            %% Cancel monitoring if we were monitoring this process
            NewMonitors = maps:fold(fun
                (Ref, {Tid, _MonPid}, Acc) when Tid =:= TransportId ->
                    demonitor(Ref, [flush]),
                    maps:remove(Ref, Acc);
                (_Ref, _TidMonPid, Acc) ->
                    Acc
            end, State#state.monitors, State#state.monitors),

            ?LOG_INFO("Transport unregistered", #{transport_id => TransportId}),
            {reply, ok, State#state{monitors = NewMonitors}};
        [] ->
            {reply, {error, not_found}, State}
    end;

handle_call({register_type, TransportType, Module}, _From, State) ->
    case ets:lookup(?TYPES_TAB, TransportType) of
        [] ->
            ets:insert(?TYPES_TAB, {TransportType, Module}),
            ?LOG_DEBUG("Transport type registered",
                      #{type => TransportType, module => Module}),
            {reply, ok, State};
        [{TransportType, _ExistingModule}] ->
            {reply, {error, already_exists}, State}
    end;

handle_call(get_stats, _From, State) ->
    TransportCount = ets:info(?TRANSPORTS_TAB, size),
    TypeCount = ets:info(?TYPES_TAB, size),
    MonitorCount = maps:size(State#state.monitors),
    Memory = ets:info(?TRANSPORTS_TAB, memory) + ets:info(?TYPES_TAB, memory),

    Stats = #{transport_count => TransportCount,
              type_count => TypeCount,
              monitor_count => MonitorCount,
              memory => Memory},
    {reply, Stats, State};

handle_call(health_check, _From, State) ->
    TransportCount = ets:info(?TRANSPORTS_TAB, size),
    ActiveMonitors = maps:size(State#state.monitors),

    %% Check if all monitored processes are still alive
    MonitoredAlive = lists:foldl(fun
        ({_Ref, {_Tid, MPid}}, Acc) ->
            case erlang:is_process_alive(MPid) of
                true -> Acc + 1;
                false -> Acc
            end
    end, 0, maps:to_list(State#state.monitors)),

    Status = case MonitoredAlive of
        ActiveMonitors -> healthy;
        _ -> degraded
    end,

    Health = #{status => Status,
               transport_count => TransportCount,
               active_monitors => ActiveMonitors,
               total_monitors => ActiveMonitors},
    {reply, Health, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% @private
-spec handle_cast(term(), #state{}) -> {noreply, #state{}}.
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
-spec handle_info(term(), #state{}) -> {noreply, #state{}}.
handle_info({'DOWN', Ref, process, Pid, Reason}, State) ->
    case maps:get(Ref, State#state.monitors, undefined) of
        undefined ->
            %% Not our monitor - ignore
            {noreply, State};
        {TransportId, _MonPid} ->
            %% Process died - clean up from registry
            ets:delete(?TRANSPORTS_TAB, TransportId),
            NewMonitors = maps:remove(Ref, State#state.monitors),

            ?LOG_WARNING("Transport process died, cleaning up registry",
                        #{transport_id => TransportId,
                          pid => Pid,
                          reason => Reason}),

            {noreply, State#state{monitors = NewMonitors}}
    end;

handle_info(_Info, State) ->
    {noreply, State}.

%% @private
-spec terminate(term(), #state{}) -> ok.
terminate(_Reason, #state{transports_tab = TransportsTab,
                          types_tab = TypesTab,
                          monitors = Monitors}) ->
    %% Demonitor all processes
    maps:foreach(fun(Ref, _Info) ->
        demonitor(Ref, [flush])
    end, Monitors),

    %% Delete ETS tables if we own them
    try
        ets:delete(TransportsTab),
        ets:delete(TypesTab)
    catch
        _:_ ->
            ok
    end,

    ?LOG_INFO("Transport registry terminating"),
    ok.

%% @private
-spec code_change(term(), #state{}, term()) -> {ok, #state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private
%% @doc Registers built-in transport types.
-spec register_builtin_types(ets:tid()) -> ok.
register_builtin_types(TypesTab) ->
    BuiltinTypes = [
        {stdio, erlmcp_transport_stdio},
        {tcp, erlmcp_transport_tcp},
        {http, erlmcp_transport_http},
        {websocket, erlmcp_transport_ws},
        {sse, erlmcp_transport_sse}
    ],

    lists:foreach(fun({Type, Module}) ->
        %% Check if module exists before registering
        case code:is_loaded(Module) of
            false ->
                case erlang:module_loaded(Module) of
                    false ->
                        %% Module not loaded - skip registration
                        %% It will be registered when the module is loaded
                        ok;
                    true ->
                        ets:insert(TypesTab, {Type, Module})
                end;
            _ ->
                ets:insert(TypesTab, {Type, Module})
        end
    end, BuiltinTypes),

    ok.
