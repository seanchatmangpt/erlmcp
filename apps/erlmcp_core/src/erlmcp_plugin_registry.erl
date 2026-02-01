%%%-------------------------------------------------------------------
%%% @doc
%%% Plugin Registry - Tracks loaded plugins and provides routing
%%%
%%% Uses gproc for O(log N) plugin lookups and routing.
%%% Maintains plugin metadata and state.
%%%
%%% == Features ==
%%% - Plugin registration via gproc
%%% - Metadata storage (name, version, type, state)
%%% - Query API for plugin discovery
%%% - Type-based filtering (validators, formatters, etc.)
%%%
%%% == Process Model ==
%%% - gen_server maintains ETS table for metadata
%%% - gproc provides routing (process-per-plugin)
%%% - Supervised by erlmcp_plugin_sup
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_plugin_registry).

-behaviour(gen_server).

%% API
-export([start_link/0, register_plugin/3, unregister_plugin/1, get_plugin/1, list_plugins/0,
         list_plugins_by_type/1, plugin_exists/1]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-define(SERVER, ?MODULE).
-define(TABLE, erlmcp_plugin_registry_table).

-record(state, {table :: ets:tid()}).
-record(plugin_entry,
        {module :: module(), metadata :: map(), pid :: pid(), registered_at :: integer()}).

%%====================================================================
%% API
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Register a plugin with the registry
-spec register_plugin(module(), map(), pid()) -> ok | {error, term()}.
register_plugin(Module, Metadata, Pid) ->
    gen_server:call(?SERVER, {register_plugin, Module, Metadata, Pid}, 5000).

%% @doc Unregister a plugin from the registry
-spec unregister_plugin(module()) -> ok | {error, term()}.
unregister_plugin(Module) ->
    gen_server:call(?SERVER, {unregister_plugin, Module}, 5000).

%% @doc Get plugin information
-spec get_plugin(module()) -> {ok, map()} | {error, not_found}.
get_plugin(Module) ->
    gen_server:call(?SERVER, {get_plugin, Module}, 5000).

%% @doc List all registered plugins
-spec list_plugins() -> {ok, [map()]}.
list_plugins() ->
    gen_server:call(?SERVER, list_plugins, 5000).

%% @doc List plugins by type
-spec list_plugins_by_type(atom()) -> {ok, [map()]}.
list_plugins_by_type(Type) ->
    gen_server:call(?SERVER, {list_plugins_by_type, Type}, 5000).

%% @doc Check if plugin exists
-spec plugin_exists(module()) -> boolean().
plugin_exists(Module) ->
    gen_server:call(?SERVER, {plugin_exists, Module}, 5000).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    %% Create ETS table for plugin metadata
    Table =
        ets:new(?TABLE,
                [set,
                 named_table,
                 protected,
                 {keypos, #plugin_entry.module},
                 {read_concurrency, true}]),
    {ok, #state{table = Table}}.

handle_call({register_plugin, Module, Metadata, Pid}, _From, State) ->
    case ets:lookup(State#state.table, Module) of
        [] ->
            Entry =
                #plugin_entry{module = Module,
                              metadata = Metadata,
                              pid = Pid,
                              registered_at = erlang:system_time(second)},
            ets:insert(State#state.table, Entry),

            %% Register with gproc for routing
            PluginName = maps:get(name, Metadata),
            PluginType = maps:get(type, Metadata),
            gproc:reg({n, l, {plugin, PluginName}}, Pid),
            gproc:reg({p, l, {plugin_type, PluginType}}, Pid),

            %% Monitor plugin process
            erlang:monitor(process, Pid),

            {reply, ok, State};
        [_Existing] ->
            {reply, {error, already_registered}, State}
    end;
handle_call({unregister_plugin, Module}, _From, State) ->
    case ets:lookup(State#state.table, Module) of
        [#plugin_entry{metadata = Metadata}] ->
            ets:delete(State#state.table, Module),

            %% Unregister from gproc
            PluginName = maps:get(name, Metadata),
            PluginType = maps:get(type, Metadata),
            try
                gproc:unreg({n, l, {plugin, PluginName}}),
                gproc:unreg({p, l, {plugin_type, PluginType}})
            catch
                _:_ ->
                    ok  %% Already unregistered
            end,

            {reply, ok, State};
        [] ->
            {reply, {error, not_found}, State}
    end;
handle_call({get_plugin, Module}, _From, State) ->
    case ets:lookup(State#state.table, Module) of
        [#plugin_entry{metadata = Metadata,
                       pid = Pid,
                       registered_at = RegisteredAt}] ->
            PluginInfo =
                #{module => Module,
                  metadata => Metadata,
                  pid => Pid,
                  registered_at => RegisteredAt,
                  alive => erlang:is_process_alive(Pid)},
            {reply, {ok, PluginInfo}, State};
        [] ->
            {reply, {error, not_found}, State}
    end;
handle_call(list_plugins, _From, State) ->
    Plugins =
        ets:foldl(fun(#plugin_entry{module = Module,
                                    metadata = Metadata,
                                    pid = Pid,
                                    registered_at = RegisteredAt},
                      Acc) ->
                     PluginInfo =
                         #{module => Module,
                           metadata => Metadata,
                           pid => Pid,
                           registered_at => RegisteredAt,
                           alive => erlang:is_process_alive(Pid)},
                     [PluginInfo | Acc]
                  end,
                  [],
                  State#state.table),
    {reply, {ok, Plugins}, State};
handle_call({list_plugins_by_type, Type}, _From, State) ->
    Plugins =
        ets:foldl(fun(#plugin_entry{module = Module,
                                    metadata = Metadata,
                                    pid = Pid,
                                    registered_at = RegisteredAt},
                      Acc) ->
                     case maps:get(type, Metadata) of
                         Type ->
                             PluginInfo =
                                 #{module => Module,
                                   metadata => Metadata,
                                   pid => Pid,
                                   registered_at => RegisteredAt,
                                   alive => erlang:is_process_alive(Pid)},
                             [PluginInfo | Acc];
                         _ ->
                             Acc
                     end
                  end,
                  [],
                  State#state.table),
    {reply, {ok, Plugins}, State};
handle_call({plugin_exists, Module}, _From, State) ->
    Exists = ets:member(State#state.table, Module),
    {reply, Exists, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'DOWN', _Ref, process, Pid, _Reason}, State) ->
    %% Plugin process died - remove from registry
    case ets:match_object(State#state.table, #plugin_entry{pid = Pid, _ = '_'}) of
        [#plugin_entry{module = Module}] ->
            ets:delete(State#state.table, Module),
            ok;
        [] ->
            ok
    end,
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    ets:delete(State#state.table),
    ok.
