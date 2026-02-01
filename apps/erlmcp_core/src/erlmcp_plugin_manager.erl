%%%-------------------------------------------------------------------
%%% @doc
%%% Plugin Manager - Orchestrates plugin lifecycle
%%%
%%% Responsibilities:
%%% - Plugin discovery on startup
%%% - Plugin loading/unloading
%%% - Hook execution (pre/post command)
%%% - Plugin instance management
%%%
%%% == Lifecycle ==
%%% 1. Startup: Discover plugins from filesystem
%%% 2. Load: Validate and load plugin modules
%%% 3. Initialize: Start plugin worker processes
%%% 4. Register: Add to plugin registry
%%% 5. Execute: Run plugin hooks/commands
%%% 6. Shutdown: Graceful plugin termination
%%%
%%% == Process Model ==
%%% - gen_server maintains plugin state
%%% - Delegates to erlmcp_plugin_loader for code loading
%%% - Delegates to erlmcp_plugin_registry for tracking
%%% - Delegates to erlmcp_plugin_worker_sup for workers
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_plugin_manager).
-behaviour(gen_server).

%% API
-export([
    start_link/0,
    discover_and_load_plugins/0,
    load_plugin/1,
    load_plugin/2,
    unload_plugin/1,
    reload_plugin/1,
    list_loaded_plugins/0,
    execute_pre_command_hooks/1,
    execute_post_command_hooks/2,
    call_plugin/3
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2
]).

-define(SERVER, ?MODULE).

-record(state, {
    loaded_plugins :: #{module() => pid()},
    auto_discover :: boolean()
}).

%%====================================================================
%% API
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Discover and load all plugins from configured paths
-spec discover_and_load_plugins() -> {ok, [module()]} | {error, term()}.
discover_and_load_plugins() ->
    gen_server:call(?SERVER, discover_and_load_plugins, 10000).

%% @doc Load a specific plugin module
-spec load_plugin(module()) -> {ok, pid()} | {error, term()}.
load_plugin(Module) ->
    load_plugin(Module, #{}).

%% @doc Load a specific plugin module with options
-spec load_plugin(module(), map()) -> {ok, pid()} | {error, term()}.
load_plugin(Module, Opts) ->
    gen_server:call(?SERVER, {load_plugin, Module, Opts}, 10000).

%% @doc Unload a plugin
-spec unload_plugin(module()) -> ok | {error, term()}.
unload_plugin(Module) ->
    gen_server:call(?SERVER, {unload_plugin, Module}, 10000).

%% @doc Reload a plugin (unload + load)
-spec reload_plugin(module()) -> {ok, pid()} | {error, term()}.
reload_plugin(Module) ->
    gen_server:call(?SERVER, {reload_plugin, Module}, 10000).

%% @doc List all loaded plugins
-spec list_loaded_plugins() -> {ok, [module()]}.
list_loaded_plugins() ->
    gen_server:call(?SERVER, list_loaded_plugins, 5000).

%% @doc Execute pre-command hooks for middleware plugins
-spec execute_pre_command_hooks(term()) -> {ok, term()} | {error, term()}.
execute_pre_command_hooks(Request) ->
    gen_server:call(?SERVER, {execute_pre_command_hooks, Request}, 10000).

%% @doc Execute post-command hooks for middleware plugins
-spec execute_post_command_hooks(term(), term()) -> {ok, term()} | {error, term()}.
execute_post_command_hooks(Request, Response) ->
    gen_server:call(?SERVER, {execute_post_command_hooks, Request, Response}, 10000).

%% @doc Call a plugin function
-spec call_plugin(module(), atom(), [term()]) -> term().
call_plugin(Module, Function, Args) ->
    gen_server:call(?SERVER, {call_plugin, Module, Function, Args}, 10000).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    %% Check if auto-discovery is enabled
    AutoDiscover = application:get_env(erlmcp_core, plugin_auto_discover, true),

    State = #state{
        loaded_plugins = #{},
        auto_discover = AutoDiscover
    },

    %% Schedule auto-discovery if enabled
    case AutoDiscover of
        true ->
            self() ! discover_plugins;
        false ->
            ok
    end,

    {ok, State}.

handle_call(discover_and_load_plugins, _From, State) ->
    case do_discover_and_load(State) of
        {ok, Modules, NewState} ->
            {reply, {ok, Modules}, NewState};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({load_plugin, Module, Opts}, _From, State) ->
    case do_load_plugin(Module, Opts, State) of
        {ok, Pid, NewState} ->
            {reply, {ok, Pid}, NewState};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({unload_plugin, Module}, _From, State) ->
    case do_unload_plugin(Module, State) of
        {ok, NewState} ->
            {reply, ok, NewState};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({reload_plugin, Module}, _From, State) ->
    case do_reload_plugin(Module, State) of
        {ok, Pid, NewState} ->
            {reply, {ok, Pid}, NewState};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call(list_loaded_plugins, _From, State) ->
    Modules = maps:keys(State#state.loaded_plugins),
    {reply, {ok, Modules}, State};

handle_call({execute_pre_command_hooks, Request}, _From, State) ->
    case do_execute_pre_hooks(Request, State) of
        {ok, ModifiedRequest} ->
            {reply, {ok, ModifiedRequest}, State};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({execute_post_command_hooks, Request, Response}, _From, State) ->
    case do_execute_post_hooks(Request, Response, State) of
        {ok, ModifiedResponse} ->
            {reply, {ok, ModifiedResponse}, State};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({call_plugin, Module, Function, Args}, _From, State) ->
    case maps:get(Module, State#state.loaded_plugins, undefined) of
        undefined ->
            {reply, {error, plugin_not_loaded}, State};
        Pid ->
            try
                Result = gen_server:call(Pid, {call_function, Function, Args}, 5000),
                {reply, Result, State}
            catch
                _:Error:Stack ->
                    {reply, {error, {plugin_call_failed, Error, Stack}}, State}
            end
    end.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(discover_plugins, State) ->
    case do_discover_and_load(State) of
        {ok, _Modules, NewState} ->
            {noreply, NewState};
        {error, _Reason} ->
            {noreply, State}
    end;

handle_info({'DOWN', _Ref, process, Pid, _Reason}, State) ->
    %% Plugin worker died - remove from loaded plugins
    LoadedPlugins = State#state.loaded_plugins,
    NewLoadedPlugins = maps:filter(fun(_Module, WorkerPid) ->
        WorkerPid =/= Pid
    end, LoadedPlugins),
    {noreply, State#state{loaded_plugins = NewLoadedPlugins}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    %% Gracefully unload all plugins
    maps:foreach(fun(Module, _Pid) ->
        do_unload_plugin(Module, State)
    end, State#state.loaded_plugins),
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

%% @private Discover and load all plugins
do_discover_and_load(State) ->
    try
        %% Discover plugins
        case erlmcp_plugin_loader:discover_plugins() of
            {ok, Modules} ->
                %% Load each discovered plugin
                {LoadedModules, NewState} = lists:foldl(fun(Module, {Acc, StateAcc}) ->
                    case do_load_plugin(Module, #{}, StateAcc) of
                        {ok, _Pid, NewStateAcc} ->
                            {[Module | Acc], NewStateAcc};
                        {error, _Reason} ->
                            %% Skip plugins that fail to load
                            {Acc, StateAcc}
                    end
                end, {[], State}, Modules),

                {ok, lists:reverse(LoadedModules), NewState};
            {error, Reason} ->
                {error, {discovery_failed, Reason}}
        end
    catch
        _:Error:Stack ->
            {error, {load_error, Error, Stack}}
    end.

%% @private Load a plugin
do_load_plugin(Module, Opts, State) ->
    try
        %% Check if already loaded
        case maps:is_key(Module, State#state.loaded_plugins) of
            true ->
                {error, already_loaded};
            false ->
                %% Load plugin module
                case erlmcp_plugin_loader:load_plugin(Module) of
                    {ok, Module} ->
                        %% Get plugin metadata
                        Metadata = Module:metadata(),

                        %% Start plugin worker
                        case erlmcp_plugin_worker_sup:start_plugin(Module, Opts) of
                            {ok, Pid} ->
                                %% Register plugin
                                ok = erlmcp_plugin_registry:register_plugin(Module, Metadata, Pid),

                                %% Monitor plugin worker
                                erlang:monitor(process, Pid),

                                %% Update state
                                NewLoadedPlugins = maps:put(Module, Pid, State#state.loaded_plugins),
                                NewState = State#state{loaded_plugins = NewLoadedPlugins},

                                {ok, Pid, NewState};
                            {error, Reason} ->
                                {error, {worker_start_failed, Reason}}
                        end;
                    {error, Reason} ->
                        {error, {load_failed, Reason}}
                end
        end
    catch
        _:Error:Stack ->
            {error, {load_error, Error, Stack}}
    end.

%% @private Unload a plugin
do_unload_plugin(Module, State) ->
    try
        case maps:get(Module, State#state.loaded_plugins, undefined) of
            undefined ->
                {error, not_loaded};
            Pid ->
                %% Stop plugin worker
                erlmcp_plugin_worker_sup:stop_plugin(Pid),

                %% Unregister plugin
                ok = erlmcp_plugin_registry:unregister_plugin(Module),

                %% Unload module
                ok = erlmcp_plugin_loader:unload_plugin(Module),

                %% Update state
                NewLoadedPlugins = maps:remove(Module, State#state.loaded_plugins),
                NewState = State#state{loaded_plugins = NewLoadedPlugins},

                {ok, NewState}
        end
    catch
        _:Error:Stack ->
            {error, {unload_error, Error, Stack}}
    end.

%% @private Reload a plugin
do_reload_plugin(Module, State) ->
    try
        %% Unload first
        case do_unload_plugin(Module, State) of
            {ok, IntermediateState} ->
                %% Load again
                do_load_plugin(Module, #{}, IntermediateState);
            {error, not_loaded} ->
                %% Not loaded, just load it
                do_load_plugin(Module, #{}, State);
            {error, Reason} ->
                {error, Reason}
        end
    catch
        _:Error:Stack ->
            {error, {reload_error, Error, Stack}}
    end.

%% @private Execute pre-command hooks
do_execute_pre_hooks(Request, State) ->
    %% Get middleware plugins
    case erlmcp_plugin_registry:list_plugins_by_type(middleware) of
        {ok, Plugins} ->
            %% Execute each middleware's pre_execute hook
            lists:foldl(fun(#{module := Module, pid := Pid}, {ok, Req}) ->
                try
                    case erlang:is_process_alive(Pid) of
                        true ->
                            gen_server:call(Pid, {call_function, pre_execute, [Req]}, 5000);
                        false ->
                            {ok, Req}  %% Skip dead plugins
                    end
                catch
                    _:_Error ->
                        {ok, Req}  %% Skip failed plugins
                end;
            (_, {error, _} = Error) ->
                Error
            end, {ok, Request}, Plugins);
        {error, Reason} ->
            {error, Reason}
    end.

%% @private Execute post-command hooks
do_execute_post_hooks(_Request, Response, State) ->
    %% Get middleware plugins
    case erlmcp_plugin_registry:list_plugins_by_type(middleware) of
        {ok, Plugins} ->
            %% Execute each middleware's post_execute hook
            lists:foldl(fun(#{module := Module, pid := Pid}, {ok, Resp}) ->
                try
                    case erlang:is_process_alive(Pid) of
                        true ->
                            gen_server:call(Pid, {call_function, post_execute, [Resp]}, 5000);
                        false ->
                            {ok, Resp}  %% Skip dead plugins
                    end
                catch
                    _:_Error ->
                        {ok, Resp}  %% Skip failed plugins
                end;
            (_, {error, _} = Error) ->
                Error
            end, {ok, Response}, Plugins);
        {error, Reason} ->
            {error, Reason}
    end.
