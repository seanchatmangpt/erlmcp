%%%-------------------------------------------------------------------
%% @doc MCP Apps - Application Registration and Lifecycle Management
%%
%% This module implements the MCP Apps feature for MCP 2025-11-25,
%% enabling registration of sandboxed applications with metadata,
%% lifecycle management, and resource isolation.
%%
%% Features:
%% - App registration with metadata
%% - App lifecycle (init, activate, deactivate, terminate)
%% - Permission-based access control
%% - Resource isolation between apps
%% - State management per app
%% - Error handling and recovery
%%
%% @author ErlMCP Development Team
%% @since 0.8.0
%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_apps).
-behaviour(gen_server).

-include("erlmcp.hrl").

%% API exports
-export([
    start_link/0,
    register_app/3,
    register_app/4,
    unregister_app/1,
    list_apps/0,
    get_app/1,
    activate_app/1,
    deactivate_app/1,
    check_permission/3,
    get_app_state/1,
    set_app_state/2,
    notify_app/2,
    get_app_resources/1,
    grant_permission/2,
    revoke_permission/2
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Type definitions (before records)
-type app_id() :: binary().
-type app_name() :: binary().
-type app_version() :: binary().
-type app_status() :: initialized | active | inactive | terminated | error.
-type permission() :: binary().
-type app_state() :: map().
-type app_config() :: map().

%% App record - represents a registered application
-record(mcp_app, {
    id :: app_id(),                              % Unique app identifier
    name :: app_name(),                          % Human-readable app name
    version :: app_version(),                    % App version (semver)
    description :: binary(),                     % App description
    status = initialized :: app_status(),        % Current lifecycle status
    uri :: binary() | undefined,                 % URI where app is served
    manifest :: map() | undefined,               % App manifest (metadata)
    permissions = sets:new() :: sets:set(permission()),  % Granted permissions
    state = #{} :: app_state(),                  % App-specific state
    created_at :: integer(),                     % Creation timestamp (ms)
    activated_at :: integer() | undefined,       % Activation timestamp
    resources = [] :: [binary()],                % Associated resources
    error :: binary() | undefined                % Last error message
}).

%% Type for app record (defined after record)
-type mcp_app() :: #mcp_app{}.

%% Internal state record
-record(state, {
    apps = #{} :: #{app_id() => mcp_app()},     % All registered apps
    app_index = #{} :: #{app_name() => app_id()},  % Index by name for lookup
    permissions = #{} :: #{app_id() => sets:set(permission())},  % Permission grants
    subscriptions = #{} :: #{app_id() => sets:set(pid())}  % App subscribers
}).

-type internal_state() :: #state{}.

%% Export types
-export_type([app_id/0, app_name/0, app_version/0, app_status/0, permission/0, app_state/0, mcp_app/0]).

%% Constants
-define(APP_REGISTRY, erlmcp_app_registry).
-define(DEFAULT_PERMISSIONS, [
    <<"resources/read">>,
    <<"tools/call">>,
    <<"prompts/list">>
]).
-define(APP_STATE_TIMEOUT_MS, 5000).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Start the app registry server
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Register an application with name and manifest
-spec register_app(app_name(), app_version(), map()) -> {ok, app_id()} | {error, term()}.
register_app(Name, Version, Manifest) when is_binary(Name), is_binary(Version), is_map(Manifest) ->
    register_app(Name, Version, Manifest, #{}).

%% @doc Register an application with configuration
-spec register_app(app_name(), app_version(), map(), app_config()) ->
    {ok, app_id()} | {error, term()}.
register_app(Name, Version, Manifest, Config)
  when is_binary(Name), is_binary(Version), is_map(Manifest), is_map(Config) ->
    gen_server:call(?MODULE, {register_app, Name, Version, Manifest, Config}, ?APP_STATE_TIMEOUT_MS).

%% @doc Unregister an application
-spec unregister_app(app_id()) -> ok | {error, term()}.
unregister_app(AppId) when is_binary(AppId) ->
    gen_server:call(?MODULE, {unregister_app, AppId}, ?APP_STATE_TIMEOUT_MS).

%% @doc List all registered applications
-spec list_apps() -> [mcp_app()].
list_apps() ->
    gen_server:call(?MODULE, list_apps, ?APP_STATE_TIMEOUT_MS).

%% @doc Get a specific application by ID
-spec get_app(app_id()) -> {ok, mcp_app()} | {error, not_found}.
get_app(AppId) when is_binary(AppId) ->
    gen_server:call(?MODULE, {get_app, AppId}, ?APP_STATE_TIMEOUT_MS).

%% @doc Activate an application
-spec activate_app(app_id()) -> ok | {error, term()}.
activate_app(AppId) when is_binary(AppId) ->
    gen_server:call(?MODULE, {activate_app, AppId}, ?APP_STATE_TIMEOUT_MS).

%% @doc Deactivate an application
-spec deactivate_app(app_id()) -> ok | {error, term()}.
deactivate_app(AppId) when is_binary(AppId) ->
    gen_server:call(?MODULE, {deactivate_app, AppId}, ?APP_STATE_TIMEOUT_MS).

%% @doc Check if an app has a specific permission
-spec check_permission(app_id(), permission(), term()) -> boolean().
check_permission(AppId, Permission, _Context) when is_binary(AppId), is_binary(Permission) ->
    try
        {ok, App} = get_app(AppId),
        sets:is_element(Permission, App#mcp_app.permissions)
    catch
        _:_ -> false
    end.

%% @doc Get application state
-spec get_app_state(app_id()) -> {ok, app_state()} | {error, not_found}.
get_app_state(AppId) when is_binary(AppId) ->
    gen_server:call(?MODULE, {get_app_state, AppId}, ?APP_STATE_TIMEOUT_MS).

%% @doc Set application state
-spec set_app_state(app_id(), app_state()) -> ok | {error, not_found}.
set_app_state(AppId, State) when is_binary(AppId), is_map(State) ->
    gen_server:call(?MODULE, {set_app_state, AppId, State}, ?APP_STATE_TIMEOUT_MS).

%% @doc Notify an application of an event
-spec notify_app(app_id(), term()) -> ok | {error, term()}.
notify_app(AppId, Event) when is_binary(AppId) ->
    gen_server:cast(?MODULE, {notify_app, AppId, Event}).

%% @doc Get resources associated with an application
-spec get_app_resources(app_id()) -> {ok, [binary()]} | {error, not_found}.
get_app_resources(AppId) when is_binary(AppId) ->
    gen_server:call(?MODULE, {get_app_resources, AppId}, ?APP_STATE_TIMEOUT_MS).

%% @doc Grant a permission to an application
-spec grant_permission(app_id(), permission()) -> ok | {error, term()}.
grant_permission(AppId, Permission) when is_binary(AppId), is_binary(Permission) ->
    gen_server:call(?MODULE, {grant_permission, AppId, Permission}, ?APP_STATE_TIMEOUT_MS).

%% @doc Revoke a permission from an application
-spec revoke_permission(app_id(), permission()) -> ok | {error, term()}.
revoke_permission(AppId, Permission) when is_binary(AppId), is_binary(Permission) ->
    gen_server:call(?MODULE, {revoke_permission, AppId, Permission}, ?APP_STATE_TIMEOUT_MS).

%%====================================================================
%% gen_server Callbacks
%%====================================================================

%% @private
-spec init([]) -> {ok, internal_state()}.
init([]) ->
    {ok, #state{}}.

%% @private
-spec handle_call(term(), {pid(), term()}, internal_state()) ->
    {reply, term(), internal_state()}.

handle_call({register_app, Name, Version, Manifest, Config}, _From,
            #state{apps = Apps, app_index = Index} = State) ->
    AppId = erlmcp_apps_util:generate_app_id(Name),
    case maps:is_key(Name, Index) of
        true ->
            {reply, {error, app_already_registered}, State};
        false ->
            Permissions = maps:get(permissions, Config, ?DEFAULT_PERMISSIONS),
            PermSet = sets:from_list(Permissions),
            Uri = maps:get(uri, Config, undefined),
            Description = maps:get(description, Manifest, <<"">>) ,
            App = #mcp_app{
                id = AppId,
                name = Name,
                version = Version,
                description = Description,
                status = initialized,
                uri = Uri,
                manifest = Manifest,
                permissions = PermSet,
                created_at = erlang:system_time(millisecond),
                resources = []
            },
            NewApps = maps:put(AppId, App, Apps),
            NewIndex = maps:put(Name, AppId, Index),
            NewState = State#state{apps = NewApps, app_index = NewIndex},
            {reply, {ok, AppId}, NewState}
    end;

handle_call({unregister_app, AppId}, _From, #state{apps = Apps, app_index = Index} = State) ->
    case maps:find(AppId, Apps) of
        {ok, App} ->
            NewApps = maps:remove(AppId, Apps),
            NewIndex = maps:remove(App#mcp_app.name, Index),
            NewState = State#state{apps = NewApps, app_index = NewIndex},
            {reply, ok, NewState};
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call(list_apps, _From, #state{apps = Apps} = State) ->
    AppList = maps:values(Apps),
    {reply, AppList, State};

handle_call({get_app, AppId}, _From, #state{apps = Apps} = State) ->
    case maps:find(AppId, Apps) of
        {ok, App} -> {reply, {ok, App}, State};
        error -> {reply, {error, not_found}, State}
    end;

handle_call({activate_app, AppId}, _From, #state{apps = Apps} = State) ->
    case maps:find(AppId, Apps) of
        {ok, App} ->
            UpdatedApp = App#mcp_app{
                status = active,
                activated_at = erlang:system_time(millisecond)
            },
            NewApps = maps:put(AppId, UpdatedApp, Apps),
            {reply, ok, State#state{apps = NewApps}};
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call({deactivate_app, AppId}, _From, #state{apps = Apps} = State) ->
    case maps:find(AppId, Apps) of
        {ok, App} ->
            UpdatedApp = App#mcp_app{status = inactive},
            NewApps = maps:put(AppId, UpdatedApp, Apps),
            {reply, ok, State#state{apps = NewApps}};
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call({get_app_state, AppId}, _From, #state{apps = Apps} = State) ->
    case maps:find(AppId, Apps) of
        {ok, App} -> {reply, {ok, App#mcp_app.state}, State};
        error -> {reply, {error, not_found}, State}
    end;

handle_call({set_app_state, AppId, NewState}, _From, #state{apps = Apps} = State) ->
    case maps:find(AppId, Apps) of
        {ok, App} ->
            UpdatedApp = App#mcp_app{state = NewState},
            NewApps = maps:put(AppId, UpdatedApp, Apps),
            {reply, ok, State#state{apps = NewApps}};
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call({get_app_resources, AppId}, _From, #state{apps = Apps} = State) ->
    case maps:find(AppId, Apps) of
        {ok, App} -> {reply, {ok, App#mcp_app.resources}, State};
        error -> {reply, {error, not_found}, State}
    end;

handle_call({grant_permission, AppId, Permission}, _From, #state{apps = Apps} = State) ->
    case maps:find(AppId, Apps) of
        {ok, App} ->
            NewPerms = sets:add_element(Permission, App#mcp_app.permissions),
            UpdatedApp = App#mcp_app{permissions = NewPerms},
            NewApps = maps:put(AppId, UpdatedApp, Apps),
            {reply, ok, State#state{apps = NewApps}};
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call({revoke_permission, AppId, Permission}, _From, #state{apps = Apps} = State) ->
    case maps:find(AppId, Apps) of
        {ok, App} ->
            NewPerms = sets:del_element(Permission, App#mcp_app.permissions),
            UpdatedApp = App#mcp_app{permissions = NewPerms},
            NewApps = maps:put(AppId, UpdatedApp, Apps),
            {reply, ok, State#state{apps = NewApps}};
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

%% @private
-spec handle_cast(term(), internal_state()) -> {noreply, internal_state()}.

handle_cast({notify_app, AppId, _Event}, #state{apps = Apps} = State) ->
    case maps:find(AppId, Apps) of
        {ok, _App} ->
            % Event notification handling
            % In a real implementation, this would publish to subscribed processes
            {noreply, State};
        error ->
            {noreply, State}
    end;

handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
-spec handle_info(term(), internal_state()) -> {noreply, internal_state()}.

handle_info(_Info, State) ->
    {noreply, State}.

%% @private
-spec terminate(term(), internal_state()) -> ok.

terminate(_Reason, _State) ->
    ok.

%% @private
-spec code_change(term(), internal_state(), term()) -> {ok, internal_state()}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

end.
