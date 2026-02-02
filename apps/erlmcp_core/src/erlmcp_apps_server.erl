-module(erlmcp_apps_server).

-behaviour(gen_server).

-include("erlmcp.hrl").

%% API exports
-export([start_link/0, register_app/3, get_apps/0, start_app/2, stop_app/1, get_app_status/1,
         unregister_app/1]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Types (app_id and app_status defined in erlmcp.hrl)
-type app_manifest() :: map().
-type permissions() :: [binary()].

%% State record
-record(state,
        {apps_ets :: ets:tid(),
         app_list = [] :: [app_id()],
         app_policies :: ets:tid(),
         running_apps = #{} :: #{app_id() => {pid(), reference()}},
         max_apps = 100 :: pos_integer(),
         manifest_validation = true :: boolean()}).

-type state() :: #state{}.

%% App record stored in ETS
-record(app,
        {id :: app_id(),
         manifest :: app_manifest(),
         permissions :: permissions(),
         status :: app_status(),
         pid :: pid() | undefined,
         started_at :: integer() | undefined,
         stopped_at :: integer() | undefined,
         error_count = 0 :: non_neg_integer(),
         last_error :: term() | undefined,
         metadata :: map()}).

%%====================================================================
%% API Functions
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec register_app(app_id(), app_manifest(), permissions()) ->
                      ok | {error, invalid_manifest | already_registered | too_many_apps}.
register_app(AppId, AppManifest, Permissions)
    when is_binary(AppId), is_map(AppManifest), is_list(Permissions) ->
    gen_server:call(?MODULE, {register_app, AppId, AppManifest, Permissions}, 5000).

-spec get_apps() -> {ok, [map()]}.
get_apps() ->
    gen_server:call(?MODULE, get_apps, 5000).

-spec start_app(app_id(), map()) -> {ok, pid()} | {error, term()}.
start_app(AppId, Config) when is_binary(AppId), is_map(Config) ->
    gen_server:call(?MODULE, {start_app, AppId, Config}, 10000).

-spec stop_app(app_id()) -> ok | {error, not_running | not_found}.
stop_app(AppId) when is_binary(AppId) ->
    gen_server:call(?MODULE, {stop_app, AppId}, 5000).

-spec get_app_status(app_id()) -> {ok, app_status(), map()} | {error, not_found}.
get_app_status(AppId) when is_binary(AppId) ->
    gen_server:call(?MODULE, {get_app_status, AppId}, 5000).

-spec unregister_app(app_id()) -> ok | {error, not_found | still_running}.
unregister_app(AppId) when is_binary(AppId) ->
    gen_server:call(?MODULE, {unregister_app, AppId}, 5000).

%%====================================================================
%% gen_server callbacks
%%====================================================================

-spec init([]) -> {ok, state()}.
init([]) ->
    process_flag(trap_exit, true),

    %% Create ETS table for apps with heir pattern
    AppsTid =
        ets:new(erlmcp_apps,
                [set,
                 public,
                 named_table,
                 {keypos, #app.id},
                 {read_concurrency, true},
                 {write_concurrency, false},
                 {heir, whereis(erlmcp_core_sup), []}]),

    %% Create ETS table for app policies (separate sandboxing)
    PoliciesTid =
        ets:new(erlmcp_app_policies,
                [set,
                 public,
                 {read_concurrency, true},
                 {write_concurrency, false},
                 {heir, whereis(erlmcp_core_sup), []}]),

    logger:info("Starting apps server with ETS tables ~p, ~p", [AppsTid, PoliciesTid]),
    {ok, #state{apps_ets = AppsTid, app_policies = PoliciesTid}}.

-spec handle_call(term(), {pid(), term()}, state()) ->
                     {reply, term(), state()} | {noreply, state()}.
handle_call({register_app, AppId, AppManifest, Permissions}, _From, State) ->
    %% Check if we've hit max apps limit
    case length(State#state.app_list) >= State#state.max_apps of
        true ->
            {reply, {error, too_many_apps}, State};
        false ->
            %% Check if app already registered
            case ets:lookup(State#state.apps_ets, AppId) of
                [_] ->
                    {reply, {error, already_registered}, State};
                [] ->
                    %% Validate manifest if enabled
                    case State#state.manifest_validation of
                        true ->
                            case validate_manifest(AppManifest) of
                                ok ->
                                    register_app_internal(AppId, AppManifest, Permissions, State);
                                {error, _} = Error ->
                                    {reply, Error, State}
                            end;
                        false ->
                            register_app_internal(AppId, AppManifest, Permissions, State)
                    end
            end
    end;
handle_call(get_apps, _From, State) ->
    Apps = ets:foldl(fun(App, Acc) -> [app_to_map(App) | Acc] end, [], State#state.apps_ets),
    {reply, {ok, Apps}, State};
handle_call({start_app, AppId, Config}, _From, State) ->
    case ets:lookup(State#state.apps_ets, AppId) of
        [] ->
            {reply, {error, not_found}, State};
        [App] ->
            case App#app.status of
                running ->
                    {reply, {error, already_running}, State};
                _ ->
                    start_app_internal(App, Config, State)
            end
    end;
handle_call({stop_app, AppId}, _From, State) ->
    case ets:lookup(State#state.apps_ets, AppId) of
        [] ->
            {reply, {error, not_found}, State};
        [App] ->
            case App#app.status of
                stopped ->
                    {reply, {error, not_running}, State};
                crashed ->
                    {reply, {error, not_running}, State};
                _ ->
                    stop_app_internal(App, State)
            end
    end;
handle_call({get_app_status, AppId}, _From, State) ->
    case ets:lookup(State#state.apps_ets, AppId) of
        [] ->
            {reply, {error, not_found}, State};
        [App] ->
            StatusMetadata =
                #{status => App#app.status,
                  started_at => App#app.started_at,
                  stopped_at => App#app.stopped_at,
                  error_count => App#app.error_count,
                  last_error => App#app.last_error},
            {reply, {ok, App#app.status, StatusMetadata}, State}
    end;
handle_call({unregister_app, AppId}, _From, State) ->
    case ets:lookup(State#state.apps_ets, AppId) of
        [] ->
            {reply, {error, not_found}, State};
        [App] ->
            case App#app.status of
                running ->
                    {reply, {error, still_running}, State};
                starting ->
                    {reply, {error, still_running}, State};
                _ ->
                    ets:delete(State#state.apps_ets, AppId),
                    ets:delete(State#state.app_policies, AppId),
                    NewAppList = lists:delete(AppId, State#state.app_list),
                    NewRunningApps = maps:remove(AppId, State#state.running_apps),
                    {reply, ok, State#state{app_list = NewAppList, running_apps = NewRunningApps}}
            end
    end;
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info({'DOWN', MonitorRef, process, Pid, Reason}, State) ->
    %% App process crashed, find and update status
    case find_app_by_monitor(MonitorRef, State) of
        {ok, AppId} ->
            case ets:lookup(State#state.apps_ets, AppId) of
                [App] ->
                    logger:warning("App ~p crashed: ~p", [AppId, Reason]),
                    UpdatedApp =
                        App#app{status = crashed,
                                pid = undefined,
                                stopped_at = erlang:system_time(millisecond),
                                error_count = App#app.error_count + 1,
                                last_error = Reason},
                    ets:insert(State#state.apps_ets, UpdatedApp),
                    NewRunningApps = maps:remove(AppId, State#state.running_apps),
                    {noreply, State#state{running_apps = NewRunningApps}};
                [] ->
                    {noreply, State}
            end;
        not_found ->
            logger:warning("Received DOWN for unknown app process ~p", [Pid]),
            {noreply, State}
    end;
handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, State) ->
    logger:info("Apps server terminating, stopping all running apps"),

    %% Stop all running apps gracefully
    maps:foreach(fun(AppId, {Pid, _MonitorRef}) ->
                    logger:info("Stopping app ~p (pid: ~p)", [AppId, Pid]),
                    catch gen_server:stop(Pid, normal, 5000)
                 end,
                 State#state.running_apps),

    %% Clean up ETS tables
    %% OTP 28: Modernize catch to try/catch for better error handling
    try ets:delete(State#state.apps_ets)
    catch
        _:Error1 -> logger:warning("Failed to delete apps_ets: ~p", [Error1])
    end,
    try ets:delete(State#state.app_policies)
    catch
        _:Error2 -> logger:warning("Failed to delete app_policies: ~p", [Error2])
    end,
    ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

-spec validate_manifest(app_manifest()) -> ok | {error, invalid_manifest}.
validate_manifest(Manifest) ->
    %% Required fields
    RequiredFields = [<<"name">>, <<"version">>],
    HasRequired = lists:all(fun(Field) -> maps:is_key(Field, Manifest) end, RequiredFields),

    case HasRequired of
        false ->
            {error, invalid_manifest};
        true ->
            %% Validate version format (simple semver check)
            Version = maps:get(<<"version">>, Manifest),
            case validate_version(Version) of
                ok ->
                    %% Optional: validate dependencies if present
                    case maps:get(<<"dependencies">>, Manifest, []) of
                        Deps when is_list(Deps) ->
                            ok;
                        _ ->
                            {error, invalid_manifest}
                    end;
                error ->
                    {error, invalid_manifest}
            end
    end.

-spec validate_version(binary()) -> ok | error.
validate_version(Version) when is_binary(Version) ->
    %% Simple semver validation: X.Y.Z
    case binary:split(Version, <<".">>, [global]) of
        [Major, Minor, Patch] ->
            case {is_numeric(Major), is_numeric(Minor), is_numeric(Patch)} of
                {true, true, true} ->
                    ok;
                _ ->
                    error
            end;
        _ ->
            error
    end;
validate_version(_) ->
    error.

-spec is_numeric(binary()) -> boolean().
is_numeric(Bin) ->
    try binary_to_integer(Bin) of
        _ ->
            true
    catch
        _:_ ->
            false
    end.

-spec register_app_internal(app_id(), app_manifest(), permissions(), state()) ->
                               {reply, ok, state()}.
register_app_internal(AppId, AppManifest, Permissions, State) ->
    App = #app{id = AppId,
               manifest = AppManifest,
               permissions = Permissions,
               status = stopped,
               metadata = #{registered_at => erlang:system_time(millisecond)}},

    ets:insert(State#state.apps_ets, App),

    %% Create sandbox policy entry
    Policy =
        #{app_id => AppId,
          permissions => Permissions,
          sandbox_ets =>
              ets:new(list_to_atom("app_sandbox_" ++ binary_to_list(AppId)),
                      [set, public, {read_concurrency, true}])},
    ets:insert(State#state.app_policies, {AppId, Policy}),

    NewAppList = [AppId | State#state.app_list],
    {reply, ok, State#state{app_list = NewAppList}}.

-spec start_app_internal(#app{}, map(), state()) -> {reply, {ok, pid()} | {error, term()}, state()}.
start_app_internal(App, Config, State) ->
    AppId = App#app.id,

    %% Start app as a simple gen_server (in real impl, would use supervisor)
    case start_app_process(AppId, App#app.manifest, Config) of
        {ok, Pid} ->
            %% Monitor app process
            MonitorRef = monitor(process, Pid),

            %% Update app status
            UpdatedApp =
                App#app{status = running,
                        pid = Pid,
                        started_at = erlang:system_time(millisecond),
                        stopped_at = undefined},
            ets:insert(State#state.apps_ets, UpdatedApp),

            NewRunningApps = maps:put(AppId, {Pid, MonitorRef}, State#state.running_apps),
            {reply, {ok, Pid}, State#state{running_apps = NewRunningApps}};
        {error, Reason} ->
            %% Update error count
            UpdatedApp = App#app{error_count = App#app.error_count + 1, last_error = Reason},
            ets:insert(State#state.apps_ets, UpdatedApp),
            {reply, {error, Reason}, State}
    end.

-spec stop_app_internal(#app{}, state()) -> {reply, ok, state()}.
stop_app_internal(App, State) ->
    AppId = App#app.id,

    case maps:get(AppId, State#state.running_apps, undefined) of
        undefined ->
            %% Not in running apps, just update status
            UpdatedApp =
                App#app{status = stopped,
                        pid = undefined,
                        stopped_at = erlang:system_time(millisecond)},
            ets:insert(State#state.apps_ets, UpdatedApp),
            {reply, ok, State};
        {Pid, MonitorRef} ->
            %% Stop the process gracefully
            demonitor(MonitorRef, [flush]),
            catch gen_server:stop(Pid, normal, 5000),

            %% Update app status
            UpdatedApp =
                App#app{status = stopped,
                        pid = undefined,
                        stopped_at = erlang:system_time(millisecond)},
            ets:insert(State#state.apps_ets, UpdatedApp),

            NewRunningApps = maps:remove(AppId, State#state.running_apps),
            {reply, ok, State#state{running_apps = NewRunningApps}}
    end.

-spec start_app_process(app_id(), app_manifest(), map()) -> {ok, pid()} | {error, term()}.
start_app_process(AppId, Manifest, Config) ->
    %% Simplified app process starter
    %% In production, this would use a proper supervisor
    try
        Pid = spawn_link(fun() ->
                            logger:info("App ~p started with config ~p", [AppId, Config]),
                            app_loop(AppId, Manifest, Config)
                         end),
        {ok, Pid}
    catch
        _:Reason ->
            {error, Reason}
    end.

-spec app_loop(app_id(), app_manifest(), map()) -> no_return().
app_loop(AppId, Manifest, Config) ->
    receive
        stop ->
            logger:info("App ~p stopping", [AppId]),
            exit(normal);
        {call, From, Request} ->
            Response = handle_app_call(Request, Manifest, Config),
            From ! {response, self(), Response},
            app_loop(AppId, Manifest, Config);
        _Other ->
            app_loop(AppId, Manifest, Config)
    end.

-spec handle_app_call(term(), app_manifest(), map()) -> term().
handle_app_call(status, Manifest, _Config) ->
    {ok,
     #{name => maps:get(<<"name">>, Manifest, <<"unknown">>),
       version => maps:get(<<"version">>, Manifest, <<"0.0.0">>),
       status => running}};
handle_app_call(_Request, _Manifest, _Config) ->
    {error, unknown_request}.

-spec find_app_by_monitor(reference(), state()) -> {ok, app_id()} | not_found.
find_app_by_monitor(MonitorRef, State) ->
    case maps:fold(fun(AppId, {_Pid, MRef}, Acc) ->
                      case MRef =:= MonitorRef of
                          true ->
                              {ok, AppId};
                          false ->
                              Acc
                      end
                   end,
                   not_found,
                   State#state.running_apps)
    of
        {ok, AppId} ->
            {ok, AppId};
        not_found ->
            not_found
    end.

-spec app_to_map(#app{}) -> map().
app_to_map(App) ->
    #{id => App#app.id,
      manifest => App#app.manifest,
      permissions => App#app.permissions,
      status => App#app.status,
      started_at => App#app.started_at,
      stopped_at => App#app.stopped_at,
      error_count => App#app.error_count,
      metadata => App#app.metadata}.
