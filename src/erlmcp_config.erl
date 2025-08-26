%% @doc Configuration Management Module
%% Provides centralized configuration management with dynamic updates,
%% environment variable support, validation, and hot reload capabilities.
%%
%% Features:
%% - Load configuration from sys.config files
%% - Environment variable overrides
%% - Dynamic runtime reconfiguration
%% - Configuration validation
%% - Hot reload without restart
%% - Configuration backup/restore
%% - Schema-based validation
%% - Migration from old config formats
-module(erlmcp_config).

-behaviour(gen_server).

%% API
-export([start_link/0, stop/0, get/1, get/2, set_config/1, update_config/1, load_config/1,
         load_and_watch/1, stop_watching/0, reload_config/0, validate_config/1,
         reset_to_defaults/0, apply_env_overrides/0, get_server_config/1, set_server_config/1,
         validate_server_config/1, get_transport_config/1, set_transport_configs/1,
         validate_transport_config/1, get_enabled_transports/0, merge_configs/2, persist_config/1,
         backup_config/1, restore_config/1, migrate_config/1, get_all_config/0,
         subscribe_changes/0, unsubscribe_changes/0]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-include("erlmcp.hrl").

%% Configuration record
-record(config_state,
        {config = #{} :: map(),
         config_file :: string() | undefined,
         file_watcher :: pid() | undefined,
         subscribers = [] :: [pid()],
         schema :: map() | undefined}).

%% Default configuration values
-define(DEFAULT_CONFIG,
        #{server_name => erlmcp_server,
          port => 8080,
          max_connections => 100,
          timeout => 5000,
          acceptors => 10,
          socket_opts => [{reuseaddr, true}],
          protocol_opts => [],
          transports =>
              [{stdio,
                #{enabled => true,
                  buffer_size => 1024,
                  timeout => 5000}},
               {tcp,
                #{enabled => false,
                  port => 8080,
                  acceptors => 10}},
               {http,
                #{enabled => false,
                  port => 8081,
                  path => "/mcp"}}],
          log_level => info,
          log_file => undefined,
          metrics_enabled => true,
          tracing_enabled => false}).
%% Environment variable mappings
-define(ENV_MAPPINGS,
        #{"ERLMCP_SERVER_NAME" => server_name,
          "ERLMCP_PORT" => {port, fun erlang:list_to_integer/1},
          "ERLMCP_MAX_CONNECTIONS" => {max_connections, fun erlang:list_to_integer/1},
          "ERLMCP_TIMEOUT" => {timeout, fun erlang:list_to_integer/1},
          "ERLMCP_LOG_LEVEL" => {log_level, fun erlang:list_to_atom/1},
          "ERLMCP_LOG_FILE" => log_file,
          "ERLMCP_METRICS_ENABLED" => {metrics_enabled, fun string_to_boolean/1},
          "ERLMCP_TRACING_ENABLED" => {tracing_enabled, fun string_to_boolean/1}}).

%%--------------------------------------------------------------------
%% API Functions
%%--------------------------------------------------------------------

%% @doc Start the configuration server
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Stop the configuration server
-spec stop() -> ok.
stop() ->
    gen_server:stop(?MODULE).

%% @doc Get a configuration value
-spec get(atom()) -> term().
get(Key) ->
    get(Key, undefined).

%% @doc Get a configuration value with default
-spec get(atom(), term()) -> term().
get(Key, Default) ->
    try
        gen_server:call(?MODULE, {get, Key, Default})
    catch
        exit:{noproc, _} ->
            %% Server not started, return from default config
            maps:get(Key, ?DEFAULT_CONFIG, Default)
    end.

%% @doc Set entire configuration
-spec set_config(list() | map()) -> ok | {error, term()}.
set_config(Config) when is_list(Config) ->
    set_config(maps:from_list(Config));
set_config(Config) when is_map(Config) ->
    case validate_config(Config) of
        ok ->
            gen_server:call(?MODULE, {set_config, Config});
        Error ->
            Error
    end.

%% @doc Update configuration with new values
-spec update_config(list() | map()) -> ok | {error, term()}.
update_config(Updates) when is_list(Updates) ->
    update_config(maps:from_list(Updates));
update_config(Updates) when is_map(Updates) ->
    gen_server:call(?MODULE, {update_config, Updates}).

%% @doc Load configuration from file
-spec load_config(string()) -> {ok, map()} | {error, term()}.
load_config(ConfigFile) ->
    try
        case file:consult(ConfigFile) of
            {ok, [Config]} when is_list(Config) ->
                %% Extract erlmcp configuration
                ErlmcpConfig =
                    case lists:keyfind(erlmcp, 1, Config) of
                        {erlmcp, AppConfig} ->
                            AppConfig;
                        false ->
                            Config
                    end,
                ConfigMap = maps:from_list(ErlmcpConfig),
                case validate_config(ConfigMap) of
                    ok ->
                        gen_server:call(?MODULE, {set_config, ConfigMap}),
                        {ok, ConfigMap};
                    Error ->
                        Error
                end;
            {ok, Config} when is_list(Config) ->
                {error, {invalid_config_format, "Expected single term"}};
            {error, Reason} ->
                {error, {file_error, Reason}}
        end
    catch
        Class:CReason:CStacktrace ->
            {error, {exception, Class, CReason, CStacktrace}}
    end.

%% @doc Load configuration and watch for changes
-spec load_and_watch(string()) -> ok | {error, term()}.
load_and_watch(ConfigFile) ->
    case load_config(ConfigFile) of
        {ok, _} ->
            gen_server:call(?MODULE, {watch_file, ConfigFile});
        Error ->
            Error
    end.

%% @doc Stop watching configuration file
-spec stop_watching() -> ok.
stop_watching() ->
    gen_server:call(?MODULE, stop_watching).

%% @doc Reload configuration from watched file
-spec reload_config() -> ok | {error, term()}.
reload_config() ->
    gen_server:call(?MODULE, reload_config).

%% @doc Validate configuration
-spec validate_config(term()) -> ok | {error, term()}.
validate_config(Config) when is_map(Config) ->
    try
        erlmcp_config_schema:validate(Config)
    catch
        error:undef ->
            %% Schema module not available, do basic validation
            basic_validate_config(Config);
        Class:CReason:CStacktrace ->
            {error, {validation_exception, Class, CReason, CStacktrace}}
    end;
validate_config(Config) when is_list(Config) ->
    validate_config(maps:from_list(Config));
validate_config(_Config) ->
    {error, {validation_failed, "Configuration must be a map or property list"}}.

%% @doc Reset configuration to defaults
-spec reset_to_defaults() -> ok.
reset_to_defaults() ->
    gen_server:call(?MODULE, reset_to_defaults).

%% @doc Apply environment variable overrides
-spec apply_env_overrides() -> ok.
apply_env_overrides() ->
    gen_server:call(?MODULE, apply_env_overrides).

%% @doc Get server configuration value
-spec get_server_config(atom()) -> term().
get_server_config(Key) ->
    ?MODULE:get(Key).

%% @doc Set server configuration
-spec set_server_config(list() | map()) -> ok | {error, term()}.
set_server_config(Config) ->
    case validate_server_config(Config) of
        ok ->
            update_config(Config);
        Error ->
            Error
    end.

%% @doc Validate server configuration
-spec validate_server_config(term()) -> ok | {error, term()}.
validate_server_config(Config) when is_list(Config) ->
    validate_server_config(maps:from_list(Config));
validate_server_config(Config) when is_map(Config) ->
    %% Basic server config validation
    try
        maps:fold(fun validate_server_key/3, ok, Config)
    catch
        Error ->
            Error
    end;
validate_server_config(_) ->
    {error, {validation_failed, "Server config must be a map or property list"}}.

%% @doc Get transport configuration
-spec get_transport_config(atom()) -> map() | undefined.
get_transport_config(Transport) ->
    Transports = get(transports, []),
    case lists:keyfind(Transport, 1, Transports) of
        {Transport, Config} ->
            Config;
        false ->
            undefined
    end.

%% @doc Set transport configurations
-spec set_transport_configs(list()) -> ok | {error, term()}.
set_transport_configs(Configs) when is_list(Configs) ->
    %% Validate all transport configs
    case validate_all_transports(Configs) of
        ok ->
            update_config([{transports, Configs}]);
        Error ->
            Error
    end.

%% @doc Validate transport configuration
-spec validate_transport_config({atom(), map()}) -> ok | {error, term()}.
validate_transport_config({Transport, Config}) when is_atom(Transport), is_map(Config) ->
    case Transport of
        stdio ->
            validate_stdio_config(Config);
        tcp ->
            validate_tcp_config(Config);
        http ->
            validate_http_config(Config);
        _ ->
            {error, {unknown_transport, Transport}}
    end;
validate_transport_config(_) ->
    {error, {validation_failed, "Transport config must be {atom(), map()}"}}.

%% @doc Get list of enabled transports
-spec get_enabled_transports() -> [atom()].
get_enabled_transports() ->
    Transports = get(transports, []),
    [Transport || {Transport, Config} <- Transports, maps:get(enabled, Config, false)].

%% @doc Merge two configuration maps
-spec merge_configs(map() | list(), map() | list()) -> map().
merge_configs(Base, Override) when is_list(Base) ->
    merge_configs(maps:from_list(Base), Override);
merge_configs(Base, Override) when is_list(Override) ->
    merge_configs(Base, maps:from_list(Override));
merge_configs(Base, Override) when is_map(Base), is_map(Override) ->
    maps:fold(fun merge_config_key/3, Base, Override).

%% @doc Persist current configuration to file
-spec persist_config(string()) -> ok | {error, term()}.
persist_config(File) ->
    Config = get_all_config(),
    ConfigTerm = [{erlmcp, maps:to_list(Config)}],
    Content = io_lib:format("~p.~n", [ConfigTerm]),
    file:write_file(File, Content).

%% @doc Backup current configuration
-spec backup_config(string()) -> ok | {error, term()}.
backup_config(BackupFile) ->
    persist_config(BackupFile).

%% @doc Restore configuration from backup
-spec restore_config(string()) -> ok | {error, term()}.
restore_config(BackupFile) ->
    load_config(BackupFile).

%% @doc Migrate configuration from old format
-spec migrate_config(list()) -> {ok, map()} | {error, term()}.
migrate_config(OldConfig) when is_list(OldConfig) ->
    try
        %% Check for version
        Version =
            case lists:keyfind(config_version, 1, OldConfig) of
                {config_version, V} ->
                    V;
                false ->
                    "1.0" % Assume old version
            end,

        %% Apply migration based on version
        MigratedConfig =
            case Version of
                "1.0" ->
                    migrate_from_v1_0(OldConfig);
                "2.0" ->
                    migrate_from_v2_0(OldConfig);
                _ ->
                    maps:from_list(OldConfig) % No migration needed
            end,

        {ok, MigratedConfig}
    catch
        Class:CReason:CStacktrace ->
            {error, {migration_failed, Class, CReason, CStacktrace}}
    end.

%% @doc Get all configuration as map
-spec get_all_config() -> map().
get_all_config() ->
    gen_server:call(?MODULE, get_all_config).

%% @doc Subscribe to configuration changes
-spec subscribe_changes() -> ok.
subscribe_changes() ->
    gen_server:call(?MODULE, {subscribe, self()}).

%% @doc Unsubscribe from configuration changes
-spec unsubscribe_changes() -> ok.
unsubscribe_changes() ->
    gen_server:call(?MODULE, {unsubscribe, self()}).

%%--------------------------------------------------------------------
%% gen_server Callbacks
%%--------------------------------------------------------------------

init([]) ->
    %% Load schema if available
    Schema =
        try
            erlmcp_config_schema:get_schema()
        catch
            error:undef ->
                undefined
        end,

    State = #config_state{config = ?DEFAULT_CONFIG, schema = Schema},

    %% Apply environment overrides on startup
    {ok, NewState} = handle_call(apply_env_overrides, undefined, State),

    {ok, NewState}.

handle_call({get, Key, Default}, _From, #config_state{config = Config} = State) ->
    Value = maps:get(Key, Config, Default),
    {reply, Value, State};
handle_call({set_config, NewConfig}, _From, State) ->
    NewState = State#config_state{config = NewConfig},
    notify_subscribers(config_changed, NewConfig, NewState),
    {reply, ok, NewState};
handle_call({update_config, Updates}, _From, #config_state{config = Config} = State) ->
    NewConfig = maps:merge(Config, Updates),
    case validate_config(NewConfig) of
        ok ->
            NewState = State#config_state{config = NewConfig},
            notify_subscribers(config_updated, Updates, NewState),
            {reply, ok, NewState};
        Error ->
            {reply, Error, State}
    end;
handle_call({watch_file, File}, _From, State) ->
    %% Stop existing watcher if any
    case State#config_state.file_watcher of
        undefined ->
            ok;
        Pid ->
            exit(Pid, normal)
    end,

    %% Start new file watcher
    Watcher = spawn_link(fun() -> file_watcher(File) end),
    NewState = State#config_state{config_file = File, file_watcher = Watcher},
    {reply, ok, NewState};
handle_call(stop_watching, _From, State) ->
    case State#config_state.file_watcher of
        undefined ->
            ok;
        Pid ->
            exit(Pid, normal)
    end,
    NewState = State#config_state{config_file = undefined, file_watcher = undefined},
    {reply, ok, NewState};
handle_call(reload_config, _From, #config_state{config_file = undefined} = State) ->
    {reply, {error, no_config_file}, State};
handle_call(reload_config, _From, #config_state{config_file = File} = State) ->
    case load_config_from_file(File) of
        {ok, NewConfig} ->
            NewState = State#config_state{config = NewConfig},
            notify_subscribers(config_reloaded, NewConfig, NewState),
            {reply, ok, NewState};
        Error ->
            {reply, Error, State}
    end;
handle_call(reset_to_defaults, _From, State) ->
    NewState = State#config_state{config = ?DEFAULT_CONFIG},
    notify_subscribers(config_reset, ?DEFAULT_CONFIG, NewState),
    {reply, ok, NewState};
handle_call(apply_env_overrides, _From, #config_state{config = Config} = State) ->
    NewConfig = apply_env_overrides_to_config(Config),
    NewState = State#config_state{config = NewConfig},
    {reply, ok, NewState};
handle_call(get_all_config, _From, #config_state{config = Config} = State) ->
    {reply, Config, State};
handle_call({subscribe, Pid}, _From, #config_state{subscribers = Subs} = State) ->
    monitor(process, Pid),
    NewState = State#config_state{subscribers = [Pid | Subs]},
    {reply, ok, NewState};
handle_call({unsubscribe, Pid}, _From, #config_state{subscribers = Subs} = State) ->
    demonitor(Pid, [flush]),
    NewSubs = lists:delete(Pid, Subs),
    NewState = State#config_state{subscribers = NewSubs},
    {reply, ok, NewState};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({file_changed, File}, #config_state{config_file = File} = State) ->
    %% Automatically reload configuration when file changes
    case load_config_from_file(File) of
        {ok, NewConfig} ->
            NewState = State#config_state{config = NewConfig},
            notify_subscribers(config_file_changed, NewConfig, NewState),
            {noreply, NewState};
        _Error ->
            %% Log error but don't change state
            {noreply, State}
    end;
handle_info({'DOWN', _Ref, process, Pid, _Reason},
            #config_state{subscribers = Subs} = State) ->
    NewSubs = lists:delete(Pid, Subs),
    NewState = State#config_state{subscribers = NewSubs},
    {noreply, NewState};
handle_info({'EXIT', Pid, _Reason}, #config_state{file_watcher = Pid} = State) ->
    %% File watcher died, clear it
    NewState = State#config_state{file_watcher = undefined},
    {noreply, NewState};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #config_state{file_watcher = Watcher}) ->
    case Watcher of
        undefined ->
            ok;
        Pid ->
            exit(Pid, normal)
    end,
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% Internal Functions
%%--------------------------------------------------------------------

%% @doc Basic configuration validation when schema is not available
basic_validate_config(Config) when is_map(Config) ->
    RequiredKeys = [server_name, port],
    case check_required_keys(RequiredKeys, Config) of
        ok ->
            validate_config_types(Config);
        Error ->
            Error
    end.

%% @doc Check if required keys are present
check_required_keys([], _Config) ->
    ok;
check_required_keys([Key | Rest], Config) ->
    case maps:is_key(Key, Config) orelse maps:is_key(Key, ?DEFAULT_CONFIG) of
        true ->
            check_required_keys(Rest, Config);
        false ->
            {error, {missing_required_key, Key}}
    end.

%% @doc Validate configuration value types
validate_config_types(Config) ->
    try
        maps:fold(fun validate_config_type/3, ok, Config)
    catch
        Error ->
            Error
    end.

validate_config_type(server_name, Value, ok) when is_atom(Value) ->
    ok;
validate_config_type(port, Value, ok) when is_integer(Value), Value > 0, Value < 65536 ->
    ok;
validate_config_type(max_connections, Value, ok) when is_integer(Value), Value > 0 ->
    ok;
validate_config_type(timeout, Value, ok) when is_integer(Value), Value > 0 ->
    ok;
validate_config_type(acceptors, Value, ok) when is_integer(Value), Value > 0 ->
    ok;
validate_config_type(log_level, Value, ok)
    when Value =:= debug; Value =:= info; Value =:= warning; Value =:= error ->
    ok;
validate_config_type(transports, Value, ok) when is_list(Value) ->
    validate_all_transports(Value);
validate_config_type(Key, Value, ok) ->
    %% Allow unknown keys for extensibility
    ok;
validate_config_type(Key, Value, ok) ->
    throw({error, {invalid_type, Key, Value}}).

%% @doc Validate server configuration key
validate_server_key(acceptors, Value, ok) when is_integer(Value), Value > 0 ->
    ok;
validate_server_key(server_name, Value, ok) when is_atom(Value) ->
    ok;
validate_server_key(port, Value, ok) when is_integer(Value), Value > 0, Value < 65536 ->
    ok;
validate_server_key(socket_opts, Value, ok) when is_list(Value) ->
    ok;
validate_server_key(protocol_opts, Value, ok) when is_list(Value) ->
    ok;
validate_server_key(Key, _Value, ok) ->
    throw({error, {invalid_server_key, Key}}).

%% @doc Validate all transport configurations
validate_all_transports([]) ->
    ok;
validate_all_transports([Transport | Rest]) ->
    case validate_transport_config(Transport) of
        ok ->
            validate_all_transports(Rest);
        Error ->
            Error
    end.

%% @doc Validate stdio transport configuration
validate_stdio_config(Config) ->
    AllowedKeys = [enabled, buffer_size, timeout],
    validate_transport_keys(Config, AllowedKeys).

%% @doc Validate TCP transport configuration
validate_tcp_config(Config) ->
    AllowedKeys = [enabled, port, acceptors, socket_opts],
    case validate_transport_keys(Config, AllowedKeys) of
        ok ->
            %% Additional TCP-specific validation
            case maps:get(port, Config, undefined) of
                Port when is_integer(Port), Port > 0, Port < 65536 ->
                    ok;
                undefined ->
                    ok; % Optional
                _ ->
                    {error, {invalid_tcp_port, maps:get(port, Config)}}
            end;
        Error ->
            Error
    end.

%% @doc Validate HTTP transport configuration
validate_http_config(Config) ->
    AllowedKeys = [enabled, port, path, ssl_opts],
    validate_transport_keys(Config, AllowedKeys).

%% @doc Validate transport configuration keys
validate_transport_keys(Config, AllowedKeys) ->
    ConfigKeys = maps:keys(Config),
    case lists:all(fun(Key) -> lists:member(Key, AllowedKeys) end, ConfigKeys) of
        true ->
            ok;
        false ->
            InvalidKeys = ConfigKeys -- AllowedKeys,
            {error, {invalid_transport_keys, InvalidKeys}}
    end.

%% @doc Merge configuration key with special handling
merge_config_key(transports, NewTransports, Base) when is_list(NewTransports) ->
    %% Merge transport configurations
    OldTransports = maps:get(transports, Base, []),
    MergedTransports = merge_transports(OldTransports, NewTransports),
    maps:put(transports, MergedTransports, Base);
merge_config_key(Key, Value, Base) ->
    maps:put(Key, Value, Base).

%% @doc Merge transport configurations
merge_transports(Old, New) ->
    %% Convert to maps for easier merging
    OldMap = maps:from_list(Old),
    NewMap = maps:from_list(New),
    MergedMap = maps:merge(OldMap, NewMap),
    maps:to_list(MergedMap).

%% @doc Apply environment variable overrides to configuration
apply_env_overrides_to_config(Config) ->
    maps:fold(fun apply_env_override/3, Config, ?ENV_MAPPINGS).

apply_env_override(EnvVar, ConfigKey, Config) ->
    case os:getenv(EnvVar) of
        false ->
            Config;
        Value ->
            case ConfigKey of
                {Key, Converter} ->
                    try
                        ConvertedValue = Converter(Value),
                        maps:put(Key, ConvertedValue, Config)
                    catch
                        _:_ ->
                            Config % Ignore conversion errors
                    end;
                Key ->
                    maps:put(Key, Value, Config)
            end
    end.

%% @doc Convert string to boolean
string_to_boolean("true") ->
    true;
string_to_boolean("false") ->
    false;
string_to_boolean("1") ->
    true;
string_to_boolean("0") ->
    false;
string_to_boolean(_) ->
    throw({error, invalid_boolean}).

%% @doc Load configuration from file (internal)
load_config_from_file(File) ->
    case file:consult(File) of
        {ok, [Config]} when is_list(Config) ->
            ErlmcpConfig =
                case lists:keyfind(erlmcp, 1, Config) of
                    {erlmcp, AppConfig} ->
                        AppConfig;
                    false ->
                        Config
                end,
            ConfigMap = maps:from_list(ErlmcpConfig),
            case validate_config(ConfigMap) of
                ok ->
                    {ok, ConfigMap};
                Error ->
                    Error
            end;
        {error, Reason} ->
            {error, {file_error, Reason}}
    end.

%% @doc File watcher process
file_watcher(File) ->
    case filelib:last_modified(File) of
        0 ->
            timer:sleep(1000),
            file_watcher(File);
        LastModified ->
            timer:sleep(1000),
            case filelib:last_modified(File) of
                LastModified ->
                    file_watcher(File);
                _ ->
                    ?MODULE ! {file_changed, File},
                    file_watcher(File)
            end
    end.

%% @doc Notify all subscribers of configuration changes
notify_subscribers(Event, Data, #config_state{subscribers = Subscribers}) ->
    lists:foreach(fun(Pid) -> Pid ! {config_event, Event, Data} end, Subscribers).

%% @doc Migration from version 1.0
migrate_from_v1_0(Config) ->
    Mappings = [{name, server_name}, {tcp_port, port}, {connections, max_connections}],
    migrate_keys(Config, Mappings).

%% @doc Migration from version 2.0
migrate_from_v2_0(Config) ->
    %% Minimal changes from 2.0
    maps:from_list(Config).

%% @doc Apply key mappings for migration
migrate_keys(Config, []) ->
    maps:from_list(Config);
migrate_keys(Config, [{OldKey, NewKey} | Rest]) ->
    case lists:keytake(OldKey, 1, Config) of
        {value, {OldKey, Value}, NewConfig} ->
            migrate_keys([{NewKey, Value} | NewConfig], Rest);
        false ->
            migrate_keys(Config, Rest)
    end.
