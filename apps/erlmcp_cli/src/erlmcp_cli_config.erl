%%%-------------------------------------------------------------------
%%% @doc
%%% erlmcp_cli_config - CLI Configuration Manager
%%%
%%% Manages CLI configuration including environment variables,
%%% config files, and runtime configuration validation.
%%% Provides centralized access to CLI settings with proper OTP
%%% supervision and configuration merging.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_cli_config).

-behaviour(gen_server).

%% API
-export([start_link/0, get_config/0, get_config/1, get_config/2, set_config/2, set_config/3,
         reload_config/0, merge_env_vars/0, validate_config/1]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("erlmcp.hrl").
-include("erlmcp_observability.hrl").

%% Records
-record(config_state,
        {config :: map(),           % Current configuration
         defaults :: map(),         % Default configuration values
         env_mappings :: map(),     % Environment variable mappings
         config_file :: binary(),   % Path to config file
         watchers :: list(),        % Configuration watchers
         metrics :: map()}).        % Configuration metrics

%% Configuration schema with defaults
-define(DEFAULT_CONFIG,
        #{<<"transport">> => <<"stdio">>,
          <<"host">> => <<"localhost">>,
          <<"port">> => 8080,
          <<"timeout">> => 30000,
          <<"max_retries">> => 3,
          <<"retry_delay">> => 1000,
          <<"log_level">> => <<"info">>,
          <<"enable_metrics">> => true,
          <<"enable_tracing">> => true,
          <<"session_timeout">> => 300000,
          <<"auth">> => #{<<"type">> => <<"none">>, <<"token">> => undefined},
          <<"resources">> => #{<<"poll_interval">> => 5000},
          <<"tools">> => #{<<"max_concurrent">> => 10},
          <<"compression">> => #{<<"enabled">> => true, <<"level">> => 6}}).
%% Environment variable mappings
-define(ENV_MAPPINGS,
        #{ { << "transport" >> , binary } => << "ERLMCP_CLI_TRANSPORT" >> , { << "host" >> , binary } => << "ERLMCP_CLI_HOST" >> , { << "port" >> , integer } => << "ERLMCP_CLI_PORT" >> , { << "timeout" >> , integer } => << "ERLMCP_CLI_TIMEOUT" >> , { << "max_retries" >> , integer } => << "ERLMCP_CLI_MAX_RETRIES" >> , { << "retry_delay" >> , integer } => << "ERLMCP_CLI_RETRY_DELAY" >> , { << "log_level" >> , binary } => << "ERLMCP_CLI_LOG_LEVEL" >> , { << "enable_metrics" >> , boolean } => << "ERLMCP_CLI_ENABLE_METRICS" >> , { << "enable_tracing" >> , boolean } => << "ERLMCP_CLI_ENABLE_TRACING" >> , { << "session_timeout" >> , integer } => << "ERLMCP_CLI_SESSION_TIMEOUT" >> , { << "auth" >> , map } => << "ERLMCP_CLI_AUTH_CONFIG" >> , { << "resources" >> , map } => << "ERLMCP_CLI_RESOURCES_CONFIG" >> , { << "tools" >> , map } => << "ERLMCP_CLI_TOOLS_CONFIG" >> , { << "compression" >> , map } => << "ERLMCP_CLI_COMPRESSION_CONFIG" >> , { << "config_file" >> , binary } => << "ERLMCP_CLI_CONFIG_FILE" >> } }).
-define(SERVER, ?MODULE).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Start the configuration manager
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Get full configuration
-spec get_config() -> map().
get_config() ->
    gen_server:call(?SERVER, get_config, 5000).

%% @doc Get configuration value by key
-spec get_config(binary()) -> term().
get_config(Key) ->
    gen_server:call(?SERVER, {get_config, Key}, 5000).

%% @doc Get configuration value by key with default
-spec get_config(binary(), term()) -> term().
get_config(Key, Default) ->
    case get_config(Key) of
        undefined ->
            Default;
        Value ->
            Value
    end.

%% @doc Set configuration value
-spec set_config(binary(), term()) -> ok.
set_config(Key, Value) ->
    gen_server:cast(?SERVER, {set_config, Key, Value}).

%% @doc Set configuration value with persistence
-spec set_config(binary(), term(), boolean()) -> ok.
set_config(Key, Value, Persist) ->
    gen_server:cast(?SERVER, {set_config, Key, Value, Persist}).

%% @doc Reload configuration from file and environment
-spec reload_config() -> ok.
reload_config() ->
    gen_server:call(?SERVER, reload_config, 10000).

%% @doc Merge environment variables into configuration
-spec merge_env_vars() -> ok.
merge_env_vars() ->
    gen_server:cast(?SERVER, merge_env_vars).

%% @doc Validate configuration against schema
-spec validate_config(map()) -> ok | {error, term()}.
validate_config(Config) ->
    gen_server:call(?SERVER, {validate_config, Config}, 5000).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @doc Initialize the configuration manager
-spec init(term()) -> {ok, #config_state{}} | {stop, term()}.
init(_Args) ->
    %% Create OTEL span for configuration initialization
    erlmcp_otel:with_span("cli.config.init",
                          #{<<"module">> => atom_to_binary(?MODULE, utf8)},
                          fun() ->
                             %% Initialize configuration state
                             DefaultConfig = ?DEFAULT_CONFIG,

                             %% Load configuration file if exists
                             ConfigFile = get_config_file_path(),
                             LoadedConfig = load_config_file(ConfigFile),

                             %% Merge environment variables
                             EnvConfig = merge_environment_variables(DefaultConfig),

                             %% Merge all configurations
                             MergedConfig = merge_configs([DefaultConfig, LoadedConfig, EnvConfig]),

                             %% Validate configuration
                             case validate_config_schema(MergedConfig) of
                                 ok ->
                                     %% Notify watchers of configuration changes
                                     notify_watchers(MergedConfig),

                                     %% Initialize metrics
                                     Metrics = init_metrics(),

                                     State =
                                         #config_state{config = MergedConfig,
                                                       defaults = DefaultConfig,
                                                       env_mappings = ?ENV_MAPPINGS,
                                                       config_file = ConfigFile,
                                                       watchers = [],
                                                       metrics = Metrics},

                                     erlmcp_metrics:record("cli.config.initialized", 1),
                                     {ok, State};
                                 {error, Reason} ->
                                     erlmcp_metrics:record("cli.config.validation_failed", 1),
                                     {stop, Reason}
                             end
                          end).

%% @doc Handle synchronous calls
-spec handle_call(term(), {pid(), term()}, #config_state{}) ->
                     {reply, term(), #config_state{}} | {stop, term(), #config_state{}}.
handle_call(get_config, _From, State) ->
    {reply, State#config_state.config, State};
handle_call({get_config, Key}, _From, State) ->
    Value = get_nested_value(Key, State#config_state.config),
    {reply, Value, State};
handle_call(reload_config, _From, State) ->
    %% Create OTEL span for configuration reload
    SpanCtx =
        erlmcp_otel:inject_span("cli.config.reload",
                                #{<<"config_file">> => State#config_state.config_file},
                                undefined),

    try
        %% Reload from file
        FileConfig = load_config_file(State#config_state.config_file),

        %% Merge with current config
        NewConfig = merge_configs([State#config_state.config, FileConfig]),

        %% Validate new configuration
        case validate_config_schema(NewConfig) of
            ok ->
                %% Notify watchers
                notify_watchers(NewConfig),

                %% Update metrics
                Metrics = update_metrics(State#config_state.metrics, reload),

                erlmcp_otel:record_event(SpanCtx, <<"config.reloaded">>, #{}),
                erlmcp_metrics:record("cli.config.reloaded", 1),

                {reply, ok, State#config_state{config = NewConfig, metrics = Metrics}};
            {error, Reason} ->
                erlmcp_otel:record_error(SpanCtx, {config_validation_failed, Reason}),
                erlmcp_metrics:record("cli.config.reload_failed", 1),
                {reply, {error, Reason}, State}
        end
    catch
        Error:Reason ->
            erlmcp_otel:record_error(SpanCtx, {config_reload_error, Error, Reason}),
            erlmcp_metrics:record("cli.config.reload_failed", 1),
            {reply, {error, Reason}, State}
    end;
handle_call({validate_config, Config}, _From, State) ->
    case validate_config_schema(Config) of
        ok ->
            {reply, ok, State};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

%% @doc Handle asynchronous casts
-spec handle_cast(term(), #config_state{}) -> {noreply, #config_state{}}.
handle_cast({set_config, Key, Value}, State) ->
    %% Create OTEL span for configuration change
    SpanCtx =
        erlmcp_otel:inject_span("cli.config.set",
                                #{<<"key">> => Key, <<"value">> => format_value(Value)},
                                undefined),

    %% Update configuration
    NewConfig = set_nested_value(Key, Value, State#config_state.config),

    %% Validate new configuration
    case validate_config_schema(NewConfig) of
        ok ->
            %% Update metrics
            Metrics = update_metrics(State#config_state.metrics, set),

            %% Notify watchers
            notify_watchers(NewConfig),

            erlmcp_otel:record_event(SpanCtx, <<"config.set">>, #{<<"key">> => Key}),
            {noreply, State#config_state{config = NewConfig, metrics = Metrics}};
        {error, Reason} ->
            erlmcp_otel:record_error(SpanCtx, {config_validation_failed, Reason}),
            erlmcp_metrics:record("cli.config.set_failed", 1),
            {noreply, State}
    end;
handle_cast({set_config, Key, Value, true}, State) ->
    %% Persist configuration to file
    case persist_config(Key, Value, State#config_state.config_file) of
        ok ->
            handle_cast({set_config, Key, Value}, State);
        {error, Reason} ->
            %% Log error but continue in-memory
            lager:warning("Failed to persist config ~p: ~p", [Key, Reason]),
            handle_cast({set_config, Key, Value}, State)
    end;
handle_cast(merge_env_vars, State) ->
    %% Create OTEL span for environment variable merge
    SpanCtx = erlmcp_otel:inject_span("cli.config.merge_env", #{}, undefined),

    %% Get current environment config
    EnvConfig = merge_environment_variables(State#config_state.config),

    %% Merge with existing config
    NewConfig = merge_configs([State#config_state.config, EnvConfig]),

    %% Validate and update
    case validate_config_schema(NewConfig) of
        ok ->
            notify_watchers(NewConfig),
            Metrics = update_metrics(State#config_state.metrics, env_merge),
            erlmcp_otel:record_event(SpanCtx, <<"config.env_merged">>, #{}),
            {noreply, State#config_state{config = NewConfig, metrics = Metrics}};
        {error, Reason} ->
            erlmcp_otel:record_error(SpanCtx, {env_merge_failed, Reason}),
            {noreply, State}
    end;
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @doc Handle messages
-spec handle_info(term(), #config_state{}) -> {noreply, #config_state{}}.
handle_info({watch, Key, Pid}, State) ->
    %% Add watcher for configuration changes
    Watchers = [{Key, Pid} | lists:keydelete(Key, 1, State#config_state.watchers)],
    {noreply, State#config_state{watchers = Watchers}};
handle_info(_Info, State) ->
    {noreply, State}.

%% @doc Terminate the server
-spec terminate(term(), #config_state{}) -> ok.
terminate(_Reason, State) ->
    %% Create OTEL span for configuration termination
    erlmcp_otel:with_span("cli.config.terminate",
                          #{<<"config_count">> => map_size(State#config_state.config)},
                          fun() ->
                             %% Notify watchers of shutdown
                             lists:foreach(fun({Key, Pid}) -> Pid ! {config_stopped, Key} end,
                                           State#config_state.watchers),

                             %% Record final metrics
                             erlmcp_metrics:record("cli.config.terminated", 1),

                             ok
                          end).

%% @doc Handle code changes
-spec code_change(term(), #config_state{}, term()) -> {ok, #config_state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @doc Get configuration file path
-spec get_config_file_path() -> binary().
get_config_file_path() ->
    case os:getenv("ERLMCP_CLI_CONFIG_FILE") of
        false ->
            filename:join(
                os:getenv("HOME", "."), ".erlmcp_cli.json");
        Path ->
            list_to_binary(Path)
    end.

%% @doc Load configuration from file
-spec load_config_file(binary()) -> map().
load_config_file(File) ->
    try
        case file:read_file(File) of
            {ok, Content} ->
                jsx:decode(Content, [{labels, binary}, return_maps]);
            {error, enoent} ->
                #{};
            {error, Reason} ->
                lager:warning("Failed to read config file ~p: ~p", [File, Reason]),
                #{}
        end
    catch
        Error:Reason ->
            lager:warning("Failed to parse config file ~p: ~p:~p", [File, Error, Reason]),
            #{}
    end.

%% @doc Merge environment variables into configuration
-spec merge_environment_variables(map()) -> map().
merge_environment_variables(Config) ->
    lists:foldl(fun({{Key, Type}, EnvVar}, Acc) ->
                   case os:getenv(EnvVar) of
                       false ->
                           Acc;
                       Value ->
                           ConvertedValue = convert_env_value(Value, Type),
                           set_nested_value(Key, ConvertedValue, Acc)
                   end
                end,
                Config,
                ?ENV_MAPPINGS).

%% @doc Convert environment variable value to proper type
-spec convert_env_value(binary(), atom()) -> term().
convert_env_value(Value, binary) ->
    Value;
convert_env_value(Value, integer) ->
    case list_to_integer(binary_to_list(Value)) of
        Int when is_integer(Int) ->
            Int
    end;
convert_env_value(Value, boolean) ->
    case binary_to_list(Value) of
        "true" ->
            true;
        "1" ->
            true;
        "false" ->
            false;
        "0" ->
            false;
        _ ->
            false
    end;
convert_env_value(Value, _) ->
    Value.

%% @doc Merge multiple configurations
-spec merge_configs([map()]) -> map().
merge_configs( Configs ) -> lists : foldl( fun merge_config , ?DEFAULT_CONFIG , Configs ) .

%% @doc Merge two configurations
-spec merge_config(map(), map()) -> map().
merge_config(Config, Acc) ->
    maps:merge(Acc, Config).

%% @doc Get nested value from configuration
-spec get_nested_value(binary(), map()) -> term().
get_nested_value(Key, Config) ->
    case binary:split(Key, <<".">>, [global]) of
        [Key] ->
            maps:get(Key, Config, undefined);
        Parts ->
            get_nested_value_parts(Parts, Config)
    end.

%% @doc Get nested value using parts
-spec get_nested_value_parts([binary()], map()) -> term().
get_nested_value_parts([Part], Map) ->
    maps:get(Part, Map, undefined);
get_nested_value_parts([Part | Rest], Map) ->
    case maps:get(Part, Map, undefined) of
        undefined ->
            undefined;
        SubMap when is_map(SubMap) ->
            get_nested_value_parts(Rest, SubMap);
        _ ->
            undefined
    end.

%% @doc Set nested value in configuration
-spec set_nested_value(binary(), term(), map()) -> map().
set_nested_value(Key, Value, Config) ->
    case binary:split(Key, <<".">>, [global]) of
        [Key] ->
            maps:put(Key, Value, Config);
        Parts ->
            set_nested_value_parts(Parts, Value, Config)
    end.

%% @doc Set nested value using parts
-spec set_nested_value_parts([binary()], term(), map()) -> map().
set_nested_value_parts([Part], Value, Map) ->
    maps:put(Part, Value, Map);
set_nested_value_parts([Part | Rest], Value, Map) ->
    SubMap =
        case maps:get(Part, Map, undefined) of
            undefined ->
                #{};
            Existing when is_map(Existing) ->
                Existing
        end,
    NewSubMap = set_nested_value_parts(Rest, Value, SubMap),
    maps:put(Part, NewSubMap, Map).

%% @doc Validate configuration against schema
-spec validate_config_schema(map()) -> ok | {error, term()}.
validate_config_schema(Config) ->
    %% Validate required fields
    RequiredFields = [<<"transport">>, <<"host">>, <<"port">>],
    lists:foreach(fun(Field) ->
                     case maps:is_key(Field, Config) of
                         true ->
                             case validate_field(Field, maps:get(Field, Config)) of
                                 ok ->
                                     ok;
                                 {error, Reason} ->
                                     {error, {field_validation, Field, Reason}}
                             end;
                         false ->
                             {error, {missing_required_field, Field}}
                     end
                  end,
                  RequiredFields),

    %% Validate transport types
    case maps:get(<<"transport">>, Config, undefined) of
        undefined ->
            ok;
        Transport
            when Transport == <<"stdio">>;
                 Transport == <<"tcp">>;
                 Transport == <<"http">>;
                 Transport == <<"ws">>;
                 Transport == <<"sse">> ->
            ok;
        _ ->
            {error, {invalid_transport, Transport}}
    end,

    %% Validate port range
    case maps:get(<<"port">>, Config, undefined) of
        undefined ->
            ok;
        Port when is_integer(Port), Port > 0, Port =< 65535 ->
            ok;
        _ ->
            {error, {invalid_port, Port}}
    end,

    %% Validate timeout
    case maps:get(<<"timeout">>, Config, undefined) of
        undefined ->
            ok;
        Timeout when is_integer(Timerout), Timerout > 0 ->
            ok;
        _ ->
            {error, {invalid_timeout, Timeout}}
    end,

    ok.

%% @doc Validate individual field
-spec validate_field(binary(), term()) -> ok | {error, term()}.
validate_field( << "transport" >> , Value ) when is_binary( Value ) -> case lists : member( Value , [ << "stdio" >> , << "tcp" >> , << "http" >> , << "ws" >> , << "sse" >> ] ) of true -> ok ; false -> { error , unsupported_transport } end ; validate_field( << "host" >> , Value ) when is_binary( Value ) -> case byte_size( Value ) > 0 of true -> ok ; false -> { error , empty_host } end ; validate_field( << "port" >> , Value ) when is_integer( Value ) , Value > 0 , Value =< 65535 -> ok ; validate_field( << "port" >> , _ ) -> { error , invalid_port_range } ; validate_field( << "timeout" >> , Value ) when is_integer( Value ) , Value > 0 -> ok ; validate_field( << "timeout" >> , _ ) -> { error , invalid_timeout } ; validate_field( << "max_retries" >> , Value ) when is_integer( Value ) , Value >= 0 -> ok ; validate_field( << "max_retries" >> , _ ) -> { error , invalid_max_retries } ; validate_field( << "retry_delay" >> , Value ) when is_integer( Value ) , Value >= 0 -> ok ; validate_field( << "retry_delay" >> , _ ) -> { error , invalid_retry_delay } ; validateField( << "log_level" >> , Value ) when is_binary( Value ) -> case lists : member( Value , [ << "debug" >> , << "info" >> , << "warn" >> , << "error" >> ] ) of true -> ok ; false -> { error , invalid_log_level } end ; validateField( << "enable_metrics" >> , Value ) when is_boolean( Value ) -> ok ; validateField( << "enable_metrics" >> , _ ) -> { error , invalid_enable_metrics } ; validateField( << "enable_tracing" >> , Value ) when is_boolean( Value ) -> ok ; validateField( << "enable_tracing" >> , _ ) -> { error , invalid_enable_tracing } ; validateField( << "session_timeout" >> , Value ) when is_integer( Value ) , Value > 0 -> ok ; validateField( << "session_timeout" >> , _ ) -> { error , invalid_session_timeout } ; validateField( _ , _ ) -> ok .

%% @doc Notify configuration watchers
-spec notify_watchers(map()) -> ok.
notify_watchers(Config) ->
    lists:foreach(fun({Key, Pid}) -> Pid ! {config_updated, Key, get_nested_value(Key, Config)} end,
                  ?SERVER:watchers()).

%% @doc Persist configuration to file
-spec persist_config(binary(), term(), binary()) -> ok | {error, term()}.
persist_config(Key, Value, File) ->
    try
        %% Read existing config
        ExistingConfig = load_config_file(File),

        %% Update config
        NewConfig = set_nested_value(Key, Value, ExistingConfig),

        %% Write to file
        JsonData = jsx:encode(NewConfig, [{indent, 2}]),
        file:write_file(File, JsonData),

        ok
    catch
        Error:Reason ->
            {error, {file_error, Error, Reason}}
    end.

%% @doc Initialize metrics
-spec init_metrics() -> map().
init_metrics() ->
    #{"config.get" => 0,
      "config.set" => 0,
      "config.reload" => 0,
      "config.watchers" => 0,
      "config.validation_errors" => 0}.

%% @doc Update metrics
-spec update_metrics(map(), atom()) -> map().
update_metrics(Metrics, Type) ->
    case Type of
        get ->
            maps:update_with("config.get", fun(V) -> V + 1 end, Metrics);
        set ->
            maps:update_with("config.set", fun(V) -> V + 1 end, Metrics);
        reload ->
            maps:update_with("config.reload", fun(V) -> V + 1 end, Metrics);
        env_merge ->
            Metrics;
        _ ->
            Metrics
    end.

%% @doc Format value for logging
-spec format_value(term()) -> binary().
format_value(Value) when is_binary(Value) ->
    Value;
format_value(Value) ->
    list_to_binary(io_lib:format("~p", [Value])).
