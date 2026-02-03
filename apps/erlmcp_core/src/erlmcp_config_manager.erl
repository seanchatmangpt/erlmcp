%%%-------------------------------------------------------------------
%%% @doc
%%% Configuration Manager with Hot-Reload Support
%%%
%%% This module provides centralized configuration management with:
%%% - Loading configuration from multiple sources (ConfigMap, env vars, files)
%%% - Hot-reloading configuration without restart
%%% - Environment-specific overrides
%%% - Configuration validation
%%% - Change notification
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_config_manager).

-behaviour(gen_server).

%% API
-export([
    start_link/0,
    start_link/1,
    reload_config/0,
    reload_config/1,
    get_config/0,
    get_config/1,
    get_config/2,
    set_config/2,
    set_config/3,
    subscribe/1,
    unsubscribe/1,
    get_env/1,
    get_env/2,
    get_env/3,
    set_env/2,
    validate_config/1,
    export_config/0,
    import_config/1
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-define(SERVER, ?MODULE).
-define(CONFIG_TABLE, erlmcp_config).
-define(SUBSCRIBERS_TABLE, erlmcp_config_subscribers).

%% State
-record(state, {
    config :: map(),
    version :: integer(),
    last_update :: integer(),
    watchers :: map()
}).

%% Type definitions
-type config_key() :: binary() | atom() | string().
-type config_value() :: term().
-type config_change() :: {added, config_key(), config_value()} |
                          {updated, config_key(), config_value(), config_value()} |
                          {deleted, config_key(), config_value()}.
-type config_path() :: [binary() | atom()].

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Start the configuration manager
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link(#{}).

%% @doc Start the configuration manager with options
-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Options) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Options, []).

%% @doc Reload configuration from all sources
-spec reload_config() -> ok | {error, term()}.
reload_config() ->
    reload_config(default).

%% @doc Reload configuration from specific source
-spec reload_config(default | file:filename() | binary()) -> ok | {error, term()}.
reload_config(Source) ->
    gen_server:call(?SERVER, {reload_config, Source}).

%% @doc Get entire configuration
-spec get_config() -> map().
get_config() ->
    gen_server:call(?SERVER, get_config).

%% @doc Get configuration value at path
-spec get_config(config_path() | config_key()) -> config_value() | undefined.
get_config(Path) when is_list(Path) ->
    gen_server:call(?SERVER, {get_config, Path});
get_config(Key) ->
    gen_server:call(?SERVER, {get_config, [Key]}).

%% @doc Get configuration value with default
-spec get_config(config_path() | config_key(), config_value()) -> config_value().
get_config(Path, Default) ->
    case get_config(Path) of
        undefined -> Default;
        Value -> Value
    end.

%% @doc Set configuration value
-spec set_config(config_path() | config_key(), config_value()) -> ok | {error, term()}.
set_config(Path, Value) when is_list(Path) ->
    gen_server:call(?SERVER, {set_config, Path, Value});
set_config(Key, Value) ->
    gen_server:call(?SERVER, {set_config, [Key], Value}).

%% @doc Set configuration value with options
-spec set_config(config_path() | config_key(), config_value(), map()) -> ok | {error, term()}.
set_config(Path, Value, Options) when is_list(Path) ->
    gen_server:call(?SERVER, {set_config, Path, Value, Options});
set_config(Key, Value, Options) ->
    gen_server:call(?SERVER, {set_config, [Key], Value, Options}).

%% @doc Subscribe to configuration changes
-spec subscribe(pid()) -> ok.
subscribe(Subscriber) when is_pid(Subscriber) ->
    gen_server:call(?SERVER, {subscribe, Subscriber}).

%% @doc Unsubscribe from configuration changes
-spec unsubscribe(pid()) -> ok.
unsubscribe(Subscriber) when is_pid(Subscriber) ->
    gen_server:call(?SERVER, {unsubscribe, Subscriber}).

%% @doc Get application environment variable
-spec get_env(atom()) -> term() | undefined.
get_env(Key) ->
    get_env(erlmcp, Key).

%% @doc Get application environment variable with default
-spec get_env(atom(), atom()) -> term().
get_env(App, Key) ->
    application:get_env(App, Key, undefined).

%% @doc Get application environment variable with default and scope
-spec get_env(atom(), atom(), term()) -> term().
get_env(App, Key, Default) ->
    application:get_env(App, Key, Default).

%% @doc Set application environment variable
-spec set_env({atom(), atom()}, term()) -> ok.
set_env({App, Key}, Value) ->
    application:set_env(App, Key, Value);
set_env(Key, Value) ->
    application:set_env(erlmcp, Key, Value).

%% @doc Validate configuration
-spec validate_config(map()) -> ok | {error, [binary()]}.
validate_config(Config) when is_map(Config) ->
    validate_schema(Config, get_schema()).

%% @doc Export current configuration
-spec export_config() -> {ok, binary()} | {error, term()}.
export_config() ->
    gen_server:call(?SERVER, export_config).

%% @doc Import configuration
-spec import_config(binary() | map()) -> ok | {error, term()}.
import_config(JSON) when is_binary(JSON) ->
    try json:decode(JSON) of
        Config -> import_config(Config)
    catch
        error:_ -> {error, invalid_json}
    end;
import_config(Config) when is_map(Config) ->
    gen_server:call(?SERVER, {import_config, Config}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init(Options) ->
    % Initialize ETS tables
    ets:new(?CONFIG_TABLE, [
        named_table,
        set,
        public,
        {read_concurrency, true},
        {write_concurrency, true}
    ]),
    ets:new(?SUBSCRIBERS_TABLE, [
        named_table,
        bag,
        public
    ]),

    % Load initial configuration
    InitialConfig = load_initial_config(Options),

    % Store in ETS
    ets:insert(?CONFIG_TABLE, {current_config, InitialConfig}),
    ets:insert(?CONFIG_TABLE, {config_version, 1}),
    ets:insert(?CONFIG_TABLE, {last_update, system_time()}),

    logger:info("Configuration manager started with ~p keys", [maps:size(InitialConfig)]),

    {ok, #state{
        config = InitialConfig,
        version = 1,
        last_update = system_time(),
        watchers = #{}
    }}.

handle_call(get_config, _From, State = #state{config = Config}) ->
    {reply, Config, State};

handle_call({get_config, Path}, _From, State = #state{config = Config}) ->
    Value = get_path(Config, Path),
    {reply, Value, State};

handle_call({set_config, Path, Value}, From, State) ->
    handle_call({set_config, Path, Value, #{}}, From, State);

handle_call({set_config, Path, Value, Options}, _From, State = #state{config = Config, version = Version}) ->
    OldValue = get_path(Config, Path),
    NewConfig = set_path(Config, Path, Value),
    NewVersion = Version + 1,
    Now = system_time(),

    % Update ETS
    ets:insert(?CONFIG_TABLE, {current_config, NewConfig}),
    ets:insert(?CONFIG_TABLE, {config_version, NewVersion}),
    ets:insert(?CONFIG_TABLE, {last_update, Now}),

    % Notify subscribers
    Change = case OldValue of
        undefined -> {added, Path, Value};
        _ -> {updated, Path, OldValue, Value}
    end,
    notify_subscribers(Change),

    % Persist if requested
    case maps:get(persist, Options, false) of
        true -> persist_config(NewConfig);
        _ -> ok
    end,

    {reply, ok, State#state{
        config = NewConfig,
        version = NewVersion,
        last_update = Now
    }};

handle_call({reload_config, Source}, _From, State) ->
    case load_config(Source) of
        {ok, NewConfig} ->
            OldConfig = State#state.config,
            Changes = compute_changes(OldConfig, NewConfig),

            % Update ETS
            ets:insert(?CONFIG_TABLE, {current_config, NewConfig}),
            ets:insert(?CONFIG_TABLE, {config_version, State#state.version + 1}),
            ets:insert(?CONFIG_TABLE, {last_update, system_time()}),

            % Notify subscribers of changes
            lists:foreach(fun notify_subscribers/1, Changes),

            logger:info("Configuration reloaded from ~p, ~p changes detected",
                       [Source, length(Changes)]),

            {reply, ok, State#state{
                config = NewConfig,
                version = State#state.version + 1,
                last_update = system_time()
            }};
        {error, Reason} ->
            logger:error("Failed to reload configuration: ~p", [Reason]),
            {reply, {error, Reason}, State}
    end;

handle_call({subscribe, Subscriber}, _From, State) ->
    ets:insert(?SUBSCRIBERS_TABLE, {Subscriber, self()}),
    monitor(process, Subscriber),
    {reply, ok, State};

handle_call({unsubscribe, Subscriber}, _From, State) ->
    try
        ets:delete_object(?SUBSCRIBERS_TABLE, {Subscriber, self()}),
        {reply, ok, State}
    catch
        error:badarg ->
            {reply, ok, State}
    end;

handle_call(export_config, _From, State = #state{config = Config}) ->
    Export = #{
        version => State#state.version,
        last_updated => State#state.last_update,
        config => Config
    },
    try
        JSON = json:encode(Export),
        {reply, {ok, JSON}, State}
    catch
        error:_ ->
            {reply, {error, encode_failed}, State}
    end;

handle_call({import_config, Config}, _From, State) ->
    case validate_config(Config) of
        ok ->
            OldConfig = State#state.config,
            Changes = compute_changes(OldConfig, Config),

            ets:insert(?CONFIG_TABLE, {current_config, Config}),
            ets:insert(?CONFIG_TABLE, {config_version, State#state.version + 1}),
            ets:insert(?CONFIG_TABLE, {last_update, system_time()}),

            lists:foreach(fun notify_subscribers/1, Changes),

            {reply, ok, State#state{
                config = Config,
                version = State#state.version + 1,
                last_update = system_time()
            }};
        {error, Errors} ->
            {reply, {error, {validation_failed, Errors}}, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'DOWN', _Ref, process, Pid, _Reason}, State) ->
    % Subscriber died, remove them
    ets:match_delete(?SUBSCRIBERS_TABLE, {Pid, '_'}),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private Load initial configuration
load_initial_config(Options) ->
    % Priority: 1. Options passed in, 2. ConfigMap, 3. Environment variables, 4. defaults
    BaseConfig = get_default_config(),

    ConfigMapConfig = case maps:get(configmap_path, Options, "/opt/erlmcp/etc/erlmcp.conf") of
        Path when is_list(Path) -> load_from_file(Path);
        _ -> #{}
    end,

    EnvConfig = load_from_env(),

    % Merge with priority
    Merged = maps:merge(BaseConfig, maps:merge(ConfigMapConfig, EnvConfig)),

    % Apply options overrides
    maps:merge(Merged, Options).

%% @private Load configuration from file
load_from_file(Path) ->
    case file:read_file(Path) of
        {ok, Content} ->
            parse_config(Content);
        {error, Reason} ->
            logger:warning("Failed to read config file ~s: ~p", [Path, Reason]),
            #{}
    end.

%% @private Parse configuration content
parse_config(Content) ->
    try
        % Try to parse as Erlang term
        {ok, Tokens, _} = erl_scan:string(binary_to_list(Content)),
        {ok, Term} = erl_parse:parse_term(Tokens),
        convert_to_map(Term)
    catch
        _:_ ->
            try
                % Try to parse as JSON
                json:decode(Content)
            catch
                _:_ ->
                    logger:error("Failed to parse configuration"),
                    #{}
            end
    end.

%% @private Load configuration from environment variables
load_from_env() ->
    EnvVars = application:get_all_env(erlmcp),
    maps:from_list([{K, V} || {K, V} <- EnvVars, V =/= undefined]).

%% @private Load from specific source
load_config(default) ->
    ConfigMapPath = os:getenv("ERLMCP_CONFIG_PATH", "/opt/erlmcp/etc/erlmcp.conf"),
    load_config(ConfigMapPath);
load_config(Path) when is_list(Path) ->
    Config = load_from_file(Path),
    {ok, maps:merge(load_from_env(), Config)};
load_config(JSON) when is_binary(JSON) ->
    try
        Config = json:decode(JSON),
        {ok, maps:merge(load_from_env(), Config)}
    catch
        _:_ -> {error, invalid_json}
    end.

%% @private Get default configuration
get_default_config() ->
    #{
        log_level => info,
        log_format => json,
        server => #{
            max_subscriptions_per_resource => 5000,
            max_progress_tokens => 50000,
            request_timeout => 30000
        },
        transport => #{
            tcp => #{enabled => true, port => 8080},
            http => #{enabled => true, port => 8081},
            websocket => #{enabled => true, port => 8082}
        },
        auth => #{
            enabled => true,
            rbac => #{enabled => true},
            rate_limit => #{enabled => true}
        },
        monitoring => #{
            enabled => true,
            prometheus_port => 9090
        }
    }.

%% @private Get value at path
get_path(Map, [Key]) when is_atom(Key) ->
    maps:get(Key, Map, maps:get(atom_to_binary(Key), Map, undefined));
get_path(Map, [Key]) when is_binary(Key) ->
    maps:get(Key, Map, maps:get(binary_to_atom(Key, utf8), Map, undefined));
get_path(Map, [Key | Rest]) ->
    case get_path(Map, [Key]) of
        undefined -> undefined;
        SubMap when is_map(SubMap) -> get_path(SubMap, Rest);
        Value -> Value
    end.

%% @private Set value at path
set_path(Map, [Key], Value) ->
    maps:put(Key, Value, Map);
set_path(Map, [Key | Rest], Value) ->
    SubMap = maps:get(Key, Map, #{}),
    maps:put(Key, set_path(SubMap, Rest, Value), Map).

%% @private Compute configuration changes
compute_changes(OldConfig, NewConfig) ->
    AllKeys = lists:usort(
        maps:keys(OldConfig) ++ maps:keys(NewConfig)
    ),
    lists:filtermap(
        fun(Key) ->
            OldValue = maps:get(Key, OldConfig, undefined),
            NewValue = maps:get(Key, NewConfig, undefined),
            case {OldValue, NewValue} of
                {undefined, undefined} -> false;
                {undefined, _} -> {true, {added, [Key], NewValue}};
                {_, undefined} -> {true, {deleted, [Key], OldValue}};
                {V, V} -> false;
                {_, _} -> {true, {updated, [Key], OldValue, NewValue}}
            end
        end,
        AllKeys
    ).

%% @private Notify subscribers of changes
notify_subscribers(Change) ->
    Subscribers = ets:tab2list(?SUBSCRIBERS_TABLE),
    lists:foreach(
        fun({Pid, _}) ->
            try
                Pid ! {config_change, Change}
            catch
                _:_ -> ok
            end
        end,
        Subscribers
    ).

%% @private Persist configuration
persist_config(Config) ->
    Path = os:getenv("ERLMCP_CONFIG_PATH", "/opt/erlmcp/etc/erlmcp.conf"),
    Term = convert_from_map(Config),
    Content = io_lib:format("~p.~n", [Term]),
    file:write_file(Path, Content).

%% @private Convert Erlang config tuple list to map
convert_to_map({List}) when is_list(List) ->
    convert_list_to_map(List);
convert_to_map(List) when is_list(List) ->
    convert_list_to_map(List);
convert_to_map(Map) when is_map(Map) ->
    Map;
convert_to_map(Term) ->
    Term.

%% @private Convert proplist to map
convert_list_to_map(List) ->
    maps:from_list(
        lists:map(
            fun
                ({Key, {SubList}}) when is_list(SubList) ->
                    {Key, convert_list_to_map(SubList)};
                ({Key, Value}) ->
                    {Key, Value};
                (Other) ->
                    Other
            end,
            List
        )
    ).

%% @private Convert map to Erlang config format
convert_from_map(Map) when is_map(Map) ->
    maps:to_list(
        maps:map(
            fun(_K, V) when is_map(V) -> convert_from_map(V);
               (_K, V) -> V
            end,
            Map
        )
    );
convert_from_map(Term) ->
    Term.

%% @private Get configuration schema
get_schema() ->
    #{
        <<"log_level">> => #{type => atom, enum => [debug, info, warning, error]},
        <<"log_format">> => #{type => atom, enum => [text, json]},
        <<"server">> => #{
            type => map,
            properties => #{
                <<"max_subscriptions_per_resource">> => #{type => integer, min => 1},
                <<"max_progress_tokens">> => #{type => integer, min => 1},
                <<"request_timeout">> => #{type => integer, min => 0}
            }
        }
    }.

%% @private Validate against schema
validate_schema(Config, Schema) ->
    Errors = lists:flatten([
        validate_key(Key, Value, maps:get(Key, Schema, #{}))
        || {Key, Value} <- maps:to_list(Config)
    ]),
    case Errors of
        [] -> ok;
        _ -> {error, Errors}
    end.

%% @private Validate a single key
validate_key(_Key, _Value, undefined) -> [];
validate_key(Key, Value, #{type := Type} = Rule) ->
    TypeErrors = validate_type(Key, Value, Type),
    EnumErrors = case maps:get(enum, Rule, undefined) of
        undefined -> [];
        Enum -> validate_enum(Key, Value, Enum)
    end,
    MinErrors = case maps:get(min, Rule, undefined) of
        undefined -> [];
        Min -> validate_min(Key, Value, Min)
    end,
    TypeErrors ++ EnumErrors ++ MinErrors.

%% @private Validate type
validate_type(Key, Value, atom) when not is_atom(Value) ->
    [<<"Key ", (to_binary(Key))/binary, " must be an atom">>];
validate_type(Key, Value, integer) when not is_integer(Value) ->
    [<<"Key ", (to_binary(Key))/binary, " must be an integer">>];
validate_type(Key, Value, boolean) when not is_boolean(Value) ->
    [<<"Key ", (to_binary(Key))/binary, " must be a boolean">>];
validate_type(Key, Value, map) when not is_map(Value) ->
    [<<"Key ", (to_binary(Key))/binary, " must be a map">>];
validate_type(_Key, _Value, _Type) -> [].

%% @private Validate enum
validate_enum(Key, Value, Enum) when is_list(Enum) ->
    case lists:member(Value, Enum) of
        true -> [];
        false -> [<<"Key ", (to_binary(Key))/binary, " must be one of: ", (list_to_binary(io_lib:format("~p", [Enum])))/binary>>]
    end.

%% @private Validate minimum
validate_min(Key, Value, Min) when is_integer(Value), Value < Min ->
    [<<"Key ", (to_binary(Key))/binary, " must be at least ", (integer_to_binary(Min))/binary>>];
validate_min(_Key, _Value, _Min) -> [].

%% @private Convert to binary
to_binary(B) when is_binary(B) -> B;
to_binary(A) when is_atom(A) -> atom_to_binary(A, utf8);
to_binary(L) when is_list(L) -> list_to_binary(L);
to_binary(I) when is_integer(I) -> integer_to_binary(I).

%% @private Get system time
system_time() ->
    erlang:system_time(millisecond).
