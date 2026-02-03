%% @doc Enterprise Configuration Manager
%% Centralized configuration management for enterprise integrations
-module(erlmcp_enterprise_config).
-behaviour(gen_server).

-export([start_link/0, stop/0]).
-export([get_config/1, set_config/2, get_all_configs/0, reload_config/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("kernel/include/logger.hrl").

-record(state, {
    configs :: map(),  % Key -> Config
    watchers :: map(),  % Key -> [Pids]
    config_file :: string()  % Path to config file
}).

%%====================================================================
%% API functions
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec stop() -> ok.
stop() ->
    gen_server:stop(?MODULE).

-spec get_config(Key :: atom()) -> {ok, map()} | {error, not_found}.
get_config(Key) ->
    gen_server:call(?MODULE, {get_config, Key}).

-spec set_config(Key :: atom(), Value :: map()) -> ok.
set_config(Key, Value) ->
    gen_server:call(?MODULE, {set_config, Key, Value}).

-spec get_all_configs() -> map().
get_all_configs() ->
    gen_server:call(?MODULE, {get_all_configs}).

-spec reload_config(Key :: atom()) -> ok | {error, term()}.
reload_config(Key) ->
    gen_server:call(?MODULE, {reload_config, Key}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

-spec init([]) -> {ok, #state{}}.
init([]) ->
    process_flag(trap_exit, true),

    %% Set config file path
    ConfigFile = filename:join([code:priv_dir(erlmcp_enterprise), "config", "enterprise_config.json"]),

    %% Load initial configuration
    case load_config_from_file(ConfigFile) of
        {ok, Configs} ->
            %% Setup watchers for each config
            Watchers = setup_config_watchers(Configs),

            State = #state{
                configs = Configs,
                watchers = Watchers,
                config_file = ConfigFile
            },

            {ok, State};
        {error, Reason} ->
            %% Use default configuration
            DefaultConfigs = get_default_config(),
            Watchers = setup_config_watchers(DefaultConfigs),

            State = #state{
                configs = DefaultConfigs,
                watchers = Watchers,
                config_file = ConfigFile
            },

            %% Save default configuration
            save_config_to_file(ConfigFile, DefaultConfigs),

            {ok, State}
    end.

-spec handle_call(term(), {pid(), term()}, #state{}) -> {reply, term(), #state{}}.
handle_call({get_config, Key}, _From, State) ->
    case maps:find(Key, State#state.configs) of
        {ok, Config} ->
            {reply, {ok, Config}, State};
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call({set_config, Key, Value}, _From, State) ->
    %% Validate configuration
    case validate_config(Key, Value) of
        valid ->
            %% Update configuration
            UpdatedConfigs = State#state.configs#{Key => Value},

            %% Save to file
            case save_config_to_file(State#state.config_file, UpdatedConfigs) of
                ok ->
                    %% Notify watchers
                    notify_config_change(Key, Value, State#state.watchers),

                    %% Notify enterprise bus
                    erlmcp_enterprise_bus:publish(config_updated, {Key, Value}),

                    {reply, ok, State#state{configs = UpdatedConfigs}};
                {error, Reason} ->
                    {reply, {error, Reason}, State}
            end;
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({get_all_configs}, _From, State) ->
    {reply, {ok, State#state.configs}, State};

handle_call({reload_config, Key}, _From, State) ->
    case reload_config_from_file(State#state.config_file, Key) of
        {ok, NewConfig} ->
            %% Update configuration
            UpdatedConfigs = State#state.configs#{Key => NewConfig},

            %% Notify watchers
            notify_config_change(Key, NewConfig, State#state.watchers),

            %% Notify enterprise bus
            erlmcp_enterprise_bus:publish(config_reloaded, {Key, NewConfig}),

            {reply, ok, State#state{configs = UpdatedConfigs}};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call(_Msg, _From, State) ->
    {reply, {error, unknown_call}, State}.

-spec handle_cast(term(), #state{}) -> {noreply, #state{}}.
handle_cast({watch_config, Key, Pid}, State) ->
    %% Add watcher for config key
    Watchers = case maps:find(Key, State#state.watchers) of
        {ok, Pids} when is_list(Pids) ->
            State#state.watchers#{Key => [Pid | Pids]};
        error ->
            State#state.watchers#{Key => [Pid]}
    end,
    {noreply, State#state{watchers = Watchers}};

handle_cast({unwatch_config, Key, Pid}, State) ->
    %% Remove watcher for config key
    Watchers = case maps:find(Key, State#state.watchers) of
        {ok, Pids} ->
            UpdatedPids = lists:delete(Pid, Pids),
            case UpdatedPids of
                [] -> maps:remove(Key, State#state.watchers);
                _ -> State#state.watchers#{Key => UpdatedPids}
            end;
        error ->
            State#state.watchers
    end,
    {noreply, State#state{watchers = Watchers}};

handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), #state{}) -> {noreply, #state{}}.
handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), #state{}) -> ok.
terminate(_Reason, _State) ->
    ok.

-spec code_change(term(), #state{}, term()) -> {ok, #state{}} | {error, term()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

-spec load_config_from_file(string()) -> {ok, map()} | {error, term()}.
load_config_from_file(File) ->
    case file:read_file(File) of
        {ok, Content} ->
            case jsx:decode(Content, [{labels, atom}]) of
                Map when is_map(Map) ->
                    {ok, Map};
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

-spec save_config_to_file(string(), map()) -> ok | {error, term()}.
save_config_to_file(File, Config) ->
    case jsx:encode(Config) of
        Bin when is_binary(Bin) ->
            file:write_file(File, Bin);
        {error, Reason} ->
            {error, Reason}
    end.

-spec reload_config_from_file(string(), atom()) -> {ok, map()} | {error, term()}.
reload_config_from_file(File, Key) ->
    case load_config_from_file(File) of
        {ok, Configs} ->
            case maps:find(Key, Configs) of
                {ok, Config} ->
                    {ok, Config};
                error ->
                    {error, not_found}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

-spec validate_config(atom(), map()) -> valid | {error, term()}.
validate_config(identity_providers, Config) ->
    %% Validate identity provider configuration
    case Config of
        #{okta := OktaConfig} when is_map(OktaConfig) ->
            case required_fields_okta(OktaConfig) of
                true -> valid;
                false -> {error, missing_fields}
            end;
        #{azure_ad := AzureConfig} when is_map(AzureConfig) ->
            case required_fields_azure(AzureConfig) of
                true -> valid;
                false -> {error, missing_fields}
            end;
        #{adfs := ADFSConfig} when is_map(ADFSConfig) ->
            case required_fields_adfs(ADFSConfig) of
                true -> valid;
                false -> {error, missing_fields}
            end;
        _ ->
            {error, invalid_config}
    end;

validate_config(monitoring_systems, Config) ->
    %% Validate monitoring system configuration
    case Config of
        #{splunk := SplunkConfig} when is_map(SplunkConfig) ->
            case required_fields_splunk(SplunkConfig) of
                true -> valid;
                false -> {error, missing_fields}
            end;
        #{datadog := DatadogConfig} when is_map(DatadogConfig) ->
            case required_fields_datadog(DatadogConfig) of
                true -> valid;
                false -> {error, missing_fields}
            end;
        #{new_relic := NewRelicConfig} when is_map(NewRelicConfig) ->
            case required_fields_newrelic(NewRelicConfig) of
                true -> valid;
                false -> {error, missing_fields}
            end;
        _ ->
            {error, invalid_config}
    end;

validate_config(_Key, _Config) ->
    %% For now, accept any configuration
    valid.

-spec required_fields_okta(map()) -> boolean().
required_fields_okta(Config) ->
    maps:is_key(endpoint, Config) andalso
    maps:is_key(client_id, Config) andalso
    maps:is_key(client_secret, Config).

-spec required_fields_azure(map()) -> boolean().
required_fields_azure(Config) ->
    maps:is_key(tenant_id, Config) andalso
    maps:is_key(client_id, Config) andalso
    maps:is_key(client_secret, Config).

-spec required_fields_adfs(map()) -> boolean().
required_fields_adfs(Config) ->
    maps:is_key(endpoint, Config) andalso
    maps:is_key(client_id, Config) andalso
    maps:is_key(client_secret, Config).

-spec required_fields_splunk(map()) -> boolean().
required_fields_splunk(Config) ->
    maps:is_key(host, Config) andalso
    maps:is_key(port, Config) andalso
    maps:is_key(token, Config).

-spec required_fields_datadog(map()) -> boolean().
required_fields_datadog(Config) ->
    maps:is_key(api_key, Config) andalso
    maps:is_key(app_key, Config).

-spec required_fields_newrelic(map()) -> boolean().
required_fields_newrelic(Config) ->
    maps:is_key(api_key, Config) andalso
    maps:is_key(account_id, Config).

-spec setup_config_watchers(map()) -> map().
setup_config_watchers(Configs) ->
    %% Initialize watchers map
    maps:fold(fun(Key, _Value, Acc) ->
        Acc#{Key => []}
    end, #{}, Configs).

-spec notify_config_change(atom(), map(), map()) -> ok.
notify_config_change(Key, Value, Watchers) ->
    case maps:find(Key, Watchers) of
        {ok, Pids} ->
            lists:foreach(fun(Pid) ->
                Pid ! {config_updated, Key, Value}
            end, Pids);
        error ->
            ok
    end.

-spec get_default_config() -> map().
get_default_config() ->
    %% Return default configuration structure
    #{
        identity_providers => #{
            okta => #{
                enabled => false,
                endpoint => "https://your-org.okta.com",
                client_id => undefined,
                client_secret => undefined
            },
            azure_ad => #{
                enabled => false,
                tenant_id => undefined,
                client_id => undefined,
                client_secret => undefined
            },
            adfs => #{
                enabled => false,
                endpoint => undefined,
                client_id => undefined,
                client_secret => undefined
            }
        },
        monitoring_systems => #{
            splunk => #{
                enabled => false,
                host => undefined,
                port => 8088,
                token => undefined
            },
            datadog => #{
                enabled => false,
                api_key => undefined,
                app_key => undefined
            },
            new_relic => #{
                enabled => false,
                api_key => undefined,
                account_id => undefined
            }
        }
    }.