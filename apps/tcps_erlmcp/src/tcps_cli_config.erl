%%%-----------------------------------------------------------------------------
%%% @doc TCPS CLI Configuration Management
%%%
%%% Handles loading and managing configuration for the TCPS CLI tools.
%%%
%%% Configuration sources (in order of precedence):
%%% 1. Command-line flags (--config FILE)
%%% 2. TCPS_CONFIG environment variable
%%% 3. ~/.tcps/config
%%% 4. ./tcps.config
%%% 5. Built-in defaults
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(tcps_cli_config).

-export([
    load/0,
    load/1,
    get/1,
    get/2,
    set/2,
    save/1,
    default_config/0
]).

-type config() :: #{atom() => term()}.

-export_type([config/0]).

%%%=============================================================================
%%% API
%%%=============================================================================

%% @doc Load configuration from default locations
-spec load() -> config().
load() ->
    load([]).

%% @doc Load configuration with options
-spec load(Opts :: list()) -> config().
load(Opts) ->
    % Start with defaults
    Config = default_config(),

    % Load from files (in order)
    Config1 = load_from_file(Config, "./tcps.config"),
    Config2 = load_from_file(Config1, home_config_path()),

    % Environment variable override
    Config3 = case os:getenv("TCPS_CONFIG") of
        false -> Config2;
        EnvPath -> load_from_file(Config2, EnvPath)
    end,

    % Command-line option override
    Config4 = case proplists:get_value(config, Opts) of
        undefined -> Config3;
        CmdPath -> load_from_file(Config3, CmdPath)
    end,

    % Apply command-line overrides
    apply_cli_overrides(Config4, Opts).

%% @doc Get configuration value
-spec get(Key :: atom()) -> term() | undefined.
get(Key) ->
    Config = load(),
    maps:get(Key, Config, undefined).

%% @doc Get configuration value with default
-spec get(Key :: atom(), Default :: term()) -> term().
get(Key, Default) ->
    Config = load(),
    maps:get(Key, Config, Default).

%% @doc Set configuration value
-spec set(Key :: atom(), Value :: term()) -> ok.
set(Key, Value) ->
    Config = load(),
    NewConfig = maps:put(Key, Value, Config),
    save(NewConfig).

%% @doc Save configuration to user config file
-spec save(config()) -> ok | {error, term()}.
save(Config) ->
    ConfigPath = home_config_path(),
    ensure_config_dir(),

    % Convert map to Erlang terms
    Terms = maps:to_list(Config),
    Content = lists:map(fun({K, V}) ->
        io_lib:format("{~p, ~p}.~n", [K, V])
    end, Terms),

    file:write_file(ConfigPath, Content).

%% @doc Get default configuration
-spec default_config() -> config().
default_config() ->
    #{
        % Paths
        ontology_path => "./ontology",
        receipts_path => "./priv/receipts",
        shacl_shapes => "./shapes",
        work_orders_path => "./priv/work_orders",

        % Output settings
        output_format => table,
        color_output => is_tty(),
        verbose => false,

        % Quality targets (from tcps_kaizen)
        quality_targets => #{
            lead_time => 2.0,          % hours
            defect_rate => 1.0,        % percent
            rework_pct => 5.0,         % percent
            cycle_time => 0.5,         % hours
            first_pass_yield => 95.0,  % percent
            throughput => 10.0         % SKUs per day
        },

        % Kanban WIP limits
        wip_limits => #{
            reliability => 5,
            security => 5,
            cost => 5,
            compliance => 5
        },

        % Andon settings
        andon_notifications_enabled => true,
        andon_auto_block => true,

        % Root cause analysis
        require_5_whys => true,
        auto_generate_prevention => true,

        % TPM settings
        maintenance_schedule => daily,
        health_check_interval => 3600,  % seconds

        % Kaizen settings
        kaizen_improvement_target => 0.05,  % 5% per week
        kaizen_auto_apply => false,

        % Display settings
        page_size => 20,
        date_format => iso8601,
        timezone => utc
    }.

%%%=============================================================================
%%% Internal Functions
%%%=============================================================================

%% @private Load configuration from file
-spec load_from_file(config(), file:filename()) -> config().
load_from_file(Config, Path) ->
    case file:consult(Path) of
        {ok, Terms} ->
            % Merge with existing config
            lists:foldl(fun({Key, Value}, Acc) ->
                maps:put(Key, Value, Acc)
            end, Config, Terms);
        {error, enoent} ->
            % File doesn't exist, use existing config
            Config;
        {error, Reason} ->
            io:format(standard_error,
                      "Warning: Failed to load config from ~s: ~p~n",
                      [Path, Reason]),
            Config
    end.

%% @private Get home config path
-spec home_config_path() -> file:filename().
home_config_path() ->
    Home = case os:getenv("HOME") of
        false -> ".";
        H -> H
    end,
    filename:join([Home, ".tcps", "config"]).

%% @private Ensure config directory exists
-spec ensure_config_dir() -> ok.
ensure_config_dir() ->
    ConfigDir = filename:dirname(home_config_path()),
    filelib:ensure_dir(ConfigDir ++ "/"),
    ok.

%% @private Check if output is to a TTY
-spec is_tty() -> boolean().
is_tty() ->
    case io:columns() of
        {ok, _} -> true;
        {error, _} -> false
    end.

%% @private Apply command-line overrides
-spec apply_cli_overrides(config(), list()) -> config().
apply_cli_overrides(Config, Opts) ->
    % Handle common CLI flags
    Config1 = case proplists:get_value(format, Opts) of
        undefined -> Config;
        Format -> maps:put(output_format, list_to_atom(Format), Config)
    end,

    Config2 = case lists:member(color, Opts) of
        true -> maps:put(color_output, true, Config1);
        false ->
            case lists:member(no_color, Opts) of
                true -> maps:put(color_output, false, Config1);
                false -> Config1
            end
    end,

    Config3 = case lists:member(verbose, Opts) orelse lists:member(v, Opts) of
        true -> maps:put(verbose, true, Config2);
        false -> Config2
    end,

    Config4 = case lists:member(quiet, Opts) orelse lists:member(q, Opts) of
        true -> maps:put(verbose, false, Config3);
        false -> Config3
    end,

    Config4.
