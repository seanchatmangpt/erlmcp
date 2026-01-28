%% @doc Configuration Profiles Module
%% Manages pre-configured profiles for different deployment scenarios:
%% - dev: Local development with minimal resources
%% - staging: Staging environment with moderate resources
%% - production: Production deployment with conservative tuning
%% - load_test: Extreme tuning for 100K+ concurrent connections
%%
%% Each profile defines complete configuration for erlmcp application,
%% transport layers, and VM arguments.
%%
%% Usage:
%%   Profile = erlmcp_config_profiles:get_profile(production),
%%   erlmcp_config_profiles:apply_profile(production),
%%   erlmcp_config_profiles:list_profiles()
%%
-module(erlmcp_config_profiles).

-export([
    get_profile/1,
    list_profiles/0,
    apply_profile/1,
    apply_profile/2,
    describe_profile/1,
    validate_profile/1,
    merge_with_env/1,
    get_sys_config/1,
    get_vm_args/1
]).

-type profile_name() :: dev | staging | production | load_test.
-type profile() :: #{
    name := atom(),
    description := string(),
    system_config := list(),
    vm_args := list(),
    transport_config := map(),
    resource_estimates := map()
}.

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Get configuration profile by name
-spec get_profile(profile_name()) -> {ok, profile()} | {error, term()}.
get_profile(dev) ->
    {ok, profile_dev()};
get_profile(staging) ->
    {ok, profile_staging()};
get_profile(production) ->
    {ok, profile_production()};
get_profile(load_test) ->
    {ok, profile_load_test()};
get_profile(Name) ->
    {error, {unknown_profile, Name}}.

%% @doc List all available profile names
-spec list_profiles() -> [profile_name()].
list_profiles() ->
    [dev, staging, production, load_test].

%% @doc Apply a profile configuration to the application
-spec apply_profile(profile_name()) -> ok | {error, term()}.
apply_profile(ProfileName) ->
    apply_profile(ProfileName, []).

%% @doc Apply profile with additional options
-spec apply_profile(profile_name(), list()) -> ok | {error, term()}.
apply_profile(ProfileName, _Options) ->
    case get_profile(ProfileName) of
        {ok, _Profile} ->
            % Load configuration via erlmcp_config module
            case erlmcp_config:load_and_watch(ProfileName) of
                ok -> ok;
                Error -> {error, {failed_to_apply, Error}}
            end;
        Error ->
            Error
    end.

%% @doc Get system config file content for profile
-spec get_sys_config(profile_name()) -> list().
get_sys_config(ProfileName) ->
    case get_profile(ProfileName) of
        {ok, Profile} ->
            maps:get(system_config, Profile);
        {error, _} ->
            []
    end.

%% @doc Get VM arguments for profile
-spec get_vm_args(profile_name()) -> list().
get_vm_args(ProfileName) ->
    case get_profile(ProfileName) of
        {ok, Profile} ->
            maps:get(vm_args, Profile);
        {error, _} ->
            []
    end.

%% @doc Describe profile characteristics
-spec describe_profile(profile_name()) -> ok.
describe_profile(ProfileName) ->
    case get_profile(ProfileName) of
        {ok, Profile} ->
            io:format("~n=== Profile: ~w ===~n", [maps:get(name, Profile)]),
            io:format("Description: ~s~n", [maps:get(description, Profile)]),

            case maps:get(resource_estimates, Profile, undefined) of
                undefined -> ok;
                Estimates ->
                    io:format("~nResource Estimates:~n", []),
                    maps:foreach(fun(Key, Value) ->
                        io:format("  ~w: ~w~n", [Key, Value])
                    end, Estimates)
            end,
            ok;
        {error, Reason} ->
            io:format("Error: ~w~n", [Reason])
    end.

%% @doc Validate profile configuration
-spec validate_profile(profile_name()) -> ok | {error, [term()]}.
validate_profile(ProfileName) ->
    case get_profile(ProfileName) of
        {ok, Profile} ->
            Errors = validate_profile_internal(Profile),
            case Errors of
                [] -> ok;
                _ -> {error, Errors}
            end;
        Error ->
            Error
    end.

%% @doc Merge profile with environment variable overrides
-spec merge_with_env(profile_name()) -> list().
merge_with_env(ProfileName) ->
    SysConfig = get_sys_config(ProfileName),
    apply_env_overrides(SysConfig, erlmcp, get_erlmcp_env_overrides()).

%%====================================================================
%% Profile Definitions
%%====================================================================

%% @doc Development profile - minimal resources, all features enabled for testing
-spec profile_dev() -> profile().
profile_dev() ->
    SmartDefaults = erlmcp_smart_defaults:detect_and_calculate(dev),
    #{
        name => dev,
        description => "Development profile: Local development with debug logging, minimal resources",
        system_config => [
            {erlmcp, [
                {log_level, debug},
                {client_defaults, #{
                    timeout => 5000,
                    strict_mode => false,
                    max_pending_requests => 100
                }},
                {server_defaults, #{
                    max_subscriptions_per_resource => 1000,
                    max_progress_tokens => 10000
                }},
                {transport_defaults, #{
                    tcp => #{
                        connect_timeout => 5000,
                        keepalive => false,
                        nodelay => false,
                        max_connections => 100
                    },
                    http => #{
                        connect_timeout => 5000,
                        request_timeout => 30000,
                        max_connections => 50
                    }
                }},
                {connection_pool_config, extract_pool_config(SmartDefaults)},
                {rate_limiting, #{enabled => false}}
            ]},
            {kernel, [
                {logger_level, debug}
            ]}
        ],
        vm_args => [
            "-name erlmcp_dev@127.0.0.1",
            "-setcookie erlmcp_dev_cookie",
            "+K true",
            "+sbt db",
            "+sbwt medium",
            "+sub true",
            "+P 10000",
            "+t 1000000",
            "+A 4",
            "-env ERL_MAX_PORTS 1024",
            "-env ERL_MAX_ETS_TABLES 5000",
            "-heart"
        ],
        transport_config => #{
            stdio => #{enabled => true, buffer_size => 1024},
            tcp => #{enabled => false, port => 8080},
            http => #{enabled => false, port => 8081}
        },
        resource_estimates => #{
            max_connections => 100,
            memory_usage_mb => 200,
            cpu_usage => "varies",
            suitable_for => "local development, testing"
        }
    }.

%% @doc Staging profile - moderate resources, production-like configuration
-spec profile_staging() -> profile().
profile_staging() ->
    SmartDefaults = erlmcp_smart_defaults:detect_and_calculate(staging),
    #{
        name => staging,
        description => "Staging profile: Pre-production environment with moderate resources",
        system_config => [
            {erlmcp, [
                {log_level, info},
                {client_defaults, #{
                    timeout => 5000,
                    strict_mode => true,
                    max_pending_requests => 250
                }},
                {server_defaults, #{
                    max_subscriptions_per_resource => 2500,
                    max_progress_tokens => 25000
                }},
                {transport_defaults, #{
                    tcp => #{
                        connect_timeout => 5000,
                        keepalive => true,
                        nodelay => true,
                        max_connections => 500
                    },
                    http => #{
                        connect_timeout => 5000,
                        request_timeout => 30000,
                        max_connections => 200
                    }
                }},
                {connection_pool_config, extract_pool_config(SmartDefaults)},
                {rate_limiting, #{
                    enabled => true,
                    max_messages_per_sec => 100,
                    global_max_messages_per_sec => 5000
                }}
            ]},
            {kernel, [
                {logger_level, info}
            ]}
        ],
        vm_args => [
            "-name erlmcp_staging@0.0.0.0",
            "-setcookie erlmcp_staging_cookie",
            "+K true",
            "+sbt db",
            "+sbwt short",
            "+sub true",
            "+P 100000",
            "+t 5000000",
            "+A 8",
            "-env ERL_MAX_PORTS 10000",
            "-env ERL_MAX_ETS_TABLES 50000",
            "-heart"
        ],
        transport_config => #{
            stdio => #{enabled => true, buffer_size => 4096},
            tcp => #{enabled => true, port => 8080, max_connections => 500},
            http => #{enabled => true, port => 8081, max_connections => 200}
        },
        resource_estimates => #{
            max_connections => 1000,
            memory_usage_mb => 1024,
            cpu_usage => "2-4 cores",
            suitable_for => "staging, testing under load"
        }
    }.

%% @doc Production profile - full resources, conservative tuning
-spec profile_production() -> profile().
profile_production() ->
    SmartDefaults = erlmcp_smart_defaults:detect_and_calculate(production),
    #{
        name => production,
        description => "Production profile: Full production deployment with conservative tuning",
        system_config => [
            {erlmcp, [
                {log_level, warning},
                {client_defaults, #{
                    timeout => 5000,
                    strict_mode => true,
                    max_pending_requests => 500
                }},
                {server_defaults, #{
                    max_subscriptions_per_resource => 5000,
                    max_progress_tokens => 50000
                }},
                {transport_defaults, #{
                    tcp => #{
                        connect_timeout => 5000,
                        keepalive => true,
                        nodelay => true,
                        max_connections => 2000
                    },
                    http => #{
                        connect_timeout => 5000,
                        request_timeout => 30000,
                        max_connections => 1000
                    }
                }},
                {connection_pool_config, extract_pool_config(SmartDefaults)},
                {rate_limiting, #{
                    enabled => true,
                    max_messages_per_sec => 500,
                    global_max_messages_per_sec => 10000
                }}
            ]},
            {kernel, [
                {logger_level, warning}
            ]}
        ],
        vm_args => [
            "-name erlmcp_prod@0.0.0.0",
            "-setcookie erlmcp_prod_cookie",
            "+K true",
            "+sbt db",
            "+sbwt very_short",
            "+sub true",
            "+P 262144",
            "+t 10000000",
            "+A 16",
            "-env ERL_MAX_PORTS 65536",
            "-env ERL_MAX_ETS_TABLES 100000",
            "-heart",
            "-env ERL_CRASH_DUMP_SECONDS 10"
        ],
        transport_config => #{
            stdio => #{enabled => false, buffer_size => 16384},
            tcp => #{enabled => true, port => 8080, max_connections => 2000},
            http => #{enabled => true, port => 8081, max_connections => 1000}
        },
        resource_estimates => #{
            max_connections => 10000,
            memory_usage_mb => 4096,
            cpu_usage => "4-8 cores",
            suitable_for => "production, heavy workloads"
        }
    }.

%% @doc Load Test profile - extreme tuning for 100K+ concurrent connections
-spec profile_load_test() -> profile().
profile_load_test() ->
    SmartDefaults = erlmcp_smart_defaults:detect_and_calculate(load_test),
    #{
        name => load_test,
        description => "Load Test profile: Extreme tuning for 100K+ concurrent connections",
        system_config => [
            {erlmcp, [
                {log_level, error},
                {client_defaults, #{
                    timeout => 10000,
                    strict_mode => true,
                    max_pending_requests => 1000
                }},
                {server_defaults, #{
                    max_subscriptions_per_resource => 10000,
                    max_progress_tokens => 100000
                }},
                {transport_defaults, #{
                    tcp => #{
                        connect_timeout => 10000,
                        keepalive => true,
                        nodelay => true,
                        max_connections => 100000
                    },
                    http => #{
                        connect_timeout => 10000,
                        request_timeout => 60000,
                        max_connections => 50000
                    }
                }},
                {connection_pool_config, extract_pool_config(SmartDefaults)},
                {rate_limiting, #{
                    enabled => true,
                    max_messages_per_sec => 10000,
                    global_max_messages_per_sec => 100000
                }}
            ]},
            {kernel, [
                {logger_level, error}
            ]}
        ],
        vm_args => [
            "-name erlmcp_loadtest@0.0.0.0",
            "-setcookie erlmcp_loadtest",
            "+K true",
            "+sbt db",
            "+sbwt very_short",
            "+sub true",
            "+P 1000000",
            "+t 50000000",
            "+A 32",
            "-env ERL_MAX_PORTS 131072",
            "-env ERL_MAX_ETS_TABLES 200000",
            "-heart",
            "-env ERL_CRASH_DUMP_SECONDS 10",
            "-env ERL_CRASH_DUMP ./logs/erl_crash.dump"
        ],
        transport_config => #{
            stdio => #{enabled => false, buffer_size => 131072},
            tcp => #{enabled => true, port => 8080, max_connections => 100000},
            http => #{enabled => true, port => 8081, max_connections => 50000}
        },
        resource_estimates => #{
            max_connections => 100000,
            memory_usage_mb => 16384,
            cpu_usage => "8-32+ cores",
            suitable_for => "load testing, 100K+ concurrent connections"
        }
    }.

%%====================================================================
%% Helper Functions
%%====================================================================

%% @doc Extract pool config from smart defaults
-spec extract_pool_config(map()) -> map().
extract_pool_config(Defaults) ->
    case maps:get(pool_config, Defaults, undefined) of
        undefined -> #{};
        PoolCfg -> PoolCfg
    end.

%% @doc Get erlmcp-specific environment variable overrides
-spec get_erlmcp_env_overrides() -> map().
get_erlmcp_env_overrides() ->
    #{
        "ERLMCP_LOG_LEVEL" => log_level,
        "ERLMCP_MAX_CONNECTIONS" => max_connections,
        "ERLMCP_POOL_SIZE" => pool_size,
        "ERLMCP_BUFFER_SIZE" => buffer_size,
        "ERLMCP_TIMEOUT" => timeout,
        "ERLMCP_PORT" => port
    }.

%% @doc Apply environment variable overrides to system config
-spec apply_env_overrides(list(), atom(), map()) -> list().
apply_env_overrides(Config, AppName, EnvMap) ->
    % Get current app config
    case proplists:get_value(AppName, Config, undefined) of
        undefined ->
            Config;
        AppCfg ->
            % Apply overrides
            UpdatedAppCfg = apply_env_overrides_to_config(AppCfg, EnvMap),
            lists:keyreplace(AppName, 1, Config, {AppName, UpdatedAppCfg})
    end.

%% @doc Apply environment overrides to app config
-spec apply_env_overrides_to_config(list(), map()) -> list().
apply_env_overrides_to_config(Config, EnvMap) ->
    maps:fold(
        fun(EnvVar, ConfigKey, Acc) ->
            case os:getenv(EnvVar) of
                false -> Acc;
                Value -> apply_env_override(Acc, ConfigKey, Value)
            end
        end,
        Config,
        EnvMap
    ).

%% @doc Apply a single environment override
-spec apply_env_override(list(), term(), string()) -> list().
apply_env_override(Config, Key, Value) when is_atom(Key) ->
    lists:keystore(Key, 1, Config, {Key, parse_env_value(Value)});
apply_env_override(Config, _Key, _Value) ->
    Config.

%% @doc Parse environment variable value
-spec parse_env_value(string()) -> term().
parse_env_value(Value) ->
    case string:to_lower(Value) of
        "true" -> true;
        "false" -> false;
        _ ->
            try
                list_to_integer(Value)
            catch
                _:_ ->
                    case string:to_atom(Value) of
                        {error, _} -> Value;
                        Atom -> Atom
                    end
            end
    end.

%% @doc Validate profile configuration
-spec validate_profile_internal(profile()) -> [term()].
validate_profile_internal(Profile) ->
    Errors = [],

    % Check required fields
    Errors1 = case maps:get(name, Profile, undefined) of
        undefined -> [missing_name | Errors];
        _ -> Errors
    end,

    Errors2 = case maps:get(system_config, Profile, undefined) of
        undefined -> [missing_system_config | Errors1];
        _ -> Errors1
    end,

    Errors3 = case maps:get(vm_args, Profile, undefined) of
        undefined -> [missing_vm_args | Errors2];
        _ -> Errors2
    end,

    Errors3.
