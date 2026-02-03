%% @doc erlmcp_enterprise application supervisor
%% Enterprise Integration Suite for Fortune 500 systems
-module(erlmcp_enterprise_app).
-behaviour(application).

-export([start/2, stop/1, prep_stop/1]).

%%====================================================================
%% API functions
%%====================================================================

-spec start(StartType :: term(), StartArgs :: term()) ->
    {ok, pid()} | {error, term()}.
start(_StartType, _StartArgs) ->
    %% Ensure environment setup
    ensure_environment(),

    %% Start top-level supervisor
    erlmcp_enterprise_sup:start_link().

-spec stop(State :: term()) -> ok.
stop(_State) ->
    ok.

-spec prep_stop(term()) -> term().
prep_stop(State) ->
    logger:info("Preparing erlmcp_enterprise for graceful shutdown"),
    %% Close enterprise integrations gracefully
    try
        erlmcp_enterprise_adapters:shutdown(),
        erlmcp_enterprise_monitor:shutdown()
    catch
        _:_ -> ok
    end,
    State.

%%====================================================================
%% Internal functions
%%====================================================================

-spec ensure_environment() -> ok.
ensure_environment() ->
    %% Load enterprise configuration
    case application:get_env(erlmcp_enterprise, config) of
        undefined ->
            load_default_config();
        _ ->
            ok
    end,

    %% Initialize connection pools
    erlmcp_enterprise_pools:init(),

    %% Load integration adapters
    erlmcp_enterprise_adapters:load(),

    %% Initialize monitoring
    erlmcp_enterprise_monitor:init().

-spec load_default_config() -> ok.
load_default_config() ->
    DefaultConfig = #{
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
                token => undefined,
                index => "erlmcp"
            },
            datadog => #{
                enabled => false,
                api_key => undefined,
                app_key => undefined,
                host => "https://api.datadoghq.com"
            },
            new_relic => #{
                enabled => false,
                api_key => undefined,
                account_id => undefined
            }
        },
        logging_platforms => #{
            elasticsearch => #{
                enabled => false,
                hosts => ["localhost:9200"],
                username => undefined,
                password => undefined,
                index => "erlmcp-logs"
            },
            graylog => #{
                enabled => false,
                host => "localhost",
                port => 12201,
                protocol => udp
            },
            sumo_logic => #{
                enabled => false,
                endpoint => undefined,
                access_id => undefined,
                access_key => undefined
            }
        },
        business_intelligence => #{
            tableau => #{
                enabled => false,
                server_url => undefined,
                site_id => undefined,
                personal_access_token => undefined
            },
            power_bi => #{
                enabled => false,
                client_id => undefined,
                client_secret => undefined,
                tenant_id => undefined
            }
        },
        service_buses => #{
            kafka => #{
                enabled => false,
                brokers => ["localhost:9092"],
                topic_prefix => "erlmcp",
                sasl => undefined
            },
            esb => #{
                enabled => false,
                endpoint => undefined,
                username => undefined,
                password => undefined
            }
        },
        data_warehouses => #{
            snowflake => #{
                enabled => false,
                account => undefined,
                warehouse => undefined,
                database => undefined,
                schema => undefined,
                user => undefined,
                password => undefined
            },
            bigquery => #{
                enabled => false,
                project_id => undefined,
                dataset_id => undefined,
                service_account_key => undefined
            }
        },
        devops_tools => #{
            jenkins => #{
                enabled => false,
                endpoint => "http://localhost:8080",
                username => undefined,
                api_token => undefined
            },
            gitlab_ci => #{
                enabled => false,
                endpoint => "https://gitlab.com",
                private_token => undefined
            }
        },
        api_gateways => #{
            kong => #{
                enabled => false,
                endpoint => "http://localhost:8001",
                admin_key => undefined
            },
            apigee => #{
                enabled => false,
                endpoint => undefined,
                org_name => undefined,
                api_key => undefined
            }
        },
        cloud_platforms => #{
            aws => #{
                enabled => false,
                region => "us-east-1",
                access_key_id => undefined,
                secret_access_key => undefined
            },
            azure => #{
                enabled => false,
                subscription_id => undefined,
                tenant_id => undefined,
                client_id => undefined,
                client_secret => undefined
            },
            gcp => #{
                enabled => false,
                project_id => undefined,
                service_account_key => undefined
            }
        },
        security_systems => #{
            siem => #{
                enabled => false,
                endpoint => undefined,
                api_key => undefined
            }
        },
        configuration_management => #{
            ansible => #{
                enabled => false,
                endpoint => "http://localhost:8080",
                username => undefined,
                password => undefined
            },
            puppet => #{
                enabled => false,
                endpoint => "https://puppet.example.com",
                cert => undefined,
                key => undefined
            }
        },
        container_orchestration => #{
            kubernetes => #{
                enabled => false,
                kube_config => undefined,
                namespace => "default"
            },
            swarm => #{
                enabled => false,
                endpoint => "tcp://localhost:2375",
                tls => false
            }
        }
    },

    %% Store configuration
    application:set_env(erlmcp_enterprise, config, DefaultConfig),

    %% Create config file
    ConfigFile = filename:join([code:priv_dir(erlmcp_enterprise), "config", "enterprise_config.json"]),
    filelib:ensure_dir(filename:dirname(ConfigFile)),
    ok = file:write_file(ConfigFile, jsx:encode(DefaultConfig)).