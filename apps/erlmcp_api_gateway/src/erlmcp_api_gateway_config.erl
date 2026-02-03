-module(erlmcp_api_gateway_config).

-behaviour(gen_server).

%% API exports
-export([start_link/0, init/0, get_config/0, update_config/2, get_rate_limits/0,
         get_security_policies/0, get_analytics_config/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Records
-record(state, {
    config :: map(),
    rate_limits :: map(),
    security_policies :: map(),
    analytics_config :: map()
}).

-include("erlmcp_api_gateway.hrl").

%%====================================================================
%% API Functions
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init() ->
    gen_server:call(?MODULE, init).

get_config() ->
    gen_server:call(?MODULE, get_config).

update_config(Key, Value) ->
    gen_server:call(?MODULE, {update_config, Key, Value}).

get_rate_limits() ->
    gen_server:call(?MODULE, get_rate_limits).

get_security_policies() ->
    gen_server:call(?MODULE, get_security_policies).

get_analytics_config() ->
    gen_server:call(?MODULE, get_analytics_config).

%%====================================================================
%% gen_server Callbacks
%%====================================================================

init([]) ->
    %% Load default enterprise API gateway configuration
    DefaultConfig = load_default_config(),
    RateLimits = load_rate_limits(),
    SecurityPolicies = load_security_policies(),
    AnalyticsConfig = load_analytics_config(),

    State = #state{
        config = DefaultConfig,
        rate_limits = RateLimits,
        security_policies = SecurityPolicies,
        analytics_config = AnalyticsConfig
    },

    %% Persist configuration to mnesia for high availability
    persist_config(State),

    {ok, State}.

handle_call(init, _From, State) ->
    %% Initialize configuration from persistent storage or defaults
    case load_persisted_config() of
        {ok, Config} ->
            {reply, ok, State#state{config = Config}};
        {error, not_found} ->
            %% Use defaults
            {reply, ok, State}
    end;

handle_call(get_config, _From, State) ->
    {reply, {ok, State#state.config}, State};

handle_call({update_config, Key, Value}, _From, State) ->
    NewConfig = maps:put(Key, Value, State#state.config),
    persist_config(State#state{config = NewConfig}),
    {reply, ok, State#state{config = NewConfig}};

handle_call(get_rate_limits, _From, State) ->
    {reply, {ok, State#state.rate_limits}, State};

handle_call(get_security_policies, _From, State) ->
    {reply, {ok, State#state.security_policies}, State};

handle_call(get_analytics_config, _From, State) ->
    {reply, {ok, State#state.analytics_config}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

load_default_config() ->
    #{
        %% Gateway Configuration
        gateway_name => <<"erlmcp-enterprise-api-gateway">>,
        version => <<"3.0.0">>,
        description => <<"Enterprise API Gateway for erlmcp">>,

        %% Network Configuration
        port => 8080,
        ssl_port => 8443,
        bind_address => {0, 0, 0, 0},
        ssl_enabled => true,

        %% Performance Configuration
        max_connections => 10000,
        max_request_size => 10485760, %% 10MB
        max_response_size => 10485760, %% 10MB
        timeout => 30000, %% 30 seconds

        %% Load Balancing
        load_balancer => #{
            strategy => round_robin,
            health_check_interval => 30000, %% 30 seconds
            health_check_timeout => 5000,
            unhealthy_threshold => 3,
            healthy_threshold => 2
        },

        %% Circuit Breaker
        circuit_breaker => #{
            enabled => true,
            error_threshold => 50,
            timeout => 10000, %% 10 seconds
            recovery_timeout => 30000, %% 30 seconds
            half_open_requests => 10
        },

        %% Caching
        cache => #{
            enabled => true,
            ttl => 300, %% 5 minutes
            size => 1000,
            eviction_policy => lru
        },

        %% Logging
        logging => #{
            level => info,
            format => json,
            access_log => true,
            error_log => true,
            audit_log => true
        },

        %% Monitoring
        monitoring => #{
            enabled => true,
            metrics_port => 9090,
            health_check_interval => 15000, %% 15 seconds
            metrics_retention => 86400 %% 24 hours
        }
    }.

load_rate_limits() ->
    #{
        %% Default Rate Limits
        default => #{
            requests_per_minute => 1000,
            requests_per_hour => 10000,
            requests_per_day => 100000,
            burst_size => 100
        },

        %% Enterprise Rate Limit tiers
        enterprise => #{
            gold => #{
                requests_per_minute => 5000,
                requests_per_hour => 50000,
                requests_per_day => 500000,
                burst_size => 500
            },
            platinum => #{
                requests_per_minute => 10000,
                requests_per_hour => 100000,
                requests_per_day => 1000000,
                burst_size => 1000
            },
            diamond => #{
                requests_per_minute => unlimited,
                requests_per_hour => unlimited,
                requests_per_day => unlimited,
                burst_size => unlimited
            }
        },

        %% Per-API rate limits
        per_api => #{
            analytics => #{
                requests_per_minute => 500,
                requests_per_hour => 5000,
                requests_per_day => 50000,
                burst_size => 50
            },
            auth => #{
                requests_per_minute => 100,
                requests_per_hour => 1000,
                requests_per_day => 10000,
                burst_size => 10
            },
            management => #{
                requests_per_minute => 200,
                requests_per_hour => 2000,
                requests_per_day => 20000,
                burst_size => 20
            }
        }
    }.

load_security_policies() ->
    #{
        %% Authentication
        authentication => #{
            enabled => true,
            methods => [oauth2, jwt, api_key, mutual_tls],
            default_method => oauth2,
            token_validation => true,
            token_refresh => true,
            session_timeout => 3600 %% 1 hour
        },

        %% Authorization
        authorization => #{
            enabled => true,
            mode => rbac, %% Role-Based Access Control
            default_role => consumer,
            policies => #{
                admin => [manage_apis, manage_consumers, view_analytics, manage_plugins],
                developer => [create_apis, manage_own_apis, test_apis, view_analytics],
                consumer => [use_apis, view_docs],
                guest => [view_docs]
            }
        },

        %% IP Filtering
        ip_filtering => #{
            enabled => true,
            allow_list => [],
            deny_list => [],
            geo_blocking => false,
            trusted_proxies => []
        },

        %% TLS Configuration
        tls => #{
            enabled => true,
            min_version => tlsv1.2,
            cipher_suites => [
                "ECDHE-ECDSA-AES256-GCM-SHA384",
                "ECDHE-RSA-AES256-GCM-SHA384",
                "ECDHE-ECDSA-AES128-GCM-SHA256",
                "ECDHE-RSA-AES128-GCM-SHA256"
            ],
            certificate_validation => true,
            ocsp_stapling => true
        },

        %% CORS
        cors => #{
            enabled => true,
            allowed_origins => [<<"*">>],
            allowed_methods => [<<"GET">>, <<"POST">>, <<"PUT">>, <<"DELETE">>, <<"OPTIONS">>],
            allowed_headers => [<<"*">>],
            exposed_headers => [<<"X-API-Version">>, <<"X-Request-ID">>],
            max_age => 3600
        },

        %% Security Headers
        security_headers => #{
            enabled => true,
            headers => [
                {<<"X-Frame-Options">>, <<"DENY">>},
                {<<"X-Content-Type-Options">>, <<"nosniff">>},
                {<<"X-XSS-Protection">>, <<"1; mode=block">>},
                {<<"Strict-Transport-Security">>, <<"max-age=31536000; includeSubDomains">>},
                {<<"Content-Security-Policy">>, <<"default-src 'self'">>}
            ]
        }
    }.

load_analytics_config() ->
    #{
        %% Analytics Collection
        collection => #{
            enabled => true,
            request_logging => true,
            response_logging => true,
            error_logging => true,
            performance_metrics => true
        },

        %% Metrics Storage
        storage => #{
            backend => prometheus,
            retention => 86400, %% 24 hours
            aggregation_interval => 60000, %% 1 minute
            batch_size => 1000
        },

        %% Analytics Events
        events => #[
            api_call,
            auth_attempt,
            rate_limit_violation,
            error_response,
            performance_threshold
        ],

        %% Alerting
        alerting => #{
            enabled => true,
            rules => #{
                high_error_rate => #{
                    threshold => 0.05, %% 5%
                    window => 300000, %% 5 minutes
                    severity => warning
                },
                high_latency => #{
                    threshold => 5000, %% 5 seconds
                    window => 300000, %% 5 minutes
                    severity => warning
                },
                rate_limit_exceeded => #{
                    threshold => 100, %% 100 violations
                    window => 300000, %% 5 minutes
                    severity => critical
                }
            }
        }
    }.

persist_config(State) ->
    %% Persist to mnesia for high availability
    try
        mnesia:transaction(fun() ->
            mnesia:write(#api_gateway_config{
                id = gateway_config,
                config = State#state.config,
                rate_limits = State#state.rate_limits,
                security_policies = State#state.security_policies,
                analytics_config = State#state.analytics_config,
                updated_at = erlang:system_time(millisecond)
            })
        end)
    catch
        _:Reason ->
            error_logger:error_msg("Failed to persist API gateway config: ~p", [Reason])
    end.

load_persisted_config() ->
    case mnesia:transaction(fun() ->
        mnesia:read({api_gateway_config, gateway_config})
    end) of
        {atomic, [#api_gateway_config{} = Config]} -> {ok, Config};
        {atomic, []} -> {error, not_found};
        {aborted, Reason} -> {error, Reason}
    end.