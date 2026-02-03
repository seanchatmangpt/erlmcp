-module(erlmcp_api_gateway_sup).
-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    %% Define the strategy for the supervisor
    SupFlags = #{
        strategy => one_for_all,
        intensity => 10,
        period => 10
    },

    %% Define the children with enterprise-grade services
    ChildSpecs = [
        %% API Gateway Configuration
        #{
            id => erlmcp_api_gateway_config,
            start => {erlmcp_api_gateway_config, start_link, []},
            type => worker,
            modules => [erlmcp_api_gateway_config],
            restart => permanent,
            shutdown => 5000,
            significance => critical
        },

        %% API Management Core Service
        #{
            id => erlmcp_api_management,
            start => {erlmcp_api_management, start_link, []},
            type => worker,
            modules => [erlmcp_api_management],
            restart => permanent,
            shutdown => 5000,
            significance => critical
        },

        %% Rate Limiter Service
        #{
            id => erlmcp_api_rate_limiter,
            start => {erlmcp_api_rate_limiter, start_link, []},
            type => worker,
            modules => [erlmcp_api_rate_limiter],
            restart => permanent,
            shutdown => 5000,
            significance => critical
        },

        %% Analytics Service
        #{
            id => erlmcp_api_analytics,
            start => {erlmcp_api_analytics, start_link, []},
            type => worker,
            modules => [erlmcp_api_analytics],
            restart => permanent,
            shutdown => 5000,
            significance => critical
        },

        %% Authentication Service
        #{
            id => erlmcp_api_auth,
            start => {erlmcp_api_auth, start_link, []},
            type => worker,
            modules => [erlmcp_api_auth],
            restart => permanent,
            shutdown => 5000,
            significance => critical
        },

        %% Plugin Manager
        #{
            id => erlmcp_api_plugin_manager,
            start => {erlmcp_api_plugin_manager, start_link, []},
            type => worker,
            modules => [erlmcp_api_plugin_manager],
            restart => permanent,
            shutdown => 5000,
            significance => non_critical
        },

        %% Monitoring Service
        #{
            id => erlmcp_api_monitor,
            start => {erlmcp_api_monitor, start_link, []},
            type => worker,
            modules => [erlmcp_api_monitor],
            restart => permanent,
            shutdown => 5000,
            significance => critical
        },

        %% Health Checker
        #{
            id => erlmcp_api_health,
            start => {erlmcp_api_health, start_link, []},
            type => worker,
            modules => [erlmcp_api_health],
            restart => permanent,
            shutdown => 5000,
            significance => critical
        },

        %% Load Balancer Service
        #{
            id => erlmcp_api_balancer,
            start => {erlmcp_api_balancer, start_link, []},
            type => worker,
            modules => [erlmcp_api_balancer],
            restart => permanent,
            shutdown => 5000,
            significance => critical
        },

        %% Circuit Breaker
        #{
            id => erlmcp_api_circuit_breaker,
            start => {erlmcp_api_circuit_breaker, start_link, []},
            type => worker,
            modules => [erlmcp_api_circuit_breaker],
            restart => permanent,
            shutdown => 5000,
            significance => critical
        },

        %% API Registry
        #{
            id => erlmcp_api_gateway_registry,
            start => {erlmcp_api_gateway_registry, start_link, []},
            type => worker,
            modules => [erlmcp_api_gateway_registry],
            restart => permanent,
            shutdown => 5000,
            significance => critical
        },

        %% Cache Service
        #{
            id => erlmcp_api_cache,
            start => {erlmcp_api_cache, start_link, []},
            type => worker,
            modules => [erlmcp_api_cache],
            restart => permanent,
            shutdown => 5000,
            significance => non_critical
        }
    ],

    {ok, {SupFlags, ChildSpecs}}.