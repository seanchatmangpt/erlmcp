%% @doc Enterprise Integrations Top Supervisor
%% Manages all enterprise integration services
-module(erlmcp_enterprise_integrations_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

%%====================================================================
%% API functions
%%====================================================================

-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%====================================================================
 Supervisor callbacks
%%====================================================================

-spec init(_) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    %% Define child specifications for all enterprise integration services
    Children = [
        %% Integration Gateway (main service)
        #{
            id => erlmcp_integration_gateway,
            start => {erlmcp_integration_gateway, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [erlmcp_integration_gateway]
        },
        %% Identity Providers Supervisor
        #{
            id => erlmcp_identity_sup,
            start => {erlmcp_identity_sup, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => supervisor,
            modules => [erlmcp_identity_sup]
        },
        %% Monitoring Systems Supervisor
        #{
            id => erlmcp_monitoring_sup,
            start => {erlmcp_monitoring_sup, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => supervisor,
            modules => [erlmcp_monitoring_sup]
        },
        %% Logging Platforms Supervisor
        #{
            id => erlmcp_logging_sup,
            start => {erlmcp_logging_sup, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => supervisor,
            modules => [erlmcp_logging_sup]
        },
        %% Business Intelligence Supervisor
        #{
            id => erlmcp_bi_sup,
            start => {erlmcp_bi_sup, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => supervisor,
            modules => [erlmcp_bi_sup]
        },
        %% Service Bus Supervisor
        #{
            id => erlmcp_esb_sup,
            start => {erlmcp_esb_sup, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => supervisor,
            modules => [erlmcp_esb_sup]
        },
        %% Data Warehouse Supervisor
        #{
            id => erlmcp_dwh_sup,
            start => {erlmcp_dwh_sup, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => supervisor,
            modules => [erlmcp_dwh_sup]
        },
        %% DevOps Tools Supervisor
        #{
            id => erlmcp_devops_sup,
            start => {erlmcp_devops_sup, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => supervisor,
            modules => [erlmcp_devops_sup]
        },
        %% API Gateway Supervisor
        #{
            id => erlmcp_api_gateway_sup,
            start => {erlmcp_api_gateway_sup, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => supervisor,
            modules => [erlmcp_api_gateway_sup]
        },
        %% Cloud Platforms Supervisor
        #{
            id => erlmcp_cloud_sup,
            start => {erlmcp_cloud_sup, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => supervisor,
            modules => [erlmcp_cloud_sup]
        },
        %% Security Systems Supervisor
        #{
            id => erlmcp_security_sup,
            start => {erlmcp_security_sup, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => supervisor,
            modules => [erlmcp_security_sup]
        },
        %% Configuration Management Supervisor
        #{
            id => erlmcp_config_sup,
            start => {erlmcp_config_sup, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => supervisor,
            modules => [erlmcp_config_sup]
        },
        %% Container Orchestration Supervisor
        #{
            id => erlmcp_container_sup,
            start => {erlmcp_container_sup, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => supervisor,
            modules => [erlmcp_container_sup]
        }
    ],

    %% Configure supervision strategy
    SupFlags = #{
        strategy => one_for_all,
        intensity => 1,
        period => 5
    },

    {ok, {SupFlags, Children}}.