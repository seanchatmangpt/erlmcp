-module(erlmcp_dev_portal_sup).

-behaviour(supervisor).

%% API exports
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%%====================================================================
%% API Functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%====================================================================
%% Supervisor Callbacks
%%====================================================================

init([]) ->
    %% Define the strategy for the supervisor
    Strategy = one_for_all,
    Intensity = 10,
    Period = 10,

    %% Define the children with enterprise-grade services
    ChildSpecs = [
        %% Dev Portal Configuration
        #{
            id => erlmcp_dev_portal_config,
            start => {erlmcp_dev_portal_config, start_link, []},
            type => worker,
            modules => [erlmcp_dev_portal_config],
            restart => permanent,
            shutdown => 5000,
            significance => critical
        },

        %% Documentation Service
        #{
            id => erlmcp_dev_portal_docs,
            start => {erlmcp_dev_portal_docs, start_link, []},
            type => worker,
            modules => [erlmcp_dev_portal_docs],
            restart => permanent,
            shutdown => 5000,
            significance => critical
        },

        %% API Explorer Service
        #{
            id => erlmcp_dev_portal_explorer,
            start => {erlmcp_dev_portal_explorer, start_link, []},
            type => worker,
            modules => [erlmcp_dev_portal_explorer],
            restart => permanent,
            shutdown => 5000,
            significance => critical
        },

        %% Developer Dashboard Service
        #{
            id => erlmcp_dev_portal_dashboard,
            start => {erlmcp_dev_portal_dashboard, start_link, []},
            type => worker,
            modules => [erlmcp_dev_portal_dashboard],
            restart => permanent,
            shutdown => 5000,
            significance => critical
        },

        %% Testing Service
        #{
            id => erlmcp_dev_portal_test,
            start => {erlmcp_dev_portal_test, start_link, []},
            type => worker,
            modules => [erlmcp_dev_portal_test],
            restart => permanent,
            shutdown => 5000,
            significance => non_critical
        },

        %% Authentication Service
        #{
            id => erlmcp_dev_portal_auth,
            start => {erlmcp_dev_portal_auth, start_link, []},
            type => worker,
            modules => [erlmcp_dev_portal_auth],
            restart => permanent,
            shutdown => 5000,
            significance => critical
        },

        %% Community Service
        #{
            id => erlmcp_dev_portal_community,
            start => {erlmcp_dev_portal_community, start_link, []},
            type => worker,
            modules => [erlmcp_dev_portal_community],
            restart => permanent,
            shutdown => 5000,
            significance => non_critical
        },

        %% Support Service
        #{
            id => erlmcp_dev_portal_support,
            start => {erlmcp_dev_portal_support, start_link, []},
            type => worker,
            modules => [erlmcp_dev_portal_support],
            restart => permanent,
            shutdown => 5000,
            significance => critical
        },

        %% Analytics Service
        #{
            id => erlmcp_dev_portal_analytics,
            start => {erlmcp_dev_portal_analytics, start_link, []},
            type => worker,
            modules => [erlmcp_dev_portal_analytics],
            restart => permanent,
            shutdown => 5000,
            significance => non_critical
        },

        %% Static Asset Manager
        #{
            id => erlmcp_dev_portal_assets,
            start => {erlmcp_dev_portal_assets, start_link, []},
            type => worker,
            modules => [erlmcp_dev_portal_assets],
            restart => permanent,
            shutdown => 5000,
            significance => non_critical
        }
    ],

    {ok, {{Strategy, Intensity, Period}, ChildSpecs}}.