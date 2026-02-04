-module(erlmcp_new_features_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SUP, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link() ->
    supervisor:start_link({local, ?SUP}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 10,
        period => 60
    },

    ChildSpecs = [
        % Tier 1: Observability and monitoring services (one_for_all)
        #{
            id => erlmcp_observability,
            start => {erlmcp_observability, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [erlmcp_observability]
        },
        #{
            id => erlmcp_health_check,
            start => {erlmcp_health_check, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [erlmcp_health_check]
        },
        #{
            id => erlmcp_dashboard_integration,
            start => {erlmcp_dashboard_integration, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [erlmcp_dashboard_integration]
        },

        % Tier 2: Connection-based services (per-connection)
        #{
            id => erlmcp_mcp_proxy_relay,
            start => {erlmcp_mcp_proxy_relay, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [erlmcp_mcp_proxy_relay]
        },

        % Tier 3: Event-driven services
        #{
            id => erlmcp_event_bus,
            start => {erlmcp_event_bus, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [erlmcp_event_bus]
        },

        % Tier 4: Processing services
        #{
            id => erlmcp_batch_processor,
            start => {erlmcp_batch_processor, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [erlmcp_batch_processor]
        },

        % Tier 5: Validation services
        #{
            id => erlmcp_json_schema_validator,
            start => {erlmcp_json_schema_validator, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [erlmcp_json_schema_validator]
        },

        % Tier 6: Security services
        #{
            id => erlmcp_tool_sandbox,
            start => {erlmcp_tool_sandbox, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [erlmcp_tool_sandbox]
        }
    ],

    {ok, {SupFlags, ChildSpecs}}.
