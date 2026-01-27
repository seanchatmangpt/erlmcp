%% @doc Monitoring Supervisor - Isolated Failure Domain for Observability

-module(erlmcp_monitoring_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-include("erlmcp.hrl").

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    SupFlags = #{
        strategy => rest_for_one,
        intensity => 10,
        period => 60
    },

    ChildSpecs = [
        #{
            id => erlmcp_metrics,
            start => {erlmcp_metrics, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [erlmcp_metrics]
        },

        #{
            id => erlmcp_monitor_dashboard,
            start => {erlmcp_monitor_dashboard, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [erlmcp_monitor_dashboard]
        },

        #{
            id => erlmcp_simple_monitor,
            start => {erlmcp_simple_monitor, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [erlmcp_simple_monitor]
        }
    ],

    {ok, {SupFlags, ChildSpecs}}.
