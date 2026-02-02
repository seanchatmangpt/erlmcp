%%%-------------------------------------------------------------------
%%% @doc erlmcp-flow Top-Level Supervisor
%%% Supervision strategy: one_for_all (all components depend on each other)
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_flow_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor Callbacks
%%%===================================================================

init([]) ->
    SupFlags = #{
        strategy => one_for_all,
        intensity => 5,
        period => 60
    },

    ChildSpecs = [
        %% Core Registry (gproc-based)
        #{
            id => erlmcp_flow_registry,
            start => {erlmcp_flow_registry, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [erlmcp_flow_registry]
        },

        %% Q-Learning Engine
        #{
            id => erlmcp_flow_q_learning,
            start => {erlmcp_flow_q_learning, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [erlmcp_flow_q_learning]
        },

        %% Circuit Breaker
        #{
            id => erlmcp_flow_circuit_breaker,
            start => {erlmcp_flow_circuit_breaker, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [erlmcp_flow_circuit_breaker]
        },

        %% Correlation Tracker
        #{
            id => erlmcp_flow_correlation_tracker,
            start => {erlmcp_flow_correlation_tracker, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [erlmcp_flow_correlation_tracker]
        },

        %% Byzantine Detector
        #{
            id => erlmcp_flow_byzantine,
            start => {erlmcp_flow_byzantine, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [erlmcp_flow_byzantine]
        },

        %% Failure Detector
        #{
            id => erlmcp_flow_failure_detector,
            start => {erlmcp_flow_failure_detector, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [erlmcp_flow_failure_detector]
        },

        %% Main Router (depends on all above)
        #{
            id => erlmcp_flow_router,
            start => {erlmcp_flow_router, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [erlmcp_flow_router]
        }
    ],

    {ok, {SupFlags, ChildSpecs}}.
