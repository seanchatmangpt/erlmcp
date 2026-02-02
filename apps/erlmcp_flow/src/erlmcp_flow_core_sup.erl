%%%-------------------------------------------------------------------
%%% @doc erlmcp-flow Core Supervisor (TIER 2)
%%% Manages swarm/agent supervisors and service workers
%%% Strategy: one_for_one (each subsystem fails independently)
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_flow_core_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

%%%===================================================================
%%% API
%%%===================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%%===================================================================
%%% Supervisor Callbacks
%%%===================================================================

-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    %% ================================================================
    %% TIER 2: Core Supervisor (one_for_one)
    %% Strategy: Each subsystem fails independently
    %% - Swarm supervisor failures don't affect agent supervisor
    %% - Service worker failures don't affect supervisors
    %% ================================================================
    SupFlags = #{
        strategy => one_for_one,    % Isolated failures
        intensity => 5,              % Max 5 restarts
        period => 60                 % Within 60 seconds
    },

    ChildSpecs = [
        %% ================================================================
        %% SWARM SUPERVISOR: Dynamic swarm instances (simple_one_for_one)
        %% Each swarm is a separate gen_server coordinating multiple agents
        %% Restart: permanent (swarm coordinator must survive crashes)
        %% ================================================================
        #{id => erlmcp_flow_swarm_sup,
          start => {erlmcp_flow_swarm_sup, start_link, []},
          restart => permanent,
          shutdown => infinity,       % Supervisor: wait for all children
          type => supervisor,
          modules => [erlmcp_flow_swarm_sup]},

        %% ================================================================
        %% AGENT SUPERVISOR: Dynamic agent instances (simple_one_for_one)
        %% Each agent is a separate gen_server handling requests
        %% Restart: permanent (supervisor must survive)
        %% ================================================================
        #{id => erlmcp_flow_agent_sup,
          start => {erlmcp_flow_agent_sup, start_link, []},
          restart => permanent,
          shutdown => infinity,       % Supervisor: wait for all children
          type => supervisor,
          modules => [erlmcp_flow_agent_sup]},

        %% ================================================================
        %% Q-LEARNING ENGINE: Adaptive agent selection
        %% Learns optimal agent routing patterns
        %% ================================================================
        #{id => erlmcp_flow_q_learning,
          start => {erlmcp_flow_q_learning, start_link, []},
          restart => permanent,
          shutdown => 5000,
          type => worker,
          modules => [erlmcp_flow_q_learning]},

        %% ================================================================
        %% CIRCUIT BREAKER: Failure protection
        %% Prevents cascading failures via circuit breaker pattern
        %% ================================================================
        #{id => erlmcp_flow_circuit_breaker,
          start => {erlmcp_flow_circuit_breaker, start_link, []},
          restart => permanent,
          shutdown => 5000,
          type => worker,
          modules => [erlmcp_flow_circuit_breaker]},

        %% ================================================================
        %% CORRELATION TRACKER: Request tracking
        %% Tracks request correlation across agents
        %% ================================================================
        #{id => erlmcp_flow_correlation_tracker,
          start => {erlmcp_flow_correlation_tracker, start_link, []},
          restart => permanent,
          shutdown => 5000,
          type => worker,
          modules => [erlmcp_flow_correlation_tracker]},

        %% ================================================================
        %% BYZANTINE DETECTOR: Malicious behavior detection
        %% Detects Byzantine failures in agent swarms
        %% ================================================================
        #{id => erlmcp_flow_byzantine,
          start => {erlmcp_flow_byzantine, start_link, []},
          restart => permanent,
          shutdown => 5000,
          type => worker,
          modules => [erlmcp_flow_byzantine]},

        %% ================================================================
        %% FAILURE DETECTOR: Heartbeat monitoring
        %% Monitors agent heartbeats and detects failures
        %% ================================================================
        #{id => erlmcp_flow_failure_detector,
          start => {erlmcp_flow_failure_detector, start_link, []},
          restart => permanent,
          shutdown => 5000,
          type => worker,
          modules => [erlmcp_flow_failure_detector]},

        %% ================================================================
        %% ROUTER: Message routing (depends on all above)
        %% Routes messages between agents using registry + circuit breaker
        %% ================================================================
        #{id => erlmcp_flow_router,
          start => {erlmcp_flow_router, start_link, []},
          restart => permanent,
          shutdown => 5000,
          type => worker,
          modules => [erlmcp_flow_router]}
    ],

    {ok, {SupFlags, ChildSpecs}}.
