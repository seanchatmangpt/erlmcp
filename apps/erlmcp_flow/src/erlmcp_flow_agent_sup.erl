%%%-------------------------------------------------------------------
%%% @doc erlmcp-flow Agent Supervisor (TIER 3)
%%% Dynamic agent instances (simple_one_for_one)
%%% Each agent is a gen_server handling individual tasks
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_flow_agent_sup).

-behaviour(supervisor).

-export([start_link/0, start_child/3]).
-export([init/1]).

%%%===================================================================
%%% API
%%%===================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @doc Start an agent with given role and configuration
-spec start_child(AgentId, Role, Config) -> {ok, pid()} | {error, term()}
    when AgentId :: binary(),
         Role :: binary(),
         Config :: map().
start_child(AgentId, Role, Config) ->
    supervisor:start_child(?MODULE, [AgentId, Role, Config]).

%%%===================================================================
%%% Supervisor Callbacks
%%%===================================================================

-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    %% ================================================================
    %% TIER 3: Agent Supervisor (simple_one_for_one)
    %% Strategy: Dynamic worker pool for agent instances
    %% - Each agent handles individual tasks
    %% - Agents are spawned on-demand
    %% - Restart: temporary (do NOT restart on exit - let parent swarm decide)
    %% - Rationale: Agent crashes trigger task requeue by parent swarm
    %% ================================================================
    SupFlags = #{
        strategy => simple_one_for_one,  % Dynamic agent instances
        intensity => 10,                  % Agents may crash more frequently (exploratory tasks)
        period => 60                      % Within 60 seconds
    },

    %% Template child spec for agent instances
    %% Args (AgentId, Role, Config) are appended by start_child/3
    ChildSpecs = [
        #{id => erlmcp_flow_agent,
          start => {erlmcp_flow_agent, start_link, []},
          restart => temporary,  % Do NOT restart on exit (let parent swarm decide)
          shutdown => 2000,       % 2s graceful shutdown (cancel pending operations)
          type => worker,
          modules => [erlmcp_flow_agent]}
    ],

    {ok, {SupFlags, ChildSpecs}}.
