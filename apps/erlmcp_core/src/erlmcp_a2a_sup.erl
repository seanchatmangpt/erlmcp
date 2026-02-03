%%%-------------------------------------------------------------------
%%% @doc A2A (Agent-to-Agent) Protocol Supervisor
%%%
%%% This supervisor manages all A2A protocol components:
%%% - Task Manager: Manages task lifecycle and state
%%% - Agent Card: Agent discovery and capabilities
%%% - Push: Push notification delivery
%%% - Streams: Dynamic streaming connections (simple_one_for_one)
%%%
%%% Strategy: one_for_one - each component fails independently
%%% Shutdown: Ordered - static workers first, then stream supervisor
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_a2a_sup).

-behaviour(supervisor).

-include("erlmcp_a2a.hrl").

%% API
-export([start_link/0, start_link/1]).
-export([start_stream/1]).

%% Supervisor callbacks
-export([init/1]).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Starts the A2A supervisor with default options.
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link(#{}).

%% @doc Starts the A2A supervisor with options.
%% Options:
%%   - agent_card => #a2a_agent_card{} - Initial agent card configuration
%%   - capabilities => #a2a_agent_capabilities{} - Agent capabilities
%%   - tenant => binary() - Multi-tenant identifier
-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Opts) when is_map(Opts) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Opts).

%% @doc Starts a new stream child dynamically.
%% StreamOpts must contain:
%%   - task_id => binary() - Task ID for the stream
%%   - subscriber => pid() - Subscriber process
%% Optional:
%%   - context_id => binary() - Context ID
%%   - metadata => map() - Stream metadata
-spec start_stream(map()) -> {ok, pid()} | {error, term()}.
start_stream(StreamOpts) when is_map(StreamOpts) ->
    supervisor:start_child(erlmcp_a2a_stream_sup, [StreamOpts]).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% @doc Initializes the A2A supervisor.
%%
%% Child startup order (shutdown is reverse):
%% 1. Task Manager - core task state (must start first)
%% 2. Agent Card - agent discovery and capabilities
%% 3. Push - push notification delivery
%% 4. Stream Supervisor - dynamic streams (simple_one_for_one)
%%
%% Shutdown order (reverse of startup):
%% 1. Stream Supervisor - close all streams first
%% 2. Push - stop push delivery
%% 3. Agent Card - stop discovery
%% 4. Task Manager - final cleanup
-spec init(map()) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init(Opts) ->
    SupFlags =
        #{strategy => one_for_one,  % Each component fails independently
          intensity => 5,           % 5 restarts
          period => 60},            % per 60 seconds

    %% Extract options for child processes
    AgentCardOpts = maps:get(agent_card_opts, Opts, #{}),
    TaskManagerOpts = maps:get(task_manager_opts, Opts, #{}),
    PushOpts = maps:get(push_opts, Opts, #{}),

    ChildSpecs = [
        %% ================================================================
        %% TASK MANAGER: Core task lifecycle and state management
        %% Must start first - other components depend on task state
        %% ================================================================
        #{id => erlmcp_a2a_task_manager,
          start => {erlmcp_a2a_task_manager, start_link, [TaskManagerOpts]},
          restart => permanent,
          shutdown => 5000,
          type => worker,
          modules => [erlmcp_a2a_task_manager]},

        %% ================================================================
        %% AGENT CARD: Agent discovery and capability advertisement
        %% Depends on task manager for capability checks
        %% ================================================================
        #{id => erlmcp_a2a_agent_card,
          start => {erlmcp_a2a_agent_card, start_link, [AgentCardOpts]},
          restart => permanent,
          shutdown => 5000,
          type => worker,
          modules => [erlmcp_a2a_agent_card]},

        %% ================================================================
        %% PUSH: Push notification delivery service
        %% Depends on task manager for task state
        %% ================================================================
        #{id => erlmcp_a2a_push,
          start => {erlmcp_a2a_push, start_link, [PushOpts]},
          restart => permanent,
          shutdown => 5000,
          type => worker,
          modules => [erlmcp_a2a_push]},

        %% ================================================================
        %% STREAM SUPERVISOR: Dynamic stream process management
        %% Uses simple_one_for_one for dynamic stream spawning
        %% Shutdown infinity - wait for all streams to close
        %% ================================================================
        #{id => erlmcp_a2a_stream_sup,
          start => {erlmcp_a2a_stream_sup, start_link, []},
          restart => permanent,
          shutdown => infinity,  % Wait for all streams
          type => supervisor,
          modules => [erlmcp_a2a_stream_sup]}
    ],

    {ok, {SupFlags, ChildSpecs}}.
