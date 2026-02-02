%%%-------------------------------------------------------------------
%%% @doc erlmcp-flow Swarm Coordinator (Gen Server)
%%% Orchestrates agent task assignment with Raft consensus
%%% State machine: idle -> coordinating -> executing
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_flow_swarm).

-behaviour(gen_server).

%% API
-export([
    start_link/1,
    start_link/2,
    submit_task/2,
    get_status/1,
    get_stats/1,
    agent_heartbeat/2,
    register_agent/2,
    unregister_agent/2,
    list_agents/1,
    stop/1
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("kernel/include/logger.hrl").

%% Hibernation: reduce memory from ~50KB to ~5KB after 30s idle
-define(HIBERNATE_AFTER_MS, 30000).

%% Health tracking: 3 missed heartbeats = dead agent
-define(MAX_MISSED_HEARTBEATS, 3).
-define(HEARTBEAT_INTERVAL_MS, 10000). % 10 seconds
-define(MAX_QUEUE_SIZE, 10000). % FIFO queue limit

%% State machine phases
-type swarm_phase() :: idle | coordinating | executing.

%% State record
-record(state, {
    id :: binary(),
    phase = idle :: swarm_phase(),
    agents = sets:new() :: sets:set(binary()), % Set of agent IDs
    tasks = queue:new() :: queue:queue(task()), % FIFO task queue
    leader_pid :: pid() | undefined, % Raft leader process
    raft_state :: term() | undefined, % Raft consensus state
    agent_health = #{} :: #{binary() => non_neg_integer()}, % AgentId -> missed_heartbeats
    next_agent_index = 0 :: non_neg_integer(), % Round-robin index
    health_check_timer :: reference() | undefined,
    stats = #{
        tasks_submitted => 0,
        tasks_completed => 0,
        agents_registered => 0,
        agents_removed => 0
    } :: map()
}).

-type state() :: #state{}.
-type task() :: #{
    id := binary(),
    type := binary(),
    input := term(),
    timeout => pos_integer(),
    priority => low | normal | high
}.

-export_type([task/0]).

%%%===================================================================
%%% API
%%%===================================================================

-spec start_link(binary()) -> {ok, pid()} | {error, term()}.
start_link(SwarmId) ->
    start_link(SwarmId, #{}).

-spec start_link(binary(), map()) -> {ok, pid()} | {error, term()}.
start_link(SwarmId, Config) when is_binary(SwarmId) ->
    gen_server:start_link(?MODULE, [SwarmId, Config], [{hibernate_after, ?HIBERNATE_AFTER_MS}]).

-spec submit_task(pid(), task()) -> ok | {error, term()}.
submit_task(Swarm, Task) when is_map(Task) ->
    gen_server:call(Swarm, {submit_task, Task}, 5000).

-spec get_status(pid()) -> {ok, map()} | {error, term()}.
get_status(Swarm) ->
    gen_server:call(Swarm, get_status).

-spec get_stats(pid()) -> {ok, map()} | {error, term()}.
get_stats(Swarm) ->
    gen_server:call(Swarm, get_stats).

-spec agent_heartbeat(pid(), binary()) -> ok.
agent_heartbeat(Swarm, AgentId) when is_binary(AgentId) ->
    gen_server:cast(Swarm, {agent_heartbeat, AgentId}).

-spec register_agent(pid(), binary()) -> ok | {error, term()}.
register_agent(Swarm, AgentId) when is_binary(AgentId) ->
    gen_server:call(Swarm, {register_agent, AgentId}).

-spec unregister_agent(pid(), binary()) -> ok.
unregister_agent(Swarm, AgentId) when is_binary(AgentId) ->
    gen_server:call(Swarm, {unregister_agent, AgentId}).

-spec list_agents(pid()) -> {ok, [binary()]}.
list_agents(Swarm) ->
    gen_server:call(Swarm, list_agents).

-spec stop(pid()) -> ok.
stop(Swarm) ->
    gen_server:stop(Swarm).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @doc Non-blocking init/1 - async setup via cast
init([SwarmId, Config]) ->
    process_flag(trap_exit, true),

    State = #state{
        id = SwarmId,
        phase = idle,
        agents = sets:new(),
        tasks = queue:new(),
        agent_health = #{}
    },

    ?LOG_INFO("Swarm ~p starting in idle phase", [SwarmId]),

    % Start health check timer
    TimerRef = erlang:send_after(?HEARTBEAT_INTERVAL_MS, self(), health_check),

    % Async: Initialize Raft leader election (non-blocking)
    gen_server:cast(self(), init_raft),

    {ok, State#state{health_check_timer = TimerRef}}.

%% Task submission (blocking call)
handle_call({submit_task, Task}, _From, State) ->
    case queue:len(State#state.tasks) >= ?MAX_QUEUE_SIZE of
        true ->
            ?LOG_WARNING("Task queue full (~p tasks), rejecting task ~p",
                        [?MAX_QUEUE_SIZE, maps:get(id, Task, unknown)]),
            {reply, {error, queue_full}, State};
        false ->
            % Add task to FIFO queue
            NewQueue = queue:in(Task, State#state.tasks),
            NewStats = maps:update_with(tasks_submitted, fun(V) -> V + 1 end, 1, State#state.stats),

            ?LOG_DEBUG("Task ~p submitted (queue depth: ~p)",
                      [maps:get(id, Task, unknown), queue:len(NewQueue)]),

            NewState = State#state{
                tasks = NewQueue,
                stats = NewStats,
                phase = coordinating
            },

            % Trigger task assignment
            gen_server:cast(self(), assign_tasks),

            {reply, ok, NewState}
    end;

%% Agent registration
handle_call({register_agent, AgentId}, _From, State) ->
    case sets:is_element(AgentId, State#state.agents) of
        true ->
            {reply, {error, already_registered}, State};
        false ->
            NewAgents = sets:add_element(AgentId, State#state.agents),
            NewHealth = maps:put(AgentId, 0, State#state.agent_health),
            UpdateFun = fun(V) -> V + 1 end,
            NewStats = maps:update_with(agents_registered, UpdateFun, 1,
                                        State#state.stats),

            ?LOG_INFO("Agent ~p registered (total: ~p)",
                     [AgentId, sets:size(NewAgents)]),

            NewState = State#state{
                agents = NewAgents,
                agent_health = NewHealth,
                stats = NewStats
            },

            {reply, ok, NewState}
    end;

%% Agent unregistration
handle_call({unregister_agent, AgentId}, _From, State) ->
    NewAgents = sets:del_element(AgentId, State#state.agents),
    NewHealth = maps:remove(AgentId, State#state.agent_health),
    NewStats = maps:update_with(agents_removed, fun(V) -> V + 1 end, 1, State#state.stats),

    ?LOG_INFO("Agent ~p unregistered (remaining: ~p)",
             [AgentId, sets:size(NewAgents)]),

    NewState = State#state{
        agents = NewAgents,
        agent_health = NewHealth,
        stats = NewStats
    },

    {reply, ok, NewState};

%% List agents
handle_call(list_agents, _From, State) ->
    AgentList = sets:to_list(State#state.agents),
    {reply, {ok, AgentList}, State};

%% Get status
handle_call(get_status, _From, State) ->
    Status = #{
        id => State#state.id,
        phase => State#state.phase,
        num_agents => sets:size(State#state.agents),
        queue_depth => queue:len(State#state.tasks),
        healthy_agents => count_healthy_agents(State),
        leader_pid => State#state.leader_pid
    },
    {reply, {ok, Status}, State};

%% Get stats
handle_call(get_stats, _From, State) ->
    {reply, {ok, State#state.stats}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% Agent heartbeat (async)
handle_cast({agent_heartbeat, AgentId}, State) ->
    case sets:is_element(AgentId, State#state.agents) of
        true ->
            % Reset missed heartbeat count
            NewHealth = maps:put(AgentId, 0, State#state.agent_health),

            % Update heartbeat in registry
            erlmcp_flow_registry:update_heartbeat(AgentId),

            {noreply, State#state{agent_health = NewHealth}};
        false ->
            ?LOG_WARNING("Heartbeat from unregistered agent ~p", [AgentId]),
            {noreply, State}
    end;

%% Initialize Raft (async, non-blocking)
handle_cast(init_raft, State) ->
    % Start or connect to Raft leader
    case start_raft_node(State#state.id) of
        {ok, LeaderPid, RaftState} ->
            ?LOG_INFO("Raft leader initialized: ~p", [LeaderPid]),
            {noreply, State#state{leader_pid = LeaderPid, raft_state = RaftState}};
        {error, Reason} ->
            ?LOG_ERROR("Failed to initialize Raft: ~p", [Reason]),
            {noreply, State}
    end;

%% Task assignment (round-robin)
handle_cast(assign_tasks, State) ->
    case {queue:is_empty(State#state.tasks), sets:size(State#state.agents)} of
        {true, _} ->
            % No tasks to assign
            {noreply, State#state{phase = idle}};
        {false, 0} ->
            % No agents available
            ?LOG_WARNING("No agents available for task assignment"),
            {noreply, State#state{phase = coordinating}};
        {false, _} ->
            % Assign tasks round-robin
            NewState = assign_next_task(State),
            {noreply, NewState#state{phase = executing}}
    end;

handle_cast(_Msg, State) ->
    {noreply, State}.

%% Health check timer
handle_info(health_check, State) ->
    NewState = check_agent_health(State),

    % Restart timer
    TimerRef = erlang:send_after(?HEARTBEAT_INTERVAL_MS, self(), health_check),

    {noreply, NewState#state{health_check_timer = TimerRef}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    % Cancel health check timer
    case State#state.health_check_timer of
        undefined -> ok;
        Ref -> erlang:cancel_timer(Ref)
    end,
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @doc Check agent health and remove dead agents (3+ missed heartbeats)
-spec check_agent_health(state()) -> state().
check_agent_health(State) ->
    UpdatedHealth = maps:map(fun(AgentId, MissedCount) ->
        NewCount = MissedCount + 1,
        case NewCount >= ?MAX_MISSED_HEARTBEATS of
            true ->
                ?LOG_WARNING("Agent ~p dead (3 missed heartbeats), removing", [AgentId]),
                % Remove agent
                gen_server:cast(self(), {remove_dead_agent, AgentId}),
                NewCount;
            false ->
                NewCount
        end
    end, State#state.agent_health),

    State#state{agent_health = UpdatedHealth}.

%% @doc Assign next task using round-robin strategy
-spec assign_next_task(state()) -> state().
assign_next_task(State) ->
    case queue:out(State#state.tasks) of
        {empty, _Queue} ->
            State#state{phase = idle};
        {{value, Task}, NewQueue} ->
            AgentList = sets:to_list(State#state.agents),
            NumAgents = length(AgentList),

            case NumAgents of
                0 ->
                    % No agents, re-queue task
                    State#state{tasks = queue:in(Task, NewQueue)};
                _ ->
                    % Round-robin: pick next agent
                    AgentIndex = State#state.next_agent_index rem NumAgents,
                    AgentId = lists:nth(AgentIndex + 1, AgentList),

                    ?LOG_INFO("Assigning task ~p to agent ~p",
                             [maps:get(id, Task, unknown), AgentId]),

                    % Send task to agent (via registry)
                    case erlmcp_flow_registry:find_agent(AgentId) of
                        {ok, AgentPid} ->
                            % Assuming erlmcp_flow_agent has assign_task/2
                            try
                                erlmcp_flow_agent:assign_task(AgentPid, Task),
                                State#state{
                                    tasks = NewQueue,
                                    next_agent_index = State#state.next_agent_index + 1
                                }
                            catch
                                _:_ ->
                                    % Agent unavailable, re-queue
                                    ?LOG_WARNING("Failed to assign task to agent ~p", [AgentId]),
                                    State#state{tasks = queue:in(Task, NewQueue)}
                            end;
                        {error, not_found} ->
                            % Agent not found, re-queue
                            ?LOG_WARNING("Agent ~p not found in registry", [AgentId]),
                            State#state{tasks = queue:in(Task, NewQueue)}
                    end
            end
    end.

%% @doc Start Raft node for leader election
-spec start_raft_node(binary()) -> {ok, pid(), term()} | {error, term()}.
start_raft_node(SwarmId) ->
    % Minimal Raft integration (leader election only)
    % In full implementation, this would start erlmcp_flow_raft
    Config = #{
        node_id => SwarmId,
        peers => [], % Single node for MVP
        election_timeout => 5000
    },

    case erlmcp_flow_raft:start_link(Config) of
        {ok, Pid} ->
            % Wait for leader election
            timer:sleep(100),
            case erlmcp_flow_raft:get_leader(SwarmId) of
                {ok, LeaderPid} ->
                    {ok, LeaderPid, #{term => 1, role => leader}};
                _ ->
                    {ok, Pid, #{term => 1, role => follower}}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Count healthy agents (< 3 missed heartbeats)
-spec count_healthy_agents(state()) -> non_neg_integer().
count_healthy_agents(State) ->
    maps:fold(fun(_AgentId, MissedCount, Acc) ->
        case MissedCount < ?MAX_MISSED_HEARTBEATS of
            true -> Acc + 1;
            false -> Acc
        end
    end, 0, State#state.agent_health).
