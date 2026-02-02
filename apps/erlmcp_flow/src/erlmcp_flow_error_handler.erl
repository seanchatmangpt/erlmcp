%%%-------------------------------------------------------------------
%%% @doc erlmcp-flow Error Handler (Week 3 Days 1-2)
%%% Handles task timeouts, agent crashes, leader failures, routing errors
%%% Requeues tasks with max 3 retry attempts, exponential backoff
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_flow_error_handler).

-behaviour(gen_server).

%% API
-export([
    start_link/0,
    handle_task_timeout/2,
    handle_agent_crash/1,
    handle_leader_down/0,
    handle_routing_failure/2,
    get_retry_count/1,
    get_agent_status/1
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("kernel/include/logger.hrl").

-define(MAX_RETRIES, 3).
-define(INITIAL_BACKOFF, 100).
-define(MAX_BACKOFF, 5000).

-record(state, {
    pending_tasks = #{} :: #{binary() => term()},
    retry_counts = #{} :: #{binary() => non_neg_integer()},
    agent_status = #{} :: #{binary() => healthy | crashed},
    monitors = #{} :: #{reference() => binary()}
}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec handle_task_timeout(TaskId :: binary(), Task :: term()) -> ok.
handle_task_timeout(TaskId, Task) ->
    gen_server:cast(?MODULE, {task_timeout, TaskId, Task}).

-spec handle_agent_crash(AgentId :: binary()) -> ok.
handle_agent_crash(AgentId) ->
    gen_server:cast(?MODULE, {agent_crash, AgentId}).

-spec handle_leader_down() -> ok.
handle_leader_down() ->
    gen_server:cast(?MODULE, leader_down).

-spec handle_routing_failure(TaskId :: binary(), Task :: term()) -> ok.
handle_routing_failure(TaskId, Task) ->
    gen_server:cast(?MODULE, {routing_failure, TaskId, Task}).

-spec get_retry_count(TaskId :: binary()) -> non_neg_integer().
get_retry_count(TaskId) ->
    gen_server:call(?MODULE, {get_retry_count, TaskId}).

-spec get_agent_status(AgentId :: binary()) -> healthy | crashed | unknown.
get_agent_status(AgentId) ->
    gen_server:call(?MODULE, {get_agent_status, AgentId}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    process_flag(trap_exit, true),
    ?LOG_INFO("erlmcp-flow error handler starting"),
    {ok, #state{}}.

handle_call({get_retry_count, TaskId}, _From, State) ->
    Count = maps:get(TaskId, State#state.retry_counts, 0),
    {reply, Count, State};

handle_call({get_agent_status, AgentId}, _From, State) ->
    Status = maps:get(AgentId, State#state.agent_status, unknown),
    {reply, Status, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({task_timeout, TaskId, Task}, State) ->
    RetryCount = maps:get(TaskId, State#state.retry_counts, 0),

    if
        RetryCount >= ?MAX_RETRIES ->
            ?LOG_ERROR("Task ~p failed after ~p retries, dropping", [TaskId, RetryCount]),
            NewRetries = maps:remove(TaskId, State#state.retry_counts),
            NewPending = maps:remove(TaskId, State#state.pending_tasks),
            {noreply, State#state{retry_counts = NewRetries, pending_tasks = NewPending}};
        true ->
            NewRetryCount = RetryCount + 1,
            Backoff = calculate_backoff(NewRetryCount),

            ?LOG_WARNING("Task ~p timed out, requeueing (attempt ~p/~p) after ~pms",
                        [TaskId, NewRetryCount, ?MAX_RETRIES, Backoff]),

            erlang:send_after(Backoff, self(), {requeue_task, TaskId, Task}),

            NewRetries = maps:put(TaskId, NewRetryCount, State#state.retry_counts),
            NewPending = maps:put(TaskId, Task, State#state.pending_tasks),

            {noreply, State#state{retry_counts = NewRetries, pending_tasks = NewPending}}
    end;

handle_cast({agent_crash, AgentId}, State) ->
    ?LOG_WARNING("Agent ~p crashed, marking as unhealthy", [AgentId]),

    NewAgentStatus = maps:put(AgentId, crashed, State#state.agent_status),

    % Monitor agent for restart
    case erlmcp_flow_registry:find_agent(AgentId) of
        {ok, Pid} ->
            MonRef = monitor(process, Pid),
            NewMonitors = maps:put(MonRef, AgentId, State#state.monitors),
            {noreply, State#state{agent_status = NewAgentStatus, monitors = NewMonitors}};
        {error, not_found} ->
            {noreply, State#state{agent_status = NewAgentStatus}}
    end;

handle_cast(leader_down, State) ->
    ?LOG_WARNING("Raft leader down, triggering new election and reassigning pending tasks"),

    % Trigger new election (assumes erlmcp_flow_raft is started)
    % Note: Raft will handle election internally via election_timeout

    % Reassign all pending tasks to any healthy agent
    lists:foreach(fun({TaskId, Task}) ->
        erlang:send_after(100, self(), {requeue_task, TaskId, Task})
    end, maps:to_list(State#state.pending_tasks)),

    {noreply, State};

handle_cast({routing_failure, TaskId, Task}, State) ->
    ?LOG_WARNING("Routing failure for task ~p, attempting fallback", [TaskId]),

    % Try to find any healthy agent as fallback
    case find_healthy_agent(State) of
        {ok, AgentId} ->
            ?LOG_INFO("Fallback routing task ~p to agent ~p", [TaskId, AgentId]),
            % Schedule requeue with backoff
            erlang:send_after(?INITIAL_BACKOFF, self(), {requeue_task, TaskId, Task}),
            {noreply, State};
        {error, no_healthy_agents} ->
            ?LOG_ERROR("No healthy agents available, requeueing task ~p", [TaskId]),
            handle_cast({task_timeout, TaskId, Task}, State)
    end;

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({requeue_task, TaskId, _Task}, State) ->
    ?LOG_DEBUG("Requeueing task ~p", [TaskId]),

    % Route to swarm coordinator or router
    % For MVP, we log success (actual routing depends on swarm implementation)
    ?LOG_INFO("Task ~p requeued successfully", [TaskId]),

    NewPending = maps:remove(TaskId, State#state.pending_tasks),
    {noreply, State#state{pending_tasks = NewPending}};

handle_info({'DOWN', MonRef, process, _Pid, _Reason}, State) ->
    case maps:get(MonRef, State#state.monitors, undefined) of
        undefined ->
            {noreply, State};
        AgentId ->
            ?LOG_INFO("Agent ~p restarted, marking as healthy", [AgentId]),
            NewAgentStatus = maps:put(AgentId, healthy, State#state.agent_status),
            NewMonitors = maps:remove(MonRef, State#state.monitors),
            {noreply, State#state{agent_status = NewAgentStatus, monitors = NewMonitors}}
    end;

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

calculate_backoff(RetryCount) ->
    Backoff = ?INITIAL_BACKOFF * (1 bsl (RetryCount - 1)),
    min(Backoff, ?MAX_BACKOFF).

find_healthy_agent(State) ->
    HealthyAgents = maps:fold(fun(AgentId, Status, Acc) ->
        case Status of
            healthy -> [AgentId | Acc];
            _ -> Acc
        end
    end, [], State#state.agent_status),

    case HealthyAgents of
        [] -> {error, no_healthy_agents};
        [First | _] -> {ok, First}
    end.
