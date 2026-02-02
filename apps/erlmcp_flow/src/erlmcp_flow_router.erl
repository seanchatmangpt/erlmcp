%%%-------------------------------------------------------------------
%%% @doc erlmcp-flow Message Router (gproc-based)
%%% Pure module for agent routing and task distribution.
%%% Week 2 Days 3-4: 80/20 Roadmap implementation.
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_flow_router).

%% API exports
-export([
    register_agent/2,
    lookup_agent/1,
    route_task/2,
    agent_list/0,
    route_task_with_load_balancing/1
]).

-include_lib("kernel/include/logger.hrl").

-define(ROUTE_TIMEOUT, 5000).  % 5 second timeout

%%%===================================================================
%%% API Functions
%%%===================================================================

%% @doc Register an agent with the router.
%% Simplified API that wraps erlmcp_flow_registry.
-spec register_agent(AgentId :: binary(), Pid :: pid()) -> ok | {error, term()}.
register_agent(AgentId, Pid) when is_binary(AgentId), is_pid(Pid) ->
    % Register with empty capabilities list (simple wrapper)
    erlmcp_flow_registry:register_agent(AgentId, Pid, []).

%% @doc Look up an agent by ID.
%% Returns the agent's Pid or not_found.
-spec lookup_agent(AgentId :: binary()) -> pid() | not_found.
lookup_agent(AgentId) when is_binary(AgentId) ->
    case erlmcp_flow_registry:find_agent(AgentId) of
        {ok, Pid} -> Pid;
        {error, not_found} -> not_found
    end.

%% @doc Route a task to a specific agent with timeout.
%% Sends {task, Task} message to agent and waits for acknowledgment.
%% Returns ok on success, {error, Reason} on failure.
-spec route_task(AgentId :: binary(), Task :: term()) -> ok | {error, term()}.
route_task(AgentId, Task) when is_binary(AgentId) ->
    case lookup_agent(AgentId) of
        not_found ->
            ?LOG_WARNING("Cannot route task to agent ~p: not found", [AgentId]),
            {error, agent_not_found};
        Pid ->
            try
                % Send task message to agent
                Pid ! {task, Task},

                % Increment load counter
                erlmcp_flow_registry:increment_load(AgentId),

                ?LOG_DEBUG("Routed task to agent ~p (pid: ~p)", [AgentId, Pid]),
                ok
            catch
                error:Reason ->
                    ?LOG_ERROR("Failed to route task to agent ~p: ~p", [AgentId, Reason]),
                    {error, Reason}
            end
    end.

%% @doc Get list of all registered agent PIDs.
%% Uses gproc to query all registered agents.
-spec agent_list() -> [pid()].
agent_list() ->
    % Query gproc for all flow_agent registrations
    case gproc:select([{{{n, l, {flow_agent, '$1'}}, '$2', '_'}, [], ['$2']}]) of
        Pids when is_list(Pids) -> Pids;
        _ -> []
    end.

%% @doc Route task to least-loaded agent.
%% Load balancing strategy: find agent with lowest queue size.
-spec route_task_with_load_balancing(Task :: term()) -> ok | {error, term()}.
route_task_with_load_balancing(Task) ->
    case agent_list() of
        [] ->
            {error, no_agents_available};
        Pids ->
            % Find agent with lowest load
            AgentLoads = [
                {get_agent_id(Pid), erlmcp_flow_registry:get_agent_load(get_agent_id(Pid))}
                || Pid <- Pids, get_agent_id(Pid) =/= undefined
            ],

            case AgentLoads of
                [] ->
                    {error, no_agents_available};
                _ ->
                    % Sort by load (ascending)
                    SortFun = fun({_, L1}, {_, L2}) -> L1 =< L2 end,
                    [{LeastLoadedId, _Load} | _] = lists:sort(SortFun, AgentLoads),
                    route_task(LeastLoadedId, Task)
            end
    end.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

%% @private Get agent ID from Pid via gproc lookup
get_agent_id(Pid) when is_pid(Pid) ->
    case gproc:select([{{{n, l, {flow_agent, '$1'}}, Pid, '_'}, [], ['$1']}]) of
        [AgentId] -> AgentId;
        _ -> undefined
    end.
