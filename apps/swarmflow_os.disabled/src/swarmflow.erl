%%%-------------------------------------------------------------------
%%% @doc SwarmFlow OS Main API Facade
%%%
%%% Public interface for SwarmFlow OS - the autonomic workflow runtime
%%% powered by Petri net / YAWL semantics with process-mining swarm.
%%%
%%% This module provides a clean, unified API for:
%%% - Workflow net management (register, get, list, delete)
%%% - Case lifecycle (create, start, fire, suspend, resume, cancel, compensate)
%%% - State queries (marking, enabled transitions, variables)
%%% - Event log access
%%% - Process mining (conformance checking, replay)
%%% - Swarm management (workers, tasks, patches)
%%% - A2A/MCP integration bindings
%%% - Promotion policy management
%%%
%%% Architecture:
%%% - Thin facade delegating to specialized subsystem modules
%%% - Each workflow case runs as a supervised gen_statem process
%%% - Append-only event log enables replay and conformance checking
%%% - Process-mining swarm proposes improvements with policy-based promotion
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(swarmflow).

-include("swarmflow.hrl").

%% Version API
-export([version/0]).

%%====================================================================
%% Net Management API
%%====================================================================
-export([
    register_net/1,
    get_net/1,
    get_net/2,
    list_nets/0,
    delete_net/1
]).

%%====================================================================
%% Case Management API
%%====================================================================
-export([
    create_case/2,
    create_case/3,
    start_case/1,
    get_case/1,
    list_cases/0,
    list_cases/1,
    fire/2,
    suspend/1,
    resume/1,
    cancel/1,
    cancel/2,
    compensate/1
]).

%%====================================================================
%% Query API
%%====================================================================
-export([
    get_marking/1,
    get_enabled/1,
    get_variables/1,
    get_events/1,
    get_events/2
]).

%%====================================================================
%% Process Mining API
%%====================================================================
-export([
    check_conformance/1,
    check_conformance/2,
    replay/1,
    replay/2,
    calculate_fitness/2,
    find_deviations/2
]).

%%====================================================================
%% Swarm API
%%====================================================================
-export([
    get_swarm_status/0,
    propose_patches/1,
    get_workers/0,
    get_workers/1,
    spawn_worker/1,
    spawn_worker/2,
    terminate_worker/1,
    submit_swarm_task/2,
    submit_swarm_task/3,
    collect_proposals/1,
    collect_proposals/2
]).

%%====================================================================
%% Integration API (A2A/MCP)
%%====================================================================
-export([
    bind_a2a_task/2,
    bind_mcp_tool/2,
    unbind_a2a_task/1,
    unbind_mcp_tool/2
]).

%%====================================================================
%% Promotion Policy API
%%====================================================================
-export([
    register_policy/1,
    get_policy/1,
    list_policies/0,
    update_policy/2,
    delete_policy/1,
    evaluate_patch/2,
    promote_patch/1,
    rollback_patch/1
]).

%%====================================================================
%% Types
%%====================================================================

-type case_id() :: binary().
-type net_id() :: binary().
-type transition_id() :: binary().
-type policy_id() :: binary().
-type worker_id() :: binary().
-type a2a_task_id() :: binary().
-type mcp_tool_name() :: binary().
-type case_ref() :: pid() | case_id().
-type event_filter() :: #{
    from_seq => non_neg_integer(),
    to_seq => non_neg_integer() | infinity,
    limit => pos_integer(),
    event_types => [event_type()] | all
}.

-export_type([
    case_id/0,
    net_id/0,
    transition_id/0,
    policy_id/0,
    worker_id/0,
    case_ref/0,
    event_filter/0
]).

%%====================================================================
%% Version
%%====================================================================

%% @doc Get SwarmFlow OS version information.
-spec version() -> #{version => binary(), protocol => binary()}.
version() ->
    #{
        version => ?SWARMFLOW_VERSION,
        protocol => ?SWARMFLOW_PROTOCOL_VERSION
    }.

%%====================================================================
%% Net Management Implementation
%%====================================================================

%% @doc Register a new workflow net definition.
%% The net is validated and compiled before storage.
%% Returns the registered net with any auto-generated fields.
-spec register_net(#swf_net{}) -> {ok, #swf_net{}} | {error, term()}.
register_net(#swf_net{} = Net) ->
    swf_net_registry:register_net(Net).

%% @doc Get a workflow net by ID.
%% Returns the currently active version.
-spec get_net(net_id()) -> {ok, #swf_net{}} | {error, not_found}.
get_net(NetId) when is_binary(NetId) ->
    swf_net_registry:get_net(NetId).

%% @doc Get a specific version of a workflow net.
-spec get_net(net_id(), binary()) -> {ok, #swf_net{}} | {error, not_found}.
get_net(NetId, Version) when is_binary(NetId), is_binary(Version) ->
    swf_net_registry:get_net(NetId, Version).

%% @doc List all registered workflow net IDs.
-spec list_nets() -> [net_id()].
list_nets() ->
    swf_net_registry:list_nets().

%% @doc Delete a workflow net and all its versions.
-spec delete_net(net_id()) -> ok | {error, not_found}.
delete_net(NetId) when is_binary(NetId) ->
    swf_net_registry:delete_net(NetId).

%%====================================================================
%% Case Management Implementation
%%====================================================================

%% @doc Create a new workflow case for a net with initial variables.
%% The case is created in 'created' state and must be started with start_case/1
%% or by firing a transition.
-spec create_case(net_id(), map()) -> {ok, case_id(), pid()} | {error, term()}.
create_case(NetId, InitialVars) when is_binary(NetId), is_map(InitialVars) ->
    create_case(NetId, InitialVars, #{}).

%% @doc Create a new workflow case with additional options.
%% Options:
%%   parent_case_id - Parent case for sub-workflows
%%   deadline - Unix timestamp ms for case deadline
%%   priority - Case priority (higher = more priority)
%%   tenant_id - Tenant identifier for multi-tenancy
%%   context_id - A2A context reference
-spec create_case(net_id(), map(), map()) -> {ok, case_id(), pid()} | {error, term()}.
create_case(NetId, InitialVars, Options) when is_binary(NetId), is_map(InitialVars), is_map(Options) ->
    case swf_case:start_link(NetId, InitialVars, Options) of
        {ok, Pid} ->
            {ok, CaseRecord} = swf_case:get_case_record(Pid),
            {ok, CaseRecord#swf_case.id, Pid};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Start executing a created case.
%% This transitions the case from 'created' to 'running' state.
%% Equivalent to firing an enabled automatic transition.
-spec start_case(case_ref()) -> ok | {error, term()}.
start_case(CaseRef) ->
    Pid = resolve_case_ref(CaseRef),
    case swf_case:get_enabled_transitions(Pid) of
        {ok, []} ->
            {error, no_enabled_transitions};
        {ok, [FirstEnabled | _]} ->
            case swf_case:fire_transition(Pid, FirstEnabled) of
                {ok, _Marking} -> ok;
                {error, Reason} -> {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Get case information.
-spec get_case(case_ref()) -> {ok, #swf_case{}} | {error, not_found}.
get_case(CaseRef) ->
    try
        Pid = resolve_case_ref(CaseRef),
        swf_case:get_case_record(Pid)
    catch
        error:{case_not_found, _} -> {error, not_found}
    end.

%% @doc List all active cases.
%% Note: This scans all registered case processes.
-spec list_cases() -> [#swf_case{}].
list_cases() ->
    %% Implementation depends on case registry - using gproc if available
    case code:is_loaded(gproc) of
        false -> [];
        _ ->
            try
                Keys = gproc:select({l, n}, [{{{n, l, {swf_case, '_'}}, '_', '_'}, [], ['$_']}]),
                lists:filtermap(
                    fun({{n, l, {swf_case, _CaseId}}, Pid, _}) ->
                        case swf_case:get_case_record(Pid) of
                            {ok, Case} -> {true, Case};
                            _ -> false
                        end
                    end,
                    Keys
                )
            catch
                _:_ -> []
            end
    end.

%% @doc List cases for a specific net.
-spec list_cases(net_id()) -> [#swf_case{}].
list_cases(NetId) when is_binary(NetId) ->
    AllCases = list_cases(),
    [C || C <- AllCases, C#swf_case.net_id =:= NetId].

%% @doc Fire a transition on a case.
%% The transition must be enabled (sufficient tokens, guard satisfied).
%% Returns the new marking after firing.
-spec fire(case_ref(), transition_id()) ->
    {ok, #{binary() => non_neg_integer()}} | {error, term()}.
fire(CaseRef, TransitionId) when is_binary(TransitionId) ->
    Pid = resolve_case_ref(CaseRef),
    swf_case:fire_transition(Pid, TransitionId).

%% @doc Suspend a running case.
%% Pauses execution and timers. Can be resumed with resume/1.
-spec suspend(case_ref()) -> ok | {error, invalid_state}.
suspend(CaseRef) ->
    Pid = resolve_case_ref(CaseRef),
    swf_case:suspend(Pid).

%% @doc Resume a suspended case.
-spec resume(case_ref()) -> ok | {error, invalid_state}.
resume(CaseRef) ->
    Pid = resolve_case_ref(CaseRef),
    swf_case:resume(Pid).

%% @doc Cancel a case.
%% Terminates the case without compensation.
-spec cancel(case_ref()) -> ok | {error, invalid_state}.
cancel(CaseRef) ->
    Pid = resolve_case_ref(CaseRef),
    swf_case:cancel(Pid).

%% @doc Cancel a case with a reason.
-spec cancel(case_ref(), binary()) -> ok | {error, invalid_state}.
cancel(CaseRef, Reason) when is_binary(Reason) ->
    Pid = resolve_case_ref(CaseRef),
    swf_case:cancel(Pid, Reason).

%% @doc Start SAGA compensation for a case.
%% Executes compensation handlers for completed transitions in reverse order.
-spec compensate(case_ref()) -> ok | {error, term()}.
compensate(CaseRef) ->
    Pid = resolve_case_ref(CaseRef),
    swf_case:compensate(Pid).

%%====================================================================
%% Query Implementation
%%====================================================================

%% @doc Get the current marking (token distribution) for a case.
-spec get_marking(case_ref()) -> {ok, #{binary() => non_neg_integer()}}.
get_marking(CaseRef) ->
    Pid = resolve_case_ref(CaseRef),
    swf_case:get_marking(Pid).

%% @doc Get currently enabled transitions for a case.
%% These are transitions that can be fired given the current marking.
-spec get_enabled(case_ref()) -> {ok, [transition_id()]}.
get_enabled(CaseRef) ->
    Pid = resolve_case_ref(CaseRef),
    swf_case:get_enabled_transitions(Pid).

%% @doc Get case variables.
-spec get_variables(case_ref()) -> {ok, map()}.
get_variables(CaseRef) ->
    Pid = resolve_case_ref(CaseRef),
    swf_case:get_variables(Pid).

%% @doc Get all events for a case.
-spec get_events(case_id()) -> {ok, [#swf_event{}]} | {error, term()}.
get_events(CaseId) when is_binary(CaseId) ->
    get_events(CaseId, #{}).

%% @doc Get events for a case with filtering options.
%% Options:
%%   from_seq - Start sequence (inclusive)
%%   to_seq - End sequence (inclusive)
%%   limit - Maximum events to return
%%   event_types - Filter by event types
-spec get_events(case_id(), event_filter()) -> {ok, [#swf_event{}]} | {error, term()}.
get_events(CaseId, Opts) when is_binary(CaseId), is_map(Opts) ->
    swf_event_log:get_events(CaseId, Opts).

%%====================================================================
%% Process Mining Implementation
%%====================================================================

%% @doc Check conformance of a case against its workflow net.
%% Returns comprehensive metrics: fitness, precision, deviations.
-spec check_conformance(case_id()) -> {ok, #swf_conformance_result{}} | {error, term()}.
check_conformance(CaseId) when is_binary(CaseId) ->
    case get_case(CaseId) of
        {ok, Case} ->
            check_conformance(CaseId, Case#swf_case.net_id);
        {error, not_found} ->
            %% Case process may have terminated, try to get events
            case get_events(CaseId) of
                {ok, Events} when length(Events) > 0 ->
                    %% Extract net_id from first event
                    [FirstEvent | _] = Events,
                    NetId = maps:get(net_id, FirstEvent#swf_event.data, undefined),
                    case NetId of
                        undefined -> {error, net_id_not_found};
                        _ -> check_conformance_internal(Events, NetId)
                    end;
                {ok, []} ->
                    {error, no_events};
                {error, Reason} ->
                    {error, Reason}
            end
    end.

%% @doc Check conformance of a case against a specific net.
-spec check_conformance(case_id(), net_id()) -> {ok, #swf_conformance_result{}} | {error, term()}.
check_conformance(CaseId, NetId) when is_binary(CaseId), is_binary(NetId) ->
    case get_events(CaseId) of
        {ok, Events} when length(Events) > 0 ->
            check_conformance_internal(Events, NetId);
        {ok, []} ->
            {error, no_events};
        {error, Reason} ->
            {error, Reason}
    end.

%% @private Internal conformance checking
-spec check_conformance_internal([#swf_event{}], net_id()) ->
    {ok, #swf_conformance_result{}} | {error, term()}.
check_conformance_internal(Events, NetId) ->
    case get_net(NetId) of
        {ok, Net} ->
            swf_conformance:check_conformance(Events, Net);
        {error, not_found} ->
            {error, net_not_found}
    end.

%% @doc Replay case events through the workflow net.
%% Returns replay state with token flow and any deviations.
-spec replay(case_id()) -> {ok, map()} | {error, term()}.
replay(CaseId) when is_binary(CaseId) ->
    case get_case(CaseId) of
        {ok, Case} ->
            replay(CaseId, Case#swf_case.net_id);
        {error, _} ->
            {error, case_not_found}
    end.

%% @doc Replay case events against a specific net.
-spec replay(case_id(), net_id()) -> {ok, map()} | {error, term()}.
replay(CaseId, NetId) when is_binary(CaseId), is_binary(NetId) ->
    case {get_events(CaseId), get_net(NetId)} of
        {{ok, Events}, {ok, Net}} when length(Events) > 0 ->
            swf_conformance:replay_log(Events, Net);
        {{ok, []}, _} ->
            {error, no_events};
        {{error, Reason}, _} ->
            {error, Reason};
        {_, {error, not_found}} ->
            {error, net_not_found}
    end.

%% @doc Calculate fitness score for a case.
%% Fitness measures how well the log fits the model (0.0 to 1.0).
-spec calculate_fitness(case_id(), net_id()) -> {ok, float()} | {error, term()}.
calculate_fitness(CaseId, NetId) when is_binary(CaseId), is_binary(NetId) ->
    case {get_events(CaseId), get_net(NetId)} of
        {{ok, Events}, {ok, Net}} when length(Events) > 0 ->
            swf_conformance:calculate_fitness(Events, Net);
        {{ok, []}, _} ->
            {error, no_events};
        {{error, Reason}, _} ->
            {error, Reason};
        {_, {error, not_found}} ->
            {error, net_not_found}
    end.

%% @doc Find deviations between case behavior and workflow model.
-spec find_deviations(case_id(), net_id()) -> {ok, [#swf_deviation{}]} | {error, term()}.
find_deviations(CaseId, NetId) when is_binary(CaseId), is_binary(NetId) ->
    case {get_events(CaseId), get_net(NetId)} of
        {{ok, Events}, {ok, Net}} when length(Events) > 0 ->
            swf_conformance:find_deviations(Events, Net);
        {{ok, []}, _} ->
            {error, no_events};
        {{error, Reason}, _} ->
            {error, Reason};
        {_, {error, not_found}} ->
            {error, net_not_found}
    end.

%%====================================================================
%% Swarm Implementation
%%====================================================================

%% @doc Get status of the process-mining swarm.
%% Returns worker counts, pending tasks, and performance metrics.
-spec get_swarm_status() -> map().
get_swarm_status() ->
    case erlang:whereis(swf_swarm_coordinator) of
        undefined ->
            #{status => not_running, workers => [], stats => #{}};
        _Pid ->
            Stats = swf_swarm_coordinator:get_stats(),
            Workers = swf_swarm_coordinator:get_workers(),
            #{
                status => running,
                workers => length(Workers),
                worker_types => count_worker_types(Workers),
                stats => Stats
            }
    end.

%% @private Count workers by type
-spec count_worker_types([#swf_swarm_worker{}]) -> #{swarm_worker_type() => non_neg_integer()}.
count_worker_types(Workers) ->
    lists:foldl(
        fun(#swf_swarm_worker{type = Type}, Acc) ->
            maps:update_with(Type, fun(C) -> C + 1 end, 1, Acc)
        end,
        #{},
        Workers
    ).

%% @doc Request patch proposals for a workflow net.
%% Triggers the swarm to analyze the net and propose improvements.
-spec propose_patches(net_id()) -> {ok, [#swf_patch{}]} | {error, term()}.
propose_patches(NetId) when is_binary(NetId) ->
    case erlang:whereis(swf_swarm_coordinator) of
        undefined ->
            {error, swarm_not_running};
        _Pid ->
            %% Submit task to patch_proposer workers
            case get_net(NetId) of
                {ok, Net} ->
                    %% Get recent events for analysis
                    %% In production, would aggregate across cases
                    Payload = #{net => Net, deviations => []},
                    case swf_swarm_coordinator:submit_task(patch_proposer, Payload, #{sync => true, timeout_ms => 30000}) of
                        #{status := completed, result := {ok, Patches}} ->
                            {ok, Patches};
                        #{status := failed, error := Error} ->
                            {error, Error};
                        {error, Reason} ->
                            {error, Reason}
                    end;
                {error, not_found} ->
                    {error, net_not_found}
            end
    end.

%% @doc Get all swarm workers.
-spec get_workers() -> [#swf_swarm_worker{}].
get_workers() ->
    case erlang:whereis(swf_swarm_coordinator) of
        undefined -> [];
        _Pid -> swf_swarm_coordinator:get_workers()
    end.

%% @doc Get swarm workers of a specific type.
-spec get_workers(swarm_worker_type()) -> [#swf_swarm_worker{}].
get_workers(Type) when is_atom(Type) ->
    case erlang:whereis(swf_swarm_coordinator) of
        undefined -> [];
        _Pid -> swf_swarm_coordinator:get_workers(Type)
    end.

%% @doc Spawn a new swarm worker.
-spec spawn_worker(swarm_worker_type()) -> {ok, worker_id()} | {error, term()}.
spawn_worker(Type) when is_atom(Type) ->
    case erlang:whereis(swf_swarm_coordinator) of
        undefined -> {error, swarm_not_running};
        _Pid -> swf_swarm_coordinator:spawn_worker(Type)
    end.

%% @doc Spawn a new swarm worker with configuration.
-spec spawn_worker(swarm_worker_type(), map()) -> {ok, worker_id()} | {error, term()}.
spawn_worker(Type, Config) when is_atom(Type), is_map(Config) ->
    case erlang:whereis(swf_swarm_coordinator) of
        undefined -> {error, swarm_not_running};
        _Pid -> swf_swarm_coordinator:spawn_worker(Type, Config)
    end.

%% @doc Terminate a swarm worker.
-spec terminate_worker(worker_id()) -> ok | {error, term()}.
terminate_worker(WorkerId) when is_binary(WorkerId) ->
    case erlang:whereis(swf_swarm_coordinator) of
        undefined -> {error, swarm_not_running};
        _Pid -> swf_swarm_coordinator:terminate_worker(WorkerId)
    end.

%% @doc Submit a task to the swarm.
-spec submit_swarm_task(swarm_worker_type(), term()) -> {ok, binary()} | {error, term()}.
submit_swarm_task(Type, Payload) when is_atom(Type) ->
    submit_swarm_task(Type, Payload, #{}).

%% @doc Submit a task to the swarm with options.
-spec submit_swarm_task(swarm_worker_type(), term(), map()) ->
    {ok, binary()} | {ok, binary(), term()} | {error, term()}.
submit_swarm_task(Type, Payload, Opts) when is_atom(Type), is_map(Opts) ->
    case erlang:whereis(swf_swarm_coordinator) of
        undefined -> {error, swarm_not_running};
        _Pid -> swf_swarm_coordinator:submit_task(Type, Payload, Opts)
    end.

%% @doc Collect patch proposals for a net.
-spec collect_proposals(net_id()) -> {ok, [#swf_patch{}]}.
collect_proposals(NetId) when is_binary(NetId) ->
    collect_proposals(NetId, #{}).

%% @doc Collect patch proposals with filtering/ranking options.
%% Options:
%%   min_confidence - Minimum confidence threshold
%%   max_risk - Maximum risk score
%%   rank_by - Ranking strategy (confidence | improvement | risk | composite)
%%   limit - Maximum proposals to return
-spec collect_proposals(net_id(), map()) -> {ok, [#swf_patch{}]}.
collect_proposals(NetId, Opts) when is_binary(NetId), is_map(Opts) ->
    case erlang:whereis(swf_swarm_coordinator) of
        undefined -> {ok, []};
        _Pid -> swf_swarm_coordinator:collect_proposals(NetId, Opts)
    end.

%%====================================================================
%% Integration Implementation (A2A/MCP)
%%====================================================================

%% @doc Bind a workflow case to an A2A task.
%% Enables bidirectional synchronization between case state and A2A task state.
-spec bind_a2a_task(case_id(), a2a_task_id()) -> ok | {error, term()}.
bind_a2a_task(CaseId, TaskId) when is_binary(CaseId), is_binary(TaskId) ->
    case get_case(CaseId) of
        {ok, _Case} ->
            %% Store binding in case metadata
            %% In production, this would update the case record and
            %% register with the A2A protocol handler
            Binding = #swf_a2a_binding{
                case_id = CaseId,
                task_id = TaskId,
                context_id = undefined,
                sync_mode = bidirectional,
                status_mapping = default_a2a_status_mapping()
            },
            store_a2a_binding(Binding);
        {error, not_found} ->
            {error, case_not_found}
    end.

%% @doc Bind a workflow transition to an MCP tool.
%% When the transition fires, it invokes the MCP tool.
-spec bind_mcp_tool(transition_id(), mcp_tool_name()) -> ok | {error, term()}.
bind_mcp_tool(TransitionId, ToolName) when is_binary(TransitionId), is_binary(ToolName) ->
    %% Store the tool binding
    %% In production, this would register with the MCP client
    Binding = #swf_tool_binding{
        transition_id = TransitionId,
        tool_name = ToolName,
        tool_server = undefined,
        input_mapping = fun(Vars) -> Vars end,
        output_mapping = fun(Result) -> Result end,
        timeout_ms = 30000,
        retry_policy = undefined
    },
    store_tool_binding(Binding).

%% @doc Unbind a workflow case from an A2A task.
-spec unbind_a2a_task(case_id()) -> ok | {error, term()}.
unbind_a2a_task(CaseId) when is_binary(CaseId) ->
    remove_a2a_binding(CaseId).

%% @doc Unbind a workflow transition from an MCP tool.
-spec unbind_mcp_tool(transition_id(), mcp_tool_name()) -> ok | {error, term()}.
unbind_mcp_tool(TransitionId, ToolName) when is_binary(TransitionId), is_binary(ToolName) ->
    remove_tool_binding(TransitionId, ToolName).

%% @private Default A2A status mapping
-spec default_a2a_status_mapping() -> #{case_status() => atom()}.
default_a2a_status_mapping() ->
    #{
        created => submitted,
        running => working,
        suspended => working,
        completed => completed,
        failed => failed,
        cancelled => canceled,
        compensating => working
    }.

%% @private Store A2A binding (placeholder)
-spec store_a2a_binding(#swf_a2a_binding{}) -> ok.
store_a2a_binding(_Binding) ->
    %% In production, would persist to ETS/DB and register with A2A handler
    ok.

%% @private Store tool binding (placeholder)
-spec store_tool_binding(#swf_tool_binding{}) -> ok.
store_tool_binding(_Binding) ->
    %% In production, would persist to ETS/DB
    ok.

%% @private Remove A2A binding (placeholder)
-spec remove_a2a_binding(case_id()) -> ok.
remove_a2a_binding(_CaseId) ->
    ok.

%% @private Remove tool binding (placeholder)
-spec remove_tool_binding(transition_id(), mcp_tool_name()) -> ok.
remove_tool_binding(_TransitionId, _ToolName) ->
    ok.

%%====================================================================
%% Promotion Policy Implementation
%%====================================================================

%% @doc Register a new promotion policy.
%% Policies define criteria for automatic patch promotion/rollback.
-spec register_policy(#swf_promotion_policy{}) -> {ok, policy_id()} | {error, term()}.
register_policy(#swf_promotion_policy{} = Policy) ->
    case erlang:whereis(swf_promotion_engine) of
        undefined -> {error, promotion_engine_not_running};
        _Pid -> swf_promotion_engine:register_policy(Policy)
    end.

%% @doc Get a promotion policy by ID.
-spec get_policy(policy_id()) -> {ok, #swf_promotion_policy{}} | {error, not_found}.
get_policy(PolicyId) when is_binary(PolicyId) ->
    case erlang:whereis(swf_promotion_engine) of
        undefined -> {error, promotion_engine_not_running};
        _Pid -> swf_promotion_engine:get_policy(PolicyId)
    end.

%% @doc List all promotion policies.
-spec list_policies() -> {ok, [#swf_promotion_policy{}]} | {error, term()}.
list_policies() ->
    case erlang:whereis(swf_promotion_engine) of
        undefined -> {ok, []};
        _Pid -> swf_promotion_engine:list_policies()
    end.

%% @doc Update an existing promotion policy.
-spec update_policy(policy_id(), map()) -> ok | {error, term()}.
update_policy(PolicyId, Updates) when is_binary(PolicyId), is_map(Updates) ->
    case erlang:whereis(swf_promotion_engine) of
        undefined -> {error, promotion_engine_not_running};
        _Pid -> swf_promotion_engine:update_policy(PolicyId, Updates)
    end.

%% @doc Delete a promotion policy.
-spec delete_policy(policy_id()) -> ok | {error, term()}.
delete_policy(PolicyId) when is_binary(PolicyId) ->
    case erlang:whereis(swf_promotion_engine) of
        undefined -> {error, promotion_engine_not_running};
        _Pid -> swf_promotion_engine:delete_policy(PolicyId)
    end.

%% @doc Evaluate a patch against a policy.
%% Returns a promotion decision (promote, reject, or defer).
-spec evaluate_patch(#swf_patch{}, policy_id()) ->
    {ok, #swf_promotion_decision{}} | {error, term()}.
evaluate_patch(#swf_patch{} = Patch, PolicyId) when is_binary(PolicyId) ->
    case erlang:whereis(swf_promotion_engine) of
        undefined -> {error, promotion_engine_not_running};
        _Pid -> swf_promotion_engine:evaluate_patch(Patch, PolicyId)
    end.

%% @doc Promote a patch (apply to workflow net).
-spec promote_patch(binary()) -> ok | {error, term()}.
promote_patch(PatchId) when is_binary(PatchId) ->
    case erlang:whereis(swf_promotion_engine) of
        undefined -> {error, promotion_engine_not_running};
        _Pid -> swf_promotion_engine:promote_patch(PatchId)
    end.

%% @doc Rollback a promoted patch.
-spec rollback_patch(binary()) -> ok | {error, term()}.
rollback_patch(PatchId) when is_binary(PatchId) ->
    case erlang:whereis(swf_promotion_engine) of
        undefined -> {error, promotion_engine_not_running};
        _Pid -> swf_promotion_engine:rollback_patch(PatchId)
    end.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private Resolve a case reference to a pid.
-spec resolve_case_ref(case_ref()) -> pid().
resolve_case_ref(Pid) when is_pid(Pid) ->
    Pid;
resolve_case_ref(CaseId) when is_binary(CaseId) ->
    %% Look up via gproc if available
    case code:is_loaded(gproc) of
        false ->
            error({case_not_found, CaseId});
        _ ->
            try
                gproc:lookup_pid({n, l, {swf_case, CaseId}})
            catch
                error:badarg ->
                    error({case_not_found, CaseId})
            end
    end.
