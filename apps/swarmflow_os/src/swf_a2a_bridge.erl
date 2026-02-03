%%%-------------------------------------------------------------------
%%% @doc SwarmFlow A2A Bridge
%%%
%%% This module bridges SwarmFlow workflow cases with A2A (Agent-to-Agent)
%%% tasks, messages, and artifacts, enabling bi-directional synchronization
%%% between the two systems.
%%%
%%% Features:
%%% - Bi-directional sync between workflow cases and A2A tasks
%%% - Status mapping (case_status <-> a2a_task_state)
%%% - Message-to-transition mapping (A2A messages trigger transitions)
%%% - Artifact generation from workflow outputs
%%% - Context management (A2A context_id <-> workflow case hierarchy)
%%% - Streaming support (forward workflow events as A2A streaming updates)
%%%
%%% State Mapping:
%%%   case_status      | a2a_task_state
%%%   -----------------+----------------
%%%   created          | submitted
%%%   running          | working
%%%   suspended        | input_required
%%%   completed        | completed
%%%   failed           | failed
%%%   cancelled        | canceled
%%%   compensating     | working
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(swf_a2a_bridge).

-behaviour(gen_server).

-include("swarmflow.hrl").
-include_lib("erlmcp_core/include/erlmcp_a2a.hrl").

%% API exports
-export([
    start_link/0,
    start_link/1,
    %% Binding management
    bind_case_to_task/3,
    unbind/1,
    get_binding/1,
    list_bindings/0,
    %% Synchronization
    sync_case_to_task/1,
    sync_task_to_case/1,
    %% Message handling
    handle_a2a_message/2,
    %% Artifact management
    emit_artifact/3,
    %% Streaming
    subscribe_to_case/1,
    unsubscribe_from_case/1,
    %% Context management
    create_context_for_case/1,
    get_context_for_case/1,
    %% Utilities
    case_status_to_a2a_state/1,
    a2a_state_to_case_status/1
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    handle_continue/2,
    terminate/2,
    code_change/3
]).

%% Types
-type case_id() :: binary().
-type task_id() :: binary().
-type context_id() :: binary().
-type sync_mode() :: bidirectional | case_to_task | task_to_case.

-export_type([case_id/0, task_id/0, context_id/0, sync_mode/0]).

%% ETS table names
-define(BINDING_TABLE, swf_a2a_bindings).
-define(CASE_INDEX, swf_a2a_case_index).
-define(TASK_INDEX, swf_a2a_task_index).
-define(CONTEXT_INDEX, swf_a2a_context_index).
-define(SUBSCRIPTION_TABLE, swf_a2a_subscriptions).

%% Default status mapping (case_status => a2a_task_state)
-define(DEFAULT_STATUS_MAPPING, #{
    created => submitted,
    running => working,
    suspended => input_required,
    completed => completed,
    failed => failed,
    cancelled => canceled,
    compensating => working
}).

%% Reverse mapping (a2a_task_state => case_status)
-define(DEFAULT_REVERSE_MAPPING, #{
    unspecified => created,
    submitted => created,
    working => running,
    completed => completed,
    failed => failed,
    canceled => cancelled,
    input_required => suspended,
    rejected => cancelled,
    auth_required => suspended
}).

%% State record
-record(state, {
    binding_table :: ets:tid(),
    case_index :: ets:tid(),
    task_index :: ets:tid(),
    context_index :: ets:tid(),
    subscription_table :: ets:tid(),
    binding_count = 0 :: non_neg_integer(),
    default_status_mapping :: map(),
    default_reverse_mapping :: map(),
    task_manager_pid :: pid() | undefined,
    event_log_pid :: pid() | undefined
}).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Start the bridge with default options
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link(#{}).

%% @doc Start the bridge with options
-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Opts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Opts, []).

%% @doc Bind a workflow case to an A2A task
%% Creates bidirectional synchronization between case and task
-spec bind_case_to_task(case_id(), task_id(), map()) ->
    {ok, #swf_a2a_binding{}} | {error, term()}.
bind_case_to_task(CaseId, TaskId, Opts) when is_binary(CaseId), is_binary(TaskId), is_map(Opts) ->
    gen_server:call(?MODULE, {bind, CaseId, TaskId, Opts}).

%% @doc Remove a binding by case_id or task_id
-spec unbind(case_id() | task_id()) -> ok | {error, not_found}.
unbind(Id) when is_binary(Id) ->
    gen_server:call(?MODULE, {unbind, Id}).

%% @doc Get binding info for case_id or task_id
-spec get_binding(case_id() | task_id()) -> {ok, #swf_a2a_binding{}} | {error, not_found}.
get_binding(Id) when is_binary(Id) ->
    gen_server:call(?MODULE, {get_binding, Id}).

%% @doc List all bindings
-spec list_bindings() -> [#swf_a2a_binding{}].
list_bindings() ->
    gen_server:call(?MODULE, list_bindings).

%% @doc Sync case state to A2A task
%% Updates the A2A task to reflect the current case state
-spec sync_case_to_task(case_id()) -> ok | {error, term()}.
sync_case_to_task(CaseId) when is_binary(CaseId) ->
    gen_server:call(?MODULE, {sync_case_to_task, CaseId}).

%% @doc Sync A2A task to case state
%% Updates the workflow case to reflect the current task state
-spec sync_task_to_case(task_id()) -> ok | {error, term()}.
sync_task_to_case(TaskId) when is_binary(TaskId) ->
    gen_server:call(?MODULE, {sync_task_to_case, TaskId}).

%% @doc Process incoming A2A message and fire appropriate transitions
-spec handle_a2a_message(task_id(), #a2a_message{}) ->
    {ok, fired | no_transition} | {error, term()}.
handle_a2a_message(TaskId, Message) when is_binary(TaskId) ->
    gen_server:call(?MODULE, {handle_a2a_message, TaskId, Message}).

%% @doc Create A2A artifact from workflow output
-spec emit_artifact(case_id(), binary(), map()) -> {ok, #a2a_artifact{}} | {error, term()}.
emit_artifact(CaseId, ArtifactName, OutputData) when is_binary(CaseId), is_binary(ArtifactName), is_map(OutputData) ->
    gen_server:call(?MODULE, {emit_artifact, CaseId, ArtifactName, OutputData}).

%% @doc Subscribe to case events for A2A streaming
-spec subscribe_to_case(case_id()) -> ok | {error, term()}.
subscribe_to_case(CaseId) when is_binary(CaseId) ->
    gen_server:call(?MODULE, {subscribe, CaseId, self()}).

%% @doc Unsubscribe from case events
-spec unsubscribe_from_case(case_id()) -> ok.
unsubscribe_from_case(CaseId) when is_binary(CaseId) ->
    gen_server:call(?MODULE, {unsubscribe, CaseId, self()}).

%% @doc Create A2A context for a case hierarchy
-spec create_context_for_case(case_id()) -> {ok, context_id()} | {error, term()}.
create_context_for_case(CaseId) when is_binary(CaseId) ->
    gen_server:call(?MODULE, {create_context, CaseId}).

%% @doc Get A2A context for a case
-spec get_context_for_case(case_id()) -> {ok, context_id()} | {error, not_found}.
get_context_for_case(CaseId) when is_binary(CaseId) ->
    gen_server:call(?MODULE, {get_context, CaseId}).

%% @doc Convert case_status to a2a_task_state
-spec case_status_to_a2a_state(case_status()) -> a2a_task_state().
case_status_to_a2a_state(Status) ->
    maps:get(Status, ?DEFAULT_STATUS_MAPPING, working).

%% @doc Convert a2a_task_state to case_status
-spec a2a_state_to_case_status(a2a_task_state()) -> case_status().
a2a_state_to_case_status(State) ->
    maps:get(State, ?DEFAULT_REVERSE_MAPPING, running).

%%====================================================================
%% gen_server callbacks
%%====================================================================

-spec init(map()) -> {ok, #state{}, {continue, initialize}}.
init(Opts) ->
    process_flag(trap_exit, true),

    StatusMapping = maps:get(status_mapping, Opts, ?DEFAULT_STATUS_MAPPING),
    ReverseMapping = maps:get(reverse_mapping, Opts, ?DEFAULT_REVERSE_MAPPING),

    State = #state{
        default_status_mapping = StatusMapping,
        default_reverse_mapping = ReverseMapping
    },

    logger:info("Starting SwarmFlow A2A bridge (async initialization)"),
    {ok, State, {continue, initialize}}.

-spec handle_continue(term(), #state{}) -> {noreply, #state{}}.
handle_continue(initialize, State) ->
    %% Create ETS tables
    BindingTable = ets:new(?BINDING_TABLE, [
        set,
        public,
        named_table,
        {read_concurrency, true},
        {write_concurrency, true},
        {keypos, 1}
    ]),

    CaseIndex = ets:new(?CASE_INDEX, [
        set,
        public,
        named_table,
        {read_concurrency, true},
        {write_concurrency, true},
        {keypos, 1}
    ]),

    TaskIndex = ets:new(?TASK_INDEX, [
        set,
        public,
        named_table,
        {read_concurrency, true},
        {write_concurrency, true},
        {keypos, 1}
    ]),

    ContextIndex = ets:new(?CONTEXT_INDEX, [
        set,
        public,
        named_table,
        {read_concurrency, true},
        {write_concurrency, true},
        {keypos, 1}
    ]),

    SubscriptionTable = ets:new(?SUBSCRIPTION_TABLE, [
        bag,
        public,
        named_table,
        {read_concurrency, true},
        {write_concurrency, true},
        {keypos, 1}
    ]),

    %% Locate task manager
    TaskManagerPid = erlang:whereis(erlmcp_a2a_task_manager),
    EventLogPid = erlang:whereis(swf_event_log),

    %% Subscribe to workflow events if event log is available
    case EventLogPid of
        undefined -> ok;
        Pid when is_pid(Pid) ->
            %% Subscribe to all workflow events for forwarding
            Pid ! {subscribe, self(), all}
    end,

    NewState = State#state{
        binding_table = BindingTable,
        case_index = CaseIndex,
        task_index = TaskIndex,
        context_index = ContextIndex,
        subscription_table = SubscriptionTable,
        task_manager_pid = TaskManagerPid,
        event_log_pid = EventLogPid
    },

    logger:info("SwarmFlow A2A bridge initialized"),
    {noreply, NewState};
handle_continue(_Continue, State) ->
    {noreply, State}.

-spec handle_call(term(), {pid(), term()}, #state{}) -> {reply, term(), #state{}}.
%% Bind case to task
handle_call({bind, CaseId, TaskId, Opts}, _From, State) ->
    %% Check if already bound
    case lookup_by_case(CaseId, State) of
        {ok, _ExistingBinding} ->
            {reply, {error, already_bound}, State};
        {error, not_found} ->
            case lookup_by_task(TaskId, State) of
                {ok, _ExistingBinding} ->
                    {reply, {error, task_already_bound}, State};
                {error, not_found} ->
                    %% Create binding
                    ContextId = maps:get(context_id, Opts, generate_context_id()),
                    SyncMode = maps:get(sync_mode, Opts, bidirectional),
                    CustomMapping = maps:get(status_mapping, Opts, State#state.default_status_mapping),

                    Binding = #swf_a2a_binding{
                        case_id = CaseId,
                        task_id = TaskId,
                        context_id = ContextId,
                        sync_mode = SyncMode,
                        status_mapping = CustomMapping
                    },

                    %% Store binding and indexes
                    BindingKey = {CaseId, TaskId},
                    true = ets:insert(State#state.binding_table, {BindingKey, Binding}),
                    true = ets:insert(State#state.case_index, {CaseId, BindingKey}),
                    true = ets:insert(State#state.task_index, {TaskId, BindingKey}),
                    true = ets:insert(State#state.context_index, {ContextId, CaseId}),

                    NewState = State#state{binding_count = State#state.binding_count + 1},

                    logger:debug("Bound case ~s to task ~s in context ~s",
                        [CaseId, TaskId, ContextId]),

                    {reply, {ok, Binding}, NewState}
            end
    end;

%% Unbind
handle_call({unbind, Id}, _From, State) ->
    case do_unbind(Id, State) of
        {ok, NewState} ->
            {reply, ok, NewState};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

%% Get binding
handle_call({get_binding, Id}, _From, State) ->
    Result = case lookup_by_case(Id, State) of
        {ok, Binding} -> {ok, Binding};
        {error, not_found} -> lookup_by_task(Id, State)
    end,
    {reply, Result, State};

%% List bindings
handle_call(list_bindings, _From, State) ->
    Bindings = ets:foldl(
        fun({_Key, Binding}, Acc) -> [Binding | Acc] end,
        [],
        State#state.binding_table
    ),
    {reply, Bindings, State};

%% Sync case to task
handle_call({sync_case_to_task, CaseId}, _From, State) ->
    Result = do_sync_case_to_task(CaseId, State),
    {reply, Result, State};

%% Sync task to case
handle_call({sync_task_to_case, TaskId}, _From, State) ->
    Result = do_sync_task_to_case(TaskId, State),
    {reply, Result, State};

%% Handle A2A message
handle_call({handle_a2a_message, TaskId, Message}, _From, State) ->
    Result = do_handle_a2a_message(TaskId, Message, State),
    {reply, Result, State};

%% Emit artifact
handle_call({emit_artifact, CaseId, ArtifactName, OutputData}, _From, State) ->
    Result = do_emit_artifact(CaseId, ArtifactName, OutputData, State),
    {reply, Result, State};

%% Subscribe to case events
handle_call({subscribe, CaseId, Pid}, _From, State) ->
    erlang:monitor(process, Pid),
    ets:insert(State#state.subscription_table, {CaseId, Pid}),
    {reply, ok, State};

%% Unsubscribe from case events
handle_call({unsubscribe, CaseId, Pid}, _From, State) ->
    ets:delete_object(State#state.subscription_table, {CaseId, Pid}),
    {reply, ok, State};

%% Create context for case
handle_call({create_context, CaseId}, _From, State) ->
    case ets:lookup(State#state.context_index, CaseId) of
        [{_CaseId, ContextId}] ->
            %% Already has context
            {reply, {ok, ContextId}, State};
        [] ->
            ContextId = generate_context_id(),
            ets:insert(State#state.context_index, {CaseId, ContextId}),
            {reply, {ok, ContextId}, State}
    end;

%% Get context for case
handle_call({get_context, CaseId}, _From, State) ->
    case lookup_by_case(CaseId, State) of
        {ok, Binding} ->
            {reply, {ok, Binding#swf_a2a_binding.context_id}, State};
        {error, not_found} ->
            %% Check direct context index
            case ets:lookup(State#state.context_index, CaseId) of
                [{_CaseId, ContextId}] ->
                    {reply, {ok, ContextId}, State};
                [] ->
                    {reply, {error, not_found}, State}
            end
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

-spec handle_cast(term(), #state{}) -> {noreply, #state{}}.
handle_cast({workflow_event, Event}, State) ->
    %% Forward workflow events to A2A subscribers
    handle_workflow_event(Event, State),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), #state{}) -> {noreply, #state{}}.
%% Handle workflow events from event log
handle_info({swf_event, Event}, State) when is_record(Event, swf_event) ->
    handle_workflow_event(Event, State),
    {noreply, State};

%% Handle subscriber death
handle_info({'DOWN', _Ref, process, Pid, _Reason}, State) ->
    %% Clean up subscriptions for dead processes
    ets:foldl(
        fun({CaseId, SubPid}, _Acc) when SubPid =:= Pid ->
            ets:delete_object(State#state.subscription_table, {CaseId, Pid});
           (_, Acc) -> Acc
        end,
        ok,
        State#state.subscription_table
    ),
    {noreply, State};

%% Handle task manager availability changes
handle_info({task_manager_available, Pid}, State) ->
    {noreply, State#state{task_manager_pid = Pid}};

handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), #state{}) -> ok.
terminate(_Reason, State) ->
    %% Clean up ETS tables
    catch ets:delete(State#state.binding_table),
    catch ets:delete(State#state.case_index),
    catch ets:delete(State#state.task_index),
    catch ets:delete(State#state.context_index),
    catch ets:delete(State#state.subscription_table),
    ok.

-spec code_change(term(), #state{}, term()) -> {ok, #state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions: Binding Lookup
%%====================================================================

%% @private Lookup binding by case ID
-spec lookup_by_case(case_id(), #state{}) -> {ok, #swf_a2a_binding{}} | {error, not_found}.
lookup_by_case(CaseId, State) ->
    case ets:lookup(State#state.case_index, CaseId) of
        [{_CaseId, BindingKey}] ->
            case ets:lookup(State#state.binding_table, BindingKey) of
                [{_Key, Binding}] -> {ok, Binding};
                [] -> {error, not_found}
            end;
        [] ->
            {error, not_found}
    end.

%% @private Lookup binding by task ID
-spec lookup_by_task(task_id(), #state{}) -> {ok, #swf_a2a_binding{}} | {error, not_found}.
lookup_by_task(TaskId, State) ->
    case ets:lookup(State#state.task_index, TaskId) of
        [{_TaskId, BindingKey}] ->
            case ets:lookup(State#state.binding_table, BindingKey) of
                [{_Key, Binding}] -> {ok, Binding};
                [] -> {error, not_found}
            end;
        [] ->
            {error, not_found}
    end.

%% @private Unbind case/task
-spec do_unbind(binary(), #state{}) -> {ok, #state{}} | {error, not_found}.
do_unbind(Id, State) ->
    %% Try to find by case first, then by task
    case lookup_by_case(Id, State) of
        {ok, Binding} ->
            do_remove_binding(Binding, State);
        {error, not_found} ->
            case lookup_by_task(Id, State) of
                {ok, Binding} ->
                    do_remove_binding(Binding, State);
                {error, not_found} ->
                    {error, not_found}
            end
    end.

%% @private Remove binding from all indexes
-spec do_remove_binding(#swf_a2a_binding{}, #state{}) -> {ok, #state{}}.
do_remove_binding(Binding, State) ->
    CaseId = Binding#swf_a2a_binding.case_id,
    TaskId = Binding#swf_a2a_binding.task_id,
    ContextId = Binding#swf_a2a_binding.context_id,
    BindingKey = {CaseId, TaskId},

    ets:delete(State#state.binding_table, BindingKey),
    ets:delete(State#state.case_index, CaseId),
    ets:delete(State#state.task_index, TaskId),
    ets:delete(State#state.context_index, ContextId),

    NewState = State#state{binding_count = max(0, State#state.binding_count - 1)},
    {ok, NewState}.

%%====================================================================
%% Internal Functions: Synchronization
%%====================================================================

%% @private Sync case state to A2A task
-spec do_sync_case_to_task(case_id(), #state{}) -> ok | {error, term()}.
do_sync_case_to_task(CaseId, State) ->
    case lookup_by_case(CaseId, State) of
        {ok, Binding} ->
            SyncMode = Binding#swf_a2a_binding.sync_mode,
            case SyncMode of
                task_to_case ->
                    {error, sync_mode_not_allowed};
                _ ->
                    %% Get case state
                    case get_case_record(CaseId) of
                        {ok, CaseRecord} ->
                            %% Map status
                            CaseStatus = CaseRecord#swf_case.status,
                            StatusMapping = Binding#swf_a2a_binding.status_mapping,
                            A2AState = maps:get(CaseStatus, StatusMapping, working),

                            %% Create status update
                            TaskStatus = #a2a_task_status{
                                state = A2AState,
                                timestamp = iso8601_timestamp()
                            },

                            %% Update task
                            TaskId = Binding#swf_a2a_binding.task_id,
                            case State#state.task_manager_pid of
                                undefined ->
                                    {error, task_manager_not_available};
                                _Pid ->
                                    erlmcp_a2a_task_manager:update_task_status(TaskId, TaskStatus)
                            end;
                        {error, Reason} ->
                            {error, {case_not_found, Reason}}
                    end
            end;
        {error, not_found} ->
            {error, binding_not_found}
    end.

%% @private Sync A2A task to case state
-spec do_sync_task_to_case(task_id(), #state{}) -> ok | {error, term()}.
do_sync_task_to_case(TaskId, State) ->
    case lookup_by_task(TaskId, State) of
        {ok, Binding} ->
            SyncMode = Binding#swf_a2a_binding.sync_mode,
            case SyncMode of
                case_to_task ->
                    {error, sync_mode_not_allowed};
                _ ->
                    %% Get task state
                    case erlmcp_a2a_task_manager:get_task(TaskId) of
                        {ok, Task} ->
                            %% Map status
                            TaskStatus = Task#a2a_task.status,
                            A2AState = TaskStatus#a2a_task_status.state,
                            CaseStatus = a2a_state_to_case_status(A2AState),

                            %% Update case
                            CaseId = Binding#swf_a2a_binding.case_id,
                            update_case_status(CaseId, CaseStatus);
                        {error, Reason} ->
                            {error, {task_not_found, Reason}}
                    end
            end;
        {error, not_found} ->
            {error, binding_not_found}
    end.

%%====================================================================
%% Internal Functions: Message Handling
%%====================================================================

%% @private Handle incoming A2A message and fire appropriate transitions
-spec do_handle_a2a_message(task_id(), #a2a_message{}, #state{}) ->
    {ok, fired | no_transition} | {error, term()}.
do_handle_a2a_message(TaskId, Message, State) ->
    case lookup_by_task(TaskId, State) of
        {ok, Binding} ->
            CaseId = Binding#swf_a2a_binding.case_id,

            %% Extract message content for transition matching
            MessageParts = Message#a2a_message.parts,
            TransitionData = extract_transition_data(MessageParts),

            %% Try to find and fire matching transition
            case find_and_fire_transition(CaseId, TransitionData) of
                {ok, _Marking} ->
                    %% Add message to task history
                    erlmcp_a2a_task_manager:add_message_to_history(TaskId, Message),
                    {ok, fired};
                {error, no_matching_transition} ->
                    %% No transition found, just add to history
                    erlmcp_a2a_task_manager:add_message_to_history(TaskId, Message),
                    {ok, no_transition};
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, not_found} ->
            {error, binding_not_found}
    end.

%% @private Extract data from message parts for transition matching
-spec extract_transition_data([#a2a_part{}]) -> map().
extract_transition_data(Parts) ->
    lists:foldl(
        fun(Part, Acc) ->
            case Part of
                #a2a_part{text = Text} when Text =/= undefined ->
                    maps:put(text, Text, Acc);
                #a2a_part{data = Data} when Data =/= undefined ->
                    case Data of
                        Map when is_map(Map) -> maps:merge(Acc, Map);
                        _ -> maps:put(data, Data, Acc)
                    end;
                _ ->
                    Acc
            end
        end,
        #{},
        Parts
    ).

%% @private Find and fire transition based on message data
-spec find_and_fire_transition(case_id(), map()) ->
    {ok, term()} | {error, term()}.
find_and_fire_transition(CaseId, TransitionData) ->
    case get_case_pid(CaseId) of
        {ok, Pid} ->
            %% Get enabled transitions
            case swf_case:get_enabled_transitions(Pid) of
                {ok, EnabledTransitions} ->
                    %% Try to find matching transition
                    case find_matching_transition(EnabledTransitions, TransitionData) of
                        {ok, TransitionId} ->
                            %% Fire the transition
                            swf_case:fire_transition(Pid, TransitionId);
                        {error, not_found} ->
                            {error, no_matching_transition}
                    end;
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% @private Find transition matching message data
-spec find_matching_transition([binary()], map()) -> {ok, binary()} | {error, not_found}.
find_matching_transition([], _TransitionData) ->
    {error, not_found};
find_matching_transition([TransitionId | Rest], TransitionData) ->
    %% Check if transition matches based on metadata or naming convention
    case matches_transition(TransitionId, TransitionData) of
        true -> {ok, TransitionId};
        false -> find_matching_transition(Rest, TransitionData)
    end.

%% @private Check if transition matches data
-spec matches_transition(binary(), map()) -> boolean().
matches_transition(TransitionId, TransitionData) ->
    %% Match by transition_id in data or text content
    case maps:get(transition_id, TransitionData, undefined) of
        TransitionId -> true;
        undefined ->
            %% Try matching by command in text
            case maps:get(text, TransitionData, undefined) of
                undefined -> false;
                Text when is_binary(Text) ->
                    %% Simple text matching - transition ID appears in text
                    binary:match(Text, TransitionId) =/= nomatch
            end;
        _ -> false
    end.

%%====================================================================
%% Internal Functions: Artifact Management
%%====================================================================

%% @private Emit artifact from workflow output
-spec do_emit_artifact(case_id(), binary(), map(), #state{}) ->
    {ok, #a2a_artifact{}} | {error, term()}.
do_emit_artifact(CaseId, ArtifactName, OutputData, State) ->
    case lookup_by_case(CaseId, State) of
        {ok, Binding} ->
            TaskId = Binding#swf_a2a_binding.task_id,

            %% Create artifact
            ArtifactId = generate_artifact_id(),
            Parts = create_parts_from_output(OutputData),

            Artifact = #a2a_artifact{
                artifact_id = ArtifactId,
                name = ArtifactName,
                description = <<"Generated from workflow output">>,
                parts = Parts,
                metadata = #{
                    case_id => CaseId,
                    generated_at => iso8601_timestamp()
                }
            },

            %% Add to task
            case erlmcp_a2a_task_manager:add_artifact(TaskId, Artifact) of
                ok ->
                    {ok, Artifact};
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, not_found} ->
            {error, binding_not_found}
    end.

%% @private Create A2A parts from workflow output data
-spec create_parts_from_output(map()) -> [#a2a_part{}].
create_parts_from_output(OutputData) when is_map(OutputData) ->
    %% Convert map to JSON data part
    [#a2a_part{
        data = OutputData,
        media_type = ?A2A_MIME_APPLICATION_JSON
    }].

%%====================================================================
%% Internal Functions: Workflow Event Handling
%%====================================================================

%% @private Handle workflow event and forward to A2A subscribers
-spec handle_workflow_event(#swf_event{}, #state{}) -> ok.
handle_workflow_event(Event, State) ->
    CaseId = Event#swf_event.case_id,

    %% Check if this case has a binding
    case lookup_by_case(CaseId, State) of
        {ok, Binding} ->
            %% Forward to A2A task as status or artifact update
            TaskId = Binding#swf_a2a_binding.task_id,
            forward_event_to_task(Event, TaskId, Binding, State),

            %% Notify direct subscribers
            notify_case_subscribers(Event, CaseId, State);
        {error, not_found} ->
            %% No binding, just notify direct subscribers
            notify_case_subscribers(Event, CaseId, State)
    end.

%% @private Forward workflow event to A2A task
-spec forward_event_to_task(#swf_event{}, task_id(), #swf_a2a_binding{}, #state{}) -> ok.
forward_event_to_task(Event, TaskId, Binding, _State) ->
    EventType = Event#swf_event.event_type,
    ContextId = Binding#swf_a2a_binding.context_id,

    case event_to_a2a_update(EventType, Event, Binding) of
        {status_update, Status} ->
            %% Create status update event
            UpdateEvent = #a2a_task_status_update_event{
                task_id = TaskId,
                context_id = ContextId,
                status = Status,
                metadata = Event#swf_event.data
            },

            %% Notify task subscribers
            erlmcp_a2a_task_manager:trigger_push_notification(TaskId, {status, UpdateEvent});

        {artifact_update, Artifact} ->
            %% Create artifact update event
            UpdateEvent = #a2a_task_artifact_update_event{
                task_id = TaskId,
                context_id = ContextId,
                artifact = Artifact,
                append = false,
                last_chunk = true,
                metadata = Event#swf_event.data
            },

            %% Notify task subscribers
            erlmcp_a2a_task_manager:trigger_push_notification(TaskId, {artifact, UpdateEvent});

        no_update ->
            ok
    end.

%% @private Convert workflow event to A2A update
-spec event_to_a2a_update(event_type(), #swf_event{}, #swf_a2a_binding{}) ->
    {status_update, #a2a_task_status{}} | {artifact_update, #a2a_artifact{}} | no_update.
event_to_a2a_update(case_started, Event, Binding) ->
    StatusMapping = Binding#swf_a2a_binding.status_mapping,
    A2AState = maps:get(running, StatusMapping, working),
    {status_update, #a2a_task_status{
        state = A2AState,
        timestamp = format_timestamp(Event#swf_event.timestamp)
    }};

event_to_a2a_update(case_completed, Event, Binding) ->
    StatusMapping = Binding#swf_a2a_binding.status_mapping,
    A2AState = maps:get(completed, StatusMapping, completed),
    {status_update, #a2a_task_status{
        state = A2AState,
        timestamp = format_timestamp(Event#swf_event.timestamp)
    }};

event_to_a2a_update(case_failed, Event, Binding) ->
    StatusMapping = Binding#swf_a2a_binding.status_mapping,
    A2AState = maps:get(failed, StatusMapping, failed),
    {status_update, #a2a_task_status{
        state = A2AState,
        timestamp = format_timestamp(Event#swf_event.timestamp)
    }};

event_to_a2a_update(case_cancelled, Event, Binding) ->
    StatusMapping = Binding#swf_a2a_binding.status_mapping,
    A2AState = maps:get(cancelled, StatusMapping, canceled),
    {status_update, #a2a_task_status{
        state = A2AState,
        timestamp = format_timestamp(Event#swf_event.timestamp)
    }};

event_to_a2a_update(case_suspended, Event, Binding) ->
    StatusMapping = Binding#swf_a2a_binding.status_mapping,
    A2AState = maps:get(suspended, StatusMapping, input_required),
    {status_update, #a2a_task_status{
        state = A2AState,
        timestamp = format_timestamp(Event#swf_event.timestamp)
    }};

event_to_a2a_update(case_resumed, Event, Binding) ->
    StatusMapping = Binding#swf_a2a_binding.status_mapping,
    A2AState = maps:get(running, StatusMapping, working),
    {status_update, #a2a_task_status{
        state = A2AState,
        timestamp = format_timestamp(Event#swf_event.timestamp)
    }};

event_to_a2a_update(transition_completed, Event, _Binding) ->
    %% Create artifact from transition output
    TransitionId = Event#swf_event.transition_id,
    EventData = Event#swf_event.data,

    case maps:get(output, EventData, undefined) of
        undefined -> no_update;
        Output ->
            Artifact = #a2a_artifact{
                artifact_id = generate_artifact_id(),
                name = <<"transition_output_", TransitionId/binary>>,
                parts = [#a2a_part{
                    data = Output,
                    media_type = ?A2A_MIME_APPLICATION_JSON
                }],
                metadata = #{
                    transition_id => TransitionId,
                    timestamp => format_timestamp(Event#swf_event.timestamp)
                }
            },
            {artifact_update, Artifact}
    end;

event_to_a2a_update(_EventType, _Event, _Binding) ->
    no_update.

%% @private Notify case subscribers of workflow event
-spec notify_case_subscribers(#swf_event{}, case_id(), #state{}) -> ok.
notify_case_subscribers(Event, CaseId, State) ->
    Subscribers = [Pid || {_CId, Pid} <- ets:lookup(State#state.subscription_table, CaseId)],

    %% Create streaming update
    StreamEvent = {swf_a2a_stream, CaseId, Event},

    lists:foreach(
        fun(Pid) ->
            Pid ! StreamEvent
        end,
        Subscribers
    ),
    ok.

%%====================================================================
%% Internal Functions: Case Utilities
%%====================================================================

%% @private Get case record
-spec get_case_record(case_id()) -> {ok, #swf_case{}} | {error, term()}.
get_case_record(CaseId) ->
    case get_case_pid(CaseId) of
        {ok, Pid} ->
            swf_case:get_case_record(Pid);
        {error, Reason} ->
            {error, Reason}
    end.

%% @private Get case pid from registry
-spec get_case_pid(case_id()) -> {ok, pid()} | {error, not_found}.
get_case_pid(CaseId) ->
    case code:is_loaded(gproc) of
        false ->
            {error, gproc_not_loaded};
        _ ->
            try
                case gproc:lookup_pid({n, l, {swf_case, CaseId}}) of
                    Pid when is_pid(Pid) -> {ok, Pid};
                    _ -> {error, not_found}
                end
            catch
                _:_ -> {error, not_found}
            end
    end.

%% @private Update case status
-spec update_case_status(case_id(), case_status()) -> ok | {error, term()}.
update_case_status(CaseId, NewStatus) ->
    case get_case_pid(CaseId) of
        {ok, Pid} ->
            %% Use appropriate case API based on status
            case NewStatus of
                suspended -> swf_case:suspend(Pid);
                running -> swf_case:resume(Pid);
                cancelled -> swf_case:cancel(Pid);
                _ -> ok  % Other status changes handled by workflow
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%%====================================================================
%% Internal Functions: Utilities
%%====================================================================

%% @private Generate unique context ID
-spec generate_context_id() -> binary().
generate_context_id() ->
    Rand = crypto:strong_rand_bytes(16),
    <<"ctx-", (binary:encode_hex(Rand))/binary>>.

%% @private Generate unique artifact ID
-spec generate_artifact_id() -> binary().
generate_artifact_id() ->
    Rand = crypto:strong_rand_bytes(8),
    <<"art-", (binary:encode_hex(Rand))/binary>>.

%% @private Generate ISO 8601 timestamp
-spec iso8601_timestamp() -> binary().
iso8601_timestamp() ->
    {{Year, Month, Day}, {Hour, Min, Sec}} = calendar:universal_time(),
    iolist_to_binary(io_lib:format("~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0BZ",
        [Year, Month, Day, Hour, Min, Sec])).

%% @private Format microsecond timestamp to ISO 8601
-spec format_timestamp(integer()) -> binary().
format_timestamp(MicroSecs) when is_integer(MicroSecs) ->
    Secs = MicroSecs div 1000000,
    {{Year, Month, Day}, {Hour, Min, Sec}} = calendar:gregorian_seconds_to_datetime(
        Secs + calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}})
    ),
    iolist_to_binary(io_lib:format("~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0BZ",
        [Year, Month, Day, Hour, Min, Sec])).
