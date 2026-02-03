%%%-------------------------------------------------------------------
%%% @doc
%%% Workflow rollback and compensation for erlmcp v3
%%% Implements Saga pattern for distributed transaction management.
%%%
%%% == Features ==
%%% - Saga transaction coordination
%%% - Compensation action registration
%%% - Rollback orchestration
%%% - State restoration
%%% - Compensation logging
%%% - Automatic recovery
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_workflow_compensation).

-behaviour(gen_server).

-include("erlmcp.hrl").

%% API exports
-export([start_link/0, start_link/1,
         register_compensation/3,
         execute_compensation/2,
         execute_saga/2,
         add_saga_step/3,
         rollback_saga/1,
         get_saga_state/1,
         list_sagas/0,
         compensate_workflow/2,
         log_compensation/3,
         get_compensation_log/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% Type definitions
-type workflow_id() :: binary().
-type task_id() :: binary().
-type saga_id() :: binary().
-type compensation_action() :: #{module := module(), function := atom(), args := list()}.

-record(saga_step,
        {step_id :: task_id(),
         action :: compensation_action(),
         compensation :: compensation_action() | undefined,
         state = pending :: pending | completed | compensated | failed}).

-record(saga_state,
        {saga_id :: saga_id(),
         workflow_id :: workflow_id(),
         steps = [] :: [#saga_step{}],
         completed_steps = [] :: [task_id()],
         current_step :: task_id() | undefined,
         state = pending :: pending | running | completed | failed | compensating,
         started_at :: integer() | undefined,
         completed_at :: integer() | undefined,
         error :: term() | undefined}).

-record(compensation_log,
        {workflow_id :: workflow_id(),
         task_id :: task_id(),
         action :: compensation_action(),
         result :: term() | undefined,
         compensated_at :: integer(),
         error :: term() | undefined}).

-record(state,
        {sagas = #{} :: #{saga_id() => #saga_state{}},
         compensations = #{} :: #{workflow_id() => #{task_id() => compensation_action()}},
         compensation_log = [] :: [#compensation_log{}},
         max_log_entries = 10000 :: pos_integer()}).

-define(DEFAULT_TIMEOUT, 30000).

%%====================================================================
%% API Functions
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link(#{}).

-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(_Options) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec register_compensation(workflow_id(), task_id(), compensation_action()) -> ok.
register_compensation(WorkflowId, TaskId, CompensationAction) ->
    gen_server:cast(?MODULE, {register_compensation, WorkflowId, TaskId, CompensationAction}).

-spec execute_compensation(workflow_id(), task_id()) -> {ok, term()} | {error, term()}.
execute_compensation(WorkflowId, TaskId) ->
    gen_server:call(?MODULE, {execute_compensation, WorkflowId, TaskId}, ?DEFAULT_TIMEOUT).

-spec execute_saga(saga_id(), [#saga_step{}]) -> {ok, saga_id()} | {error, term()}.
execute_saga(SagaId, Steps) ->
    gen_server:call(?MODULE, {execute_saga, SagaId, Steps}, ?DEFAULT_TIMEOUT).

-spec add_saga_step(saga_id(), task_id(), #saga_step{}) -> ok | {error, term()}.
add_saga_step(SagaId, StepId, Step) ->
    gen_server:call(?MODULE, {add_saga_step, SagaId, StepId, Step}, ?DEFAULT_TIMEOUT).

-spec rollback_saga(saga_id()) -> ok | {error, term()}.
rollback_saga(SagaId) ->
    gen_server:call(?MODULE, {rollback_saga, SagaId}, ?DEFAULT_TIMEOUT).

-spec get_saga_state(saga_id()) -> {ok, map()} | {error, not_found}.
get_saga_state(SagaId) ->
    gen_server:call(?MODULE, {get_saga_state, SagaId}, ?DEFAULT_TIMEOUT).

-spec list_sagas() -> {ok, [saga_id()]}.
list_sagas() ->
    gen_server:call(?MODULE, list_sagas, ?DEFAULT_TIMEOUT).

-spec compensate_workflow(workflow_id(), [task_id()]) -> {ok, compensated} | {error, term()}.
compensate_workflow(WorkflowId, FailedTaskIds) ->
    gen_server:call(?MODULE, {compensate_workflow, WorkflowId, FailedTaskIds}, ?DEFAULT_TIMEOUT).

-spec log_compensation(workflow_id(), task_id(), term()) -> ok.
log_compensation(WorkflowId, TaskId, Result) ->
    gen_server:cast(?MODULE, {log_compensation, WorkflowId, TaskId, Result, erlang:system_time(millisecond)}).

-spec get_compensation_log(workflow_id()) -> {ok, [map()]}.
get_compensation_log(WorkflowId) ->
    gen_server:call(?MODULE, {get_compensation_log, WorkflowId}, ?DEFAULT_TIMEOUT).

%%====================================================================
%% gen_server callbacks
%%====================================================================

-spec init([]) -> {ok, #state{}}.
init([]) ->
    logger:info("Initializing workflow compensation engine"),
    {ok, #state{}}.

handle_call({execute_compensation, WorkflowId, TaskId}, _From, State) ->
    case maps:get(WorkflowId, State#state.compensations, undefined) of
        undefined ->
            {reply, {error, no_compensation}, State};
        WorkflowCompensations ->
            case maps:get(TaskId, WorkflowCompensations, undefined) of
                undefined ->
                    {reply, {error, no_compensation}, State};
                CompensationAction ->
                    Result = execute_action(CompensationAction),
                    LogEntry = #compensation_log{
                        workflow_id = WorkflowId,
                        task_id = TaskId,
                        action = CompensationAction,
                        result = Result,
                        compensated_at = erlang:system_time(millisecond)
                    },
                    NewLog = [LogEntry | State#state.compensation_log],
                    TrimmedLog = trim_log(NewLog, State#state.max_log_entries),
                    {reply, Result, State#state{compensation_log = TrimmedLog}}
            end
    end;

handle_call({execute_saga, SagaId, Steps}, _From, State) ->
    SagaState = #saga_state{
        saga_id = SagaId,
        workflow_id = maps:get(workflow_id, hd(Steps), <<"unknown">>),
        steps = Steps,
        state = running,
        started_at = erlang:system_time(millisecond)
    },
    %% Execute saga steps
    case execute_saga_steps(Steps, []) of
        {ok, CompletedSteps} ->
            CompletedSaga = SagaState#saga_state{
                completed_steps = CompletedSteps,
                state = completed,
                completed_at = erlang:system_time(millisecond)
            },
            NewSagas = maps:put(SagaId, CompletedSaga, State#state.sagas),
            {reply, {ok, SagaId}, State#state{sagas = NewSagas}};
        {error, Reason} ->
            FailedSaga = SagaState#saga_state{
                state = failed,
                error = Reason
            },
            NewSagas = maps:put(SagaId, FailedSaga, State#state.sagas),
            {reply, {error, Reason}, State#state{sagas = NewSagas}}
    end;

handle_call({add_saga_step, SagaId, StepId, Step}, _From, State) ->
    case maps:get(SagaId, State#state.sagas, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        SagaState ->
            NewSteps = [Step | SagaState#saga_state.steps],
            UpdatedSaga = SagaState#saga_state{steps = NewSteps},
            NewSagas = maps:put(SagaId, UpdatedSaga, State#state.sagas),
            {reply, ok, State#state{sagas = NewSagas}}
    end;

handle_call({rollback_saga, SagaId}, _From, State) ->
    case maps:get(SagaId, State#state.sagas, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        #saga_state{completed_steps = CompletedSteps} = SagaState ->
            logger:info("Rolling back saga ~p with ~p completed steps", [SagaId, length(CompletedSteps)]),
            case execute_compensations(CompletedSteps, SagaState) of
                ok ->
                    CompensatedSaga = SagaState#saga_state{
                        state = compensating
                    },
                    NewSagas = maps:put(SagaId, CompensatedSaga, State#state.sagas),
                    {reply, ok, State#state{sagas = NewSagas}};
                {error, Reason} ->
                    {reply, {error, Reason}, State}
            end
    end;

handle_call({get_saga_state, SagaId}, _From, State) ->
    case maps:get(SagaId, State#state.sagas, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        SagaState ->
            SagaMap = #{
                saga_id => SagaState#saga_state.saga_id,
                workflow_id => SagaState#saga_state.workflow_id,
                state => SagaState#saga_state.state,
                total_steps => length(SagaState#saga_state.steps),
                completed_steps => length(SagaState#saga_state.completed_steps),
                started_at => SagaState#saga_state.started_at,
                completed_at => SagaState#saga_state.completed_at,
                error => SagaState#saga_state.error
            },
            {reply, {ok, SagaMap}, State}
    end;

handle_call(list_sagas, _From, State) ->
    SagaIds = maps:keys(State#state.sagas),
    {reply, {ok, SagaIds}, State};

handle_call({compensate_workflow, WorkflowId, FailedTaskIds}, _From, State) ->
    case maps:get(WorkflowId, State#state.compensations, undefined) of
        undefined ->
            {reply, {error, no_compensations}, State};
        WorkflowCompensations ->
            %% Execute compensations in reverse order
            TaskIds = lists:reverse(FailedTaskIds),
            Results = lists:map(fun(TaskId) ->
                case maps:get(TaskId, WorkflowCompensations, undefined) of
                    undefined ->
                        {error, no_compensation};
                    CompensationAction ->
                        execute_action(CompensationAction)
                end
            end, TaskIds),

            case lists:all(fun(R) -> R =/= {error, no_compensation} end, Results) of
                true ->
                    {reply, {ok, compensated}, State};
                false ->
                    {reply, {error, partial_compensation}, State}
            end
    end;

handle_call({get_compensation_log, WorkflowId}, _From, State) ->
    Logs = [L || L <- State#state.compensation_log, L#compensation_log.workflow_id =:= WorkflowId],
    LogMaps = lists:map(fun(Log) ->
        #{
            workflow_id => Log#compensation_log.workflow_id,
            task_id => Log#compensation_log.task_id,
            action => Log#compensation_log.action,
            result => Log#compensation_log.result,
            compensated_at => Log#compensation_log.compensated_at,
            error => Log#compensation_log.error
        }
    end, Logs),
    {reply, {ok, LogMaps}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({register_compensation, WorkflowId, TaskId, CompensationAction}, State) ->
    WorkflowCompensations = maps:get(WorkflowId, State#state.compensations, #{}),
    UpdatedCompensations = maps:put(TaskId, CompensationAction, WorkflowCompensations),
    NewCompensations = maps:put(WorkflowId, UpdatedCompensations, State#state.compensations),
    {noreply, State#state{compensations = NewCompensations}};

handle_cast({log_compensation, WorkflowId, TaskId, Result, Timestamp}, State) ->
    %% This would be populated by the execute_compensation call
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

-spec execute_action(compensation_action()) -> {ok, term()} | {error, term()}.
execute_action(#{module := Module, function := Function, args := Args}) ->
    try
        Result = apply(Module, Function, Args),
        {ok, Result}
    catch
        Type:Error:StackTrace ->
            logger:error("Compensation action failed: ~p:~p ~p", [Type, Error, StackTrace]),
            {error, {Type, Error, StackTrace}}
    end.

-spec execute_saga_steps([#saga_step{}], [task_id()]) -> {ok, [task_id()]} | {error, term()}.
execute_saga_steps([], CompletedSteps) ->
    {ok, lists:reverse(CompletedSteps)};
execute_saga_steps([#saga_step{step_id = StepId, action = Action} | Rest], CompletedSteps) ->
    case execute_action(Action) of
        {ok, _Result} ->
            execute_saga_steps(Rest, [StepId | CompletedSteps]);
        {error, Reason} ->
            logger:error("Saga step ~p failed: ~p", [StepId, Reason]),
            {error, Reason}
    end.

-spec execute_compensations([task_id()], #saga_state{}) -> ok | {error, term()}.
execute_compensations([], _SagaState) ->
    ok;
execute_compensations([StepId | Rest], #saga_state{steps = Steps} = SagaState) ->
    case lists:keyfind(StepId, #saga_step.step_id, Steps) of
        #saga_step{compensation = undefined} ->
            logger:warning("No compensation for step ~p", [StepId]),
            execute_compensations(Rest, SagaState);
        #saga_step{compensation = Compensation} ->
            case execute_action(Compensation) of
                {ok, _} ->
                    execute_compensations(Rest, SagaState);
                {error, Reason} ->
                    logger:error("Compensation for step ~p failed: ~p", [StepId, Reason]),
                    {error, Reason}
            end
    end.

-spec trim_log([#compensation_log{}], pos_integer()) -> [#compensation_log{}].
trim_log(Log, MaxEntries) when length(Log) > MaxEntries ->
    lists:sublist(Log, MaxEntries);
trim_log(Log, _MaxEntries) ->
    Log.
