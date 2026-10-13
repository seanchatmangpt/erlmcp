%%%-------------------------------------------------------------------
%%% @doc
%%% erlmcp_workflow_cli - Workflow Orchestration CLI Module
%%%
%%% Provides CLI commands for managing workflows including starting,
%%% monitoring status, and canceling workflow executions.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_workflow_cli).

%% API
-export([start_workflow/2,
         start_workflow/3,
         start_workflow/4,
         status/1,
         status/0,
         cancel/1,
         list/1,
         resume/2,
         retry/1,
         get_history/1,
         export_report/1]).

%% Workflow types and status
-type workflow_id() :: binary().
-type workflow_type() :: sequential | parallel | fanout_fanin | dag.
-type workflow_status() :: pending | running | paused | completed | failed | cancelled.

-record(workflow_execution, {
    id :: workflow_id(),
    type :: workflow_type(),
    status :: workflow_status(),
    steps :: [map()],
    current_step :: non_neg_integer(),
    input :: map(),
    output :: map() | undefined,
    error :: term() | undefined,
    started_at :: integer(),
    updated_at :: integer(),
    completed_at :: integer() | undefined,
    options :: map()
}).

-record(state, {
    workflows_table :: ets:tid()
}).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Start a workflow with type and steps
-spec start_workflow(workflow_type(), [map()]) -> {ok, workflow_id()} | {error, term()}.
start_workflow(Type, Steps) when is_atom(Type), is_list(Steps) ->
    start_workflow(Type, Steps, #{}).

%% @doc Start a workflow with type, steps, and input
-spec start_workflow(workflow_type(), [map()], map()) -> {ok, workflow_id()} | {error, term()}.
start_workflow(Type, Steps, Input) when is_atom(Type), is_list(Steps), is_map(Input) ->
    start_workflow(Type, Steps, Input, #{}).

%% @doc Start a workflow with all options
-spec start_workflow(workflow_type(), [map()], map(), map()) -> {ok, workflow_id()} | {error, term()}.
start_workflow(Type, Steps, Input, Options) when is_atom(Type), is_list(Steps), is_map(Input), is_map(Options) ->
    WorkflowId = generate_workflow_id(),
    Now = erlang:system_time(millisecond),

    Execution = #workflow_execution{
        id = WorkflowId,
        type = Type,
        status = pending,
        steps = validate_steps(Steps),
        current_step = 0,
        input = Input,
        output = undefined,
        error = undefined,
        started_at = Now,
        updated_at = Now,
        completed_at = undefined,
        options = Options
    },

    %% Store in ETS
    ets:insert(workflow_executions, Execution),

    %% Start execution
    spawn(fun() -> execute_workflow(Execution) end),

    io:format("Workflow started: ~s~n", [WorkflowId]),
    io:format("  Type: ~p~n", [Type]),
    io:format("  Steps: ~p~n", [length(Steps)]),
    {ok, WorkflowId}.

%% @doc Get status of a specific workflow
-spec status(workflow_id()) -> {ok, map()} | {error, not_found}.
status(WorkflowId) when is_binary(WorkflowId) ->
    case ets:lookup(workflow_executions, WorkflowId) of
        [Execution] ->
            {ok, execution_to_map(Execution)};
        [] ->
            {error, not_found}
    end.

%% @doc List all workflows
-spec status() -> {ok, [map()]}.
status() ->
    list(all).

%% @doc Cancel a running workflow
-spec cancel(workflow_id()) -> ok | {error, term()}.
cancel(WorkflowId) when is_binary(WorkflowId) ->
    case ets:lookup(workflow_executions, WorkflowId) of
        [#workflow_execution{status = running} = Exec] ->
            Updated = Exec#workflow_execution{
                status = cancelled,
                updated_at = erlang:system_time(millisecond),
                completed_at = erlang:system_time(millisecond)
            },
            ets:insert(workflow_executions, Updated),
            io:format("Workflow cancelled: ~s~n", [WorkflowId]),
            ok;
        [#workflow_execution{status = Status}] ->
            {error, {cannot_cancel, Status}};
        [] ->
            {error, not_found}
    end.

%% @doc List workflows with optional filter
-spec list(all | running | completed | failed) -> {ok, [map()]}.
list(Filter) when is_atom(Filter) ->
    AllExecutions = ets:tab2list(workflow_executions),
    Filtered = filter_executions(AllExecutions, Filter),
    {ok, lists:map(fun execution_to_map/1, Filtered)}.

%% @doc Resume a paused workflow
-spec resume(workflow_id(), map()) -> ok | {error, term()}.
resume(WorkflowId, NewInput) when is_binary(WorkflowId), is_map(NewInput) ->
    case ets:lookup(workflow_executions, WorkflowId) of
        [#workflow_execution{status = paused} = Exec] ->
            Updated = Exec#workflow_execution{
                status = running,
                input = maps:merge(Exec#workflow_execution.input, NewInput),
                updated_at = erlang:system_time(millisecond)
            },
            ets:insert(workflow_executions, Updated),
            spawn(fun() -> execute_workflow(Updated) end),
            io:format("Workflow resumed: ~s~n", [WorkflowId]),
            ok;
        [#workflow_execution{status = Status}] ->
            {error, {cannot_resume, Status}};
        [] ->
            {error, not_found}
    end.

%% @doc Retry a failed workflow
-spec retry(workflow_id()) -> {ok, workflow_id()} | {error, term()}.
retry(WorkflowId) when is_binary(WorkflowId) ->
    case ets:lookup(workflow_executions, WorkflowId) of
        [#workflow_execution{status = failed, type = Type, steps = Steps, input = Input}] ->
            %% Create new workflow with same config
            start_workflow(Type, Steps, Input);
        [#workflow_execution{status = Status}] ->
            {error, {cannot_retry, Status}};
        [] ->
            {error, not_found}
    end.

%% @doc Get workflow execution history
-spec get_history(workflow_id()) -> {ok, [map()]} | {error, not_found}.
get_history(WorkflowId) when is_binary(WorkflowId) ->
    case ets:lookup(workflow_history, WorkflowId) of
        [{_, History}] ->
            {ok, History};
        [] ->
            %% Check if workflow exists
            case ets:lookup(workflow_executions, WorkflowId) of
                [_] -> {ok, []};
                [] -> {error, not_found}
            end
    end.

%% @doc Export workflow report
-spec export_report(workflow_id()) -> {ok, binary()} | {error, term()}.
export_report(WorkflowId) when is_binary(WorkflowId) ->
    case status(WorkflowId) of
        {ok, WorkflowMap} ->
            History = case get_history(WorkflowId) of
                {ok, H} -> H;
                _ -> []
            end,
            Report = #{
                workflow => WorkflowMap,
                history => History,
                generated_at => erlang:system_time(millisecond)
            },
            ReportJson = jsx:encode(Report, [space]),
            Filename = <<"workflow_", WorkflowId/binary, "_report.json">>,
            file:write_file(Filename, ReportJson),
            io:format("Report exported: ~s~n", [Filename]),
            {ok, Filename};
        Error ->
            Error
    end.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private Generate workflow ID
generate_workflow_id() ->
    Timestamp = erlang:system_time(microsecond),
    Random = crypto:strong_rand_bytes(4),
    <<"wf_", (base64:encode(<<Timestamp:64, Random/binary>>))/binary>>.

%% @private Validate workflow steps
validate_steps(Steps) when is_list(Steps) ->
    lists:map(fun validate_step/1, Steps).

%% @private Validate single step
validate_step(Step) when is_map(Step) ->
    Required = [name, action],
    lists:foreach(fun(Key) ->
        case maps:is_key(Key, Step) of
            true -> ok;
            false -> throw({missing_step_key, Key})
        end
    end, Required),
    Step;
validate_step(Step) when is_list(Step) ->
    validate_step(maps:from_list(Step)).

%% @private Execute workflow
execute_workflow(Execution) ->
    %% Update status to running
    Running = Execution#workflow_execution{
        status = running,
        updated_at = erlang:system_time(millisecond)
    },
    ets:insert(workflow_executions, Running),

    Result = case Execution#workflow_execution.type of
        sequential ->
            execute_sequential(Running);
        parallel ->
            execute_parallel(Running);
        fanout_fanin ->
            execute_fanout_fanin(Running);
        dag ->
            execute_dag(Running)
    end,

    handle_execution_result(Execution#workflow_execution.id, Result).

%% @private Execute sequential workflow
execute_sequential(Execution) ->
    Steps = Execution#workflow_execution.steps,
    Input = Execution#workflow_execution.input,

    execute_sequential_steps(Steps, Input, 0, Execution, #{}).

execute_sequential_steps([], _AccInput, _Index, _Execution, AccOutput) ->
    {completed, AccOutput};
execute_sequential_steps([Step | Rest], Input, Index, Execution, AccOutput) ->
    %% Update current step
    ets:update_element(workflow_executions, Execution#workflow_execution.id,
                      {#workflow_execution.current_step, Index}),

    %% Log step start
    log_step_event(Execution#workflow_execution.id, Step, start),

    case execute_step(Step, Input, Execution) of
        {ok, StepOutput} ->
            log_step_event(Execution#workflow_execution.id, Step, {complete, StepOutput}),
            NewInput = maps:merge(Input, StepOutput),
            execute_sequential_steps(Rest, NewInput, Index + 1, Execution,
                                   maps:merge(AccOutput, StepOutput));
        {error, Reason} ->
            log_step_event(Execution#workflow_execution.id, Step, {error, Reason}),
            {failed, {step_failed, Index, Reason}}
    end.

%% @private Execute parallel workflow
execute_parallel(Execution) ->
    Steps = Execution#workflow_execution.steps,
    Input = Execution#workflow_execution.input,

    %% Spawn all steps in parallel
    Pids = lists:map(fun(Step) ->
        spawn_monitor(fun() ->
            execute_step(Step, Input, Execution)
        end)
    end, Steps),

    %% Collect results
    Results = collect_parallel_results(Pids, #{}),

    case lists:keyfind(error, 1, maps:values(Results)) of
        false ->
            {completed, maps:fold(fun(_K, {ok, V}, Acc) -> maps:merge(Acc, V) end, #{}, Results)};
        {error, Reason} ->
            {failed, Reason}
    end.

%% @private Execute fanout-fanin workflow
execute_fanout_fanin(Execution) ->
    Steps = Execution#workflow_execution.steps,
    Input = Execution#workflow_execution.input,

    %% Fanout: Execute all steps
    Pids = lists:map(fun(Step) ->
        spawn_monitor(fun() ->
            execute_step(Step, Input, Execution)
        end)
    end, Steps),

    %% Fanin: Wait for all and aggregate
    Results = collect_parallel_results(Pids, []),
    {completed, #{results => Results}}.

%% @private Execute DAG workflow
execute_dag(Execution) ->
    Steps = Execution#workflow_execution.steps,
    Input = Execution#workflow_execution.input,

    %% Build dependency graph and execute in topological order
    case build_dag_graph(Steps) of
        {ok, Graph} ->
            execute_dag_steps(Graph, Input, Execution, #{});
        {error, Reason} ->
            {failed, {invalid_dag, Reason}}
    end.

%% @private Execute single step
execute_step(Step, Input, Execution) ->
    Action = maps:get(action, Step),
    Config = maps:get(config, Step, #{}),
    Timeout = maps:get(timeout, Execution#workflow_execution.options, 30000),

    try
        case execute_action(Action, Input, Config) of
            {ok, Output} ->
                {ok, Output};
            {error, Reason} ->
                {error, Reason}
        end
    catch
        Type:Error:Stack ->
            {error, {exception, Type, Error, Stack}}
    end.

%% @private Execute action
execute_action(Action, Input, Config) ->
    case Action of
        <<"mcp_call">> ->
            execute_mcp_call(Input, Config);
        <<"http_request">> ->
            execute_http_request(Input, Config);
        <<"transform">> ->
            execute_transform(Input, Config);
        <<"validate">> ->
            execute_validate(Input, Config);
        <<"delay">> ->
            execute_delay(Config);
        <<"log">> ->
            execute_log(Input, Config);
        _ ->
            {error, {unknown_action, Action}}
    end.

%% @private Action implementations
execute_mcp_call(Input, Config) ->
    Server = maps:get(server, Config),
    Method = maps:get(method, Config),
    Params = maps:get(params, Config, Input),
    %% Integrate with erlmcp_client
    try
        case whereis(erlmcp_client_sup) of
            undefined ->
                %% Mock for now
                {ok, #{result => mock_mcp_result}};
            _ ->
                %% Would call actual client
                {ok, #{server => Server, method => Method, params => Params}}
        end
    catch
        _:_ -> {error, mcp_client_unavailable}
    end.

execute_http_request(Input, Config) ->
    Url = maps:get(url, Config, Input),
    Method = maps:get(method, Config, <<"GET">>),
    %% Would use gun or httpc
    {ok, #{url => Url, method => Method, status => 200}}.

execute_transform(Input, Config) ->
    Transform = maps:get(transform, Config),
    case Transform of
        <<"uppercase">> ->
            Value = maps:get(value, Input, <<>>),
            {ok, #{result => string:uppercase(Value)}};
        <<"lowercase">> ->
            Value = maps:get(value, Input, <<>>),
            {ok, #{result => string:lowercase(Value)}};
        <<"json_decode">> ->
            Value = maps:get(value, Input, <<>>),
            {ok, #{result => jsx:decode(Value, [return_maps])}};
        <<"json_encode">> ->
            Value = maps:get(value, Input, #{}),
            {ok, #{result => jsx:encode(Value)}};
        _ ->
            {error, {unknown_transform, Transform}}
    end.

execute_validate(Input, Config) ->
    Rules = maps:get(rules, Config, []),
    Results = lists:map(fun(Rule) ->
        Field = maps:get(field, Rule),
        Type = maps:get(type, Rule, required),
        Value = maps:get(binary_to_list(Field), Input, undefined),
        case Type of
            required when Value =:= undefined -> {Field, false};
            required -> {Field, true};
            {optional, _} -> {Field, true}
        end
    end, Rules),
    AllPassed = lists:all(fun({_, R}) -> R =:= true end, Results),
    if AllPassed -> {ok, #{validation => passed}};
       true -> {error, #{validation => failed, results => Results}}
    end.

execute_delay(Config) ->
    Duration = maps:get(duration_ms, Config, 1000),
    timer:sleep(Duration),
    {ok, #{delayed => Duration}}.

execute_log(Input, Config) ->
    Level = maps:get(level, Config, info),
    Message = maps:get(message, Config, Input),
    logger:log(Level, "~p", [Message]),
    {ok, #{logged => Message}}.

%% @private Collect parallel results
collect_parallel_results(Pids, Acc) when map_size(Acc) =:= length(Pids) ->
    Acc;
collect_parallel_results(Pids, Acc) ->
    receive
        {'DOWN', Ref, process, Pid, Result} ->
            Index = find_pid_index(Pids, Pid, Ref),
            collect_parallel_results(Pids, Acc#{Index => Result})
    after 60000 ->
        {error, timeout}
    end.

%% @private Find PID index
find_pid_index(Pids, PidRefList, Ref) ->
    {Index, {Pid, Ref}} = lists:foldl(fun({P, R}, {I, _}) when R =:= Ref -> {I, {P, R}};
                                         (_, Acc) -> Acc
                                      end, {0, undefined}, PidRefList),
    Index.

%% @private Build DAG graph
build_dag_graph(Steps) ->
    %% Extract dependencies and build adjacency list
    try
        Graph = lists:foldl(fun(Step, Acc) ->
            Name = maps:get(name, Step),
            DependsOn = maps:get(depends_on, Step, []),
            Acc#{Name => DependsOn}
        end, #{}, Steps),
        {ok, Graph}
    catch
        _:_ -> {error, invalid_step_format}
    end.

%% @private Execute DAG steps in topological order
execute_dag_steps(Graph, Input, Execution, AccOutput) ->
    %% Simple topological sort and execution
    Nodes = maps:keys(Graph),
    execute_dag_nodes(Nodes, Graph, Input, Execution, AccOutput).

execute_dag_nodes([], _Graph, _Input, _Execution, AccOutput) ->
    {completed, AccOutput};
execute_dag_nodes([Node | Rest], Graph, Input, Execution, AccOutput) ->
    %% Check if dependencies are satisfied
    Deps = maps:get(Node, Graph, []),
    DepsSatisfied = lists:all(fun(Dep) -> maps:is_key(Dep, AccOutput) end, Deps),

    case DepsSatisfied of
        true ->
            %% Execute this node
            Step = #{name => Node, action => Node},
            case execute_step(Step, Input, Execution) of
                {ok, Output} ->
                    NewAcc = maps:merge(AccOutput, Output),
                    execute_dag_nodes(Rest, Graph, Input, Execution, NewAcc);
                {error, Reason} ->
                    {failed, {node_failed, Node, Reason}}
            end;
        false ->
            %% Skip for now, will execute later
            execute_dag_nodes(Rest ++ [Node], Graph, Input, Execution, AccOutput)
    end.

%% @private Handle execution result
handle_execution_result(WorkflowId, Result) ->
    Now = erlang:system_time(millisecond),
    case ets:lookup(workflow_executions, WorkflowId) of
        [Execution] ->
            {Status, Output, Error} = case Result of
                {completed, Out} -> {completed, Out, undefined};
                {failed, Reason} -> {failed, undefined, Reason};
                {cancelled, _} -> {cancelled, undefined, cancelled}
            end,
            Updated = Execution#workflow_execution{
                status = Status,
                output = Output,
                error = Error,
                updated_at = Now,
                completed_at = Now
            },
            ets:insert(workflow_executions, Updated);
        [] ->
            ok
    end.

%% @private Log step event
log_step_event(WorkflowId, Step, Event) ->
    HistoryKey = {workflow_history, WorkflowId},
    EventRecord = #{
        step => maps:get(name, Step),
        event => Event,
        timestamp => erlang:system_time(millisecond)
    },
    case ets:lookup(workflow_history, HistoryKey) of
        [{_, History}] ->
            ets:insert(workflow_history, {HistoryKey, [EventRecord | History]});
        [] ->
            ets:insert(workflow_history, {HistoryKey, [EventRecord]})
    end.

%% @private Filter executions
filter_executions(Executions, Filter) ->
    lists:filter(fun(Exec) ->
        case Filter of
            all -> true;
            running -> Exec#workflow_execution.status =:= running;
            completed -> Exec#workflow_execution.status =:= completed;
            failed -> Exec#workflow_execution.status =:= failed
        end
    end, Executions).

%% @private Convert execution to map
execution_to_map(#workflow_execution{
    id = Id,
    type = Type,
    status = Status,
    current_step = Step,
    input = Input,
    output = Output,
    error = Error,
    started_at = Started,
    updated_at = Updated,
    completed_at = Completed
}) ->
    Duration = case Completed of
        undefined -> undefined;
        _ -> Completed - Started
    end,
    #{
        id => Id,
        type => Type,
        status => Status,
        current_step => Step,
        total_steps => Step,
        input => Input,
        output => Output,
        error => Error,
        started_at => Started,
        updated_at => Updated,
        completed_at => Completed,
        duration_ms => Duration
    }.

%%====================================================================
%% CLI Output
%%====================================================================

%% @doc Format workflow status for CLI
-spec format_workflow_status(map()) -> iolist().
format_workflow_status(Workflow) ->
    Id = maps:get(id, Workflow),
    Status = atom_to_list(maps:get(status, Workflow)),
    Type = atom_to_list(maps:get(type, Workflow)),
    Started = format_timestamp(maps:get(started_at, Workflow)),
    Duration = maps:get(duration_ms, Workflow),

    io_lib:format(
        "Workflow: ~s~n"
        "  Status: ~s~n"
        "  Type: ~s~n"
        "  Started: ~s~n"
        "  Duration: ~p ms~n",
        [Id, Status, Type, Started, Duration]).

%% @private Format timestamp
format_timestamp(Ms) ->
    Sec = Ms div 1000,
    {{Y, M, D}, {H, Min, S}} = calendar:system_time_to_universal_time(Sec, second),
    io_lib:format("~4..0B-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B", [Y, M, D, H, Min, S]).

%% @doc Initialize tables
-spec init_tables() -> ok.
init_tables() ->
    try
        ets:new(workflow_executions, [
            named_table,
            set,
            public,
            {keypos, #workflow_execution.id}
        ]),
        ets:new(workflow_history, [
            named_table,
            bag,
            public
        ]),
        ok
    catch
        _:_ -> ok
    end.
