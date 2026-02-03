%%%-------------------------------------------------------------------
%%% @doc
%%% Simple workflow example for erlmcp v3 orchestration
%%%
%%% This example demonstrates:
%%% - Creating a workflow with sequential tasks
%%% - Starting and monitoring workflow execution
%%% - Handling completion and errors
%%% @end
%%%-------------------------------------------------------------------
-module(simple_workflow_example).

-include("erlmcp.hrl").

%% API
-export([run/0, run/1]).
-export([task1/0, task2/0, task3/0]).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Run the simple workflow example
-spec run() -> ok | {error, term()}.
run() ->
    run(<<"simple-example">>).

%% @doc Run the workflow with a specific ID
-spec run(binary()) -> ok | {error, term()}.
run(WorkflowId) ->
    logger:info("Starting simple workflow example: ~p", [WorkflowId]),

    %% Step 1: Create workflow orchestrator
    {ok, OrchestratorPid} = erlmcp_workflow_orchestrator:start_link(),

    %% Step 2: Define workflow tasks
    Tasks = #{
        <<"task1">> => #{
            name => <<"Initialize">>,
            module => ?MODULE,
            function => task1,
            args => [],
            dependencies => [],
            timeout => 5000
        },
        <<"task2">> => #{
            name => <<"Process">>,
            module => ?MODULE,
            function => task2,
            args => [],
            dependencies => [<<"task1">>],
            timeout => 5000
        },
        <<"task3">> => #{
            name => <<"Finalize">>,
            module => ?MODULE,
            function => task3,
            args => [],
            dependencies => [<<"task2">>],
            timeout => 5000
        }
    },

    %% Step 3: Create workflow
    case erlmcp_workflow_orchestrator:create_workflow(WorkflowId, <<"Simple Example">>, Tasks) of
        {ok, WorkflowId} ->
            logger:info("Workflow created: ~p", [WorkflowId]),

            %% Step 4: Execute workflow
            ok = erlmcp_workflow_orchestrator:execute_workflow(WorkflowId),

            %% Step 5: Monitor workflow
            monitor_workflow(WorkflowId, 5000),

            %% Step 6: Get final status
            {ok, Status} = erlmcp_workflow_orchestrator:get_workflow_status(WorkflowId),
            logger:info("Workflow final status: ~p", [Status]),

            gen_server:stop(OrchestratorPid),
            ok;
        {error, Reason} ->
            logger:error("Failed to create workflow: ~p", [Reason]),
            {error, Reason}
    end.

%%====================================================================
%% Task Implementations
%%====================================================================

%% @doc Task 1: Initialize
-spec task1() -> {ok, term()}.
task1() ->
    logger:info("Task 1: Initializing..."),
    timer:sleep(1000),
    logger:info("Task 1: Complete"),
    {ok, initialized}.

%% @doc Task 2: Process
-spec task2() -> {ok, term()}.
task2() ->
    logger:info("Task 2: Processing..."),
    timer:sleep(1000),
    logger:info("Task 2: Complete"),
    {ok, processed}.

%% @doc Task 3: Finalize
-spec task3() -> {ok, term()}.
task3() ->
    logger:info("Task 3: Finalizing..."),
    timer:sleep(1000),
    logger:info("Task 3: Complete"),
    {ok, finalized}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @doc Monitor workflow execution
-spec monitor_workflow(binary(), pos_integer()) -> ok.
monitor_workflow(WorkflowId, Timeout) ->
    monitor_workflow(WorkflowId, Timeout, 0).

monitor_workflow(_WorkflowId, Timeout, Elapsed) when Elapsed >= Timeout ->
    logger:warning("Workflow monitoring timeout"),
    ok;
monitor_workflow(WorkflowId, Timeout, Elapsed) ->
    case erlmcp_workflow_orchestrator:get_workflow_status(WorkflowId) of
        {ok, completed} ->
            logger:info("Workflow completed successfully"),
            ok;
        {ok, failed} ->
            logger:error("Workflow failed"),
            ok;
        {ok, Status} ->
            logger:info("Workflow status: ~p (~pms elapsed)", [Status, Elapsed]),
            timer:sleep(500),
            monitor_workflow(WorkflowId, Timeout, Elapsed + 500);
        {error, Reason} ->
            logger:error("Error checking workflow status: ~p", [Reason]),
            ok
    end.
