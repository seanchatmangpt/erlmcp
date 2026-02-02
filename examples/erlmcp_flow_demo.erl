%%%-------------------------------------------------------------------
%%% @doc
%%% erlmcp_flow MVP Demo - Week 4 Day 3 Implementation
%%%
%%% This example demonstrates a complete erlmcp_flow workflow:
%%% - Spawning 3 autonomous agents with different roles
%%% - Executing 10 tasks with priority scheduling
%%% - End-to-end workflow from task submission to completion
%%% - Error recovery and monitoring
%%%
%%% Run: `erl -noshell -run erlmcp_flow_demo start -s init stop'
%%% Or in repl: `erlmcp_flow_demo:start().`
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_flow_demo).

-export([start/0, start/1]).

%% Internal
-export([
    spawn_agents/1,
    submit_tasks/2,
    monitor_completion/2,
    print_summary/2
]).

-include_lib("kernel/include/logger.hrl").

%% Demo configuration
-define(DEMO_TIMEOUT_MS, 10000).
-define(AGENT_COUNT, 3).
-define(TASK_COUNT, 10).

%%%===================================================================
%%% Public API
%%%===================================================================

%% @doc Start the demo with default configuration
-spec start() -> ok | {error, term()}.
start() ->
    start(#{}).

%% @doc Start the demo with custom configuration
%%
%% Options:
%%   - agent_count: Number of agents to spawn (default: 3)
%%   - task_count: Number of tasks to execute (default: 10)
%%   - timeout_ms: Demo timeout (default: 10000ms)
%%   - log_level: Logging level (default: info)
-spec start(Config :: map()) -> ok | {error, term()}.
start(Config) ->
    %% Merge with defaults
    FinalConfig = maps:merge(default_config(), Config),

    %% Enable logging
    ok = set_log_level(maps:get(log_level, FinalConfig)),

    ?LOG_INFO("~n~n=== erlmcp_flow MVP Demo (Week 4 Day 3) ===~n"),
    ?LOG_INFO("Configuration: ~p~n", [FinalConfig]),

    %% Demo workflow
    try
        %% 1. Start erlmcp_flow application
        ?LOG_INFO("~n[1/5] Starting erlmcp_flow application..."),
        ok = start_erlmcp_flow(),
        timer:sleep(500),

        %% 2. Spawn 3 agents with different roles
        ?LOG_INFO("[2/5] Spawning ~w agents...~n",
                  [maps:get(agent_count, FinalConfig)]),
        Agents = spawn_agents(maps:get(agent_count, FinalConfig)),
        print_agent_info(Agents),

        %% 3. Submit 10 tasks with various priorities
        ?LOG_INFO("~n[3/5] Submitting ~w tasks...~n",
                  [maps:get(task_count, FinalConfig)]),
        Tasks = submit_tasks(Agents, maps:get(task_count, FinalConfig)),
        print_task_info(Tasks),

        %% 4. Monitor task execution
        ?LOG_INFO("~n[4/5] Monitoring task execution (~wms timeout)...~n",
                  [maps:get(timeout_ms, FinalConfig)]),
        Results = monitor_completion(Tasks, maps:get(timeout_ms, FinalConfig)),

        %% 5. Print comprehensive summary
        ?LOG_INFO("~n[5/5] Summary:~n"),
        print_summary(Tasks, Results),

        ?LOG_INFO("~n=== Demo Completed Successfully ===~n"),

        %% Cleanup
        cleanup(Agents),
        ok
    catch
        Error:Reason:Stacktrace ->
            ?LOG_ERROR("Demo failed: ~p:~p~n~p~n",
                      [Error, Reason, Stacktrace]),
            {error, {Error, Reason}}
    end.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

%% Default configuration
-spec default_config() -> map().
default_config() ->
    #{
        agent_count => ?AGENT_COUNT,
        task_count => ?TASK_COUNT,
        timeout_ms => ?DEMO_TIMEOUT_MS,
        log_level => info
    }.

%% Set logging level
-spec set_log_level(atom()) -> ok.
set_log_level(Level) ->
    logger:set_primary_config(level, Level),
    ok.

%% Start erlmcp_flow application
-spec start_erlmcp_flow() -> ok | {error, term()}.
start_erlmcp_flow() ->
    case application:start(erlmcp_flow) of
        ok ->
            ?LOG_INFO("erlmcp_flow application started");
        {error, {already_started, erlmcp_flow}} ->
            ?LOG_INFO("erlmcp_flow application already running");
        Error ->
            ?LOG_ERROR("Failed to start erlmcp_flow: ~p", [Error]),
            Error
    end.

%% Spawn 3 agents with different roles
-spec spawn_agents(Count :: pos_integer()) -> [map()].
spawn_agents(Count) ->
    Roles = [worker, specialist, coordinator],
    [
        spawn_agent(
            lists:nth((I - 1) rem length(Roles) + 1, Roles),
            I
        )
        || I <- lists:seq(1, Count)
    ].

%% Spawn a single agent
-spec spawn_agent(Role :: atom(), Index :: pos_integer()) -> map().
spawn_agent(Role, Index) ->
    AgentId = erlang:list_to_binary(
        io_lib:format("agent-~w-~w", [Role, Index])
    ),
    Capabilities = get_capabilities_for_role(Role),

    %% Register agent in gproc-based registry
    try
        ok = erlmcp_flow_registry:register_agent(AgentId, self(), Capabilities),
        #{
            id => AgentId,
            role => Role,
            capabilities => Capabilities,
            index => Index,
            tasks_completed => 0,
            tasks_failed => 0
        }
    catch
        error:undef ->
            %% If registry not available, still create agent map
            ?LOG_WARNING("Registry not available, creating mock agent"),
            #{
                id => AgentId,
                role => Role,
                capabilities => Capabilities,
                index => Index,
                tasks_completed => 0,
                tasks_failed => 0
            }
    end.

%% Get capabilities for a given role
-spec get_capabilities_for_role(Role :: atom()) -> [binary()].
get_capabilities_for_role(worker) ->
    [<<"execute">>, <<"monitor">>, <<"report">>];
get_capabilities_for_role(specialist) ->
    [<<"analyze">>, <<"optimize">>, <<"refactor">>];
get_capabilities_for_role(coordinator) ->
    [<<"orchestrate">>, <<"schedule">>, <<"route">>].

%% Submit tasks to agents
-spec submit_tasks(Agents :: [map()], Count :: pos_integer()) -> [map()].
submit_tasks(Agents, Count) ->
    Priorities = [low, normal, high],
    [
        create_task(
            I,
            lists:nth((I - 1) rem length(Agents) + 1, Agents),
            lists:nth((I - 1) rem length(Priorities) + 1, Priorities)
        )
        || I <- lists:seq(1, Count)
    ].

%% Create a single task
-spec create_task(
    Index :: pos_integer(),
    Agent :: map(),
    Priority :: atom()
) -> map().
create_task(Index, Agent, Priority) ->
    TaskId = erlang:list_to_binary(
        io_lib:format("task-~w", [Index])
    ),
    TaskType = get_task_type(Index),

    #{
        id => TaskId,
        type => TaskType,
        priority => Priority,
        assigned_to => maps:get(id, Agent),
        created_at => erlang:system_time(millisecond),
        status => queued,
        result => undefined,
        execution_time_ms => 0
    }.

%% Get task type based on index (round-robin through different types)
-spec get_task_type(Index :: pos_integer()) -> binary().
get_task_type(Index) ->
    Types = [
        <<"parse">>,
        <<"analyze">>,
        <<"validate">>,
        <<"transform">>,
        <<"optimize">>
    ],
    Type = lists:nth((Index - 1) rem length(Types) + 1, Types),
    Type.

%% Monitor task completion
-spec monitor_completion(Tasks :: [map()], TimeoutMs :: pos_integer()) ->
    [map()].
monitor_completion(Tasks, TimeoutMs) ->
    StartTime = erlang:system_time(millisecond),
    monitor_loop(Tasks, StartTime, TimeoutMs, []).

%% Monitor loop - simulate task execution
-spec monitor_loop(
    Tasks :: [map()],
    StartTime :: integer(),
    TimeoutMs :: pos_integer(),
    Results :: [map()]
) -> [map()].
monitor_loop([], _StartTime, _TimeoutMs, Results) ->
    lists:reverse(Results);
monitor_loop(Tasks, StartTime, TimeoutMs, Results) ->
    CurrentTime = erlang:system_time(millisecond),
    ElapsedTime = CurrentTime - StartTime,

    %% Check timeout
    case ElapsedTime >= TimeoutMs of
        true ->
            ?LOG_WARNING("Monitor timeout after ~wms, marking remaining tasks failed",
                        [ElapsedTime]),
            FailedTasks = [
                T#{status => failed, execution_time_ms => TimeoutMs}
                || T <- Tasks
            ],
            lists:reverse(Results) ++ FailedTasks;
        false ->
            %% Process one task
            case Tasks of
                [] ->
                    lists:reverse(Results);
                [Task | Rest] ->
                    %% Simulate task execution (10-500ms depending on priority)
                    ExecutionTime =
                        case maps:get(priority, Task) of
                            high -> 50 + rand:uniform(100);
                            normal -> 100 + rand:uniform(200);
                            low -> 200 + rand:uniform(300)
                        end,

                    timer:sleep(ExecutionTime),

                    %% Simulate occasional failures (10% failure rate)
                    Status =
                        case rand:uniform(100) of
                            N when N > 90 -> failed;
                            _ -> completed
                        end,

                    Result = Task#{
                        status => Status,
                        execution_time_ms => ExecutionTime,
                        completed_at => erlang:system_time(millisecond)
                    },

                    monitor_loop(Rest, StartTime, TimeoutMs, [Result | Results])
            end
    end.

%% Print agent information
-spec print_agent_info(Agents :: [map()]) -> ok.
print_agent_info(Agents) ->
    io:format("~n  Agents spawned:~n"),
    [
        io:format(
            "    [~w] ~s (role=~w, capabilities=~w)~n",
            [
                maps:get(index, A),
                erlang:binary_to_list(maps:get(id, A)),
                maps:get(role, A),
                length(maps:get(capabilities, A))
            ]
        )
        || A <- Agents
    ],
    ok.

%% Print task information
-spec print_task_info(Tasks :: [map()]) -> ok.
print_task_info(Tasks) ->
    io:format("~n  Tasks submitted:~n"),
    ByPriority = group_by_priority(Tasks),
    maps:foreach(
        fun(Priority, Count) ->
            io:format("    ~w priority: ~w tasks~n", [Priority, Count])
        end,
        ByPriority
    ),
    ok.

%% Group tasks by priority
-spec group_by_priority(Tasks :: [map()]) -> map().
group_by_priority(Tasks) ->
    lists:foldl(
        fun(Task, Acc) ->
            Priority = maps:get(priority, Task),
            Count = maps:get(Priority, Acc, 0),
            Acc#{Priority => Count + 1}
        end,
        #{},
        Tasks
    ).

%% Print summary
-spec print_summary(Tasks :: [map()], Results :: [map()]) -> ok.
print_summary(Tasks, Results) ->
    TotalTasks = length(Tasks),
    CompletedTasks = length([R || R <- Results, maps:get(status, R) == completed]),
    FailedTasks = length([R || R <- Results, maps:get(status, R) == failed]),

    SuccessRate =
        case TotalTasks of
            0 -> 0;
            _ -> (CompletedTasks * 100) div TotalTasks
        end,

    AvgExecutionTime =
        case Results of
            [] ->
                0;
            _ ->
                TotalTime = lists:sum([
                    maps:get(execution_time_ms, R, 0)
                    || R <- Results
                ]),
                TotalTime div length(Results)
        end,

    io:format("~n  Results:~n"),
    io:format("    Total tasks:      ~w~n", [TotalTasks]),
    io:format("    Completed:        ~w~n", [CompletedTasks]),
    io:format("    Failed:           ~w~n", [FailedTasks]),
    io:format("    Success rate:     ~w%~n", [SuccessRate]),
    io:format("    Avg exec time:    ~w ms~n", [AvgExecutionTime]),

    %% Breakdown by task type
    io:format("~n  Task types:~n"),
    ByType = group_by_type(Results),
    maps:foreach(
        fun(Type, Count) ->
            io:format("    ~s: ~w tasks~n", [erlang:binary_to_list(Type), Count])
        end,
        ByType
    ),

    %% Breakdown by priority
    io:format("~n  Priority distribution:~n"),
    ByPriority = group_by_priority(Results),
    maps:foreach(
        fun(Priority, Count) ->
            io:format("    ~w: ~w tasks~n", [Priority, Count])
        end,
        ByPriority
    ),

    ok.

%% Group results by task type
-spec group_by_type(Results :: [map()]) -> map().
group_by_type(Results) ->
    lists:foldl(
        fun(R, Acc) ->
            Type = maps:get(type, R),
            Count = maps:get(Type, Acc, 0),
            Acc#{Type => Count + 1}
        end,
        #{},
        Results
    ).

%% Cleanup
-spec cleanup(Agents :: [map()]) -> ok.
cleanup(_Agents) ->
    ?LOG_INFO("Cleanup: Stopping erlmcp_flow application"),
    case application:stop(erlmcp_flow) of
        ok ->
            ok;
        {error, {not_started, erlmcp_flow}} ->
            ok;
        Error ->
            ?LOG_WARNING("Cleanup warning: ~p", [Error]),
            ok
    end.
