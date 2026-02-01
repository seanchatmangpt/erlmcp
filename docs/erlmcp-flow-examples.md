# erlmcp-flow: Code Examples

**Version**: 1.0.0
**Date**: 2026-02-01

---

## Example 1: Agent Registration & Discovery

```erlang
%% Register an OTP developer agent
-spec register_otp_developer() -> ok.
register_otp_developer() ->
    AgentId = <<"agent-erlang-otp-developer-01">>,

    ok = erlmcp_flow_registry:register_agent(AgentId, self(), #{
        type => <<"erlang-otp-developer">>,
        capabilities => [
            <<"gen_server">>,
            <<"supervisor">>,
            <<"gen_statem">>,
            <<"gen_event">>
        ],
        max_concurrent_tasks => 5,
        skill_level => expert
    }),

    logger:info("Registered OTP developer agent: ~s", [AgentId]).

%% Find all agents capable of writing gen_servers
-spec find_gen_server_experts() -> [pid()].
find_gen_server_experts() ->
    erlmcp_flow_registry:find_agents_by_capability(<<"gen_server">>).

%% Find the least loaded test engineer
-spec assign_to_available_tester() -> {ok, pid()} | {error, no_agents}.
assign_to_available_tester() ->
    case erlmcp_flow_registry:find_least_loaded_agent(<<"erlang-test-engineer">>) of
        {ok, AgentPid} ->
            {ok, AgentPid};
        {error, not_found} ->
            {error, no_agents}
    end.
```

---

## Example 2: Direct Task Assignment

```erlang
%% Assign a gen_server implementation task to a specific agent
-spec assign_gen_server_task(agent_id(), filename()) -> ok | {error, term()}.
assign_gen_server_task(AgentId, Filename) ->
    Task = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => generate_task_id(),
        <<"method">> => <<"flow/assign_task">>,
        <<"params">> => #{
            <<"task_type">> => <<"implement_gen_server">>,
            <<"file">> => Filename,
            <<"requirements">> => #{
                <<"callbacks">> => [<<"init/1">>, <<"handle_call/3">>, <<"handle_cast/2">>],
                <<"supervision">> => true,
                <<"test_coverage">> => 0.82
            },
            <<"flow">> => #{
                <<"source_agent">> => <<"coordinator-001">>,
                <<"target_agent">> => AgentId,
                <<"routing">> => <<"direct">>,
                <<"priority">> => <<"high">>,
                <<"deadline">> => 30000,  % 30 seconds
                <<"trace_id">> => generate_trace_id()
            }
        }
    },

    erlmcp_flow_router:send_direct(AgentId, Task).

%% Example usage
example_assign_task() ->
    %% Find an available OTP developer
    case erlmcp_flow_registry:find_least_loaded_agent(<<"erlang-otp-developer">>) of
        {ok, AgentPid} ->
            AgentId = erlmcp_flow_registry:agent_id_from_pid(AgentPid),
            assign_gen_server_task(AgentId, <<"src/my_server.erl">>);
        {error, not_found} ->
            logger:error("No OTP developers available")
    end.
```

---

## Example 3: Broadcast Notifications

```erlang
%% Broadcast compile errors to all build agents
-spec broadcast_compile_errors([file_error()]) -> ok.
broadcast_compile_errors(Errors) ->
    Notification = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"method">> => <<"flow/notification">>,
        <<"params">> => #{
            <<"event">> => <<"compile_errors">>,
            <<"errors">> => [format_error(E) || E <- Errors],
            <<"timestamp">> => erlang:system_time(millisecond),
            <<"flow">> => #{
                <<"source_agent">> => <<"build-system">>,
                <<"routing">> => <<"broadcast">>,
                <<"topic">> => <<"build">>
            }
        }
    },

    erlmcp_flow_router:broadcast(<<"build">>, Notification).

%% Subscribe to build notifications
-spec subscribe_to_build_events() -> ok.
subscribe_to_build_events() ->
    erlmcp_flow_router:subscribe_topic(<<"build">>),

    %% Handle incoming notifications
    receive
        {flow_message, broadcast, <<"build">>, Notification} ->
            handle_build_notification(Notification)
    end.

%% Example: All build agents subscribe
example_build_coordination() ->
    %% Start 5 build agents
    BuildAgents = [spawn(fun() ->
        AgentId = list_to_binary("build-agent-" ++ integer_to_list(I)),

        %% Register
        erlmcp_flow_registry:register_agent(AgentId, self(), #{
            type => <<"build-engineer">>,
            capabilities => [<<"compile">>, <<"link">>]
        }),

        %% Subscribe to build topic
        erlmcp_flow_router:subscribe_topic(<<"build">>),

        %% Process notifications
        build_agent_loop()
    end) || I <- lists:seq(1, 5)],

    %% Broadcast a compile error
    timer:sleep(100),
    broadcast_compile_errors([
        {error, <<"src/test.erl">>, 42, <<"undefined function foo/1">>}
    ]),

    BuildAgents.
```

---

## Example 4: Gossip-Based State Synchronization

```erlang
%% Gossip system health status to agents
-spec gossip_health_status(health_status()) -> ok.
gossip_health_status(HealthStatus) ->
    StateUpdate = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"method">> => <<"flow/gossip">>,
        <<"params">> => #{
            <<"state_type">> => <<"health_status">>,
            <<"status">> => #{
                <<"circuit_breaker">> => maps:get(circuit_breaker, HealthStatus),
                <<"memory_usage">> => maps:get(memory_usage, HealthStatus),
                <<"active_agents">> => maps:get(active_agents, HealthStatus)
            },
            <<"version">> => erlang:system_time(millisecond),
            <<"flow">> => #{
                <<"source_agent">> => <<"health-monitor">>,
                <<"routing">> => <<"gossip">>,
                <<"fan_out">> => 3,  % Gossip to 3 random agents
                <<"ttl">> => 5  % Max 5 hops
            }
        }
    },

    erlmcp_flow_router:gossip(StateUpdate, #{fan_out => 3}).

%% Handle gossip messages
-spec handle_gossip(gossip_message()) -> ok.
handle_gossip(#{<<"params">> := Params} = Message) ->
    StateType = maps:get(<<"state_type">>, Params),
    Status = maps:get(<<"status">>, Params),
    Version = maps:get(<<"version">>, Params),

    %% Update local state
    update_local_state(StateType, Status, Version),

    %% Propagate to neighbors if TTL > 0
    case maps:get([<<"flow">>, <<"ttl">>], Message) of
        TTL when TTL > 0 ->
            propagate_gossip(Message, TTL - 1);
        _ ->
            ok  % Don't propagate further
    end.

%% Example: Gossip circuit breaker status
example_gossip_circuit_breaker() ->
    %% Detect circuit breaker open
    case erlmcp_memory_guard:is_circuit_breaker_open() of
        true ->
            gossip_health_status(#{
                circuit_breaker => open,
                memory_usage => 0.95,
                active_agents => 45
            });
        false ->
            ok
    end.
```

---

## Example 5: Transport Bridging (stdio → flow)

```erlang
%% Start stdio bridge for Claude Code integration
-spec start_stdio_flow_bridge() -> {ok, pid()}.
start_stdio_flow_bridge() ->
    erlmcp_flow_stdio_bridge:start_link(#{
        agent_id => <<"flow-coordinator">>,
        agent_type => <<"coordinator">>,
        capabilities => [
            <<"task_routing">>,
            <<"load_balancing">>,
            <<"workflow_orchestration">>
        ]
    }).

%% Example: Route Claude Code task through stdio to flow agent
example_claude_to_agent() ->
    %% Start bridge
    {ok, BridgePid} = start_stdio_flow_bridge(),

    %% Simulate Claude Code sending task via stdio
    Task = jsx:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 1,
        <<"method">> => <<"flow/task">>,
        <<"params">> => #{
            <<"task">> => <<"Design supervision tree">>,
            <<"context">> => #{
                <<"files">> => [<<"apps/my_app/src/my_sup.erl">>],
                <<"requirements">> => <<"3-tier supervision">>
            },
            <<"flow">> => #{
                <<"source_agent">> => <<"flow-coordinator">>,
                <<"target_agent">> => <<"agent-erlang-architect-01">>,
                <<"routing">> => <<"direct">>,
                <<"priority">> => <<"high">>,
                <<"deadline">> => 60000
            }
        }
    }),

    %% Send through stdio (simulated)
    BridgePid ! {transport_message, Task},

    {ok, BridgePid}.
```

---

## Example 6: Flow Control & Backpressure

```erlang
%% Initialize flow control with token bucket
-spec init_agent_flow_control() -> flow_control_state().
init_agent_flow_control() ->
    erlmcp_flow_backpressure:init_flow_control(#{
        max_tokens => 1000,      % 1000 tokens
        refill_rate => 100,      % 100 tokens/sec
        max_queue_size => 10000  % Max 10K pending messages
    }).

%% Send message with backpressure handling
-spec send_with_backpressure(agent_id(), term(), flow_control_state()) ->
    {ok, flow_control_state()} | {error, backpressure}.
send_with_backpressure(TargetAgent, Message, FlowControl) ->
    MessageBinary = jsx:encode(Message),
    MessageSize = byte_size(MessageBinary),

    case erlmcp_flow_backpressure:consume_tokens(MessageSize, FlowControl) of
        {ok, NewFlowControl} ->
            %% Tokens available, send message
            erlmcp_flow_router:send_direct(TargetAgent, Message),
            {ok, NewFlowControl};
        {error, backpressure} ->
            %% No tokens available, queue or reject
            logger:warning("Backpressure detected, message queued"),

            %% Queue message for later
            QueuedFlowControl = erlmcp_flow_backpressure:queue_message(Message, FlowControl),
            {error, backpressure}
    end.

%% Example: Handle backpressure with retry
example_backpressure_handling() ->
    FlowControl = init_agent_flow_control(),

    %% Try to send large batch
    Messages = [{task, I} || I <- lists:seq(1, 1000)],

    lists:foldl(fun(Msg, FC) ->
        case send_with_backpressure(<<"target-agent">>, Msg, FC) of
            {ok, NewFC} ->
                NewFC;
            {error, backpressure} ->
                %% Wait for refill
                timer:sleep(100),
                %% Retry
                {ok, NewFC} = send_with_backpressure(<<"target-agent">>, Msg, FC),
                NewFC
        end
    end, FlowControl, Messages).
```

---

## Example 7: Multi-Agent Workflow Orchestration

```erlang
%% Orchestrate EPIC 9 workflow with multiple agents
-spec orchestrate_epic9_workflow(task_spec()) -> {ok, workflow_id()}.
orchestrate_epic9_workflow(TaskSpec) ->
    WorkflowId = generate_workflow_id(),

    %% Phase 1: Fan-out (parallel research)
    ResearchAgents = [
        <<"agent-erlang-researcher-01">>,
        <<"agent-erlang-researcher-02">>,
        <<"agent-erlang-researcher-03">>
    ],

    [erlmcp_flow_router:send_direct(Agent, #{
        <<"method">> => <<"research">>,
        <<"params">> => #{
            <<"workflow_id">> => WorkflowId,
            <<"phase">> => <<"fan_out">>,
            <<"task">> => TaskSpec
        }
    }) || Agent <- ResearchAgents],

    %% Wait for research results
    ResearchResults = collect_results(ResearchAgents, 5000),

    %% Phase 2: Independent construction (parallel implementation)
    BuildAgents = [
        <<"agent-erlang-otp-developer-01">>,
        <<"agent-erlang-otp-developer-02">>,
        <<"agent-build-engineer-01">>
    ],

    [erlmcp_flow_router:send_direct(Agent, #{
        <<"method">> => <<"implement">>,
        <<"params">> => #{
            <<"workflow_id">> => WorkflowId,
            <<"phase">> => <<"independent_construction">>,
            <<"research">> => ResearchResults,
            <<"task">> => TaskSpec
        }
    }) || Agent <- BuildAgents],

    %% Phase 3: Collision detection (code review)
    ReviewAgent = <<"agent-code-reviewer-01">>,

    erlmcp_flow_router:send_direct(ReviewAgent, #{
        <<"method">> => <<"review">>,
        <<"params">> => #{
            <<"workflow_id">> => WorkflowId,
            <<"phase">> => <<"collision_detection">>,
            <<"implementations">> => collect_results(BuildAgents, 10000)
        }
    }),

    %% Phase 4: Convergence (merge and refactor)
    RefactorAgent = <<"agent-erlang-architect-01">>,

    erlmcp_flow_router:send_direct(RefactorAgent, #{
        <<"method">> => <<"refactor">>,
        <<"params">> => #{
            <<"workflow_id">> => WorkflowId,
            <<"phase">> => <<"convergence">>
        }
    }),

    {ok, WorkflowId}.

%% Collect results from multiple agents
-spec collect_results([agent_id()], timeout()) -> [result()].
collect_results(Agents, Timeout) ->
    Deadline = erlang:monotonic_time(millisecond) + Timeout,

    collect_results_loop(Agents, Deadline, []).

collect_results_loop([], _Deadline, Acc) ->
    lists:reverse(Acc);
collect_results_loop(Agents, Deadline, Acc) ->
    Remaining = Deadline - erlang:monotonic_time(millisecond),

    case Remaining > 0 of
        true ->
            receive
                {result, AgentId, Result} ->
                    collect_results_loop(Agents -- [AgentId], Deadline, [Result | Acc])
            after Remaining ->
                logger:warning("Timeout waiting for results from ~p", [Agents]),
                lists:reverse(Acc)
            end;
        false ->
            lists:reverse(Acc)
    end.
```

---

## Example 8: Performance Monitoring

```erlang
%% Monitor agent performance with OpenTelemetry
-spec monitor_agent_performance(agent_id(), fun(() -> term())) -> term().
monitor_agent_performance(AgentId, Task) ->
    TraceId = generate_trace_id(),

    otel_tracer:with_span(<<"agent.task">>, #{
        <<"agent.id">> => AgentId,
        <<"trace.id">> => TraceId
    }, fun() ->
        StartTime = erlang:monotonic_time(microsecond),

        Result = Task(),

        EndTime = erlang:monotonic_time(microsecond),
        Duration = EndTime - StartTime,

        %% Record metrics
        erlmcp_metrics:histogram(<<"agent.task.duration">>, Duration, #{
            <<"agent_id">> => AgentId
        }),

        %% Update load counter
        erlmcp_flow_registry:update_agent_load(AgentId, -1),

        Result
    end).

%% Example: Monitor gen_server implementation task
example_monitor_task() ->
    AgentId = <<"agent-erlang-otp-developer-01">>,

    monitor_agent_performance(AgentId, fun() ->
        %% Simulate gen_server implementation
        implement_gen_server(<<"src/my_server.erl">>)
    end).
```

---

## Example 9: Error Handling & Recovery

```erlang
%% Handle agent crash with automatic recovery
-spec handle_agent_crash(agent_id(), reason()) -> ok.
handle_agent_crash(AgentId, Reason) ->
    logger:error("Agent ~s crashed: ~p", [AgentId, Reason]),

    %% gproc automatically unregistered the agent

    %% Restart agent via supervisor
    case erlmcp_flow_agent_sup:restart_agent(AgentId) of
        {ok, NewPid} ->
            logger:info("Agent ~s restarted with pid ~p", [AgentId, NewPid]),

            %% Re-register with same ID
            erlmcp_flow_registry:register_agent(AgentId, NewPid, #{
                type => get_agent_type(AgentId),
                capabilities => get_agent_capabilities(AgentId)
            }),

            %% Retry pending tasks
            retry_pending_tasks(AgentId);
        {error, restart_failed} ->
            logger:error("Failed to restart agent ~s", [AgentId]),
            alert_operator(AgentId, restart_failed)
    end,

    ok.

%% Retry pending tasks after agent restart
-spec retry_pending_tasks(agent_id()) -> ok.
retry_pending_tasks(AgentId) ->
    PendingTasks = erlmcp_flow_router:get_pending_tasks(AgentId),

    lists:foreach(fun(Task) ->
        case maps:get(<<"retries">>, Task, 0) of
            Retries when Retries < 3 ->
                %% Retry task
                UpdatedTask = maps:put(<<"retries">>, Retries + 1, Task),
                erlmcp_flow_router:send_direct(AgentId, UpdatedTask);
            _ ->
                %% Max retries exceeded, mark as failed
                logger:error("Task failed after 3 retries: ~p", [Task]),
                mark_task_failed(Task)
        end
    end, PendingTasks).
```

---

## Example 10: Integration with erlmcp Server/Client

```erlang
%% Integrate flow transport with erlmcp_server
-spec start_server_with_flow() -> {ok, pid()}.
start_server_with_flow() ->
    %% Start erlmcp server
    {ok, ServerPid} = erlmcp_server:start_link(#{
        server_id => flow_server,
        capabilities => #{
            tools => #{enabled => true},
            resources => #{enabled => true}
        }
    }),

    %% Start flow transport
    {ok, FlowPid} = erlmcp_flow_transport:start_link(flow_transport, #{
        agent_id => <<"server-agent-01">>,
        agent_type => <<"mcp-server">>,
        capabilities => [<<"tool_execution">>, <<"resource_management">>],
        server_pid => ServerPid
    }),

    %% Bind transport to server
    erlmcp_registry:bind_transport_to_server(flow_transport, flow_server),

    {ok, ServerPid}.

%% Handle tool call via flow
-spec handle_tool_call_via_flow(tool_name(), tool_arguments()) -> {ok, result()}.
handle_tool_call_via_flow(ToolName, Arguments) ->
    %% Find agent capable of executing tool
    case erlmcp_flow_registry:find_agents_by_capability(ToolName) of
        [AgentPid | _] ->
            AgentId = erlmcp_flow_registry:agent_id_from_pid(AgentPid),

            %% Send tool execution request
            erlmcp_flow_router:send_direct(AgentId, #{
                <<"method">> => <<"tool/execute">>,
                <<"params">> => #{
                    <<"name">> => ToolName,
                    <<"arguments">> => Arguments
                }
            }),

            %% Wait for result
            receive
                {tool_result, Result} -> {ok, Result}
            after 5000 ->
                {error, timeout}
            end;
        [] ->
            {error, no_capable_agent}
    end.
```

---

## Performance Examples

### Benchmark: 60 Agents, 100K Messages

```erlang
%% Benchmark full system with 60 agents
benchmark_full_system() ->
    %% Setup
    {ok, _RegPid} = erlmcp_flow_registry:start_link(),
    {ok, _RouterPid} = erlmcp_flow_router:start_link(),

    %% Register 60 agents (20 each of 3 types)
    Agents = lists:flatten([
        [register_agent(<<"erlang-otp-developer">>, I) || I <- lists:seq(1, 20)],
        [register_agent(<<"erlang-test-engineer">>, I) || I <- lists:seq(21, 40)],
        [register_agent(<<"build-engineer">>, I) || I <- lists:seq(41, 60)]
    ]),

    %% Send 100K messages randomly
    Messages = 100000,

    {Time, _} = timer:tc(fun() ->
        [erlmcp_flow_router:send_direct(
            random_agent(Agents),
            {task, I}
        ) || I <- lists:seq(1, Messages)]
    end),

    Throughput = (Messages * 1000000) div Time,
    AvgLatency = Time div Messages,

    io:format("~n=== Full System Benchmark ===~n"),
    io:format("Agents: 60~n"),
    io:format("Messages: ~p~n", [Messages]),
    io:format("Throughput: ~p msg/sec~n", [Throughput]),
    io:format("Average latency: ~pμs~n", [AvgLatency]),

    %% Cleanup
    [exit(Pid, kill) || {_AgentId, Pid} <- Agents].
```

---

## Summary

These examples demonstrate:

1. **O(log N) registry operations** with gproc
2. **Direct messaging** for task assignment
3. **Broadcast** for notifications
4. **Gossip** for state synchronization
5. **Transport bridging** (stdio, TCP, HTTP)
6. **Flow control** with backpressure
7. **Multi-agent workflows** (EPIC 9)
8. **Performance monitoring** with OpenTelemetry
9. **Error handling** and recovery
10. **Integration** with existing erlmcp infrastructure

**Key Takeaways**:
- All messaging patterns use gproc for O(log N) routing
- Transport behavior extension maintains compatibility
- Flow control prevents overload
- Supervision ensures resilience
- Observability built-in with OTEL

**Files**:
- `/home/user/erlmcp/docs/erlmcp-flow-architecture.md` - Architecture design
- `/home/user/erlmcp/docs/erlmcp-flow-implementation-plan.md` - Implementation plan
- `/home/user/erlmcp/docs/erlmcp-flow-examples.md` - Code examples
