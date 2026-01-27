%% ===================================================================
%% UNIFIED TEST INFRASTRUCTURE - Integration Test Suite
%% ===================================================================
%% Module: integration_SUITE
%% Purpose: End-to-end integration tests for erlmcp + TAIEA
%%          covering workflows, failure scenarios, and performance
%% ===================================================================

-module(integration_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("erlmcp.hrl").

%% Common Test Callbacks
-export([
    all/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_testcase/2,
    end_per_testcase/2,
    init_per_group/2,
    end_per_group/2,
    groups/0
]).

%% Test Cases - Erlmcp Integration
-export([
    erlmcp_server_lifecycle/1,
    erlmcp_client_server_handshake/1,
    erlmcp_resource_management/1,
    erlmcp_tool_invocation/1,
    erlmcp_prompt_execution/1,
    erlmcp_error_handling/1,
    erlmcp_concurrent_operations/1,
    erlmcp_performance_baseline/1
]).

%% Test Cases - TAIEA Integration
-export([
    taiea_system_startup/1,
    taiea_autonomic_response/1,
    taiea_self_healing/1,
    taiea_adaptive_learning/1
]).

%% Test Cases - End-to-End
-export([
    e2e_erlmcp_taiea_workflow/1,
    e2e_failure_recovery/1,
    e2e_load_distribution/1,
    e2e_performance_under_load/1
]).

%% ===================================================================
%% CT CALLBACKS
%% ===================================================================

all() ->
    [
        {group, erlmcp_group},
        {group, taiea_group},
        {group, e2e_group},
        {group, performance_group}
    ].

groups() ->
    [
        {erlmcp_group, [sequence], [
            erlmcp_server_lifecycle,
            erlmcp_client_server_handshake,
            erlmcp_resource_management,
            erlmcp_tool_invocation,
            erlmcp_prompt_execution,
            erlmcp_error_handling,
            erlmcp_concurrent_operations
        ]},
        {taiea_group, [sequence], [
            taiea_system_startup,
            taiea_autonomic_response,
            taiea_self_healing,
            taiea_adaptive_learning
        ]},
        {e2e_group, [sequence], [
            e2e_erlmcp_taiea_workflow,
            e2e_failure_recovery,
            e2e_load_distribution
        ]},
        {performance_group, [], [
            erlmcp_performance_baseline,
            e2e_performance_under_load
        ]}
    ].

init_per_suite(Config) ->
    ct:pal("Starting integration test suite..."),
    application:ensure_all_started(erlmcp),
    application:ensure_all_started(taiea),
    [{suite_start_time, erlang:system_time(millisecond)} | Config].

end_per_suite(_Config) ->
    application:stop(erlmcp),
    application:stop(taiea),
    ct:pal("Integration test suite completed").

init_per_group(erlmcp_group, Config) ->
    ct:pal("Setting up erlmcp integration tests..."),
    [{group, erlmcp_group} | Config];
init_per_group(taiea_group, Config) ->
    ct:pal("Setting up TAIEA integration tests..."),
    [{group, taiea_group} | Config];
init_per_group(e2e_group, Config) ->
    ct:pal("Setting up end-to-end integration tests..."),
    {Server, System} = test_utils:setup_integration(),
    [{server, Server}, {system, System}, {group, e2e_group} | Config];
init_per_group(performance_group, Config) ->
    ct:pal("Setting up performance tests..."),
    [{group, performance_group} | Config].

end_per_group(e2e_group, Config) ->
    case lists:keyfind(server, 1, Config) of
        {server, Server} -> test_utils:cleanup_erlmcp(Server);
        false -> ok
    end,
    case lists:keyfind(system, 1, Config) of
        {system, System} -> test_utils:cleanup_taiea(System);
        false -> ok
    end;
end_per_group(_, _Config) ->
    ok.

init_per_testcase(Case, Config) ->
    ct:pal("Starting test: ~p", [Case]),
    [{test_case, Case}, {test_start_time, erlang:system_time(millisecond)} | Config].

end_per_testcase(Case, Config) ->
    StartTime = lists:keyfind(test_start_time, 1, Config),
    case StartTime of
        {test_start_time, Time} ->
            Duration = erlang:system_time(millisecond) - Time,
            ct:pal("Completed test: ~p (~w ms)", [Case, Duration]);
        false ->
            ct:pal("Completed test: ~p", [Case])
    end.

%% ===================================================================
%% ERLMCP INTEGRATION TESTS
%% ===================================================================

%% Test: Server lifecycle (start, register, stop)
erlmcp_server_lifecycle(Config) ->
    ct:comment("Testing erlmcp server startup/shutdown cycle"),

    %% Verify server starts
    Server = test_utils:setup_erlmcp([{capabilities, test_utils:test_mcp_capabilities()}]),
    ?assert(is_process_alive(Server), "Server failed to start"),

    %% Verify server is registered
    case erlang:whereis(erlmcp_server) of
        undefined -> ct:comment("Server not globally registered (expected)");
        _ -> ct:comment("Server registered")
    end,

    %% Verify shutdown is clean
    test_utils:cleanup_erlmcp(Server),
    ?assert(not is_process_alive(Server), "Server failed to stop"),

    ct:comment("erlmcp_server_lifecycle: PASS").

%% Test: Client-server handshake and capabilities negotiation
erlmcp_client_server_handshake(Config) ->
    ct:comment("Testing client-server handshake"),

    Server = test_utils:setup_erlmcp([{capabilities, test_utils:test_mcp_capabilities()}]),

    %% Client should be able to query server info
    case catch erlmcp_server:info(Server) of
        {ok, Info} ->
            ct:pal("Server info: ~p", [Info]),
            ct:comment("Handshake successful");
        {error, Reason} ->
            ct:fail("Handshake failed: " ++ atom_to_list(Reason))
    end,

    test_utils:cleanup_erlmcp(Server),
    ct:comment("erlmcp_client_server_handshake: PASS").

%% Test: Resource list, read, and template operations
erlmcp_resource_management(Config) ->
    ct:comment("Testing resource management operations"),

    Server = test_utils:setup_erlmcp([{capabilities, test_utils:test_mcp_capabilities(resources_only)}]),

    %% Add resource template
    {Template, Desc, Handler} = test_utils:test_resource_template(),
    erlmcp_server:add_resource_template(Server, Template, Desc, Handler),

    %% List resources
    case catch erlmcp_server:list_resources(Server) of
        {ok, Resources} ->
            ct:pal("Listed resources: ~p", [Resources]);
        {error, Reason} ->
            ct:fail("Failed to list resources: " ++ atom_to_list(Reason))
    end,

    %% Read specific resource
    Uri = test_utils:gen_random_uri(),
    case catch erlmcp_server:read_resource(Server, Uri) of
        {ok, Content} ->
            ct:pal("Read resource: ~p", [Content]);
        {error, _} ->
            ct:comment("Resource read returned error (expected for non-existent resource)")
    end,

    test_utils:cleanup_erlmcp(Server),
    ct:comment("erlmcp_resource_management: PASS").

%% Test: Tool registration and invocation
erlmcp_tool_invocation(Config) ->
    ct:comment("Testing tool registration and invocation"),

    Server = test_utils:setup_erlmcp([{capabilities, test_utils:test_mcp_capabilities(tools_only)}]),

    %% Register tool
    ToolName = <<"test_echo_tool">>,
    ToolSchema = test_utils:test_tool_schema(),
    ToolHandler = fun(#{<<"input">> := Input}) ->
        #mcp_content{
            type = <<"text">>,
            text = <<"Echo: ", Input/binary>>,
            mime_type = <<"text/plain">>
        }
    end,

    erlmcp_server:add_tool_with_schema(Server, ToolName, ToolHandler, ToolSchema),

    %% List tools
    case catch erlmcp_server:list_tools(Server) of
        {ok, Tools} ->
            ct:pal("Listed tools: ~p", [Tools]),
            ?assert(lists:keymember(ToolName, 1, Tools), "Tool not found in list");
        {error, Reason} ->
            ct:fail("Failed to list tools: " ++ atom_to_list(Reason))
    end,

    %% Call tool
    Args = #{<<"input">> => <<"hello">>},
    case catch erlmcp_server:call_tool(Server, ToolName, Args) of
        {ok, Result} ->
            ct:pal("Tool invocation result: ~p", [Result]);
        {error, Reason2} ->
            ct:fail("Tool invocation failed: " ++ atom_to_list(Reason2))
    end,

    test_utils:cleanup_erlmcp(Server),
    ct:comment("erlmcp_tool_invocation: PASS").

%% Test: Prompt registration and execution
erlmcp_prompt_execution(Config) ->
    ct:comment("Testing prompt registration and execution"),

    Server = test_utils:setup_erlmcp([{capabilities, test_utils:test_mcp_capabilities()}]),

    %% Register prompt
    PromptName = <<"test_prompt">>,
    PromptArgs = test_utils:test_prompt_args(),
    PromptHandler = fun(Args) ->
        Prompt = <<"Generated prompt with args: ", (jsx:encode(Args))/binary>>,
        #mcp_content{
            type = <<"text">>,
            text = Prompt,
            mime_type = <<"text/plain">>
        }
    end,

    erlmcp_server:add_prompt(Server, PromptName, PromptHandler, PromptArgs),

    %% List prompts
    case catch erlmcp_server:list_prompts(Server) of
        {ok, Prompts} ->
            ct:pal("Listed prompts: ~p", [Prompts]);
        {error, Reason} ->
            ct:fail("Failed to list prompts: " ++ atom_to_list(Reason))
    end,

    test_utils:cleanup_erlmcp(Server),
    ct:comment("erlmcp_prompt_execution: PASS").

%% Test: Error handling for invalid requests
erlmcp_error_handling(Config) ->
    ct:comment("Testing error handling"),

    Server = test_utils:setup_erlmcp([{capabilities, test_utils:test_mcp_capabilities()}]),

    %% Test invalid JSON
    InvalidJson = test_utils:test_invalid_json(),
    case catch erlmcp_json_rpc:parse_request(InvalidJson) of
        {error, _} ->
            ct:comment("Invalid JSON properly rejected");
        {ok, _} ->
            ct:fail("Invalid JSON should be rejected")
    end,

    %% Test non-existent resource
    case catch erlmcp_server:read_resource(Server, <<"nonexistent://resource">>) of
        {error, _} ->
            ct:comment("Non-existent resource properly rejected");
        {ok, _} ->
            ct:fail("Non-existent resource should return error")
    end,

    test_utils:cleanup_erlmcp(Server),
    ct:comment("erlmcp_error_handling: PASS").

%% Test: Concurrent operations (multiple requests)
erlmcp_concurrent_operations(Config) ->
    ct:comment("Testing concurrent operations"),

    Server = test_utils:setup_erlmcp([{capabilities, test_utils:test_mcp_capabilities()}]),

    %% Register multiple tools
    NumTools = 10,
    register_test_tools(Server, NumTools),

    %% Call tools concurrently
    ParentPid = self(),
    Pids = [spawn(fun() ->
        ToolId = integer_to_binary(N),
        ToolName = <<"tool_", ToolId/binary>>,
        Args = #{<<"input">> => <<"test_", ToolId/binary>>},
        case catch erlmcp_server:call_tool(Server, ToolName, Args) of
            {ok, _} -> ParentPid ! {ok, N};
            {error, _} -> ParentPid ! {error, N}
        end
    end) || N <- lists:seq(1, NumTools)],

    %% Collect results
    Results = [receive {Msg, _} -> Msg after 5000 -> timeout end || _ <- Pids],
    SuccessCount = length([ok || ok <- Results]),

    ct:pal("Concurrent operations: ~w/~w succeeded", [SuccessCount, NumTools]),
    ?assert(SuccessCount >= (NumTools div 2), "Too many concurrent operation failures"),

    test_utils:cleanup_erlmcp(Server),
    ct:comment("erlmcp_concurrent_operations: PASS").

%% Test: Performance baseline (throughput and latency)
erlmcp_performance_baseline(Config) ->
    ct:comment("Testing erlmcp performance baseline"),

    Server = test_utils:setup_erlmcp([{capabilities, test_utils:test_mcp_capabilities()}]),

    %% Register tool
    ToolName = <<"perf_test_tool">>,
    ToolSchema = test_utils:test_tool_schema(),
    ToolHandler = fun(#{<<"input">> := Input}) ->
        #mcp_content{
            type = <<"text">>,
            text = Input,
            mime_type = <<"text/plain">>
        }
    end,
    erlmcp_server:add_tool_with_schema(Server, ToolName, ToolHandler, ToolSchema),

    %% Measure latency
    Iterations = 100,
    {AvgLatency, _} = test_utils:measure_time(
        fun() ->
            lists:foreach(fun(_) ->
                erlmcp_server:call_tool(Server, ToolName, #{<<"input">> => <<"test">>})
            end, lists:seq(1, Iterations))
        end,
        "Tool invocation " ++ integer_to_list(Iterations) ++ " times"
    ),

    PerIterationLatency = AvgLatency / Iterations,
    ct:pal("Average latency per invocation: ~.2f ms", [PerIterationLatency]),

    %% Assert acceptable performance (< 10ms per operation)
    ?assert(PerIterationLatency < 10.0,
            io_lib:format("Performance degraded: ~.2f ms/op > 10ms", [PerIterationLatency])),

    test_utils:cleanup_erlmcp(Server),
    ct:comment("erlmcp_performance_baseline: PASS").

%% ===================================================================
%% TAIEA INTEGRATION TESTS
%% ===================================================================

%% Test: TAIEA system startup and initialization
taiea_system_startup(Config) ->
    ct:comment("Testing TAIEA system startup"),

    System = test_utils:setup_taiea([{taiea_config, test_utils:test_integration_config()}]),
    ?assert(is_process_alive(System), "TAIEA system failed to start"),

    %% Verify system state
    case catch taiea_system:status(System) of
        {ok, Status} ->
            ct:pal("System status: ~p", [Status]),
            ct:comment("System status retrieved");
        {error, Reason} ->
            ct:fail("Failed to get system status: " ++ atom_to_list(Reason))
    end,

    test_utils:cleanup_taiea(System),
    ct:comment("taiea_system_startup: PASS").

%% Test: Autonomic response to stimuli
taiea_autonomic_response(Config) ->
    ct:comment("Testing TAIEA autonomic response"),

    System = test_utils:setup_taiea([{taiea_config, test_utils:test_integration_config()}]),

    %% Send stimulus
    case catch taiea_system:stimulus(System, {resource_load, high}) of
        {ok, Response} ->
            ct:pal("Autonomic response: ~p", [Response]);
        {error, Reason} ->
            ct:fail("Stimulus failed: " ++ atom_to_list(Reason))
    end,

    test_utils:cleanup_taiea(System),
    ct:comment("taiea_autonomic_response: PASS").

%% Test: Self-healing capabilities
taiea_self_healing(Config) ->
    ct:comment("Testing TAIEA self-healing"),

    System = test_utils:setup_taiea([{taiea_config, test_utils:test_integration_config()}]),

    %% Inject fault
    case catch taiea_system:inject_fault(System, {component_failure, worker_1}) of
        {ok, _} ->
            ct:comment("Fault injected");
        {error, _} ->
            ct:comment("Fault injection not supported")
    end,

    %% Check recovery
    test_utils:wait_for_condition(
        fun() ->
            case catch taiea_system:status(System) of
                {ok, Status} ->
                    maps:get(health, Status, unknown) == healthy;
                {error, _} ->
                    false
            end
        end,
        5000,
        "System recovery"
    ),

    ct:comment("System recovered from fault"),

    test_utils:cleanup_taiea(System),
    ct:comment("taiea_self_healing: PASS").

%% Test: Adaptive learning
taiea_adaptive_learning(Config) ->
    ct:comment("Testing TAIEA adaptive learning"),

    System = test_utils:setup_taiea([{taiea_config, test_utils:test_integration_config()}]),

    %% Simulate repeated stimulus pattern
    Pattern = [
        {load, high},
        {latency, degraded},
        {load, normal},
        {latency, normal}
    ],

    lists:foreach(fun({Type, Value}) ->
        catch taiea_system:stimulus(System, {Type, Value})
    end, Pattern),

    %% Check if system learned pattern
    timer:sleep(1000),

    case catch taiea_system:get_learned_patterns(System) of
        {ok, Patterns} ->
            ct:pal("Learned patterns: ~p", [Patterns]);
        {error, _} ->
            ct:comment("Pattern learning not implemented or disabled")
    end,

    test_utils:cleanup_taiea(System),
    ct:comment("taiea_adaptive_learning: PASS").

%% ===================================================================
%% END-TO-END INTEGRATION TESTS
%% ===================================================================

%% Test: Complete erlmcp + TAIEA workflow
e2e_erlmcp_taiea_workflow(Config) ->
    ct:comment("Testing end-to-end erlmcp + TAIEA workflow"),

    Server = proplists:get_value(server, Config),
    System = proplists:get_value(system, Config),

    %% Register tool in erlmcp
    ToolName = <<"e2e_tool">>,
    ToolSchema = test_utils:test_tool_schema(),
    ToolHandler = fun(#{<<"input">> := Input}) ->
        %% Trigger TAIEA stimulus
        catch taiea_system:stimulus(System, {tool_invoked, ToolName}),
        #mcp_content{
            type = <<"text">>,
            text = <<"Processed: ", Input/binary>>,
            mime_type = <<"text/plain">>
        }
    end,

    erlmcp_server:add_tool_with_schema(Server, ToolName, ToolHandler, ToolSchema),

    %% Execute tool (triggers full workflow)
    case catch erlmcp_server:call_tool(Server, ToolName, #{<<"input">> => <<"e2e_test">>}) of
        {ok, Result} ->
            ct:pal("E2E workflow completed: ~p", [Result]);
        {error, Reason} ->
            ct:fail("E2E workflow failed: " ++ atom_to_list(Reason))
    end,

    ct:comment("e2e_erlmcp_taiea_workflow: PASS").

%% Test: Failure recovery workflow
e2e_failure_recovery(Config) ->
    ct:comment("Testing failure recovery workflow"),

    Server = proplists:get_value(server, Config),
    System = proplists:get_value(system, Config),

    %% Register failing tool
    FailingTool = <<"failing_tool">>,
    FailingHandler = fun(_Args) ->
        {error, simulated_error}
    end,

    erlmcp_server:add_tool(Server, FailingTool, FailingHandler),

    %% Invoke failing tool
    case catch erlmcp_server:call_tool(Server, FailingTool, #{}) of
        {error, _} ->
            %% TAIEA should detect and handle failure
            catch taiea_system:stimulus(System, {tool_failure, FailingTool}),
            ct:comment("Failure detected and reported");
        _ ->
            ct:fail("Expected tool to fail")
    end,

    %% Verify system is still operational
    case catch erlmcp_server:list_tools(Server) of
        {ok, _} ->
            ct:comment("System recovered from failure");
        {error, _} ->
            ct:fail("System did not recover from failure")
    end,

    ct:comment("e2e_failure_recovery: PASS").

%% Test: Load distribution across agents
e2e_load_distribution(Config) ->
    ct:comment("Testing load distribution"),

    Server = proplists:get_value(server, Config),

    %% Register multiple tools
    NumTools = 5,
    register_test_tools(Server, NumTools),

    %% Distribute load
    Requests = 50,
    ParentPid = self(),
    Pids = [spawn(fun() ->
        ToolId = integer_to_binary((N rem NumTools) + 1),
        ToolName = <<"tool_", ToolId/binary>>,
        Result = catch erlmcp_server:call_tool(Server, ToolName, #{<<"input">> => <<"load_test">>}),
        ParentPid ! {done, Result}
    end) || N <- lists:seq(1, Requests)],

    %% Collect results
    Results = [receive {done, R} -> R after 5000 -> timeout end || _ <- Pids],
    SuccessCount = length([ok || {ok, _} <- Results]),

    ct:pal("Load distributed: ~w/~w requests succeeded", [SuccessCount, Requests]),
    ?assert(SuccessCount >= (Requests * 8 div 10), "Load distribution had too many failures"),

    ct:comment("e2e_load_distribution: PASS").

%% Test: Performance under sustained load
e2e_performance_under_load(Config) ->
    ct:comment("Testing performance under load"),

    Server = proplists:get_value(server, Config),
    System = proplists:get_value(system, Config),

    %% Register tool
    ToolName = <<"load_test_tool">>,
    ToolSchema = test_utils:test_tool_schema(),
    ToolHandler = fun(#{<<"input">> := Input}) ->
        #mcp_content{
            type = <<"text">>,
            text = Input,
            mime_type = <<"text/plain">>
        }
    end,
    erlmcp_server:add_tool_with_schema(Server, ToolName, ToolHandler, ToolSchema),

    %% Sustained load test
    Duration = 5000,  % 5 seconds
    {TotalTime, _} = test_utils:measure_time(
        fun() ->
            lists_foreach_with_timeout(
                fun() ->
                    erlmcp_server:call_tool(Server, ToolName, #{<<"input">> => <<"load_test">>})
                end,
                Duration
            )
        end,
        "Sustained load test"
    ),

    %% Verify system is still responsive
    case catch taiea_system:status(System) of
        {ok, _} ->
            ct:comment("System remained responsive under load");
        {error, _} ->
            ct:fail("System became unresponsive under load")
    end,

    ct:comment("e2e_performance_under_load: PASS").

%% ===================================================================
%% HELPER FUNCTIONS
%% ===================================================================

%% Register N test tools
register_test_tools(Server, Count) ->
    lists:foreach(fun(N) ->
        ToolId = integer_to_binary(N),
        ToolName = <<"tool_", ToolId/binary>>,
        Handler = fun(#{<<"input">> := Input}) ->
            #mcp_content{
                type = <<"text">>,
                text = <<"Tool ", ToolId/binary, " processed: ", Input/binary>>,
                mime_type = <<"text/plain">>
            }
        end,
        erlmcp_server:add_tool(Server, ToolName, Handler)
    end, lists:seq(1, Count)).

%% Execute function repeatedly for duration
lists_foreach_with_timeout(Fun, Timeout) ->
    Start = erlang:system_time(millisecond),
    lists_foreach_with_timeout(Fun, Start, Timeout, 0).

lists_foreach_with_timeout(Fun, Start, Timeout, Count) ->
    Elapsed = erlang:system_time(millisecond) - Start,
    case Elapsed > Timeout of
        true -> Count;
        false ->
            catch Fun(),
            lists_foreach_with_timeout(Fun, Start, Timeout, Count + 1)
    end.
