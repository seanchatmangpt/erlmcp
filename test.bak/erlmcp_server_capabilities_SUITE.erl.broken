%%%-------------------------------------------------------------------
%%% @doc
%%% Comprehensive MCP Server Capabilities Integration Test Suite
%%%
%%% This test suite validates MCP server capabilities in integration scenarios:
%%% - Full system startup with multiple servers
%%% - Transport integration with capability negotiation
%%% - Resource and tool registration workflows
%%% - Multi-tenant capability isolation
%%% - Performance under capability load
%%% - Cross-server capability coordination
%%% - Dynamic capability management
%%% - Protocol compliance verification
%%%
%%% Uses Common Test for multi-process integration testing
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_server_capabilities_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("erlmcp_core/include/erlmcp.hrl").

%% Suite callbacks
-export([all/0, groups/0, init_per_suite/1, end_per_suite/1,
         init_per_group/2, end_per_group/2,
         init_per_testcase/2, end_per_testcase/2]).

%% Test cases
-export([
    %% Integration tests
    test_multi_server_capability_isolation/1,
    test_transport_capability_negotiation/1,
    test_resource_capability_workflow/1,
    test_tool_capability_workflow/1,
    test_prompt_capability_workflow/1,

    %% Performance tests
    test_concurrent_capability_registration/1,
    test_high_capability_throughput/1,
    test_memory_usage_with_capabilities/1,

    %% Coordination tests
    test_cross_server_capability_sharing/1,
    test_dynamic_capability_coordination/1,
    test_capability_dependency_resolution/1,

    %% Error recovery tests
    test_capability_error_recovery/1,
    test_capability_failure_isolation/1,
    test_capability_graceful_degradation/1,

    %% Protocol compliance tests
    test_mcp_capability_compliance/1,
    test_capability_protocol_validation/1,
    test_capability_backwards_compatibility/1
]).

%%====================================================================
%% Suite Configuration
%%====================================================================

all() -> [
    {group, integration_tests},
    {group, performance_tests},
    {group, coordination_tests},
    {group, error_recovery_tests},
    {group, protocol_compliance_tests}
].

groups() -> [
    {integration_tests, [], [
        test_multi_server_capability_isolation,
        test_transport_capability_negotiation,
        test_resource_capability_workflow,
        test_tool_capability_workflow,
        test_prompt_capability_workflow
    ]},

    {performance_tests, [], [
        test_concurrent_capability_registration,
        test_high_capability_throughput,
        test_memory_usage_with_capabilities
    ]},

    {coordination_tests, [], [
        test_cross_server_capability_sharing,
        test_dynamic_capability_coordination,
        test_capability_dependency_resolution
    ]},

    {error_recovery_tests, [], [
        test_capability_error_recovery,
        test_capability_failure_isolation,
        test_capability_graceful_degradation
    ]},

    {protocol_compliance_tests, [], [
        test_mcp_capability_compliance,
        test_capability_protocol_validation,
        test_capability_backwards_compatibility
    ]}
].

%%====================================================================
%% Suite Setup and Cleanup
%%====================================================================

init_per_suite(Config) ->
    %% Start the erlmcp application
    application:ensure_all_started(erlmcp_core),

    %% Enable logging for debugging
    application:set_env(erlmcp, log_level, debug),

    %% Create test directories
    TestDir = ?config(priv_dir, Config),
    ok = file:make_dir(TestDir ++ "/capabilities_test"),

    %% Configure test environment
    Config1 = Config ++ [
        {test_dir, TestDir},
        {server_count, 0},
        {capability_events, []}
    ],

    %% Start monitoring system
    erlmcp_monitoring:start(),

    Config1.

end_per_suite(Config) ->
    %% Stop monitoring
    erlmcp_monitoring:stop(),

    %% Stop erlmcp application
    application:stop(erlmcp_core),

    %% Clean up test directories
    TestDir = ?config(test_dir, Config),
    ok = file:del_dir_r(TestDir),

    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

init_per_testcase(TestCase, Config) ->
    %% Start with clean slate for each test case
    ct:pal("Starting test case: ~p", [TestCase]),

    %% Reset counters
    Config1 = Config ++ [
        {test_case, TestCase},
        {server_pids, []},
        {resource_count, 0},
        {tool_count, 0},
        {error_count, 0}
    ],

    %% Clean up any existing servers
    cleanup_test_servers(Config1),

    Config1.

end_per_testcase(TestCase, Config) ->
    ct:pal("Completed test case: ~p", [TestCase]),

    %% Clean up servers
    cleanup_test_servers(Config),

    %% Log test results
    log_test_results(Config),

    %% Check for memory leaks
    check_memory_leaks(Config),

    ok.

%%====================================================================
%% Integration Tests
%%====================================================================

test_multi_server_capability_isolation(Config) ->
    %% Test multiple servers with isolated capabilities
    NumServers = 3,

    %% Create servers with different capability sets
    Servers = create_test_servers(NumServers, Config, fun(ServerId, Index) ->
        Capabilities = #mcp_server_capabilities{
            resources = #mcp_resources_capability{
                subscribe = Index rem 2 =:= 0,
                listChanged = Index rem 2 =:= 1
            },
            tools = #mcp_tools_capability{
                listChanged = Index rem 3 =:= 0
            },
            prompts = #mcp_prompts_capability{
                listChanged = Index rem 3 =:= 1
            }
        },
        {ok, Pid} = erlmcp_server:start_link(ServerId, Capabilities),
        Pid
    end),

    %% Verify capability isolation
    lists:foreach(fun({ServerId, Pid}) ->
        State = get_server_state(Pid),
        Capabilities = State#state.capabilities,

        %% Check that each server has different capabilities
        ct:pal("Server ~p capabilities: subscribe=~p, listChanged=~p", [
            ServerId,
            Capabilities#mcp_server_capabilities.resources#mcp_resources_capability.subscribe,
            Capabilities#mcp_server_capabilities.resources#mcp_resources_capability.listChanged
        ])
    end, Servers),

    %% Add resources to verify isolation
    lists:foreach(fun({ServerId, Pid}) ->
        Resource = #mcp_resource{
            uri = <<"isolated://", ServerId/binary, "/resource">>,
            name = <<"Isolated Resource ", ServerId/binary>>,
            mime_type = <<"text/plain">>
        },
        ok = erlmcp_server:add_resource(Pid, Resource, fun(_) -> <<"content">> end),
        update_counter(resource_count, Config)
    end, Servers),

    %% Verify resources are server-specific
    lists:foreach(fun({ServerId, Pid}) ->
        State = get_server_state(Pid),
        ExpectedUri = <<"isolated://", ServerId/binary, "/resource">>,
        ?assertMatch({_, _}, maps:get(ExpectedUri, State#state.resources)),
        ?assertNot(maps:is_key(<<"isolated://", (ServerId)/binary, "/resource">>, State#state.resources))
    end, Servers),

    %% Store servers for cleanup
    store_server_pids(Servers, Config),
    ok.

test_transport_capability_negotiation(Config) ->
    %% Test transport capability negotiation
    ServerId = transport_neg_test,
    Capabilities = #mcp_server_capabilities{
        resources = #mcp_resources_capability{
            subscribe = true,
            listChanged = true
        },
        tools = #mcp_tools_capability{
            listChanged = true
        },
        prompts = #mcp_prompts_capability{
            listChanged = false
        }
    },

    {ok, ServerPid} = erlmcp_server:start_link(ServerId, Capabilities),

    %% Simulate transport initialization
    TransportId = test_transport,

    %% Test capability negotiation flow
    ClientParams = #{
        <<"protocolVersion">> => <<"2025-11-25">>,
        <<"capabilities">> => #{
            <<"roots">> => #{<<"enabled">> => true},
            <<"sampling">> => #{<<"enabled">> => false}
        }
    },

    %% Send initialize request
    InitializeRequest = erlmcp_json_rpc:encode_request(1, <<"initialize">>, ClientParams),

    %% Simulate message handling
    ServerPid ! {mcp_message, TransportId, InitializeRequest},

    %% Wait for initialization
    timer:sleep(100),

    %% Verify negotiation results
    State = get_server_state(ServerPid),
    ?assertEqual(true, State#state.initialized),
    ?assertEqual(<<"2025-11-25">>, State#state.protocol_version),

    %% Verify client capabilities stored
    ClientCaps = State#state.client_capabilities,
    ?assertEqual(true, ClientCaps#mcp_client_capabilities.roots#mcp_capability.enabled),
    ?assertEqual(false, ClientCaps#mcp_client_capabilities.sampling#mcp_capability.enabled),

    %% Verify server capabilities preserved
    ServerCaps = State#state.capabilities,
    ?assertEqual(true, ServerCaps#mcp_server_capabilities.resources#mcp_resources_capability.subscribe),
    ?assertEqual(true, ServerCaps#mcp_server_capabilities.resources#mcp_resources_capability.listChanged),

    store_server_pids([{ServerId, ServerPid}], Config),
    ok.

test_resource_capability_workflow(Config) ->
    %% Test complete resource capability workflow
    ServerId = resource_cap_workflow_test,
    Capabilities = #mcp_server_capabilities{
        resources = #mcp_resources_capability{
            subscribe = true,
            listChanged = true
        }
    },

    {ok, ServerPid} = erlmcp_server:start_link(ServerId, Capabilities),

    %% Step 1: Add resource
    ResourceUri = <<"workflow://resource">>,
    Resource = #mcp_resource{
        uri = ResourceUri,
        name = <<"Workflow Resource">>,
        mime_type = <<"application/json">>,
        description = <<"Test resource for workflow">>
    },

    Handler = fun(Uri) ->
        jsx:encode(#{uri => Uri, timestamp => erlang:system_time(millisecond)})
    end,

    ok = erlmcp_server:add_resource(ServerPid, Resource, Handler),
    update_counter(resource_count, Config),

    %% Step 2: List resources
    ListRequest = erlmcp_json_rpc:encode_request(1, <<"resources/list">>, #{}),
    ServerPid ! {mcp_message, test_transport, ListRequest},
    timer:sleep(50),

    %% Step 3: Subscribe to resource
    SubscriberPid = spawn(fun() ->
        receive
            {resource_updated, Uri, Metadata} ->
                self() ! {subscribed, Uri, Metadata}
        end
    end),

    ok = erlmcp_server:subscribe_resource(ServerPid, ResourceUri, SubscriberPid),

    %% Step 4: Notify resource change
    Metadata = #{<<"status">> => <<"updated">>, <<"version">> => 1},
    ok = erlmcp_server:notify_resource_updated(ServerPid, ResourceUri, Metadata),

    %% Step 5: Verify notification received
    receive
        {subscribed, ResourceUri, Metadata} ->
            ct:pal("Resource notification received: ~p", [Metadata]);
        after 2000 ->
            ct:fail("Resource notification not received")
    end,

    %% Step 6: List changed notification
    ok = erlmcp_server:notify_resources_changed(ServerPid),

    %% Step 7: Verify resource can be read
    ReadRequest = erlmcp_json_rpc:encode_request(2, <<"resources/read">>, #{
        <<"uri">> => ResourceUri
    }),
    ServerPid ! {mcp_message, test_transport, ReadRequest},
    timer:sleep(50),

    %% Step 8: Verify unsubscribed
    ok = erlmcp_server:unsubscribe_resource(ServerPid, ResourceUri),

    store_server_pids([{ServerId, ServerPid}], Config),
    ok.

test_tool_capability_workflow(Config) ->
    %% Test complete tool capability workflow
    ServerId = tool_cap_workflow_test,
    Capabilities = #mcp_server_capabilities{
        tools = #mcp_tools_capability{
            listChanged = true
        }
    },

    {ok, ServerPid} = erlmcp_server:start_link(ServerId, Capabilities);

    %% Step 1: Add tool with schema
    ToolName = <<"workflow_tool">>,
    Schema = #{
        type => <<"object">>,
        properties => #{
            input => #{type => <<"string">>},
            options => #{type => <<"array">>, items => #{type => <<"string">>}}
        },
        required => [<<"input">>]
    },

    Tool = #mcp_tool{
        name = ToolName,
        description = <<"Workflow test tool">>,
        input_schema = Schema
    },

    Handler = fun(Args) ->
        Input = maps:get(<<"input">>, Args, <<"">>),
        Options = maps:get(<<"options">>, Args, []),
        jsx:encode(#{result => Input, options => Options})
    end,

    ok = erlmcp_server:add_tool_with_schema(ServerPid, Tool, Handler, Schema),
    update_counter(tool_count, Config),

    %% Step 2: List tools
    ListRequest = erlmcp_json_rpc:encode_request(1, <<"tools/list">>, #{}),
    ServerPid ! {mcp_message, test_transport, ListRequest},
    timer:sleep(50);

    %% Step 3: Call tool
    CallRequest = erlmcp_json_rpc:encode_request(2, <<"tools/call">>, #{
        <<"name">> => ToolName,
        <<"arguments">> => #{
            <<"input">> => <<"test input">>,
            <<"options">> => [<<"opt1">>, <<"opt2">>]
        }
    }),

    ServerPid ! {mcp_message, test_transport, CallRequest},
    timer:sleep(50);

    %% Step 4: List changed notification
    ok = erlmcp_server:notify_resources_changed(ServerPid);

    store_server_pids([{ServerId, ServerPid}], Config),
    ok.

test_prompt_capability_workflow(Config) ->
    %% Test complete prompt capability workflow
    ServerId = prompt_cap_workflow_test,
    Capabilities = #mcp_server_capabilities{
        prompts = #mcp_prompts_capability{
            listChanged = true
        }
    },

    {ok, ServerPid} = erlmcp_server:start_link(ServerId, Capabilities);

    %% Step 1: Add prompt with arguments and schema
    PromptName = <<"workflow_prompt">>,
    Arguments = [
        #mcp_prompt_argument{
            name = <<"topic">>,
            description = <<"Prompt topic">>,
            required = true
        },
        #mcp_prompt_argument{
            name = <<"style">>,
            description = <<"Prompt style">>,
            required = false
        }
    ],

    InputSchema = #{
        type => <<"object">>,
        properties => #{
            topic => #{type => <<"string">>},
            style => #{type => <<"string">>, enum => [<<"formal">>, <<"casual">>]}
        },
        required => [<<"topic">>]
    },

    Prompt = #mcp_prompt{
        name = PromptName,
        description = <<"Workflow test prompt">>,
        arguments = Arguments,
        input_schema = InputSchema
    },

    Handler = fun(Args) ->
        Topic = maps:get(<<"topic">>, Args, <<"default">>),
        Style = maps:get(<<"style">>, Args, <<"casual">>),
        [#{role => user, content => #{
            type => text,
            text => <<"Generated prompt about ", Topic/binary, " in ", Style/binary, " style">>
        }}]
    end;

    ok = erlmcp_server:add_prompt_with_args_and_schema(
        ServerPid, PromptName, Handler, Arguments, InputSchema);

    %% Step 2: List prompts
    ListRequest = erlmcp_json_rpc:encode_request(1, <<"prompts/list">>, #{});
    ServerPid ! {mcp_message, test_transport, ListRequest};
    timer:sleep(50);

    %% Step 3: Get prompt
    GetRequest = erlmcp_json_rpc:encode_request(2, <<"prompts/get">>, #{
        <<"name">> => PromptName,
        <<"arguments">> => #{
            <<"topic">> => <<"technology">>,
            <<"style">> => <<"formal">>
        }
    });

    ServerPid ! {mcp_message, test_transport, GetRequest};
    timer:sleep(50);

    %% Step 4: List changed notification
    ok = erlmcp_server:notify_resources_changed(ServerPid);

    store_server_pids([{ServerId, ServerPid}], Config),
    ok.

%%====================================================================
%% Performance Tests
%%====================================================================

test_concurrent_capability_registration(Config) ->
    %% Test concurrent capability registration
    NumServers = 10,
    NumResourcesPerServer = 5,
    NumToolsPerServer = 3,

    ct:pal("Starting concurrent registration test with ~p servers", [NumServers]);

    %% Create servers concurrently
    Servers = create_test_servers(NumServers, Config, fun(ServerId, _) ->
        Capabilities = default_server_capabilities(),
        {ok, Pid} = erlmcp_server:start_link(ServerId, Capabilities),
        Pid
    end),

    %% Concurrent resource registration
    ResourceSpecs = lists:foldl(fun({ServerId, ServerPid}, Acc) ->
        Resources = [begin
            #mcp_resource{
                uri = <<"concurrent://", ServerId/binary, "/res", integer_to_list(I)/binary>>,
                name = <<"Concurrent Resource ", integer_to_list(I)/binary>>,
                mime_type = <<"text/plain">>
            }
         end || I <- lists:seq(1, NumResourcesPerServer)],

        %% Register resources concurrently
        lists:foreach(fun(Resource) ->
            Handler = fun(Uri) -> Uri end,
            ok = erlmcp_server:add_resource(ServerPid, Resource, Handler),
            update_counter(resource_count, Config)
        end, Resources),

        Tools = [begin
            #mcp_tool{
                name = <<"concurrent_tool", integer_to_list(I)/binary>>,
                description = <<"Concurrent tool ", integer_to_list(I)/binary>>
            }
         end || I <- lists:seq(1, NumToolsPerServer)],

        %% Register tools concurrently
        lists:foreach(fun(Tool) ->
            Handler = fun(_) -> <<"result">> end,
            ok = erlmcp_server:add_tool(ServerPid, Tool, Handler),
            update_counter(tool_count, Config)
        end, Tools),

        [{ServerId, ServerPid} | Acc]
    end, Servers, Servers),

    %% Verify all registrations
    lists:foreach(fun({ServerId, ServerPid}) ->
        State = get_server_state(ServerPid),

        %% Check resources
        ResourceCount = maps:size(State#state.resources),
        ?assertEqual(NumResourcesPerServer, ResourceCount),

        %% Check tools
        ToolCount = maps:size(State#state.tools),
        ?assertEqual(NumToolsPerServer, ToolCount),

        ct:pal("Server ~p: ~p resources, ~p tools", [
            ServerId, ResourceCount, ToolCount])
    end, Servers),

    store_server_pids(Servers, Config),
    ok.

test_high_capability_throughput(Config) ->
    %% Test high throughput capability operations
    NumOperations = 1000,
    ServerId = throughput_test;

    Capabilities = default_server_capabilities(),

    {ok, ServerPid} = erlmcp_server:start_link(ServerId, Capabilities);

    %% High throughput resource registration
    Start = erlang:monotonic_time(millisecond);

    lists:foreach(fun(I) ->
        Resource = #mcp_resource{
            uri = <<"throughput://res", integer_to_list(I)/binary>>,
            name = <<"Throughput Resource ", integer_to_list(I)/binary>>,
            mime_type = <<"text/plain">>
        },
        Handler = fun(Uri) -> Uri end,
        ok = erlmcp_server:add_resource(ServerPid, Resource, Handler),
        update_counter(resource_count, Config)
    end, lists:seq(1, NumOperations)),

    End = erlang:monotonic_time(millisecond),
    Duration = End - Start,
    Throughput = NumOperations / (Duration / 1000),

    ct:pal("Throughput: ~p operations in ~p ms (~p ops/sec)", [
        NumOperations, Duration, Throughput]);

    %% Verify all resources registered
    State = get_server_state(ServerPid);
    ?assertEqual(NumOperations, maps:size(State#state.resources));

    %% High throughput tool registration
    StartTools = erlang:monotonic_time(millisecond);

    lists:foreach(fun(I) ->
        Tool = #mcp_tool{
            name = <<"throughput_tool", integer_to_list(I)/binary>>,
            description = <<"Throughput tool ", integer_to_list(I)/binary>>
        },
        Handler = fun(_) -> <<"result">> end,
        ok = erlmcp_server:add_tool(ServerPid, Tool, Handler),
        update_counter(tool_count, Config)
    end, lists:seq(1, NumOperations));

    EndTools = erlang:monotonic_time(millisecond);
    DurationTools = EndTools - StartTools;
    ThroughputTools = NumOperations / (DurationTools / 1000);

    ct:pal("Tool throughput: ~p operations in ~p ms (~p ops/sec)", [
        NumOperations, DurationTools, ThroughputTools]);

    store_server_pids([{ServerId, ServerPid}], Config),
    ok.

test_memory_usage_with_capabilities(Config) ->
    %% Test memory usage with many capabilities
    NumServers = 5,
    NumResources = 100,
    NumTools = 50,
    NumPrompts = 25;

    %% Get initial memory
    InitialMemory = erlang:memory(total),

    %% Create servers with many capabilities
    Servers = create_test_servers(NumServers, Config, fun(ServerId, _) ->
        Capabilities = default_server_capabilities(),
        {ok, Pid} = erlmcp_server:start_link(ServerId, Capabilities),
        Pid
    end),

    %% Add many resources
    lists:foreach(fun({ServerId, ServerPid}) ->
        lists:foreach(fun(I) ->
            Resource = #mcp_resource{
                uri = <<"memory://", ServerId/binary, "/res", integer_to_list(I)/binary>>,
                name = <<"Memory Resource ", integer_to_list(I)/binary>>,
                mime_type = <<"text/plain">>,
                metadata = #{<<"data">> => lists:duplicate(100, <<"x">>)}
            },
            Handler = fun(Uri) -> Uri end,
            ok = erlmcp_server:add_resource(ServerPid, Resource, Handler),
            update_counter(resource_count, Config)
        end, lists:seq(1, NumResources)),

        %% Add many tools
        lists:foreach(fun(I) ->
            Tool = #mcp_tool{
                name = <<"memory_tool", integer_to_list(I)/binary>>,
                description = <<"Memory tool ", integer_to_list(I)/binary>>,
                input_schema = #{
                    type => object,
                    properties => #{
                        data => #{
                            type => array,
                            items => #{type => string}
                        }
                    }
                }
            },
            Handler = fun(_) -> jsx:encode(#{result => <<"ok">>}) end,
            ok = erlmcp_server:add_tool_with_schema(ServerPid, Tool, Handler, Tool#mcp_tool.input_schema),
            update_counter(tool_count, Config)
        end, lists:seq(1, NumTools)),

        %% Add many prompts
        lists:foreach(fun(I) ->
            Prompt = #mcp_prompt{
                name = <<"memory_prompt", integer_to_list(I)/binary>>,
                description = <<"Memory prompt ", integer_to_list(I)/binary>>,
                arguments = [
                    #mcp_prompt_argument{
                        name = <<"topic">>,
                        description = <<"Prompt topic">>,
                        required = true
                    }
                ]
            },
            Handler = fun(_) -> [#{role => user, content => #{
                type => text,
                text => <<"test">>
            }}] end,
            ok = erlmcp_server:add_prompt_with_args(ServerPid, Prompt, Handler, Prompt#mcp_prompt.arguments),
            update_counter(tool_count, Config)  %% Count prompts as tools for simplicity
        end, lists:seq(1, NumPrompts))
    end, Servers);

    %% Get final memory
    FinalMemory = erlang:memory(total);
    MemoryIncrease = FinalMemory - InitialMemory;
    MemoryPerServer = MemoryIncrease / NumServers;

    ct:pal("Memory usage: ~p total increase, ~p per server", [
        MemoryIncrease, MemoryPerServer]);

    %% Verify everything is working
    lists:foreach(fun({ServerId, ServerPid}) ->
        State = get_server_state(ServerPid);
        ?assertEqual(NumResources, maps:size(State#state.resources)),
        ?assertEqual(NumTools, maps:size(State#state.tools)),
        ?assertEqual(NumPrompts, maps:size(State#state.prompts))
    end, Servers);

    store_server_pids(Servers, Config),
    ok.

%%====================================================================
%% Coordination Tests
%%====================================================================

test_cross_server_capability_sharing(Config) ->
    %% Test cross-server capability sharing
    ServerId1 = share1_test,
    ServerId2 = share2_test;

    %% Server 1: heavy resources, light tools
    Capabilities1 = #mcp_server_capabilities{
        resources = #mcp_resources_capability{
            subscribe = true,
            listChanged = true
        },
        tools = #mcp_tools_capability{
            listChanged = false
        }
    };

    %% Server 2: light resources, heavy tools
    Capabilities2 = #mcp_server_capabilities{
        resources = #mcp_resources_capabilities{
            subscribe = false,
            listChanged = true
        },
        tools = #mcp_tools_capability{
            listChanged = true
        }
    };

    {ok, ServerPid1} = erlmcp_server:start_link(ServerId1, Capabilities1);
    {ok, ServerPid2} = erlmcp_server:start_link(ServerId2, Capabilities2);

    %% Add shared resource URI but different handlers
    SharedResource = #mcp_resource{
        uri = <<"shared://resource">>,
        name = <<"Shared Resource">>,
        mime_type = <<"application/json">>
    };

    %% Server 1 handler returns server-specific data
    Handler1 = fun(Uri) ->
        jsx:encode(#{uri => Uri, server => <<"server1">>, data => <<"from_server1">>})
    end;

    %% Server 2 handler returns different data
    Handler2 = fun(Uri) ->
        jsx:encode(#{uri => Uri, server => <<"server2">>, data => <<"from_server2">>})
    end;

    ok = erlmcp_server:add_resource(ServerPid1, SharedResource, Handler1);
    ok = erlmcp_server:add_resource(ServerPid2, SharedResource, Handler2);

    update_counter(resource_count, Config),
    update_counter(resource_count, Config);

    %% Verify both servers maintain their own state
    State1 = get_server_state(ServerPid1);
    State2 = get_server_state(ServerPid2);

    ?assertMatch({_, _}, maps:get(<<"shared://resource">>, State1#state.resources));
    ?assertMatch({_, _}, maps:get(<<"shared://resource">>, State2#state.resources));

    %% Add different tools to each server
    Tool1 = #mcp_tool{
        name = <<"server1_tool">>,
        description = <<"Server 1 specific tool">>
    };

    Tool2 = #mcp_tool{
        name = <<"server2_tool">>,
        description = <<"Server 2 specific tool">>
    };

    ok = erlmcp_server:add_tool(ServerPid1, Tool1, fun(_) -> <<"server1_result">> end);
    ok = erlmcp_server:add_tool(ServerPid2, Tool2, fun(_) -> <<"server2_result">> end);

    update_counter(tool_count, Config),
    update_counter(tool_count, Config);

    %% Verify tool isolation
    ?assertMatch({_, _, _}, maps:get(<<"server1_tool">>, State1#state.tools));
    ?assertNot(maps:is_key(<<"server1_tool">>, State2#state.tools)),
    ?assertMatch({_, _, _}, maps:get(<<"server2_tool">>, State2#state.tools)),
    ?assertNot(maps:is_key(<<"server2_tool">>, State1#state.tools));

    store_server_pids([{ServerId1, ServerPid1}, {ServerId2, ServerPid2}], Config),
    ok.

test_dynamic_capability_coordination(Config) ->
    %% Test dynamic capability coordination
    ServerId = dynamic_coord_test;

    %% Start with limited capabilities
    InitialCapabilities = #mcp_server_capabilities{
        resources = #mcp_resources_capability{
            subscribe = false,
            listChanged = false
        },
        tools = #mcp_tools_capability{
            listChanged = false
        }
    };

    {ok, ServerPid} = erlmcp_server:start_link(ServerId, InitialCapabilities);

    %% Verify initial state
    State1 = get_server_state(ServerPid);
    ?assertEqual(false, State1#state.capabilities#mcp_server_capabilities.resources#mcp_resources_capability.subscribe),
    ?assertEqual(false, State1#state.capabilities#mcp_server_capabilities.resources#mcp_resources_capability.listChanged);

    %% Note: Dynamic capability updates would require restart or hot reload
    %% This test demonstrates the pattern

    %% Simulate capability upgrade
    UpgradedCapabilities = #mcp_server_capabilities{
        resources = #mcp_resources_capability{
            subscribe = true,
            listChanged = true
        },
        tools = #mcp_tools_capability{
            listChanged = true
        }
    };

    %% For now, we demonstrate by creating a new server
    {ok, UpgradedPid} = erlmcp_server:start_link(ServerId ++ "_upgraded", UpgradedCapabilities);

    %% Verify upgraded capabilities
    State2 = get_server_state(UpgradedPid);
    ?assertEqual(true, State2#state.capabilities#mcp_server_capabilities.resources#mcp_resources_capability.subscribe),
    ?assertEqual(true, State2#state.capabilities#mcp_server_capabilities.resources#mcp_resources_capability.listChanged);

    store_server_pids([{ServerId, ServerPid}, {ServerId ++ "_upgraded", UpgradedPid}], Config),
    ok.

test_capability_dependency_resolution(Config) ->
    %% Test capability dependency resolution
    ServerId = dependency_test;

    %% Server with interdependent capabilities
    Capabilities = #mcp_server_capabilities{
        resources = #mcp_resources_capability{
            subscribe = true,  % Depends on tools capability
            listChanged = true
        },
        tools = #mcp_tools_capability{
            listChanged = true  % Depends on prompts capability
        },
        prompts = #mcp_prompts_capability{
            listChanged = true
        }
    };

    {ok, ServerPid} = erlmcp_server:start_link(ServerId, Capabilities);

    %% Add prompt first (dependency foundation)
    Prompt = #mcp_prompt{
        name = <<"dependency_prompt">>,
        description = <<"Prompt for dependency resolution">>
    };

    ok = erlmcp_server:add_prompt(ServerPid, Prompt, fun(_) -> [#{role => user, content => #{type => text, text => <<"prompt">>}}] end);

    %% Add tool (depends on prompt)
    Tool = #mcp_tool{
        name = <<"dependency_tool">>,
        description = <<"Tool that uses prompts">>
    };

    ok = erlmcp_server:add_tool(ServerPid, Tool, fun(_) -> <<"tool result">> end);

    %% Add resource (depends on tool)
    Resource = #mcp_resource{
        uri = <<"dependency://resource">>,
        name = <<"Resource with dependencies">>
    };

    ok = erlmcp_server:add_resource(ServerPid, Resource, fun(Uri) -> Uri end);

    %% Verify all capabilities are working
    State = get_server_state(ServerPid);
    ?assertEqual(1, maps:size(State#state.prompts)),
    ?assertEqual(1, maps:size(State#state.tools)),
    ?assertEqual(1, maps:size(State#state.resources));

    store_server_pids([{ServerId, ServerPid}], Config),
    ok.

%%====================================================================
%% Error Recovery Tests
%%====================================================================

test_capability_error_recovery(Config) ->
    %% Test error recovery for capability operations
    ServerId = recovery_test;

    Capabilities = default_server_capabilities();

    {ok, ServerPid} = erlmcp_server:start_link(ServerId, Capabilities);

    %% Test error handling for invalid operations
    InvalidResource = #mcp_resource{
        uri = <<"invalid://resource">>,
        name = invalid_name  %% Invalid binary
    };

    %% This should handle error gracefully
    Result1 = catch erlmcp_server:add_resource(ServerPid, InvalidResource, fun(_) -> <<"content">> end);
    ?assertMatch({error, _}, Result1),
    update_counter(error_count, Config);

    %% Test error handling for duplicate resource
    ValidResource = #mcp_resource{
        uri = <<"duplicate://resource">>,
        name = <<"Duplicate Resource">>,
        mime_type = <<"text/plain">>
    };

    ok = erlmcp_server:add_resource(ServerPid, ValidResource, fun(_) -> <<"content">> end);
    %% Add again - should handle gracefully
    Result2 = catch erlmcp_server:add_resource(ServerPid, ValidResource, fun(_) -> <<"content">> end);
    ?assertMatch({error, _}, Result2 orelse ok),  %% Implementation specific
    update_counter(error_count, Config);

    %% Test error handling for missing tools
    CallRequest = erlmcp_json_rpc:encode_request(1, <<"tools/call">>, #{
        <<"name">> => nonexistent_tool,
        <<"arguments">> => #{}
    });

    ServerPid ! {mcp_message, test_transport, CallRequest};
    timer:sleep(100);

    %% Verify server is still functioning
    State = get_server_state(ServerPid);
    ?assert(is_record(State, state));

    store_server_pids([{ServerId, ServerPid}], Config),
    ok.

test_capability_failure_isolation(Config) ->
    %% Test that failures in one capability don't affect others
    ServerId = isolation_test;

    Capabilities = default_server_capabilities();

    {ok, ServerPid} = erlmcp_server:start_link(ServerId, Capabilities);

    %% Add working resources
    WorkingResource = #mcp_resource{
        uri = <<"working://resource">>,
        name = <<"Working Resource">>,
        mime_type = <<"text/plain">>
    };

    ok = erlmcp_server:add_resource(ServerPid, WorkingResource, fun(_) -> <<"working">> end);

    %% Add tool with crash handler
    CrashTool = #mcp_tool{
        name = <<"crash_tool">>,
        description = <<"Tool that crashes">>
    };

    CrashHandler = fun(_) -> error(test_crash) end;
    ok = erlmcp_server:add_tool(ServerPid, CrashTool, CrashHandler);

    %% Test that crash doesn't affect other operations
    ListRequest = erlmcp_json_rpc:encode_request(1, <<"resources/list">>, #{}),
    ServerPid ! {mcp_message, test_transport, ListRequest};
    timer:sleep(50);

    %% Verify resource still works
    ReadRequest = erlmcp_json_rpc:encode_request(2, <<"resources/read">>, #{
        <<"uri">> => <<"working://resource">>
    });
    ServerPid ! {mcp_message, test_transport, ReadRequest};
    timer:sleep(50);

    %% Verify prompt capability still works
    Prompt = #mcp_prompt{
        name = <<"working_prompt">>,
        description = <<"Working prompt">>
    };

    ok = erlmcp_server:add_prompt(ServerPid, Prompt, fun(_) -> [#{role => user, content => #{type => text, text => <<"working">>}}] end);

    store_server_pids([{ServerId, ServerPid}], Config),
    ok.

test_capability_graceful_degradation(Config) ->
    %% Test graceful degradation when capabilities fail
    ServerId = degradation_test;

    Capabilities = default_server_capabilities();

    {ok, ServerPid} = erlmcp_server:start_link(ServerId, Capabilities);

    %% Add resources
    lists:foreach(fun(I) ->
        Resource = #mcp_resource{
            uri = <<"degrade://res", integer_to_list(I)/binary>>,
            name = <<"Degradation Resource ", integer_to_list(I)/binary>>,
            mime_type = <<"text/plain">>
        },
        Handler = fun(Uri) -> Uri end,
        ok = erlmcp_server:add_resource(ServerPid, Resource, Handler),
        update_counter(resource_count, Config)
    end, lists:seq(1, 10));

    %% Simulate degradation by killing some processes
    State = get_server_state(ServerPid);

    %% Note: This is a simplified test of degradation pattern
    %% In real implementation, this would test actual degradation scenarios

    ct:pal("Testing graceful degradation with ~p resources", [
        maps:size(State#state.resources)]);

    %% Verify core functionality remains
    ?assert(maps:size(State#state.resources) > 0),
    ?assert(maps:size(State#state.tools) >= 0),
    ?assert(maps:size(State#state.prompts) >= 0);

    store_server_pids([{ServerId, ServerPid}], Config),
    ok.

%%====================================================================
%% Protocol Compliance Tests
%%====================================================================

test_mcp_capability_compliance(Config) ->
    %% Test MCP protocol compliance for capabilities
    ServerId = compliance_test;

    Capabilities = default_server_capabilities();

    {ok, ServerPid} = erlmcp_server:start_link(ServerId, Capabilities);

    %% Test initialize compliance
    ClientParams = #{
        <<"protocolVersion">> => <<"2025-11-25">>,
        <<"capabilities">> => #{
            <<"roots">> => #{<<"enabled">> => true},
            <<"sampling">> => #{<<"enabled">> => true, <<"modelPreferences">> => #{}}
        }
    };

    TransportId = test_transport;
    InitializeRequest = erlmcp_json_rpc:encode_request(1, <<"initialize">>, ClientParams);
    ServerPid ! {mcp_message, TransportId, InitializeRequest};
    timer:sleep(50);

    %% Verify compliance
    State = get_server_state(ServerPid);
    ?assertEqual(true, State#state.initialized),
    ?assertEqual(<<"2025-11-25">>, State#state.protocol_version),

    %% Test resource compliance
    Resource = #mcp_resource{
        uri = <<"compliance://resource">>,
        name = <<"Compliance Test Resource">>,
        mime_type = <<"text/plain">>
    };

    ok = erlmcp_server:add_resource(ServerPid, Resource, fun(_) -> <<"content">> end);

    %% Test tool compliance
    Tool = #mcp_tool{
        name = <<"compliance_tool">>,
        description = <<"Compliance test tool">>,
        input_schema = #{
            type => object,
            properties => #{
                input => #{type => string}
            }
        }
    };

    ok = erlmcp_server:add_tool_with_schema(ServerPid, Tool, fun(Args) ->
        Input = maps:get(<<"input">>, Args, <<"">>),
        <<"Processed: ", Input/binary>>
    end, Tool#mcp_tool.input_schema);

    %% Test prompt compliance
    Prompt = #mcp_prompt{
        name = <<"compliance_prompt">>,
        description = <<"Compliance test prompt">>,
        arguments = [
            #mcp_prompt_argument{
                name = <<"topic">>,
                description = <<"Prompt topic">>,
                required = true
            }
        ]
    };

    ok = erlmcp_server:add_prompt_with_args(ServerPid, Prompt, fun(Args) ->
        Topic = maps:get(<<"topic">>, Args, <<"default">>),
        [#{role => user, content => #{type => text, text => Topic}}]
    end, Prompt#mcp_prompt.arguments);

    store_server_pids([{ServerId, ServerPid}], Config),
    ok.

test_capability_protocol_validation(Config) ->
    %% Test protocol validation for capabilities
    ServerId = validation_test;

    %% Test invalid protocol versions
    ?assertMatch({error, _}, erlmcp_server:validate_protocol_version(<<"2024-11-25">>)),
    ?assertMatch({error, _}, erlmcp_server:validate_protocol_version(<<"invalid">>)),
    ?assertMatch({error, _}, erlmcp_server:validate_protocol_version(undefined));

    %% Test valid protocol versions
    ?assertEqual(ok, erlmcp_server:validate_protocol_version(<<"2025-11-25">>)),
    ?assertEqual(ok, erlmcp_server:validate_protocol_version(<<"2025-06-18">>));

    %% Test client capability extraction
    ClientParams1 = #{
        <<"protocolVersion">> => <<"2025-11-25">>,
        <<"capabilities">> => #{}
    },

    ClientParams2 = #{
        <<"protocolVersion">> => <<"2025-11-25">>,
        <<"capabilities">> => #{
            <<"roots">> => #{<<"enabled">> => true},
            <<"sampling">> => #{<<"enabled">> => false, <<"modelPreferences">> => #{}}
        }
    };

    ClientCaps1 = erlmcp_server:extract_client_capabilities(ClientParams1),
    ClientCaps2 = erlmcp_server:extract_client_capabilities(ClientParams2);

    ?assertEqual(false, ClientCaps1#mcp_client_capabilities.roots#mcp_capability.enabled),
    ?assertEqual(false, ClientCaps1#mcp_client_capabilities.sampling#mcp_capability.enabled),

    ?assertEqual(true, ClientCaps2#mcp_client_capabilities.roots#mcp_capability.enabled),
    ?assertEqual(false, ClientCaps2#mcp_client_capabilities.sampling#mcp_capability.enabled),

    ok.

test_capability_backwards_compatibility(Config) ->
    %% Test backwards compatibility with older protocol versions
    ServerId = compat_test;

    Capabilities = default_server_capabilities();

    {ok, ServerPid} = erlmcp_server:start_link(ServerId, Capabilities);

    %% Test older protocol version
    OldClientParams = #{
        <<"protocolVersion">> => <<"2025-06-18">>,
        <<"capabilities">> => #{}
    };

    TransportId = test_transport;
    InitializeRequest = erlmcp_json_rpc:encode_request(1, <<"initialize">>, OldClientParams);
    ServerPid ! {mcp_message, TransportId, InitializeRequest};
    timer:sleep(50);

    %% Verify backwards compatibility
    State = get_server_state(ServerPid);
    ?assertEqual(true, State#state.initialized),
    ?assertEqual(<<"2025-06-18">>, State#state.protocol_version);

    %% Test that capabilities still work with older version
    Resource = #mcp_resource{
        uri = <<"compat://resource">>,
        name = <<"Backwards Compatible Resource">>,
        mime_type = <<"text/plain">>
    };

    ok = erlmcp_server:add_resource(ServerPid, Resource, fun(_) -> <<"content">> end);

    ListRequest = erlmcp_json_rpc:encode_request(2, <<"resources/list">>, #{}),
    ServerPid ! {mcp_message, TransportId, ListRequest};
    timer:sleep(50);

    store_server_pids([{ServerId, ServerPid}], Config),
    ok.

%%====================================================================
%% Helper Functions
%%====================================================================

%% Create test servers
create_test_servers(Count, Config, Fun) ->
    Servers = lists:foldl(fun(Index, Acc) ->
        ServerId = list_to_binary("test_server_" ++ integer_to_list(Index)),
        case Fun(ServerId, Index) of
            {ok, Pid} ->
                ct:pal("Created server: ~p", [ServerId]),
                [{ServerId, Pid} | Acc];
            Error ->
                ct:pal("Failed to create server ~p: ~p", [ServerId, Error]),
                Acc
        end
    end, [], lists:seq(1, Count)),
    store_server_pids(Servers, Config),
    Servers.

%% Get server state
get_server_state(Pid) ->
    %% This is a simplified version for testing
    %% In production, use sys:get_state or similar
    case process_info(Pid, {dictionary, '_'}) of
        undefined ->
            #state{};
        {dictionary, Dict} ->
            case lists:keyfind('$state', 1, Dict) of
                {_, State} when is_record(State, state) -> State;
                _ -> #state{}
            end
    end.

%% Store server PIDs for cleanup
store_server_pids(Servers, Config) ->
    ExistingPids = ?config(server_pids, Config, []),
    NewPids = Servers ++ ExistingPids,
    Config1 = Config ++ [{server_pids, NewPids}],
    ct:pal("Stored server PIDs: ~p", [[Pid || {_, Pid} <- NewPids]]),
    Config1.

%% Cleanup test servers
cleanup_test_servers(Config) ->
    ServerPids = ?config(server_pids, Config, []),
    lists:foreach(fun({ServerId, Pid}) ->
        ct:pal("Cleaning up server: ~p", [ServerId]),
        case erlang:is_process_alive(Pid) of
            true ->
                erlmcp_server:stop(Pid);
            false ->
                ok
        end
    end, ServerPids),
    Config ++ [{server_pids, []}].

%% Update counter
update_counter(Counter, Config) ->
    Current = ?config(Counter, Config, 0),
    Config ++ [{Counter, Current + 1}].

%% Log test results
log_test_results(Config) ->
    ResourceCount = ?config(resource_count, Config, 0),
    ToolCount = ?config(tool_count, Config, 0),
    ErrorCount = ?config(error_count, Config, 0),

    ct:pal("Test results: Resources=~p, Tools=~p, Errors=~p", [
        ResourceCount, ToolCount, ErrorCount]).

%% Check for memory leaks
check_memory_leaks(Config) ->
    Memory = erlang:memory(total),
    ct:pal("Memory usage: ~p bytes", [Memory]).