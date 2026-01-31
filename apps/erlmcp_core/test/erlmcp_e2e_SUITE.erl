-module(erlmcp_e2e_SUITE).
-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("erlmcp.hrl").

%% Test server lifecycle and MCP protocol flow
all() ->
    [
     e2e_test_full_mcp_lifecycle,
     e2e_test_resource_operations,
     e2e_test_tool_operations,
     e2e_test_prompt_operations,
     e2e_test_client_server_interaction
    ].

init_per_suite(Config) ->
    %% Start only the core applications without observability (which requires ranch/cowboy)
    {ok, _} = application:ensure_all_started(gproc),
    {ok, _} = application:ensure_all_started(jsx),
    ok = erlmcp_registry_utils:ensure_gproc_started(),

    %% Start the registry manually
    {ok, _} = erlmcp_registry:start_link(),

    Config.

end_per_suite(_Config) ->
    %% Stop registry
    case whereis(erlmcp_registry) of
        undefined -> ok;
        Pid -> gen_server:stop(Pid)
    end,
    ok.

%%%===================================================================
%%% Test Cases
%%%===================================================================

%% Full MCP lifecycle test
e2e_test_full_mcp_lifecycle(_Config) ->
    %% Step 1: Start a server with full capabilities
    ServerId = e2e_test_server,
    Capabilities = #mcp_server_capabilities{
        resources = #mcp_capability{enabled = true},
        tools = #mcp_capability{enabled = true},
        prompts = #mcp_capability{enabled = true},
        logging = #mcp_capability{enabled = true}
    },

    {ok, Server} = erlmcp_server:start_link(ServerId, Capabilities),
    ct:log("Started MCP server: ~p", [Server]),

    %% Verify server is alive
    ?assert(erlang:is_process_alive(Server)),

    %% Step 2: Add test resources
    ResourceHandler = fun(_Uri) ->
        #{
            contents => [#{text => <<"Hello from resource!">>}]
        }
    end,
    ok = erlmcp_server:add_resource(Server, <<"/test.txt">>, ResourceHandler),
    ct:log("Added resource to server"),

    %% Step 3: Add test tool
    ToolHandler = fun(Arguments) ->
        #{<<"value">> := Value} = Arguments,
        #{
            content => [#{type => text, text => <<"Echo: ", Value/binary>>}]
        }
    end,
    ok = erlmcp_server:add_tool(Server, <<"echo">>, ToolHandler),
    ct:log("Added tool to server"),

    %% Step 4: Add test prompt
    PromptHandler = fun(_Arguments) ->
        #{
            messages => [#{
                role => user,
                content => #{
                    type => text,
                    text => <<"Test prompt content">>
                }
            }]
        }
    end,
    ok = erlmcp_server:add_prompt(Server, <<"test_prompt">>, PromptHandler),
    ct:log("Added prompt to server"),

    %% Step 5: Subscribe to resource
    ok = erlmcp_server:subscribe_resource(Server, <<"/test.txt">>, self()),
    ct:log("Subscribed to resource"),

    %% Step 6: Notify resource update
    ok = erlmcp_server:notify_resource_updated(Server, <<"/test.txt">>, #{}),

    %% Verify notification received
    receive
        {resource_updated, <<"/test.txt">>, _} ->
            ct:log("Received resource update notification")
    after 1000 ->
        ct:fail("Timeout waiting for resource update notification")
    end,

    %% Step 7: Unsubscribe
    ok = erlmcp_server:unsubscribe_resource(Server, <<"/test.txt">>),
    ct:log("Unsubscribed from resource"),

    %% Step 8: Stop server gracefully
    ok = erlmcp_server:stop(Server),
    ct:log("Stopped server"),

    %% Verify server is stopped
    ?assertNot(erlang:is_process_alive(Server)),

    {comment, "Full MCP lifecycle test completed successfully"}.

%% Test resource operations end-to-end
e2e_test_resource_operations(_Config) ->
    ServerId = resource_test_server,
    {ok, Server} = erlmcp_server:start_link(ServerId, #mcp_server_capabilities{
        resources = #mcp_capability{enabled = true}
    }),

    %% Add multiple resources
    Handler1 = fun(_) -> #{contents => [#{text => <<"Resource 1">>}]} end,
    Handler2 = fun(_) -> #{contents => [#{text => <<"Resource 2">>}]} end,

    ok = erlmcp_server:add_resource(Server, <<"/res1.txt">>, Handler1),
    ok = erlmcp_server:add_resource(Server, <<"/res2.txt">>, Handler2),
    ct:log("Added 2 resources"),

    %% Test resource template
    TemplateHandler = fun(_Params) ->
        #{contents => [#{text => <<"Template result">>}]}
    end,
    ok = erlmcp_server:add_resource_template(
        Server,
        <<"/template/{id}">>,
        <<"Test Template">>,
        TemplateHandler
    ),
    ct:log("Added resource template"),

    %% Delete a resource
    ok = erlmcp_server:delete_resource(Server, <<"/res1.txt">>),
    ct:log("Deleted resource"),

    %% Verify deletion fails for non-existent resource
    ?assertEqual({error, not_found}, erlmcp_server:delete_resource(Server, <<"/nonexistent.txt">>)),

    %% Notify resources changed
    ok = erlmcp_server:notify_resources_changed(Server),

    %% Stop server
    ok = erlmcp_server:stop(Server),

    {comment, "Resource operations test completed"}.

%% Test tool operations end-to-end
e2e_test_tool_operations(_Config) ->
    ServerId = tool_test_server,
    {ok, Server} = erlmcp_server:start_link(ServerId, #mcp_server_capabilities{
        tools = #mcp_capability{enabled = true}
    }),

    %% Add tools with different configurations
    SimpleHandler = fun(Args) ->
        #{<<"name">> := Name} = Args,
        #{
            content => [#{type => text, text => <<"Hello, ", Name/binary>>}]
        }
    end,

    ok = erlmcp_server:add_tool(Server, <<"greet">>, SimpleHandler),

    %% Add tool with description
    Handler2 = fun(Args) ->
        #{<<"x">> := X, <<"y">> := Y} = Args,
        Result = X + Y,
        #{
            content => [#{type => text, text => <<"Sum: ", (integer_to_binary(Result))/binary>>}]
        }
    end,

    ok = erlmcp_server:add_tool_with_description(
        Server,
        <<"add">>,
        <<"Add two numbers">>,
        Handler2
    ),

    %% Add tool with schema
    Schema = #{
        <<"type">> => <<"object">>,
        <<"properties">> => #{
            <<"value">> => #{
                <<"type">> => <<"string">>
            }
        }
    },

    Handler3 = fun(Args) ->
        #{<<"value">> := Value} = Args,
        #{
            content => [#{type => text, text => Value}]
        }
    end,

    ok = erlmcp_server:add_tool_with_schema(Server, <<"echo_with_schema">>, Handler3, Schema),

    %% Add full tool
    Handler4 = fun(Args) ->
        #{<<"text">> := Text} = Args,
        #{
            content => [#{type => text, text => Text}]
        }
    end,

    ok = erlmcp_server:add_tool_full(
        Server,
        <<"repeat">>,
        <<"Repeat the input text">>,
        Handler4,
        #{
            <<"inputSchema">> => Schema,
            <<"deprecated">> => false
        }
    ),

    ct:log("Added 4 tools with various configurations"),

    %% Delete a tool
    ok = erlmcp_server:delete_tool(Server, <<"greet">>),
    ct:log("Deleted tool"),

    %% Verify deletion fails for non-existent tool
    ?assertEqual({error, not_found}, erlmcp_server:delete_tool(Server, <<"nonexistent_tool">>)),

    %% Stop server
    ok = erlmcp_server:stop(Server),

    {comment, "Tool operations test completed"}.

%% Test prompt operations end-to-end
e2e_test_prompt_operations(_Config) ->
    ServerId = prompt_test_server,
    {ok, Server} = erlmcp_server:start_link(ServerId, #mcp_server_capabilities{
        prompts = #mcp_capability{enabled = true}
    }),

    %% Add simple prompt
    SimpleHandler = fun(_Args) ->
        #{
            messages => [#{
                role => user,
                content => #{type => text, text => <<"Simple prompt">>}
            }]
        }
    end,

    ok = erlmcp_server:add_prompt(Server, <<"simple">>, SimpleHandler),

    %% Add prompt with arguments
    ArgsHandler = fun(Args) ->
        #{<<"topic">> := Topic} = Args,
        #{
            messages => [#{
                role => user,
                content => #{type => text, text => <<"Tell me about ", Topic/binary>>}
            }]
        }
    end,

    Arguments = [
        #mcp_prompt_argument{
            name = <<"topic">>,
            description = <<"The topic to discuss">>,
            required = true
        }
    ],

    ok = erlmcp_server:add_prompt_with_args(Server, <<"with_args">>, ArgsHandler, Arguments),

    %% Add prompt with schema
    Schema = #{
        <<"type">> => <<"object">>,
        <<"properties">> => #{
            <<"language">> => #{
                <<"type">> => <<"string">>,
                <<"enum">> => [<<"en">>, <<"es">>, <<"fr">>]
            }
        },
        <<"required">> => [<<"language">>]
    },

    SchemaHandler = fun(Args) ->
        #{<<"language">> := Lang} = Args,
        #{
            messages => [#{
                role => user,
                content => #{type => text, text => <<"Switch to ", Lang/binary>>}
            }]
        }
    end,

    ok = erlmcp_server:add_prompt_with_args_and_schema(
        Server,
        <<"with_schema">>,
        SchemaHandler,
        Arguments,
        Schema
    ),

    ct:log("Added 3 prompts with various configurations"),

    %% Delete a prompt
    ok = erlmcp_server:delete_prompt(Server, <<"simple">>),
    ct:log("Deleted prompt"),

    %% Verify deletion fails for non-existent prompt
    ?assertEqual({error, not_found}, erlmcp_server:delete_prompt(Server, <<"nonexistent_prompt">>)),

    %% Stop server
    ok = erlmcp_server:stop(Server),

    {comment, "Prompt operations test completed"}.

%% Test client-server interaction
e2e_test_client_server_interaction(_Config) ->
    %% Start server
    ServerId = interaction_test_server,
    {ok, Server} = erlmcp_server:start_link(ServerId, #mcp_server_capabilities{
        resources = #mcp_capability{enabled = true},
        tools = #mcp_capability{enabled = true}
    }),

    %% Register notification handler
    HandlerPid = spawn(fun() ->
        receive
            {mcp_notification, Method, Params} ->
                ct:log("Handler received notification: ~p with params: ~p", [Method, Params])
        end
    end),

    ok = erlmcp_server:register_notification_handler(
        Server,
        <<"resources/updated">>,
        HandlerPid
    ),
    ct:log("Registered notification handler"),

    %% Add a resource
    ResourceHandler = fun(_Uri) ->
        #{contents => [#{text => <<"Test resource">>}]}
    end,

    ok = erlmcp_server:add_resource(Server, <<"/interactive.txt">>, ResourceHandler),

    %% Notify resource update (should trigger handler)
    ok = erlmcp_server:notify_resource_updated(Server, <<"/interactive.txt">>, #{
        <<"timestamp">> => erlang:system_time(millisecond)
    }),

    %% Give handler time to process
    timer:sleep(100),

    %% Unregister handler
    ok = erlmcp_server:unregister_notification_handler(Server, <<"resources/updated">>),
    ct:log("Unregistered notification handler"),

    %% Test progress reporting
    ProgressToken = 42,
    ok = erlmcp_server:report_progress(Server, ProgressToken, 50.0, 100.0),
    ct:log("Reported progress: 50%"),

    ok = erlmcp_server:report_progress(Server, ProgressToken, 100.0, 100.0),
    ct:log("Reported progress: 100%"),

    %% Stop server
    ok = erlmcp_server:stop(Server),

    {comment, "Client-server interaction test completed"}.
