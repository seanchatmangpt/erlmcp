#!/usr/bin/env escript
%%%-------------------------------------------------------------------
%%% @doc End-to-End MCP Integration Example
%%%
%%% Demonstrates complete MCP workflow:
%%% 1. Start MCP server with all features
%%% 2. Client connects via stdio
%%% 3. List resources/tools/prompts
%%% 4. Subscribe to resource
%%% 5. Call tool with secret injection
%%% 6. Demonstrate full MCP flow
%%%
%%% Prerequisites:
%%% - erlmcp_core and erlmcp_transports applications
%%% - stdio transport configured
%%%
%%% @end
%%%-------------------------------------------------------------------

-mode(compile).

main(_) ->
    io:format("~n=== End-to-End MCP Integration Example ===~n~n"),

    %% Step 1: Start applications
    io:format("Step 1: Starting erlmcp applications...~n"),
    {ok, _} = application:ensure_all_started(erlmcp_core),
    {ok, _} = application:ensure_all_started(erlmcp_transports),
    io:format("✓ Applications started~n~n"),

    %% Step 2: Start resource subscriptions manager
    io:format("Step 2: Starting resource subscriptions manager...~n"),
    {ok, _SubsPid} = erlmcp_resource_subscriptions:start_link(),
    io:format("✓ Resource subscriptions manager started~n~n"),

    %% Step 3: Start secrets manager
    io:format("Step 3: Starting secrets manager...~n"),
    {ok, _SecretsPid} = erlmcp_secrets:start_link(#{
        backend => local_encrypted,
        ttl_seconds => 300,
        storage_path => "examples/mcp_complete/secrets.enc",
        encryption_key_path => "examples/mcp_complete/master.key"
    }),

    %% Store a secret for tool injection
    erlmcp_secrets:set_secret(<<"api_key">>, <<"sk-demo-key-12345">>),
    io:format("✓ Secrets manager started~n~n"),

    %% Step 4: Start MCP server with stdio transport
    io:format("Step 4: Starting MCP server with stdio transport...~n"),
    {ok, Server} = erlmcp_server:start_link(
        <<"complete-example-server">>,
        #{
            transport => stdio,
            capabilities => #{
                resources => true,
                tools => true,
                prompts => true,
                logging => true
            }
        }
    ),
    io:format("✓ MCP server started: ~p~n~n", [Server]),

    %% Step 5: Add resources
    io:format("Step 5: Adding resources...~n"),

    %% Static resource
    ok = erlmcp_server:add_resource(
        Server,
        <<"mcp://config">>,
        <<"MCP Server Configuration">>,
        fun(_Uri) ->
            jsx:encode(#{
                version => <<"1.0.0">>,
                features => [<<"resources">>, <<"tools">>, <<"prompts">>],
                settings => #{
                    timeout => 30000,
                    max_connections => 100
                }
            })
        end,
        <<"application/json">>
    ),
    io:format("✓ Added: mcp://config~n"),

    %% Dynamic resource (counter)
    ok = erlmcp_server:add_resource(
        Server,
        <<"mcp://counter">>,
        <<"Request Counter">>,
        fun(_Uri) ->
            Count = get_request_count(),
            jsx:encode(#{
                count => Count,
                timestamp => erlang:system_time(second)
            })
        end,
        <<"application/json">>
    ),
    io:format("✓ Added: mcp://counter~n"),

    %% Resource that uses secrets
    ok = erlmcp_server:add_resource(
        Server,
        <<"mcp://credentials">>,
        <<"External Credentials (Redacted)">>,
        fun(_Uri) ->
            case erlmcp_secrets:get_secret(<<"api_key">>) of
                {ok, Key} ->
                    <<"API Key: ", (mask_key(Key))/binary>>;
                {error, _} ->
                    <<"API Key: [not configured]">>
            end
        end,
        <<"text/plain">>
    ),
    io:format("✓ Added: mcp://credentials~n~n"),

    %% Step 6: Add tools
    io:format("Step 6: Adding tools...~n"),

    %% Simple calculation tool
    ok = erlmcp_server:add_tool(
        Server,
        <<"calculate">>,
        <<"Perform basic arithmetic calculations">>,
        fun(Args) ->
            case Args of
                #{<<"a">> := A, <<"b">> := B, <<"op">> := Op} ->
                    case Op of
                        <<"add">> -> float_to_binary(A + B, [{decimals, 2}]);
                        <<"subtract">> -> float_to_binary(A - B, [{decimals, 2}]);
                        <<"multiply">> -> float_to_binary(A * B, [{decimals, 2}]);
                        <<"divide">> when B =/= 0.0 -> float_to_binary(A / B, [{decimals, 2}]);
                        <<"divide">> -> <<"Error: Division by zero">>;
                        _ -> <<"Error: Unknown operation">>
                    end;
                _ ->
                    <<"Error: Missing required arguments">>
            end
        end,
        #{
            <<"type">> => <<"object">>,
            <<"properties">> => #{
                <<"a">> => #{
                    <<"type">> => <<"number">>,
                    <<"description">> => <<"First operand">>
                },
                <<"b">> => #{
                    <<"type">> => <<"number">>,
                    <<"description">> => <<"Second operand">>
                },
                <<"op">> => #{
                    <<"type">> => <<"string">>,
                    <<"enum">> => [<<"add">>, <<"subtract">>, <<"multiply">>, <<"divide">>],
                    <<"description">> => <<"Operation to perform">>
                }
            },
            <<"required">> => [<<"a">>, <<"b">>, <<"op">>]
        }
    ),
    io:format("✓ Added: calculate~n"),

    %% Tool with secret injection
    ok = erlmcp_server:add_tool(
        Server,
        <<"fetch_external_data">>,
        <<"Fetch data from external API using stored credentials">>,
        fun(_Args) ->
            case erlmcp_secrets:get_secret(<<"api_key">>) of
                {ok, Key} ->
                    increment_request_count(),
                    jsx:encode(#{
                        status => success,
                        data => #{
                            message => <<"Simulated API response">>,
                            authenticated_as => mask_key(Key),
                            timestamp => erlang:system_time(second)
                        }
                    });
                {error, Reason} ->
                    jsx:encode(#{
                        status => error,
                        error => io_lib:format("Failed to get credentials: ~p", [Reason])
                    })
            end
        end,
        #{
            <<"type">> => <<"object">>,
            <<"properties">> => #{
                <<"endpoint">> => #{
                    <<"type">> => <<"string">>,
                    <<"description">> => <<"API endpoint to call">>
                }
            }
        }
    ),
    io:format("✓ Added: fetch_external_data (with secret injection)~n~n"),

    %% Step 7: Add prompts
    io:format("Step 7: Adding prompts...~n"),

    ok = erlmcp_server:add_prompt(
        Server,
        <<"generate_report">>,
        <<"Generate a structured report">>,
        fun(Args) ->
            Type = maps:get(<<"type">>, Args, <<"summary">>),
            [
                #{
                    <<"role">> => <<"user">>,
                    <<"content">> => #{
                        <<"type">> => <<"text">>,
                        <<"text">> => <<"Generate a ", Type/binary, " report of the current system state.">>
                    }
                }
            ]
        end,
        [
            #{
                <<"name">> => <<"type">>,
                <<"description">> => <<"Report type">>,
                <<"required">> => false
            }
        ]
    ),
    io:format("✓ Added: generate_report~n~n"),

    %% Step 8: Subscribe to resource changes
    io:format("Step 8: Subscribing to resource changes...~n"),
    spawn(fun() -> resource_subscriber(Server) end),
    io:format("✓ Resource subscriber started~n~n"),

    %% Step 9: Demonstrate complete MCP workflow
    io:format("Step 9: Demonstrating MCP workflow...~n~n"),

    %% 9.1: Initialize (simulated)
    io:format("9.1: Initialize (handshake)~n"),
    io:format("  → Client: initialize~n"),
    io:format("  ← Server: capabilities = {resources, tools, prompts}~n~n"),

    %% 9.2: List resources
    io:format("9.2: List Resources~n"),
    io:format("  Resources:~n"),
    io:format("    - mcp://config~n"),
    io:format("    - mcp://counter~n"),
    io:format("    - mcp://credentials~n~n"),

    %% 9.3: Read resource
    io:format("9.3: Read Resource (mcp://config)~n"),
    ConfigJson = read_resource(Server, <<"mcp://config">>),
    io:format("  Response: ~s~n~n", [ConfigJson]),

    %% 9.4: Subscribe to resource
    io:format("9.4: Subscribe to Resource (mcp://counter)~n"),
    case erlmcp_resource_subscriptions:subscribe_to_resource(
        <<"mcp://counter">>,
        Server,
        #{rate_limit => 1000}
    ) of
        ok -> io:format("  ✓ Subscribed~n~n");
        {error, Reason} -> io:format("  ✗ Failed: ~p~n~n", [Reason])
    end,

    %% 9.5: List tools
    io:format("9.5: List Tools~n"),
    io:format("  Tools:~n"),
    io:format("    - calculate: Perform arithmetic~n"),
    io:format("    - fetch_external_data: Fetch from external API~n~n"),

    %% 9.6: Call tool
    io:format("9.6: Call Tool (calculate)~n"),
    io:format("  Request: a=10, b=5, op=multiply~n"),
    ResultJson = call_tool(Server, <<"calculate">>, #{
        <<"a">> => 10,
        <<"b">> => 5,
        <<"op">> => <<"multiply">>
    }),
    io:format("  Response: ~s~n~n", [ResultJson]),

    %% 9.7: Call tool with secret injection
    io:format("9.7: Call Tool (fetch_external_data) with secret injection~n"),
    io:format("  Request: endpoint=/api/data~n"),
    Result2Json = call_tool(Server, <<"fetch_external_data">>, #{
        <<"endpoint">> => <<"/api/data">>
    }),
    io:format("  Response: ~s~n~n", [Result2Json]),

    %% 9.8: List prompts
    io:format("9.8: List Prompts~n"),
    io:format("  Prompts:~n"),
    io:format("    - generate_report: Generate structured report~n~n"),

    %% 9.9: Get prompt
    io:format("9.9: Get Prompt (generate_report)~n"),
    io:format("  Arguments: type=detailed~n"),
    PromptResult = get_prompt(Server, <<"generate_report">>, #{
        <<"type">> => <<"detailed">>
    }),
    io:format("  Response: ~p~n~n", [PromptResult]),

    %% 9.10: Trigger resource change notification
    io:format("9.10: Trigger Resource Change~n"),
    io:format("  Notifying: mcp://counter changed~n"),
    erlmcp_resource_subscriptions:notify_resource_changed(
        <<"mcp://counter">>,
        #{reason => periodic_update}
    ),
    timer:sleep(500),
    io:format("  ✓ Notification sent~n~n"),

    %% Step 10: Demonstrate progress support
    io:format("Step 10: Demonstrating progress support...~n"),
    ProgressToken = <<"progress-123">>,
    ok = erlmcp_server:report_progress(Server, ProgressToken, 0.5, <<"Processing...">>),
    io:format("✓ Progress reported: 50% complete~n~n"),

    %% Step 11: Statistics
    io:format("Step 11: Statistics...~n"),
    SubsStats = erlmcp_resource_subscriptions:get_stats(),
    io:format("Resource Subscriptions:~n"),
    io:format("  Total Resources: ~p~n", [maps:get(total_resources, SubsStats)]),
    io:format("  Total Subscriptions: ~p~n", [maps:get(total_subscriptions, SubsStats)]),
    io:format("  Pending Changes: ~p~n~n", [maps:get(resources_with_pending_changes, SubsStats)]),

    %% Step 12: Cleanup
    io:format("Step 12: Cleaning up...~n"),
    erlmcp_server:stop(Server),
    erlmcp_resource_subscriptions:stop(),
    erlmcp_secrets:stop(),
    application:stop(erlmcp_transports),
    application:stop(erlmcp_core),
    io:format("✓ Cleanup complete~n~n"),

    io:format("=== Example Complete ===~n"),
    init:stop().

%%--------------------------------------------------------------------
%% Helper Functions
%%--------------------------------------------------------------------

%% @doc Resource subscriber loop
resource_subscriber(Server) ->
    receive
        {'$mcp_resource', #{method := <<"resources/updated">>, params := Params}} ->
            Uri = maps:get(<<"uri">>, Params),
            Timestamp = maps:get(<<"timestamp">>, Params),
            io:format("  [SUBSCRIBER] Resource ~s updated at ~p~n", [Uri, Timestamp]),
            resource_subscriber(Server);
        stop ->
            ok
    after 10000 ->
        resource_subscriber(Server)
    end.

%% @doc Read a resource (simulated)
read_resource(Server, Uri) ->
    %% In real MCP, this would go through the transport
    %% For demo, we simulate direct access
    case Uri of
        <<"mcp://config">> ->
            jsx:encode(#{
                version => <<"1.0.0">>,
                features => [<<"resources">>, <<"tools">>, <<"prompts">>]
            });
        <<"mcp://counter">> ->
            Count = get_request_count(),
            jsx:encode(#{count => Count, timestamp => erlang:system_time(second)});
        _ ->
            <<"{\"error\": \"Resource not found\"}">>
    end.

%% @doc Call a tool (simulated)
call_tool(_Server, <<"calculate">>, Args) ->
    #{<<"a">> := A, <<"b">> := B, <<"op">> := Op} = Args,
    Result = case Op of
        <<"add">> -> A + B;
        <<"subtract">> -> A - B;
        <<"multiply">> -> A * B;
        <<"divide">> when B =/= 0 -> A / B;
        _ -> error
    end,
    jsx:encode(#{result => Result});
call_tool(_Server, <<"fetch_external_data">>, _Args) ->
    %% Simulated tool that injects secret
    {ok, Key} = erlmcp_secrets:get_secret(<<"api_key">>),
    jsx:encode(#{
        status => success,
        data => #{
            message => <<"Simulated API response">>,
            authenticated_as => mask_key(Key)
        }
    }).

%% @doc Get a prompt (simulated)
get_prompt(_Server, <<"generate_report">>, Args) ->
    Type = maps:get(<<"type">>, Args, <<"summary">>),
    [
        #{
            role => user,
            content => #{
                type => text,
                text => <<"Generate a ", Type/binary, " report">>
            }
        }
    ].

%% @doc Mask API key for display
mask_key(<<Prefix:3/binary, _Rest/binary>>) ->
    <<Prefix/binary, "****">>;
mask_key(Short) when byte_size(Short) < 3 ->
    <<"***">>.

%% @doc Get request count (mock state)
get_request_count() ->
    case get(request_count) of
        undefined -> 0;
        Count -> Count
    end.

%% @doc Increment request count
increment_request_count() ->
    Current = get_request_count(),
    put(request_count, Current + 1).
