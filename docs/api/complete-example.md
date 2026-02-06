# Complete Client-Server Example

## Overview

This document provides a complete, working example of an erlmcp client-server application with multiple transports, error handling, and best practices.

---

## Server Implementation

### File: `example_mcp_server.erl`

```erlang
-module(example_mcp_server).
-behaviour(gen_server).

%% API
-export([start_link/0, stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-record(state, {
    mcp_server :: pid(),
    transports :: [pid()]
}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:call(?MODULE, stop).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    logger:info("Starting example MCP server"),

    %% Start required applications
    application:ensure_all_started(erlmcp_core),
    application:ensure_all_started(erlmcp_transports),

    %% Define server capabilities
    Capabilities = #{
        tools => #{
            listChanged => true
        },
        resources => #{
            subscribe => true,
            listChanged => true
        },
        prompts => #{
            listChanged => true
        }
    },

    %% Start MCP server
    {ok, McpServer} = erlmcp_server:start_link(example_server, Capabilities),

    %% Register tools
    register_tools(McpServer),

    %% Register resources
    register_resources(McpServer),

    %% Register prompts
    register_prompts(McpServer),

    %% Start transports
    Transports = start_transports(McpServer),

    logger:info("Example MCP server started successfully"),

    {ok, #state{
        mcp_server = McpServer,
        transports = Transports
    }}.

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{transports = Transports}) ->
    %% Stop all transports
    lists:foreach(fun(Transport) ->
        catch erlmcp_transport:stop(Transport)
    end, Transports),
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

register_tools(Server) ->
    %% Echo tool
    erlmcp_server:add_tool(Server, #{
        name => <<"echo">>,
        description => <<"Echo back the input text">>,
        input_schema => #{
            type => object,
            properties => #{
                text => #{
                    type => string,
                    description => <<"Text to echo">>
                }
            },
            required => [text]
        },
        handler => fun(Args) ->
            Text = maps:get(<<"text">>, Args),
            #{content => [#{
                type => text,
                text => <<"Echo: ", Text/binary>>
            }]}
        end
    }),

    %% Calculator tool
    erlmcp_server:add_tool(Server, #{
        name => <<"calculate">>,
        description => <<"Perform arithmetic operations">>,
        input_schema => #{
            type => object,
            properties => #{
                a => #{type => number, description => <<"First number">>},
                b => #{type => number, description => <<"Second number">>},
                op => #{
                    type => string,
                    description => <<"Operation">>,
                    enum => [<<"add">>, <<"subtract">>, <<"multiply">>, <<"divide">>]
                }
            },
            required => [a, b, op]
        },
        handler => fun(Args) ->
            A = maps:get(<<"a">>, Args),
            B = maps:get(<<"b">>, Args),
            Op = maps:get(<<"op">>, Args),

            try
                Result = case Op of
                    <<"add">> -> A + B;
                    <<"subtract">> -> A - B;
                    <<"multiply">> -> A * B;
                    <<"divide">> when B =/= 0 -> A / B;
                    <<"divide">> -> error(division_by_zero)
                end,
                #{content => [#{
                    type => text,
                    text => float_to_binary(Result, [{decimals, 4}])
                }]}
            catch
                error:division_by_zero ->
                    #{
                        content => [#{
                            type => text,
                            text => <<"Error: Division by zero">>
                        }],
                        isError => true
                    }
            end
        end
    }),

    %% System info tool
    erlmcp_server:add_tool(Server, #{
        name => <<"system_info">>,
        description => <<"Get system information">>,
        handler => fun(_Args) ->
            Info = #{
                erlang_version => erlang:system_info(otp_release),
                process_count => erlang:system_info(process_count),
                memory_total => erlang:memory(total),
                uptime => element(1, erlang:statistics(wall_clock))
            },
            #{content => [#{
                type => text,
                text => jsx:encode(Info)
            }]}
        end
    }),

    %% Long running task with progress
    erlmcp_server:add_tool(Server, #{
        name => <<"long_task">>,
        description => <<"A long-running task with progress updates">>,
        handler => fun(Args) ->
            ProgressToken = maps:get(<<"_progressToken">>, Args, undefined),
            Steps = 5,

            lists:foreach(fun(Step) ->
                timer:sleep(1000),
                case ProgressToken of
                    undefined -> ok;
                    Token ->
                        erlmcp_server:report_progress(
                            Server, Token,
                            Step, Steps,
                            <<"Processing step ", (integer_to_binary(Step))/binary, "...">>
                        )
                end
            end, lists:seq(1, Steps)),

            #{content => [#{
                type => text,
                text => <<"Task completed!">>
            }]}
        end,
        timeout => 10000
    }).

register_resources(Server) ->
    %% Server status resource
    erlmcp_server:add_resource(Server, #{
        uri => <<"mcp://status">>,
        name => <<"Server Status">>,
        description => <<"Current server status">>,
        mime_type => <<"application/json">>,
        handler => fun(_Uri) ->
            Status = #{
                status => running,
                uptime_ms => element(1, erlang:statistics(wall_clock)),
                processes => erlang:system_info(process_count),
                memory_mb => erlang:memory(total) div 1048576
            },
            #{content => [#{
                type => text,
                text => jsx:encode(Status)
            }]}
        end
    }),

    %% Configuration resource
    erlmcp_server:add_resource(Server, #{
        uri => <<"mcp://config">>,
        name => <<"Configuration">>,
        description => <<"Server configuration">>,
        mime_type => <<"application/json">>,
        handler => fun(_Uri) ->
            Config = #{
                server_name => <<"example_server">>,
                version => <<"1.0.0">>,
                features => [<<"tools">>, <<"resources">>, <<"prompts">>]
            },
            #{content => [#{
                type => text,
                text => jsx:encode(Config)
            }]}
        end
    }),

    %% Log resource template
    erlmcp_server:add_resource_template(Server, #{
        uri_template => <<"logs://{level}">>,
        name => <<"Log Messages">>,
        description => <<"Access log messages by level">>,
        mime_type => <<"text/plain">>,
        handler => fun(_Template, Variables) ->
            Level = proplists:get_value(level, Variables),
            Logs = get_logs_by_level(Level),
            #{content => [#{
                type => text,
                text => list_to_binary(Logs)
            }]}
        end
    }).

register_prompts(Server) ->
    %% Code generation prompt
    erlmcp_server:add_prompt(Server, #{
        name => <<"write_code">>,
        description => <<"Generate a code writing prompt">>,
        arguments => [
            #{
                name => <<"language">>,
                description => <<"Programming language">>,
                required => true
            },
            #{
                name => <<"task">>,
                description => <<"What the code should do">>,
                required => true
            }
        ],
        handler => fun(Args) ->
            Language = maps:get(<<"language">>, Args),
            Task = maps:get(<<"task">>, Args),

            #{
                description => <<"Code generation prompt">>,
                messages => [
                    #{
                        role => system,
                        content => #{
                            type => text,
                            text => <<"You are an expert programmer.">>
                        }
                    },
                    #{
                        role => user,
                        content => #{
                            type => text,
                            text => <<"Write ", Language/binary, " code to: ", Task/binary>>
                        }
                    }
                ]
            }
        end
    }).

start_transports(Server) ->
    Transports = [],

    %% Start stdio transport
    StdioTransport = case erlmcp_transport_stdio:start_link(Server) of
        {ok, Pid1} ->
            logger:info("stdio transport started"),
            [Pid1];
        {error, Reason1} ->
            logger:warning("Failed to start stdio transport: ~p", [Reason1]),
            []
    end,

    %% Start HTTP transport
    HttpTransport = case erlmcp_transport_http:start_link(Server, [
        {port, 8765},
        {ip, {0, 0, 0, 0}}
    ]) of
        {ok, Pid2} ->
            logger:info("HTTP transport started on port 8765"),
            [Pid2];
        {error, Reason2} ->
            logger:warning("Failed to start HTTP transport: ~p", [Reason2]),
            []
    end,

    %% Start WebSocket transport
    WsTransport = case erlmcp_transport_ws:start_link(Server, [
        {port, 8766},
        {path, "/mcp"}
    ]) of
        {ok, Pid3} ->
            logger:info("WebSocket transport started on port 8766"),
            [Pid3];
        {error, Reason3} ->
            logger:warning("Failed to start WebSocket transport: ~p", [Reason3]),
            []
    end,

    Transports ++ StdioTransport ++ HttpTransport ++ WsTransport.

get_logs_by_level(Level) ->
    %% Mock implementation
    io_lib:format("Sample ~s log message~n", [Level]).
```

---

## Client Implementation

### File: `example_mcp_client.erl`

```erlang
-module(example_mcp_client).
-export([start/0, run_examples/1]).

start() ->
    %% Start required applications
    application:ensure_all_started(erlmcp_core),

    %% Connect to HTTP server
    {ok, HttpClient} = connect_http(),
    io:format("~nConnected to HTTP server~n"),

    %% Run examples
    run_examples(HttpClient),

    %% Keep client alive
    receive
        stop -> ok
    after 60000 ->
        ok
    end.

connect_http() ->
    erlmcp_client:start_link(#{
        transport => http,
        transport_opts => #{
            host => "localhost",
            port => 8765
        },
        capabilities => #{},
        client_info => #{
            name => <<"example-client">>,
            version => <<"1.0.0">>
        }
    }).

run_examples(Client) ->
    %% Get server info
    example_server_info(Client),

    %% List and call tools
    example_tools(Client),

    %% Access resources
    example_resources(Client),

    %% Use prompts
    example_prompts(Client),

    %% Progress tracking
    example_progress(Client),

    %% Error handling
    example_error_handling(Client).

example_server_info(Client) ->
    io:format("~n=== Server Info ===~n"),
    {ok, ServerInfo} = erlmcp_client:get_server_info(Client),
    #{
        <<"name">> := Name,
        <<"version">> := Version
    } = ServerInfo,
    io:format("Connected to: ~s v~s~n", [Name, Version]),

    {ok, Caps} = erlmcp_client:get_server_capabilities(Client),
    io:format("Server capabilities: ~p~n", [maps:keys(Caps)]).

example_tools(Client) ->
    io:format("~n=== Tools ===~n"),

    %% List tools
    {ok, #{<<"tools">> := Tools}} = erlmcp_client:list_tools(Client),
    io:format("Available tools:~n"),
    lists:foreach(fun(Tool) ->
        Name = maps:get(<<"name">>, Tool),
        Desc = maps:get(<<"description">>, Tool),
        io:format("  - ~s: ~s~n", [Name, Desc])
    end, Tools),

    %% Call echo tool
    {ok, EchoResult} = erlmcp_client:call_tool(Client, <<"echo">>, #{
        <<"text">> => <<"Hello from Erlang!">>
    }),
    #{<<"content">> := [#{<<"text">> := EchoText}]} = EchoResult,
    io:format("~nEcho result: ~s~n", [EchoText]),

    %% Call calculator
    {ok, CalcResult} = erlmcp_client:call_tool(Client, <<"calculate">>, #{
        <<"a">> => 42,
        <<"b">> => 8,
        <<"op">> => <<"multiply">>
    }),
    #{<<"content">> := [#{<<"text">> := CalcText}]} = CalcResult,
    io:format("Calculate result: ~s~n", [CalcText]),

    %% Get system info
    {ok, SysResult} = erlmcp_client:call_tool(Client, <<"system_info">>, #{}),
    #{<<"content">> := [#{<<"text">> := SysText}]} = SysResult,
    SysInfo = jsx:decode(SysText, [return_maps]),
    io:format("System info: ~p~n", [SysInfo]).

example_resources(Client) ->
    io:format("~n=== Resources ===~n"),

    %% List resources
    {ok, #{<<"resources">> := Resources}} = erlmcp_client:list_resources(Client),
    io:format("Available resources:~n"),
    lists:foreach(fun(Resource) ->
        Uri = maps:get(<<"uri">>, Resource),
        Name = maps:get(<<"name">>, Resource),
        io:format("  - ~s (~s)~n", [Name, Uri])
    end, Resources),

    %% Read status resource
    {ok, StatusContents} = erlmcp_client:read_resource(Client, <<"mcp://status">>),
    #{<<"contents">> := [#{<<"text">> := StatusText}]} = StatusContents,
    Status = jsx:decode(StatusText, [return_maps]),
    io:format("~nServer status: ~p~n", [Status]),

    %% Read config resource
    {ok, ConfigContents} = erlmcp_client:read_resource(Client, <<"mcp://config">>),
    #{<<"contents">> := [#{<<"text">> := ConfigText}]} = ConfigContents,
    Config = jsx:decode(ConfigText, [return_maps]),
    io:format("Server config: ~p~n", [Config]).

example_prompts(Client) ->
    io:format("~n=== Prompts ===~n"),

    %% List prompts
    {ok, #{<<"prompts">> := Prompts}} = erlmcp_client:list_prompts(Client),
    io:format("Available prompts:~n"),
    lists:foreach(fun(Prompt) ->
        Name = maps:get(<<"name">>, Prompt),
        Desc = maps:get(<<"description">>, Prompt),
        io:format("  - ~s: ~s~n", [Name, Desc])
    end, Prompts),

    %% Get code writing prompt
    {ok, Prompt} = erlmcp_client:get_prompt(Client, <<"write_code">>, #{
        <<"language">> => <<"Erlang">>,
        <<"task">> => <<"calculate factorial">>
    }),
    #{<<"messages">> := Messages} = Prompt,
    io:format("~nPrompt messages:~n"),
    lists:foreach(fun(Msg) ->
        Role = maps:get(<<"role">>, Msg),
        Content = maps:get(<<"content">>, Msg),
        io:format("  [~s]: ~p~n", [Role, Content])
    end, Messages).

example_progress(Client) ->
    io:format("~n=== Progress Tracking ===~n"),

    %% Create progress token
    ProgressToken = <<"progress-", (integer_to_binary(erlang:unique_integer([positive])))/binary>>,

    %% Register progress callback
    erlmcp_client:on_progress(Client, ProgressToken, fun(Progress) ->
        #{
            <<"progress">> := Current,
            <<"total">> := Total,
            <<"message">> := Msg
        } = Progress,
        Percent = (Current / Total) * 100,
        io:format("Progress: ~.0f%% - ~s~n", [Percent, Msg])
    end),

    %% Call long-running task
    io:format("Starting long task...~n"),
    {ok, Result} = erlmcp_client:call_tool(Client, <<"long_task">>, #{}, #{
        progress_token => ProgressToken
    }),
    #{<<"content">> := [#{<<"text">> := ResultText}]} = Result,
    io:format("Task result: ~s~n", [ResultText]).

example_error_handling(Client) ->
    io:format("~n=== Error Handling ===~n"),

    %% Try to call nonexistent tool
    case erlmcp_client:call_tool(Client, <<"nonexistent">>, #{}) of
        {ok, _} ->
            io:format("Unexpected success~n");
        {error, {jsonrpc_error, -32002, Message, _}} ->
            io:format("Expected error: ~s~n", [Message])
    end,

    %% Try division by zero
    case erlmcp_client:call_tool(Client, <<"calculate">>, #{
        <<"a">> => 10,
        <<"b">> => 0,
        <<"op">> => <<"divide">>
    }) of
        {ok, #{<<"isError">> := true, <<"content">> := [#{<<"text">> := ErrText}]}} ->
            io:format("Tool error: ~s~n", [ErrText]);
        {ok, _} ->
            io:format("Expected error response~n")
    end.
```

---

## Running the Example

### Using Docker (Recommended)

```bash
# Start the server via Docker
docker compose run --rm erlmcp-server erl \
  -pa /app/_build/default/lib/*/ebin \
  -eval "example_mcp_server:start_link()" \
  -noshell

# In another terminal, run the client
docker compose run --rm erlmcp-client erl \
  -pa /app/_build/default/lib/*/ebin \
  -eval "example_mcp_client:start()" \
  -noshell
```

### Local Development (Testing Only)

```bash
# Compile the code
erlc -I /path/to/erlmcp/apps/erlmcp_core/include \
     -pa /path/to/erlmcp/_build/default/lib/*/ebin \
     example_mcp_server.erl example_mcp_client.erl

# Start the server
erl -pa /path/to/erlmcp/_build/default/lib/*/ebin \
    -eval "example_mcp_server:start_link()" \
    -noshell

# In another terminal, start the client
erl -pa /path/to/erlmcp/_build/default/lib/*/ebin \
    -eval "example_mcp_client:start()" \
    -noshell
```

---

## Expected Output

```
Connected to HTTP server

=== Server Info ===
Connected to: erlmcp v3.0.0
Server capabilities: [<<"tools">>,<<"resources">>,<<"prompts">>]

=== Tools ===
Available tools:
  - echo: Echo back the input text
  - calculate: Perform arithmetic operations
  - system_info: Get system information
  - long_task: A long-running task with progress updates

Echo result: Echo: Hello from Erlang!
Calculate result: 336.0000
System info: #{<<"erlang_version">> => <<"28">>,...}

=== Resources ===
Available resources:
  - Server Status (mcp://status)
  - Configuration (mcp://config)

Server status: #{<<"status">> => <<"running">>,...}
Server config: #{<<"server_name">> => <<"example_server">>,...}

=== Prompts ===
Available prompts:
  - write_code: Generate a code writing prompt

Prompt messages:
  [system]: #{<<"type">> => <<"text">>,...}
  [user]: #{<<"type">> => <<"text">>,...}

=== Progress Tracking ===
Starting long task...
Progress: 20% - Processing step 1...
Progress: 40% - Processing step 2...
Progress: 60% - Processing step 3...
Progress: 80% - Processing step 4...
Progress: 100% - Processing step 5...
Task result: Task completed!

=== Error Handling ===
Expected error: Tool not found
Tool error: Error: Division by zero
```

---

## Key Takeaways

1. **Server Setup**: Define capabilities, register handlers, start transports
2. **Client Connection**: Connect to server, exchange capabilities
3. **Tool Calls**: List tools, call with arguments, handle results
4. **Resources**: List and read both static and dynamic resources
5. **Progress**: Track long-running operations with progress tokens
6. **Error Handling**: Gracefully handle errors at multiple levels
7. **Multiple Transports**: Support stdio, HTTP, WebSocket simultaneously

---

## See Also

- [Server API Reference](server-api.md) - Complete server API
- [Client API Reference](client-api.md) - Complete client API
- [Error Handling Patterns](error-handling-patterns.md) - Error handling guide
- [Transport Protocols](transport-protocols.md) - Transport configuration
