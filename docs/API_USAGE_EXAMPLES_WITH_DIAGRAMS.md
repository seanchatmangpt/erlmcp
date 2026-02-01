# API Usage Examples with Diagram References

This document provides practical examples of using the erlmcp API with visual diagram references for better understanding.

## Table of Contents

1. [Quick Start Examples](#quick-start-examples)
2. [Client Examples](#client-examples)
3. [Server Examples](#server-examples)
4. [Resource Management](#resource-management)
5. [Tool Implementation](#tool-implementation)
6. [Error Handling](#error-handling)
7. [Advanced Patterns](#advanced-patterns)

---

## Quick Start Examples

### Minimal Client Example

```erlang
%% Start a basic STDIO client
{ok, Client} = erlmcp_client:start_link(
    {stdio, []},
    #{timeout => 5000}
).

%% Initialize connection
{ok, InitResult} = erlmcp_client:initialize(Client, #{
    <<"protocolVersion">> => <<"2025-11-25">>,
    <<"capabilities">> => #{
        <<"roots">> => #{},
        <<"sampling">> => #{}
    },
    <<"clientInfo">> => #{
        <<"name">> => <<"my-client">>,
        <<"version">> => <<"1.0.0">>
    }
}
).
```

**Flow Reference**: See [Client API Flow](./api-reference.md#request-response-flow) and [Capability Negotiation](./MCP_ENDPOINTS_AND_CAPABILITIES_GUIDE.md#capability-negotiation-flow)

### Minimal Server Example

```erlang
%% Start a basic TCP server
{ok, Server} = erlmcp_server:start_link(
    {tcp, #{port => 8080}},
    #mcp_server_capabilities{
        resources = #mcp_resources_capability{
            subscribe = true,
            list_changed = true
        },
        tools = #mcp_tools_capability{
            list_changed = true
        }
    }
).
```

**Flow Reference**: See [Server Architecture Flow](./api-reference.md#server-architecture-flow)

---

## Client Examples

### Example 1: Resource Discovery and Reading

```erlang
%% Start client
{ok, Client} = erlmcp_client:start_link(
    {stdio, []},
    #{}
).

%% Initialize
{ok, _} = erlmcp_client:initialize(Client, #{
    <<"protocolVersion">> => <<"2025-11-25">>,
    <<"capabilities">> => #{<<"resources">> => #{<<>> => true}},
    <<"clientInfo">> => #{
        <<"name">> => <<"resource-client">>,
        <<"version">> => <<"1.0.0">>
    }
}).

%% List available resources
{ok, #{<<"resources">> := Resources}} = erlmcp_client:list_resources(Client).

%% Read a specific resource
{ok, #{<<"contents">> := Contents}} = erlmcp_client:read_resource(
    Client,
    <<"config://app">>
).

%% Process content
lists:foreach(fun(#{<<"type">> := <<"text">>, <<"text">> := Text}) ->
    io:format("Content: ~s~n", [Text])
end, Contents).
```

**Diagram Reference**:
- [Resource API Flow](./MCP_ENDPOINTS_AND_CAPABILITIES_GUIDE.md#resource-api-flow)
- [Request-Response Flow](./api-reference.md#request-response-flow)

### Example 2: Tool Execution

```erlang
%% Start and initialize client
{ok, Client} = erlmcp_client:start_link({stdio, [], #{}).
{ok, _} = erlmcp_client:initialize(Client, ClientCaps).

%% List available tools
{ok, #{<<"tools">> := Tools}} = erlmcp_client:list_tools(Client).

%% Execute a tool
{ok, #{<<"content">> := Content}} = erlmcp_client:call_tool(
    Client,
    <<"sql_query">>,
    #{<<"query">> => <<"SELECT * FROM users LIMIT 10">>}
).

%% Handle tool response
case Content of
    [#{<<"type">> := <<"text">>, <<"text">> := Result}] ->
        io:format("Query result: ~s~n", [Result]);
    [#{<<"type">> := <<"text">>, <<"text">> := Error}, #{<<"isError">> := true}] ->
        io:format("Tool error: ~s~n", [Error])
end.
```

**Diagram Reference**:
- [Tool API Flow](./MCP_ENDPOINTS_AND_CAPABILITIES_GUIDE.md#tool-api-flow)
- [Tool Execution Sequence](./diagrams/protocol/json-rpc-flow.mmd)

### Example 3: Resource Subscription

```erlang
%% Start client with notification handler
{ok, Client} = erlmcp_client:start_link(
    {stdio, []},
    #{notification_handler => fun handle_notification/2}
).

%% Initialize
{ok, _} = erlmcp_client:initialize(Client, ClientCaps).

%% Subscribe to resource updates
ok = erlmcp_client:subscribe_to_resource(
    Client,
    <<"weather://city">>
).

%% Handle notifications
handle_notification(<<"resources/updated">>, Params) ->
    #{<<"uri">> := Uri, <<"metadata">> := Metadata} = Params,
    io:format("Resource updated: ~s~n", [Uri]),
    io:format("Metadata: ~p~n", [Metadata]);

handle_notification(<<"resources/list_changed">>, _Params) ->
    io:format("Resource list changed, refreshing...~n"),
    {ok, #{<<"resources">> := Resources}} = erlmcp_client:list_resources(Client),
    %% Process new resource list
    ok.
```

**Diagram Reference**:
- [Subscription Flow](./api-reference.md#resource-subscription-flow)
- [Notification Methods](./MCP_ENDPOINTS_AND_CAPABILITIES_GUIDE.md#notification-methods)

---

## Server Examples

### Example 1: Simple Echo Server

```erlang
-module(echo_server).
-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%%====================================================================
%% API
%%====================================================================

start_link() ->
    erlmcp_server:start_link(
        {stdio, []},
        #mcp_server_capabilities{
            tools = #mcp_tools_capability{}
        }
    ).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    %% Register echo tool
    {ok, Server} = erlmcp_server:start_link(...),

    erlmcp_server:add_tool(Server, #{
        name => <<"echo">>,
        description => <<"Echo back the input text">>,
        input_schema => #{
            <<"type">> => <<"object">>,
            <<"properties">> => #{
                <<"text">> => #{
                    <<"type">> => <<"string">>,
                    <<"description">> => <<"Text to echo back">>
                }
            },
            <<"required">> => [<<"text">>]
        }
    }, fun echo_handler/1),

    {ok, #{server => Server}}.

echo_handler(#{<<"text">> := Text}) ->
    #{
        <<"content">> => [
            #{
                <<"type">> => <<"text">>,
                <<"text">> => <<"Echo: ", Text/binary>>
            }
        ],
        <<"isError">> => false
    }.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
```

**Diagram Reference**:
- [Server Request Processing Flow](./reference/api-reference/server.md#server-request-processing-flow)
- [Tool Execution Sequence](./diagrams/protocol/json-rpc-flow.mmd)

### Example 2: Resource Server with Templates

```erlang
-module(template_server).
-export([start_link/0, init/1]).

start_link() ->
    erlmcp_server:start_link(
        {tcp, #{port => 8080}},
        #mcp_server_capabilities{
            resources = #mcp_resources_capability{
                subscribe = true
            }
        }
    ).

init([]) ->
    {ok, Server} = ?MODULE:start_link(),

    %% Add static resource
    erlmcp_server:add_resource(Server,
        <<"config://app">>,
        fun(_Uri) ->
            #{
                <<"contents">> => [
                    #{
                        <<"type">> => <<"text">>,
                        <<"text">> => jsx:encode(get_app_config())
                    }
                ]
            }
        end
    ),

    %% Add resource template
    erlmcp_server:add_resource_template(Server,
        <<"user://{id}/profile">>,
        <<"User Profile">>,
        fun user_profile_handler/1
    ),

    {ok, #{}}.

user_profile_handler(#{<<"id">> := UserId}) ->
    case get_user_profile(UserId) of
        {ok, Profile} ->
            #{
                <<"contents">> => [
                    #{
                        <<"type">> => <<"text">>,
                        <<"text">> => jsx:encode(Profile)
                    }
                ]
            };
        {error, not_found} ->
            {error, not_found}
    end.
```

**Diagram Reference**:
- [Resource API Flow](./MCP_ENDPOINTS_AND_CAPABILITIES_GUIDE.md#resource-api-flow)
- [Server Architecture](./api-reference.md#server-architecture-flow)

---

## Resource Management

### Example: Multi-Resource Server

```erlang
-module(multi_resource_server).
-export([start_link/0]).

start_link() ->
    {ok, Server} = erlmcp_server:start_link(
        {stdio, []},
        #mcp_server_capabilities{
            resources = #mcp_resources_capability{
                subscribe = true,
                list_changed = true
            }
        }
    ),

    %% Register multiple resources
    lists:foreach(fun({Uri, Handler}) ->
        erlmcp_server:add_resource(Server, Uri, Handler)
    end, [
        {<<"config://database">>, fun db_config_handler/1},
        {<<"config://cache">>, fun cache_config_handler/1},
        {<<"status://health">>, fun health_status_handler/1},
        {<<"logs://app">>, fun app_logs_handler/1}
    ]),

    %% Enable notifications
    erlmcp_server:notify_resources_changed(Server),

    {ok, Server}.

%% Resource handlers
db_config_handler(<<"config://database">>) ->
    Config = get_db_config(),
    #{
        <<"contents">> => [
            #{
                <<"type">> => <<"text">>,
                <<"text">> => jsx:encode(Config),
                <<"mimeType">> => <<"application/json">>
            }
        ]
    }.

cache_config_handler(<<"config://cache">>) ->
    %% Similar implementation
    ok.

health_status_handler(<<"status://health">>) ->
    Status = check_system_health(),
    #{
        <<"contents">> => [
            #{
                <<"type">> => <<"text">>,
                <<"text">> => format_health_report(Status)
            }
        ]
    }.

app_logs_handler(<<"logs://app">>) ->
    Logs = get_recent_logs(),
    #{
        <<"contents">> => [
            #{
                <<"type">> => <<"text">>,
                <<"text">> => Logs
            }
        ]
    }.
```

**Diagram Reference**:
- [Resource Subscription Flow](./api-reference.md#resource-subscription-flow)
- [Resource API Flow](./MCP_ENDPOINTS_AND_CAPABILITIES_GUIDE.md#resource-api-flow)

---

## Tool Implementation

### Example: Database Query Tool

```erlang
-module(db_tool_server).
-export([start_link/0]).

start_link() ->
    {ok, Server} = erlmcp_server:start_link(
        {tcp, #{port => 8080}},
        #mcp_server_capabilities{
            tools = #mcp_tools_capability{}
        }
    ),

    %% Register SQL query tool with schema
    Schema = #{
        <<"type">> => <<"object">>,
        <<"properties">> => #{
            <<"query">> => #{
                <<"type">> => <<"string">>,
                <<"description">> => <<"SQL query to execute">>
            },
            <<"limit">> => #{
                <<"type">> => <<"integer">>,
                <<"minimum">> => 1,
                <<"maximum">> => 1000,
                <<"default">> => 100,
                <<"description">> => <<"Maximum rows to return">>
            }
        },
        <<"required">> => [<<"query">>]
    },

    erlmcp_server:add_tool_with_schema(
        Server,
        <<"sql_query">>,
        fun sql_query_handler/1,
        Schema
    ),

    %% Register table list tool
    erlmcp_server:add_tool(Server,
        <<"list_tables">>,
        fun list_tables_handler/1
    ),

    {ok, Server}.

%% Tool handlers
sql_query_handler(#{<<"query">> := Query, <<"limit">> := Limit}) ->
    case execute_sql(Query, Limit) of
        {ok, Results} ->
            #{
                <<"content">> => [
                    #{
                        <<"type">> => <<"text">>,
                        <<"text">> => jsx:encode(Results),
                        <<"mimeType">> => <<"application/json">>
                    }
                ],
                <<"isError">> => false
            };
        {error, Reason} ->
            #{
                <<"content">> => [
                    #{
                        <<"type">> => <<"text">>,
                        <<"text">> => io_lib:format("SQL Error: ~p", [Reason])
                    }
                ],
                <<"isError">> => true
            }
    end.

sql_query_handler(#{<<"query">> := Query}) ->
    sql_query_handler(#{<<"query">> => Query, <<"limit">> => 100}).

list_tables_handler(_Args) ->
    {ok, Tables} = get_database_tables(),
    #{
        <<"content">> => [
            #{
                <<"type">> => <<"text">>,
                <<"text">> => jsx:encode(#{<<"tables">> => Tables})
            }
        ],
        <<"isError">> => false
    }.
```

**Diagram Reference**:
- [Tool API Flow](./MCP_ENDPOINTS_AND_CAPABILITIES_GUIDE.md#tool-api-flow)
- [Server Request Processing](./reference/api-reference/server.md#server-request-processing-flow)

### Example: File System Tools

```erlang
-module(fs_tool_server).
-export([start_link/0]).

start_link() ->
    {ok, Server} = erlmcp_server:start_link(
        {stdio, []},
        #mcp_server_capabilities{
            tools = #mcp_tools_capability{}
        }
    ),

    %% Register file operations
    lists:foreach(fun({Name, Handler, Schema}) ->
        case Schema of
            undefined ->
                erlmcp_server:add_tool(Server, Name, Handler);
            _ ->
                erlmcp_server:add_tool_with_schema(Server, Name, Handler, Schema)
        end
    end, [
        {<<"read_file">>, fun read_file_handler/1, read_file_schema()},
        {<<"write_file">>, fun write_file_handler/1, write_file_schema()},
        {<<"list_dir">>, fun list_dir_handler/1, list_dir_schema()},
        {<<"file_info">>, fun file_info_handler/1, file_info_schema()}
    ]),

    {ok, Server}.

%% Tool handlers
read_file_handler(#{<<"path">> := Path}) ->
    case file:read_file(binary_to_list(Path)) of
        {ok, Content} ->
            #{
                <<"content">> => [
                    #{
                        <<"type">> => <<"text">>,
                        <<"text">> => Content
                    }
                ],
                <<"isError">> => false
            };
        {error, Reason} ->
            error_response(Reason, <<"read_file">>)
    end.

list_dir_handler(#{<<"path">> := Path}) ->
    case file:list_dir(binary_to_list(Path)) of
        {ok, Files} ->
            #{
                <<"content">> => [
                    #{
                        <<"type">> => <<"text">>,
                        <<"text">> => jsx:encode(#{<<"files">> => Files})
                    }
                ],
                <<"isError">> => false
            };
        {error, Reason} ->
            error_response(Reason, <<"list_dir">>)
    end.

%% Schemas
read_file_schema() ->
    #{
        <<"type">> => <<"object">>,
        <<"properties">> => #{
            <<"path">> => #{
                <<"type">> => <<"string">>,
                <<"description">> => <<"File path to read">>
            }
        },
        <<"required">> => [<<"path">>]
    }.

list_dir_schema() ->
    #{
        <<"type">> => <<"object">>,
        <<"properties">> => #{
            <<"path">> => #{
                <<"type">> => <<"string">>,
                <<"description">> => <<"Directory path to list">>
            }
        },
        <<"required">> => [<<"path">>]
    }.

%% Helper
error_response(Reason, Operation) ->
    #{
        <<"content">> => [
            #{
                <<"type">> => <<"text">>,
                <<"text">> => io_lib:format(
                    "~s error: ~p",
                    [Operation, Reason]
                )
            }
        ],
        <<"isError">> => true
    }.
```

---

## Error Handling

### Example: Comprehensive Error Handling

```erlang
-module(robust_client).
-export([start_with_retry/0, safe_tool_call/3]).

start_with_retry() ->
    case erlmcp_client:start_link({stdio, [], #{}) of
        {ok, Client} ->
            case initialize_with_timeout(Client, 10000) of
                {ok, _} ->
                    {ok, Client};
                {error, Reason} ->
                    io:format("Initialize failed: ~p~n", [Reason]),
                    {error, initialization_failed}
            end;
        {error, Reason} ->
            io:format("Start failed: ~p~n", [Reason]),
            {error, start_failed}
    end.

initialize_with_timeout(Client, Timeout) ->
    try
        InitResult = erlmcp_client:initialize(Client, get_client_caps()),
        receive
            {init_complete, Result} -> Result
        after Timeout ->
            {error, timeout}
        end
    catch
        Error:Reason:Stacktrace ->
            io:format("Initialize exception: ~p:~p~nStack: ~p~n",
                     [Error, Reason, Stacktrace]),
            {error, exception}
    end.

safe_tool_call(Client, ToolName, Args) ->
    try
        case erlmcp_client:call_tool(Client, ToolName, Args) of
            {ok, #{<<"content">> := Content, <<"isError">> := false}} ->
                {ok, Content};
            {ok, #{<<"content">> := Content, <<"isError">> := true}} ->
                {error, {tool_error, Content}};
            {error, {error_response, Error}} ->
                #{<<"code">> := Code, <<"message">> := Message} = Error,
                handle_error_code(Code, Message, Error);
            {error, Reason} ->
                {error, {client_error, Reason}}
        end
    catch
        Error:Reason ->
            {error, {exception, Error, Reason}}
    end.

%% Error code handlers
handle_error_code(-32002, Message, Error) ->
    %% Tool not found
    io:format("Tool not found: ~s~n", [Message]),
    {error, tool_not_found};

handle_error_code(-32009, Message, Error) ->
    %% Request timeout
    io:format("Request timeout: ~s~n", [Message]),
    {error, timeout};

handle_error_code(-32010, Message, Error) ->
    %% Rate limited
    io:format("Rate limited: ~s~n", [Message]),
    {error, rate_limited};

handle_error_code(Code, Message, Error) ->
    %% Generic error
    io:format("Error ~p: ~s~n", [Code, Message]),
    {error, {unknown_error, Code, Message}}.
```

**Diagram Reference**:
- [Error Handling Flow](./MCP_ENDPOINTS_AND_CAPABILITIES_GUIDE.md#error-handling-flow)
- [Session Lifecycle](./MCP_ENDPOINTS_AND_CAPABILITIES_GUIDE.md#session-lifecycle)

---

## Advanced Patterns

### Example: Progress Tracking

```erlang
-module(progress_client).
-export([long_running_task/1]).

long_running_task(Client) ->
    %% Generate progress token
    ProgressToken = generate_progress_token(),

    %% Set up progress handler
    erlmcp_client:set_notification_handler(
        Client,
        <<"notifications/progress">>,
        fun(Progress) ->
            #{<<"progressToken">> := Token,
              <<"progress">> := ProgressValue,
              <<"total">> := Total} = Progress,

            Percentage = (ProgressValue / Total) * 100,
            io:format("Progress: ~.1f%~n", [Percentage])
        end
    ),

    %% Execute tool with progress token
    Args = #{
        <<"operation">> => <<"long_task">>,
        <<"_meta">> => #{
            <<"progressToken">> => ProgressToken
        }
    },

    case erlmcp_client:call_tool(Client, <<"async_task">>, Args) of
        {ok, Result} ->
            io:format("Task completed: ~p~n", [Result]);
        {error, Reason} ->
            io:format("Task failed: ~p~n", [Reason])
    end.
```

**Diagram Reference**:
- [Progress Notification Flow](./MCP_ENDPOINTS_AND_CAPABILITIES_GUIDE.md#notification-methods)
- [Task Lifecycle](./MCP_ENDPOINTS_AND_CAPABILITIES_GUIDE.md#task-lifecycle-state-diagram)

### Example: Batch Operations

```erlang
-module(batch_client).
-export([batch_tools/2]).

batch_tools(Client, ToolCalls) ->
    %% Execute multiple tools in batch
    {ok, BatchId} = erlmcp_client:with_batch(Client, fun(BatchId) ->
        lists:map(fun({ToolName, Args}) ->
            erlmcp_client:send_batch_request(
                Client,
                BatchId,
                ToolName,
                Args
            )
        end, ToolCalls)
    end).

%% Usage
batch_tools(Client, [
    {<<"get_user">>, #{<<"id">> => 1}},
    {<<"get_user">>, #{<<"id">> => 2}},
    {<<"get_user">>, #{<<"id">> => 3}}
]).
```

**Diagram Reference**:
- [Batch Request Flow](./protocol/MCP_JSON_RPC_SPECIFICATION.md#batch-request-processing)

### Example: Reconnection Handling

```erlang
-module(resilient_client).
-export([start_with_auto_reconnect/0]).

start_with_auto_reconnect() ->
    {ok, Client} = erlmcp_client:start_link(
        {tcp, #{
            host => "localhost",
            port => 8080,
            max_reconnect_attempts => 10,
            reconnect_delay => 1000
        }},
        #{
            reconnect_handler => fun reconnect_handler/2
        }
    ).

reconnect_handler(disconnected, State) ->
    io:format("Disconnected, attempting to reconnect...~n"),
    {reconnect, State};

reconnect_handler(reconnected, State) ->
    io:format("Successfully reconnected!~n"),
    %% Re-initialize connection
    {ok, _} = erlmcp_client:initialize(self(), get_client_caps()),
    {ok, State};

reconnect_handler(reconnect_failed, State) ->
    io:format("Reconnection failed, will retry...~n"),
    {retry, State}.
```

**Diagram Reference**:
- [Session Lifecycle - Reconnecting State](./MCP_ENDPOINTS_AND_CAPABILITIES_GUIDE.md#session-lifecycle)
- [Error Handling Flow](./MCP_ENDPOINTS_AND_CAPABILITIES_GUIDE.md#error-handling-flow)

---

## Testing Examples

### Example: Testing Tool Execution

```erlang
-module(tool_tests).
-include_lib("eunit/include/eunit.hrl").

tool_execution_test() ->
    %% Start test server
    {ok, Server} = test_server:start_link(),
    {ok, Client} = test_client:start_link(Server),

    %% Initialize
    {ok, _} = erlmcp_client:initialize(Client, test_caps()),

    %% Test tool execution
    {ok, Result} = erlmcp_client:call_tool(
        Client,
        <<"echo">>,
        #{<<"text">> => <<"hello">>}
    ),

    %% Assert result
    #{
        <<"content">> := [
            #{
                <<"type">> := <<"text">>,
                <<"text">> := EchoText
            }
        ],
        <<"isError">> := false
    } = Result,

    ?assertEqual(<<"Echo: hello">>, EchoText).

error_handling_test() ->
    {ok, Server} = test_server:start_link(),
    {ok, Client} = test_client:start_link(Server),

    {ok, _} = erlmcp_client:initialize(Client, test_caps()),

    %% Test tool not found
    {error, {error_response, #{
        <<"code">> := -32002,
        <<"message">> := <<"Tool not found">>
    }}} = erlmcp_client:call_tool(
        Client,
        <<"nonexistent_tool">>,
        #{}
    ).
```

---

## Diagram Index

All examples reference these diagrams:

1. **Client API Flow**: [docs/api-reference.md](./api-reference.md)
2. **Server Architecture Flow**: [docs/api-reference.md](./api-reference.md)
3. **Request-Response Flow**: [docs/api-reference.md](./api-reference.md)
4. **Resource API Flow**: [docs/MCP_ENDPOINTS_AND_CAPABILITIES_GUIDE.md](./MCP_ENDPOINTS_AND_CAPABILITIES_GUIDE.md)
5. **Tool API Flow**: [docs/MCP_ENDPOINTS_AND_CAPABILITIES_GUIDE.md](./MCP_ENDPOINTS_AND_CAPABILITIES_GUIDE.md)
6. **Capability Negotiation**: [docs/MCP_ENDPOINTS_AND_CAPABILITIES_GUIDE.md](./MCP_ENDPOINTS_AND_CAPABILITIES_GUIDE.md)
7. **Error Handling Flow**: [docs/MCP_ENDPOINTS_AND_CAPABILITIES_GUIDE.md](./MCP_ENDPOINTS_AND_CAPABILITIES_GUIDE.md)
8. **Session Lifecycle**: [docs/MCP_ENDPOINTS_AND_CAPABILITIES_GUIDE.md](./MCP_ENDPOINTS_AND_CAPABILITIES_GUIDE.md)
9. **JSON-RPC Flow**: [docs/protocol/MCP_JSON_RPC_SPECIFICATION.md](./protocol/MCP_JSON_RPC_SPECIFICATION.md)
10. **Mermaid Diagrams**: [docs/diagrams/](./diagrams/)

---

**Related Documentation**:
- [API Reference](./api-reference.md)
- [MCP Endpoints Guide](./MCP_ENDPOINTS_AND_CAPABILITIES_GUIDE.md)
- [Protocol Specification](./protocol/MCP_JSON_RPC_SPECIFICATION.md)
- [Server Reference](./reference/api-reference/server.md)
