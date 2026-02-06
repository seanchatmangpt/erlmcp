# erlmcp Client API Reference

## Overview

The erlmcp client API provides functions for connecting to MCP servers and invoking server methods. The client handles protocol negotiation, request/response correlation, and transport management.

---

## Client Module: erlmcp_client

### start_link/1

Start a new MCP client connection.

**Signature:**
```erlang
start_link(Config) -> {ok, Pid} | {error, Reason}
```

**Parameters:**

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| Config | map() | Yes | Client configuration |

**Config Fields:**

| Field | Type | Required | Default | Description |
|-------|------|----------|---------|-------------|
| transport | atom() | Yes | - | Transport type: stdio, tcp, http, ws, sse |
| transport_opts | map() | No | #{} | Transport-specific options |
| capabilities | map() | No | #{} | Client capabilities |
| client_info | map() | Yes | - | Client name and version |
| protocol_version | binary() | No | <<"2025-11-25">> | MCP protocol version |
| timeout | integer() | No | 30000 | Request timeout in milliseconds |

**Example:**

```erlang
%% Connect via stdio
{ok, Client} = erlmcp_client:start_link(#{
    transport => stdio,
    capabilities => #{
        roots => #{},
        sampling => #{}
    },
    client_info => #{
        name => <<"my-client">>,
        version => <<"1.0.0">>
    }
}).

%% Connect via HTTP
{ok, Client} = erlmcp_client:start_link(#{
    transport => http,
    transport_opts => #{
        host => "localhost",
        port => 8765,
        path => "/mcp"
    },
    capabilities => #{},
    client_info => #{
        name => <<"http-client">>,
        version => <<"1.0.0">>
    }
}).

%% Connect via WebSocket
{ok, Client} = erlmcp_client:start_link(#{
    transport => ws,
    transport_opts => #{
        host => "localhost",
        port => 8765,
        path => "/mcp"
    },
    capabilities => #{},
    client_info => #{
        name => <<"ws-client">>,
        version => <<"1.0.0">>
    }
}).

%% Connect via TCP
{ok, Client} = erlmcp_client:start_link(#{
    transport => tcp,
    transport_opts => #{
        host => "localhost",
        port => 8765
    },
    capabilities => #{},
    client_info => #{
        name => <<"tcp-client">>,
        version => <<"1.0.0">>
    }
}).
```

---

## Tool Methods

### list_tools/1

List all available tools from the server.

**Signature:**
```erlang
list_tools(Client) -> {ok, Result} | {error, Reason}
```

**Example:**

```erlang
{ok, #{<<"tools">> := Tools}} = erlmcp_client:list_tools(Client),
lists:foreach(fun(Tool) ->
    Name = maps:get(<<"name">>, Tool),
    Desc = maps:get(<<"description">>, Tool),
    io:format("Tool: ~s - ~s~n", [Name, Desc])
end, Tools).
```

**Response:**

```erlang
{ok, #{
    <<"tools">> => [
        #{
            <<"name">> => <<"calculate">>,
            <<"description">> => <<"Perform arithmetic operations">>,
            <<"inputSchema">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"a">> => #{<<"type">> => <<"number">>},
                    <<"b">> => #{<<"type">> => <<"number">>},
                    <<"op">> => #{
                        <<"type">> => <<"string">>,
                        <<"enum">> => [<<"add">>, <<"subtract">>, <<"multiply">>, <<"divide">>]
                    }
                },
                <<"required">> => [<<"a">>, <<"b">>, <<"op">>]
            }
        }
    ]
}}
```

### call_tool/3, call_tool/4

Execute a tool with the provided arguments.

**Signature:**
```erlang
call_tool(Client, ToolName, Arguments) -> {ok, Result} | {error, Reason}
call_tool(Client, ToolName, Arguments, Options) -> {ok, Result} | {error, Reason}
```

**Parameters:**

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| Client | pid() | Yes | Client process |
| ToolName | binary() | Yes | Name of the tool to call |
| Arguments | map() | Yes | Tool arguments |
| Options | map() | No | Call options |

**Options:**

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| progress_token | binary() | undefined | Progress token for long-running operations |
| timeout | integer() | 30000 | Request timeout in milliseconds |

**Example:**

```erlang
%% Simple tool call
{ok, Result} = erlmcp_client:call_tool(Client, <<"calculate">>, #{
    <<"a">> => 10,
    <<"b">> => 5,
    <<"op">> => <<"multiply">>
}),

#{<<"content">> := [#{<<"text">> := Text}]} = Result,
io:format("Result: ~s~n", [Text]).  %% Output: Result: 50

%% Tool call with progress token
ProgressToken = <<"progress-", (integer_to_binary(erlang:unique_integer([positive])))/binary>>,

%% Register progress callback
erlmcp_client:on_progress(Client, ProgressToken, fun(Progress) ->
    #{
        <<"progress">> := Current,
        <<"total">> := Total,
        <<"message">> := Msg
    } = Progress,
    io:format("Progress: ~p/~p - ~s~n", [Current, Total, Msg])
end),

{ok, Result} = erlmcp_client:call_tool(Client, <<"long_task">>, #{}, #{
    progress_token => ProgressToken
}).
```

**Response:**

```erlang
{ok, #{
    <<"content">> => [
        #{
            <<"type">> => <<"text">>,
            <<"text">> => <<"50">>
        }
    ]
}}
```

---

## Resource Methods

### list_resources/1

List all available resources from the server.

**Signature:**
```erlang
list_resources(Client) -> {ok, Result} | {error, Reason}
```

**Example:**

```erlang
{ok, #{<<"resources">> := Resources}} = erlmcp_client:list_resources(Client),
lists:foreach(fun(Resource) ->
    Uri = maps:get(<<"uri">>, Resource),
    Name = maps:get(<<"name">>, Resource),
    io:format("Resource: ~s (~s)~n", [Name, Uri])
end, Resources).
```

**Response:**

```erlang
{ok, #{
    <<"resources">> => [
        #{
            <<"uri">> => <<"mcp://config">>,
            <<"name">> => <<"Configuration">>,
            <<"description">> => <<"Server configuration">>,
            <<"mimeType">> => <<"application/json">>
        }
    ]
}}
```

### read_resource/2

Read the contents of a specific resource.

**Signature:**
```erlang
read_resource(Client, Uri) -> {ok, Result} | {error, Reason}
```

**Example:**

```erlang
{ok, #{<<"contents">> := Contents}} = erlmcp_client:read_resource(
    Client,
    <<"mcp://config">>
),

[#{<<"text">> := Text}] = Contents,
Config = jsx:decode(Text, [return_maps]),
io:format("Config: ~p~n", [Config]).
```

**Response:**

```erlang
{ok, #{
    <<"contents">> => [
        #{
            <<"uri">> => <<"mcp://config">>,
            <<"type">> => <<"text">>,
            <<"text">> => <<"{\"version\":\"3.0.0\"}">>
        }
    ]
}}
```

### list_resource_templates/1

List resource URI templates for dynamic resource access.

**Signature:**
```erlang
list_resource_templates(Client) -> {ok, Result} | {error, Reason}
```

**Example:**

```erlang
{ok, #{<<"resourceTemplates">> := Templates}} =
    erlmcp_client:list_resource_templates(Client),

lists:foreach(fun(Template) ->
    UriTemplate = maps:get(<<"uriTemplate">>, Template),
    io:format("Template: ~s~n", [UriTemplate])
end, Templates).
```

### subscribe_resource/2

Subscribe to change notifications for a resource.

**Signature:**
```erlang
subscribe_resource(Client, Uri) -> ok | {error, Reason}
```

**Example:**

```erlang
%% Register notification handler
erlmcp_client:on_notification(Client, fun(Notification) ->
    case Notification of
        #{<<"method">> := <<"resources/updated">>, <<"params">> := Params} ->
            Uri = maps:get(<<"uri">>, Params),
            io:format("Resource updated: ~s~n", [Uri]),
            %% Re-read the resource
            {ok, _} = erlmcp_client:read_resource(Client, Uri);
        _ ->
            ok
    end
end),

%% Subscribe to resource updates
ok = erlmcp_client:subscribe_resource(Client, <<"mcp://config">>).
```

### unsubscribe_resource/2

Unsubscribe from resource change notifications.

**Signature:**
```erlang
unsubscribe_resource(Client, Uri) -> ok | {error, Reason}
```

---

## Prompt Methods

### list_prompts/1

List all available prompt templates.

**Signature:**
```erlang
list_prompts(Client) -> {ok, Result} | {error, Reason}
```

**Example:**

```erlang
{ok, #{<<"prompts">> := Prompts}} = erlmcp_client:list_prompts(Client),
lists:foreach(fun(Prompt) ->
    Name = maps:get(<<"name">>, Prompt),
    Desc = maps:get(<<"description">>, Prompt),
    io:format("Prompt: ~s - ~s~n", [Name, Desc])
end, Prompts).
```

### get_prompt/3

Get a prompt template with arguments filled in.

**Signature:**
```erlang
get_prompt(Client, PromptName, Arguments) -> {ok, Result} | {error, Reason}
```

**Example:**

```erlang
{ok, Result} = erlmcp_client:get_prompt(Client, <<"write_essay">>, #{
    <<"topic">> => <<"climate change">>
}),

#{
    <<"description">> := Desc,
    <<"messages">> := Messages
} = Result,

io:format("Prompt: ~s~n", [Desc]),
lists:foreach(fun(Msg) ->
    Role = maps:get(<<"role">>, Msg),
    Content = maps:get(<<"content">>, Msg),
    io:format("[~s]: ~p~n", [Role, Content])
end, Messages).
```

---

## Completion Methods

### complete/3

Request completion for a reference.

**Signature:**
```erlang
complete(Client, Ref, Argument) -> {ok, Result} | {error, Reason}
```

**Example:**

```erlang
{ok, Completion} = erlmcp_client:complete(Client,
    #{type => <<"resource">>, uri => <<"mcp://config">>},
    #{name => <<"feature">>, value => <<"too">>}
),

#{<<"completion">> := #{<<"items">> := Items}} = Completion,
lists:foreach(fun(Item) ->
    Text = maps:get(<<"text">>, Item),
    io:format("Suggestion: ~s~n", [Text])
end, Items).
```

---

## Task Methods

### create_task/2

Create an asynchronous task.

**Signature:**
```erlang
create_task(Client, TaskSpec) -> {ok, TaskId} | {error, Reason}
```

**Example:**

```erlang
{ok, #{<<"taskId">> := TaskId}} = erlmcp_client:create_task(Client, #{
    <<"action">> => #{
        <<"type">> => <<"tool">>,
        <<"name">> => <<"long_operation">>
    },
    <<"timeoutMs">> => 60000
}),

io:format("Task created: ~s~n", [TaskId]).
```

### get_task/2

Get task status and details.

**Signature:**
```erlang
get_task(Client, TaskId) -> {ok, Task} | {error, Reason}
```

**Example:**

```erlang
{ok, Task} = erlmcp_client:get_task(Client, TaskId),
Status = maps:get(<<"status">>, Task),
io:format("Task status: ~s~n", [Status]).
```

### list_tasks/1

List all tasks.

**Signature:**
```erlang
list_tasks(Client) -> {ok, Tasks} | {error, Reason}
```

### cancel_task/2

Cancel a running task.

**Signature:**
```erlang
cancel_task(Client, TaskId) -> ok | {error, Reason}
```

---

## Root Methods (v3.0)

### list_roots/1

List all root directories the server has access to.

**Signature:**
```erlang
list_roots(Client) -> {ok, Result} | {error, Reason}
```

**Example:**

```erlang
{ok, #{<<"roots">> := Roots}} = erlmcp_client:list_roots(Client),
lists:foreach(fun(Root) ->
    Uri = maps:get(<<"uri">>, Root),
    Name = maps:get(<<"name">>, Root),
    io:format("Root: ~s (~s)~n", [Name, Uri])
end, Roots).
```

### add_root/2

Add a root directory to the server's workspace.

**Signature:**
```erlang
add_root(Client, Uri) -> ok | {error, Reason}
```

**Example:**

```erlang
ok = erlmcp_client:add_root(Client, <<"file:///Users/user/project">>).
```

### remove_root/2

Remove a root directory from the server's workspace.

**Signature:**
```erlang
remove_root(Client, Uri) -> ok | {error, Reason}
```

---

## Utility Methods

### ping/1

Send a ping request to check server liveness.

**Signature:**
```erlang
ping(Client) -> ok | {error, Reason}
```

**Example:**

```erlang
case erlmcp_client:ping(Client) of
    ok -> io:format("Server is alive~n");
    {error, Reason} -> io:format("Ping failed: ~p~n", [Reason])
end.
```

### get_server_info/1

Get server information from the initialize response.

**Signature:**
```erlang
get_server_info(Client) -> {ok, ServerInfo}
```

**Example:**

```erlang
{ok, ServerInfo} = erlmcp_client:get_server_info(Client),
#{
    <<"name">> := Name,
    <<"version">> := Version
} = ServerInfo,
io:format("Connected to ~s v~s~n", [Name, Version]).
```

### get_server_capabilities/1

Get server capabilities.

**Signature:**
```erlang
get_server_capabilities(Client) -> {ok, Capabilities}
```

**Example:**

```erlang
{ok, Caps} = erlmcp_client:get_server_capabilities(Client),
HasTools = maps:is_key(<<"tools">>, Caps),
HasResources = maps:is_key(<<"resources">>, Caps),
io:format("Tools: ~p, Resources: ~p~n", [HasTools, HasResources]).
```

---

## Notification Handling

### on_notification/2

Register a callback for server-to-client notifications.

**Signature:**
```erlang
on_notification(Client, Callback) -> ok
```

**Callback Signature:**
```erlang
Callback :: fun((Notification :: map()) -> any())
```

**Example:**

```erlang
erlmcp_client:on_notification(Client, fun(Notification) ->
    Method = maps:get(<<"method">>, Notification),
    Params = maps:get(<<"params">>, Notification, #{}),

    case Method of
        <<"resources/updated">> ->
            Uri = maps:get(<<"uri">>, Params),
            io:format("Resource updated: ~s~n", [Uri]);

        <<"resources/list_changed">> ->
            io:format("Resource list changed~n"),
            {ok, _} = erlmcp_client:list_resources(Client);

        <<"tools/list_changed">> ->
            io:format("Tool list changed~n"),
            {ok, _} = erlmcp_client:list_tools(Client);

        <<"notifications/progress">> ->
            #{
                <<"progressToken">> := Token,
                <<"progress">> := Progress,
                <<"total">> := Total,
                <<"message">> := Message
            } = Params,
            io:format("[~s] ~p/~p: ~s~n", [Token, Progress, Total, Message]);

        <<"notifications/message">> ->
            Level = maps:get(<<"level">>, Params),
            Data = maps:get(<<"data">>, Params),
            io:format("[~s] ~p~n", [Level, Data]);

        _ ->
            io:format("Unknown notification: ~s~n", [Method])
    end
end).
```

### on_progress/3

Register a callback for progress updates on a specific token.

**Signature:**
```erlang
on_progress(Client, ProgressToken, Callback) -> ok
```

**Example:**

```erlang
Token = <<"progress-123">>,

erlmcp_client:on_progress(Client, Token, fun(Progress) ->
    #{
        <<"progress">> := Current,
        <<"total">> := Total,
        <<"message">> := Msg
    } = Progress,
    Percent = (Current / Total) * 100,
    io:format("~.1f%: ~s~n", [Percent, Msg])
end),

{ok, _} = erlmcp_client:call_tool(Client, <<"long_task">>, #{}, #{
    progress_token => Token
}).
```

---

## Error Handling

### Error Response Format

All client methods return errors in the format:

```erlang
{error, Reason}
```

Where `Reason` can be:

- `{jsonrpc_error, Code, Message, Data}` - JSON-RPC error from server
- `{transport_error, Reason}` - Transport layer error
- `{timeout, Timeout}` - Request timeout
- `{invalid_response, Response}` - Invalid response format
- `{not_initialized}` - Client not initialized

**Example Error Handling:**

```erlang
case erlmcp_client:call_tool(Client, <<"nonexistent">>, #{}) of
    {ok, Result} ->
        handle_result(Result);

    {error, {jsonrpc_error, -32002, Message, Data}} ->
        io:format("Tool not found: ~s~n", [Message]),
        io:format("Details: ~p~n", [Data]);

    {error, {timeout, Timeout}} ->
        io:format("Request timed out after ~pms~n", [Timeout]);

    {error, {transport_error, Reason}} ->
        io:format("Transport error: ~p~n", [Reason]);

    {error, Reason} ->
        io:format("Unknown error: ~p~n", [Reason])
end.
```

---

## Complete Example

```erlang
-module(example_client).
-export([run/0]).

run() ->
    %% Start client
    {ok, Client} = erlmcp_client:start_link(#{
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
    }),

    %% Get server info
    {ok, ServerInfo} = erlmcp_client:get_server_info(Client),
    io:format("Connected to: ~p~n", [ServerInfo]),

    %% Register notification handler
    erlmcp_client:on_notification(Client, fun handle_notification/1),

    %% List and call tools
    {ok, #{<<"tools">> := Tools}} = erlmcp_client:list_tools(Client),
    io:format("Available tools: ~p~n", [Tools]),

    {ok, Result} = erlmcp_client:call_tool(Client, <<"echo">>, #{
        <<"text">> => <<"Hello from Erlang!">>
    }),
    io:format("Tool result: ~p~n", [Result]),

    %% List and read resources
    {ok, #{<<"resources">> := Resources}} = erlmcp_client:list_resources(Client),
    io:format("Available resources: ~p~n", [Resources]),

    case Resources of
        [#{<<"uri">> := Uri} | _] ->
            {ok, Contents} = erlmcp_client:read_resource(Client, Uri),
            io:format("Resource contents: ~p~n", [Contents]);
        [] ->
            io:format("No resources available~n")
    end,

    %% Keep client alive
    receive
        stop -> ok
    after 60000 ->
        ok
    end.

handle_notification(Notification) ->
    Method = maps:get(<<"method">>, Notification),
    io:format("Received notification: ~s~n", [Method]).
```

---

## See Also

- [JSON-RPC Reference](json-rpc-reference.md) - Complete protocol specification
- [Transport Protocols](transport-protocols.md) - Transport configuration
- [Error Codes](error-codes.md) - Error code reference
- [Integration Examples](integration-examples.md) - Real-world examples
