# ErlMCP API Reference

## Overview

This document provides comprehensive API documentation for all public functions and modules in the ErlMCP SDK.

## Core Modules

### erlmcp

The main entry point module providing high-level convenience functions.

#### Functions

##### start/0
```erlang
start() -> ok | {error, Reason}.
```
Start the ErlMCP application and all dependencies.

**Returns:**
- `ok` - Application started successfully
- `{error, Reason}` - Start failed with reason

##### stop/0
```erlang
stop() -> ok.
```
Stop the ErlMCP application.

**Returns:**
- `ok` - Application stopped

##### version/0
```erlang
version() -> binary().
```
Get the current ErlMCP version.

**Returns:**
- `binary()` - Version string (e.g., <<"1.0.0">>)

---

### erlmcp_server

Main server module for creating MCP servers.

#### Types

```erlang
-type server_capabilities() :: #{
    resources => mcp_capability(),
    tools => mcp_capability(), 
    prompts => mcp_capability(),
    logging => mcp_capability()
}.

-type transport_config() :: 
    {stdio, transport_opts()} |
    {tcp, transport_opts()} |
    {http, transport_opts()}.

-type server_opts() :: #{
    capabilities => server_capabilities(),
    transport => transport_config(),
    strict_mode => boolean(),
    timeout => timeout()
}.
```

#### Functions

##### start_link/1,2
```erlang
start_link(TransportConfig) -> {ok, pid()} | {error, Reason}.
start_link(TransportConfig, Capabilities) -> {ok, pid()} | {error, Reason}.
```
Start a linked MCP server process.

**Parameters:**
- `TransportConfig` - Transport configuration tuple
- `Capabilities` - Server capability specification (optional)

**Returns:**
- `{ok, Pid}` - Server started with process ID
- `{error, Reason}` - Start failed with reason

**Example:**
```erlang
Capabilities = #{
    resources => #{enabled => true},
    tools => #{enabled => true}
},
{ok, Server} = erlmcp_server:start_link({stdio, []}, Capabilities).
```

##### add_resource/3,4
```erlang
add_resource(Server, Uri, Handler) -> ok | {error, Reason}.
add_resource(Server, Uri, Handler, Metadata) -> ok | {error, Reason}.
```
Add a resource to the server.

**Parameters:**
- `Server` - Server process ID
- `Uri` - Resource URI (binary)
- `Handler` - Resource handler function `fun(Uri) -> Content`
- `Metadata` - Optional resource metadata map

**Returns:**
- `ok` - Resource added successfully
- `{error, Reason}` - Add failed with reason

**Example:**
```erlang
Handler = fun(<<"file://", Path/binary>>) ->
    {ok, Content} = file:read_file(Path),
    Content
end,
ok = erlmcp_server:add_resource(Server, <<"file://config.json">>, Handler).
```

##### add_tool/3,4
```erlang
add_tool(Server, Name, Handler) -> ok | {error, Reason}.
add_tool_with_schema(Server, Name, Handler, Schema) -> ok | {error, Reason}.
```
Add a tool to the server.

**Parameters:**
- `Server` - Server process ID
- `Name` - Tool name (binary)
- `Handler` - Tool handler function `fun(Args) -> Result`
- `Schema` - JSON Schema for argument validation (optional)

**Returns:**
- `ok` - Tool added successfully
- `{error, Reason}` - Add failed with reason

**Example:**
```erlang
Schema = #{
    <<"type">> => <<"object">>,
    <<"properties">> => #{
        <<"message">> => #{<<"type">> => <<"string">>}
    },
    <<"required">> => [<<"message">>]
},
Handler = fun(#{<<"message">> := Msg}) ->
    <<"Echo: ", Msg/binary>>
end,
ok = erlmcp_server:add_tool_with_schema(Server, <<"echo">>, Handler, Schema).
```

##### add_prompt/3,4
```erlang
add_prompt(Server, Name, Handler) -> ok | {error, Reason}.
add_prompt(Server, Name, Handler, Arguments) -> ok | {error, Reason}.
```
Add a prompt template to the server.

**Parameters:**
- `Server` - Server process ID
- `Name` - Prompt name (binary)
- `Handler` - Prompt handler function `fun(Args) -> Messages`
- `Arguments` - Prompt argument specification (optional)

**Returns:**
- `ok` - Prompt added successfully
- `{error, Reason}` - Add failed with reason

##### remove_resource/2
```erlang
remove_resource(Server, Uri) -> ok | {error, Reason}.
```
Remove a resource from the server.

##### remove_tool/2
```erlang
remove_tool(Server, Name) -> ok | {error, Reason}.
```
Remove a tool from the server.

##### remove_prompt/2
```erlang
remove_prompt(Server, Name) -> ok | {error, Reason}.
```
Remove a prompt from the server.

##### stop/1
```erlang
stop(Server) -> ok.
```
Stop the server gracefully.

---

### erlmcp_client

Main client module for connecting to MCP servers.

#### Types

```erlang
-type client_capabilities() :: #{
    roots => mcp_capability(),
    sampling => mcp_capability(),
    experimental => map()
}.

-type client_opts() :: #{
    capabilities => client_capabilities(),
    strict_mode => boolean(),
    timeout => timeout(),
    retry_attempts => non_neg_integer()
}.
```

#### Functions

##### start_link/1,2
```erlang
start_link(TransportConfig) -> {ok, pid()} | {error, Reason}.
start_link(TransportConfig, Opts) -> {ok, pid()} | {error, Reason}.
```
Start a linked MCP client process.

**Parameters:**
- `TransportConfig` - Transport configuration tuple
- `Opts` - Client options (optional)

**Returns:**
- `{ok, Pid}` - Client started with process ID
- `{error, Reason}` - Start failed with reason

##### initialize/2,3
```erlang
initialize(Client, ClientInfo) -> {ok, ServerInfo} | {error, Reason}.
initialize(Client, ClientInfo, Capabilities) -> {ok, ServerInfo} | {error, Reason}.
```
Initialize connection with the server.

**Parameters:**
- `Client` - Client process ID
- `ClientInfo` - Client information map
- `Capabilities` - Client capabilities (optional)

**Returns:**
- `{ok, ServerInfo}` - Initialization successful with server info
- `{error, Reason}` - Initialization failed

**Example:**
```erlang
ClientInfo = #{
    name => <<"MyClient">>,
    version => <<"1.0.0">>
},
{ok, ServerInfo} = erlmcp_client:initialize(Client, ClientInfo).
```

##### list_resources/1
```erlang
list_resources(Client) -> {ok, Resources} | {error, Reason}.
```
Get list of available resources from server.

**Returns:**
- `{ok, #{resources := [Resource]}}` - List of resources
- `{error, Reason}` - Request failed

##### read_resource/2
```erlang
read_resource(Client, Uri) -> {ok, Content} | {error, Reason}.
```
Read content from a resource.

**Parameters:**
- `Client` - Client process ID
- `Uri` - Resource URI (binary)

**Returns:**
- `{ok, Content}` - Resource content
- `{error, Reason}` - Read failed

##### list_tools/1
```erlang
list_tools(Client) -> {ok, Tools} | {error, Reason}.
```
Get list of available tools from server.

##### call_tool/3
```erlang
call_tool(Client, Name, Arguments) -> {ok, Result} | {error, Reason}.
```
Call a tool on the server.

**Parameters:**
- `Client` - Client process ID
- `Name` - Tool name (binary)
- `Arguments` - Tool arguments map

**Returns:**
- `{ok, Result}` - Tool execution result
- `{error, Reason}` - Call failed

**Example:**
```erlang
Args = #{<<"message">> => <<"Hello, World!">>},
{ok, Result} = erlmcp_client:call_tool(Client, <<"echo">>, Args).
```

##### list_prompts/1
```erlang
list_prompts(Client) -> {ok, Prompts} | {error, Reason}.
```
Get list of available prompts from server.

##### get_prompt/3
```erlang
get_prompt(Client, Name, Arguments) -> {ok, Messages} | {error, Reason}.
```
Get a prompt with provided arguments.

##### subscribe_resource/2
```erlang
subscribe_resource(Client, Uri) -> ok | {error, Reason}.
```
Subscribe to resource change notifications.

##### unsubscribe_resource/2
```erlang
unsubscribe_resource(Client, Uri) -> ok | {error, Reason}.
```
Unsubscribe from resource change notifications.

##### stop/1
```erlang
stop(Client) -> ok.
```
Stop the client gracefully.

---

## Transport Modules

### erlmcp_transport

Transport behavior module defining the interface for all transport implementations.

#### Callbacks

```erlang
-callback init(Opts) -> {ok, State} | {error, Reason}.
-callback send(State, Data) -> ok | {error, Reason}.
-callback close(State) -> ok.  % Optional
```

### erlmcp_transport_stdio

STDIO transport implementation.

#### Functions

##### start_link/1
```erlang
start_link(Owner) -> {ok, pid()} | {error, Reason}.
```
Start STDIO transport process.

##### send/2
```erlang
send(Transport, Message) -> ok | {error, Reason}.
```
Send message via STDIO.

##### close/1
```erlang
close(Transport) -> ok.
```
Close STDIO transport.

### erlmcp_transport_tcp

TCP transport implementation with automatic reconnection.

#### Functions

##### start_link/1
```erlang
start_link(Opts) -> {ok, pid()} | {error, Reason}.
```
Start TCP transport with options:
```erlang
Opts = #{
    host => string() | inet:ip_address(),
    port => inet:port_number(),
    owner => pid(),
    connect_timeout => timeout(),
    keepalive => boolean(),
    nodelay => boolean()
}.
```

##### connect/2
```erlang
connect(Transport, Opts) -> ok | {error, Reason}.
```
Connect or reconnect with new options.

##### send/2
```erlang
send(Transport, Data) -> ok | {error, Reason}.
```
Send data over TCP connection.

##### close/1
```erlang
close(Transport) -> ok.
```
Close TCP connection.

### erlmcp_transport_http

HTTP/HTTPS transport implementation.

#### Functions

##### start_link/1
```erlang
start_link(Opts) -> {ok, pid()} | {error, Reason}.
```
Start HTTP transport with options:
```erlang
Opts = #{
    url => binary() | string(),
    owner => pid(),
    method => get | post,
    headers => [{string(), string()}],
    timeout => timeout(),
    ssl_options => [ssl:tls_option()]
}.
```

---

## Utility Modules

### erlmcp_json_rpc

JSON-RPC 2.0 protocol implementation.

#### Functions

##### encode_request/3
```erlang
encode_request(Id, Method, Params) -> binary().
```
Encode a JSON-RPC request.

##### encode_response/2,3
```erlang
encode_response(Id, Result) -> binary().
encode_error_response(Id, Error) -> binary().
```
Encode JSON-RPC responses.

##### decode_message/1
```erlang
decode_message(Data) -> {ok, Message} | {error, Reason}.
```
Decode JSON-RPC message from binary data.

### erlmcp_registry

Process registry for managing server and transport instances.

#### Functions

##### register_server/2
```erlang
register_server(Name, Pid) -> ok.
```
Register a server process.

##### register_transport/2
```erlang
register_transport(Name, Pid) -> ok.
```
Register a transport process.

##### lookup_server/1
```erlang
lookup_server(Name) -> {ok, Pid} | {error, not_found}.
```
Look up a registered server.

##### unregister/1
```erlang
unregister(Name) -> ok.
```
Unregister a process.

---

## Error Types

### Common Error Reasons

#### JSON-RPC Errors
- `{parse_error, Reason}` - JSON parsing failed
- `{invalid_request, Reason}` - Malformed request
- `{method_not_found, Method}` - Unknown method
- `{invalid_params, Reason}` - Invalid parameters

#### MCP-Specific Errors  
- `{resource_not_found, Uri}` - Resource doesn't exist
- `{tool_not_found, Name}` - Tool doesn't exist
- `{prompt_not_found, Name}` - Prompt doesn't exist
- `not_initialized` - Client not initialized
- `capability_not_supported` - Requested capability unavailable

#### Transport Errors
- `connection_lost` - Connection dropped unexpectedly
- `timeout` - Operation timed out
- `not_connected` - Transport not ready
- `{protocol_error, Reason}` - Protocol-level error

---

## Examples

### Complete Server Example

```erlang
-module(my_mcp_server).
-export([start/0]).

start() ->
    % Define server capabilities
    Capabilities = #{
        resources => #{enabled => true},
        tools => #{enabled => true},
        prompts => #{enabled => true}
    },
    
    % Start server with STDIO transport
    {ok, Server} = erlmcp_server:start_link({stdio, []}, Capabilities),
    
    % Add a simple resource
    ok = erlmcp_server:add_resource(Server, <<"config://app">>, 
        fun(_) -> <<"app_config: production">> end),
    
    % Add a tool with schema validation
    EchoSchema = #{
        <<"type">> => <<"object">>,
        <<"properties">> => #{
            <<"message">> => #{<<"type">> => <<"string">>}
        },
        <<"required">> => [<<"message">>]
    },
    ok = erlmcp_server:add_tool_with_schema(Server, <<"echo">>,
        fun(#{<<"message">> := Msg}) -> 
            <<"Echo: ", Msg/binary>> 
        end, EchoSchema),
    
    % Add a prompt template
    ok = erlmcp_server:add_prompt(Server, <<"greeting">>,
        fun(#{<<"name">> := Name}) ->
            [#{
                role => <<"user">>,
                content => #{
                    type => <<"text">>,
                    text => <<"Hello, ", Name/binary, "!">>
                }
            }]
        end),
    
    {ok, Server}.
```

### Complete Client Example

```erlang
-module(my_mcp_client).
-export([demo/0]).

demo() ->
    % Client info
    ClientInfo = #{
        name => <<"Demo Client">>,
        version => <<"1.0.0">>
    },
    
    % Client capabilities
    Capabilities = #{
        roots => #{enabled => false},
        sampling => #{enabled => false}
    },
    
    % Start client with TCP transport
    TcpOpts = #{
        host => "localhost",
        port => 8080,
        owner => self()
    },
    {ok, Client} = erlmcp_client:start_link({tcp, TcpOpts}),
    
    % Initialize connection
    {ok, ServerInfo} = erlmcp_client:initialize(Client, ClientInfo, Capabilities),
    io:format("Connected to server: ~p~n", [ServerInfo]),
    
    % List and read resources
    {ok, #{resources := Resources}} = erlmcp_client:list_resources(Client),
    io:format("Available resources: ~p~n", [Resources]),
    
    case Resources of
        [#{uri := Uri} | _] ->
            {ok, Content} = erlmcp_client:read_resource(Client, Uri),
            io:format("Resource content: ~p~n", [Content]);
        [] ->
            io:format("No resources available~n")
    end,
    
    % List and call tools
    {ok, #{tools := Tools}} = erlmcp_client:list_tools(Client),
    io:format("Available tools: ~p~n", [Tools]),
    
    case Tools of
        [#{name := ToolName} | _] ->
            Args = #{<<"message">> => <<"Hello from client!">>},
            {ok, Result} = erlmcp_client:call_tool(Client, ToolName, Args),
            io:format("Tool result: ~p~n", [Result]);
        [] ->
            io:format("No tools available~n")
    end,
    
    % Clean up
    ok = erlmcp_client:stop(Client).
```

---

This completes the comprehensive API reference. All functions include parameter descriptions, return values, and practical examples to help developers effectively use the ErlMCP SDK.