# erlmcp Quick Start Guide

## What is erlmcp?

erlmcp is a production-grade Erlang/OTP implementation of the Model Context Protocol (MCP). It enables AI assistants (like Claude) to securely interact with external data, tools, and resources.

### Key Features

- **Multiple Transports**: stdio, TCP, HTTP, WebSocket, SSE
- **Comprehensive Security**: mTLS, JWT, OAuth 2.0, encrypted secrets
- **High Performance**: 100K+ messages/second, built on Erlang/OTP
- **Production Ready**: Built-in observability, rate limiting, health checks
- **MCP Compliant**: Full implementation of MCP 2025-11-25 specification

---

## Installation

### Prerequisites

- Erlang/OTP 26, 27, or 28
- rebar3 build tool

### From Source

```bash
git clone https://github.com/erlmcp/erlmcp.git
cd erlmcp
rebar3 compile
```

### From Hex (Coming Soon)

```bash
# rebar.config
{deps, [
    {erlmcp_core, "~> 3.0"},
    {erlmcp_transports, "~> 3.0"}
]}.
```

---

## Your First MCP Server

### Step 1: Create a Server Module

Create `my_server.erl`:

```erlang
-module(my_server).
-export([start/0]).

start() ->
    %% Start applications
    application:ensure_all_started(erlmcp_core),
    application:ensure_all_started(erlmcp_transports),

    %% Define capabilities
    Capabilities = #mcp_server_capabilities{
        resources = #mcp_resources_capability{},
        tools = #mcp_tools_capability{},
        prompts = #mcp_prompts_capability{}
    },

    %% Start server
    {ok, Server} = erlmcp_server:start_link(my_server, Capabilities),

    %% Add a tool
    erlmcp_server:add_tool(Server, #{
        name => <<"echo">>,
        description => <<"Echo back the input text">>,
        input_schema => #{
            type => object,
            properties => #{
                text => #{type => string}
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

    %% Add a resource
    erlmcp_server:add_resource(Server, #{
        uri => <<"mcp://hello">>,
        name => <<"Hello">>,
        description => <<"A greeting">>,
        mime_type => <<"text/plain">>,
        handler => fun(_Uri) ->
            #{content => [#{
                type => text,
                text => <<"Hello from erlmcp!">>
            }]}
        end
    }),

    %% Start stdio transport
    {ok, _Transport} = erlmcp_transport_stdio:start_link(Server),

    %% Keep process alive
    io:format("MCP Server started on stdio~n"),
    receive stop -> ok end.
```

### Step 2: Compile and Run

```bash
# Compile
erlc -I /path/to/erlmcp/apps/erlmcp/include my_server.erl

# Run
erl -pa /path/to/erlmcp/_build/default/lib/*/ebin -eval "my_server:start()" -noshell
```

### Step 3: Test Your Server

Send a JSON-RPC request:

```json
{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2025-11-25","capabilities":{},"clientInfo":{"name":"test","version":"1.0"}}}
```

Expected response:

```json
{"jsonrpc":"2.0","id":1,"result":{"protocolVersion":"2025-11-25","capabilities":{"resources":{},"tools":{},"prompts":{}},"serverInfo":{"name":"erlmcp","version":"3.0.0"}}}
```

---

## Using with Claude Desktop

### Configure Claude Desktop

Edit `claude_desktop_config.json`:

**macOS**: `~/Library/Application Support/Claude/claude_desktop_config.json`
**Windows**: `%APPDATA%\Claude\claude_desktop_config.json`
**Linux**: `~/.config/Claude/claude_desktop_config.json`

```json
{
  "mcpServers": {
    "my-server": {
      "command": "erl",
      "args": [
        "-pa", "/path/to/erlmcp/_build/default/lib/*/ebin",
        "-pa", "/path/to/your/project",
        "-eval", "my_server:start()",
        "-noshell"
      ]
    }
  }
}
```

**Restart Claude Desktop** after editing the configuration.

### Test with Claude

```
User: What tools do you have available?

Claude: I have access to the following tools:
- echo: Echo back the input text

User: Use the echo tool to repeat "Hello, World!"

Claude: [Calls echo tool with {"text": "Hello, World!"}]
Echo: Hello, World!
```

---

## Transport Examples

### HTTP Server

```erlang
start_http_server() ->
    Capabilities = #mcp_server_capabilities{
        tools = #mcp_tools_capability{}
    },
    {ok, Server} = erlmcp_server:start_link(http_server, Capabilities),

    %% Add tools...

    %% Start HTTP transport
    {ok, _HTTP} = erlmcp_transport_http:start_link(Server, [
        {port, 8765},
        {ip, {0, 0, 0, 0}}
    ]),

    io:format("HTTP MCP Server listening on http://localhost:8765/mcp~n"),
    receive stop -> ok end.
```

Test with cURL:

```bash
curl -X POST http://localhost:8765/mcp \
  -H "Content-Type: application/json" \
  -d '{"jsonrpc":"2.0","id":1,"method":"tools/list"}'
```

### WebSocket Server

```erlang
start_websocket_server() ->
    Capabilities = #mcp_server_capabilities{
        tools = #mcp_tools_capability{}
    },
    {ok, Server} = erlmcp_server:start_link(ws_server, Capabilities),

    %% Add tools...

    %% Start WebSocket transport
    {ok, _WS} = erlmcp_transport_ws:start_link(Server, [
        {port, 8765},
        {path, "/mcp"}
    ]),

    io:format("WebSocket MCP Server listening on ws://localhost:8765/mcp~n"),
    receive stop -> ok end.
```

### TCP Server

```erlang
start_tcp_server() ->
    Capabilities = #mcp_server_capabilities{
        tools = #mcp_tools_capability{}
    },
    {ok, Server} = erlmcp_server:start_link(tcp_server, Capabilities),

    %% Add tools...

    %% Start TCP transport
    {ok, _TCP} = erlmcp_transport_tcp:start_listener(Server, [
        {port, 8765},
        {ip, {0, 0, 0, 0}}
    ]),

    io:format("TCP MCP Server listening on localhost:8765~n"),
    receive stop -> ok end.
```

---

## Common Patterns

### Tool with JSON Schema Validation

```erlang
erlmcp_server:add_tool(Server, #{
    name => <<"calculate">>,
    description => <<"Perform arithmetic operations">>,
    input_schema => #{
        type => object,
        properties => #{
            a => #{type => number},
            b => #{type => number},
            op => #{
                type => string,
                enum => [<<"add">>, <<"subtract">>, <<"multiply">>, <<"divide">>]
            }
        },
        required => [a, b, op]
    },
    handler => fun(Args) ->
        A = maps:get(<<"a">>, Args),
        B = maps:get(<<"b">>, Args),
        Op = maps:get(<<"op">>, Args),
        Result = case Op of
            <<"add">> -> A + B;
            <<"subtract">> -> A - B;
            <<"multiply">> -> A * B;
            <<"divide">> -> A / B
        end,
        #{content => [#{
            type => text,
            text => float_to_binary(Result, [{decimals, 2}])
        }]}
    end
}).
```

### Tool with Progress Updates

```erlang
erlmcp_server:add_tool(Server, #{
    name => <<"long_task">>,
    description => <<"A long-running task with progress">>,
    handler => fun(#{<<"_progressToken">> := ProgressToken} = Args) ->
        %% Report initial progress
        erlmcp_server:report_progress(Server, ProgressToken, 0.0, 1.0, <<"Starting...">>),

        %% Do some work
        timer:sleep(1000),
        erlmcp_server:report_progress(Server, ProgressToken, 0.5, 1.0, <<"Halfway...">>),

        %% More work
        timer:sleep(1000),
        erlmcp_server:report_progress(Server, ProgressToken, 1.0, 1.0, <<"Complete!">>),

        #{content => [#{
            type => text,
            text => <<"Task completed!">>
        }]}
    end
}).
```

### Resource with Template

```erlang
%% Add a resource template for log files by date
erlmcp_server:add_resource_template(Server, #{
    uri_template => <<"logs://{date}">>,
    name => <<"Daily Logs">>,
    description => <<"Access logs by date (YYYY-MM-DD)">>,
    mime_type => <<"text/plain">>,
    handler => fun(UriTemplate, [{date, Date}]) ->
        Uri = <<"logs://", Date/binary>>,
        case read_log_file(Date) of
            {ok, Content} ->
                #{content => [#{
                    type => text,
                    text => Content
                }]};
            {error, _} ->
                #{content => [#{
                    type => text,
                    text => <<"Log not found">>
                }]}
        end
    end
}).
```

### Prompt Template

```erlang
erlmcp_server:add_prompt(Server, #{
    name => <<"write_code">>,
    description => <<"Generate a code writing prompt">>,
    arguments => [
        #mcp_prompt_argument{
            name = <<"language">>,
            description = <<"Programming language">>,
            required = true
        },
        #mcp_prompt_argument{
            name = <<"task">>,
            description = <<"What the code should do">>,
            required = true
        }
    ],
    handler => fun(Args) ->
        Language = maps:get(<<"language">>, Args),
        Task = maps:get(<<"task">>, Args),
        Prompt = <<"Write ", Language/binary, " code to: ", Task/binary>>,
        #{messages => [#{
            role => user,
            content => Prompt
        }]}
    end
}).
```

---

## Configuration

### sys.config

```erlang
[
    {erlmcp_core, [
        {protocol_version, <<"2025-11-25">>},
        {server_info, #{
            name => <<"my-mcp-server">>,
            version => <<"1.0.0">>
        }},
        {capabilities, #mcp_server_capabilities{
            resources = #mcp_resources_capability{
                subscribe = true
            },
            tools = #mcp_tools_capability{
                listChanged = true
            },
            prompts = #mcp_prompts_capability{}
        }},
        {auth, #{
            enabled => false  %% Set to true to enable auth
        }}
    ]},
    {erlmcp_transports, [
        {http, [
            {port, 8765},
            {enable_tls, false}
        ]}
    ]},
    {erlmcp_observability, [
        {otel, #{
            enabled => true,
            exporter => otlp
        }}
    ]}
].
```

---

## Testing

### Unit Test Example

```erlang
-module(my_server_tests).
-include_lib("eunit/include/eunit.hrl").

echo_tool_test() ->
    %% Start server
    {ok, Server} = my_server:start(),

    %% Call tool
    Args = #{<<"text">> => <<"Hello">>},
    Result = erlmcp_server:call_tool(Server, <<"echo">>, Args),

    %% Assert result
    ?assertMatch(#{content := [#{
        type := text,
        text := <<"Echo: Hello">>
    }]}, Result).
```

### Manual Testing with MCP Inspector

```bash
# Install MCP Inspector
npm install -g @modelcontextprotocol/inspector

# Test your server
mcp-inspector erl -pa ebin -eval "my_server:start()" -noshell
```

---

## Next Steps

- Read the [JSON-RPC Protocol Reference](json-rpc-reference.md) for all available methods
- Check [Transport Protocols](transport-protocols.md) for advanced configuration
- See [Security Documentation](security.md) for production deployment
- Review [Error Codes](error-codes.md) for troubleshooting

---

## Getting Help

- **Documentation**: `/Users/sac/erlmcp/docs/`
- **Examples**: `/Users/sac/erlmcp/examples/`
- **Issues**: https://github.com/erlmcp/erlmcp/issues
- **Discord**: https://discord.gg/erlmcp
