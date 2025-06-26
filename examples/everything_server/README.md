# Everything Server Example

A comprehensive MCP (Model Context Protocol) server implementation demonstrating all major capabilities of the erlmcp SDK. This example showcases proper OTP application design with supervision, state management, and various MCP features.

## Features

### Complete MCP Implementation
- **8 Tools** with JSON Schema validation
- **3 Prompt Types** with different argument configurations
- **100 Test Resources** with dynamic URI templates
- **Real-time Notifications** for subscriptions, logs, and stderr

### OTP Design
- Full OTP application with supervision tree
- Fault-tolerant gen_server implementation
- Configurable transport options (stdio, TCP, HTTP)
- Graceful shutdown and cleanup

### Advanced Features
- Resource subscriptions with periodic updates
- Multi-level logging with configurable verbosity
- Progress tracking for long-running operations
- Annotated messages with priority and audience targeting

## Architecture

```
everything_server_app (application)
    └── everything_server_sup (supervisor)
            └── everything_server (gen_server)
                    └── erlmcp_server (MCP protocol handler)
```

### Components

- **everything_server_app**: OTP application behavior
- **everything_server_sup**: Supervisor with one_for_all strategy
- **everything_server**: Main gen_server implementing all business logic
- **erlmcp_server**: Protocol handler from erlmcp SDK

## Building and Running

### Prerequisites
- Erlang/OTP 24 or later
- rebar3

### Build
```bash
cd examples/everything_server
rebar3 compile
```

### Run with stdio transport (default)
```bash
rebar3 run
```

### Run with TCP transport
```bash
ERL_FLAGS="-everything_server transport '{tcp,8080}'" rebar3 run
```

### Run with custom configuration
```bash
# Create config/sys.config
cat > config/sys.config << EOF
[
  {everything_server, [
    {transport, {tcp, 8080}},
    {subscription_interval, 5000},
    {log_interval, 10000},
    {log_level, info}
  ]}
].
EOF

rebar3 run
```

## Configuration Options

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| `transport` | `stdio | {tcp, Port} | {http, Url}` | `stdio` | Transport configuration |
| `subscription_interval` | `pos_integer()` | `10000` | Milliseconds between subscription updates |
| `log_interval` | `pos_integer()` | `20000` | Milliseconds between log messages |
| `stderr_interval` | `pos_integer()` | `30000` | Milliseconds between stderr messages |
| `log_level` | `atom()` | `debug` | Minimum log level to emit |

## MCP Tools

### 1. echo
Echoes back the provided message.
```json
{
  "name": "echo",
  "arguments": {
    "message": "Hello, World!"
  }
}
```

### 2. add
Performs addition of two numbers.
```json
{
  "name": "add",
  "arguments": {
    "a": 5,
    "b": 3
  }
}
```

### 3. longRunningOperation
Simulates a long-running operation with configurable duration and steps.
```json
{
  "name": "longRunningOperation",
  "arguments": {
    "duration": 10,
    "steps": 5
  }
}
```

### 4. printEnv
Returns environment variables as JSON.
```json
{
  "name": "printEnv",
  "arguments": {}
}
```

### 5. sampleLLM
Mock LLM sampling (demonstrates the interface).
```json
{
  "name": "sampleLLM",
  "arguments": {
    "prompt": "Write a poem",
    "maxTokens": 100
  }
}
```

### 6. getTinyImage
Returns a tiny PNG image with multiple content types.
```json
{
  "name": "getTinyImage",
  "arguments": {}
}
```

### 7. annotatedMessage
Demonstrates message annotations with priorities and audiences.
```json
{
  "name": "annotatedMessage",
  "arguments": {
    "messageType": "error",
    "includeImage": false
  }
}
```

### 8. getResourceReference
Returns a reference to a numbered resource.
```json
{
  "name": "getResourceReference",
  "arguments": {
    "resourceId": 42
  }
}
```

## MCP Resources

### Static Resources
- URIs: `test://static/resource/1` through `test://static/resource/100`
- Even-numbered resources return plain text
- Odd-numbered resources return base64-encoded data

### Resource Template
- URI Template: `test://static/resource/{id}`
- Supports dynamic resource access by ID

## MCP Prompts

### 1. simple_prompt
Basic prompt without arguments.

### 2. complex_prompt
Prompt with temperature and style configuration.
```json
{
  "name": "complex_prompt",
  "arguments": {
    "temperature": "0.7",
    "style": "formal"
  }
}
```

### 3. resource_prompt
Prompt that includes a resource reference.
```json
{
  "name": "resource_prompt",
  "arguments": {
    "resourceId": "42"
  }
}
```

## Testing with MCP Clients

### Using mcp-cli
```bash
# List available tools
mcp-cli --server stdio://rebar3 --server-args "run" tools list

# Call a tool
mcp-cli --server stdio://rebar3 --server-args "run" tools call add --arguments '{"a": 5, "b": 3}'

# Read a resource
mcp-cli --server stdio://rebar3 --server-args "run" resources read --uri "test://static/resource/1"

# Subscribe to resource updates
mcp-cli --server stdio://rebar3 --server-args "run" resources subscribe --uri "test://static/resource/1"
```

## Design Patterns

### State Management
The server uses a record-based state with clear separation of concerns:
```erlang
-record(state, {
    mcp_server :: pid(),
    subscriptions :: sets:set(),
    timers :: #{atom() => reference()},
    log_level :: atom(),
    intervals :: #{atom() => pos_integer()}
}).
```

### Error Handling
- Proper supervision ensures fault tolerance
- All timer references are tracked and cleaned up
- Process links ensure proper cleanup on crashes

### Resource Handlers
Resources use functional handlers for flexibility:
```erlang
Handler = fun(_Uri) -> 
    generate_resource_content(Id) 
end
```

### Schema Validation
All tools use JSON Schema for input validation:
```erlang
Schema = #{
    <<"type">> => <<"object">>,
    <<"properties">> => #{
        <<"message">> => #{<<"type">> => <<"string">>}
    },
    <<"required">> => [<<"message">>]
}
```

## Performance Considerations

- Efficient resource generation using pattern matching
- Sets for O(1) subscription lookups
- Lazy evaluation of resource content
- Configurable intervals for periodic tasks

## Extending the Server

### Adding a New Tool
1. Define the schema in a function
2. Implement the handler function
3. Add to `setup_tools/1`

Example:
```erlang
my_tool_schema() ->
    #{
        <<"type">> => <<"object">>,
        <<"properties">> => #{
            <<"input">> => #{<<"type">> => <<"string">>}
        }
    }.

handle_my_tool(#{<<"input">> := Input}) ->
    [#mcp_content{
        type = <<"text">>,
        text = process_input(Input)
    }].
```

### Adding a New Resource Type
```erlang
erlmcp_server:add_resource(
    McpServer,
    <<"custom://resource">>,
    fun(_Uri) -> 
        <<"Custom resource content">>
    end
).
```

## Troubleshooting

### Server won't start
- Check if another process is using the configured port
- Verify erlmcp is properly included as a dependency
- Check application configuration syntax

### No notifications received
- Ensure client has subscribed to resources
- Verify intervals are configured correctly
- Check log level settings

### Tools not working
- Verify JSON schema matches the arguments
- Check handler function error handling
- Enable debug logging for detailed errors

## License

MIT License - See LICENSE file for details.
