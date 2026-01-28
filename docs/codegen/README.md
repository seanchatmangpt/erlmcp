# erlmcp Code Generation - Client SDK Generator

## Overview

The erlmcp code generator (`erlmcp_codegen`) automatically generates type-safe client SDKs in multiple languages from Erlang MCP server definitions. This enables rapid development of MCP clients across different technology stacks.

## Supported Languages

- **TypeScript** - Modern async/await with full type safety
- **Python** - Type-annotated with Protocol-based transports
- **Go** - Idiomatic Go with context support

## Features

- **Type-safe API** - Generate strongly-typed client methods for tools, resources, and prompts
- **Automatic retries** - Built-in exponential backoff retry logic
- **Connection pooling** - Optional transport-level connection management
- **Error handling** - Comprehensive error types and JSON-RPC error mapping
- **Idiomatic patterns** - Each language follows its ecosystem conventions
- **Extensible templates** - Mustache-based templates for customization

## Architecture

```
erlmcp_server (Erlang)
    ↓ extract_definitions/1
Server Definitions (Map)
    ↓ render_template/3
Mustache Template + Data
    ↓ generate/4
Type-safe Client SDK (TS/Python/Go)
```

## Usage

### Command Line (rebar3 plugin)

```bash
# Generate TypeScript SDK
rebar3 erlmcp codegen \
    --language typescript \
    --server calculator_server \
    --output ./client/ \
    --package calculator_client \
    --version 1.0.0

# Generate Python SDK
rebar3 erlmcp codegen \
    --language python \
    --server weather_server \
    --output ./sdk/python/ \
    --package weather_client

# Generate Go SDK
rebar3 erlmcp codegen \
    --language go \
    --server database_server \
    --output ./sdk/go/ \
    --package database_client
```

### Programmatic API

```erlang
%% Start the codegen server
{ok, _Pid} = erlmcp_codegen:start_link(),

%% Extract server definitions
{ok, Definitions} = erlmcp_codegen:extract_definitions(ServerPid),

%% Generate TypeScript SDK
Options = #{
    package_name => <<"my_client">>,
    version => <<"1.0.0">>,
    filename => "mcp_client.ts"
},
ok = erlmcp_codegen:generate(typescript, Definitions, "./output", Options).
```

## Server Definitions Format

The generator extracts MCP definitions from a running server:

```erlang
#{
    tools => [
        #{
            <<"name">> => <<"add">>,
            <<"description">> => <<"Add two numbers">>,
            <<"inputSchema">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"a">> => #{<<"type">> => <<"number">>},
                    <<"b">> => #{<<"type">> => <<"number">>}
                },
                <<"required">> => [<<"a">>, <<"b">>]
            }
        }
    ],
    resources => [
        #{
            <<"uri">> => <<"file:///data/config.json">>,
            <<"name">> => <<"config">>,
            <<"description">> => <<"Server configuration">>,
            <<"mimeType">> => <<"application/json">>
        }
    ],
    prompts => [
        #{
            <<"name">> => <<"summarize">>,
            <<"description">> => <<"Summarize text">>,
            <<"arguments">> => [
                #{
                    <<"name">> => <<"text">>,
                    <<"required">> => true
                }
            ]
        }
    ],
    capabilities => #{}
}
```

## Generated SDK Examples

### TypeScript

```typescript
import { McpClient } from './mcp_client';

// Initialize client
const client = new McpClient({ transport });
await client.initialize({ name: "my-app", version: "1.0.0" });

// Call tools with type safety
const result = await client.add({ a: 5, b: 3 });

// Read resources
const config = await client.config();

// Get prompts
const messages = await client.summarize({ text: "..." });
```

### Python

```python
from mcp_client import McpClient

# Initialize client
client = McpClient(transport=transport)
await client.initialize({"name": "my-app", "version": "1.0.0"})

# Call tools with type hints
result = await client.add({"a": 5, "b": 3})

# Read resources
config = await client.config()

# Get prompts
messages = await client.summarize({"text": "..."})
```

### Go

```go
import "calculator_client"

// Initialize client
client := calculator_client.NewClient(opts)
result, err := client.Initialize(ctx, calculator_client.McpServerInfo{
    Name:    "my-app",
    Version: "1.0.0",
})

// Call tools with type safety
sum, err := client.Add(ctx, calculator_client.AddInput{A: 5, B: 3})

// Read resources
config, err := client.Config(ctx)

// Get prompts
messages, err := client.Summarize(ctx, calculator_client.SummarizeArguments{
    Text: "...",
})
```

## Template Customization

Templates are located in `priv/codegen/templates/`:

- `typescript_client.mustache` - TypeScript SDK template
- `python_client.mustache` - Python SDK template
- `go_client.mustache` - Go SDK template

### Template Variables

```mustache
{{package_name}}        - Package/module name
{{version}}             - SDK version
{{language}}            - Target language
{{timestamp}}           - Generation timestamp
{{generator_version}}   - Codegen version

{{#has_tools}}          - Boolean: server has tools
  {{#tools}}            - Iterate over tools
    {{name}}            - Tool name (original)
    {{method_name}}     - Tool name (formatted for language)
    {{description}}     - Tool description
    {{#parameters}}     - Tool parameters
      {{param_name}}    - Parameter name (formatted)
      {{type}}          - Parameter type (language-specific)
      {{required}}      - Boolean: parameter required
    {{/parameters}}
  {{/tools}}
{{/has_tools}}

{{#has_resources}}      - Boolean: server has resources
  {{#resources}}        - Iterate over resources
    {{uri}}             - Resource URI
    {{name}}            - Resource name
    {{method_name}}     - Resource accessor method name
  {{/resources}}
{{/has_resources}}

{{#has_prompts}}        - Boolean: server has prompts
  {{#prompts}}          - Iterate over prompts
    {{name}}            - Prompt name
    {{method_name}}     - Prompt method name
    {{#arguments}}      - Prompt arguments
      {{param_name}}    - Argument name
    {{/arguments}}
  {{/prompts}}
{{/has_prompts}}
```

## Name Formatting

The generator automatically formats names according to language conventions:

| Input | TypeScript | Python | Go |
|-------|-----------|--------|-----|
| `echo-message` | `echoMessage` | `echo_message` | `EchoMessage` |
| `get_user_info` | `getUserInfo` | `get_user_info` | `GetUserInfo` |
| `calculate sum` | `calculateSum` | `calculate_sum` | `CalculateSum` |

## Type Mapping

JSON Schema types are mapped to language-specific types:

| JSON Schema | TypeScript | Python | Go |
|-------------|-----------|--------|-----|
| `string` | `string` | `str` | `string` |
| `number` | `number` | `float` | `float64` |
| `integer` | `number` | `int` | `int64` |
| `boolean` | `boolean` | `bool` | `bool` |
| `array` | `any[]` | `List[Any]` | `[]interface{}` |
| `object` | `Record<string, any>` | `Dict[str, Any]` | `map[string]interface{}` |

## Error Handling

All generated SDKs include comprehensive error handling:

### TypeScript
```typescript
try {
  await client.add({ a: 5, b: 3 });
} catch (error) {
  console.error(`MCP Error ${error.code}: ${error.message}`);
}
```

### Python
```python
try:
    await client.add({"a": 5, "b": 3})
except McpError as e:
    print(f"MCP Error {e.code}: {e.message}")
```

### Go
```go
result, err := client.Add(ctx, AddInput{A: 5, B: 3})
if err != nil {
    if mcpErr, ok := err.(*McpError); ok {
        log.Printf("MCP Error %d: %s", mcpErr.Code, mcpErr.Message)
    }
}
```

## Transport Implementation

Clients require a transport implementation. Example for stdio:

### TypeScript
```typescript
class StdioTransport implements Transport {
  async send(message: JsonRpcRequest): Promise<void> {
    process.stdout.write(JSON.stringify(message) + '\n');
  }

  async receive(): Promise<JsonRpcResponse> {
    return new Promise((resolve) => {
      process.stdin.once('data', (data) => {
        resolve(JSON.parse(data.toString()));
      });
    });
  }

  async close(): Promise<void> {
    process.stdin.pause();
  }
}
```

### Python
```python
class StdioTransport:
    async def send(self, message: JsonRpcRequest) -> None:
        sys.stdout.write(json.dumps(message) + '\n')
        sys.stdout.flush()

    async def receive(self) -> JsonRpcResponse:
        line = await asyncio.get_event_loop().run_in_executor(
            None, sys.stdin.readline
        )
        return json.loads(line)

    async def close(self) -> None:
        pass
```

### Go
```go
type StdioTransport struct{}

func (t *StdioTransport) Send(ctx context.Context, msg JsonRpcRequest) error {
    data, _ := json.Marshal(msg)
    _, err := os.Stdout.Write(append(data, '\n'))
    return err
}

func (t *StdioTransport) Receive(ctx context.Context) (JsonRpcResponse, error) {
    reader := bufio.NewReader(os.Stdin)
    line, err := reader.ReadString('\n')
    if err != nil {
        return JsonRpcResponse{}, err
    }

    var resp JsonRpcResponse
    err = json.Unmarshal([]byte(line), &resp)
    return resp, err
}

func (t *StdioTransport) Close() error {
    return nil
}
```

## Testing

```bash
# Run codegen tests
rebar3 eunit --module=erlmcp_codegen_tests

# Test specific SDK generation
rebar3 eunit --module=erlmcp_codegen_tests --tests=generate_typescript_sdk_test

# Verify template rendering
rebar3 eunit --module=erlmcp_codegen_tests --tests=render_typescript_template_test
```

## Performance

- **Template loading** - Templates loaded once at startup (cached)
- **Generation speed** - ~10-50ms per SDK (typical server)
- **Memory usage** - ~5-10MB for codegen process
- **Concurrent generation** - Safe for parallel SDK generation

## Troubleshomat

### Template not found
```
Error: {template_not_found, typescript}
```
**Solution**: Ensure templates exist in `priv/codegen/templates/`

### Render failed
```
Error: {render_failed, Reason}
```
**Solution**: Check template syntax and definition format

### Write failed
```
Error: {write_failed, eacces}
```
**Solution**: Verify output directory permissions

## Best Practices

1. **Version SDKs** - Include version in generated file header
2. **Type coverage** - Use strict types where possible
3. **Error messages** - Preserve server error details
4. **Documentation** - Include tool/resource descriptions
5. **Idiomatic code** - Follow language conventions
6. **Testing** - Test generated SDKs with real servers

## Future Enhancements

- [ ] Rust client generation
- [ ] Java/Kotlin client generation
- [ ] Swift client generation (iOS/macOS)
- [ ] C# client generation
- [ ] Custom template plugins
- [ ] OpenAPI spec generation
- [ ] Automated SDK testing
- [ ] Package publishing automation

## See Also

- [MCP Protocol Specification](../protocol.md)
- [erlmcp Server API](../api-reference.md)
- [Transport Development](../transports.md)
- [Examples](../../examples/)
