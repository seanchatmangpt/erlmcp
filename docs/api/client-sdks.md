# erlmcp Client SDK Reference

## Overview

This document provides reference implementations for MCP clients in multiple programming languages. These SDKs handle JSON-RPC communication, transport management, and provide high-level APIs for MCP operations.

---

## Python Client SDK

### Installation

```bash
pip install erlmcp-client
```

### Quick Start

```python
import asyncio
from erlmcp import ClientSession, StdioTransport

async def main():
    # Create client with stdio transport
    transport = StdioTransport({
        "command": "erl",
        "args": ["-pa", "/path/to/ebin", "-eval", "mcp_server:start()", "-noshell"]
    })

    async with ClientSession(transport) as session:
        # Initialize
        result = await session.initialize()
        print(f"Connected to {result.server_info.name} v{result.server_info.version}")

        # List tools
        tools = await session.list_tools()
        for tool in tools:
            print(f"  - {tool.name}: {tool.description}")

        # Call a tool
        response = await session.call_tool("calculate", {"a": 10, "b": 5, "op": "multiply"})
        print(f"Result: {response.content[0].text}")

        # List resources
        resources = await session.list_resources()
        for resource in resources:
            print(f"  - {resource.uri}: {resource.name}")

asyncio.run(main())
```

### Advanced Usage

#### HTTP Transport

```python
from erlmcp import ClientSession, HttpTransport

async with ClientSession(HttpTransport("http://localhost:8765/mcp")) as session:
    await session.initialize()
    # Use session...
```

#### WebSocket Transport

```python
from erlmcp import ClientSession, WebSocketTransport

async with ClientSession(WebSocketTransport("ws://localhost:8765/mcp")) as session:
    await session.initialize()
    # Use session...
```

#### Progress Token Handling

```python
async def call_with_progress(session):
    progress_token = await session.call_tool(
        "long_running_task",
        {"input": "data"},
        progress_callback=lambda update: print(f"Progress: {update.progress * 100}%")
    )

    result = await session.get_result(progress_token)
    return result
```

#### Resource Subscriptions

```python
async def subscribe_to_resource(session, uri):
    # Subscribe
    await session.subscribe_resource(uri)

    # Set up notification handler
    @session.on_resource_updated
    def handle_update(updated_uri):
        if updated_uri == uri:
            content = await session.read_resource(uri)
            print(f"Resource updated: {content}")
```

#### Error Handling

```python
from erlmcp.exceptions import (
    McpError,
    ToolNotFoundError,
    InvalidParamsError,
    TimeoutError
)

async def safe_tool_call(session, tool_name, arguments):
    try:
        result = await session.call_tool(tool_name, arguments)
        return result
    except ToolNotFoundError:
        print(f"Tool {tool_name} not found")
        return None
    except InvalidParamsError as e:
        print(f"Invalid arguments: {e.details}")
        return None
    except TimeoutError:
        print("Tool call timed out")
        return None
```

### Complete Example

```python
import asyncio
from erlmcp import ClientSession, StdioTransport
from erlmcp.exceptions import McpError

class McpClient:
    def __init__(self, transport_config):
        self.transport = StdioTransport(transport_config)
        self.session = None

    async def connect(self):
        self.session = ClientSession(self.transport)
        await self.session.__aenter__()
        result = await self.session.initialize()
        return result

    async def list_available_tools(self):
        tools = await self.session.list_tools()
        return {tool.name: tool for tool in tools}

    async def execute_tool(self, name, arguments=None):
        try:
            response = await self.session.call_tool(name, arguments or {})
            return response.content
        except McpError as e:
            return {"error": str(e)}

    async def get_resource(self, uri):
        try:
            result = await self.session.read_resource(uri)
            return result.contents
        except McpError as e:
            return {"error": str(e)}

    async def get_prompt(self, name, arguments=None):
        try:
            result = await self.session.get_prompt(name, arguments or {})
            return result.messages
        except McpError as e:
            return {"error": str(e)}

    async def close(self):
        if self.session:
            await self.session.__aexit__(None, None, None)

async def main():
    config = {
        "command": "erl",
        "args": ["-noshell", "-eval", "mcp_server:start()"]
    }

    client = McpClient(config)
    try:
        init_result = await client.connect()
        print(f"Connected to {init_result.server_info.name}")

        tools = await client.list_available_tools()
        print(f"Available tools: {list(tools.keys())}")

        result = await client.execute_tool("calculate", {"a": 10, "b": 5, "op": "add"})
        print(f"Result: {result}")

    finally:
        await client.close()

asyncio.run(main())
```

---

## Go Client SDK

### Installation

```bash
go get github.com/erlmcp/go-client
```

### Quick Start

```go
package main

import (
    "context"
    "fmt"
    "log"

    "github.com/erlmcp/go-client"
    "github.com/erlmcp/go-client/transport"
)

func main() {
    // Create stdio transport
    tr := transport.NewStdio(transport.StdioConfig{
        Command: "erl",
        Args:    []string{"-noshell", "-eval", "mcp_server:start()"},
    })

    // Create client
    client := erlmcp.NewClient(tr)

    ctx := context.Background()

    // Connect and initialize
    serverInfo, err := client.Connect(ctx)
    if err != nil {
        log.Fatal(err)
    }
    fmt.Printf("Connected to %s v%s\n", serverInfo.Name, serverInfo.Version)

    // List tools
    tools, err := client.ListTools(ctx)
    if err != nil {
        log.Fatal(err)
    }
    for _, tool := range tools {
        fmt.Printf("  - %s: %s\n", tool.Name, tool.Description)
    }

    // Call tool
    result, err := client.CallTool(ctx, "calculate", map[string]interface{}{
        "a":  10,
        "b":  5,
        "op": "multiply",
    })
    if err != nil {
        log.Fatal(err)
    }
    fmt.Printf("Result: %s\n", result.Content[0].Text)
}
```

### Advanced Usage

#### HTTP Transport

```go
tr := transport.NewHTTP(transport.HTTPConfig{
    URL: "http://localhost:8765/mcp",
})
```

#### WebSocket Transport

```go
tr := transport.NewWebSocket(transport.WebSocketConfig{
    URL: "ws://localhost:8765/mcp",
})
```

#### Progress Callback

```go
progressCh := make(chan *erlmcp.ProgressUpdate, 10)

go func() {
    for update := range progressCh {
        fmt.Printf("Progress: %.0f%%\n", update.Progress*100)
    }
}()

result, err := client.CallToolWithProgress(ctx, "long_task", args, progressCh)
```

#### Resource Subscription

```go
// Subscribe to resource
err := client.SubscribeResource(ctx, "mcp://config")

// Set up notification handler
client.OnResourceUpdated(func(uri string) {
    if uri == "mcp://config" {
        content, _ := client.ReadResource(ctx, uri)
        fmt.Printf("Updated: %v\n", content)
    }
})
```

#### Error Handling

```go
result, err := client.CallTool(ctx, "tool_name", args)
if err != nil {
    switch e := err.(type) {
    case *erlmcp.ToolNotFoundError:
        fmt.Printf("Tool not found: %s\n", e.ToolName)
    case *erlmcp.InvalidParamsError:
        fmt.Printf("Invalid params: %v\n", e.Details)
    case *erlmcp.TimeoutError:
        fmt.Println("Request timed out")
    default:
        fmt.Printf("Error: %v\n", err)
    }
    return
}
```

---

## Java Client SDK

### Installation (Maven)

```xml
<dependency>
    <groupId>io.github.erlmcp</groupId>
    <artifactId>erlmcp-client</artifactId>
    <version>3.0.0</version>
</dependency>
```

### Quick Start

```java
import io.github.erlmcp.*;
import io.github.erlmcp.transport.*;

public class McpExample {
    public static void main(String[] args) {
        // Create transport
        StdioTransport transport = new StdioTransport(
            "erl",
            new String[]{"-noshell", "-eval", "mcp_server:start()"}
        );

        // Create client
        McpClient client = new McpClient(transport);

        try {
            // Connect and initialize
            ServerInfo info = client.connect();
            System.out.println("Connected to " + info.getName() + " v" + info.getVersion());

            // List tools
            List<Tool> tools = client.listTools();
            for (Tool tool : tools) {
                System.out.println("  - " + tool.getName() + ": " + tool.getDescription());
            }

            // Call tool
            ToolResult result = client.callTool("calculate", Map.of(
                "a", 10,
                "b", 5,
                "op", "multiply"
            ));
            System.out.println("Result: " + result.getContent().get(0).getText());

        } catch (McpException e) {
            System.err.println("Error: " + e.getMessage());
        } finally {
            client.close();
        }
    }
}
```

### Async Usage

```java
import java.util.concurrent.CompletableFuture;

public class AsyncExample {
    public static void main(String[] args) {
        McpClient client = new McpClient(new StdioTransport("erl", args));

        // Async connection
        CompletableFuture<ServerInfo> future = client.connectAsync()
            .thenCompose(info -> {
                System.out.println("Connected!");
                // List tools async
                return client.listToolsAsync();
            })
            .thenApply(tools -> {
                tools.forEach(tool -> System.out.println("Tool: " + tool.getName()));
                return tools;
            });

        // Wait for completion
        try {
            future.get();
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
```

---

## JavaScript/TypeScript Client SDK

### Installation

```bash
npm install @erlmcp/client
```

### Quick Start (TypeScript)

```typescript
import { ClientSession, StdioTransport } from '@erlmcp/client';

async function main() {
    // Create transport
    const transport = new StdioTransport({
        command: 'erl',
        args: ['-noshell', '-eval', 'mcp_server:start()']
    });

    // Create session
    const session = new ClientSession(transport);

    try {
        // Initialize
        const initResult = await session.initialize();
        console.log(`Connected to ${initResult.serverInfo.name}`);

        // List tools
        const tools = await session.listTools();
        for (const tool of tools) {
            console.log(`  - ${tool.name}: ${tool.description}`);
        }

        // Call tool
        const result = await session.callTool('calculate', {
            a: 10,
            b: 5,
            op: 'multiply'
        });
        console.log(`Result: ${result.content[0].text}`);

    } finally {
        await session.close();
    }
}

main();
```

### Browser WebSocket Example

```typescript
import { ClientSession, WebSocketTransport } from '@erlmcp/client';

// Browser-based client
const wsTransport = new WebSocketTransport('ws://localhost:8765/mcp');
const session = new ClientSession(wsTransport);

await session.initialize();

// Handle notifications
session.on('resources/updated', (params) => {
    console.log(`Resource updated: ${params.uri}`);
});

// Call tool
const result = await session.callTool('my_tool', { param: 'value' });
```

---

## Rust Client SDK

### Installation

```toml
[dependencies]
erlmcp-client = "3.0"
```

### Quick Start

```rust
use erlmcp_client::{Client, StdioTransport};
use erlmcp_client::Result;

#[tokio::main]
async fn main() -> Result<()> {
    // Create transport
    let transport = StdioTransport::new("erl", &["-noshell", "-eval", "mcp_server:start()"]);

    // Create client
    let mut client = Client::new(transport);

    // Connect
    let server_info = client.connect().await?;
    println!("Connected to {} v{}", server_info.name, server_info.version);

    // List tools
    let tools = client.list_tools().await?;
    for tool in tools {
        println!("  - {}: {}", tool.name, tool.description);
    }

    // Call tool
    let result = client.call_tool("calculate", serde_json::json!({
        "a": 10,
        "b": 5,
        "op": "multiply"
    })).await?;
    println!("Result: {}", result.content[0].text);

    Ok(())
}
```

---

## Common Patterns

### Retry with Backoff

```python
import asyncio
from erlmcp import McpError

async def call_with_retry(session, tool_name, args, max_retries=3):
    for attempt in range(max_retries):
        try:
            return await session.call_tool(tool_name, args)
        except McpError as e:
            if e.code == -32010:  # Rate limited
                wait_time = 2 ** attempt
                print(f"Rate limited, waiting {wait_time}s...")
                await asyncio.sleep(wait_time)
            else:
                raise
    raise Exception("Max retries exceeded")
```

### Batch Operations

```go
// Concurrent tool calls in Go
var wg sync.WaitGroup
results := make(chan *ToolResult, len(tools))

for _, tool := range tools {
    wg.Add(1)
    go func(name string, args map[string]interface{}) {
        defer wg.Done()
        result, err := client.CallTool(ctx, name, args)
        if err == nil {
            results <- result
        }
    }(tool.Name, tool.Args)
}

go func() {
    wg.Wait()
    close(results)
}()

for result := range results {
    // Process results...
}
```

### Caching Layer

```java
public class CachedMcpClient {
    private final McpClient client;
    private final Cache<String, ToolResult> cache;

    public CachedMcpClient(McpClient client) {
        this.client = client;
        this.cache = CacheBuilder.newBuilder()
            .expireAfterWrite(5, TimeUnit.MINUTES)
            .maximumSize(100)
            .build();
    }

    public ToolResult callTool(String name, Map<String, Object> args) throws McpException {
        String key = name + args.toString();

        return cache.get(key, () -> client.callTool(name, args));
    }
}
```

---

## SDK Feature Comparison

| Feature | Python | Go | Java | JavaScript | Rust |
|---------|--------|-------|------|------------|------|
| stdio transport | Yes | Yes | Yes | Yes | Yes |
| HTTP transport | Yes | Yes | Yes | Yes | Yes |
| WebSocket | Yes | Yes | Yes | Yes | Yes |
| SSE | Yes | Yes | Planned | Yes | Planned |
| Async/await | Yes | Yes | Yes | Yes | Yes |
| Type hints | Yes | No | Yes | Yes | Yes |
| Progress tokens | Yes | Yes | Yes | Yes | Yes |
| Subscriptions | Yes | Yes | Yes | Yes | Yes |
| Retry logic | Built-in | Built-in | Manual | Built-in | Manual |
| Connection pooling | Planned | Yes | Yes | Yes | Planned |

---

## Testing with SDKs

### Python

```python
import pytest
from erlmcp import ClientSession
from erlmcp.testing import MockServer

@pytest.mark.asyncio
async def test_tool_call():
    async with MockServer() as server:
        server.add_tool("test", {"result": "ok"})

        async with ClientSession(server.transport) as session:
            result = await session.call_tool("test", {})
            assert result.content[0].text == "ok"
```

### Go

```go
func TestToolCall(t *testing.T) {
    server := mock.NewServer(t)
    server.AddTool("test", `{"result": "ok"}`)

    client := erlmcp.NewClient(server.Transport())
    defer client.Close()

    result, err := client.CallTool(context.Background(), "test", nil)
    assert.NoError(t, err)
    assert.Equal(t, "ok", result.Content[0].Text)
}
```

---

## Best Practices

1. **Always close sessions** to release resources
2. **Use context/timeouts** to prevent hanging
3. **Handle errors explicitly** based on error codes
4. **Implement retries** for transient errors
5. **Cache results** for expensive operations
6. **Use progress tokens** for long-running tasks
7. **Unsubscribe** when done with resources
