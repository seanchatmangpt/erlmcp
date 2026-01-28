# erlmcp v2.0 Examples

This directory contains example applications demonstrating erlmcp v2.0 functionality with the new umbrella project structure.

## v2.0 Umbrella Structure

erlmcp v2.0 uses an umbrella project with separate applications:
- **`erlmcp_core`** - Core protocol, JSON-RPC, client/server, registry
- **`erlmcp_transports`** - STDIO, TCP, HTTP, WebSocket transports
- **`erlmcp_observability`** - Metrics, traces, receipts (OpenTelemetry)
- **`tcps_erlmcp`** - Toyota Code Production System (optional)

See `v2_migration_guide.md` for detailed migration instructions.

## Examples

| Example | Description | Key Features |
|---------|-------------|--------------|
| **simple/** | Basic MCP server with tools, resources, prompts | STDIO transport, echo tool, system_info |
| **calculator/** | Mathematical computation server | Multiple tools, error handling, expression parsing |
| **weather/** | Weather information service | Complex resources, tool chaining |
| **poolboy/** | Connection pool management | Poolboy integration, resource pooling |

## Quick Start

### Prerequisites

1. Compile the root umbrella project first:
```bash
cd /path/to/erlmcp
rebar3 compile
```

2. Navigate to an example directory:
```bash
cd examples/simple
```

### Build & Run

Each example has a Makefile with standard targets:

```bash
# Compile the example
make compile

# Run the server
make run

# Start an Erlang shell with compiled modules
make shell

# Run tests (if available)
make test

# Clean build artifacts
make clean
```

### Example: Simple Server

```bash
cd examples/simple
make compile
make run
```

The simple server will start and wait for MCP protocol messages on stdin/stdout.

## How Examples Work (v2.0)

### Build System

Examples use Makefiles that reference the umbrella project's compiled modules:

```makefile
# Makefile structure
ROOT := ../..
BUILD := $(ROOT)/_build/default/lib
LIB_DIRS := $(wildcard $(BUILD)/*/ebin)
PA_FLAGS := $(foreach dir,$(LIB_DIRS),-pa $(dir))
ERLC_OPTS := +debug_info -I$(ROOT)/include $(PA_FLAGS)
```

### Application Startup

Examples explicitly start required umbrella applications:

```erlang
%% Start v2.0 umbrella apps
application:ensure_all_started(erlmcp_core),
application:ensure_all_started(erlmcp_transports),

%% Then use modules normally
erlmcp_stdio:start().
```

### Module Imports

Module names remain unchanged from v1.x:
```erlang
-include("erlmcp.hrl").  % Still from project root

erlmcp_client:start_link(...),
erlmcp_server:add_tool(...),
erlmcp_stdio:start().
```

## Integration with Claude Desktop

### Configuration

To use an example as an MCP server with Claude Desktop, update `claud_desktop_config.json`:

```json
{
  "mcpServers": {
    "erlmcp-simple": {
      "command": "erl",
      "args": [
        "-pa", "/path/to/erlmcp/_build/default/lib/*/ebin",
        "-pa", "/path/to/erlmcp/examples/simple",
        "-eval", "simple_server_stdio:start()",
        "-noshell"
      ]
    }
  }
}
```

**Note**: Use absolute paths. The wildcard `-pa` pattern works in `erl` command (not in `erlc`).

### Testing with Claude

Once configured and Claude Desktop restarted, you can test:
```
User: Use the echo tool to repeat "Hello, erlmcp v2.0!"
Claude: [Calls the echo tool] Echo: Hello, erlmcp v2.0!
```

## Development Workflow

### From Root Project

You can also compile and run examples from the root project:

```bash
# Compile root + all examples
cd /path/to/erlmcp
rebar3 compile
cd examples/simple && make compile

# Run via rebar3 shell
rebar3 shell
1> c("examples/simple/simple_server_stdio").
2> simple_server_stdio:start().
```

### Testing Changes

After modifying core modules:
```bash
# Recompile root project
cd /path/to/erlmcp
rebar3 compile

# Recompile examples
cd examples/simple
make clean compile
make test
```

## Architecture Benefits

### v2.0 Advantages

1. **Modular** - Only load what you need (core, transports, observability)
2. **Scalable** - Apps can be versioned and deployed independently
3. **Clean** - Clear separation of concerns
4. **Flexible** - Optional components (TCPS can be excluded)
5. **Production-Ready** - Proper OTP application structure

### Backwards Compatible

- ✅ All module names unchanged
- ✅ All function signatures unchanged
- ✅ All protocol behavior unchanged
- ✅ Existing code works with minor app startup changes

## Troubleshooting

### Issue: Module Not Found

**Symptom:** `undefined function erlmcp_stdio:start/0`

**Solution:** Ensure root project is compiled:
```bash
cd /path/to/erlmcp && rebar3 compile
```

### Issue: Include File Not Found

**Symptom:** `can't find include file "erlmcp.hrl"`

**Solution:** Check Makefile has correct include path:
```makefile
ERLC_OPTS := +debug_info -I../../include
```

### Issue: Application Not Started

**Symptom:** `undefined function erlmcp_registry:register/2`

**Solution:** Start applications before using modules:
```erlang
application:ensure_all_started(erlmcp_core).
```

### Issue: Compilation Error

**Symptom:** `File has no extension` or wildcard issues

**Solution:** Use proper Makefile from examples (wildcards work differently in `erlc` vs `make`).

## Example Structure

Each example contains:
```
example_name/
├── Makefile              # Build system with standard targets
├── rebar.config          # Minimal config for IDE support
├── README.md             # Specific example documentation
├── *_server_stdio.erl    # Server implementation
├── *_client.erl          # Client implementation (if applicable)
└── *_test.erl            # Tests (if available)
```

## Next Steps

1. **Explore Examples** - Run each example to understand MCP features
2. **Read Migration Guide** - See `v2_migration_guide.md` for full details
3. **Check Documentation** - See `docs/` for architecture and patterns
4. **Build Your Own** - Use examples as templates for your MCP servers

## Resources

- **Migration Guide**: `examples/v2_migration_guide.md`
- **Architecture**: `docs/architecture.md`
- **OTP Patterns**: `docs/otp-patterns.md`
- **Protocol Spec**: `docs/protocol.md`
- **Root Config**: `rebar.config` (umbrella configuration)

## Questions?

1. Review the migration guide
2. Check example READMEs
3. Verify root project compilation
4. Ensure Makefiles match templates
5. Test with `make compile` in example directory
