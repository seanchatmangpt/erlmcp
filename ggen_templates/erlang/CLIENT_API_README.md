# Erlang MCP Client API Generator

This directory contains Tera templates for generating Erlang MCP client API methods using the ggen code generation system.

## Files

- `client_api.erl.tera` - Main template for generating client API methods
- `client_api_example_context.json` - Example context showing template usage
- `CLIENT_API_README.md` - This file

## Template: client_api.erl.tera

Generates complete MCP client API methods including function exports, type specs, documentation, API functions, and gen_server callbacks.

### Key Features

- **Phase Checking**: Enforces client lifecycle phases (pre_initialization → initializing → initialized)
- **Capability Validation**: Checks server capabilities when strict_mode is enabled
- **Request ID Safety**: Overflow protection with threshold monitoring
- **Timeout Support**: Configurable timeouts per method
- **Error Handling**: Comprehensive error tuples for all failure modes
- **Optional Parameters**: Generates overloaded functions for optional args

## Quick Start

```bash
# Test template with example context
cd /home/user/erlmcp

# View example context
cat ggen_templates/erlang/client_api_example_context.json

# Manual template test (if ggen is available)
# ggen render \
#   --template ggen_templates/erlang/client_api.erl.tera \
#   --context ggen_templates/erlang/client_api_example_context.json \
#   --output /tmp/test_client_api.erl
```

## Context Structure

See `client_api_example_context.json` for a complete example with 15 methods including:
- initialize/2, initialize/3
- list_resources/1, read_resource/2
- list_tools/1, call_tool/3
- list_prompts/1, get_prompt/2, get_prompt/3
- create_task/2, list_tasks/1, get_task/2, cancel_task/2
- ping/1

## Integration

Generated code can be:
1. Integrated into `erlmcp_client.erl` directly
2. Generated as a separate module and included
3. Used as reference for manual implementation

## Documentation

For full documentation, see the template header comments in `client_api.erl.tera`.

## License

Apache 2.0 (same as erlmcp project)
