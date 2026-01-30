# Erlang MCP Server Handler Template

## Overview

This directory contains Tera templates for generating production-ready Erlang MCP (Model Context Protocol) server message handlers. The templates follow erlmcp OTP patterns and best practices.

## Template: server_handler.erl.tera

Generates complete MCP server handlers with:

- **Phase validation** - Checks current phase (initialization/initialized) matches required phase
- **Capability validation** - Verifies negotiated capabilities support the operation
- **Schema validation** - Validates params against JSON Schema from ontology
- **Handler execution** - Try-catch error handling with proper MCP error codes
- **Progress notifications** - Optional progress token generation and tracking
- **Cancellation support** - Optional request cancellation checking
- **Logging integration** - Structured logging with logger module
- **Tracing integration** - OpenTelemetry-compatible tracing spans
- **CPU protection** - Quota-based execution with timeouts
- **Helper functions** - Validation and formatting utilities

## Template Variables

### Required Variables

| Variable | Type | Description |
|----------|------|-------------|
| module_name | string | Generated module name (e.g., "mcp_handler_tools_call") |
| method | string | MCP method name (e.g., "tools/call") |
| timestamp | string | Generation timestamp (ISO 8601) |
| generator_version | string | ggen version number |
| handler_function | string | Name of business logic function to call |

### Optional Variables

| Variable | Type | Default | Description |
|----------|------|---------|-------------|
| method_const | string | - | Constant name (e.g., "MCP_METHOD_TOOLS_CALL") |
| requires_phase | string | "initialized" | Required phase: "initialization", "initialized", or "any" |
| capability | string | - | Required capability: "tools", "resources", "prompts", "logging", "sampling", "roots" |
| capability_record | string | capability | Override capability record field name |
| validate_schema | boolean | false | Enable JSON Schema validation |
| schema | map | {} | JSON Schema definition if validate_schema is true |
| params_required | list | [] | List of required parameter names |
| supports_progress | boolean | false | Enable progress notification support |
| supports_cancellation | boolean | false | Enable cancellation token checking |
| timeout_ms | integer | 5000 | Operation timeout in milliseconds |
| description | string | "MCP protocol handler implementation." | Handler description |

## References

- MCP Specification: docs/protocol.md
- erlmcp_server patterns: apps/erlmcp_core/src/erlmcp_server.erl:1163-1377
- Error codes: include/erlmcp.hrl
- OTP patterns: docs/otp-patterns.md
