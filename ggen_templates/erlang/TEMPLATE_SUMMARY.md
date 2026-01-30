# Server Handler Template Summary

## Files Created

### 1. server_handler.erl.tera (602 lines)
Comprehensive Tera template for generating MCP server message handlers.

**Key Features:**
- Module header with generation metadata and warnings
- Phase validation (initialization state machine per MCP 2025-11-25)
- Capability validation (negotiated capabilities checking)
- Schema validation (JSON Schema with jesse library)
- Parameter validation (required fields checking)
- Handler execution with CPU protection and quotas
- Progress notification support (optional)
- Cancellation token checking (optional)
- Error handling with proper MCP error codes
- Tracing integration (OpenTelemetry-compatible spans)
- Logging integration (structured logger)
- Response formatting and normalization
- Helper functions for validation and error messages

**Generated Code Structure:**
```erlang
-module(generated_handler_name).
-behaviour(erlmcp_handler).
-include("erlmcp.hrl").

%% API
-export([handle/4, validate_phase/1, validate_capability/2, validate_params/1]).

%% Main handler (validates phase, capability, params, schema, then executes)
handle(Id, Params, TransportId, State) -> ...

%% Validation functions
validate_phase(State) -> ...
validate_capability(State, Capability) -> ...
validate_params(Params) -> ...
validate_schema(Params) -> ...  % If validate_schema: true

%% Execution
execute_handler(Id, Params, TransportId, State, SpanCtx) -> ...

%% Response formatting
format_response(Result) -> ...
normalize_result(Result) -> ...

%% Business logic placeholder (TO BE IMPLEMENTED)
handler_function(Params, State) -> ...
```

### 2. README.md
Comprehensive documentation explaining:
- Template variables (required and optional)
- Phase validation (initialization/initialized/any)
- Capability validation (resources/tools/prompts/logging/sampling/roots)
- Schema validation with JSON Schema
- Error codes and their usage
- Progress notification patterns
- Cancellation support patterns
- Integration with erlmcp_server
- Quality gates and testing

### 3. example_context.json
Example context showing how to invoke the template for a "tools/call" handler:
```json
{
  "module_name": "mcp_handler_tools_call",
  "method": "tools/call",
  "requires_phase": "initialized",
  "capability": "tools",
  "validate_schema": true,
  "supports_progress": true,
  "supports_cancellation": true,
  "timeout_ms": 30000
}
```

## Template Variables

### Required
- module_name - Generated module name
- method - MCP method name
- timestamp - Generation timestamp
- generator_version - ggen version
- handler_function - Business logic function name

### Optional (with defaults)
- requires_phase: "initialized"
- capability: none
- validate_schema: false
- params_required: []
- supports_progress: false
- supports_cancellation: false
- timeout_ms: 5000
- description: "MCP protocol handler implementation."

## MCP 2025-11-25 Compliance

The template generates code compliant with MCP 2025-11-25 specification:

1. **Phase State Machine** (Gap #4)
   - initialization: Only initialize method allowed
   - initialized: All methods allowed
   - Proper error codes on phase violations

2. **Capability Negotiation**
   - Validates capabilities were negotiated
   - Checks capability records are enabled
   - Returns MCP_ERROR_CAPABILITY_NOT_SUPPORTED on violations

3. **Schema Validation**
   - Uses JSON Schema for parameter validation
   - Returns detailed validation errors
   - Supports jesse library integration

4. **Error Codes**
   - All error codes from include/erlmcp.hrl
   - Proper JSON-RPC 2.0 standard codes
   - MCP-specific error codes (-32000 to -32099)

5. **Progress Notifications** (Gap #10)
   - Generates unique progress tokens
   - Tracks operations with erlmcp_progress
   - Includes token in response metadata
   - Cleanup on completion or error

6. **CPU Protection** (Task #107)
   - Uses erlmcp_cpu_guard:execute_with_protection/5
   - Enforces quotas and timeouts
   - Returns proper errors on quota violations

7. **Tracing** (OpenTelemetry)
   - Start/end spans with erlmcp_tracing
   - Set attributes for observability
   - Record errors and exceptions
   - Add events for validation steps

## Pattern Compliance

The template follows erlmcp_server.erl patterns (lines 1163-1377):

1. **Span Management**
   ```erlang
   SpanCtx = erlmcp_tracing:start_server_span(<<"method">>, ServerId),
   try
       %% operation
   after
       erlmcp_tracing:end_span(SpanCtx)
   end
   ```

2. **State Access**
   ```erlang
   CurrentPhase = element(3, State),  % #state.phase
   Capabilities = element(6, State),  % #state.capabilities
   ServerId = element(2, State),      % #state.server_id
   ```

3. **Error Handling**
   ```erlang
   catch
       Class:Reason:Stacktrace ->
           erlmcp_tracing:record_exception(SpanCtx, Class, Reason, Stacktrace),
           logger:error("Handler crashed: ~p:~p~n~p", [Class, Reason, Stacktrace]),
           {error, ?JSONRPC_INTERNAL_ERROR, ?JSONRPC_MSG_INTERNAL_ERROR}
   ```

4. **Progress Tracking**
   ```erlang
   ProgressToken = erlmcp_progress:generate_token(),
   _ = erlmcp_progress:track_operation(ProgressToken, Method, ServerPid),
   %% ... operation ...
   erlmcp_progress:cleanup_completed(ProgressToken)
   ```

## Usage Workflow

1. **Define Handler in Ontology**
   ```turtle
   :ToolsCallHandler a mcp:ServerHandler ;
       mcp:method "tools/call" ;
       mcp:requiresPhase :initialized ;
       mcp:requiresCapability :tools ;
       mcp:supportsProgress true ;
       mcp:supportsCancellation true ;
       mcp:timeoutMs 30000 .
   ```

2. **Query Ontology with SPARQL**
   ```sparql
   SELECT ?method ?phase ?capability ?timeout
   WHERE {
       ?handler a mcp:ServerHandler ;
                mcp:method ?method ;
                mcp:requiresPhase ?phase .
   }
   ```

3. **Render Template with ggen**
   ```bash
   ggen render \
     --template ggen_templates/erlang/server_handler.erl.tera \
     --context context.json \
     --output apps/erlmcp_core/src/handlers/mcp_handler_tools_call.erl
   ```

4. **Implement Business Logic**
   ```erlang
   %% Replace placeholder in generated code
   execute_tool(Params, State) ->
       Name = maps:get(<<"name">>, Params),
       Arguments = maps:get(<<"arguments">>, Params),
       %% ... actual tool execution ...
       {ok, Result}.
   ```

5. **Test Generated Handler**
   ```bash
   rebar3 eunit --module=mcp_handler_tools_call_tests
   rebar3 dialyzer
   rebar3 xref
   ```

## Quality Gates

Generated code must pass:

```bash
✅ Compilation: TERM=dumb rebar3 compile (0 errors)
✅ Tests: rebar3 eunit (100% pass rate)
✅ Dialyzer: rebar3 dialyzer (0 type warnings)
✅ Xref: rebar3 xref (0 undefined calls)
✅ Format: rebar3 format --verify (no changes)
```

## Error Code Reference

| Code | Constant | Generated Usage |
|------|----------|----------------|
| -32600 | JSONRPC_INVALID_REQUEST | Malformed request |
| -32602 | JSONRPC_INVALID_PARAMS | Missing required params |
| -32603 | JSONRPC_INTERNAL_ERROR | Handler crashes, quota exceeded |
| -32004 | MCP_ERROR_CAPABILITY_NOT_SUPPORTED | Capability not negotiated |
| -32005 | MCP_ERROR_NOT_INITIALIZED | Phase violation |
| -32007 | MCP_ERROR_VALIDATION_FAILED | Schema validation failed |
| -32009 | MCP_ERROR_TIMEOUT | Handler timeout |
| -32033 | MCP_ERROR_TOOL_CANCELLED | Cancellation requested |

## References

- **MCP Spec:** docs/protocol.md
- **erlmcp_server patterns:** apps/erlmcp_core/src/erlmcp_server.erl:1163-1377
- **Error codes:** include/erlmcp.hrl
- **OTP patterns:** docs/otp-patterns.md
- **Phase machine:** include/erlmcp.hrl:436-451
- **Capabilities:** include/erlmcp.hrl:531-540

## Version

- Template Version: 1.0.0
- Created: 2026-01-30
- MCP Compliance: 2025-11-25
- Erlang/OTP: 25+
