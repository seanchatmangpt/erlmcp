# Logger Improvements for MCP - OTP 26-28

## Overview

This document describes the structured logging implementation for erlmcp using OTP 26-28 logger improvements. The logging system provides MCP-specific structured logging with domains, metadata extraction, filtering, and compression support.

**OTP Version**: Requires OTP 26+ (features from OTP 26, 27, and 28)

## Features

### OTP 26 Features

- **Logger Domain Support**: Hierarchical log domains for categorization
- **Metadata Improvements**: Process-specific metadata propagation
- **Filter Enhancements**: Advanced log filtering capabilities

### OTP 27 Features

- **Enhanced Formatters**: Improved log message formatting with templates
- **Structured Metadata**: Better metadata handling and extraction

### OTP 28 Features

- **Log Compression**: zstd compression for rotated logs
- **Metadata Extractors**: Automatic metadata enrichment from process context

## Architecture

### Log Domains

The MCP logger uses hierarchical domains for organizing log events:

```
[mcp]           - All MCP-related events
  [tools]       - Tool execution events
  [sessions]    - Session lifecycle events
  [requests]    - Request/response events
  [errors]      - Error and failure events
  [logger]      - Logger internal events
```

### Metadata Fields

Standard metadata fields extracted automatically:

- `session_id`: MCP session identifier (binary)
- `tool_id`: Tool name/identifier (binary)
- `request_id`: JSON-RPC request ID (binary | integer)
- `client_pid`: Client process PID (pid())
- `domain`: Log domain ([atom()])
- `timestamp`: Event timestamp (milliseconds)

## Modules

### erlmcp_logger

Main logging module providing structured logging API.

#### Configuration

```erlang
% Configure logger with default settings
ok = erlmcp_logger:configure_logger().
```

Environment variables:

- `ERLMCP_LOG_LEVEL`: Set log level (debug | info | notice | warning | error)
- Application env: `erlmcp_observability.log_level`

#### API Functions

**Tool Logging**

```erlang
% Log tool call
erlmcp_logger:log_tool_call(<<"my_tool">>, #{param1 => value1}).

% Log tool result (simple)
erlmcp_logger:log_tool_result(<<"my_tool">>, {ok, Result}).

% Log tool result with duration
erlmcp_logger:log_tool_result(<<"my_tool">>, {ok, Result}, DurationMs).
```

**Session Logging**

```erlang
% Log session start
Event = #{
    event_type => session_start,
    session_id => <<"session-123">>
},
Metadata = #{client_pid => ClientPid},
erlmcp_logger:log_session_event(Event, Metadata).

% Log session end
Event = #{
    event_type => session_end,
    session_id => <<"session-123">>,
    reason => normal
},
erlmcp_logger:log_session_event(Event, Metadata).
```

**Request Logging**

```erlang
% Log request received
Event = #{
    method => <<"tools/call">>,
    request_id => 1,
    direction => request
},
Metadata = #{session_id => <<"session-123">>},
erlmcp_logger:log_request_event(Event, Metadata).

% Log response sent
Event = #{
    method => <<"tools/call">>,
    request_id => 1,
    direction => response
},
Metadata = #{duration_ms => 150},
erlmcp_logger:log_request_event(Event, Metadata).
```

**Error Logging**

```erlang
% Log error
Event = #{
    category => tool_execution,
    reason => timeout,
    stacktrace => erlang:get_stacktrace()
},
Metadata = #{tool_id => <<"failing_tool">>},
erlmcp_logger:log_error(Event, Metadata).
```

**Log Level Management**

```erlang
% Set log level
erlmcp_logger:set_log_level(debug).

% Get current log level
Level = erlmcp_logger:get_log_level().
```

**Metadata Management**

```erlang
% Add process-specific metadata
erlmcp_logger:add_metadata(session_id, <<"session-456">>).
erlmcp_logger:add_metadata(client_pid, self()).

% Get current metadata
Metadata = erlmcp_logger:get_metadata().
```

### erlmcp_log_rotation

Log rotation and compression module using OTP 28 features.

#### Configuration

```erlang
% Start with default configuration
{ok, Pid} = erlmcp_log_rotation:start_link().

% Start with custom configuration
Config = #{
    enabled => true,
    schedule => daily,           % daily | hourly | weekly
    compression => zstd,         % zstd | gzip | none
    retention_days => 30,
    log_dir => "log"
},
{ok, Pid} = erlmcp_log_rotation:start_link(Config).
```

#### API Functions

```erlang
% Trigger immediate rotation
erlmcp_log_rotation:rotate_now().

% Get rotation status
{ok, Status} = erlmcp_log_rotation:get_rotation_status().
% Status: #{
%   last_rotation => datetime(),
%   next_rotation => datetime(),
%   rotated_files => integer(),
%   compression_enabled => boolean()
% }

% Update configuration
ok = erlmcp_log_rotation:configure_rotation(#{retention_days => 14}).

% Clean up old logs manually
{ok, DeletedCount} = erlmcp_log_rotation:cleanup_old_logs().
```

## Usage Examples

### Example 1: Basic Tool Execution Logging

```erlang
% In your tool handler
-module(my_tool_handler).
-behaviour(gen_server).

handle_call({execute_tool, ToolName, Params}, From, State) ->
    % Log tool call
    erlmcp_logger:add_metadata(session_id, State#state.session_id),
    erlmcp_logger:log_tool_call(ToolName, Params),

    % Execute tool
    StartTime = erlang:monotonic_time(millisecond),
    Result = execute_tool_internal(ToolName, Params),
    Duration = erlang:monotonic_time(millisecond) - StartTime,

    % Log result
    erlmcp_logger:log_tool_result(ToolName, Result, Duration),

    {reply, Result, State}.
```

### Example 2: Session Lifecycle Logging

```erlang
% Session manager
-module(session_manager).

create_session(ClientPid) ->
    SessionId = generate_session_id(),
    erlmcp_logger:log_session_event(
        #{event_type => session_start, session_id => SessionId},
        #{client_pid => ClientPid}
    ),
    {ok, SessionId}.

terminate_session(SessionId, Reason) ->
    erlmcp_logger:log_session_event(
        #{event_type => session_end, session_id => SessionId, reason => Reason},
        #{}
    ),
    ok.
```

### Example 3: Request/Response Logging

```erlang
% In MCP server
handle_request(Req) ->
    RequestId = maps:get(<<"id">>, Req),
    Method = maps:get(<<"method">>, Req),

    % Log request
    erlmcp_logger:log_request_event(
        #{method => Method, request_id => RequestId, direction => request},
        #{client_pid => self()}
    ),

    % Process request
    {ok, Response} = process_request(Req),

    % Log response
    erlmcp_logger:log_request_event(
        #{method => Method, request_id => RequestId, direction => response},
        #{duration_ms => Duration}
    ),

    {ok, Response}.
```

### Example 4: Error Logging with Context

```erlang
% Tool execution with error handling
execute_tool_with_logging(ToolName, Params) ->
    try
        Result = do_execute_tool(ToolName, Params),
        erlmcp_logger:log_tool_result(ToolName, {ok, Result}),
        {ok, Result}
    catch
        Type:Reason:Stacktrace ->
            erlmcp_logger:log_error(
                #{category => tool_execution, reason => Reason, stacktrace => Stacktrace},
                #{tool_id => ToolName, params => Params}
            ),
            {error, Reason}
    end.
```

## Configuration

### Application Configuration

Add to `config/sys.config`:

```erlang
{erlmcp_observability, [
    {log_level, info},
    {rotation_enabled, true},
    {rotation_schedule, daily},
    {rotation_compression, zstd},
    {rotation_retention_days, 30},
    {log_dir, "log"}
]}.
```

### Environment Variables

```bash
# Set log level
export ERLMCP_LOG_LEVEL=debug

# Set log directory
export ERLMCP_LOG_DIR=/var/log/erlmcp

# Set retention days
export ERLMCP_LOG_RETENTION_DAYS=30
```

## Log Output Format

### Standard Format (with OTP 27 formatter)

```
2025-01-15T10:30:45.123+00:00 info [mcp,tools] Tool call
  Metadata: #{session_id => <<"session-123">>,tool_id => <<"my_tool">>}
```

### Format Template

The logger uses the following template (configurable):

```erlang
#{template => [time, " ", level, " [", domain, "] ", message, "\n",
               "  Metadata: ", metadata, "\n"],
  single_line => true,
  time_designator => $T,
  time_offset => "+00:00"}
```

## Performance Considerations

### Logging Overhead

- **Synchronous logging**: ~10-50 microseconds per log entry
- **Asynchronous logging**: ~1-5 microseconds per log entry
- **Metadata extraction**: ~1-2 microseconds per field

### Optimization Tips

1. **Use appropriate log levels**: Debug logging in production adds overhead
2. **Metadata extraction**: Use OTP 28 extractors for automatic enrichment
3. **Compression**: Use zstd for best compression ratio/speed tradeoff
4. **Filtering**: Apply filters early to reduce log volume

## Testing

### Unit Tests

Run EUnit tests:

```bash
rebar3 eunit --module=erlmcp_logger_tests
rebar3 eunit --module=erlmcp_log_rotation_tests
```

### Integration Tests

```erlang
% Test complete logging workflow
logger_workflow_test() ->
    % Configure logger
    ok = erlmcp_logger:configure_logger(),

    % Add metadata
    ok = erlmcp_logger:add_metadata(session_id, <<"test-session">>),

    % Log events
    ok = erlmcp_logger:log_tool_call(<<"test_tool">>, #{}),
    ok = erlmcp_logger:log_tool_result(<<"test_tool">>, {ok, result}),

    % Verify metadata
    Metadata = erlmcp_logger:get_metadata(),
    ?assertMatch(#{session_id := <<"test-session">>}, Metadata).
```

## Troubleshooting

### Logs Not Appearing

1. Check log level: `erlmcp_logger:get_log_level()`
2. Verify handler config: `logger:get_handler_config(default)`
3. Check filters: `logger:get_handler_filters(default)`

### Rotation Not Working

1. Check rotation status: `erlmcp_log_rotation:get_rotation_status()`
2. Verify schedule config
3. Check log directory permissions
4. Manually trigger: `erlmcp_log_rotation:rotate_now()`

### Compression Fails

1. Verify zstd is installed: `os:find_executable("zstd")`
2. Check file permissions
3. Try gzip as fallback: `configure_rotation(#{compression => gzip})`

## Best Practices

### 1. Use Appropriate Log Levels

- **debug**: Detailed diagnostic information
- **info**: General informational messages (default)
- **notice**: Normal but significant conditions
- **warning**: Warning messages (not errors)
- **error**: Error conditions
- **critical**: Critical conditions
- **alert**: Immediate action required
- **emergency**: System unusable

### 2. Add Contextual Metadata

```erlang
% Good: Contextual metadata
erlmcp_logger:log_error(
    #{category => timeout, reason => {tool_timeout, 5000}},
    #{tool_id => ToolName, session_id => SessionId, client_pid => Pid}
).

% Bad: No context
logger:error("Tool timeout").
```

### 3. Use Structured Data

```erlang
% Good: Structured maps
erlmcp_logger:log_tool_call(ToolName, #{
    param1 => Value1,
    param2 => Value2,
    timeout => 5000
}).

% Bad: Unstructured strings
logger:info("Tool ~p called with params ~p", [ToolName, Params]).
```

### 4. Filter by Domain

```erlang
% Enable only errors in production
erlmcp_logger:configure_handler(default, #{
    level => error,
    filters => [mcp_domain_filter]
}).
```

## Migration Guide

### From Old Logger to Structured Logger

**Before** (basic logger):

```erlang
logger:info("Tool ~p executed", [ToolName]).
```

**After** (structured logger):

```erlang
erlmcp_logger:log_tool_result(ToolName, {ok, Result}).
```

**Before** (manual metadata):

```erlang
logger:info("Session ~p for client ~p", [SessionId, ClientPid]).
```

**After** (automatic metadata):

```erlang
erlmcp_logger:add_metadata(session_id, SessionId),
erlmcp_logger:add_metadata(client_pid, ClientPid),
erlmcp_logger:log_session_event(#{event_type => session_start, session_id => SessionId}, #{}).
```

## OTP Version Compatibility

| Feature | OTP 26 | OTP 27 | OTP 28 |
|---------|--------|--------|--------|
| Domains | ✅ | ✅ | ✅ |
| Metadata | ✅ | ✅ | ✅ |
| Enhanced Formatters | ❌ | ✅ | ✅ |
| Metadata Extractors | ❌ | ❌ | ✅ |
| zstd Compression | ❌ | ❌ | ✅ |

**Minimum Version**: OTP 26 (recommended: OTP 28)

## References

- [Erlang/OTP Logger Documentation](https://www.erlang.org/doc/apps/kernel/logger)
- [Logger Filters](https://www.erlang.org/doc/apps/kernel/logger_chapter#filters)
- [Logger Handlers](https://www.erlang.org/doc/apps/kernel/logger_chapter#handlers)
- [MCP Specification](https://modelcontextprotocol.io/)

## Changelog

### Version 2.1.0 (2025-01-15)

- Initial implementation of OTP 26-28 structured logger
- MCP-specific domains and metadata
- Log rotation with compression (zstd/gzip)
- Comprehensive test coverage (EUnit)

### Future Enhancements

- [ ] JSON log output format
- [ ] Distributed log aggregation (OTLP)
- [ ] Log-based metrics extraction
- [ ] Real-time log streaming
- [ ] Machine learning log analysis
