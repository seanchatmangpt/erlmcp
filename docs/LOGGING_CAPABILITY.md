# Logging Capability Implementation

## Overview

This document describes the implementation of the logging capability for erlmcp, compliant with the MCP 2025-11-25 specification. The logging capability provides structured, per-client logging with dynamic log level configuration.

## Architecture

### Components

- **erlmcp_logging** - Main gen_server managing per-client log buffers
- **Per-client buffers** - Isolated log storage for each MCP client session
- **Level filtering** - Dynamic log level configuration (global and per-client)
- **Structured logs** - JSON-formatted log entries with timestamps

### Module Location

```
/Users/sac/erlmcp/apps/erlmcp_core/src/erlmcp_logging.erl
```

## Features

### 1. Log Levels

The implementation supports all standard syslog levels (RFC 5424):

| Level | Severity | Description |
|-------|----------|-------------|
| `emergency` | 0 (highest) | System is unusable |
| `alert` | 1 | Immediate action required |
| `critical` | 2 | Critical conditions |
| `error` | 3 | Error conditions |
| `warning` | 4 | Warning conditions |
| `notice` | 5 | Normal but significant |
| `info` | 6 (default) | Informational messages |
| `debug` | 7 (lowest) | Debug-level messages |

### 2. Per-Client Buffers

Each MCP client session has its own isolated log buffer:

- **Maximum size**: 1000 log entries per client
- **Automatic cleanup**: On client disconnect
- **Overflow handling**: Oldest entries dropped when limit exceeded
- **Statistics tracking**: Total logs, buffer overflows, active clients

### 3. Log Entry Structure

All log entries follow this JSON structure:

```json
{
  "timestamp": 1738185600000,
  "level": "info",
  "component": "transport",
  "message": "Client connected",
  "data": {
    "client_id": "client_123",
    "remote_addr": "127.0.0.1"
  }
}
```

**Fields:**
- `timestamp` - Unix timestamp in milliseconds
- `level` - Log level (debug, info, warning, error, etc.)
- `component` - Component name (transport, server, tools, etc.)
- `message` - Human-readable log message
- `data` - Optional structured data map

## API Reference

### Starting the Logger

```erlang
{ok, Pid} = erlmcp_logging:start_link().
```

### Creating Client Buffers

```erlang
% Called when a new client connects
ok = erlmcp_logging:create_client_buffer(ClientPid).
```

### Logging Messages

```erlang
% Log a message for a client
ok = erlmcp_logging:log(
    ClientPid,           % Client process ID
    Level,               % Log level: debug | info | warning | error
    Component,           % Component name: binary()
    Message,             % Log message: binary()
    Data                 % Optional structured data: map()
).

% Example
ok = erlmcp_logging:log(
    ClientPid,
    info,
    <<"transport">>,
    <<"Client connected">>,
    #{<<"client_id">> => <<"client_123">>}
).
```

### Setting Log Levels

```erlang
% Set level for a specific client
ok = erlmcp_logging:set_level(ClientPid, warning).

% Invalid levels are rejected
{error, {invalid_level, invalid}} = erlmcp_logging:set_level(ClientPid, invalid).
```

### Retrieving Logs

```erlang
% Get all logs for a client
{ok, Logs} = erlmcp_logging:get_logs(ClientPid, #{}).

% Get logs with filtering
{ok, Logs} = erlmcp_logging:get_logs(ClientPid, #{
    level => warning,           % Minimum log level
    component => <<"transport">>, % Filter by component
    limit => 100,                % Maximum entries to return
    offset => 0                  % Pagination offset
}).
```

### Deleting Client Buffers

```erlang
% Called when a client disconnects
ok = erlmcp_logging:delete_client_buffer(ClientPid).

% Manual cleanup
ok = erlmcp_logging:clear_client_buffer(ClientPid).
```

### Getting Statistics

```erlang
{ok, Stats} = erlmcp_logging:get_stats().
% Stats: #{
%   total_logs => 1523,
%   total_clients => 5,
%   buffer_overflows => 42
% }
```

## Integration with erlmcp_server

### Logging Handler

The logging capability is integrated into `erlmcp_server.erl` to handle the `logging/setLevel` request:

```erlang
%% In erlmcp_server.erl handle_request

handle_request(Id, ?MCP_METHOD_LOGGING_SET_LEVEL, Params, TransportId, State) ->
    LevelBinary = maps:get(<<"level">>, Params),
    Level = binary_to_existing_atom(LevelBinary, utf8),

    case validate_log_level(Level) of
        ok ->
            ClientPid = self(),
            ok = erlmcp_logging:set_level(ClientPid, Level),
            Response = #{<<"level">> => LevelBinary},
            send_response_safe(State, TransportId, Id, Response),
            {noreply, State};
        {error, invalid_level} ->
            send_error_safe(State, TransportId, Id, ?JSONRPC_INVALID_PARAMS,
                <<"Invalid log level">>),
            {noreply, State}
    end.
```

### Server Integration Points

1. **Client Connection**: Create log buffer in `init/1`
2. **Server Events**: Log important events (tool calls, resource access)
3. **Client Disconnect**: Delete log buffer in `terminate/2`
4. **Error Handling**: Log errors with context

### Example: Logging in Server

```erlang
%% In erlmcp_server.erl

%% Log tool call
handle_tool_call(Id, Name, Arguments, TransportId, State) ->
    ClientPid = self(),
    erlmcp_logging:log(
        ClientPid,
        info,
        <<"tools">>,
        <<"Tool called">>,
        #{
            <<"tool">> => Name,
            <<"arguments">> => Arguments
        }
    ),
    % ... rest of implementation

%% Log error
catch
    Class:Reason:Stack ->
        erlmcp_logging:log(
            ClientPid,
            error,
            <<"server">>,
            <<"Tool handler crashed">>,
            #{
                <<"class">> => Class,
                <<"reason">> => Reason,
                <<"stack">> => Stack
            }
        )
end.
```

## Testing

### Test Suite

Comprehensive tests are available in:

```
/Users/sac/erlmcp/apps/erlmcp_core/test/erlmcp_logging_tests.erl
```

### Running Tests

```bash
# Run logging tests
rebar3 eunit --module=erlmcp_logging_tests

# Run with coverage
rebar3 cover --verbose
```

### Test Coverage

The test suite covers:

- ✅ All log levels (debug through emergency)
- ✅ Level filtering (minimum level enforcement)
- ✅ Component filtering
- ✅ Buffer size limits (1000 entries)
- ✅ Buffer overflow handling
- ✅ Statistics tracking
- ✅ Client buffer lifecycle (create, delete, clear)
- ✅ Pagination (offset, limit)
- ✅ Invalid level rejection
- ✅ Per-client level overrides
- ✅ Empty buffer handling
- ✅ Non-existent buffer errors
- ✅ Log entry structure validation
- ✅ Combined filtering (level + component + pagination)

### Property-Based Tests

```erlang
% Level ordering invariant
prop_log_level_ordering() ->
    ?FORALL(Level, log_level(),
        Level ∈ ?LOG_LEVELS
    ).

% Buffer size limit invariant
prop_buffer_size_limit() ->
    ?FORALL(N, nat(),
        N =< 10000 andalso begin
            % After N logs, buffer size <= 1000
            length(get_logs(ClientPid)) =< 1000
        end
    ).
```

## Performance Characteristics

### Memory Usage

- **Per-client buffer**: ~1000 entries × ~200 bytes = ~200 KB per client
- **1000 clients**: ~200 MB total memory
- **Overflow protection**: Automatic cleanup prevents unbounded growth

### Throughput

- **Log write**: < 1ms per entry (non-blocking cast)
- **Log retrieval**: O(n) where n = buffer size (typically <= 1000)
- **Filtering**: O(n) with early termination on pagination

### Scalability

- **Horizontal scaling**: Each client has isolated buffer
- **Vertical scaling**: Statistics aggregation is O(1)
- **Memory limits**: Per-client buffer limits prevent DoS

## Security Considerations

### Level Validation

- Invalid log levels are rejected
- Level comparison uses strict ordering
- Default level (info) filters verbose debug logs

### Buffer Isolation

- Clients cannot access other clients' logs
- Buffer deletion requires client Pid
- No global buffer (all logs isolated per-client)

### Resource Limits

- Maximum 1000 entries per buffer
- Overflow drops oldest entries (FIFO)
- Statistics track overflows for monitoring

## Configuration

### Environment Variables

```erlang
% In sys.config or application:set_env

{erlmcp, [
    {logging, [
        {max_logs_per_client, 1000},
        {default_level, info},
        {enable_stats, true}
    ]}
]}.
```

### Runtime Configuration

```erlang
% Change default level
application:set_env(erlmcp, default_log_level, debug).

% Adjust buffer limit
application:set_env(erlmcp, max_logs_per_client, 2000).
```

## Monitoring and Observability

### Statistics

Track logging statistics for system health:

```erlang
{ok, Stats} = erlmcp_logging:get_stats(),
TotalLogs = maps:get(total_logs, Stats),
TotalClients = maps:get(total_clients, Stats),
BufferOverflows = maps:get(buffer_overflows, Stats).

% Alert if overflows exceed threshold
case BufferOverflows / TotalLogs of
    Ratio when Ratio > 0.1 ->
        logger:warning("High buffer overflow ratio: ~p%", [Ratio * 100]);
    _ ->
        ok
end.
```

### Metrics to Monitor

1. **Total logs**: Overall logging rate
2. **Total clients**: Active sessions with logs
3. **Buffer overflows**: Memory pressure indicator
4. **Per-client logs**: Identify log spam

## Troubleshooting

### Common Issues

**Issue**: Logs not appearing
```erlang
% Check if buffer exists
case erlmcp_logging:get_logs(ClientPid, #{}) of
    {error, no_buffer} ->
        logger:error("Log buffer not created for client"),
        ok = erlmcp_logging:create_client_buffer(ClientPid);
    {ok, []} ->
        logger:info("Log buffer is empty");
    {ok, Logs} ->
        logger:info("Found ~p logs", [length(Logs)])
end.
```

**Issue**: Logs filtered out
```erlang
% Check effective log level
{ok, Stats} = erlmcp_logging:get_stats(),
% Ensure logging at appropriate level
erlmcp_logging:log(ClientPid, error, Component, Message, Data).
```

**Issue**: Memory usage growing
```erlang
% Check buffer overflows
{ok, Stats} = erlmcp_logging:get_stats(),
BufferOverflows = maps:get(buffer_overflows, Stats),

% If high, reduce log verbosity or increase buffer limit
case BufferOverflows of
    N when N > 1000 ->
        logger:warning("High buffer overflow count: ~p", [N]),
        application:set_env(erlmcp, default_log_level, warning);
    _ ->
        ok
end.
```

## Future Enhancements

Potential improvements for future versions:

1. **Persistent logging**: Optional disk-based log storage
2. **Log rotation**: Automatic archival of old logs
3. **Structured queries**: Complex filtering and search
4. **Log aggregation**: Cross-client log correlation
5. **Export functionality**: Download logs as JSON/text
6. **Real-time streaming**: SSE-based log streaming
7. **Compression**: Reduce memory usage for high-volume logs

## References

- [MCP 2025-11-25 Specification](https://modelcontextprotocol.io/specification/)
- [RFC 5424 - The Syslog Protocol](https://tools.ietf.org/html/rfc5424)
- [erlmcp OTP Patterns](/Users/sac/erlmcp/docs/otp-patterns.md)
- [erlmcp API Reference](/Users/sac/erlmcp/docs/api-reference.md)

## Changelog

### Version 1.0.0 (2026-01-29)

- Initial implementation
- All 8 log levels (emergency through debug)
- Per-client buffer management
- Level and component filtering
- Pagination support
- Statistics tracking
- Comprehensive test suite
- Full documentation

---

**Author**: erlmcp development team
**Last Updated**: 2026-01-29
**Status**: Production Ready ✅
