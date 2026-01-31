# Session Persistence Integration Example

## Overview

This example demonstrates session persistence using DETS (Disk Erlang Term Storage), allowing sessions to survive server restarts.

## Features Demonstrated

1. **DETS Configuration** - Setting up disk-based session storage
2. **Session Creation** - Creating sessions with metadata and TTL
3. **Session Retrieval** - Fetching sessions from storage
4. **Session Updates** - Modifying session data
5. **Persistence Across Restarts** - Sessions survive shutdown/restart
6. **TTL Expiry** - Automatic session expiration
7. **Cleanup** - Removing expired and deleted sessions

## Prerequisites

- Erlang/OTP 25+
- rebar3 build system
- erlmcp_core application

## Running the Example

### Method 1: Using escript (Recommended)

```bash
# From the erlmcp root directory
escript examples/session_persistence/example.erl
```

### Method 2: Using rebar3 shell

```bash
# Start the Erlang shell
rebar3 shell

# Run the example
c("examples/session_persistence/example.erl").
example:main([]).
```

### Method 3: Interactive testing

```bash
# Start the Erlang shell
rebar3 shell

# Configure DETS backend
application:set_env(erlmcp_core, session_backend, dets).
application:set_env(erlmcp_core, session_file, "test_sessions.DETS").

# Start applications
application:ensure_all_started(erlmcp_core).

# Create a session
{ok, SessionId} = erlmcp_session:create(#{
    user_id => <<"test_user">>,
    auth_method => api_key
}, 3600).

% Retrieve it
{ok, Session} = erlmcp_session:retrieve(SessionId).

% List all sessions
erlmcp_session:list_sessions().

% Update session
erlmcp_session:update(SessionId, fun(S) ->
    maps:put(last_seen, erlang:system_time(second), S)
end).

% Delete session
erlmcp_session:delete(SessionId).
```

## Expected Output

```
=== Session Persistence Integration Example ===

Step 1: Configuring DETS backend...
✓ DETS backend configured: examples/session_persistence/sessions.DETS

Step 2: Starting erlmcp applications...
✓ Applications started

Step 3: Creating sessions with metadata...
✓ Created session 1: a1b2c3d4e5f6...
✓ Created session 2: f6e5d4c3b2a1...
✓ Total sessions: 2

Step 4: Retrieving and updating sessions...
✓ Retrieved session 1: <<"user123">>
✓ Updated session 1 with last_activity

Step 5: Verifying DETS file...
✓ DETS file created: examples/session_persistence/sessions.DETS (2048 bytes)

Step 6: Stopping applications (simulating shutdown)...
✓ Applications stopped
✓ Sessions should be persisted to disk

Step 7: Restarting applications...
✓ Applications restarted

Step 8: Restoring sessions from disk...
✓ Restored 2 sessions
  - Session: a1b2c3d4... (User: user123)
  - Session: f6e5d4c3... (User: user456)

Step 9: Verifying restored session data...
✓ Session 1 restored successfully
  User ID: user123
  Theme: dark
  Language: en

Step 10: Demonstrating session TTL...
✓ Created short-lived session: x9y8z7w6... (TTL: 2s)
✓ Session expired and removed from DETS

Step 11: Cleaning up expired sessions...
✓ Cleaned up 0 expired sessions

Step 12: Deleting a session...
✓ Deleted session: f6e5d4c3...
✓ Remaining sessions: 1

Step 13: Checking backend configuration...
✓ Current backend: dets

Step 14: Final cleanup...
✓ Cleanup complete

=== Example Complete ===
```

## Key Concepts

### Session Backend Options

erlmcp supports multiple session backends:

| Backend | Description | Persistence | Performance | Use Case |
|---------|-------------|-------------|-------------|----------|
| **ets** | In-memory (default) | No | Fastest | Development, testing |
| **dets** | Disk-based | Yes | Medium | Production (single node) |
| **leveldb** | Key-value store | Yes | Fast | Production (large datasets) |
| **mnesia** | Distributed DB | Yes | Medium | Production (clustered) |

### DETS Configuration

```erlang
% In sys.config or application:set_env
{erlmcp_core, [
    {session_backend, dets},
    {session_file, "path/to/sessions.DETS"}
]}
```

### Session Lifecycle

```
1. create(Metadata, TTL)
   - Generate unique session ID
   - Store in DETS with expiry timestamp
   - Return session ID

2. retrieve(SessionId)
   - Read from DETS
   - Check expiry
   - Return session data

3. update(SessionId, UpdateFun)
   - Read from DETS
   - Apply transformation
   - Write back to DETS

4. delete(SessionId)
   - Remove from DETS
   - Free disk space

5. cleanup_expired()
   - Scan all sessions
   - Remove expired entries
   - Return count cleaned
```

### Session Data Structure

```erlang
#{
    id => <<"a1b2c3d4e5f6...">>,           % Unique session ID
    created_at => 1706630400000,           % Creation timestamp (ms)
    last_accessed => 1706630405000,        % Last access (ms)
    timeout_ms => 3600000,                 % TTL (ms) or infinity
    metadata => #{
        user_id => <<"user123">>,
        auth_method => api_key,
        % ... custom metadata
    }
}
```

## Advanced Usage

### Automatic Session Cleanup

```erlang
% Schedule periodic cleanup (every 5 minutes)
timer:apply_interval(300000, erlmcp_session, cleanup_expired, []).
```

### Session Replication (Mnesia)

```erlang
% Configure Mnesia for multi-node replication
application:set_env(erlmcp_core, session_backend, mnesia).
application:set_env(erlmcp_core, mnesia_tables, [
    {erlmcp_sessions, [{disc_copies, [node()]}]}
]).
```

### Custom Session ID Generation

```erlang
% Use custom session ID format
SessionId = iolist_to_binary([
    "user_",
    integer_to_binary(erlang:unique_integer([positive])),
    "_",
    base64:encode(crypto:strong_rand_bytes(16))
]).
```

## Troubleshooting

### "DETS file already exists"

**Solution**: DETS automatically opens existing files. To start fresh:
```bash
rm examples/session_persistence/sessions.DETS
```

**Or programmatically**:
```erlang
dets:delete_all_objects(session_table).
```

### "Session not found after restart"

**Possible causes**:
1. DETS file path changed between runs
2. DETS file corrupted
3. Incorrect backend configuration

**Debug**:
```erlang
% Check backend
{ok, Backend} = erlmcp_session:get_backend().

% Check DETS file exists
file:read_file_info("path/to/sessions.DETS").

% List all sessions
erlmcp_session:list_sessions().
```

### "Performance issues with DETS"

**DETS is slower than ETS**: This is expected. Consider:
1. Using ETS for development/testing
2. Using LevelDB for production (better performance)
3. Implementing write-through cache

```erlang
% Switch to LevelDB (requires luerl dependency)
application:set_env(erlmcp_core, session_backend, leveldb).
application:set_env(erlmcp_core, leveldb_path, "/var/lib/erlmcp/sessions").
```

## DETS File Management

### Backup Sessions

```bash
# Copy DETS file (while server stopped)
cp examples/session_persistence/sessions.DETS \
   examples/session_persistence/sessions.DETS.backup
```

### Inspect DETS Contents

```erlang
% Open DETS file directly
{ok, Name} = dets:open_file(session_file, [{file, "sessions.DETS"}]).
dets:traverse(Name, fun({K, V}) -> io:format("~p: ~p~n", [K, V]), continue end).
dets:close(Name).
```

### Repair Corrupted DETS

```bash
# DETS auto-repairs on open, but manual repair:
erl -eval "dets:repair('sessions.DETS'), init:stop()."
```

## Integration with MCP Protocol

Sessions in MCP are used for:

- **Request Context**: Track conversation state
- **Authentication**: Store user credentials/tokens
- **Rate Limiting**: Track usage per session
- **Preferences**: User-specific settings
- **Resource Subscriptions**: Track active subscriptions

## Files

- `example.erl` - Main example script
- `README.md` - This file
- `sessions.DETS` - DETS storage file (created at runtime)

## See Also

- [Session Module](../../../apps/erlmcp_core/src/erlmcp_session.erl)
- [DETS Backend Module](../../../apps/erlmcp_core/src/erlmcp_session_dets.erl)
- [Session Manager](../../../apps/erlmcp_core/src/erlmcp_session_manager.erl)
- [Erlang DETS Documentation](https://www.erlang.org/doc/man/dets.html)
