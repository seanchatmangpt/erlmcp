# erlmcp Migration Guide

## Overview

This guide helps you migrate between versions of erlmcp. Each section covers breaking changes, new features, and step-by-step migration instructions.

---

## Version History

| Version | Release Date | Status | Key Changes |
|---------|--------------|--------|-------------|
| 3.0.0 | 2025-01 | Current | MCP 2025-11-25 spec, new features |
| 2.1.0 | 2024-11 | Previous | Transport improvements |
| 2.0.0 | 2024-09 | Legacy | Umbrella restructuring |

---

## Migrating from 2.x to 3.0.0

### Breaking Changes

1. **Protocol Version Update**
   - Old: `2024-11-05`
   - New: `2025-11-25`
   - Impact: All initialize requests must use new version

2. **Roots Management (New)**
   - Added `roots/add` and `roots/remove` methods
   - `roots/list_changed` notification added

3. **Completion API (New)**
   - Added `completion/complete` method
   - Prompt and resource completion now supported

4. **Error Code Changes**
   - New error codes: -32110 to -32113 (completion errors)
   - Experimental error codes: 1090 to 1099

### Migration Steps

#### Step 1: Update Protocol Version

**Before (2.x):**
```erlang
#{protocolVersion => <<"2024-11-05">>, ...}
```

**After (3.0):**
```erlang
#{protocolVersion => <<"2025-11-25">>, ...}
```

#### Step 2: Update Initialize Request

```erlang
%% Add new capabilities if needed
InitializeParams = #{
    protocolVersion => <<"2025-11-25">>,
    capabilities => #{
        roots => #{},  %% New in 3.0
        sampling => #{
            modelPreferences => #{
                costPriority => 0.5,
                speedPriority => 0.3,
                intelligencePriority => 0.2
            }
        }
    },
    clientInfo => #{
        name => <<"my-client">>,
        version => <<"3.0.0">>
    }
}.
```

#### Step 3: Update Tool Registration

```erlang
%% Tool descriptions now have max length of 10,000 characters
%% Add version and deprecated fields
Tool = #mcp_tool{
    name = <<"my_tool">>,
    description = <<"Tool description">>,  %% Max 10,000 chars
    input_schema => Schema,
    metadata => #{},
    version => <<"1.0.0">>,  %% New
    deprecated = false  %% New
}.
```

#### Step 4: Update Resource Handling

```erlang
%% Resources now support annotations
Resource = #mcp_resource{
    uri = <<"mcp://data">>,
    name = <<"Data">>,
    annotations = #{  %% New
        <<"audience">> => <<"user">>,
        <<"priority">> => 1
    }
}.
```

#### Step 5: Update Error Handling

```erlang
%% Handle new error codes
handle_error(#{<<"code">> := Code} = Error) when Code >= -32113, Code =< -32110 ->
    %% Completion errors
    handle_completion_error(Error);
handle_error(Other) ->
    handle_old_error(Other).
```

### Configuration Changes

**Before (2.x):**
```erlang
{erlmcp_core, [
    {protocol_version, <<"2024-11-05">>},
    {capabilities, #{
        resources => true,
        tools => true,
        prompts => true
    }}
]}.
```

**After (3.0):**
```erlang
{erlmcp_core, [
    {protocol_version, <<"2025-11-25">>},
    {capabilities, #mcp_server_capabilities{
        resources = #mcp_resources_capability{
            subscribe = true,
            listChanged = true
        },
        tools = #mcp_tools_capability{
            listChanged = true
        },
        prompts = #mcp_prompts_capability{
            listChanged = true
        },
        roots = #mcp_roots_capability{  %% New
            list_changed = true
        },
        completions = #mcp_capability{}  %% New
    }}
]}.
```

---

## Migrating from 1.x to 2.0.0

### Breaking Changes

1. **Umbrella Project Structure**
   - Applications split into: `erlmcp_core`, `erlmcp_transports`, `erlmcp_observability`
   - Include paths changed

2. **Transport API Changes**
   - Transports now use behavior pattern
   - `erlmcp_transport` behavior required

3. **Error Code Standardization**
   - All errors now use JSON-RPC 2.0 codes
   - Custom error codes moved to -32000 range

### Migration Steps

#### Step 1: Update Dependencies

**Before (1.x):**
```erlang
{deps, [
    {erlmcp, {git, "https://github.com/erlmcp/erlmcp", {tag, "1.0.0"}}}
]}.
```

**After (2.0):**
```erlang
{deps, [
    {erlmcp_core, {git, "https://github.com/erlmcp/erlmcp", {branch, "main"}}},
    {erlmcp_transports, {git, "https://github.com/erlmcp/erlmcp", {branch, "main"}}},
    {erlmcp_observability, {git, "https://github.com/erlmcp/erlmcp", {branch, "main"}}}
]}.
```

#### Step 2: Update Include Paths

**Before (1.x):**
```erlang
-include_lib("erlmcp/include/erlmcp.hrl").
```

**After (2.0):**
```erlang
-include_lib("erlmcp_core/include/erlmcp.hrl").
```

#### Step 3: Update Transport Usage

**Before (1.x):**
```erlang
{ok, Server} = erlmcp_server:start_link(my_server).
erlmcp_server:start_stdio(Server).
```

**After (2.0):**
```erlang
{ok, Server} = erlmcp_server:start_link(my_server, Capabilities).
{ok, Transport} = erlmcp_transport_stdio:start_link(Server).
```

#### Step 4: Update Error Handling

**Before (1.x):**
```erlang
case Result of
    {error, custom_error} -> handle_custom_error();
    _ -> ok
end.
```

**After (2.0):**
```erlang
case Result of
    {error, #{<<"code">> := -32000}} -> handle_mcp_error();
    _ -> ok
end.
```

---

## Upgrading Procedure

### Pre-Upgrade Checklist

1. **Backup Configuration**
   ```bash
   cp sys.config sys.config.backup
   cp -r priv/secrets priv/secrets.backup
   ```

2. **Check Current Version**
   ```bash
   grep erlmcp rebar.config
   ```

3. **Review Breaking Changes**
   - Read migration guide for target version
   - Identify affected components

### Upgrade Steps

#### 1. Update Dependencies

```bash
# Update rebar.config
vim rebar.config

# Fetch new dependencies
rebar3 get-deps
```

#### 2. Compile

```bash
rebar3 compile
```

#### 3. Run Tests

```bash
rebar3 eunit
rebar3 ct
```

#### 4. Update Configuration

```bash
# Update sys.config with new options
vim sys.config
```

#### 5. Perform Rolling Upgrade (Production)

```erlang
%% On node 1
release_handler:unpack_release("erlmcp_3.0.0.tar.gz").
{ok, Vsn} = release_handler:install_release("3.0.0", [{abort_if_error, true}]).

%% Verify node 1 is healthy
erlmcp_health:check().

%% Repeat on remaining nodes
```

#### 6. Verify Upgrade

```bash
# Check version
curl http://localhost:8765/health

# Run smoke tests
rebar3 ct --suite=test/smoke_tests_SUITE
```

### Post-Upgrade Checklist

- [ ] All nodes running new version
- [ ] Health checks passing
- [ ] Tests passing
- [ ] Metrics normal
- [ ] No error spikes in logs
- [ ] Performance baseline maintained

---

## Rollback Procedure

If upgrade fails, follow these steps:

### 1. Identify Failure

```bash
# Check logs
tail -f /var/log/erlmcp/error.log

# Check health
curl http://localhost:8765/health
```

### 2. Rollback to Previous Version

```erlang
%% Downgrade to previous release
{ok, OldVsn} = release_handler:install_release("2.1.0", [{abort_if_error, true}]).

%% Make permanent
release_handler:make_permanent("2.1.0").
```

### 3. Restore Configuration

```bash
cp sys.config.backup sys.config
cp -r priv/secrets.backup priv/secrets
```

### 4. Restart

```bash
# Restart service
systemctl restart erlmcp

# Or for releases
bin/erlmcp restart
```

---

## Data Migration

### ETS Table Migration

```erlang
%% Export data from old version
export_ets_data(Table) ->
    ets:foldl(fun({Key, Value}, Acc) ->
        [{Key, Value} | Acc]
    end, [], Table).

%% Import data into new version
import_ets_data(Data, Table) ->
    lists:foreach(fun({Key, Value}) ->
        ets:insert(Table, {Key, Value})
    end, Data).
```

### Secrets Migration

```erlang
%% Re-encrypt secrets with new key
migrate_secrets(OldKey, NewKey) ->
    {ok, Secrets} = erlmcp_secrets:list_secrets(),
    lists:foreach(fun(SecretName) ->
        {ok, Value} = erlmcp_secrets:get_secret(SecretName),
        ok = erlmcp_secrets:set_secret(SecretName, Value),
        ok = erlmcp_secrets:reencrypt(SecretName, OldKey, NewKey)
    end, Secrets).
```

### Session Migration

```erlang
%% Migrate sessions to new backend
migrate_sessions(OldBackend, NewBackend) ->
    {ok, Sessions} = erlmcp_session_backend:list_sessions(OldBackend),
    lists:foreach(fun(SessionId) ->
        {ok, Session} = erlmcp_session_backend:get_session(OldBackend, SessionId),
        ok = erlmcp_session_backend:create_session(NewBackend, SessionId, Session)
    end, Sessions).
```

---

## Compatibility Matrix

| Feature | 1.x | 2.0 | 2.1 | 3.0 |
|---------|-----|-----|-----|-----|
| stdio transport | Yes | Yes | Yes | Yes |
| TCP transport | Yes | Yes | Yes | Yes |
| HTTP transport | Yes | Yes | Yes | Yes |
| WebSocket | No | Yes | Yes | Yes |
| SSE | No | No | Yes | Yes |
| mTLS | Basic | Basic | Full | Full |
| JWT auth | No | Yes | Yes | Yes |
| Roots | No | No | No | Yes |
| Completion | No | No | No | Yes |
| Tasks | Basic | Yes | Yes | Yes |

---

## Deprecation Notice

### Deprecated in 3.0

The following features are deprecated and will be removed in 4.0:

1. **Old record definitions**
   - Use new record definitions with version fields

2. **Direct transport process spawning**
   - Use `erlmcp_transport_*:start_link/1` instead

3. **Legacy error atoms**
   - Use JSON-RPC error codes instead

### Removal Schedule

| Deprecated | Removed | Replacement |
|------------|---------|-------------|
| Direct spawn | 4.0 | start_link/1 |
| Error atoms | 4.0 | Error codes |
| Old records | 4.0 | New records |

---

## Troubleshooting

### Common Issues

#### Issue: Version Mismatch

**Error:** `{"code": -32061, "message": "Unsupported protocol version"}`

**Solution:**
```erlang
%% Update client to use correct protocol version
#{protocolVersion => <<"2025-11-25">>}
```

#### Issue: Capability Not Supported

**Error:** `{"code": -32004, "message": "Capability not supported"}`

**Solution:**
```erlang
%% Check server capabilities before using feature
case maps:get(<<"completions">>, ServerCapabilities) of
    undefined -> logger:warning("Completions not supported");
    _ -> use_completion()
end.
```

#### Issue: Tool Description Too Long

**Error:** `{"code": -32011, "message": "Tool description exceeds maximum length"}`

**Solution:**
```erlang
%% Truncate or shorten description
ShortDesc = binary:part(LongDesc, 0, 10000),
Tool = #mcp_tool{description = ShortDesc}.
```

### Getting Help

1. Check documentation: `/Users/sac/erlmcp/docs/`
2. Review examples: `/Users/sac/erlmcp/examples/`
3. File issue: https://github.com/erlmcp/erlmcp/issues
4. Contact support: support@erlmcp.org

---

## Upgrade Testing

### Automated Upgrade Tests

```erlang
%% Test upgrade path
upgrade_test() ->
    %% Start old version
    {ok, OldNode} = start_old_version(),

    %% Perform upgrade
    {ok, NewNode} = upgrade_to_new_version(OldNode),

    %% Verify functionality
    ok = test_resources(NewNode),
    ok = test_tools(NewNode),
    ok = test_prompts(NewNode),

    %% Cleanup
    stop_node(NewNode).
```

### Smoke Tests

```bash
#!/bin/bash
# smoke_test.sh

echo "Testing resources..."
curl -X POST http://localhost:8765/mcp \
  -d '{"jsonrpc":"2.0","id":1,"method":"resources/list"}'

echo "Testing tools..."
curl -X POST http://localhost:8765/mcp \
  -d '{"jsonrpc":"2.0","id":2,"method":"tools/list"}'

echo "Upgrade smoke test complete!"
```
