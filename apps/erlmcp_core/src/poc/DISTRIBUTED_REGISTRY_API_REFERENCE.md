# Distributed Registry POC - API Reference Card

## Quick API Reference

### Registration API

```erlang
%% Register a process globally
ok | {error, Reason} = erlmcp_distributed_registry_poc:register(Name, Pid).
%% Name: atom()
%% Pid: pid()
%% Returns: ok | {error, {already_registered, ExistingPid}}

%% Lookup a registered process
Pid | undefined = erlmcp_distributed_registry_poc:whereis(Name).
%% Name: atom()
%% Returns: pid() | undefined

%% Unregister a process
ok = erlmcp_distributed_registry_poc:unregister(Name).
%% Name: atom()

%% Get all registered names
Names = erlmcp_distributed_registry_poc:registered().
%% Returns: [atom()]
```

### Group API

```erlang
%% Join a process group
ok = erlmcp_distributed_registry_poc:join_group(Group, Pid).
%% Group: atom() (e.g., mcp_tool_servers, mcp_resource_servers)
%% Pid: pid()

%% Leave a process group
ok = erlmcp_distributed_registry_poc:leave_group(Group, Pid).
%% Group: atom()
%% Pid: pid()

%% Get all members of a group
Members = erlmcp_distributed_registry_poc:get_group_members(Group).
%% Group: atom()
%% Returns: [pid()]

%% Get all groups
Groups = erlmcp_distributed_registry_poc:get_all_groups().
%% Returns: [atom()]
```

### High-Level MCP Server API

```erlang
%% Start an MCP server (auto-registers and joins group)
{ok, Pid} | {error, Reason} = 
    erlmcp_distributed_registry_poc:start_mcp_server(Type, Name).
%% Type: tool | resource | prompt
%% Name: atom()
%% Automatically registers globally and joins type-specific group

%% Stop an MCP server (auto-unregisters and leaves groups)
ok = erlmcp_distributed_registry_poc:stop_mcp_server(Name).
%% Name: atom()

%% List all servers with their groups
Servers = erlmcp_distributed_registry_poc:list_servers().
%% Returns: [{Name :: atom(), Pid :: pid(), Groups :: [atom()]}]
```

### Demo & Testing API

```erlang
%% Run complete demonstration
ok = erlmcp_distributed_registry_poc:run_demo().
%% Shows all features: registration, lookup, groups, failover, benchmarks

%% Run performance benchmark
ok = erlmcp_distributed_registry_poc:benchmark_vs_gproc().
%% Compares global+pg vs gproc performance

%% Simulate node failure
ok = erlmcp_distributed_registry_poc:simulate_node_failure(Name).
%% Name: atom()
%% Kills process and demonstrates automatic cleanup
```

### Process Groups (Built-in)

```erlang
%% Defined groups
-define(GROUP_TOOL_SERVERS, mcp_tool_servers).
-define(GROUP_RESOURCE_SERVERS, mcp_resource_servers).
-define(GROUP_PROMPT_SERVERS, mcp_prompt_servers).
-define(GROUP_ALL_SERVERS, mcp_all_servers).
```

## Usage Examples

### Example 1: Basic Registration

```erlang
%% Start a process
Pid = spawn(fun() -> my_server_loop() end).

%% Register it
ok = erlmcp_distributed_registry_poc:register(my_server, Pid).

%% Lookup from anywhere (even other nodes!)
Pid = erlmcp_distributed_registry_poc:whereis(my_server).

%% Use it
gen_server:call(Pid, {do_something}).

%% Unregister when done
ok = erlmcp_distributed_registry_poc:unregister(my_server).
```

### Example 2: Group-Based Server Discovery

```erlang
%% Start multiple tool servers
{ok, _} = erlmcp_distributed_registry_poc:start_mcp_server(tool, calculator).
{ok, _} = erlmcp_distributed_registry_poc:start_mcp_server(tool, weather).
{ok, _} = erlmcp_distributed_registry_poc:start_mcp_server(tool, search).

%% Get all tool servers
ToolServers = erlmcp_distributed_registry_poc:get_group_members(mcp_tool_servers).
%% => [<0.123.0>, <0.124.0>, <0.125.0>]

%% Round-robin selection
Server = lists:nth(RequestId rem length(ToolServers) + 1, ToolServers).

%% Call the selected server
Result = gen_server:call(Server, {handle_request, Data}).
```

### Example 3: Cross-Node Communication

```erlang
%% On node1@localhost
{ok, Pid1} = erlmcp_distributed_registry_poc:start_mcp_server(tool, calculator).
%% Pid1 is on node1

%% On node2@localhost
Pid1 = erlmcp_distributed_registry_poc:whereis(calculator).
%% Found! Even though it's on node1
node(Pid1).  %% => 'node1@localhost'

%% Call it directly (works cross-node!)
{ok, 8} = gen_server:call(Pid1, {calculate, add, [5, 3]}).
```

### Example 4: Failover Demonstration

```erlang
%% Node1: Start primary server
{ok, Pid1} = erlmcp_distributed_registry_poc:start_mcp_server(tool, important_service).

%% Clients use the service
Pid1 = erlmcp_distributed_registry_poc:whereis(important_service).
Result = gen_server:call(Pid1, {do_work}).

%% Node1 crashes or process dies
exit(Pid1, kill).
timer:sleep(100).

%% Service is gone
undefined = erlmcp_distributed_registry_poc:whereis(important_service).

%% Node2: Start backup server (automatic failover)
{ok, Pid2} = erlmcp_distributed_registry_poc:start_mcp_server(tool, important_service).

%% Clients automatically find new server
Pid2 = erlmcp_distributed_registry_poc:whereis(important_service).
Result = gen_server:call(Pid2, {do_work}).
%% Service restored!
```

### Example 5: Duplicate Prevention

```erlang
%% Node1: Register server
{ok, Pid1} = erlmcp_distributed_registry_poc:start_mcp_server(tool, unique_service).

%% Node2: Try to register duplicate (prevented)
{ok, Pid2} = mcp_server_demo:start_link(tool, unique_service),
{error, {already_registered, Pid1}} = 
    erlmcp_distributed_registry_poc:register(unique_service, Pid2).
%% Registration rejected! Pid1 still owns the name
```

## Underlying Erlang/OTP APIs

This POC wraps these standard Erlang modules:

### global module

```erlang
%% Register globally
yes | no = global:register_name(Name, Pid).

%% Lookup
Pid | undefined = global:whereis_name(Name).

%% Unregister
_ = global:unregister_name(Name).

%% List all
Names = global:registered_names().

%% Synchronize (after network partition)
ok = global:sync().
```

### pg module

```erlang
%% Start pg
{ok, Pid} = pg:start_link(Scope).

%% Join group
ok = pg:join(Scope, Group, Pid).

%% Leave group
ok = pg:leave(Scope, Group, Pid).

%% Get members
[Pid] = pg:get_members(Scope, Group).

%% List groups
[Group] = pg:which_groups(Scope).
```

## Error Handling

```erlang
%% Registration errors
case erlmcp_distributed_registry_poc:register(Name, Pid) of
    ok -> 
        %% Success
        ok;
    {error, {already_registered, ExistingPid}} ->
        %% Name already taken
        io:format("Already registered: ~p~n", [ExistingPid])
end.

%% Lookup (may return undefined)
case erlmcp_distributed_registry_poc:whereis(Name) of
    undefined -> 
        %% Not found
        {error, not_found};
    Pid when is_pid(Pid) ->
        %% Found
        {ok, Pid}
end.
```

## Performance Characteristics

```
Operation        | Complexity | Latency
-----------------|------------|-------------
register/2       | O(nodes)   | 5-10ms
whereis/1        | O(1)       | <1ms (cached)
unregister/1     | O(nodes)   | 5-10ms
join_group/2     | O(1)       | <1ms
get_group_members/1 | O(1)    | <1ms
```

## Best Practices

1. **Always unregister** - Prevent resource leaks
2. **Handle undefined** - Lookup may return undefined
3. **Monitor processes** - Auto-cleanup is done by POC, but you may want your own monitors
4. **Use groups for discovery** - Faster than listing all names
5. **Test failover** - Simulate failures regularly

## Comparison: Direct vs POC API

```erlang
%% Direct global API (manual cleanup)
global:register_name(Name, Pid),
%% ... 
%% Must manually unregister if process dies

%% POC API (automatic cleanup)
erlmcp_distributed_registry_poc:register(Name, Pid).
%% Automatic monitor + cleanup on process death
```

## Integration Pattern

```erlang
-module(my_server).
-behaviour(gen_server).

start_link(Name) ->
    case gen_server:start_link(?MODULE, [], []) of
        {ok, Pid} ->
            %% Register with distributed registry
            case erlmcp_distributed_registry_poc:register(Name, Pid) of
                ok ->
                    %% Join appropriate group
                    erlmcp_distributed_registry_poc:join_group(my_server_group, Pid),
                    {ok, Pid};
                {error, Reason} ->
                    gen_server:stop(Pid),
                    {error, Reason}
            end;
        Error ->
            Error
    end.

terminate(_Reason, _State) ->
    %% Unregister (optional, auto-cleanup happens anyway)
    erlmcp_distributed_registry_poc:unregister(?MODULE),
    ok.
```

---

**For full documentation**, see `README_DISTRIBUTED_REGISTRY.md` and `DISTRIBUTED_REGISTRY_POC_SUMMARY.md`.
