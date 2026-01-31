# erlmcp_test_helpers - Test Infrastructure Helper Module

## Overview

`erlmcp_test_helpers` provides comprehensive utilities for erlmcp testing to eliminate common test failures related to:

- Port conflicts
- Process registration conflicts
- Unique test ID generation
- Process cleanup
- Test data generation

## Features

### 1. Unique ID Generation

Generate unique, timestamped IDs for test entities to avoid conflicts.

```erlang
%% Generate unique server ID
ServerId = erlmcp_test_helpers:generate_server_id(),
% => <<"server_1706739200000000_abc123_1">>

%% Generate with custom prefix
ServerId = erlmcp_test_helpers:generate_server_id(<<"my_test">>),
% => <<"my_test_1706739200000000_def456_1">>

%% Generate unique client/transport IDs
ClientId = erlmcp_test_helpers:generate_client_id(),
TransportId = erlmcp_test_helpers:generate_transport_id(<<"tcp_test">>),
```

### 2. Port Management

Allocate and manage ports to prevent conflicts between concurrent tests.

```erlang
%% Get a free port (fire-and-forget, no tracking)
Port = erlmcp_test_helpers:get_free_port(),

%% Allocate a port with tracking
{ok, Port} = erlmcp_test_helpers:allocate_port(),
% => Port in range 10000-65000

%% Allocate from custom range
{ok, Port} = erlmcp_test_helpers:allocate_port({8000, 9000}),

%% Release port when done
ok = erlmcp_test_helpers:release_port(Port),
```

### 3. Process Management

Start and stop test servers/clients with automatic cleanup.

```erlang
%% Start test server with unique ID
ServerId = erlmcp_test_helpers:generate_server_id(),
{ok, Server} = erlmcp_test_helpers:start_test_server(ServerId),

%% Start with custom capabilities
Caps = #mcp_server_capabilities{
    resources = #mcp_capability{enabled = true},
    tools = #mcp_capability{enabled = true}
},
{ok, Server} = erlmcp_test_helpers:start_test_server(ServerId, Caps),

%% Start test client
{ok, Client} = erlmcp_test_helpers:start_test_client({stdio, #{test_mode => true}}),

%% Stop individual process
ok = erlmcp_test_helpers:stop_test_process(Server),

%% Stop all tracked test processes
ok = erlmcp_test_helpers:stop_all_test_processes(),
```

### 4. Test Data Generation

Generate random test data for resources, tools, and prompts.

```erlang
%% Generate random binary
Bin = erlmcp_test_helpers:random_binary(16),

%% Generate random URI
Uri = erlmcp_test_helpers:random_uri(),
% => <<"test://1706739200000000_abc123/12345">>

Uri = erlmcp_test_helpers:random_uri(<<"custom">>),
% => <<"custom://1706739200000000_def456/67890">>

%% Generate test resource with handler
{Uri, Handler} = erlmcp_test_helpers:generate_test_resource(),

%% Generate test tool with handler
{Name, Handler} = erlmcp_test_helpers:generate_test_tool(),

%% Generate test prompt with handler
{Name, Handler} = erlmcp_test_helpers:generate_test_prompt(),
```

### 5. Mock Transport Setup

Create mock transports for testing.

```erlang
%% Create default mock transport (stdio)
{Module, Config} = erlmcp_test_helpers:create_mock_transport(),
% => {erlmcp_transport_stdio, #{test_mode => true, mock_responses => []}}

%% Create with custom options
{Module, Config} = erlmcp_test_helpers:create_mock_transport(#{
    type => tcp,
    test_mode => true,
    mock_responses => [{<<"test">>, <<"response">>}]
}),
```

### 6. Test Setup/Teardown

Standardized setup and cleanup helpers.

```erlang
%% Setup test environment
ok = erlmcp_test_helpers:setup_test(),

%% Cleanup test environment
ok = erlmcp_test_helpers:cleanup_test(),

%% Execute function with automatic cleanup
Result = erlmcp_test_helpers:with_test_server(<<"my_test">>, fun(Server) ->
    %% Do something with Server
    %% Automatically cleaned up after function returns
    ok
end),

%% Execute with client
Result = erlmcp_test_helpers:with_test_client({stdio, #{}}, fun(Client) ->
    %% Do something with Client
    ok
end),
```

### 7. Assertions

Convenient assertion helpers for process state.

```erlang
%% Assert process is alive
true = erlmcp_test_helpers:assert_process_alive(ServerPid),

%% Assert process is dead
true = erlmcp_test_helpers:assert_process_dead(ServerPid),

%% Wait for process to die (default 5s timeout)
ok = erlmcp_test_helpers:wait_for_process_death(ServerPid),

%% Wait with custom timeout
ok = erlmcp_test_helpers:wait_for_process_death(ServerPid, 10000),
```

### 8. Utilities

General utility functions for testing.

```erlang
%% Sleep for specified milliseconds
ok = erlmcp_test_helpers:sleep(100),

%% Get current timestamp (microseconds)
Timestamp = erlmcp_test_helpers:timestamp(),

%% Generate unique reference binary
Ref = erlmcp_test_helpers:unique_ref(),
% => <<"1706739200_123456_789_12345">>
```

## Complete Example

```erlang
-module(my_resource_tests).
-include_lib("eunit/include/eunit.hrl").

resource_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          {"Add and retrieve resource", fun test_add_resource/0},
          {"List resources", fun test_list_resources/0}
         ]
     end}.

setup() ->
    erlmcp_test_helpers:setup_test().

cleanup(_) ->
    erlmcp_test_helpers:cleanup_test().

test_add_resource() ->
    %% Start server with unique ID
    ServerId = erlmcp_test_helpers:generate_server_id(),
    {ok, Server} = erlmcp_test_helpers:start_test_server(ServerId),

    %% Add resource
    {Uri, Handler} = erlmcp_test_helpers:generate_test_resource(),
    ok = erlmcp_server:add_resource(Server, Uri, Handler),

    %% Cleanup
    ok = erlmcp_test_helpers:stop_test_process(Server).

test_list_resources() ->
    %% Using with_test_server for automatic cleanup
    ServerId = erlmcp_test_helpers:generate_server_id(),
    erlmcp_test_helpers:with_test_server(ServerId, fun(Server) ->
        %% Add some resources
        lists:foreach(fun(_) ->
            {Uri, Handler} = erlmcp_test_helpers:generate_test_resource(),
            erlmcp_server:add_resource(Server, Uri, Handler)
        end, lists:seq(1, 10)),

        %% List resources
        {ok, Resources} = erlmcp_server:list_resources(Server),
        ?assertEqual(10, length(Resources))
    end).
```

## Port Conflict Resolution

The most common test failure in parallel execution is port conflicts. Use these patterns:

### Pattern 1: Fire-and-Forget (Simple)

```erlang
test_with_port() ->
    Port = erlmcp_test_helpers:get_free_port(),
    %% Use port immediately
    {ok, Socket} = gen_tcp:connect("localhost", Port, []),
    %% No cleanup needed - port released when socket closes
    ok.
```

### Pattern 2: Tracked Allocation (Complex Tests)

```erlang
test_with_multiple_ports() ->
    %% Allocate multiple ports
    Ports = [begin {ok, P} = erlmcp_test_helpers:allocate_port(), P end
             || _ <- lists:seq(1, 5)],

    %% Use all ports
    [do_something_with_port(P) || P <- Ports],

    %% Explicitly release
    [erlmcp_test_helpers:release_port(P) || P <- Ports],
    ok.
```

## Process Registration

Use unique IDs to avoid process registration conflicts:

```erlang
%% BAD - hardcoded ID
{ok, S1} = erlmcp_server:start_link(<<"test_server">>, #{}),
{ok, S2} = erlmcp_server:start_link(<<"test_server">>, #{}),  % CONFLICT!

%% GOOD - unique IDs
Id1 = erlmcp_test_helpers:generate_server_id(),
Id2 = erlmcp_test_helpers:generate_server_id(),
{ok, S1} = erlmcp_server:start_link(Id1, #{}),
{ok, S2} = erlmcp_server:start_link(Id2, #{}),  % No conflict
```

## Parallel Test Execution

All helper functions are designed for parallel test execution:

```erlang
%% Tests can run in parallel without conflicts
parallel_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     fun(_) ->
         [
          {"Test 1", fun test1/0},
          {"Test 2", fun test2/0},
          {"Test 3", fun test3/0}
         ]
     end}.

test1() ->
    ServerId = erlmcp_test_helpers:generate_server_id(),
    {ok, Server} = erlmcp_test_helpers:start_test_server(ServerId),
    %% ... test code ...
    ok = erlmcp_test_helpers:stop_test_process(Server).

test2() ->
    %% Different unique ID, no conflict with test1
    ServerId = erlmcp_test_helpers:generate_server_id(),
    {ok, Server} = erlmcp_test_helpers:start_test_server(ServerId),
    %% ... test code ...
    ok = erlmcp_test_helpers:stop_test_process(Server).

test3() ->
    %% Yet another unique ID
    ServerId = erlmcp_test_helpers:generate_server_id(),
    {ok, Server} = erlmcp_test_helpers:start_test_server(ServerId),
    %% ... test code ...
    ok = erlmcp_test_helpers:stop_test_process(Server).
```

## Helper Server

The test helpers module runs as a gen_server to track allocated ports and processes. It's automatically started by `setup_test/0` and should be left running between tests.

```erlang
%% Manual control (usually not needed)
{ok, Pid} = erlmcp_test_helpers:start_link(),
gen_server:stop(erlmcp_test_helpers),
```

## Migration from Existing Tests

To update existing tests:

1. Replace hardcoded IDs with generated IDs:
```erlang
%% Before
{ok, Server} = erlmcp_server:start_link(<<"test">>, #{}),

%% After
ServerId = erlmcp_test_helpers:generate_server_id(),
{ok, Server} = erlmcp_server:start_link(ServerId, #{}),
```

2. Replace port binding with port helpers:
```erlang
%% Before
{ok, S} = gen_tcp:listen(0, []),
{ok, Port} = inet:port(S),
gen_tcp:close(S),

%% After
Port = erlmcp_test_helpers:get_free_port(),
```

3. Add automatic cleanup:
```erlang
%% Before
{ok, Server} = erlmcp_server:start_link(Id, #{}),
%% ... test code ...
erlmcp_server:stop(Server),

%% After
erlmcp_test_helpers:with_test_server(Id, fun(Server) ->
    %% ... test code ...
    %% Automatically cleaned up
end),
```

## API Reference

See `erlmcp_test_helpers.erl` for complete API documentation with types and specs.

### Key Functions

- `generate_server_id/0,1` - Generate unique server IDs
- `generate_client_id/0,1` - Generate unique client IDs
- `allocate_port/0,1` - Allocate tracked ports
- `get_free_port/0` - Get free port without tracking
- `start_test_server/1,2,3` - Start test servers
- `start_test_client/1,2` - Start test clients
- `stop_test_process/1` - Stop test process
- `stop_all_test_processes/0` - Stop all tracked processes
- `with_test_server/2` - Execute function with automatic cleanup
- `with_test_client/2` - Execute with client, auto cleanup
- `assert_process_alive/1` - Assert process alive
- `wait_for_process_death/1,2` - Wait for process termination

## Best Practices

1. **Always use unique IDs** for test servers/clients
2. **Prefer `with_test_server/2`** for automatic cleanup
3. **Release ports explicitly** when using `allocate_port/1`
4. **Call `cleanup_test/0`** in test teardown
5. **Use helpers for all test data** to ensure randomness

## Troubleshooting

### Port Already in Use

```erlang
%% If you get "eaddrinuse", use tracked allocation:
{ok, Port} = erlmcp_test_helpers:allocate_port(),
%% Use port...
ok = erlmcp_test_helpers:release_port(Port),
```

### Process Registration Conflict

```erlang
%% If you get "already registered", use unique IDs:
ServerId = erlmcp_test_helpers:generate_server_id(),
```

### Processes Not Cleaning Up

```erlang
%% Use stop_all_test_processes in cleanup:
cleanup(_) ->
    erlmcp_test_helpers:stop_all_test_processes(),
    ok.
```
