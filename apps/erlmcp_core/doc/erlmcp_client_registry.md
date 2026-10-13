# erlmcp_client_registry

A simple named client registry for erlmcp that provides convenient client connection management with process monitoring and automatic cleanup.

## Overview

The `erlmcp_client_registry` module implements a client registry for Phase 1 MCP API compatibility. It provides:

- **Named client registration** - Register clients with unique names
- **Process monitoring** - Automatic monitoring and cleanup of crashed clients
- **Concurrent access support** - Thread-safe operations with proper synchronization
- **Metadata support** - Attach additional information to client registrations
- **Cleanup utilities** - Manual and automatic dead client cleanup

## API Reference

### Starting the Registry

```erlang
%% Start with default options
{ok, Pid} = erlmcp_client_registry:start_link().

%% Start with custom options
{ok, Pid} = erlmcp_client_registry:start_link(#{name => custom_registry}).
```

### Client Registration

#### Basic Registration

```erlang
%% Register a client with name and PID
ClientName = <<"my-client">>,
ClientPid = self(),
{ok, MonitorRef} = erlmcp_client_registry:register(ClientName, ClientPid).
```

#### Registration with Metadata

```erlang
%% Register client with metadata
Metadata = #{
    version => <<"1.0.0">>,
    type => <<"tool">>,
    capabilities => [<<"list">>, <<"create">>]
},
{ok, MonitorRef} = erlmcp_client_registry:register(ClientName, ClientPid, Metadata).
```

### Client Lookup

```erlang
%% Lookup client by name
case erlmcp_client_registry:lookup(<<"my-client">>) of
    {ok, Pid, Metadata, Timestamp} ->
        io:format("Client found: ~p, registered at: ~p~n", [Pid, Timestamp]);
    {error, not_found} ->
        io:format("Client not found~n")
end.
```

### Client Management

```erlang
%% Unregister a client
ok = erlmcp_client_registry:unregister(<<"my-client">>).

%% Check if client exists
Exists = erlmcp_client_registry:exists(<<"my-client">>).

%% Get total client count
Count = erlmcp_client_registry:count().

%% List all client names
{ok, ClientNames} = erlmcp_client_registry:list().

%% Get all client PIDs
Pids = erlmcp_client_registry:get_pids().

%% Cleanup dead clients
ok = erlmcp_client_registry:cleanup_dead().
```

### Error Handling

```erlang
%% Handle registration errors
case erlmcp_client_registry:register(ClientName, ClientPid) of
    {ok, MonitorRef} ->
        %% Registration successful
        ok;
    {error, already_exists} ->
        %% Client already registered
        io:format("Client already exists~n");
    {error, invalid_name} ->
        %% Invalid client name
        io:format("Invalid client name~n")
end.

%% Handle lookup errors
case erlmcp_client_registry:lookup(ClientName) of
    {ok, Pid, Metadata, Timestamp} ->
        %% Client found
        process_client(Pid, Metadata);
    {error, not_found} ->
        %% Client not found
        handle_not_found()
end.
```

## Usage Examples

### Basic Usage

```erlang
%% Start the registry
{ok, _RegistryPid} = erlmcp_client_registry:start_link().

%% Register a client
{ok, _MonitorRef} = erlmcp_client_registry:register(<<"api-client">>, self()).

%% Check if client exists
io:format("Client exists: ~p~n", [erlmcp_client_registry:exists(<<"api-client">>)]).

%% Get client count
io:format("Total clients: ~p~n", [erlmcp_client_registry:count()]).

%% List all clients
{ok, AllClients} = erlmcp_client_registry:list(),
io:format("All clients: ~p~n", [AllClients]).

%% Lookup client details
case erlmcp_client_registry:lookup(<<"api-client">>) of
    {ok, Pid, Metadata, Timestamp} ->
        io:format("Client: ~p, Metadata: ~p, Registered: ~p~n",
                 [Pid, Metadata, Timestamp]);
    {error, not_found} ->
        io:format("Client not found~n")
end.

%% Unregister client
ok = erlmcp_client_registry:unregister(<<"api-client">>).
```

### Advanced Usage with Metadata

```erlang
%% Register multiple clients with metadata
Client1 = #{
    name => <<"webhook-client">>,
    pid = spawn_link(fun webhook_handler/0),
    metadata => #{
        type => <<"webhook">>,
        version => <<"2.1.0">>,
        endpoint => <<"https://example.com/webhook">>
    }
},

Client2 = #{
    name => <<"tool-client">>,
    pid = spawn_link(fun tool_handler/0),
    metadata => #{
        type => <<"tool">>,
        version => <<"1.5.0">>,
        capabilities => [<<"read">>, <<"write">>, <<"delete">>]
    }
},

%% Register both clients
{ok, _Ref1} = erlmcp_client_registry:register(Client1#name, Client1#pid, Client1#metadata),
{ok, _Ref2} = erlmcp_client_registry:register(Client2#name, Client2#pid, Client2#metadata),

%% Filter clients by type
ToolClients = lists:filter(fun(Name) ->
    case erlmcp_client_registry:lookup(Name) of
        {ok, _, Metadata, _} ->
            maps:get(<<"type">>, Metadata) == <<"tool">>;
        {error, not_found} ->
            false
    end
end, [Client1#name, Client2#name]),

io:format("Tool clients: ~p~n", [ToolClients]).
```

### Concurrent Access

```erlang
%% Spawn multiple client processes
ClientPids = lists:map(fun(I) ->
    spawn_link(fun() ->
        ClientName = list_to_binary("client-" ++ integer_to_list(I)),
        ClientPid = self(),

        %% Register with registry
        case erlmcp_client_registry:register(ClientName, ClientPid) of
            {ok, _MonitorRef} ->
                %% Do client work
                do_client_work(ClientName),
                %% Unregister when done
                erlmcp_client_registry:unregister(ClientName);
            {error, Reason} ->
                io:format("Failed to register ~p: ~p~n", [ClientName, Reason])
        end
    end)
end, lists:seq(1, 10)).

%% Wait for all clients to complete
wait_for_completion(ClientPids).
```

### Process Death Handling

```erlang
%% Register a process that might crash
CrashPid = spawn_link(fun() ->
    %% Process will crash after 2 seconds
    timer:sleep(2000),
    exit(crash)
end),

{ok, _MonitorRef} = erlmcp_client_registry:register(<<"crash-client">>, CrashPid).

%% Process is automatically monitored
%% If it crashes, it will be automatically cleaned up
%% Manual cleanup can also be triggered
ok = erlmcp_client_registry:cleanup_dead().

%% Verify cleanup
case erlmcp_client_registry:lookup(<<"crash-client">>) of
    {error, not_found} ->
        io:format("Client was cleaned up automatically~n");
    {ok, _, _, _} ->
        io:format("Client still exists~n")
end.
```

## Integration with erlmcp

The client registry integrates seamlessly with erlmcp's connection patterns:

```erlang
%% In your connection handler
handle_connection(Socket, Options) ->
    %% Create unique client identifier
    ClientId = generate_client_id(Socket),

    %% Register client
    case erlmcp_client_registry:register(ClientId, self(), Options) of
        {ok, _MonitorRef} ->
            %% Set up message handling
            ok;
        {error, Reason} ->
            %% Handle registration failure
            error_handling(Reason)
    end.

%% In your message processing loop
process_message(Message) ->
    %% Check if client is still registered
    case erlmcp_client_registry:exists(ClientId) of
        true ->
            %% Process message
            process_message(Message);
        false ->
            %% Client was unregistered or crashed
            handle_client_gone()
    end.

%% When client connection closes
close_connection() ->
    %% Unregister client
    erlmcp_client_registry:unregister(ClientId).
```

## Configuration Options

The registry can be configured with various options:

```erlang
%% Start with custom configuration
Options = #{
    name => custom_registry,          %% Local process name
    cleanup_interval => 300000,      %% Cleanup interval in ms
    max_clients => 1000,             %% Maximum clients (optional)
    metadata_validation => true       %% Enable metadata validation
},

{ok, Pid} = erlmcp_client_registry:start_link(Options).
```

## Performance Considerations

### Concurrency
- The registry uses `gen_server` for thread-safe operations
- All operations are serialized through the server process
- Monitor operations are handled asynchronously

### Memory Usage
- Each client registration stores name, PID, metadata, and timestamp
- Monitor references are stored for cleanup coordination
- Periodic cleanup helps prevent memory leaks

### Monitoring Overhead
- Automatic monitoring adds minimal overhead
- Dead processes are cleaned up both automatically and manually
- Monitor references are properly demonitored on unregister

## Error Scenarios

### Common Error Cases

```erlang
%% Invalid client name
{error, invalid_name} = erlmcp_client_registry:register(<<>>, self()).

%% Duplicate client registration
{error, already_exists} = erlmcp_client_registry:register(<<"same">>, self()).

%% Unregistering non-existent client
{error, not_found} = erlmcp_client_registry:unregister(<<"non-existent">>).

%% Looking up non-existent client
{error, not_found} = erlmcp_client_registry:lookup(<<"non-existent">>).
```

### Recovery Strategies

```erlang
%% Handle temporary failures
register_with_retry(ClientName, ClientPid, MaxRetries) ->
    register_with_retry(ClientName, ClientPid, MaxRetries, 0).

register_with_retry(_ClientName, _ClientPid, 0, _RetryCount) ->
    {error, max_retries_exceeded};
register_with_retry(ClientName, ClientPid, MaxRetries, RetryCount) ->
    case erlmcp_client_registry:register(ClientName, ClientPid) of
        {ok, MonitorRef} ->
            {ok, MonitorRef};
        {error, already_exists} ->
            %% Wait and retry
            timer:sleep(100),
            register_with_retry(ClientName, ClientPid, MaxRetries, RetryCount + 1);
        {error, Reason} ->
            {error, Reason}
    end.
```

## Testing

The module includes comprehensive test coverage:

```bash
%% Run all tests
rebar3 eunit app=erlmcp_core

%% Run specific test
rebar3 eunit app=erlmcp_core suites=erlmcp_client_registry_tests

%% Run tests with coverage
rebar3 cover --app erlmcp_core
```

## Troubleshooting

### Common Issues

1. **Client not found**: Check if client was properly registered
2. **Registration failed**: Verify client name format and uniqueness
3. **Process cleanup**: Ensure dead processes are properly monitored
4. **Memory usage**: Check for excessive client registrations

### Debug Commands

```erlang
%% Check current registry state
{ok, Clients} = erlmcp_client_registry:list(),
io:format("Registered clients: ~p~n", [Clients]).

%% Check client count
io:format("Client count: ~p~n", [erlmcp_client_registry:count()]).

%% Cleanup dead clients manually
ok = erlmcp_client_registry:cleanup_dead().
```

## Migration from Legacy Systems

If migrating from a client registry implementation:

```erlang
%% Old way (if any)
%% OldRegistry:register(ClientName, ClientPid)

%% New way
{ok, MonitorRef} = erlmcp_client_registry:register(ClientName, ClientPid),

%% Store monitor reference for cleanup
ClientState#client_state{monitor_ref = MonitorRef}.
```

## Future Enhancements

Potential future improvements:

- **Distributed registry**: Support for cluster-wide client registration
- **TTL support**: Time-to-live for client registrations
- **Metrics**: Registry usage metrics and monitoring
- **Event notifications**: Client registration/unregister events
- **Persistence**: Optional persistent client registry

## API Reference Summary

| Function | Description | Return Type |
|----------|-------------|-------------|
| `start_link()` | Start registry with default options | `{ok, pid()} \| {error, term()}` |
| `start_link(Options)` | Start registry with options | `{ok, pid()} \| {error, term()}` |
| `register(Name, Pid)` | Register client | `{ok, reference()} \| {error, term()}` |
| `register(Name, Pid, Metadata)` | Register client with metadata | `{ok, reference()} \| {error, term()}` |
| `unregister(Name)` | Unregister client | `ok \| {error, not_found}` |
| `lookup(Name)` | Lookup client | `{ok, pid(), map(), integer()} \| {error, not_found}` |
| `list()` | List all clients | `{ok, [binary()]}` |
| `exists(Name)` | Check if client exists | `boolean()` |
| `count()` | Get client count | `non_neg_integer()` |
| `get_pids()` | Get all client PIDs | `[pid()]` |
| `cleanup_dead()` | Cleanup dead clients | `ok` |