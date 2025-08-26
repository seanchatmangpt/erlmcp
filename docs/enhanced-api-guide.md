# Enhanced High-Level API Guide - Phase 3 Step 7

## Overview

The ErlMCP enhanced high-level API provides comprehensive transport creation, validation, and management capabilities. This guide covers the new features implemented in Phase 3 Step 7.

## Enhanced Transport Creation

### start_transport/3 with Validation

The enhanced `start_transport/3` function now includes comprehensive validation integration:

```erlang
% Create a validated TCP transport
Result = erlmcp:start_transport(my_tcp_transport, tcp, #{
    host => "localhost",
    port => 8080,
    keepalive => true
}).

% Returns: {ok, TransportPid} or enhanced error information
case Result of
    {ok, TransportPid} ->
        logger:info("Transport started successfully: ~p", [TransportPid]);
    {error, #{error_type := validation_failed, suggestion := Suggestion}} ->
        logger:error("Validation failed: ~s", [Suggestion]);
    {error, #{error_type := start_failed, reason := Reason}} ->
        logger:error("Transport start failed: ~p", [Reason])
end.
```

### Enhanced Convenience Functions

#### TCP Setup with Validation

```erlang
% Enhanced TCP setup with comprehensive validation and error reporting
ServerConfig = #{capabilities => #mcp_server_capabilities{
    tools => #mcp_capability{enabled => true}
}},

TcpConfig = #{
    host => "192.168.1.100",
    port => 9090,
    keepalive => true,
    connect_timeout => 5000,
    max_reconnect_attempts => 3
},

case erlmcp:start_tcp_setup(my_server, ServerConfig, TcpConfig) of
    {ok, #{server := ServerPid, transport := TransportPid, config := Config}} ->
        logger:info("TCP setup completed: server ~p, transport ~p", [ServerPid, TransportPid]);
    {error, #{error_type := validation_failed, validation_error := Details}} ->
        logger:error("TCP config validation failed: ~p", [Details]);
    {error, #{error_type := transport_start_failed, suggestion := Suggestion}} ->
        logger:error("TCP transport failed to start: ~s", [Suggestion])
end.
```

#### HTTP Setup with Validation

```erlang
% Enhanced HTTP setup with URL validation and error reporting
HttpConfig = #{
    url => "https://api.example.com/mcp",
    method => post,
    headers => #{<<"Authorization">> => <<"Bearer token123">>},
    timeout => 10000,
    cors => ["https://app.example.com"]
},

case erlmcp:start_http_setup(my_http_server, ServerConfig, HttpConfig) of
    {ok, Result} ->
        logger:info("HTTP setup completed: ~p", [Result]);
    {error, #{validation_error := #{field := url}}} ->
        logger:error("Invalid URL format in HTTP config");
    {error, Error} ->
        logger:error("HTTP setup failed: ~p", [Error])
end.
```

## Enhanced Error Handling

### Validation Error Formatting

The API provides comprehensive error formatting with helpful suggestions:

```erlang
% Example validation error with suggestion
{error, #{
    error_type => validation_failed,
    entity_id => my_transport,
    transport_type => tcp,
    validation_error => #{
        type => missing_field,
        field => port,
        message => "Required field is missing",
        severity => error
    },
    suggestion => "TCP transport requires 'port' field. Example: #{port => 8080}",
    timestamp => {1756, 188136, 9404}
}}
```

### Transport Error Formatting

```erlang
% Example transport error with context
{error, #{
    error_type => start_failed,
    transport_id => my_tcp_transport,
    transport_type => tcp,
    reason => {port_in_use, 8080},
    suggestion => "Port 8080 is already in use. Try a different port number",
    timestamp => {1756, 188136, 9404}
}}
```

### Setup Error Formatting

```erlang
% Example setup error with cleanup information
{error, #{
    error_type => server_start_failed,
    server_id => my_server,
    transport_type => tcp,
    reason => {missing_dependency, erlmcp_registry},
    suggestion => "Server startup failed. Check server configuration and ensure required dependencies are available",
    timestamp => {1756, 188136, 9404}
}}
```

## Transport Binding Management

### Get Binding Information

```erlang
% Get detailed information about a transport binding
case erlmcp:get_transport_binding_info(my_transport) of
    {ok, Info} ->
        #{
            transport_id := TransportId,
            server_id := ServerId,
            transport_type := Type,
            bound := IsBound,
            status := Status,
            config := Config
        } = Info,
        logger:info("Transport ~p bound to server ~p, status: ~p", 
                   [TransportId, ServerId, Status]);
    {error, transport_not_found} ->
        logger:warning("Transport not found");
    {error, registry_not_available} ->
        logger:error("Registry service not available")
end.
```

### List All Transport Bindings

```erlang
% List all transport bindings with details
case erlmcp:list_transport_bindings() of
    Bindings when is_list(Bindings) ->
        lists:foreach(fun({TransportId, Info}) ->
            Status = maps:get(status, Info, unknown),
            Type = maps:get(transport_type, Info, unknown),
            logger:info("Transport ~p (~p): ~p", [TransportId, Type, Status])
        end, Bindings);
    {error, Reason} ->
        logger:error("Failed to list bindings: ~p", [Reason])
end.
```

### Rebind Transport

```erlang
% Rebind a transport to a different server
case erlmcp:rebind_transport(my_transport, new_server) of
    ok ->
        logger:info("Transport successfully rebound to new_server");
    {error, Reason} ->
        logger:error("Failed to rebind transport: ~p", [Reason])
end.
```

### Validate Binding Compatibility

```erlang
% Check if a transport can be bound to a server
case erlmcp:validate_transport_binding(my_tcp_transport, my_server) of
    ok ->
        logger:info("Binding is compatible");
    {error, {incompatible_binding, Reason}} ->
        logger:warning("Incompatible binding: ~s", [Reason]);
    {error, transport_not_found} ->
        logger:error("Transport not found");
    {error, server_not_found} ->
        logger:error("Server not found")
end.
```

### Audit Transport Bindings

```erlang
% Perform a comprehensive audit of all transport bindings
case erlmcp:audit_transport_bindings() of
    {ok, Audit} ->
        #{
            total_bindings := Total,
            healthy_bindings := Healthy,
            issues := Issues,
            audit_time := Timestamp
        } = Audit,
        
        logger:info("Binding audit: ~p total, ~p healthy, ~p issues", 
                   [Total, Healthy, length(Issues)]),
        
        % Log issues
        lists:foreach(fun({TransportId, IssueList}) ->
            logger:warning("Transport ~p has issues: ~p", [TransportId, IssueList])
        end, Issues);
    {error, Reason} ->
        logger:error("Binding audit failed: ~p", [Reason])
end.
```

## Configuration Examples and Support

### Get Configuration Examples

```erlang
% Get comprehensive configuration examples
Examples = erlmcp:get_config_examples(),
#{
    stdio := #{
        type := stdio,
        server_id := my_server,
        test_mode := false,
        buffer_size := 8192
    },
    tcp := #{
        type := tcp,
        host := "localhost",
        port := 8080,
        keepalive := true,
        connect_timeout := 5000,
        max_reconnect_attempts := 3,
        ssl := false
    },
    http := #{
        type := http,
        url := "https://api.example.com/mcp",
        method := post,
        headers := #{<<"Content-Type">> := <<"application/json">>},
        timeout := 30000,
        cors := true,
        max_body_size := 1048576
    }
} = Examples.
```

### List Supported Transport Types

```erlang
% Get list of supported transport types
SupportedTypes = erlmcp:list_supported_transport_types(),
% Returns: [stdio, tcp, http]

lists:foreach(fun(Type) ->
    logger:info("Supported transport type: ~p", [Type])
end, SupportedTypes).
```

## Validation Module Integration

The enhanced API integrates with `erlmcp_transport_validation` module for comprehensive validation:

### Direct Validation Usage

```erlang
% Validate configuration directly
Config = #{host => "localhost", port => 8080},
case erlmcp_transport_validation:validate_transport_config(tcp, Config) of
    ok ->
        logger:info("Configuration is valid");
    {error, {validation_error, ErrorType, Field, Message}} ->
        logger:error("Validation failed: ~p in field ~p: ~s", [ErrorType, Field, Message])
end.
```

### Get Validation Errors

```erlang
% Get detailed validation error information
Config = #{host => "localhost"}, % Missing port
Errors = erlmcp_transport_validation:get_validation_errors(Config#{type => tcp}),
lists:foreach(fun({validation_error, ErrorType, Field, Message}) ->
    logger:error("~p error in ~p: ~s", [ErrorType, Field, Message])
end, Errors).
```

## Best Practices

### 1. Always Handle Enhanced Error Responses

```erlang
handle_enhanced_error({error, #{error_type := Type, suggestion := Suggestion}}) ->
    logger:error("Operation failed (~p): ~s", [Type, Suggestion]);
handle_enhanced_error({error, Reason}) ->
    logger:error("Operation failed: ~p", [Reason]).
```

### 2. Use Validation Before Creation

```erlang
validate_and_create_transport(TransportId, Type, Config) ->
    case erlmcp_transport_validation:validate_transport_config(Type, Config) of
        ok ->
            erlmcp:start_transport(TransportId, Type, Config);
        {error, ValidationError} ->
            FormattedError = erlmcp:format_validation_error(TransportId, Type, ValidationError),
            FormattedError
    end.
```

### 3. Regular Binding Audits

```erlang
% Schedule regular binding health checks
schedule_binding_audit() ->
    timer:apply_interval(300000, fun() -> % Every 5 minutes
        case erlmcp:audit_transport_bindings() of
            {ok, Audit} ->
                Issues = maps:get(issues, Audit, []),
                case Issues of
                    [] -> ok; % All healthy
                    _ -> 
                        logger:warning("Transport binding issues detected: ~p", [Issues])
                end;
            {error, Reason} ->
                logger:error("Binding audit failed: ~p", [Reason])
        end
    end).
```

### 4. Graceful Error Recovery

```erlang
robust_transport_setup(ServerId, Type, Config) ->
    case start_transport_with_retry(ServerId, Type, Config, 3) of
        {ok, Result} -> {ok, Result};
        {error, FinalError} -> 
            logger:error("Transport setup failed after retries: ~p", [FinalError]),
            {error, FinalError}
    end.

start_transport_with_retry(_ServerId, _Type, _Config, 0) ->
    {error, max_retries_exceeded};
start_transport_with_retry(ServerId, Type, Config, Retries) ->
    case Type of
        tcp -> erlmcp:start_tcp_setup(ServerId, #{}, Config);
        http -> erlmcp:start_http_setup(ServerId, #{}, Config);
        stdio -> erlmcp:start_stdio_setup(ServerId, Config)
    end of
        {ok, Result} -> {ok, Result};
        {error, #{error_type := validation_failed}} = Error -> 
            Error; % Don't retry validation errors
        {error, _} -> 
            timer:sleep(1000), % Wait 1 second between retries
            start_transport_with_retry(ServerId, Type, Config, Retries - 1)
    end.
```

## Summary

The enhanced high-level API provides:

1. **Comprehensive Validation**: Integration with dedicated validation module
2. **Enhanced Error Handling**: Detailed error information with suggestions
3. **Transport Binding Management**: Complete binding lifecycle management
4. **Configuration Support**: Examples and validation helpers
5. **Robust Error Recovery**: Built-in error formatting and suggestion system

This makes the ErlMCP system much more user-friendly and production-ready with clear error messages, helpful suggestions, and comprehensive management capabilities.