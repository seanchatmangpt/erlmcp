# Configuration Validation Guide

This document describes the comprehensive configuration validation system in erlmcp, which ensures that transport configurations are valid and provides helpful error messages for debugging.

## Overview

The configuration validation system provides:
- Schema-based validation for all transport types
- Comprehensive field validation with type checking
- Helpful error messages with suggestions
- Configuration examples and documentation
- Programmatic access to validation rules

## Supported Transport Types

### STDIO Transport

**Description**: Direct process communication transport for local servers.

**Required Fields**:
- `type`: Must be `stdio`

**Optional Fields**:
- `server_id`: Atom identifying the server (default: auto-generated)
- `test_mode`: Boolean enabling test mode (default: `false`)
- `buffer_size`: Positive integer for buffer size in bytes (default: 8192)

**Example**:
```erlang
StdioConfig = #{
    type => stdio,
    server_id => my_server,
    test_mode => false,
    buffer_size => 8192
}.
```

### TCP Transport

**Description**: Network communication transport for remote servers.

**Required Fields**:
- `type`: Must be `tcp`
- `host`: String or binary hostname/IP address
- `port`: Integer between 1 and 65535

**Optional Fields**:
- `keepalive`: Boolean enabling TCP keepalive (default: `true`)
- `connect_timeout`: Positive integer timeout in milliseconds (default: 5000)
- `max_reconnect_attempts`: Non-negative integer (default: 3)
- `server_id`: Atom identifying the server
- `ssl`: Boolean enabling SSL/TLS (default: `false`)
- `certfile`: String path to SSL certificate file (required if ssl=true)
- `keyfile`: String path to SSL private key file (required if ssl=true)

**Example**:
```erlang
TcpConfig = #{
    type => tcp,
    host => \"localhost\",
    port => 8080,
    keepalive => true,
    connect_timeout => 5000,
    max_reconnect_attempts => 3,
    ssl => false
}.

% SSL Example
TcpSSLConfig = #{
    type => tcp,
    host => \"secure.example.com\",
    port => 8443,
    ssl => true,
    certfile => \"/path/to/cert.pem\",
    keyfile => \"/path/to/key.pem\"
}.
```

### HTTP Transport

**Description**: Web-based communication transport using HTTP/HTTPS.

**Required Fields**:
- `type`: Must be `http`
- `url`: String or binary valid HTTP/HTTPS URL

**Optional Fields**:
- `method`: HTTP method atom (`get`, `post`, `put`, `delete`, `patch`, `head`, `options`) (default: `post`)
- `headers`: Map of binary/string header name/value pairs
- `timeout`: Positive integer timeout in milliseconds (default: 30000)
- `server_id`: Atom identifying the server
- `cors`: Boolean or list of origin URLs for CORS support (default: `false`)
- `max_body_size`: Positive integer maximum request body size in bytes (default: 1MB)

**Example**:
```erlang
HttpConfig = #{
    type => http,
    url => \"https://api.example.com/mcp\",
    method => post,
    headers => #{<<\"Content-Type\">> => <<\"application/json\">>},
    timeout => 30000,
    cors => true,
    max_body_size => 1048576
}.

% CORS with specific origins
HttpCORSConfig = #{
    type => http,
    url => \"https://api.example.com/mcp\",
    cors => [\"https://app.example.com\", \"https://admin.example.com\"]
}.
```

## Validation API

### Basic Validation

```erlang
% Validate a complete configuration
Result = erlmcp:validate_transport_config(Config).

% Returns:
% ok                                    - Configuration is valid
% {error, {validation_error, ...}}     - Configuration is invalid
```

### Schema Access

```erlang
% Get schema for a transport type
{ok, Schema} = erlmcp:get_config_schema(stdio).
{ok, Schema} = erlmcp:get_config_schema(tcp).
{ok, Schema} = erlmcp:get_config_schema(http).

% Get list of supported transport types
Types = erlmcp:list_supported_transport_types().
% Returns: [stdio, tcp, http]
```

### Field Validation

```erlang
% Validate a single field
ok = erlmcp:validate_config_field(tcp, port, 8080).
{error, _} = erlmcp:validate_config_field(tcp, port, -1).
```

### Configuration Examples

```erlang
% Get example configurations for all types
Examples = erlmcp:get_config_examples().
% Returns a map with example configs for stdio, tcp, and http
```

## Error Messages

The validation system provides detailed, helpful error messages:

### Missing Required Fields

```erlang
{error, {validation_error, missing_required_field, type, 
         \"Configuration must specify transport type. Valid types: stdio, tcp, http\"}}

{error, {validation_error, missing_required_field, host, 
         \"Required field missing for TCP transport configuration for network communication. Required fields: [type,host,port]\"}}
```

### Unknown Fields

```erlang
{error, {validation_error, unknown_field, bad_field, 
         \"Unknown field for STDIO transport configuration for direct process communication. Allowed fields: [type,server_id,test_mode,buffer_size]\"}}
```

### Invalid Field Values

```erlang
{error, {validation_error, invalid_field_value, port, 
         \"must be integer between 1 and 65535\"}}

{error, {validation_error, invalid_field_value, ssl_cert, 
         \"SSL certificate file does not exist\"}}

{error, {validation_error, invalid_field_value, cors, 
         \"must be boolean or list of origin URLs\"}}
```

### Transport Type Errors

```erlang
{error, {validation_error, unknown_transport_type, invalid_type, 
         \"Unknown transport type. Supported types: stdio, tcp, http\"}}
```

## Validation Rules

### Field Presence Rules

1. **Required fields** must be present in the configuration
2. **Optional fields** are validated only if present
3. **Unknown fields** cause validation failure

### Field Value Rules

#### Common Rules
- String fields accept both lists and binaries
- Numeric fields must be proper integers where specified
- Boolean fields must be `true` or `false`
- File path fields are checked for existence when applicable

#### Type-Specific Rules

**STDIO**:
- `type` must be exactly `stdio`
- `server_id` must be an atom
- `test_mode` must be boolean
- `buffer_size` must be positive integer

**TCP**:
- `type` must be exactly `tcp`
- `host` cannot be empty string
- `port` must be 1-65535
- `connect_timeout` must be positive
- `max_reconnect_attempts` must be non-negative
- SSL certificate/key files must exist if SSL enabled

**HTTP**:
- `type` must be exactly `http`
- `url` must be valid HTTP/HTTPS URL
- `method` must be valid HTTP method
- `headers` must be map with string keys/values
- `cors` can be boolean or list of valid origin URLs
- `timeout` must be positive

## Usage Patterns

### Validating User Input

```erlang
validate_user_config(UserInput) ->
    case erlmcp:validate_transport_config(UserInput) of
        ok ->
            {ok, UserInput};
        {error, {validation_error, ErrorType, Field, Message}} ->
            logger:error(\"Configuration error - ~s: ~s (~p)\", [Field, Message, ErrorType]),
            {error, {invalid_config, Field, Message}}
    end.
```

### Building Configuration UI

```erlang
build_config_form(TransportType) ->
    {ok, Schema} = erlmcp:get_config_schema(TransportType),
    RequiredFields = maps:get(required_fields, Schema),
    OptionalFields = maps:get(optional_fields, Schema),
    Description = maps:get(description, Schema),
    
    {ok, #{
        description => Description,
        required => RequiredFields,
        optional => OptionalFields,
        example => maps:get(TransportType, erlmcp:get_config_examples())
    }}.
```

### Runtime Configuration Updates

```erlang
update_transport_config(TransportId, NewConfig) ->
    case erlmcp:validate_transport_config(NewConfig) of
        ok ->
            erlmcp:update_transport_config(TransportId, NewConfig);
        {error, ValidationError} ->
            {error, {validation_failed, ValidationError}}
    end.
```

## Best Practices

### Configuration Management

1. **Always validate** configurations before use
2. **Provide examples** to users for each transport type
3. **Use schema information** to build configuration UIs
4. **Log validation errors** with full context
5. **Validate early** in the application startup process

### Error Handling

1. **Parse error messages** to provide user-friendly feedback
2. **Suggest corrections** based on validation failures
3. **Provide examples** when validation fails
4. **Document common mistakes** and their solutions

### Development

1. **Use field validation** for individual field checks during development
2. **Access schemas** programmatically for code generation
3. **Test with examples** to ensure they remain valid
4. **Update validation rules** when adding new features

## Testing

The configuration validation system includes comprehensive tests:

```bash
# Run configuration validation tests
rebar3 ct --suite test/erlmcp_config_validation_SUITE

# Run specific test groups
rebar3 ct --suite test/erlmcp_config_validation_SUITE --group stdio_config
rebar3 ct --suite test/erlmcp_config_validation_SUITE --group tcp_config
rebar3 ct --suite test/erlmcp_config_validation_SUITE --group http_config
```

## Integration

The validation system integrates with:
- Transport startup (`start_transport/3`)
- Configuration updates (`update_transport_config/2`)  
- Setup helpers (`start_tcp_setup/3`, `start_http_setup/3`)
- Registry operations (validates before registration)

All transport operations automatically validate configurations and provide meaningful error messages on validation failures.