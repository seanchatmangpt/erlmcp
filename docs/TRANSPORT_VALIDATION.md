# Transport Configuration Validation

## Overview

The `erlmcp` module provides comprehensive configuration validation for all transport types (stdio, tcp, http). This ensures that transport configurations are validated before use, preventing runtime errors and providing clear error messages.

## API

### Main Validation Function

```erlang
-spec validate_transport_config(map()) -> ok | {error, {validation_error, term()}}.
```

Validates transport configuration based on the transport type specified in the `type` field.

## Supported Transport Types

### 1. STDIO Transport

**Required Fields:**
- `type` - Must be `stdio`

**Optional Fields:**
- `server_id` (atom) - Associated server identifier
- `buffer_size` (positive integer) - I/O buffer size in bytes
- `read_timeout` (positive integer | infinity) - Read timeout in milliseconds

**Example:**
```erlang
Config = #{
    type => stdio,
    server_id => my_server,
    buffer_size => 8192,
    read_timeout => 5000
}.
```

### 2. TCP Transport

**Required Fields:**
- `type` - Must be `tcp`
- `host` (binary | string) - Hostname or IP address (non-empty)
- `port` (integer) - Port number (1-65535)

**Optional Fields:**
- `server_id` (atom) - Associated server identifier
- `keepalive` (boolean) - Enable TCP keepalive
- `connect_timeout` (positive integer | infinity) - Connection timeout in milliseconds
- `recv_timeout` (positive integer | infinity) - Receive timeout in milliseconds
- `send_timeout` (positive integer | infinity) - Send timeout in milliseconds
- `nodelay` (boolean) - Enable TCP_NODELAY (disable Nagle's algorithm)
- `buffer_size` (positive integer) - Socket buffer size in bytes
- `max_connections` (positive integer) - Maximum concurrent connections

**Example:**
```erlang
Config = #{
    type => tcp,
    host => <<"localhost">>,
    port => 8080,
    server_id => my_server,
    keepalive => true,
    connect_timeout => 5000,
    recv_timeout => 30000,
    send_timeout => 5000,
    nodelay => true,
    buffer_size => 4096,
    max_connections => 100
}.
```

### 3. HTTP Transport

**Required Fields:**
- `type` - Must be `http`
- `url` (binary | string) - HTTP/HTTPS URL (must start with http:// or https://)

**Optional Fields:**
- `server_id` (atom) - Associated server identifier
- `method` (binary | atom) - HTTP method (GET, POST, PUT, DELETE, PATCH, HEAD, OPTIONS)
- `headers` (map | list of tuples) - HTTP headers
- `timeout` (positive integer | infinity) - Request timeout in milliseconds
- `max_redirects` (non-negative integer) - Maximum number of redirects to follow
- `verify_ssl` (boolean) - Enable SSL certificate verification
- `compression` (boolean) - Enable response compression

**Example:**
```erlang
Config = #{
    type => http,
    url => <<"https://api.example.com/mcp">>,
    server_id => my_server,
    method => <<"POST">>,
    headers => #{<<"Content-Type">> => <<"application/json">>},
    timeout => 30000,
    max_redirects => 5,
    verify_ssl => true,
    compression => true
}.
```

## Validation Rules

### Field Validation

1. **Required Fields**: All required fields must be present
2. **Unknown Fields**: No unknown fields are allowed
3. **Type Checking**: All field values must be of the correct type

### Value Validation

#### Host Validation
- Must be a non-empty binary or string
- Cannot be `undefined`

#### Port Validation
- Must be an integer between 1 and 65535 (inclusive)

#### URL Validation
- Must start with `http://` or `https://`
- Can be binary or string

#### HTTP Method Validation
- Binary: `<<"GET">>`, `<<"POST">>`, `<<"PUT">>`, `<<"DELETE">>`, `<<"PATCH">>`, `<<"HEAD">>`, `<<"OPTIONS">>`
- Atom: `get`, `post`, `put`, `delete`, `patch`, `head`, `options`

#### Headers Validation
- Must be a map or list of `{Key, Value}` tuples
- Keys and values must be binaries or strings

#### Timeout Validation
- Must be a positive integer or the atom `infinity`
- Field-specific timeouts include: `connect_timeout`, `recv_timeout`, `send_timeout`, `read_timeout`, `timeout`

#### Buffer Size Validation
- Must be a positive integer

#### Max Connections Validation
- Must be a positive integer

#### Max Redirects Validation
- Must be a non-negative integer (0 or greater)

#### Boolean Validation
- Must be `true` or `false`
- Fields: `keepalive`, `nodelay`, `verify_ssl`, `compression`

## Error Messages

All validation errors are returned in the format:
```erlang
{error, {validation_error, ErrorDetail}}
```

### Common Error Types

1. **Missing Required Fields**
   ```erlang
   {error, {validation_error, {missing_required_fields, [field1, field2]}}}
   ```

2. **Unknown Fields**
   ```erlang
   {error, {validation_error, {unknown_fields, [unknown_field1, unknown_field2]}}}
   ```

3. **Missing Type**
   ```erlang
   {error, {validation_error, missing_required_field_type}}
   ```

4. **Unsupported Transport Type**
   ```erlang
   {error, {validation_error, {unsupported_transport_type, websocket}}}
   ```

5. **Invalid Field Value**
   ```erlang
   {error, {validation_error, {invalid_port, 70000}}}
   {error, {validation_error, {invalid_host, <<>>}}}
   {error, {validation_error, {invalid_url, <<"ftp://example.com">>}}}
   {error, {validation_error, {invalid_http_method, <<"INVALID">>}}}
   {error, {validation_error, {invalid_timeout, Field, Value}}}
   {error, {validation_error, {invalid_buffer_size, -1}}}
   {error, {validation_error, {invalid_boolean, Field, "true"}}}
   ```

6. **Config Must Be Map**
   ```erlang
   {error, {validation_error, config_must_be_map}}
   ```

## Usage Examples

### Successful Validation

```erlang
% STDIO
1> erlmcp:validate_transport_config(#{type => stdio}).
ok

% TCP
2> erlmcp:validate_transport_config(#{
    type => tcp,
    host => <<"localhost">>,
    port => 8080
}).
ok

% HTTP
3> erlmcp:validate_transport_config(#{
    type => http,
    url => <<"https://example.com/api">>
}).
ok
```

### Failed Validation

```erlang
% Missing required field
4> erlmcp:validate_transport_config(#{type => tcp, host => <<"localhost">>}).
{error,{validation_error,{missing_required_fields,[port]}}}

% Invalid port
5> erlmcp:validate_transport_config(#{
    type => tcp,
    host => <<"localhost">>,
    port => 70000
}).
{error,{validation_error,{invalid_port,70000}}}

% Invalid URL
6> erlmcp:validate_transport_config(#{
    type => http,
    url => <<"ftp://example.com">>
}).
{error,{validation_error,{invalid_url,<<"ftp://example.com">>}}}

% Unknown field
7> erlmcp:validate_transport_config(#{
    type => stdio,
    unknown_field => value
}).
{error,{validation_error,{unknown_fields,[unknown_field]}}}
```

## Integration with start_transport/3

The `start_transport/3` function automatically validates configurations before starting the transport:

```erlang
% This will validate the config before starting
erlmcp:start_transport(my_transport, tcp, #{
    host => <<"localhost">>,
    port => 8080
}).
```

If validation fails, `start_transport/3` will return the validation error without attempting to start the transport.

## Helper Functions (Stubs for Future Poolboy Integration)

### transport_config_to_poolboy_args/1

Converts a validated transport configuration to poolboy worker arguments. Currently implemented as stubs for all three transport types.

```erlang
-spec transport_config_to_poolboy_args(map()) -> {ok, list()} | {error, term()}.
```

### default_pool_config/1

Returns default poolboy pool configuration for a given transport type.

```erlang
-spec default_pool_config(transport_type()) -> list().
```

**Default Pool Sizes:**
- STDIO: 5 workers, max overflow 10
- TCP: 10 workers, max overflow 20
- HTTP: 20 workers, max overflow 40

These functions will be fully implemented when poolboy integration is added.

## Testing

Comprehensive tests are available in `test/erlmcp_config_validation_tests.erl` covering:

- Valid configurations for all transport types
- Missing required fields
- Invalid field values
- Unknown fields
- Edge cases (infinity timeouts, string vs binary formats)
- General validation errors

Run tests with:
```bash
rebar3 eunit --module=erlmcp_config_validation_tests
```

## Implementation Details

### Architecture

The validation system is organized hierarchically:

1. **Main Entry Point**: `validate_transport_config/1`
   - Dispatches to transport-specific validators based on `type` field

2. **Transport-Specific Validators**:
   - `validate_stdio_specific/1`
   - `validate_tcp_specific/1`
   - `validate_http_specific/1`

3. **Common Validation**:
   - `validate_fields/3` - Checks required/optional/unknown fields
   - `run_validations/1` - Runs a list of validation functions

4. **Value Validators**:
   - `validate_host/1`
   - `validate_port/1`
   - `validate_url/1`
   - `validate_http_method/1`
   - `validate_headers/1`
   - `validate_timeout/2`
   - `validate_buffer_size/1`
   - `validate_max_connections/1`
   - `validate_max_redirects/1`
   - `validate_boolean/2`

### Design Principles

1. **Fail Fast**: Validation stops at the first error encountered
2. **Clear Error Messages**: Each error includes the field name and invalid value
3. **Type Safety**: Dialyzer specs ensure type correctness
4. **Extensible**: Easy to add new transport types or validation rules
5. **Optional Fields**: All optional fields default to `undefined` and are validated only if present

## Future Enhancements

1. **Poolboy Integration**: Fully implement the poolboy helper functions
2. **Custom Validators**: Allow users to register custom validation functions
3. **Config Transformation**: Add functions to normalize/transform configs
4. **Validation Warnings**: Support warnings for deprecated or discouraged configurations
5. **Schema Export**: Generate JSON Schema or similar from validation rules
