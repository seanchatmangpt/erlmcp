# Transport Configuration Guide

## Overview

This guide provides detailed configuration examples and patterns for all supported transport types in ErlMCP. Each transport has specific configuration requirements and optional parameters.

## STDIO Transport

The STDIO transport enables communication through standard input/output streams, commonly used for command-line MCP servers.

### Basic Configuration

```erlang
% Simple STDIO configuration
TransportConfig = {stdio, []}.

% With owner process
TransportConfig = {stdio, #{owner => self()}}.
```

### Advanced Configuration

```erlang
% Full STDIO configuration
TransportConfig = {stdio, #{
    owner => self(),
    buffer_size => 8192,
    test_mode => false,
    reader_options => [
        {timeout, 5000},
        {error_handler, fun handle_read_error/1}
    ]
}}.
```

### Configuration Parameters

| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `owner` | `pid()` | Required | Process to receive transport messages |
| `buffer_size` | `pos_integer()` | 8192 | Input buffer size for line reading |
| `test_mode` | `boolean()` | Auto-detected | Enable test mode (disables stdin reader) |
| `reader_options` | `list()` | `[]` | Options for the background reader process |

### Usage Examples

```erlang
% Starting an MCP server with STDIO transport
{ok, Server} = erlmcp_server:start_link({stdio, []}, ServerCapabilities).

% Starting an MCP client with STDIO transport
{ok, Client} = erlmcp_client:start_link({stdio, []}, ClientOptions).
```

## TCP Transport

The TCP transport provides network-based communication with automatic reconnection and connection pooling.

### Basic Configuration

```erlang
% Simple TCP client configuration
TransportConfig = {tcp, #{
    host => "localhost",
    port => 8080,
    owner => self()
}}.
```

### Advanced Configuration

```erlang
% Full TCP configuration with all options
TransportConfig = {tcp, #{
    host => {192, 168, 1, 100},  % IP address or hostname
    port => 8080,
    owner => self(),

    % Connection options
    connect_timeout => 5000,
    keepalive => true,
    nodelay => true,
    buffer_size => 65536,

    % Retry configuration
    max_reconnect_attempts => 10,
    initial_backoff => 1000,
    max_backoff => 60000,

    % SSL/TLS options (for secure connections)
    ssl_options => [
        {verify, verify_peer},
        {cacertfile, "/path/to/cacert.pem"},
        {certfile, "/path/to/cert.pem"},
        {keyfile, "/path/to/key.pem"}
    ]
}}.
```

### Configuration Parameters

| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `host` | `string() \| inet:ip_address()` | Required | Server hostname or IP |
| `port` | `inet:port_number()` | Required | Server port number |
| `owner` | `pid()` | Required | Process to receive transport messages |
| `connect_timeout` | `timeout()` | 5000 | Connection timeout in ms |
| `keepalive` | `boolean()` | false | Enable TCP keepalive |
| `nodelay` | `boolean()` | false | Disable Nagle's algorithm |
| `buffer_size` | `pos_integer()` | 65536 | Socket buffer size |
| `max_reconnect_attempts` | `pos_integer() \| infinity` | infinity | Max reconnection attempts |
| `ssl_options` | `[ssl:tls_option()]` | `[]` | SSL/TLS configuration |

### Usage Examples

```erlang
% TCP server connection
TcpOpts = #{
    host => "mcp-server.example.com",
    port => 3000,
    owner => self(),
    keepalive => true
},
{ok, Client} = erlmcp_client:start_link({tcp, TcpOpts}, ClientCapabilities).

% Secure TCP connection with SSL
SecureTcpOpts = #{
    host => "secure-mcp.example.com",
    port => 443,
    owner => self(),
    ssl_options => [
        {verify, verify_peer},
        {cacertfile, "/etc/ssl/certs/ca-certificates.crt"}
    ]
},
{ok, SecureClient} = erlmcp_client:start_link({tcp, SecureTcpOpts}, ClientCapabilities).
```

## HTTP Transport

The HTTP transport enables MCP communication over HTTP/HTTPS with support for various HTTP methods and authentication.

### Basic Configuration

```erlang
% Simple HTTP POST configuration
TransportConfig = {http, #{
    url => "https://api.example.com/mcp",
    owner => self()
}}.
```

### Advanced Configuration

```erlang
% Full HTTP configuration
TransportConfig = {http, #{
    url => "https://api.example.com/mcp/v1",
    owner => self(),

    % HTTP method and headers
    method => post,  % or get
    headers => [
        {"Authorization", "Bearer " ++ ApiToken},
        {"User-Agent", "MyApp/1.0"},
        {"X-API-Version", "2025-06-18"}
    ],

    % Timeouts
    timeout => 30000,
    connect_timeout => 5000,

    % Retry configuration
    max_retries => 3,
    retry_delay => 1000,

    % Connection pooling
    pool_size => 5,

    % SSL options for HTTPS
    ssl_options => [
        {verify, verify_peer},
        {depth, 2},
        {ciphers, [
            "ECDHE-RSA-AES128-GCM-SHA256",
            "ECDHE-RSA-AES256-GCM-SHA384"
        ]}
    ],

    % HTTP client options
    http_options => [
        {autoredirect, true},
        {relaxed, true}
    ]
}}.
```

### Configuration Parameters

| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `url` | `binary() \| string()` | Required | HTTP endpoint URL |
| `owner` | `pid()` | Required | Process to receive transport messages |
| `method` | `get \| post` | `post` | HTTP method to use |
| `headers` | `[{string(), string()}]` | Basic headers | Custom HTTP headers |
| `timeout` | `timeout()` | 30000 | Request timeout in ms |
| `connect_timeout` | `timeout()` | 5000 | Connection timeout in ms |
| `max_retries` | `non_neg_integer()` | 3 | Maximum retry attempts |
| `retry_delay` | `pos_integer()` | 1000 | Base retry delay in ms |
| `pool_size` | `pos_integer()` | 5 | Connection pool size |
| `ssl_options` | `[ssl:tls_option()]` | `[]` | SSL/TLS configuration |
| `http_options` | `[term()]` | `[]` | HTTP client options |

### Usage Examples

```erlang
% HTTP API with authentication
ApiOpts = #{
    url => "https://mcp-api.example.com/v1/messages",
    owner => self(),
    method => post,
    headers => [
        {"Authorization", "Bearer " ++ get_api_token()},
        {"Content-Type", "application/json"}
    ],
    timeout => 15000
},
{ok, HttpClient} = erlmcp_client:start_link({http, ApiOpts}, ClientCapabilities).

% GET-based HTTP transport with query parameters
GetOpts = #{
    url => "https://webhook.site/unique-id",
    owner => self(),
    method => get,
    max_retries => 5,
    retry_delay => 2000
},
{ok, GetClient} = erlmcp_client:start_link({http, GetOpts}, ClientCapabilities).
```

## Server-Side Transport Configuration

When creating MCP servers, transport configuration follows similar patterns:

### STDIO Server
```erlang
% STDIO server (most common for CLI tools)
ServerOpts = #{
    capabilities => ServerCapabilities,
    transport => {stdio, []}
},
{ok, Server} = erlmcp_server:start_link(ServerOpts).
```

### TCP Server
```erlang
% TCP server listening on port 8080
ServerOpts = #{
    capabilities => ServerCapabilities,
    transport => {tcp_server, #{
        port => 8080,
        acceptor_pool_size => 10,
        socket_options => [
            {reuseaddr, true},
            {keepalive, true}
        ]
    }}
},
{ok, Server} = erlmcp_server:start_link(ServerOpts).
```

### HTTP Server
```erlang
% HTTP server using Cowboy
ServerOpts = #{
    capabilities => ServerCapabilities,
    transport => {http_server, #{
        port => 3000,
        routes => [
            {"/mcp", mcp_handler, []}
        ],
        ssl => false
    }}
},
{ok, Server} = erlmcp_server:start_link(ServerOpts).
```

## Configuration Schemas

### Environment-Based Configuration

```erlang
% Load configuration from application environment
get_transport_config() ->
    case application:get_env(my_app, mcp_transport) of
        {ok, {stdio, Opts}} ->
            {stdio, Opts};
        {ok, {tcp, #{host := Host, port := Port} = Opts}} ->
            {tcp, Opts#{owner => self()}};
        {ok, {http, #{url := Url} = Opts}} ->
            {http, Opts#{owner => self()}};
        undefined ->
            {stdio, []}  % Default to STDIO
    end.
```

### Dynamic Configuration

```erlang
% Select transport based on runtime conditions
select_transport_config(Mode) ->
    case Mode of
        development ->
            {stdio, []};
        testing ->
            {stdio, #{test_mode => true}};
        production ->
            {tcp, #{
                host => os:getenv("MCP_HOST", "localhost"),
                port => list_to_integer(os:getenv("MCP_PORT", "8080")),
                owner => self(),
                keepalive => true,
                max_reconnect_attempts => 10
            }}
    end.
```

## Error Handling Patterns

### Transport-Specific Error Handling

```erlang
handle_transport_error({error, Reason}, Transport) ->
    case {Transport, Reason} of
        {{stdio, _}, {read_error, eof}} ->
            logger:info("STDIO transport closed normally"),
            shutdown;
        {{tcp, _}, {connection_lost, Reason}} ->
            logger:warning("TCP connection lost: ~p", [Reason]),
            retry;
        {{http, _}, {http_error, 429, _}} ->
            logger:warning("HTTP rate limited, backing off"),
            backoff;
        {_, _} ->
            logger:error("Transport error: ~p", [Reason]),
            error
    end.
```

### Graceful Fallback Configuration

```erlang
% Try multiple transports in order of preference
try_transports([]) ->
    {error, no_transport_available};
try_transports([Config | Rest]) ->
    case start_transport(Config) of
        {ok, Transport} ->
            {ok, Transport};
        {error, Reason} ->
            logger:warning("Transport ~p failed: ~p", [Config, Reason]),
            try_transports(Rest)
    end.

get_fallback_transports() ->
    [
        {tcp, #{host => "primary.example.com", port => 8080, owner => self()}},
        {tcp, #{host => "backup.example.com", port => 8080, owner => self()}},
        {http, #{url => "https://api.example.com/mcp", owner => self()}},
        {stdio, []}
    ].
```

## Best Practices

### Configuration Management
1. **Externalize Configuration**: Use application environment or config files
2. **Validate Early**: Check configuration parameters during startup
3. **Provide Defaults**: Ensure system works with minimal configuration
4. **Environment Awareness**: Adjust settings based on deployment environment

### Security Considerations
1. **SSL/TLS**: Always use encryption for network transports in production
2. **Authentication**: Include proper authentication headers for HTTP transport
3. **Certificate Validation**: Configure proper certificate verification
4. **Credential Management**: Keep sensitive credentials out of source code

### Performance Optimization
1. **Connection Pooling**: Use appropriate pool sizes for HTTP transport
2. **Buffer Sizes**: Tune buffer sizes based on expected message sizes
3. **Timeout Values**: Set realistic timeouts for your use case
4. **Retry Logic**: Implement exponential backoff for failed connections

### Monitoring and Observability
1. **Logging**: Enable appropriate logging levels for different environments
2. **Metrics**: Track connection success rates and response times
3. **Health Checks**: Implement transport health checking mechanisms
4. **Alerts**: Set up monitoring for transport failures and degraded performance
