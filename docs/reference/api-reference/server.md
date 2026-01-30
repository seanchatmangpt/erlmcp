# Server API Reference

The erlmcp server API provides a complete interface for implementing MCP servers. This document covers all server functions, tool registration, and request handling.

## API Overview

### Server Types
```erlang
% Server reference
-type server_ref() :: pid() | atom().

% Server configuration record
-record(server_config, {
    port,              % Server port
    transport,         % Transport type
    tools = [],       % Available tools
    registry_ref,     % Registry reference
    request_id = 1,   % Request counter
    metrics,         % Performance metrics
    limiters         % Rate limiters
}).

% Tool interface
-type tool_spec() :: #{
    name => binary(),
    handler => module(),
    config => map()
}.
```

## Core Functions

### `start/1` - Start Server

```erlang
-spec start(Config :: map()) -> {ok, Ref :: server_ref()} | {error, Reason :: term()}.
```

Start an MCP server instance.

**Configuration Options:**
| Key | Type | Default | Description |
|-----|------|---------|-------------|
| `port` | integer() | 8080 | Server port |
| `transport` | atom() | tcp | Transport type (tcp, http, stdio) |
| `host` | string() | "0.0.0.0" | Bind host |
| `tools` | [tool_spec()] | [] | List of available tools |
| `timeout` | integer() | 5000 | Request timeout |
| `ssl` | boolean() | false | Enable SSL/TLS |
| `ssl_opts` | list() | [] | SSL options |
| `max_connections` | integer() | 1000 | Max concurrent connections |
| `log_level` | atom() | info | Logging level |

**Example:**
```erlang
% Basic TCP server
Config = #{
    port => 8080,
    transport => tcp,
    host => "0.0.0.0"
},
{ok, Server} = erlmcp_server:start(Config).

% HTTP server with SSL
Config = #{
    port => 8081,
    transport => http,
    ssl => true,
    ssl_opts => [
        {certfile, "/path/to/cert.pem"},
        {keyfile, "/path/to/key.pem"}
    ]
},
{ok, Server} = erlmcp_server:start(Config).

% stdio server
Config = #{
    transport => stdio,
    tools => [
        #{
            name => "calculator",
            handler => my_calculator
        }
    ]
},
{ok, Server} = erlmcp_server:start(Config).
```

### `stop/1` - Stop Server

```erlang
-spec stop(Ref :: server_ref()) -> ok.
```

Stop the server and clean up resources.

**Example:**
```erlang
erlmcp_server:stop(Server).
```

### `register_tool/2` - Register Tool

```erlang
-spec register_tool(Name :: binary(), Module :: module()) -> ok | {error, Reason :: term()}.
```

Register a tool with the server.

**Example:**
```erlang
% Register a simple tool
erlmcp_server:register_tool(<<"calculator">>, my_calculator).

% Register with configuration
Config = #{
    timeout => 10000,
    retries => 3
},
erlmcp_server:register_tool(<<"weather">>, weather_tool, Config).
```

### `unregister_tool/1` - Unregister Tool

```erlang
-spec unregister_tool(Name :: binary()) -> ok | {error, Reason :: term()}.
```

Unregister a tool from the server.

**Example:**
```erlang
erlmcp_server:unregister_tool(<<"weather">>).
```

### `tools/0` - List Registered Tools

```erlang
-spec tools() -> [binary()].
```

Get list of all registered tools.

**Example:**
```erlang
AvailableTools = erlmcp_server:tools(),
io:format("Available tools: ~p~n", [AvailableTools]).
```

## Tool Management

### `add_tool/1` - Add Tool Specification

```erlang
-spec add_tool(Spec :: tool_spec()) -> ok | {error, Reason :: term()}.
```

Add a new tool specification.

**Example:**
```erlang
ToolSpec = #{
    name => "calculator",
    handler => my_calculator,
    config => #{
        timeout => 5000,
        retries => 3
    }
},
erlmcp_server:add_tool(ToolSpec).
```

### `remove_tool/1` - Remove Tool Specification

```erlang
-spec remove_tool(Name :: binary()) -> ok | {error, Reason :: term()}.
```

Remove a tool specification.

**Example:**
```erlang
erlmcp_server:remove_tool("calculator").
```

### `get_tool/1` - Get Tool Specification

```erlang
-spec get_tool(Name :: binary()) -> {ok, Spec :: tool_spec()} | {error, not_found}.
```

Get tool specification by name.

**Example:**
```erlang
case erlmcp_server:get_tool("calculator") of
    {ok, Spec} -> io:format("Tool spec: ~p~n", [Spec]);
    {error, not_found} -> io:format("Tool not found~n")
end.
```

### `update_tool/2` - Update Tool Specification

```erlang
-spec update_tool(Name :: binary(), NewSpec :: tool_spec()) -> ok | {error, Reason :: term()}.
```

Update an existing tool specification.

**Example:**
```erlang
NewSpec = #{
    name => "calculator",
    handler => my_calculator_v2,
    config => #{timeout => 10000}
},
erlmcp_server:update_tool("calculator", NewSpec).
```

## Request Handling

### `handle_request/2` - Handle Request

```erlang
-spec handle_request(Request :: map(), State :: term()) ->
                           {ok, Response :: map(), NewState :: term()} |
                           {error, Reason :: term(), NewState :: term()}.
```

Handle an incoming MCP request.

**Request Format:**
```erlang
Request = #{
    jsonrpc => "2.0",
    id => binary(),
    method => binary(),
    params => map() | list()
}.
```

**Example:**
```erlang
case erlmcp_server:handle_request(Request, State) of
    {ok, Response, NewState} ->
        send_response(Response),
        {noreply, NewState};
    {error, Reason, NewState} ->
        send_error(Reason),
        {noreply, NewState}
end.
```

### `call_tool/3` - Call Tool Function

```erlang
-spec call_tool(ToolName :: binary(), Args :: map(), State :: term()) ->
                      {ok, Result :: map()} | {error, Reason :: term()}.
```

Call a tool function directly.

**Example:**
```erlang
case erlmcp_server:call_tool("calculator", #{expression => "2 + 2"}, State) of
    {ok, Result} -> io:format("Result: ~p~n", [Result]);
    {error, Reason} -> io:format("Error: ~p~n", [Reason])
end.
```

### `validate_request/2` - Validate Request

```erlang
-spec validate_request(Request :: map(), State :: term()) ->
                              {ok, ValidRequest :: map()} | {error, Reason :: term()}.
```

Validate an incoming request.

**Example:**
```erlang
case erlmcp_server:validate_request(Request, State) of
    {ok, ValidRequest} ->
        handle_valid_request(ValidRequest);
    {error, {missing_field, Field}} ->
        send_error(#{
            code => -32602,
            message => "Invalid params",
            data => #{missing_field => Field}
        })
end.
```

## State Management

### `state/1` - Get Server State

```erlang
-spec state(Ref :: server_ref()) -> {ok, State :: map()} | {error, Reason :: term()}.
```

Get current server state.

**Example:**
```erlang
{ok, State} = erlmcp_server:state(Server),
io:format("Server state: ~p~n", [State]).
```

### `update_state/2` - Update State

```erlang
-spec update_state(Ref :: server_ref(), NewState :: map()) ->
                         {ok, Updated :: boolean()} | {error, Reason :: term()}.
```

Update server state.

**Example:**
```erlang
NewState = State#server_config{timeout = 10000},
{ok, Updated} = erlmcp_server:update_state(Server, NewState).
```

## Monitoring and Metrics

### `stats/0` - Get Server Statistics

```erlang
-spec stats() -> map().
```

Get server statistics.

**Example:**
```erlang
Statistics = erlmcp_server:stats(),
io:format("Stats: ~p~n", [Statistics]).
```

### `metrics/1` - Get Metric Value

```erlang
-spec metrics(Name :: binary()) -> {ok, Value :: number()} | {error, not_found}.
```

Get specific metric value.

**Example:**
```erlang
case erlmcp_server:metrics("requests_per_second") of
    {ok, Value} -> io:format("RPS: ~p~n", [Value]);
    {error, not_found} -> io:format("Metric not found~n")
end.
```

### `reset_metrics/0` - Reset Metrics

```erlang
-spec reset_metrics() -> ok.
```

Reset all server metrics.

**Example:**
```erlang
erlmcp_server:reset_metrics().
```

## Rate Limiting

### `set_rate_limit/2` - Set Rate Limit

```erlang
-spec set_rate_limit(ToolName :: binary(), Limit :: {integer(), integer()}) -> ok.
```

Set rate limit for a tool.

**Format:** `{Requests, Seconds}` - requests per seconds

**Example:**
```erlang
% 10 requests per minute
erlmcp_server:set_rate_limit("calculator", {10, 60}).

% 100 requests per hour
erlmcp_server:set_rate_limit("weather", {100, 3600}).
```

### `check_rate_limit/1` - Check Rate Limit

```erlang
-spec check_rate_limit(ToolName :: binary()) -> {ok, allowed} | {error, exceeded}.
```

Check if rate limit allows request.

**Example:**
```erlang
case erlmcp_server:check_rate_limit("calculator") of
    {ok, allowed} -> proceed_with_request();
    {error, exceeded} -> send_rate_limit_error()
end.
```

## Authentication and Authorization

### `authenticate/2` - Authenticate Request

```erlang
-spec authenticate(Request :: map(), Credentials :: map()) ->
                          {ok, User :: map()} | {error, Reason :: term()}.
```

Authenticate incoming request.

**Example:**
```erlang
case erlmcp_server:authenticate(Request, AuthData) of
    {ok, User} -> authorize_request(User, Request);
    {error, unauthenticated} -> send_auth_error()
end.
```

### `authorize/2` - Authorize Request

```erlang
-spec authorize(User :: map(), Request :: map()) -> boolean().
```

Check if user is authorized for request.

**Example:**
```erlang
case erlmcp_server:authorize(User, Request) of
    true -> proceed_with_request(Request);
    false -> send_permission_error()
end.
```

## Configuration Management

### `reload_config/0` - Reload Configuration

```erlang
-spec reload_config() -> ok | {error, Reason :: term()}.
```

Reload server configuration from file.

**Example:**
```erlang
case erlmcp_server:reload_config() of
    ok -> io:format("Config reloaded~n");
    {error, Reason} -> io:format("Reload failed: ~p~n", [Reason])
end.
```

### `save_config/1` - Save Configuration

```erlang
-spec save_config(Config :: map()) -> ok | {error, Reason :: term()}.
```

Save configuration to file.

**Example:**
```erlang
Config = #{
    port => 8080,
    transport => tcp,
    tools => [...]
},
case erlmcp_server:save_config(Config) of
    ok -> io:format("Config saved~n");
    {error, Reason} -> io:format("Save failed: ~p~n", [Reason])
end.
```

## Health Checks

### `health_check/0` - Perform Health Check

```erlang
-spec health_check() -> {ok, Status :: map()} | {error, Reason :: term()}.
```

Perform server health check.

**Example:**
```erlang
case erlmcp_server:health_check() of
    {ok, Status} -> io:format("Status: ~p~n", [Status]);
    {error, Reason} -> io:format("Health check failed: ~p~n", [Reason])
end.
```

### `is_healthy/0` - Check Health Status

```erlang
-spec is_healthy() -> boolean().
```

Quick health status check.

**Example:**
```erlang
case erlmcp_server:is_healthy() of
    true -> io:format("Server is healthy~n");
    false -> io:format("Server needs attention~n")
end.
```

## Event Handling

### `subscribe/2` - Subscribe to Events

```erlang
-spec subscribe(Event :: binary(), Pid :: pid()) -> ok.
```

Subscribe to server events.

**Example:**
```erlang
erlmcp_server:subscribe("tool_register", self()).

receive
    {tool_register, ToolName} -> io:format("Tool registered: ~p~n", [ToolName])
end.
```

### `unsubscribe/1` - Unsubscribe from Events

```erlang
-spec unsubscribe(Event :: binary()) -> ok.
```

Unsubscribe from server events.

**Example:**
```erlang
erlmcp_server:unsubscribe("tool_register").
```

### `publish/2` - Publish Event

```erlang
-spec publish(Event :: binary(), Data :: term()) -> ok.
```

Publish server event.

**Example:**
```erlang
erlmcp_server:publish("tool_usage", #{
    tool => "calculator",
    user => "user1",
    timestamp => erlang:system_time(millisecond)
}).
```

## Tool Callback Interface

### Tool Behavior
```erlang
-callback init(Config :: map()) -> {ok, State :: term()} | {error, Reason :: term()}.
-callback call_tool(ToolName :: binary(), Args :: map(), State :: term()) ->
                        {ok, Result :: map()} | {error, Reason :: term()}.
-callback terminate(Reason :: term(), State :: term()) -> ok.
```

### Example Tool Implementation
```erlang
-module(my_tool).
-behaviour(erlmcp_server).

-export([init/1, call_tool/3, terminate/2]).

init(Config) ->
    % Initialize tool state
    {ok, #{config => Config, cache => #{}}}.

call_tool("calculate", Args, State) ->
    Expression = maps:get(expression, Args),
    Result = calculate_expression(Expression),
    {ok, #{content => [#{type => text, text => Result}]}};

call_tool("history", _Args, State) ->
    History = maps:get(history, State, []),
    {ok, #{history => History}}.

terminate(_Reason, _State) ->
    ok.

calculate_expression(Expression) ->
    % Simplified calculation
    case erl_eval:expr_list(parse_expression(Expression), []) of
        {value, Result, _} -> float_to_binary(Result, [{decimals, 2}]);
        Error -> "Error: " ++ atom_to_list(Error)
    end.
```

## Error Handling

### Error Responses
```erlang
% Standard error response
ErrorResponse = #{
    jsonrpc => "2.0",
    id => RequestId,
    error => #{
        code => ErrorCode,
        message => ErrorMessage,
        data => ErrorData
    }
}.
```

### Common Error Codes
| Code | Message | Description |
|------|---------|-------------|
| -32600 | Parse Error | Invalid JSON |
| -32601 | Method Not Found | Unknown method |
| -32602 | Invalid Params | Invalid parameters |
| -32603 | Internal Error | Internal error |
| -32604 | Tool Not Found | Tool not registered |
| -32605 | Permission Denied | Authorization failed |

## Examples

### Basic Server Example
```erlang
% Start server
Config = #{
    port => 8080,
    transport => tcp,
    tools => [
        #{
            name => "calculator",
            handler => my_calculator
        }
    ]
},
{ok, Server} = erlmcp_server:start(Config).

% Register additional tool
ToolSpec = #{
    name => "weather",
    handler => weather_tool,
    config => #{timeout => 10000}
},
erlmcp_server:add_tool(ToolSpec).

% Check server health
case erlmcp_server:health_check() of
    {ok, Status} -> io:format("Server healthy: ~p~n", [Status]);
    {error, Reason} -> io:format("Server unhealthy: ~p~n", [Reason])
end.

% Stop server
erlmcp_server:stop(Server).
```

### Advanced Server Example
```erlang
% With authentication and rate limiting
start_secure_server() ->
    Config = #{
        port => 8080,
        transport => tcp,
        auth_method => jwt,
        jwt_secret => "your-secret",
        rate_limits => [
            {<<"calculator">>, {100, 60}},  % 100/min
            {<<"weather">>, {1000, 3600}}  % 1000/hour
        ],
        tools => [
            #{
                name => "calculator",
                handler => secure_calculator
            },
            #{
                name => "weather",
                handler => weather_tool,
                auth_required => true
            }
        ]
    },
    erlmcp_server:start(Config).

% Tool with state management
handle_request_with_state(Request, State) ->
    case erlmcp_server:validate_request(Request, State) of
        {ok, ValidRequest} ->
            case erlmcp_server:check_rate_limit(maps:get(tool, ValidRequest)) of
                {ok, allowed} ->
                    erlmcp_server:call_tool(maps:get(tool, ValidRequest),
                                            maps:get(args, ValidRequest),
                                            State);
                {error, exceeded} ->
                    {error, rate_limit_exceeded}
            end;
        {error, Reason} ->
            {error, Reason}
    end.
```

---

**Related Documentation:**
- [Client API](./client.md) - Client-side implementation
- [Transport API](./transport.md) - Transport layer interfaces
- [Configuration Schema](../../reference/configuration/schema.md) - Configuration options