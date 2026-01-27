# Transport Validation Quick Reference

## API

```erlang
erlmcp:validate_transport_config(Config :: map()) -> ok | {error, {validation_error, term()}}
```

## STDIO Transport

```erlang
#{
    type => stdio,              % REQUIRED
    server_id => atom(),        % optional
    buffer_size => pos_integer(),  % optional
    read_timeout => timeout()   % optional (pos_integer() | infinity)
}
```

**Example:**
```erlang
erlmcp:validate_transport_config(#{
    type => stdio,
    buffer_size => 8192
}).
% => ok
```

## TCP Transport

```erlang
#{
    type => tcp,                % REQUIRED
    host => binary() | string(),  % REQUIRED (non-empty)
    port => 1..65535,           % REQUIRED
    server_id => atom(),        % optional
    keepalive => boolean(),     % optional
    connect_timeout => timeout(),  % optional
    recv_timeout => timeout(),  % optional
    send_timeout => timeout(),  % optional
    nodelay => boolean(),       % optional
    buffer_size => pos_integer(),  % optional
    max_connections => pos_integer()  % optional
}
```

**Example:**
```erlang
erlmcp:validate_transport_config(#{
    type => tcp,
    host => <<"localhost">>,
    port => 8080,
    keepalive => true
}).
% => ok
```

## HTTP Transport

```erlang
#{
    type => http,               % REQUIRED
    url => binary() | string(), % REQUIRED (http:// or https://)
    server_id => atom(),        % optional
    method => method(),         % optional (GET|POST|PUT|DELETE|PATCH|HEAD|OPTIONS)
    headers => headers(),       % optional (map or [{K,V}])
    timeout => timeout(),       % optional
    max_redirects => non_neg_integer(),  % optional
    verify_ssl => boolean(),    % optional
    compression => boolean()    % optional
}
```

**Example:**
```erlang
erlmcp:validate_transport_config(#{
    type => http,
    url => <<"https://api.example.com/mcp">>,
    method => <<"POST">>,
    headers => #{<<"Content-Type">> => <<"application/json">>}
}).
% => ok
```

## Common Errors

```erlang
% Missing required field
{error, {validation_error, {missing_required_fields, [host]}}}

% Invalid value
{error, {validation_error, {invalid_port, 70000}}}

% Unknown field
{error, {validation_error, {unknown_fields, [bad_field]}}}

% Wrong type
{error, {validation_error, {unsupported_transport_type, websocket}}}

% Missing type field
{error, {validation_error, missing_required_field_type}}
```

## Automatic Validation

`start_transport/3` validates automatically:

```erlang
erlmcp:start_transport(my_transport, tcp, #{
    host => <<"localhost">>,
    port => 8080
}).
% Validates config before starting transport
```

## Testing

```bash
rebar3 eunit --module=erlmcp_config_validation_tests
```

## Full Documentation

See `/Users/sac/erlmcp/docs/TRANSPORT_VALIDATION.md`
