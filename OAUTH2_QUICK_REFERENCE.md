# OAuth2 Token Introspection - Quick Reference Guide

## Setup

### 1. Configure OAuth2 Settings

```erlang
%% In sys.config or via application:set_env/3
{erlmcp_core, [
    {auth, #{
        oauth2 => #{
            enabled => true,
            introspect_url => <<"https://auth.example.com/oauth2/introspect">>,
            client_id => <<"your_client_id">>,
            client_secret => <<"your_client_secret">>,
            timeout => 5000,           % Connection timeout (default: 5000ms)
            response_timeout => 10000  % Response timeout (default: 10000ms)
        }
    }}
]}
```

### 2. Start erlmcp_auth Server

```erlang
%% Automatic startup via application supervisor
{ok, _} = application:ensure_all_started(erlmcp_core).

%% Or manual startup with config
Config = #{
    oauth2 => #{
        enabled => true,
        introspect_url => <<"https://auth.example.com/oauth2/introspect">>,
        client_id => <<"client_123">>,
        client_secret => <<"secret_abc">>
    }
},
{ok, Pid} = erlmcp_auth:start_link(Config).
```

## Usage

### Validate OAuth2 Token

```erlang
%% Basic validation
Token = <<"eyJhbGciOiJSUzI1NiIsInR5cCI6IkpXVCJ9...">>,
case erlmcp_auth:validate_oauth2_token(Token) of
    {ok, TokenInfo} ->
        UserId = maps:get(<<"user_id">>, TokenInfo),
        logger:info("Token valid for user: ~p", [UserId]),
        % Use TokenInfo for authorization decisions
        {ok, UserId};

    {error, token_invalid} ->
        logger:warning("Token is inactive or revoked"),
        {error, unauthorized};

    {error, token_expired} ->
        logger:warning("Token has expired"),
        {error, token_expired};

    {error, token_not_yet_valid} ->
        logger:warning("Token not yet valid (nbf in future)"),
        {error, unauthorized};

    {error, Reason} ->
        logger:error("Token validation failed: ~p", [Reason]),
        {error, unauthorized}
end.
```

### Authenticate with OAuth2

```erlang
%% Full authentication flow (creates session)
Credentials = #{
    token => <<"eyJhbGciOiJSUzI1NiIsInR5cCI6IkpXVCJ9...">>,
    ip_address => {127, 0, 0, 1}  % Optional, for rate limiting
},

case erlmcp_auth:authenticate(oauth2, Credentials) of
    {ok, SessionId} ->
        logger:info("Authentication successful, session: ~p", [SessionId]),
        {ok, SessionId};

    {error, rate_limited} ->
        logger:warning("Rate limit exceeded"),
        {error, too_many_requests};

    {error, Reason} ->
        logger:error("Authentication failed: ~p", [Reason]),
        {error, unauthorized}
end.
```

### Revoke Token

```erlang
%% Revoke a compromised or logout token
Token = <<"eyJhbGciOiJSUzI1NiIsInR5cCI6IkpXVCJ9...">>,
ok = erlmcp_auth:revoke_token(Token),
logger:info("Token revoked successfully").

%% Future validations will fail with {error, token_revoked}
```

## Token Info Structure

```erlang
%% Successful validation returns:
TokenInfo = #{
    <<"active">> => true,
    <<"user_id">> => <<"user_12345">>,      % Extracted from 'sub' or 'username'
    <<"sub">> => <<"user_12345">>,          % Subject (user identifier)
    <<"exp">> => 1738368000,                % Expiration timestamp (seconds)
    <<"nbf">> => 1738364400,                % Not before timestamp (optional)
    <<"iss">> => <<"https://auth.example.com">>,  % Issuer (optional)
    <<"aud">> => <<"api">>,                 % Audience (optional)
    <<"scope">> => <<"read write">>,        % Scopes (optional)
    <<"client_id">> => <<"app_123">>,       % Client ID (optional)
    % ... additional claims from authorization server
}.
```

## Error Reference

| Error Code | Meaning | Action |
|------------|---------|--------|
| `token_revoked` | Token in revocation list | Reject request, user must re-authenticate |
| `token_invalid` | Token inactive (`active=false`) | Reject request, token not valid |
| `token_expired` | Token past expiration (`exp <= now`) | Reject request, user must refresh token |
| `token_not_yet_valid` | Token not yet valid (`nbf > now`) | Reject request, wait or check clock sync |
| `invalid_client` | Client credentials invalid (401) | Check client_id/client_secret config |
| `forbidden` | Client not authorized (403) | Check OAuth2 server client permissions |
| `invalid_request` | Malformed request (400) | Check token format |
| `connection_failed` | Cannot connect to auth server | Check introspect_url, network, firewall |
| `introspection_timeout` | Auth server timeout | Increase timeout config, check server |
| `introspection_failed` | Other introspection error | Check logs for details |
| `oauth2_not_configured` | OAuth2 not enabled | Set `enabled => true` in config |

## Performance Tuning

### Cache Hit Rate Optimization

```erlang
%% Check cache statistics (implement in production)
CacheSize = ets:info(auth_oauth2_cache, size),
logger:info("OAuth2 cache size: ~p entries", [CacheSize]).

%% Factors affecting cache hit rate:
%% - Token reuse patterns (same token used multiple times)
%% - TTL settings (longer TTL = higher hit rate, but stale risk)
%% - Request rate (high rate = better reuse)
%% - Token expiration (short-lived tokens = lower hit rate)

%% Typical hit rates:
%% - API gateway scenario: 80-95%
%% - Single-use tokens: 0-20%
%% - Mixed workload: 50-70%
```

### Timeout Tuning

```erlang
%% Adjust based on network conditions
Config = #{
    oauth2 => #{
        timeout => 3000,           % Fast local network
        response_timeout => 5000   % Fast auth server
    }
},

%% OR for slower networks
Config = #{
    oauth2 => #{
        timeout => 10000,          % Slow network / distant auth server
        response_timeout => 20000  % Allow time for database lookups
    }
}.
```

## Security Best Practices

### 1. Protect Client Credentials

```erlang
%% BAD - credentials in code/config file
Config = #{
    oauth2 => #{
        client_secret => <<"hardcoded_secret">>  % NEVER DO THIS
    }
}.

%% GOOD - credentials from environment
Config = #{
    oauth2 => #{
        client_secret => list_to_binary(os:getenv("OAUTH2_CLIENT_SECRET"))
    }
}.

%% BETTER - use erlmcp_secrets for secret management
{ok, ClientSecret} = erlmcp_secrets:get(<<"oauth2_client_secret">>),
Config = #{
    oauth2 => #{
        client_secret => ClientSecret
    }
}.
```

### 2. Use HTTPS Only

```erlang
%% ALWAYS use HTTPS for introspection endpoint
Config = #{
    oauth2 => #{
        introspect_url => <<"https://auth.example.com/oauth2/introspect">>  % ✓
        % introspect_url => <<"http://auth.example.com/oauth2/introspect">>   % ✗ NEVER
    }
}.
```

### 3. Minimize Token Logging

```erlang
%% Tokens are logged at debug level only
%% In production, set log level to info or warning:
logger:set_primary_config(level, info).

%% If you must log tokens, sanitize them:
sanitize_token(Token) ->
    Size = byte_size(Token),
    case Size > 10 of
        true ->
            <<Prefix:5/binary, _:Size-10, Suffix:5/binary>> = Token,
            <<Prefix/binary, "...", Suffix/binary>>;
        false ->
            <<"***">>
    end.
```

### 4. Handle Revocation Properly

```erlang
%% On user logout
logout(SessionId, Token) ->
    % Revoke token to prevent reuse
    ok = erlmcp_auth:revoke_token(Token),

    % Destroy session
    ok = erlmcp_auth:destroy_session(SessionId),

    logger:info("User logged out, token revoked: ~p", [SessionId]).

%% On security event (e.g., password change, suspicious activity)
revoke_user_tokens(UserId) ->
    % Get all user sessions
    Sessions = get_user_sessions(UserId),

    % Revoke all tokens
    lists:foreach(fun(#{token := Token}) ->
        ok = erlmcp_auth:revoke_token(Token)
    end, Sessions),

    logger:warning("All tokens revoked for user: ~p", [UserId]).
```

## Monitoring & Observability

### Metrics to Collect

```erlang
%% Implement these metrics in production monitoring

-spec record_token_validation(Result :: ok | error, Latency :: integer()) -> ok.
record_token_validation(ok, Latency) ->
    erlmcp_metrics:increment(oauth2_validations_success),
    erlmcp_metrics:observe(oauth2_validation_latency, Latency);
record_token_validation(error, Latency) ->
    erlmcp_metrics:increment(oauth2_validations_failed),
    erlmcp_metrics:observe(oauth2_validation_latency, Latency).

%% Cache metrics
-spec record_cache_hit(Hit :: boolean()) -> ok.
record_cache_hit(true) ->
    erlmcp_metrics:increment(oauth2_cache_hits);
record_cache_hit(false) ->
    erlmcp_metrics:increment(oauth2_cache_misses).

%% Introspection metrics
-spec record_introspection(Status :: integer(), Latency :: integer()) -> ok.
record_introspection(Status, Latency) ->
    erlmcp_metrics:increment(oauth2_introspections, #{status => Status}),
    erlmcp_metrics:observe(oauth2_introspection_latency, Latency).
```

### Health Checks

```erlang
%% Implement health check endpoint
health_check() ->
    % Check if auth server is reachable
    TestToken = <<"invalid_token_for_health_check">>,

    Start = erlang:system_time(millisecond),
    Result = erlmcp_auth:validate_oauth2_token(TestToken),
    Latency = erlang:system_time(millisecond) - Start,

    case Result of
        {error, token_invalid} ->
            % Expected error, auth server is up
            {ok, #{
                status => healthy,
                latency_ms => Latency,
                message => <<"OAuth2 introspection available">>
            }};

        {error, connection_failed} ->
            {error, #{
                status => unhealthy,
                latency_ms => Latency,
                message => <<"Cannot connect to OAuth2 server">>
            }};

        {error, introspection_timeout} ->
            {error, #{
                status => degraded,
                latency_ms => Latency,
                message => <<"OAuth2 server slow or unresponsive">>
            }};

        _ ->
            {ok, #{
                status => healthy,
                latency_ms => Latency,
                message => <<"OAuth2 introspection working">>
            }}
    end.
```

## Troubleshooting

### Problem: Token validation always returns `{error, connection_failed}`

**Possible Causes:**
1. Incorrect introspect_url
2. Network/firewall blocking HTTPS
3. Auth server down

**Debug:**
```erlang
% Check configuration
{ok, Config} = application:get_env(erlmcp_core, auth),
OAuth2Config = maps:get(oauth2, Config),
io:format("OAuth2 Config: ~p~n", [OAuth2Config]).

% Test network connectivity
IntrospectUrl = maps:get(introspect_url, OAuth2Config),
httpc:request(get, {binary_to_list(IntrospectUrl), []}, [], []).
```

### Problem: Token validation returns `{error, invalid_client}`

**Possible Causes:**
1. Incorrect client_id or client_secret
2. Client not authorized for introspection

**Debug:**
```erlang
% Verify credentials (DO NOT LOG IN PRODUCTION)
ClientId = maps:get(client_id, OAuth2Config),
ClientSecret = maps:get(client_secret, OAuth2Config),
io:format("Client ID: ~p~n", [ClientId]),
% DO NOT: io:format("Client Secret: ~p~n", [ClientSecret]).

% Test with curl
% curl -X POST https://auth.example.com/oauth2/introspect \
%   -u client_id:client_secret \
%   -d "token=your_token"
```

### Problem: Cache hit rate too low

**Possible Causes:**
1. Tokens not being reused (single-use pattern)
2. TTL too short
3. Token expiration too short

**Debug:**
```erlang
% Check cache size and entries
ets:info(auth_oauth2_cache, size).
ets:tab2list(auth_oauth2_cache).

% Check token TTL
case ets:lookup(auth_oauth2_cache, Token) of
    [{Token, {TokenInfo, ExpiresAt}}] ->
        Now = erlang:system_time(second),
        TTL = ExpiresAt - Now,
        io:format("Token TTL: ~p seconds~n", [TTL]);
    [] ->
        io:format("Token not in cache~n")
end.
```

## Example Integration

```erlang
-module(my_api_handler).
-export([handle_request/2]).

handle_request(Req, State) ->
    % Extract Bearer token from Authorization header
    case cowboy_req:header(<<"authorization">>, Req) of
        <<"Bearer ", Token/binary>> ->
            % Validate token
            case erlmcp_auth:validate_oauth2_token(Token) of
                {ok, TokenInfo} ->
                    % Check permissions
                    UserId = maps:get(<<"user_id">>, TokenInfo),
                    Scope = maps:get(<<"scope">>, TokenInfo, <<>>),

                    case check_scope(Scope, <<"read">>) of
                        true ->
                            % Handle authorized request
                            handle_authorized_request(Req, State, UserId);
                        false ->
                            % Insufficient permissions
                            {403, #{}, <<"Forbidden">>, State}
                    end;

                {error, _Reason} ->
                    % Unauthorized
                    {401, #{}, <<"Unauthorized">>, State}
            end;

        _ ->
            % Missing or invalid Authorization header
            {401, #{}, <<"Unauthorized">>, State}
    end.

check_scope(Scope, RequiredScope) ->
    Scopes = binary:split(Scope, <<" ">>, [global]),
    lists:member(RequiredScope, Scopes).
```

## Testing

```erlang
%% Unit test example
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

oauth2_validation_test() ->
    % Setup
    Config = #{
        oauth2 => #{
            enabled => true,
            introspect_url => <<"https://test.auth.example.com/introspect">>,
            client_id => <<"test_client">>,
            client_secret => <<"test_secret">>
        }
    },
    {ok, _Pid} = erlmcp_auth:start_link(Config),

    % Test valid token
    ValidToken = <<"valid_token_123">>,
    {ok, TokenInfo} = erlmcp_auth:validate_oauth2_token(ValidToken),
    ?assertEqual(true, maps:get(<<"active">>, TokenInfo)),

    % Test invalid token
    InvalidToken = <<"invalid_token_456">>,
    {error, token_invalid} = erlmcp_auth:validate_oauth2_token(InvalidToken),

    % Cleanup
    erlmcp_auth:stop().

-endif.
```
