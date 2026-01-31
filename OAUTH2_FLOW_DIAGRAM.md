# OAuth2 Token Introspection Flow Diagram

```
┌─────────────────────────────────────────────────────────────────────┐
│                    OAuth2 Token Validation Flow                     │
└─────────────────────────────────────────────────────────────────────┘

1. API Call
   │
   ├─► validate_oauth2_token(Token)
   │
   └─► gen_server:call(erlmcp_auth, {validate_oauth2_token, Token})


2. Check Revocation
   │
   ├─► do_validate_oauth2_token(Token, State)
   │
   ├─► Check: ets:lookup(revoked_tokens, Token)
   │   │
   │   ├─► [FOUND] ──► {error, token_revoked}
   │   │
   │   └─► [NOT FOUND] ──► Continue


3. Cache Lookup
   │
   ├─► Check: ets:lookup(oauth2_cache, Token)
   │   │
   │   ├─► [CACHE HIT]
   │   │   │
   │   │   ├─► Check: Now < ExpiresAt?
   │   │   │   │
   │   │   │   ├─► [YES] ──► {ok, TokenInfo} (FAST PATH)
   │   │   │   │
   │   │   │   └─► [NO] ──► Delete cache entry ──► Continue
   │   │
   │   └─► [CACHE MISS] ──► Continue


4. HTTP Introspection (SLOW PATH)
   │
   ├─► introspect_oauth2_token(Token, Config, State)
   │   │
   │   ├─► Parse introspect_url ──► {Scheme, Host, Port, Path}
   │   │
   │   ├─► scheme_to_atom(Scheme) ──► http | https
   │   │
   │   ├─► gun:open(Host, Port, GunOpts)
   │   │   │
   │   │   └─► [SUCCESS] ──► MonitorRef = monitor(process, GunPid)
   │   │
   │   ├─► try
   │   │   │
   │   │   ├─► gun:await_up(GunPid, Timeout)
   │   │   │
   │   │   ├─► gun:post(GunPid, Path, Headers, Body)
   │   │   │   │
   │   │   │   └─► Headers:
   │   │   │       - content-type: application/x-www-form-urlencoded
   │   │   │       - authorization: Basic {base64(client_id:client_secret)}
   │   │   │       - accept: application/json
   │   │   │
   │   │   ├─► gun:await(GunPid, StreamRef, ResponseTimeout)
   │   │   │   │
   │   │   │   ├─► {response, nofin, Status, Headers}
   │   │   │   │   │
   │   │   │   │   └─► gun:await_body(GunPid, StreamRef, Timeout)
   │   │   │   │       │
   │   │   │   │       └─► {ok, Body}
   │   │   │   │
   │   │   │   └─► handle_introspect_response(Status, Headers, Body, Token, State)
   │   │   │
   │   │   └─► after
   │   │       │
   │   │       ├─► demonitor(MonitorRef, [flush])
   │   │       │
   │   │       └─► gun:close(GunPid)


5. Response Handling
   │
   ├─► handle_introspect_response(Status, Headers, Body, Token, State)
   │   │
   │   ├─► [Status 200]
   │   │   │
   │   │   ├─► TokenInfo = jsx:decode(Body)
   │   │   │
   │   │   ├─► validate_introspection_response(TokenInfo)
   │   │   │   │
   │   │   │   ├─► Check: active = true?
   │   │   │   │   │
   │   │   │   │   ├─► [NO] ──► {error, token_invalid}
   │   │   │   │   │
   │   │   │   │   └─► [YES] ──► Continue
   │   │   │   │
   │   │   │   ├─► Check: exp <= now()?
   │   │   │   │   │
   │   │   │   │   ├─► [YES] ──► {error, token_expired}
   │   │   │   │   │
   │   │   │   │   └─► [NO] ──► Continue
   │   │   │   │
   │   │   │   ├─► Check: nbf > now()?
   │   │   │   │   │
   │   │   │   │   ├─► [YES] ──► {error, token_not_yet_valid}
   │   │   │   │   │
   │   │   │   │   └─► [NO] ──► Continue
   │   │   │   │
   │   │   │   └─► finalize_token_validation(TokenInfo, Now)
   │   │   │       │
   │   │   │       ├─► Extract: user_id = sub | username
   │   │   │       │
   │   │   │       ├─► Calculate: TTL = min(exp - now, 300)
   │   │   │       │
   │   │   │       └─► {ok, EnrichedTokenInfo, TTL}
   │   │   │
   │   │   └─► cache_oauth2_token(Token, TokenInfo, TTL, State)
   │   │       │
   │   │       ├─► Check: TTL > 0?
   │   │       │   │
   │   │       │   ├─► [YES]
   │   │       │   │   │
   │   │       │   │   ├─► ExpiresAt = now() + TTL
   │   │       │   │   │
   │   │       │   │   └─► ets:insert(oauth2_cache, {Token, {TokenInfo, ExpiresAt}})
   │   │       │   │
   │   │       │   └─► [NO] ──► Skip caching
   │   │       │
   │   │       └─► {ok, EnrichedTokenInfo}
   │   │
   │   ├─► [Status 401] ──► {error, invalid_client}
   │   │
   │   ├─► [Status 403] ──► {error, forbidden}
   │   │
   │   ├─► [Status 400] ──► {error, invalid_request}
   │   │
   │   └─► [Status Other] ──► {error, introspection_failed}


6. Periodic Cleanup (Every 60 seconds)
   │
   ├─► handle_info(cleanup_expired, State)
   │   │
   │   ├─► cleanup_expired_sessions(State, Now)
   │   │
   │   ├─► cleanup_revoked_tokens(State, Now)
   │   │
   │   └─► cleanup_oauth2_cache(State, Now)
   │       │
   │       └─► ets:foldl(fun({Token, {_, ExpiresAt}}, Acc) ->
   │               case ExpiresAt < Now of
   │                   true -> ets:delete(oauth2_cache, Token);
   │                   false -> ok
   │               end
   │           end, ok, oauth2_cache)


┌─────────────────────────────────────────────────────────────────────┐
│                         Performance Metrics                          │
└─────────────────────────────────────────────────────────────────────┘

Cache Hit (Hot Path)
├─► Latency: 1-5 ms
├─► Operations: 1 ETS lookup + timestamp comparison
└─► Throughput: ~100K requests/sec

Cache Miss (Cold Path)
├─► Latency: 50-200 ms (network RTT + processing)
├─► Operations: 1 HTTP POST + JSON decode + validation + ETS insert
└─► Throughput: ~100-500 requests/sec (network bound)


┌─────────────────────────────────────────────────────────────────────┐
│                         Security Layers                             │
└─────────────────────────────────────────────────────────────────────┘

Layer 1: Revocation Check
├─► Check: Token in revoked_tokens table?
└─► Purpose: Prevent network calls for known-revoked tokens

Layer 2: Cache Validation
├─► Check: Token in cache and not expired?
└─► Purpose: Fast validation for recently-validated tokens

Layer 3: HTTP Introspection
├─► Check: Call OAuth2 authorization server
└─► Purpose: Authoritative token validation

Layer 4: Response Validation
├─► Check: active=true, exp valid, nbf valid, iss valid
└─► Purpose: Ensure token meets all RFC 7662 requirements

Layer 5: Cache TTL Limit
├─► Check: TTL capped at 5 minutes
└─► Purpose: Prevent long-lived stale token reuse


┌─────────────────────────────────────────────────────────────────────┐
│                     Error Handling Strategy                          │
└─────────────────────────────────────────────────────────────────────┘

Network Errors
├─► Connection failed ──► {error, connection_failed}
├─► Timeout ──► {error, introspection_timeout}
└─► Body read error ──► {error, introspection_failed}

Validation Errors
├─► active=false ──► {error, token_invalid}
├─► exp <= now ──► {error, token_expired}
├─► nbf > now ──► {error, token_not_yet_valid}
└─► Invalid issuer ──► {error, invalid_issuer}

HTTP Status Errors
├─► 401 ──► {error, invalid_client}
├─► 403 ──► {error, forbidden}
├─► 400 ──► {error, invalid_request}
└─► Other ──► {error, introspection_failed}

Resource Cleanup (Always)
├─► Demonitor gun process
└─► Close gun connection
```

## Configuration Flow

```
Application Startup
│
├─► erlmcp_auth:start_link(Config)
│   │
│   ├─► init([Config])
│   │   │
│   │   ├─► oauth2_config = maps:get(oauth2, Config, #{})
│   │   │   │
│   │   │   ├─► enabled: true | false
│   │   │   ├─► introspect_url: <<"https://auth.example.com/oauth2/introspect">>
│   │   │   ├─► client_id: <<"erlmcp_service">>
│   │   │   ├─► client_secret: <<"secret_xyz">>
│   │   │   ├─► timeout: 5000 (ms)
│   │   │   └─► response_timeout: 10000 (ms)
│   │   │
│   │   └─► oauth2_cache = ets:new(auth_oauth2_cache, [set, protected])
│   │
│   └─► {ok, State}
```

## Cache State Management

```
ETS Table: oauth2_cache
├─► Type: set
├─► Access: protected
├─► Key: Token (binary)
└─► Value: {TokenInfo (map), ExpiresAt (integer)}

Example Entry:
{
    <<"eyJhbGciOiJSUzI1NiIsInR5cCI6IkpXVCJ9...">>,
    {
        #{
            <<"active">> => true,
            <<"sub">> => <<"user_12345">>,
            <<"exp">> => 1738368000,
            <<"iss">> => <<"https://auth.example.com">>,
            <<"user_id">> => <<"user_12345">>
        },
        1738368000  % ExpiresAt
    }
}
```

## Monitoring Points

```
Metrics to Track:
├─► cache_hit_rate: (cache_hits / total_requests) * 100
├─► introspection_latency_p95: 95th percentile HTTP latency
├─► introspection_error_rate: (errors / total_requests) * 100
├─► cache_size: Number of entries in oauth2_cache
├─► cache_eviction_rate: Entries removed per minute
└─► token_validation_throughput: Validations per second

Alerts:
├─► cache_hit_rate < 70% ──► Increase TTL or check token reuse patterns
├─► introspection_latency_p95 > 500ms ──► Network or auth server issues
├─► introspection_error_rate > 5% ──► Auth server or config issues
└─► cache_size > 100K ──► Memory pressure, reduce TTL
```
