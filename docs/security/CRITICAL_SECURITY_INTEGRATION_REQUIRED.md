# CRITICAL: Security Validators Not Integrated

**Issue Type**: Security Gap
**Priority**: CRITICAL
**Effort**: 4 hours
**Risk**: CVSS 8.1 (DNS Rebinding) + CVSS 7.5 (CRLF Injection)

---

## Problem

Two essential security validators are **implemented, tested, and documented** but **NOT ENFORCED** at the transport layer:

1. **Origin Validation** (`erlmcp_origin_validator.erl`)
   - Module: 146 lines of production code
   - Tests: 25 comprehensive tests
   - Integration: ❌ NOT CALLED in transport handlers
   - Risk: DNS rebinding attacks (CVSS 8.1)

2. **Header Validation** (`erlmcp_http_header_validator.erl`)
   - Module: 282 lines of production code
   - Tests: 25 comprehensive tests
   - Integration: ❌ NOT CALLED in transport handlers
   - Risk: CRLF injection attacks (CVSS 7.5)

This creates a **false sense of security** where developers assume protection exists.

---

## Impact

**Without Origin Validation**:
```http
GET /mcp/sse HTTP/1.1
Host: malicious.com
Origin: http://evil.com
Accept: text/event-stream
```
**Result**: Connection ACCEPTED (DNS rebinding attack possible)

**Without Header Validation**:
```http
POST /mcp HTTP/1.1
X-Custom: value\r\nX-Admin: true
```
**Result**: CRLF injection ACCEPTED (HTTP response splitting possible)

---

## Solution

### Task 1: Integrate Origin Validation (1 hour)

**File**: `apps/erlmcp_transports/src/erlmcp_transport_sse.erl`

**Changes**:

1. **Line 138** (`handle_sse_stream/3`):
```erlang
handle_sse_stream(Req, TransportId, State) ->
    SpanCtx = erlmcp_tracing:start_span(<<"transport_sse.handle_stream">>),

    %% ADD: Origin validation (DNS rebinding protection)
    Origin = cowboy_req:header(<<"origin">>, Req),
    AllowedOrigins = application:get_env(erlmcp, allowed_origins,
        erlmcp_origin_validator:get_default_allowed_origins()),

    case erlmcp_origin_validator:validate_origin(Origin, AllowedOrigins) of
        {ok, _ValidOrigin} ->
            proceed_with_stream_setup(Req, TransportId, State, SpanCtx);
        {error, forbidden} ->
            logger:warning("SSE rejected: invalid Origin ~s", [Origin]),
            ReqReply = cowboy_req:reply(403, #{
                <<"content-type">> => <<"application/json">>
            }, jsx:encode(#{
                <<"error">> => <<"forbidden">>,
                <<"message">> => <<"Origin not allowed">>,
                <<"allowed_origins">> => AllowedOrigins
            }), Req),
            erlmcp_tracing:end_span(SpanCtx),
            {ok, ReqReply, State}
    end.

proceed_with_stream_setup(Req, TransportId, State, SpanCtx) ->
    %% EXISTING CODE from line 142 onwards
    ClientId = erlang:list_to_binary(erlang:pid_to_list(self())),
    SessionId = generate_session_id(ClientId),
    %% ... rest of existing implementation
```

2. **Line 180** (`handle_post_request/3`):
```erlang
handle_post_request(Req, TransportId, State) ->
    SpanCtx = erlmcp_tracing:start_span(<<"transport_sse.handle_post">>),

    %% ADD: Origin validation for POST requests
    Origin = cowboy_req:header(<<"origin">>, Req),
    AllowedOrigins = application:get_env(erlmcp, allowed_origins,
        erlmcp_origin_validator:get_default_allowed_origins()),

    case erlmcp_origin_validator:validate_origin(Origin, AllowedOrigins) of
        {ok, _ValidOrigin} ->
            proceed_with_post(Req, TransportId, State, SpanCtx);
        {error, forbidden} ->
            logger:warning("POST rejected: invalid Origin ~s", [Origin]),
            ReqReply = cowboy_req:reply(403, #{}, <<"Origin not allowed">>, Req),
            erlmcp_tracing:end_span(SpanCtx),
            {ok, ReqReply, State}
    end.

proceed_with_post(Req, TransportId, State, SpanCtx) ->
    %% EXISTING CODE from line 183 onwards
    {ok, Body, Req2} = cowboy_req:read_body(Req),
    %% ... rest of existing implementation
```

### Task 2: Integrate Header Validation (1 hour)

**File**: `apps/erlmcp_transports/src/erlmcp_transport_sse.erl`

**Changes**:

1. **Line 116** (`handle/2`):
```erlang
handle(Req, #{transport_id := TransportId} = State) ->
    SpanCtx = erlmcp_tracing:start_span(<<"transport_sse.handle">>),

    %% ADD: HTTP header validation (CRLF injection prevention)
    Headers = maps:to_list(cowboy_req:headers(Req)),
    Method = case cowboy_req:method(Req) of
        <<"GET">> -> get;
        <<"POST">> -> post;
        <<"DELETE">> -> delete;
        _ -> get
    end,

    case erlmcp_http_header_validator:validate_request_headers(Headers, Method) of
        {ok, ValidatedHeaders} ->
            proceed_with_validated_request(Req, Method, ValidatedHeaders, TransportId, State, SpanCtx);
        {error, {StatusCode, Message, Data}} ->
            logger:warning("Header validation failed: ~s", [Message]),
            {Code, Hdrs, Body} = erlmcp_http_header_validator:format_error_response(
                StatusCode, Message, Data
            ),
            ReqReply = cowboy_req:reply(Code, Hdrs, Body, Req),
            erlmcp_tracing:end_span(SpanCtx),
            {ok, ReqReply, State}
    end.

proceed_with_validated_request(Req, Method, _ValidatedHeaders, TransportId, State, SpanCtx) ->
    %% EXISTING CODE from line 119 onwards
    case Method of
        get -> handle_sse_stream(Req, TransportId, State);
        post -> handle_post_request(Req, TransportId, State);
        _ ->
            ReqReply = cowboy_req:reply(405, #{}, <<"Method not allowed">>, Req),
            {ok, ReqReply, State}
    end.
```

### Task 3: Add Integration Tests (2 hours)

**File**: `apps/erlmcp_transports/test/erlmcp_transport_sse_security_tests.erl` (NEW)

```erlang
-module(erlmcp_transport_sse_security_tests).
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Origin Validation Tests
%%%===================================================================

origin_validation_blocks_external_test() ->
    %% Setup: Start SSE transport
    {ok, _Pid} = erlmcp_transport_sse:init(<<"test_transport">>, #{port => 8081}),

    %% Test: Send request with external origin
    Req = mock_request(#{
        method => <<"GET">>,
        headers => #{
            <<"origin">> => <<"http://evil.com">>,
            <<"accept">> => <<"text/event-stream">>
        }
    }),

    %% Execute
    {ok, Reply, _State} = erlmcp_transport_sse:handle(Req, #{transport_id => <<"test">>}),

    %% Verify: 403 Forbidden
    ?assertEqual(403, get_status(Reply)),
    Body = get_body(Reply),
    Decoded = jsx:decode(Body),
    ?assertEqual(<<"forbidden">>, maps:get(<<"error">>, Decoded)).

origin_validation_allows_localhost_test() ->
    %% Setup
    {ok, _Pid} = erlmcp_transport_sse:init(<<"test_transport">>, #{port => 8081}),

    %% Test: Localhost origin should be allowed
    Req = mock_request(#{
        method => <<"GET">>,
        headers => #{
            <<"origin">> => <<"http://localhost:8080">>,
            <<"accept">> => <<"text/event-stream">>
        }
    }),

    %% Execute
    {ok, Reply, _State} = erlmcp_transport_sse:handle(Req, #{transport_id => <<"test">>}),

    %% Verify: Connection allowed (200 or stream established)
    Status = get_status(Reply),
    ?assert(Status =:= 200 orelse Status =:= undefined). % undefined for streaming

origin_validation_post_rejected_test() ->
    %% Test POST with invalid origin
    {ok, _Pid} = erlmcp_transport_sse:init(<<"test_transport">>, #{port => 8081}),

    Req = mock_request(#{
        method => <<"POST">>,
        headers => #{
            <<"origin">> => <<"http://attacker.com">>,
            <<"content-type">> => <<"application/json">>
        },
        body => <<"{\"jsonrpc\":\"2.0\",\"method\":\"test\"}">>
    }),

    {ok, Reply, _State} = erlmcp_transport_sse:handle(Req, #{transport_id => <<"test">>}),

    ?assertEqual(403, get_status(Reply)).

%%%===================================================================
%%% Header Validation Tests
%%%===================================================================

header_validation_blocks_crlf_injection_test() ->
    %% Test CRLF injection in header value
    Req = mock_request(#{
        method => <<"POST">>,
        headers => #{
            <<"origin">> => <<"http://localhost">>,
            <<"content-type">> => <<"application/json">>,
            <<"x-custom">> => <<"value\r\nX-Admin: true">>
        }
    }),

    {ok, Reply, _State} = erlmcp_transport_sse:handle(Req, #{transport_id => <<"test">>}),

    %% Should reject with 400
    ?assertEqual(400, get_status(Reply)),
    Body = get_body(Reply),
    Decoded = jsx:decode(Body),
    ?assertEqual(<<"header_validation_failed">>, maps:get(<<"error">>, Decoded)).

header_validation_requires_accept_test() ->
    %% GET without Accept header should fail
    Req = mock_request(#{
        method => <<"GET">>,
        headers => #{
            <<"origin">> => <<"http://localhost">>
            %% MISSING: accept header
        }
    }),

    {ok, Reply, _State} = erlmcp_transport_sse:handle(Req, #{transport_id => <<"test">>}),

    ?assertEqual(400, get_status(Reply)),
    Body = get_body(Reply),
    Decoded = jsx:decode(Body),
    ?assertMatch(#{<<"message">> := <<"Missing required header">>}, Decoded).

header_validation_requires_content_type_test() ->
    %% POST without Content-Type should fail
    Req = mock_request(#{
        method => <<"POST">>,
        headers => #{
            <<"origin">> => <<"http://localhost">>
            %% MISSING: content-type header
        }
    }),

    {ok, Reply, _State} = erlmcp_transport_sse:handle(Req, #{transport_id => <<"test">>}),

    ?assertEqual(400, get_status(Reply)).

header_validation_rejects_oversized_headers_test() ->
    %% Header value > 8KB should be rejected
    LargeValue = binary:copy(<<"x">>, 9000),
    Req = mock_request(#{
        method => <<"POST">>,
        headers => #{
            <<"origin">> => <<"http://localhost">>,
            <<"content-type">> => <<"application/json">>,
            <<"x-large">> => LargeValue
        }
    }),

    {ok, Reply, _State} = erlmcp_transport_sse:handle(Req, #{transport_id => <<"test">>}),

    ?assertEqual(431, get_status(Reply)). % Request Header Fields Too Large

%%%===================================================================
%%% Helper Functions
%%%===================================================================

mock_request(Opts) ->
    %% Build mock cowboy_req for testing
    %% Implementation depends on cowboy version
    #{
        method => maps:get(method, Opts, <<"GET">>),
        headers => maps:get(headers, Opts, #{}),
        body => maps:get(body, Opts, <<>>)
    }.

get_status(Reply) ->
    %% Extract status code from reply
    maps:get(status, Reply, undefined).

get_body(Reply) ->
    %% Extract body from reply
    maps:get(body, Reply, <<>>).
```

### Task 4: Update Configuration (30 minutes)

**File**: `config/sys.config`

**Add** (after line 94):
```erlang
%% Transport Security Configuration
{erlmcp_transports, [
    %% Origin validation (DNS rebinding protection)
    %% PRODUCTION: Configure allowed origins explicitly
    %% DEVELOPMENT: Defaults to localhost only
    {allowed_origins, [
        <<"http://localhost">>,
        <<"http://localhost:8080">>,
        <<"http://localhost:8081">>,
        <<"http://127.0.0.1">>,
        <<"http://127.0.0.1:8080">>,
        <<"http://[::1]">>,
        <<"null">>
    ]},

    %% Bind to localhost only (requires reverse proxy for external access)
    {bind_address, {127, 0, 0, 1}},

    %% Maximum header size (per header, 8KB)
    {max_header_size, 8192},

    %% Maximum total headers size (64KB)
    {max_total_headers_size, 65536}
]},
```

**File**: `config/production.config`

**Add** (after line 65):
```erlang
%% Production Transport Security
{erlmcp_transports, [
    %% Production allowed origins (HTTPS only)
    {allowed_origins, [
        <<"https://app.example.com">>,
        <<"https://api.example.com">>
        %% DO NOT include wildcards
        %% DO NOT include http:// URLs
        %% DO NOT include "null"
    ]},

    %% Bind to localhost only (nginx/HAProxy required)
    {bind_address, {127, 0, 0, 1}},

    %% Strict header limits
    {max_header_size, 4096},  % 4KB per header
    {max_total_headers_size, 32768}  % 32KB total
]},
```

---

## Testing Plan

### Manual Testing

1. **Origin Validation**:
```bash
# Should REJECT (external origin)
curl -H "Origin: http://evil.com" http://localhost:8081/mcp/sse
# Expected: 403 Forbidden

# Should ACCEPT (localhost)
curl -H "Origin: http://localhost:8080" http://localhost:8081/mcp/sse
# Expected: 200 OK or streaming connection
```

2. **Header Validation**:
```bash
# Should REJECT (CRLF injection)
curl -H "X-Custom: value\r\nX-Admin: true" http://localhost:8081/mcp
# Expected: 400 Bad Request

# Should REJECT (missing Accept)
curl -X GET http://localhost:8081/mcp/sse
# Expected: 400 Bad Request

# Should ACCEPT (valid headers)
curl -H "Accept: text/event-stream" -H "Origin: http://localhost" http://localhost:8081/mcp/sse
# Expected: 200 OK
```

### Automated Testing

```bash
# Run integration tests
TERM=dumb rebar3 eunit --module=erlmcp_transport_sse_security_tests

# Expected output:
# All 8 tests .......... OK
# Test time: X.XXX s
# Pass rate: 100%
```

---

## Acceptance Criteria

- [ ] Origin validation integrated in `handle_sse_stream/3`
- [ ] Origin validation integrated in `handle_post_request/3`
- [ ] Header validation integrated in `handle/2`
- [ ] Configuration added to `sys.config` and `production.config`
- [ ] 8 integration tests added and passing
- [ ] Manual testing completed (all 5 test cases)
- [ ] Code compiles without warnings
- [ ] Documentation updated

---

## Files Modified

1. `apps/erlmcp_transports/src/erlmcp_transport_sse.erl` (3 functions)
2. `apps/erlmcp_transports/test/erlmcp_transport_sse_security_tests.erl` (NEW)
3. `config/sys.config` (transport security config)
4. `config/production.config` (production security config)

**Total Lines Changed**: ~150 lines
**Effort**: 4 hours
**Risk Reduction**: CVSS 8.1 + CVSS 7.5 = Critical vulnerabilities eliminated

---

## References

- Full Audit: `/home/user/erlmcp/docs/security/TRANSPORT_SECURITY_DEFAULTS_AUDIT.md`
- Summary: `/home/user/erlmcp/docs/security/TRANSPORT_SECURITY_AUDIT_SUMMARY.md`
- Origin Validator: `apps/erlmcp_transports/src/erlmcp_origin_validator.erl`
- Header Validator: `apps/erlmcp_transports/src/erlmcp_http_header_validator.erl`

---

**Created**: 2026-02-01
**Priority**: CRITICAL
**Assignee**: TBD
**Estimated Completion**: 4 hours after start
