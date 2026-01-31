# Phase 3a: OAuth 2.0 OIDC Discovery & Enhancement

**Version:** 1.0.0
**Date:** 2026-01-31
**Status:** PLANNING
**Effort:** 40-56 hours (2 weeks)
**Priority:** HIGH
**Impact:** Enterprise authentication compliance

---

## 1. Overview

### Executive Summary

This implementation plan details the enhancement of erlmcp's OAuth 2.0 authentication system to achieve full OpenID Connect (OIDC) Discovery 1.0 compliance and enterprise-grade OAuth 2.0 capabilities. The current implementation provides basic OAuth 2.0 client credentials flow with RFC 7662 token introspection. However, to support enterprise deployments and identity federation, erlmcp requires:

1. **OIDC Discovery Endpoint** - RFC 8414 compliance for automatic client configuration
2. **Authorization Code Flow** - OAuth 2.0 interactive user authentication
3. **ID Token Support** - OIDC identity layer for user authentication
4. **Incremental Consent** - Dynamic scope expansion without re-authentication
5. **Dynamic Client Registration** - RFC 7591 self-service client onboarding
6. **Revocation Endpoint** - RFC 7009 token lifecycle management

### Current State

**File:** `/home/user/erlmcp/apps/erlmcp_core/src/erlmcp_auth.erl` (1084 LOC)

**Implemented:**
- OAuth 2.0 token introspection (RFC 7662) via `do_validate_oauth2_token/2`
- ETS-based OAuth2 token caching with configurable TTL
- Gun HTTP client for introspection endpoint communication
- Token validation: active, exp, nbf, iss claims
- Session creation from validated tokens
- Rate limiting integration for authentication

**Missing:**
- Authorization server endpoints (`/authorize`, `/token`, `/userinfo`, `/revoke`)
- OIDC Discovery endpoint (`/.well-known/openid-configuration`)
- JWKS endpoint (`/.well-known/jwks.json`) for public key distribution
- ID token creation and validation (JWT-based identity tokens)
- Incremental scope consent management
- Authorization code generation and exchange
- Refresh token support
- Dynamic client registration
- PKCE (Proof Key for Code Exchange) support

### Target Architecture

erlmcp will function as both:
1. **Resource Server** (current) - Validates access tokens from external OAuth2/OIDC providers
2. **Authorization Server** (new) - Issues tokens for erlmcp-protected resources

This dual-mode operation enables:
- **Federation Mode:** erlmcp validates tokens from Auth0, Okta, Keycloak, Azure AD
- **Standalone Mode:** erlmcp issues tokens for internal services, MCP clients, test environments

### Business Impact

**Enterprise Requirements:**
- SSO integration with corporate identity providers
- Compliance with OAuth 2.0 / OIDC standards for audits
- Multi-tenant SaaS deployment support
- Developer portal with self-service client registration
- Fine-grained permission delegation via scopes

**Operational Benefits:**
- Reduced credential management (centralized identity)
- Improved security posture (short-lived tokens, refresh rotation)
- Better user experience (incremental consent, SSO)
- Audit trail compliance (token lifecycle events)

### Effort Estimate

**Total:** 40-56 hours (2 weeks, 1 developer)

| Phase | Task | Hours |
|-------|------|-------|
| Design | Specifications, API design, security review | 4 |
| Core Modules | 6 new modules (1,250 LOC) | 20-24 |
| HTTP Integration | Route handlers, middleware | 3-4 |
| Testing | 4 test suites (650 LOC) | 10-12 |
| Integration | End-to-end flows, debugging | 4-5 |
| Documentation | API docs, deployment guide | 3-4 |
| Code Review | Security audit, peer review | 2-3 |

---

## 2. Requirements Analysis

### Functional Requirements

#### FR1: OIDC Discovery Endpoint (RFC 8414)
**Priority:** CRITICAL
**Specification:** https://tools.ietf.org/html/rfc8414

**Requirements:**
- Serve `/.well-known/openid-configuration` with JSON metadata
- Include all supported endpoints (authorization, token, userinfo, revocation, jwks)
- Advertise supported grant types: `authorization_code`, `client_credentials`, `refresh_token`
- Advertise response types: `code`, `token` (implicit flow optional)
- Advertise scopes: `openid`, `profile`, `email`, custom scopes
- Advertise signing algorithms: `RS256`, `ES256`, `HS256`
- Cache discovery document (1 hour TTL)
- Support CORS for browser-based clients

**Success Criteria:**
- Discovery document validates against RFC 8414 schema
- Client libraries (oauth2-client.js, passport-oidc) can auto-configure
- 200 OK response with correct Content-Type: application/json

#### FR2: JWKS Endpoint (RFC 7517)
**Priority:** CRITICAL
**Specification:** https://tools.ietf.org/html/rfc7517

**Requirements:**
- Serve `/.well-known/jwks.json` with public keys in JWK format
- Support RSA keys (RS256) and ECDSA keys (ES256)
- Include key metadata: `kid`, `use`, `alg`, `n`, `e` (RSA) or `crv`, `x`, `y` (ECDSA)
- Automatic key rotation (30-day default)
- Old keys available for 7 days post-rotation (grace period)
- Cache JWKS response (1 hour TTL)

**Success Criteria:**
- JWT libraries (jose, nimbus-jose-jwt) can verify signatures
- Key rotation triggers no service disruption
- JWKS validates against RFC 7517 schema

#### FR3: Authorization Code Flow (RFC 6749 §4.1)
**Priority:** HIGH
**Specification:** https://tools.ietf.org/html/rfc6749#section-4.1

**Requirements:**
- `GET /oauth/authorize` endpoint with query parameters:
  - `response_type=code` (required)
  - `client_id` (required)
  - `redirect_uri` (required)
  - `scope` (optional, space-delimited)
  - `state` (recommended for CSRF protection)
  - `code_challenge`, `code_challenge_method` (PKCE, optional)
- User authentication and consent UI (simple HTML form)
- Generate authorization code (cryptographically random, 32 bytes)
- Authorization code TTL: 10 minutes (configurable)
- One-time use enforcement (code invalidated after token exchange)
- Redirect to `redirect_uri` with `code` and `state` parameters

**Success Criteria:**
- User completes OAuth flow and receives authorization code
- Code exchange succeeds at `/oauth/token` endpoint
- PKCE validation prevents code interception attacks
- Invalid redirect_uri returns error

#### FR4: Token Endpoint (RFC 6749 §3.2)
**Priority:** HIGH
**Specification:** https://tools.ietf.org/html/rfc6749#section-3.2

**Requirements:**
- `POST /oauth/token` endpoint with form-encoded body
- Grant type: `authorization_code`
  - Parameters: `code`, `redirect_uri`, `client_id`, `client_secret`, `code_verifier` (PKCE)
  - Response: `access_token`, `token_type`, `expires_in`, `refresh_token`, `id_token` (OIDC), `scope`
- Grant type: `client_credentials` (already supported via introspection)
  - Parameters: `client_id`, `client_secret`, `scope`
  - Response: `access_token`, `token_type`, `expires_in`, `scope`
- Grant type: `refresh_token`
  - Parameters: `refresh_token`, `client_id`, `client_secret`, `scope` (optional)
  - Response: new `access_token`, `refresh_token` (rotated), `expires_in`
- Client authentication methods:
  - `client_secret_basic` (HTTP Basic Auth)
  - `client_secret_post` (POST body)
- Token format: JWT (signed with RS256 or ES256)
- Access token TTL: 1 hour (configurable)
- Refresh token TTL: 30 days (configurable)
- ID token TTL: 5 minutes (short-lived for client-side validation)

**Success Criteria:**
- Authorization code exchange returns valid tokens
- Tokens are valid JWTs with correct claims
- Refresh token rotation invalidates old token
- Client authentication failures return 401

#### FR5: ID Token (OIDC Core §2)
**Priority:** HIGH
**Specification:** https://openid.net/specs/openid-connect-core-1_0.html#IDToken

**Requirements:**
- JWT structure with header, payload, signature
- Required claims:
  - `iss` (issuer, e.g., `https://erlmcp.example.com`)
  - `sub` (subject, user ID)
  - `aud` (audience, client_id)
  - `exp` (expiration timestamp)
  - `iat` (issued at timestamp)
  - `nonce` (if provided in authorization request)
- Optional claims:
  - `auth_time` (authentication timestamp)
  - `azp` (authorized party, if multiple audiences)
  - `name`, `email`, `picture` (from userinfo)
- Signature algorithm: RS256 or ES256 (configurable)
- Signature verification against JWKS endpoint

**Success Criteria:**
- ID token validates with jose library
- Claims extraction returns user identity
- Nonce validation prevents replay attacks
- Signature verification succeeds with public key

#### FR6: Userinfo Endpoint (OIDC Core §5.3)
**Priority:** MEDIUM
**Specification:** https://openid.net/specs/openid-connect-core-1_0.html#UserInfo

**Requirements:**
- `GET /oauth/userinfo` endpoint
- Requires `Authorization: Bearer <access_token>` header
- Returns JSON with user claims:
  - `sub` (subject, user ID)
  - `name`, `given_name`, `family_name`
  - `email`, `email_verified`
  - `picture`, `profile`, `website`
  - Custom claims from scopes
- Scope-based filtering: only return claims authorized by scopes
- CORS support for browser clients

**Success Criteria:**
- Valid access token returns user claims
- Invalid/expired token returns 401
- Scope filtering works correctly (e.g., `profile` scope excludes `email`)

#### FR7: Incremental Consent
**Priority:** MEDIUM

**Requirements:**
- Detect when client requests new scopes not in existing token
- Prompt user for additional consent
- Combine new scopes with existing authorization
- Issue new token with expanded scope set
- Track consent grants per user per client

**Success Criteria:**
- User grants additional scopes without re-authentication
- Denied scopes return error without breaking existing authorization
- Consent audit log records all grants/denials

#### FR8: Revocation Endpoint (RFC 7009)
**Priority:** MEDIUM
**Specification:** https://tools.ietf.org/html/rfc7009

**Requirements:**
- `POST /oauth/revoke` endpoint
- Parameters: `token`, `token_type_hint` (access_token or refresh_token)
- Client authentication required
- Revoke token and invalidate all dependent tokens
- Idempotent (revoking already-revoked token returns success)
- Revocation propagates to introspection cache

**Success Criteria:**
- Revoked access token returns `active: false` in introspection
- Revoked refresh token cannot issue new access tokens
- Client can revoke own tokens without admin privileges

### Non-Functional Requirements

#### NFR1: Performance
- Discovery endpoint: <10ms response time (cached)
- JWKS endpoint: <10ms response time (cached)
- Authorization code generation: <50ms
- Token issuance: <100ms (signature generation)
- Token validation: <50ms (signature verification)
- Userinfo endpoint: <50ms (ETS lookup)
- Revocation: <20ms (ETS update)

**Measurement:** `erlmcp_bench_integration` suite with OIDC flow benchmarks

#### NFR2: Security
- Authorization codes: 256-bit entropy, single-use, 10-minute TTL
- Access tokens: JWT with RS256/ES256 signature
- Refresh tokens: 256-bit entropy, rotated on use, 30-day TTL
- Client secrets: bcrypt hashed storage (cost factor 12)
- PKCE: S256 code challenge method (SHA-256)
- Rate limiting: 10 auth attempts per minute per client
- Token revocation audit log: immutable append-only log

**Validation:** Security test suite with penetration testing scenarios

#### NFR3: Scalability
- Support 10,000 concurrent authorization flows (queued)
- Support 100,000 active refresh tokens per node
- ETS-based token storage (in-memory, <100ms lookup)
- Optional Mnesia backend for clustered deployments
- Horizontal scaling via distributed session backend

**Measurement:** `erlmcp_bench_stress` with sustained OAuth flows

#### NFR4: Observability
- OpenTelemetry spans for all OAuth flows
- Metrics: token issuance rate, validation rate, revocation rate
- Metrics: authorization flow duration (p50, p95, p99)
- Audit log: all token lifecycle events (issue, refresh, revoke)
- Dashboard: real-time OAuth2/OIDC metrics

**Tooling:** `erlmcp_dashboard_server` with OAuth2 panel

#### NFR5: Compatibility
- OIDC conformance: OpenID Connect Conformance Test Suite (Basic RP)
- OAuth 2.0 conformance: OAuth 2.0 Security Best Current Practice (RFC 8252)
- Client libraries: oauth2-client.js, passport-openidconnect, nimbus-oauth2-sdk
- Identity providers: Interoperable with Auth0, Okta, Keycloak as federation partners

---

## 3. OIDC Discovery Endpoint (.well-known/openid-configuration)

### Specification: RFC 8414

**Endpoint:** `GET /.well-known/openid-configuration`
**Content-Type:** `application/json`
**Cache-Control:** `public, max-age=3600`

### Response Schema

```json
{
  "issuer": "https://erlmcp.example.com",
  "authorization_endpoint": "https://erlmcp.example.com/oauth/authorize",
  "token_endpoint": "https://erlmcp.example.com/oauth/token",
  "userinfo_endpoint": "https://erlmcp.example.com/oauth/userinfo",
  "revocation_endpoint": "https://erlmcp.example.com/oauth/revoke",
  "jwks_uri": "https://erlmcp.example.com/.well-known/jwks.json",
  "registration_endpoint": "https://erlmcp.example.com/oauth/register",

  "scopes_supported": [
    "openid",
    "profile",
    "email",
    "address",
    "phone",
    "offline_access"
  ],

  "response_types_supported": [
    "code",
    "token",
    "id_token",
    "code token",
    "code id_token",
    "token id_token",
    "code token id_token"
  ],

  "response_modes_supported": [
    "query",
    "fragment",
    "form_post"
  ],

  "grant_types_supported": [
    "authorization_code",
    "client_credentials",
    "refresh_token"
  ],

  "subject_types_supported": [
    "public"
  ],

  "id_token_signing_alg_values_supported": [
    "RS256",
    "ES256"
  ],

  "token_endpoint_auth_methods_supported": [
    "client_secret_basic",
    "client_secret_post",
    "private_key_jwt"
  ],

  "claims_supported": [
    "sub",
    "iss",
    "aud",
    "exp",
    "iat",
    "auth_time",
    "nonce",
    "name",
    "given_name",
    "family_name",
    "email",
    "email_verified",
    "picture",
    "profile"
  ],

  "code_challenge_methods_supported": [
    "S256"
  ],

  "service_documentation": "https://erlmcp.example.com/docs/oauth2",
  "ui_locales_supported": ["en-US"],
  "op_policy_uri": "https://erlmcp.example.com/privacy",
  "op_tos_uri": "https://erlmcp.example.com/terms"
}
```

### Implementation Approach

**Module:** `erlmcp_oauth_oidc_discovery.erl`
**Behavior:** None (stateless utility module)
**Dependencies:** `erlmcp_config` (for base URL)

**Key Functions:**
```erlang
-spec get_discovery_config() -> map().
%% Returns OIDC Discovery metadata map

-spec get_discovery_json() -> binary().
%% Returns cached JSON-encoded discovery document

-spec invalidate_cache() -> ok.
%% Invalidate discovery document cache (after config change)
```

**Caching Strategy:**
- ETS table: `oidc_discovery_cache`
- TTL: 3600 seconds (1 hour)
- Invalidation: On config change (manual trigger)

**HTTP Handler:**
```erlang
% In erlmcp_transport_http_router or new erlmcp_oauth_http_handler
handle_well_known_oidc(Req, State) ->
    Json = erlmcp_oauth_oidc_discovery:get_discovery_json(),
    Headers = #{
        <<"content-type">> => <<"application/json">>,
        <<"cache-control">> => <<"public, max-age=3600">>,
        <<"access-control-allow-origin">> => <<"*">>
    },
    {200, Headers, Json, State}.
```

**Configuration Source:**
```erlang
% In sys.config or development.config
{erlmcp_auth, [
    {oauth2, [
        {enabled, true},
        {issuer, "https://erlmcp.example.com"},
        {base_url, "http://localhost:8080"},
        {supported_scopes, [
            <<"openid">>,
            <<"profile">>,
            <<"email">>,
            <<"address">>,
            <<"phone">>,
            <<"offline_access">>
        ]},
        {supported_grant_types, [
            <<"authorization_code">>,
            <<"client_credentials">>,
            <<"refresh_token">>
        ]},
        {supported_signing_algs, [
            <<"RS256">>,
            <<"ES256">>
        ]}
    ]}
]}
```

---

## 4. Implementation Plan

### Module Breakdown

#### Module 1: erlmcp_oauth_oidc_discovery.erl (200 LOC)

**Purpose:** Generate and cache OIDC Discovery document

**Exports:**
```erlang
-export([
    get_discovery_config/0,
    get_discovery_json/0,
    invalidate_cache/0
]).
```

**Implementation:**
```erlang
-module(erlmcp_oauth_oidc_discovery).
-export([get_discovery_config/0, get_discovery_json/0, invalidate_cache/0]).

-define(CACHE_TTL, 3600).  % 1 hour

%% @doc Get OIDC Discovery metadata map
-spec get_discovery_config() -> map().
get_discovery_config() ->
    Config = application:get_env(erlmcp_auth, oauth2, #{}),
    BaseURL = maps:get(base_url, Config, <<"http://localhost:8080">>),
    Issuer = maps:get(issuer, Config, BaseURL),

    #{
        <<"issuer">> => Issuer,
        <<"authorization_endpoint">> => <<BaseURL/binary, "/oauth/authorize">>,
        <<"token_endpoint">> => <<BaseURL/binary, "/oauth/token">>,
        <<"userinfo_endpoint">> => <<BaseURL/binary, "/oauth/userinfo">>,
        <<"revocation_endpoint">> => <<BaseURL/binary, "/oauth/revoke">>,
        <<"jwks_uri">> => <<BaseURL/binary, "/.well-known/jwks.json">>,
        <<"registration_endpoint">> => <<BaseURL/binary, "/oauth/register">>,

        <<"scopes_supported">> => maps:get(supported_scopes, Config, [
            <<"openid">>, <<"profile">>, <<"email">>
        ]),

        <<"response_types_supported">> => [
            <<"code">>, <<"token">>, <<"id_token">>
        ],

        <<"grant_types_supported">> => maps:get(supported_grant_types, Config, [
            <<"authorization_code">>,
            <<"client_credentials">>,
            <<"refresh_token">>
        ]),

        <<"subject_types_supported">> => [<<"public">>],

        <<"id_token_signing_alg_values_supported">> => maps:get(
            supported_signing_algs, Config, [<<"RS256">>, <<"ES256">>]
        ),

        <<"token_endpoint_auth_methods_supported">> => [
            <<"client_secret_basic">>,
            <<"client_secret_post">>,
            <<"private_key_jwt">>
        ],

        <<"claims_supported">> => [
            <<"sub">>, <<"iss">>, <<"aud">>, <<"exp">>, <<"iat">>,
            <<"auth_time">>, <<"nonce">>, <<"name">>, <<"email">>
        ],

        <<"code_challenge_methods_supported">> => [<<"S256">>]
    }.

%% @doc Get cached JSON-encoded discovery document
-spec get_discovery_json() -> binary().
get_discovery_json() ->
    case ets:lookup(oidc_discovery_cache, discovery_json) of
        [{_, Json, ExpiresAt}] ->
            case erlang:system_time(second) < ExpiresAt of
                true -> Json;
                false ->
                    % Cache expired, regenerate
                    generate_and_cache_discovery()
            end;
        [] ->
            generate_and_cache_discovery()
    end.

%% @private Generate and cache discovery JSON
generate_and_cache_discovery() ->
    Config = get_discovery_config(),
    Json = jsx:encode(Config),
    ExpiresAt = erlang:system_time(second) + ?CACHE_TTL,

    % Ensure cache table exists
    ensure_cache_table(),
    ets:insert(oidc_discovery_cache, {discovery_json, Json, ExpiresAt}),
    Json.

%% @doc Invalidate discovery cache (call after config change)
-spec invalidate_cache() -> ok.
invalidate_cache() ->
    ets:delete(oidc_discovery_cache, discovery_json),
    ok.

%% @private Ensure ETS cache table exists
ensure_cache_table() ->
    case ets:whereis(oidc_discovery_cache) of
        undefined ->
            ets:new(oidc_discovery_cache, [set, public, named_table]);
        _ ->
            ok
    end.
```

**Tests:**
```erlang
% In erlmcp_oauth_oidc_discovery_tests.erl
get_discovery_config_has_required_fields_test() ->
    Config = erlmcp_oauth_oidc_discovery:get_discovery_config(),
    ?assert(maps:is_key(<<"issuer">>, Config)),
    ?assert(maps:is_key(<<"authorization_endpoint">>, Config)),
    ?assert(maps:is_key(<<"token_endpoint">>, Config)),
    ?assert(maps:is_key(<<"jwks_uri">>, Config)).

get_discovery_json_caches_response_test() ->
    Json1 = erlmcp_oauth_oidc_discovery:get_discovery_json(),
    Json2 = erlmcp_oauth_oidc_discovery:get_discovery_json(),
    ?assertEqual(Json1, Json2).

invalidate_cache_forces_regeneration_test() ->
    Json1 = erlmcp_oauth_oidc_discovery:get_discovery_json(),
    ok = erlmcp_oauth_oidc_discovery:invalidate_cache(),
    Json2 = erlmcp_oauth_oidc_discovery:get_discovery_json(),
    % Even if same, shows cache was regenerated
    ?assert(is_binary(Json2)).
```

---

#### Module 2: erlmcp_oauth_jwks.erl (250 LOC)

**Purpose:** Manage JSON Web Key Sets for signing/verification

**Exports:**
```erlang
-export([
    get_jwks/0,
    get_jwks_json/0,
    get_private_key/1,
    rotate_keys/0,
    invalidate_cache/0
]).
```

**Key Management:**
- RSA 2048-bit keys for RS256
- ECDSA P-256 keys for ES256
- Key rotation: 30-day interval
- Old keys retained 7 days (grace period)
- Keys stored in `erlmcp_oauth_keys` ETS table

**Implementation:**
```erlang
-module(erlmcp_oauth_jwks).
-export([get_jwks/0, get_jwks_json/0, get_private_key/1, rotate_keys/0]).

-record(key_entry, {
    kid :: binary(),
    use :: binary(),  % <<"sig">>
    alg :: binary(),  % <<"RS256">> | <<"ES256">>
    public_jwk :: map(),
    private_jwk :: map(),
    created_at :: integer(),
    expires_at :: integer()
}).

%% @doc Get JWKS public keys
-spec get_jwks() -> map().
get_jwks() ->
    Keys = get_active_public_keys(),
    #{<<"keys">> => Keys}.

%% @doc Get cached JWKS JSON
-spec get_jwks_json() -> binary().
get_jwks_json() ->
    Jwks = get_jwks(),
    jsx:encode(Jwks).

%% @doc Get private key for signing
-spec get_private_key(binary()) -> {ok, map()} | {error, not_found}.
get_private_key(Kid) ->
    case ets:lookup(erlmcp_oauth_keys, Kid) of
        [#key_entry{private_jwk = PrivateJWK}] ->
            {ok, PrivateJWK};
        [] ->
            {error, not_found}
    end.

%% @doc Rotate keys (generate new, retain old)
-spec rotate_keys() -> ok.
rotate_keys() ->
    % Generate new RSA key
    NewRSAKid = generate_kid(<<"RS256">>),
    RSAKey = jose_jwk:generate_key({rsa, 2048}),
    RSAPublic = jose_jwk:to_public(RSAKey),

    % Generate new ECDSA key
    NewECKid = generate_kid(<<"ES256">>),
    ECKey = jose_jwk:generate_key({ec, secp256r1}),
    ECPublic = jose_jwk:to_public(ECKey),

    Now = erlang:system_time(second),
    Expires = Now + (30 * 24 * 3600),  % 30 days

    % Store keys
    ets:insert(erlmcp_oauth_keys, #key_entry{
        kid = NewRSAKid,
        use = <<"sig">>,
        alg = <<"RS256">>,
        public_jwk = RSAPublic,
        private_jwk = RSAKey,
        created_at = Now,
        expires_at = Expires
    }),

    ets:insert(erlmcp_oauth_keys, #key_entry{
        kid = NewECKid,
        use = <<"sig">>,
        alg = <<"ES256">>,
        public_jwk = ECPublic,
        private_jwk = ECKey,
        created_at = Now,
        expires_at = Expires
    }),

    % Cleanup old keys (older than 7 days past expiration)
    cleanup_expired_keys(),
    ok.

%% @private Get active public keys for JWKS
get_active_public_keys() ->
    Now = erlang:system_time(second),
    GracePeriod = 7 * 24 * 3600,  % 7 days

    Keys = ets:tab2list(erlmcp_oauth_keys),
    ActiveKeys = lists:filter(fun(#key_entry{expires_at = Exp}) ->
        Exp + GracePeriod > Now
    end, Keys),

    lists:map(fun(#key_entry{kid = Kid, use = Use, alg = Alg, public_jwk = PubJWK}) ->
        #{
            <<"kid">> => Kid,
            <<"use">> => Use,
            <<"alg">> => Alg,
            <<"kty">> => maps:get(<<"kty">>, jose_jwk:to_map(PubJWK))
            % Additional JWK fields from public key
        }
    end, ActiveKeys).

%% @private Generate unique key ID
generate_kid(Alg) ->
    Timestamp = integer_to_binary(erlang:system_time(millisecond)),
    Random = base64:encode(crypto:strong_rand_bytes(8)),
    <<Alg/binary, "-", Timestamp/binary, "-", Random/binary>>.

%% @private Cleanup expired keys
cleanup_expired_keys() ->
    Now = erlang:system_time(second),
    GracePeriod = 7 * 24 * 3600,

    ets:foldl(fun(#key_entry{kid = Kid, expires_at = Exp}, Acc) ->
        case Exp + GracePeriod < Now of
            true ->
                ets:delete(erlmcp_oauth_keys, Kid),
                logger:info("Deleted expired key: ~p", [Kid]);
            false ->
                ok
        end,
        Acc
    end, ok, erlmcp_oauth_keys).
```

---

#### Module 3: erlmcp_oauth_authorization.erl (300 LOC)

**Purpose:** Handle OAuth 2.0 authorization code flow

**Endpoints:**
- `GET /oauth/authorize` - User authorization request
- `POST /oauth/token` - Token exchange

**State Management:**
- Authorization codes stored in ETS (10-minute TTL)
- PKCE code challenges validated on exchange
- One-time use enforcement

**Implementation:**
```erlang
-module(erlmcp_oauth_authorization).
-export([
    handle_authorize_request/1,
    handle_token_request/1,
    generate_auth_code/5,
    exchange_auth_code/4
]).

-record(auth_code, {
    code :: binary(),
    client_id :: binary(),
    user_id :: binary(),
    scopes :: [binary()],
    redirect_uri :: binary(),
    code_challenge :: binary() | undefined,
    code_challenge_method :: binary() | undefined,
    nonce :: binary() | undefined,
    created_at :: integer(),
    expires_at :: integer(),
    used :: boolean()
}).

%% @doc Handle GET /oauth/authorize request
-spec handle_authorize_request(map()) -> {redirect, binary()} | {error, term()}.
handle_authorize_request(Params) ->
    % Extract parameters
    ResponseType = maps:get(<<"response_type">>, Params),
    ClientId = maps:get(<<"client_id">>, Params),
    RedirectUri = maps:get(<<"redirect_uri">>, Params),
    Scope = maps:get(<<"scope">>, Params, <<"openid">>),
    State = maps:get(<<"state">>, Params, undefined),
    CodeChallenge = maps:get(<<"code_challenge">>, Params, undefined),
    CodeChallengeMethod = maps:get(<<"code_challenge_method">>, Params, undefined),
    Nonce = maps:get(<<"nonce">>, Params, undefined),

    % Validate client
    case validate_client(ClientId, RedirectUri) of
        ok ->
            % TODO: Authenticate user (session check or login form)
            % TODO: Show consent screen (if first authorization)
            UserId = <<"user123">>,  % Placeholder

            % Generate authorization code
            case ResponseType of
                <<"code">> ->
                    Code = generate_auth_code(
                        ClientId, UserId, binary:split(Scope, <<" ">>, [global]),
                        RedirectUri, CodeChallenge, CodeChallengeMethod, Nonce
                    ),

                    % Build redirect URL
                    RedirectUrl = build_redirect_url(RedirectUri, #{
                        <<"code">> => Code,
                        <<"state">> => State
                    }),
                    {redirect, RedirectUrl};
                _ ->
                    {error, unsupported_response_type}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Generate authorization code
-spec generate_auth_code(binary(), binary(), [binary()], binary(),
                          binary() | undefined, binary() | undefined,
                          binary() | undefined) -> binary().
generate_auth_code(ClientId, UserId, Scopes, RedirectUri,
                   CodeChallenge, CodeChallengeMethod, Nonce) ->
    Code = base64:encode(crypto:strong_rand_bytes(32)),
    Now = erlang:system_time(second),
    ExpiresAt = Now + 600,  % 10 minutes

    Entry = #auth_code{
        code = Code,
        client_id = ClientId,
        user_id = UserId,
        scopes = Scopes,
        redirect_uri = RedirectUri,
        code_challenge = CodeChallenge,
        code_challenge_method = CodeChallengeMethod,
        nonce = Nonce,
        created_at = Now,
        expires_at = ExpiresAt,
        used = false
    },

    ets:insert(erlmcp_oauth_auth_codes, {Code, Entry}),
    Code.

%% @doc Exchange authorization code for tokens
-spec exchange_auth_code(binary(), binary(), binary(), binary() | undefined) ->
    {ok, map()} | {error, term()}.
exchange_auth_code(Code, ClientId, RedirectUri, CodeVerifier) ->
    case ets:lookup(erlmcp_oauth_auth_codes, Code) of
        [{Code, #auth_code{
            client_id = ExpectedClientId,
            user_id = UserId,
            scopes = Scopes,
            redirect_uri = ExpectedRedirectUri,
            code_challenge = CodeChallenge,
            code_challenge_method = ChallengeMethod,
            nonce = Nonce,
            expires_at = ExpiresAt,
            used = Used
        }}] ->
            Now = erlang:system_time(second),

            % Validate code not expired
            case Now < ExpiresAt of
                false ->
                    {error, authorization_code_expired};
                true ->
                    % Validate code not used
                    case Used of
                        true ->
                            {error, authorization_code_used};
                        false ->
                            % Validate client and redirect URI
                            case {ClientId =:= ExpectedClientId,
                                  RedirectUri =:= ExpectedRedirectUri} of
                                {true, true} ->
                                    % Validate PKCE if present
                                    case validate_pkce(CodeChallenge, ChallengeMethod, CodeVerifier) of
                                        ok ->
                                            % Mark code as used
                                            ets:update_element(erlmcp_oauth_auth_codes, Code,
                                                             {#auth_code.used, true}),

                                            % Generate tokens
                                            AccessToken = erlmcp_oauth_tokens:create_access_token(
                                                ClientId, UserId, Scopes
                                            ),
                                            RefreshToken = erlmcp_oauth_tokens:create_refresh_token(
                                                ClientId, UserId, Scopes
                                            ),
                                            IdToken = erlmcp_oauth_id_token:create_id_token(
                                                UserId, ClientId, Nonce, Scopes
                                            ),

                                            {ok, #{
                                                <<"access_token">> => AccessToken,
                                                <<"token_type">> => <<"Bearer">>,
                                                <<"expires_in">> => 3600,
                                                <<"refresh_token">> => RefreshToken,
                                                <<"id_token">> => IdToken,
                                                <<"scope">> => lists:join(<<" ">>, Scopes)
                                            }};
                                        {error, Reason} ->
                                            {error, Reason}
                                    end;
                                {false, _} ->
                                    {error, invalid_client};
                                {_, false} ->
                                    {error, invalid_redirect_uri}
                            end
                    end
            end;
        [] ->
            {error, invalid_authorization_code}
    end.

%% @private Validate PKCE code verifier
validate_pkce(undefined, undefined, undefined) ->
    ok;  % PKCE not used
validate_pkce(CodeChallenge, <<"S256">>, CodeVerifier) when CodeVerifier =/= undefined ->
    Hash = crypto:hash(sha256, CodeVerifier),
    Challenge = base64:encode(Hash, #{mode => urlsafe, padding => false}),
    case Challenge =:= CodeChallenge of
        true -> ok;
        false -> {error, invalid_code_verifier}
    end;
validate_pkce(_, _, _) ->
    {error, pkce_required}.

%% @private Validate client and redirect URI
validate_client(ClientId, RedirectUri) ->
    % TODO: Lookup client in registry
    % Validate redirect_uri is registered for client
    ok.

%% @private Build OAuth redirect URL with query parameters
build_redirect_url(BaseUri, Params) ->
    Query = uri_string:compose_query([
        {K, V} || {K, V} <- maps:to_list(Params), V =/= undefined
    ]),
    <<BaseUri/binary, "?", Query/binary>>.
```

---

#### Module 4: erlmcp_oauth_id_token.erl (200 LOC)

**Purpose:** Create and validate OIDC ID tokens

**JWT Structure:**
```json
{
  "header": {
    "alg": "RS256",
    "typ": "JWT",
    "kid": "RS256-1738364800000-abc123"
  },
  "payload": {
    "iss": "https://erlmcp.example.com",
    "sub": "user123",
    "aud": "client_abc",
    "exp": 1738365100,
    "iat": 1738364800,
    "nonce": "xyz789",
    "name": "John Doe",
    "email": "john@example.com"
  }
}
```

**Implementation:**
```erlang
-module(erlmcp_oauth_id_token).
-export([
    create_id_token/4,
    validate_id_token/2,
    decode_id_token/1
]).

%% @doc Create signed ID token
-spec create_id_token(binary(), binary(), binary() | undefined, [binary()]) -> binary().
create_id_token(UserId, ClientId, Nonce, Scopes) ->
    Config = application:get_env(erlmcp_auth, oauth2, #{}),
    Issuer = maps:get(issuer, Config, <<"https://erlmcp.example.com">>),

    Now = erlang:system_time(second),
    Exp = Now + 300,  % 5 minutes

    % Build claims
    Claims = #{
        <<"iss">> => Issuer,
        <<"sub">> => UserId,
        <<"aud">> => ClientId,
        <<"exp">> => Exp,
        <<"iat">> => Now,
        <<"auth_time">> => Now
    },

    % Add nonce if provided
    ClaimsWithNonce = case Nonce of
        undefined -> Claims;
        _ -> Claims#{<<"nonce">> => Nonce}
    end,

    % Add user claims from scopes
    ClaimsWithUser = add_user_claims(ClaimsWithNonce, UserId, Scopes),

    % Get signing key
    {ok, PrivateKey} = erlmcp_oauth_jwks:get_private_key(<<"RS256-latest">>),

    % Create JWS (JSON Web Signature)
    Header = #{<<"alg">> => <<"RS256">>, <<"typ">> => <<"JWT">>},
    JWS = #{
        <<"header">> => Header,
        <<"payload">> => ClaimsWithUser
    },

    % Sign with JOSE library
    Signed = jose_jws:sign(PrivateKey, jsx:encode(ClaimsWithUser), Header),
    {_Module, Token} = jose_jws:compact(Signed),
    Token.

%% @doc Validate ID token signature and claims
-spec validate_id_token(binary(), binary()) -> {ok, map()} | {error, term()}.
validate_id_token(Token, ClientId) ->
    case jose_jws:verify_strict(erlmcp_oauth_jwks:get_jwks(), [<<"RS256">>], Token) of
        {true, Payload, _JWS} ->
            Claims = jsx:decode(Payload, [return_maps]),
            validate_id_token_claims(Claims, ClientId);
        {false, _, _} ->
            {error, invalid_signature}
    end.

%% @private Validate ID token claims
validate_id_token_claims(Claims, ClientId) ->
    Now = erlang:system_time(second),

    % Validate required claims
    case {
        maps:get(<<"aud">>, Claims, undefined),
        maps:get(<<"exp">>, Claims, undefined),
        maps:get(<<"iss">>, Claims, undefined)
    } of
        {ClientId, Exp, _Iss} when is_integer(Exp), Exp > Now ->
            {ok, Claims};
        {ClientId, Exp, _Iss} when is_integer(Exp) ->
            {error, token_expired};
        {WrongAud, _, _} when WrongAud =/= ClientId ->
            {error, invalid_audience};
        _ ->
            {error, invalid_claims}
    end.

%% @doc Decode ID token without validation
-spec decode_id_token(binary()) -> {ok, map()} | {error, term()}.
decode_id_token(Token) ->
    try
        Payload = jose_jws:peek_payload(Token),
        Claims = jsx:decode(Payload, [return_maps]),
        {ok, Claims}
    catch
        _:_ -> {error, invalid_token_format}
    end.

%% @private Add user claims based on scopes
add_user_claims(Claims, UserId, Scopes) ->
    % TODO: Fetch user profile from database
    BaseUser = #{
        <<"name">> => <<"User ", UserId/binary>>,
        <<"email">> => <<UserId/binary, "@example.com">>
    },

    % Filter claims by scopes
    case lists:member(<<"profile">>, Scopes) of
        true ->
            ProfileClaims = maps:with([<<"name">>], BaseUser),
            EmailClaims = case lists:member(<<"email">>, Scopes) of
                true -> maps:with([<<"email">>], BaseUser);
                false -> #{}
            end,
            maps:merge(Claims, maps:merge(ProfileClaims, EmailClaims));
        false ->
            Claims
    end.
```

---

#### Module 5: erlmcp_oauth_userinfo.erl (150 LOC)

**Purpose:** Serve user information endpoint

**Endpoint:** `GET /oauth/userinfo`
**Auth:** Bearer token required

**Implementation:**
```erlang
-module(erlmcp_oauth_userinfo).
-export([handle_userinfo_request/1]).

%% @doc Handle GET /oauth/userinfo
-spec handle_userinfo_request(binary()) -> {ok, map()} | {error, term()}.
handle_userinfo_request(AccessToken) ->
    % Validate access token
    case erlmcp_auth:validate_oauth2_token(AccessToken) of
        {ok, TokenInfo} ->
            UserId = maps:get(<<"user_id">>, TokenInfo),
            Scopes = maps:get(<<"scope">>, TokenInfo, <<"">>),
            ScopeList = binary:split(Scopes, <<" ">>, [global]),

            % Fetch user claims
            UserClaims = fetch_user_claims(UserId, ScopeList),
            {ok, UserClaims};
        {error, Reason} ->
            {error, Reason}
    end.

%% @private Fetch user claims based on scopes
fetch_user_claims(UserId, Scopes) ->
    % Base claim (always included)
    BaseClaims = #{<<"sub">> => UserId},

    % Add profile claims if scope granted
    ProfileClaims = case lists:member(<<"profile">>, Scopes) of
        true ->
            #{
                <<"name">> => <<"User ", UserId/binary>>,
                <<"given_name">> => <<"User">>,
                <<"family_name">> => UserId,
                <<"picture">> => <<"https://example.com/avatar/", UserId/binary>>
            };
        false ->
            #{}
    end,

    % Add email claims if scope granted
    EmailClaims = case lists:member(<<"email">>, Scopes) of
        true ->
            #{
                <<"email">> => <<UserId/binary, "@example.com">>,
                <<"email_verified">> => true
            };
        false ->
            #{}
    end,

    maps:merge(BaseClaims, maps:merge(ProfileClaims, EmailClaims)).
```

---

#### Module 6: erlmcp_oauth_scope_handler.erl (250 LOC)

**Purpose:** Manage incremental scope consent

**Functions:**
- `validate_scopes/2` - Check token has required scopes
- `request_additional_scopes/3` - Initiate consent for new scopes
- `combine_scopes/2` - Merge scopes from multiple authorizations

**Implementation:**
```erlang
-module(erlmcp_oauth_scope_handler).
-export([
    validate_scopes/2,
    request_additional_scopes/3,
    combine_scopes/2,
    get_user_consents/2
]).

%% @doc Validate token has required scopes
-spec validate_scopes([binary()], [binary()]) -> ok | {error, insufficient_scope}.
validate_scopes(RequiredScopes, GrantedScopes) ->
    case lists:all(fun(Scope) -> lists:member(Scope, GrantedScopes) end, RequiredScopes) of
        true -> ok;
        false -> {error, insufficient_scope}
    end.

%% @doc Request additional scopes (incremental consent)
-spec request_additional_scopes(binary(), binary(), [binary()]) ->
    {redirect, binary()} | {error, term()}.
request_additional_scopes(ClientId, UserId, NewScopes) ->
    % Check which scopes are already granted
    ExistingScopes = get_granted_scopes(ClientId, UserId),
    AdditionalScopes = NewScopes -- ExistingScopes,

    case AdditionalScopes of
        [] ->
            % All scopes already granted
            {ok, already_granted};
        _ ->
            % Trigger consent flow for additional scopes
            ConsentUrl = build_consent_url(ClientId, UserId, AdditionalScopes),
            {redirect, ConsentUrl}
    end.

%% @doc Combine scopes from multiple authorizations
-spec combine_scopes([binary()], [binary()]) -> [binary()].
combine_scopes(Scopes1, Scopes2) ->
    lists:usort(Scopes1 ++ Scopes2).

%% @doc Get user's granted scopes for client
-spec get_user_consents(binary(), binary()) -> [binary()].
get_user_consents(ClientId, UserId) ->
    case ets:lookup(erlmcp_oauth_consents, {ClientId, UserId}) of
        [{_, Scopes, _GrantedAt}] -> Scopes;
        [] -> []
    end.

%% @private Get granted scopes
get_granted_scopes(ClientId, UserId) ->
    get_user_consents(ClientId, UserId).

%% @private Build consent URL
build_consent_url(ClientId, UserId, Scopes) ->
    Query = uri_string:compose_query([
        {<<"client_id">>, ClientId},
        {<<"user_id">>, UserId},
        {<<"scopes">>, lists:join(<<" ">>, Scopes)}
    ]),
    <<"/oauth/consent?", Query/binary>>.
```

---

### 5. HTTP Endpoint Registration

**File:** `/home/user/erlmcp/apps/erlmcp_transports/src/erlmcp_oauth_http_handler.erl` (NEW)

**Purpose:** Cowboy HTTP handler for OAuth2/OIDC endpoints

```erlang
-module(erlmcp_oauth_http_handler).
-export([init/2]).

init(Req, State) ->
    Path = cowboy_req:path(Req),
    Method = cowboy_req:method(Req),

    Response = case {Method, Path} of
        {<<"GET">>, <<"/.well-known/openid-configuration">>} ->
            handle_oidc_discovery(Req);

        {<<"GET">>, <<"/.well-known/jwks.json">>} ->
            handle_jwks(Req);

        {<<"GET">>, <<"/oauth/authorize">>} ->
            handle_authorize(Req);

        {<<"POST">>, <<"/oauth/token">>} ->
            handle_token(Req);

        {<<"GET">>, <<"/oauth/userinfo">>} ->
            handle_userinfo(Req);

        {<<"POST">>, <<"/oauth/revoke">>} ->
            handle_revoke(Req);

        _ ->
            {404, #{}, <<"Not Found">>}
    end,

    {Status, Headers, Body} = Response,
    Req2 = cowboy_req:reply(Status, Headers, Body, Req),
    {ok, Req2, State}.

handle_oidc_discovery(_Req) ->
    Json = erlmcp_oauth_oidc_discovery:get_discovery_json(),
    {200, #{
        <<"content-type">> => <<"application/json">>,
        <<"cache-control">> => <<"public, max-age=3600">>
    }, Json}.

handle_jwks(_Req) ->
    Json = erlmcp_oauth_jwks:get_jwks_json(),
    {200, #{
        <<"content-type">> => <<"application/json">>,
        <<"cache-control">> => <<"public, max-age=3600">>
    }, Json}.

handle_authorize(Req) ->
    Params = cowboy_req:parse_qs(Req),
    ParamsMap = maps:from_list(Params),

    case erlmcp_oauth_authorization:handle_authorize_request(ParamsMap) of
        {redirect, Url} ->
            {302, #{<<"location">> => Url}, <<>>};
        {error, Reason} ->
            {400, #{}, jsx:encode(#{<<"error">> => Reason})}
    end.

handle_token(Req) ->
    {ok, Body, _Req2} = cowboy_req:read_urlencoded_body(Req),
    Params = maps:from_list(Body),

    GrantType = maps:get(<<"grant_type">>, Params),

    Result = case GrantType of
        <<"authorization_code">> ->
            Code = maps:get(<<"code">>, Params),
            ClientId = maps:get(<<"client_id">>, Params),
            RedirectUri = maps:get(<<"redirect_uri">>, Params),
            CodeVerifier = maps:get(<<"code_verifier">>, Params, undefined),

            erlmcp_oauth_authorization:exchange_auth_code(
                Code, ClientId, RedirectUri, CodeVerifier
            );

        <<"refresh_token">> ->
            RefreshToken = maps:get(<<"refresh_token">>, Params),
            ClientId = maps:get(<<"client_id">>, Params),

            erlmcp_oauth_tokens:refresh_access_token(RefreshToken, ClientId);

        _ ->
            {error, unsupported_grant_type}
    end,

    case Result of
        {ok, TokenResponse} ->
            {200, #{<<"content-type">> => <<"application/json">>},
             jsx:encode(TokenResponse)};
        {error, Reason} ->
            {400, #{}, jsx:encode(#{<<"error">> => Reason})}
    end.

handle_userinfo(Req) ->
    case cowboy_req:header(<<"authorization">>, Req) of
        <<"Bearer ", Token/binary>> ->
            case erlmcp_oauth_userinfo:handle_userinfo_request(Token) of
                {ok, UserClaims} ->
                    {200, #{<<"content-type">> => <<"application/json">>},
                     jsx:encode(UserClaims)};
                {error, _Reason} ->
                    {401, #{}, <<"Unauthorized">>}
            end;
        _ ->
            {401, #{}, <<"Unauthorized">>}
    end.

handle_revoke(Req) ->
    {ok, Body, _Req2} = cowboy_req:read_urlencoded_body(Req),
    Params = maps:from_list(Body),
    Token = maps:get(<<"token">>, Params),

    ok = erlmcp_auth:revoke_token(Token),
    {200, #{}, <<>>}.
```

**Route Configuration:**

```erlang
% In erlmcp_transports application startup
Dispatch = cowboy_router:compile([
    {'_', [
        {"/.well-known/[...]", erlmcp_oauth_http_handler, []},
        {"/oauth/[...]", erlmcp_oauth_http_handler, []},
        {"/[...]", erlmcp_default_handler, []}
    ]}
]),

{ok, _} = cowboy:start_clear(http_listener,
    [{port, 8080}],
    #{env => #{dispatch => Dispatch}}
).
```

---

## 6. Configuration

**File:** `/home/user/erlmcp/config/sys.config`

Add OAuth2/OIDC configuration:

```erlang
{erlmcp_auth, [
    {oauth2, [
        {enabled, true},
        {issuer, "https://erlmcp.example.com"},
        {base_url, "http://localhost:8080"},

        {supported_scopes, [
            <<"openid">>,
            <<"profile">>,
            <<"email">>,
            <<"address">>,
            <<"phone">>,
            <<"offline_access">>
        ]},

        {supported_grant_types, [
            <<"authorization_code">>,
            <<"client_credentials">>,
            <<"refresh_token">>
        ]},

        {supported_signing_algs, [
            <<"RS256">>,
            <<"ES256">>
        ]},

        {key_rotation_days, 30},
        {token_ttl_seconds, 3600},
        {refresh_token_ttl_seconds, 2592000},  % 30 days
        {id_token_ttl_seconds, 300},
        {auth_code_ttl_seconds, 600},

        {pkce_required, false},  % Set true to enforce PKCE
        {pkce_methods, [<<"S256">>]},

        {cors_enabled, true},
        {cors_allowed_origins, [<<"*">>]}
    ]}
]}.
```

**Development Configuration:**

```erlang
% In config/development.config
{erlmcp_auth, [
    {oauth2, [
        {enabled, true},
        {issuer, "http://localhost:8080"},
        {base_url, "http://localhost:8080"},
        {pkce_required, false},  % More lenient for development
        {token_ttl_seconds, 7200}  % 2 hours for easier debugging
    ]}
]}.
```

**Production Configuration:**

```erlang
% In config/production.config
{erlmcp_auth, [
    {oauth2, [
        {enabled, true},
        {issuer, "https://auth.erlmcp.example.com"},
        {base_url, "https://auth.erlmcp.example.com"},
        {pkce_required, true},  % ENFORCE PKCE in production
        {cors_allowed_origins, [
            <<"https://app.erlmcp.example.com">>,
            <<"https://admin.erlmcp.example.com">>
        ]}
    ]}
]}.
```

---

## 7. Testing Plan

### Unit Tests

#### Test Suite 1: erlmcp_oauth_oidc_discovery_tests.erl (150 LOC)

```erlang
-module(erlmcp_oauth_oidc_discovery_tests).
-include_lib("eunit/include/eunit.hrl").

%% Discovery metadata tests
discovery_has_required_fields_test() ->
    Config = erlmcp_oauth_oidc_discovery:get_discovery_config(),
    ?assert(maps:is_key(<<"issuer">>, Config)),
    ?assert(maps:is_key(<<"authorization_endpoint">>, Config)),
    ?assert(maps:is_key(<<"token_endpoint">>, Config)),
    ?assert(maps:is_key(<<"jwks_uri">>, Config)),
    ?assert(maps:is_key(<<"scopes_supported">>, Config)),
    ?assert(maps:is_key(<<"grant_types_supported">>, Config)).

discovery_endpoints_match_base_url_test() ->
    application:set_env(erlmcp_auth, oauth2, #{
        base_url => <<"https://example.com">>
    }),
    Config = erlmcp_oauth_oidc_discovery:get_discovery_config(),
    AuthzEndpoint = maps:get(<<"authorization_endpoint">>, Config),
    ?assertEqual(<<"https://example.com/oauth/authorize">>, AuthzEndpoint).

discovery_json_is_valid_json_test() ->
    Json = erlmcp_oauth_oidc_discovery:get_discovery_json(),
    ?assert(is_binary(Json)),
    Decoded = jsx:decode(Json, [return_maps]),
    ?assert(is_map(Decoded)).

discovery_cache_works_test() ->
    Json1 = erlmcp_oauth_oidc_discovery:get_discovery_json(),
    Json2 = erlmcp_oauth_oidc_discovery:get_discovery_json(),
    ?assertEqual(Json1, Json2).

discovery_cache_invalidation_test() ->
    _Json1 = erlmcp_oauth_oidc_discovery:get_discovery_json(),
    ok = erlmcp_oauth_oidc_discovery:invalidate_cache(),
    Json2 = erlmcp_oauth_oidc_discovery:get_discovery_json(),
    ?assert(is_binary(Json2)).
```

#### Test Suite 2: erlmcp_oauth_jwks_tests.erl (150 LOC)

```erlang
-module(erlmcp_oauth_jwks_tests).
-include_lib("eunit/include/eunit.hrl").

jwks_has_keys_array_test() ->
    Jwks = erlmcp_oauth_jwks:get_jwks(),
    ?assert(maps:is_key(<<"keys">>, Jwks)),
    Keys = maps:get(<<"keys">>, Jwks),
    ?assert(is_list(Keys)).

jwks_keys_have_required_fields_test() ->
    ok = erlmcp_oauth_jwks:rotate_keys(),
    Jwks = erlmcp_oauth_jwks:get_jwks(),
    [Key | _] = maps:get(<<"keys">>, Jwks),
    ?assert(maps:is_key(<<"kid">>, Key)),
    ?assert(maps:is_key(<<"use">>, Key)),
    ?assert(maps:is_key(<<"alg">>, Key)).

key_rotation_generates_new_keys_test() ->
    Jwks1 = erlmcp_oauth_jwks:get_jwks(),
    ok = erlmcp_oauth_jwks:rotate_keys(),
    Jwks2 = erlmcp_oauth_jwks:get_jwks(),

    Keys1 = maps:get(<<"keys">>, Jwks1),
    Keys2 = maps:get(<<"keys">>, Jwks2),

    % Should have more keys after rotation (old + new)
    ?assert(length(Keys2) >= length(Keys1)).

private_key_retrieval_test() ->
    ok = erlmcp_oauth_jwks:rotate_keys(),
    Jwks = erlmcp_oauth_jwks:get_jwks(),
    [Key | _] = maps:get(<<"keys">>, Jwks),
    Kid = maps:get(<<"kid">>, Key),

    {ok, PrivateKey} = erlmcp_oauth_jwks:get_private_key(Kid),
    ?assert(is_map(PrivateKey)).
```

#### Test Suite 3: erlmcp_oauth_authorization_flow_tests.erl (250 LOC)

```erlang
-module(erlmcp_oauth_authorization_flow_tests).
-include_lib("eunit/include/eunit.hrl").

authorization_code_generation_test() ->
    Code = erlmcp_oauth_authorization:generate_auth_code(
        <<"client123">>,
        <<"user456">>,
        [<<"openid">>, <<"profile">>],
        <<"https://example.com/callback">>,
        undefined,
        undefined,
        undefined
    ),
    ?assert(is_binary(Code)),
    ?assert(byte_size(Code) > 32).

authorization_code_exchange_success_test() ->
    % Generate code
    Code = erlmcp_oauth_authorization:generate_auth_code(
        <<"client123">>,
        <<"user456">>,
        [<<"openid">>, <<"profile">>],
        <<"https://example.com/callback">>,
        undefined,
        undefined,
        <<"nonce123">>
    ),

    % Exchange code
    {ok, TokenResponse} = erlmcp_oauth_authorization:exchange_auth_code(
        Code,
        <<"client123">>,
        <<"https://example.com/callback">>,
        undefined
    ),

    ?assert(maps:is_key(<<"access_token">>, TokenResponse)),
    ?assert(maps:is_key(<<"refresh_token">>, TokenResponse)),
    ?assert(maps:is_key(<<"id_token">>, TokenResponse)),
    ?assertEqual(<<"Bearer">>, maps:get(<<"token_type">>, TokenResponse)).

authorization_code_single_use_test() ->
    Code = erlmcp_oauth_authorization:generate_auth_code(
        <<"client123">>,
        <<"user456">>,
        [<<"openid">>],
        <<"https://example.com/callback">>,
        undefined,
        undefined,
        undefined
    ),

    % First exchange succeeds
    {ok, _} = erlmcp_oauth_authorization:exchange_auth_code(
        Code, <<"client123">>, <<"https://example.com/callback">>, undefined
    ),

    % Second exchange fails
    {error, authorization_code_used} = erlmcp_oauth_authorization:exchange_auth_code(
        Code, <<"client123">>, <<"https://example.com/callback">>, undefined
    ).

pkce_validation_success_test() ->
    CodeVerifier = base64:encode(crypto:strong_rand_bytes(32)),
    CodeChallenge = base64:encode(crypto:hash(sha256, CodeVerifier), #{
        mode => urlsafe, padding => false
    }),

    Code = erlmcp_oauth_authorization:generate_auth_code(
        <<"client123">>,
        <<"user456">>,
        [<<"openid">>],
        <<"https://example.com/callback">>,
        CodeChallenge,
        <<"S256">>,
        undefined
    ),

    {ok, _} = erlmcp_oauth_authorization:exchange_auth_code(
        Code, <<"client123">>, <<"https://example.com/callback">>, CodeVerifier
    ).

pkce_validation_failure_test() ->
    CodeVerifier = base64:encode(crypto:strong_rand_bytes(32)),
    WrongVerifier = base64:encode(crypto:strong_rand_bytes(32)),
    CodeChallenge = base64:encode(crypto:hash(sha256, CodeVerifier), #{
        mode => urlsafe, padding => false
    }),

    Code = erlmcp_oauth_authorization:generate_auth_code(
        <<"client123">>,
        <<"user456">>,
        [<<"openid">>],
        <<"https://example.com/callback">>,
        CodeChallenge,
        <<"S256">>,
        undefined
    ),

    {error, invalid_code_verifier} = erlmcp_oauth_authorization:exchange_auth_code(
        Code, <<"client123">>, <<"https://example.com/callback">>, WrongVerifier
    ).
```

#### Test Suite 4: erlmcp_oauth_id_token_tests.erl (150 LOC)

```erlang
-module(erlmcp_oauth_id_token_tests).
-include_lib("eunit/include/eunit.hrl").

id_token_creation_test() ->
    IdToken = erlmcp_oauth_id_token:create_id_token(
        <<"user123">>,
        <<"client456">>,
        <<"nonce789">>,
        [<<"openid">>, <<"profile">>]
    ),
    ?assert(is_binary(IdToken)),
    % JWT has 3 parts: header.payload.signature
    ?assertEqual(3, length(binary:split(IdToken, <<".">>, [global]))).

id_token_validation_success_test() ->
    IdToken = erlmcp_oauth_id_token:create_id_token(
        <<"user123">>,
        <<"client456">>,
        <<"nonce789">>,
        [<<"openid">>, <<"profile">>]
    ),

    {ok, Claims} = erlmcp_oauth_id_token:validate_id_token(
        IdToken, <<"client456">>
    ),

    ?assertEqual(<<"user123">>, maps:get(<<"sub">>, Claims)),
    ?assertEqual(<<"client456">>, maps:get(<<"aud">>, Claims)),
    ?assertEqual(<<"nonce789">>, maps:get(<<"nonce">>, Claims)).

id_token_expired_validation_test() ->
    % Create token with past expiration
    Claims = #{
        <<"iss">> => <<"https://erlmcp.example.com">>,
        <<"sub">> => <<"user123">>,
        <<"aud">> => <<"client456">>,
        <<"exp">> => erlang:system_time(second) - 3600,  % 1 hour ago
        <<"iat">> => erlang:system_time(second) - 7200
    },

    % Sign manually for testing
    {ok, PrivateKey} = erlmcp_oauth_jwks:get_private_key(<<"RS256-latest">>),
    Header = #{<<"alg">> => <<"RS256">>, <<"typ">> => <<"JWT">>},
    Signed = jose_jws:sign(PrivateKey, jsx:encode(Claims), Header),
    {_Module, Token} = jose_jws:compact(Signed),

    {error, token_expired} = erlmcp_oauth_id_token:validate_id_token(
        Token, <<"client456">>
    ).

id_token_decode_test() ->
    IdToken = erlmcp_oauth_id_token:create_id_token(
        <<"user123">>,
        <<"client456">>,
        undefined,
        [<<"openid">>]
    ),

    {ok, Claims} = erlmcp_oauth_id_token:decode_id_token(IdToken),
    ?assert(is_map(Claims)),
    ?assertEqual(<<"user123">>, maps:get(<<"sub">>, Claims)).
```

---

### Integration Tests

#### Test Suite 5: erlmcp_oauth_integration_SUITE.erl (400 LOC)

```erlang
-module(erlmcp_oauth_integration_SUITE).
-include_lib("common_test/include/ct.hrl").

all() -> [
    complete_authorization_code_flow,
    pkce_protected_flow,
    incremental_consent_flow,
    token_refresh_flow,
    token_revocation_flow,
    userinfo_endpoint_flow
].

init_per_suite(Config) ->
    application:ensure_all_started(erlmcp_auth),
    application:ensure_all_started(erlmcp_transports),
    erlmcp_oauth_jwks:rotate_keys(),
    Config.

end_per_suite(_Config) ->
    application:stop(erlmcp_transports),
    application:stop(erlmcp_auth),
    ok.

complete_authorization_code_flow(_Config) ->
    % Step 1: Request authorization code
    AuthParams = #{
        <<"response_type">> => <<"code">>,
        <<"client_id">> => <<"test_client">>,
        <<"redirect_uri">> => <<"https://example.com/callback">>,
        <<"scope">> => <<"openid profile email">>,
        <<"state">> => <<"random_state_123">>
    },

    {redirect, RedirectUrl} = erlmcp_oauth_authorization:handle_authorize_request(AuthParams),

    % Step 2: Extract authorization code from redirect
    #{query := Query} = uri_string:parse(RedirectUrl),
    QueryParams = uri_string:dissect_query(Query),
    Code = proplists:get_value(<<"code">>, QueryParams),
    State = proplists:get_value(<<"state">>, QueryParams),

    ct:pal("Received code: ~p, state: ~p", [Code, State]),
    ?assertEqual(<<"random_state_123">>, State),

    % Step 3: Exchange code for tokens
    {ok, TokenResponse} = erlmcp_oauth_authorization:exchange_auth_code(
        Code,
        <<"test_client">>,
        <<"https://example.com/callback">>,
        undefined
    ),

    AccessToken = maps:get(<<"access_token">>, TokenResponse),
    RefreshToken = maps:get(<<"refresh_token">>, TokenResponse),
    IdToken = maps:get(<<"id_token">>, TokenResponse),

    ct:pal("Tokens received: access=~p, refresh=~p, id=~p",
           [AccessToken, RefreshToken, IdToken]),

    ?assert(is_binary(AccessToken)),
    ?assert(is_binary(RefreshToken)),
    ?assert(is_binary(IdToken)),

    % Step 4: Validate ID token
    {ok, IdClaims} = erlmcp_oauth_id_token:validate_id_token(IdToken, <<"test_client">>),
    ?assert(maps:is_key(<<"sub">>, IdClaims)),

    % Step 5: Call userinfo endpoint
    {ok, UserClaims} = erlmcp_oauth_userinfo:handle_userinfo_request(AccessToken),
    ?assert(maps:is_key(<<"sub">>, UserClaims)),
    ?assert(maps:is_key(<<"email">>, UserClaims)).

pkce_protected_flow(_Config) ->
    % Generate PKCE parameters
    CodeVerifier = base64:encode(crypto:strong_rand_bytes(32), #{
        mode => urlsafe, padding => false
    }),
    CodeChallenge = base64:encode(crypto:hash(sha256, CodeVerifier), #{
        mode => urlsafe, padding => false
    }),

    % Request authorization with PKCE
    Code = erlmcp_oauth_authorization:generate_auth_code(
        <<"test_client">>,
        <<"user123">>,
        [<<"openid">>],
        <<"https://example.com/callback">>,
        CodeChallenge,
        <<"S256">>,
        undefined
    ),

    % Exchange with correct verifier
    {ok, _} = erlmcp_oauth_authorization:exchange_auth_code(
        Code,
        <<"test_client">>,
        <<"https://example.com/callback">>,
        CodeVerifier
    ),

    ok.

token_refresh_flow(_Config) ->
    % Issue initial tokens
    Code = erlmcp_oauth_authorization:generate_auth_code(
        <<"test_client">>,
        <<"user123">>,
        [<<"openid">>, <<"offline_access">>],
        <<"https://example.com/callback">>,
        undefined,
        undefined,
        undefined
    ),

    {ok, TokenResponse} = erlmcp_oauth_authorization:exchange_auth_code(
        Code, <<"test_client">>, <<"https://example.com/callback">>, undefined
    ),

    RefreshToken = maps:get(<<"refresh_token">>, TokenResponse),

    % Refresh access token
    {ok, NewTokenResponse} = erlmcp_oauth_tokens:refresh_access_token(
        RefreshToken, <<"test_client">>
    ),

    NewAccessToken = maps:get(<<"access_token">>, NewTokenResponse),
    NewRefreshToken = maps:get(<<"refresh_token">>, NewTokenResponse),

    ?assert(is_binary(NewAccessToken)),
    ?assert(is_binary(NewRefreshToken)),
    ?assertNotEqual(RefreshToken, NewRefreshToken).  % Rotation
```

---

## 8. Database Schema

For persistent storage (optional, ETS default for development):

### Table: oauth_auth_codes
```sql
CREATE TABLE oauth_auth_codes (
    code TEXT PRIMARY KEY,
    client_id TEXT NOT NULL,
    user_id TEXT NOT NULL,
    scopes TEXT[] NOT NULL,
    redirect_uri TEXT NOT NULL,
    code_challenge TEXT,
    code_challenge_method TEXT,
    nonce TEXT,
    created_at INTEGER NOT NULL,
    expires_at INTEGER NOT NULL,
    used BOOLEAN DEFAULT FALSE
);

CREATE INDEX idx_oauth_auth_codes_expires ON oauth_auth_codes(expires_at);
```

### Table: oauth_tokens
```sql
CREATE TABLE oauth_tokens (
    token_id TEXT PRIMARY KEY,
    client_id TEXT NOT NULL,
    user_id TEXT NOT NULL,
    token_type TEXT NOT NULL,  -- 'access', 'refresh', 'id'
    scopes TEXT[] NOT NULL,
    created_at INTEGER NOT NULL,
    expires_at INTEGER NOT NULL,
    revoked BOOLEAN DEFAULT FALSE,
    parent_token_id TEXT  -- For refresh token lineage
);

CREATE INDEX idx_oauth_tokens_user ON oauth_tokens(user_id);
CREATE INDEX idx_oauth_tokens_client ON oauth_tokens(client_id);
CREATE INDEX idx_oauth_tokens_expires ON oauth_tokens(expires_at);
```

### Table: oauth_consents
```sql
CREATE TABLE oauth_consents (
    id SERIAL PRIMARY KEY,
    client_id TEXT NOT NULL,
    user_id TEXT NOT NULL,
    scopes TEXT[] NOT NULL,
    granted_at INTEGER NOT NULL,
    UNIQUE(client_id, user_id)
);

CREATE INDEX idx_oauth_consents_user ON oauth_consents(user_id);
```

### ETS Implementation (Default)

```erlang
% In erlmcp_auth:init/1
State#state{
    oauth_auth_codes = ets:new(erlmcp_oauth_auth_codes, [set, protected]),
    oauth_tokens = ets:new(erlmcp_oauth_tokens, [set, protected]),
    oauth_consents = ets:new(erlmcp_oauth_consents, [set, protected]),
    oauth_keys = ets:new(erlmcp_oauth_keys, [set, protected])
}
```

---

## 9. Security Considerations

### PKCE Support (RFC 7636)

**Rationale:** Prevents authorization code interception attacks (mobile apps, SPAs)

**Implementation:**
```erlang
% In erlmcp_oauth_authorization:validate_pkce/3
validate_pkce(CodeChallenge, <<"S256">>, CodeVerifier) ->
    Hash = crypto:hash(sha256, CodeVerifier),
    Challenge = base64:encode(Hash, #{mode => urlsafe, padding => false}),
    case Challenge =:= CodeChallenge of
        true -> ok;
        false -> {error, invalid_code_verifier}
    end.
```

**Configuration:**
```erlang
{oauth2, [
    {pkce_required, true},  % ENFORCE in production
    {pkce_methods, [<<"S256">>]}  % Only SHA-256, not 'plain'
]}
```

### Client Authentication

**Methods:**
1. **client_secret_basic** (HTTP Basic Auth)
2. **client_secret_post** (POST body)
3. **private_key_jwt** (asymmetric, future)

**Implementation:**
```erlang
% In erlmcp_oauth_http_handler:handle_token/1
authenticate_client(Req, Params) ->
    case cowboy_req:header(<<"authorization">>, Req) of
        <<"Basic ", Encoded/binary>> ->
            Decoded = base64:decode(Encoded),
            [ClientId, ClientSecret] = binary:split(Decoded, <<":">>),
            validate_client_secret(ClientId, ClientSecret);
        undefined ->
            ClientId = maps:get(<<"client_id">>, Params),
            ClientSecret = maps:get(<<"client_secret">>, Params),
            validate_client_secret(ClientId, ClientSecret)
    end.
```

### Scope Validation

**Enforcement:**
- Scopes must be pre-registered for each client
- Token introspection returns only granted scopes
- Userinfo endpoint filters claims by scopes

**Implementation:**
```erlang
% In erlmcp_oauth_scope_handler:validate_scopes/2
validate_scopes(RequiredScopes, GrantedScopes) ->
    Missing = RequiredScopes -- GrantedScopes,
    case Missing of
        [] -> ok;
        _ -> {error, {insufficient_scope, Missing}}
    end.
```

### Token Revocation Audit

**Compliance:** SOC 2, GDPR, HIPAA require audit trails

**Implementation:**
```erlang
% In erlmcp_auth:revoke_token/1
revoke_token(Token) ->
    gen_server:call(?MODULE, {revoke_token, Token}),

    % Write to immutable audit log
    erlmcp_audit_log:record(#{
        event => token_revoked,
        token_hash => crypto:hash(sha256, Token),
        timestamp => erlang:system_time(second),
        reason => user_requested
    }),
    ok.
```

### Rate Limiting

**Enforcement:**
- 10 authorization requests per minute per client
- 5 token exchanges per minute per client
- 100 userinfo requests per minute per token

**Implementation:**
```erlang
% In erlmcp_oauth_http_handler (before handle_authorize)
case erlmcp_rate_limiter:check_rate_limit(ClientId, authorization) of
    ok -> handle_authorize(Req);
    {error, rate_limited} -> {429, #{}, <<"Too Many Requests">>}
end.
```

---

## 10. Files Summary

### New Files (CREATE)

| File | LOC | Purpose |
|------|-----|---------|
| `apps/erlmcp_core/src/erlmcp_oauth_oidc_discovery.erl` | 200 | OIDC Discovery metadata |
| `apps/erlmcp_core/src/erlmcp_oauth_jwks.erl` | 250 | JWKS endpoint, key management |
| `apps/erlmcp_core/src/erlmcp_oauth_authorization.erl` | 300 | Authorization code flow |
| `apps/erlmcp_core/src/erlmcp_oauth_id_token.erl` | 200 | ID token creation/validation |
| `apps/erlmcp_core/src/erlmcp_oauth_userinfo.erl` | 150 | Userinfo endpoint |
| `apps/erlmcp_core/src/erlmcp_oauth_scope_handler.erl` | 250 | Incremental consent |
| `apps/erlmcp_transports/src/erlmcp_oauth_http_handler.erl` | 200 | HTTP route handler |
| `apps/erlmcp_core/test/erlmcp_oauth_oidc_discovery_tests.erl` | 150 | Discovery tests |
| `apps/erlmcp_core/test/erlmcp_oauth_jwks_tests.erl` | 150 | JWKS tests |
| `apps/erlmcp_core/test/erlmcp_oauth_authorization_flow_tests.erl` | 250 | Auth flow tests |
| `apps/erlmcp_core/test/erlmcp_oauth_id_token_tests.erl` | 150 | ID token tests |
| `apps/erlmcp_core/test/erlmcp_oauth_integration_SUITE.erl` | 400 | Integration tests |
| **TOTAL NEW CODE** | **2,650 LOC** | |

### Modified Files (MODIFY)

| File | Changes | LOC |
|------|---------|-----|
| `apps/erlmcp_core/src/erlmcp_auth.erl` | Add OIDC integration calls | +30 |
| `config/sys.config` | Add OAuth2 configuration | +20 |
| `config/development.config` | Add dev-specific OAuth2 config | +15 |
| `apps/erlmcp_transports/src/erlmcp_transport_http_app.erl` | Register OAuth handler routes | +25 |
| **TOTAL MODIFICATIONS** | | **+90 LOC** |

---

## 11. Timeline

### Week 1: Core Implementation (24-32 hours)

**Day 1-2: Discovery & JWKS (8 hours)**
- Implement `erlmcp_oauth_oidc_discovery.erl`
- Implement `erlmcp_oauth_jwks.erl`
- Write discovery tests
- Write JWKS tests
- **Deliverable:** Discovery endpoint returns valid metadata

**Day 3-4: Authorization Code Flow (10 hours)**
- Implement `erlmcp_oauth_authorization.erl`
- Authorization code generation
- PKCE validation
- Code exchange logic
- **Deliverable:** Authorization code flow working end-to-end

**Day 5: ID Token & Userinfo (6-8 hours)**
- Implement `erlmcp_oauth_id_token.erl`
- Implement `erlmcp_oauth_userinfo.erl`
- JWT signing/verification
- **Deliverable:** ID tokens issued and validated

**Day 6: Scope Handling (4 hours)**
- Implement `erlmcp_oauth_scope_handler.erl`
- Incremental consent logic
- Consent storage
- **Deliverable:** Scope validation working

**Day 7: HTTP Integration (4 hours)**
- Implement `erlmcp_oauth_http_handler.erl`
- Route configuration
- CORS setup
- **Deliverable:** All endpoints accessible via HTTP

### Week 2: Testing & Polish (16-24 hours)

**Day 8-9: Integration Testing (10-12 hours)**
- Write `erlmcp_oauth_integration_SUITE.erl`
- Complete authorization flow test
- PKCE flow test
- Token refresh test
- Revocation test
- **Deliverable:** Full test suite passing

**Day 10: Debugging & Fixes (4-5 hours)**
- Fix test failures
- Performance profiling
- Memory leak checks
- **Deliverable:** All tests green

**Day 11: Documentation (3-4 hours)**
- API reference documentation
- Deployment guide
- Configuration examples
- Migration guide from client credentials
- **Deliverable:** Complete documentation

**Day 12: Code Review & Security Audit (2-3 hours)**
- Peer review
- Security checklist
- OWASP OAuth 2.0 Cheat Sheet compliance
- **Deliverable:** Approved for production

---

## 12. Deployment Strategy

### Phase 1: Discovery + JWKS (Non-Breaking)

**Duration:** 2 days
**Risk:** LOW

**Deploy:**
- `erlmcp_oauth_oidc_discovery.erl`
- `erlmcp_oauth_jwks.erl`
- HTTP routes for `/.well-known/*`

**Validation:**
```bash
curl http://localhost:8080/.well-known/openid-configuration
curl http://localhost:8080/.well-known/jwks.json
```

**Impact:** No breaking changes, existing OAuth2 introspection continues working

### Phase 2: Authorization Code Flow (New Endpoints)

**Duration:** 5 days
**Risk:** MEDIUM

**Deploy:**
- `erlmcp_oauth_authorization.erl`
- `erlmcp_oauth_id_token.erl`
- `/oauth/authorize`, `/oauth/token` endpoints

**Validation:**
```bash
# Authorization request
curl "http://localhost:8080/oauth/authorize?response_type=code&client_id=test&redirect_uri=https://example.com/callback&scope=openid"

# Token exchange
curl -X POST http://localhost:8080/oauth/token \
  -d "grant_type=authorization_code&code=ABC123&redirect_uri=https://example.com/callback&client_id=test&client_secret=secret"
```

**Impact:** New functionality, opt-in for clients

### Phase 3: OIDC/Scope Features (Optional)

**Duration:** 3 days
**Risk:** LOW

**Deploy:**
- `erlmcp_oauth_userinfo.erl`
- `erlmcp_oauth_scope_handler.erl`
- `/oauth/userinfo`, consent UI

**Validation:**
```bash
curl -H "Authorization: Bearer <token>" http://localhost:8080/oauth/userinfo
```

**Impact:** Enhanced features, backwards compatible

### Rollback Plan

**Scenario:** Critical bug in authorization flow

**Action:**
1. Disable OAuth handler routes in HTTP server
2. Revert to client credentials only
3. Redeploy previous version
4. Debug in staging environment

**Data Impact:** Authorization codes are ephemeral (10 min TTL), no persistent data loss

---

## 13. Success Metrics

### Functional Metrics

- **OIDC Discovery Compliance:** 100% of required fields present (RFC 8414)
- **Authorization Flow Success Rate:** >99% for valid requests
- **Token Issuance Rate:** >99.9% for valid codes
- **ID Token Validation Rate:** 100% signature verification success

### Performance Metrics

- **Discovery Endpoint Latency:** p95 <10ms (cached)
- **JWKS Endpoint Latency:** p95 <10ms (cached)
- **Authorization Code Generation:** p95 <50ms
- **Token Issuance:** p95 <100ms
- **Token Validation:** p95 <50ms
- **Userinfo Endpoint:** p95 <50ms

### Security Metrics

- **PKCE Enforcement:** 100% for public clients in production
- **Code Reuse Attempts Blocked:** 100%
- **Expired Code Rejection:** 100%
- **Invalid Signature Rejection:** 100%
- **Audit Log Coverage:** 100% of token lifecycle events

### Compatibility Metrics

- **OpenID Connect Basic RP Conformance:** PASS
- **OAuth 2.0 Security BCP Compliance:** PASS
- **Client Library Support:** oauth2-client.js, passport-openidconnect PASS

---

## 14. Future Enhancements

### Phase 4: Dynamic Client Registration (RFC 7591)

**Endpoint:** `POST /oauth/register`
**Effort:** 12-16 hours
**Priority:** MEDIUM

**Functionality:**
- Self-service client registration
- Client metadata validation
- Client credentials issuance
- Registration access tokens

### Phase 5: Token Introspection Server (RFC 7662)

**Endpoint:** `POST /oauth/introspect`
**Effort:** 8-10 hours
**Priority:** HIGH

**Functionality:**
- Introspection for erlmcp-issued tokens
- Peer-to-peer token validation
- Resource server integration

### Phase 6: Refresh Token Rotation (RFC 6749)

**Effort:** 6-8 hours
**Priority:** HIGH

**Functionality:**
- Automatic refresh token rotation on use
- Refresh token family tracking
- Breach detection (token reuse)

### Phase 7: Advanced Grant Types

**Effort:** 20-24 hours
**Priority:** LOW

**Functionality:**
- Device authorization grant (RFC 8628)
- CIBA (Client-Initiated Backchannel Authentication)
- Token exchange (RFC 8693)

---

## 15. Risks & Mitigation

### Risk 1: JWT Signature Performance Bottleneck

**Likelihood:** MEDIUM
**Impact:** HIGH

**Mitigation:**
- Pre-generate JWKs in ETS cache
- Use RSA 2048-bit (faster than 4096-bit)
- Consider ECDSA ES256 (faster signing)
- Benchmark: `erlmcp_bench_integration` with token issuance

### Risk 2: Authorization Code Brute-Force

**Likelihood:** LOW
**Impact:** HIGH

**Mitigation:**
- 256-bit entropy (2^256 guesses)
- 10-minute TTL (small attack window)
- Rate limiting on `/oauth/token` endpoint
- Intrusion detection via `erlmcp_health_monitor`

### Risk 3: PKCE Bypass Attack

**Likelihood:** LOW
**Impact:** HIGH

**Mitigation:**
- Enforce PKCE for public clients (config flag)
- Only support S256 (SHA-256), not 'plain'
- Validate code_verifier length (43-128 chars)
- Security test suite with attack scenarios

### Risk 4: Key Rotation Downtime

**Likelihood:** LOW
**Impact:** MEDIUM

**Mitigation:**
- Grace period: Old keys valid 7 days post-rotation
- JWKS includes both old and new keys
- Clients automatically refresh JWKS on validation failure
- Monitoring: `erlmcp_metrics` tracks validation failures

### Risk 5: OAuth Client Impersonation

**Likelihood:** MEDIUM
**Impact:** HIGH

**Mitigation:**
- Validate redirect_uri against pre-registered whitelist
- Client secret hashing (bcrypt cost 12)
- mTLS client authentication (future)
- Audit log all authorization grants

---

## 16. References

### Specifications

- **RFC 6749:** OAuth 2.0 Authorization Framework
- **RFC 8414:** OAuth 2.0 Authorization Server Metadata
- **RFC 7517:** JSON Web Key (JWK)
- **RFC 7519:** JSON Web Token (JWT)
- **RFC 7662:** OAuth 2.0 Token Introspection
- **RFC 7009:** OAuth 2.0 Token Revocation
- **RFC 7636:** PKCE for OAuth Public Clients
- **RFC 8252:** OAuth 2.0 for Native Apps
- **OpenID Connect Core 1.0**
- **OpenID Connect Discovery 1.0**

### Security Guidelines

- **OWASP OAuth 2.0 Cheat Sheet:** https://cheatsheetseries.owasp.org/cheatsheets/OAuth2_Cheat_Sheet.html
- **OAuth 2.0 Security Best Current Practice:** https://datatracker.ietf.org/doc/html/draft-ietf-oauth-security-topics
- **OAuth 2.0 Threat Model:** RFC 6819

### Erlang Libraries

- **jose:** https://hex.pm/packages/jose (JWT/JWS/JWK/JWE)
- **cowboy:** https://hex.pm/packages/cowboy (HTTP server)
- **gun:** https://hex.pm/packages/gun (HTTP client)
- **jsx:** https://hex.pm/packages/jsx (JSON)

---

**END OF IMPLEMENTATION PLAN**
